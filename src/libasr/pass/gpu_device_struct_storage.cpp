#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_data_layout.h>
#include <libasr/pass/gpu_device_struct_storage.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/symbol_expr_substitution.h>

#include <map>
#include <set>
#include <string>
#include <tuple>
#include <vector>

namespace LCompilers {

/*
Give the allocatable array components of a derived-type local of device code
storage of their own.

A device has no heap, so an allocatable array of device code becomes an array
of the shape its `allocate` gives it, known when the routine is entered (see
gpu_device_allocatable). An allocatable array component of a derived-type
local -- a local of the routine, or a value the routine gets from a function
such as a structure constructor, `associate (t => tensor_t(x))` -- is the
same: it is given a shape inside device code, and has no storage there.

So every such component becomes a local array of its own, `__s_v` for
`s%v`, declared in the scope of `s` with the shape the routine gives the
component, written over the routine's dummy arguments: the extents of the
`allocate` or reallocation of `s%v`, of the array an assignment to `s%v`
reallocates it to, of the same component of a value assigned to `s` as a
whole (which the assignment then copies into `__s_v`), or the shape a
procedure gives the component of its `intent(out)` or `intent(inout)` dummy
argument when `s` is passed for it, in terms of the actual arguments of that
call. In a kernel the shape is left to the kernel layout, which writes it
over the kernel's parameters and its thread index. Every reference to `s%v`
becomes one to `__s_v`, and the scope of `s` starts with `s%v => __s_v`,
which tells the code generators where the component is stored: a reference
to `s` as a whole -- an actual argument -- hands the procedure `__s_v` as
the component's data, as it hands over the components of every
derived-type dummy argument. `__s_v` is then a run-time sized local like any other, which
gpu_device_workspace gives the callers of the routine, up to the kernel,
where the host sizes it before the launch.

A component whose shape cannot be told on entry to the routine -- it is
given several different ones, or one that reads a value the routine
computes -- keeps no storage, and the loop is reported as one that cannot be
offloaded yet.
*/

void gpu_routine_scopes(SymbolTable *scope, std::vector<SymbolTable*> &out) {
    out.push_back(scope);
    for (auto &item : scope->get_scope()) {
        if (ASR::is_a<ASR::Block_t>(*item.second)) {
            gpu_routine_scopes(ASR::down_cast<ASR::Block_t>(
                item.second)->m_symtab, out);
        } else if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
            gpu_routine_scopes(ASR::down_cast<ASR::AssociateBlock_t>(
                item.second)->m_symtab, out);
        }
    }
}

bool gpu_struct_component_storage(ASR::stmt_t *stmt,
        ASR::StructInstanceMember_t *&component, ASR::expr_t *&storage) {
    if (!ASR::is_a<ASR::Associate_t>(*stmt)) return false;
    ASR::Associate_t *x = ASR::down_cast<ASR::Associate_t>(stmt);
    if (!ASR::is_a<ASR::StructInstanceMember_t>(*x->m_target)) return false;
    component = ASR::down_cast<ASR::StructInstanceMember_t>(x->m_target);
    storage = x->m_value;
    return ASR::is_a<ASR::Var_t>(*component->m_v) &&
        ASR::is_a<ASR::Var_t>(*storage);
}

namespace {

ASR::expr_t* duplicate(Allocator &al, ASR::expr_t *e) {
    if (e == nullptr) return nullptr;
    ASRUtils::ExprStmtDuplicator duplicator(al);
    duplicator.allow_procedure_calls = true;
    duplicator.success = true;
    ASR::expr_t *copy = duplicator.duplicate_expr(e);
    return duplicator.success ? copy : nullptr;
}

ASR::symbol_t* variable_of(ASR::expr_t *e) {
    if (e == nullptr) return nullptr;
    e = ASRUtils::get_past_array_physical_cast(e);
    if (!ASR::is_a<ASR::Var_t>(*e)) return nullptr;
    return ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(e)->m_v);
}

// The name of the component `e` selects of the variable `base`, or "" when
// `e` is not such a component.
std::string component_name(ASR::expr_t *e, ASR::symbol_t *base) {
    if (e == nullptr) return "";
    e = ASRUtils::get_past_array_physical_cast(e);
    if (!ASR::is_a<ASR::StructInstanceMember_t>(*e)) return "";
    ASR::StructInstanceMember_t *m =
        ASR::down_cast<ASR::StructInstanceMember_t>(e);
    if (variable_of(m->m_v) != base) return "";
    return ASRUtils::symbol_name(ASRUtils::symbol_get_past_external(m->m_m));
}

ASR::Struct_t* struct_of(ASR::Variable_t *var) {
    if (var->m_type_declaration == nullptr) return nullptr;
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
        var->m_type_declaration);
    if (!ASR::is_a<ASR::Struct_t>(*s)) return nullptr;
    return ASR::down_cast<ASR::Struct_t>(s);
}

// A derived-type scalar local whose type hands allocatable array components
// over as device buffers of their own.
bool is_struct_local(ASR::Variable_t *var) {
    if (var->m_intent != ASR::intentType::Local) return false;
    if (!ASR::is_a<ASR::StructType_t>(*var->m_type)) return false;
    ASR::Struct_t *st = struct_of(var);
    return st != nullptr && !gpu_decomposed_components(st).empty();
}

ASR::Function_t* call_target(ASR::symbol_t *name) {
    ASR::symbol_t *target = ASRUtils::symbol_get_past_external(name);
    if (target == nullptr) return nullptr;
    target = ASRUtils::symbol_get_past_StructMethodDeclaration(target);
    target = ASRUtils::symbol_get_past_external(target);
    if (target == nullptr || !ASR::is_a<ASR::Function_t>(*target)) {
        return nullptr;
    }
    return ASR::down_cast<ASR::Function_t>(target);
}

// The dummy argument of `callee` the actual argument `i` of a call is
// associated with, or nullptr when the call does not say.
ASR::Variable_t* dummy_of(ASR::Function_t *callee, size_t i, size_t n_args) {
    if (callee == nullptr || n_args != callee->n_args) return nullptr;
    ASR::symbol_t *dummy = variable_of(callee->m_args[i]);
    if (dummy == nullptr || !ASR::is_a<ASR::Variable_t>(*dummy)) {
        return nullptr;
    }
    return ASR::down_cast<ASR::Variable_t>(dummy);
}

bool dummy_is_read_only(ASR::Variable_t *dummy) {
    return dummy != nullptr && dummy->m_intent == ASR::intentType::In;
}

// How many times the statements of a routine give the variable `target` a
// value as a whole, and the last value they give it: an assignment to a
// scalar, a pointer association of an array name. Anything else that may
// change it -- a loop over it, an argument of a call that may define it --
// counts too, without a value.
class Bindings : public ASRUtils::BlockBodyWalkVisitor<Bindings> {
    using Base = ASRUtils::BlockBodyWalkVisitor<Bindings>;
    ASR::symbol_t *target;
    // A pointer is bound by a pointer association: a value written to it
    // goes to what it points to.
    bool pointer;

    bool is_target(ASR::expr_t *e) { return variable_of(e) == target; }

    void call_arguments(ASR::symbol_t *name, ASR::call_arg_t *args,
            size_t n_args) {
        ASR::Function_t *callee = call_target(name);
        for (size_t i = 0; i < n_args; i++) {
            if (!is_target(args[i].m_value)) continue;
            ASR::Variable_t *dummy = dummy_of(callee, i, n_args);
            if (dummy_is_read_only(dummy)) continue;
            if (pointer && dummy && !ASRUtils::is_pointer(dummy->m_type) &&
                    !ASRUtils::is_allocatable(dummy->m_type)) {
                continue;
            }
            writes++;
            value = nullptr;
        }
    }

public:
    size_t writes = 0;
    ASR::expr_t *value = nullptr;

    explicit Bindings(ASR::symbol_t *target) : target(target),
        pointer(ASRUtils::is_pointer(ASRUtils::symbol_type(target))) {}

    void visit_Assignment(const ASR::Assignment_t &x) {
        if (!pointer && is_target(x.m_target)) {
            writes++;
            value = x.m_value;
        }
        Base::visit_Assignment(x);
    }

    void visit_Associate(const ASR::Associate_t &x) {
        if (is_target(x.m_target)) {
            writes++;
            value = x.m_value;
        }
        Base::visit_Associate(x);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        if (is_target(x.m_head.m_v)) {
            writes++;
            value = nullptr;
        }
        Base::visit_DoLoop(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        call_arguments(x.m_name, x.m_args, x.n_args);
        Base::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        call_arguments(x.m_name, x.m_args, x.n_args);
        Base::visit_FunctionCall(x);
    }

    template <typename T>
    void allocation(const T &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (is_target(x.m_args[i].m_a)) {
                writes++;
                value = nullptr;
            }
        }
    }

    void visit_Allocate(const ASR::Allocate_t &x) { allocation(x); }
    void visit_ReAlloc(const ASR::ReAlloc_t &x) { allocation(x); }

    void visit_Nullify(const ASR::Nullify_t &x) {
        for (size_t i = 0; i < x.n_vars; i++) {
            if (is_target(x.m_vars[i])) {
                writes++;
                value = nullptr;
            }
        }
    }

    void visit_IntrinsicImpureSubroutine(
            const ASR::IntrinsicImpureSubroutine_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (is_target(x.m_args[i])) {
                writes++;
                value = nullptr;
            }
        }
        Base::visit_IntrinsicImpureSubroutine(x);
    }
};

typedef std::map<std::pair<ASR::symbol_t*, std::string>, ASR::symbol_t*>
    StorageMap;

// Writes an expression of a device routine as the value it has on entry to
// the routine, over the routine's dummy arguments and named constants
// alone. A local that the routine binds once -- a scalar assigned once, an
// array name associated once, as an ASSOCIATE construct leaves them -- is
// replaced by what it is bound to; the shape of a local array of an
// explicit shape, or of a section, by its extents. A dummy argument is read
// as a value only when it is `intent(in)`: the shape of any array dummy is
// fixed for the whole call. Anything else invalidates the writer.
class EntryValueWriter : public ASR::BaseExprReplacer<EntryValueWriter> {
    using Base = ASR::BaseExprReplacer<EntryValueWriter>;
    Allocator &al;
    ASR::Function_t &fn;
    const StorageMap &storage;
    std::set<ASR::symbol_t*> dummies, expanding;
    bool in_shape = false;

    ASR::Variable_t* local(ASR::symbol_t *sym) {
        if (sym == nullptr || !ASR::is_a<ASR::Variable_t>(*sym)) {
            return nullptr;
        }
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
        if (dummies.count(sym)) return nullptr;
        return var;
    }

    ASR::expr_t* binding(ASR::symbol_t *sym) {
        Bindings bindings(sym);
        for (size_t i = 0; i < fn.n_body; i++) {
            bindings.visit_stmt(*fn.m_body[i]);
        }
        return bindings.writes == 1 ? bindings.value : nullptr;
    }

    // Writes `e` in place under the shape flag `shape`.
    ASR::expr_t* rewrite(ASR::expr_t *e, bool shape) {
        ASR::expr_t *copy = duplicate(al, e);
        if (copy == nullptr) {
            valid = false;
            return nullptr;
        }
        bool outer_shape = in_shape;
        ASR::expr_t **outer = current_expr;
        in_shape = shape;
        current_expr = &copy;
        replace_expr(copy);
        current_expr = outer;
        in_shape = outer_shape;
        return copy;
    }

    ASR::expr_t* as(ASR::expr_t *e, ASR::ttype_t *type) {
        ASRUtils::ASRBuilder b(al, e->base.loc);
        return b.i2i_t(e, type);
    }

    // The storage a component of a derived-type local of the routine has.
    ASR::symbol_t* storage_of(ASR::expr_t *e) {
        e = ASRUtils::get_past_array_physical_cast(e);
        if (!ASR::is_a<ASR::StructInstanceMember_t>(*e)) return nullptr;
        ASR::StructInstanceMember_t *m =
            ASR::down_cast<ASR::StructInstanceMember_t>(e);
        ASR::symbol_t *base = variable_of(m->m_v);
        auto it = storage.find({base, ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(m->m_m))});
        return it == storage.end() ? nullptr : it->second;
    }

    // The dimensions an array of an explicit shape declares: a local, or
    // a dummy argument, such as storage the routine is given by its
    // callers (see gpu_device_workspace.cpp).
    ASR::Array_t* explicit_shape(ASR::symbol_t *sym) {
        if (sym == nullptr || !ASR::is_a<ASR::Variable_t>(*sym)) {
            return nullptr;
        }
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
        if (!ASR::is_a<ASR::Array_t>(*var->m_type)) {
            return nullptr;
        }
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(var->m_type);
        for (size_t d = 0; d < arr->n_dims; d++) {
            if (arr->m_dims[d].m_length == nullptr) return nullptr;
        }
        return arr;
    }

    // The extent of the dimensions [first, last) of an array, written out,
    // or one of its bounds when `bound` is given.
    struct Extent {
        ASR::expr_t *lower = nullptr;
        ASR::expr_t *length = nullptr;
    };

    bool extents(ASR::expr_t *array, std::vector<Extent> &out, int depth) {
        if (depth > 16) return false;
        array = ASRUtils::get_past_array_physical_cast(array);
        ASRUtils::ASRBuilder b(al, array->base.loc);
        ASR::ttype_t *index_type = ASRUtils::TYPE(ASR::make_Integer_t(al,
            array->base.loc, 4));
        if (ASR::symbol_t *s = storage_of(array)) {
            return extents(ASRUtils::EXPR(ASR::make_Var_t(al,
                array->base.loc, s)), out, depth + 1);
        }
        if (ASR::is_a<ASR::Var_t>(*array)) {
            ASR::symbol_t *sym = variable_of(array);
            if (ASR::Array_t *arr = explicit_shape(sym)) {
                for (size_t d = 0; d < arr->n_dims; d++) {
                    Extent e;
                    e.lower = arr->m_dims[d].m_start
                        ? rewrite(arr->m_dims[d].m_start, false)
                        : b.i_t(1, index_type);
                    e.length = rewrite(arr->m_dims[d].m_length, false);
                    if (!valid) return false;
                    out.push_back(e);
                }
                return true;
            }
            if (local(sym) == nullptr || expanding.count(sym)) return false;
            ASR::expr_t *bound = binding(sym);
            if (bound == nullptr) return false;
            expanding.insert(sym);
            bool ok = extents(bound, out, depth + 1);
            expanding.erase(sym);
            return ok;
        }
        if (ASR::is_a<ASR::ArraySection_t>(*array)) {
            ASR::ArraySection_t *section =
                ASR::down_cast<ASR::ArraySection_t>(array);
            std::vector<Extent> base;
            for (size_t i = 0; i < section->n_args; i++) {
                ASR::array_index_t &index = section->m_args[i];
                if (index.m_step == nullptr) continue;
                ASR::expr_t *lo = index.m_left ? index.m_left
                    : b.ArrayLBound(section->m_v, i + 1);
                ASR::expr_t *hi = index.m_right ? index.m_right
                    : b.ArrayUBound(section->m_v, i + 1);
                lo = attempt(as(lo, index_type));
                hi = attempt(as(hi, index_type));
                ASR::expr_t *step = attempt(as(index.m_step, index_type));
                Extent e;
                e.lower = b.i_t(1, index_type);
                if (lo == nullptr || hi == nullptr || step == nullptr) {
                    // A section's extent the routine only knows where it
                    // forms the section; its lower bound is 1 anyway.
                    if (!bounding) {
                        out.push_back(e);
                        continue;
                    }
                    // A section never has more elements in a dimension
                    // than its array has there.
                    if (base.empty() &&
                            !extents(section->m_v, base, depth + 1)) {
                        return false;
                    }
                    if (i >= base.size() || base[i].length == nullptr) {
                        return false;
                    }
                    e.length = base[i].length;
                    out.push_back(e);
                    continue;
                }
                ASR::expr_t *count = b.Div(b.Add(b.Sub(hi, lo), step), step);
                e.length = ASRUtils::EXPR(ASR::make_IfExp_t(al,
                    array->base.loc, b.Gt(count, b.i_t(0, index_type)),
                    duplicate(al, count), b.i_t(0, index_type), index_type,
                    nullptr));
                out.push_back(e);
            }
            return true;
        }
        // An array whose shape is fixed for the whole call.
        if (dummy_shape(array)) {
            size_t rank = ASRUtils::extract_n_dims_from_ttype(
                ASRUtils::expr_type(array));
            for (size_t d = 0; d < rank; d++) {
                Extent e;
                e.lower = b.ArrayLBound(duplicate(al, array), d + 1);
                e.length = b.ArraySize(duplicate(al, array),
                    b.i_t(d + 1, index_type), index_type);
                out.push_back(e);
            }
            return true;
        }
        return false;
    }

    // `e` written out, or nullptr, leaving the writer valid either way.
    ASR::expr_t* attempt(ASR::expr_t *e) {
        bool outer = valid;
        valid = true;
        ASR::expr_t *written = rewrite(e, false);
        bool ok = valid;
        valid = outer;
        return ok ? written : nullptr;
    }

    // Whether the shape of `array` is fixed for the whole call: an array
    // dummy argument, or a component of one.
    bool dummy_shape(ASR::expr_t *array) {
        array = ASRUtils::get_past_array_physical_cast(array);
        while (true) {
            if (ASR::is_a<ASR::StructInstanceMember_t>(*array)) {
                array = ASR::down_cast<ASR::StructInstanceMember_t>(
                    array)->m_v;
            } else if (ASR::is_a<ASR::ArrayItem_t>(*array)) {
                array = ASR::down_cast<ASR::ArrayItem_t>(array)->m_v;
            } else {
                break;
            }
            array = ASRUtils::get_past_array_physical_cast(array);
        }
        return dummies.count(variable_of(array)) > 0;
    }

public:
    bool valid = true;
    // Leaves what it cannot write over the dummy arguments as it is,
    // rather than failing: the kernel layout writes the extents of a
    // kernel's own arrays over its parameters and its thread index.
    bool lenient = false;
    // Writes a bound of the value rather than the value: the extent of a
    // section whose subscripts cannot be written on entry is bounded by
    // the extent of its array.
    bool bounding = false;

    EntryValueWriter(Allocator &al, ASR::Function_t &fn,
            const StorageMap &storage)
        : al(al), fn(fn), storage(storage) {
        for (size_t i = 0; i < fn.n_args; i++) {
            if (ASR::symbol_t *dummy = variable_of(fn.m_args[i])) {
                dummies.insert(dummy);
            }
        }
    }

    ASR::expr_t* write(ASR::expr_t *e) {
        if (e == nullptr) return nullptr;
        ASR::expr_t *copy = rewrite(e, false);
        return valid ? copy : nullptr;
    }

    void replace_ttype(ASR::ttype_t *) {}

    void replace_Var(ASR::Var_t *x) {
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x->m_v);
        if (dummies.count(sym)) {
            if (!lenient && !in_shape && !dummy_is_read_only(
                    ASR::down_cast<ASR::Variable_t>(sym))) {
                valid = false;
            }
            return;
        }
        if (!ASR::is_a<ASR::Variable_t>(*sym)) {
            valid = false;
            return;
        }
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
        if (var->m_storage == ASR::storage_typeType::Parameter) {
            if (var->m_value == nullptr) {
                valid = false;
                return;
            }
            *current_expr = duplicate(al, var->m_value);
            return;
        }
        if (lenient && !ASRUtils::is_pointer(var->m_type)) return;
        if (expanding.count(sym)) {
            valid = false;
            return;
        }
        // An array name stands for what it is associated with only as a
        // pointer: an array a value is assigned to can be changed element
        // by element afterwards.
        if (ASRUtils::is_array(var->m_type) &&
                !ASRUtils::is_pointer(var->m_type)) {
            valid = false;
            return;
        }
        ASR::expr_t *bound = binding(sym);
        if (bound == nullptr) {
            if (!lenient) valid = false;
            return;
        }
        expanding.insert(sym);
        ASR::expr_t *value = rewrite(bound, in_shape);
        expanding.erase(sym);
        if (valid) *current_expr = value;
    }

    void replace_ArraySize(ASR::ArraySize_t *x) {
        if (dummy_shape(x->m_v) && storage_of(x->m_v) == nullptr) {
            bool outer = in_shape;
            in_shape = true;
            Base::replace_ArraySize(x);
            in_shape = outer;
            return;
        }
        std::vector<Extent> all;
        if (!extents(x->m_v, all, 0)) {
            if (lenient) {
                valid = true;
                return;
            }
            valid = false;
            return;
        }
        size_t first = 0, last = all.size();
        if (x->m_dim) {
            int64_t d = 0;
            if (!ASRUtils::extract_value(ASRUtils::expr_value(x->m_dim), d)
                    || d < 1 || (size_t)d > all.size()) {
                valid = false;
                return;
            }
            first = d - 1;
            last = d;
        }
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        ASR::expr_t *size = nullptr;
        for (size_t d = first; d < last; d++) {
            if (all[d].length == nullptr) {
                valid = lenient;
                return;
            }
            ASR::expr_t *length = as(all[d].length, x->m_type);
            size = size ? b.Mul(size, length) : length;
        }
        if (size == nullptr) size = b.i_t(1, x->m_type);
        *current_expr = size;
    }

    void replace_ArrayBound(ASR::ArrayBound_t *x) {
        if (dummy_shape(x->m_v) && storage_of(x->m_v) == nullptr) {
            bool outer = in_shape;
            in_shape = true;
            Base::replace_ArrayBound(x);
            in_shape = outer;
            return;
        }
        std::vector<Extent> all;
        int64_t d = 0;
        if (x->m_dim == nullptr || !ASRUtils::extract_value(
                ASRUtils::expr_value(x->m_dim), d) || !extents(x->m_v, all, 0)
                || d < 1 || (size_t)d > all.size()) {
            valid = lenient;
            return;
        }
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        ASR::expr_t *lower = as(all[d - 1].lower, x->m_type);
        if (x->m_bound == ASR::arrayboundType::LBound) {
            *current_expr = lower;
            return;
        }
        if (all[d - 1].length == nullptr) {
            valid = lenient;
            return;
        }
        *current_expr = b.Sub(b.Add(lower, as(all[d - 1].length, x->m_type)),
            b.i_t(1, x->m_type));
    }

    void replace_ArrayItem(ASR::ArrayItem_t *x) {
        bool outer = in_shape;
        in_shape = false;
        Base::replace_ArrayItem(x);
        in_shape = outer;
    }

    void replace_ArraySection(ASR::ArraySection_t *) {
        valid = false;
    }

    // A function without side effects, such as the routine shared
    // lowering gives `maxval`, has the value it has on entry when its
    // arguments do; the host calls it as the device does.
    void replace_FunctionCall(ASR::FunctionCall_t *x) {
        ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(x->m_name);
        if (callee == nullptr || !ASR::is_a<ASR::Function_t>(*callee) ||
                !ASRUtils::is_side_effect_free_function(
                    *ASR::down_cast<ASR::Function_t>(callee))) {
            valid = false;
            return;
        }
        bool outer = in_shape;
        in_shape = false;
        Base::replace_FunctionCall(x);
        in_shape = outer;
    }
};

// The bindings of the components of the derived-type locals of `fn` to
// their storage, as gpu_give_struct_component_storage writes them.
StorageMap storage_bindings(ASR::Function_t &fn) {
    StorageMap bound;
    std::vector<SymbolTable*> scopes;
    gpu_routine_scopes(fn.m_symtab, scopes);
    for (SymbolTable *scope : scopes) {
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
        ASR::stmt_t **body = nullptr;
        size_t n_body = 0;
        if (ASR::is_a<ASR::Function_t>(*owner)) {
            body = ASR::down_cast<ASR::Function_t>(owner)->m_body;
            n_body = ASR::down_cast<ASR::Function_t>(owner)->n_body;
        } else if (ASR::is_a<ASR::Block_t>(*owner)) {
            body = ASR::down_cast<ASR::Block_t>(owner)->m_body;
            n_body = ASR::down_cast<ASR::Block_t>(owner)->n_body;
        } else if (ASR::is_a<ASR::AssociateBlock_t>(*owner)) {
            body = ASR::down_cast<ASR::AssociateBlock_t>(owner)->m_body;
            n_body = ASR::down_cast<ASR::AssociateBlock_t>(owner)->n_body;
        }
        for (size_t i = 0; i < n_body; i++) {
            ASR::StructInstanceMember_t *component = nullptr;
            ASR::expr_t *array = nullptr;
            if (!gpu_struct_component_storage(body[i], component, array)) {
                continue;
            }
            bound[{variable_of(component->m_v), ASRUtils::symbol_name(
                ASRUtils::symbol_get_past_external(component->m_m))}] =
                variable_of(array);
        }
    }
    return bound;
}

// One shape the statements of a routine give the component `component` of
// the variable `base`, as an allocation states it.
typedef std::vector<ASR::dimension_t> Shape;

// What a procedure does to the component of a dummy argument: gives it a
// shape over its own dummy arguments, leaves it alone, or something that
// cannot be told.
enum class CallEffect { Unknown, Untouched, Shaped };

class StructStorage;

// Every shape the statements of a routine give the component `component` of
// the variable `base`, and whether anything else may change its allocation
// in a way that cannot be told.
class ComponentShapes :
        public ASRUtils::BlockBodyWalkVisitor<ComponentShapes> {
    using Base = ASRUtils::BlockBodyWalkVisitor<ComponentShapes>;
    Allocator &al;
    StructStorage &pass;
    ASR::symbol_t *base;
    std::string component;

    Shape value_shape(ASR::expr_t *value) {
        Shape shape;
        ASRUtils::ASRBuilder b(al, value->base.loc);
        ASR::ttype_t *index_type = ASRUtils::TYPE(ASR::make_Integer_t(al,
            value->base.loc, 4));
        size_t rank = ASRUtils::extract_n_dims_from_ttype(
            ASRUtils::expr_type(value));
        for (size_t d = 0; d < rank; d++) {
            ASR::dimension_t dim;
            dim.loc = value->base.loc;
            dim.m_start = b.i_t(1, index_type);
            dim.m_length = b.ArraySize(duplicate(al, value),
                b.i_t(d + 1, index_type), index_type);
            shape.push_back(dim);
        }
        return shape;
    }

    template <typename T>
    void allocation(const T &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (component_name(x.m_args[i].m_a, base) != component) continue;
            Shape shape;
            for (size_t d = 0; d < x.m_args[i].n_dims; d++) {
                shape.push_back(x.m_args[i].m_dims[d]);
            }
            shapes.push_back(shape);
        }
    }

    void call_arguments(const Location &loc, ASR::symbol_t *name,
        ASR::call_arg_t *args, size_t n_args);

public:
    bool known = true;
    std::vector<Shape> shapes;

    ComponentShapes(Allocator &al, StructStorage &pass, ASR::symbol_t *base,
            const std::string &component)
        : al(al), pass(pass), base(base), component(component) {}

    void visit_Allocate(const ASR::Allocate_t &x) { allocation(x); }
    void visit_ReAlloc(const ASR::ReAlloc_t &x) { allocation(x); }

    void visit_Assignment(const ASR::Assignment_t &x) {
        Base::visit_Assignment(x);
        if (component_name(x.m_target, base) == component) {
            if (x.m_realloc_lhs && ASRUtils::is_array(
                    ASRUtils::expr_type(x.m_value)) &&
                    !ASR::is_a<ASR::ArrayBroadcast_t>(*x.m_value)) {
                shapes.push_back(value_shape(x.m_value));
            }
        } else if (variable_of(x.m_target) == base) {
            // A whole value assigned to the variable, a variable or an
            // element or a component of one: its component has the shape
            // of the component of the value.
            ASR::expr_t *from = ASRUtils::get_past_array_physical_cast(
                x.m_value);
            ASR::symbol_t *member = nullptr;
            if (ASR::is_a<ASR::Var_t>(*from) ||
                    ASR::is_a<ASR::ArrayItem_t>(*from) ||
                    ASR::is_a<ASR::StructInstanceMember_t>(*from)) {
                ASR::symbol_t *st =
                    ASRUtils::get_struct_sym_from_struct_expr(from);
                member = st ? gpu_struct_lookup_member(st, component)
                    : nullptr;
            }
            if (member == nullptr) {
                known = false;
                return;
            }
            ASR::expr_t *value = ASRUtils::EXPR(
                ASR::make_StructInstanceMember_t(al, x.base.base.loc,
                    duplicate(al, ASRUtils::get_past_array_physical_cast(
                        x.m_value)),
                    member, ASRUtils::symbol_type(member), nullptr));
            shapes.push_back(value_shape(value));
        }
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        call_arguments(x.base.base.loc, x.m_name, x.m_args, x.n_args);
        Base::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        call_arguments(x.base.base.loc, x.m_name, x.m_args, x.n_args);
        Base::visit_FunctionCall(x);
    }

    void visit_IntrinsicImpureSubroutine(
            const ASR::IntrinsicImpureSubroutine_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (variable_of(x.m_args[i]) == base ||
                    component_name(x.m_args[i], base) == component) {
                known = false;
            }
        }
        Base::visit_IntrinsicImpureSubroutine(x);
    }
};

class StructStorage {
    Allocator &al;
    ASR::TranslationUnit_t &unit;
    struct Summary {
        CallEffect effect = CallEffect::Unknown;
        Shape shape;
    };
    std::map<std::tuple<ASR::Function_t*, ASR::symbol_t*, std::string>,
        Summary> summaries;
    std::set<std::tuple<ASR::Function_t*, ASR::symbol_t*, std::string>>
        summarizing;

    std::vector<ASR::Function_t*> device_routines() {
        std::vector<ASR::Function_t*> out;
        std::vector<SymbolTable*> work = {unit.m_symtab};
        while (!work.empty()) {
            SymbolTable *scope = work.back();
            work.pop_back();
            for (auto &item : scope->get_scope()) {
                ASR::symbol_t *sym = item.second;
                if (ASR::is_a<ASR::Module_t>(*sym)) {
                    work.push_back(ASR::down_cast<ASR::Module_t>(sym)
                        ->m_symtab);
                } else if (ASR::is_a<ASR::Program_t>(*sym)) {
                    work.push_back(ASR::down_cast<ASR::Program_t>(sym)
                        ->m_symtab);
                } else if (ASR::is_a<ASR::Function_t>(*sym)) {
                    ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
                    if (ASRUtils::runs_on_device(*fn)) out.push_back(fn);
                    work.push_back(fn->m_symtab);
                }
            }
        }
        return out;
    }

    // The single shape of a list of shapes, written over the dummy
    // arguments of `fn`, or false.
    bool single_shape(ASR::Function_t &fn, const std::vector<Shape> &shapes,
            Shape &out) {
        if (shapes.empty()) return false;
        for (size_t k = 1; k < shapes.size(); k++) {
            if (shapes[k].size() != shapes[0].size()) return false;
            for (size_t d = 0; d < shapes[0].size(); d++) {
                if (!same(shapes[0][d].m_length, shapes[k][d].m_length) ||
                        !same(shapes[0][d].m_start, shapes[k][d].m_start)) {
                    return false;
                }
            }
        }
        out.clear();
        EntryValueWriter writer(al, fn, storage);
        writer.lenient = ASRUtils::is_device_kernel(&fn.base);
        for (const ASR::dimension_t &dim : shapes[0]) {
            if (dim.m_length == nullptr) return false;
            ASR::dimension_t written;
            written.loc = dim.loc;
            written.m_start = dim.m_start ? writer.write(dim.m_start)
                : nullptr;
            written.m_length = writer.write(dim.m_length);
            if (!writer.valid) return false;
            out.push_back(written);
        }
        return true;
    }

    static bool same(ASR::expr_t *a, ASR::expr_t *b) {
        return PassUtils::same_expression(a, b);
    }

    // The shapes the statements of `fn` give the component of `base`.
    ComponentShapes shapes_of(ASR::Function_t &fn, ASR::symbol_t *base,
            const std::string &component) {
        ComponentShapes collector(al, *this, base, component);
        for (size_t i = 0; i < fn.n_body; i++) {
            collector.visit_stmt(*fn.m_body[i]);
        }
        return collector;
    }

public:
    StorageMap storage;

    StructStorage(Allocator &al, ASR::TranslationUnit_t &unit)
        : al(al), unit(unit) {}

    // What a call of `callee` does to the component `component` of the
    // actual argument associated with `dummy`, as a shape over the actual
    // arguments `args`.
    CallEffect call_effect(ASR::Function_t &callee, ASR::Variable_t &dummy,
            const std::string &component, ASR::call_arg_t *args,
            size_t n_args, Shape &shape) {
        auto key = std::make_tuple(&callee, &dummy.base, component);
        auto it = summaries.find(key);
        if (it == summaries.end()) {
            if (summarizing.count(key)) return CallEffect::Unknown;
            summarizing.insert(key);
            Summary summary;
            if (ASRUtils::get_FunctionType(callee)->m_deftype ==
                    ASR::deftypeType::Implementation) {
                ComponentShapes found = shapes_of(callee, &dummy.base,
                    component);
                if (found.known && found.shapes.empty()) {
                    summary.effect = CallEffect::Untouched;
                } else if (found.known && single_shape(callee,
                        found.shapes, summary.shape)) {
                    summary.effect = CallEffect::Shaped;
                }
            }
            summarizing.erase(key);
            it = summaries.emplace(key, summary).first;
        }
        if (it->second.effect != CallEffect::Shaped) return it->second.effect;
        std::map<ASR::symbol_t*, ASR::expr_t*> actuals;
        for (size_t i = 0; i < n_args && i < callee.n_args; i++) {
            ASR::symbol_t *d = variable_of(callee.m_args[i]);
            if (d && args[i].m_value) actuals[d] = args[i].m_value;
        }
        AssociateVarResolver resolver(al, actuals);
        shape.clear();
        for (const ASR::dimension_t &dim : it->second.shape) {
            ASR::dimension_t bound;
            bound.loc = dim.loc;
            bound.m_start = duplicate(al, dim.m_start);
            bound.m_length = duplicate(al, dim.m_length);
            ASR::expr_t **parts[2] = {&bound.m_start, &bound.m_length};
            for (ASR::expr_t **part : parts) {
                if (*part == nullptr) continue;
                resolver.current_expr = part;
                resolver.replace_expr(*part);
            }
            shape.push_back(bound);
        }
        return CallEffect::Shaped;
    }

    // Gives the derived-type locals of `fn` the storage of their
    // components. Returns whether it gave any.
    bool give(ASR::Function_t &fn) {
        std::vector<SymbolTable*> scopes;
        gpu_routine_scopes(fn.m_symtab, scopes);
        bool changed = false;
        for (SymbolTable *scope : scopes) {
            std::vector<ASR::Variable_t*> locals;
            for (auto &item : scope->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *var =
                    ASR::down_cast<ASR::Variable_t>(item.second);
                if (is_struct_local(var)) locals.push_back(var);
            }
            for (ASR::Variable_t *var : locals) {
                if (give(fn, *scope, *var)) changed = true;
            }
        }
        return changed;
    }

    bool give(ASR::Function_t &fn, SymbolTable &scope, ASR::Variable_t &var) {
        ASR::Struct_t *st = struct_of(&var);
        std::vector<GpuComponentLayout> components =
            gpu_decomposed_components(st);
        std::vector<Shape> shapes;
        for (const GpuComponentLayout &component : components) {
            if (storage.count({&var.base, component.name()})) return false;
            if (component.element_struct != nullptr) return false;
            ComponentShapes found = shapes_of(fn, &var.base,
                component.name());
            Shape shape;
            if (!found.known || !single_shape(fn, found.shapes, shape) ||
                    shape.size() != component.rank) {
                return false;
            }
            shapes.push_back(shape);
        }
        Vec<ASR::stmt_t*> bindings;
        bindings.reserve(al, components.size());
        std::map<std::string, ASR::symbol_t*> stored;
        for (size_t k = 0; k < components.size(); k++) {
            const GpuComponentLayout &component = components[k];
            const Location &loc = var.base.base.loc;
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, shapes[k].size());
            for (ASR::dimension_t &dim : shapes[k]) dims.push_back(al, dim);
            ASR::ttype_t *type = ASRUtils::make_Array_t_util(al, loc,
                ASRUtils::duplicate_type(al, component.element_type),
                dims.p, dims.size());
            ASRUtils::ASRBuilder b(al, loc);
            ASR::expr_t *array = b.Variable(&scope, scope.get_unique_name(
                "__" + std::string(var.m_name) + "_" + component.name(),
                false), type, ASR::intentType::Local);
            ASR::symbol_t *array_sym = ASR::down_cast<ASR::Var_t>(array)->m_v;
            storage[{&var.base, component.name()}] = array_sym;
            stored[component.name()] = array_sym;
            ASR::expr_t *member = ASRUtils::EXPR(
                ASR::make_StructInstanceMember_t(al, loc,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, &var.base)),
                    component.component,
                    ASRUtils::symbol_type(component.component), nullptr));
            bindings.push_back(al, ASRUtils::STMT(ASR::make_Associate_t(al,
                loc, member, array)));
        }
        rewrite_references(fn, var, stored);
        std::set<ASR::symbol_t*> visited;
        copy_components(fn.m_body, fn.n_body, var, stored, visited);
        prepend(scope, bindings);
        return true;
    }

    // The component `component` of a derived-type value: the storage of
    // `var` when the value is `var`.
    ASR::expr_t* component_of(ASR::expr_t *value, ASR::Variable_t &var,
            const std::string &component,
            const std::map<std::string, ASR::symbol_t*> &stored) {
        const Location &loc = value->base.loc;
        if (variable_of(value) == &var.base) {
            return ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                stored.at(component)));
        }
        ASR::symbol_t *st = ASRUtils::get_struct_sym_from_struct_expr(value);
        ASR::symbol_t *member = st
            ? gpu_struct_lookup_member(st, component) : nullptr;
        if (member == nullptr) return nullptr;
        return ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, loc,
            duplicate(al, value), member, ASRUtils::symbol_type(member),
            nullptr));
    }

    // A derived-type value assigned to or from `var` as a whole carries its
    // components with it, which are stored apart from the value: each is
    // copied after the assignment into the storage the target has.
    void copy_components(ASR::stmt_t **&body, size_t &n_body,
            ASR::Variable_t &var,
            const std::map<std::string, ASR::symbol_t*> &stored,
            std::set<ASR::symbol_t*> &visited) {
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n_body);
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t *s = body[i];
            out.push_back(al, s);
            switch (s->type) {
                case ASR::stmtType::If: {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    copy_components(x->m_body, x->n_body, var, stored,
                        visited);
                    copy_components(x->m_orelse, x->n_orelse, var, stored,
                        visited);
                    break;
                }
                case ASR::stmtType::DoLoop: {
                    ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                    copy_components(x->m_body, x->n_body, var, stored,
                        visited);
                    break;
                }
                case ASR::stmtType::WhileLoop: {
                    ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                    copy_components(x->m_body, x->n_body, var, stored,
                        visited);
                    break;
                }
                case ASR::stmtType::Select: {
                    ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                    for (size_t c = 0; c < x->n_body; c++) {
                        if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[c])) {
                            ASR::CaseStmt_t *t =
                                ASR::down_cast<ASR::CaseStmt_t>(x->m_body[c]);
                            copy_components(t->m_body, t->n_body, var,
                                stored, visited);
                        } else {
                            ASR::CaseStmt_Range_t *t =
                                ASR::down_cast<ASR::CaseStmt_Range_t>(
                                    x->m_body[c]);
                            copy_components(t->m_body, t->n_body, var,
                                stored, visited);
                        }
                    }
                    copy_components(x->m_default, x->n_default, var, stored,
                        visited);
                    break;
                }
                case ASR::stmtType::BlockCall:
                case ASR::stmtType::AssociateBlockCall: {
                    ASR::symbol_t *m = ASRUtils::symbol_get_past_external(
                        ASR::is_a<ASR::BlockCall_t>(*s)
                            ? ASR::down_cast<ASR::BlockCall_t>(s)->m_m
                            : ASR::down_cast<ASR::AssociateBlockCall_t>(s)
                                ->m_m);
                    if (!visited.insert(m).second) break;
                    if (ASR::is_a<ASR::Block_t>(*m)) {
                        ASR::Block_t *x = ASR::down_cast<ASR::Block_t>(m);
                        copy_components(x->m_body, x->n_body, var, stored,
                            visited);
                    } else if (ASR::is_a<ASR::AssociateBlock_t>(*m)) {
                        ASR::AssociateBlock_t *x =
                            ASR::down_cast<ASR::AssociateBlock_t>(m);
                        copy_components(x->m_body, x->n_body, var, stored,
                            visited);
                    }
                    break;
                }
                case ASR::stmtType::Assignment: {
                    ASR::Assignment_t *x = ASR::down_cast<ASR::Assignment_t>(s);
                    if (ASRUtils::is_array(ASRUtils::expr_type(x->m_target))
                            || (variable_of(x->m_target) != &var.base &&
                                variable_of(x->m_value) != &var.base)) {
                        break;
                    }
                    for (auto &c : stored) {
                        ASR::expr_t *target = component_of(x->m_target, var,
                            c.first, stored);
                        ASR::expr_t *value = component_of(x->m_value, var,
                            c.first, stored);
                        if (target == nullptr || value == nullptr) continue;
                        out.push_back(al, ASRUtils::STMT(
                            ASR::make_Assignment_t(al, s->base.loc, target,
                                value, nullptr, false, false)));
                    }
                    break;
                }
                default:
                    break;
            }
        }
        body = out.p;
        n_body = out.n;
    }

    void rewrite_references(ASR::Function_t &fn, ASR::Variable_t &var,
        const std::map<std::string, ASR::symbol_t*> &stored);

    void prepend(SymbolTable &scope, Vec<ASR::stmt_t*> &bindings) {
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
            scope.asr_owner);
        ASR::stmt_t ***body = nullptr;
        size_t *n_body = nullptr;
        if (ASR::is_a<ASR::Function_t>(*owner)) {
            ASR::Function_t *x = ASR::down_cast<ASR::Function_t>(owner);
            body = &x->m_body;
            n_body = &x->n_body;
        } else if (ASR::is_a<ASR::Block_t>(*owner)) {
            ASR::Block_t *x = ASR::down_cast<ASR::Block_t>(owner);
            body = &x->m_body;
            n_body = &x->n_body;
        } else {
            LCOMPILERS_ASSERT(ASR::is_a<ASR::AssociateBlock_t>(*owner));
            ASR::AssociateBlock_t *x =
                ASR::down_cast<ASR::AssociateBlock_t>(owner);
            body = &x->m_body;
            n_body = &x->n_body;
        }
        for (size_t i = 0; i < *n_body; i++) {
            bindings.push_back(al, (*body)[i]);
        }
        *body = bindings.p;
        *n_body = bindings.size();
    }

    bool run() {
        // A component already given storage is known by the binding the
        // routine starts with.
        for (ASR::Function_t *fn : device_routines()) {
            StorageMap bound = storage_bindings(*fn);
            storage.insert(bound.begin(), bound.end());
        }
        bool changed = false;
        for (ASR::Function_t *fn : device_routines()) {
            if (give(*fn)) changed = true;
        }
        return changed;
    }
};

void ComponentShapes::call_arguments(const Location &/*loc*/,
        ASR::symbol_t *name, ASR::call_arg_t *args, size_t n_args) {
    ASR::Function_t *callee = call_target(name);
    for (size_t i = 0; i < n_args; i++) {
        ASR::expr_t *arg = args[i].m_value;
        if (arg == nullptr) continue;
        ASR::Variable_t *dummy = dummy_of(callee, i, n_args);
        if (component_name(arg, base) == component) {
            // The component itself, which a procedure can reallocate only
            // through an allocatable dummy argument.
            if (dummy == nullptr || (!dummy_is_read_only(dummy) &&
                    ASRUtils::is_allocatable(dummy->m_type))) {
                known = false;
            }
            continue;
        }
        if (variable_of(arg) != base) continue;
        if (dummy_is_read_only(dummy)) continue;
        if (dummy == nullptr) {
            known = false;
            continue;
        }
        Shape shape;
        switch (pass.call_effect(*callee, *dummy, component, args, n_args,
                shape)) {
            case CallEffect::Shaped:
                shapes.push_back(shape);
                break;
            case CallEffect::Untouched:
                // An `intent(out)` dummy leaves the component deallocated.
                if (dummy->m_intent == ASR::intentType::Out) known = false;
                break;
            default:
                known = false;
        }
    }
}

// Replaces every reference to a component of `var` that has storage by
// that storage.
class StorageReplacer : public ASR::BaseExprReplacer<StorageReplacer> {
    using Base = ASR::BaseExprReplacer<StorageReplacer>;
    Allocator &al;
    ASR::symbol_t *var;
    const std::map<std::string, ASR::symbol_t*> &stored;

    ASR::symbol_t* storage_of(ASR::expr_t *e) {
        std::string name;
        if (!ASR::is_a<ASR::StructInstanceMember_t>(*e)) return nullptr;
        ASR::StructInstanceMember_t *m =
            ASR::down_cast<ASR::StructInstanceMember_t>(e);
        if (variable_of(m->m_v) != var) return nullptr;
        auto it = stored.find(ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(m->m_m)));
        return it == stored.end() ? nullptr : it->second;
    }

public:
    StorageReplacer(Allocator &al, ASR::symbol_t *var,
            const std::map<std::string, ASR::symbol_t*> &stored)
        : al(al), var(var), stored(stored) {}

    void replace_StructInstanceMember(ASR::StructInstanceMember_t *x) {
        if (ASR::symbol_t *s = storage_of(&x->base)) {
            *current_expr = ASRUtils::EXPR(ASR::make_Var_t(al,
                x->base.base.loc, s));
            return;
        }
        Base::replace_StructInstanceMember(x);
    }

    // A cast of the component to another representation is now one of the
    // storage, which may have that representation already.
    void replace_ArrayPhysicalCast(ASR::ArrayPhysicalCast_t *x) {
        ASR::symbol_t *s = storage_of(x->m_arg);
        Base::replace_ArrayPhysicalCast(x);
        if (s == nullptr) return;
        ASR::array_physical_typeType physical =
            ASRUtils::extract_physical_type(ASRUtils::symbol_type(s));
        x->m_old = physical;
        if (x->m_old == x->m_new) *current_expr = x->m_arg;
    }

    // Storage always exists.
    void replace_IntrinsicImpureFunction(ASR::IntrinsicImpureFunction_t *x) {
        if (x->m_impure_intrinsic_id == static_cast<int64_t>(
                ASRUtils::IntrinsicImpureFunctions::Allocated) &&
                x->n_args == 1 && storage_of(
                    ASRUtils::get_past_array_physical_cast(x->m_args[0]))) {
            *current_expr = ASRUtils::EXPR(ASR::make_LogicalConstant_t(al,
                x->base.base.loc, true, x->m_type));
            return;
        }
        Base::replace_IntrinsicImpureFunction(x);
    }
};

class StorageReferenceRewriter :
        public ASR::CallReplacerOnExpressionsVisitor<StorageReferenceRewriter> {
public:
    StorageReplacer replacer;

    StorageReferenceRewriter(Allocator &al, ASR::symbol_t *var,
            const std::map<std::string, ASR::symbol_t*> &stored)
        : replacer(al, var, stored) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    // The storage has its one shape: an assignment to it fills it.
    void visit_Assignment(const ASR::Assignment_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<StorageReferenceRewriter>
            ::visit_Assignment(x);
        if (!ASRUtils::is_allocatable(ASRUtils::expr_type(x.m_target))) {
            const_cast<ASR::Assignment_t&>(x).m_realloc_lhs = false;
        }
    }
};

void StructStorage::rewrite_references(ASR::Function_t &fn,
        ASR::Variable_t &var,
        const std::map<std::string, ASR::symbol_t*> &stored) {
    StorageReferenceRewriter rewriter(al, &var.base, stored);
    rewriter.visit_Function(fn);
}

// The allocations of the allocatable rank-one locals of a device routine,
// and the locals that something other than an allocation may reallocate.
class Allocations : public ASRUtils::BlockBodyWalkVisitor<Allocations> {
    using Base = ASRUtils::BlockBodyWalkVisitor<Allocations>;

    template <typename T>
    void allocation(const T &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (ASR::symbol_t *sym = variable_of(x.m_args[i].m_a)) {
                shapes[sym].push_back(&x.m_args[i]);
            }
        }
    }

    void call_arguments(ASR::symbol_t *name, ASR::call_arg_t *args,
            size_t n_args) {
        ASR::Function_t *callee = call_target(name);
        for (size_t i = 0; i < n_args; i++) {
            ASR::symbol_t *sym = variable_of(args[i].m_value);
            if (sym == nullptr) continue;
            ASR::Variable_t *dummy = dummy_of(callee, i, n_args);
            if (dummy == nullptr || ASRUtils::is_allocatable(dummy->m_type)
                    || ASRUtils::is_pointer(dummy->m_type)) {
                reallocated.insert(sym);
            }
        }
    }

public:
    std::map<ASR::symbol_t*, std::vector<const ASR::alloc_arg_t*>> shapes;
    std::set<ASR::symbol_t*> reallocated;

    void visit_Allocate(const ASR::Allocate_t &x) { allocation(x); }
    void visit_ReAlloc(const ASR::ReAlloc_t &x) { allocation(x); }

    void visit_Assignment(const ASR::Assignment_t &x) {
        if (x.m_realloc_lhs || x.m_move_allocation) {
            if (ASR::symbol_t *sym = variable_of(x.m_target)) {
                reallocated.insert(sym);
            }
            if (ASR::symbol_t *sym = variable_of(x.m_value)) {
                reallocated.insert(sym);
            }
        }
        Base::visit_Assignment(x);
    }

    void visit_Associate(const ASR::Associate_t &x) {
        if (ASR::symbol_t *sym = variable_of(x.m_target)) {
            reallocated.insert(sym);
        }
        Base::visit_Associate(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        call_arguments(x.m_name, x.m_args, x.n_args);
        Base::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        call_arguments(x.m_name, x.m_args, x.n_args);
        Base::visit_FunctionCall(x);
    }

    void visit_IntrinsicImpureSubroutine(
            const ASR::IntrinsicImpureSubroutine_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (ASR::symbol_t *sym = variable_of(x.m_args[i])) {
                reallocated.insert(sym);
            }
        }
        Base::visit_IntrinsicImpureSubroutine(x);
    }

    void visit_IntrinsicImpureFunction(
            const ASR::IntrinsicImpureFunction_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (ASR::symbol_t *sym = variable_of(x.m_args[i])) {
                reallocated.insert(sym);
            }
        }
        Base::visit_IntrinsicImpureFunction(x);
    }
};

// Gives an allocatable rank-one local of a device routine, that the
// routine allocates with a shape it only knows where it allocates it --
// inside a loop, with an extent the loop index picks -- a buffer of an
// upper bound of that shape known on entry, and makes the local a pointer
// to the leading part of the buffer the allocation asks for.
class AllocationBuffers {
    Allocator &al;
    StorageMap no_storage;

    // The allocation `x` becomes when it allocates `var`: an association
    // of `var` with the leading part of `buffer` it has the extent of.
    ASR::stmt_t* association(const Location &loc, ASR::symbol_t *var,
            ASR::symbol_t *buffer, ASR::expr_t *length) {
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *index_type = ASRUtils::TYPE(ASR::make_Integer_t(al,
            loc, 4));
        Vec<ASR::array_index_t> range;
        range.reserve(al, 1);
        ASR::array_index_t index;
        index.loc = loc;
        index.m_left = b.i_t(1, index_type);
        index.m_right = length;
        index.m_step = b.i_t(1, index_type);
        range.push_back(al, index);
        ASR::ttype_t *element = ASRUtils::duplicate_type(al,
            ASRUtils::extract_type(ASRUtils::symbol_type(buffer)));
        Vec<ASR::dimension_t> unknown;
        unknown.reserve(al, 1);
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = nullptr;
        dim.m_length = nullptr;
        unknown.push_back(al, dim);
        ASR::ttype_t *section_type = ASRUtils::TYPE(ASR::make_Array_t(al, loc,
            element, unknown.p, unknown.n,
            ASR::array_physical_typeType::DescriptorArray,
            ASR::memory_spaceType::Global));
        ASR::expr_t *section = ASRUtils::EXPR(ASR::make_ArraySection_t(al,
            loc, ASRUtils::EXPR(ASR::make_Var_t(al, loc, buffer)), range.p,
            range.n, section_type, nullptr));
        return ASRUtils::STMT(ASR::make_Associate_t(al, loc,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, var)), section));
    }

    void transform(ASR::stmt_t **&body, size_t &n_body,
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &buffers,
            std::set<ASR::symbol_t*> &visited) {
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n_body);
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t *s = body[i];
            switch (s->type) {
                case ASR::stmtType::If: {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    transform(x->m_body, x->n_body, buffers, visited);
                    transform(x->m_orelse, x->n_orelse, buffers, visited);
                    break;
                }
                case ASR::stmtType::DoLoop: {
                    ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                    transform(x->m_body, x->n_body, buffers, visited);
                    break;
                }
                case ASR::stmtType::WhileLoop: {
                    ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                    transform(x->m_body, x->n_body, buffers, visited);
                    break;
                }
                case ASR::stmtType::Select: {
                    ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                    for (size_t c = 0; c < x->n_body; c++) {
                        if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[c])) {
                            ASR::CaseStmt_t *t =
                                ASR::down_cast<ASR::CaseStmt_t>(x->m_body[c]);
                            transform(t->m_body, t->n_body, buffers, visited);
                        } else {
                            ASR::CaseStmt_Range_t *t =
                                ASR::down_cast<ASR::CaseStmt_Range_t>(
                                    x->m_body[c]);
                            transform(t->m_body, t->n_body, buffers, visited);
                        }
                    }
                    transform(x->m_default, x->n_default, buffers, visited);
                    break;
                }
                case ASR::stmtType::BlockCall:
                case ASR::stmtType::AssociateBlockCall: {
                    ASR::symbol_t *m = ASRUtils::symbol_get_past_external(
                        ASR::is_a<ASR::BlockCall_t>(*s)
                            ? ASR::down_cast<ASR::BlockCall_t>(s)->m_m
                            : ASR::down_cast<ASR::AssociateBlockCall_t>(s)
                                ->m_m);
                    if (!visited.insert(m).second) break;
                    if (ASR::is_a<ASR::Block_t>(*m)) {
                        ASR::Block_t *x = ASR::down_cast<ASR::Block_t>(m);
                        transform(x->m_body, x->n_body, buffers, visited);
                    } else if (ASR::is_a<ASR::AssociateBlock_t>(*m)) {
                        ASR::AssociateBlock_t *x =
                            ASR::down_cast<ASR::AssociateBlock_t>(m);
                        transform(x->m_body, x->n_body, buffers, visited);
                    }
                    break;
                }
                case ASR::stmtType::Allocate:
                case ASR::stmtType::ReAlloc: {
                    ASR::alloc_arg_t *args = nullptr;
                    size_t n_args = 0;
                    if (ASR::is_a<ASR::Allocate_t>(*s)) {
                        args = ASR::down_cast<ASR::Allocate_t>(s)->m_args;
                        n_args = ASR::down_cast<ASR::Allocate_t>(s)->n_args;
                    } else {
                        args = ASR::down_cast<ASR::ReAlloc_t>(s)->m_args;
                        n_args = ASR::down_cast<ASR::ReAlloc_t>(s)->n_args;
                    }
                    Vec<ASR::alloc_arg_t> kept;
                    kept.reserve(al, n_args);
                    std::vector<ASR::stmt_t*> associations;
                    for (size_t k = 0; k < n_args; k++) {
                        ASR::symbol_t *var = variable_of(args[k].m_a);
                        auto it = buffers.find(var);
                        if (it == buffers.end()) {
                            kept.push_back(al, args[k]);
                            continue;
                        }
                        associations.push_back(association(s->base.loc, var,
                            it->second, args[k].m_dims[0].m_length));
                    }
                    if (associations.empty()) break;
                    if (kept.size() > 0) {
                        if (ASR::is_a<ASR::Allocate_t>(*s)) {
                            ASR::down_cast<ASR::Allocate_t>(s)->m_args = kept.p;
                            ASR::down_cast<ASR::Allocate_t>(s)->n_args = kept.n;
                        } else {
                            ASR::down_cast<ASR::ReAlloc_t>(s)->m_args = kept.p;
                            ASR::down_cast<ASR::ReAlloc_t>(s)->n_args = kept.n;
                        }
                        out.push_back(al, s);
                    }
                    for (ASR::stmt_t *a : associations) out.push_back(al, a);
                    continue;
                }
                default:
                    break;
            }
            out.push_back(al, s);
        }
        body = out.p;
        n_body = out.n;
    }

    // Whether an allocation's lower bound is 1, as the leading part of the
    // buffer the local points to starts at 1: a reallocation to the shape
    // of a section, say, has the lower bound of the section.
    bool starts_at_one(ASR::Function_t &fn, ASR::expr_t *start) {
        if (start == nullptr) return true;
        int64_t value = 0;
        if (ASRUtils::extract_value(ASRUtils::expr_value(start), value)) {
            return value == 1;
        }
        EntryValueWriter writer(al, fn, no_storage);
        ASR::expr_t *written = writer.write(start);
        return written != nullptr && ASRUtils::extract_value(
            ASRUtils::expr_value(written), value) && value == 1;
    }

public:
    explicit AllocationBuffers(Allocator &al) : al(al) {}

    bool give(ASR::Function_t &fn) {
        Allocations found;
        for (size_t i = 0; i < fn.n_body; i++) {
            found.visit_stmt(*fn.m_body[i]);
        }
        std::map<ASR::symbol_t*, ASR::symbol_t*> buffers;
        for (auto &item : found.shapes) {
            ASR::symbol_t *sym = item.first;
            if (found.reallocated.count(sym) ||
                    !ASR::is_a<ASR::Variable_t>(*sym)) {
                continue;
            }
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
            if (var->m_intent != ASR::intentType::Local ||
                    !ASRUtils::is_allocatable(var->m_type) ||
                    ASRUtils::extract_n_dims_from_ttype(var->m_type) != 1) {
                continue;
            }
            ASR::ttype_t *element = ASRUtils::extract_type(var->m_type);
            if (!ASRUtils::is_integer(*element) &&
                    !ASRUtils::is_real(*element) &&
                    !ASRUtils::is_logical(*element) &&
                    !ASRUtils::is_complex(*element)) {
                continue;
            }
            // The upper bound each allocation gives, on entry.
            ASR::expr_t *bound = nullptr;
            bool ok = true;
            for (const ASR::alloc_arg_t *shape : item.second) {
                if (shape->n_dims != 1 || shape->m_dims[0].m_length == nullptr
                        || !starts_at_one(fn, shape->m_dims[0].m_start)) {
                    ok = false;
                    break;
                }
                EntryValueWriter writer(al, fn, no_storage);
                writer.bounding = true;
                ASR::expr_t *one = writer.write(shape->m_dims[0].m_length);
                if (one == nullptr) {
                    ok = false;
                    break;
                }
                if (bound == nullptr ||
                        PassUtils::same_expression(bound, one)) {
                    bound = bound ? bound : one;
                    continue;
                }
                ASRUtils::ASRBuilder b(al, one->base.loc);
                bound = ASRUtils::EXPR(ASR::make_IfExp_t(al, one->base.loc,
                    b.Gt(bound, one), duplicate(al, bound), duplicate(al, one),
                    ASRUtils::expr_type(one), nullptr));
            }
            if (!ok || bound == nullptr) continue;
            const Location &loc = var->base.base.loc;
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, 1);
            ASR::dimension_t dim;
            dim.loc = loc;
            dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                1, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)),
                ASR::integerbozType::Decimal));
            dim.m_length = bound;
            dims.push_back(al, dim);
            ASRUtils::ASRBuilder b(al, loc);
            ASR::expr_t *buffer = b.Variable(fn.m_symtab,
                fn.m_symtab->get_unique_name("__gpu_buffer_" +
                    std::string(var->m_name), false),
                ASRUtils::make_Array_t_util(al, loc,
                    ASRUtils::duplicate_type(al, element), dims.p, dims.n),
                ASR::intentType::Local);
            Vec<ASR::dimension_t> unknown;
            unknown.reserve(al, 1);
            dim.m_start = nullptr;
            dim.m_length = nullptr;
            unknown.push_back(al, dim);
            var->m_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                ASRUtils::TYPE(ASR::make_Array_t(al, loc,
                    ASRUtils::duplicate_type(al, element), unknown.p,
                    unknown.n, ASR::array_physical_typeType::DescriptorArray,
                    ASR::memory_spaceType::Global))));
            buffers[sym] = ASR::down_cast<ASR::Var_t>(buffer)->m_v;
        }
        if (buffers.empty()) return false;
        std::set<ASR::symbol_t*> visited;
        transform(fn.m_body, fn.n_body, buffers, visited);
        return true;
    }
};

class LocalReferences : public ASRUtils::BlockBodyWalkVisitor<LocalReferences> {
public:
    std::set<ASR::symbol_t*> referenced;

    void visit_Var(const ASR::Var_t &x) {
        referenced.insert(ASRUtils::symbol_get_past_external(x.m_v));
    }
};

} // namespace

bool gpu_bind_allocations_to_buffers(Allocator &al,
        ASR::TranslationUnit_t &unit) {
    std::vector<ASR::Function_t*> routines;
    std::vector<SymbolTable*> work = {unit.m_symtab};
    while (!work.empty()) {
        SymbolTable *scope = work.back();
        work.pop_back();
        for (auto &item : scope->get_scope()) {
            ASR::symbol_t *sym = item.second;
            if (ASR::is_a<ASR::Module_t>(*sym)) {
                work.push_back(ASR::down_cast<ASR::Module_t>(sym)->m_symtab);
            } else if (ASR::is_a<ASR::Program_t>(*sym)) {
                work.push_back(ASR::down_cast<ASR::Program_t>(sym)->m_symtab);
            } else if (ASR::is_a<ASR::Function_t>(*sym)) {
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
                if (ASRUtils::runs_on_device(*fn) &&
                        !ASRUtils::is_device_kernel(sym)) {
                    routines.push_back(fn);
                }
                work.push_back(fn->m_symtab);
            }
        }
    }
    AllocationBuffers buffers(al);
    bool changed = false;
    for (ASR::Function_t *fn : routines) {
        if (buffers.give(*fn)) changed = true;
    }
    return changed;
}

bool gpu_give_struct_component_storage(Allocator &al,
        ASR::TranslationUnit_t &unit) {
    StructStorage storage(al, unit);
    return storage.run();
}

bool gpu_write_extents_at_entry(Allocator &al, ASR::Function_t &fn,
        ASR::dimension_t *dims, size_t n_dims) {
    StorageMap stored = storage_bindings(fn);
    EntryValueWriter writer(al, fn, stored);
    std::vector<ASR::dimension_t> written;
    for (size_t d = 0; d < n_dims; d++) {
        ASR::dimension_t dim = dims[d];
        if (dim.m_start) dim.m_start = writer.write(dim.m_start);
        if (dim.m_length) dim.m_length = writer.write(dim.m_length);
        if (!writer.valid) return false;
        written.push_back(dim);
    }
    for (size_t d = 0; d < n_dims; d++) dims[d] = written[d];
    return true;
}

ASR::Variable_t* gpu_struct_local_without_storage(ASR::Function_t &fn,
        std::string &component) {
    LocalReferences references;
    for (size_t i = 0; i < fn.n_body; i++) {
        references.visit_stmt(*fn.m_body[i]);
    }
    std::vector<SymbolTable*> scopes;
    gpu_routine_scopes(fn.m_symtab, scopes);
    StorageMap stored = storage_bindings(fn);
    for (SymbolTable *scope : scopes) {
        for (auto &item : scope->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(item.second);
            if (!is_struct_local(var) ||
                    !references.referenced.count(&var->base)) {
                continue;
            }
            for (const GpuComponentLayout &c :
                    gpu_decomposed_components(struct_of(var))) {
                if (!stored.count({&var->base, c.name()})) {
                    component = c.name();
                    return var;
                }
            }
        }
    }
    return nullptr;
}

} // namespace LCompilers

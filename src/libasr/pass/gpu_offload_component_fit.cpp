#include <cstdint>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/parallel_canonicalize.h>

namespace LCompilers {

namespace {

ASR::expr_t* duplicate(Allocator &al, ASR::expr_t *e) {
    ASRUtils::ExprStmtDuplicator duplicator(al);
    return duplicator.duplicate_expr(e);
}

ASR::ttype_t* integer_type(Allocator &al, const Location &loc, int kind) {
    return ASRUtils::TYPE(ASR::make_Integer_t(al, loc, kind));
}

// The variable a designator -- a variable, or an element, a section or a
// component of one -- is rooted at, or nullptr for any other expression.
ASR::symbol_t* designator_root(ASR::expr_t *e) {
    while (e) {
        if (ASR::is_a<ASR::Var_t>(*e)) {
            return ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v);
        } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
            e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
        } else if (ASR::is_a<ASR::ArraySection_t>(*e)) {
            e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
        } else if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
            e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
        } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
            e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
        } else {
            return nullptr;
        }
    }
    return nullptr;
}

// A key for the array a variable or a chain of its components names: two
// such designators have the same key exactly when they name the same array.
// False for any other expression.
bool array_key(ASR::expr_t *e, std::string &key) {
    key.clear();
    while (e) {
        if (ASR::is_a<ASR::Var_t>(*e)) {
            key = std::to_string((uintptr_t)ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v)) + key;
            return true;
        } else if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
            ASR::StructInstanceMember_t *m =
                ASR::down_cast<ASR::StructInstanceMember_t>(e);
            key = "%" + std::string(ASRUtils::symbol_name(
                ASRUtils::symbol_get_past_external(m->m_m))) + key;
            e = m->m_v;
        } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
            e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
        } else {
            return false;
        }
    }
    return false;
}

// How the source names a designator, for a message.
std::string designator_name(ASR::expr_t *e) {
    if (ASR::is_a<ASR::Var_t>(*e)) {
        return ASRUtils::symbol_name(ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(e)->m_v));
    } else if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
        ASR::StructInstanceMember_t *m =
            ASR::down_cast<ASR::StructInstanceMember_t>(e);
        return designator_name(m->m_v) + "%" + ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(m->m_m));
    } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
        ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(e);
        std::string name = designator_name(item->m_v) + "(";
        for (size_t i = 0; i < item->n_args; i++) {
            ASR::expr_t *index = item->m_args[i].m_right;
            if (i > 0) name += ", ";
            if (index && (ASR::is_a<ASR::Var_t>(*index) ||
                    ASR::is_a<ASR::StructInstanceMember_t>(*index))) {
                name += designator_name(index);
            } else if (index && ASR::is_a<ASR::IntegerConstant_t>(*index)) {
                name += std::to_string(
                    ASR::down_cast<ASR::IntegerConstant_t>(index)->m_n);
            } else {
                name += "...";
            }
        }
        return name + ")";
    } else if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*e)) {
        return designator_name(
            ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg);
    }
    return "...";
}

bool is_allocatable_array(ASR::symbol_t *component) {
    component = ASRUtils::symbol_get_past_external(component);
    if (!component || !ASR::is_a<ASR::Variable_t>(*component)) return false;
    ASR::ttype_t *type = ASR::down_cast<ASR::Variable_t>(component)->m_type;
    return ASRUtils::is_allocatable(type) && ASRUtils::is_array(type);
}

// An element of an array of derived type, picked by one subscript per
// dimension.
ASR::ArrayItem_t* struct_element(ASR::expr_t *e) {
    if (!ASR::is_a<ASR::ArrayItem_t>(*e)) return nullptr;
    ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(e);
    for (size_t i = 0; i < item->n_args; i++) {
        if (!item->m_args[i].m_right || item->m_args[i].m_left ||
                item->m_args[i].m_step) {
            return nullptr;
        }
    }
    if (!ASR::is_a<ASR::StructType_t>(*ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(e)))) {
        return nullptr;
    }
    return item;
}

// The components of a derived type, those of the types it extends first:
// the order a structure constructor takes them in.
void struct_components(ASR::Struct_t *st, std::vector<ASR::symbol_t*> &out) {
    if (st->m_parent) {
        ASR::symbol_t *parent = ASRUtils::symbol_get_past_external(
            st->m_parent);
        if (ASR::is_a<ASR::Struct_t>(*parent)) {
            struct_components(ASR::down_cast<ASR::Struct_t>(parent), out);
        }
    }
    for (size_t i = 0; i < st->n_members; i++) {
        out.push_back(st->m_symtab->get_symbol(st->m_members[i]));
    }
}

// The extents of an array value, one expression per dimension.
std::vector<ASR::expr_t*> value_extents(Allocator &al, ASR::expr_t *value) {
    value = ASRUtils::get_past_array_physical_cast(value);
    ASRUtils::ASRBuilder b(al, value->base.loc);
    size_t rank = ASRUtils::extract_n_dims_from_ttype(
        ASRUtils::expr_type(value));
    std::vector<ASR::expr_t*> extents;
    for (size_t d = 0; d < rank; d++) {
        extents.push_back(b.ArraySize(duplicate(al, value),
            rank > 1 ? b.i32((int)d + 1) : nullptr,
            integer_type(al, value->base.loc, 4)));
    }
    return extents;
}

// Whether statements can stop running before their end other than by
// stopping the program: a return, a go to, or, when they are the body of a
// loop, an exit or a cycle of that loop.
class ControlTransferFinder :
        public ASRUtils::BlockBodyWalkVisitor<ControlTransferFinder> {
public:
    bool found = false;
    bool loop_body = false;
    // How many loops of the statements themselves the walk is in.
    size_t loops = 0;

    void visit_Return(const ASR::Return_t &/*x*/) { found = true; }

    void visit_GoTo(const ASR::GoTo_t &/*x*/) { found = true; }

    void visit_Exit(const ASR::Exit_t &x) {
        if (loop_body && (loops == 0 || x.m_stmt_name)) found = true;
    }

    void visit_Cycle(const ASR::Cycle_t &x) {
        if (loop_body && (loops == 0 || x.m_stmt_name)) found = true;
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        loops++;
        ASRUtils::BlockBodyWalkVisitor<ControlTransferFinder>::visit_DoLoop(x);
        loops--;
    }

    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        loops++;
        ASRUtils::BlockBodyWalkVisitor<ControlTransferFinder>::visit_WhileLoop(
            x);
        loops--;
    }
};

bool transfers_control(ASR::stmt_t **body, size_t n_body, bool loop_body) {
    ControlTransferFinder finder;
    finder.loop_body = loop_body;
    for (size_t i = 0; i < n_body && !finder.found; i++) {
        finder.visit_stmt(*body[i]);
    }
    return finder.found;
}

// Whether assigning `value` to an allocatable array gives it the shape of
// `value`: a scalar, broadcast or not, fills the storage the array has.
bool shapes_array(ASR::expr_t *value) {
    return ASRUtils::is_array(ASRUtils::expr_type(value)) &&
        !ASR::is_a<ASR::ArrayBroadcast_t>(*value);
}

// Whether `e` is the component `component` of the variable `r`.
bool names_component(ASR::expr_t *e, ASR::symbol_t *r,
        const std::string &component) {
    e = ASRUtils::get_past_array_physical_cast(e);
    if (!ASR::is_a<ASR::StructInstanceMember_t>(*e)) return false;
    ASR::StructInstanceMember_t *m =
        ASR::down_cast<ASR::StructInstanceMember_t>(e);
    ASR::expr_t *base = ASRUtils::get_past_array_physical_cast(m->m_v);
    return ASR::is_a<ASR::Var_t>(*base) &&
        ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(base)->m_v) == r &&
        component == ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(m->m_m));
}

bool names_variable(ASR::expr_t *e, ASR::symbol_t *r) {
    e = ASRUtils::get_past_array_physical_cast(e);
    return ASR::is_a<ASR::Var_t>(*e) && ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(e)->m_v) == r;
}

// The value a structure constructor gives the component `component`, or
// nullptr.
ASR::expr_t* constructor_value(ASR::StructConstructor_t *x,
        const std::string &component) {
    ASR::symbol_t *type = ASRUtils::symbol_get_past_external(x->m_dt_sym);
    if (!type || !ASR::is_a<ASR::Struct_t>(*type)) return nullptr;
    std::vector<ASR::symbol_t*> components;
    struct_components(ASR::down_cast<ASR::Struct_t>(type), components);
    if (components.size() != x->n_args) return nullptr;
    for (size_t i = 0; i < x->n_args; i++) {
        if (components[i] && component == ASRUtils::symbol_name(
                components[i])) {
            return x->m_args[i].m_value;
        }
    }
    return nullptr;
}

// Whether a statement gives the component `component` of `r` a shape: an
// allocate of it, or an assignment of an array to it or of a structure
// constructor to `r`. `extents` are the extents it gives, empty when they
// cannot be told.
bool gives_shape(Allocator &al, ASR::stmt_t *stmt, ASR::symbol_t *r,
        const std::string &component, std::vector<ASR::expr_t*> &extents) {
    extents.clear();
    if (ASR::is_a<ASR::Allocate_t>(*stmt)) {
        ASR::Allocate_t *x = ASR::down_cast<ASR::Allocate_t>(stmt);
        for (size_t i = 0; i < x->n_args; i++) {
            if (!names_component(x->m_args[i].m_a, r, component)) continue;
            for (size_t d = 0; d < x->m_args[i].n_dims; d++) {
                if (!x->m_args[i].m_dims[d].m_length) {
                    extents.clear();
                    break;
                }
                extents.push_back(x->m_args[i].m_dims[d].m_length);
            }
            return true;
        }
    } else if (ASR::is_a<ASR::Assignment_t>(*stmt)) {
        ASR::Assignment_t *x = ASR::down_cast<ASR::Assignment_t>(stmt);
        ASR::expr_t *value = nullptr;
        if (names_component(x->m_target, r, component)) {
            value = x->m_value;
        } else if (names_variable(x->m_target, r) &&
                ASR::is_a<ASR::StructConstructor_t>(*x->m_value)) {
            value = constructor_value(
                ASR::down_cast<ASR::StructConstructor_t>(x->m_value),
                component);
            if (!value) return false;
        } else {
            return false;
        }
        if (!shapes_array(value)) return false;
        extents = value_extents(al, value);
        return true;
    }
    return false;
}

// How many statements of a routine give the component `component` of `r` a
// shape, and whether anything else may change it as a whole.
class ComponentShapeCounter :
        public ASRUtils::BlockBodyWalkVisitor<ComponentShapeCounter> {
    using Base = ASRUtils::BlockBodyWalkVisitor<ComponentShapeCounter>;
    Allocator &al;
    ASR::symbol_t *r;
    const std::string &component;

    bool names(ASR::expr_t *e) {
        return e && (names_variable(e, r) || names_component(e, r, component));
    }

    void count_or_poison(ASR::Assignment_t *x) {
        std::vector<ASR::expr_t*> extents;
        if (gives_shape(al, &x->base, r, component, extents)) {
            shapes++;
        } else if (names_variable(x->m_target, r) ||
                (names_component(x->m_target, r, component) &&
                    shapes_array(x->m_value))) {
            // A scalar assigned to the component fills the storage it has.
            poisoned = true;
        }
    }

    void args_poison(ASR::call_arg_t *args, size_t n_args) {
        for (size_t i = 0; i < n_args; i++) {
            if (names(args[i].m_value)) poisoned = true;
        }
    }

public:
    size_t shapes = 0;
    bool poisoned = false;

    ComponentShapeCounter(Allocator &al, ASR::symbol_t *r,
            const std::string &component)
        : al(al), r(r), component(component) {}

    void visit_Allocate(const ASR::Allocate_t &x) {
        std::vector<ASR::expr_t*> extents;
        if (gives_shape(al, (ASR::stmt_t*)&x, r, component, extents)) {
            shapes++;
        } else {
            for (size_t i = 0; i < x.n_args; i++) {
                if (names_variable(x.m_args[i].m_a, r)) poisoned = true;
            }
        }
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        count_or_poison((ASR::Assignment_t*)&x);
        Base::visit_Assignment(x);
    }

    void visit_Associate(const ASR::Associate_t &x) {
        if (names(x.m_value)) poisoned = true;
    }

    void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t &x) {
        for (size_t i = 0; i < x.n_vars; i++) {
            if (names(x.m_vars[i])) poisoned = true;
        }
    }

    void visit_ImplicitDeallocate(const ASR::ImplicitDeallocate_t &x) {
        for (size_t i = 0; i < x.n_vars; i++) {
            if (names(x.m_vars[i])) poisoned = true;
        }
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        args_poison(x.m_args, x.n_args);
        Base::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        args_poison(x.m_args, x.n_args);
        Base::visit_FunctionCall(x);
    }

    void visit_IntrinsicImpureSubroutine(
            const ASR::IntrinsicImpureSubroutine_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (names(x.m_args[i])) poisoned = true;
        }
    }
};

// Whether every way through `body` that runs to its end gives the
// component a shape.
bool shaped_on_every_path(Allocator &al, ASR::stmt_t **body, size_t n_body,
        ASR::symbol_t *r, const std::string &component) {
    for (size_t i = 0; i < n_body; i++) {
        std::vector<ASR::expr_t*> extents;
        if (gives_shape(al, body[i], r, component, extents)) return true;
        if (ASR::is_a<ASR::If_t>(*body[i])) {
            ASR::If_t *x = ASR::down_cast<ASR::If_t>(body[i]);
            if (shaped_on_every_path(al, x->m_body, x->n_body, r, component) &&
                    shaped_on_every_path(al, x->m_orelse, x->n_orelse, r,
                        component)) {
                return true;
            }
        }
    }
    return false;
}

// The shape a write gives an allocatable array component.
struct ComponentShape {
    // Whether the write gives the component a shape at all.
    bool shaped = false;
    // Whether it can give it only the one shape `extents` tell.
    bool single = false;
    // Whether every run of the write gives it a shape.
    bool always = false;
    // The extents, empty when they cannot be told.
    std::vector<ASR::expr_t*> extents;
};

// The shape a routine gives the component `component` of its variable `r`:
// a single one when only statements of its own body shape it, which run
// one after the other, and it runs to its end, so the last one decides.
ComponentShape routine_component_shape(Allocator &al,
        const ASR::Function_t &fn, ASR::symbol_t *r,
        const std::string &component) {
    ComponentShape shape;
    ComponentShapeCounter counter(al, r, component);
    for (size_t i = 0; i < fn.n_body; i++) counter.visit_stmt(*fn.m_body[i]);
    if (counter.shapes == 0) return shape;
    shape.shaped = true;
    bool transfers = transfers_control(fn.m_body, fn.n_body, false);
    size_t top_level = 0;
    std::vector<ASR::expr_t*> last;
    for (size_t i = 0; i < fn.n_body; i++) {
        std::vector<ASR::expr_t*> extents;
        if (gives_shape(al, fn.m_body[i], r, component, extents)) {
            top_level++;
            last = extents;
        }
    }
    shape.single = !counter.poisoned && !transfers &&
        top_level == counter.shapes;
    shape.always = !counter.poisoned && !transfers &&
        shaped_on_every_path(al, fn.m_body, fn.n_body, r, component);
    if (shape.single) shape.extents = last;
    return shape;
}

// Rewrites an expression over the dummy arguments of a procedure into one
// over the actual arguments of a call to it. Anything else of the procedure
// has no counterpart where the call is.
class DummyArgumentBinder :
        public ASR::BaseExprReplacer<DummyArgumentBinder> {
    using Base = ASR::BaseExprReplacer<DummyArgumentBinder>;
    Allocator &al;
    const ASR::Function_t &fn;
    ASR::call_arg_t *args;
    size_t n_args;

    // The dummy argument `e` names, with its position.
    ASR::Variable_t* dummy(ASR::expr_t *e, size_t &position) {
        e = ASRUtils::get_past_array_physical_cast(e);
        if (!ASR::is_a<ASR::Var_t>(*e)) return nullptr;
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(e)->m_v);
        for (size_t i = 0; i < fn.n_args; i++) {
            if (ASR::is_a<ASR::Var_t>(*fn.m_args[i]) &&
                    ASRUtils::symbol_get_past_external(ASR::down_cast<
                        ASR::Var_t>(fn.m_args[i])->m_v) == sym &&
                    ASR::is_a<ASR::Variable_t>(*sym)) {
                position = i;
                return ASR::down_cast<ASR::Variable_t>(sym);
            }
        }
        return nullptr;
    }

    ASR::expr_t* as_type(ASR::expr_t *e, ASR::ttype_t *type) {
        if (ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(e)) ==
                ASRUtils::extract_kind_from_ttype_t(type)) {
            return e;
        }
        ASRUtils::ASRBuilder b(al, e->base.loc);
        return b.i2i_t(e, type);
    }

    // The element count of the ranges of a section of an explicit-shape or
    // assumed-shape dummy whose bounds it names: the one `dim` picks, or all
    // of them. The actual argument need not have the dummy's bounds, so no
    // section of the actual is formed.
    ASR::expr_t* section_count(ASR::ArraySection_t *section, ASR::expr_t *dim,
            ASR::ttype_t *type) {
        int64_t picked = 0;
        if (dim && (!ASRUtils::expr_value(dim) ||
                !ASRUtils::extract_value(ASRUtils::expr_value(dim), picked))) {
            return nullptr;
        }
        ASRUtils::ASRBuilder b(al, section->base.base.loc);
        ASR::expr_t *count = nullptr;
        int64_t range = 0;
        for (size_t i = 0; i < section->n_args; i++) {
            ASR::array_index_t &index = section->m_args[i];
            if (!index.m_step) continue;
            range++;
            if (dim && range != picked) continue;
            if (!index.m_left || !index.m_right) return nullptr;
            ASR::expr_t *lo = as_type(duplicate(al, index.m_left), type);
            ASR::expr_t *hi = as_type(duplicate(al, index.m_right), type);
            ASR::expr_t *step = as_type(duplicate(al, index.m_step), type);
            ASR::expr_t *one = b.Max(b.Div(b.Add(b.Sub(hi, lo), step), step),
                b.i_t(0, type));
            count = count ? b.Mul(count, one) : one;
        }
        return count;
    }

public:
    bool valid = true;

    DummyArgumentBinder(Allocator &al, const ASR::Function_t &fn,
            ASR::call_arg_t *args, size_t n_args)
        : al(al), fn(fn), args(args), n_args(n_args) {}

    void replace_Var(ASR::Var_t *x) {
        size_t position = 0;
        if (dummy(&x->base, position)) {
            ASR::expr_t *actual = position < n_args
                ? args[position].m_value : nullptr;
            if (actual) {
                // The cast a call makes of its actual argument belongs to
                // the call.
                *current_expr = duplicate(al,
                    ASRUtils::get_past_array_physical_cast(actual));
            } else {
                valid = false;
            }
            return;
        }
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x->m_v);
        if (ASR::is_a<ASR::Variable_t>(*sym) &&
                ASRUtils::symbol_parent_symtab(sym) == fn.m_symtab) {
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
            if (var->m_storage == ASR::storage_typeType::Parameter &&
                    var->m_value) {
                *current_expr = duplicate(al, var->m_value);
                return;
            }
        }
        valid = false;
    }

    void replace_ArraySize(ASR::ArraySize_t *x) {
        size_t position = 0;
        ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(x->m_v);
        if (ASR::is_a<ASR::ArraySection_t>(*v) && dummy(
                ASR::down_cast<ASR::ArraySection_t>(v)->m_v, position)) {
            ASR::expr_t *count = section_count(
                ASR::down_cast<ASR::ArraySection_t>(v), x->m_dim, x->m_type);
            if (!count) {
                valid = false;
                return;
            }
            *current_expr = count;
            replace_expr(*current_expr);
            return;
        }
        ASR::Variable_t *d = dummy(x->m_v, position);
        if (!d) {
            Base::replace_ArraySize(x);
            return;
        }
        ASR::dimension_t *dims = nullptr;
        size_t rank = ASRUtils::extract_dimensions_from_ttype(d->m_type, dims);
        bool declared = rank > 0;
        for (size_t k = 0; k < rank; k++) {
            if (!dims[k].m_length) declared = false;
        }
        if (!declared) {
            // An assumed-shape dummy has the shape of the actual argument.
            Base::replace_ArraySize(x);
            return;
        }
        // An explicit-shape dummy has the extents it declares.
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        ASR::expr_t *count = nullptr;
        int64_t picked = 0;
        if (x->m_dim) {
            if (!ASRUtils::expr_value(x->m_dim) || !ASRUtils::extract_value(
                    ASRUtils::expr_value(x->m_dim), picked) || picked < 1 ||
                    picked > (int64_t)rank) {
                valid = false;
                return;
            }
            count = as_type(duplicate(al, dims[picked - 1].m_length),
                x->m_type);
        } else {
            for (size_t k = 0; k < rank; k++) {
                ASR::expr_t *one = as_type(duplicate(al, dims[k].m_length),
                    x->m_type);
                count = count ? b.Mul(count, one) : one;
            }
        }
        *current_expr = count;
        replace_expr(*current_expr);
    }

    // A bound, an element or a section of a dummy array need not be the
    // same of the actual argument.
    void replace_ArrayBound(ASR::ArrayBound_t *x) {
        size_t position = 0;
        if (dummy(x->m_v, position)) {
            valid = false;
            return;
        }
        Base::replace_ArrayBound(x);
    }

    void replace_ArrayItem(ASR::ArrayItem_t *x) {
        size_t position = 0;
        if (dummy(x->m_v, position)) {
            valid = false;
            return;
        }
        Base::replace_ArrayItem(x);
    }

    void replace_ArraySection(ASR::ArraySection_t *x) {
        size_t position = 0;
        if (dummy(x->m_v, position)) {
            valid = false;
            return;
        }
        Base::replace_ArraySection(x);
    }

    // The host does not call a procedure to size storage.
    void replace_FunctionCall(ASR::FunctionCall_t */*x*/) {
        valid = false;
    }
};

// Replaces variables by other expressions.
class VariableSubstituter :
        public ASR::BaseExprReplacer<VariableSubstituter> {
    Allocator &al;
    const std::map<ASR::symbol_t*, ASR::expr_t*> &values;
public:
    VariableSubstituter(Allocator &al,
            const std::map<ASR::symbol_t*, ASR::expr_t*> &values)
        : al(al), values(values) {}

    void replace_Var(ASR::Var_t *x) {
        auto value = values.find(ASRUtils::symbol_get_past_external(x->m_v));
        if (value != values.end()) {
            *current_expr = duplicate(al, value->second);
        }
    }
};

// The variables an expression reads, and whether it calls a procedure.
class VariableReader : public ASR::BaseWalkVisitor<VariableReader> {
public:
    std::set<ASR::symbol_t*> variables;
    bool calls = false;

    void visit_Var(const ASR::Var_t &x) {
        variables.insert(ASRUtils::symbol_get_past_external(x.m_v));
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        calls = true;
        ASR::BaseWalkVisitor<VariableReader>::visit_FunctionCall(x);
    }
};

// The scopes the statements open, and the associations they make.
class ScopeCollector : public ASRUtils::BlockBodyWalkVisitor<ScopeCollector> {
public:
    std::set<SymbolTable*> scopes;
    std::vector<const ASR::Associate_t*> associations;

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (s && ASR::is_a<ASR::Block_t>(*s)) {
            scopes.insert(ASR::down_cast<ASR::Block_t>(s)->m_symtab);
        }
        ASRUtils::BlockBodyWalkVisitor<ScopeCollector>::visit_BlockCall(x);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (s && ASR::is_a<ASR::AssociateBlock_t>(*s)) {
            scopes.insert(ASR::down_cast<ASR::AssociateBlock_t>(s)->m_symtab);
        }
        ASRUtils::BlockBodyWalkVisitor<ScopeCollector>
            ::visit_AssociateBlockCall(x);
    }

    void visit_Associate(const ASR::Associate_t &x) {
        associations.push_back(&x);
    }
};

class AssignmentCollector :
        public ASRUtils::BlockBodyWalkVisitor<AssignmentCollector> {
public:
    std::vector<ASR::Assignment_t*> assignments;

    void visit_Assignment(const ASR::Assignment_t &x) {
        assignments.push_back((ASR::Assignment_t*)&x);
    }
};

bool is_association_of(ASR::symbol_t *name, SymbolTable *scope) {
    return ASRUtils::symbol_parent_symtab(name) == scope;
}

// A write of an allocatable array component of an element of an array of
// derived type, in the names of the loop.
struct ComponentWrite {
    ASR::ArrayItem_t *element = nullptr;
    ASR::symbol_t *component = nullptr;
    ComponentShape shape;
    Location loc;
};

// Builds the statements that, before an offloaded loop runs, give the
// components the loop writes the storage the kernel writes into, or check
// that they have it; see build_component_fit.
class GpuComponentFit {
    Allocator &al;
    SymbolTable *scope;
    bool realloc;
    bool checks;
    const ParallelLoopNest &loop;

    // What the loop body changes from one iteration to the next.
    std::shared_ptr<GpuIterationVaryingSymbols> changed;
    std::set<ASR::symbol_t*> indices;
    // Scalars assigned only by statements the replay runs, and those of
    // them the replay holds a value of at the point it is at.
    std::set<ASR::symbol_t*> trackable, tracked;
    // Arrays the loop may write through an association.
    std::set<ASR::symbol_t*> alias_roots;
    std::set<SymbolTable*> inner_scopes;
    std::map<ASR::symbol_t*, ASR::expr_t*> associations;

    // What the loop writes of a component of an array of derived type.
    struct Written {
        size_t sites = 0;
        // Whether a site is where the replay cannot tell which element it
        // writes.
        bool unknown = false;
        ASR::expr_t *array = nullptr;
        ASR::symbol_t *component = nullptr;
        // Extents that are the same for every element, if there are.
        std::vector<ASR::expr_t*> uniform;
    };
    std::map<std::string, Written> written;
    // Whether the loop can write components the replay does not see.
    bool everything_unknown = false;

    // The first walk only finds out what is written where.
    bool dry = true;
    size_t fits = 0;
    SymbolTable *block_scope = nullptr;
    // The replay's own variable for a loop index or a tracked scalar.
    std::map<ASR::symbol_t*, ASR::expr_t*> host;

    bool known(const Written &w) const {
        return !everything_unknown && w.sites == 1 && !w.unknown;
    }

    ASR::expr_t* resolved(ASR::expr_t *e) {
        ASR::expr_t *copy = duplicate(al, e);
        if (!associations.empty()) {
            VariableSubstituter substituter(al, associations);
            substituter.current_expr = &copy;
            substituter.replace_expr(copy);
        }
        return copy;
    }

    ASR::expr_t* on_host(ASR::expr_t *e) {
        ASR::expr_t *copy = duplicate(al, e);
        VariableSubstituter substituter(al, host);
        substituter.current_expr = &copy;
        substituter.replace_expr(copy);
        return copy;
    }

    ASR::expr_t* host_variable(ASR::symbol_t *sym) {
        auto found = host.find(sym);
        if (found != host.end()) return found->second;
        ASRUtils::ASRBuilder b(al, sym->base.loc);
        ASR::expr_t *var = b.Variable(block_scope,
            block_scope->get_unique_name(ASRUtils::symbol_name(sym), false),
            ASRUtils::duplicate_type(al, ASRUtils::symbol_type(sym)),
            ASR::intentType::Local);
        host[sym] = var;
        return var;
    }

    ASR::expr_t* new_variable(const Location &loc, const std::string &name,
            ASR::ttype_t *type) {
        ASRUtils::ASRBuilder b(al, loc);
        return b.Variable(block_scope, block_scope->get_unique_name(name,
            false), type, ASR::intentType::Local);
    }

    // Whether the replay can evaluate `e` where it is: what it reads is the
    // same before the loop runs, or is a loop index or a tracked scalar,
    // unless `at_start`.
    bool evaluable(ASR::expr_t *e, bool at_start) {
        VariableReader reader;
        reader.visit_expr(*e);
        if (reader.calls) return false;
        std::set<ASR::symbol_t*> ignored;
        for (ASR::symbol_t *sym : reader.variables) {
            if (indices.count(sym) || tracked.count(sym)) {
                if (at_start) return false;
                ignored.insert(sym);
            } else if (alias_roots.count(sym) || inner_scopes.count(
                    ASRUtils::symbol_parent_symtab(sym))) {
                return false;
            }
        }
        return !gpu_reads_changed(*changed, e, &ignored);
    }

    // Whether an array is the same array before the loop runs; its
    // elements may not be.
    bool same_array(ASR::expr_t *array) {
        ASRUtils::ASRBuilder b(al, array->base.loc);
        ASR::expr_t *size = b.ArraySize(duplicate(al, array), nullptr,
            integer_type(al, array->base.loc, 4));
        VariableReader reader;
        reader.visit_expr(*size);
        for (ASR::symbol_t *sym : reader.variables) {
            if (indices.count(sym) || tracked.count(sym) ||
                    inner_scopes.count(ASRUtils::symbol_parent_symtab(sym))) {
                return false;
            }
        }
        return !reader.calls && !gpu_reads_changed(*changed, size, nullptr);
    }

    void analyse() {
        for (size_t d = 0; d < loop.n_heads(); d++) {
            indices.insert(ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(loop.head(d).m_v)->m_v));
        }
        changed = gpu_symbols_changed_in(loop.body, loop.n_body);
        // An iteration that stops early skips the writes after that point.
        everything_unknown = transfers_control(loop.body, loop.n_body, true);

        ScopeCollector scopes;
        for (size_t i = 0; i < loop.n_body; i++) {
            scopes.visit_stmt(*loop.body[i]);
        }
        inner_scopes = scopes.scopes;
        for (const ASR::Associate_t *x : scopes.associations) {
            ASR::symbol_t *name = designator_root(x->m_target);
            if (!name) continue;
            if (!inner_scopes.count(ASRUtils::symbol_parent_symtab(name))) {
                // A pointer the loop associates can be written through.
                everything_unknown = true;
            }
            // What the name is written through is written.
            if (ASR::symbol_t *root = designator_root(x->m_value)) {
                alias_roots.insert(root);
            }
        }

        // A scalar every statement changing which is an assignment at the
        // top of the body is tracked by replaying those assignments.
        std::vector<ASR::stmt_t*> top, others;
        top_level(loop.body, loop.n_body, top);
        std::set<ASR::symbol_t*> candidates;
        for (ASR::stmt_t *stmt : top) {
            ASR::symbol_t *sym = assigned_scalar(stmt);
            if (sym) {
                candidates.insert(sym);
            } else {
                others.push_back(stmt);
            }
        }
        std::shared_ptr<GpuIterationVaryingSymbols> changed_otherwise =
            gpu_symbols_changed_in(others.data(), others.size());
        for (ASR::symbol_t *sym : candidates) {
            ASR::expr_t *var = ASRUtils::EXPR(ASR::make_Var_t(al,
                sym->base.loc, sym));
            if (!gpu_reads_changed(*changed_otherwise, var)) {
                trackable.insert(sym);
            }
        }
    }

    // The statements of a body that run one after the other in each
    // iteration, with those of a block or an association in it.
    void top_level(ASR::stmt_t **body, size_t n_body,
            std::vector<ASR::stmt_t*> &out) {
        for (size_t i = 0; i < n_body; i++) {
            ASR::symbol_t *construct = nullptr;
            if (ASR::is_a<ASR::BlockCall_t>(*body[i])) {
                construct = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(body[i])->m_m);
            } else if (ASR::is_a<ASR::AssociateBlockCall_t>(*body[i])) {
                construct = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(body[i])->m_m);
            }
            if (construct && ASR::is_a<ASR::Block_t>(*construct)) {
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(construct);
                top_level(block->m_body, block->n_body, out);
            } else if (construct &&
                    ASR::is_a<ASR::AssociateBlock_t>(*construct)) {
                ASR::AssociateBlock_t *block =
                    ASR::down_cast<ASR::AssociateBlock_t>(construct);
                top_level(block->m_body, block->n_body, out);
            } else {
                out.push_back(body[i]);
            }
        }
    }

    // The scalar variable an assignment gives a value, other than a loop
    // index.
    ASR::symbol_t* assigned_scalar(ASR::stmt_t *stmt) {
        if (!ASR::is_a<ASR::Assignment_t>(*stmt)) return nullptr;
        ASR::expr_t *target = ASR::down_cast<ASR::Assignment_t>(stmt)->m_target;
        if (!ASR::is_a<ASR::Var_t>(*target)) return nullptr;
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(target)->m_v);
        ASR::ttype_t *type = ASRUtils::expr_type(target);
        if (!ASR::is_a<ASR::Variable_t>(*sym) || indices.count(sym) ||
                ASRUtils::is_array(type) || ASRUtils::is_allocatable(type) ||
                ASRUtils::is_pointer(type) ||
                !(ASRUtils::is_integer(*type) || ASRUtils::is_real(*type) ||
                    ASRUtils::is_logical(*type))) {
            return nullptr;
        }
        return sym;
    }

    // The component writes of an assignment, in the names of the loop.
    std::vector<ComponentWrite> component_writes(ASR::Assignment_t *x) {
        std::vector<ComponentWrite> writes;
        ASR::expr_t *target = resolved(x->m_target);
        ASR::expr_t *value = resolved(x->m_value);
        if (ASR::is_a<ASR::StructInstanceMember_t>(*target)) {
            // `t(i)%v = value`
            ASR::StructInstanceMember_t *m =
                ASR::down_cast<ASR::StructInstanceMember_t>(target);
            ComponentWrite write;
            write.element = struct_element(m->m_v);
            write.component = ASRUtils::symbol_get_past_external(m->m_m);
            if (write.element && is_allocatable_array(write.component) &&
                    shapes_array(value)) {
                write.shape.shaped = write.shape.single = true;
                write.shape.always = true;
                write.shape.extents = value_extents(al, value);
                write.loc = x->base.base.loc;
                writes.push_back(write);
            }
            return writes;
        }
        ASR::ArrayItem_t *element = struct_element(target);
        if (!element) return writes;
        ASR::symbol_t *type = ASRUtils::symbol_get_past_external(
            ASRUtils::get_struct_sym_from_struct_expr(target));
        if (!type || !ASR::is_a<ASR::Struct_t>(*type)) return writes;
        std::vector<ASR::symbol_t*> components;
        struct_components(ASR::down_cast<ASR::Struct_t>(type), components);
        for (ASR::symbol_t *component : components) {
            if (!component || !is_allocatable_array(component)) continue;
            std::string name = ASRUtils::symbol_name(component);
            ComponentWrite write;
            write.element = element;
            write.component = component;
            write.loc = x->base.base.loc;
            if (ASR::is_a<ASR::FunctionCall_t>(*value)) {
                // `t(i) = f(...)`, where `f` shapes the component of its
                // result.
                ASR::FunctionCall_t *call =
                    ASR::down_cast<ASR::FunctionCall_t>(value);
                ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(
                    call->m_name);
                if (call->m_dt || !callee ||
                        !ASR::is_a<ASR::Function_t>(*callee)) {
                    continue;
                }
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(callee);
                if (!fn->m_return_var ||
                        !ASR::is_a<ASR::Var_t>(*fn->m_return_var)) {
                    continue;
                }
                write.shape = routine_component_shape(al, *fn,
                    ASRUtils::symbol_get_past_external(ASR::down_cast<
                        ASR::Var_t>(fn->m_return_var)->m_v), name);
                for (ASR::expr_t *&extent : write.shape.extents) {
                    extent = duplicate(al, extent);
                    DummyArgumentBinder binder(al, *fn, call->m_args,
                        call->n_args);
                    binder.current_expr = &extent;
                    binder.replace_expr(extent);
                    if (!binder.valid) {
                        write.shape.extents.clear();
                        break;
                    }
                }
            } else if (ASR::is_a<ASR::StructConstructor_t>(*value)) {
                // `t(i) = tt(...)`
                ASR::expr_t *given = constructor_value(
                    ASR::down_cast<ASR::StructConstructor_t>(value), name);
                if (!given || !shapes_array(given)) {
                    continue;
                }
                write.shape.shaped = write.shape.single = true;
                write.shape.always = true;
                write.shape.extents = value_extents(al, given);
            }
            if (write.shape.shaped) writes.push_back(write);
        }
        return writes;
    }

    std::vector<ASR::stmt_t*> slice(ASR::stmt_t **body, size_t n_body,
            bool replayed, bool top) {
        std::vector<ASR::stmt_t*> out;
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t *stmt = body[i];
            ASR::symbol_t *construct = nullptr;
            if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
                construct = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            } else if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
                construct = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
            }
            std::vector<ASR::stmt_t*> part;
            if (ASR::is_a<ASR::Assignment_t>(*stmt)) {
                part = assignment(ASR::down_cast<ASR::Assignment_t>(stmt),
                    replayed, top);
            } else if (ASR::is_a<ASR::If_t>(*stmt)) {
                part = if_statement(ASR::down_cast<ASR::If_t>(stmt), replayed);
            } else if (ASR::is_a<ASR::Select_t>(*stmt)) {
                part = select_statement(ASR::down_cast<ASR::Select_t>(stmt),
                    replayed);
            } else if (construct && ASR::is_a<ASR::Block_t>(*construct)) {
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(construct);
                part = slice(block->m_body, block->n_body, replayed, top);
            } else if (construct &&
                    ASR::is_a<ASR::AssociateBlock_t>(*construct)) {
                part = associate_block(
                    ASR::down_cast<ASR::AssociateBlock_t>(construct),
                    replayed, top);
            } else {
                nested(stmt);
            }
            out.insert(out.end(), part.begin(), part.end());
        }
        return out;
    }

    std::vector<ASR::stmt_t*> assignment(ASR::Assignment_t *x, bool replayed,
            bool top) {
        std::vector<ASR::stmt_t*> out;
        ASR::symbol_t *scalar = assigned_scalar(&x->base);
        if (scalar && trackable.count(scalar)) {
            // Only a top-level statement assigns a trackable scalar.
            LCOMPILERS_ASSERT(top && replayed);
            ASR::expr_t *value = resolved(x->m_value);
            if (evaluable(value, false)) {
                if (!dry) {
                    ASRUtils::ASRBuilder b(al, x->base.base.loc);
                    out.push_back(b.Assignment(host_variable(scalar),
                        on_host(value)));
                }
                tracked.insert(scalar);
            } else {
                tracked.erase(scalar);
            }
            return out;
        }
        for (ComponentWrite &write : component_writes(x)) {
            site(write, replayed, out);
        }
        return out;
    }

    std::vector<ASR::stmt_t*> if_statement(ASR::If_t *x, bool replayed) {
        ASR::expr_t *test = resolved(x->m_test);
        bool evaluated = replayed && evaluable(test, false);
        std::vector<ASR::stmt_t*> then_part = slice(x->m_body, x->n_body,
            evaluated, false);
        std::vector<ASR::stmt_t*> else_part = slice(x->m_orelse, x->n_orelse,
            evaluated, false);
        if (dry || (then_part.empty() && else_part.empty())) return {};
        ASRUtils::ASRBuilder b(al, x->base.base.loc);
        return {b.If(on_host(test), then_part, else_part)};
    }

    std::vector<ASR::stmt_t*> select_statement(ASR::Select_t *x,
            bool replayed) {
        ASR::expr_t *test = resolved(x->m_test);
        bool evaluated = replayed && evaluable(test, false);
        for (size_t i = 0; i < x->n_body && evaluated; i++) {
            if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[i])) {
                ASR::CaseStmt_t *c = ASR::down_cast<ASR::CaseStmt_t>(
                    x->m_body[i]);
                for (size_t j = 0; j < c->n_test; j++) {
                    evaluated = evaluated && evaluable(c->m_test[j], true);
                }
            } else {
                ASR::CaseStmt_Range_t *c = ASR::down_cast<ASR::CaseStmt_Range_t>(
                    x->m_body[i]);
                evaluated = evaluated && (!c->m_start ||
                    evaluable(c->m_start, true)) && (!c->m_end ||
                    evaluable(c->m_end, true));
            }
        }
        Vec<ASR::case_stmt_t*> cases;
        cases.reserve(al, x->n_body);
        bool any = false;
        for (size_t i = 0; i < x->n_body; i++) {
            const Location &loc = x->m_body[i]->base.loc;
            Vec<ASR::stmt_t*> body;
            if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[i])) {
                ASR::CaseStmt_t *c = ASR::down_cast<ASR::CaseStmt_t>(
                    x->m_body[i]);
                std::vector<ASR::stmt_t*> part = slice(c->m_body, c->n_body,
                    evaluated, false);
                any = any || !part.empty();
                if (dry) continue;
                body.from_pointer_n_copy(al, part.data(), part.size());
                Vec<ASR::expr_t*> tests;
                tests.reserve(al, c->n_test);
                for (size_t j = 0; j < c->n_test; j++) {
                    tests.push_back(al, duplicate(al, c->m_test[j]));
                }
                cases.push_back(al, ASR::down_cast<ASR::case_stmt_t>(
                    ASR::make_CaseStmt_t(al, loc, tests.p, tests.n, body.p,
                        body.n, c->m_fall_through)));
            } else {
                ASR::CaseStmt_Range_t *c = ASR::down_cast<ASR::CaseStmt_Range_t>(
                    x->m_body[i]);
                std::vector<ASR::stmt_t*> part = slice(c->m_body, c->n_body,
                    evaluated, false);
                any = any || !part.empty();
                if (dry) continue;
                body.from_pointer_n_copy(al, part.data(), part.size());
                cases.push_back(al, ASR::down_cast<ASR::case_stmt_t>(
                    ASR::make_CaseStmt_Range_t(al, loc,
                        c->m_start ? duplicate(al, c->m_start) : nullptr,
                        c->m_end ? duplicate(al, c->m_end) : nullptr,
                        body.p, body.n)));
            }
        }
        std::vector<ASR::stmt_t*> default_part = slice(x->m_default,
            x->n_default, evaluated, false);
        if (dry || (!any && default_part.empty())) return {};
        Vec<ASR::stmt_t*> default_body;
        default_body.from_pointer_n_copy(al, default_part.data(),
            default_part.size());
        return {ASRUtils::STMT(ASR::make_Select_t(al, x->base.base.loc,
            nullptr, on_host(test), cases.p, cases.n, default_body.p,
            default_body.n, x->m_enable_fall_through))};
    }

    std::vector<ASR::stmt_t*> associate_block(ASR::AssociateBlock_t *block,
            bool replayed, bool top) {
        std::map<ASR::symbol_t*, ASR::expr_t*> saved = associations;
        std::vector<ASR::stmt_t*> rest;
        for (size_t i = 0; i < block->n_body; i++) {
            ASR::stmt_t *stmt = block->m_body[i];
            ASR::symbol_t *name = ASR::is_a<ASR::Associate_t>(*stmt)
                ? designator_root(ASR::down_cast<ASR::Associate_t>(stmt)
                    ->m_target) : nullptr;
            if (name && is_association_of(name, block->m_symtab)) {
                associations[name] = resolved(
                    ASR::down_cast<ASR::Associate_t>(stmt)->m_value);
            } else {
                rest.push_back(stmt);
            }
        }
        std::vector<ASR::stmt_t*> out = slice(rest.data(), rest.size(),
            replayed, top);
        associations = saved;
        return out;
    }

    // A statement the replay does not run: whatever component it writes,
    // the replay cannot tell which elements.
    void nested(ASR::stmt_t *stmt) {
        if (!dry) return;
        AssignmentCollector collector;
        collector.visit_stmt(*stmt);
        for (ASR::Assignment_t *x : collector.assignments) {
            ASR::symbol_t *root = designator_root(x->m_target);
            if (root && inner_scopes.count(ASRUtils::symbol_parent_symtab(
                    root)) && ASRUtils::symbol_parent_symtab(root)->asr_owner &&
                    ASR::is_a<ASR::symbol_t>(*ASRUtils::symbol_parent_symtab(
                        root)->asr_owner) && ASR::is_a<ASR::AssociateBlock_t>(
                    *ASR::down_cast<ASR::symbol_t>(
                        ASRUtils::symbol_parent_symtab(root)->asr_owner))) {
                // Written through an association the replay did not make.
                everything_unknown = true;
            }
            for (ComponentWrite &write : component_writes(x)) {
                std::string key;
                if (!array_key(write.element->m_v, key)) {
                    everything_unknown = true;
                    continue;
                }
                Written &w = written[key + "%" +
                    ASRUtils::symbol_name(write.component)];
                w.sites++;
                w.unknown = true;
                w.array = write.element->m_v;
                w.component = write.component;
            }
        }
    }

    void site(const ComponentWrite &write, bool replayed,
            std::vector<ASR::stmt_t*> &out) {
        std::string key;
        if (!array_key(write.element->m_v, key)) {
            everything_unknown = true;
            return;
        }
        Written &w = written[key + "%" +
            ASRUtils::symbol_name(write.component)];
        bool subscripts = true;
        for (size_t i = 0; i < write.element->n_args; i++) {
            subscripts = subscripts &&
                evaluable(write.element->m_args[i].m_right, false);
        }
        bool array = same_array(write.element->m_v);
        bool sized = write.shape.single && !write.shape.extents.empty();
        for (ASR::expr_t *extent : write.shape.extents) {
            sized = sized && evaluable(extent, false);
        }
        if (dry) {
            w.sites++;
            w.array = write.element->m_v;
            w.component = write.component;
            if (!replayed || !subscripts || !array) w.unknown = true;
            bool uniform = array && write.shape.single &&
                !write.shape.extents.empty();
            for (ASR::expr_t *extent : write.shape.extents) {
                uniform = uniform && evaluable(extent, true);
            }
            if (uniform) w.uniform = write.shape.extents;
            return;
        }
        if (!known(w)) return;
        ASR::expr_t *element = on_host(&write.element->base);
        ASR::expr_t *component = ASRUtils::EXPR(
            ASRUtils::getStructInstanceMember_t(al, write.loc,
                (ASR::asr_t*)element, nullptr, write.component, scope));
        std::vector<ASR::stmt_t*> fit;
        if (sized) {
            std::vector<ASR::expr_t*> extents;
            for (ASR::expr_t *extent : write.shape.extents) {
                extents.push_back(on_host(extent));
            }
            if (realloc) {
                fit = allocate_to_fit(write.loc, component, extents);
            } else if (checks) {
                fit = check_fits(write.loc, component, extents);
            }
        } else if (checks && write.shape.always) {
            if (realloc) {
                fit.push_back(require_allocated(write.loc, component,
                    designator_name(&write.element->base) + "%" +
                        ASRUtils::symbol_name(write.component)));
            } else {
                fit.push_back(debug_check(write.loc, component,
                    duplicate(al, component)));
            }
        }
        if (fit.empty()) return;
        fits++;
        out.insert(out.end(), fit.begin(), fit.end());
    }

    ASR::expr_t* is_allocated(const Location &loc, ASR::expr_t *x) {
        Vec<ASR::expr_t*> args;
        args.reserve(al, 1);
        args.push_back(al, x);
        return ASRUtils::EXPR(ASR::make_IntrinsicImpureFunction_t(al, loc,
            static_cast<int64_t>(ASRUtils::IntrinsicImpureFunctions::Allocated),
            args.p, args.n, 0, ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)),
            nullptr));
    }

    // Evaluates `extents`, never below zero, into new variables.
    std::vector<ASR::expr_t*> evaluate_extents(const Location &loc,
            const std::vector<ASR::expr_t*> &extents,
            std::vector<ASR::stmt_t*> &stmts) {
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *length_type = integer_type(al, loc, 4);
        std::vector<ASR::expr_t*> lengths;
        for (ASR::expr_t *extent : extents) {
            ASR::expr_t *length = new_variable(loc, "gpu_extent", length_type);
            if (ASRUtils::extract_kind_from_ttype_t(
                    ASRUtils::expr_type(extent)) != 4) {
                extent = b.i2i_t(extent, length_type);
            }
            stmts.push_back(b.Assignment(length, b.Max(extent, b.i32(0))));
            lengths.push_back(length);
        }
        return lengths;
    }

    ASR::stmt_t* allocate_with(const Location &loc, ASR::expr_t *x,
            const std::vector<ASR::expr_t*> &lengths) {
        ASRUtils::ASRBuilder b(al, loc);
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, lengths.size());
        for (ASR::expr_t *length : lengths) {
            ASR::dimension_t dim;
            dim.loc = loc;
            dim.m_start = b.i32(1);
            dim.m_length = length;
            dims.push_back(al, dim);
        }
        return b.Allocate(x, dims.p, dims.n);
    }

    // Allocates `component` with `extents`, first deallocating it when it
    // is allocated with other extents, as an assignment with
    // --realloc-lhs-arrays does.
    std::vector<ASR::stmt_t*> allocate_to_fit(const Location &loc,
            ASR::expr_t *component, const std::vector<ASR::expr_t*> &extents) {
        ASRUtils::ASRBuilder b(al, loc);
        std::vector<ASR::stmt_t*> stmts;
        std::vector<ASR::expr_t*> lengths = evaluate_extents(loc, extents,
            stmts);
        size_t rank = lengths.size();
        ASR::expr_t *differs = nullptr;
        for (size_t d = 0; d < rank; d++) {
            ASR::expr_t *one = b.NotEq(b.ArraySize(duplicate(al, component),
                rank > 1 ? b.i32((int)d + 1) : nullptr,
                integer_type(al, loc, 4)), lengths[d]);
            differs = differs ? b.Or(differs, one) : one;
        }
        stmts.push_back(b.If(is_allocated(loc, duplicate(al, component)),
            {b.If(differs, {b.Deallocate(duplicate(al, component)),
                allocate_with(loc, duplicate(al, component), lengths)}, {})},
            {allocate_with(loc, component, lengths)}));
        return stmts;
    }

    // The check bounds checking makes of an assignment to `component` of a
    // value with `extents`: that it is allocated, with those extents.
    std::vector<ASR::stmt_t*> check_fits(const Location &loc,
            ASR::expr_t *component, const std::vector<ASR::expr_t*> &extents) {
        ASRUtils::ASRBuilder b(al, loc);
        std::vector<ASR::stmt_t*> stmts;
        std::vector<ASR::expr_t*> lengths = evaluate_extents(loc, extents,
            stmts);
        ASR::expr_t *value = new_variable(loc, "gpu_component_shape",
            b.allocatable(b.Array(std::vector<int64_t>(lengths.size(), -1),
                integer_type(al, loc, 1))));
        stmts.push_back(allocate_with(loc, value, lengths));
        stmts.push_back(debug_check(loc, component, duplicate(al, value)));
        stmts.push_back(b.Deallocate(duplicate(al, value)));
        return stmts;
    }

    // With --realloc-lhs-arrays the write would allocate `component`, but
    // its size cannot be told before the loop runs, so the program has to
    // have allocated it.
    ASR::stmt_t* require_allocated(const Location &loc, ASR::expr_t *component,
            const std::string &name) {
        ASRUtils::ASRBuilder b(al, loc);
        std::string message = "the size an offloaded loop gives '" + name +
            "' cannot be determined before the loop runs, so it cannot be "
            "allocated automatically; allocate it before the loop";
        ASR::expr_t *code = b.StringConstant(message,
            b.String(b.i32(message.size()), ASR::ExpressionLength));
        return b.If(b.Not(is_allocated(loc, component)),
            {ASRUtils::STMT(ASR::make_ErrorStop_t(al, loc, code))}, {});
    }

    ASR::stmt_t* debug_check(const Location &loc, ASR::expr_t *target,
            ASR::expr_t *value) {
        Vec<ASR::expr_t*> components;
        components.reserve(al, 1);
        components.push_back(al, value);
        return ASRUtils::STMT(ASR::make_DebugCheckArrayBounds_t(al, loc,
            target, components.p, components.n, false));
    }

    // Gives every element of the array `w` names whose component is not
    // allocated the extents that are the same for every element.
    std::vector<ASR::stmt_t*> allocate_unallocated(const Location &loc,
            const Written &w) {
        ASRUtils::ASRBuilder b(al, loc);
        std::vector<ASR::stmt_t*> stmts;
        size_t rank = ASRUtils::extract_n_dims_from_ttype(
            ASRUtils::expr_type(w.array));
        if (rank == 0) return stmts;
        std::vector<ASR::expr_t*> extents;
        for (ASR::expr_t *extent : w.uniform) {
            extents.push_back(duplicate(al, extent));
        }
        std::vector<ASR::expr_t*> lengths = evaluate_extents(loc, extents,
            stmts);
        std::vector<ASR::expr_t*> subscripts;
        for (size_t d = 0; d < rank; d++) {
            subscripts.push_back(new_variable(loc, "gpu_element",
                integer_type(al, loc, 4)));
        }
        ASR::expr_t *component = ASRUtils::EXPR(
            ASRUtils::getStructInstanceMember_t(al, loc,
                (ASR::asr_t*)b.ArrayItem_01(duplicate(al, w.array), subscripts),
                nullptr, w.component, scope));
        std::vector<ASR::stmt_t*> body = {b.If(b.Not(is_allocated(loc,
            duplicate(al, component))), {allocate_with(loc, component,
                lengths)}, {})};
        for (size_t d = rank; d-- > 0;) {
            body = {b.DoLoop(subscripts[d],
                b.ArrayLBound(duplicate(al, w.array), (int64_t)d + 1),
                b.ArrayUBound(duplicate(al, w.array), (int64_t)d + 1), body)};
        }
        stmts.insert(stmts.end(), body.begin(), body.end());
        return stmts;
    }

public:
    GpuComponentFit(Allocator &al, SymbolTable *scope, bool realloc,
            bool checks, const ParallelLoopNest &loop)
        : al(al), scope(scope), realloc(realloc), checks(checks),
          loop(loop) {}

    std::vector<ASR::stmt_t*> build(const Location &loc) {
        if (!realloc && !checks) return {};
        analyse();
        slice(loop.body, loop.n_body, true, true);
        if (written.empty()) return {};

        block_scope = al.make_new<SymbolTable>(scope);
        std::vector<ASR::stmt_t*> stmts;
        if (realloc) {
            for (auto &w : written) {
                if (!known(w.second) && w.second.sites == 1 &&
                        !w.second.uniform.empty()) {
                    std::vector<ASR::stmt_t*> part =
                        allocate_unallocated(loc, w.second);
                    stmts.insert(stmts.end(), part.begin(), part.end());
                }
            }
        }

        dry = false;
        tracked.clear();
        for (ASR::symbol_t *index : indices) host_variable(index);
        std::vector<ASR::stmt_t*> body = slice(loop.body, loop.n_body, true,
            true);
        if (fits > 0) {
            for (size_t d = loop.n_heads(); d-- > 0;) {
                const ASR::do_loop_head_t &head = loop.head(d);
                ASRUtils::ASRBuilder b(al, loc);
                body = {b.DoLoop(host_variable(ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(head.m_v)->m_v)),
                    duplicate(al, head.m_start), duplicate(al, head.m_end),
                    body, head.m_increment
                        ? duplicate(al, head.m_increment) : nullptr)};
            }
            stmts.insert(stmts.end(), body.begin(), body.end());
        }
        if (stmts.empty()) return {};

        Vec<ASR::stmt_t*> block_body;
        block_body.from_pointer_n_copy(al, stmts.data(), stmts.size());
        std::string name = scope->get_unique_name("__gpu_component_fit");
        ASR::asr_t *block = ASR::make_Block_t(al, loc, block_scope,
            s2c(al, name), block_body.p, block_body.n);
        block_scope->asr_owner = block;
        ASR::symbol_t *block_sym = ASR::down_cast<ASR::symbol_t>(block);
        scope->add_symbol(name, block_sym);
        return {ASRUtils::STMT(ASR::make_BlockCall_t(al, loc, -1, block_sym))};
    }
};

} // namespace

// A kernel cannot allocate, so the storage it writes an allocatable array
// component of an element of an array of derived type into has to exist
// before the launch. The host gets it ready from the loop as the source
// wrote it: it replays the loop's iterations, the `if` and `select case`
// tests and the scalar assignments the writes depend on, and at each write
// allocates the component to the size the write gives it (with
// --realloc-lhs-arrays) or, with bounds checking, checks that it has that
// size. What the replay cannot evaluate before the loop runs it does not
// guess; see doc/src/gpu_offloading.md.
std::vector<ASR::stmt_t*> GpuOffloadVisitor::build_component_fit(
        const ParallelLoopNest &loop, const Location &loc) {
    GpuComponentFit fit(al, current_scope, pass_options.realloc_lhs_arrays,
        pass_options.bounds_checking, loop);
    return fit.build(loc);
}

} // namespace LCompilers

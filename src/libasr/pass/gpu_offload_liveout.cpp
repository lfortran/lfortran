// What a parallel loop defines and the host reads after it.
//
// A variable of unspecified locality that only one iteration defines has
// that value after the loop (Fortran 2018, 11.1.7.5), and one that an
// iteration defines before it references it is a temporary of that
// iteration. The threads of a kernel run the iterations at the same time,
// so a variable all of them read and write in place would serve neither:
// one thread could read what another wrote in between. Each thread keeps
// a private copy of the variable instead, which it takes from where the
// value is shared on entry, and after each statement that defines the
// copy or a part of it puts that part back (GpuLiveoutWriteBack).
//
// A scalar is shared in a one-element buffer the launch copies the value
// into and back out of. A derived-type variable is a kernel argument, which
// the device writes in place; a thread keeps a copy of each scalar
// component (or component of a component) the loop defines, and of the
// whole value only where the loop names it whole. An element or a section
// of an array component is defined in place, as an element of an array
// is.

#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_data_layout.h>
#include <libasr/pass/gpu_offload_collect.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_visitor.h>

namespace LCompilers {

namespace {

// The kind of scalar a one-element buffer can carry back to the host, or
// what to call the one `type` is not.
std::string gpu_liveout_scalar_unsupported(ASR::ttype_t *type) {
    if (ASRUtils::is_allocatable(type)) return "allocatable";
    if (ASRUtils::is_pointer(type)) return "pointer";
    ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(type);
    if (ASR::is_a<ASR::Integer_t>(*t) || ASR::is_a<ASR::Real_t>(*t)
            || ASR::is_a<ASR::Logical_t>(*t)) {
        return "";
    }
    if (ASR::is_a<ASR::Complex_t>(*t)) return gpu_scalar_type_name(t);
    if (ASR::is_a<ASR::String_t>(*t)) return "character";
    return "non-numeric";
}

// The variable `e` is a chain of components of, and the names and the
// symbols of those components, outermost last, if it is one of `variables`.
ASR::symbol_t *gpu_component_chain(ASR::expr_t *e,
        const std::set<ASR::symbol_t*> &variables,
        std::vector<std::string> &path, std::vector<ASR::symbol_t*> &members) {
    path.clear();
    members.clear();
    while (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
        ASR::StructInstanceMember_t *m =
            ASR::down_cast<ASR::StructInstanceMember_t>(e);
        path.push_back(ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(m->m_m)));
        members.push_back(m->m_m);
        e = m->m_v;
    }
    if (!ASR::is_a<ASR::Var_t>(*e)) return nullptr;
    ASR::symbol_t *v = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(e)->m_v);
    if (!variables.count(v)) return nullptr;
    std::reverse(path.begin(), path.end());
    std::reverse(members.begin(), members.end());
    return v;
}

std::string gpu_designator_text(ASR::symbol_t *v,
        const std::vector<std::string> &path) {
    std::string text = ASRUtils::symbol_name(v);
    for (auto &c : path) text += "%" + c;
    return text;
}

bool gpu_struct_has_array_component(ASR::symbol_t *struct_sym) {
    ASR::Struct_t *st = gpu_struct_definition(struct_sym);
    if (!st) return true;
    if (st->m_parent && gpu_struct_has_array_component(st->m_parent)) {
        return true;
    }
    for (size_t i = 0; i < st->n_members; i++) {
        ASR::symbol_t *member = st->m_symtab->get_symbol(st->m_members[i]);
        if (!member || !ASR::is_a<ASR::Variable_t>(*member)) continue;
        ASR::ttype_t *t = ASRUtils::symbol_type(member);
        if (ASRUtils::is_array(t)) return true;
        if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(t)) &&
                gpu_struct_has_array_component(
                    ASR::down_cast<ASR::Variable_t>(member)
                        ->m_type_declaration)) {
            return true;
        }
    }
    return false;
}

// The number of bytes a value of a derived type takes, or a number larger
// than any limit when that is not known at compile time.
int64_t gpu_struct_bytes(ASR::symbol_t *struct_sym) {
    const int64_t unknown = int64_t(1) << 40;
    ASR::Struct_t *st = gpu_struct_definition(struct_sym);
    if (!st) return unknown;
    int64_t bytes = st->m_parent ? gpu_struct_bytes(st->m_parent) : 0;
    for (size_t i = 0; i < st->n_members; i++) {
        ASR::symbol_t *member = st->m_symtab->get_symbol(st->m_members[i]);
        if (!member || !ASR::is_a<ASR::Variable_t>(*member)) continue;
        ASR::ttype_t *t = ASRUtils::symbol_type(member);
        if (ASRUtils::is_allocatable_or_pointer(t)) return unknown;
        int64_t count = 1;
        ASR::dimension_t *dims = nullptr;
        size_t rank = ASRUtils::extract_dimensions_from_ttype(t, dims);
        for (size_t d = 0; d < rank; d++) {
            int64_t extent;
            if (dims[d].m_length == nullptr || !ASRUtils::extract_value(
                    ASRUtils::expr_value(dims[d].m_length), extent)) {
                return unknown;
            }
            count *= extent;
        }
        ASR::ttype_t *e = ASRUtils::extract_type(t);
        int64_t each;
        if (ASR::is_a<ASR::StructType_t>(*e)) {
            each = gpu_struct_bytes(ASR::down_cast<ASR::Variable_t>(
                member)->m_type_declaration);
        } else if (ASR::is_a<ASR::Integer_t>(*e) || ASR::is_a<ASR::Real_t>(*e)
                || ASR::is_a<ASR::Logical_t>(*e)
                || ASR::is_a<ASR::Complex_t>(*e)) {
            each = ASRUtils::extract_kind_from_ttype_t(e)
                * (ASR::is_a<ASR::Complex_t>(*e) ? 2 : 1);
        } else {
            return unknown;
        }
        bytes += count * each;
        if (bytes >= unknown) return unknown;
    }
    return bytes;
}

// A thread keeps a copy of a whole derived-type value, a derived-type
// component or an array component only up to this size: the copy is a
// local of every thread and is read from the variable on entry. A kernel
// local of a derived type, which a temporary of the iteration used to be,
// costs as much.
constexpr int64_t gpu_private_struct_limit = 4096;

// The type of a thread's private copy of an array component of type `t`:
// the same array, with its bounds as constants, which the kernel can
// declare. Nullptr when the component is allocatable or a pointer, its
// bounds are not known at compile time, its elements are not numbers or
// logicals, or it is larger than a thread keeps a copy of.
ASR::ttype_t *gpu_private_array_type(Allocator &al, const Location &loc,
        ASR::ttype_t *t) {
    if (!ASR::is_a<ASR::Array_t>(*t)) return nullptr;
    ASR::Array_t *a = ASR::down_cast<ASR::Array_t>(t);
    ASR::ttype_t *e = a->m_type;
    if (!(ASR::is_a<ASR::Integer_t>(*e) || ASR::is_a<ASR::Real_t>(*e)
            || ASR::is_a<ASR::Logical_t>(*e))) {
        return nullptr;
    }
    int64_t count = 1;
    Vec<ASR::dimension_t> dims;
    dims.reserve(al, a->n_dims);
    ASR::ttype_t *int4 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    for (size_t d = 0; d < a->n_dims; d++) {
        int64_t start, length;
        if (a->m_dims[d].m_start == nullptr
                || a->m_dims[d].m_length == nullptr
                || !ASRUtils::extract_value(ASRUtils::expr_value(
                    a->m_dims[d].m_start), start)
                || !ASRUtils::extract_value(ASRUtils::expr_value(
                    a->m_dims[d].m_length), length)) {
            return nullptr;
        }
        count *= length;
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
            start, int4, ASR::integerbozType::Decimal));
        dim.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
            length, int4, ASR::integerbozType::Decimal));
        dims.push_back(al, dim);
    }
    if (count * ASRUtils::extract_kind_from_ttype_t(e)
            > gpu_private_struct_limit) {
        return nullptr;
    }
    return ASRUtils::TYPE(ASR::make_Array_t(al, loc,
        ASRUtils::duplicate_type(al, e), dims.p, dims.n,
        ASR::array_physical_typeType::FixedSizeArray, a->m_memory_space));
}

// Whether an expression names one of `names`.
class GpuNamesFinder : public ASR::BaseWalkVisitor<GpuNamesFinder> {
public:
    const std::set<std::string> &names;
    bool found = false;
    GpuNamesFinder(const std::set<std::string> &n) : names(n) {}
    void visit_Var(const ASR::Var_t &x) {
        if (names.count(ASRUtils::symbol_name(x.m_v))) found = true;
    }
};

// Whether a subscript of an element or section names one of `names`.
bool gpu_subscript_names(const ASR::array_index_t *args, size_t n,
        const std::set<std::string> &names) {
    GpuNamesFinder finder(names);
    for (size_t i = 0; i < n; i++) {
        if (args[i].m_left) finder.visit_expr(*args[i].m_left);
        if (args[i].m_right) finder.visit_expr(*args[i].m_right);
        if (args[i].m_step) finder.visit_expr(*args[i].m_step);
    }
    return finder.found;
}

bool gpu_private_scalar_type(ASR::ttype_t *t) {
    return !ASRUtils::is_array(t) && !ASRUtils::is_allocatable_or_pointer(t)
        && (ASR::is_a<ASR::Integer_t>(*t) || ASR::is_a<ASR::Real_t>(*t)
            || ASR::is_a<ASR::Logical_t>(*t) || ASR::is_a<ASR::Complex_t>(*t));
}

// What a loop body does with derived-type variables: every part of one it
// names, and every part of one it defines.
class GpuStructParts
        : public ASRUtils::BlockBodyWalkVisitor<GpuStructParts> {
public:
    const std::set<ASR::symbol_t*> &variables;
    struct Use {
        ASR::symbol_t *variable;
        std::vector<std::string> path;
        std::vector<ASR::symbol_t*> members;
        ASR::ttype_t *type;
    };
    std::vector<Use> uses;
    struct Definition {
        ASR::symbol_t *variable;
        std::vector<std::string> path;
        // How a thread keeps what is defined: in a private copy, or in
        // place -- an element or a section of an array, and a value with
        // array parts or of a polymorphic type that a procedure defines,
        // which may be just the part the iteration owns.
        enum class Kind { Private, InPlace, InPlaceByCall, Unsupported } kind;
        Location loc;
        // For InPlaceByCall: whether the procedure may define a scalar
        // part of what it is passed, not only elements of arrays.
        bool defines_scalars;
        // The type of the part of the variable the path selects, and
        // whether only single elements of it are defined.
        ASR::ttype_t *type;
        bool element_only;
    };
    std::vector<Definition> definitions;

    // The index variables of the parallel loop.
    const std::set<std::string> &indices;

    GpuStructParts(const std::set<ASR::symbol_t*> &v,
        const std::set<std::string> &indices_) : variables(v),
        indices(indices_) {}

    void note_definition(ASR::expr_t *designator, const Location &loc,
            bool by_call) {
        while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*designator)) {
            designator = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
                designator)->m_arg;
        }
        // The chain of components of the variable the designator selects
        // from, under any element or section taken of it.
        ASR::expr_t *e = designator;
        bool part_of_array = false;
        // Whether an element or section it is selects by an index of the
        // parallel loop, which makes it the part one iteration owns.
        bool indexed = false;
        bool element_only = true;
        std::vector<std::string> path;
        std::vector<ASR::symbol_t*> members;
        ASR::symbol_t *v = nullptr;
        while (true) {
            v = gpu_component_chain(e, variables, path, members);
            if (v != nullptr) break;
            if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
                e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
            } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
                ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(e);
                part_of_array = true;
                indexed = indexed || gpu_subscript_names(item->m_args,
                    item->n_args, indices);
                e = item->m_v;
            } else if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                ASR::ArraySection_t *section =
                    ASR::down_cast<ASR::ArraySection_t>(e);
                part_of_array = true;
                element_only = false;
                indexed = indexed || gpu_subscript_names(section->m_args,
                    section->n_args, indices);
                e = section->m_v;
            } else {
                return;
            }
        }
        using Kind = Definition::Kind;
        Kind kind;
        bool defines_scalars = false;
        ASR::ttype_t *t = ASRUtils::expr_type(e);
        if (by_call && ASRUtils::is_array(ASRUtils::expr_type(designator))) {
            // A procedure may define any element of an array it is passed,
            // or just the one the iteration owns, in place.
            kind = Kind::InPlaceByCall;
        } else if ((part_of_array || ASRUtils::is_array(t)) && !indexed
                && !path.empty() && ASRUtils::is_array(t)
                && !ASRUtils::is_allocatable_or_pointer(t)) {
            // An array component that is not defined by the index of the
            // parallel loop, whole or in parts every iteration can name,
            // is a temporary of the iteration that each thread keeps a
            // copy of (see gpu_private_array_type).
            kind = Kind::Private;
        } else if (part_of_array || ASRUtils::is_array(t)
                || ASRUtils::is_pointer(t)) {
            // An element or section of an array the iteration owns, or
            // what an allocatable or a pointer designates, is not stored
            // in the variable as a scalar is, and is defined in place.
            kind = Kind::InPlace;
        } else if (ASRUtils::is_allocatable(t)) {
            kind = Kind::Unsupported;
        } else if (ASR::is_a<ASR::StructType_t>(*t)) {
            if (by_call && (ASRUtils::is_class_type(t)
                    || gpu_struct_has_array_component(
                        ASRUtils::get_struct_sym_from_struct_expr(e)))) {
                kind = Kind::InPlaceByCall;
                defines_scalars = true;
            } else {
                kind = ASRUtils::is_class_type(t) ? Kind::Unsupported
                    : Kind::Private;
            }
        } else {
            kind = gpu_private_scalar_type(t) ? Kind::Private
                : Kind::Unsupported;
        }
        definitions.push_back({v, path, kind, loc, defines_scalars, t,
            part_of_array && element_only});
        uses.push_back({v, path, members, t});
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        note_definition(x.m_target, x.base.base.loc, false);
        ASR::BaseWalkVisitor<GpuStructParts>::visit_Assignment(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i].m_value && gpu_call_may_define(x, i)) {
                note_definition(x.m_args[i].m_value, x.base.base.loc, true);
            }
        }
        if (x.m_dt && !gpu_call_object_is_argument(x)
                && gpu_call_may_define(x, x.n_args)) {
            note_definition(x.m_dt, x.base.base.loc, true);
        }
        ASR::BaseWalkVisitor<GpuStructParts>::visit_SubroutineCall(x);
    }

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        std::vector<std::string> path;
        std::vector<ASR::symbol_t*> members;
        ASR::expr_t *e = const_cast<ASR::expr_t*>(&x.base);
        if (ASR::symbol_t *v = gpu_component_chain(e, variables, path,
                members)) {
            uses.push_back({v, path, members, x.m_type});
            return;
        }
        ASR::BaseWalkVisitor<GpuStructParts>::visit_StructInstanceMember(x);
    }

    void visit_Var(const ASR::Var_t &x) {
        ASR::symbol_t *v = ASRUtils::symbol_get_past_external(x.m_v);
        if (variables.count(v)) {
            uses.push_back({v, {}, {}, ASRUtils::symbol_type(v)});
        }
    }
};

// Whether `a` is a prefix of `b`, or `b` itself.
bool gpu_path_prefix(const std::vector<std::string> &a,
        const std::vector<std::string> &b) {
    return a.size() <= b.size() && std::equal(a.begin(), a.end(), b.begin());
}

// The first of `variables` the statements walked pass to an optional
// dummy argument of a procedure, or nullptr.
class GpuOptionalPassedOn
        : public ASRUtils::BlockBodyWalkVisitor<GpuOptionalPassedOn> {
public:
    const std::set<ASR::symbol_t*> &variables;
    ASR::symbol_t *found = nullptr;
    GpuOptionalPassedOn(const std::set<ASR::symbol_t*> &v) : variables(v) {}

    void check(ASR::expr_t *actual, ASR::Variable_t *dummy) {
        if (actual == nullptr || dummy == nullptr || found != nullptr
                || dummy->m_presence != ASR::presenceType::Optional) {
            return;
        }
        while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*actual)) {
            actual = ASR::down_cast<ASR::ArrayPhysicalCast_t>(actual)->m_arg;
        }
        if (!ASR::is_a<ASR::Var_t>(*actual)) return;
        ASR::symbol_t *v = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(actual)->m_v);
        if (variables.count(v)) found = v;
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            check(x.m_args[i].m_value, gpu_call_dummy(x, i));
        }
        ASR::BaseWalkVisitor<GpuOptionalPassedOn>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(x.m_name);
        if (callee && ASR::is_a<ASR::Function_t>(*callee)) {
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(callee);
            for (size_t i = 0; i < x.n_args && i < f->n_args; i++) {
                if (!ASR::is_a<ASR::Var_t>(*f->m_args[i])) continue;
                ASR::symbol_t *d = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v);
                if (ASR::is_a<ASR::Variable_t>(*d)) {
                    check(x.m_args[i].m_value,
                        ASR::down_cast<ASR::Variable_t>(d));
                }
            }
        }
        ASR::BaseWalkVisitor<GpuOptionalPassedOn>::visit_FunctionCall(x);
    }
};

// Replaces each part of a derived-type variable a thread keeps a private
// copy of with that copy.
class GpuPrivatePartReplacer
        : public ASR::BaseExprReplacer<GpuPrivatePartReplacer> {
public:
    Allocator &al;
    std::set<ASR::symbol_t*> variables;
    // (variable, path) -> the private copy.
    std::map<std::pair<ASR::symbol_t*, std::vector<std::string>>,
        ASR::symbol_t*> copies;

    GpuPrivatePartReplacer(Allocator &al_) : al(al_) {}

    bool replace_with_copy(ASR::expr_t *e) {
        std::vector<std::string> path;
        std::vector<ASR::symbol_t*> members;
        ASR::symbol_t *v = gpu_component_chain(e, variables, path, members);
        if (v == nullptr) return false;
        auto it = copies.find({v, path});
        if (it == copies.end()) return false;
        *current_expr = ASRUtils::EXPR(ASR::make_Var_t(al, e->base.loc,
            it->second));
        return true;
    }

    void replace_StructInstanceMember(ASR::StructInstanceMember_t *x) {
        if (replace_with_copy(&x->base)) return;
        ASR::BaseExprReplacer<GpuPrivatePartReplacer>::
            replace_StructInstanceMember(x);
    }

    void replace_Var(ASR::Var_t *x) {
        replace_with_copy(&x->base);
    }
};

class GpuPrivatePartVisitor : public ASR::CallReplacerOnExpressionsVisitor<
        GpuPrivatePartVisitor> {
public:
    GpuPrivatePartReplacer replacer;
    GpuPrivatePartVisitor(Allocator &al) : replacer(al) {}
    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};

} // namespace

bool gpu_struct_privately_copyable(SymbolTable *scope, const std::string &name,
        ASR::ttype_t *type) {
    if (ASRUtils::is_array(type) || ASRUtils::is_class_type(type)
            || ASRUtils::is_allocatable_or_pointer(type)
            || !ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(type))) {
        return false;
    }
    ASR::symbol_t *sym = scope->resolve_symbol(name);
    if (sym == nullptr) return false;
    sym = ASRUtils::symbol_get_past_external(sym);
    if (!ASR::is_a<ASR::Variable_t>(*sym)) return false;
    ASR::symbol_t *struct_sym =
        ASR::down_cast<ASR::Variable_t>(sym)->m_type_declaration;
    return !gpu_struct_has_allocatable_parts(struct_sym)
        && gpu_struct_bytes(struct_sym) <= gpu_private_struct_limit;
}

namespace {

// How the assignments of the loop body define the variable `v`: whether
// any does, whether all define it whole, and whether all define it whole
// or define a component of it whole (`y = ...`, `y%k = ...`, `y%c = ...`,
// not `y%c(i) = ...` or `y%in%k = ...`).
class GpuAssignmentsOf
        : public ASRUtils::BlockBodyWalkVisitor<GpuAssignmentsOf> {
public:
    ASR::symbol_t *v;
    bool any = false, all_whole = true, all_whole_or_component = true;
    GpuAssignmentsOf(ASR::symbol_t *v_) : v(v_) {}
    void visit_Assignment(const ASR::Assignment_t &x) {
        ASR::expr_t *target = x.m_target;
        while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*target)) {
            target = ASR::down_cast<ASR::ArrayPhysicalCast_t>(target)->m_arg;
        }
        ASR::expr_t *root = target;
        while (true) {
            if (ASR::is_a<ASR::StructInstanceMember_t>(*root)) {
                root = ASR::down_cast<ASR::StructInstanceMember_t>(root)->m_v;
            } else if (ASR::is_a<ASR::ArrayItem_t>(*root)) {
                root = ASR::down_cast<ASR::ArrayItem_t>(root)->m_v;
            } else if (ASR::is_a<ASR::ArraySection_t>(*root)) {
                root = ASR::down_cast<ASR::ArraySection_t>(root)->m_v;
            } else {
                break;
            }
        }
        if (ASR::is_a<ASR::Var_t>(*root) && ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(root)->m_v) == v) {
            any = true;
            bool whole = ASR::is_a<ASR::Var_t>(*target);
            all_whole = all_whole && whole;
            all_whole_or_component = all_whole_or_component && (whole
                || (ASR::is_a<ASR::StructInstanceMember_t>(*target)
                    && ASR::is_a<ASR::Var_t>(*ASR::down_cast<
                        ASR::StructInstanceMember_t>(target)->m_v)));
        }
        ASR::BaseWalkVisitor<GpuAssignmentsOf>::visit_Assignment(x);
    }
};

} // namespace

GpuLocality gpu_locality(SymbolTable *scope, const std::string &name,
        ASR::ttype_t *type, bool live, bool assigned, ASR::stmt_t **body,
        size_t n_body) {
    if (live) return GpuLocality::PrivateCopies;
    if (ASRUtils::is_array(type)) return GpuLocality::KernelArgument;
    if (!ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(type))) {
        return assigned ? GpuLocality::KernelLocal
            : GpuLocality::KernelArgument;
    }
    bool copyable = gpu_struct_privately_copyable(scope, name, type);
    if (!assigned) {
        // Only passed to procedures that may define it.
        return copyable ? GpuLocality::PrivateCopies
            : GpuLocality::KernelArgument;
    }
    ASR::symbol_t *sym = scope->resolve_symbol(name);
    sym = sym ? ASRUtils::symbol_get_past_external(sym) : nullptr;
    GpuAssignmentsOf assignments(sym);
    for (size_t i = 0; i < n_body; i++) assignments.visit_stmt(*body[i]);
    if (assignments.all_whole) return GpuLocality::KernelLocal;
    if (copyable || gpu_no_kernel_local_struct(scope, name, type)) {
        return GpuLocality::PrivateCopies;
    }
    if (assignments.all_whole_or_component) return GpuLocality::KernelLocal;
    return GpuLocality::PrivateCopies;
}

bool GpuOffloadVisitor::plan_liveouts(const Location &loc,
        ASR::stmt_t **body, size_t n_body,
        const std::set<std::string> &assigned_vars,
        const std::set<std::string> &live_vars,
        const std::set<std::string> &loop_var_set,
        const std::set<std::string> &reduction_names,
        std::set<std::string> &local_scalar_names,
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>>
            &involved_syms,
        GpuLiveoutPlan &plan) {
    // Every question that can stop the offload is asked before anything
    // is created.
    std::vector<std::string> scalars;
    std::set<ASR::symbol_t*> structs;
    std::map<ASR::symbol_t*, ASR::symbol_t*> named;
    for (auto &name : assigned_vars) {
        if (loop_var_set.count(name)) continue;
        if (local_scalar_names.count(name)) continue;
        // A reduction accumulator is written by every thread. One shared
        // slot would be a race, so it gets a slot per thread instead.
        if (reduction_names.count(name)) continue;
        auto it = involved_syms.find(name);
        if (it == involved_syms.end()) continue;
        ASR::ttype_t *type = it->second.first;
        if (ASRUtils::is_array(type)) continue;
        if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(type))) {
            ASR::symbol_t *sym = current_scope->resolve_symbol(name);
            if (sym == nullptr) continue;
            ASR::symbol_t *v = ASRUtils::symbol_get_past_external(sym);
            if (!ASR::is_a<ASR::Variable_t>(*v)) continue;
            structs.insert(v);
            named[v] = sym;
            continue;
        }
        std::string unsupported = gpu_liveout_scalar_unsupported(type);
        if (!unsupported.empty()) {
            GpuDecline decline(GpuDeclineReason::LiveoutScalar, name);
            decline.type_name = unsupported;
            report_not_offloaded(loc, decline);
            return false;
        }
        scalars.push_back(name);
    }
    // A thread's copy of an optional scalar is present whether or not the
    // argument is, so it cannot be passed on to an optional dummy argument
    // in its place.
    {
        std::set<ASR::symbol_t*> optionals;
        for (auto &name : scalars) {
            ASR::symbol_t *sym = current_scope->resolve_symbol(name);
            ASR::symbol_t *v = sym ? ASRUtils::symbol_get_past_external(sym)
                : nullptr;
            if (v && ASR::is_a<ASR::Variable_t>(*v)
                    && ASR::down_cast<ASR::Variable_t>(v)->m_presence
                        == ASR::presenceType::Optional) {
                optionals.insert(v);
            }
        }
        if (!optionals.empty()) {
            GpuOptionalPassedOn passed(optionals);
            for (size_t i = 0; i < n_body && !passed.found; i++) {
                passed.visit_stmt(*body[i]);
            }
            if (passed.found) {
                report_not_offloaded(loc, GpuDecline(
                    GpuDeclineReason::LiveoutOptionalPassedOn,
                    ASRUtils::symbol_name(passed.found)));
                return false;
            }
        }
    }
    // The parts of each derived-type variable a thread keeps a copy of:
    // each scalar it defines, or the part around it the loop also names,
    // which a copy of the scalar alone would leave stale.
    std::vector<GpuLiveoutPlan::Copy> copies;
    if (!structs.empty()) {
        GpuStructParts parts(structs, loop_var_set);
        for (size_t i = 0; i < n_body; i++) parts.visit_stmt(*body[i]);
        using Kind = GpuStructParts::Definition::Kind;
        for (ASR::symbol_t *v : structs) {
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(v);
            bool live = live_vars.count(ASRUtils::symbol_name(named[v])) > 0;
            // A temporary of the iteration whose other components keep
            // the values they had: each thread works on a copy of the whole
            // value, which nothing needs put back.
            if (!live && gpu_struct_privately_copyable(current_scope,
                    ASRUtils::symbol_name(named[v]), var->m_type)) {
                copies.push_back({v, named[v], {}, {}, var->m_type,
                    "__gpu_private_" + std::string(ASRUtils::symbol_name(v)),
                    false});
                continue;
            }
            std::set<std::vector<std::string>> privatized;
            Location first = loc;
            for (auto &d : parts.definitions) {
                if (d.variable != v) continue;
                if (d.kind == Kind::Unsupported) {
                    report_not_offloaded(d.loc, GpuDecline(
                        GpuDeclineReason::LiveoutDerivedType,
                        gpu_designator_text(v, d.path)));
                    return false;
                }
                if (d.kind != Kind::Private) continue;
                if (privatized.empty()) first = d.loc;
                privatized.insert(d.path);
            }
            // An array component no thread can keep a copy of, of which
            // the loop defines only single elements, is defined in place,
            // as an element one iteration alone defines has to be.
            for (auto &d : parts.definitions) {
                if (d.variable != v || d.kind != Kind::Private
                        || !ASRUtils::is_array(d.type)
                        || gpu_private_array_type(al, loc, d.type)) {
                    continue;
                }
                bool elements = true;
                for (auto &e : parts.definitions) {
                    if (e.variable == v && e.kind == Kind::Private
                            && e.path == d.path && !e.element_only) {
                        elements = false;
                    }
                }
                if (elements) privatized.erase(d.path);
            }
            // A copy of a part the loop also names a part around is
            // replaced by a copy of that part.
            bool changed = true;
            while (changed) {
                changed = false;
                for (auto &u : parts.uses) {
                    if (u.variable != v) continue;
                    for (auto &p : privatized) {
                        if (u.path.size() < p.size()
                                && gpu_path_prefix(u.path, p)) {
                            privatized.erase(p);
                            privatized.insert(u.path);
                            changed = true;
                            break;
                        }
                    }
                    if (changed) break;
                }
            }
            // What a procedure defines in place, other threads may read in
            // place: the loop cannot also keep a private copy of a part of
            // it, or name a scalar part of it that the procedure may
            // define as a temporary of the iteration.
            for (auto &d : parts.definitions) {
                if (d.variable != v || d.kind != Kind::InPlaceByCall) continue;
                bool conflict = false;
                for (auto &p : privatized) {
                    if (gpu_path_prefix(p, d.path)
                            || gpu_path_prefix(d.path, p)) {
                        conflict = true;
                    }
                }
                for (auto &u : parts.uses) {
                    if (!d.defines_scalars) break;
                    if (u.variable == v && u.path.size() > d.path.size()
                            && gpu_path_prefix(d.path, u.path)
                            && !ASRUtils::is_array(u.type)) {
                        conflict = true;
                    }
                }
                if (conflict) {
                    report_not_offloaded(d.loc, GpuDecline(
                        GpuDeclineReason::LiveoutDerivedType,
                        gpu_designator_text(v, d.path)));
                    return false;
                }
            }
            if (privatized.empty()) continue;
            if (ASRUtils::is_class_type(var->m_type)
                    || ASRUtils::is_allocatable_or_pointer(var->m_type)) {
                report_not_offloaded(first, GpuDecline(
                    GpuDeclineReason::LiveoutDerivedType,
                    ASRUtils::symbol_name(v)));
                return false;
            }
            for (auto &p : privatized) {
                // What the path selects, as the loop names it.
                const GpuStructParts::Use *use = nullptr;
                for (auto &u : parts.uses) {
                    if (u.variable == v && u.path == p) {
                        use = &u;
                        break;
                    }
                }
                LCOMPILERS_ASSERT(use != nullptr);
                ASR::ttype_t *t = use->type;
                if (ASR::is_a<ASR::StructType_t>(*t)) {
                    // The whole variable, or a derived-type component of
                    // it, which the thread keeps a copy of whole.
                    ASR::symbol_t *struct_sym = p.empty()
                        ? var->m_type_declaration
                        : ASR::down_cast<ASR::Variable_t>(
                            ASRUtils::symbol_get_past_external(
                                use->members.back()))->m_type_declaration;
                    if ((!p.empty() && (ASRUtils::is_class_type(t)
                                || ASRUtils::is_allocatable_or_pointer(t)))
                            || gpu_struct_has_allocatable_parts(struct_sym)
                            || gpu_struct_bytes(struct_sym)
                                > gpu_private_struct_limit) {
                        report_not_offloaded(first, GpuDecline(
                            GpuDeclineReason::LiveoutDerivedType,
                            gpu_designator_text(v, p)));
                        return false;
                    }
                } else if (ASRUtils::is_array(t)) {
                    t = gpu_private_array_type(al, loc, t);
                    if (t == nullptr) {
                        report_not_offloaded(first, GpuDecline(
                            GpuDeclineReason::LiveoutDerivedType,
                            gpu_designator_text(v, p)));
                        return false;
                    }
                } else if (!gpu_private_scalar_type(t)) {
                    report_not_offloaded(first, GpuDecline(
                        GpuDeclineReason::LiveoutDerivedType,
                        gpu_designator_text(v, p)));
                    return false;
                }
                std::string copy = "__gpu_private_"
                    + std::string(ASRUtils::symbol_name(v));
                for (auto &c : p) copy += "_" + c;
                copies.push_back({v, named[v], p, use->members, t, copy,
                    live});
            }
        }
    }

    // The kernel takes the buffer, and the private copy is a local of it
    // under the scalar's own name, which the body names.
    ASR::ttype_t *int4_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    for (auto &name : scalars) {
        auto it = involved_syms.find(name);
        ASR::ttype_t *scalar_type = it->second.first;
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1,
            int4_type, ASR::integerbozType::Decimal));
        dim.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
            1, int4_type, ASR::integerbozType::Decimal));
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        dims.push_back(al, dim);
        ASR::ttype_t *buffer_type = ASRUtils::TYPE(ASR::make_Array_t(al,
            loc, ASRUtils::duplicate_type(al, scalar_type), dims.p, 1,
            ASR::array_physical_typeType::FixedSizeArray,
            ASR::memory_spaceType::Global));
        std::string buf_name = current_scope->get_unique_name(
            "__gpu_buf_" + name);
        ASR::symbol_t *buf_sym = gpu_new_variable(al, loc, current_scope,
            buf_name, ASRUtils::duplicate_type(al, buffer_type));
        involved_syms.erase(it);
        involved_syms[buf_name] = {buffer_type,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, buf_sym))};
        plan.scalars.push_back({name, buf_name, buf_sym,
            current_scope->resolve_symbol(name), scalar_type});
    }

    plan.copies = copies;
    return true;
}

void GpuOffloadVisitor::declare_liveout_copies(const Location &loc,
        SymbolTable *kernel_scope, GpuLiveoutPlan &plan) {
    for (auto &ls : plan.scalars) {
        gpu_new_variable(al, loc, kernel_scope, ls.orig_name,
            ASRUtils::duplicate_type(al, ls.scalar_type));
    }
    for (auto &c : plan.copies) {
        // Two variables can have one name, a local and a module variable
        // renamed on use, and so their copies.
        c.copy = kernel_scope->get_unique_name(c.copy);
        ASR::symbol_t *type_decl = nullptr;
        if (c.path.empty()) {
            type_decl = import_struct_type(c.named, current_scope,
                kernel_scope, loc);
        } else if (ASR::is_a<ASR::StructType_t>(*c.type)) {
            type_decl = import_struct_type(ASRUtils::symbol_get_past_external(
                c.members.back()), current_scope, kernel_scope, loc);
        }
        gpu_new_variable(al, loc, kernel_scope, c.copy,
            ASRUtils::duplicate_type(al, c.type), ASR::intentType::Local,
            type_decl);
    }
}

void GpuOffloadVisitor::privatize_liveout_block(SymbolTable *kernel_scope,
        ASR::stmt_t **body, size_t n_body, GpuLiveoutPlan &plan) {
    if (plan.copies.empty()) return;
    GpuPrivatePartVisitor replace(al);
    for (auto &c : plan.copies) {
        ASR::symbol_t *copy = kernel_scope->get_symbol(c.copy);
        replace.replacer.variables.insert(c.variable);
        replace.replacer.copies[{c.variable, c.path}] = copy;
        // The kernel argument, which a remapped body names instead.
        if (ASR::symbol_t *argument = kernel_scope->get_symbol(
                ASRUtils::symbol_name(c.named))) {
            replace.replacer.variables.insert(argument);
            replace.replacer.copies[{argument, c.path}] = copy;
        }
    }
    for (size_t i = 0; i < n_body; i++) {
        replace.visit_stmt(*body[i]);
    }
}

void GpuOffloadVisitor::privatize_liveout_body(const Location &loc,
        SymbolTable *kernel_scope, Vec<ASR::stmt_t*> &body,
        GpuLiveoutPlan &plan) {
    if (plan.copies.empty()) return;
    privatize_liveout_block(kernel_scope, body.p, body.n, plan);
    // Each copy starts as the part of the variable, read through the
    // host's symbols like the rest of the body, and remapped with it.
    Vec<ASR::stmt_t*> entered;
    entered.reserve(al, plan.copies.size() + body.n);
    for (auto &c : plan.copies) {
        ASR::expr_t *part = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
            c.named));
        for (ASR::symbol_t *m : c.members) {
            part = ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, loc,
                part, m, ASRUtils::duplicate_type(al, ASRUtils::symbol_type(
                    ASRUtils::symbol_get_past_external(m))), nullptr));
        }
        ASR::stmt_t *entry = ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                kernel_scope->get_symbol(c.copy))),
            part, nullptr, false, false, false));
        if (c.live) plan.entries.insert(entry);
        entered.push_back(al, entry);
    }
    for (size_t i = 0; i < body.n; i++) entered.push_back(al, body.p[i]);
    body = entered;
}

void GpuOffloadVisitor::enter_liveouts(const Location &loc,
        SymbolTable *kernel_scope, Vec<ASR::stmt_t*> &kernel_body,
        GpuLiveoutPlan &plan) {
    for (auto &ls : plan.scalars) {
        ASR::stmt_t *entry = ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                kernel_scope->get_symbol(ls.orig_name))),
            gpu_liveout_slot(al, loc, kernel_scope->get_symbol(ls.buf_name),
                ls.scalar_type),
            nullptr, false, false, false));
        plan.entries.insert(entry);
        kernel_body.push_back(al, entry);
    }
}

void GpuOffloadVisitor::write_back_liveouts(ASR::Function_t &kernel,
        GpuLiveoutPlan &plan) {
    if (plan.entries.empty()) return;
    GpuLiveoutWriteBack write_back(al);
    write_back.entries = plan.entries;
    write_back.run(kernel);
}

} // namespace LCompilers

#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/explicit_shape_extent.h>
#include <libasr/pass/symbol_expr_substitution.h>

#include <map>
#include <set>
#include <vector>

/*
The extent of an explicit-shape dimension is max(0, ub - lb + 1)
(Fortran 2018, 8.5.8.2). Semantics folds this for constant bounds, but keeps
`ub - lb + 1` as the length of a dimension whose bounds are known only when the
procedure is entered. That length is part of the procedure's signature and of
its module file, where callers use it with their own actual arguments.

Inside the procedure every use of such an array (its storage, SIZE, the
bounds, element addressing, loops, run-time checks) reads the bounds from the
type of the variable. As GFortran and Flang do, this pass evaluates each
non-constant lower bound and extent of an explicit-shape dummy, result and
automatic object of a procedure, main program or BLOCK once, on entry, in
locals initialized on entry (see ASRUtils::is_entry_initialized_local), and
gives the array a type of its own that refers to them:

    subroutine s(lo, n, a)                subroutine s(lo, n, a)
        integer, intent(in) :: lo, n          integer, intent(in) :: lo, n
        real :: a(lo:n), b(n)         =>      integer :: __libasr_lbound_a_1 = lo
                                              integer :: __libasr_extent_a_1 = max(0, n - lo + 1)
                                              integer :: __libasr_extent_b_1 = max(0, n)
                                              real :: a(__libasr_lbound_a_1:...)
                                              real :: b(__libasr_extent_b_1)

Every later use then reads the locals instead of evaluating the bound
expressions again, and redefining a variable of a bound in the body changes
neither the bounds nor the extent. The code generators set these locals
before they set up the dummies from their types. The only arrays that keep
an expression, max(0, length), as their extent are the dummies and the result
of an interface body, which has no body to evaluate it in.

The pass runs right after function_call_in_declaration, which captures each
bound of an automatic object that the body can change in such a local (this
pass clamps it there), and before any other pass copies the type of an array
into a temporary, a new procedure or a loop. They all see the bounds fixed on
entry and an extent that is never negative.

An ArrayPhysicalCast of such an array (for example, to pass it to an
assumed-shape dummy) was given a copy of the declared length by semantics. Its
type gets the dimensions of its operand, so that the extent passed on (as a
descriptor or by pass_array_by_data) is the extent of the array.

The signature of the procedure is left unchanged; a pass that builds it again
from the types of the dummies substitutes the initializer of the local
(ASRUtils::ReplaceWithFunctionParamVisitor). This includes a caller that sizes
the result of a procedure from its type, also when the procedure was loaded
from a module file (in another translation unit, the locals are never set).
The GPU offload passes do the same in the copy of a procedure that they make
for device code (inline_entry_initialized_locals).
The maximum is a conditional expression (see Max::create_extent), which the
code generators lower directly.
*/

namespace LCompilers {

class ExplicitShapeExtentVisitor:
        public ASR::BaseWalkVisitor<ExplicitShapeExtentVisitor> {
private:
    Allocator& al;
    std::set<const ASR::Variable_t*>& clamped;

    enum class Owner { None, Interface, Body };

    // Body if `x` is a dummy, the result or an automatic object of a host
    // procedure, or an automatic object of a BLOCK of one or of the main
    // program; Interface if it is a dummy or the result of an interface
    // body (which includes the declaration of a separate module procedure).
    // The GPU passes size the objects of device code themselves.
    static Owner get_owner(const ASR::Variable_t& x) {
        SymbolTable* scope = x.m_parent_symtab;
        while (scope != nullptr && scope->asr_owner != nullptr
                && ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) {
            ASR::symbol_t* owner = ASR::down_cast<ASR::symbol_t>(
                scope->asr_owner);
            if (ASR::is_a<ASR::Function_t>(*owner)) {
                ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(owner);
                if (ASRUtils::is_device_only_function(*f)) {
                    return Owner::None;
                }
                return ASRUtils::get_FunctionType(f)->m_deftype
                        == ASR::deftypeType::Implementation
                    ? Owner::Body : Owner::Interface;
            }
            if (ASR::is_a<ASR::Program_t>(*owner)) {
                return Owner::Body;
            }
            if (!ASR::is_a<ASR::Block_t>(*owner)) {
                return Owner::None;
            }
            scope = scope->parent;
        }
        return Owner::None;
    }

    static bool is_runtime(ASR::expr_t* bound) {
        return bound != nullptr && ASRUtils::expr_value(bound) == nullptr
            && !ASRUtils::is_value_constant(bound);
    }

    // Whether a dimension of the explicit-shape or assumed-size `array` has
    // a lower bound or a length that is known only on entry.
    static bool has_runtime_bound(const ASR::Array_t& array) {
        for (size_t i = 0; i < array.n_dims; i++) {
            if (array.m_dims[i].m_length != nullptr
                    && (is_runtime(array.m_dims[i].m_start)
                        || is_runtime(array.m_dims[i].m_length))) {
                return true;
            }
        }
        return false;
    }

    // Locals initialized on entry that hold an extent.
    std::set<const ASR::Variable_t*> extents;

    // The local of `scope` initialized on entry that `bound` is, if any.
    static ASR::Variable_t* get_entry_local(SymbolTable* scope,
            ASR::expr_t* bound) {
        if (ASR::is_a<ASR::Var_t>(*bound) && ASR::is_a<ASR::Variable_t>(
                *ASR::down_cast<ASR::Var_t>(bound)->m_v)) {
            ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(
                ASR::down_cast<ASR::Var_t>(bound)->m_v);
            if (v->m_parent_symtab == scope
                    && ASRUtils::is_entry_initialized_local(*v)) {
                return v;
            }
        }
        return nullptr;
    }

    // A new local of `scope`, initialized on entry with `value`.
    ASR::expr_t* create_entry_local(SymbolTable* scope,
            const ASR::Variable_t& array, const char* what, size_t dim,
            ASR::expr_t* value) {
        const Location& loc = value->base.loc;
        std::string name = scope->get_unique_name("__libasr_"
            + std::string(what) + "_" + std::string(array.m_name) + "_"
            + std::to_string(dim + 1), false);
        ASR::ttype_t* type = ASRUtils::duplicate_type(al,
            ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(value)));
        SetChar dependencies;
        dependencies.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, dependencies, type, value);
        ASR::symbol_t* sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, scope, s2c(al, name),
                dependencies.p, dependencies.size(), ASR::intentType::Local,
                value, nullptr, ASR::storage_typeType::Default, type, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false));
        scope->add_symbol(name, sym);
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
    }

    // The lower bound `start` of the dimension `dim` of `array`, evaluated
    // on entry in a local of `scope`.
    ASR::expr_t* lbound_local(SymbolTable* scope,
            const ASR::Variable_t& array, size_t dim, ASR::expr_t* start) {
        if (get_entry_local(scope, start) != nullptr) {
            return start;
        }
        ASRUtils::ExprStmtDuplicator duplicator(al);
        return create_entry_local(scope, array, "lbound", dim,
            duplicator.duplicate_expr(start));
    }

    // max(0, length) for the dimension `dim` of `array`, where `length` is
    // evaluated on entry in a local of `scope`. The maximum refers to
    // `length` twice, so a length that references a function (whose result
    // may be finalized) is first evaluated once in a local of its own.
    ASR::expr_t* clamped_length(SymbolTable* scope,
            const ASR::Variable_t& array, size_t dim, ASR::expr_t* length) {
        PassUtils::FunctionCallFinder finder;
        finder.visit_expr(*length);
        if (finder.found) {
            length = create_entry_local(scope, array, "length", dim, length);
        }
        return ASRUtils::Max::create_extent(al, length);
    }

    // The extent of the dimension `dim` of `array`, whose declared length is
    // `length`, evaluated on entry in a local of `scope`. A length that
    // function_call_in_declaration already captured in such a local is
    // clamped there.
    ASR::expr_t* extent_local(SymbolTable* scope,
            const ASR::Variable_t& array, size_t dim, ASR::expr_t* length) {
        if (ASR::Variable_t* v = get_entry_local(scope, length)) {
            if (extents.insert(v).second) {
                v->m_symbolic_value = clamped_length(scope, array, dim,
                    v->m_symbolic_value);
            }
            return length;
        }
        ASRUtils::ExprStmtDuplicator duplicator(al);
        ASR::expr_t* extent = clamped_length(scope, array, dim,
            duplicator.duplicate_expr(length));
        if (ASRUtils::is_value_constant(extent)) {
            // E.g. a template parameter, a named constant without a value.
            return extent;
        }
        ASR::expr_t* local = create_entry_local(scope, array, "extent", dim,
            extent);
        extents.insert(ASR::down_cast<ASR::Variable_t>(
            ASR::down_cast<ASR::Var_t>(local)->m_v));
        return local;
    }

public:
    ExplicitShapeExtentVisitor(Allocator& al_,
        std::set<const ASR::Variable_t*>& clamped_): al(al_),
        clamped(clamped_) {}

    void visit_Variable(const ASR::Variable_t& x) {
        if (!ASR::is_a<ASR::Array_t>(*x.m_type)) {
            return;
        }
        ASR::Array_t* array = ASR::down_cast<ASR::Array_t>(x.m_type);
        if (!has_runtime_bound(*array)) {
            return;
        }
        Owner owner = get_owner(x);
        if (owner == Owner::None) {
            return;
        }
        // An interface body has no body to evaluate the bounds on entry in,
        // so it keeps them with the extent max(0, length), the value that
        // the procedure computes on entry. Callers, which size the result
        // and check the arguments with this type, then agree with it.
        bool inline_extent = owner == Owner::Interface;
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, array->n_dims);
        for (size_t i = 0; i < array->n_dims; i++) {
            ASR::dimension_t dim = array->m_dims[i];
            if (is_runtime(dim.m_start) && !inline_extent) {
                dim.m_start = lbound_local(x.m_parent_symtab, x, i,
                    dim.m_start);
            }
            if (dim.m_length != nullptr
                    && ASRUtils::expr_value(dim.m_length) == nullptr) {
                dim.m_length = inline_extent
                    ? ASRUtils::Max::create_extent(al, dim.m_length)
                    : extent_local(x.m_parent_symtab, x, i, dim.m_length);
            }
            dims.push_back(al, dim);
        }
        const_cast<ASR::Variable_t&>(x).m_type = ASRUtils::TYPE(
            ASR::make_Array_t(al, array->base.base.loc, array->m_type,
                dims.p, dims.size(), array->m_physical_type,
                array->m_memory_space));
        clamped.insert(&x);
    }
};

class PhysicalCastExtentVisitor:
        public ASR::BaseWalkVisitor<PhysicalCastExtentVisitor> {
private:
    Allocator& al;
    const std::set<const ASR::Variable_t*>& clamped;

public:
    PhysicalCastExtentVisitor(Allocator& al_,
        const std::set<const ASR::Variable_t*>& clamped_): al(al_),
        clamped(clamped_) {}

    void visit_ArrayPhysicalCast(const ASR::ArrayPhysicalCast_t& x) {
        ASR::BaseWalkVisitor<PhysicalCastExtentVisitor>::visit_ArrayPhysicalCast(x);
        if (!ASR::is_a<ASR::Var_t>(*x.m_arg)) {
            return;
        }
        ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(x.m_arg)->m_v);
        if (!ASR::is_a<ASR::Variable_t>(*sym) || clamped.find(
                ASR::down_cast<ASR::Variable_t>(sym)) == clamped.end()) {
            return;
        }
        ASR::Array_t* arg_array = ASR::down_cast<ASR::Array_t>(
            ASRUtils::expr_type(x.m_arg));
        if (!ASR::is_a<ASR::Array_t>(*x.m_type)) {
            return;
        }
        ASR::Array_t* array = ASR::down_cast<ASR::Array_t>(x.m_type);
        if (array->n_dims != arg_array->n_dims) {
            return;
        }
        // The dimensions are copied, so that a later pass rewriting the
        // expressions of the cast (e.g. inlining a call in a bound) leaves
        // the type of the array unchanged.
        ASRUtils::ExprStmtDuplicator duplicator(al);
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, arg_array->n_dims);
        for (size_t i = 0; i < arg_array->n_dims; i++) {
            ASR::dimension_t dim = arg_array->m_dims[i];
            if (dim.m_start != nullptr) {
                dim.m_start = duplicator.duplicate_expr(dim.m_start);
            }
            if (dim.m_length != nullptr) {
                dim.m_length = duplicator.duplicate_expr(dim.m_length);
            }
            dims.push_back(al, dim);
        }
        const_cast<ASR::ArrayPhysicalCast_t&>(x).m_type = ASRUtils::TYPE(
            ASR::make_Array_t(al, array->base.base.loc, array->m_type,
                dims.p, dims.size(), array->m_physical_type,
                array->m_memory_space));
    }
};

// The locals initialized on entry of `scope`, of the procedures it contains
// and of its BLOCKs, with their initializers, and those scopes. Each local
// of the same name in the same place of `original`, of which `scope` is a
// copy, is given the same initializer.
static void collect_entry_initialized_locals(SymbolTable* scope,
        SymbolTable* original, std::map<ASR::symbol_t*, ASR::expr_t*>& locals,
        std::vector<SymbolTable*>& scopes) {
    scopes.push_back(scope);
    for (auto& item : scope->get_scope()) {
        ASR::symbol_t* sym = item.second;
        ASR::symbol_t* original_sym = original != nullptr
            ? original->get_symbol(item.first) : nullptr;
        if (ASR::is_a<ASR::Variable_t>(*sym)) {
            ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sym);
            if (ASRUtils::is_entry_initialized_local(*v)) {
                locals[sym] = v->m_symbolic_value;
                if (original_sym != nullptr && original_sym != sym) {
                    locals[original_sym] = v->m_symbolic_value;
                }
            }
        } else if (ASR::is_a<ASR::Function_t>(*sym)
                || ASR::is_a<ASR::Block_t>(*sym)) {
            SymbolTable* original_scope = original_sym != nullptr
                    && original_sym->type == sym->type
                ? ASRUtils::symbol_symtab(original_sym) : nullptr;
            collect_entry_initialized_locals(ASRUtils::symbol_symtab(sym),
                original_scope, locals, scopes);
        }
    }
}

void inline_entry_initialized_locals(Allocator &al, ASR::symbol_t* owner,
        ASR::symbol_t* original) {
    std::map<ASR::symbol_t*, ASR::expr_t*> locals;
    std::vector<SymbolTable*> scopes;
    collect_entry_initialized_locals(ASRUtils::symbol_symtab(owner),
        original != nullptr ? ASRUtils::symbol_symtab(original) : nullptr,
        locals, scopes);
    if (locals.empty()) {
        return;
    }
    // An initializer can refer to another such local (an extent to the
    // length it clamps), which each round replaces one level deeper.
    AssociateVarResolver resolver(al, locals);
    for (size_t round = 0; round < locals.size(); round++) {
        for (auto& item : locals) {
            resolver.current_expr = &item.second;
            resolver.replace_expr(item.second);
        }
    }
    AssociateVarResolverVisitor visitor(al, locals);
    visitor.visit_symbol(*owner);
    for (SymbolTable* scope : scopes) {
        std::vector<std::string> names;
        for (auto& item : scope->get_scope()) {
            if (locals.find(item.second) != locals.end()) {
                names.push_back(item.first);
            }
        }
        for (const std::string& name : names) {
            scope->erase_symbol(name);
        }
    }
    for (SymbolTable* scope : scopes) {
        for (auto& item : scope->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) {
                continue;
            }
            ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(item.second);
            SetChar dependencies;
            dependencies.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, dependencies,
                v->m_type, v->m_symbolic_value, v->m_value, v->m_name);
            v->m_dependencies = dependencies.p;
            v->n_dependencies = dependencies.size();
        }
    }
}

void substitute_entry_initialized_locals(Allocator &al, ASR::Function_t &fn,
        std::map<ASR::symbol_t*, ASR::expr_t*> &substitutions) {
    std::map<ASR::symbol_t*, ASR::expr_t*> locals;
    std::vector<SymbolTable*> scopes;
    collect_entry_initialized_locals(fn.m_symtab, nullptr, locals, scopes);
    if (locals.empty()) {
        return;
    }
    std::map<ASR::symbol_t*, ASR::expr_t*> values = substitutions;
    for (auto& item : locals) {
        ASRUtils::ExprStmtDuplicator duplicator(al);
        values[item.first] = duplicator.duplicate_expr(item.second);
    }
    // As in inline_entry_initialized_locals, one level per round.
    AssociateVarResolver resolver(al, values);
    for (size_t round = 0; round <= locals.size(); round++) {
        for (auto& item : locals) {
            resolver.current_expr = &values[item.first];
            resolver.replace_expr(values[item.first]);
        }
    }
    for (auto& item : locals) {
        substitutions[item.first] = values[item.first];
    }
}

void pass_explicit_shape_extent(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &/*pass_options*/) {
    std::set<const ASR::Variable_t*> clamped;
    ExplicitShapeExtentVisitor v(al, clamped);
    v.visit_TranslationUnit(unit);
    PhysicalCastExtentVisitor c(al, clamped);
    c.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers

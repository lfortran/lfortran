#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/explicit_shape_extent.h>
#include <libasr/pass/replace_function_call_in_declaration.h>

#include <set>

/*
The extent of an explicit-shape dimension is max(0, ub - lb + 1)
(Fortran 2018, 8.5.8.2). Semantics folds this for constant bounds, but keeps
`ub - lb + 1` as the length of a dimension whose bounds are known only when the
procedure is entered. That length is part of the procedure's signature and of
its module file, where callers use it with their own actual arguments.

Inside the procedure every use of such an array (its storage, SIZE, the
bounds, loops, run-time checks) reads the length from the type of the
variable. This pass therefore evaluates the extent of each such dimension of
an explicit-shape dummy, result and automatic object of a procedure or BLOCK
once, on entry, in a local initialized on entry (see
ASRUtils::is_entry_initialized_local), and gives the array a type of its own
whose length is that local:

    subroutine s(n, a)              subroutine s(n, a)
        integer, intent(in) :: n        integer, intent(in) :: n
        real :: a(n), b(n)      =>      integer :: __libasr_extent_a_1 = max(0, n)
                                        integer :: __libasr_extent_b_1 = max(0, n)
                                        real :: a(__libasr_extent_a_1)
                                        real :: b(__libasr_extent_b_1)

Every later use then reads the local instead of evaluating the bound
expression again. The code generators set up some dummies when the procedure
is entered, before its locals are initialized (see is_extent_used_on_entry);
the extent of such a dummy is max(0, length) in its type itself.

An ArrayPhysicalCast of such an array (for example, to pass it to an
assumed-shape dummy) was given a copy of the declared length by semantics. Its
type gets the dimensions of its operand, so that the extent passed on (as a
descriptor or by pass_array_by_data) is the extent of the array.

The signature of the procedure is left unchanged; a pass that builds it again
from the types of the dummies substitutes the initializer of the local
(ASRUtils::ReplaceWithFunctionParamVisitor). The maximum is created after the
intrinsic function pass, so it is lowered by the backends directly and never
becomes a helper procedure that only one translation unit could see.
*/

namespace LCompilers {

// Whether an expression or a type refers to the variable `v`.
class ReferencesVariable: public ASR::BaseWalkVisitor<ReferencesVariable> {
public:
    const ASR::Variable_t& v;
    bool found = false;

    ReferencesVariable(const ASR::Variable_t& v_): v(v_) {}

    void visit_Var(const ASR::Var_t& x) {
        if (ASRUtils::symbol_get_past_external(x.m_v) == &v.base) {
            found = true;
        }
    }
};

class ExplicitShapeExtentVisitor:
        public ASR::BaseWalkVisitor<ExplicitShapeExtentVisitor> {
private:
    Allocator& al;
    std::set<const ASR::Variable_t*>& clamped;

    // True if `x` is a dummy, the result or an automatic object of a host
    // procedure, or an automatic object of a BLOCK of one or of the main
    // program. The GPU passes size the objects of device code themselves.
    static bool is_host_procedure_object(const ASR::Variable_t& x) {
        SymbolTable* scope = x.m_parent_symtab;
        while (scope != nullptr && scope->asr_owner != nullptr
                && ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) {
            ASR::symbol_t* owner = ASR::down_cast<ASR::symbol_t>(
                scope->asr_owner);
            if (ASR::is_a<ASR::Function_t>(*owner)) {
                ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(owner);
                // A procedure of a module loaded from its modfile is not
                // defined in this translation unit.
                return ASRUtils::get_FunctionType(f)->m_deftype
                        == ASR::deftypeType::Implementation
                    && ASRUtils::get_FunctionType(f)->m_abi
                        != ASR::abiType::ExternalUndefined
                    && !ASRUtils::is_device_only_function(*f);
            }
            if (ASR::is_a<ASR::Program_t>(*owner)) {
                return true;
            }
            if (!ASR::is_a<ASR::Block_t>(*owner)) {
                return false;
            }
            scope = scope->parent;
        }
        return false;
    }

    static bool has_runtime_length(const ASR::Array_t& array) {
        for (size_t i = 0; i < array.n_dims; i++) {
            if (array.m_dims[i].m_length != nullptr
                    && ASRUtils::expr_value(array.m_dims[i].m_length) == nullptr) {
                return true;
            }
        }
        return false;
    }

    /*
        Whether the extent of the dummy or result `x` is evaluated when the
        procedure is entered, before its locals are initialized: a dummy
        passed by descriptor or by value gets a descriptor or a copy of its
        own, an INTENT(OUT) array of derived type (or such a result, which
        becomes one) is finalized, and the declaration of another dummy or of
        the result can refer to `x` (e.g. `character(len=size(x))`).
    */
    static bool is_extent_used_on_entry(const ASR::Variable_t& x,
            const ASR::Array_t& array) {
        bool result = x.m_intent == ASR::intentType::ReturnVar;
        if ((!ASRUtils::is_arg_dummy(x.m_intent) && !result)
                || !ASR::is_a<ASR::Function_t>(*ASR::down_cast<ASR::symbol_t>(
                    x.m_parent_symtab->asr_owner))) {
            return false;
        }
        if (x.m_value_attr || array.m_physical_type
                == ASR::array_physical_typeType::DescriptorArray) {
            return true;
        }
        if ((x.m_intent == ASR::intentType::Out || result)
                && ASR::is_a<ASR::StructType_t>(*array.m_type)) {
            return true;
        }
        ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(
            ASR::down_cast<ASR::symbol_t>(x.m_parent_symtab->asr_owner));
        for (size_t i = 0; i <= f->n_args; i++) {
            ASR::expr_t* other = i < f->n_args ? f->m_args[i] : f->m_return_var;
            if (other == nullptr || !ASR::is_a<ASR::Var_t>(*other)
                    || ASR::down_cast<ASR::Var_t>(other)->m_v == &x.base
                    || !ASR::is_a<ASR::Variable_t>(
                        *ASR::down_cast<ASR::Var_t>(other)->m_v)) {
                continue;
            }
            ReferencesVariable references(x);
            references.visit_ttype(*ASR::down_cast<ASR::Variable_t>(
                ASR::down_cast<ASR::Var_t>(other)->m_v)->m_type);
            if (references.found) {
                return true;
            }
        }
        return false;
    }

    // Locals initialized on entry that hold an extent.
    std::set<const ASR::Variable_t*> extents;

    // A local of `scope`, initialized on entry with the extent of the
    // dimension `dim` of `array`, whose declared length is `length`. A length
    // that function_call_in_declaration already captured in such a local is
    // clamped there.
    ASR::expr_t* extent_local(SymbolTable* scope,
            const ASR::Variable_t& array, size_t dim, ASR::expr_t* length) {
        if (ASR::is_a<ASR::Var_t>(*length) && ASR::is_a<ASR::Variable_t>(
                *ASR::down_cast<ASR::Var_t>(length)->m_v)) {
            ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(
                ASR::down_cast<ASR::Var_t>(length)->m_v);
            if (v->m_parent_symtab == scope
                    && ASRUtils::is_entry_initialized_local(*v)) {
                if (extents.insert(v).second) {
                    v->m_symbolic_value = ASRUtils::Max::create_extent(al,
                        v->m_symbolic_value);
                }
                return length;
            }
        }
        ASRUtils::ExprStmtDuplicator duplicator(al);
        ASR::expr_t* extent = ASRUtils::Max::create_extent(al,
            duplicator.duplicate_expr(length));
        if (ASRUtils::is_value_constant(extent)) {
            // E.g. a template parameter, a named constant without a value.
            return extent;
        }
        const Location& loc = length->base.loc;
        std::string name = scope->get_unique_name("__libasr_extent_"
            + std::string(array.m_name) + "_" + std::to_string(dim + 1), false);
        ASR::ttype_t* type = ASRUtils::duplicate_type(al,
            ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(extent)));
        SetChar dependencies;
        dependencies.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, dependencies, type, extent);
        ASR::symbol_t* sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, scope, s2c(al, name),
                dependencies.p, dependencies.size(), ASR::intentType::Local,
                extent, nullptr, ASR::storage_typeType::Default, type, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false));
        scope->add_symbol(name, sym);
        extents.insert(ASR::down_cast<ASR::Variable_t>(sym));
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
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
        if (!has_runtime_length(*array) || !is_host_procedure_object(x)) {
            return;
        }
        bool used_on_entry = is_extent_used_on_entry(x, *array);
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, array->n_dims);
        for (size_t i = 0; i < array->n_dims; i++) {
            ASR::dimension_t dim = array->m_dims[i];
            if (dim.m_length != nullptr
                    && ASRUtils::expr_value(dim.m_length) == nullptr) {
                // function_call_in_declaration captured each bound of an
                // automatic object that the body can change (outside the
                // scopes it leaves alone), so a local whose length can still
                // change is a temporary that a later pass created for a
                // statement of the body: it keeps the length it is used with.
                bool temporary = x.m_intent == ASR::intentType::Local
                    && bound_may_change(dim.m_length);
                dim.m_length = used_on_entry || temporary
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

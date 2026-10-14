#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/explicit_shape_extent.h>

#include <set>

/*
The extent of an explicit-shape dimension is max(0, ub - lb + 1)
(Fortran 2018, 8.5.8.2). Semantics folds this for constant bounds, but keeps
`ub - lb + 1` as the length of a dimension whose bounds are known only when the
procedure is entered. That length is part of the procedure's signature and of
its module file, where callers use it with their own actual arguments.

Inside the procedure every use of such an array (its storage, SIZE, the
bounds, loops, run-time checks) reads the length from the type of the
variable. This pass therefore gives each explicit-shape dummy, result and
automatic object of a procedure or BLOCK a type of its own with the length
max(0, length):

    subroutine s(n, a)              subroutine s(n, a)
        integer :: n                    integer :: n
        real :: a(n), b(n)      =>      real :: a(max(0, n)), b(max(0, n))

An ArrayPhysicalCast of such an array (for example, to pass it to an
assumed-shape dummy) was given a copy of the declared length by semantics. Its
type gets the dimensions of its operand, so that the extent passed on (as a
descriptor or by pass_array_by_data) is the extent of the array.

The signature of the procedure is left unchanged. The maximum is created after
the intrinsic function pass, so it is lowered by the backends directly and
never becomes a helper procedure that only one translation unit could see.
*/

namespace LCompilers {

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
                return ASRUtils::get_FunctionType(f)->m_deftype
                        == ASR::deftypeType::Implementation
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
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, array->n_dims);
        for (size_t i = 0; i < array->n_dims; i++) {
            ASR::dimension_t dim = array->m_dims[i];
            if (dim.m_length != nullptr) {
                dim.m_length = ASRUtils::Max::create_extent(al, dim.m_length);
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

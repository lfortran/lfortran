#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/string_format_array_args.h>

#include <iostream>
#include <vector>

/*
Array arguments of StringFormat
===============================

The formatting runtime reads every array argument of a `StringFormat` as
contiguous data in array element order, with its element count. This pass
makes that explicit: afterwards every array argument (and an array format) is
an expression of PointerArray physical type whose type has all its
dimensions, so a code generator only passes the data pointer and multiplies
the lengths of those dimensions; it creates nothing.

An array that is contiguous by construction (a fixed-size, SIMD or
bind(c) character array, or an allocatable one) is cast to PointerArray:

```fortran
print *, x                  ! integer, allocatable :: x(:)
```

becomes

```fortran
print *, ArrayPhysicalCast(x, DescriptorArray -> PointerArray,
                           integer :: (lbound(x, 1), size(x, 1)))
```

Any other array (a pointer, an assumed-shape dummy, a pointer component, the
real part of a complex array, the selector of `select rank`) may be strided.
It is copied to a contiguous temporary first, unless it turns out to be
contiguous at run time, in which case the temporary is only associated with
it. An output item is only read, so nothing is copied back:

```fortran
print *, s%p                ! integer, pointer :: p(:)
```

becomes

```fortran
c = is_contiguous(s%p)
if (.not. c) then
    allocate(t(lbound(s%p, 1):ubound(s%p, 1)))
    do i = lbound(t, 1), ubound(t, 1)
        t(i) = s%p(i)
    end do
else
    t => s%p
end if
print *, ArrayPhysicalCast(t, DescriptorArray -> PointerArray,
                           integer :: (lbound(t, 1), size(t, 1)))
if (c) then
    nullify(t)
else
    deallocate(t)
end if
```

An expression that cannot be associated with a pointer (`z%re`, a
`select rank` selector) is always copied.

Only the LLVM backend reads this form; the other backends skip the pass and
expand or reject the array items themselves. The pass runs after `print_arr`
and `print_struct_type`, which expand array items for the code generators
that use them, and after every pass that changes the physical type of an
array, and before `do_loops`, which lowers the copy loops. When the passes are verified, it checks the invariant it
establishes (ASRVerifyOptions::string_format_array_args).
*/

namespace LCompilers {

namespace {

class StringFormatArrayArgs
    : public ASR::CallReplacerOnExpressionsVisitor<StringFormatArrayArgs> {
public:
    Allocator &al;
    const PassOptions &pass_options;
    Vec<ASR::stmt_t*> *current_body = nullptr;
    Vec<ASR::stmt_t*> *body_after_curr_stmt = nullptr;

    StringFormatArrayArgs(Allocator &al_, const PassOptions &pass_options_)
        : al(al_), pass_options(pass_options_) {}

    int index_kind() const {
        return pass_options.descriptor_index_64 ? 8 : 4;
    }

    void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
        Vec<ASR::stmt_t*> *current_body_copy = current_body;
        Vec<ASR::stmt_t*> *body_after_curr_stmt_copy = body_after_curr_stmt;
        Vec<ASR::stmt_t*> body; body.reserve(al, n_body);
        current_body = &body;
        for (size_t i = 0; i < n_body; i++) {
            Vec<ASR::stmt_t*> after; after.reserve(al, 1);
            body_after_curr_stmt = &after;
            visit_stmt(*m_body[i]);
            body.push_back(al, m_body[i]);
            for (size_t j = 0; j < after.size(); j++) {
                body.push_back(al, after[j]);
            }
        }
        m_body = body.p;
        n_body = body.size();
        current_body = current_body_copy;
        body_after_curr_stmt = body_after_curr_stmt_copy;
    }

    static bool is_contiguous_by_construction(ASR::ttype_t *type) {
        switch (ASRUtils::extract_physical_type(type)) {
            case ASR::array_physical_typeType::FixedSizeArray:
            case ASR::array_physical_typeType::SIMDArray:
            case ASR::array_physical_typeType::StringArraySinglePointer:
                return true;
            case ASR::array_physical_typeType::DescriptorArray:
                return ASRUtils::is_allocatable(type);
            default:
                return false;
        }
    }

    // The array whose bounds are those of `e`.
    static ASR::expr_t *bounds_source(ASR::expr_t *e) {
        if (ASR::is_a<ASR::ComplexRe_t>(*e)) {
            return bounds_source(ASR::down_cast<ASR::ComplexRe_t>(e)->m_arg);
        }
        if (ASR::is_a<ASR::ComplexIm_t>(*e)) {
            return bounds_source(ASR::down_cast<ASR::ComplexIm_t>(e)->m_arg);
        }
        return e;
    }

    // The element of `e` at the subscripts `idx`.
    ASR::expr_t *element(ASR::expr_t *e, const Vec<ASR::expr_t*> &idx) {
        const Location &loc = e->base.loc;
        ASR::ttype_t *elem_type = ASRUtils::extract_type(ASRUtils::expr_type(e));
        if (ASR::is_a<ASR::ComplexRe_t>(*e)) {
            return ASRUtils::EXPR(ASR::make_ComplexRe_t(al, loc,
                element(ASR::down_cast<ASR::ComplexRe_t>(e)->m_arg, idx),
                elem_type, nullptr));
        }
        if (ASR::is_a<ASR::ComplexIm_t>(*e)) {
            return ASRUtils::EXPR(ASR::make_ComplexIm_t(al, loc,
                element(ASR::down_cast<ASR::ComplexIm_t>(e)->m_arg, idx),
                elem_type, nullptr));
        }
        Vec<ASR::array_index_t> args; args.reserve(al, idx.size());
        for (size_t i = 0; i < idx.size(); i++) {
            ASR::array_index_t ai;
            ai.loc = loc;
            ai.m_left = nullptr;
            ai.m_right = idx[i];
            ai.m_step = nullptr;
            args.push_back(al, ai);
        }
        return ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al, loc, e,
            args.p, args.size(), elem_type, ASR::arraystorageType::ColMajor,
            nullptr));
    }

    // `ArrayPhysicalCast(e, old, PointerArray, type)` whose type has the
    // dimensions of `e`.
    ASR::expr_t *cast_to_pointer_array(ASR::expr_t *e) {
        const Location &loc = e->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *type = ASRUtils::expr_type(e);
        ASR::array_physical_typeType old = ASRUtils::extract_physical_type(type);
        ASR::dimension_t *m_dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(type, m_dims);
        Vec<ASR::dimension_t> dims; dims.reserve(al, n_dims);
        bool type_has_dims = old != ASR::array_physical_typeType::DescriptorArray;
        for (size_t i = 0; i < n_dims; i++) {
            ASR::dimension_t dim;
            dim.loc = loc;
            if (type_has_dims) {
                dim.m_start = m_dims[i].m_start;
                dim.m_length = m_dims[i].m_length;
            } else {
                dim.m_start = b.ArrayLBound(e, i + 1, index_kind());
                dim.m_length = b.ArraySize(e, b.i_t(i + 1, b.int_type(index_kind())),
                    b.int_type(index_kind()));
            }
            dims.push_back(al, dim);
        }
        ASR::ttype_t *pointer_array_type = ASRUtils::TYPE(ASR::make_Array_t(al,
            loc, ASRUtils::extract_type(type), dims.p, dims.size(),
            ASR::array_physical_typeType::PointerArray,
            ASR::memory_spaceType::Global));
        return ASRUtils::EXPR(ASR::make_ArrayPhysicalCast_t(al, loc, e, old,
            ASR::array_physical_typeType::PointerArray, pointer_array_type,
            nullptr));
    }

    // A pointer array that is `e` when `e` is contiguous, and a contiguous
    // copy of it otherwise, with the statements that make and release it.
    ASR::expr_t *contiguous_temporary(ASR::expr_t *e) {
        const Location &loc = e->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *type = ASRUtils::expr_type(e);
        size_t n_dims = ASRUtils::extract_n_dims_from_ttype(type);
        ASR::ttype_t *elem_type = ASRUtils::extract_type(type);
        ASR::expr_t *len_expr = nullptr;
        if (ASR::is_a<ASR::String_t>(*elem_type)) {
            ASR::String_t *str = ASR::down_cast<ASR::String_t>(elem_type);
            if (!ASRUtils::is_value_constant(str->m_len)) {
                elem_type = ASRUtils::TYPE(ASR::make_String_t(al, loc,
                    str->m_kind, nullptr, ASR::string_length_kindType::DeferredLength,
                    ASR::string_physical_typeType::DescriptorString));
                len_expr = b.StringLen(e);
            }
        }
        ASR::ttype_t *temporary_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
            ASRUtils::create_array_type_with_empty_dims(al, n_dims, elem_type)));
        ASR::expr_t *temporary = b.Variable(current_scope,
            PassUtils::temporary_name(current_scope, "string_format_arg"),
            temporary_type, ASR::intentType::Local,
            ASRUtils::get_struct_sym_from_struct_expr(e));

        ASR::expr_t *source = bounds_source(e);
        Vec<ASR::dimension_t> alloc_dims; alloc_dims.reserve(al, n_dims);
        for (size_t i = 0; i < n_dims; i++) {
            ASR::dimension_t dim;
            dim.loc = loc;
            dim.m_start = b.ArrayLBound(source, i + 1, index_kind());
            dim.m_length = b.ArraySize(source, b.i_t(i + 1, b.int_type(index_kind())),
                b.int_type(index_kind()));
            alloc_dims.push_back(al, dim);
        }
        Vec<ASR::expr_t*> idx; idx.reserve(al, n_dims);
        Vec<ASR::stmt_t*> loop_body; loop_body.reserve(al, 1);
        ASR::stmt_t *copy_loop = b.create_do_loop(loc, n_dims, temporary,
            current_scope, idx, loop_body, [&]() {
                loop_body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                    al, loc, element(temporary, idx), element(e, idx),
                    nullptr, false, false)));
            }, index_kind());
        std::vector<ASR::stmt_t*> copy = {
            b.Allocate(temporary, alloc_dims.p, alloc_dims.size(), len_expr),
            copy_loop
        };
        Vec<ASR::expr_t*> vars; vars.reserve(al, 1);
        vars.push_back(al, temporary);
        ASR::stmt_t *deallocate = ASRUtils::STMT(ASR::make_ExplicitDeallocate_t(
            al, loc, vars.p, vars.size()));
        if (ASR::is_a<ASR::Var_t>(*e) || ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
            // The release is chosen by the same test as the acquisition,
            // even if the statement re-associates `e`.
            ASR::ttype_t *logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4));
            ASR::expr_t *is_contiguous = b.Variable(current_scope,
                PassUtils::temporary_name(current_scope, "string_format_arg_contiguous"),
                logical_type, ASR::intentType::Local);
            current_body->push_back(al, b.Assignment(is_contiguous,
                ASRUtils::EXPR(ASR::make_ArrayIsContiguous_t(al, loc, e,
                    logical_type, nullptr))));
            current_body->push_back(al, b.If(b.Not(is_contiguous), copy, {
                ASRUtils::STMT(ASR::make_Associate_t(al, loc, temporary, e))
            }));
            body_after_curr_stmt->push_back(al, b.If(is_contiguous, {
                ASRUtils::STMT(ASR::make_Nullify_t(al, loc, vars.p, vars.size()))
            }, {deallocate}));
        } else {
            for (ASR::stmt_t *s : copy) {
                current_body->push_back(al, s);
            }
            body_after_curr_stmt->push_back(al, deallocate);
        }
        return temporary;
    }

    ASR::expr_t *pointer_array(ASR::expr_t *e) {
        ASR::ttype_t *type = ASRUtils::expr_type(e);
        if (ASRUtils::extract_physical_type(type) ==
                ASR::array_physical_typeType::PointerArray) {
            return e;
        }
        if (!is_contiguous_by_construction(type)) {
            LCOMPILERS_ASSERT(current_body != nullptr);
            e = contiguous_temporary(e);
        }
        return cast_to_pointer_array(e);
    }

    void visit_StringFormat(const ASR::StringFormat_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<StringFormatArrayArgs>::visit_StringFormat(x);
        ASR::StringFormat_t &xx = const_cast<ASR::StringFormat_t&>(x);
        if (xx.m_fmt && ASRUtils::is_array(ASRUtils::expr_type(xx.m_fmt))) {
            xx.m_fmt = pointer_array(xx.m_fmt);
        }
        for (size_t i = 0; i < xx.n_args; i++) {
            if (ASRUtils::is_array(ASRUtils::expr_type(xx.m_args[i]))) {
                xx.m_args[i] = pointer_array(xx.m_args[i]);
            }
        }
    }
};

// The verification after each pass does not know which passes have run, so
// this pass checks the invariant it establishes itself, whenever the passes
// are verified.
void verify_string_format_array_args(const ASR::TranslationUnit_t &unit,
        const LCompilers::PassOptions &pass_options) {
    bool verify = pass_options.verify_all_passes;
#if defined(WITH_LFORTRAN_ASSERT)
    verify = true;
#endif
    if (!verify) return;
    diag::Diagnostics local_diagnostics;
    diag::Diagnostics &diagnostics = pass_options.diagnostics != nullptr
        ? *pass_options.diagnostics : local_diagnostics;
    ASRVerifyOptions verify_options;
    verify_options.string_format_array_args = true;
    if (!asr_verify(unit, verify_options, diagnostics)) {
        std::cerr << diagnostics.render2();
        throw LCompilersException("pass=string_format_array_args",
            LFORTRAN_ASR_PASS_VERIFY_FAILED);
    }
}

} // namespace

void pass_string_format_array_args(Allocator &al, ASR::TranslationUnit_t &unit,
        const LCompilers::PassOptions &pass_options) {
    StringFormatArrayArgs v(al, pass_options);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
    verify_string_format_array_args(unit, pass_options);
}

} // namespace LCompilers

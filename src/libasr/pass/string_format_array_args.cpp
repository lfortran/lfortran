#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/string_format_array_args.h>

#include <set>
#include <vector>

/*
Array arguments of StringFormat and arrays of data transfer statements
======================================================================

The formatting and the output runtimes read every array argument of a
`StringFormat`, and every array item of a WRITE statement, as contiguous data
in array element order, with its element count; an internal file that is an
array is a sequence of contiguous records. This pass makes that explicit:
afterwards every such array (and an array format) is an expression of
PointerArray physical type whose type has all its dimensions, so a code
generator only passes the data pointer and multiplies the lengths of those
dimensions; it creates nothing. The array items of a READ statement are
described below.

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

A READ statement defines its items in the order of the list, and a later
item may define storage of an earlier one (`read(u) a(1:5:2), a(1:3)`), or
take a subscript or a bound from a value read by an earlier one
(`read(u) a(1:5:2), n, a(n)`). A temporary would be copied back after the
statement, over what the later items defined, so an input item is not
copied: an array item that is contiguous by construction is cast to
PointerArray as above, and the code generator transfers any other one
element by element, in array element order, through its descriptor.

An internal file that a WRITE statement writes is copied like an output
item, and the copy is also copied back to it after the statement, before it
is released:

```fortran
write(p, '(i4)') 1, 2       ! character(len=4), pointer :: p(:)
```

becomes

```fortran
c = is_contiguous(p)
if (.not. c) then
    allocate(t(lbound(p, 1):ubound(p, 1)))
    do i = lbound(t, 1), ubound(t, 1)
        t(i) = p(i)
    end do
else
    t => p
end if
write(ArrayPhysicalCast(t, DescriptorArray -> PointerArray,
                        character(4) :: (lbound(t, 1), size(t, 1))),
      '(i4)') 1, 2
if (c) then
    nullify(t)
else
    do i = lbound(t, 1), ubound(t, 1)
        p(i) = t(i)
    end do
    deallocate(t)
end if
```

The records of an internal file that a READ statement reads are only read,
and are copied like an output item: no item of the statement can be in the
file or associated with it. An implied DO loop transfers its items one
element at a time and is left as it is; so is a namelist or a user-defined
derived-type input/output statement.

Only the LLVM backend reads this form; the other backends skip the pass and
expand or reject the array items themselves. The pass runs after `print_arr`
and `print_struct_type`, which expand array items for the code generators
that use them, after `array_struct_temporary`, which makes an array section
item a pointer to the section, and after every pass that changes the
physical type of an array, and before `do_loops`, which lowers the copy
loops. When the passes are verified, it checks the invariant it establishes
(ASRVerifyOptions::string_format_array_args).
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
    // The StringFormats whose arguments are the items of a READ statement.
    std::set<const ASR::StringFormat_t*> input_formats;

    StringFormatArrayArgs(Allocator &al_, const PassOptions &pass_options_)
        : al(al_), pass_options(pass_options_) {}

    // What a statement does with the elements of an array it is given.
    enum class Access {
        // It only reads them.
        Read,
        // It can define some of them and leave the others as they are.
        Update
    };

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

    // A loop that copies the elements of `from` to those of `to`, over the
    // bounds of `temporary`, one of the two.
    ASR::stmt_t *copy_elements(ASR::expr_t *temporary, ASR::expr_t *to,
            ASR::expr_t *from) {
        const Location &loc = to->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        size_t n_dims = ASRUtils::extract_n_dims_from_ttype(
            ASRUtils::expr_type(temporary));
        Vec<ASR::expr_t*> idx; idx.reserve(al, n_dims);
        Vec<ASR::stmt_t*> loop_body; loop_body.reserve(al, 1);
        return b.create_do_loop(loc, n_dims, temporary, current_scope, idx,
            loop_body, [&]() {
                loop_body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                    al, loc, element(to, idx), element(from, idx),
                    nullptr, false, false)));
            }, index_kind());
    }

    // A pointer array that is `e` when `e` is contiguous, and a contiguous
    // copy of it otherwise, with the statements that make and release it.
    // When the statement can define elements of `e`, the copy is copied back
    // to `e` before it is released.
    ASR::expr_t *contiguous_temporary(ASR::expr_t *e, Access access) {
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
        std::vector<ASR::stmt_t*> copy = {
            b.Allocate(temporary, alloc_dims.p, alloc_dims.size(), len_expr),
            copy_elements(temporary, temporary, e)
        };
        Vec<ASR::expr_t*> vars; vars.reserve(al, 1);
        vars.push_back(al, temporary);
        std::vector<ASR::stmt_t*> release;
        if (access == Access::Update) {
            release.push_back(copy_elements(temporary, e, temporary));
        }
        release.push_back(ASRUtils::STMT(ASR::make_ExplicitDeallocate_t(
            al, loc, vars.p, vars.size())));
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
                ASRUtils::STMT(ASR::make_Associate_t(al, loc, temporary, e, ASR::pointer_bounds_formType::NoPointerBounds))
            }));
            body_after_curr_stmt->push_back(al, b.If(is_contiguous, {
                ASRUtils::STMT(ASR::make_Nullify_t(al, loc, vars.p, vars.size()))
            }, release));
        } else {
            for (ASR::stmt_t *s : copy) {
                current_body->push_back(al, s);
            }
            for (ASR::stmt_t *s : release) {
                body_after_curr_stmt->push_back(al, s);
            }
        }
        return temporary;
    }

    ASR::expr_t *pointer_array(ASR::expr_t *e, Access access) {
        ASR::ttype_t *type = ASRUtils::expr_type(e);
        if (ASRUtils::extract_physical_type(type) ==
                ASR::array_physical_typeType::PointerArray) {
            return e;
        }
        if (!is_contiguous_by_construction(type)) {
            LCOMPILERS_ASSERT(current_body != nullptr);
            e = contiguous_temporary(e, access);
        }
        return cast_to_pointer_array(e);
    }

    void visit_StringFormat(const ASR::StringFormat_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<StringFormatArrayArgs>::visit_StringFormat(x);
        ASR::StringFormat_t &xx = const_cast<ASR::StringFormat_t&>(x);
        if (xx.m_fmt && ASRUtils::is_array(ASRUtils::expr_type(xx.m_fmt))) {
            xx.m_fmt = pointer_array(xx.m_fmt, Access::Read);
        }
        if (input_formats.count(&x)) {
            // Its arguments are the items of a READ statement.
            return;
        }
        for (size_t i = 0; i < xx.n_args; i++) {
            if (ASRUtils::is_array(ASRUtils::expr_type(xx.m_args[i]))) {
                xx.m_args[i] = pointer_array(xx.m_args[i], Access::Read);
            }
        }
    }

    // The array items of an output statement. An implied DO loop
    // transfers its items one element at a time.
    void output_items(ASR::expr_t **values, size_t n_values) {
        for (size_t i = 0; i < n_values; i++) {
            if (!ASR::is_a<ASR::ImpliedDoLoop_t>(*values[i]) &&
                    ASRUtils::is_array(ASRUtils::expr_type(values[i]))) {
                values[i] = pointer_array(values[i], Access::Read);
            }
        }
    }

    // The array items of an input statement are defined in place, in the
    // order of the list: one that is contiguous by construction is cast to
    // PointerArray, and the code generator transfers any other one element
    // by element through its descriptor. The items of an input statement
    // with a format label are the arguments of a StringFormat.
    void input_items(ASR::expr_t **values, size_t n_values) {
        for (size_t i = 0; i < n_values; i++) {
            if (ASR::is_a<ASR::StringFormat_t>(*values[i])) {
                ASR::StringFormat_t *sf = ASR::down_cast<ASR::StringFormat_t>(values[i]);
                input_formats.insert(sf);
                input_items(sf->m_args, sf->n_args);
            } else if (!ASR::is_a<ASR::ImpliedDoLoop_t>(*values[i])) {
                ASR::ttype_t *type = ASRUtils::expr_type(values[i]);
                if (ASRUtils::is_array(type) && ASRUtils::extract_physical_type(type) !=
                            ASR::array_physical_typeType::PointerArray &&
                        is_contiguous_by_construction(type)) {
                    values[i] = cast_to_pointer_array(values[i]);
                }
            }
        }
    }

    // An internal file that is an array.
    void internal_file(ASR::expr_t *&unit, Access access) {
        if (unit && ASRUtils::is_array(ASRUtils::expr_type(unit))) {
            unit = pointer_array(unit, access);
        }
    }

    void visit_FileWrite(const ASR::FileWrite_t &x) {
        ASR::FileWrite_t &xx = const_cast<ASR::FileWrite_t&>(x);
        if (!xx.m_overloaded && !xx.m_nml) {
            internal_file(xx.m_unit, Access::Update);
            output_items(xx.m_values, xx.n_values);
        }
        ASR::CallReplacerOnExpressionsVisitor<StringFormatArrayArgs>::visit_FileWrite(x);
    }

    void visit_FileRead(const ASR::FileRead_t &x) {
        ASR::FileRead_t &xx = const_cast<ASR::FileRead_t&>(x);
        if (!xx.m_overloaded && !xx.m_nml) {
            internal_file(xx.m_unit, Access::Read);
            input_items(xx.m_values, xx.n_values);
        }
        ASR::CallReplacerOnExpressionsVisitor<StringFormatArrayArgs>::visit_FileRead(x);
    }
};

} // namespace

void pass_string_format_array_args(Allocator &al, ASR::TranslationUnit_t &unit,
        const LCompilers::PassOptions &pass_options) {
    StringFormatArrayArgs v(al, pass_options);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
    ASRVerifyOptions verify_options;
    verify_options.string_format_array_args = true;
    PassUtils::verify_pass_invariant(unit, pass_options, verify_options,
        "string_format_array_args");
}

} // namespace LCompilers

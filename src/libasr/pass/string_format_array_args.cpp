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

The formatting and the input/output runtimes read every array argument of a
`StringFormat`, and every array item of a READ or WRITE statement, as
contiguous data in array element order, with its element count; an internal
file that is an array is a sequence of contiguous records. This pass makes
that explicit: afterwards every such array (and an array format) is an
expression of PointerArray physical type whose type has all its dimensions,
so a code generator only passes the data pointer and multiplies the lengths
of those dimensions; it creates nothing.

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

A READ statement can define some elements of an input item and leave the
others as they are, so a copy of the item is also copied back to it after the
statement, before it is released. The statement defines its items in the
order of the list, so every item after the first one that may be copied, if
it can share storage with another item, is read into a temporary that is
copied back after the statement, in the order of the list: an array into a
copy (it is never associated), a scalar into a scalar copy. A variable that
is neither a pointer nor a target shares storage with no other item and is
read directly, as is every item before the first one that may be copied:

```fortran
read(u) p                   ! integer, pointer :: p(:)
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
read(u) ArrayPhysicalCast(t, DescriptorArray -> PointerArray,
                          integer :: (lbound(t, 1), size(t, 1)))
if (c) then
    nullify(t)
else
    do i = lbound(t, 1), ubound(t, 1)
        p(i) = t(i)
    end do
    deallocate(t)
end if
```

```fortran
read(u) p, a(2)             ! integer, target :: a(3)
```

becomes

```fortran
...                         ! p as above
s = a(2)
read(u) ArrayPhysicalCast(t, ...), s
...                         ! t is copied back to p, as above
a(2) = s
```

The same holds for an internal file that a WRITE statement writes, while the
records of an internal file that a READ statement reads are only read. An
implied DO loop transfers its items one element at a time and is left as it
is; so is a namelist or a user-defined derived-type input/output statement.

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
    // copy of it otherwise (or always, with `always_copy`), with the
    // statements that make and release it. When the statement can define
    // elements of `e`, the copy is copied back to `e` before it is released.
    ASR::expr_t *contiguous_temporary(ASR::expr_t *e, Access access,
            bool always_copy = false) {
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
        if (!always_copy && (ASR::is_a<ASR::Var_t>(*e) ||
                ASR::is_a<ASR::StructInstanceMember_t>(*e))) {
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

    // Whether the array `e` is copied to a contiguous temporary (or
    // associated with one) to be transferred.
    static bool is_copied(ASR::expr_t *e) {
        ASR::ttype_t *type = ASRUtils::expr_type(e);
        return ASRUtils::extract_physical_type(type) !=
                ASR::array_physical_typeType::PointerArray &&
            !is_contiguous_by_construction(type);
    }

    ASR::expr_t *pointer_array(ASR::expr_t *e, Access access) {
        ASR::ttype_t *type = ASRUtils::expr_type(e);
        if (ASRUtils::extract_physical_type(type) ==
                ASR::array_physical_typeType::PointerArray) {
            return e;
        }
        if (is_copied(e)) {
            LCOMPILERS_ASSERT(current_body != nullptr);
            e = contiguous_temporary(e, access);
        }
        return cast_to_pointer_array(e);
    }

    // A temporary for the scalar input item `e`: it is a copy of `e`
    // before the statement and is copied back to `e` after it.
    ASR::expr_t *scalar_temporary(ASR::expr_t *e) {
        const Location &loc = e->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASRUtils::ExprStmtDuplicator dup(al);
        ASR::ttype_t *type = ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(e));
        ASR::expr_t *len_expr = nullptr;
        if (ASR::is_a<ASR::String_t>(*type) && !ASRUtils::is_value_constant(
                ASR::down_cast<ASR::String_t>(type)->m_len)) {
            type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, loc,
                ASRUtils::TYPE(ASR::make_String_t(al, loc,
                    ASR::down_cast<ASR::String_t>(type)->m_kind, nullptr,
                    ASR::string_length_kindType::DeferredLength,
                    ASR::string_physical_typeType::DescriptorString))));
            len_expr = b.StringLen(dup.duplicate_expr(e));
        }
        ASR::expr_t *temporary = b.Variable(current_scope,
            PassUtils::temporary_name(current_scope, "read_item"),
            type, ASR::intentType::Local);
        if (len_expr) {
            current_body->push_back(al, b.Allocate(temporary, nullptr, 0, len_expr));
        }
        current_body->push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
            al, loc, temporary, dup.duplicate_expr(e), nullptr, false, false)));
        body_after_curr_stmt->push_back(al, ASRUtils::STMT(
            ASRUtils::make_Assignment_t_util(al, loc, dup.duplicate_expr(e),
                temporary, nullptr, false, false)));
        if (len_expr) {
            Vec<ASR::expr_t*> vars; vars.reserve(al, 1);
            vars.push_back(al, temporary);
            body_after_curr_stmt->push_back(al, ASRUtils::STMT(
                ASR::make_ExplicitDeallocate_t(al, loc, vars.p, vars.size())));
        }
        return temporary;
    }

    // Whether another item of a statement can designate storage of the
    // scalar `e`, an item of intrinsic type: a variable that is neither a
    // pointer nor a target cannot be.
    static bool may_share_storage(ASR::expr_t *e) {
        switch (ASRUtils::extract_type(ASRUtils::expr_type(e))->type) {
            case ASR::ttypeType::Integer:
            case ASR::ttypeType::UnsignedInteger:
            case ASR::ttypeType::Real:
            case ASR::ttypeType::Complex:
            case ASR::ttypeType::Logical:
            case ASR::ttypeType::String:
                break;
            default:
                return false;
        }
        if (!ASR::is_a<ASR::Var_t>(*e)) {
            return true;
        }
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(e)->m_v);
        if (!ASR::is_a<ASR::Variable_t>(*sym)) {
            return true;
        }
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
        return v->m_target_attr || ASRUtils::is_pointer(v->m_type);
    }

    // Whether `e` refers to a variable of `syms`.
    static bool references_any(ASR::expr_t *e,
            const std::set<ASR::symbol_t*> &syms) {
        if (e == nullptr) return false;
        for (ASR::symbol_t *sym : syms) {
            if (ASRUtils::expr_references_symbol(e, sym)) return true;
        }
        return false;
    }

    // Whether a subscript or a substring bound of the designator `e` refers
    // to a variable of `syms`.
    static bool subscripts_reference(ASR::expr_t *e,
            const std::set<ASR::symbol_t*> &syms) {
        switch (e->type) {
            case ASR::exprType::Var:
                return false;
            case ASR::exprType::ArrayItem: {
                ASR::ArrayItem_t *x = ASR::down_cast<ASR::ArrayItem_t>(e);
                for (size_t i = 0; i < x->n_args; i++) {
                    if (references_any(x->m_args[i].m_left, syms) ||
                            references_any(x->m_args[i].m_right, syms) ||
                            references_any(x->m_args[i].m_step, syms)) {
                        return true;
                    }
                }
                return subscripts_reference(x->m_v, syms);
            }
            case ASR::exprType::StructInstanceMember:
                return subscripts_reference(
                    ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v, syms);
            case ASR::exprType::StringItem: {
                ASR::StringItem_t *x = ASR::down_cast<ASR::StringItem_t>(e);
                return references_any(x->m_idx, syms) ||
                    subscripts_reference(x->m_arg, syms);
            }
            case ASR::exprType::StringSection: {
                ASR::StringSection_t *x = ASR::down_cast<ASR::StringSection_t>(e);
                return references_any(x->m_start, syms) ||
                    references_any(x->m_end, syms) ||
                    references_any(x->m_step, syms) ||
                    subscripts_reference(x->m_arg, syms);
            }
            default:
                return references_any(e, syms);
        }
    }

    // Adds the variables whose storage the input item `e` designates.
    static void add_defined(ASR::expr_t *e, std::set<ASR::symbol_t*> &syms) {
        switch (e->type) {
            case ASR::exprType::Var:
                syms.insert(ASR::down_cast<ASR::Var_t>(e)->m_v);
                break;
            case ASR::exprType::ArrayItem:
                add_defined(ASR::down_cast<ASR::ArrayItem_t>(e)->m_v, syms);
                break;
            case ASR::exprType::ArraySection:
                add_defined(ASR::down_cast<ASR::ArraySection_t>(e)->m_v, syms);
                break;
            case ASR::exprType::ArrayPhysicalCast:
                add_defined(ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg, syms);
                break;
            case ASR::exprType::StructInstanceMember:
                add_defined(ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v, syms);
                break;
            case ASR::exprType::StringItem:
                add_defined(ASR::down_cast<ASR::StringItem_t>(e)->m_arg, syms);
                break;
            case ASR::exprType::StringSection:
                add_defined(ASR::down_cast<ASR::StringSection_t>(e)->m_arg, syms);
                break;
            case ASR::exprType::ImpliedDoLoop: {
                ASR::ImpliedDoLoop_t *x = ASR::down_cast<ASR::ImpliedDoLoop_t>(e);
                add_defined(x->m_var, syms);
                for (size_t i = 0; i < x->n_values; i++) {
                    add_defined(x->m_values[i], syms);
                }
                break;
            }
            default:
                break;
        }
    }

    void visit_StringFormat(const ASR::StringFormat_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<StringFormatArrayArgs>::visit_StringFormat(x);
        ASR::StringFormat_t &xx = const_cast<ASR::StringFormat_t&>(x);
        if (xx.m_fmt && ASRUtils::is_array(ASRUtils::expr_type(xx.m_fmt))) {
            xx.m_fmt = pointer_array(xx.m_fmt, Access::Read);
        }
        for (size_t i = 0; i < xx.n_args; i++) {
            if (ASRUtils::is_array(ASRUtils::expr_type(xx.m_args[i]))) {
                xx.m_args[i] = pointer_array(xx.m_args[i], Access::Read);
            }
        }
    }

    // The array items of a data transfer statement. An implied DO loop
    // transfers its items one element at a time. The items of an input
    // statement with a format label are the arguments of a StringFormat.
    void transfer_items(ASR::expr_t **values, size_t n_values, Access access) {
        for (size_t i = 0; i < n_values; i++) {
            if (ASR::is_a<ASR::StringFormat_t>(*values[i])) {
                ASR::StringFormat_t *sf = ASR::down_cast<ASR::StringFormat_t>(values[i]);
                transfer_items(sf->m_args, sf->n_args, access);
            } else if (!ASR::is_a<ASR::ImpliedDoLoop_t>(*values[i]) &&
                    ASRUtils::is_array(ASRUtils::expr_type(values[i]))) {
                values[i] = pointer_array(values[i], access);
            }
        }
    }

    // The items of a data transfer statement, past the StringFormat of
    // a format label.
    static void collect_items(ASR::expr_t **values, size_t n_values,
            std::vector<ASR::expr_t**> &items) {
        for (size_t i = 0; i < n_values; i++) {
            if (ASR::is_a<ASR::StringFormat_t>(*values[i])) {
                ASR::StringFormat_t *sf = ASR::down_cast<ASR::StringFormat_t>(values[i]);
                collect_items(sf->m_args, sf->n_args, items);
            } else {
                items.push_back(&values[i]);
            }
        }
    }

    // The items of an input statement. A copied item is copied back after
    // the statement, so every later item that can share storage with it is
    // read into a temporary too, which is copied back after it: the items
    // are then defined in the order of the list. Such an array is always
    // copied; a scalar is read into a scalar temporary. A temporary is a
    // copy of its item before the statement, so an element that the
    // statement leaves as it is (a null value, or end of file) keeps its
    // value. An implied DO loop, a scalar of derived type and an item with
    // a subscript that uses a variable an earlier item reads are still
    // transferred directly; they are only wrong if they share storage with
    // an earlier copied item.
    void input_items(ASR::expr_t **values, size_t n_values) {
        std::vector<ASR::expr_t**> items;
        collect_items(values, n_values, items);
        size_t first_copied = items.size();
        for (size_t i = 0; i < items.size(); i++) {
            ASR::expr_t *e = *items[i];
            if (!ASR::is_a<ASR::ImpliedDoLoop_t>(*e) &&
                    ASRUtils::is_array(ASRUtils::expr_type(e)) && is_copied(e)) {
                first_copied = i;
                break;
            }
        }
        std::set<ASR::symbol_t*> defined;
        for (size_t i = 0; i < items.size(); i++) {
            ASR::expr_t *e = *items[i];
            if (ASR::is_a<ASR::ImpliedDoLoop_t>(*e)) {
                add_defined(e, defined);
                continue;
            }
            if (ASRUtils::is_array(ASRUtils::expr_type(e))) {
                if (i <= first_copied) {
                    *items[i] = pointer_array(e, Access::Update);
                } else {
                    *items[i] = cast_to_pointer_array(
                        contiguous_temporary(e, Access::Update, true));
                }
            } else if (i > first_copied && may_share_storage(e) &&
                    !subscripts_reference(e, defined)) {
                *items[i] = scalar_temporary(e);
            }
            add_defined(e, defined);
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
            transfer_items(xx.m_values, xx.n_values, Access::Read);
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

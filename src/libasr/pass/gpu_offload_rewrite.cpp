#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_offload_rewrite.h>

namespace LCompilers {

ASR::symbol_t* gpu_new_variable(Allocator &al, const Location &loc,
        SymbolTable *scope, const std::string &name, ASR::ttype_t *type,
        ASR::intentType intent,
        ASR::symbol_t *type_decl) {
    return ASR::down_cast<ASR::Var_t>(ASRUtils::ASRBuilder(al, loc).Variable(
        scope, name, type, intent, type_decl))->m_v;
}

std::pair<ASR::expr_t*, ASR::expr_t*> get_dim_bounds(Allocator &al,
        const Location &loc, ASR::dimension_t *dims, size_t d,
        ASR::expr_t *arr_expr) {
    if (dims && dims[d].m_start && dims[d].m_length) {
        return {dims[d].m_start, dims[d].m_length};
    }
    ASR::ttype_t *int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    ASR::expr_t *dim_expr = ASRUtils::EXPR(
        ASR::make_IntegerConstant_t(al, loc, (int64_t)d + 1, int_type,
            ASR::integerbozType::Decimal));
    return {ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc, arr_expr,
                dim_expr, int_type, ASR::arrayboundType::LBound, nullptr)),
            ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc, arr_expr,
                dim_expr, int_type, ASR::arrayboundType::UBound, nullptr))};
}

void set_loop_head_bounds(Allocator &al, const Location &loc,
        ASR::do_loop_head_t &head, ASR::dimension_t *dims, size_t d,
        ASR::expr_t *arr_expr) {
    std::pair<ASR::expr_t*, ASR::expr_t*> bounds =
        get_dim_bounds(al, loc, dims, d, arr_expr);
    head.m_start = bounds.first;
    head.m_end = bounds.second;
}

std::string struct_member_owner_name(ASR::symbol_t *member,
        const std::string &fallback) {
    ASR::symbol_t *owner = ASRUtils::get_asr_owner(member);
    if (owner && ASR::is_a<ASR::Struct_t>(*owner)) {
        return std::string(ASRUtils::symbol_name(owner));
    }
    return fallback;
}

ASR::expr_t* gpu_array_size_from_type(Allocator &al,
        const ASR::ArraySize_t *sz) {
    if (!sz->m_v || ASR::is_a<ASR::Var_t>(*sz->m_v)) return nullptr;
    ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(
        ASRUtils::expr_type(sz->m_v));
    if (!t || !ASR::is_a<ASR::Array_t>(*t)) return nullptr;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(t);
    if (arr->n_dims == 0) return nullptr;
    size_t begin = 0, end = arr->n_dims;
    if (sz->m_dim) {
        int64_t d;
        if (!ASRUtils::extract_value(ASRUtils::expr_value(sz->m_dim), d)) {
            return nullptr;
        }
        if (d < 1 || (size_t)d > arr->n_dims) return nullptr;
        begin = (size_t)d - 1;
        end = begin + 1;
    }
    ASR::ttype_t *int_t = ASRUtils::TYPE(ASR::make_Integer_t(al,
        sz->base.base.loc, 4));
    ASR::expr_t *acc = nullptr;
    for (size_t d = begin; d < end; d++) {
        ASR::expr_t *len = arr->m_dims[d].m_length;
        if (!len) return nullptr;
        ASRUtils::ExprStmtDuplicator dup(al);
        dup.success = true;
        ASR::expr_t *one = dup.duplicate_expr(len);
        if (!one || !dup.success) return nullptr;
        acc = acc ? ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al,
            sz->base.base.loc, acc, ASR::binopType::Mul, one, int_t,
            nullptr)) : one;
    }
    return acc;
}

ASR::expr_t* gpu_simplify_array_sizes(Allocator &al,
        ASR::expr_t *expr) {
    if (!expr) return expr;
    GpuArraySizeFromTypeReplacer r(al);
    ASR::expr_t *root = expr;
    r.current_expr = &root;
    r.replace_expr(root);
    return root;
}

} // namespace LCompilers

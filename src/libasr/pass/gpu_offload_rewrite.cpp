#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/intrinsic_function_registry.h>

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
        int64_t d = 0;
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

namespace LCompilers {

namespace {

// `present(x)` of an optional dummy of the procedure the loop is in, as the
// host local the host sets before the launch.
class GpuPresentHoister : public ASR::BaseExprReplacer<GpuPresentHoister> {
public:
    Allocator &al;
    SymbolTable *host_scope;
    Vec<ASR::stmt_t*> &before_launch;
    std::map<ASR::symbol_t*, ASR::symbol_t*> hoisted;

    GpuPresentHoister(Allocator &al_, SymbolTable *host_scope_,
            Vec<ASR::stmt_t*> &before_launch_)
        : al(al_), host_scope(host_scope_), before_launch(before_launch_) {}

    void replace_IntrinsicElementalFunction(
            ASR::IntrinsicElementalFunction_t *x) {
        ASR::BaseExprReplacer<GpuPresentHoister>::
            replace_IntrinsicElementalFunction(x);
        if (x->m_intrinsic_id != static_cast<int64_t>(
                ASRUtils::IntrinsicElementalFunctions::Present)
                || x->n_args != 1 || !ASR::is_a<ASR::Var_t>(*x->m_args[0])) {
            return;
        }
        ASR::symbol_t *dummy = ASR::down_cast<ASR::Var_t>(x->m_args[0])->m_v;
        if (host_scope->resolve_symbol(ASRUtils::symbol_name(dummy))
                != dummy) {
            return;
        }
        auto it = hoisted.find(dummy);
        if (it == hoisted.end()) {
            const Location &loc = x->base.base.loc;
            ASR::symbol_t *flag = gpu_new_variable(al, loc, host_scope,
                host_scope->get_unique_name(std::string("__gpu_present_")
                    + ASRUtils::symbol_name(dummy)),
                ASRUtils::duplicate_type(al, x->m_type));
            ASRUtils::ExprStmtDuplicator dup(al);
            before_launch.push_back(al, ASRUtils::STMT(
                ASR::make_Assignment_t(al, loc,
                    ASRUtils::EXPR(ASR::make_Var_t(al, loc, flag)),
                    dup.duplicate_expr(ASRUtils::EXPR((ASR::asr_t*)x)),
                    nullptr, false, false)));
            it = hoisted.emplace(dummy, flag).first;
        }
        *current_expr = ASRUtils::EXPR(ASR::make_Var_t(al,
            x->base.base.loc, it->second));
    }
};

class GpuPresentHoisterVisitor : public
        ASR::CallReplacerOnExpressionsVisitor<GpuPresentHoisterVisitor> {
public:
    GpuPresentHoister replacer;

    GpuPresentHoisterVisitor(Allocator &al, SymbolTable *host_scope,
            Vec<ASR::stmt_t*> &before_launch)
        : replacer(al, host_scope, before_launch) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (s == nullptr || !ASR::is_a<ASR::Block_t>(*s)) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(s);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (s == nullptr || !ASR::is_a<ASR::AssociateBlock_t>(*s)) return;
        ASR::AssociateBlock_t *block = ASR::down_cast<ASR::AssociateBlock_t>(s);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

} // namespace

void gpu_hoist_present_tests(Allocator &al, SymbolTable *host_scope,
        ASR::stmt_t **body, size_t n_body, Vec<ASR::stmt_t*> &before_launch) {
    GpuPresentHoisterVisitor hoister(al, host_scope, before_launch);
    for (size_t i = 0; i < n_body; i++) hoister.visit_stmt(*body[i]);
}

} // namespace LCompilers

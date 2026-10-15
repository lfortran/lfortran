#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_data_layout.h>
#include <libasr/codegen/gpu_utils.h>

namespace LCompilers {

ASR::symbol_t* gpu_new_variable(Allocator &al, const Location &loc,
        SymbolTable *scope, const std::string &name, ASR::ttype_t *type,
        ASR::intentType intent,
        ASR::symbol_t *type_decl) {
    return ASR::down_cast<ASR::Var_t>(ASRUtils::ASRBuilder(al, loc).Variable(
        scope, name, type, intent, type_decl))->m_v;
}

static void push_argument(const Location &loc, Vec<ASR::call_arg_t> &args,
        Allocator &al, ASR::expr_t *value) {
    ASR::call_arg_t arg;
    arg.loc = loc;
    arg.m_value = value;
    args.push_back(al, arg);
}

void gpu_struct_member_extent_arguments(Allocator &al, const Location &loc,
        SymbolTable *kernel_scope, const std::string &name, ASR::Struct_t *st,
        const std::function<ASR::expr_t*(ASR::symbol_t*)> &host_member,
        Vec<ASR::expr_t*> &kernel_args, Vec<ASR::call_arg_t> &call_args) {
    ASR::ttype_t *int_type_sz = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    auto parameter = [&](const std::string &parameter_name) {
        ASR::symbol_t *sym = gpu_new_variable(al, loc, kernel_scope,
            parameter_name, ASRUtils::duplicate_type(al, int_type_sz),
            ASR::intentType::InOut);
        kernel_args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc,
            sym)));
    };
    auto dimension = [&](size_t d) {
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
            (int64_t)(d + 1), int_type_sz, ASR::integerbozType::Decimal));
    };
    for (auto &component : gpu_decomposed_components(st)) {
        std::string mem_name = component.name();
        ASR::symbol_t *mem_sym = component.component;
        // The total size, which size() of the component reads.
        parameter(GpuNames::member_size(name, mem_name));
        push_argument(loc, call_args, al, ASRUtils::EXPR(
            ASR::make_ArraySize_t(al, loc, host_member(mem_sym), nullptr,
                int_type_sz, nullptr)));
        // The lower bound of each dimension, which subscripts, lbound()
        // and ubound() of the component in device code are counted from.
        size_t rank = component.rank;
        for (size_t d = 0; d < rank; d++) {
            parameter(GpuNames::member_lower_bound(name, mem_name, d));
            push_argument(loc, call_args, al, ASRUtils::EXPR(
                ASR::make_ArrayBound_t(al, loc, host_member(mem_sym),
                    dimension(d), int_type_sz, ASR::arrayboundType::LBound,
                    nullptr)));
        }
        // For a component of rank > 1 the total size is not the extent of
        // any single dimension, so also pass each per-dimension extent;
        // size(struct%member, dim) in the kernel reads these.
        if (rank <= 1) continue;
        for (size_t d = 0; d < rank; d++) {
            parameter(GpuNames::member_dim_size(name, mem_name, d));
            push_argument(loc, call_args, al, ASRUtils::EXPR(
                ASR::make_ArraySize_t(al, loc, host_member(mem_sym),
                    dimension(d), int_type_sz, nullptr)));
        }
    }
}

void gpu_struct_member_data_arguments(Allocator &al, const Location &loc,
        SymbolTable *kernel_scope, const std::string &name, ASR::Struct_t *st,
        const std::function<ASR::expr_t*(ASR::symbol_t*)> &host_member,
        const std::function<ASR::symbol_t*(ASR::Struct_t*)>
            &element_declaration,
        Vec<ASR::expr_t*> &kernel_args, Vec<ASR::call_arg_t> &call_args) {
    for (auto &component : gpu_decomposed_components(st)) {
        std::string data_name = GpuNames::member_data(name, component.name());
        ASR::Variable_t *mv = ASR::down_cast<ASR::Variable_t>(
            component.component);
        ASR::ttype_t *data_type = ASRUtils::duplicate_type(al,
            component.type);
        ASR::symbol_t *data_type_decl = nullptr;
        if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(data_type))
                && mv->m_type_declaration) {
            ASR::symbol_t *inner_struct_sym =
                ASRUtils::symbol_get_past_external(mv->m_type_declaration);
            if (ASR::is_a<ASR::Struct_t>(*inner_struct_sym)) {
                data_type_decl = element_declaration(
                    ASR::down_cast<ASR::Struct_t>(inner_struct_sym));
            }
        }
        SetChar deps_vec;
        deps_vec.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, deps_vec, data_type,
            nullptr, nullptr, data_name);
        ASR::symbol_t *data_sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, kernel_scope,
                s2c(al, data_name), deps_vec.p, deps_vec.size(),
                ASR::intentType::InOut, nullptr, nullptr,
                ASR::storage_typeType::Default, data_type, data_type_decl,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false));
        kernel_scope->add_symbol(data_name, data_sym);
        kernel_args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc,
            data_sym)));
        push_argument(loc, call_args, al, host_member(component.component));
    }
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

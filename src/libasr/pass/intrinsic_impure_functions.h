#ifndef LIBASR_PASS_INTRINSIC_IMPURE_FUNCTIONS_H
#define LIBASR_PASS_INTRINSIC_IMPURE_FUNCTIONS_H

#include <libasr/pass/intrinsic_function_registry_util.h>

namespace LCompilers {

namespace ASRUtils {

namespace IsIostatEnd {

    static inline ASR::asr_t* create_IsIostatEnd(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args,
            diag::Diagnostics& /*diag*/) {
        // Compile time value cannot be computed
        return ASR::make_IntrinsicImpureFunction_t(al, loc,
                static_cast<int64_t>(ASRUtils::IntrinsicImpureFunctions::IsIostatEnd),
                args.p, args.n, 0, logical, nullptr);
    }

} // namespace IsIostatEnd

namespace Allocated {

    static inline ASR::asr_t* create_Allocated(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args, diag::Diagnostics& diag) {
        // Compile time value cannot be computed
        if( args.n != 1 ) {
            append_error(diag, "Intrinsic `allocated` accepts exactly one argument", \
                loc);                                                           \
            return nullptr;
        }
        if( !ASRUtils::is_allocatable(args.p[0]) ) {
            append_error(diag, "Intrinsic `allocated` can be called only on" \
                " allocatable argument", loc);
            return nullptr;
        }
        return ASR::make_IntrinsicImpureFunction_t(al, loc,
                static_cast<int64_t>(ASRUtils::IntrinsicImpureFunctions::Allocated),
                args.p, args.n, 0, logical, nullptr);
    }

} // namespace Allocated

namespace IsIostatEor {

    static inline ASR::asr_t* create_IsIostatEor(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args,
            diag::Diagnostics& /*diag*/) {
        // Compile time value cannot be computed
        return ASR::make_IntrinsicImpureFunction_t(al, loc,
                static_cast<int64_t>(ASRUtils::IntrinsicImpureFunctions::IsIostatEor),
                args.p, args.n, 0, logical, nullptr);
    }

} // namespace IsIostatEor

namespace GetCwd {

    static inline bool definable_output(ASR::expr_t* arg) {
        if (!is_modifiable_actual_argument_expr(arg)) return false;
        switch (arg->type) {
            case ASR::exprType::Var: {
                ASR::Variable_t* var = EXPR2VAR(arg);
                return var->m_intent != ASR::intentType::In || is_pointer(var->m_type);
            }
            case ASR::exprType::ArrayItem:
                return definable_output(ASR::down_cast<ASR::ArrayItem_t>(arg)->m_v);
            case ASR::exprType::StringSection:
                return definable_output(ASR::down_cast<ASR::StringSection_t>(arg)->m_arg);
            case ASR::exprType::StringItem:
                return definable_output(ASR::down_cast<ASR::StringItem_t>(arg)->m_arg);
            case ASR::exprType::StructInstanceMember:
                if (is_pointer(expr_type(arg))) return true;
                return definable_output(ASR::down_cast<ASR::StructInstanceMember_t>(arg)->m_v);
            default:
                return false;
        }
    }

    static inline bool valid_output(ASR::expr_t* arg) {
        ASR::ttype_t* type = expr_type(arg);
        return is_character(*type) && !is_array(type) &&
            extract_kind_from_ttype_t(type) == 1 && definable_output(arg);
    }

    static inline ASR::asr_t* create_GetCwd(Allocator& al, const Location& loc,
            Vec<ASR::expr_t*>& args, diag::Diagnostics& diag) {
        if (args.n != 1 || !valid_output(args[0])) {
            append_error(diag, "getcwd requires one scalar default-kind character output variable", loc);
            return nullptr;
        }
        return ASR::make_IntrinsicImpureFunction_t(al, loc,
            static_cast<int64_t>(IntrinsicImpureFunctions::GetCwd),
            args.p, args.n, 0, int32, nullptr);
    }

    static inline void verify_args(const ASR::IntrinsicImpureFunction_t& x,
            diag::Diagnostics& diagnostics) {
        require_impl(x.n_args == 1, "getcwd requires one argument",
            x.base.base.loc, diagnostics);
        if (x.n_args == 1) {
            require_impl(valid_output(x.m_args[0]),
                "getcwd requires a scalar default-kind character output variable",
                x.base.base.loc, diagnostics);
        }
        require_impl(x.m_type && is_integer(*x.m_type) &&
            extract_kind_from_ttype_t(x.m_type) == 4,
            "getcwd result must be integer(4)", x.base.base.loc, diagnostics);
    }

    // Both GNU forms use the same explicitly typed, length-aware C interface.
    static inline ASR::expr_t* runtime_call(Allocator& al, const Location& loc,
            SymbolTable* scope, ASR::expr_t* buffer, SetChar& dep) {
        ASRBuilder b(al, loc);
        std::string name = "_lfortran_getcwd";
        Vec<ASR::ttype_t*> types; types.reserve(al, 2);
        types.push_back(al, b.UnboundedArray(b.String(b.i32(1), ASR::ExpressionLength, ASR::CChar), 1));
        types.push_back(al, int64);
        ASR::symbol_t* interface = b.create_c_func(name, scope, int32, 2, types);
        ASR::Variable_t* output = EXPR2VAR(ASR::down_cast<ASR::Function_t>(interface)->m_args[0]);
        output->m_intent = ASR::intentType::Out;
        output->m_value_attr = false;
        scope->add_symbol(name, interface);
        dep.push_back(al, s2c(al, name));
        Vec<ASR::expr_t*> actuals; actuals.reserve(al, 2);
        actuals.push_back(al, create_string_physical_cast(al, buffer, ASR::CChar));
        actuals.push_back(al, EXPR(ASR::make_StringLen_t(al, loc, buffer, int64, nullptr)));
        return b.Call(interface, actuals, int32);
    }

    static inline ASR::expr_t* instantiate_GetCwd(Allocator& al, const Location& loc,
            SymbolTable* scope, Vec<ASR::call_arg_t>& new_args) {
        declare_basic_variables("_lcompilers_getcwd_function");
        fill_func_arg_sub("buffer", b.String(nullptr, ASR::AssumedLength), Out);
        ASR::expr_t* result = declare("status", int32, ReturnVar);
        body.push_back(al, b.Assignment(result, runtime_call(al, loc, fn_symtab, args[0], dep)));
        ASR::symbol_t* function = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, result, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, function);
        return b.Call(function, new_args, int32);
    }

} // namespace GetCwd

} // namespace ASRUtils

} // namespace LCompilers

#endif // LIBASR_PASS_INTRINSIC_IMPURE_FUNCTIONS_H

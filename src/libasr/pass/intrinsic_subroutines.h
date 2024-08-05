#ifndef LIBASR_PASS_INTRINSIC_SUBROUTINES_H
#define LIBASR_PASS_INTRINSIC_SUBROUTINES_H


#include <libasr/asr_builder.h>
#include <libasr/casting_utils.h>

namespace LCompilers::ASRUtils {

/*
To add a new subroutine implementation,

1. Create a new namespace like, `Sin`, `LogGamma` in this file.
2. In the above created namespace add `eval_*`, `instantiate_*`, and `create_*`.
3. Then register in the maps present in `IntrinsicImpureSubroutineRegistry`.

You can use helper macros and define your own helper macros to reduce
the code size.
*/

enum class IntrinsicImpureSubroutines : int64_t {
    RandomNumber,
    RandomInit,
    GetCommand,
    // ...
};

typedef ASR::stmt_t* (*impl_subroutine)(
    Allocator&, const Location &,
    SymbolTable*, Vec<ASR::ttype_t*>&,
    Vec<ASR::call_arg_t>&, int64_t);

typedef ASR::asr_t* (*create_intrinsic_subroutine)(
    Allocator&, const Location&,
    Vec<ASR::expr_t*>&,
    diag::Diagnostics&);

typedef void (*verify_subroutine)(
    const ASR::IntrinsicImpureSubroutine_t&,
    diag::Diagnostics&);

typedef ASR::expr_t* (*get_initial_value_sub)(Allocator&, ASR::ttype_t*);

namespace RandomInit {

    static inline void verify_args(const ASR::IntrinsicImpureSubroutine_t& x, diag::Diagnostics& diagnostics) {
        if (x.n_args == 2) {
            ASRUtils::require_impl(x.m_overload_id == 0, "Overload Id for random_init expected to be 0, found " + std::to_string(x.m_overload_id), x.base.base.loc, diagnostics);
            ASRUtils::require_impl(ASRUtils::is_logical(*ASRUtils::expr_type(x.m_args[0])), "First argument must be of logical type", x.base.base.loc, diagnostics);
            ASRUtils::require_impl(ASRUtils::is_logical(*ASRUtils::expr_type(x.m_args[1])), "Second argument must be of logical type", x.base.base.loc, diagnostics);
        } else {
            ASRUtils::require_impl(false, "Unexpected number of args, random_init takes 2 arguments, found " + std::to_string(x.n_args), x.base.base.loc, diagnostics);
        }
    }

    static inline ASR::asr_t* create_RandomInit(Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args, diag::Diagnostics& /*diag*/) {
        Vec<ASR::expr_t*> m_args; m_args.reserve(al, 2);
        m_args.push_back(al, args[0]);
        m_args.push_back(al, args[1]);
        return ASR::make_IntrinsicImpureSubroutine_t(al, loc, static_cast<int64_t>(IntrinsicImpureSubroutines::RandomInit), m_args.p, m_args.n, 0);
    }

    static inline ASR::stmt_t* instantiate_RandomInit(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        
        std::string c_func_name = "_lfortran_random_init";
        std::string new_name = "_lcompilers_random_init_";

        declare_basic_variables(new_name);
        fill_func_arg_sub("repeatable", arg_types[0], InOut);
        fill_func_arg_sub("image_distinct", arg_types[1], InOut);
        SymbolTable *fn_symtab_1 = al.make_new<SymbolTable>(fn_symtab);
        Vec<ASR::expr_t*> args_1; args_1.reserve(al, 0);
        ASR::expr_t *return_var_1 = b.Variable(fn_symtab_1, c_func_name,
           ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(arg_types[0])),
           ASRUtils::intent_return_var, ASR::abiType::BindC, false);
        SetChar dep_1; dep_1.reserve(al, 1);
        Vec<ASR::stmt_t*> body_1; body_1.reserve(al, 1);
        ASR::symbol_t *s = make_ASR_Function_t(c_func_name, fn_symtab_1, dep_1, args_1,
            body_1, return_var_1, ASR::abiType::BindC, ASR::deftypeType::Interface, s2c(al, c_func_name));
        fn_symtab->add_symbol(c_func_name, s);
        dep.push_back(al, s2c(al, c_func_name));
        Vec<ASR::expr_t*> call_args; call_args.reserve(al, 0);
        body.push_back(al, b.Assignment(args[0], b.Call(s, call_args, arg_types[0])));
        body.push_back(al, b.Assignment(args[1], b.Call(s, call_args, arg_types[1])));
        ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, nullptr, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, new_symbol);
        return b.SubroutineCall(new_symbol, new_args);
    }
} // namespace RandomInit

namespace RandomNumber {

    static inline void verify_args(const ASR::IntrinsicImpureSubroutine_t& x, diag::Diagnostics& diagnostics) {
        if (x.n_args == 1)  {
            ASRUtils::require_impl(x.m_overload_id == 0, "Overload Id for random_number expected to be 0, found " + std::to_string(x.m_overload_id), x.base.base.loc, diagnostics);
        }
        else {
            ASRUtils::require_impl(false, "Unexpected number of args, random_number takes 1 arguments, found " + std::to_string(x.n_args), x.base.base.loc, diagnostics);
        }
    }

    static inline ASR::asr_t* create_RandomNumber(Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args, diag::Diagnostics& /*diag*/) {
        Vec<ASR::expr_t*> m_args; m_args.reserve(al, 1); m_args.push_back(al, args[0]);
        return ASR::make_IntrinsicImpureSubroutine_t(al, loc, static_cast<int64_t>(IntrinsicImpureSubroutines::RandomNumber), m_args.p, m_args.n, 0);
    }

    static inline ASR::stmt_t* instantiate_RandomNumber(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        std::string c_func_name;
        int kind = ASRUtils::extract_kind_from_ttype_t(arg_types[0]);
        if ( kind == 4 ) {
            c_func_name = "_lfortran_sp_rand_num";
        } else {
            c_func_name = "_lfortran_dp_rand_num";
        }
        std::string new_name = "_lcompilers_random_number_";

        declare_basic_variables(new_name);
        fill_func_arg_sub("r", arg_types[0], InOut);
        SymbolTable *fn_symtab_1 = al.make_new<SymbolTable>(fn_symtab);
        Vec<ASR::expr_t*> args_1; args_1.reserve(al, 0);
        ASR::expr_t *return_var_1 = b.Variable(fn_symtab_1, c_func_name,
           ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(arg_types[0])),
           ASRUtils::intent_return_var, ASR::abiType::BindC, false);
        SetChar dep_1; dep_1.reserve(al, 1);
        Vec<ASR::stmt_t*> body_1; body_1.reserve(al, 1);
        ASR::symbol_t *s = make_ASR_Function_t(c_func_name, fn_symtab_1, dep_1, args_1,
            body_1, return_var_1, ASR::abiType::BindC, ASR::deftypeType::Interface, s2c(al, c_func_name));
        fn_symtab->add_symbol(c_func_name, s);
        dep.push_back(al, s2c(al, c_func_name));

        if (ASRUtils::is_array(ASRUtils::expr_type(args[0]))) {
            /*
                real :: b(3)
                call random_number(b)
                    To
                real :: b(3)
                do i=lbound(b,1),ubound(b,1)
                    call random_number(b(i))
                end do
            */
            ASR::dimension_t* array_dims = nullptr;
            int array_rank = extract_dimensions_from_ttype(arg_types[0], array_dims);
            std::vector<ASR::expr_t*> do_loop_variables;
            for (int i = 0; i < array_rank; i++) {
                do_loop_variables.push_back(declare("i_" + std::to_string(i), int32, Local));
            }
            ASR::stmt_t* func_call = b.CallIntrinsicSubroutine(scope, {ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(arg_types[0]))},
                                    {b.ArrayItem_01(args[0], do_loop_variables)}, 0, RandomNumber::instantiate_RandomNumber);
            fn_name = scope->get_unique_name(fn_name, false);
            body.push_back(al, PassUtils::create_do_loop_helper_random_number(al, loc, do_loop_variables, s, args[0],
                    ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(arg_types[0])),
                    b.ArrayItem_01(args[0], do_loop_variables), func_call, 1));
        } else {
            Vec<ASR::expr_t*> call_args; call_args.reserve(al, 0);
            body.push_back(al, b.Assignment(args[0], b.Call(s, call_args, arg_types[0])));
        }
        ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, nullptr, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, new_symbol);
        return b.SubroutineCall(new_symbol, new_args);
    }

} // namespace RandomNumber

namespace GetCommand {

    static inline void verify_args(const ASR::IntrinsicImpureSubroutine_t& x, diag::Diagnostics& diagnostics) {
        if (x.n_args == 3) {
            ASRUtils::require_impl(x.m_overload_id == 0, "Overload Id for get_command expected to be 0, found " + std::to_string(x.m_overload_id), x.base.base.loc, diagnostics);
            ASRUtils::require_impl(ASRUtils::is_character(*ASRUtils::expr_type(x.m_args[0])), "First argument must be of character type", x.base.base.loc, diagnostics);
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[1])), "Second argument must be of integer type", x.base.base.loc, diagnostics);
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[2])), "Third argument must be of integer type", x.base.base.loc, diagnostics);
        } else {
            ASRUtils::require_impl(false, "Unexpected number of args, get_command takes 3 arguments, found " + std::to_string(x.n_args), x.base.base.loc, diagnostics);
        }
    }

    static inline ASR::asr_t* create_GetCommand(Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args, diag::Diagnostics& /*diag*/) {
        Vec<ASR::expr_t*> m_args; m_args.reserve(al, 3);
        m_args.push_back(al, args[0]);
        m_args.push_back(al, args[1]);
        m_args.push_back(al, args[2]);
        return ASR::make_IntrinsicImpureSubroutine_t(al, loc, static_cast<int64_t>(IntrinsicImpureSubroutines::GetCommand), m_args.p, m_args.n, 0);
    }

    static inline ASR::stmt_t* instantiate_GetCommand(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        
        std::string c_func_name_1 = "_lfortran_get_command_command";
        std::string c_func_name_2 = "_lfortran_get_command_length";
        std::string c_func_name_3 = "_lfortran_get_command_status";

        std::string new_name = "_lcompilers_get_command_";
        declare_basic_variables(new_name);
        fill_func_arg_sub("command", arg_types[0], InOut);
        fill_func_arg_sub("length", arg_types[1], InOut);
        fill_func_arg_sub("status", arg_types[2], InOut);

        ASR::symbol_t *s_1 = b.create_c_func_subroutines(c_func_name_1, fn_symtab, 0, arg_types[0]);
        ASR::symbol_t *s_2 = b.create_c_func_subroutines(c_func_name_2, fn_symtab, 0, arg_types[1]);
        ASR::symbol_t *s_3 = b.create_c_func_subroutines(c_func_name_3, fn_symtab, 0, arg_types[2]);
    
        fn_symtab->add_symbol(c_func_name_1, s_1);
        fn_symtab->add_symbol(c_func_name_2, s_2);
        fn_symtab->add_symbol(c_func_name_3, s_3);

        dep.push_back(al, s2c(al, c_func_name_1));
        dep.push_back(al, s2c(al, c_func_name_2));
        dep.push_back(al, s2c(al, c_func_name_3));

        Vec<ASR::expr_t*> call_args; call_args.reserve(al, 0);
        body.push_back(al, b.Assignment(args[0], b.Call(s_1, call_args, arg_types[0])));
        body.push_back(al, b.Assignment(args[1], b.Call(s_2, call_args, arg_types[1])));
        body.push_back(al, b.Assignment(args[2], b.Call(s_3, call_args, arg_types[2])));

        ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, nullptr, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, new_symbol);
        return b.SubroutineCall(new_symbol, new_args);
    }
} // namespace GetCommand

} // namespace LCompilers::ASRUtils

#endif // LIBASR_PASS_INTRINSIC_SUBROUTINES_H

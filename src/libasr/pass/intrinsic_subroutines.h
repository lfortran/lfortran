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
    RandomSeed,
    GetCommand,
    GetEnvironmentVariable,
    ExecuteCommandLine,
    GetCommandArgument,
    CpuTime,
    Srand,
    SystemClock,
    DateAndTime,
    MoveAlloc,
    Mvbits,
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
           ASRUtils::intent_return_var, nullptr, ASR::abiType::BindC, false);
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

namespace RandomSeed {

    static inline void verify_args(const ASR::IntrinsicImpureSubroutine_t& x, diag::Diagnostics& diagnostics) {
        ASRUtils::require_impl(x.n_args <= 3, "random_seed can have maximum 3 args", x.base.base.loc, diagnostics);
        if (x.n_args == 1) {
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[0])), "Arguments to random_seed must be of integer type", x.base.base.loc, diagnostics);
        } else if (x.n_args == 2) {
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[0])) && ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[0])), "Arguments to random_seed must be of integer type", x.base.base.loc, diagnostics);
        } else if (x.n_args == 3) {
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[0])) && ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[1])) && ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[2])), "Arguments to random_seed must be of integer type", x.base.base.loc, diagnostics);
        }
    }

    static inline ASR::asr_t* create_RandomSeed(Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args, diag::Diagnostics& /*diag*/) {
        Vec<ASR::expr_t*> m_args; m_args.reserve(al, 0);
        ASRBuilder b(al, loc);
        for (int i = 0; i < int(args.size()); i++) {
            if(args[i]) {
                m_args.push_back(al, args[i]);
            } else {
                m_args.push_back(al, b.f32(1));
            }
        }
        return ASR::make_IntrinsicImpureSubroutine_t(al, loc, static_cast<int64_t>(IntrinsicImpureSubroutines::RandomSeed), m_args.p, m_args.n, 0);
    }

    static inline ASR::stmt_t* instantiate_RandomSeed(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {

        std::string c_func_name_1 = "_lfortran_random_seed";
        std::string c_func_name_2 = "_lfortran_dp_rand_num";
        std::string new_name = "_lcompilers_random_seed_";
        declare_basic_variables(new_name);
        int flag = 0;
        if (!is_real(*arg_types[0])) {
            fill_func_arg_sub("size", arg_types[0], InOut);
            ASR::symbol_t *s_1 = b.create_c_func_subroutines(c_func_name_1, fn_symtab, 1, arg_types[0]);
            fn_symtab->add_symbol(c_func_name_1, s_1);
            dep.push_back(al, s2c(al, c_func_name_1));
            Vec<ASR::expr_t*> call_args; call_args.reserve(al, 1);
            call_args.push_back(al, b.i32(8));
            body.push_back(al, b.Assignment(args[0], b.Call(s_1, call_args, arg_types[0])));
        } else {
            fill_func_arg_sub("size", real32, InOut);
            body.push_back(al, b.Assignment(args[0], b.f32(0)));
        }
        if (!is_real(*arg_types[1])) {
            flag = 1;
            fill_func_arg_sub("put", arg_types[1], InOut);
            body.push_back(al, b.Assignment(args[1], args[1]));
        } else {
            fill_func_arg_sub("put", real32, InOut);
            body.push_back(al, b.Assignment(args[1], b.f32(0)));
        }
        if (!is_real(*arg_types[2])) {
            fill_func_arg_sub("get", arg_types[2], InOut);
            if (flag == 1) {
                body.push_back(al, b.Assignment(args[2], args[1]));
            } else {
                std::vector<LCompilers::ASR::expr_t *> vals;
                std::string c_func = c_func_name_2;
                int kind = ASRUtils::extract_kind_from_ttype_t(arg_types[2]);
                if ( is_real(*arg_types[2]) ) {
                    if (kind == 4) {
                        c_func = "_lfortran_sp_rand_num";
                    } else {
                        c_func = "_lfortran_dp_rand_num";
                    }
                } else if ( is_integer(*arg_types[2]) ) {
                    if (kind == 4) {
                        c_func = "_lfortran_int32_rand_num";
                    } else {
                        c_func = "_lfortran_int64_rand_num";
                    }
                }
                ASR::symbol_t *s_2 = b.create_c_func_subroutines(c_func, fn_symtab, 0, arg_types[2]);
                fn_symtab->add_symbol(c_func, s_2);
                dep.push_back(al, s2c(al, c_func));
                Vec<ASR::expr_t*> call_args2; call_args2.reserve(al, 0);
                ASR::ttype_t* elem_type = extract_type(arg_types[2]);
                for (int i = 0; i < 8; i++) {
                    auto xx = declare("i_" + std::to_string(i), elem_type, Local);
                    body.push_back(al, b.Assignment(xx, b.Call(s_2, call_args2, int32)));
                    vals.push_back(xx);
                }
                body.push_back(al, b.Assignment(args[2], b.ArrayConstant(vals, extract_type(arg_types[2]), false)));
            }
        } else {
            fill_func_arg_sub("get", real32, InOut);
            body.push_back(al, b.Assignment(args[2], b.f32(0)));
        }
       
        ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, nullptr, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, new_symbol);
        return b.SubroutineCall(new_symbol, new_args);
    }

} // namespace RandomSeed

namespace Srand {

    static inline void verify_args(const ASR::IntrinsicImpureSubroutine_t& x, diag::Diagnostics& diagnostics) {
        ASRUtils::require_impl(x.n_args == 1, "srand takes 1 argument only", x.base.base.loc, diagnostics);
        if (x.n_args == 1) {
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[0])), "Arguments to srand must be of integer type", x.base.base.loc, diagnostics);
        }
    }

    static inline ASR::asr_t* create_Srand(Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args, diag::Diagnostics& diag) {
        diag.semantic_warning_label(
                "`srand` is an LFortran extension", { loc }, "Use `random_init` instead");
        Vec<ASR::expr_t*> m_args; m_args.reserve(al, 1); m_args.push_back(al, args[0]);
        return ASR::make_IntrinsicImpureSubroutine_t(al, loc, static_cast<int64_t>(IntrinsicImpureSubroutines::Srand), m_args.p, m_args.n, 0);
    }

    static inline ASR::stmt_t* instantiate_Srand(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {

        std::string c_func_name = "_lfortran_init_random_seed";
        std::string new_name = "_lcompilers_srand_";
        declare_basic_variables(new_name);
        fill_func_arg_sub("r", arg_types[0], InOut);
        SymbolTable *fn_symtab_1 = al.make_new<SymbolTable>(fn_symtab);
        Vec<ASR::expr_t*> args_1; args_1.reserve(al, 1);
        ASR::expr_t *arg = b.Variable(fn_symtab_1, "n", arg_types[0],
            ASR::intentType::In, nullptr, ASR::abiType::BindC, true);
        args_1.push_back(al, arg);

        ASR::expr_t *return_var_1 = b.Variable(fn_symtab_1, c_func_name,
           ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(arg_types[0])),
           ASRUtils::intent_return_var, nullptr, ASR::abiType::BindC, false);
           
        SetChar dep_1; dep_1.reserve(al, 1);
        Vec<ASR::stmt_t*> body_1; body_1.reserve(al, 1);
        ASR::symbol_t *s = make_ASR_Function_t(c_func_name, fn_symtab_1, dep_1, args_1,
            body_1, return_var_1, ASR::abiType::BindC, ASR::deftypeType::Interface, s2c(al, c_func_name));
        fn_symtab->add_symbol(c_func_name, s);
        dep.push_back(al, s2c(al, c_func_name));

        Vec<ASR::expr_t*> call_args; call_args.reserve(al, 1);
        call_args.push_back(al, args[0]);
        body.push_back(al, b.Assignment(args[0], b.Call(s, call_args, arg_types[0])));
        
        ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, nullptr, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, new_symbol);
        return b.SubroutineCall(new_symbol, new_args);
    }

} // namespace Srand

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
        fill_func_arg_sub("r", ASRUtils::duplicate_type_with_empty_dims(al, arg_types[0]), InOut);
        SymbolTable *fn_symtab_1 = al.make_new<SymbolTable>(fn_symtab);
        Vec<ASR::expr_t*> args_1; args_1.reserve(al, 0);
        ASR::expr_t *return_var_1 = b.Variable(fn_symtab_1, c_func_name,
           ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(arg_types[0])),
           ASRUtils::intent_return_var, nullptr, ASR::abiType::BindC, false);
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
        ASRUtils::require_impl(x.m_overload_id == 0, "Overload Id for get_command expected to be 0, found " + std::to_string(x.m_overload_id), x.base.base.loc, diagnostics);
        if( x.n_args > 0 ) {
            ASRUtils::require_impl(ASRUtils::is_character(*ASRUtils::expr_type(x.m_args[0])), 
                            "First argument must be of character type", x.base.base.loc, diagnostics);
        }
        if( x.n_args > 1 ) {
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[1])), 
                            "Second argument must be of integer type", x.base.base.loc, diagnostics);
        }
        if( x.n_args > 2 ) {
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[2])), 
                            "Third argument must be of integer type", x.base.base.loc, diagnostics);
        }
        if( x.n_args > 3 ) {
            ASRUtils::require_impl(false, "Unexpected number of args, get_command takes 3 arguments, found " + 
                            std::to_string(x.n_args), x.base.base.loc, diagnostics);
        }
    }

    static inline ASR::asr_t* create_GetCommand(Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args, diag::Diagnostics& /*diag*/) {
        Vec<ASR::expr_t*> m_args; m_args.reserve(al, 3);
        if(args[0]) m_args.push_back(al, args[0]);
        if(args[1]) m_args.push_back(al, args[1]);
        if(args[2]) m_args.push_back(al, args[2]);
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
        Vec<ASR::expr_t*> call_args; call_args.reserve(al, 0);

        if(arg_types.size() > 0){
            /*
                interface
                    function _lfortran_get_command_length() result(ret) bind(c)
                        integer :: ret
                    end function

                    subroutine lfortran_get_command_command(receiver) bind(c)
                        character(len=1, kind=c_char), intent(out) :: receiver(*)
                    end subroutine lfortran_get_command_command
                end interface

                integer :: length_to_allocate
                length_to_allocate = _lfortran_get_command_length()

                character(:), allocatable :: result
                allocate(character(len=length_to_allocate) :: result)
                call lfortran_get_command_command(result)

                character(len=*), intent(inout) :: command
                command = result

                deallocate(result)
            */

            // Create interface `lfortran_get_command_command`
            ASR::ttype_t* array_type = b.UnboundedArray(b.String(b.i64(1), ASR::ExpressionLength, ASR::CString), 1);
            Vec<ASR::ttype_t*> parameter_types; parameter_types.reserve(al,1);
            parameter_types.push_back(al, array_type);
            ASR::symbol_t *lfortran_get_command_command = b.create_c_subroutine_interface(c_func_name_1, fn_symtab, parameter_types, {"receiver"});

            fn_symtab->add_symbol(c_func_name_1, lfortran_get_command_command);
            dep.push_back(al, s2c(al, c_func_name_1));

            // Create Interface `lcompilers_get_command_length`
            ASR::symbol_t *_lfortran_get_command_length = b.create_c_func_subroutines(c_func_name_2, fn_symtab, 0, int32);
            fn_symtab->add_symbol(c_func_name_2, _lfortran_get_command_length);
            dep.push_back(al, s2c(al, c_func_name_2));
            
            // Call `_lfortran_get_command_length`
            ASR::expr_t* length_to_allocate = declare("length_to_allocate", int32, Local);
            body.push_back(al, b.Assignment(length_to_allocate, b.Call(_lfortran_get_command_length, call_args, int32)));

            // Create and allocate `string_holder` variable
            ASR::expr_t* string_holder = declare("string_holder",
                b.allocatable(b.String(nullptr, ASR::DeferredLength, ASR::DescriptorString)), Local);
            body.push_back(al, b.Allocate(string_holder, nullptr, 0, length_to_allocate));

            // Call `lfortran_get_command_command`
            Vec<ASR::call_arg_t> call_args_to_lfortran_get_command_command;
            call_args_to_lfortran_get_command_command.reserve(al, 1);
            call_args_to_lfortran_get_command_command.push_back(al, ASR::call_arg_t{loc, string_holder});
            body.push_back(al, b.SubroutineCall(lfortran_get_command_command, call_args_to_lfortran_get_command_command));

            // Assign `string_holder` to `command` + deallocate
            fill_func_arg_sub("command", arg_types[0], InOut);

            body.push_back(al, b.Assignment(args[0],
                ASRUtils::create_string_physical_cast(al, string_holder, 
                    ASR::down_cast<ASR::String_t>(ASRUtils::extract_type(arg_types[0]))->m_physical_type)));
            body.push_back(al, b.Deallocate(string_holder));
        }
        if(arg_types.size() > 1){
            fill_func_arg_sub("length", arg_types[1], InOut);
            // `length = length_to_allocate` (Reuse the variable)
            body.push_back(al, b.Assignment(args[1], b.Var(fn_symtab->get_symbol("length_to_allocate"))));
        }
        if(arg_types.size() > 2){
            fill_func_arg_sub("status", arg_types[2], InOut);
            ASR::symbol_t *s_3 = b.create_c_func_subroutines(c_func_name_3, fn_symtab, 0, arg_types[2]);
            fn_symtab->add_symbol(c_func_name_3, s_3);
            dep.push_back(al, s2c(al, c_func_name_3));
            body.push_back(al, b.Assignment(args[2], b.Call(s_3, call_args, arg_types[2])));
        }

        ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, nullptr, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, new_symbol);
        return b.SubroutineCall(new_symbol, new_args);
    }
    
} // namespace GetCommand

namespace GetCommandArgument {

    static inline void verify_args(const ASR::IntrinsicImpureSubroutine_t& x, diag::Diagnostics& diagnostics) {
        if (x.n_args > 0) {
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[0])), "First argument must be of integer type", x.base.base.loc, diagnostics);
        } 
        if(x.n_args > 1) {
            ASRUtils::require_impl(ASRUtils::is_character(*ASRUtils::expr_type(x.m_args[1])), "Second argument must be of character type", x.base.base.loc, diagnostics);
        }
        if (x.n_args > 2) {
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[2])), "Third argument must be of integer type", x.base.base.loc, diagnostics);
        } 
        if (x.n_args == 4) {
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[3])), "Fourth argument must be of integer type", x.base.base.loc, diagnostics);
        } else {
            ASRUtils::require_impl(false, "Unexpected number of args, get_command_argument takes atmost 4 arguments, found " + std::to_string(x.n_args), x.base.base.loc, diagnostics);
        }
    }

    static inline ASR::asr_t* create_GetCommandArgument(Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args, diag::Diagnostics& /*diag*/) {
        Vec<ASR::expr_t*> m_args; m_args.reserve(al, args.size());
        m_args.push_back(al, args[0]);
        for (int i = 1; i < int(args.size()); i++) {
            if(args[i]) m_args.push_back(al, args[i]);
        }
        return ASR::make_IntrinsicImpureSubroutine_t(al, loc, static_cast<int64_t>(IntrinsicImpureSubroutines::GetCommandArgument), m_args.p, m_args.n, 0);
    }

    static inline ASR::stmt_t* instantiate_GetCommandArgument(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        
        std::string c_func_name_1 = "_lfortran_get_command_argument_value";
        std::string c_func_name_2 = "_lfortran_get_command_argument_length";
        std::string c_func_name_3 = "_lfortran_get_command_argument_status";

        std::string new_name = "_lcompilers_get_command_argument_";
        declare_basic_variables(new_name);
        Vec<ASR::expr_t*> call_args; call_args.reserve(al, 0);
        fill_func_arg_sub("number", arg_types[0], In);

        std::string first_param_c_func_name = ASRUtils::is_integer(*arg_types[1]) ? c_func_name_2 : c_func_name_1;

        if (arg_types.size() > 1) { // TODO : Correct this as it assumes `arg[1]` to be the `value` (this function has all parameters as optional)
            ASR::ttype_t* CString_type = character(-1);
            ASR::down_cast<ASR::String_t>(CString_type)->m_physical_type = ASR::string_physical_typeType::CString;
            fill_func_arg_sub("value", arg_types[1], InOut);
            Vec<ASR::expr_t*> args_1; args_1.reserve(al, 0);
            SymbolTable *fn_symtab_1 = al.make_new<SymbolTable>(fn_symtab);
            ASR::expr_t *arg = b.Variable(fn_symtab_1, "n", arg_types[0],
                ASR::intentType::In, nullptr, ASR::abiType::BindC, true);
            args_1.push_back(al, arg);
            SetChar dep_1; dep_1.reserve(al, 1);
            Vec<ASR::stmt_t*> body_1; body_1.reserve(al, 1);
            ASR::expr_t *return_var_1 = b.Variable(fn_symtab_1, first_param_c_func_name,
                ASRUtils::is_character(*arg_types[1])? CString_type : ASRUtils::extract_type(arg_types[1]),
                ASRUtils::intent_return_var, nullptr, ASR::abiType::BindC, false);
            ASR::symbol_t *s_1 = make_ASR_Function_t(first_param_c_func_name, fn_symtab_1, dep_1, args_1,
            body_1, return_var_1, ASR::abiType::BindC, ASR::deftypeType::Interface, s2c(al, first_param_c_func_name));
            
            fn_symtab->add_symbol(first_param_c_func_name, s_1);
            dep.push_back(al, s2c(al, first_param_c_func_name));
            Vec<ASR::expr_t*> call_args1; call_args1.reserve(al, 1);
            call_args1.push_back(al, args[0]);
            body.push_back(al, b.Assignment(args[1], 
                ASRUtils::is_character(*arg_types[1])?
                    ASRUtils::create_string_physical_cast(al, b.Call(s_1, call_args1, CString_type),
                    ASR::down_cast<ASR::String_t>(ASRUtils::extract_type(arg_types[1]))->m_physical_type)
                    :b.Call(s_1, call_args1, arg_types[1])));
        }
        if (arg_types.size() > 2 && !ASRUtils::is_integer(*arg_types[1])) {
            fill_func_arg_sub("length", arg_types[2], InOut);
            
            ASR::symbol_t *s_2 = b.create_c_func_subroutines(c_func_name_2, fn_symtab, 1, arg_types[2]);
            fn_symtab->add_symbol(c_func_name_2, s_2);
            dep.push_back(al, s2c(al, c_func_name_2));
            Vec<ASR::expr_t*> call_args2; call_args2.reserve(al, 1);
            call_args2.push_back(al, args[0]);
            body.push_back(al, b.Assignment(args[2], b.Call(s_2, call_args2, arg_types[2])));
        } else if ( arg_types.size() > 2 && ASRUtils::is_integer(*arg_types[1]) ) {
            fill_func_arg_sub("status", arg_types[2], InOut);
            ASR::symbol_t *s_3 = b.create_c_func_subroutines(c_func_name_3, fn_symtab, 0, arg_types[2]);
            fn_symtab->add_symbol(c_func_name_3, s_3);
            dep.push_back(al, s2c(al, c_func_name_3));
            body.push_back(al, b.Assignment(args[2], b.Call(s_3, call_args, arg_types[2])));
        }
        if (arg_types.size() == 4) {
            fill_func_arg_sub("status", arg_types[3], InOut);
            ASR::symbol_t *s_3 = b.create_c_func_subroutines(c_func_name_3, fn_symtab, 0, arg_types[3]);
            fn_symtab->add_symbol(c_func_name_3, s_3);
            dep.push_back(al, s2c(al, c_func_name_3));
            body.push_back(al, b.Assignment(args[3], b.Call(s_3, call_args, arg_types[3])));
        }

        ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, nullptr, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, new_symbol);
        return b.SubroutineCall(new_symbol, new_args);
    }
    
} // namespace GetCommandArgument

namespace SystemClock {

    static inline void verify_args(const ASR::IntrinsicImpureSubroutine_t& x, diag::Diagnostics& diagnostics) {
        if (x.n_args > 0) {
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[0])), "`count` argument must be of integer type", x.base.base.loc, diagnostics);
        }
        if (x.n_args > 1) {
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[1])) || ASRUtils::is_real(*ASRUtils::expr_type(x.m_args[1])), "`count_rate` argument must be of integer or real type", x.base.base.loc, diagnostics);
        }
        if (x.n_args > 2) {
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[2])), "`count_max` argument must be of integer type", x.base.base.loc, diagnostics);
        } 
        if (x.n_args > 3) {
            ASRUtils::require_impl(false, "Unexpected number of args, system_clock takes atmost 3 arguments, found " + std::to_string(x.n_args), x.base.base.loc, diagnostics);
        }
    }

    static inline ASR::asr_t* create_SystemClock(Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args, diag::Diagnostics& /*diag*/) {
        int64_t count_id = 0, count_rate_id = 1, count_max_id = 2, count_count_rate_id = 3, count_count_max_id = 4, count_rate_count_max_id = 5, count_count_rate_count_max_id = 6;
        int64_t overload_id = -1;
        Vec<ASR::expr_t*> m_args; m_args.reserve(al, args.size());
        ASRBuilder b(al, loc);
        if(args[0]) overload_id = count_id;
        if(args[1]) overload_id = count_rate_id;
        if(args[2]) overload_id = count_max_id;
        if(args[0] && args[1]) overload_id = count_count_rate_id;
        if(args[0] && args[2]) overload_id = count_count_max_id;
        if(args[1] && args[2]) overload_id = count_rate_count_max_id;
        if(args[0] && args[1] && args[2]) overload_id = count_count_rate_count_max_id;
        for (int i = 0; i < int(args.size()); i++) {
            if(args[i]) m_args.push_back(al, args[i]);
            else m_args.push_back(al, b.i32(1));
        }
        return ASR::make_IntrinsicImpureSubroutine_t(al, loc, static_cast<int64_t>(IntrinsicImpureSubroutines::SystemClock), m_args.p, m_args.n, overload_id);
    }

    static inline ASR::stmt_t* instantiate_SystemClock(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types,
            Vec<ASR::call_arg_t>& new_args, int64_t overload_id) {

        std::string c_func_name_1 = "_lfortran_i32sys_clock_count";
        std::string c_func_name_2 = "_lfortran_i32sys_clock_count_rate";
        std::string c_func_name_3 = "_lfortran_i32sys_clock_count_max";
        std::string new_name = "_lcompilers_system_clock_";
        declare_basic_variables(new_name);
        Vec<ASR::expr_t*> call_args; call_args.reserve(al, 0);
        if (overload_id == 0 || overload_id == 3 || overload_id == 4 || overload_id == 6) {
            if (ASRUtils::extract_kind_from_ttype_t(arg_types[0]) == 8) {
                c_func_name_1 = "_lfortran_i64sys_clock_count";
            }
            fill_func_arg_sub("count", arg_types[0], InOut);
            ASR::symbol_t *s_1 = b.create_c_func_subroutines(c_func_name_1, fn_symtab, 0, arg_types[0]);
            fn_symtab->add_symbol(c_func_name_1, s_1);
            dep.push_back(al, s2c(al, c_func_name_1));
            body.push_back(al, b.Assignment(args[0], b.Call(s_1, call_args, arg_types[0])));
        } else {
            fill_func_arg_sub("count", int32, InOut);
            body.push_back(al, b.Assignment(args[0], b.i32(0)));
        }
        if (overload_id == 1 || overload_id == 3 || overload_id == 5 || overload_id == 6) {
            if (ASRUtils::extract_kind_from_ttype_t(arg_types[1]) == 8) {
                if (is_real(*arg_types[1])) {
                    c_func_name_2 = "_lfortran_i64r64sys_clock_count_rate";
                } else {
                    c_func_name_2 = "_lfortran_i64sys_clock_count_rate";
                }
            } else if (is_real(*arg_types[1])) {
                c_func_name_2 = "_lfortran_i32r32sys_clock_count_rate";
            }
            fill_func_arg_sub("count_rate", arg_types[1], InOut);
            ASR::symbol_t *s_2 = b.create_c_func_subroutines(c_func_name_2, fn_symtab, 0, arg_types[1]);
            fn_symtab->add_symbol(c_func_name_2, s_2);
            dep.push_back(al, s2c(al, c_func_name_2));
            body.push_back(al, b.Assignment(args[1], b.Call(s_2, call_args, arg_types[1])));
        } else {
            fill_func_arg_sub("count_rate", int32, InOut);
            body.push_back(al, b.Assignment(args[1], b.i32(0)));
        }
        if (overload_id == 2 || overload_id == 4 || overload_id == 5 || overload_id == 6) {
            if (ASRUtils::extract_kind_from_ttype_t(arg_types[2]) == 8) {
                c_func_name_3 = "_lfortran_i64sys_clock_count_max";
            }
            fill_func_arg_sub("count_max", arg_types[2], InOut);
            ASR::symbol_t *s_3 = b.create_c_func_subroutines(c_func_name_3, fn_symtab, 0, arg_types[2]);
            fn_symtab->add_symbol(c_func_name_3, s_3);
            dep.push_back(al, s2c(al, c_func_name_3));
            body.push_back(al, b.Assignment(args[2], b.Call(s_3, call_args, arg_types[2])));
        } else {
            fill_func_arg_sub("count_max", int32, InOut);
            body.push_back(al, b.Assignment(args[2], b.i32(0)));
        }

        ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, nullptr, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, new_symbol);
        return b.SubroutineCall(new_symbol, new_args);
    }
    
} // namespace SystemClock

namespace DateAndTime {

    static inline void verify_args(const ASR::IntrinsicImpureSubroutine_t& x, diag::Diagnostics& diagnostics) {
        if (x.n_args > 0) {
            ASRUtils::require_impl(ASRUtils::is_character(*ASRUtils::expr_type(x.m_args[0])), "`date` argument must be of character type", x.base.base.loc, diagnostics);
        }
        if (x.n_args > 1) {
            ASRUtils::require_impl(ASRUtils::is_character(*ASRUtils::expr_type(x.m_args[1])), "`time` argument must be of character or real type", x.base.base.loc, diagnostics);
        }
        if (x.n_args > 2) {
            ASRUtils::require_impl(ASRUtils::is_character(*ASRUtils::expr_type(x.m_args[2])), "`zone` argument must be of character type", x.base.base.loc, diagnostics);
        } 
        if (x.n_args > 3) {
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[3])), "`values` argument must be of integer array type", x.base.base.loc, diagnostics);
        } 
        if (x.n_args > 4) {
            ASRUtils::require_impl(false, "Unexpected number of args, date_and_time takes atmost 4 arguments, found " + std::to_string(x.n_args), x.base.base.loc, diagnostics);
        }
    }

    static inline ASR::asr_t* create_DateAndTime(Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args, diag::Diagnostics& /*diag*/) {
        Vec<ASR::expr_t*> m_args; m_args.reserve(al, args.size());
        ASRBuilder b(al, loc);
        for (int i = 0; i < int(args.size()); i++) {
            if(args[i]) {
                m_args.push_back(al, args[i]);
            } else {
                m_args.push_back(al, b.f32(1));
            }
        }
        return ASR::make_IntrinsicImpureSubroutine_t(al, loc, static_cast<int64_t>(IntrinsicImpureSubroutines::DateAndTime), m_args.p, m_args.n, 0);
    }

    static inline ASR::stmt_t* instantiate_DateAndTime(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {

        std::string c_func_name_1 = "_lfortran_date";
        std::string c_func_name_2 = "_lfortran_time";
        std::string c_func_name_3 = "_lfortran_zone";
        std::string c_func_name_4 = "_lfortran_values";
        std::string new_name = "_lcompilers_date_and_time_";
        declare_basic_variables(new_name);
        Vec<ASR::expr_t*> call_args; call_args.reserve(al, 0);

        if (!is_real(*arg_types[0])) {
            /*
                interface 
                    subroutine _lfortran_date(string_receiver) bind(c)
                        character(len=1, c_char) :: string_receiver(*)
                    end subroutine
                end interface
                character(*) :: date
                character(32) :: date_string_holder
                call _lfortran_date(date_string_holder)
                date = date_string_holder
            */

            // Create `_lfortran_date` interface
            Vec<ASR::ttype_t*> parameter_types; parameter_types.reserve(al, 1);
            parameter_types.push_back(al, b.UnboundedArray(b.String(b.i32(1), ASR::ExpressionLength, ASR::CString), 1));
            
            ASR::symbol_t *_lfortran_date = b.create_c_subroutine_interface(c_func_name_1, fn_symtab, parameter_types, {"string_receiver"});
            fn_symtab->add_symbol(c_func_name_1, _lfortran_date);
            dep.push_back(al, s2c(al, c_func_name_1));

            // Create a `date_string_holder` variable + call subroutine `_lfortran_date` 
            ASR::expr_t* date_string_holder = declare("date_string_holder", b.String(b.i32(32), ASR::ExpressionLength, ASR::PointerString), Local);
            Vec<ASR::call_arg_t> call_to_lfortran_date; call_to_lfortran_date.reserve(al, 1);
            call_to_lfortran_date.push_back(al, {loc, date_string_holder});
            body.push_back(al, b.SubroutineCall(_lfortran_date, call_to_lfortran_date));

            // Declare func_arg `date` + assign `date_string_holder` into func arg `date`
            fill_func_arg_sub("date", arg_types[0], InOut);
            body.push_back(al, b.Assignment(args[0], // TODO : remove stringConcat, it's a workaround to avoid `strcpy`
                b.StringConcat(date_string_holder, b.StringConstant(" ", character(1)), character(-1))));

        } else {
            fill_func_arg_sub("date", real32, InOut);
            body.push_back(al, b.Assignment(args[0], b.f32(0)));
        }
        if (!is_real(*arg_types[1])) {
            /*
                interface 
                    subroutine _lfortran_time(string_receiver) bind(c)
                        character(len=1, c_char) :: string_receiver(*)
                    end subroutine
                end interface
                character(*) :: time
                character(32) :: time_string_holder
                call _lfortran_time(time_string_holder)
                time = time_string_holder
            */

            // Create `_lfortran_time` interface
            Vec<ASR::ttype_t*> parameter_types; parameter_types.reserve(al, 1);
            parameter_types.push_back(al, b.UnboundedArray(b.String(b.i32(1), ASR::ExpressionLength, ASR::CString), 1));
            
            ASR::symbol_t *_lfortran_time = b.create_c_subroutine_interface(c_func_name_2, fn_symtab, parameter_types, {"string_receiver"});
            fn_symtab->add_symbol(c_func_name_2, _lfortran_time);
            dep.push_back(al, s2c(al, c_func_name_2));

            // Create a `time_string_holder` variable + call subroutine `_lfortran_time` 
            ASR::expr_t* time_string_holder = declare("time_string_holder", b.String(b.i32(13), ASR::ExpressionLength, ASR::PointerString), Local);
            Vec<ASR::call_arg_t> call_to_lfortran_time; call_to_lfortran_time.reserve(al, 1);
            call_to_lfortran_time.push_back(al, {loc, time_string_holder});
            body.push_back(al, b.SubroutineCall(_lfortran_time, call_to_lfortran_time));

            // Declare func_arg `date` + assign `string_holder` into func arg `date`
            fill_func_arg_sub("time", arg_types[1], InOut);
            body.push_back(al, b.Assignment(args[1], // TODO : remove stringConcat, it's a workaround to avoid `strcpy`
                b.StringConcat(time_string_holder, b.StringConstant(" ", character(1)), character(-1))));
        }  else {
            fill_func_arg_sub("time", real32, InOut);
            body.push_back(al, b.Assignment(args[1], b.f32(0)));
        }
        if (!is_real(*arg_types[2])) {
            /*
                interface 
                    subroutine _lfortran_zone(string_receiver) bind(c)
                        character(len=1, c_char) :: string_receiver(*)
                    end subroutine
                end interface
                character(*), intent(inout) :: zone
                character(32) :: zone_string_holder
                call _lfortran_time(zone_string_holder)
                zone = zone_string_holder
            */

            // Create `_lfortran_zone` interface
            Vec<ASR::ttype_t*> parameter_types; parameter_types.reserve(al, 1);
            parameter_types.push_back(al, b.UnboundedArray(b.String(b.i32(1), ASR::ExpressionLength, ASR::CString), 1));
            
            ASR::symbol_t *_lfortran_zone = b.create_c_subroutine_interface(c_func_name_3, fn_symtab, parameter_types, {"string_receiver"});
            fn_symtab->add_symbol(c_func_name_3, _lfortran_zone);
            dep.push_back(al, s2c(al, c_func_name_1));

            // Create a `zone_string_holder` variable + call subroutine `_lfortran_zone` 
            ASR::expr_t* zone_string_holder = declare("zone_string_holder", b.String(b.i32(12), ASR::ExpressionLength, ASR::PointerString), Local);
            Vec<ASR::call_arg_t> call_to_lfortran_zone; call_to_lfortran_zone.reserve(al, 1);
            call_to_lfortran_zone.push_back(al, {loc, zone_string_holder});
            body.push_back(al, b.SubroutineCall(_lfortran_zone, call_to_lfortran_zone));

            // Declare func_arg `zone` + assign `string_holder` into func arg `zone`
            fill_func_arg_sub("zone", arg_types[2], InOut);
            body.push_back(al, b.Assignment(args[2], // TODO : remove stringConcat, it's a workaround to avoid `strcpy`
                b.StringConcat(zone_string_holder, b.StringConstant(" ", character(1)), character(-1))));
        } else {
            fill_func_arg_sub("zone", real32, InOut);
            body.push_back(al, b.Assignment(args[2], b.f32(0)));
        }
        if (!is_real(*arg_types[3])) {
            fill_func_arg_sub("values", arg_types[3], InOut);
            std::vector<LCompilers::ASR::expr_t *> vals;
            ASR::symbol_t *s_4 = b.create_c_func_subroutines(c_func_name_4, fn_symtab, 1, int32);
            fn_symtab->add_symbol(c_func_name_4, s_4);
            dep.push_back(al, s2c(al, c_func_name_4));
            for (int i = 0; i < 8; i++) {
                Vec<ASR::expr_t*> call_args2; call_args2.reserve(al, 1);
                call_args2.push_back(al, b.i32(i+1));
                auto xx = declare("i_" + std::to_string(i), int32, Local);
                body.push_back(al, b.Assignment(xx, b.Call(s_4, call_args2, int32)));
                vals.push_back(xx);
            }
            body.push_back(al, b.Assignment(args[3], b.ArrayConstant(vals, extract_type(arg_types[3]), false)));
        } else {
            fill_func_arg_sub("values", real32, InOut);
            body.push_back(al, b.Assignment(args[3], b.f32(0)));
        }
        ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, nullptr, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, new_symbol);
        return b.SubroutineCall(new_symbol, new_args);
    }
    
} // namespace DateAndTime

namespace GetEnvironmentVariable {

    static inline void verify_args(const ASR::IntrinsicImpureSubroutine_t& x, diag::Diagnostics& diagnostics) {
        if (x.n_args == 1) {
            ASRUtils::require_impl(ASRUtils::is_character(*ASRUtils::expr_type(x.m_args[0])), "First argument must be of character type", x.base.base.loc, diagnostics);
        } else if (x.n_args == 2) {
            ASRUtils::require_impl(ASRUtils::is_character(*ASRUtils::expr_type(x.m_args[0])) && ASRUtils::is_character(*ASRUtils::expr_type(x.m_args[1])), "First two arguments of `get_environment_variable` must be of character type", x.base.base.loc, diagnostics);
        }
    }

    static inline ASR::asr_t* create_GetEnvironmentVariable(Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args, diag::Diagnostics& /*diag*/) {
        Vec<ASR::expr_t*> m_args; m_args.reserve(al, args.size());
        m_args.push_back(al, args[0]);
        for (int i = 1; i < int(args.size()); i++) {
            if(args[i]) m_args.push_back(al, args[i]);
        }
        return ASR::make_IntrinsicImpureSubroutine_t(al, loc, static_cast<int64_t>(IntrinsicImpureSubroutines::GetEnvironmentVariable), m_args.p, m_args.n, 0);
    }

    static inline ASR::stmt_t* instantiate_GetEnvironmentVariable(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
                
        std::string c_func_name = "_lfortran_get_environment_variable";
        std::string new_name = "_lcompilers_get_environment_variable_";
        declare_basic_variables(new_name);
        fill_func_arg_sub("name", arg_types[0], InOut);
        if ( arg_types.size() >= 2 && ASRUtils::is_character(*arg_types[1]) ) {// this is the case where args[1] is `value`
            /*
            interface 
                subroutine _lfortran_get_environment_variable(name, string_receiver) bind(c)
                    character(len=1, c_char) :: name(*)
                    character(len=1, c_char) :: string_receiver(*)
                end subroutine

                integer function _lfortran_get_length_of_environment_variable(name) bind(c)
                    character(len=1, c_char) :: name(*)
                end function 
            end interface

            integer :: length_to_allocate
            length_to_allocate =  _lfortran_get_length_of_environment_variable(name)

            character(:), allocatable :: envVar_string_holder
            allocate(character(length_to_allocate) :: envVar_string_holder)

            call_lfortran_get_environment_variable(name, envVar_string_holder)
            value = envVar_string_holder

            deallocate(envVar_string_holder)
            */
           
           // Declare interface `_lfortran_get_environment_variable`
            Vec<ASR::ttype_t*> parameter_types; parameter_types.reserve(al, 1);
            parameter_types.push_back(al, b.UnboundedArray(b.String(b.i32(1), ASR::ExpressionLength, ASR::CString), 1));
            parameter_types.push_back(al, b.UnboundedArray(b.String(b.i32(1), ASR::ExpressionLength, ASR::CString), 1));
            ASR::symbol_t* _lfortran_get_environment_variable = b.create_c_subroutine_interface(c_func_name, fn_symtab, parameter_types, {"name", "string_receiver"});
            fn_symtab->add_symbol(c_func_name, _lfortran_get_environment_variable);
            dep.push_back(al, s2c(al, c_func_name));

            // Declare interface `_lfortran_get_length_of_environment_variable`
            std::string c_func_name = "_lfortran_get_length_of_environment_variable";
            ASR::symbol_t *_lfortran_get_length_of_environment_variable = b.create_c_func_subroutines_with_return_type(
                c_func_name, fn_symtab, 1, 
                {b.UnboundedArray(b.String(b.i32(1), ASR::ExpressionLength, ASR::CString), 1)}, int32);
            fn_symtab->add_symbol(c_func_name, _lfortran_get_length_of_environment_variable);
            dep.push_back(al, s2c(al, c_func_name));

            // `length_to_allocate = _lfortran_get_length_of_environment_variable(name)`
            ASR::expr_t* length_to_allocate = declare("length_to_allocate", int32, Local);
            Vec<ASR::expr_t*> call_args; call_args.reserve(al, 1);
            call_args.push_back(al, args[0]);
            body.push_back(al, b.Assignment(length_to_allocate, b.Call(_lfortran_get_length_of_environment_variable, call_args, int32)));

            // Declare allocatable string_holder + allocate
            ASR::expr_t* envVar_string_holder = declare("envVar_string_holder", b.allocatable(b.String(nullptr, ASR::DeferredLength, ASR::DescriptorString)), Local);
            body.push_back(al, b.Allocate(envVar_string_holder, nullptr, 0, length_to_allocate));

            // Call `_lfortran_get_environment_variable`
            Vec<ASR::call_arg_t> call_to_lfortran_get_environment_variable; call_to_lfortran_get_environment_variable.reserve(al, 2);
            call_to_lfortran_get_environment_variable.push_back(al, {loc, args[0]});
            call_to_lfortran_get_environment_variable.push_back(al, {loc, envVar_string_holder});
            body.push_back(al, b.SubroutineCall(_lfortran_get_environment_variable, call_to_lfortran_get_environment_variable));

            // Declare `value` +  Assign `envVar_string_holder` into func arg `value`
            fill_func_arg_sub("value", arg_types[1], InOut);
            body.push_back(al, b.Assignment(args[1],
                ASRUtils::create_string_physical_cast(al, envVar_string_holder,
                    ASR::down_cast<ASR::String_t>(ASRUtils::extract_type(arg_types[1]))->m_physical_type)));
            // Deallocate `envVar_string_holder`
            body.push_back(al, b.Deallocate(envVar_string_holder));

            if (arg_types.size() >= 3) {
                fill_func_arg_sub("length", arg_types[2], InOut);
                body.push_back(al, b.Assignment(args[2], length_to_allocate)); // Reuse `length_to_allocate`
            }
            if (arg_types.size() >= 4) {
                fill_func_arg_sub("status", arg_types[3], InOut);
            }
            if (arg_types.size() >= 5) {
                fill_func_arg_sub("trim_name", arg_types[4], InOut);
            }
        } else if ( arg_types.size() >= 2 && ASRUtils::is_integer(*arg_types[1]) ) {
            // this is the case where args[1] is `length`
            c_func_name = "_lfortran_get_length_of_environment_variable";
            fill_func_arg_sub("length", arg_types[1], InOut);
            ASR::symbol_t *s = b.create_c_func_subroutines_with_return_type(c_func_name, fn_symtab, 1, 
                {b.UnboundedArray(b.String(b.i32(1), ASR::ExpressionLength, ASR::CString), 1)},
                arg_types[1]);
            fn_symtab->add_symbol(c_func_name, s);
            dep.push_back(al, s2c(al, c_func_name));
            Vec<ASR::expr_t*> call_args; call_args.reserve(al, 1);
            call_args.push_back(al, args[0]);
            body.push_back(al, b.Assignment(args[1], b.Call(s, call_args, arg_types[1])));

            if (arg_types.size() >= 3) {
                fill_func_arg_sub("status", arg_types[2], InOut);
            }

            if (arg_types.size() >= 4) {
                fill_func_arg_sub("trim_name", arg_types[3], InOut);
            }
        }
        ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, nullptr, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, new_symbol);
        return b.SubroutineCall(new_symbol, new_args);
    }

} // namespace GetEnvironmentVariable

namespace ExecuteCommandLine {

    static inline void verify_args(const ASR::IntrinsicImpureSubroutine_t& x, diag::Diagnostics& diagnostics) {
        if (x.n_args == 1) {
            ASRUtils::require_impl(ASRUtils::is_character(*ASRUtils::expr_type(x.m_args[0])), "First argument must be of character type", x.base.base.loc, diagnostics);
        }
    }

    static inline ASR::asr_t* create_ExecuteCommandLine(Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args, diag::Diagnostics& /*diag*/) {
        Vec<ASR::expr_t*> m_args; m_args.reserve(al, args.size());
        ASRBuilder b(al, loc);
        int64_t overload_id = 0;
        if (args[1]) overload_id |= 1 << 0; // WAIT
        if (args[2]) overload_id |= 1 << 1; // EXITSTAT
        if (args[3]) overload_id |= 1 << 2; // CMDSTAT
        if (args[4]) overload_id |= 1 << 3; // CMDMSG
        m_args.push_back(al, args[0]);
        if (args[1]) m_args.push_back(al, args[1]);
        else m_args.push_back(al, b.logical_true()); // default value for WAIT is true
        for (int i = 2; i < int(args.size()); i++) {
            if(args[i]) m_args.push_back(al, args[i]);
        }
        return ASR::make_IntrinsicImpureSubroutine_t(al, loc, static_cast<int64_t>(IntrinsicImpureSubroutines::ExecuteCommandLine), m_args.p, m_args.n, overload_id);
    }

    static inline ASR::stmt_t* instantiate_ExecuteCommandLine(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types,
            Vec<ASR::call_arg_t>& new_args, int64_t overload_id) {

        std::string c_func_name = "_lfortran_exec_command";
        std::string new_name = "_lcompilers_execute_command_line_";
        ASR::ttype_t* ret_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
        ASR::ttype_t* str_type = ASRUtils::TYPE(ASR::make_String_t(al, loc, 1, nullptr, ASR::string_length_kindType::AssumedLength, ASR::string_physical_typeType::PointerString));
        declare_basic_variables(new_name);
        fill_func_arg_sub("command", str_type, InOut);
        ASR::expr_t* exit_status_local = declare("_lcompilers_exit_status", ret_type, Local);
        constexpr int WAIT_BIT     = 1 << 0;
        constexpr int EXITSTAT_BIT = 1 << 1;
        constexpr int CMDSTAT_BIT  = 1 << 2;
        constexpr int CMDMSG_BIT   = 1 << 3;
        int optional_arg_index = 1;
        fill_func_arg_sub("wait", arg_types[optional_arg_index++], InOut);
        if (overload_id & EXITSTAT_BIT) {
            fill_func_arg_sub("exitstat", arg_types[optional_arg_index++], InOut);
        }
        if (overload_id & CMDSTAT_BIT) {
            fill_func_arg_sub("cmdstat", arg_types[optional_arg_index++], InOut);
        }
        if (overload_id & CMDMSG_BIT) {
            fill_func_arg_sub("cmdmsg", arg_types[optional_arg_index], InOut);
        }

        SymbolTable *fn_symtab_1 = al.make_new<SymbolTable>(fn_symtab);
        Vec<ASR::expr_t*> args_1; args_1.reserve(al, 1);
        ASR::expr_t *arg = b.Variable(fn_symtab_1, "n", str_type,
            ASR::intentType::InOut, nullptr, ASR::abiType::BindC, true);
        args_1.push_back(al, arg);

        ASR::expr_t *return_var_1 = b.Variable(fn_symtab_1, c_func_name,
        ret_type, ASRUtils::intent_return_var, nullptr, ASR::abiType::BindC, false);

        SetChar dep_1; dep_1.reserve(al, 1);
        Vec<ASR::stmt_t*> body_1; body_1.reserve(al, 1);
        ASR::symbol_t *s = make_ASR_Function_t(c_func_name, fn_symtab_1, dep_1, args_1,
            body_1, return_var_1, ASR::abiType::BindC, ASR::deftypeType::Interface, s2c(al, c_func_name));
        fn_symtab->add_symbol(c_func_name, s);
        dep.push_back(al, s2c(al, c_func_name));

        Vec<ASR::expr_t*> call_args; call_args.reserve(al, 1);
        call_args.push_back(al, args[0]);
        optional_arg_index = 2;
        if (overload_id & WAIT_BIT) {
            // TODO: handle this
        }
        body.push_back(al, b.Assignment(exit_status_local, b.Call(s, call_args, ret_type)));
        if (overload_id & EXITSTAT_BIT) {
            body.push_back(al, b.Assignment(args[optional_arg_index], exit_status_local));
            optional_arg_index++;
        }
        ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, nullptr, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, new_symbol);
        return b.SubroutineCall(new_symbol, new_args);
    }

} // namespace ExecuteCommandLine

namespace CpuTime {

    static inline void verify_args(const ASR::IntrinsicImpureSubroutine_t& x, diag::Diagnostics& diagnostics) {
        if (x.n_args == 1) {
            ASRUtils::require_impl(ASRUtils::is_real(*ASRUtils::expr_type(x.m_args[0])), "First argument must be of real type", x.base.base.loc, diagnostics);
        }
    }

    static inline ASR::asr_t* create_CpuTime(Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args, diag::Diagnostics& /*diag*/) {
        Vec<ASR::expr_t*> m_args; m_args.reserve(al, 1); m_args.push_back(al, args[0]);
        return ASR::make_IntrinsicImpureSubroutine_t(al, loc, static_cast<int64_t>(IntrinsicImpureSubroutines::CpuTime), m_args.p, m_args.n, 0);
    }

    static inline ASR::stmt_t* instantiate_CpuTime(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
                
        std::string c_func_name;
        if (ASRUtils::extract_kind_from_ttype_t(arg_types[0]) == 4) {
            c_func_name = "_lfortran_s_cpu_time";
        } else {
            c_func_name = "_lfortran_d_cpu_time";
        }
        std::string new_name = "_lcompilers_cpu_time_" + type_to_str_python(arg_types[0]);
        declare_basic_variables(new_name);
        fill_func_arg_sub("time", arg_types[0], InOut);

        ASR::symbol_t *s = b.create_c_func_subroutines(c_func_name, fn_symtab, 0, arg_types[0]);
        fn_symtab->add_symbol(c_func_name, s);
        dep.push_back(al, s2c(al, c_func_name));

        Vec<ASR::expr_t*> call_args; call_args.reserve(al, 0);
        body.push_back(al, b.Assignment(args[0], b.Call(s, call_args, arg_types[0])));
        ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, nullptr, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, new_symbol);
        return b.SubroutineCall(new_symbol, new_args);
    }

} // namespace CpuTime

namespace MoveAlloc {

    static inline void verify_args(const ASR::IntrinsicImpureSubroutine_t& x, diag::Diagnostics& diagnostics) {
        if (x.n_args != 2) {
            ASRUtils::require_impl(false, "MoveAlloc takes exactly 2 arguments", x.base.base.loc, diagnostics);
        }
        ASRUtils::require_impl(ASRUtils::is_allocatable(ASRUtils::expr_type(x.m_args[0])), "First argument must be an allocatable type", x.base.base.loc, diagnostics);
        ASRUtils::require_impl(ASRUtils::is_allocatable(ASRUtils::expr_type(x.m_args[1])), "Second argument must be an allocatable type", x.base.base.loc, diagnostics);
    }

    static inline ASR::asr_t* create_MoveAlloc(Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args, diag::Diagnostics& /*diag*/) {
        Vec<ASR::expr_t*> m_args; m_args.reserve(al, 2);
        m_args.push_back(al, args[0]); m_args.push_back(al, args[1]);
        return ASR::make_IntrinsicImpureSubroutine_t(al, loc, static_cast<int64_t>(IntrinsicImpureSubroutines::MoveAlloc), m_args.p, m_args.n, 0);
    }

    static inline ASR::stmt_t* instantiate_MoveAlloc(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {

        std::string new_name = "_lcompilers_move_alloc_" + type_to_str_fortran(arg_types[0]);
        declare_basic_variables(new_name);
        fill_func_arg_sub("from", arg_types[0], In);
        fill_func_arg_sub("to", arg_types[1], InOut);
        if (!ASRUtils::is_character(*arg_types[0])) {
            int n_dims = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(args[0]));
            Vec<ASR::dimension_t> alloc_dims; alloc_dims.reserve(al, n_dims);
            ASR::ttype_t* integer_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            for(int i=0; i<n_dims; i++) {
                ASR::dimension_t dim;
                dim.loc = loc;
                dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                    al, loc, 1, integer_type));
                dim.m_length = ASRUtils::EXPR(ASR::make_ArraySize_t(
        al, loc, args[0], ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, i+1, integer_type)), integer_type, nullptr));
                alloc_dims.push_back(al, dim);
            }
            body.push_back(al, b.Allocate(args[1], alloc_dims));
        }
        body.push_back(al, b.Assignment(args[1], args[0]));
        Vec<ASR::expr_t*> explicit_deallocate_args; explicit_deallocate_args.reserve(al, 1);
                    explicit_deallocate_args.push_back(al, args[0]);
        ASR::stmt_t* explicit_deallocate = ASRUtils::STMT(ASR::make_ExplicitDeallocate_t(al, loc, explicit_deallocate_args.p, explicit_deallocate_args.n));
        body.push_back(al, explicit_deallocate);
        ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, nullptr, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, new_symbol);
        return b.SubroutineCall(new_symbol, new_args);
    }

} // namespace MoveAlloc


namespace Mvbits {

    static inline void verify_args(const ASR::IntrinsicImpureSubroutine_t& x, diag::Diagnostics& diagnostics) {
        if (x.n_args != 5) {
            ASRUtils::require_impl(false, "Mvbits requires exactly 5 arguments", x.base.base.loc, diagnostics);
        }
        for (int i = 0; i < 5; i++) {
            ASRUtils::require_impl(ASRUtils::is_integer(*ASRUtils::expr_type(x.m_args[i])), "All arguments of Mvbits must be of integer type", x.base.base.loc, diagnostics);
        }
    }

    static inline ASR::asr_t* create_Mvbits(Allocator& al, const Location& loc, Vec<ASR::expr_t*>& args, diag::Diagnostics& /*diag*/) {
        ASRBuilder b(al, loc);
        Vec<ASR::expr_t*> m_args; m_args.reserve(al, args.size());
        for (int i = 0; i < int(args.size()); i++) {
            m_args.push_back(al, args[i]);
        }
        return ASR::make_IntrinsicImpureSubroutine_t(al, loc, static_cast<int64_t>(IntrinsicImpureSubroutines::Mvbits), m_args.p, m_args.n, 0);
    }

    static inline ASR::stmt_t* instantiate_Mvbits(Allocator &al, const Location &loc,
            SymbolTable *scope, Vec<ASR::ttype_t*>& arg_types,
            Vec<ASR::call_arg_t>& new_args, int64_t /*overload_id*/) {
        std::string c_func_name;
        if (ASRUtils::extract_kind_from_ttype_t(arg_types[0]) == 4) {
            c_func_name = "_lfortran_mvbits32";
        } else {
            c_func_name = "_lfortran_mvbits64";
        }
        std::string new_name = "_lcompilers_mvbits_" + type_to_str_fortran(arg_types[0]);
        declare_basic_variables(new_name);
        fill_func_arg_sub("from", arg_types[0], In);
        fill_func_arg_sub("frompos", arg_types[1], In);
        fill_func_arg_sub("len", arg_types[2], In);
        fill_func_arg_sub("to", arg_types[3], InOut);
        fill_func_arg_sub("topos", arg_types[4], In);
        {
            SymbolTable *fn_symtab_1 = al.make_new<SymbolTable>(fn_symtab);
            Vec<ASR::expr_t*> args_1;
            {
                args_1.reserve(al, 5);
                ASR::expr_t *arg = b.Variable(fn_symtab_1, "from", arg_types[0],
                    ASR::intentType::In, nullptr, ASR::abiType::BindC, true);
                args_1.push_back(al, arg);
                arg = b.Variable(fn_symtab_1, "frompos", arg_types[1],
                    ASR::intentType::In, nullptr, ASR::abiType::BindC, true);
                args_1.push_back(al, arg);
                arg = b.Variable(fn_symtab_1, "len", arg_types[2],
                    ASR::intentType::In, nullptr, ASR::abiType::BindC, true);
                args_1.push_back(al, arg);
                arg = b.Variable(fn_symtab_1, "to", arg_types[3],
                    ASR::intentType::In, nullptr, ASR::abiType::BindC, true);
                args_1.push_back(al, arg);
                arg = b.Variable(fn_symtab_1, "topos", arg_types[4],
                    ASR::intentType::In, nullptr, ASR::abiType::BindC, true);
                args_1.push_back(al, arg);
            }

            ASR::expr_t *return_var_1 = b.Variable(fn_symtab_1, c_func_name,
                arg_types[3], ASRUtils::intent_return_var, nullptr, ASR::abiType::BindC, false);

            SetChar dep_1; dep_1.reserve(al, 1);
            Vec<ASR::stmt_t*> body_1; body_1.reserve(al, 1);
            ASR::symbol_t *s = make_ASR_Function_t(c_func_name, fn_symtab_1, dep_1, args_1,
                body_1, return_var_1, ASR::abiType::BindC, ASR::deftypeType::Interface, s2c(al, c_func_name));
            fn_symtab->add_symbol(c_func_name, s);
            dep.push_back(al, s2c(al, c_func_name));
            body.push_back(al, b.Assignment(args[3], b.Call(s, args, arg_types[3])));
        }
        ASR::symbol_t *new_symbol = make_ASR_Function_t(fn_name, fn_symtab, dep, args,
            body, nullptr, ASR::abiType::Source, ASR::deftypeType::Implementation, nullptr);
        scope->add_symbol(fn_name, new_symbol);
        return b.SubroutineCall(new_symbol, new_args);
    }

} // namespace Mvbits

} // namespace LCompilers::ASRUtils

#endif // LIBASR_PASS_INTRINSIC_SUBROUTINES_H

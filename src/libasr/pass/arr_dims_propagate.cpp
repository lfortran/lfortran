#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/for_all.h>
#include <libasr/pass/stmt_walk_visitor.h>

namespace LFortran
{

/*
 * This ASR pass replaces ttype for all arrays passed.
 *
 * Converts:
 *      integer :: a(:, :)
 *
 * to:
 *      integer :: a(2, 3)
 */

class PassArrDimInFuncCalls : public ASR::BaseExprReplacer<PassArrDimInFuncCalls>
{
private:
    Allocator& al;

public:
    PassArrDimInFuncCalls(Allocator& al_)
        : al(al_)
    {
    }

    void replace_FunctionCall(ASR::FunctionCall_t* x)
    {
        ASR::Function_t* fn
            = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(x->m_name));

        Vec<ASR::call_arg_t> new_args;
        new_args.reserve(al, x->n_args);

        for (size_t i = 0; i < x->n_args; i++) {
            if (ASR::is_a<ASR::Var_t>(*x->m_args[i].m_value)
                && ASRUtils::is_array(ASRUtils::expr_type(x->m_args[i].m_value))) {
                ASR::Variable_t* v = ASRUtils::EXPR2VAR(x->m_args[i].m_value);
                // ASR::Variable_t* fn_param = ASRUtils::EXPR2VAR(fn->m_args[i]);
                // int n_dims = ASRUtils::extract_dimensions_from_ttype(fn_param->m_type, m_dims);
                // if (n_dims > 0 && !m_dims[0].m_length
                //     && ASRUtils::check_equal_type(v->m_type, fn_param->m_type)) {
                // fn_param->m_type = v->m_type;
                ASR::dimension_t* m_dims;
                size_t n_dims = ASRUtils::extract_dimensions_from_ttype(v->m_type, m_dims);
                for (size_t j = 0; j < n_dims; j++) {
                    auto type = ASR::make_Integer_t(
                        al, v->base.base.loc, 4 /* FIXME: support other kinds */, nullptr, 0);
                    auto dim = ASR::make_IntegerConstant_t(
                        al, v->base.base.loc, j + 1, ASRUtils::TYPE(type));
                    auto call_array_size = ASR::make_ArraySize_t(al,
                                                                 v->base.base.loc,
                                                                 v->m_value,
                                                                 ASRUtils::EXPR(dim),
                                                                 ASRUtils::TYPE(type),
                                                                 nullptr);
                    ASR::call_arg_t new_arg;
                    new_arg.loc = v->base.base.loc;
                    new_arg.m_value = ASRUtils::EXPR(call_array_size);
                    new_args.push_back(al, new_arg);
                }
                // }
            }
            new_args.push_back(al, x->m_args[i]);
        }

        std::cout << "new_args.size() = " << new_args.size() << std::endl;
        x->n_args = new_args.size();
        x->m_args = new_args.p;

        auto modified_func_call = ASR::make_FunctionCall_t(al,
                                                           x->base.base.loc,
                                                           x->m_name,
                                                           x->m_original_name,
                                                           x->m_args,
                                                           x->n_args,
                                                           x->m_type,
                                                           x->m_value,
                                                           x->m_dt);
        *current_expr = ASRUtils::EXPR(modified_func_call);
    }

    // void replace_ArraySize(ASR::ArraySize_t* x)
    // {
    //     if (!ASR::is_a<ASR::Var_t>(*x->m_v)) {
    //         return;
    //     }

    //     ASR::Variable_t* v = ASRUtils::EXPR2VAR(x->m_v);
    //     ASR::ttype_t* array_type = ASRUtils::expr_type(x->m_v);
    //     ASR::dimension_t* dims = nullptr;
    //     int n = ASRUtils::extract_dimensions_from_ttype(array_type, dims);
    //     bool is_argument = v->m_intent == ASRUtils::intent_in;
    //     if (!(n > 0 && is_argument && !ASRUtils::is_dimension_empty(dims, n))) {
    //         return;
    //     }
    //     ASR::expr_t* array_size
    //         = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x->base.base.loc, 1, x->m_type));
    //     for (int i = 0; i < n; i++) {
    //         array_size = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al,
    //                                                              x->base.base.loc,
    //                                                              array_size,
    //                                                              ASR::binopType::Mul,
    //                                                              dims[i].m_length,
    //                                                              x->m_type,
    //                                                              nullptr));
    //     }
    //     *current_expr = array_size;
    // }
};

// class PassArrDimInSubCalls : public ASR::BaseStmtReplacer<PassArrDimInSubCalls>
// {
// private:
//     Allocator& al;

// public:
//     PassArrDimInSubCalls(Allocator& al_)
//         : al(al_)
//     {
//     }

//     void replace_SubroutineCall(ASR::SubroutineCall_t* x)
//     {
//         ASR::Subroutine_t* sb
//             = ASR::down_cast<ASR::Subroutine_t>(ASRUtils::symbol_get_past_external(x->m_name));
//         Vec<ASR::call_arg_t> new_args;
//         new_args.reserve(al, x->n_args);

//         for (size_t i = 0; i < x->n_args; i++) {
//             if (ASR::is_a<ASR::Var_t>(*x->m_args[i].m_value)
//                 && ASRUtils::is_array(ASRUtils::expr_type(x->m_args[i].m_value))) {
//                 ASR::Variable_t* v = ASRUtils::EXPR2VAR(x->m_args[i].m_value);
//                 ASR::Variable_t* sb_param = ASRUtils::EXPR2VAR(sb->m_args[i]);
//                 ASR::dimension_t* m_dims;
//                 int n_dims = ASRUtils::extract_dimensions_from_ttype(sb_param->m_type, m_dims);
//                 if (n_dims > 0 && !m_dims[0].m_length
//                     && ASRUtils::check_equal_type(v->m_type, sb_param->m_type)) {
//                     sb_param->m_type = v->m_type;
//                     size_t n_dims = ASRUtils::extract_dimensions_from_ttype(v->m_type, m_dims);
//                     for (size_t j = 0; j < n_dims; j++) {
//                         auto type = ASR::make_Integer_t(
//                             al, v->base.base.loc, 4 /* FIXME: support other kinds */, nullptr,
//                             0);
//                         auto dim = ASR::make_IntegerConstant_t(
//                             al, v->base.base.loc, j + 1, ASRUtils::TYPE(type));
//                         auto call_array_size = ASR::make_ArraySize_t(al,
//                                                                      v->base.base.loc,
//                                                                      v->m_value,
//                                                                      ASRUtils::EXPR(dim),
//                                                                      ASRUtils::TYPE(type),
//                                                                      nullptr);
//                         ASR::call_arg_t new_arg;
//                         new_arg.loc = v->base.base.loc;
//                         new_arg.m_value = ASRUtils::EXPR(call_array_size);
//                         new_args.push_back(al, new_arg);
//                     }
//                 }
//                 new_args.push_back(al, x->m_args[i]);
//             }

//             x->n_args = new_args.size();
//             x->m_args = new_args.p;

//             *current_stmt = ASRUtils::STMT((ASR::asr_t*) x);
//         }
//     }


// void visit_Function(const ASR::Function_t& x)
// {
//     Vec<ASR::expr_t*> params;
//     params.reserve(al, x.n_args);

//     for (size_t i = 0; i < x.n_args; i++) {
//         ASR::Variable_t* arg = ASRUtils::EXPR2VAR(x.m_args[i]);
//         if (ASRUtils::is_array(arg->m_type)) {
//             ASR::dimension_t* m_dims;
//             size_t n_dims = ASRUtils::extract_dimensions_from_ttype(arg->m_type, m_dims);

//             for (size_t j = 0; j < n_dims; j++) {
//                 auto type = ASR::make_Integer_t(
//                     al, arg->base.base.loc, 4 /* FIXME: support other kinds */, nullptr, 0);
//                 auto variable = ASR::make_Variable_t(
//                     al,
//                     arg->base.base.loc,
//                     nullptr,
//                     s2c(al, "n" + std::string(arg->m_name) + std::to_string(j + 1)),
//                     ASR::intentType::In,
//                     nullptr,
//                     nullptr,
//                     ASR::storage_typeType::Default,
//                     ASRUtils::TYPE(type),
//                     ASR::abiType::Source,
//                     ASR::accessType::Public,
//                     ASR::presenceType::Required,
//                     false);
//                 auto var = ASR::make_Var_t(
//                     al, arg->base.base.loc, ASR::down_cast<ASR::symbol_t>(variable));
//                 params.push_back(al, ASRUtils::EXPR(var));
//                 m_dims[j].m_length = ASRUtils::EXPR(var);
//             }
//         }
//         params.push_back(al, x.m_args[i]);
//     }

//     x.n_args = params.size();
//     x.m_args = params.p;
// }

// void visit_Subroutine(const ASR::Subroutine_t& x)
// {
//     Vec<ASR::expr_t*> params;
//     params.reserve(al, x.n_args);

//     for (size_t i = 0; i < x.n_args; i++) {
//         ASR::Variable_t* arg = ASRUtils::EXPR2VAR(x.m_args[i]);
//         if (ASRUtils::is_array(arg->m_type)) {
//             ASR::dimension_t* m_dims;
//             size_t n_dims = ASRUtils::extract_dimensions_from_ttype(arg->m_type, m_dims);

//             for (size_t j = 0; j < n_dims; j++) {
//                 auto type = ASR::make_Integer_t(
//                     al, arg->base.base.loc, 4 /* FIXME: support other kinds */, nullptr, 0);
//                 auto variable = ASR::make_Variable_t(
//                     al,
//                     arg->base.base.loc,
//                     nullptr,
//                     s2c(al, "n" + std::string(arg->m_name) + std::to_string(j + 1)),
//                     ASR::intentType::In,
//                     nullptr,
//                     nullptr,
//                     ASR::storage_typeType::Default,
//                     ASRUtils::TYPE(type),
//                     ASR::abiType::Source,
//                     ASR::accessType::Public,
//                     ASR::presenceType::Required,
//                     false);
//                 auto var = ASR::make_Var_t(
//                     al, arg->base.base.loc, ASR::down_cast<ASR::symbol_t>(variable));
//                 params.push_back(al, ASRUtils::EXPR(var));
//                 m_dims[j].m_length = ASRUtils::EXPR(var);
//             }
//         }
//         params.push_back(al, x.m_args[i]);
//     }

//     x.n_args = params.size();
//     x.m_args = params.p;
// }
// };

class ArrayDimIntrinsicCallsVisitor2
    : public ASR::CallReplacerOnExpressionsVisitor<ArrayDimIntrinsicCallsVisitor2>
{
private:
    PassArrDimInFuncCalls replacer;

public:
    ArrayDimIntrinsicCallsVisitor2(Allocator& al_)
        : replacer(al_)
    {
    }

    void call_replacer()
    {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};

void
pass_propagate_arr_dims(Allocator& al, ASR::TranslationUnit_t& unit)
{
    ArrayDimIntrinsicCallsVisitor2 v(al);
    // PassArrDimInSubCalls pass_arr_dim_in_sub_calls(al);
    v.visit_TranslationUnit(unit);
    LFORTRAN_ASSERT(asr_verify(unit));
}

}  // namespace LFortran

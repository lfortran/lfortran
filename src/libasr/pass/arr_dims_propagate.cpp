#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/for_all.h>
#include <libasr/pass/stmt_walk_visitor.h>

namespace LFortran {

/*
 * This ASR pass replaces ttype for all arrays passed.
 *
 * Converts:
 *      integer :: a(:, :)
 *
 * to:
 *      integer :: a(2, 3)
 */

class ArrDimsPropagate : public ASR::StatementWalkVisitor<ArrDimsPropagate>
{
public:
    ArrDimsPropagate(Allocator &al) : StatementWalkVisitor(al) { }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {

        Vec<ASR::call_arg_t> new_args;
        new_args.reserve(al, x.n_args);

        for (size_t i = 0; i < x.n_args; i++) {
            if (ASR::is_a<ASR::Var_t>(*x.m_args[i].m_value) && ASRUtils::is_array(ASRUtils::expr_type(x.m_args[i].m_value))) {
                ASR::Variable_t* v = ASRUtils::EXPR2VAR(x.m_args[i].m_value);
                ASR::dimension_t* m_dims;
                size_t n_dims = ASRUtils::extract_dimensions_from_ttype(v->m_type, m_dims);
                for (size_t j = 0; j < n_dims; j++) {
                    auto type = ASR::make_Integer_t(al, v->base.base.loc, 4 /* FIXME: support other kinds */, nullptr, 0);
                    auto dim = ASR::make_IntegerConstant_t(al, v->base.base.loc, j + 1, ASRUtils::TYPE(type));
                    auto call_array_size = ASR::make_ArraySize_t(al, v->base.base.loc, v->m_value, ASRUtils::EXPR(dim),
                                            ASRUtils::TYPE(type), nullptr);
                    ASR::call_arg_t new_arg;
                    new_arg.loc = v->base.base.loc;
                    new_arg.m_value = ASRUtils::EXPR(call_array_size);
                    new_args.push_back(al, new_arg);
                }
            }
            new_args.push_back(al, x.m_args[i]);
        }

        ASR::FunctionCall_t xx = const_cast<ASR::FunctionCall_t &>(x);
        xx.n_args = new_args.size();
        xx.m_args = new_args.p;
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        Vec<ASR::call_arg_t> new_args;
        new_args.reserve(al, x.n_args);

        for (size_t i = 0; i < x.n_args; i++) {
            if (ASR::is_a<ASR::Var_t>(*x.m_args[i].m_value) && ASRUtils::is_array(ASRUtils::expr_type(x.m_args[i].m_value))) {
                ASR::Variable_t* v = ASRUtils::EXPR2VAR(x.m_args[i].m_value);
                ASR::dimension_t* m_dims;
                size_t n_dims = ASRUtils::extract_dimensions_from_ttype(v->m_type, m_dims);
                for (size_t j = 0; j < n_dims; j++) {
                    auto type = ASR::make_Integer_t(al, v->base.base.loc, 4 /* FIXME: support other kinds */, nullptr, 0);
                    auto dim = ASR::make_IntegerConstant_t(al, v->base.base.loc, j + 1, ASRUtils::TYPE(type));
                    auto call_array_size = ASR::make_ArraySize_t(al, v->base.base.loc, v->m_value, ASRUtils::EXPR(dim),
                                            ASRUtils::TYPE(type), nullptr);
                    ASR::call_arg_t new_arg;
                    new_arg.loc = v->base.base.loc;
                    new_arg.m_value = ASRUtils::EXPR(call_array_size);
                    new_args.push_back(al, new_arg);
                }
            }
            new_args.push_back(al, x.m_args[i]);
        }

        ASR::SubroutineCall_t xx = const_cast<ASR::SubroutineCall_t &>(x);
        xx.n_args = new_args.size();
        xx.m_args = new_args.p;
    }

    void visit_Function(const ASR::Function_t &x) {
        Vec<ASR::expr_t*> params;
        params.reserve(al, x.n_args);

        for (size_t i = 0; i < x.n_args; i++) {
            ASR::Variable_t *arg = ASRUtils::EXPR2VAR(x.m_args[i]);
            if (ASRUtils::is_array(arg->m_type)) {

                ASR::dimension_t* m_dims;
                size_t n_dims = ASRUtils::extract_dimensions_from_ttype(arg->m_type, m_dims);

                for (size_t j = 0; j < n_dims; j++) {
                    auto type = ASR::make_Integer_t(al, arg->base.base.loc, 4 /* FIXME: support other kinds */, nullptr, 0);
                    auto variable = ASR::make_Variable_t(al, arg->base.base.loc, nullptr, s2c(al, "n" + std::string(arg->m_name) + std::to_string(j + 1)),
                        ASR::intentType::In, nullptr, nullptr, ASR::storage_typeType::Default,
                        ASRUtils::TYPE(type), ASR::abiType::Source, ASR::accessType::Public,
                        ASR::presenceType::Required, false);
                    auto var = ASR::make_Var_t(al, arg->base.base.loc, ASR::down_cast<ASR::symbol_t>(variable));
                    params.push_back(al, ASRUtils::EXPR(var));
                    m_dims[j].m_length = ASRUtils::EXPR(var);
                }
            }
            params.push_back(al, x.m_args[i]);
        }

        ASR::Function_t xx = const_cast<ASR::Function_t &>(x);
        xx.n_args = params.size();
        xx.m_args = params.p;
    }

    void visit_Subroutine(const ASR::Subroutine_t &x) {
        Vec<ASR::expr_t*> params;
        params.reserve(al, x.n_args);

        for (size_t i = 0; i < x.n_args; i++) {
            ASR::Variable_t *arg = ASRUtils::EXPR2VAR(x.m_args[i]);
            if (ASRUtils::is_array(arg->m_type)) {

                ASR::dimension_t* m_dims;
                size_t n_dims = ASRUtils::extract_dimensions_from_ttype(arg->m_type, m_dims);

                for (size_t j = 0; j < n_dims; j++) {
                    auto type = ASR::make_Integer_t(al, arg->base.base.loc, 4 /* FIXME: support other kinds */, nullptr, 0);
                    auto variable = ASR::make_Variable_t(al, arg->base.base.loc, nullptr, s2c(al, "n" + std::string(arg->m_name) + std::to_string(j + 1)),
                        ASR::intentType::In, nullptr, nullptr, ASR::storage_typeType::Default,
                        ASRUtils::TYPE(type), ASR::abiType::Source, ASR::accessType::Public,
                        ASR::presenceType::Required, false);
                    auto var = ASR::make_Var_t(al, arg->base.base.loc, ASR::down_cast<ASR::symbol_t>(variable));
                    params.push_back(al, ASRUtils::EXPR(var));
                    m_dims[j].m_length = ASRUtils::EXPR(var);
                }
            }
            params.push_back(al, x.m_args[i]);
        }

        ASR::Subroutine_t xx = const_cast<ASR::Subroutine_t &>(x);
        xx.n_args = params.size();
        xx.m_args = params.p;
    }
};

void pass_propagate_arr_dims(Allocator &al, ASR::TranslationUnit_t &unit) {
    ArrDimsPropagate v(al);
    v.visit_TranslationUnit(unit);
    LFORTRAN_ASSERT(asr_verify(unit));
}

} // namespace LFortran

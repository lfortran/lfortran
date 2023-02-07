#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/arr_slice.h>

#include <vector>
#include <utility>


namespace LCompilers {

class ReplaceArraySection: public ASR::BaseExprReplacer<ReplaceArraySection> {

    private:

    Allocator& al;

    public:

    ReplaceArraySection(Allocator& al_) : al(al_)
    {}

    void replace_ArraySection(ASR::ArraySection_t* x) {
        ASR::expr_t* x_arr_var = x.m_v;
        Str new_name_str;
        new_name_str.from_str(al, "~" + std::to_string(slice_counter) + "_slice");
        slice_counter += 1;
        char* new_var_name = (char*)new_name_str.c_str(al);
        ASR::asr_t* slice_asr = ASR::make_Variable_t(al, x.base.base.loc, current_scope, new_var_name, nullptr, 0,
                                                    ASR::intentType::Local, nullptr, nullptr, ASR::storage_typeType::Default,
                                                    get_array_from_slice(x, x_arr_var), ASR::abiType::Source, ASR::accessType::Public,
                                                    ASR::presenceType::Required, false);
        ASR::symbol_t* slice_sym = ASR::down_cast<ASR::symbol_t>(slice_asr);
        current_scope->add_symbol(std::string(new_var_name), slice_sym);
        slice_var = ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, slice_sym));
        Vec<ASR::expr_t*> idx_vars_target, idx_vars_value;
        PassUtils::create_idx_vars(idx_vars_target, x.n_args, x.base.base.loc, al, current_scope, "_t");
        PassUtils::create_idx_vars(idx_vars_value, x.n_args, x.base.base.loc, al, current_scope, "_v");
        ASR::stmt_t* doloop = nullptr;
        int a_kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(x.m_v));
        ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, x.base.base.loc, a_kind, nullptr, 0));
        ASR::expr_t* const_1 = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, x.base.base.loc, 1, int_type));
        for( int i = (int)x.n_args - 1; i >= 0; i-- ) {
            ASR::do_loop_head_t head;
            head.m_v = idx_vars_value[i];
            if( x.m_args[i].m_step != nullptr ) {
                if( x.m_args[i].m_left == nullptr ) {
                    head.m_start = PassUtils::get_bound(x_arr_var, i + 1, "lbound", al);
                } else {
                    head.m_start = x.m_args[i].m_left;
                }
                if( x.m_args[i].m_right == nullptr ) {
                    head.m_end = PassUtils::get_bound(x_arr_var, i + 1, "ubound", al);
                } else {
                    head.m_end = x.m_args[i].m_right;
                }
            } else {
                head.m_start = x.m_args[i].m_right;
                head.m_end = x.m_args[i].m_right;
            }
            head.m_increment = x.m_args[i].m_step;
            head.loc = head.m_v->base.loc;
            Vec<ASR::stmt_t*> doloop_body;
            doloop_body.reserve(al, 1);
            if( doloop == nullptr ) {
                ASR::expr_t* target_ref = PassUtils::create_array_ref(slice_sym, idx_vars_target, al, x.base.base.loc, x.m_type);
                ASR::expr_t* value_ref = PassUtils::create_array_ref(x.m_v, idx_vars_value, al);
                ASR::stmt_t* assign_stmt = ASRUtils::STMT(ASR::make_Assignment_t(al, x.base.base.loc, target_ref, value_ref, nullptr));
                doloop_body.push_back(al, assign_stmt);
            } else {
                ASR::stmt_t* set_to_one = ASRUtils::STMT(ASR::make_Assignment_t(al, x.base.base.loc, idx_vars_target[i+1], const_1, nullptr));
                doloop_body.push_back(al, set_to_one);
                doloop_body.push_back(al, doloop);
            }
            ASR::expr_t* inc_expr = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, x.base.base.loc, idx_vars_target[i], ASR::binopType::Add, const_1, int_type, nullptr));
            ASR::stmt_t* assign_stmt = ASRUtils::STMT(ASR::make_Assignment_t(al, x.base.base.loc, idx_vars_target[i], inc_expr, nullptr));
            doloop_body.push_back(al, assign_stmt);
            doloop = ASRUtils::STMT(ASR::make_DoLoop_t(al, x.base.base.loc, head, doloop_body.p, doloop_body.size()));
        }
        ASR::stmt_t* set_to_one = ASRUtils::STMT(ASR::make_Assignment_t(al, x.base.base.loc, idx_vars_target[0], const_1, nullptr));
        pass_result.push_back(al, set_to_one);
        pass_result.push_back(al, doloop);
    }

};

class ArraySectionVisitor : public ASR::CallReplacerOnExpressionsVisitor<ArraySectionVisitor>
{
    private:

        ReplaceArraySection replacer;

    public:

        ArraySectionVisitor(Allocator& al_) : replacer(al_) {}

        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.replace_expr(*current_expr);
        }

};

void pass_replace_arr_slice(Allocator &al, ASR::TranslationUnit_t &unit,
                            const LCompilers::PassOptions& pass_options) {
    ArraySectionVisitor v(al);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers

#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/replace_coarray.h>

namespace LCompilers {

class PRIFInterface {
    private:
        Allocator &al;
        ASR::TranslationUnit_t &unit;

        ASR::symbol_t* get_or_create_prif_get(const Location &loc,
                                              ASR::ttype_t *return_type,
                                              ASR::expr_t *base_expr) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string type_suffix = ASRUtils::type_to_str_with_kind(return_type, base_expr);
            std::string symbol_name = "prif_get_" + type_suffix;

            if (ASR::symbol_t *existing = global_scope->get_symbol(symbol_name)) {
                return existing;
            }

            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);

            ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            ASR::expr_t *base_arg = b.Variable(fn_symtab, "base", return_type,
                                               ASR::intentType::In, nullptr,
                                               ASR::abiType::BindC, true);
            ASR::expr_t *coidx_arg = b.Variable(fn_symtab, "coindices", int32_type,
                                                ASR::intentType::In, nullptr,
                                                ASR::abiType::BindC, true);
            ASR::expr_t *return_var = b.Variable(fn_symtab, "result", return_type,
                                                ASR::intentType::ReturnVar, nullptr,
                                                ASR::abiType::BindC, true);

            Vec<ASR::expr_t*> args;
            args.reserve(al, 2);
            args.push_back(al, base_arg);
            args.push_back(al, coidx_arg);

            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, symbol_name), nullptr, 0,
                args.p, args.n, nullptr, 0, return_var,
                ASR::abiType::BindC, ASR::accessType::Public,
                ASR::deftypeType::Interface, s2c(al, "prif_get"),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);

            global_scope->add_symbol(symbol_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }

    public:
        PRIFInterface(Allocator &al_, ASR::TranslationUnit_t &unit_)
            : al(al_), unit(unit_) {}

        ASR::expr_t* make_prif_get_call(const Location &loc,
                                        ASR::expr_t *base_expr,
                                        ASR::array_index_t *coindices,
                                        size_t n_coindices,
                                        ASR::ttype_t *return_type) {
            Vec<ASR::expr_t*> coidx_values;
            coidx_values.reserve(al, n_coindices);
            for (size_t i = 0; i < n_coindices; i++) {
                if (coindices[i].m_left) {
                    coidx_values.push_back(al, coindices[i].m_left);
                }
            }

            ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            ASR::expr_t *coidx_array = ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(
                al, loc, coidx_values.p, coidx_values.n, int32_type,
                ASR::arraystorageType::ColMajor));

            ASR::symbol_t *prif_get_sym = get_or_create_prif_get(loc, return_type, base_expr);

            Vec<ASR::call_arg_t> args;
            args.reserve(al, 2);
            ASR::call_arg_t base_arg;
            base_arg.loc = loc;
            base_arg.m_value = base_expr;
            args.push_back(al, base_arg);
            ASR::call_arg_t coidx_arg;
            coidx_arg.loc = loc;
            coidx_arg.m_value = coidx_array;
            args.push_back(al, coidx_arg);

            return ASRUtils::EXPR(ASR::make_FunctionCall_t(
                al, loc, prif_get_sym, prif_get_sym,
                args.p, args.n, return_type, nullptr, nullptr));
        }
};

class CoarrayPrifReplacer : public ASR::BaseExprReplacer<CoarrayPrifReplacer> {
    public:
        Allocator &al;
        PRIFInterface &prif;
        SymbolTable *current_scope = nullptr;
        bool allow_coarray_ref = true;

        CoarrayPrifReplacer(Allocator &al_, PRIFInterface &prif_)
            : al(al_), prif(prif_) {}

        void replace_CoarrayRef(ASR::CoarrayRef_t *x) {
            if (!allow_coarray_ref) {
                return;
            }
            ASR::expr_t *call = prif.make_prif_get_call(
                x->base.base.loc, x->m_var, x->m_coindices,
                x->n_coindices, x->m_type);
            *current_expr = call;
        }
};

class CoarrayPrifVisitor : public ASR::CallReplacerOnExpressionsVisitor<CoarrayPrifVisitor> {
    private:
        CoarrayPrifReplacer replacer;
    public:
        CoarrayPrifVisitor(Allocator &al, PRIFInterface &prif)
            : replacer(al, prif) {}

        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.current_scope = current_scope;
            replacer.replace_expr(*current_expr);
        }

        void visit_Assignment(const ASR::Assignment_t &x) {
            ASR::Assignment_t &xx = const_cast<ASR::Assignment_t &>(x);
            bool allow_copy = replacer.allow_coarray_ref;
            replacer.allow_coarray_ref = false;
            ASR::expr_t **current_expr_copy_0 = current_expr;
            current_expr = &(xx.m_target);
            call_replacer();
            current_expr = current_expr_copy_0;
            if (xx.m_target && visit_expr_after_replacement) {
                visit_expr(*xx.m_target);
            }
            replacer.allow_coarray_ref = allow_copy;

            ASR::expr_t **current_expr_copy_1 = current_expr;
            current_expr = &(xx.m_value);
            call_replacer();
            current_expr = current_expr_copy_1;
            if (xx.m_value && visit_expr_after_replacement) {
                visit_expr(*xx.m_value);
            }
            if (x.m_overloaded) {
                visit_stmt(*x.m_overloaded);
            }
        }
};

void pass_replace_coarray(Allocator &al, ASR::TranslationUnit_t &unit,
                               const LCompilers::PassOptions &) {
    PRIFInterface prif(al, unit);
    CoarrayPrifVisitor v(al, prif);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
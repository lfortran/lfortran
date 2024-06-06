#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/replace_openmp.h>

namespace LCompilers {

class InvolvedSymbolsCollector:
    public ASR::BaseWalkVisitor<InvolvedSymbolsCollector>
{
    private:
        std::map<std::string, ASR::ttype_t*> &symbols;
    public:
        InvolvedSymbolsCollector(std::map<std::string, ASR::ttype_t*> &symbols) :
            symbols(symbols) {}

        void visit_Var(const ASR::Var_t &x) {
            symbols[to_lower(ASRUtils::symbol_name(x.m_v))] = ASRUtils::symbol_type(x.m_v);
            return;
        }
};

class DoConcurrentVisitor :
    public ASR::BaseWalkVisitor<DoConcurrentVisitor>
{
    private:
        Allocator& al;
        bool remove_original_statement;
        Vec<ASR::stmt_t*> pass_result;
        SymbolTable* current_scope;
        PassOptions pass_options;
    public:
        DoConcurrentVisitor(Allocator& al_, PassOptions pass_options_) :
        al(al_), remove_original_statement(false), pass_options(pass_options_) {
            pass_result.n = 0;
        }

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            bool remove_original_statement_copy = remove_original_statement;
            Vec<ASR::stmt_t*> body;
            body.reserve(al, n_body);
            for (size_t i=0; i<n_body; i++) {
                pass_result.n = 0;
                pass_result.reserve(al, 1);
                remove_original_statement = false;
                visit_stmt(*m_body[i]);
                for (size_t j=0; j < pass_result.size(); j++) {
                    body.push_back(al, pass_result[j]);
                }
                if( !remove_original_statement ) {
                    body.push_back(al, m_body[i]);
                }
            }
            m_body = body.p;
            n_body = body.size();
            pass_result.n = 0;
            remove_original_statement = remove_original_statement_copy;
        }

        std::string import_all(const ASR::Module_t* m, bool to_submodule=false) {
            // Import all symbols from the module, e.g.:
            //     use a
            for (auto &item : m->m_symtab->get_scope()) {
                if( current_scope->get_symbol(item.first) != nullptr ) {
                    continue;
                }
                // TODO: only import "public" symbols from the module
                if (ASR::is_a<ASR::Function_t>(*item.second)) {
                    ASR::Function_t *mfn = ASR::down_cast<ASR::Function_t>(item.second);
                    ASR::asr_t *fn = ASR::make_ExternalSymbol_t(
                        al, mfn->base.base.loc,
                        /* a_symtab */ current_scope,
                        /* a_name */ mfn->m_name,
                        (ASR::symbol_t*)mfn,
                        m->m_name, nullptr, 0, mfn->m_name,
                        ASR::accessType::Public
                        );
                    std::string sym = to_lower(mfn->m_name);
                    current_scope->add_symbol(sym, ASR::down_cast<ASR::symbol_t>(fn));
                } else if (ASR::is_a<ASR::GenericProcedure_t>(*item.second)) {
                    ASR::GenericProcedure_t *gp = ASR::down_cast<
                        ASR::GenericProcedure_t>(item.second);
                    ASR::asr_t *ep = ASR::make_ExternalSymbol_t(
                        al, gp->base.base.loc,
                        current_scope,
                        /* a_name */ gp->m_name,
                        (ASR::symbol_t*)gp,
                        m->m_name, nullptr, 0, gp->m_name,
                        ASR::accessType::Public
                        );
                    std::string sym = to_lower(gp->m_name);
                    current_scope->add_symbol(sym, ASR::down_cast<ASR::symbol_t>(ep));
                }  else if (ASR::is_a<ASR::CustomOperator_t>(*item.second)) {
                    ASR::CustomOperator_t *gp = ASR::down_cast<
                        ASR::CustomOperator_t>(item.second);
                    ASR::asr_t *ep = ASR::make_ExternalSymbol_t(
                        al, gp->base.base.loc,
                        current_scope,
                        /* a_name */ gp->m_name,
                        (ASR::symbol_t*)gp,
                        m->m_name, nullptr, 0, gp->m_name,
                        ASR::accessType::Public
                        );
                    std::string sym = to_lower(gp->m_name);
                    current_scope->add_symbol(sym, ASR::down_cast<ASR::symbol_t>(ep));
                } else if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                    ASR::Variable_t *mvar = ASR::down_cast<ASR::Variable_t>(item.second);
                    // check if m_access of mvar is public
                    if ( mvar->m_access == ASR::accessType::Public || to_submodule ) {
                        ASR::asr_t *var = ASR::make_ExternalSymbol_t(
                            al, mvar->base.base.loc,
                            /* a_symtab */ current_scope,
                            /* a_name */ mvar->m_name,
                            (ASR::symbol_t*)mvar,
                            m->m_name, nullptr, 0, mvar->m_name,
                            ASR::accessType::Public
                            );
                        std::string sym = to_lower(mvar->m_name);
                        current_scope->add_symbol(sym, ASR::down_cast<ASR::symbol_t>(var));
                    }
                } else if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) {
                    // We have to "repack" the ExternalSymbol so that it lives in the
                    // local symbol table
                    ASR::ExternalSymbol_t *es0 = ASR::down_cast<ASR::ExternalSymbol_t>(item.second);
                    std::string sym;
                    sym = to_lower(es0->m_original_name);
                    ASR::asr_t *es = ASR::make_ExternalSymbol_t(
                        al, es0->base.base.loc,
                        /* a_symtab */ current_scope,
                        /* a_name */ s2c(al, sym),
                        es0->m_external,
                        es0->m_module_name, nullptr, 0,
                        es0->m_original_name,
                        ASR::accessType::Public
                        );
                    current_scope->add_or_overwrite_symbol(sym, ASR::down_cast<ASR::symbol_t>(es));
                } else if( ASR::is_a<ASR::StructType_t>(*item.second) ) {
                    ASR::StructType_t *mv = ASR::down_cast<ASR::StructType_t>(item.second);
                    // `mv` is the Variable in a module. Now we construct
                    // an ExternalSymbol that points to it.
                    Str name;
                    name.from_str(al, item.first);
                    char *cname = name.c_str(al);
                    ASR::asr_t *v = ASR::make_ExternalSymbol_t(
                        al, mv->base.base.loc,
                        /* a_symtab */ current_scope,
                        /* a_name */ cname,
                        (ASR::symbol_t*)mv,
                        m->m_name, nullptr, 0, mv->m_name,
                        ASR::accessType::Public
                        );
                    current_scope->add_symbol(item.first, ASR::down_cast<ASR::symbol_t>(v));
                } else if (ASR::is_a<ASR::Requirement_t>(*item.second)) {
                    ASR::Requirement_t *req = ASR::down_cast<ASR::Requirement_t>(item.second);
                    Str name;
                    name.from_str(al, item.first);
                    char *cname = name.c_str(al);
                    ASR::asr_t *v = ASR::make_ExternalSymbol_t(
                        al, req->base.base.loc,
                        current_scope,
                        cname,
                        (ASR::symbol_t*)req,
                        m->m_name, nullptr, 0, req->m_name,
                        ASR::accessType::Public
                    );
                    current_scope->add_symbol(item.first, ASR::down_cast<ASR::symbol_t>(v));
                } else if (ASR::is_a<ASR::Template_t>(*item.second)) {
                    ASR::Template_t *temp = ASR::down_cast<ASR::Template_t>(item.second);
                    Str name;
                    name.from_str(al, item.first);
                    char *cname = name.c_str(al);
                    ASR::asr_t *v = ASR::make_ExternalSymbol_t(
                        al, temp->base.base.loc,
                        current_scope,
                        cname,
                        (ASR::symbol_t*)temp,
                        m->m_name, nullptr, 0, temp->m_name,
                        ASR::accessType::Public
                    );
                    current_scope->add_symbol(item.first, ASR::down_cast<ASR::symbol_t>(v));
                } else {
                    return item.first;
                }
            }
            return "";
        }

        std::pair<std::string, ASR::symbol_t*> create_thread_data_module(std::map<std::string, ASR::ttype_t*> &involved_symbols, const Location& loc) {
            SymbolTable* current_scope_copy = current_scope;
            while (current_scope->parent != nullptr) {
                current_scope = current_scope->parent;
            }
            SetChar module_dependencies; module_dependencies.reserve(al, 1);
            module_dependencies.push_back(al, s2c(al, "iso_c_binding"));
            ASR::symbol_t* iso_c_binding = (ASR::symbol_t*)(ASRUtils::load_module(al, current_scope,
                "iso_c_binding", loc, false, pass_options, true,
                [&](const std::string &/*msg*/, const Location &/*loc*/) { }
                ));
            LCOMPILERS_ASSERT(iso_c_binding != nullptr && ASR::is_a<ASR::Module_t>(*iso_c_binding));
            current_scope = al.make_new<SymbolTable>(current_scope);
            std::string unsupported_sym_name = import_all(ASR::down_cast<ASR::Module_t>(iso_c_binding));
            LCOMPILERS_ASSERT(unsupported_sym_name == "");

            // create StructType
            ASRUtils::ASRBuilder b(al, loc);
            SymbolTable* parent_scope = current_scope;
            current_scope = al.make_new<SymbolTable>(parent_scope);
            SetChar involved_symbols_set; involved_symbols_set.reserve(al, involved_symbols.size());
            for (auto it: involved_symbols) {
                // TODO: handle array dimension correctly
                LCOMPILERS_ASSERT(b.Variable(current_scope, it.first, it.second, ASR::intentType::Local) != nullptr);
                involved_symbols_set.push_back(al, s2c(al, it.first));
            }
            ASR::symbol_t* thread_data_struct = ASR::down_cast<ASR::symbol_t>(ASR::make_StructType_t(al, loc,
                current_scope, s2c(al, "thread_data"), nullptr, 0, involved_symbols_set.p, involved_symbols_set.n, ASR::abiType::Source,
                ASR::accessType::Public, false, false, nullptr, 0, nullptr, nullptr));
            current_scope->parent->add_symbol("thread_data", thread_data_struct);
            current_scope = parent_scope;
            ASR::symbol_t* thread_data_module = ASR::down_cast<ASR::symbol_t>(ASR::make_Module_t(al, loc,
                                                current_scope, s2c(al, "thread_data_module"), module_dependencies.p, module_dependencies.n, false, false));
            current_scope->parent->add_symbol("thread_data_module", thread_data_module);
            current_scope = current_scope_copy;
            return {ASRUtils::symbol_name(thread_data_module), thread_data_struct};
        }

        ASR::ttype_t* f_type_to_c_type(ASR::ttype_t* /*f_type*/) {
            // populate it when required
            // right now in ASR `integer(c_int) :: n` is represented same as `integer :: n`
            return nullptr;
        }

        void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
            std::map<std::string, ASR::ttype_t*> involved_symbols;

            InvolvedSymbolsCollector c(involved_symbols);
            c.visit_DoConcurrentLoop(x);

            // create thread data module
            std::pair<std::string, ASR::symbol_t*> thread_data_module = create_thread_data_module(involved_symbols, x.base.base.loc);

            // create external symbol for the thread data module
            ASR::symbol_t* thread_data_ext_sym = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(al, x.base.base.loc,
                current_scope, ASRUtils::symbol_name(thread_data_module.second), thread_data_module.second, s2c(al, thread_data_module.first),
                nullptr, 0, ASRUtils::symbol_name(thread_data_module.second), ASR::accessType::Public));
            current_scope->add_symbol(ASRUtils::symbol_name(thread_data_module.second), thread_data_ext_sym);

            // create data variable for the thread data module
            ASRUtils::ASRBuilder b(al, x.base.base.loc);
            ASR::expr_t* data_expr = b.Variable(current_scope, "data", ASRUtils::TYPE(ASR::make_Struct_t(al, x.base.base.loc, thread_data_ext_sym)), ASR::intentType::Local);
            LCOMPILERS_ASSERT(data_expr != nullptr);

            // now create a tdata (cptr)
            ASR::expr_t* tdata_expr = b.Variable(current_scope, "tdata", ASRUtils::TYPE(ASR::make_CPtr_t(al, x.base.base.loc)), ASR::intentType::Local);
            LCOMPILERS_ASSERT(tdata_expr != nullptr);

            // TODO: update symbols with correct type
            
            // add external symbols to struct members, we need those for `data%n = n`
            ASR::symbol_t* thread_data_sym = thread_data_module.second;
            SymbolTable* thread_data_symtab = ASRUtils::symbol_symtab(thread_data_sym);
            for (auto it: involved_symbols) {
                std::string sym_name = std::string(ASRUtils::symbol_name(thread_data_sym)) + "_" + it.first;
                ASR::symbol_t* sym = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(al, x.base.base.loc,
                    current_scope, s2c(al, sym_name), thread_data_symtab->resolve_symbol(it.first), ASRUtils::symbol_name(thread_data_sym), nullptr, 0,
                    s2c(al, it.first), ASR::accessType::Public));
                current_scope->add_symbol(sym_name, sym);

                // handle arrays
                pass_result.push_back(al, b.Assignment(
                    ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, x.base.base.loc, data_expr,
                    sym, ASRUtils::symbol_type(sym), nullptr)),
                    b.Var(current_scope->get_symbol(it.first))
                ));
            }

            // tdata = c_loc(data)
            pass_result.push_back(al, b.Assignment(
                tdata_expr,
                ASRUtils::EXPR(ASR::make_PointerToCPtr_t(al, x.base.base.loc,
                    ASRUtils::EXPR(ASR::make_GetPointer_t(al, x.base.base.loc, data_expr, ASRUtils::expr_type(data_expr), nullptr)),
                    ASRUtils::expr_type(tdata_expr), nullptr))
            ));

            remove_original_statement = true;
            /* TODO:
                1. create lcompilers_xx function that takes loop body and thread_data_module as input
                2. create a GOMP_parallel call with lcompilers_xx as input
            */
            return;
        }

        void visit_Function(const ASR::Function_t &x) {
            // FIXME: this is a hack, we need to pass in a non-const `x`,
            // which requires to generate a TransformVisitor.
            ASR::Function_t& xx = const_cast<ASR::Function_t&>(x);
            SymbolTable* current_scope_copy = current_scope;
            current_scope = xx.m_symtab;

            for (auto &item : x.m_symtab->get_scope()) {
                this->visit_symbol(*item.second);
            }

            transform_stmts(xx.m_body, xx.n_body);
            current_scope = current_scope_copy;
        }

};

void pass_replace_openmp(Allocator &al, ASR::TranslationUnit_t &unit,
                            const PassOptions &pass_options) {
    if (pass_options.openmp) {
        DoConcurrentVisitor v(al, pass_options);
        v.visit_TranslationUnit(unit);
    }
    return;
}

} // namespace LCompilers

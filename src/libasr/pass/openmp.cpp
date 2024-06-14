#include <limits.h>
#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/replace_openmp.h>

namespace LCompilers {

class ReplaceArrayVariable: public ASR::BaseExprReplacer<ReplaceArrayVariable> {
    private:
        Allocator& al;
    public:
        SymbolTable* current_scope;
        std::vector<std::string> array_variables;

        ReplaceArrayVariable(Allocator& al_) :
            al(al_) {}

        void replace_Var(ASR::Var_t* x) {
            ASRUtils::ASRBuilder b(al, x->base.base.loc);
            if (std::find(array_variables.begin(), array_variables.end(), ASRUtils::symbol_name(x->m_v)) != array_variables.end() &&
                ASRUtils::symbol_parent_symtab(x->m_v)->counter == current_scope->counter) {
                // TODO: Ideally we shall not need any check for the symbol parent symtab
                // This is a bug where somehow it changes symbol present in lcompilers_function or say not of the current_scope
                ASR::symbol_t* sym = current_scope->get_symbol(std::string(ASRUtils::symbol_name(x->m_v)));
                LCOMPILERS_ASSERT(sym != nullptr);
                *current_expr = b.Var(sym);
            }
        }
};

class ArrayVisitor: public ASR::CallReplacerOnExpressionsVisitor<ArrayVisitor> {
    private:
        SymbolTable* current_scope;
        ReplaceArrayVariable replacer;
        std::vector<std::string> array_variables;

    public:
        ArrayVisitor(Allocator &al_, SymbolTable* current_scope_, std::vector<std::string> array_variables_) :
            current_scope(current_scope_), replacer(al_) , array_variables(array_variables_) {}

        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.current_scope = current_scope;
            replacer.array_variables = array_variables;
            replacer.replace_expr(*current_expr);
        }
};

class CheckIfAlreadyAllocatedVisitor: public ASR::BaseWalkVisitor<CheckIfAlreadyAllocatedVisitor> {
    private:
        bool &already_allocated;
        int array_variable_index;
        std::string function_name;
        SymbolTable* current_scope;
        std::string array_variable_name;

    public:
        CheckIfAlreadyAllocatedVisitor(int array_variable_index_, std::string function_name_, std::string array_variable_name_, bool &already_allocated_) :
            already_allocated(already_allocated_), array_variable_index(array_variable_index_),
            function_name(function_name_), array_variable_name(array_variable_name_) {}

        void visit_Program(const ASR::Program_t &x) {
            ASR::Program_t& xx = const_cast<ASR::Program_t&>(x);
            SymbolTable* current_scope_copy = current_scope;
            current_scope = xx.m_symtab;

            BaseWalkVisitor::visit_Program(x);

            current_scope = current_scope_copy;
        }

        void visit_Function(const ASR::Function_t &x) {
            ASR::Function_t& xx = const_cast<ASR::Function_t&>(x);
            SymbolTable* current_scope_copy = current_scope;
            current_scope = xx.m_symtab;

            BaseWalkVisitor::visit_Function(x);

            current_scope = current_scope_copy;
        }

        void visit_FunctionCall(const ASR::FunctionCall_t &x) {
            if (ASRUtils::symbol_name(x.m_name) == function_name) {
                ASR::expr_t* arg = x.m_args[array_variable_index].m_value;
                if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg)) {
                    arg = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg)->m_arg;
                }
                if (ASR::is_a<ASR::Var_t>(*arg)) {
                    ASR::ttype_t* sym_type = ASRUtils::symbol_type(current_scope->get_symbol(ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(arg)->m_v)));
                    already_allocated &= (ASRUtils::is_pointer(sym_type) || ASRUtils::is_allocatable(sym_type));
                }
            }
            BaseWalkVisitor::visit_FunctionCall(x);
        }

        void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
            if (ASRUtils::symbol_name(x.m_name) == function_name) {
                ASR::expr_t* arg = x.m_args[array_variable_index].m_value;
                if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg)) {
                    arg = ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg)->m_arg;
                }
                if (ASR::is_a<ASR::Var_t>(*arg)) {
                    ASR::ttype_t* sym_type = ASRUtils::symbol_type(current_scope->get_symbol(ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(arg)->m_v)));
                    already_allocated &= (ASRUtils::is_pointer(sym_type) || ASRUtils::is_allocatable(sym_type));
                }
            }
            BaseWalkVisitor::visit_SubroutineCall(x);
        }
};

class FunctionSubroutineCallVisitor: public ASR::BaseWalkVisitor<FunctionSubroutineCallVisitor> {
    private:
        std::string function_name;
        SymbolTable* current_scope;
        std::vector<SymbolTable*> &scopes;
    
    public:
        FunctionSubroutineCallVisitor(std::string function_name_, std::vector<SymbolTable*> &scopes_) :
            function_name(function_name_), scopes(scopes_) {}
        
        void visit_Program(const ASR::Program_t &x) {
            ASR::Program_t& xx = const_cast<ASR::Program_t&>(x);
            SymbolTable* current_scope_copy = current_scope;
            current_scope = xx.m_symtab;

            BaseWalkVisitor::visit_Program(x);

            current_scope = current_scope_copy;
        }

        void visit_Function(const ASR::Function_t &x) {
            ASR::Function_t& xx = const_cast<ASR::Function_t&>(x);
            SymbolTable* current_scope_copy = current_scope;
            current_scope = xx.m_symtab;

            BaseWalkVisitor::visit_Function(x);

            current_scope = current_scope_copy;
        }

        void visit_FunctionCall(const ASR::FunctionCall_t& x) {
            if (ASRUtils::symbol_name(x.m_name) == function_name)
                scopes.push_back(current_scope);

            BaseWalkVisitor::visit_FunctionCall(x);
        }

        void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
            if (ASRUtils::symbol_name(x.m_name) == function_name)
                scopes.push_back(current_scope);

            BaseWalkVisitor::visit_SubroutineCall(x);
        }
};

class ReplaceReductionVariable: public ASR::BaseExprReplacer<ReplaceReductionVariable> {
    private:
        Allocator& al;
    public:
        ASR::expr_t* data_expr;
        SymbolTable* current_scope;
        std::string thread_data_name;
        std::vector<std::string> reduction_variables;

        ReplaceReductionVariable(Allocator& al_) :
            al(al_) {}

        void replace_Var(ASR::Var_t* x) {
            if (std::find(reduction_variables.begin(), reduction_variables.end(), ASRUtils::symbol_name(x->m_v)) != reduction_variables.end()) {
                ASR::symbol_t* sym = current_scope->get_symbol(thread_data_name + "_" + std::string(ASRUtils::symbol_name(x->m_v)));
                LCOMPILERS_ASSERT(sym != nullptr);
                *current_expr = ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, x->base.base.loc, data_expr, sym, ASRUtils::symbol_type(sym), nullptr));
            }
        }
};

class ReductionVariableVisitor: public ASR::CallReplacerOnExpressionsVisitor<ReductionVariableVisitor> {
    private:
        ASR::expr_t* data_expr;
        SymbolTable* current_scope;
        std::string thread_data_name;
        ReplaceReductionVariable replacer;
        std::vector<std::string> reduction_variables;

    public:
        ReductionVariableVisitor(Allocator &al_, SymbolTable* current_scope_, std::string thread_data_name_, ASR::expr_t* data_expr_,
            std::vector<std::string> reduction_variables_) :
            data_expr(data_expr_), current_scope(current_scope_), thread_data_name(thread_data_name_), replacer(al_) {
                reduction_variables = reduction_variables_;
            }

        void call_replacer() {
            replacer.data_expr = data_expr;
            replacer.current_expr = current_expr;
            replacer.current_scope = current_scope;
            replacer.thread_data_name = thread_data_name;
            replacer.reduction_variables = reduction_variables;
            replacer.replace_expr(*current_expr);
        }
};

class ReplaceExpression: public ASR::BaseExprReplacer<ReplaceExpression> {
    private:
        Allocator& al;
    public:
        SymbolTable* current_scope;

        ReplaceExpression(Allocator& al_) :
            al(al_) {}

        void replace_Var(ASR::Var_t* x) {
            ASR::symbol_t* sym = current_scope->get_symbol(ASRUtils::symbol_name(x->m_v));
            LCOMPILERS_ASSERT(sym != nullptr);
            *current_expr = ASRUtils::EXPR(ASR::make_Var_t(al, x->base.base.loc, sym));
        }

};

class DoConcurrentStatementVisitor : public ASR::CallReplacerOnExpressionsVisitor<DoConcurrentStatementVisitor> {
    private:
        SymbolTable* current_scope;
        ReplaceExpression replacer;

    public:
        DoConcurrentStatementVisitor(Allocator &al_, SymbolTable* current_scope_) :
            current_scope(current_scope_), replacer(al_) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.current_scope = current_scope;
        replacer.replace_expr(*current_expr);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::FunctionCall_t* x_copy = const_cast<ASR::FunctionCall_t*>(&x);
        ASR::symbol_t* func_sym = current_scope->get_symbol(ASRUtils::symbol_name(x.m_name));
        LCOMPILERS_ASSERT(func_sym != nullptr);
        x_copy->m_name = func_sym;
        x_copy->m_original_name = func_sym;
        CallReplacerOnExpressionsVisitor::visit_FunctionCall(x);
    }
};

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
        int current_stmt_index = -1;
        ASR::stmt_t** current_m_body; size_t current_n_body;
        std::vector<std::string> reduction_variables;
    public:
        DoConcurrentVisitor(Allocator& al_, PassOptions pass_options_) :
        al(al_), remove_original_statement(false), pass_options(pass_options_) {
            pass_result.n = 0;
        }

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            bool remove_original_statement_copy = remove_original_statement;
            Vec<ASR::stmt_t*> body;
            body.reserve(al, n_body);
            current_m_body = m_body;
            current_n_body = n_body;
            for (size_t i=0; i<n_body; i++) {
                pass_result.n = 0;
                current_stmt_index = i;
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
            current_m_body = nullptr;
            current_n_body = 0;
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
                } else if( ASR::is_a<ASR::Struct_t>(*item.second) ) {
                    ASR::Struct_t *mv = ASR::down_cast<ASR::Struct_t>(item.second);
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

        ASR::symbol_t* create_module(const Location& loc, std::string module_name) {
            SymbolTable* current_scope_copy = current_scope;
            while (current_scope->parent != nullptr) {
                current_scope = current_scope->parent;
            }
            SetChar module_dependencies; module_dependencies.reserve(al, 1);
            module_dependencies.push_back(al, s2c(al, module_name));
            ASR::symbol_t* module_sym = (ASR::symbol_t*)(ASRUtils::load_module(al, current_scope,
                module_name, loc, false, pass_options, true,
                [&](const std::string &/*msg*/, const Location &/*loc*/) { }
                ));
            LCOMPILERS_ASSERT(module_sym != nullptr && ASR::is_a<ASR::Module_t>(*module_sym));
            current_scope = current_scope_copy;
            return module_sym;
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

            // create Struct
            ASRUtils::ASRBuilder b(al, loc);
            SymbolTable* parent_scope = current_scope;
            current_scope = al.make_new<SymbolTable>(parent_scope);
            SetChar involved_symbols_set; involved_symbols_set.reserve(al, involved_symbols.size());
            for (auto it: involved_symbols) {
                ASR::ttype_t* sym_type = nullptr;
                sym_type = ASRUtils::is_array(it.second) ? b.CPtr() : it.second;
                b.VariableDeclaration(current_scope, it.first, sym_type, ASR::intentType::Local);
                involved_symbols_set.push_back(al, s2c(al, it.first));
            }
            std::string thread_data_module_name = parent_scope->parent->get_unique_name("thread_data_module");
            std::string suffix = thread_data_module_name.substr(18);
            std::string thread_data_name = "thread_data" + suffix;
            ASR::symbol_t* thread_data_struct = ASR::down_cast<ASR::symbol_t>(ASR::make_Struct_t(al, loc,
                current_scope, s2c(al, thread_data_name), nullptr, 0, involved_symbols_set.p, involved_symbols_set.n, ASR::abiType::Source,
                ASR::accessType::Public, false, false, nullptr, 0, nullptr, nullptr));
            current_scope->parent->add_symbol(thread_data_name, thread_data_struct);
            current_scope = parent_scope;
            ASR::symbol_t* thread_data_module = ASR::down_cast<ASR::symbol_t>(ASR::make_Module_t(al, loc,
                                                current_scope, s2c(al, thread_data_module_name),
                                                module_dependencies.p, module_dependencies.n, false, false));
            current_scope->parent->add_symbol(thread_data_module_name, thread_data_module);
            current_scope = current_scope_copy;
            return {thread_data_module_name, thread_data_struct};
        }

        ASR::symbol_t* create_lcompilers_function(const Location &loc, const ASR::DoConcurrentLoop_t &do_loop,
                    std::map<std::string, ASR::ttype_t*> &involved_symbols,
                    std::map<std::string, ASR::expr_t*> &array_variables_to_allocate, std::string thread_data_module_name) {
            SymbolTable* current_scope_copy = current_scope;
            while (current_scope->parent != nullptr) {
                current_scope = current_scope->parent;
            }
            current_scope = al.make_new<SymbolTable>(current_scope);
            // load modules
            ASR::symbol_t* mod_sym = create_module(loc, "iso_c_binding");
            LCOMPILERS_ASSERT(mod_sym != nullptr && ASR::is_a<ASR::Module_t>(*mod_sym));
            std::string unsupported_sym_name = import_all(ASR::down_cast<ASR::Module_t>(mod_sym));
            LCOMPILERS_ASSERT(unsupported_sym_name == "");
            mod_sym = create_module(loc, "omp_lib");
            LCOMPILERS_ASSERT(mod_sym != nullptr && ASR::is_a<ASR::Module_t>(*mod_sym));
            unsupported_sym_name = import_all(ASR::down_cast<ASR::Module_t>(mod_sym));
            LCOMPILERS_ASSERT(unsupported_sym_name == "");
            mod_sym = create_module(loc, thread_data_module_name);
            LCOMPILERS_ASSERT(mod_sym != nullptr && ASR::is_a<ASR::Module_t>(*mod_sym));
            unsupported_sym_name = import_all(ASR::down_cast<ASR::Module_t>(mod_sym));
            LCOMPILERS_ASSERT(unsupported_sym_name == "");


            ASRUtils::ASRBuilder b(al, loc);
            ASR::symbol_t* thread_data_sym = current_scope->get_symbol("thread_data" + thread_data_module_name.substr(18));
            ASR::symbol_t* thread_data_ext_sym = ASRUtils::symbol_get_past_external(thread_data_sym);

            // create data variable: `type(c_ptr), value :: data`
            ASR::expr_t* data_expr = b.Variable(current_scope, "data", ASRUtils::TYPE(ASR::make_CPtr_t(al, loc)), ASR::intentType::Unspecified,
                    ASR::abiType::BindC, true);
            LCOMPILERS_ASSERT(data_expr != nullptr);

            // create tdata variable: `type(thread_data), pointer :: tdata`
            ASR::expr_t* tdata_expr = b.Variable(current_scope, "tdata", ASRUtils::TYPE(ASR::make_Pointer_t(al, loc, ASRUtils::TYPE(ASR::make_StructType_t(al, loc, thread_data_sym)))),
                    ASR::intentType::Local, ASR::abiType::BindC);
            LCOMPILERS_ASSERT(tdata_expr != nullptr);

            Vec<ASR::stmt_t*> body; body.reserve(al, involved_symbols.size() + 1);
            body.push_back(al, b.CPtrToPointer(data_expr, tdata_expr));

            Vec<ASR::expr_t*> args; args.reserve(al, 1);
            args.push_back(al, data_expr);

            // declare involved variables
            for (auto it: involved_symbols) {
                LCOMPILERS_ASSERT(b.Variable(current_scope, it.first, it.second, ASR::intentType::Local, ASR::abiType::BindC));
            }

            // add external symbols to struct members, we need those for `data%n = n`
            SymbolTable* thread_data_symtab = ASRUtils::symbol_symtab(thread_data_ext_sym);
            for (auto it: involved_symbols) {
                std::string sym_name = std::string(ASRUtils::symbol_name(thread_data_sym)) + "_" + it.first;
                ASR::symbol_t* sym = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(al, loc,
                    current_scope, s2c(al, sym_name), thread_data_symtab->resolve_symbol(it.first), ASRUtils::symbol_name(thread_data_sym), nullptr, 0,
                    s2c(al, it.first), ASR::accessType::Public));
                current_scope->add_symbol(sym_name, sym);

                // handle arrays
                ASR::ttype_t* sym_type = it.second;
                if (ASRUtils::is_array(sym_type)) {
                    DoConcurrentStatementVisitor v(al, current_scope);
                    v.current_expr = nullptr;
                    v.visit_expr(*array_variables_to_allocate[it.first]);
                    body.push_back(al, b.CPtrToPointer(
                        ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, loc, tdata_expr,
                        sym, ASRUtils::symbol_type(sym), nullptr)),
                        b.Var(current_scope->get_symbol(it.first)),
                        array_variables_to_allocate[it.first]
                    ));
                } else {
                    body.push_back(al, b.Assignment(
                        b.Var(current_scope->get_symbol(it.first)),
                        ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, loc, tdata_expr,
                        sym, ASRUtils::symbol_type(sym), nullptr))
                    ));
                }
            }

            // Partitioning logic
            // declare start, end, num_threads, chunk, leftovers, thread_num
            // TODO: find a better way to declare these
            ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            ASR::expr_t* start = b.Variable(current_scope, current_scope->get_unique_name("start"), int_type, ASR::intentType::Local, ASR::abiType::BindC);
            ASR::expr_t* end = b.Variable(current_scope, current_scope->get_unique_name("end"), int_type, ASR::intentType::Local, ASR::abiType::BindC);
            ASR::expr_t* num_threads = b.Variable(current_scope, current_scope->get_unique_name("num_threads"), int_type, ASR::intentType::Local, ASR::abiType::BindC);
            ASR::expr_t* chunk = b.Variable(current_scope, current_scope->get_unique_name("chunk"), int_type, ASR::intentType::Local, ASR::abiType::BindC);
            ASR::expr_t* leftovers = b.Variable(current_scope, current_scope->get_unique_name("leftovers"), int_type, ASR::intentType::Local, ASR::abiType::BindC);
            ASR::expr_t* thread_num = b.Variable(current_scope, current_scope->get_unique_name("thread_num"), int_type, ASR::intentType::Local, ASR::abiType::BindC);

            // update all expr present in DoConcurrent to use the new symbols
            DoConcurrentStatementVisitor v(al, current_scope);
            v.current_expr = nullptr;
            v.visit_DoConcurrentLoop(do_loop);

            ASR::do_loop_head_t loop_head = do_loop.m_head;
            // always this shall be IntegerBinOp_t
            ASR::expr_t* loop_length = b.Add(b.Sub(loop_head.m_end, loop_head.m_start), b.i32(1));
            // calculate chunk size
            body.push_back(al, b.Assignment(num_threads,
                            ASRUtils::EXPR(ASR::make_FunctionCall_t(al, loc, current_scope->get_symbol("omp_get_max_threads"),
                            current_scope->get_symbol("omp_get_max_threads"), nullptr, 0, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)), nullptr, nullptr))));
            body.push_back(al, b.Assignment(chunk,
                            b.Div(loop_length, num_threads)));
            Vec<ASR::expr_t*> mod_args; mod_args.reserve(al, 2);
            mod_args.push_back(al, loop_length);
            mod_args.push_back(al, num_threads);
            body.push_back(al, b.Assignment(leftovers,
                            ASRUtils::EXPR(ASRUtils::make_IntrinsicElementalFunction_t_util(al, loc,
                            2,
                            mod_args.p, 2, 0, ASRUtils::expr_type(loop_length), nullptr))));
            body.push_back(al, b.Assignment(thread_num,
                            ASRUtils::EXPR(ASR::make_FunctionCall_t(al, loc, current_scope->get_symbol("omp_get_thread_num"),
                            current_scope->get_symbol("omp_get_thread_num"), nullptr, 0, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)), nullptr, nullptr))));
            body.push_back(al, b.Assignment(start, b.Mul(chunk, thread_num)));
            body.push_back(al, b.If(b.Lt(thread_num, leftovers), {
                b.Assignment(start, b.Add(start, thread_num))
            }, {
                b.Assignment(start, b.Add(start, leftovers))
            }));
            body.push_back(al, b.Assignment(end, b.Add(start, chunk)));
            body.push_back(al, b.If(b.Lt(thread_num, leftovers), {
                b.Assignment(end, b.Add(end, b.i32(1)))
            }, {
                // do nothing
            }));

            // Partioning logic ends

            // initialize reduction variables
            for ( size_t i = 0; i < do_loop.n_reduction; i++ ) {
                ASR::reduction_expr_t red = do_loop.m_reduction[i];
                reduction_variables.push_back(ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(red.m_arg)->m_v));
                switch (red.m_op) {
                    case ASR::reduction_opType::ReduceAdd : {
                        body.push_back(al, b.Assignment(red.m_arg, b.constant_t(0.0, ASRUtils::expr_type(red.m_arg))));
                        break;
                    }
                    case ASR::reduction_opType::ReduceMul : {
                        body.push_back(al, b.Assignment(red.m_arg, b.constant_t(1.0, ASRUtils::expr_type(red.m_arg))));
                        break;
                    }
                    case ASR::reduction_opType::ReduceSub : {
                        body.push_back(al, b.Assignment(red.m_arg, b.constant_t(0.0, ASRUtils::expr_type(red.m_arg))));
                        break;
                    }
                    case ASR::reduction_opType::ReduceMAX : {
                        if (ASRUtils::is_integer(*ASRUtils::expr_type(red.m_arg))) {
                            body.push_back(al, b.Assignment(red.m_arg, b.i_t(INT_MIN, ASRUtils::expr_type(red.m_arg))));
                        } else if (ASRUtils::is_real(*ASRUtils::expr_type(red.m_arg))) {
                            body.push_back(al, b.Assignment(red.m_arg, b.f_t(std::numeric_limits<double>::min(), ASRUtils::expr_type(red.m_arg))));
                        } else {
                            // handle other types
                            LCOMPILERS_ASSERT(false);
                        }
                        break;
                    }
                    case ASR::reduction_opType::ReduceMIN : {
                        if (ASRUtils::is_integer(*ASRUtils::expr_type(red.m_arg))) {
                            body.push_back(al, b.Assignment(red.m_arg, b.i_t(INT_MAX, ASRUtils::expr_type(red.m_arg))));
                        } else if (ASRUtils::is_real(*ASRUtils::expr_type(red.m_arg))) {
                            body.push_back(al, b.Assignment(red.m_arg, b.f_t(std::numeric_limits<double>::max(), ASRUtils::expr_type(red.m_arg))));
                        } else {
                            // handle other types
                            LCOMPILERS_ASSERT(false);
                        }
                        break;
                    }
                    default: {
                        LCOMPILERS_ASSERT(false);
                    }
                }
            }

            std::vector<ASR::stmt_t*> loop_body;
            for (size_t i = 0; i < do_loop.n_body; i++) {
                loop_body.push_back(do_loop.m_body[i]);
            }
            body.push_back(al, b.DoLoop(loop_head.m_v, b.Add(start, b.i32(1)), end, loop_body, loop_head.m_increment));

            body.push_back(al, ASRUtils::STMT(ASR::make_SubroutineCall_t(al, loc, current_scope->get_symbol("gomp_barrier"), nullptr, nullptr, 0, nullptr)));

            /*
                handle reduction variables if any then:
                call gomp_atomic_start()
                => perform atomic operation
                call gomp_atomic_end()
            */
            if (do_loop.n_reduction > 0) {
                body.push_back(al, ASRUtils::STMT(ASR::make_SubroutineCall_t(al, loc,
                        current_scope->get_symbol("gomp_atomic_start"), nullptr, nullptr, 0, nullptr)));
            }
            for ( size_t i = 0; i < do_loop.n_reduction; i++ ) {
                ASR::reduction_expr_t red = do_loop.m_reduction[i];
                ASR::symbol_t* red_sym = current_scope->get_symbol(std::string(ASRUtils::symbol_name(thread_data_sym)) + "_" + std::string(ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(red.m_arg)->m_v)));
                ASR::expr_t* lhs = ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, loc, tdata_expr, red_sym, ASRUtils::symbol_type(red_sym), nullptr));

                switch (red.m_op) {
                    case ASR::reduction_opType::ReduceAdd : {
                        body.push_back(al, b.Assignment(lhs, b.Add(lhs, red.m_arg)));
                        break;
                    }
                    case ASR::reduction_opType::ReduceSub : {
                        body.push_back(al, b.Assignment(lhs, b.Sub(lhs, red.m_arg)));
                        break;
                    }
                    case ASR::reduction_opType::ReduceMul : {
                        body.push_back(al, b.Assignment(lhs, b.Mul(lhs, red.m_arg)));
                        break;
                    }
                    default : {
                        // TODO: support ReduceMAX, ReduceMIN
                        LCOMPILERS_ASSERT(false);
                    }
                }
            }
            if (do_loop.n_reduction > 0) {
                body.push_back(al, ASRUtils::STMT(ASR::make_SubroutineCall_t(al, loc,
                        current_scope->get_symbol("gomp_atomic_end"), nullptr, nullptr, 0, nullptr)));
            }

            ASR::symbol_t* function = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Function_t_util(al, loc, current_scope, s2c(al, current_scope->parent->get_unique_name("lcompilers_function")),
                nullptr, 0,
                args.p, args.n,
                body.p, body.n,
                nullptr, ASR::abiType::BindC, ASR::accessType::Public,
                ASR::deftypeType::Implementation, nullptr, false, false, false, false, false,
                nullptr, 0,
                false, false, false, nullptr));
            
            current_scope->parent->add_symbol(ASRUtils::symbol_name(function), function);
            current_scope = current_scope_copy;
            return function;
        }

        ASR::ttype_t* f_type_to_c_type(ASR::ttype_t* /*f_type*/) {
            // populate it when required
            // right now in ASR `integer(c_int) :: n` is represented same as `integer :: n`
            return nullptr;
        }

        ASR::symbol_t* create_interface_lcompilers_function(ASR::Function_t* func) {
            ASRUtils::ASRBuilder b(al, func->base.base.loc);
            SymbolTable* current_scope_copy = current_scope;
            current_scope = al.make_new<SymbolTable>(current_scope);
            ASR::expr_t* data_expr = b.Variable(current_scope, "data", ASRUtils::TYPE(ASR::make_CPtr_t(al, func->base.base.loc)), ASR::intentType::Unspecified, ASR::abiType::BindC, true);
            Vec<ASR::expr_t*> args; args.reserve(al, 1);
            args.push_back(al, data_expr);
            ASR::symbol_t* interface_function = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Function_t_util(al, func->base.base.loc,
                    current_scope, func->m_name, func->m_dependencies, func->n_dependencies,
                    args.p, args.n, nullptr, 0, nullptr, ASR::abiType::BindC, ASR::accessType::Public,
                    ASR::deftypeType::Interface, nullptr, false, false, false, false, false, nullptr, 0,
                    false, false, false, nullptr));
            current_scope->parent->add_symbol(ASRUtils::symbol_name(interface_function), interface_function);
            current_scope = current_scope_copy;
            return interface_function;
        }

        /*
            This function is invoked only if array variables are used in OMP DoConcurrent
            It does the following:
            1. As we changed declaration of array variables to pointers, we need to allocate memory for them
               and update the function body accordingly
            2. Update the function signature for array variables
            3. Search for function / subroutine calls to existing function in entire ASR
            4. Recursively call this function for all the function calls found in step 3

            TODO: right now we assume array variables will have same name in different scopes
            this won't work, need to find a way to handle this
        */
        void recursive_function_call_resolver(SymbolTable* current_scope, std::vector<std::string> &array_variables, bool first_call=false) {
            ASR::asr_t* asr_owner = current_scope->asr_owner;
            if (ASR::is_a<ASR::symbol_t>(*asr_owner)) {
                ASR::symbol_t* sym = ASR::down_cast<ASR::symbol_t>(asr_owner);
                if (ASR::is_a<ASR::Function_t>(*sym)) {
                    ASRUtils::ASRBuilder b(al, sym->base.loc);
                    ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(sym);
                    if (!first_call) {
                        Vec<ASR::stmt_t*> new_body; new_body.reserve(al, func->n_body);
                        // update declaration of array variables
                        // TODO: array_variables will not have same name in different scopes
                        // this won't work, need to find a way to handle this
                        for (size_t i = 0; i < array_variables.size(); i++) {
                            ASR::symbol_t* sym = current_scope->resolve_symbol(array_variables[i]);
                            ASR::ttype_t* sym_type = ASRUtils::symbol_type(sym);
                            if (ASR::is_a<ASR::Pointer_t>(*sym_type)) {
                                continue;
                            } else if (ASR::is_a<ASR::Array_t>(*sym_type)) {
                                ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(sym_type);
                                if (!ASRUtils::is_dimension_empty(*array_type->m_dims)) {
                                    Vec<ASR::dimension_t> dims; dims.reserve(al, array_type->n_dims);
                                    ASR::dimension_t empty_dim; empty_dim.loc = array_type->base.base.loc;
                                    empty_dim.m_start = nullptr; empty_dim.m_length = nullptr;
                                    for (size_t i = 0; i < array_type->n_dims; i++) {
                                        dims.push_back(al, empty_dim);
                                    }
                                    ASR::expr_t* array_expr = b.VariableOverwrite(current_scope, array_variables[i],
                                            ASRUtils::TYPE(ASR::make_Pointer_t(al, array_type->base.base.loc,
                                                    ASRUtils::TYPE(ASR::make_Array_t(al, array_type->base.base.loc,
                                                    array_type->m_type, dims.p, dims.n, ASR::array_physical_typeType::DescriptorArray)))),
                                                ASR::intentType::InOut);
                                    LCOMPILERS_ASSERT(array_expr != nullptr);
                                    new_body.push_back(al, b.Allocate(array_expr, array_type->m_dims, array_type->n_dims));
                                } else {
                                    // we have no information about what size to allocate
                                }
                            } else if (ASR::is_a<ASR::Allocatable_t>(*sym_type)) {
                                // TODO: handle allocatable
                                continue;
                            }
                        }
                        for (size_t i = 0; i < func->n_body; i++) {
                            new_body.push_back(al, func->m_body[i]);
                        }
                        func->m_body = new_body.p;
                        func->n_body = new_body.n;
                    }
                    // update function body
                    ArrayVisitor v(al, func->m_symtab, array_variables);
                    for (size_t i=0; i<func->n_body; i++) {
                        v.visit_stmt(*func->m_body[i]);
                    }

                    // update function signature for arrays
                    std::map<std::string, int> array_arg_mapping;
                    for (size_t i = 0; i < func->n_args; i++) {
                        std::string arg_name = ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(func->m_args[i])->m_v);
                        if (std::find(array_variables.begin(), array_variables.end(), arg_name) != array_variables.end()) {
                            array_arg_mapping[arg_name] = i;
                            func->m_args[i] = b.Var(func->m_symtab->resolve_symbol(arg_name));
                        }
                    }
                    ASR::FunctionType_t* func_type = ASR::down_cast<ASR::FunctionType_t>(func->m_function_signature);
                    for (auto it: array_arg_mapping) {
                        func_type->m_arg_types[it.second] = ASRUtils::symbol_type(func->m_symtab->resolve_symbol(it.first));
                    }

                    // search for function / subroutine calls to existing function
                    std::vector<SymbolTable*> scopes;
                    FunctionSubroutineCallVisitor fsv(func->m_name, scopes);

                    // get global scope
                    SymbolTable* global_scope = current_scope;
                    while (global_scope->parent != nullptr) {
                        global_scope = global_scope->parent;
                    }
                    fsv.visit_TranslationUnit(*ASR::down_cast2<ASR::TranslationUnit_t>(global_scope->asr_owner));

                    std::vector<SymbolTable*> unique_scopes;
                    for(auto it: scopes) {
                        if (std::find(unique_scopes.begin(), unique_scopes.end(), it) == unique_scopes.end()) {
                            unique_scopes.push_back(it);
                        }
                    }
                    for (auto it: unique_scopes ) {
                        if (it->counter != current_scope->counter) {
                            recursive_function_call_resolver(it, array_variables);
                        }
                    }
                    scopes.clear();
                } else if (ASR::is_a<ASR::Program_t>(*sym)) {
                    ASRUtils::ASRBuilder b(al, sym->base.loc);
                    ASR::Program_t* prog = ASR::down_cast<ASR::Program_t>(sym);
                    if (!first_call) {
                        Vec<ASR::stmt_t*> new_body; new_body.reserve(al, prog->n_body);
                        // update declaration of array variables
                        // TODO: array_variables will not have same name in different scopes
                        // this won't work, need to find a way to handle this
                        for (size_t i = 0; i < array_variables.size(); i++) {
                            ASR::symbol_t* sym = current_scope->resolve_symbol(array_variables[i]);
                            ASR::ttype_t* sym_type = ASRUtils::symbol_type(sym);
                            if (ASR::is_a<ASR::Pointer_t>(*sym_type)) {
                                continue;
                            } else if (ASR::is_a<ASR::Array_t>(*sym_type)) {
                                ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(sym_type);
                                if (!ASRUtils::is_dimension_empty(*array_type->m_dims)) {
                                    Vec<ASR::dimension_t> dims; dims.reserve(al, array_type->n_dims);
                                    ASR::dimension_t empty_dim; empty_dim.loc = array_type->base.base.loc;
                                    empty_dim.m_start = nullptr; empty_dim.m_length = nullptr;
                                    for (size_t i = 0; i < array_type->n_dims; i++) {
                                        dims.push_back(al, empty_dim);
                                    }
                                    ASR::expr_t* array_expr = b.VariableOverwrite(prog->m_symtab, array_variables[i],
                                            ASRUtils::TYPE(ASR::make_Pointer_t(al, array_type->base.base.loc,
                                                    ASRUtils::TYPE(ASR::make_Array_t(al, array_type->base.base.loc,
                                                    array_type->m_type, dims.p, dims.n, ASR::array_physical_typeType::DescriptorArray)))),
                                                ASR::intentType::Local);
                                    LCOMPILERS_ASSERT(array_expr != nullptr);
                                    new_body.push_back(al, b.Allocate(array_expr, array_type->m_dims, array_type->n_dims));
                                } else {
                                    // we have no information about what size to allocate
                                }
                            } else if (ASR::is_a<ASR::Allocatable_t>(*sym_type)) {
                                // TODO: handle allocatable
                                continue;
                            }
                        }
                        for (size_t i = 0; i < prog->n_body; i++) {
                            new_body.push_back(al, prog->m_body[i]);
                        }
                        prog->m_body = new_body.p;
                        prog->n_body = new_body.n;
                    }
                    // update function body
                    ArrayVisitor v(al, prog->m_symtab, array_variables);
                    for (size_t i=0; i<prog->n_body; i++) {
                        // we can create a replacer but then there will be too many replacers to handle.
                        if (ASR::is_a<ASR::SubroutineCall_t>(*prog->m_body[i])) {
                            ASR::SubroutineCall_t* sub_call = ASR::down_cast<ASR::SubroutineCall_t>(prog->m_body[i]);
                            for (size_t j = 0; j < sub_call->n_args; j++) {
                                ASR::expr_t* arg = sub_call->m_args[j].m_value;
                                if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*arg)) {
                                    ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(ASR::down_cast<ASR::ArrayPhysicalCast_t>(arg)->m_arg)->m_v;
                                    std::string sym_name = ASRUtils::symbol_name(sym);
                                    if (std::find(array_variables.begin(), array_variables.end(), sym_name) != array_variables.end()) {
                                        sub_call->m_args[j].m_value = b.Var(sym);
                                    }
                                }
                            }
                        }
                        // TODO: handle function calls
                        v.visit_stmt(*prog->m_body[i]);
                    }
                }
            } else {
                LCOMPILERS_ASSERT(false);
            }
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

            std::map<std::string, ASR::expr_t*> array_variables_to_allocate;
            std::vector<std::string> array_variables;
            // create data variable for the thread data module
            ASRUtils::ASRBuilder b(al, x.base.base.loc);
            ASR::expr_t* data_expr = b.Variable(current_scope, "data", ASRUtils::TYPE(ASR::make_StructType_t(al, x.base.base.loc, thread_data_ext_sym)), ASR::intentType::Local);
            LCOMPILERS_ASSERT(data_expr != nullptr);

            // now create a tdata (cptr)
            ASR::expr_t* tdata_expr = b.Variable(current_scope, "tdata", ASRUtils::TYPE(ASR::make_CPtr_t(al, x.base.base.loc)), ASR::intentType::Local);
            LCOMPILERS_ASSERT(tdata_expr != nullptr);

            // TODO: update symbols with correct type
            for (auto it: involved_symbols) {
                ASR::ttype_t* sym_type = it.second;
                if (ASR::is_a<ASR::Pointer_t>(*sym_type)) {
                    continue;
                } else if (ASR::is_a<ASR::Array_t>(*sym_type)) {
                    ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(sym_type);
                    if (!ASRUtils::is_dimension_empty(*array_type->m_dims)) {
                        Vec<ASR::dimension_t> dims; dims.reserve(al, array_type->n_dims);
                        ASR::dimension_t empty_dim; empty_dim.loc = array_type->base.base.loc;
                        empty_dim.m_start = nullptr; empty_dim.m_length = nullptr;
                        for (size_t i = 0; i < array_type->n_dims; i++) {
                            dims.push_back(al, empty_dim);
                        }
                        ASR::expr_t* array_expr = b.VariableOverwrite(current_scope, it.first,
                                ASRUtils::TYPE(ASR::make_Pointer_t(al, array_type->base.base.loc,
                                        ASRUtils::TYPE(ASR::make_Array_t(al, array_type->base.base.loc,
                                        array_type->m_type, dims.p, dims.n, ASR::array_physical_typeType::DescriptorArray)))),
                                    ASR::intentType::InOut);
                        LCOMPILERS_ASSERT(array_expr != nullptr);
                        bool already_allocated = true;
                        if (ASR::is_a<ASR::symbol_t>(*current_scope->asr_owner) && ASR::is_a<ASR::Function_t>(*ASR::down_cast<ASR::symbol_t>(current_scope->asr_owner))) {
                            ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(ASR::down_cast<ASR::symbol_t>(current_scope->asr_owner));
                            int arg_index = -1;
                            for (size_t i = 0; i < func->n_args; i++) {
                                if (ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(func->m_args[i])->m_v) == it.first) {
                                    arg_index = i;
                                    break;
                                }
                            }
                            if (arg_index != -1) {
                                CheckIfAlreadyAllocatedVisitor v(arg_index, func->m_name, it.first, already_allocated);
                                SymbolTable* global_scope = current_scope;
                                while (global_scope->parent != nullptr) {
                                    global_scope = global_scope->parent;
                                }
                                v.visit_TranslationUnit(*ASR::down_cast2<ASR::TranslationUnit_t>(global_scope->asr_owner));
                            }
                        }
                        if (!already_allocated) pass_result.push_back(al, b.Allocate(array_expr, array_type->m_dims, array_type->n_dims));
                        involved_symbols[it.first] = ASRUtils::expr_type(array_expr);

                        /*
                            In lcompilers_function we need to do: call c_f_pointer(tdata%a, a, [size])
                            So we'll populate array_variables_to_allocate with the size of the array
                        */
                        // TODO: handle multi-dimensional arrays
                        Vec<ASR::expr_t*> size_args; size_args.reserve(al, 1); size_args.push_back(al, array_type->m_dims[0].m_length);
                        array_variables_to_allocate[it.first] = ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(al, array_expr->base.loc,
                            size_args.p, size_args.n, ASRUtils::TYPE(ASR::make_Integer_t(al, array_expr->base.loc, 4)), ASR::arraystorageType::ColMajor));
                        array_variables.push_back(it.first);
                    } else {
                        // we have no information about what size to allocate
                    }
                } else if (ASR::is_a<ASR::Allocatable_t>(*sym_type)) {
                    // TODO: handle allocatable
                    continue;
                }
            }
            
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
                ASR::ttype_t* sym_type = it.second;
                if (ASRUtils::is_array(sym_type)) {
                    pass_result.push_back(al, b.Assignment(
                        ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, x.base.base.loc, data_expr,
                        sym, ASRUtils::symbol_type(sym), nullptr)),
                        b.PointerToCPtr(b.Var(current_scope->get_symbol(it.first)), ASRUtils::symbol_type(sym))
                    ));
                } else {
                    pass_result.push_back(al, b.Assignment(
                        ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, x.base.base.loc, data_expr,
                        sym, ASRUtils::symbol_type(sym), nullptr)),
                        b.Var(current_scope->get_symbol(it.first))
                    ));
                }
            }

            // tdata = c_loc(data)
            pass_result.push_back(al, b.Assignment(
                tdata_expr,
                ASRUtils::EXPR(ASR::make_PointerToCPtr_t(al, x.base.base.loc,
                    ASRUtils::EXPR(ASR::make_GetPointer_t(al, x.base.base.loc, data_expr, ASRUtils::TYPE(ASR::make_Pointer_t(al, x.base.base.loc, ASRUtils::expr_type(data_expr))), nullptr)),
                    ASRUtils::expr_type(tdata_expr), nullptr))
            ));

            ASR::symbol_t* lcompilers_function = create_lcompilers_function(x.base.base.loc, x, involved_symbols, array_variables_to_allocate, thread_data_module.first);
            LCOMPILERS_ASSERT(lcompilers_function != nullptr);
            ASR::Function_t* lcompilers_func = ASR::down_cast<ASR::Function_t>(lcompilers_function);
            ASR::symbol_t* lcompilers_interface_function = create_interface_lcompilers_function(lcompilers_func);
            ASR::Function_t* lcompilers_interface_func = ASR::down_cast<ASR::Function_t>(lcompilers_interface_function);

            // create interface for the lcompilers function

            // create: c_funloc(lcompilers_function)
            ASR::expr_t* c_funloc = ASRUtils::EXPR(ASR::make_PointerToCPtr_t(al, x.base.base.loc,
                                    ASRUtils::EXPR(ASR::make_GetPointer_t(al, x.base.base.loc,
                                    b.Var(lcompilers_interface_function), ASRUtils::TYPE(ASR::make_Pointer_t(al, x.base.base.loc, lcompilers_interface_func->m_function_signature)), nullptr)),
                                    ASRUtils::TYPE(ASR::make_CPtr_t(al, x.base.base.loc)), nullptr));

            Vec<ASR::call_arg_t> call_args; call_args.reserve(al, 4);
            ASR::call_arg_t arg1; arg1.loc = x.base.base.loc; arg1.m_value = c_funloc;
            ASR::call_arg_t arg2; arg2.loc = x.base.base.loc; arg2.m_value = tdata_expr;
            ASR::call_arg_t arg3; arg3.loc = x.base.base.loc; arg3.m_value = b.i32(0);
            ASR::call_arg_t arg4; arg4.loc = x.base.base.loc; arg4.m_value = b.i32(0);

            call_args.push_back(al, arg1); call_args.push_back(al, arg2);
            call_args.push_back(al, arg3); call_args.push_back(al, arg4);

            ASR::symbol_t* mod_sym = create_module(x.base.base.loc, "omp_lib");
            LCOMPILERS_ASSERT(mod_sym != nullptr && ASR::is_a<ASR::Module_t>(*mod_sym));
            std::string unsupported_sym_name = import_all(ASR::down_cast<ASR::Module_t>(mod_sym));
            LCOMPILERS_ASSERT(unsupported_sym_name == "");

            pass_result.push_back(al, ASRUtils::STMT(ASR::make_SubroutineCall_t(al, x.base.base.loc, current_scope->get_symbol("gomp_parallel"), nullptr,
                                call_args.p, call_args.n, nullptr)));

            // update all reduction variables in statements after this.
            ReductionVariableVisitor rvv(al, current_scope, ASRUtils::symbol_name(thread_data_sym), data_expr, reduction_variables);
            for (size_t i = current_stmt_index + 1; i < current_n_body; i++) {
                rvv.visit_stmt(*current_m_body[i]);
            }
            reduction_variables.clear();

            if (array_variables.size() > 0) {
                // std::vector<std::string> function_names; function_names.push_back(ASRUtils::symbol_name(ASR::down_cast<ASR::symbol_t>(current_scope->asr_owner)));
                recursive_function_call_resolver(current_scope, array_variables, true);
            }

            remove_original_statement = true;
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

        void visit_Program(const ASR::Program_t &x) {
            ASR::Program_t& xx = const_cast<ASR::Program_t&>(x);
            SymbolTable* current_scope_copy = current_scope;
            current_scope = xx.m_symtab;

            for (auto &a : xx.m_symtab->get_scope()) {
                this->visit_symbol(*a.second);
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

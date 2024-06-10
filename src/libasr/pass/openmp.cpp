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


class ReplaceReductionVariable: public ASR::BaseExprReplacer<ReplaceReductionVariable> {
    private:
        Allocator& al;
    public:
        ASR::expr_t* data_expr;
        SymbolTable* current_scope;
        std::vector<std::string> reduction_variables;

        ReplaceReductionVariable(Allocator& al_) :
            al(al_) {}

        void replace_Var(ASR::Var_t* x) {
            if (std::find(reduction_variables.begin(), reduction_variables.end(), ASRUtils::symbol_name(x->m_v)) != reduction_variables.end()) {
                ASR::symbol_t* sym = current_scope->get_symbol("thread_data_" + std::string(ASRUtils::symbol_name(x->m_v)));
                LCOMPILERS_ASSERT(sym != nullptr);
                *current_expr = ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, x->base.base.loc, data_expr, sym, ASRUtils::symbol_type(sym), nullptr));
            }
        }
};

class ReductionVariableVisitor: public ASR::CallReplacerOnExpressionsVisitor<ReductionVariableVisitor> {
    private:
        ASR::expr_t* data_expr;
        SymbolTable* current_scope;
        ReplaceReductionVariable replacer;
        std::vector<std::string> reduction_variables;

    public:
        ReductionVariableVisitor(Allocator &al_, SymbolTable* current_scope_, ASR::expr_t* data_expr_,
            std::vector<std::string> reduction_variables_) :
            data_expr(data_expr_), current_scope(current_scope_), replacer(al_) {
                reduction_variables = reduction_variables_;
            }

        void call_replacer() {
            replacer.data_expr = data_expr;
            replacer.current_expr = current_expr;
            replacer.current_scope = current_scope;
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

        for (size_t i=0; i<x.n_args; i++) {
            visit_call_arg(x.m_args[i]);
        }
        visit_ttype(*x.m_type);
        if (x.m_value) {
            ASR::expr_t** current_expr_copy_123 = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_value));
            call_replacer();
            current_expr = current_expr_copy_123;
            if( x.m_value )
            visit_expr(*x.m_value);
        }
        if (x.m_dt) {
            ASR::expr_t** current_expr_copy_124 = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_dt));
            call_replacer();
            current_expr = current_expr_copy_124;
            if( x.m_dt )
            visit_expr(*x.m_dt);
        }
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
                                                current_scope, s2c(al, current_scope->get_unique_name("thread_data_module")),
                                                module_dependencies.p, module_dependencies.n, false, false));
            current_scope->parent->add_symbol("thread_data_module", thread_data_module);
            current_scope = current_scope_copy;
            return {ASRUtils::symbol_name(thread_data_module), thread_data_struct};
        }

        ASR::symbol_t* create_lcompilers_function(const Location &loc, const ASR::DoConcurrentLoop_t &do_loop,
                    std::map<std::string, ASR::ttype_t*> &involved_symbols, std::string thread_data_module_name) {
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
            ASR::symbol_t* thread_data_sym = current_scope->get_symbol("thread_data");
            ASR::symbol_t* thread_data_ext_sym = ASRUtils::symbol_get_past_external(thread_data_sym);

            // create data variable: `type(c_ptr), value :: data`
            ASR::expr_t* data_expr = b.Variable(current_scope, "data", ASRUtils::TYPE(ASR::make_CPtr_t(al, loc)), ASR::intentType::Unspecified,
                    ASR::abiType::BindC, true);
            LCOMPILERS_ASSERT(data_expr != nullptr);

            // create tdata variable: `type(thread_data), pointer :: tdata`
            ASR::expr_t* tdata_expr = b.Variable(current_scope, "tdata", ASRUtils::TYPE(ASR::make_Pointer_t(al, loc, ASRUtils::TYPE(ASR::make_Struct_t(al, loc, thread_data_sym)))),
                    ASR::intentType::Local, ASR::abiType::BindC);
            LCOMPILERS_ASSERT(tdata_expr != nullptr);

            Vec<ASR::stmt_t*> body; body.reserve(al, involved_symbols.size() + 1);
            body.push_back(al, b.CPtrToPointer(data_expr, tdata_expr));

            Vec<ASR::expr_t*> args; args.reserve(al, 1);
            args.push_back(al, data_expr);

            // declare involved variables
            for (auto it: involved_symbols) {
                // handle arrays
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
                body.push_back(al, b.Assignment(
                    b.Var(current_scope->get_symbol(it.first)),
                    ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, loc, tdata_expr,
                    sym, ASRUtils::symbol_type(sym), nullptr))
                ));
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
                ASR::symbol_t* red_sym = current_scope->get_symbol("thread_data_" + std::string(ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(red.m_arg)->m_v)));
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
                    ASRUtils::EXPR(ASR::make_GetPointer_t(al, x.base.base.loc, data_expr, ASRUtils::TYPE(ASR::make_Pointer_t(al, x.base.base.loc, ASRUtils::expr_type(data_expr))), nullptr)),
                    ASRUtils::expr_type(tdata_expr), nullptr))
            ));

            ASR::symbol_t* lcompilers_function = create_lcompilers_function(x.base.base.loc, x, involved_symbols, thread_data_module.first);
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
            ReductionVariableVisitor rvv(al, current_scope, data_expr, reduction_variables);
            for (size_t i = current_stmt_index + 1; i < current_n_body; i++) {
                rvv.visit_stmt(*current_m_body[i]);
            }
            reduction_variables.clear();

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

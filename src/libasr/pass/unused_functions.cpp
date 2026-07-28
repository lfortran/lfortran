#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/unused_functions.h>

#include <cstring>


namespace LCompilers {

// Platform dependent fast unique hash:
uint64_t static get_hash(ASR::asr_t *node)
{
    return (uint64_t)node;
}

class CollectUnusedFunctionsVisitor :
    public ASR::BaseWalkVisitor<CollectUnusedFunctionsVisitor>
{
public:
    std::map<uint64_t, std::string> fn_declarations;
    std::map<uint64_t, std::string> fn_used;

    void visit_Function(const ASR::Function_t &x) {
        uint64_t h = get_hash((ASR::asr_t*)&x);
        ASR::FunctionType_t* ftype = ASRUtils::get_FunctionType(x);
        // BindC/BindPython functions are normally kept even when unused: a
        // definition with a body may be exported to (and called from) C.
        // A zero-argument BindC `Interface` function with no body is
        // different: it is the placeholder `create_external_function`
        // records for an implicit-interface external declaration (e.g.
        // `integer, external :: nf_create` in a module). It exists only so
        // the name resolves during semantic analysis; calls never go
        // through it (each call site synthesizes its own interface). If it
        // is unreferenced it must not survive to codegen: its zero-argument
        // declaration would be emitted first and, deduplicated by link
        // name, poison a genuine call-site signature of the same external
        // elsewhere in the unit.
        bool is_external_placeholder =
            ftype->m_abi == ASR::abiType::BindC
            && ftype->m_deftype == ASR::deftypeType::Interface
            && x.n_args == 0 && x.n_body == 0;
        if ((ftype->m_abi != ASR::abiType::BindC
          && ftype->m_abi != ASR::abiType::BindPython)
          || is_external_placeholder) {
            fn_declarations[h] = x.m_name;
        }

        for( size_t i = 0; i < x.n_args; i++ ) {
            ASR::Var_t* arg_var = ASR::down_cast<ASR::Var_t>(x.m_args[i]);
            // Consider a function as argument as used
            if( ASR::is_a<ASR::Function_t>(*arg_var->m_v) ) {
                uint64_t h = get_hash((ASR::asr_t*)arg_var->m_v);
                fn_used[h] = ASR::down_cast<ASR::Function_t>(arg_var->m_v)->m_name;
            } else if( ASR::is_a<ASR::Variable_t>(*arg_var->m_v) ){
                ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(arg_var->m_v);
                if(v->m_type_declaration){
                    ASR::symbol_t* func = v->m_type_declaration;
                    if(ASR::is_a<ASR::ExternalSymbol_t>(*func)){
                        uint64_t h = get_hash((ASR::asr_t*)func);
                        fn_used[h] = ASR::down_cast<ASR::ExternalSymbol_t>(func)->m_name;
                        func = ASR::down_cast<ASR::ExternalSymbol_t>(func)->m_external;
                    }
                    if(ASR::is_a<ASR::Function_t>(*func)){
                        uint64_t h = get_hash((ASR::asr_t*)func);
                        fn_used[h] = ASR::down_cast<ASR::Function_t>(func)->m_name;
                    }
                }
            }
        }

        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
        for (size_t i=0; i<x.n_body; i++) {
            visit_stmt(*x.m_body[i]);
        }
        visit_ttype(*x.m_function_signature);
    }

    void visit_GenericProcedure(const ASR::GenericProcedure_t &x) {
        uint64_t h = get_hash((ASR::asr_t*)&x);
        fn_declarations[h] = x.m_name;
    }

    void visit_ExternalSymbol(const ASR::ExternalSymbol_t &x) {
        if (ASR::is_a<ASR::Function_t>(*x.m_external)) {
            uint64_t h = get_hash((ASR::asr_t*)&x);
            fn_declarations[h] = x.m_name;
            h = get_hash((ASR::asr_t*)x.m_external);
            fn_used[h] = x.m_name;
        }
        if (ASR::is_a<ASR::GenericProcedure_t>(*x.m_external)) {
            uint64_t h = get_hash((ASR::asr_t*)&x);
            fn_declarations[h] = x.m_name;
            h = get_hash((ASR::asr_t*)x.m_external);
            fn_used[h] = x.m_name;
        }
    }

    void visit_Variable(const ASR::Variable_t &x) {
        // A procedure pointer or dummy procedure references its interface
        // function through `m_type_declaration`; count that as a use so the
        // interface is not pruned (a symtab walk alone never reaches it).
        // Mirrors the handling of function arguments in visit_Function.
        if (x.m_type_declaration) {
            ASR::symbol_t* func = x.m_type_declaration;
            if (ASR::is_a<ASR::ExternalSymbol_t>(*func)) {
                uint64_t h = get_hash((ASR::asr_t*)func);
                fn_used[h] = ASR::down_cast<ASR::ExternalSymbol_t>(func)->m_name;
                func = ASR::down_cast<ASR::ExternalSymbol_t>(func)->m_external;
            }
            if (func && ASR::is_a<ASR::Function_t>(*func)) {
                uint64_t h = get_hash((ASR::asr_t*)func);
                fn_used[h] = ASR::down_cast<ASR::Function_t>(func)->m_name;
            }
        }
        ASR::BaseWalkVisitor<CollectUnusedFunctionsVisitor>::visit_Variable(x);
    }


    template <typename T>
    void visit_FuncSubCall(const T& x) {
        {
            const ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_name);
            if (ASR::is_a<ASR::Function_t>(*s)) {
                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
                std::string name = f->m_name;
                uint64_t h = get_hash((ASR::asr_t*)f);
                fn_used[h] = name;
                h = get_hash((ASR::asr_t*)x.m_name);
                fn_used[h] = name;
            }
            if (ASR::is_a<ASR::GenericProcedure_t>(*s)) {
                ASR::GenericProcedure_t *g = ASR::down_cast<ASR::GenericProcedure_t>(s);
                std::string name = g->m_name;
                uint64_t h = get_hash((ASR::asr_t*)g);
                fn_used[h] = name;
                h = get_hash((ASR::asr_t*)x.m_name);
                fn_used[h] = name;
            }
        }
        if (x.m_original_name) {
            const ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_original_name);
            if (ASR::is_a<ASR::Function_t>(*s)) {
                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
                std::string name = f->m_name;
                uint64_t h = get_hash((ASR::asr_t*)f);
                fn_used[h] = name;
                h = get_hash((ASR::asr_t*)x.m_original_name);
                fn_used[h] = name;
            }
            if (ASR::is_a<ASR::GenericProcedure_t>(*s)) {
                ASR::GenericProcedure_t *g = ASR::down_cast<ASR::GenericProcedure_t>(s);
                std::string name = g->m_name;
                uint64_t h = get_hash((ASR::asr_t*)g);
                fn_used[h] = name;
                h = get_hash((ASR::asr_t*)x.m_original_name);
                fn_used[h] = name;
            }
        }
        for (size_t i=0; i<x.n_args; i++) {
            if( x.m_args[i].m_value ) {
                visit_expr(*(x.m_args[i].m_value));
            }
            if (x.m_args[i].m_value && ASR::is_a<ASR::Var_t>(*x.m_args[i].m_value)) {
                ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(x.m_args[i].m_value);
                if (ASR::is_a<ASR::ExternalSymbol_t>(*var->m_v)) {
                    ASR::ExternalSymbol_t* extsym = ASR::down_cast<ASR::ExternalSymbol_t>(var->m_v);
                    uint64_t h = get_hash((ASR::asr_t*)extsym);
                    fn_used[h] = extsym->m_name;
                }
            }
        }
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        visit_FuncSubCall(x);
        visit_ttype(*x.m_type);
        if (x.m_value)
            visit_expr(*x.m_value);
        if (x.m_dt)
            visit_expr(*x.m_dt);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        visit_FuncSubCall(x);
        if (x.m_dt)
            visit_expr(*x.m_dt);
    }

    void visit_Var(const ASR::Var_t &x) {
        const ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_v);
        if (ASR::is_a<ASR::Function_t>(*s)) {
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
            std::string name = f->m_name;
            uint64_t h = get_hash((ASR::asr_t*)f);
            fn_used[h] = name;
            h = get_hash((ASR::asr_t*)x.m_v);
            fn_used[h] = name;
        }
        if (ASR::is_a<ASR::GenericProcedure_t>(*s)) {
            ASR::GenericProcedure_t *g = ASR::down_cast<ASR::GenericProcedure_t>(s);
            std::string name = g->m_name;
            uint64_t h = get_hash((ASR::asr_t*)g);
            fn_used[h] = name;
            h = get_hash((ASR::asr_t*)x.m_v);
            fn_used[h] = name;
        }
    }

    // A procedure declaration such as `procedure(iface) :: p` names its
    // interface through the variable's type declaration, and nothing calls
    // that interface. Removing it would leave the variable pointing at a
    // symbol that is no longer in its scope. The dummy arguments of a
    // function are covered above; this covers every other variable, such as
    // the ones the nested-variable pass moves into a synthetic module.
    void visit_Variable(const ASR::Variable_t &x) {
        ASR::BaseWalkVisitor<CollectUnusedFunctionsVisitor>::visit_Variable(x);
        if (x.m_type_declaration == nullptr) return;
        ASR::symbol_t *decl = x.m_type_declaration;
        if (ASR::is_a<ASR::ExternalSymbol_t>(*decl)) {
            fn_used[get_hash((ASR::asr_t*)decl)] =
                ASR::down_cast<ASR::ExternalSymbol_t>(decl)->m_name;
            decl = ASR::down_cast<ASR::ExternalSymbol_t>(decl)->m_external;
        }
        if (decl != nullptr && ASR::is_a<ASR::Function_t>(*decl)) {
            fn_used[get_hash((ASR::asr_t*)decl)] =
                ASR::down_cast<ASR::Function_t>(decl)->m_name;
        }
    }

    void visit_Struct(const ASR::Struct_t &x) {
        // Mark final procedures (stored in member_functions) as used
        for (size_t i = 0; i < x.n_member_functions; i++) {
            ASR::symbol_t* sym = x.m_symtab->parent->get_symbol(x.m_member_functions[i]);
            if (sym) {
                ASR::symbol_t* resolved = ASRUtils::symbol_get_past_external(sym);
                if (ASR::is_a<ASR::Function_t>(*resolved)) {
                    uint64_t h = get_hash((ASR::asr_t*)resolved);
                    fn_used[h] = x.m_member_functions[i];
                    h = get_hash((ASR::asr_t*)sym);
                    fn_used[h] = x.m_member_functions[i];
                }
            }
        }
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
    }

    void visit_StructMethodDeclaration(const ASR::StructMethodDeclaration_t &x) {
        const ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_proc);
        if (ASR::is_a<ASR::Function_t>(*s)) {
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(s);
            std::string name = f->m_name;
            uint64_t h = get_hash((ASR::asr_t*)f);
            fn_used[h] = name;
            h = get_hash((ASR::asr_t*)x.m_proc);
            fn_used[h] = name;
        }
        if (ASR::is_a<ASR::GenericProcedure_t>(*s)) {
            ASR::GenericProcedure_t *g = ASR::down_cast<ASR::GenericProcedure_t>(s);
            std::string name = g->m_name;
            uint64_t h = get_hash((ASR::asr_t*)g);
            fn_used[h] = name;
            h = get_hash((ASR::asr_t*)x.m_proc);
            fn_used[h] = name;
        }
    }

};

// Returns a list of unused functions by their hash.
// The corresponding std::string is not needed, but for now we included
// it for easier debugging.
std::map<uint64_t, std::string> collect_unused_functions(ASR::TranslationUnit_t &unit) {
    CollectUnusedFunctionsVisitor v;
    v.visit_TranslationUnit(unit);
    std::map<uint64_t, std::string> fn_unused;
    for (auto &a : v.fn_declarations) {
        uint64_t h = a.first;
        if (v.fn_used.find(h) == v.fn_used.end()) {
            fn_unused[h] = a.second;
        }
    }
    return fn_unused;
}

class ProgramVisitor :
    public ASR::BaseWalkVisitor<ProgramVisitor> {
public:
    bool program_present=false;

    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        for (auto &a : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Program_t>(*a.second)) {
                this->visit_symbol(*a.second);
                break;
            }
        }
    }

    void visit_Program(const ASR::Program_t &/*x*/) {
        program_present = true;
    }
};

// Is the main program present in ASR? (true/false)
bool is_program_present(ASR::TranslationUnit_t &unit) {
    ProgramVisitor v;
    v.visit_TranslationUnit(unit);
    return v.program_present;
}

class UnusedFunctionsVisitor : public ASR::BaseWalkVisitor<UnusedFunctionsVisitor>
{
private:
    Allocator &al;
public:
    std::map<uint64_t, std::string> fn_unused;

    UnusedFunctionsVisitor(Allocator &al) : al{al} { }

    void remove_unused_fn(SymbolTable* symtab) {
        std::vector<std::string> to_be_erased;
        for (auto it = symtab->get_scope().begin(); it != symtab->get_scope().end(); ++it) {
            uint64_t h = get_hash((ASR::asr_t*)it->second);
            if ((symtab->parent || (!symtab->parent && startswith(it->first, "_lcompilers_")))
                    && fn_unused.find(h) != fn_unused.end()) {
                to_be_erased.push_back(it->first);
            } else {
                this->visit_symbol(*it->second);
            }
        }

        for (std::string& sym_name: to_be_erased) {
            symtab->erase_symbol(sym_name);
        }
    }

    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        remove_unused_fn(x.m_symtab);
    }
    void visit_Program(const ASR::Program_t &x) {
        remove_unused_fn(x.m_symtab);
    }
    void visit_Module(const ASR::Module_t &x) {
        if (!x.m_parent_module) {
            remove_unused_fn(x.m_symtab);
        }
    }
    void visit_Function(const ASR::Function_t &x) {
        remove_unused_fn(x.m_symtab);
    }
    void visit_GenericProcedure(const ASR::GenericProcedure_t &x) {
        Vec<ASR::symbol_t*> v;
        v.reserve(al, x.n_procs);
        for (size_t i=0; i<x.n_procs; i++) {
            const ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_procs[i]);
            uint64_t h = get_hash((ASR::asr_t*)s);
            if (fn_unused.find(h) != fn_unused.end()) {
                //std::cout << "GP: erase proc #" << i << " / " << x.n_procs << std::endl;
            } else {
                v.push_back(al, x.m_procs[i]);
            }
        }
        if (v.size() < x.n_procs) {
            // FIXME: this is a hack, we need to pass in a non-const `x`,
            // which requires to generate a TransformVisitor.
            ASR::GenericProcedure_t &xx = const_cast<ASR::GenericProcedure_t&>(x);
            xx.m_procs = v.p;
            xx.n_procs = v.n;
        }
    }

};

void pass_unused_functions(Allocator &al, ASR::TranslationUnit_t &unit,
                           const LCompilers::PassOptions& pass_options) {
    bool always_run = pass_options.always_run;
    if (is_program_present(unit) || always_run) {
        for (int i=0; i < 4; i++)
        {
            std::map<uint64_t, std::string> fn_unused;
            fn_unused = collect_unused_functions(unit);
    /*
            std::cout << "Unused functions:" << std::endl;
            for (auto &a : fn_unused) {
                std::cout << a.second << " ";
            }
            std::cout << std::endl;
    */
            UnusedFunctionsVisitor v(al);
            v.fn_unused = fn_unused;
            v.visit_TranslationUnit(unit);
        }
    }
}


} // namespace LCompilers

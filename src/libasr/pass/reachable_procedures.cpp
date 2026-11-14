#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/reachable_procedures.h>

#include <cstring>
#include <unordered_set>
#include <vector>


namespace LCompilers {

namespace {

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
    // Pointer identity of every procedure and external symbol that reachable
    // code names.
    std::unordered_set<uint64_t> fn_used;
    SymbolTable *tu_symtab = nullptr;
    std::set<uint64_t> walked;
    std::vector<const ASR::Function_t*> work;

    // A procedure the program (or this file's own code) actually reaches is
    // queued. Its body is walked, so only that call graph is kept. Procedures
    // of a module loaded from a .mod file that nothing reachable calls are
    // left out: lowering every one of them is what makes a short program that
    // uses a large module take minutes.
    void mark_function(const ASR::Function_t *f) {
        if (f == nullptr) return;
        uint64_t h = get_hash((ASR::asr_t*)f);
        if (fn_used.find(h) != fn_used.end()) return;
        fn_used.insert(h);
        work.push_back(f);
        link_interface_and_implementation(f);
    }

    void drain() {
        for (size_t i = 0; i < work.size(); i++) {
            visit_Function(*work[i]);
        }
    }

    void mark_symbol_used(ASR::symbol_t *sym) {
        if (sym == nullptr) return;
        if (ASR::is_a<ASR::ExternalSymbol_t>(*sym)) {
            ASR::ExternalSymbol_t *ext = ASR::down_cast<ASR::ExternalSymbol_t>(sym);
            fn_used.insert(get_hash((ASR::asr_t*)sym));
            sym = ext->m_external;
        }
        if (sym == nullptr) return;
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            mark_function(ASR::down_cast<ASR::Function_t>(sym));
        } else if (ASR::is_a<ASR::GenericProcedure_t>(*sym)) {
            ASR::GenericProcedure_t *g = ASR::down_cast<ASR::GenericProcedure_t>(sym);
            fn_used.insert(get_hash((ASR::asr_t*)g));
        }
    }

    // The call names the interface in the parent module. The body is a
    // procedure of the same name in the submodule, and the other way around
    // when this file is the submodule.
    void link_interface_and_implementation(const ASR::Function_t *f) {
        if (tu_symtab == nullptr || f->m_symtab->parent == nullptr
                || f->m_symtab->parent->asr_owner == nullptr
                || !ASR::is_a<ASR::symbol_t>(*f->m_symtab->parent->asr_owner)) {
            return;
        }
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
            f->m_symtab->parent->asr_owner);
        if (!ASR::is_a<ASR::Module_t>(*owner)) return;
        ASR::Module_t *mod = ASR::down_cast<ASR::Module_t>(owner);
        if (mod->m_parent_module != nullptr && std::strlen(mod->m_parent_module) > 0) {
            ASR::symbol_t *parent_sym = tu_symtab->get_symbol(mod->m_parent_module);
            if (parent_sym != nullptr && ASR::is_a<ASR::Module_t>(*parent_sym)) {
                ASR::symbol_t *iface = ASR::down_cast<ASR::Module_t>(parent_sym)
                    ->m_symtab->get_symbol(f->m_name);
                if (iface != nullptr && ASR::is_a<ASR::Function_t>(*iface)) {
                    mark_function(ASR::down_cast<ASR::Function_t>(iface));
                }
            }
            return;
        }
        if (ASRUtils::get_FunctionType(*f)->m_deftype != ASR::deftypeType::Interface) {
            return;
        }
        for (auto &item : tu_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Module_t>(*item.second)) continue;
            ASR::Module_t *sub = ASR::down_cast<ASR::Module_t>(item.second);
            if (sub->m_parent_module == nullptr
                    || std::string(sub->m_parent_module) != mod->m_name) {
                continue;
            }
            ASR::symbol_t *impl = sub->m_symtab->get_symbol(f->m_name);
            if (impl != nullptr && ASR::is_a<ASR::Function_t>(*impl)) {
                mark_function(ASR::down_cast<ASR::Function_t>(impl));
            }
        }
    }

    void visit_Function(const ASR::Function_t &x) {
        uint64_t h = get_hash((ASR::asr_t*)&x);
        if (ASRUtils::get_FunctionType(x)->m_abi != ASR::abiType::BindC
         && ASRUtils::get_FunctionType(x)->m_abi != ASR::abiType::BindPython) {
            fn_declarations[h] = x.m_name;
        }
        if (fn_used.find(h) == fn_used.end()) return;
        if (!walked.insert(h).second) return;

        // A nested procedure is part of the procedure that contains it.
        for (auto &a : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*a.second)) {
                ASR::Function_t *nested = ASR::down_cast<ASR::Function_t>(a.second);
                uint64_t nh = get_hash((ASR::asr_t*)nested);
                if (fn_used.find(nh) == fn_used.end()) {
                    fn_used.insert(nh);
                    work.push_back(nested);
                    link_interface_and_implementation(nested);
                }
            }
        }

        for( size_t i = 0; i < x.n_args; i++ ) {
            ASR::Var_t* arg_var = ASR::down_cast<ASR::Var_t>(x.m_args[i]);
            // Consider a function as argument as used
            if( ASR::is_a<ASR::Function_t>(*arg_var->m_v)
                    || ASR::is_a<ASR::ExternalSymbol_t>(*arg_var->m_v)
                    || ASR::is_a<ASR::GenericProcedure_t>(*arg_var->m_v) ) {
                // A procedure dummy, or a use-associated procedure an
                // interface was built for.
                mark_symbol_used(arg_var->m_v);
            } else if( ASR::is_a<ASR::Variable_t>(*arg_var->m_v) ){
                ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(arg_var->m_v);
                if(v->m_type_declaration){
                    mark_symbol_used(v->m_type_declaration);
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
        if (x.m_external != nullptr) {
            // A component of a procedure-local type is named from the module,
            // with a scope path through that procedure. Removing the procedure
            // leaves the external symbol with nowhere to resolve. A symbol of
            // the procedure itself, such as the frame a nested variable pass
            // builds, is not such a use: keeping every procedure that owns
            // one would emit two module procedures of the same name.
            ASR::symbol_t *owner = ASRUtils::symbol_get_past_external(x.m_external);
            SymbolTable *fn_scope = nullptr;
            while (owner != nullptr) {
                owner = ASRUtils::get_asr_owner(owner);
                if (owner != nullptr && ASR::is_a<ASR::Function_t>(*owner)) {
                    fn_scope = ASR::down_cast<ASR::Function_t>(owner)->m_symtab;
                    if (!ASRUtils::is_scope_between(fn_scope, x.m_parent_symtab,
                            nullptr)) {
                        mark_function(ASR::down_cast<ASR::Function_t>(owner));
                    }
                }
            }
        }
    }


    template <typename T>
    void visit_FuncSubCall(const T& x) {
        mark_symbol_used(x.m_name);
        if (x.m_original_name) {
            mark_symbol_used(x.m_original_name);
        }
        for (size_t i=0; i<x.n_args; i++) {
            if( x.m_args[i].m_value ) {
                visit_expr(*(x.m_args[i].m_value));
            }
            if (x.m_args[i].m_value && ASR::is_a<ASR::Var_t>(*x.m_args[i].m_value)) {
                ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(x.m_args[i].m_value);
                if (ASR::is_a<ASR::ExternalSymbol_t>(*var->m_v)) {
                    mark_symbol_used(var->m_v);
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
        mark_symbol_used(x.m_v);
    }

    // An array constant of derived type holds its elements as expressions,
    // which can name procedures: a structure constant gives a procedure
    // pointer component its target, as in `[t(f), t(g)]`.
    void visit_ArrayConstant(const ASR::ArrayConstant_t &x) {
        ASR::BaseWalkVisitor<CollectUnusedFunctionsVisitor>::visit_ArrayConstant(x);
        ASR::ttype_t* element_type = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable(x.m_type));
        if (!ASR::is_a<ASR::StructType_t>(*element_type)) {
            return;
        }
        ASR::expr_t** elements = static_cast<ASR::expr_t**>(x.m_data);
        size_t n = (size_t) ASRUtils::get_fixed_size_of_array(x.m_type);
        for (size_t i = 0; i < n; i++) {
            visit_expr(*elements[i]);
        }
    }

    // A cast of a procedure to another interface names that interface, and
    // nothing calls it.
    void visit_FunctionPointerCast(const ASR::FunctionPointerCast_t &x) {
        ASR::BaseWalkVisitor<CollectUnusedFunctionsVisitor>::visit_FunctionPointerCast(x);
        if (x.m_to == nullptr) return;
        mark_symbol_used(x.m_to);
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
        mark_symbol_used(x.m_type_declaration);
    }

    // Marks a procedure and the external symbol naming it as used.
    void mark_procedure_used(ASR::symbol_t *sym) {
        mark_symbol_used(sym);
    }

    // Formatted output that dispatches to a defined output procedure uses
    // that procedure and its callback, although nothing calls them there.
    void visit_dtio_binding(const ASR::dtio_binding_t &x) {
        mark_procedure_used(x.m_procedure);
        mark_procedure_used(x.m_callback);
    }

    void visit_Struct(const ASR::Struct_t &x) {
        // Mark final procedures (stored in member_functions) as used
        for (size_t i = 0; i < x.n_member_functions; i++) {
            ASR::symbol_t* sym = x.m_symtab->parent->get_symbol(x.m_member_functions[i]);
            if (sym) {
                ASR::symbol_t* resolved = ASRUtils::symbol_get_past_external(sym);
                if (ASR::is_a<ASR::Function_t>(*resolved)) {
                    mark_symbol_used(sym);
                }
            }
        }
        for (auto &a : x.m_symtab->get_scope()) {
            this->visit_symbol(*a.second);
        }
    }

    void visit_StructMethodDeclaration(const ASR::StructMethodDeclaration_t &x) {
        mark_symbol_used(x.m_proc);
    }

    void record_scope(SymbolTable *st) {
        for (auto &it : st->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*it.second)) {
                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(it.second);
                ASR::abiType abi = ASRUtils::get_FunctionType(f)->m_abi;
                if (abi != ASR::abiType::BindC && abi != ASR::abiType::BindPython) {
                    fn_declarations[get_hash((ASR::asr_t*)f)] = f->m_name;
                }
                record_scope(f->m_symtab);
            } else if (ASR::is_a<ASR::GenericProcedure_t>(*it.second)) {
                fn_declarations[get_hash((ASR::asr_t*)it.second)] = it.first;
            } else if (ASR::is_a<ASR::ExternalSymbol_t>(*it.second)) {
                ASR::ExternalSymbol_t *ext = ASR::down_cast<ASR::ExternalSymbol_t>(it.second);
                if (ext->m_external != nullptr && (ASR::is_a<ASR::Function_t>(*ext->m_external)
                        || ASR::is_a<ASR::GenericProcedure_t>(*ext->m_external))) {
                    fn_declarations[get_hash((ASR::asr_t*)ext)] = ext->m_name;
                }
            } else if (ASR::is_a<ASR::Module_t>(*it.second)) {
                record_scope(ASR::down_cast<ASR::Module_t>(it.second)->m_symtab);
            } else if (ASR::is_a<ASR::Program_t>(*it.second)) {
                record_scope(ASR::down_cast<ASR::Program_t>(it.second)->m_symtab);
            } else if (ASR::is_a<ASR::Block_t>(*it.second)) {
                record_scope(ASR::down_cast<ASR::Block_t>(it.second)->m_symtab);
            } else if (ASR::is_a<ASR::Struct_t>(*it.second)) {
                record_scope(ASR::down_cast<ASR::Struct_t>(it.second)->m_symtab);
            } else if (ASR::is_a<ASR::Enum_t>(*it.second)) {
                record_scope(ASR::down_cast<ASR::Enum_t>(it.second)->m_symtab);
            } else if (ASR::is_a<ASR::BlockData_t>(*it.second)) {
                record_scope(ASR::down_cast<ASR::BlockData_t>(it.second)->m_symtab);
            }
        }
    }

    void note_variable_init(ASR::symbol_t *sym) {
        if (!ASR::is_a<ASR::Variable_t>(*sym)) return;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
        if (v->m_symbolic_value) visit_expr(*v->m_symbolic_value);
        if (v->m_value) visit_expr(*v->m_value);
        if (v->m_type_declaration) mark_symbol_used(v->m_type_declaration);
    }

    // Roots are the program, this file's own procedures, module variable
    // initializers, and type-bound procedures. Loaded-module procedures are
    // reached only through those.
    void seed(ASR::TranslationUnit_t &unit) {
        tu_symtab = unit.m_symtab;
        for (auto &it : unit.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Program_t>(*it.second)) {
                ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(it.second);
                for (size_t i = 0; i < p->n_body; i++) visit_stmt(*p->m_body[i]);
                for (auto &s : p->m_symtab->get_scope()) {
                    note_variable_init(s.second);
                    // A procedure-pointer component names its interface here,
                    // not in an executable statement.
                    if (ASR::is_a<ASR::Struct_t>(*s.second)) {
                        visit_Struct(*ASR::down_cast<ASR::Struct_t>(s.second));
                    } else if (ASR::is_a<ASR::Block_t>(*s.second)) {
                        visit_Block(*ASR::down_cast<ASR::Block_t>(s.second));
                    } else if (ASR::is_a<ASR::Function_t>(*s.second)) {
                        // Internal procedures and abstract interfaces belong
                        // to this file, the same as a module procedure does.
                        mark_function(ASR::down_cast<ASR::Function_t>(s.second));
                    }
                }
            } else if (ASR::is_a<ASR::Function_t>(*it.second)) {
                mark_function(ASR::down_cast<ASR::Function_t>(it.second));
            } else if (ASR::is_a<ASR::Template_t>(*it.second)) {
                // A template compiled in this file is kept whole, so the
                // procedures it calls in a loaded module have to be kept too.
                ASR::Template_t *tmpl = ASR::down_cast<ASR::Template_t>(it.second);
                for (auto &s : tmpl->m_symtab->get_scope()) {
                    if (ASR::is_a<ASR::Function_t>(*s.second)) {
                        mark_function(ASR::down_cast<ASR::Function_t>(s.second));
                    }
                }
            } else if (ASR::is_a<ASR::BlockData_t>(*it.second)) {
                ASR::BlockData_t *b = ASR::down_cast<ASR::BlockData_t>(it.second);
                for (auto &s : b->m_symtab->get_scope()) note_variable_init(s.second);
            } else if (ASR::is_a<ASR::Module_t>(*it.second)) {
                ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(it.second);
                for (auto &s : m->m_symtab->get_scope()) {
                    note_variable_init(s.second);
                    if (!m->m_loaded_from_mod && ASR::is_a<ASR::Function_t>(*s.second)) {
                        mark_function(ASR::down_cast<ASR::Function_t>(s.second));
                    }
                    if (ASR::is_a<ASR::Struct_t>(*s.second)) {
                        visit_Struct(*ASR::down_cast<ASR::Struct_t>(s.second));
                    }
                }
            }
        }
        for (size_t i = 0; i < unit.n_items; i++) {
            if (unit.m_items[i]->type == ASR::asrType::expr) {
                visit_expr(*ASR::down_cast<ASR::expr_t>(unit.m_items[i]));
            } else if (unit.m_items[i]->type == ASR::asrType::stmt) {
                visit_stmt(*ASR::down_cast<ASR::stmt_t>(unit.m_items[i]));
            }
        }
        // Templates are not removed. Walk the ones in every module, loaded or
        // not, so a call they make into a loaded module is not deleted.
        for (auto &it : unit.m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Module_t>(*it.second)) continue;
            ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(it.second);
            for (auto &s : m->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Template_t>(*s.second)) continue;
                ASR::Template_t *tmpl = ASR::down_cast<ASR::Template_t>(s.second);
                for (auto &f : tmpl->m_symtab->get_scope()) {
                    if (ASR::is_a<ASR::Function_t>(*f.second)) {
                        mark_function(ASR::down_cast<ASR::Function_t>(f.second));
                    }
                }
            }
        }
        drain();
    }

};

// Returns a list of unused functions by their hash.
// The corresponding std::string is not needed, but for now we included
// it for easier debugging.
std::map<uint64_t, std::string> collect_unused_functions(ASR::TranslationUnit_t &unit) {
    CollectUnusedFunctionsVisitor v;
    v.record_scope(unit.m_symtab);
    v.seed(unit);
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

    // An import of a procedure this pass removes cannot stay behind, including
    // one that lives in a contained subroutine rather than the program itself.
    void drop_dangling_imports(SymbolTable *symtab) {
        std::vector<std::string> dead;
        std::vector<SymbolTable*> nested;
        for (auto &it : symtab->get_scope()) {
            if (ASR::is_a<ASR::ExternalSymbol_t>(*it.second)) {
                ASR::ExternalSymbol_t *ext = ASR::down_cast<ASR::ExternalSymbol_t>(it.second);
                if (will_remove_procedure(ext->m_external)) dead.push_back(it.first);
            }
            if (SymbolTable *child = ASRUtils::symbol_symtab(it.second)) {
                nested.push_back(child);
            }
        }
        for (auto &name : dead) symtab->erase_symbol(name);
        for (SymbolTable *child : nested) drop_dangling_imports(child);
    }

    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        drop_dangling_imports(x.m_symtab);
        remove_unused_fn(x.m_symtab);
    }
    // True when this pass will remove the procedure from its module. A
    // procedure compiled in this file, or in an intrinsic module, stays.
    bool will_remove_procedure(ASR::symbol_t *sym) {
        if (sym == nullptr) return false;
        sym = ASRUtils::symbol_get_past_external(sym);
        if (sym == nullptr || !ASR::is_a<ASR::Function_t>(*sym)
                && !ASR::is_a<ASR::GenericProcedure_t>(*sym)) {
            return false;
        }
        if (fn_unused.find(get_hash((ASR::asr_t*)sym)) == fn_unused.end()) {
            return false;
        }
        ASR::symbol_t *owner = ASRUtils::get_asr_owner(sym);
        if (owner == nullptr || !ASR::is_a<ASR::Module_t>(*owner)) return false;
        ASR::Module_t *mod = ASR::down_cast<ASR::Module_t>(owner);
        if (!mod->m_loaded_from_mod) return false;
        if (startswith(mod->m_name, "lfortran_intrinsic")) return false;
        return true;
    }

    void visit_Program(const ASR::Program_t &/*x*/) {
    }
    void visit_Module(const ASR::Module_t &x) {
        // A module compiled in this file, and the intrinsic modules, keep
        // every procedure. Their generics and procedure pointers name
        // specifics that no call in this file has resolved yet.
        if (!x.m_loaded_from_mod) return;
        if (startswith(x.m_name, "lfortran_intrinsic")) return;
        // A submodule holds the bodies of its parent's interfaces. An unused
        // one is removed; the interface's use keeps the body that implements it.
        remove_unused_fn(x.m_symtab);
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

} // namespace

void pass_reachable_procedures(Allocator &al, ASR::TranslationUnit_t &unit,
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

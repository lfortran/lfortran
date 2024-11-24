#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/insert_deallocate.h>


namespace LCompilers {

class InsertDeallocate: public ASR::CallReplacerOnExpressionsVisitor<InsertDeallocate>
{
    private:

        Allocator& al;

        inline bool is_deallocatable(ASR::symbol_t* s){
            if( ASR::is_a<ASR::Variable_t>(*s) && 
                ASR::is_a<ASR::Allocatable_t>(*ASRUtils::symbol_type(s)) && 
                (ASR::is_a<ASR::String_t>(*ASRUtils::type_get_past_allocatable(ASRUtils::symbol_type(s))) ||
                ASRUtils::is_array(ASRUtils::symbol_type(s))) &&
                ASRUtils::symbol_intent(s) == ASRUtils::intent_local){
                return true;
            }
            return false;
        }

        // Returns vector of default-storage `Var` expressions that're deallocatable.
        Vec<ASR::expr_t*> get_allocatable_default_storage_variables(SymbolTable* symtab){ 
            Vec<ASR::expr_t*> allocatable_local_variables;
            allocatable_local_variables.reserve(al, 1);
            for(auto& itr: symtab->get_scope()) {
                if( is_deallocatable(itr.second) && 
                    ASRUtils::symbol_StorageType(itr.second) == ASR::storage_typeType::Default) {
                    allocatable_local_variables.push_back(al, ASRUtils::EXPR(
                        ASR::make_Var_t(al, itr.second->base.loc, itr.second)));
                }
            }
            return allocatable_local_variables;
        }        

        Vec<ASR::expr_t*> get_allocatable_save_storage_variables(SymbolTable* /*symtab*/){
            LCOMPILERS_ASSERT_MSG(false, "Not implemented");
        }


    public:

        InsertDeallocate(Allocator& al_) : al(al_) {}

        template <typename T>
        void visit_Symbol(const T& x) {
            Vec<ASR::expr_t*> to_be_deallocated;
            to_be_deallocated.reserve(al, 1);
            for( auto& itr: x.m_symtab->get_scope() ) {
                if( is_deallocatable(itr.second) && 
                    ASRUtils::symbol_StorageType(itr.second) == ASR::storage_typeType::Default) {
                    to_be_deallocated.push_back(al, ASRUtils::EXPR(
                        ASR::make_Var_t(al, x.base.base.loc, itr.second)));
                }
            }
            if( to_be_deallocated.size() > 0 ) {
                T& xx = const_cast<T&>(x);
                Vec<ASR::stmt_t*> body;
                body.from_pointer_n_copy(al, xx.m_body, xx.n_body);
                body.push_back(al, ASRUtils::STMT(ASR::make_ImplicitDeallocate_t(
                    al, x.base.base.loc, to_be_deallocated.p, to_be_deallocated.size())));
                xx.m_body = body.p;
                xx.n_body = body.size();
            }
        }

        void visit_Function(const ASR::Function_t& x) {
            visit_Symbol(x);
            ASR::CallReplacerOnExpressionsVisitor<InsertDeallocate>::visit_Function(x);
        }

        void visit_Program(const ASR::Program_t& x) {
            Vec<ASR::expr_t*> default_storage_variables_to_deallocate = get_allocatable_default_storage_variables(x.m_symtab);
            if(default_storage_variables_to_deallocate.empty()){
                ASR::CallReplacerOnExpressionsVisitor<InsertDeallocate>::visit_Program(x);
            } else {
                ASR::stmt_t* implicit_deallocation_node = ASRUtils::STMT(ASR::make_ImplicitDeallocate_t(
                    al, x.base.base.loc, default_storage_variables_to_deallocate.p,
                    default_storage_variables_to_deallocate.size()));
                for (auto &a : x.m_symtab->get_scope()) {
                    ASR::CallReplacerOnExpressionsVisitor<InsertDeallocate>::visit_symbol(*a.second);
                }
                Vec<ASR::stmt_t*> new_body; // Body after inserting finalization nodes.
                new_body.reserve(al, 1);
                bool return_encounterd = false; 
                for(size_t stmt_ind = 0; (stmt_ind < x.n_body); stmt_ind++){
                    if(ASR::is_a<ASR::Return_t>(*x.m_body[stmt_ind]) && !return_encounterd ){
                        new_body.push_back(al, implicit_deallocation_node);
                        return_encounterd = true; // No need to insert finaliztion node once we encounter a return stmt in program.
                    }
                    visit_stmt(*(x.m_body[stmt_ind]));
                    new_body.push_back(al, x.m_body[stmt_ind]);
                }
                if(!return_encounterd){
                    new_body.push_back(al, implicit_deallocation_node); // Add dealloction at the ending of program.
                }
                const_cast<ASR::Program_t&>(x).m_body = new_body.p;
                const_cast<ASR::Program_t&>(x).n_body = new_body.size();
            }
        }

};

void pass_insert_deallocate(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &/*pass_options*/) {
    InsertDeallocate v(al);
    v.visit_TranslationUnit(unit);
}


} // namespace LCompilers

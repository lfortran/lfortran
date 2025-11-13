#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/insert_deallocate.h>
#include<stack>


namespace LCompilers {

// hard code avoided types (handled by llvm backend) -- We're transitioing the deallocation process into the backend. 
auto const avoided_type = [](ASR::ttype_t* const t) {
                        return (ASRUtils::extract_type(t)->type == ASR::String)
                            || (ASRUtils::extract_type(t)->type == ASR::Integer)
                            || (ASRUtils::extract_type(t)->type == ASR::Complex)
                            || (ASRUtils::extract_type(t)->type == ASR::Real)
                            || (ASRUtils::extract_type(t)->type == ASR::UnsignedInteger)
                            || (ASRUtils::type_get_past_allocatable_pointer(t)->type == ASR::Array); };

class InsertDeallocate: public ASR::CallReplacerOnExpressionsVisitor<InsertDeallocate>
{
    private:

        Allocator& al;
        std::stack<ASR::stmt_t*> implicitDeallocate_stmt_stack; // A stack to hold implicit_deallocate statement node due to nested visiting.

        void push_implicitDeallocate_into_stack(SymbolTable* symtab, Location &loc){
            Vec<ASR::expr_t*> default_storage_variables_to_deallocate = get_allocatable_default_storage_variables(symtab);
            if(default_storage_variables_to_deallocate.empty()){
                implicitDeallocate_stmt_stack.push(nullptr);
            } else {
                ASR::stmt_t* implicit_deallocation_node = ASRUtils::STMT(ASR::make_ImplicitDeallocate_t(
                    al, loc, default_storage_variables_to_deallocate.p, default_storage_variables_to_deallocate.size()));
                implicitDeallocate_stmt_stack.push(implicit_deallocation_node);
            }
        }

        inline bool is_deallocatable(ASR::symbol_t* s){
            if( ASR::is_a<ASR::Variable_t>(*s) && 
                ASR::is_a<ASR::Allocatable_t>(*ASRUtils::symbol_type(s)) && 
                (ASR::is_a<ASR::String_t>(*ASRUtils::type_get_past_allocatable(ASRUtils::symbol_type(s))) ||
                ASRUtils::is_array(ASRUtils::symbol_type(s)) ||
                ASRUtils::is_struct(*ASRUtils::symbol_type(s))) &&
                ASRUtils::symbol_intent(s) == ASRUtils::intent_local){
                if(avoided_type(ASRUtils::symbol_type(s))) return false;
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
            return Vec<ASR::expr_t*>();
        }

        // Insert `ImplicitDeallocate` before construct terminating statements.
        void visit_body_and_insert_ImplicitDeallocate(ASR::stmt_t** &m_body, size_t &n_body,
            const std::vector<ASR::stmtType> &construct_terminating_stmts = {ASR::stmtType::Return}){ 
            if(implicitDeallocate_stmt_stack.top() == nullptr){ // No variables to deallocate.
                return;
            }

            Vec<ASR::stmt_t*> new_body; // Final body after inserting finalization nodes.
            new_body.reserve(al, 1);
            bool return_or_exit_encounterd = false; // No need to insert finaliztion node once we encounter a `return` or `exit` stmt in signle body (Unreachable code).
            for(size_t i = 0; i < n_body; i++){
                for(ASR::stmtType statement_type : construct_terminating_stmts){
                    if( !return_or_exit_encounterd && 
                        (statement_type == m_body[i]->type)){
                        new_body.push_back(al, implicitDeallocate_stmt_stack.top());
                    }
                }
                if( ASR::is_a<ASR::Exit_t>(*m_body[i]) || 
                    ASR::is_a<ASR::Return_t>(*m_body[i])){
                    return_or_exit_encounterd = true; // Next statements are 'Unreachable Code'.
                }
                if(!return_or_exit_encounterd){
                    visit_stmt(*(m_body[i]));
                }
                new_body.push_back(al, m_body[i]);
            }
            m_body = new_body.p;
            n_body = new_body.size();
        }

        template <typename T>
        void insert_ImplicitDeallocate_at_end(T& x){
            LCOMPILERS_ASSERT_MSG(
                x.class_type == ASR::symbolType::Program  ||
                x.class_type == ASR::symbolType::Function ||
                x.class_type == ASR::symbolType::Block, "Only use with ASR::Program_t, Function_t or Block_t");
            if(implicitDeallocate_stmt_stack.top() == nullptr){
                return;
            }
            for(size_t i = 0; i < x.n_body; i++){
                if( ASR::is_a<ASR::Return_t>(*x.m_body[i]) || 
                    ASR::is_a<ASR::Exit_t>(*x.m_body[i])){
                    return; // already handled, and no need to insert at end.
                }
            }
            Vec<ASR::stmt_t*> new_body;
            new_body.from_pointer_n_copy(al, x.m_body, x.n_body);
            new_body.push_back(al, implicitDeallocate_stmt_stack.top());
            x.m_body = new_body.p;
            x.n_body = new_body.size();
        }



    public:

        InsertDeallocate(Allocator& al_) : al(al_) {}

        void visit_Function(const ASR::Function_t& x) {
            if (ASRUtils::get_FunctionType(&x)->m_abi == ASR::abiType::ExternalUndefined) {
                return;
            }
            ASR::Function_t &xx = const_cast<ASR::Function_t&>(x);
            push_implicitDeallocate_into_stack(xx.m_symtab, xx.base.base.loc);
            for (auto &a : x.m_symtab->get_scope()) {
                visit_symbol(*a.second);
            }
            visit_body_and_insert_ImplicitDeallocate(xx.m_body,xx.n_body);
            insert_ImplicitDeallocate_at_end(xx);
            implicitDeallocate_stmt_stack.pop(); 
        }

        void visit_Program(const ASR::Program_t& x) {
            ASR::Program_t &xx = const_cast<ASR::Program_t&>(x);
            push_implicitDeallocate_into_stack(xx.m_symtab, xx.base.base.loc);

            for (auto &a : x.m_symtab->get_scope()) {
                visit_symbol(*a.second);
            }
            visit_body_and_insert_ImplicitDeallocate(xx.m_body, xx.n_body);
            insert_ImplicitDeallocate_at_end(xx);
            implicitDeallocate_stmt_stack.pop(); 
        }

        void visit_Block(const ASR::Block_t& x){
            ASR::Block_t& xx = const_cast<ASR::Block_t&>(x);
            push_implicitDeallocate_into_stack(xx.m_symtab, xx.base.base.loc);
            for (auto &a : x.m_symtab->get_scope()) {
                visit_symbol(*a.second);
            }
            visit_body_and_insert_ImplicitDeallocate(xx.m_body, xx.n_body,{ASR::stmtType::Return, ASR::stmtType::Exit});
            insert_ImplicitDeallocate_at_end(xx);
            implicitDeallocate_stmt_stack.pop(); 
        }

        void visit_If(const ASR::If_t& x){
            ASR::If_t &xx = const_cast<ASR::If_t&>(x);
            visit_body_and_insert_ImplicitDeallocate(xx.m_body, xx.n_body);
            visit_body_and_insert_ImplicitDeallocate(xx.m_orelse, xx.n_orelse);
        }
        
        void visit_WhileLoop(const ASR::WhileLoop_t &x){
            ASR::WhileLoop_t &xx = const_cast<ASR::WhileLoop_t&>(x);
            visit_body_and_insert_ImplicitDeallocate(xx.m_body, xx.n_body);
            visit_body_and_insert_ImplicitDeallocate(xx.m_orelse, xx.n_orelse);
        }

        void visit_DoLoop(const ASR::DoLoop_t &x){
            ASR::DoLoop_t &xx = const_cast<ASR::DoLoop_t&>(x);
            visit_body_and_insert_ImplicitDeallocate(xx.m_body, xx.n_body);
            visit_body_and_insert_ImplicitDeallocate(xx.m_orelse, xx.n_orelse);
        }

};

// Collect temporary variables which are inside Assignment targets
class CollectTempVarsVisitor : public ASR::BaseWalkVisitor<CollectTempVarsVisitor>
{
    Allocator &al;
    Vec<ASR::expr_t*>& res;
    public:
        CollectTempVarsVisitor(Allocator& al_, Vec<ASR::expr_t*>& res_) : al(al_), res{res_} {}

        void visit_Assignment(const ASR::Assignment_t &x) {
            ASR::Variable_t* target_variable = ASRUtils::expr_to_variable_or_null(x.m_target);
            if (target_variable &&
                ASRUtils::is_allocatable_or_pointer(target_variable->m_type) &&
                ASRUtils::symbol_StorageType((ASR::symbol_t *)target_variable) == ASR::storage_typeType::Default) {
                if (std::string(target_variable->m_name).rfind("__libasr_created") != std::string::npos) {
                    res.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, x.m_target->base.loc, (ASR::symbol_t *)target_variable)));
                }
            }
        }

        // Don't nest into other loops
        void visit_WhileLoop(const ASR::WhileLoop_t &/*x*/){
        }

        void visit_DoLoop(const ASR::DoLoop_t &/*x*/){
        }
};

// Insert implicit deallocate before Cycle, Return, or Exit in a loop
class LoopDeallocateInserter : public ASR::CallReplacerOnExpressionsVisitor<LoopDeallocateInserter>
{
    Allocator &al;
    ASR::stmt_t* node;
    public:
        LoopDeallocateInserter(Allocator& al_, ASR::stmt_t* node_) : al(al_), node(node_) {}

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            Vec<ASR::stmt_t*> new_body;
            new_body.reserve(al, 1);
            for (size_t i = 0; i < n_body; i++){
                if (ASR::is_a<ASR::Cycle_t>(*m_body[i]) ||
                    ASR::is_a<ASR::Return_t>(*m_body[i]) ||
                    ASR::is_a<ASR::Exit_t>(*m_body[i])) {
                    new_body.push_back(al, node);
                }
                new_body.push_back(al, m_body[i]);
            }
            m_body = new_body.p;
            n_body = new_body.size();
        }

        // Don't nest into other loops
        void visit_WhileLoop(const ASR::WhileLoop_t &/*x*/){
        }

        void visit_DoLoop(const ASR::DoLoop_t &/*x*/){
        }
};

// Collects temporary variables in assignment targets inside loops and deallocates them when going out of the loop body
class LoopTempVarDeallocateVisitor : public ASR::BaseWalkVisitor<LoopTempVarDeallocateVisitor>
{
    Allocator &al;
    public:
        LoopTempVarDeallocateVisitor(Allocator& al_) : al(al_) {}

        template<typename T>
        void collect_temp_vars_and_insert_deallocate_in_loop(T& xx) {
            Vec<ASR::expr_t*> v;
            v.reserve(al, 1);
            CollectTempVarsVisitor c(al, v);
            for (size_t i = 0; i < xx.n_body; i++) {
                c.visit_stmt(*xx.m_body[i]);
            }
            if (v.size() > 0) {
                ASR::stmt_t* implicit_deallocation_node = ASRUtils::STMT(ASR::make_ImplicitDeallocate_t(
                    al, xx.base.base.loc, v.p, v.size()));

                // Insert ImplicitDeallocate after Cycle, Return, or Exit
                LoopDeallocateInserter I(al, implicit_deallocation_node);
                for (size_t i = 0; i < xx.n_body; i++) {
                    I.visit_stmt(*xx.m_body[i]);
                }

                // Insert ImplicitDeallocate at the end of the loop body
                Vec<ASR::stmt_t*> new_body;
                new_body.reserve(al, 1);
                for (size_t i = 0; i < xx.n_body; i++) {
                    new_body.push_back(al, xx.m_body[i]);
                }
                new_body.push_back(al, implicit_deallocation_node);

                xx.m_body = new_body.p;
                xx.n_body = new_body.n;
            }
        }

        void visit_WhileLoop(const ASR::WhileLoop_t &x){
            ASR::WhileLoop_t &xx = const_cast<ASR::WhileLoop_t&>(x);
            collect_temp_vars_and_insert_deallocate_in_loop(xx);

            ASR::BaseWalkVisitor<LoopTempVarDeallocateVisitor>::visit_WhileLoop(x);
        }

        void visit_DoLoop(const ASR::DoLoop_t &x){
            ASR::DoLoop_t &xx = const_cast<ASR::DoLoop_t&>(x);
            collect_temp_vars_and_insert_deallocate_in_loop(xx);

            ASR::BaseWalkVisitor<LoopTempVarDeallocateVisitor>::visit_DoLoop(x);
        }
};


void pass_insert_deallocate(Allocator &al, ASR::TranslationUnit_t &unit,
                                const PassOptions &/*pass_options*/) {
    InsertDeallocate v(al);
    v.visit_TranslationUnit(unit);

    LoopTempVarDeallocateVisitor m(al);
    m.visit_TranslationUnit(unit);
}


} // namespace LCompilers

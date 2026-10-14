#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_init_expr.h>
#include <libasr/pass/global_init.h>
#include <libasr/pass/pass_utils.h>

#include <vector>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

class ReplaceInitExpr: public ASR::BaseExprReplacer<ReplaceInitExpr> {

    public:

    Allocator& al;
    std::map<SymbolTable*, Vec<ASR::stmt_t*>>& symtab2decls;

    SymbolTable* current_scope;
    ASR::expr_t* result_var;
    ASR::cast_kindType cast_kind;
    ASR::ttype_t* casted_type;
    bool perform_cast;

    ReplaceInitExpr(
        Allocator& al_,
        std::map<SymbolTable*, Vec<ASR::stmt_t*>>& symtab2decls_) :
    al(al_), symtab2decls(symtab2decls_),
    current_scope(nullptr), result_var(nullptr),
    cast_kind(ASR::cast_kindType::IntegerToInteger),
    casted_type(nullptr), perform_cast(false) {}

    void replace_ArrayConstant(ASR::ArrayConstant_t* x) {
        if( symtab2decls.find(current_scope) == symtab2decls.end() ) {
            Vec<ASR::stmt_t*> result_vec_;
            result_vec_.reserve(al, 0);
            symtab2decls[current_scope] = result_vec_;
        }
        Vec<ASR::stmt_t*>* result_vec = &symtab2decls[current_scope];
        bool remove_original_statement = false;
        if( casted_type != nullptr ) {
            casted_type = ASRUtils::type_get_past_array(casted_type);
        }
        PassUtils::ReplacerUtils::replace_ArrayConstant(x, this,
            remove_original_statement, result_vec,
            perform_cast, cast_kind, casted_type);
        *current_expr = nullptr;
    }

    void replace_ArrayConstructor(ASR::ArrayConstructor_t* x) {
        if( symtab2decls.find(current_scope) == symtab2decls.end() ) {
            Vec<ASR::stmt_t*> result_vec_;
            result_vec_.reserve(al, 0);
            symtab2decls[current_scope] = result_vec_;
        }
        Vec<ASR::stmt_t*>* result_vec = &symtab2decls[current_scope];
        if( casted_type != nullptr ) {
            casted_type = ASRUtils::type_get_past_array(casted_type);
        }
        PassUtils::ReplacerUtils::replace_ArrayConstructor_(al, x, result_var, result_vec,
            current_scope, perform_cast, cast_kind, casted_type);
        *current_expr = nullptr;
    }

    void replace_ArrayBroadcast(ASR::ArrayBroadcast_t* x) {
        if( symtab2decls.find(current_scope) == symtab2decls.end() ) {
            Vec<ASR::stmt_t*> result_vec_;
            result_vec_.reserve(al, 0);
            symtab2decls[current_scope] = result_vec_;
        }
        Vec<ASR::stmt_t*>* result_vec = &symtab2decls[current_scope];
        ASR::expr_t* broadcast = ASRUtils::EXPR((ASR::asr_t*)x);
        result_vec->push_back(al, ASRUtils::STMT(
            ASRUtils::make_Assignment_t_util(
                al, x->base.base.loc, result_var, broadcast, nullptr,
                false, false)));
        *current_expr = nullptr;
    }

    void replace_StructConstructor(ASR::StructConstructor_t* x) {
        if( symtab2decls.find(current_scope) == symtab2decls.end() ) {
            Vec<ASR::stmt_t*> result_vec_;
            result_vec_.reserve(al, 0);
            symtab2decls[current_scope] = result_vec_;
        }
        Vec<ASR::stmt_t*>* result_vec = &symtab2decls[current_scope];
        bool remove_original_statement = false;
        PassUtils::ReplacerUtils::replace_StructConstructor(
            x, this, true, remove_original_statement, result_vec,
            perform_cast, cast_kind, casted_type);
        *current_expr = nullptr;
    }

    void replace_Cast(ASR::Cast_t* x) {
        bool perform_cast_copy = perform_cast;
        ASR::cast_kindType cast_kind_copy = cast_kind;
        ASR::ttype_t* casted_type_copy = casted_type;
        perform_cast = true;
        cast_kind = x->m_kind;
        LCOMPILERS_ASSERT(x->m_type != nullptr);
        casted_type = ASRUtils::type_get_past_allocatable(
            ASRUtils::type_get_past_pointer(x->m_type));
        BaseExprReplacer<ReplaceInitExpr>::replace_Cast(x);
        perform_cast = perform_cast_copy;
        cast_kind = cast_kind_copy;
        casted_type = casted_type_copy;
        *current_expr = nullptr;
    }

    void replace_ArrayReshape(ASR::ArrayReshape_t* x) {
        // We don't need to replace the shape here, as it will overwrite some results
        x->m_shape = nullptr;

        // If the array is a Var, get past it to it's value
        if (ASR::is_a<ASR::Var_t>(*x->m_array)) {
            ASR::Variable_t *arr_variable = ASRUtils::EXPR2VAR(x->m_array);
            if (arr_variable->m_value != nullptr) {
                x->m_array = arr_variable->m_value;
            }
        } else if (ASR::is_a<ASR::ArrayConstructor_t>(*x->m_array)) {
            ASR::ArrayConstructor_t *arr_constructor = ASR::down_cast<ASR::ArrayConstructor_t>(x->m_array);
            if (arr_constructor->m_value) {
                x->m_array = arr_constructor->m_value;
            }
        }

        BaseExprReplacer<ReplaceInitExpr>::replace_ArrayReshape(x);
    }

};

class InitExprVisitor : public ASR::CallReplacerOnExpressionsVisitor<InitExprVisitor>
{
    private:

        Allocator& al;
        ReplaceInitExpr replacer;
        std::map<SymbolTable*, Vec<ASR::stmt_t*>> symtab2decls;

    public:

        InitExprVisitor(Allocator& al_) :
        al(al_), replacer(al_, symtab2decls) {
        }

        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.current_scope = current_scope;
            replacer.replace_expr(*current_expr);
        }

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            Vec<ASR::stmt_t*> body;
            body.reserve(al, n_body);

            // Collect leading CPtrToPointer statements from the existing body
            // first (e.g. equivalence pointer setup). These must execute before
            // any init-expr assignments that index into the pointer arrays.
            size_t i = 0;
            for (; i < n_body; i++) {
                if (ASR::is_a<ASR::CPtrToPointer_t>(*m_body[i])) {
                    body.push_back(al, m_body[i]);
                } else {
                    break;
                }
            }

            if( symtab2decls.find(current_scope) != symtab2decls.end() ) {
                Vec<ASR::stmt_t*>& decls = symtab2decls[current_scope];
                for (size_t j = 0; j < decls.size(); j++) {
                    body.push_back(al, decls[j]);
                }
                symtab2decls.erase(current_scope);
            }

            for (; i < n_body; i++) {
                body.push_back(al, m_body[i]);
            }
            m_body = body.p;
            n_body = body.size();
        }

        void visit_SymbolsContainingSymbolTable() {
            std::vector<std::string> var_order = ASRUtils::determine_variable_declaration_order(current_scope);
            for (auto &a : var_order) {
                ASR::symbol_t* sym = current_scope->get_symbol(a);
                this->visit_symbol(*sym);
            }
        }

        void visit_Program(const ASR::Program_t& x) {
            ASR::Program_t& xx = const_cast<ASR::Program_t&>(x);
            SymbolTable* current_scope_copy = current_scope;
            current_scope = xx.m_symtab;
            visit_SymbolsContainingSymbolTable();
            transform_stmts(xx.m_body, xx.n_body);
            current_scope = current_scope_copy;
        }

        void visit_Function(const ASR::Function_t& x) {
            ASR::Function_t& xx = const_cast<ASR::Function_t&>(x);
            SymbolTable* current_scope_copy = current_scope;
            current_scope = xx.m_symtab;
            visit_SymbolsContainingSymbolTable();
            transform_stmts(xx.m_body, xx.n_body);
            current_scope = current_scope_copy;
        }

        void visit_Module(const ASR::Module_t& x) {
            ASR::Module_t& xx = const_cast<ASR::Module_t&>(x);
            SymbolTable* current_scope_copy = current_scope;
            current_scope = xx.m_symtab;
            visit_SymbolsContainingSymbolTable();
            current_scope = current_scope_copy;
        }

        void visit_Variable(const ASR::Variable_t &x) {
            ASR::symbol_t* asr_owner = ASRUtils::get_asr_owner(&(x.base));
            ASR::expr_t* symbolic_value = x.m_symbolic_value;
            if( symbolic_value && ASR::is_a<ASR::Cast_t>(*symbolic_value) ) {
                symbolic_value = ASR::down_cast<ASR::Cast_t>(symbolic_value)->m_arg;
            }
            bool is_struct_array_broadcast = symbolic_value &&
                ASR::is_a<ASR::ArrayBroadcast_t>(*symbolic_value) &&
                ASRUtils::is_array(ASRUtils::expr_type(symbolic_value)) &&
                ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::type_get_past_array(
                        ASRUtils::expr_type(symbolic_value)));
            bool can_lower_struct_array_broadcast = is_struct_array_broadcast &&
                x.m_storage != ASR::storage_typeType::Parameter &&
                x.m_storage != ASR::storage_typeType::Save &&
                !(asr_owner && ASR::is_a<ASR::Module_t>(*asr_owner));
            // The initial value of a variable in a common block is the
            // initial value of the block's static storage, not code.
            if( x.m_common_block != nullptr ||
                !(symbolic_value &&
                  (ASR::is_a<ASR::ArrayConstant_t>(*symbolic_value) ||
                   ASR::is_a<ASR::StructConstructor_t>(*symbolic_value) ||
                   (ASR::is_a<ASR::Cast_t>(*x.m_symbolic_value) && ASR::is_a<ASR::ArrayReshape_t>(*symbolic_value)) ||
                   ASR::is_a<ASR::ArrayConstructor_t>(*symbolic_value) ||
                   can_lower_struct_array_broadcast)) ||
                 (asr_owner && ASR::is_a<ASR::Module_t>(*asr_owner) &&
                  (ASR::is_a<ASR::ArrayConstant_t>(*symbolic_value) ||
                   ASR::is_a<ASR::ArrayConstructor_t>(*symbolic_value) ||
                   ASR::is_a<ASR::ArrayBroadcast_t>(*symbolic_value) ||
                   ASR::is_a<ASR::ArrayReshape_t>(*symbolic_value))) ||
                (x.m_storage == ASR::storage_typeType::Save &&
                ASR::is_a<ASR::Function_t>(*ASR::down_cast<ASR::symbol_t>(current_scope->asr_owner)))) {
                return ;
            }

            ASR::Variable_t& xx = const_cast<ASR::Variable_t&>(x);
            replacer.result_var = ASRUtils::EXPR(ASR::make_Var_t(al,
                                    x.base.base.loc, &(xx.base)));

            SymbolTable* current_scope_copy = current_scope;
            current_scope = x.m_parent_symtab;
            if (x.m_symbolic_value && (x.m_storage != ASR::storage_typeType::Parameter ||
                ASR::is_a<ASR::ArrayReshape_t>(*symbolic_value))) {
                ASR::expr_t** current_expr_copy = current_expr;
                current_expr = const_cast<ASR::expr_t**>(&(x.m_symbolic_value));
                call_replacer();
                current_expr = current_expr_copy;
                if( x.m_symbolic_value ) {
                    LCOMPILERS_ASSERT(x.m_value != nullptr);
                    visit_expr(*x.m_symbolic_value);
                } else {
                    xx.m_value = nullptr;
                }
            }
            visit_ttype(*x.m_type);
            current_scope = current_scope_copy;
        }

};

// LEN actuals initialize each automatic object when its scope is entered.
// Keep this independent of source declaration initialization: a LEN actual
// is a specification expression, not a SAVE or component default initializer.
class LengthParameterInitializer : public ASR::BaseWalkVisitor<LengthParameterInitializer> {
    Allocator& al;
    ASR::TranslationUnit_t& unit;

    void initialize(SymbolTable* scope, ASR::stmt_t**& statements, size_t& n_statements) {
        Vec<ASR::stmt_t*> body;
        body.reserve(al, n_statements);
        for (const auto& name : ASRUtils::determine_variable_declaration_order(scope)) {
            ASR::symbol_t* symbol = scope->get_symbol(name);
            if (!ASR::is_a<ASR::Variable_t>(*symbol)) continue;
            ASR::Variable_t* variable = ASR::down_cast<ASR::Variable_t>(symbol);
            ASR::ttype_t* type = ASRUtils::extract_type(variable->m_type);
            if (!ASR::is_a<ASR::StructType_t>(*type) || !variable->m_type_declaration
                    || (variable->m_intent != ASR::intentType::Local
                        && variable->m_intent != ASR::intentType::ReturnVar)
                    || ASRUtils::is_allocatable(variable->m_type)
                    || ASRUtils::is_pointer(variable->m_type)) continue;
            ASR::StructType_t* instance = ASR::down_cast<ASR::StructType_t>(type);
            if (instance->n_len_parameters == 0) continue;
            ASR::Struct_t* definition = ASR::down_cast<ASR::Struct_t>(
                ASRUtils::symbol_get_past_external(variable->m_type_declaration));
            LCOMPILERS_ASSERT(instance->n_len_parameters == definition->n_len_params);
            const Location& loc = variable->base.base.loc;
            ASR::expr_t* object = ASRUtils::EXPR(ASR::make_Var_t(al, loc, &variable->base));
            for (size_t i = 0; i < instance->n_len_parameters; i++) {
                ASR::expr_t* actual = instance->m_len_parameters[i].m_value;
                LCOMPILERS_ASSERT(actual);
                ASR::symbol_t* member = definition->m_symtab->get_symbol(definition->m_len_params[i]);
                member = ASRUtils::import_struct_instance_member(al, member, scope);
                ASR::ttype_t* member_type = ASRUtils::symbol_type(member);
                if (ASRUtils::is_array(variable->m_type)) {
                    ASR::dimension_t* dims = nullptr;
                    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(variable->m_type, dims);
                    Vec<ASR::dimension_t> dimensions;
                    dimensions.from_pointer_n(dims, n_dims);
                    member_type = ASRUtils::duplicate_type(al, member_type, &dimensions);
                }
                ASR::expr_t* target = ASRUtils::EXPR(ASR::make_StructInstanceMember_t(
                    al, loc, object, member, member_type, nullptr));
                ASRUtils::make_ArrayBroadcast_t_util(al, loc, target, actual);
                body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                    al, loc, target, actual, nullptr, false, false)));
            }
        }
        for (size_t i = 0; i < n_statements; i++) body.push_back(al, statements[i]);
        statements = body.p;
        n_statements = body.size();
    }
public:
    LengthParameterInitializer(Allocator& al_, ASR::TranslationUnit_t& unit_)
        : al(al_), unit(unit_) {}
    void visit_Module(const ASR::Module_t& x) {
        ASR::stmt_t** statements = nullptr;
        size_t n_statements = 0;
        auto& module = const_cast<ASR::Module_t&>(x);
        initialize(module.m_symtab, statements, n_statements);
        if (n_statements > 0) {
            ASR::Function_t* startup = ASRUtils::get_or_create_global_init(
                al, unit, (ASR::asr_t*)&module.base, module.m_loaded_from_mod);
            if (!module.m_loaded_from_mod) {
                for (size_t i = 0; i < n_statements; i++) {
                    ASRUtils::global_init_append_stmt(al, startup, statements[i]);
                }
            }
        }
        ASR::BaseWalkVisitor<LengthParameterInitializer>::visit_Module(x);
    }
    void visit_Program(const ASR::Program_t& x) {
        auto& program = const_cast<ASR::Program_t&>(x);
        initialize(program.m_symtab, program.m_body, program.n_body);
        ASR::BaseWalkVisitor<LengthParameterInitializer>::visit_Program(x);
    }
    void visit_Function(const ASR::Function_t& x) {
        auto& function = const_cast<ASR::Function_t&>(x);
        initialize(function.m_symtab, function.m_body, function.n_body);
        ASR::BaseWalkVisitor<LengthParameterInitializer>::visit_Function(x);
    }
    void visit_Block(const ASR::Block_t& x) {
        auto& block = const_cast<ASR::Block_t&>(x);
        initialize(block.m_symtab, block.m_body, block.n_body);
        ASR::BaseWalkVisitor<LengthParameterInitializer>::visit_Block(x);
    }
};

void pass_replace_init_expr(Allocator &al,
    ASR::TranslationUnit_t &unit,
    const LCompilers::PassOptions& /*pass_options*/) {
    LengthParameterInitializer lengths(al, unit);
    lengths.visit_TranslationUnit(unit);
    InitExprVisitor v(al);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor w(al);
    w.visit_TranslationUnit(unit);
}


} // namespace LCompilers

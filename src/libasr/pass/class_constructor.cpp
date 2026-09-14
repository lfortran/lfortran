#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_class_constructor.h>
#include <libasr/pass/pass_utils.h>


namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

// The StructConstant a named constant of derived type stands for, if any.
static ASR::StructConstant_t* get_struct_constant(ASR::expr_t* x) {
    if (ASR::is_a<ASR::Var_t>(*x)) {
        ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(x)->m_v);
        if (!ASR::is_a<ASR::Variable_t>(*sym)) {
            return nullptr;
        }
        ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
        if (var->m_storage != ASR::storage_typeType::Parameter) {
            return nullptr;
        }
        x = var->m_value;
    }
    if (x && ASR::is_a<ASR::StructConstant_t>(*x)) {
        return ASR::down_cast<ASR::StructConstant_t>(x);
    }
    return nullptr;
}

static bool is_parameter(ASR::expr_t* x) {
    if (!ASR::is_a<ASR::Var_t>(*x)) {
        return false;
    }
    ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(x)->m_v);
    return ASR::is_a<ASR::Variable_t>(*sym) &&
        ASR::down_cast<ASR::Variable_t>(sym)->m_storage ==
            ASR::storage_typeType::Parameter;
}

// Rebuild a StructConstant, including nested StructConstant components, as
// the equivalent StructConstructor whose value is the constant. A null()
// allocatable or pointer component is left out, as an omitted constructor
// argument is, so the component keeps its default initialization.
static ASR::expr_t* struct_constant_to_constructor(Allocator& al,
        ASR::StructConstant_t* x) {
    std::deque<ASR::symbol_t*> members;
    ASR::Struct_t* struct_sym = ASR::down_cast<ASR::Struct_t>(
        ASRUtils::symbol_get_past_external(x->m_dt_sym));
    while (struct_sym) {
        for (int i = (int) struct_sym->n_members - 1; i >= 0; i--) {
            members.push_front(struct_sym->m_symtab->get_symbol(
                struct_sym->m_members[i]));
        }
        struct_sym = struct_sym->m_parent ? ASR::down_cast<ASR::Struct_t>(
            ASRUtils::symbol_get_past_external(struct_sym->m_parent)) : nullptr;
    }
    LCOMPILERS_ASSERT(members.size() == x->n_args);
    Vec<ASR::call_arg_t> args;
    args.reserve(al, x->n_args);
    for (size_t i = 0; i < x->n_args; i++) {
        ASR::call_arg_t arg = x->m_args[i];
        ASR::ttype_t* member_type = ASRUtils::symbol_type(members[i]);
        if (arg.m_value && ASR::is_a<ASR::PointerNullConstant_t>(*arg.m_value) &&
                (ASRUtils::is_allocatable(member_type) ||
                 ASRUtils::is_pointer(member_type))) {
            arg.m_value = nullptr;
        } else if (arg.m_value && ASR::is_a<ASR::StructConstant_t>(*arg.m_value) &&
                ASR::is_a<ASR::StructType_t>(*member_type) &&
                !ASRUtils::is_class_type(member_type)) {
            arg.m_value = struct_constant_to_constructor(al,
                ASR::down_cast<ASR::StructConstant_t>(arg.m_value));
        }
        args.push_back(al, arg);
    }
    return ASRUtils::EXPR(ASR::make_StructConstructor_t(al, x->base.base.loc,
        x->m_dt_sym, args.p, args.size(), x->m_type, &x->base));
}

class ReplaceStructConstructor: public ASR::BaseExprReplacer<ReplaceStructConstructor> {

    public:

    Allocator& al;
    Vec<ASR::stmt_t*>& pass_result;
    bool& remove_original_statement;
    bool realloc_lhs;

    SymbolTable* current_scope;
    ASR::expr_t* result_var;

    ReplaceStructConstructor(Allocator& al_, Vec<ASR::stmt_t*>& pass_result_,
        bool& remove_original_statement_) :
    al(al_), pass_result(pass_result_),
    remove_original_statement(remove_original_statement_),
    current_scope(nullptr), result_var(nullptr) {}

    void replace_StructConstructor(ASR::StructConstructor_t* x) {
        Vec<ASR::stmt_t*>* result_vec = &pass_result;
        PassUtils::ReplacerUtils::replace_StructConstructor(
            x, this, false, remove_original_statement, result_vec, false,
        ASR::cast_kindType::IntegerToInteger, nullptr, realloc_lhs);
    }
};

class StructConstructorVisitor : public ASR::CallReplacerOnExpressionsVisitor<StructConstructorVisitor>
{
    private:

        Allocator& al;
        bool remove_original_statement;
        ReplaceStructConstructor replacer;
        Vec<ASR::stmt_t*> pass_result;
        bool realloc_lhs;

    public:

        StructConstructorVisitor(Allocator& al_, bool realloc_lhs_) :
        al(al_), remove_original_statement(false),
        replacer(al_, pass_result, remove_original_statement), realloc_lhs(realloc_lhs_) {
            pass_result.n = 0;
            pass_result.reserve(al, 0);
        }

        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.current_scope = current_scope;
            replacer.realloc_lhs = realloc_lhs;
            replacer.replace_expr(*current_expr);
        }

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            Vec<ASR::stmt_t*> body;
            body.reserve(al, n_body);

            for (size_t i = 0; i < n_body; i++) {
                pass_result.n = 0;
                pass_result.reserve(al, 1);
                remove_original_statement = false;
                replacer.result_var = nullptr;
                visit_stmt(*m_body[i]);
                for (size_t j = 0; j < pass_result.size(); j++) {
                    body.push_back(al, pass_result[j]);
                }
                if( !remove_original_statement ) {
                    body.push_back(al, m_body[i]);
                }
                remove_original_statement = false;
            }
            m_body = body.p;
            n_body = body.size();
            replacer.result_var = nullptr;
            pass_result.n = 0;
            pass_result.reserve(al, 0);
        }

        void visit_Variable(const ASR::Variable_t& /*x*/) {
            // Do nothing, already handled in init_expr pass
        }

        void visit_Assignment(const ASR::Assignment_t &x) {
            if (x.m_overloaded) {
                this->visit_stmt(*x.m_overloaded);
                remove_original_statement = false;
                return ;
            }

            // A structure constructor is a scalar value. When the target is an
            // array (a whole array, or an array section), the constructor is
            // broadcast over it, so its components cannot be written straight
            // into the target: a component reference built on an array-valued
            // base is itself an array, and the value being assigned is a
            // scalar. Materialise the constructor in a scalar temporary
            // instead and leave the original assignment in place, so the
            // array passes lower the broadcast.
            ASR::ttype_t* target_type = ASRUtils::expr_type(x.m_target);
            if ((ASR::is_a<ASR::Allocatable_t>(*target_type) ||
                    ASRUtils::is_array(target_type)) &&
                    ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(target_type))) {
                replacer.result_var = nullptr;
            } else {
                replacer.result_var = x.m_target;
                // Assigning a constant structure is lowered like assigning
                // the equivalent structure constructor: component by
                // component, so every kind of component is copied.
                ASR::StructConstant_t* value = get_struct_constant(x.m_value);
                if (value && ASR::is_a<ASR::StructType_t>(*target_type) &&
                        !ASRUtils::is_class_type(target_type) &&
                        !is_parameter(x.m_target)) {
                    const_cast<ASR::Assignment_t&>(x).m_value =
                        struct_constant_to_constructor(al, value);
                }
            }

            ASR::expr_t** current_expr_copy_9 = current_expr;
            current_expr = const_cast<ASR::expr_t**>(&(x.m_value));
            this->call_replacer();
            current_expr = current_expr_copy_9;
            if( !remove_original_statement ) {
                this->visit_expr(*x.m_value);
            }
        }

};

void pass_replace_class_constructor(Allocator &al,
    ASR::TranslationUnit_t &unit,
    const LCompilers::PassOptions& pass_options) {
    StructConstructorVisitor v(al, pass_options.realloc_lhs_arrays);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor w(al);
    w.visit_TranslationUnit(unit);
}


} // namespace LCompilers

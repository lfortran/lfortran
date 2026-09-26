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

// Whether evaluating an expression may read the storage of an assignment
// target. It errs on the side of `true`: a function may read the target
// through host or use association, and a pointer or an associate name may
// refer to it.
class TargetReadFinder : public ASR::BaseWalkVisitor<TargetReadFinder> {

    public:

    ASR::symbol_t* target_root;
    bool target_through_pointer;
    bool found;

    TargetReadFinder(ASR::symbol_t* target_root_, bool target_through_pointer_) :
        target_root(target_root_), target_through_pointer(target_through_pointer_),
        found(false) {}

    void visit_Var(const ASR::Var_t& x) {
        ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(x.m_v);
        if( sym == target_root || target_through_pointer ||
                !ASR::is_a<ASR::Variable_t>(*sym) ||
                ASRUtils::is_pointer(ASRUtils::symbol_type(sym)) ) {
            found = true;
        }
    }

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t& x) {
        if( ASRUtils::is_pointer(x.m_type) ) {
            found = true;
        }
        ASR::BaseWalkVisitor<TargetReadFinder>::visit_StructInstanceMember(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& x) {
        if( x.m_value == nullptr ) {
            found = true;
        }
        ASR::BaseWalkVisitor<TargetReadFinder>::visit_FunctionCall(x);
    }
};

// F2018 10.2.1.3: the value of an intrinsic assignment is evaluated before
// the target is defined. A structure constructor written component by
// component straight into the target breaks that when a component value
// reads the target, as the components assigned before it have already been
// overwritten. Returns whether `value` may do so.
static bool value_may_read_target(ASR::expr_t* target, ASR::expr_t* value) {
    bool target_through_pointer = false;
    ASR::expr_t* root = target;
    while( true ) {
        if( ASRUtils::is_pointer(ASRUtils::expr_type(root)) ) {
            target_through_pointer = true;
        }
        if( ASR::is_a<ASR::StructInstanceMember_t>(*root) ) {
            root = ASR::down_cast<ASR::StructInstanceMember_t>(root)->m_v;
        } else if( ASR::is_a<ASR::ArrayItem_t>(*root) ) {
            root = ASR::down_cast<ASR::ArrayItem_t>(root)->m_v;
        } else if( ASR::is_a<ASR::ArraySection_t>(*root) ) {
            root = ASR::down_cast<ASR::ArraySection_t>(root)->m_v;
        } else {
            break;
        }
    }
    if( !ASR::is_a<ASR::Var_t>(*root) ) {
        return true;
    }
    TargetReadFinder finder(ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(root)->m_v), target_through_pointer);
    finder.visit_expr(*value);
    return finder.found;
}

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
            } else if( value_may_read_target(x.m_target, x.m_value) ) {
                // Build the value in a temporary and assign that to the
                // target, so no component value sees a component of the
                // target already overwritten.
                replacer.result_var = nullptr;
            } else {
                replacer.result_var = x.m_target;
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

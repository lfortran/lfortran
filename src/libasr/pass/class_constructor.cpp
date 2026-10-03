#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_class_constructor.h>
#include <libasr/pass/pass_utils.h>

#include <map>
#include <set>


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

// The variable a designator such as `t%a(i)%x` refers to a part of, or
// `nullptr` when the designator is not rooted in a variable. Sets
// `through_pointer` when a pointer is dereferenced on the way, as the
// designator then refers to storage the root does not own.
static ASR::symbol_t* designator_root(ASR::expr_t* expr, bool& through_pointer) {
    through_pointer = false;
    while( true ) {
        expr = ASRUtils::get_past_array_physical_cast(expr);
        if( ASRUtils::is_pointer(ASRUtils::expr_type(expr)) ) {
            through_pointer = true;
        }
        if( ASR::is_a<ASR::StructInstanceMember_t>(*expr) ) {
            expr = ASR::down_cast<ASR::StructInstanceMember_t>(expr)->m_v;
        } else if( ASR::is_a<ASR::ArrayItem_t>(*expr) ) {
            expr = ASR::down_cast<ASR::ArrayItem_t>(expr)->m_v;
        } else if( ASR::is_a<ASR::ArraySection_t>(*expr) ) {
            expr = ASR::down_cast<ASR::ArraySection_t>(expr)->m_v;
        } else {
            break;
        }
    }
    if( !ASR::is_a<ASR::Var_t>(*expr) ) {
        return nullptr;
    }
    return ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(expr)->m_v);
}

// The procedure (or main program) whose local variable `sym` is, looking
// past BLOCK and ASSOCIATE scopes, or `nullptr` when `sym` is not a variable
// local to a procedure, such as a module variable.
static ASR::symbol_t* owning_procedure(ASR::symbol_t* sym) {
    ASR::symbol_t* owner = ASRUtils::get_asr_owner(sym);
    while( owner != nullptr && (ASR::is_a<ASR::Block_t>(*owner) ||
            ASR::is_a<ASR::AssociateBlock_t>(*owner)) ) {
        owner = ASRUtils::get_asr_owner(owner);
    }
    if( owner == nullptr || !(ASR::is_a<ASR::Function_t>(*owner) ||
            ASR::is_a<ASR::Program_t>(*owner)) ) {
        return nullptr;
    }
    return owner;
}

// Whether `sym` is a variable of a procedure that nothing outside the
// procedure can see: not a dummy argument, not SAVEd, not a TARGET.
static bool is_private_local(ASR::symbol_t* sym) {
    if( !ASR::is_a<ASR::Variable_t>(*sym) ) {
        return false;
    }
    ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sym);
    return (v->m_intent == ASR::intentType::Local ||
            v->m_intent == ASR::intentType::ReturnVar) &&
        v->m_storage == ASR::storage_typeType::Default &&
        !v->m_target_attr && owning_procedure(sym) != nullptr;
}

// The pointer associations made in a procedure. Fortran allows a pointer to
// be associated only with a TARGET, but the compiler associates pointers with
// other variables too: the nested_vars pass gives the internal procedures of
// a host access to a host variable through a pointer, and a section passed
// to a structure constructor is referenced through a pointer.
class PointerAssociations : public ASR::BaseWalkVisitor<PointerAssociations> {

    public:

    // Every variable some pointer is associated with.
    std::set<ASR::symbol_t*> associated_roots;
    // For a pointer variable, the variables it is associated with.
    std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>> pointer_roots;
    // Pointer variables whose association is not known: associated with
    // something not rooted in a variable, or passed to a procedure that
    // may associate them.
    std::set<ASR::symbol_t*> unknown_pointers;

    void pointer_defined_unknown(ASR::expr_t* ptr) {
        bool through_pointer;
        ASR::symbol_t* root = designator_root(ptr, through_pointer);
        if( root != nullptr ) {
            unknown_pointers.insert(root);
        }
    }

    void visit_Associate(const ASR::Associate_t& x) {
        bool through_pointer;
        ASR::symbol_t* root = designator_root(x.m_value, through_pointer);
        if( root != nullptr ) {
            associated_roots.insert(root);
        }
        if( ASR::is_a<ASR::Var_t>(*x.m_target) ) {
            ASR::symbol_t* ptr = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(x.m_target)->m_v);
            if( root == nullptr || through_pointer ) {
                unknown_pointers.insert(ptr);
            } else {
                pointer_roots[ptr].insert(root);
            }
        }
        ASR::BaseWalkVisitor<PointerAssociations>::visit_Associate(x);
    }

    void visit_CPtrToPointer(const ASR::CPtrToPointer_t& x) {
        pointer_defined_unknown(x.m_ptr);
        ASR::BaseWalkVisitor<PointerAssociations>::visit_CPtrToPointer(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& x) {
        for( size_t i = 0; i < x.n_args; i++ ) {
            if( x.m_args[i].m_value != nullptr ) {
                pointer_defined_unknown(x.m_args[i].m_value);
            }
        }
        ASR::BaseWalkVisitor<PointerAssociations>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
        for( size_t i = 0; i < x.n_args; i++ ) {
            if( x.m_args[i].m_value != nullptr ) {
                pointer_defined_unknown(x.m_args[i].m_value);
            }
        }
        ASR::BaseWalkVisitor<PointerAssociations>::visit_SubroutineCall(x);
    }
};

// The pointer associations of each procedure, collected when first needed.
class PointerAssociationsCache {

    public:

    std::map<ASR::symbol_t*, PointerAssociations> procedures;

    const PointerAssociations& get(ASR::symbol_t* procedure) {
        auto it = procedures.find(procedure);
        if( it == procedures.end() ) {
            PointerAssociations associations;
            associations.visit_symbol(*procedure);
            it = procedures.emplace(procedure, std::move(associations)).first;
        }
        return it->second;
    }

    // Whether a procedure called from the one that owns `root` may read it.
    // Besides through its arguments, which the calling expression itself
    // shows, it can do so only if `root` is a module variable, is SAVEd or a
    // TARGET, is a dummy argument (whose actual argument the callee may see),
    // or has a pointer associated with it (as an internal procedure reaches
    // a variable of its host). A local variable of the calling procedure is
    // otherwise out of its reach, including a compiler temporary.
    bool reachable_by_call(ASR::symbol_t* root) {
        if( !is_private_local(root) ) {
            return true;
        }
        const PointerAssociations& associations = get(owning_procedure(root));
        return associations.associated_roots.find(root) !=
            associations.associated_roots.end();
    }

    // The variables the local pointer `ptr` may be associated with, or
    // `nullptr` when that is not known.
    const std::set<ASR::symbol_t*>* pointer_roots(ASR::symbol_t* ptr) {
        if( !is_private_local(ptr) ) {
            return nullptr;
        }
        const PointerAssociations& associations = get(owning_procedure(ptr));
        if( associations.unknown_pointers.find(ptr) !=
                associations.unknown_pointers.end() ) {
            return nullptr;
        }
        auto it = associations.pointer_roots.find(ptr);
        if( it == associations.pointer_roots.end() ) {
            return nullptr;
        }
        return &it->second;
    }
};

// Whether evaluating an expression may read the storage of an assignment
// target. It errs on the side of `true`: a pointer or an associate name may
// refer to the target, two TARGET dummy arguments may share storage, and a
// function may read a target that it can reach.
class TargetReadFinder : public ASR::BaseWalkVisitor<TargetReadFinder> {

    public:

    PointerAssociationsCache& pointer_associations;
    ASR::symbol_t* target_root;
    bool target_through_pointer;
    bool target_reachable_by_call;
    bool found;

    TargetReadFinder(PointerAssociationsCache& pointer_associations_,
        ASR::symbol_t* target_root_, bool target_through_pointer_,
        bool target_reachable_by_call_) :
        pointer_associations(pointer_associations_),
        target_root(target_root_), target_through_pointer(target_through_pointer_),
        target_reachable_by_call(target_reachable_by_call_), found(false) {}

    // Whether reading the variable `sym` (not through a pointer) may read
    // the target. F2018 15.5.2.13: two variables with the TARGET attribute
    // may share storage when one of them is a dummy argument.
    bool may_be_target(ASR::symbol_t* sym) {
        if( sym == target_root ) {
            return true;
        }
        if( !ASR::is_a<ASR::Variable_t>(*sym) ||
                !ASR::is_a<ASR::Variable_t>(*target_root) ) {
            return true;
        }
        const ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sym);
        const ASR::Variable_t* root = ASR::down_cast<ASR::Variable_t>(target_root);
        return v->m_target_attr && root->m_target_attr &&
            (ASRUtils::is_arg_dummy(v->m_intent) ||
             ASRUtils::is_arg_dummy(root->m_intent));
    }

    // Whether reading through the pointer variable `ptr` may read the
    // target: unless it is a local pointer associated only with known
    // variables, none of which may be the target.
    bool pointer_may_reach_target(ASR::symbol_t* ptr) {
        const std::set<ASR::symbol_t*>* roots =
            pointer_associations.pointer_roots(ptr);
        if( roots == nullptr ) {
            return true;
        }
        for( ASR::symbol_t* root : *roots ) {
            if( may_be_target(root) ) {
                return true;
            }
        }
        return false;
    }

    void visit_Var(const ASR::Var_t& x) {
        ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(x.m_v);
        if( ASR::is_a<ASR::Function_t>(*sym) ) {
            // A procedure passed as an actual argument is called by the
            // procedure it is passed to.
            if( target_reachable_by_call ) {
                found = true;
            }
            return;
        }
        if( target_through_pointer || may_be_target(sym) ||
                (ASRUtils::is_pointer(ASRUtils::symbol_type(sym)) &&
                 pointer_may_reach_target(sym)) ) {
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
        if( x.m_value == nullptr && target_reachable_by_call ) {
            found = true;
        }
        ASR::BaseWalkVisitor<TargetReadFinder>::visit_FunctionCall(x);
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
        PointerAssociationsCache pointer_associations;

        // F2018 10.2.1.3: the value of an intrinsic assignment is evaluated
        // before the target is defined. A structure constructor written
        // component by component straight into the target breaks that when
        // a component value reads the target, as the components assigned
        // before it have already been overwritten. Returns whether `value`
        // may do so.
        bool value_may_read_target(ASR::expr_t* target, ASR::expr_t* value) {
            bool through_pointer;
            ASR::symbol_t* root = designator_root(target, through_pointer);
            if( root == nullptr ) {
                return true;
            }
            TargetReadFinder finder(pointer_associations, root, through_pointer,
                through_pointer || pointer_associations.reachable_by_call(root));
            finder.visit_expr(*value);
            return finder.found;
        }

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
            } else if( ASR::is_a<ASR::StructConstructor_t>(*x.m_value) &&
                    value_may_read_target(x.m_target, x.m_value) ) {
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

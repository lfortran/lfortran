#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_with_compile_time_values.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_array_function_registry.h>

#include <libasr/asr_builder.h>

#include <unordered_set>
#include <vector>

namespace LCompilers {

// True if the actual argument `arg` is an array element designator or an
// array element substring designator. Such an actual argument represents the
// element sequence that starts at that element (F2018 15.5.2.11 p2), so it
// cannot be replaced by the value of the element.
static bool is_array_element_designator(ASR::expr_t* arg) {
    if (ASR::is_a<ASR::StringSection_t>(*arg)) {
        arg = ASR::down_cast<ASR::StringSection_t>(arg)->m_arg;
    } else if (ASR::is_a<ASR::StringItem_t>(*arg)) {
        arg = ASR::down_cast<ASR::StringItem_t>(arg)->m_arg;
    }
    return ASR::is_a<ASR::ArrayItem_t>(*arg);
}

// True if `sym` is a procedure with an implicit interface: nothing is known
// about its dummy arguments.
static bool has_implicit_interface(ASR::symbol_t* sym) {
    sym = ASRUtils::symbol_get_past_external(sym);
    return ASRUtils::is_bare_implicit_interface(sym) ||
        (ASR::is_a<ASR::Variable_t>(*sym) && ASRUtils::is_opaque_procedure_type(
            ASR::down_cast<ASR::Variable_t>(sym)->m_type));
}

// Collects the procedure pointers associated with a procedure with an
// implicit interface, `Associate(p, FunctionPointerCast(f, iface))`. A
// reference to `f` is a call through such a `p`, whose interface `iface` is
// built from the actual arguments of the call, so the dummy arguments of the
// called procedure are unknown.
class ImplicitInterfacePointerCollector:
        public ASR::BaseWalkVisitor<ImplicitInterfacePointerCollector> {
    public:
    std::unordered_set<ASR::symbol_t*>& pointers;

    ImplicitInterfacePointerCollector(
        std::unordered_set<ASR::symbol_t*>& pointers_): pointers(pointers_) {}

    void visit_Associate(const ASR::Associate_t& x) {
        if (!ASR::is_a<ASR::Var_t>(*x.m_target) ||
                !ASR::is_a<ASR::FunctionPointerCast_t>(*x.m_value)) {
            return;
        }
        ASR::expr_t* source =
            ASR::down_cast<ASR::FunctionPointerCast_t>(x.m_value)->m_arg;
        if (ASR::is_a<ASR::Var_t>(*source) &&
                has_implicit_interface(ASR::down_cast<ASR::Var_t>(source)->m_v)) {
            pointers.insert(ASR::down_cast<ASR::Var_t>(x.m_target)->m_v);
        }
    }
};

// True if the dummy argument associated with the actual argument `i` of a
// call to `name` may be an array, in which case an array element designator
// actual argument is sequence associated with it (F2018 15.5.2.4 p14): the
// called procedure has an implicit interface, or the dummy is a non-VALUE
// array.
static bool dummy_may_be_array(ASR::symbol_t* name, ASR::expr_t* dt, size_t i,
        const std::unordered_set<ASR::symbol_t*>& implicit_interface_pointers) {
    ASR::symbol_t* callee = ASRUtils::symbol_get_past_external(name);
    if (has_implicit_interface(callee) ||
            implicit_interface_pointers.count(callee) ||
            !(ASR::is_a<ASR::Function_t>(*callee) ||
              ASR::is_a<ASR::Variable_t>(*callee) ||
              ASR::is_a<ASR::StructMethodDeclaration_t>(*callee))) {
        return true;
    }
    ASR::Function_t* fn = ASRUtils::get_function(callee);
    if (fn == nullptr) {
        return true;
    }
    if (dt && !ASRUtils::get_class_proc_nopass_val(callee) &&
            i >= ASRUtils::get_pass_arg_index(callee)) {
        i++;
    }
    if (i >= fn->n_args || !ASR::is_a<ASR::Var_t>(*fn->m_args[i])) {
        return true;
    }
    ASR::symbol_t* dummy = ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v;
    if (!ASR::is_a<ASR::Variable_t>(*dummy)) {
        return true;
    }
    ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(dummy);
    return !v->m_value_attr && ASRUtils::is_array(v->m_type);
}

class CompileTimeValueReplacer: public ASR::BaseExprReplacer<CompileTimeValueReplacer> {

    private:

    Allocator& al;

    public:

    bool inside_prohibited_expression;

    // Actual arguments that are array element designators associated with
    // a dummy that may be an array. They are kept as they are.
    std::unordered_set<ASR::expr_t*> element_sequence_args;

    const std::unordered_set<ASR::symbol_t*>& implicit_interface_pointers;

    CompileTimeValueReplacer(Allocator& al_,
        const std::unordered_set<ASR::symbol_t*>& implicit_interface_pointers_):
        al(al_), inside_prohibited_expression(false),
        implicit_interface_pointers(implicit_interface_pointers_) {}

    void mark_element_sequence_args(ASR::symbol_t* name,
            ASR::call_arg_t* args, size_t n_args, ASR::expr_t* dt) {
        for (size_t i = 0; i < n_args; i++) {
            ASR::expr_t* arg = args[i].m_value;
            if (arg && is_array_element_designator(arg) &&
                    dummy_may_be_array(name, dt, i,
                        implicit_interface_pointers)) {
                element_sequence_args.insert(arg);
            }
        }
    }

    void replace_FunctionCall(ASR::FunctionCall_t* x) {
        mark_element_sequence_args(x->m_name, x->m_args, x->n_args,
            x->m_dt);
        ASR::BaseExprReplacer<CompileTimeValueReplacer>::replace_FunctionCall(x);
    }

    void replace_ArrayReshape(ASR::ArrayReshape_t* x) {
        ASR::BaseExprReplacer<CompileTimeValueReplacer>::replace_ArrayReshape(x);
        if( ASRUtils::is_fixed_size_array(
                ASRUtils::expr_type(x->m_array)) ) {
            x->m_type = ASRUtils::duplicate_type(al, x->m_type, nullptr,
                    ASR::array_physical_typeType::FixedSizeArray, true);
        }
    }

    template <typename ExprType>
    void replace_ExprMethod(ExprType* x,
            void (ASR::BaseExprReplacer<CompileTimeValueReplacer>::*replacer_function)(ExprType*) ) {
        bool inside_prohibited_expression_copy = inside_prohibited_expression;
        inside_prohibited_expression = true;
        (this->*replacer_function)(x);
        inside_prohibited_expression = inside_prohibited_expression_copy;
    }

    void replace_ArrayItem(ASR::ArrayItem_t* x) {
        replace_ExprMethod(x, &ASR::BaseExprReplacer<CompileTimeValueReplacer>::replace_ArrayItem);
    }

    void replace_StringSection(ASR::StringSection_t* x) {
        replace_ExprMethod(x, &ASR::BaseExprReplacer<CompileTimeValueReplacer>::replace_StringSection);
    }

    void replace_StringItem(ASR::StringItem_t* x) {
        replace_ExprMethod(x, &ASR::BaseExprReplacer<CompileTimeValueReplacer>::replace_StringItem);
    }

    void replace_expr(ASR::expr_t* x) {
        if( x == nullptr || inside_prohibited_expression ||
                element_sequence_args.count(x) ) {
            return ;
        }

        bool is_array_broadcast = ASR::is_a<ASR::ArrayBroadcast_t>(*x);
        if( is_array_broadcast ) {
            return ;
        }

        ASR::BaseExprReplacer<CompileTimeValueReplacer>::replace_expr(x);

        ASR::expr_t* compile_time_value = ASRUtils::expr_value(x);
        if( compile_time_value == nullptr ||
                ASRUtils::struct_constant_names_procedure(compile_time_value) ) {
            return ;
        }

        size_t value_rank = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(compile_time_value));
        size_t expr_rank = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(x));
        // TODO: Handle with reshape later
        if( value_rank != expr_rank ) {
            return ;
        }

        *current_expr = compile_time_value;
    }
};

class ExprVisitor: public ASR::CallReplacerOnExpressionsVisitor<ExprVisitor> {

    private:

    CompileTimeValueReplacer replacer;

    public:

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    ExprVisitor(Allocator& al_,
        const std::unordered_set<ASR::symbol_t*>& implicit_interface_pointers):
        replacer(al_, implicit_interface_pointers)
    {}

    template <typename ExprType>
    void visit_ExprMethod(const ExprType& x,
            void (ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::*visitor_function)(const ExprType&) ) {
        bool inside_prohibited_expression_copy = replacer.inside_prohibited_expression;
        replacer.inside_prohibited_expression = true;
        (this->*visitor_function)(x);
        replacer.inside_prohibited_expression = inside_prohibited_expression_copy;
    }

    void visit_ArrayItem(const ASR::ArrayItem_t& x) {
        visit_ExprMethod(x, &ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_ArrayItem);
    }

    void visit_StringSection(const ASR::StringSection_t& x) {
        visit_ExprMethod(x, &ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_StringSection);
    }

    void visit_StringItem(const ASR::StringItem_t& x) {
        visit_ExprMethod(x, &ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_StringItem);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
        replacer.mark_element_sequence_args(x.m_name, x.m_args, x.n_args,
            x.m_dt);
        ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& x) {
        replacer.mark_element_sequence_args(x.m_name, x.m_args, x.n_args,
            x.m_dt);
        ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_FunctionCall(x);
    }

    void visit_Assignment(const ASR::Assignment_t& x) {
        // Don't replace the assignment target with its compile-time value
        // (e.g., ch(5:5) should not become "4" when used as an assignment target)
        ASR::Assignment_t& xx = const_cast<ASR::Assignment_t&>(x);

        // Visit the target without calling the replacer
        bool inside_prohibited_expression_copy = replacer.inside_prohibited_expression;
        replacer.inside_prohibited_expression = true;
        ASR::expr_t** current_expr_copy = current_expr;
        current_expr = &(xx.m_target);
        replacer.current_expr = current_expr;
        ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        current_expr = current_expr_copy;
        replacer.inside_prohibited_expression = inside_prohibited_expression_copy;

        // Visit the value normally (can be replaced)
        current_expr = &(xx.m_value);
        replacer.current_expr = current_expr;
        ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        current_expr = current_expr_copy;
    }

    void visit_FileWrite(const ASR::FileWrite_t& x) {
        // Don't replace string write targets as compile-time constants
        ASR::FileWrite_t& xx = const_cast<ASR::FileWrite_t&>(x);
        ASR::expr_t** current_expr_copy = current_expr;
        // Check if writing to a string (not a file unit)
        bool is_string_write = xx.m_unit && ASRUtils::is_character(*ASRUtils::expr_type(xx.m_unit));
        if (is_string_write) {
            // Visit the unit (write target) without calling the replacer
            bool inside_prohibited_expression_copy = replacer.inside_prohibited_expression;
            replacer.inside_prohibited_expression = true;
            current_expr = &(xx.m_unit);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
            replacer.inside_prohibited_expression = inside_prohibited_expression_copy;
        } else {
            // Unit can be replaced normally
            if (xx.m_unit) {
                current_expr = &(xx.m_unit);
                replacer.current_expr = current_expr;
                ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
            }
        }
        // Visit other expr_t* fields normally
        if (xx.m_iomsg) {
            current_expr = &(xx.m_iomsg);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        if (xx.m_iostat) {
            current_expr = &(xx.m_iostat);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        if (xx.m_id) {
            current_expr = &(xx.m_id);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        // Visit values (can be replaced)
        for (size_t i = 0; i < xx.n_values; i++) {
            current_expr = &(xx.m_values[i]);
            replacer.current_expr = current_expr;
            this->visit_expr(*xx.m_values[i]);
        }
        if (xx.m_separator) {
            current_expr = &(xx.m_separator);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        if (xx.m_end) {
            current_expr = &(xx.m_end);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        current_expr = current_expr_copy;
    }
    
    void visit_FileRead(const ASR::FileRead_t& x) {
        ASR::FileRead_t& xx = const_cast<ASR::FileRead_t&>(x);
        ASR::expr_t** current_expr_copy = current_expr;

        // Visit unit, iomsg, iostat, id normally
        if (xx.m_unit) {
            current_expr = &(xx.m_unit);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        if (xx.m_iomsg) {
            current_expr = &(xx.m_iomsg);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        if (xx.m_iostat) {
            current_expr = &(xx.m_iostat);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        if (xx.m_id) {
            current_expr = &(xx.m_id);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        for (size_t i = 0; i < xx.n_values; i++) {
            bool inside_copy = replacer.inside_prohibited_expression;
            replacer.inside_prohibited_expression = true;
            current_expr = &(xx.m_values[i]);
            replacer.current_expr = current_expr;
            this->visit_expr(*xx.m_values[i]);
            replacer.inside_prohibited_expression = inside_copy;
        }
        if (xx.m_fmt) {
            current_expr = &(xx.m_fmt);
            replacer.current_expr = current_expr;
            ASR::CallReplacerOnExpressionsVisitor<ExprVisitor>::visit_expr(**current_expr);
        }
        current_expr = current_expr_copy;
    }
    
};

void pass_replace_with_compile_time_values(
    Allocator &al, ASR::TranslationUnit_t &unit,
    const LCompilers::PassOptions& /*pass_options*/) {
    std::unordered_set<ASR::symbol_t*> implicit_interface_pointers;
    ImplicitInterfacePointerCollector c(implicit_interface_pointers);
    c.visit_TranslationUnit(unit);
    ExprVisitor v(al, implicit_interface_pointers);
    // v.call_replacer_on_value = false;
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}


} // namespace LCompilers

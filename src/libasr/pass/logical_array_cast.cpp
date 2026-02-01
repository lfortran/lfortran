#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/logical_array_cast.h>
#include <libasr/pass/pass_utils.h>

/*
This ASR pass inserts explicit Cast nodes for logical array element accesses.

Logical arrays use byte-backed storage (i8) in LLVM to avoid issues with i1 arrays
under certain optimizations. Scalar logicals remain i1 for efficiency.

This pass makes the i1 <-> i8 conversions explicit at ASR level:
1. ArrayItem of logical type is changed to Integer(kind=1) type
2. When used as a value, the ArrayItem is wrapped in Cast(IntegerToLogical)
3. When storing to a logical ArrayItem, the RHS is wrapped in Cast(LogicalToInteger)
4. For GetPointer (used in EQUIVALENCE), ArrayItem is transformed but NOT wrapped
   in Cast, since we need the raw address, not the value

This removes the need for scattered workarounds in codegen.
*/

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

class LogicalArrayCastReplacer : public ASR::BaseExprReplacer<LogicalArrayCastReplacer> {
public:
    Allocator& al;
    bool in_assignment_target;
    bool in_get_pointer;
    bool in_pass_by_ref;

    LogicalArrayCastReplacer(Allocator& al_) : al(al_), in_assignment_target(false),
                                               in_get_pointer(false), in_pass_by_ref(false) {
        call_replacer_on_value = false;
    }

    void replace_ArrayItem(ASR::ArrayItem_t* x) {
        ASR::ttype_t* elem_type = x->m_type;
        if (!ASRUtils::is_logical(*elem_type)) {
            return;
        }

        if (in_pass_by_ref) {
            // For pass-by-reference (intent out/inout), don't transform at all.
            // Keep the ArrayItem as Logical type since the callee expects Logical.
            // The codegen handles the i8 storage transparently via get_el_type().
            return;
        }

        Location loc = x->base.base.loc;

        // Change ArrayItem type from Logical(k) to Integer(1)
        // Integer(kind=1) maps to i8 in LLVM, matching get_el_type() for logical arrays
        ASR::ttype_t* int1_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 1));
        x->m_type = int1_type;

        if (in_assignment_target || in_get_pointer) {
            // Don't wrap in Cast:
            // - For assignment targets, the RHS will be wrapped instead
            // - For GetPointer (EQUIVALENCE), we need the raw address, not the value
            return;
        }

        // Get the original logical kind for the cast
        int logical_kind = ASRUtils::extract_kind_from_ttype_t(elem_type);

        // When used as a value, wrap in Cast(IntegerToLogical)
        // This converts the i8 from memory to i1 for expression semantics
        ASR::ttype_t* logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, logical_kind));
        ASR::expr_t* array_item_expr = ASRUtils::EXPR((ASR::asr_t*)x);
        ASR::expr_t* cast_expr = ASRUtils::EXPR(ASR::make_Cast_t(
            al, loc, array_item_expr,
            ASR::cast_kindType::IntegerToLogical,
            logical_type, nullptr));

        *current_expr = cast_expr;
    }

    void replace_GetPointer(ASR::GetPointer_t* x) {
        // Set flag BEFORE processing m_arg to prevent wrapping ArrayItem in Cast.
        // This is critical: the base class replace_GetPointer calls replace_expr(m_arg)
        // which would trigger replace_ArrayItem. Without setting the flag first, the
        // ArrayItem would be wrapped in Cast(IntegerToLogical), loading the value as i1.
        // LLVM then tries to bitcast i1 to ptr, which is invalid.
        in_get_pointer = true;

        // Process m_arg (may contain ArrayItem that needs type transformation but not Cast)
        ASR::expr_t** current_expr_copy = current_expr;
        current_expr = &(x->m_arg);
        replace_expr(x->m_arg);
        current_expr = current_expr_copy;

        // Process m_type
        replace_ttype(x->m_type);

        // Process m_value if needed
        if (call_replacer_on_value && x->m_value) {
            current_expr_copy = current_expr;
            current_expr = &(x->m_value);
            replace_expr(x->m_value);
            current_expr = current_expr_copy;
        }

        in_get_pointer = false;
    }
};

class LogicalArrayCastVisitor : public ASR::CallReplacerOnExpressionsVisitor<LogicalArrayCastVisitor> {
private:
    LogicalArrayCastReplacer replacer;
    Allocator& al;

public:
    LogicalArrayCastVisitor(Allocator& al_) : replacer(al_), al(al_) {
        // Enable natural traversal so visit_GetPointer gets called
        visit_expr_after_replacement = true;
    }

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    void visit_Assignment(const ASR::Assignment_t& x) {
        ASR::Assignment_t& xx = const_cast<ASR::Assignment_t&>(x);

        // Check if target is a logical ArrayItem before transformation
        bool target_is_logical_array_item = false;
        if (x.m_target->type == ASR::exprType::ArrayItem) {
            ASR::ArrayItem_t* arr_item = ASR::down_cast<ASR::ArrayItem_t>(x.m_target);
            if (ASRUtils::is_logical(*arr_item->m_type)) {
                target_is_logical_array_item = true;
            }
        }

        // Process target with in_assignment_target flag set
        replacer.in_assignment_target = true;
        current_expr = &(xx.m_target);
        call_replacer();
        if (xx.m_target) {
            visit_expr(*xx.m_target);
        }
        replacer.in_assignment_target = false;

        // Process RHS
        current_expr = &(xx.m_value);
        call_replacer();
        if (xx.m_value) {
            visit_expr(*xx.m_value);
        }

        // If target was logical ArrayItem, wrap RHS in Cast(LogicalToInteger)
        if (target_is_logical_array_item) {
            ASR::expr_t* value = xx.m_value;
            if (ASRUtils::is_logical(*ASRUtils::expr_type(value))) {
                Location loc = value->base.loc;
                ASR::ttype_t* int1_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 1));
                ASR::expr_t* cast_expr = ASRUtils::EXPR(ASR::make_Cast_t(
                    al, loc, value,
                    ASR::cast_kindType::LogicalToInteger,
                    int1_type, nullptr));
                xx.m_value = cast_expr;
            }
        }

        if (x.m_overloaded) {
            visit_stmt(*x.m_overloaded);
        }
    }

    void visit_GetPointer(const ASR::GetPointer_t& x) {
        ASR::GetPointer_t& xx = const_cast<ASR::GetPointer_t&>(x);

        // Check if this GetPointer contains a logical ArrayItem
        bool has_logical_array_item = false;
        if (xx.m_arg && xx.m_arg->type == ASR::exprType::ArrayItem) {
            ASR::ArrayItem_t* arr_item = ASR::down_cast<ASR::ArrayItem_t>(xx.m_arg);
            if (ASRUtils::is_logical(*arr_item->m_type)) {
                has_logical_array_item = true;
            }
        }

        // Process the argument with in_get_pointer flag set
        // This prevents wrapping ArrayItem in Cast - we need raw address, not value
        replacer.in_get_pointer = true;
        current_expr = &(xx.m_arg);
        call_replacer();
        if (xx.m_arg) {
            visit_expr(*xx.m_arg);
        }
        replacer.in_get_pointer = false;

        // Update GetPointer's type to match the transformed ArrayItem
        if (has_logical_array_item) {
            Location loc = x.base.base.loc;
            ASR::ttype_t* int1_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 1));
            xx.m_type = ASRUtils::TYPE(
                ASR::make_Pointer_t(al, loc, int1_type));
        }

        visit_ttype(*xx.m_type);

        if (xx.m_value) {
            if (call_replacer_on_value) {
                current_expr = &(xx.m_value);
                call_replacer();
            }
            visit_expr(*xx.m_value);
        }
    }

    template <typename T>
    void visit_Call(const T& x) {
        T& xx = const_cast<T&>(x);

        // Get the function symbol to access parameter intents
        ASR::expr_t** orig_args = nullptr;
        ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(x.m_name);
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            orig_args = ASR::down_cast<ASR::Function_t>(sym)->m_args;
        }

        // Process each argument
        for (size_t i = 0; i < x.n_args; i++) {
            if (xx.m_args[i].m_value == nullptr) {
                continue;
            }

            // Check if the corresponding parameter has intent(out) or intent(inout)
            bool is_pass_by_ref = false;
            if (orig_args) {
                ASR::Variable_t* param = ASRUtils::expr_to_variable_or_null(orig_args[i]);
                if (param &&
                    (param->m_intent == ASRUtils::intent_out ||
                     param->m_intent == ASRUtils::intent_inout)) {
                    is_pass_by_ref = true;
                }
            }

            // Set the flag before processing if this is a pass-by-ref argument
            replacer.in_pass_by_ref = is_pass_by_ref;
            current_expr = &(xx.m_args[i].m_value);
            call_replacer();
            if (xx.m_args[i].m_value) {
                visit_expr(*xx.m_args[i].m_value);
            }
            replacer.in_pass_by_ref = false;
        }

        // Process x.m_dt if present (for type-bound procedures)
        if (xx.m_dt) {
            current_expr = &(xx.m_dt);
            call_replacer();
            visit_expr(*xx.m_dt);
        }
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
        visit_Call(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& x) {
        ASR::FunctionCall_t& xx = const_cast<ASR::FunctionCall_t&>(x);

        // Get the function symbol to access parameter intents
        ASR::expr_t** orig_args = nullptr;
        ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(x.m_name);
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            orig_args = ASR::down_cast<ASR::Function_t>(sym)->m_args;
        }

        // Process each argument
        for (size_t i = 0; i < x.n_args; i++) {
            if (xx.m_args[i].m_value == nullptr) {
                continue;
            }

            // Check if the corresponding parameter has intent(out) or intent(inout)
            bool is_pass_by_ref = false;
            if (orig_args) {
                ASR::Variable_t* param = ASRUtils::expr_to_variable_or_null(orig_args[i]);
                if (param &&
                    (param->m_intent == ASRUtils::intent_out ||
                     param->m_intent == ASRUtils::intent_inout)) {
                    is_pass_by_ref = true;
                }
            }

            // Set the flag before processing if this is a pass-by-ref argument
            replacer.in_pass_by_ref = is_pass_by_ref;
            current_expr = &(xx.m_args[i].m_value);
            call_replacer();
            if (xx.m_args[i].m_value) {
                visit_expr(*xx.m_args[i].m_value);
            }
            replacer.in_pass_by_ref = false;
        }

        // Process return type
        visit_ttype(*xx.m_type);

        // Process value if present
        if (xx.m_value) {
            if (call_replacer_on_value) {
                current_expr = &(xx.m_value);
                call_replacer();
            }
            visit_expr(*xx.m_value);
        }

        // Process x.m_dt if present (for type-bound procedures)
        if (xx.m_dt) {
            current_expr = &(xx.m_dt);
            call_replacer();
            visit_expr(*xx.m_dt);
        }
    }
};

void pass_logical_array_cast(Allocator &al, ASR::TranslationUnit_t &unit,
                             const LCompilers::PassOptions& /*pass_options*/) {
    LogicalArrayCastVisitor v(al);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers

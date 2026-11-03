#ifndef LIBASR_CODEGEN_C_LOOP_CONTROL_H
#define LIBASR_CODEGEN_C_LOOP_CONTROL_H

#include <set>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>

// The header of the C `for` loop that the C-family generators (C, C++,
// CUDA, Metal) write a counted DO loop as.

namespace LCompilers::CUtils {

// Whether the bound or step `e` of a counted loop has the same value on
// every iteration, so that the C `for` may test it as written instead of
// a copy taken before the loop. `written` holds everything the loop can
// modify, its own indices included, and `private_scalars` the scalars
// that nothing but the loop itself can modify, such as the scalar
// arguments that a GPU kernel holds a copy of its own of. That is a
// constant, or a scalar integer nothing in the loop can change -- an
// `intent(in)` or by-value dummy, one of `private_scalars`, or a local of
// the routine that nothing else can reach (not a target, not volatile,
// not in a common block) -- that the loop does not write, combined by
// integer arithmetic, comparisons, conditional expressions, `min`, `max`
// and the bounds of explicit-shape arrays. Anything else is not.
inline bool loop_control_invariant(ASR::expr_t *e,
        const std::set<ASR::symbol_t*> &written,
        const std::set<ASR::symbol_t*> &private_scalars) {
    auto invariant = [&](ASR::expr_t *x) {
        return x == nullptr
            || loop_control_invariant(x, written, private_scalars);
    };
    if (ASRUtils::is_value_constant(ASRUtils::expr_value(e))) return true;
    switch (e->type) {
        case ASR::exprType::IntegerConstant:
            return true;
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t *op = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            return invariant(op->m_left) && invariant(op->m_right);
        }
        case ASR::exprType::IntegerCompare: {
            ASR::IntegerCompare_t *op =
                ASR::down_cast<ASR::IntegerCompare_t>(e);
            return invariant(op->m_left) && invariant(op->m_right);
        }
        case ASR::exprType::IntegerUnaryMinus:
            return invariant(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg);
        case ASR::exprType::IfExp: {
            ASR::IfExp_t *ie = ASR::down_cast<ASR::IfExp_t>(e);
            return invariant(ie->m_test) && invariant(ie->m_body)
                && invariant(ie->m_orelse);
        }
        case ASR::exprType::Cast: {
            ASR::Cast_t *c = ASR::down_cast<ASR::Cast_t>(e);
            return (c->m_kind == ASR::cast_kindType::IntegerToInteger
                    || c->m_kind == ASR::cast_kindType::LogicalToInteger)
                && invariant(c->m_arg);
        }
        case ASR::exprType::IntrinsicElementalFunction: {
            ASR::IntrinsicElementalFunction_t *f =
                ASR::down_cast<ASR::IntrinsicElementalFunction_t>(e);
            if (f->m_intrinsic_id != static_cast<int64_t>(
                        ASRUtils::IntrinsicElementalFunctions::Max)
                    && f->m_intrinsic_id != static_cast<int64_t>(
                        ASRUtils::IntrinsicElementalFunctions::Min)) {
                return false;
            }
            for (size_t i = 0; i < f->n_args; i++) {
                if (!invariant(f->m_args[i])) return false;
            }
            return true;
        }
        case ASR::exprType::ArrayBound: {
            // The device code writes a bound of an explicit-shape array
            // from the extents its type declares.
            ASR::ArrayBound_t *ab = ASR::down_cast<ASR::ArrayBound_t>(e);
            ASR::ttype_t *t = ASRUtils::expr_type(ab->m_v);
            if (!ASR::is_a<ASR::Array_t>(*t)) return false;
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(t);
            int64_t dim = 1;
            if (ab->m_dim != nullptr && !ASRUtils::extract_value(
                    ASRUtils::expr_value(ab->m_dim), dim)) {
                return false;
            }
            if (dim < 1 || (size_t)dim > arr->n_dims) return false;
            const ASR::dimension_t &d = arr->m_dims[dim - 1];
            return d.m_length != nullptr && invariant(d.m_start)
                && invariant(d.m_length);
        }
        case ASR::exprType::Var: {
            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v);
            if (s == nullptr || !ASR::is_a<ASR::Variable_t>(*s)
                    || written.count(s) > 0) {
                return false;
            }
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
            if (!ASR::is_a<ASR::Integer_t>(*v->m_type) || v->m_target_attr
                    || v->m_is_volatile || v->m_common_block != nullptr) {
                return false;
            }
            if (v->m_storage == ASR::storage_typeType::Parameter
                    || private_scalars.count(s) > 0) {
                return true;
            }
            if (v->m_intent == ASR::intentType::In || v->m_value_attr) {
                return true;
            }
            if (v->m_intent != ASR::intentType::Local
                    || v->m_storage != ASR::storage_typeType::Default) {
                return false;
            }
            // A local of a module or a program is reachable from any
            // routine the loop calls.
            ASR::asr_t *owner = v->m_parent_symtab->asr_owner;
            if (owner == nullptr || !ASR::is_a<ASR::symbol_t>(*owner)) {
                return false;
            }
            ASR::symbol_t *scope = ASR::down_cast<ASR::symbol_t>(owner);
            return ASR::is_a<ASR::Function_t>(*scope)
                || ASR::is_a<ASR::Block_t>(*scope);
        }
        default:
            return false;
    }
}

// The sign of the step `e` of a counted loop when it is evident without
// running the loop: 1 or -1, or 0 when it is not. `positive` holds the
// scalars that are positive by construction, such as the lanes of an
// iteration (gpu_kernel_layout.lanes, see gpu_vector_lanes.cpp). Products,
// sums and differences of terms of known sign keep it: the step
// `step * lanes` that gpu_vector_lanes gives a distributed loop has the
// sign of `step`.
inline int loop_step_sign(ASR::expr_t *e,
        const std::set<ASR::symbol_t*> &positive) {
    int64_t value;
    if (ASRUtils::extract_value(ASRUtils::expr_value(e), value)) {
        return value > 0 ? 1 : (value < 0 ? -1 : 0);
    }
    switch (e->type) {
        case ASR::exprType::Var: {
            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v);
            return positive.count(s) > 0 ? 1 : 0;
        }
        case ASR::exprType::Cast: {
            ASR::Cast_t *c = ASR::down_cast<ASR::Cast_t>(e);
            return c->m_kind == ASR::cast_kindType::IntegerToInteger
                ? loop_step_sign(c->m_arg, positive) : 0;
        }
        case ASR::exprType::IntegerUnaryMinus:
            return -loop_step_sign(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg, positive);
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t *op = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            int l = loop_step_sign(op->m_left, positive);
            int r = loop_step_sign(op->m_right, positive);
            switch (op->m_op) {
                case ASR::binopType::Mul:
                    return l * r;
                case ASR::binopType::Add:
                    return l == r ? l : 0;
                case ASR::binopType::Sub:
                    return l == -r ? l : 0;
                default:
                    return 0;
            }
        }
        default:
            return 0;
    }
}

// The variables that the statements of a loop body may modify, as
// `written` of loop_control_invariant. A variable counts as modified
// wherever the walk does not know that it is only read: it knows
// assignments, calls (an actual argument bound to an `intent(in)` or
// by-value dummy is read), the subscripts of a designator, and the
// statements that only evaluate expressions. `calls` is set when the body
// calls a procedure, which may modify a variable of its host or of a
// module without naming it.
class LoopModifiedVariables
        : public ASR::BaseWalkVisitor<LoopModifiedVariables> {
private:
    bool reading = false;

    void walk(ASR::expr_t *e, bool read) {
        if (e == nullptr) return;
        bool outer = reading;
        reading = read;
        visit_expr(*e);
        reading = outer;
    }

    void visit_call_args(ASR::symbol_t *name, ASR::call_arg_t *args,
            size_t n_args) {
        calls = true;
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(name);
        ASR::Function_t *f = ASR::is_a<ASR::Function_t>(*s)
                || ASR::is_a<ASR::Variable_t>(*s)
                || ASR::is_a<ASR::StructMethodDeclaration_t>(*s)
            ? ASRUtils::get_function(s) : nullptr;
        for (size_t i = 0; i < n_args; i++) {
            bool read = false;
            if (f != nullptr && i < f->n_args
                    && ASR::is_a<ASR::Var_t>(*f->m_args[i])) {
                ASR::symbol_t *d = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v);
                if (ASR::is_a<ASR::Variable_t>(*d)) {
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(d);
                    read = v->m_intent == ASR::intentType::In
                        || v->m_value_attr;
                }
            }
            walk(args[i].m_value, read);
        }
    }

public:
    std::set<ASR::symbol_t*> modified;
    bool calls = false;

    void read(ASR::expr_t *e) { walk(e, true); }

    void write(ASR::expr_t *e) { walk(e, false); }

    void visit_Var(const ASR::Var_t &x) {
        if (!reading) {
            modified.insert(ASRUtils::symbol_get_past_external(x.m_v));
        }
    }

    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        visit_expr(*x.m_v);
        for (size_t i = 0; i < x.n_args; i++) {
            read(x.m_args[i].m_left);
            read(x.m_args[i].m_right);
            read(x.m_args[i].m_step);
        }
    }

    void visit_ArraySection(const ASR::ArraySection_t &x) {
        visit_expr(*x.m_v);
        for (size_t i = 0; i < x.n_args; i++) {
            read(x.m_args[i].m_left);
            read(x.m_args[i].m_right);
            read(x.m_args[i].m_step);
        }
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        write(x.m_dt);
        visit_call_args(x.m_name, x.m_args, x.n_args);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        write(x.m_dt);
        visit_call_args(x.m_name, x.m_args, x.n_args);
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        write(x.m_target);
        read(x.m_value);
        if (x.m_overloaded != nullptr) visit_stmt(*x.m_overloaded);
    }

    void visit_If(const ASR::If_t &x) {
        read(x.m_test);
        for (size_t i = 0; i < x.n_body; i++) visit_stmt(*x.m_body[i]);
        for (size_t i = 0; i < x.n_orelse; i++) visit_stmt(*x.m_orelse[i]);
    }

    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        read(x.m_test);
        for (size_t i = 0; i < x.n_body; i++) visit_stmt(*x.m_body[i]);
        for (size_t i = 0; i < x.n_orelse; i++) visit_stmt(*x.m_orelse[i]);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        write(x.m_head.m_v);
        read(x.m_head.m_start);
        read(x.m_head.m_end);
        read(x.m_head.m_increment);
        for (size_t i = 0; i < x.n_body; i++) visit_stmt(*x.m_body[i]);
        for (size_t i = 0; i < x.n_orelse; i++) visit_stmt(*x.m_orelse[i]);
    }

    void visit_Print(const ASR::Print_t &x) { read(x.m_text); }

    void visit_ErrorStop(const ASR::ErrorStop_t &x) { read(x.m_code); }

    void visit_Stop(const ASR::Stop_t &x) { read(x.m_code); }
};

} // namespace LCompilers::CUtils

#endif // LIBASR_CODEGEN_C_LOOP_CONTROL_H

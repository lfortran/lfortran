// Liric backend: ASR -> native object file via liric's C session API.
//
// This backend emits machine code directly from ASR without going through
// LLVM IR text.  It uses liric's direct-mode API to build functions,
// blocks, and instructions in a single forward pass over the ASR tree.

#include <libasr/codegen/asr_to_liric.h>
#include <libasr/config.h>

#ifdef HAVE_LFORTRAN_LIRIC

#include <liric/liric_session.h>
#include <liric/liric_types.h>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/exception.h>

#include <string>
#include <unordered_map>
#include <vector>

namespace LCompilers {

namespace {

// Local exception (same pattern as asr_to_x86.cpp)
class CodeGenError {
public:
    diag::Diagnostic d;
    CodeGenError(const std::string &msg)
        : d{diag::Diagnostic(msg, diag::Level::Error, diag::Stage::CodeGen)}
    { }
};

using ASR::down_cast;
using ASR::is_a;

static inline uint64_t get_hash(ASR::asr_t *node) {
    return (uint64_t)node;
}

// Shorthand: wrap vreg in operand descriptor
#define V(v, t) LR_VREG((v), (t))

// Shorthand: integer immediate
#define I(v, t) LR_IMM((v), (t))

// Shorthand: float immediate
#define F(v, t) LR_IMM_F((v), (t))

// --- Macros: eliminate visitor boilerplate ---

// If compile-time value exists, use it and return early
#define LIRIC_PASSTHROUGH(x) \
    if ((x).m_value) { visit_expr(*(x).m_value); return; }

// Integer binary operation
#define LIRIC_BINOP_INT(x, div_fn) do { \
    LIRIC_PASSTHROUGH(x) \
    visit_expr(*(x).m_left); uint32_t _l = tmp; \
    visit_expr(*(x).m_right); uint32_t _r = tmp; \
    lr_type_t *_t = get_type((x).m_type); \
    switch ((x).m_op) { \
        case ASR::binopType::Add: tmp = lr_emit_add(s, _t, V(_l,_t), V(_r,_t)); break; \
        case ASR::binopType::Sub: tmp = lr_emit_sub(s, _t, V(_l,_t), V(_r,_t)); break; \
        case ASR::binopType::Mul: tmp = lr_emit_mul(s, _t, V(_l,_t), V(_r,_t)); break; \
        case ASR::binopType::Div: tmp = div_fn(s, _t, V(_l,_t), V(_r,_t)); break; \
        default: throw CodeGenError("liric: unsupported int binop"); \
    } \
} while(0)

// Real binary operation
#define LIRIC_BINOP_REAL(x) do { \
    LIRIC_PASSTHROUGH(x) \
    visit_expr(*(x).m_left); uint32_t _l = tmp; \
    visit_expr(*(x).m_right); uint32_t _r = tmp; \
    lr_type_t *_t = get_type((x).m_type); \
    switch ((x).m_op) { \
        case ASR::binopType::Add: tmp = lr_emit_fadd(s, _t, V(_l,_t), V(_r,_t)); break; \
        case ASR::binopType::Sub: tmp = lr_emit_fsub(s, _t, V(_l,_t), V(_r,_t)); break; \
        case ASR::binopType::Mul: tmp = lr_emit_fmul(s, _t, V(_l,_t), V(_r,_t)); break; \
        case ASR::binopType::Div: tmp = lr_emit_fdiv(s, _t, V(_l,_t), V(_r,_t)); break; \
        default: throw CodeGenError("liric: unsupported real binop"); \
    } \
} while(0)

// Integer constant: materialize as add(imm, 0)
#define LIRIC_CONST_INT(x) do { \
    lr_type_t *_t = get_type((x).m_type); \
    tmp = lr_emit_add(s, _t, I((x).m_n, _t), I(0, _t)); \
} while(0)

// Unary minus
#define LIRIC_UNARY_INT(x) do { \
    LIRIC_PASSTHROUGH(x) \
    visit_expr(*(x).m_arg); \
    lr_type_t *_t = get_type((x).m_type); \
    tmp = lr_emit_neg(s, _t, V(tmp, _t)); \
} while(0)

#define LIRIC_UNARY_REAL(x) do { \
    LIRIC_PASSTHROUGH(x) \
    visit_expr(*(x).m_arg); \
    lr_type_t *_t = get_type((x).m_type); \
    tmp = lr_emit_fneg(s, _t, V(tmp, _t)); \
} while(0)

// Integer comparison
#define LIRIC_CMP_INT(x) do { \
    LIRIC_PASSTHROUGH(x) \
    visit_expr(*(x).m_left); uint32_t _l = tmp; \
    visit_expr(*(x).m_right); uint32_t _r = tmp; \
    lr_type_t *_t = get_type(ASRUtils::expr_type((x).m_left)); \
    int _p; \
    switch ((x).m_op) { \
        case ASR::cmpopType::Eq:   _p = LR_CMP_EQ;  break; \
        case ASR::cmpopType::NotEq:_p = LR_CMP_NE;  break; \
        case ASR::cmpopType::Lt:   _p = LR_CMP_SLT; break; \
        case ASR::cmpopType::LtE:  _p = LR_CMP_SLE; break; \
        case ASR::cmpopType::Gt:   _p = LR_CMP_SGT; break; \
        case ASR::cmpopType::GtE:  _p = LR_CMP_SGE; break; \
    } \
    tmp = lr_emit_icmp(s, _p, V(_l,_t), V(_r,_t)); \
} while(0)

// Real comparison
#define LIRIC_CMP_REAL(x) do { \
    LIRIC_PASSTHROUGH(x) \
    visit_expr(*(x).m_left); uint32_t _l = tmp; \
    visit_expr(*(x).m_right); uint32_t _r = tmp; \
    lr_type_t *_t = get_type(ASRUtils::expr_type((x).m_left)); \
    int _p; \
    switch ((x).m_op) { \
        case ASR::cmpopType::Eq:   _p = LR_FCMP_OEQ; break; \
        case ASR::cmpopType::NotEq:_p = LR_FCMP_ONE; break; \
        case ASR::cmpopType::Lt:   _p = LR_FCMP_OLT; break; \
        case ASR::cmpopType::LtE:  _p = LR_FCMP_OLE; break; \
        case ASR::cmpopType::Gt:   _p = LR_FCMP_OGT; break; \
        case ASR::cmpopType::GtE:  _p = LR_FCMP_OGE; break; \
    } \
    tmp = lr_emit_fcmp(s, _p, V(_l,_t), V(_r,_t)); \
} while(0)


class ASRToLiricVisitor : public ASR::BaseVisitor<ASRToLiricVisitor> {
public:
    lr_session_t *s;
    uint32_t tmp;               // current expression result vreg
    bool is_target;             // true when visiting assignment LHS
    uint32_t proc_return;       // return block for current function
    Allocator &al;
    CompilerOptions &co;
    diag::Diagnostics &diag;
    std::unordered_map<uint64_t, uint32_t> lr_symtab;
    std::vector<uint32_t> loop_head_stack;
    std::vector<uint32_t> loop_end_stack;

    // Cached types
    lr_type_t *ty_void, *ty_i1, *ty_i8, *ty_i16, *ty_i32, *ty_i64;
    lr_type_t *ty_f32, *ty_f64, *ty_ptr;

    ASRToLiricVisitor(lr_session_t *session, Allocator &al_,
                      CompilerOptions &co_, diag::Diagnostics &d)
        : s(session), tmp(0), is_target(false), proc_return(0),
          al(al_), co(co_), diag(d)
    {
        ty_void = lr_type_void_s(s);
        ty_i1   = lr_type_i1_s(s);
        ty_i8   = lr_type_i8_s(s);
        ty_i16  = lr_type_i16_s(s);
        ty_i32  = lr_type_i32_s(s);
        ty_i64  = lr_type_i64_s(s);
        ty_f32  = lr_type_f32_s(s);
        ty_f64  = lr_type_f64_s(s);
        ty_ptr  = lr_type_ptr_s(s);
    }

    // --- Type mapping: ASR type -> liric type ---

    lr_type_t *get_type(ASR::ttype_t *t) {
        t = ASRUtils::type_get_past_array(
                ASRUtils::type_get_past_allocatable(t));
        switch (t->type) {
            case ASR::ttypeType::Integer: {
                int kind = ASRUtils::extract_kind_from_ttype_t(t);
                switch (kind) {
                    case 1: return ty_i8;
                    case 2: return ty_i16;
                    case 4: return ty_i32;
                    case 8: return ty_i64;
                    default: throw CodeGenError("liric: unsupported integer kind");
                }
            }
            case ASR::ttypeType::UnsignedInteger: {
                int kind = ASRUtils::extract_kind_from_ttype_t(t);
                switch (kind) {
                    case 1: return ty_i8;
                    case 2: return ty_i16;
                    case 4: return ty_i32;
                    case 8: return ty_i64;
                    default: throw CodeGenError("liric: unsupported unsigned integer kind");
                }
            }
            case ASR::ttypeType::Real: {
                int kind = ASRUtils::extract_kind_from_ttype_t(t);
                switch (kind) {
                    case 4: return ty_f32;
                    case 8: return ty_f64;
                    default: throw CodeGenError("liric: unsupported real kind");
                }
            }
            case ASR::ttypeType::Logical:
                return ty_i1;
            case ASR::ttypeType::String:
                return ty_ptr;
            default:
                throw CodeGenError("liric: unsupported type");
        }
    }

    // --- Declare an external runtime function (idempotent) ---

    void declare_func(const char *name, lr_type_t *ret,
                      lr_type_t **params, uint32_t n, bool vararg) {
        lr_error_t err;
        lr_session_declare(s, name, ret, params, n, vararg, &err);
    }

    // --- Emit a call to a named external function ---
    //
    // All runtime functions use the platform ABI, so we set
    // call_external_abi to ensure correct register/stack layout.

    uint32_t emit_call(const char *name, lr_type_t *ret,
                       lr_operand_desc_t *args, uint32_t nargs) {
        uint32_t sym = lr_session_intern(s, name);
        lr_inst_desc_t d;
        memset(&d, 0, sizeof(d));
        uint32_t nops = 1 + nargs;
        lr_operand_desc_t ops[32];
        if (nops > 32) throw CodeGenError("liric: too many call args");
        ops[0] = LR_GLOBAL(sym, ty_ptr);
        for (uint32_t i = 0; i < nargs; i++) ops[1 + i] = args[i];
        d.op = LR_OP_CALL;
        d.type = ret;
        d.operands = ops;
        d.num_operands = nops;
        d.call_external_abi = true;
        return lr_session_emit(s, &d, nullptr);
    }

    void emit_call_void(const char *name,
                        lr_operand_desc_t *args, uint32_t nargs) {
        uint32_t sym = lr_session_intern(s, name);
        lr_inst_desc_t d;
        memset(&d, 0, sizeof(d));
        uint32_t nops = 1 + nargs;
        lr_operand_desc_t ops[32];
        if (nops > 32) throw CodeGenError("liric: too many call args");
        ops[0] = LR_GLOBAL(sym, ty_ptr);
        for (uint32_t i = 0; i < nargs; i++) ops[1 + i] = args[i];
        d.op = LR_OP_CALL;
        d.type = ty_void;
        d.operands = ops;
        d.num_operands = nops;
        d.call_external_abi = true;
        lr_session_emit(s, &d, nullptr);
    }

    // --- One-liner visitors via macros ---

    void visit_IntegerBinOp(const ASR::IntegerBinOp_t &x) {
        LIRIC_BINOP_INT(x, lr_emit_sdiv);
    }
    void visit_UnsignedIntegerBinOp(const ASR::UnsignedIntegerBinOp_t &x) {
        LIRIC_BINOP_INT(x, lr_emit_udiv);
    }
    void visit_RealBinOp(const ASR::RealBinOp_t &x) {
        LIRIC_BINOP_REAL(x);
    }
    void visit_IntegerConstant(const ASR::IntegerConstant_t &x) {
        LIRIC_CONST_INT(x);
    }
    void visit_UnsignedIntegerConstant(const ASR::UnsignedIntegerConstant_t &x) {
        LIRIC_CONST_INT(x);
    }
    void visit_IntegerUnaryMinus(const ASR::IntegerUnaryMinus_t &x) {
        LIRIC_UNARY_INT(x);
    }
    void visit_RealUnaryMinus(const ASR::RealUnaryMinus_t &x) {
        LIRIC_UNARY_REAL(x);
    }
    void visit_IntegerCompare(const ASR::IntegerCompare_t &x) {
        LIRIC_CMP_INT(x);
    }
    void visit_UnsignedIntegerCompare(const ASR::UnsignedIntegerCompare_t &x) {
        LIRIC_CMP_INT(x);
    }
    void visit_RealCompare(const ASR::RealCompare_t &x) {
        LIRIC_CMP_REAL(x);
    }

    // --- RealConstant ---

    void visit_RealConstant(const ASR::RealConstant_t &x) {
        lr_type_t *t = get_type(x.m_type);
        // Materialize via fadd(imm, 0.0) so liric sees a concrete vreg
        tmp = lr_emit_fadd(s, t, F(x.m_r, t), F(0.0, t));
    }

    // --- LogicalConstant ---

    void visit_LogicalConstant(const ASR::LogicalConstant_t &x) {
        tmp = lr_emit_add(s, ty_i1, I(x.m_value ? 1 : 0, ty_i1), I(0, ty_i1));
    }

    // --- LogicalBinOp ---

    void visit_LogicalBinOp(const ASR::LogicalBinOp_t &x) {
        LIRIC_PASSTHROUGH(x)
        visit_expr(*x.m_left); uint32_t l = tmp;
        visit_expr(*x.m_right); uint32_t r = tmp;
        switch (x.m_op) {
            case ASR::logicalbinopType::And:
                tmp = lr_emit_and(s, ty_i1, V(l, ty_i1), V(r, ty_i1));
                break;
            case ASR::logicalbinopType::Or:
                tmp = lr_emit_or(s, ty_i1, V(l, ty_i1), V(r, ty_i1));
                break;
            case ASR::logicalbinopType::Xor:
            case ASR::logicalbinopType::NEqv:
                tmp = lr_emit_xor(s, ty_i1, V(l, ty_i1), V(r, ty_i1));
                break;
            case ASR::logicalbinopType::Eqv:
                tmp = lr_emit_xor(s, ty_i1, V(l, ty_i1), V(r, ty_i1));
                tmp = lr_emit_xor(s, ty_i1, V(tmp, ty_i1), I(1, ty_i1));
                break;
        }
    }

    // --- LogicalNot ---

    void visit_LogicalNot(const ASR::LogicalNot_t &x) {
        LIRIC_PASSTHROUGH(x)
        visit_expr(*x.m_arg);
        tmp = lr_emit_xor(s, ty_i1, V(tmp, ty_i1), I(1, ty_i1));
    }

    // --- TranslationUnit ---

    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        // Declare runtime functions used by the generated code
        {
            lr_type_t *p0[] = {};
            declare_func("_lfortran_get_default_allocator", ty_ptr, p0, 0, false);
            declare_func("_lfortran_internal_alloc_finalize", ty_void, p0, 0, false);
        }
        {
            lr_type_t *p[] = {ty_i32, ty_ptr};
            declare_func("_lpython_call_initial_functions", ty_void, p, 2, false);
        }
        {
            // _lcompilers_string_format_fortran(alloc, sep, sep_len,
            //     serial_info, out_len, kind, a, b, c, ...)
            lr_type_t *p[] = {ty_ptr, ty_ptr, ty_i64, ty_ptr, ty_ptr,
                              ty_i32, ty_i32, ty_i32, ty_i32};
            declare_func("_lcompilers_string_format_fortran", ty_ptr, p, 9, true);
        }
        {
            // _lfortran_printf(fmt, str, str_len, end, end_len)
            lr_type_t *p[] = {ty_ptr, ty_ptr, ty_i32, ty_ptr, ty_i32};
            declare_func("_lfortran_printf", ty_void, p, 5, false);
        }
        {
            lr_type_t *p[] = {ty_ptr, ty_ptr};
            declare_func("_lfortran_free_alloc", ty_void, p, 2, false);
        }
        {
            lr_type_t *p[] = {ty_i32};
            declare_func("exit", ty_void, p, 1, false);
        }

        // Visit all symbols
        for (auto &item : x.m_symtab->get_scope()) {
            visit_symbol(*item.second);
        }
    }

    // --- Module ---

    void visit_Module(const ASR::Module_t &x) {
        if (x.m_intrinsic) return;
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Function_t>(*item.second)) {
                visit_symbol(*item.second);
            }
        }
    }

    // --- Program ---

    void visit_Program(const ASR::Program_t &x) {
        // Visit nested functions first
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Function_t>(*item.second)) {
                visit_symbol(*item.second);
            }
        }

        // Create main(argc, argv) -> i32
        lr_type_t *main_params[] = {ty_i32, ty_ptr};
        lr_error_t err;
        lr_session_func_begin(s, "main", ty_i32, main_params, 2, false, &err);

        uint32_t entry_block = lr_session_block(s);
        proc_return = lr_session_block(s);
        lr_session_set_block(s, entry_block, &err);

        uint32_t argc = lr_session_param(s, 0);
        uint32_t argv = lr_session_param(s, 1);

        // Call _lfortran_get_default_allocator
        uint32_t allocator = emit_call("_lfortran_get_default_allocator",
                                        ty_ptr, nullptr, 0);
        lr_symtab[0] = allocator; // stash allocator at key 0

        // Call _lpython_call_initial_functions(argc, argv)
        lr_operand_desc_t init_args[] = {V(argc, ty_i32), V(argv, ty_ptr)};
        emit_call_void("_lpython_call_initial_functions", init_args, 2);

        // Allocate local variables
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *v = down_cast<ASR::Variable_t>(item.second);
                lr_type_t *vt = get_type(v->m_type);
                uint32_t slot = lr_emit_alloca(s, vt);
                lr_symtab[get_hash((ASR::asr_t *)v)] = slot;
            }
        }

        // Visit body statements
        for (size_t i = 0; i < x.n_body; i++) {
            visit_stmt(*x.m_body[i]);
        }

        lr_emit_br(s, proc_return);

        // Return block: finalize and return 0
        lr_session_set_block(s, proc_return, &err);
        emit_call_void("_lfortran_internal_alloc_finalize", nullptr, 0);
        lr_emit_ret(s, I(0, ty_i32));

        lr_session_func_end(s, nullptr, &err);
    }

    // --- Function ---

    void visit_Function(const ASR::Function_t &x) {
        ASR::FunctionType_t *ftype = down_cast<ASR::FunctionType_t>(
            x.m_function_signature);

        // Skip interface-only functions (no body)
        if (x.n_body == 0 && !x.m_return_var) return;
        if (ftype->m_abi == ASR::abiType::Intrinsic) return;
        if (ftype->m_deftype == ASR::deftypeType::Interface) return;

        // Build parameter types
        std::vector<lr_type_t *> param_types;
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::Var_t *arg_var = down_cast<ASR::Var_t>(x.m_args[i]);
            ASR::Variable_t *v = down_cast<ASR::Variable_t>(arg_var->m_v);
            // Pass by pointer
            param_types.push_back(ty_ptr);
            (void)v;
        }

        lr_type_t *ret_type = ty_void;
        if (x.m_return_var) {
            ret_type = get_type(ASRUtils::expr_type(x.m_return_var));
        }

        lr_error_t err;
        lr_session_func_begin(s, x.m_name, ret_type,
            param_types.data(), param_types.size(), false, &err);

        uint32_t entry_block = lr_session_block(s);
        proc_return = lr_session_block(s);
        lr_session_set_block(s, entry_block, &err);

        // Map parameters
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::Var_t *arg_var = down_cast<ASR::Var_t>(x.m_args[i]);
            ASR::Variable_t *v = down_cast<ASR::Variable_t>(arg_var->m_v);
            uint32_t p = lr_session_param(s, i);
            lr_symtab[get_hash((ASR::asr_t *)v)] = p;
        }

        // Allocate local variables
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *v = down_cast<ASR::Variable_t>(item.second);
                if (v->m_intent == ASR::intentType::Local
                    || v->m_intent == ASR::intentType::ReturnVar) {
                    lr_type_t *vt = get_type(v->m_type);
                    uint32_t slot = lr_emit_alloca(s, vt);
                    lr_symtab[get_hash((ASR::asr_t *)v)] = slot;
                }
            }
        }

        // Visit body
        for (size_t i = 0; i < x.n_body; i++) {
            visit_stmt(*x.m_body[i]);
        }

        lr_emit_br(s, proc_return);

        // Return block
        lr_session_set_block(s, proc_return, &err);
        if (x.m_return_var) {
            ASR::Var_t *rv = down_cast<ASR::Var_t>(x.m_return_var);
            ASR::Variable_t *v = down_cast<ASR::Variable_t>(rv->m_v);
            uint32_t slot = lr_symtab[get_hash((ASR::asr_t *)v)];
            lr_type_t *rt = get_type(v->m_type);
            uint32_t val = lr_emit_load(s, rt, V(slot, ty_ptr));
            lr_emit_ret(s, V(val, rt));
        } else {
            lr_emit_ret_void(s);
        }

        lr_session_func_end(s, nullptr, &err);
    }

    // --- Var ---

    void visit_Var(const ASR::Var_t &x) {
        ASR::Variable_t *v = down_cast<ASR::Variable_t>(x.m_v);
        uint32_t slot = lr_symtab[get_hash((ASR::asr_t *)v)];
        if (is_target) {
            // Return the address for store
            tmp = slot;
        } else {
            // Load the value
            lr_type_t *vt = get_type(v->m_type);
            tmp = lr_emit_load(s, vt, V(slot, ty_ptr));
        }
    }

    // --- Assignment ---

    void visit_Assignment(const ASR::Assignment_t &x) {
        visit_expr(*x.m_value);
        uint32_t rhs = tmp;
        lr_type_t *t = get_type(ASRUtils::expr_type(x.m_value));
        is_target = true;
        visit_expr(*x.m_target);
        is_target = false;
        lr_emit_store(s, V(rhs, t), V(tmp, ty_ptr));
    }

    // --- If ---

    void visit_If(const ASR::If_t &x) {
        visit_expr(*x.m_test);
        uint32_t cond = tmp;
        uint32_t then_bb = lr_session_block(s);
        uint32_t else_bb = lr_session_block(s);
        uint32_t merge_bb = lr_session_block(s);

        lr_emit_condbr(s, V(cond, ty_i1), then_bb,
                       x.n_orelse > 0 ? else_bb : merge_bb);

        lr_error_t err;
        lr_session_set_block(s, then_bb, &err);
        for (size_t i = 0; i < x.n_body; i++) visit_stmt(*x.m_body[i]);
        lr_emit_br(s, merge_bb);

        lr_session_set_block(s, else_bb, &err);
        for (size_t i = 0; i < x.n_orelse; i++) visit_stmt(*x.m_orelse[i]);
        lr_emit_br(s, merge_bb);

        lr_session_set_block(s, merge_bb, &err);
    }

    // --- DoLoop ---

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        lr_error_t err;
        ASR::do_loop_head_t h = x.m_head;

        // Evaluate loop bounds
        visit_expr(*h.m_start); uint32_t start = tmp;
        visit_expr(*h.m_end);   uint32_t end = tmp;
        uint32_t inc;
        if (h.m_increment) {
            visit_expr(*h.m_increment);
            inc = tmp;
        } else {
            inc = lr_emit_add(s, ty_i32, I(1, ty_i32), I(0, ty_i32));
        }

        lr_type_t *loop_t = get_type(ASRUtils::expr_type(h.m_v));

        // Store initial value
        is_target = true;
        visit_expr(*h.m_v);
        is_target = false;
        uint32_t loop_var_ptr = tmp;
        lr_emit_store(s, V(start, loop_t), V(loop_var_ptr, ty_ptr));

        uint32_t head_bb = lr_session_block(s);
        uint32_t body_bb = lr_session_block(s);
        uint32_t end_bb  = lr_session_block(s);

        loop_head_stack.push_back(head_bb);
        loop_end_stack.push_back(end_bb);

        lr_emit_br(s, head_bb);

        // Head: check condition
        lr_session_set_block(s, head_bb, &err);
        uint32_t cur = lr_emit_load(s, loop_t, V(loop_var_ptr, ty_ptr));

        // Determine direction: if inc > 0, check cur <= end; else cur >= end
        uint32_t cond;
        if (!h.m_increment) {
            cond = lr_emit_icmp(s, LR_CMP_SLE, V(cur, loop_t), V(end, loop_t));
        } else {
            // General case: (end - cur) XOR inc >= 0
            // Simplified: use SGT for inc > 0, SLT for inc < 0
            // For now, just use SLE (ascending loops)
            uint32_t inc_pos = lr_emit_icmp(s, LR_CMP_SGT,
                V(inc, loop_t), I(0, loop_t));
            uint32_t cond_asc = lr_emit_icmp(s, LR_CMP_SLE,
                V(cur, loop_t), V(end, loop_t));
            uint32_t cond_desc = lr_emit_icmp(s, LR_CMP_SGE,
                V(cur, loop_t), V(end, loop_t));
            cond = lr_emit_select(s, ty_i1,
                V(inc_pos, ty_i1), V(cond_asc, ty_i1), V(cond_desc, ty_i1));
        }
        lr_emit_condbr(s, V(cond, ty_i1), body_bb, end_bb);

        // Body
        lr_session_set_block(s, body_bb, &err);
        for (size_t i = 0; i < x.n_body; i++) visit_stmt(*x.m_body[i]);

        // Increment and loop back
        uint32_t cur2 = lr_emit_load(s, loop_t, V(loop_var_ptr, ty_ptr));
        uint32_t next = lr_emit_add(s, loop_t, V(cur2, loop_t), V(inc, loop_t));
        lr_emit_store(s, V(next, loop_t), V(loop_var_ptr, ty_ptr));
        lr_emit_br(s, head_bb);

        lr_session_set_block(s, end_bb, &err);
        loop_head_stack.pop_back();
        loop_end_stack.pop_back();
    }

    // --- WhileLoop ---

    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        lr_error_t err;
        uint32_t head_bb = lr_session_block(s);
        uint32_t body_bb = lr_session_block(s);
        uint32_t end_bb  = lr_session_block(s);

        loop_head_stack.push_back(head_bb);
        loop_end_stack.push_back(end_bb);

        lr_emit_br(s, head_bb);

        lr_session_set_block(s, head_bb, &err);
        visit_expr(*x.m_test);
        lr_emit_condbr(s, V(tmp, ty_i1), body_bb, end_bb);

        lr_session_set_block(s, body_bb, &err);
        for (size_t i = 0; i < x.n_body; i++) visit_stmt(*x.m_body[i]);
        lr_emit_br(s, head_bb);

        lr_session_set_block(s, end_bb, &err);
        loop_head_stack.pop_back();
        loop_end_stack.pop_back();
    }

    // --- Control flow: Return, Stop, ErrorStop, Exit, Cycle ---

    void visit_Return(const ASR::Return_t &) {
        lr_emit_br(s, proc_return);
    }

    void visit_Stop(const ASR::Stop_t &) {
        lr_emit_br(s, proc_return);
    }

    void visit_ErrorStop(const ASR::ErrorStop_t &) {
        lr_operand_desc_t args[] = {I(1, ty_i32)};
        emit_call_void("exit", args, 1);
        lr_emit_unreachable(s);
    }

    void visit_Exit(const ASR::Exit_t &) {
        lr_emit_br(s, loop_end_stack.back());
    }

    void visit_Cycle(const ASR::Cycle_t &) {
        lr_emit_br(s, loop_head_stack.back());
    }

    // --- SubroutineCall ---

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::Function_t *fn = down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(x.m_name));

        std::vector<lr_operand_desc_t> args;
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i].m_value) {
                visit_expr(*x.m_args[i].m_value);
                // Pass by pointer: alloca + store
                lr_type_t *at = get_type(ASRUtils::expr_type(x.m_args[i].m_value));
                uint32_t slot = lr_emit_alloca(s, at);
                lr_emit_store(s, V(tmp, at), V(slot, ty_ptr));
                args.push_back(V(slot, ty_ptr));
            } else {
                args.push_back(LR_NULL(ty_ptr));
            }
        }

        uint32_t sym = lr_session_intern(s, fn->m_name);
        lr_emit_call_void(s, LR_GLOBAL(sym, ty_ptr),
                          args.data(), args.size());
    }

    // --- FunctionCall ---

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        if (x.m_value) { visit_expr(*x.m_value); return; }

        ASR::Function_t *fn = down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(x.m_name));

        std::vector<lr_operand_desc_t> args;
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i].m_value) {
                visit_expr(*x.m_args[i].m_value);
                lr_type_t *at = get_type(ASRUtils::expr_type(x.m_args[i].m_value));
                uint32_t slot = lr_emit_alloca(s, at);
                lr_emit_store(s, V(tmp, at), V(slot, ty_ptr));
                args.push_back(V(slot, ty_ptr));
            } else {
                args.push_back(LR_NULL(ty_ptr));
            }
        }

        lr_type_t *ret = get_type(x.m_type);
        uint32_t sym = lr_session_intern(s, fn->m_name);
        tmp = lr_emit_call(s, ret, LR_GLOBAL(sym, ty_ptr),
                           args.data(), args.size());
    }

    // --- Cast ---

    void visit_Cast(const ASR::Cast_t &x) {
        if (x.m_value) { visit_expr(*x.m_value); return; }
        visit_expr(*x.m_arg);
        uint32_t val = tmp;
        lr_type_t *src_t = get_type(ASRUtils::expr_type(x.m_arg));
        lr_type_t *dst_t = get_type(x.m_type);

        switch (x.m_kind) {
            case ASR::cast_kindType::IntegerToReal:
                tmp = lr_emit_sitofp(s, dst_t, V(val, src_t));
                break;
            case ASR::cast_kindType::RealToInteger:
                tmp = lr_emit_fptosi(s, dst_t, V(val, src_t));
                break;
            case ASR::cast_kindType::IntegerToInteger: {
                unsigned sw = lr_type_width(s, src_t);
                unsigned dw = lr_type_width(s, dst_t);
                if (dw > sw)
                    tmp = lr_emit_sext(s, dst_t, V(val, src_t));
                else if (dw < sw)
                    tmp = lr_emit_trunc(s, dst_t, V(val, src_t));
                break;
            }
            case ASR::cast_kindType::RealToReal: {
                if (src_t == ty_f32 && dst_t == ty_f64)
                    tmp = lr_emit_fpext(s, dst_t, V(val, src_t));
                else if (src_t == ty_f64 && dst_t == ty_f32)
                    tmp = lr_emit_fptrunc(s, dst_t, V(val, src_t));
                break;
            }
            case ASR::cast_kindType::IntegerToLogical:
                tmp = lr_emit_icmp(s, LR_CMP_NE, V(val, src_t), I(0, src_t));
                break;
            case ASR::cast_kindType::LogicalToInteger:
                tmp = lr_emit_zext(s, dst_t, V(val, src_t));
                break;
            default:
                throw CodeGenError("liric: unsupported cast kind");
        }
    }

    // --- Print ---
    //
    // Matches the LLVM backend: format integer args via
    // _lcompilers_string_format_fortran, then print via _lfortran_printf.

    void visit_Print(const ASR::Print_t &x) {
        if (!x.m_text) return;

        ASR::expr_t *text = x.m_text;

        // Print always wraps content in StringFormat
        if (!is_a<ASR::StringFormat_t>(*text)) {
            throw CodeGenError("liric: Print without StringFormat");
        }
        ASR::StringFormat_t &sf = *down_cast<ASR::StringFormat_t>(text);

        // Get allocator
        uint32_t allocator = lr_symtab[0];

        // Build serialization info string for each arg: "I4", "I8", "R4", etc.
        std::string serial;
        for (size_t i = 0; i < sf.n_args; i++) {
            ASR::ttype_t *at = ASRUtils::expr_type(sf.m_args[i]);
            at = ASRUtils::type_get_past_array(
                    ASRUtils::type_get_past_allocatable(at));
            switch (at->type) {
                case ASR::ttypeType::Integer:
                    serial += "I" + std::to_string(
                        ASRUtils::extract_kind_from_ttype_t(at));
                    break;
                case ASR::ttypeType::Real:
                    serial += "R" + std::to_string(
                        ASRUtils::extract_kind_from_ttype_t(at));
                    break;
                case ASR::ttypeType::Logical:
                    serial += "L4";
                    break;
                default:
                    throw CodeGenError("liric: unsupported print arg type");
            }
        }

        // Create global constants for print data.
        // lr_session_global creates data; lr_session_intern maps the name
        // to a symbol ID usable in LR_GLOBAL operands.
        std::string hash = std::to_string(get_hash((ASR::asr_t *)&x));

        std::string serial_z = serial + '\0';
        std::string serial_name = "_lr_serial_" + hash;
        lr_session_global(s, serial_name.c_str(),
            lr_type_array_s(s, ty_i8, serial_z.size()),
            true, serial_z.data(), serial_z.size());
        uint32_t serial_sym = lr_session_intern(s, serial_name.c_str());

        const char nl_data[] = "\n";
        std::string nl_name = "_lr_nl_" + hash;
        lr_session_global(s, nl_name.c_str(),
            lr_type_array_s(s, ty_i8, 2),
            true, nl_data, 2);
        uint32_t nl_sym = lr_session_intern(s, nl_name.c_str());

        const char fmt_data[] = "%s%s";
        std::string fmt_name = "_lr_fmt_" + hash;
        lr_session_global(s, fmt_name.c_str(),
            lr_type_array_s(s, ty_i8, 5),
            true, fmt_data, 5);
        uint32_t fmt_sym = lr_session_intern(s, fmt_name.c_str());

        // Evaluate all args and store to alloca slots (pass by pointer)
        std::vector<uint32_t> arg_slots;
        for (size_t i = 0; i < sf.n_args; i++) {
            visit_expr(*sf.m_args[i]);
            lr_type_t *at = get_type(ASRUtils::expr_type(sf.m_args[i]));
            uint32_t slot = lr_emit_alloca(s, at);
            lr_emit_store(s, V(tmp, at), V(slot, ty_ptr));
            arg_slots.push_back(slot);
        }

        // Alloca for output length
        uint32_t out_len_ptr = lr_emit_alloca(s, ty_i64);

        // Build call args for _lcompilers_string_format_fortran
        // Fixed args: alloc, sep(null), sep_len(0), serial_info, out_len,
        //             kind(0), a(0), b(0), c(0)
        // Variadic: pointers to each formatted arg
        std::vector<lr_operand_desc_t> call_args;
        call_args.push_back(V(allocator, ty_ptr));                     // alloc
        call_args.push_back(LR_NULL(ty_ptr));                          // sep
        call_args.push_back(I(0, ty_i64));                             // sep_len
        call_args.push_back(LR_GLOBAL(serial_sym, ty_ptr));           // serial_info
        call_args.push_back(V(out_len_ptr, ty_ptr));                   // out_len
        call_args.push_back(I(0, ty_i32));                             // kind
        call_args.push_back(I(0, ty_i32));                             // a
        call_args.push_back(I(0, ty_i32));                             // b
        call_args.push_back(I(0, ty_i32));                             // c
        for (size_t i = 0; i < arg_slots.size(); i++) {
            call_args.push_back(V(arg_slots[i], ty_ptr));
        }

        // Set up the call as vararg with fixed_args=9
        uint32_t strfmt_sym = lr_session_intern(s,
            "_lcompilers_string_format_fortran");
        {
            lr_inst_desc_t d;
            memset(&d, 0, sizeof(d));
            uint32_t nops = 1 + call_args.size();
            std::vector<lr_operand_desc_t> ops(nops);
            ops[0] = LR_GLOBAL(strfmt_sym, ty_ptr);
            for (size_t i = 0; i < call_args.size(); i++) {
                ops[1 + i] = call_args[i];
            }
            d.op = LR_OP_CALL;
            d.type = ty_ptr;
            d.operands = ops.data();
            d.num_operands = nops;
            d.call_external_abi = true;
            d.call_vararg = true;
            d.call_fixed_args = 9;
            tmp = lr_session_emit(s, &d, nullptr);
        }
        uint32_t str_data = tmp;

        // Load output length and truncate to i32
        uint32_t str_len64 = lr_emit_load(s, ty_i64, V(out_len_ptr, ty_ptr));
        uint32_t str_len = lr_emit_trunc(s, ty_i32, V(str_len64, ty_i64));

        // Call _lfortran_printf(fmt, str_data, str_len, "\n", 1)
        lr_operand_desc_t printf_args[] = {
            LR_GLOBAL(fmt_sym, ty_ptr),
            V(str_data, ty_ptr),
            V(str_len, ty_i32),
            LR_GLOBAL(nl_sym, ty_ptr),
            I(1, ty_i32)
        };
        emit_call_void("_lfortran_printf", printf_args, 5);

        // Free the formatted string if non-null
        uint32_t is_null = lr_emit_icmp(s, LR_CMP_EQ,
            V(str_data, ty_ptr), LR_NULL(ty_ptr));

        uint32_t free_bb = lr_session_block(s);
        uint32_t done_bb = lr_session_block(s);
        lr_emit_condbr(s, V(is_null, ty_i1), done_bb, free_bb);

        lr_error_t err;
        lr_session_set_block(s, free_bb, &err);
        lr_operand_desc_t free_args[] = {
            V(allocator, ty_ptr), V(str_data, ty_ptr)};
        emit_call_void("_lfortran_free_alloc", free_args, 2);
        lr_emit_br(s, done_bb);

        lr_session_set_block(s, done_bb, &err);
    }

    // --- IntrinsicElementalFunction ---

    void visit_IntrinsicElementalFunction(
            const ASR::IntrinsicElementalFunction_t &x) {
        if (x.m_value) { visit_expr(*x.m_value); return; }
        throw CodeGenError("liric: runtime intrinsic not yet supported");
    }

    // --- StringFormat (evaluated inline by Print) ---

    void visit_StringFormat(const ASR::StringFormat_t &x) {
        if (x.m_value) { visit_expr(*x.m_value); return; }
        // StringFormat is handled by visit_Print; reaching here means
        // it appears outside Print context, which we don't support yet.
        throw CodeGenError("liric: StringFormat outside Print not supported");
    }
};

} // anonymous namespace


// --- Entry point ---

Result<int> asr_to_liric(ASR::TranslationUnit_t &asr,
    Allocator &al, const std::string &filename,
    CompilerOptions &co, diag::Diagnostics &diagnostics,
    int liric_backend)
{
    lr_session_config_t cfg;
    memset(&cfg, 0, sizeof(cfg));
    cfg.mode = LR_MODE_DIRECT;
    cfg.backend = (lr_session_backend_t)liric_backend;
    lr_error_t err;
    lr_session_t *session = lr_session_create(&cfg, &err);
    if (!session) {
        diagnostics.diagnostics.push_back(diag::Diagnostic(
            "liric: failed to create session: " + std::string(err.msg),
            diag::Level::Error, diag::Stage::CodeGen));
        return Error();
    }
    try {
        ASRToLiricVisitor v(session, al, co, diagnostics);
        v.visit_asr((ASR::asr_t &)asr);
    } catch (const CodeGenError &e) {
        lr_session_destroy(session);
        diagnostics.diagnostics.push_back(e.d);
        return Error();
    }
    if (lr_session_emit_object(session, filename.c_str(), &err) != 0) {
        lr_session_destroy(session);
        diagnostics.diagnostics.push_back(diag::Diagnostic(
            "liric: failed to emit object: " + std::string(err.msg),
            diag::Level::Error, diag::Stage::CodeGen));
        return Error();
    }
    lr_session_destroy(session);
    return 0;
}

} // namespace LCompilers

#else // !HAVE_LFORTRAN_LIRIC

namespace LCompilers {

Result<int> asr_to_liric(ASR::TranslationUnit_t &/*asr*/,
    Allocator &/*al*/, const std::string &/*filename*/,
    CompilerOptions &/*co*/, diag::Diagnostics &diagnostics,
    int /*liric_backend*/)
{
    diagnostics.diagnostics.push_back(diag::Diagnostic(
        "liric backend not enabled; rebuild with -DWITH_LIRIC=ON",
        diag::Level::Error, diag::Stage::CodeGen));
    return Error();
}

} // namespace LCompilers

#endif // HAVE_LFORTRAN_LIRIC

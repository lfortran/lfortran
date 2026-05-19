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
#include <libasr/pass/intrinsic_function_registry.h>

#include <cstring>
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
    std::unordered_map<uint64_t, lr_type_t *> struct_types;
    std::vector<uint32_t> loop_head_stack;
    std::vector<uint32_t> loop_end_stack;

    // Cached types
    lr_type_t *ty_void, *ty_i1, *ty_i8, *ty_i16, *ty_i32, *ty_i64;
    lr_type_t *ty_f32, *ty_f64, *ty_ptr;
    lr_type_t *ty_str_desc;     // Fortran string descriptor: {i8*, i64}

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
        // String descriptor mirrors the LLVM backend's character_type:
        // a 16-byte struct {data_ptr, length} passed by pointer at the
        // Fortran ABI boundary.
        {
            lr_type_t *fields[2] = {ty_ptr, ty_i64};
            ty_str_desc = lr_type_struct_s(s, fields, 2, false);
        }
    }

    // --- Type mapping: ASR type -> liric type ---

    lr_type_t *get_type(ASR::ttype_t *t) {
        t = ASRUtils::type_get_past_allocatable(t);
        if (ASR::is_a<ASR::Array_t>(*t)) {
            ASR::Array_t *at = down_cast<ASR::Array_t>(t);
            lr_type_t *et = get_type(at->m_type);
            if (at->m_physical_type
                    == ASR::array_physical_typeType::FixedSizeArray) {
                int64_t total = ASRUtils::get_fixed_size_of_array(t);
                if (total <= 0) total = 1;
                return lr_type_array_s(s, et, (uint64_t)total);
            }
            // Non-fixed arrays appear at ABI boundaries as raw data
            // pointers; descriptor support is a later chunk.
            return ty_ptr;
        }
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
                return ty_str_desc;
            case ASR::ttypeType::StructType:
                return get_struct_type(down_cast<ASR::StructType_t>(t));
            case ASR::ttypeType::Pointer:
                // Untyped at the liric layer; downstream code that
                // dereferences a Fortran pointer must supply its own
                // pointee type to load/store/gep.
                return ty_ptr;
            case ASR::ttypeType::CPtr:
                return ty_ptr;
            default:
                throw CodeGenError(std::string("liric: unsupported type kind ")
                    + std::to_string((int)t->type));
        }
    }

    // --- Map ASR StructType to a cached liric struct type ---
    //
    // ASR::StructType_t carries the ordered data-member ttypes; we mirror
    // them as a liric struct.  Keyed by the ttype node pointer because the
    // same Struct symbol can produce several StructType_t nodes for
    // polymorphic vs concrete views.

    lr_type_t *get_struct_type(ASR::StructType_t *stt) {
        uint64_t h = get_hash((ASR::asr_t *)stt);
        auto it = struct_types.find(h);
        if (it != struct_types.end()) return it->second;

        std::vector<lr_type_t *> fields;
        for (size_t i = 0; i < stt->n_data_member_types; i++) {
            fields.push_back(get_type(stt->m_data_member_types[i]));
        }
        lr_type_t *t;
        if (fields.empty()) {
            // Empty derived types are legal in Fortran; reserve one byte
            // so alloca produces a distinct address.
            lr_type_t *one[1] = {ty_i8};
            t = lr_type_struct_s(s, one, 1, false);
        } else {
            t = lr_type_struct_s(s, fields.data(), fields.size(), false);
        }
        struct_types[h] = t;
        return t;
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

    // --- DebugCheckArrayBounds: no-op (bounds_checking is off by default) ---

    void visit_DebugCheckArrayBounds(
            const ASR::DebugCheckArrayBounds_t & /*x*/) {
        // The frontend always emits this node before an array-shape
        // assignment.  When bounds_checking is enabled the LLVM backend
        // wires runtime asserts; we just drop the check for now so the
        // assignment itself still runs.
    }

    // --- BitCast ---
    //
    // Liric values are untyped at the operand layer; for the common
    // "reinterpret bytes" case we just pass the source value through.
    // Strings and arrays still throw because their descriptor layout
    // doesn't match a raw bit-cast.

    void visit_BitCast(const ASR::BitCast_t &x) {
        LIRIC_PASSTHROUGH(x)
        ASR::ttype_t *st = ASRUtils::expr_type(x.m_source);
        st = ASRUtils::type_get_past_allocatable_pointer(st);
        st = ASRUtils::type_get_past_array(st);
        if (ASR::is_a<ASR::String_t>(*st) ||
                ASR::is_a<ASR::StructType_t>(*st)) {
            throw CodeGenError(
                "liric: BitCast on string/struct not yet supported");
        }
        visit_expr(*x.m_source);
        // Source and destination scalars share the same bit pattern -
        // for the small fpm cases (transfer between ints and reals of
        // the same kind) this is a no-op at the IR level.
    }

    // --- Associate ---
    //
    // associate(name => value) - lower like an assignment to a Var
    // local that aliases `value`.  The frontend has already declared
    // `target` as a Var bound to the same storage on entry, so we just
    // store the rvalue.

    void visit_Associate(const ASR::Associate_t &x) {
        visit_expr(*x.m_value);
        uint32_t rhs = tmp;
        lr_type_t *t = get_type(ASRUtils::expr_type(x.m_value));
        is_target = true;
        visit_expr(*x.m_target);
        is_target = false;
        lr_emit_store(s, V(rhs, t), V(tmp, ty_ptr));
    }

    // --- ArrayItem (FixedSizeArray, single-dim only for now) ---
    //
    // Compute row-major linear index from the supplied dim indices and
    // GEP into the array storage.  FixedSizeArray only; other physical
    // types still throw a clear diagnostic.

    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        LIRIC_PASSTHROUGH(x)

        ASR::ttype_t *vt = ASRUtils::expr_type(x.m_v);
        vt = ASRUtils::type_get_past_allocatable_pointer(vt);
        if (!ASR::is_a<ASR::Array_t>(*vt)) {
            throw CodeGenError(
                "liric: ArrayItem owner is not an array type");
        }
        ASR::Array_t *array_t = down_cast<ASR::Array_t>(vt);
        lr_type_t *elem_type = get_type(array_t->m_type);

        if (array_t->m_physical_type
                == ASR::array_physical_typeType::FixedSizeArray) {
            lr_type_t *array_type = get_type(vt);

            bool was_target = is_target;
            is_target = true;
            visit_expr(*x.m_v);
            is_target = was_target;
            uint32_t base = tmp;

            uint32_t lin = 0;
            bool first = true;
            for (size_t r = 0; r < x.n_args; r++) {
                ASR::array_index_t &ai = x.m_args[r];
                visit_expr(*ai.m_right);
                lr_type_t *it = get_type(ASRUtils::expr_type(ai.m_right));
                uint32_t idx = (it == ty_i32)
                    ? tmp
                    : ((it == ty_i64)
                        ? lr_emit_trunc(s, ty_i32, V(tmp, it))
                        : lr_emit_sext(s, ty_i32, V(tmp, it)));
                int64_t lbound = 1;
                if (array_t->m_dims[r].m_start) {
                    ASRUtils::extract_value(
                        array_t->m_dims[r].m_start, lbound);
                }
                uint32_t off = lr_emit_sub(s, ty_i32,
                    V(idx, ty_i32), I(lbound, ty_i32));
                if (first) {
                    lin = off;
                    first = false;
                } else {
                    int64_t length = 1;
                    if (array_t->m_dims[r].m_length) {
                        ASRUtils::extract_value(
                            array_t->m_dims[r].m_length, length);
                    }
                    lin = lr_emit_mul(s, ty_i32,
                        V(lin, ty_i32), I(length, ty_i32));
                    lin = lr_emit_add(s, ty_i32,
                        V(lin, ty_i32), V(off, ty_i32));
                }
            }

            lr_operand_desc_t gep_idx[2] = {
                I(0, ty_i32), V(lin, ty_i32)
            };
            uint32_t elem_ptr = lr_emit_gep(s, array_type,
                V(base, ty_ptr), gep_idx, 2);

            if (is_target) {
                tmp = elem_ptr;
            } else {
                tmp = lr_emit_load(s, elem_type, V(elem_ptr, ty_ptr));
            }
            return;
        }

        // Descriptor-array path (incl. assumed-shape `arr(:)` params).
        // Read base_addr and per-dim {lbound, stride} from the CFI
        // descriptor, accumulate the element offset in *bytes*, GEP
        // i8* and load.
        uint32_t desc = desc_ptr_of(x.m_v);
        uint32_t base = desc_base_addr(desc);

        uint32_t byte_off = 0;
        bool first = true;
        for (size_t r = 0; r < x.n_args; r++) {
            ASR::array_index_t &ai = x.m_args[r];
            visit_expr(*ai.m_right);
            lr_type_t *it = get_type(ASRUtils::expr_type(ai.m_right));
            uint32_t idx64 = (it == ty_i64)
                ? tmp
                : ((it == ty_i32)
                    ? lr_emit_sext(s, ty_i64, V(tmp, it))
                    : lr_emit_sext(s, ty_i64, V(tmp, it)));
            uint32_t lb = desc_dim_lbound(desc, r);
            uint32_t delta = lr_emit_sub(s, ty_i64,
                V(idx64, ty_i64), V(lb, ty_i64));
            // dim[r].stride is in bytes (CFI "sm" / lfortran's stride).
            uint32_t stride = desc_load_i64(desc,
                DESC_HEADER_BYTES + DESC_DIM_BYTES * (int64_t)r + 16);
            uint32_t contrib = lr_emit_mul(s, ty_i64,
                V(delta, ty_i64), V(stride, ty_i64));
            if (first) {
                byte_off = contrib;
                first = false;
            } else {
                byte_off = lr_emit_add(s, ty_i64,
                    V(byte_off, ty_i64), V(contrib, ty_i64));
            }
        }
        if (first) {
            // 0-arg ArrayItem: shouldn't happen but be safe.
            byte_off = lr_emit_add(s, ty_i64,
                I(0, ty_i64), I(0, ty_i64));
        }

        lr_operand_desc_t gep_off[1] = {V(byte_off, ty_i64)};
        uint32_t elem_ptr = lr_emit_gep(s, ty_i8,
            V(base, ty_ptr), gep_off, 1);

        if (is_target) {
            tmp = elem_ptr;
        } else {
            tmp = lr_emit_load(s, elem_type, V(elem_ptr, ty_ptr));
        }
    }

    // --- Allocate (string allocatables only) ---
    //
    // Implements the minimal `allocate(character(len=N) :: s)` shape.
    // Calls _lfortran_malloc_alloc and writes the {data, N} descriptor
    // back to the variable's slot.  Anything else (arrays, source=,
    // stat=, mold=) is rejected with a clear diagnostic.

    void visit_Allocate(const ASR::Allocate_t &x) {
        if (x.m_source || x.m_stat || x.m_errmsg) {
            throw CodeGenError(
                "liric: allocate() source/stat/errmsg not supported yet");
        }
        for (size_t i = 0; i < x.n_args; i++) {
            const ASR::alloc_arg_t &arg = x.m_args[i];
            if (arg.n_dims > 0) {
                throw CodeGenError(
                    "liric: allocate() of arrays not supported yet");
            }
            ASR::ttype_t *at = ASRUtils::expr_type(arg.m_a);
            at = ASRUtils::type_get_past_allocatable_pointer(at);
            at = ASRUtils::type_get_past_array(at);
            if (!ASR::is_a<ASR::String_t>(*at)) {
                throw CodeGenError(
                    "liric: allocate() supports only string targets yet");
            }
            if (!arg.m_len_expr) {
                throw CodeGenError(
                    "liric: allocate() of string requires an explicit "
                    "len= expression");
            }

            bool was_target = is_target;
            is_target = true;
            visit_expr(*arg.m_a);
            is_target = was_target;
            uint32_t desc_ptr = tmp;

            visit_expr(*arg.m_len_expr);
            uint32_t len = tmp;
            lr_type_t *len_t = get_type(ASRUtils::expr_type(arg.m_len_expr));
            uint32_t len64 = (len_t == ty_i64)
                ? len
                : lr_emit_sext(s, ty_i64, V(len, len_t));

            uint32_t allocator = emit_call(
                "_lfortran_get_default_allocator", ty_ptr, nullptr, 0);

            lr_type_t *malloc_params[] = {ty_ptr, ty_i64};
            declare_func("_lfortran_malloc_alloc", ty_ptr,
                malloc_params, 2, false);
            lr_operand_desc_t malloc_args[] = {
                V(allocator, ty_ptr), V(len64, ty_i64)
            };
            uint32_t data = emit_call("_lfortran_malloc_alloc",
                ty_ptr, malloc_args, 2);

            uint32_t fld0 = 0, fld1 = 1;
            uint32_t d0 = lr_emit_insertvalue(s, ty_str_desc,
                LR_UNDEF(ty_str_desc), V(data, ty_ptr), &fld0, 1);
            uint32_t d1 = lr_emit_insertvalue(s, ty_str_desc,
                V(d0, ty_str_desc), V(len64, ty_i64), &fld1, 1);
            lr_emit_store(s, V(d1, ty_str_desc), V(desc_ptr, ty_ptr));
        }
    }

    // --- ExplicitDeallocate / ImplicitDeallocate (string-only path) ---

    void deallocate_string_var(ASR::expr_t *v) {
        ASR::ttype_t *at = ASRUtils::expr_type(v);
        at = ASRUtils::type_get_past_allocatable_pointer(at);
        at = ASRUtils::type_get_past_array(at);
        if (!ASR::is_a<ASR::String_t>(*at)) {
            // Non-string deallocate is a no-op until we have array
            // descriptor support.
            return;
        }

        bool was_target = is_target;
        is_target = true;
        visit_expr(*v);
        is_target = was_target;
        uint32_t desc_ptr = tmp;

        uint32_t desc = lr_emit_load(s, ty_str_desc, V(desc_ptr, ty_ptr));
        uint32_t fld0 = 0;
        uint32_t data = lr_emit_extractvalue(s, ty_ptr,
            V(desc, ty_str_desc), &fld0, 1);

        uint32_t allocator = emit_call(
            "_lfortran_get_default_allocator", ty_ptr, nullptr, 0);
        lr_operand_desc_t free_args[] = {
            V(allocator, ty_ptr), V(data, ty_ptr)
        };
        emit_call_void("_lfortran_free_alloc", free_args, 2);

        uint32_t fld1 = 1;
        uint32_t z0 = lr_emit_insertvalue(s, ty_str_desc,
            LR_UNDEF(ty_str_desc), LR_NULL(ty_ptr), &fld0, 1);
        uint32_t z1 = lr_emit_insertvalue(s, ty_str_desc,
            V(z0, ty_str_desc), I(0, ty_i64), &fld1, 1);
        lr_emit_store(s, V(z1, ty_str_desc), V(desc_ptr, ty_ptr));
    }

    void visit_ExplicitDeallocate(const ASR::ExplicitDeallocate_t &x) {
        for (size_t i = 0; i < x.n_vars; i++) {
            deallocate_string_var(x.m_vars[i]);
        }
    }

    void visit_ImplicitDeallocate(const ASR::ImplicitDeallocate_t &x) {
        for (size_t i = 0; i < x.n_vars; i++) {
            deallocate_string_var(x.m_vars[i]);
        }
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

        // Build serialization info string.  Format matches asr_to_llvm's
        // SerializeType (comma-separated, I<kind>, R<kind>, L<kind*8>,
        // S-DESC[-N] for strings).
        std::string serial;
        for (size_t i = 0; i < sf.n_args; i++) {
            if (i > 0) serial += ",";
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
                    serial += "L" + std::to_string(
                        ASRUtils::extract_kind_from_ttype_t(at) * 8);
                    break;
                case ASR::ttypeType::String: {
                    ASR::String_t *st = down_cast<ASR::String_t>(at);
                    serial += "S-";
                    if (st->m_physical_type == ASR::DescriptorString) {
                        serial += "DESC";
                    } else if (st->m_physical_type == ASR::CChar) {
                        serial += "CCHAR";
                    } else {
                        throw CodeGenError(
                            "liric: unsupported string physical type for print");
                    }
                    int64_t len = -1;
                    if (st->m_len && ASRUtils::extract_value(st->m_len, len)) {
                        serial += "-" + std::to_string(len);
                    }
                    break;
                }
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

    // --- IntrinsicImpureFunction ---
    //
    // Mirrors the LLVM backend's three supported cases.  Everything else
    // is left to fail with a clear diagnostic until we need it.

    void visit_IntrinsicImpureFunction(
            const ASR::IntrinsicImpureFunction_t &x) {
        switch (static_cast<ASRUtils::IntrinsicImpureFunctions>(
                x.m_impure_intrinsic_id)) {
            case ASRUtils::IntrinsicImpureFunctions::IsIostatEnd: {
                visit_expr(*x.m_args[0]);
                tmp = lr_emit_icmp(s, LR_CMP_EQ,
                    V(tmp, ty_i32), I(-1, ty_i32));
                break;
            }
            case ASRUtils::IntrinsicImpureFunctions::IsIostatEor: {
                visit_expr(*x.m_args[0]);
                tmp = lr_emit_icmp(s, LR_CMP_EQ,
                    V(tmp, ty_i32), I(-2, ty_i32));
                break;
            }
            case ASRUtils::IntrinsicImpureFunctions::Allocated: {
                ASR::expr_t *arg = x.m_args[0];
                ASR::ttype_t *at = ASRUtils::expr_type(arg);
                ASR::ttype_t *naked = ASRUtils::type_get_past_allocatable_pointer(at);
                if (ASR::is_a<ASR::Array_t>(*naked)) {
                    // Array: base_addr in descriptor != null.
                    uint32_t desc = desc_ptr_of(arg);
                    uint32_t base = desc_base_addr(desc);
                    tmp = lr_emit_icmp(s, LR_CMP_NE,
                        V(base, ty_ptr), LR_NULL(ty_ptr));
                    break;
                }
                ASR::ttype_t *core = ASRUtils::type_get_past_array(naked);
                if (!ASR::is_a<ASR::String_t>(*core)) {
                    throw CodeGenError(
                        "liric: allocated() supports only string and "
                        "array arguments at this stage");
                }
                visit_expr(*arg);
                uint32_t v = tmp;
                uint32_t fld0 = 0;
                uint32_t data = lr_emit_extractvalue(s, ty_ptr,
                    V(v, ty_str_desc), &fld0, 1);
                tmp = lr_emit_icmp(s, LR_CMP_NE,
                    V(data, ty_ptr), LR_NULL(ty_ptr));
                break;
            }
            default:
                throw CodeGenError(std::string("liric: impure intrinsic ")
                    + ASRUtils::get_impure_intrinsic_name(
                            x.m_impure_intrinsic_id)
                    + " not yet supported");
        }
    }

    // --- Iachar / Ichar ---
    //
    // Both dispatch to a runtime that takes the data pointer of the
    // single-character string and returns an i32.  Iachar may be widened
    // to i64 by callers; Ichar always returns i32.

    void visit_Iachar(const ASR::Iachar_t &x) {
        LIRIC_PASSTHROUGH(x)
        visit_expr(*x.m_arg);
        uint32_t desc = tmp;
        uint32_t idx0 = 0;
        uint32_t data = lr_emit_extractvalue(s, ty_ptr,
            V(desc, ty_str_desc), &idx0, 1);
        lr_type_t *params[] = {ty_ptr};
        declare_func("_lfortran_iachar", ty_i32, params, 1, false);
        lr_operand_desc_t args[] = {V(data, ty_ptr)};
        uint32_t r = emit_call("_lfortran_iachar", ty_i32, args, 1);
        lr_type_t *rt = get_type(x.m_type);
        tmp = (rt == ty_i32) ? r : lr_emit_sext(s, rt, V(r, ty_i32));
    }

    void visit_Ichar(const ASR::Ichar_t &x) {
        LIRIC_PASSTHROUGH(x)
        visit_expr(*x.m_arg);
        uint32_t desc = tmp;
        uint32_t idx0 = 0;
        uint32_t data = lr_emit_extractvalue(s, ty_ptr,
            V(desc, ty_str_desc), &idx0, 1);
        lr_type_t *params[] = {ty_ptr};
        declare_func("_lfortran_ichar", ty_i32, params, 1, false);
        lr_operand_desc_t args[] = {V(data, ty_ptr)};
        uint32_t r = emit_call("_lfortran_ichar", ty_i32, args, 1);
        lr_type_t *rt = get_type(x.m_type);
        tmp = (rt == ty_i32) ? r : lr_emit_sext(s, rt, V(r, ty_i32));
    }

    // --- FileWrite ---
    //
    // Minimal implementation: emit each value through _lfortran_printf,
    // separated by " " and terminated by m_end (or "\n").  Strings are
    // written via their {data,len}; integers/reals/logicals go through
    // _lcompilers_string_format_fortran so the runtime handles the
    // formatting.  Both unit==null (write(*,...)) and integer-unit
    // (write(stdout,...)) cases land in the same printf-based path
    // because lfortran's _lfortran_printf already writes to stdout.

    void file_write_emit_string(uint32_t data, uint32_t len64) {
        uint32_t fmt_sym = declare_global_cstring("%.*s", "_lr_fwfmt_pct_s");
        uint32_t len32 = lr_emit_trunc(s, ty_i32, V(len64, ty_i64));
        lr_type_t *printf_params[] = {ty_ptr};
        declare_func("printf", ty_i32, printf_params, 1, true);
        uint32_t printf_sym = lr_session_intern(s, "printf");
        lr_inst_desc_t d;
        memset(&d, 0, sizeof(d));
        lr_operand_desc_t ops[4] = {
            LR_GLOBAL(printf_sym, ty_ptr),
            LR_GLOBAL(fmt_sym, ty_ptr),
            V(len32, ty_i32),
            V(data, ty_ptr)
        };
        d.op = LR_OP_CALL;
        d.type = ty_i32;
        d.operands = ops;
        d.num_operands = 4;
        d.call_external_abi = true;
        d.call_vararg = true;
        d.call_fixed_args = 1;
        lr_session_emit(s, &d, nullptr);
    }

    // Declare/intern a private c-string and return its symbol id for
    // use in LR_GLOBAL operands.  Idempotent: identical (data,name)
    // pairs collapse to the same intern id.
    uint32_t declare_global_cstring(const char *data, const char *name_hint) {
        std::string name = std::string(name_hint) + "_" + data;
        // Sanitize: strip non-printable for safety in symbol names
        for (char &c : name) {
            if (c == '\n') c = 'N';
            else if (c == '\t') c = 'T';
            else if (c == '%') c = 'P';
        }
        size_t len = std::strlen(data) + 1;
        lr_session_global(s, name.c_str(),
            lr_type_array_s(s, ty_i8, len),
            true, data, len);
        return lr_session_intern(s, name.c_str());
    }

    void visit_FileWrite(const ASR::FileWrite_t &x) {
        if (x.m_overloaded) {
            throw CodeGenError("liric: derived-type I/O FileWrite not yet supported");
        }
        if (x.m_id || x.m_rec || x.m_pos) {
            throw CodeGenError(
                "liric: FileWrite with id/rec/pos not yet supported");
        }
        // iomsg / iostat are silently ignored for now: their target
        // variables will not be updated.  Most fpm uses only consult
        // iostat to check end-of-file on reads, not writes.
        if (x.m_unit) {
            ASR::ttype_t *ut = ASRUtils::expr_type(x.m_unit);
            ut = ASRUtils::type_get_past_allocatable_pointer(ut);
            ut = ASRUtils::type_get_past_array(ut);
            if (!ASR::is_a<ASR::Integer_t>(*ut)) {
                throw CodeGenError(
                    "liric: write() to a non-integer unit (e.g. internal "
                    "file) not yet supported");
            }
            // Integer unit value is evaluated but ignored: _lfortran_printf
            // always writes to stdout.  fpm only writes to stdout/stderr
            // through this path so the simplification is safe for now.
            visit_expr(*x.m_unit);
            (void)tmp;
        }

        // When the frontend wraps the value list in a StringFormat (e.g.
        // `write(*, '(A)') str` becomes FileWrite([StringFormat('(A)',
        // [str])])), unpack it so we see the real args.  Matches the
        // LLVM backend's behaviour.
        ASR::expr_t **values = x.m_values;
        size_t n_values = x.n_values;
        if (n_values == 1 && ASR::is_a<ASR::StringFormat_t>(*values[0])) {
            ASR::StringFormat_t *sf =
                down_cast<ASR::StringFormat_t>(values[0]);
            values = sf->m_args;
            n_values = sf->n_args;
        }

        // Emit values.  Skip inter-value separators (format='(A)' /
        // single-string is the only case we currently hit on fpm; the
        // multi-arg list-directed path can revisit this later).
        for (size_t i = 0; i < n_values; i++) {
            ASR::expr_t *val = values[i];
            ASR::ttype_t *vt = ASRUtils::expr_type(val);
            vt = ASRUtils::type_get_past_allocatable_pointer(vt);
            vt = ASRUtils::type_get_past_array(vt);
            if (ASR::is_a<ASR::String_t>(*vt)) {
                visit_expr(*val);
                uint32_t desc = tmp;
                uint32_t fld0 = 0, fld1 = 1;
                uint32_t data = lr_emit_extractvalue(s, ty_ptr,
                    V(desc, ty_str_desc), &fld0, 1);
                uint32_t len  = lr_emit_extractvalue(s, ty_i64,
                    V(desc, ty_str_desc), &fld1, 1);
                file_write_emit_string(data, len);
            } else {
                throw CodeGenError(
                    "liric: write() of non-string values not yet supported "
                    "(use print *, ... for now)");
            }
        }

        // Trailer: m_end overrides the default "\n".  If advance=='no' the
        // frontend passes m_end="" which suppresses the newline.
        if (x.m_end) {
            ASR::ttype_t *et = ASRUtils::expr_type(x.m_end);
            et = ASRUtils::type_get_past_allocatable_pointer(et);
            et = ASRUtils::type_get_past_array(et);
            if (ASR::is_a<ASR::String_t>(*et)) {
                visit_expr(*x.m_end);
                uint32_t desc = tmp;
                uint32_t fld0 = 0, fld1 = 1;
                uint32_t data = lr_emit_extractvalue(s, ty_ptr,
                    V(desc, ty_str_desc), &fld0, 1);
                uint32_t len  = lr_emit_extractvalue(s, ty_i64,
                    V(desc, ty_str_desc), &fld1, 1);
                file_write_emit_string(data, len);
            }
        } else {
            uint32_t nl_sym = declare_global_cstring("\n", "_lr_fwnl");
            lr_type_t *printf_params[] = {ty_ptr};
            declare_func("printf", ty_i32, printf_params, 1, true);
            uint32_t printf_sym = lr_session_intern(s, "printf");
            lr_inst_desc_t d;
            memset(&d, 0, sizeof(d));
            lr_operand_desc_t ops[2] = {
                LR_GLOBAL(printf_sym, ty_ptr),
                LR_GLOBAL(nl_sym, ty_ptr)
            };
            d.op = LR_OP_CALL;
            d.type = ty_i32;
            d.operands = ops;
            d.num_operands = 2;
            d.call_external_abi = true;
            d.call_vararg = true;
            d.call_fixed_args = 1;
            lr_session_emit(s, &d, nullptr);
        }
    }

    // --- StringConstant ---
    //
    // Emit two private globals: a [len x i8] data array, and a {ptr,i64}
    // descriptor whose data slot is fixed up via a global reloc to the
    // data array.  Return the loaded descriptor value, matching the ABI
    // used by every other String_t producer.

    void visit_StringConstant(const ASR::StringConstant_t &x) {
        ASR::String_t *st = ASRUtils::get_string_type(x.m_type);
        int64_t len = -1;
        ASRUtils::extract_value(st->m_len, len);
        size_t src_len = x.m_s ? std::strlen(x.m_s) : 0;
        if (len < 0) len = (int64_t)src_len;
        if (len < 1) len = 1; // liric requires non-empty data globals

        std::string hash = std::to_string(get_hash((ASR::asr_t *)&x));
        std::string data_name = "_lr_strdata_" + hash;
        std::string desc_name = "_lr_strdesc_" + hash;

        // Materialize the literal: truncate or right-pad with spaces to len
        std::string data;
        if (x.m_s) {
            size_t take = std::min((size_t)len, src_len);
            data.assign(x.m_s, take);
        }
        if ((int64_t)data.size() < len) {
            data.resize((size_t)len, ' ');
        }

        lr_session_global(s, data_name.c_str(),
            lr_type_array_s(s, ty_i8, (uint64_t)len),
            true, data.data(), (size_t)len);

        // Descriptor blob: {nullptr, len}.  The data pointer gets resolved
        // by the reloc below at link/JIT time.
        struct desc_blob_t { void *p; int64_t l; };
        desc_blob_t desc_init = { nullptr, len };
        uint32_t desc_id = lr_session_global(s, desc_name.c_str(),
            ty_str_desc, true, &desc_init, sizeof(desc_init));
        lr_session_global_reloc(s, desc_id, 0, data_name.c_str());

        uint32_t desc_sym = lr_session_intern(s, desc_name.c_str());
        tmp = lr_emit_load(s, ty_str_desc, LR_GLOBAL(desc_sym, ty_ptr));
    }

    // --- StringLen ---
    //
    // Extracts the length field (index 1) from the string descriptor.
    // Constant-folded results are taken straight from m_value.

    void visit_StringLen(const ASR::StringLen_t &x) {
        LIRIC_PASSTHROUGH(x)
        visit_expr(*x.m_arg);
        uint32_t desc = tmp;
        uint32_t idx = 1;
        uint32_t len64 = lr_emit_extractvalue(s, ty_i64,
            V(desc, ty_str_desc), &idx, 1);
        lr_type_t *rt = get_type(x.m_type);
        if (rt == ty_i64) {
            tmp = len64;
        } else {
            tmp = lr_emit_trunc(s, rt, V(len64, ty_i64));
        }
    }

    // --- StructInstanceMember ---
    //
    // Resolve the parent Struct symbol from the owning expression, find
    // the member's positional index in its m_members array, and emit a
    // GEP [0, idx] on the cached liric struct type.  Honours is_target:
    // returns the field address for LHS use, otherwise loads the value.

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        LIRIC_PASSTHROUGH(x)

        ASR::ttype_t *vt = ASRUtils::expr_type(x.m_v);
        vt = ASRUtils::type_get_past_allocatable_pointer(vt);
        vt = ASRUtils::type_get_past_array(vt);
        if (!ASR::is_a<ASR::StructType_t>(*vt)) {
            throw CodeGenError(
                "liric: StructInstanceMember owner is not a struct type");
        }
        lr_type_t *struct_type = get_struct_type(
            down_cast<ASR::StructType_t>(vt));

        ASR::symbol_t *parent_sym = ASRUtils::get_struct_sym_from_struct_expr(
            const_cast<ASR::expr_t *>(x.m_v));
        if (!parent_sym) {
            throw CodeGenError(
                "liric: cannot resolve parent Struct for member access");
        }
        parent_sym = ASRUtils::symbol_get_past_external(parent_sym);
        ASR::Struct_t *parent_struct = down_cast<ASR::Struct_t>(parent_sym);

        ASR::symbol_t *msym = ASRUtils::symbol_get_past_external(x.m_m);
        const char *member_name = ASRUtils::symbol_name(msym);
        int member_idx = -1;
        for (size_t i = 0; i < parent_struct->n_members; i++) {
            if (std::strcmp(parent_struct->m_members[i], member_name) == 0) {
                member_idx = (int)i;
                break;
            }
        }
        if (member_idx < 0) {
            throw CodeGenError(std::string("liric: struct member '")
                + member_name + "' not found");
        }

        bool was_target = is_target;
        is_target = true;
        visit_expr(*x.m_v);
        is_target = was_target;
        uint32_t v_ptr = tmp;

        lr_operand_desc_t indices[2] = {
            I(0, ty_i32), I(member_idx, ty_i32)
        };
        uint32_t mem_ptr = lr_emit_gep(s, struct_type,
            V(v_ptr, ty_ptr), indices, 2);

        if (is_target) {
            tmp = mem_ptr;
        } else {
            lr_type_t *mt = get_type(x.m_type);
            tmp = lr_emit_load(s, mt, V(mem_ptr, ty_ptr));
        }
    }

    // --- CFI descriptor layout (matches asr_to_llvm's SimpleCMODescriptor) ---
    //
    // struct array_desc_<n_dims> {
    //   void*    base_addr;   // 0  (8)
    //   int64_t  elem_len;    // 8  (8)
    //   int32_t  version;     // 16 (4)
    //   int8_t   rank;        // 20 (1)
    //   int8_t   type;        // 21 (1)
    //   int8_t   attribute;   // 22 (1)
    //   int8_t   extra;       // 23 (1)
    //   int64_t  offset;      // 24 (8)
    //   struct { int64_t lbound, extent, stride; } dim[n_dims]; // 32 + 24*i
    // }
    //
    // We use byte offsets (i8 GEP) into the descriptor's i8* view so the
    // backend doesn't have to construct a per-rank struct type up front.

    static constexpr int64_t DESC_HEADER_BYTES = 32;
    static constexpr int64_t DESC_DIM_BYTES    = 24;
    static constexpr int64_t DESC_DIM_LBOUND   = 0;
    static constexpr int64_t DESC_DIM_EXTENT   = 8;

    // Load i64 at descriptor base + byte_offset.
    uint32_t desc_load_i64(uint32_t desc_ptr, int64_t byte_offset) {
        lr_operand_desc_t off[1] = {I(byte_offset, ty_i64)};
        uint32_t p = lr_emit_gep(s, ty_i8,
            V(desc_ptr, ty_ptr), off, 1);
        return lr_emit_load(s, ty_i64, V(p, ty_ptr));
    }

    // Pointer to descriptor for an array expression with is_target=true
    // semantics.  Returns a ptr-typed vreg.
    uint32_t desc_ptr_of(ASR::expr_t *v) {
        bool was_target = is_target;
        is_target = true;
        visit_expr(*v);
        is_target = was_target;
        return tmp;
    }

    // Load the i64 extent for dim_idx of a descriptor array.
    uint32_t desc_dim_extent(uint32_t desc_ptr, int64_t dim_idx) {
        return desc_load_i64(desc_ptr,
            DESC_HEADER_BYTES + DESC_DIM_BYTES * dim_idx + DESC_DIM_EXTENT);
    }

    uint32_t desc_dim_lbound(uint32_t desc_ptr, int64_t dim_idx) {
        return desc_load_i64(desc_ptr,
            DESC_HEADER_BYTES + DESC_DIM_BYTES * dim_idx + DESC_DIM_LBOUND);
    }

    uint32_t desc_base_addr(uint32_t desc_ptr) {
        // base_addr is at offset 0, ty_ptr-sized.
        return lr_emit_load(s, ty_ptr, V(desc_ptr, ty_ptr));
    }

    // --- ArrayBound ---
    //
    // Constant-foldable case stays as before.  Runtime path reads the
    // CFI descriptor: lbound from dim[i].lower_bound, ubound from
    // lbound + extent - 1.

    void visit_ArrayBound(const ASR::ArrayBound_t &x) {
        LIRIC_PASSTHROUGH(x)

        // Constant-fold path: full compile-time dims + dim.
        ASR::expr_t *array_value = ASRUtils::expr_value(x.m_v);
        bool const_dim = x.m_dim &&
            ASRUtils::is_value_constant(x.m_dim);
        if (array_value && const_dim) {
            ASR::dimension_t *dims = nullptr;
            ASRUtils::extract_dimensions_from_ttype(
                ASRUtils::expr_type(array_value), dims);
            int req_dim;
            ASRUtils::extract_value(x.m_dim, req_dim);
            req_dim--;
            if (dims) {
                size_t lbound = 1;
                if (dims[req_dim].m_start) {
                    ASRUtils::extract_value(
                        dims[req_dim].m_start, lbound);
                }
                size_t length = 0;
                bool has_length = dims[req_dim].m_length != nullptr &&
                    ASRUtils::extract_value(
                        dims[req_dim].m_length, length);
                size_t bound = 0;
                if (x.m_bound == ASR::arrayboundType::LBound) {
                    bound = (has_length && length == 0) ? 1 : lbound;
                } else {
                    bound = (has_length && length == 0)
                        ? 0 : (length + lbound - 1);
                }
                lr_type_t *rt = get_type(x.m_type);
                tmp = lr_emit_add(s, rt,
                    I((int64_t)bound, rt), I(0, rt));
                return;
            }
        }

        // Runtime path: read from descriptor.
        if (!const_dim) {
            throw CodeGenError(
                "liric: ArrayBound with runtime dim not supported");
        }
        int req_dim;
        ASRUtils::extract_value(x.m_dim, req_dim);
        req_dim--;

        uint32_t desc = desc_ptr_of(x.m_v);
        uint32_t lbound = desc_dim_lbound(desc, req_dim);
        lr_type_t *rt = get_type(x.m_type);
        uint32_t result;
        if (x.m_bound == ASR::arrayboundType::LBound) {
            result = lbound;
        } else {
            uint32_t extent = desc_dim_extent(desc, req_dim);
            uint32_t sum = lr_emit_add(s, ty_i64,
                V(lbound, ty_i64), V(extent, ty_i64));
            result = lr_emit_sub(s, ty_i64,
                V(sum, ty_i64), I(1, ty_i64));
        }
        if (rt == ty_i64) {
            tmp = result;
        } else {
            tmp = lr_emit_trunc(s, rt, V(result, ty_i64));
        }
    }

    // --- ArraySize ---
    //
    // total = product of extent for each dimension.  Without a dim
    // argument, walk all n dims of the array's type.

    void visit_ArraySize(const ASR::ArraySize_t &x) {
        LIRIC_PASSTHROUGH(x)

        ASR::ttype_t *vt = ASRUtils::expr_type(x.m_v);
        vt = ASRUtils::type_get_past_allocatable_pointer(vt);
        if (!ASR::is_a<ASR::Array_t>(*vt)) {
            throw CodeGenError(
                "liric: ArraySize owner is not an array type");
        }
        ASR::Array_t *array_t = down_cast<ASR::Array_t>(vt);
        int64_t n_dims = (int64_t)array_t->n_dims;

        uint32_t desc = desc_ptr_of(x.m_v);

        int64_t start_dim = 0;
        int64_t end_dim = n_dims;
        if (x.m_dim) {
            int req_dim;
            if (!ASRUtils::is_value_constant(x.m_dim) ||
                    !ASRUtils::extract_value(x.m_dim, req_dim)) {
                throw CodeGenError(
                    "liric: ArraySize with runtime dim not supported");
            }
            start_dim = req_dim - 1;
            end_dim = req_dim;
        }

        uint32_t prod = 0;
        bool first = true;
        for (int64_t d = start_dim; d < end_dim; d++) {
            uint32_t ext = desc_dim_extent(desc, d);
            if (first) {
                prod = ext;
                first = false;
            } else {
                prod = lr_emit_mul(s, ty_i64,
                    V(prod, ty_i64), V(ext, ty_i64));
            }
        }
        if (first) {
            prod = lr_emit_add(s, ty_i64, I(1, ty_i64), I(0, ty_i64));
        }

        lr_type_t *rt = get_type(x.m_type);
        if (rt == ty_i64) {
            tmp = prod;
        } else {
            tmp = lr_emit_trunc(s, rt, V(prod, ty_i64));
        }
    }

    // --- LogicalCompare ---

    void visit_LogicalCompare(const ASR::LogicalCompare_t &x) {
        LIRIC_PASSTHROUGH(x)
        visit_expr(*x.m_left);  uint32_t l = tmp;
        visit_expr(*x.m_right); uint32_t r = tmp;
        int pred = LR_CMP_EQ;
        switch (x.m_op) {
            case ASR::cmpopType::Eq:    pred = LR_CMP_EQ;  break;
            case ASR::cmpopType::NotEq: pred = LR_CMP_NE;  break;
            default:
                throw CodeGenError(
                    "liric: LogicalCompare supports only .eqv./.neqv.");
        }
        tmp = lr_emit_icmp(s, pred, V(l, ty_i1), V(r, ty_i1));
    }

    // --- StringSection ---
    //
    // s(start:end:step) for Fortran has step==1 always.  We need:
    //   data' = data + (start - 1)
    //   len'  = max(0, end - start + 1)
    // and we return a new {data', len'} descriptor.  The result is a
    // view into the source string; no allocation.

    void visit_StringSection(const ASR::StringSection_t &x) {
        LIRIC_PASSTHROUGH(x)
        if (!x.m_start || !x.m_end) {
            throw CodeGenError(
                "liric: StringSection requires both start and end "
                "(open-ended slices not yet supported)");
        }

        visit_expr(*x.m_arg);  uint32_t desc  = tmp;
        visit_expr(*x.m_start); uint32_t start = tmp;
        visit_expr(*x.m_end);   uint32_t end   = tmp;

        lr_type_t *start_t = get_type(ASRUtils::expr_type(x.m_start));
        lr_type_t *end_t   = get_type(ASRUtils::expr_type(x.m_end));
        uint32_t start64 = (start_t == ty_i64)
            ? start
            : lr_emit_sext(s, ty_i64, V(start, start_t));
        uint32_t end64 = (end_t == ty_i64)
            ? end
            : lr_emit_sext(s, ty_i64, V(end, end_t));

        // Raw length = end - start + 1; clamp to 0 if negative.
        uint32_t raw_diff = lr_emit_sub(s, ty_i64,
            V(end64, ty_i64), V(start64, ty_i64));
        uint32_t raw_len = lr_emit_add(s, ty_i64,
            V(raw_diff, ty_i64), I(1, ty_i64));
        uint32_t is_neg = lr_emit_icmp(s, LR_CMP_SLT,
            V(raw_len, ty_i64), I(0, ty_i64));
        uint32_t new_len = lr_emit_select(s, ty_i64,
            V(is_neg, ty_i1), I(0, ty_i64), V(raw_len, ty_i64));

        // Shift the data pointer by (start - 1).
        uint32_t fld0 = 0;
        uint32_t data = lr_emit_extractvalue(s, ty_ptr,
            V(desc, ty_str_desc), &fld0, 1);
        uint32_t off = lr_emit_sub(s, ty_i64,
            V(start64, ty_i64), I(1, ty_i64));
        lr_operand_desc_t gep_idx[1] = {V(off, ty_i64)};
        uint32_t new_data = lr_emit_gep(s, ty_i8,
            V(data, ty_ptr), gep_idx, 1);

        // Assemble the new descriptor.
        uint32_t fld1 = 1;
        uint32_t d0 = lr_emit_insertvalue(s, ty_str_desc,
            LR_UNDEF(ty_str_desc), V(new_data, ty_ptr), &fld0, 1);
        tmp = lr_emit_insertvalue(s, ty_str_desc,
            V(d0, ty_str_desc), V(new_len, ty_i64), &fld1, 1);
    }

    // --- StringItem ---
    //
    // s(i:i): build a new descriptor whose data pointer points to byte
    // i-1 of the source's data and whose length is 1.  No allocation;
    // this is a view into the source string.

    void visit_StringItem(const ASR::StringItem_t &x) {
        LIRIC_PASSTHROUGH(x)

        visit_expr(*x.m_arg); uint32_t desc = tmp;
        visit_expr(*x.m_idx); uint32_t idx = tmp;

        uint32_t fld0 = 0;
        uint32_t data = lr_emit_extractvalue(s, ty_ptr,
            V(desc, ty_str_desc), &fld0, 1);

        lr_type_t *idx_t = get_type(ASRUtils::expr_type(x.m_idx));
        uint32_t idx64;
        if (idx_t == ty_i64) {
            idx64 = idx;
        } else {
            idx64 = lr_emit_sext(s, ty_i64, V(idx, idx_t));
        }
        uint32_t idx0b = lr_emit_sub(s, ty_i64,
            V(idx64, ty_i64), I(1, ty_i64));

        lr_operand_desc_t gep_idx[1] = {V(idx0b, ty_i64)};
        uint32_t item_ptr = lr_emit_gep(s, ty_i8,
            V(data, ty_ptr), gep_idx, 1);

        // Compose a {ptr, i64} descriptor with length 1.
        uint32_t fld1 = 1;
        uint32_t d0 = lr_emit_insertvalue(s, ty_str_desc,
            LR_UNDEF(ty_str_desc), V(item_ptr, ty_ptr), &fld0, 1);
        tmp = lr_emit_insertvalue(s, ty_str_desc,
            V(d0, ty_str_desc), I(1, ty_i64), &fld1, 1);
    }

    // --- StringCompare ---
    //
    // Calls the runtime `str_compare(l_data, l_len, r_data, r_len) -> i32`
    // and turns its sign into the requested comparison.  The LLVM backend
    // has a single-character fast path; we leave that to a future chunk.

    void visit_StringCompare(const ASR::StringCompare_t &x) {
        LIRIC_PASSTHROUGH(x)

        visit_expr(*x.m_left);  uint32_t l_desc = tmp;
        visit_expr(*x.m_right); uint32_t r_desc = tmp;

        uint32_t idx0 = 0, idx1 = 1;
        uint32_t l_data = lr_emit_extractvalue(s, ty_ptr,
            V(l_desc, ty_str_desc), &idx0, 1);
        uint32_t l_len  = lr_emit_extractvalue(s, ty_i64,
            V(l_desc, ty_str_desc), &idx1, 1);
        uint32_t r_data = lr_emit_extractvalue(s, ty_ptr,
            V(r_desc, ty_str_desc), &idx0, 1);
        uint32_t r_len  = lr_emit_extractvalue(s, ty_i64,
            V(r_desc, ty_str_desc), &idx1, 1);

        lr_type_t *params[] = {ty_ptr, ty_i64, ty_ptr, ty_i64};
        declare_func("str_compare", ty_i32, params, 4, false);
        lr_operand_desc_t args[] = {
            V(l_data, ty_ptr), V(l_len, ty_i64),
            V(r_data, ty_ptr), V(r_len, ty_i64)
        };
        uint32_t cmp = emit_call("str_compare", ty_i32, args, 4);

        int pred = LR_CMP_EQ;
        switch (x.m_op) {
            case ASR::cmpopType::Eq:    pred = LR_CMP_EQ;  break;
            case ASR::cmpopType::NotEq: pred = LR_CMP_NE;  break;
            case ASR::cmpopType::Lt:    pred = LR_CMP_SLT; break;
            case ASR::cmpopType::LtE:   pred = LR_CMP_SLE; break;
            case ASR::cmpopType::Gt:    pred = LR_CMP_SGT; break;
            case ASR::cmpopType::GtE:   pred = LR_CMP_SGE; break;
        }
        tmp = lr_emit_icmp(s, pred, V(cmp, ty_i32), I(0, ty_i32));
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

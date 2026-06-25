// Liric backend (direct mode): first slice.
//
// Lowers a small ASR subset straight to a native object file through
// liric's direct-mode C session API, with no LLVM in the pipeline. This
// first slice covers scalar integer arithmetic, comparisons, assignment,
// `if`, and `error stop`: enough to compile and run expr_02. Coverage
// grows slice by slice; any unsupported ASR node raises a clear error
// rather than miscompiling.

#include <libasr/codegen/asr_to_liric.h>
#include <libasr/config.h>

#ifdef HAVE_LFORTRAN_LIRIC

#include <liric/liric_session.h>
#include <liric/liric_types.h>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/exception.h>

#include <cstring>
#include <string>
#include <unordered_map>

namespace LCompilers {

namespace {

class CodeGenError {
public:
    diag::Diagnostic d;
    explicit CodeGenError(const std::string &msg)
        : d{diag::Diagnostic(msg, diag::Level::Error, diag::Stage::CodeGen)} {}
};

using ASR::down_cast;
using ASR::is_a;

// Wrap a vreg / an integer immediate in a liric operand descriptor.
#define V(vreg, ty) LR_VREG((vreg), (ty))
#define IMM(value, ty) LR_IMM((value), (ty))

static inline uint64_t sym_key(const ASR::Variable_t *v) {
    return reinterpret_cast<uint64_t>(v);
}

class ASRToLiricVisitor : public ASR::BaseVisitor<ASRToLiricVisitor> {
public:
    lr_session_t *s;
    uint32_t value;          // result vreg of the expression just visited
    bool want_address;       // true while visiting an assignment target
    uint32_t return_block;   // function exit block
    std::unordered_map<uint64_t, uint32_t> slots;  // variable -> stack slot

    lr_type_t *ty_void, *ty_i1, *ty_i8, *ty_i16, *ty_i32, *ty_i64, *ty_ptr;

    explicit ASRToLiricVisitor(lr_session_t *session)
        : s(session), value(0), want_address(false), return_block(0)
    {
        ty_void = lr_type_void_s(s);
        ty_i1   = lr_type_i1_s(s);
        ty_i8   = lr_type_i8_s(s);
        ty_i16  = lr_type_i16_s(s);
        ty_i32  = lr_type_i32_s(s);
        ty_i64  = lr_type_i64_s(s);
        ty_ptr  = lr_type_ptr_s(s);
    }

    lr_type_t *get_type(ASR::ttype_t *t) {
        t = ASRUtils::type_get_past_array(
                ASRUtils::type_get_past_allocatable(t));
        if (t->type == ASR::ttypeType::Integer) {
            switch (ASRUtils::extract_kind_from_ttype_t(t)) {
                case 1: return ty_i8;
                case 2: return ty_i16;
                case 4: return ty_i32;
                case 8: return ty_i64;
            }
        } else if (t->type == ASR::ttypeType::Logical) {
            return ty_i1;
        }
        throw CodeGenError("liric: type not supported in this slice");
    }

    void call_void(const char *name, lr_operand_desc_t *args, uint32_t nargs) {
        lr_operand_desc_t ops[8];
        if (nargs + 1 > 8) throw CodeGenError("liric: too many call arguments");
        ops[0] = LR_GLOBAL(lr_session_intern(s, name), ty_ptr);
        for (uint32_t i = 0; i < nargs; i++) ops[1 + i] = args[i];
        lr_inst_desc_t d;
        memset(&d, 0, sizeof(d));
        d.op = LR_OP_CALL;
        d.type = ty_void;
        d.operands = ops;
        d.num_operands = nargs + 1;
        d.call_external_abi = true;
        lr_session_emit(s, &d, nullptr);
    }

    void visit_IntegerConstant(const ASR::IntegerConstant_t &x) {
        lr_type_t *t = get_type(x.m_type);
        value = lr_emit_add(s, t, IMM(x.m_n, t), IMM(0, t));
    }

    void visit_IntegerBinOp(const ASR::IntegerBinOp_t &x) {
        if (x.m_value) { visit_expr(*x.m_value); return; }
        visit_expr(*x.m_left);  uint32_t lhs = value;
        visit_expr(*x.m_right); uint32_t rhs = value;
        lr_type_t *t = get_type(x.m_type);
        switch (x.m_op) {
            case ASR::binopType::Add: value = lr_emit_add(s, t, V(lhs,t), V(rhs,t)); break;
            case ASR::binopType::Sub: value = lr_emit_sub(s, t, V(lhs,t), V(rhs,t)); break;
            case ASR::binopType::Mul: value = lr_emit_mul(s, t, V(lhs,t), V(rhs,t)); break;
            case ASR::binopType::Div: value = lr_emit_sdiv(s, t, V(lhs,t), V(rhs,t)); break;
            default: throw CodeGenError("liric: integer binop not supported in this slice");
        }
    }

    void visit_IntegerCompare(const ASR::IntegerCompare_t &x) {
        if (x.m_value) { visit_expr(*x.m_value); return; }
        visit_expr(*x.m_left);  uint32_t lhs = value;
        visit_expr(*x.m_right); uint32_t rhs = value;
        lr_type_t *t = get_type(ASRUtils::expr_type(x.m_left));
        int pred = LR_CMP_EQ;
        switch (x.m_op) {
            case ASR::cmpopType::Eq:    pred = LR_CMP_EQ;  break;
            case ASR::cmpopType::NotEq: pred = LR_CMP_NE;  break;
            case ASR::cmpopType::Lt:    pred = LR_CMP_SLT; break;
            case ASR::cmpopType::LtE:   pred = LR_CMP_SLE; break;
            case ASR::cmpopType::Gt:    pred = LR_CMP_SGT; break;
            case ASR::cmpopType::GtE:   pred = LR_CMP_SGE; break;
        }
        value = lr_emit_icmp(s, pred, V(lhs,t), V(rhs,t));
    }

    void visit_Var(const ASR::Var_t &x) {
        ASR::Variable_t *v = down_cast<ASR::Variable_t>(x.m_v);
        uint32_t slot = slots[sym_key(v)];
        if (want_address) {
            value = slot;
        } else {
            value = lr_emit_load(s, get_type(v->m_type), V(slot, ty_ptr));
        }
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        visit_expr(*x.m_value);
        uint32_t rhs = value;
        lr_type_t *t = get_type(ASRUtils::expr_type(x.m_value));
        want_address = true;
        visit_expr(*x.m_target);
        want_address = false;
        lr_emit_store(s, V(rhs, t), V(value, ty_ptr));
    }

    void visit_If(const ASR::If_t &x) {
        visit_expr(*x.m_test);
        uint32_t cond = value;
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

    void visit_ErrorStop(const ASR::ErrorStop_t &) {
        lr_operand_desc_t args[] = {IMM(1, ty_i32)};
        call_void("exit", args, 1);
        lr_emit_unreachable(s);
    }

    void visit_Module(const ASR::Module_t &x) {
        if (x.m_intrinsic) return;
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Function_t>(*item.second)) {
                visit_symbol(*item.second);
            }
        }
    }

    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        lr_type_t *exit_params[] = {ty_i32};
        lr_error_t err;
        lr_session_declare(s, "exit", ty_void, exit_params, 1, false, &err);
        for (auto &item : x.m_symtab->get_scope()) {
            visit_symbol(*item.second);
        }
    }

    void visit_Program(const ASR::Program_t &x) {
        lr_type_t *main_params[] = {ty_i32, ty_ptr};
        lr_error_t err;
        lr_session_func_begin(s, "main", ty_i32, main_params, 2, false, &err);
        uint32_t entry_block = lr_session_block(s);
        return_block = lr_session_block(s);
        lr_session_set_block(s, entry_block, &err);

        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *v = down_cast<ASR::Variable_t>(item.second);
                slots[sym_key(v)] = lr_emit_alloca(s, get_type(v->m_type));
            }
        }
        for (size_t i = 0; i < x.n_body; i++) visit_stmt(*x.m_body[i]);

        lr_emit_br(s, return_block);
        lr_session_set_block(s, return_block, &err);
        lr_emit_ret(s, IMM(0, ty_i32));
        lr_session_func_end(s, nullptr, &err);
    }
};

} // namespace

Result<int> asr_to_liric(ASR::TranslationUnit_t &asr,
    Allocator &/*al*/, const std::string &filename,
    CompilerOptions &/*co*/, diag::Diagnostics &diagnostics,
    int liric_backend)
{
    lr_session_config_t cfg;
    memset(&cfg, 0, sizeof(cfg));
    cfg.mode = LR_MODE_DIRECT;
    cfg.backend = static_cast<lr_session_backend_t>(liric_backend);
    lr_error_t err;
    lr_session_t *session = lr_session_create(&cfg, &err);
    if (!session) {
        diagnostics.diagnostics.push_back(diag::Diagnostic(
            "liric: failed to create session: " + std::string(err.msg),
            diag::Level::Error, diag::Stage::CodeGen));
        return Error();
    }
    try {
        ASRToLiricVisitor v(session);
        v.visit_asr(reinterpret_cast<ASR::asr_t &>(asr));
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

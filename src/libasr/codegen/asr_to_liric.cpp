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
#include <initializer_list>
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
inline lr_operand_desc_t vreg_operand(uint32_t vreg, lr_type_t *type) {
    lr_operand_desc_t operand{};
    operand.kind = LR_OP_KIND_VREG;
    operand.vreg = vreg;
    operand.type = type;
    return operand;
}

inline lr_operand_desc_t immediate_operand(int64_t value, lr_type_t *type) {
    lr_operand_desc_t operand{};
    operand.kind = LR_OP_KIND_IMM_I64;
    operand.imm_i64 = value;
    operand.type = type;
    return operand;
}

static inline uint64_t sym_key(const ASR::Variable_t *v) {
    return reinterpret_cast<uint64_t>(v);
}

class ASRToLiricVisitor : public ASR::BaseVisitor<ASRToLiricVisitor> {
public:
    lr_session_t *s;
    uint32_t value;          // result vreg of the expression just visited
    bool want_address;       // true while visiting an assignment target
    uint32_t return_block;   // function exit block
    bool terminated;
    std::unordered_map<uint64_t, uint32_t> slots;  // variable -> stack slot

    lr_type_t *ty_void, *ty_i1, *ty_i8, *ty_i16, *ty_i32, *ty_i64, *ty_ptr;

    explicit ASRToLiricVisitor(lr_session_t *session)
        : s(session), value(0), want_address(false), return_block(0),
          terminated(false)
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
        throw CodeGenError("liric: only scalar integers and logicals are supported in this slice");
    }

    uint32_t emit(lr_opcode_t op, lr_type_t *type,
            std::initializer_list<lr_operand_desc_t> operands,
            int predicate = 0, bool external_abi = false) {
        lr_inst_desc_t d{};
        d.op = op;
        d.type = type;
        d.operands = operands.begin();
        d.num_operands = operands.size();
        d.icmp_pred = predicate;
        d.call_external_abi = external_abi;
        lr_error_t err{};
        uint32_t result = lr_session_emit(s, &d, &err);
        if (err.code) throw CodeGenError("liric: instruction emission failed");
        return result;
    }

    uint32_t block() {
        uint32_t result = lr_session_block(s);
        if (result == UINT32_MAX) throw CodeGenError("liric: block creation failed");
        return result;
    }

    void set_block(uint32_t id) {
        lr_error_t err{};
        if (lr_session_set_block(s, id, &err) != 0) {
            throw CodeGenError("liric: block selection failed");
        }
        terminated = false;
    }

    void visit_statements(ASR::stmt_t **body, size_t n_body) {
        for (size_t i = 0; i < n_body && !terminated; i++) {
            visit_stmt(*body[i]);
        }
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
        lr_error_t err{};
        lr_session_emit(s, &d, &err);
        if (err.code) throw CodeGenError("liric: call emission failed");
    }

    void visit_IntegerConstant(const ASR::IntegerConstant_t &x) {
        lr_type_t *t = get_type(x.m_type);
        value = emit(LR_OP_ADD, t, {immediate_operand(x.m_n, t), immediate_operand(0, t)});
    }

    void visit_LogicalConstant(const ASR::LogicalConstant_t &x) {
        value = emit(LR_OP_ADD, ty_i1,
            {immediate_operand(x.m_value ? 1 : 0, ty_i1), immediate_operand(0, ty_i1)});
    }

    void visit_IntegerBinOp(const ASR::IntegerBinOp_t &x) {
        if (x.m_value) { visit_expr(*x.m_value); return; }
        visit_expr(*x.m_left);  uint32_t lhs = value;
        visit_expr(*x.m_right); uint32_t rhs = value;
        lr_type_t *t = get_type(x.m_type);
        switch (x.m_op) {
            case ASR::binopType::Add: value = emit(LR_OP_ADD, t, {vreg_operand(lhs,t), vreg_operand(rhs,t)}); break;
            case ASR::binopType::Sub: value = emit(LR_OP_SUB, t, {vreg_operand(lhs,t), vreg_operand(rhs,t)}); break;
            case ASR::binopType::Mul: value = emit(LR_OP_MUL, t, {vreg_operand(lhs,t), vreg_operand(rhs,t)}); break;
            case ASR::binopType::Div: value = emit(LR_OP_SDIV, t, {vreg_operand(lhs,t), vreg_operand(rhs,t)}); break;
            default: throw CodeGenError("liric: integer operation not supported in this slice");
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
        value = emit(LR_OP_ICMP, ty_i1, {vreg_operand(lhs,t), vreg_operand(rhs,t)}, pred);
    }

    void visit_Var(const ASR::Var_t &x) {
        ASR::symbol_t *symbol = ASRUtils::symbol_get_past_external(x.m_v);
        if (!is_a<ASR::Variable_t>(*symbol)) {
            throw CodeGenError("liric: only local scalar variables are supported in this slice");
        }
        ASR::Variable_t *v = down_cast<ASR::Variable_t>(symbol);
        auto found = slots.find(sym_key(v));
        if (found == slots.end()) {
            throw CodeGenError("liric: variable storage not supported in this slice");
        }
        uint32_t slot = found->second;
        if (want_address) {
            value = slot;
        } else {
            value = emit(LR_OP_LOAD, get_type(v->m_type), {vreg_operand(slot, ty_ptr)});
        }
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        visit_expr(*x.m_value);
        uint32_t rhs = value;
        lr_type_t *t = get_type(ASRUtils::expr_type(x.m_value));
        want_address = true;
        visit_expr(*x.m_target);
        want_address = false;
        emit(LR_OP_STORE, ty_void, {vreg_operand(rhs, t), vreg_operand(value, ty_ptr)});
    }

    void visit_If(const ASR::If_t &x) {
        visit_expr(*x.m_test);
        uint32_t cond = value;
        uint32_t then_bb = block();
        uint32_t else_bb = block();
        uint32_t merge_bb = block();
        emit(LR_OP_CONDBR, ty_void, {vreg_operand(cond, ty_i1),
            LR_BLOCK(then_bb), LR_BLOCK(x.n_orelse > 0 ? else_bb : merge_bb)});

        set_block(then_bb);
        visit_statements(x.m_body, x.n_body);
        if (!terminated) emit(LR_OP_BR, ty_void, {LR_BLOCK(merge_bb)});

        set_block(else_bb);
        visit_statements(x.m_orelse, x.n_orelse);
        if (!terminated) emit(LR_OP_BR, ty_void, {LR_BLOCK(merge_bb)});

        set_block(merge_bb);
    }

    void visit_ErrorStop(const ASR::ErrorStop_t &x) {
        if (x.m_code) {
            throw CodeGenError("liric: error stop codes are not supported in this slice");
        }
        lr_operand_desc_t args[] = {immediate_operand(1, ty_i32)};
        call_void("exit", args, 1);
        emit(LR_OP_UNREACHABLE, ty_void, {});
        terminated = true;
    }

    void visit_Module(const ASR::Module_t &x) {
        if (x.m_intrinsic) return;
        throw CodeGenError("liric: modules are not supported in this slice");
    }

    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        lr_type_t *exit_params[] = {ty_i32};
        lr_error_t err;
        if (lr_session_declare(s, "exit", ty_void, exit_params, 1, false, &err) != 0) {
            throw CodeGenError("liric: function declaration failed");
        }
        for (auto &item : x.m_symtab->get_scope()) {
            visit_symbol(*item.second);
        }
    }

    void visit_Program(const ASR::Program_t &x) {
        lr_type_t *main_params[] = {ty_i32, ty_ptr};
        lr_error_t err;
        if (lr_session_func_begin(s, "main", ty_i32, main_params, 2, false, &err) != 0) {
            throw CodeGenError("liric: function creation failed");
        }
        uint32_t entry_block = block();
        return_block = block();
        set_block(entry_block);
        slots.clear();

        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *v = down_cast<ASR::Variable_t>(item.second);
                slots[sym_key(v)] = emit(LR_OP_ALLOCA, get_type(v->m_type), {});
            }
        }
        for (auto &item : x.m_symtab->get_scope()) {
            if (!is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *v = down_cast<ASR::Variable_t>(item.second);
            if (v->m_value) {
                visit_expr(*v->m_value);
                emit(LR_OP_STORE, ty_void, {vreg_operand(value, get_type(v->m_type)),
                    vreg_operand(slots.at(sym_key(v)), ty_ptr)});
            } else if (v->m_symbolic_value) {
                throw CodeGenError("liric: declaration initializer not supported in this slice");
            }
        }
        visit_statements(x.m_body, x.n_body);

        if (!terminated) emit(LR_OP_BR, ty_void, {LR_BLOCK(return_block)});
        set_block(return_block);
        emit(LR_OP_RET, ty_i32, {immediate_operand(0, ty_i32)});
        if (lr_session_func_end(s, nullptr, &err) != 0) {
            throw CodeGenError("liric: function finalization failed");
        }
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
    } catch (const LCompilersException &e) {
        lr_session_destroy(session);
        if (e.error_code() != LFORTRAN_EXCEPTION) throw;
        diagnostics.diagnostics.push_back(diag::Diagnostic(
            "liric: this feature is not supported in this slice",
            diag::Level::Error, diag::Stage::CodeGen));
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

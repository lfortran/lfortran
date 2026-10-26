#include <deque>
#include <functional>
#include <set>
#include <string>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_stop.h>

namespace LCompilers {

namespace {

bool is_stop(const ASR::stmt_t &s) {
    return ASR::is_a<ASR::Stop_t>(s) || ASR::is_a<ASR::ErrorStop_t>(s);
}

ASR::expr_t* stop_code(const ASR::stmt_t &s) {
    if (ASR::is_a<ASR::Stop_t>(s)) {
        return ASR::down_cast<ASR::Stop_t>(&s)->m_code;
    }
    return ASR::down_cast<ASR::ErrorStop_t>(&s)->m_code;
}

// The procedure a call names, past external symbols and type bound
// procedure declarations.
ASR::Function_t* called_function(ASR::symbol_t *name) {
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(name);
    if (s == nullptr) return nullptr;
    s = ASRUtils::symbol_get_past_external(
        ASRUtils::symbol_get_past_StructMethodDeclaration(s));
    if (s == nullptr || !ASR::is_a<ASR::Function_t>(*s)) return nullptr;
    return ASR::down_cast<ASR::Function_t>(s);
}

// Every `stop` and `error stop` a procedure body holds, BLOCK and ASSOCIATE
// bodies included.
class StopCollector : public ASRUtils::BlockBodyWalkVisitor<StopCollector> {
public:
    std::vector<const ASR::stmt_t*> stops;

    void visit_stmt(const ASR::stmt_t &s) {
        if (is_stop(s)) {
            stops.push_back(&s);
            return;
        }
        ASR::BaseWalkVisitor<StopCollector>::visit_stmt(s);
    }
};

std::vector<const ASR::stmt_t*> stops_of(const ASR::Function_t &fn) {
    StopCollector collector;
    for (size_t i = 0; i < fn.n_body; i++) {
        collector.visit_stmt(*fn.m_body[i]);
    }
    return collector.stops;
}

// Whether a body names one of `procedures` other than by calling it, such
// as by passing it as an actual argument.
class ProcedureValueFinder
        : public ASRUtils::BlockBodyWalkVisitor<ProcedureValueFinder> {
public:
    const std::set<ASR::Function_t*> &procedures;
    ASR::Function_t *found = nullptr;

    explicit ProcedureValueFinder(
            const std::set<ASR::Function_t*> &procedures_)
        : procedures(procedures_) {}

    void visit_Var(const ASR::Var_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_v);
        if (s == nullptr || !ASR::is_a<ASR::Function_t>(*s)) return;
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(s);
        if (procedures.count(fn) > 0) found = fn;
    }
};

// Whether `fn` is declared inside the symbol table of `kernel`, which is
// where the offload pass puts the copies of the procedures the kernel calls.
bool owned_by(const ASR::Function_t &fn, const ASR::Function_t &kernel) {
    for (SymbolTable *s = fn.m_symtab->parent; s != nullptr; s = s->parent) {
        if (s == kernel.m_symtab) return true;
    }
    return false;
}

// Rewrites the bodies of the kernel and of the procedures that can stop.
class StopLowering {
public:
    StopLowering(Allocator &al_, const Location &loc_,
            const std::set<ASR::Function_t*> &stopping_,
            ASR::symbol_t *status_)
        : al(al_), loc(loc_), b(al_, loc_), stopping(stopping_),
          status(status_) {
        int4 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    }

    // Every statement that stops, in the order of its site number.
    std::vector<const ASR::stmt_t*> sites;

    void lower_kernel(ASR::Function_t &kernel, ASR::symbol_t *site_,
            ASR::symbol_t *code_) {
        in_kernel = true;
        site = site_;
        code = code_;
        transform(kernel.m_body, kernel.n_body);
    }

    void lower_procedure(ASR::Function_t &fn, ASR::symbol_t *site_,
            ASR::symbol_t *code_) {
        in_kernel = false;
        site = site_;
        code = code_;
        transform(fn.m_body, fn.n_body);
    }

private:
    Allocator &al;
    Location loc;
    ASRUtils::ASRBuilder b;
    const std::set<ASR::Function_t*> &stopping;
    ASR::symbol_t *status;
    ASR::ttype_t *int4;

    bool in_kernel = false;
    // The site and the stop code of the procedure being rewritten: its
    // hidden dummies, or the kernel's locals.
    ASR::symbol_t *site = nullptr;
    ASR::symbol_t *code = nullptr;
    // The calls already given the hidden arguments.
    std::set<const void*> extended_calls;
    std::set<ASR::symbol_t*> lowered_blocks;

    ASR::expr_t* var(ASR::symbol_t *s) {
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, s));
    }

    ASR::expr_t* status_item(ASR::expr_t *index) {
        Vec<ASR::array_index_t> args;
        args.reserve(al, 1);
        ASR::array_index_t ai;
        ai.loc = loc;
        ai.m_left = nullptr;
        ai.m_right = index;
        ai.m_step = nullptr;
        args.push_back(al, ai);
        return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, var(status),
            args.p, args.n, int4, ASR::arraystorageType::ColMajor, nullptr,
            false));
    }

    ASR::stmt_t* ret() {
        return ASRUtils::STMT(ASR::make_Return_t(al, loc));
    }

    // Records a stop at the site `k` makes, with the integer stop code
    // `value` when there is one, in the status buffer: the code first and
    // the site after it (see gpu_offload_stop.h). A stop an earlier launch
    // recorded, which the host has not read yet, ended the program there,
    // so it is kept.
    std::vector<ASR::stmt_t*> record(
            const std::function<ASR::expr_t*()> &k, ASR::expr_t *value) {
        std::vector<ASR::stmt_t*> stmts;
        if (value != nullptr) {
            stmts.push_back(b.Assignment(
                status_item(b.Add(k(), b.i32(1))), value));
        }
        stmts.push_back(b.Assignment(status_item(b.i32(1)), k()));
        return {b.If(b.Eq(status_item(b.i32(1)), b.i32(0)), stmts, {})};
    }

    // What a `stop` turns into: the site, and its integer stop code, are
    // recorded -- in the buffer in the kernel, in the hidden dummies in a
    // procedure -- and the routine returns.
    std::vector<ASR::stmt_t*> lower_stop(const ASR::stmt_t &s) {
        sites.push_back(&s);
        int64_t k = (int64_t)sites.size();
        ASR::expr_t *value = stop_code(s);
        if (value != nullptr && ASRUtils::is_integer(
                *ASRUtils::expr_type(value))) {
            if (ASRUtils::extract_kind_from_ttype_t(
                    ASRUtils::expr_type(value)) != 4) {
                value = ASRUtils::EXPR(ASR::make_Cast_t(al, loc, value,
                    ASR::cast_kindType::IntegerToInteger, int4, nullptr, nullptr));
            }
        } else {
            value = nullptr;
        }
        std::vector<ASR::stmt_t*> stmts;
        if (in_kernel) {
            stmts = record([&]() { return b.i32(k); }, value);
        } else {
            stmts.push_back(b.Assignment(var(site), b.i32(k)));
            if (value != nullptr) {
                stmts.push_back(b.Assignment(var(code), value));
            }
        }
        stmts.push_back(ret());
        return stmts;
    }

    // What follows a statement that calls a procedure that can stop: when
    // it stopped, the kernel records it and returns, and a procedure
    // returns to its own caller.
    ASR::stmt_t* stopped_check() {
        std::vector<ASR::stmt_t*> body;
        if (in_kernel) {
            body = record([&]() { return var(site); }, var(code));
        }
        body.push_back(ret());
        return b.If(b.NotEq(var(site), b.i32(0)), body, {});
    }

    // Gives a call to a procedure that can stop the site and the code of
    // the routine making it as its two hidden arguments.
    bool extend_call(ASR::call_arg_t *&args, size_t &n_args,
            ASR::symbol_t *name, const void *call) {
        ASR::Function_t *fn = called_function(name);
        if (fn == nullptr || stopping.count(fn) == 0) return false;
        if (!extended_calls.insert(call).second) return true;
        Vec<ASR::call_arg_t> extended;
        extended.reserve(al, n_args + 2);
        for (size_t i = 0; i < n_args; i++) extended.push_back(al, args[i]);
        for (ASR::symbol_t *s : {site, code}) {
            ASR::call_arg_t arg;
            arg.loc = loc;
            arg.m_value = var(s);
            extended.push_back(al, arg);
        }
        args = extended.p;
        n_args = extended.n;
        return true;
    }

    class CallExtender : public ASR::BaseWalkVisitor<CallExtender> {
    public:
        StopLowering &lowering;
        bool extended = false;

        explicit CallExtender(StopLowering &lowering_)
            : lowering(lowering_) {}

        void visit_FunctionCall(const ASR::FunctionCall_t &x) {
            ASR::BaseWalkVisitor<CallExtender>::visit_FunctionCall(x);
            ASR::FunctionCall_t &xx = const_cast<ASR::FunctionCall_t&>(x);
            extended |= lowering.extend_call(xx.m_args, xx.n_args,
                xx.m_name, &x);
        }

        void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
            ASR::BaseWalkVisitor<CallExtender>::visit_SubroutineCall(x);
            ASR::SubroutineCall_t &xx =
                const_cast<ASR::SubroutineCall_t&>(x);
            extended |= lowering.extend_call(xx.m_args, xx.n_args,
                xx.m_name, &x);
        }
    };

    bool extend_calls_in(ASR::expr_t *e) {
        if (e == nullptr) return false;
        CallExtender extender(*this);
        extender.visit_expr(*e);
        return extender.extended;
    }

    bool extend_calls_in(const ASR::stmt_t &s) {
        CallExtender extender(*this);
        extender.visit_stmt(s);
        return extender.extended;
    }

    // A construct whose own expressions call a procedure that stopped runs
    // none of its body: each body starts with the test.
    void start_with_check(ASR::stmt_t **&body, size_t &n_body) {
        if (n_body == 0) return;
        Vec<ASR::stmt_t*> checked;
        checked.reserve(al, n_body + 1);
        checked.push_back(al, stopped_check());
        for (size_t i = 0; i < n_body; i++) checked.push_back(al, body[i]);
        body = checked.p;
        n_body = checked.n;
    }

    void lower_block_body(ASR::symbol_t *m) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(m);
        if (s == nullptr || !lowered_blocks.insert(s).second) return;
        if (ASR::is_a<ASR::Block_t>(*s)) {
            ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(s);
            transform(block->m_body, block->n_body);
        } else if (ASR::is_a<ASR::AssociateBlock_t>(*s)) {
            ASR::AssociateBlock_t *block =
                ASR::down_cast<ASR::AssociateBlock_t>(s);
            transform(block->m_body, block->n_body);
        }
    }

    void transform(ASR::stmt_t **&body, size_t &n_body) {
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n_body + 1);
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t *s = body[i];
            if (is_stop(*s)) {
                for (ASR::stmt_t *t : lower_stop(*s)) out.push_back(al, t);
                continue;
            }
            bool calls = false;
            switch (s->type) {
                case ASR::stmtType::If: {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    transform(x->m_body, x->n_body);
                    transform(x->m_orelse, x->n_orelse);
                    calls = extend_calls_in(x->m_test);
                    if (calls) {
                        start_with_check(x->m_body, x->n_body);
                        start_with_check(x->m_orelse, x->n_orelse);
                    }
                    break;
                }
                case ASR::stmtType::WhileLoop: {
                    ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                    transform(x->m_body, x->n_body);
                    transform(x->m_orelse, x->n_orelse);
                    calls = extend_calls_in(x->m_test);
                    if (calls) start_with_check(x->m_body, x->n_body);
                    break;
                }
                case ASR::stmtType::DoLoop: {
                    ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                    transform(x->m_body, x->n_body);
                    transform(x->m_orelse, x->n_orelse);
                    calls = extend_calls_in(x->m_head.m_start);
                    calls |= extend_calls_in(x->m_head.m_end);
                    calls |= extend_calls_in(x->m_head.m_increment);
                    if (calls) start_with_check(x->m_body, x->n_body);
                    break;
                }
                case ASR::stmtType::DoConcurrentLoop: {
                    ASR::DoConcurrentLoop_t *x =
                        ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                    transform(x->m_body, x->n_body);
                    for (size_t h = 0; h < x->n_head; h++) {
                        calls |= extend_calls_in(x->m_head[h].m_start);
                        calls |= extend_calls_in(x->m_head[h].m_end);
                        calls |= extend_calls_in(x->m_head[h].m_increment);
                    }
                    if (calls) start_with_check(x->m_body, x->n_body);
                    break;
                }
                case ASR::stmtType::Select: {
                    ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                    calls = extend_calls_in(x->m_test);
                    for (size_t c = 0; c < x->n_body; c++) {
                        ASR::case_stmt_t *cs = x->m_body[c];
                        if (ASR::is_a<ASR::CaseStmt_t>(*cs)) {
                            ASR::CaseStmt_t *t =
                                ASR::down_cast<ASR::CaseStmt_t>(cs);
                            for (size_t j = 0; j < t->n_test; j++) {
                                calls |= extend_calls_in(t->m_test[j]);
                            }
                            transform(t->m_body, t->n_body);
                        } else {
                            ASR::CaseStmt_Range_t *t =
                                ASR::down_cast<ASR::CaseStmt_Range_t>(cs);
                            calls |= extend_calls_in(t->m_start);
                            calls |= extend_calls_in(t->m_end);
                            transform(t->m_body, t->n_body);
                        }
                    }
                    transform(x->m_default, x->n_default);
                    if (calls) {
                        for (size_t c = 0; c < x->n_body; c++) {
                            ASR::case_stmt_t *cs = x->m_body[c];
                            if (ASR::is_a<ASR::CaseStmt_t>(*cs)) {
                                ASR::CaseStmt_t *t =
                                    ASR::down_cast<ASR::CaseStmt_t>(cs);
                                start_with_check(t->m_body, t->n_body);
                            } else {
                                ASR::CaseStmt_Range_t *t =
                                    ASR::down_cast<ASR::CaseStmt_Range_t>(cs);
                                start_with_check(t->m_body, t->n_body);
                            }
                        }
                        start_with_check(x->m_default, x->n_default);
                    }
                    break;
                }
                case ASR::stmtType::OMPRegion: {
                    ASR::OMPRegion_t *x = ASR::down_cast<ASR::OMPRegion_t>(s);
                    transform(x->m_body, x->n_body);
                    break;
                }
                case ASR::stmtType::BlockCall: {
                    lower_block_body(ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                    break;
                }
                case ASR::stmtType::AssociateBlockCall: {
                    lower_block_body(
                        ASR::down_cast<ASR::AssociateBlockCall_t>(s)->m_m);
                    break;
                }
                default: {
                    calls = extend_calls_in(*s);
                    break;
                }
            }
            out.push_back(al, s);
            if (calls) out.push_back(al, stopped_check());
        }
        body = out.p;
        n_body = out.n;
    }
};

ASR::ttype_t* status_type(Allocator &al, const Location &loc, int64_t n) {
    ASR::ttype_t *int4 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    Vec<ASR::dimension_t> dims;
    dims.reserve(al, 1);
    ASR::dimension_t dim;
    dim.loc = loc;
    dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
        al, loc, 1, int4, ASR::integerbozType::Decimal));
    dim.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
        al, loc, n, int4, ASR::integerbozType::Decimal));
    dims.push_back(al, dim);
    return ASRUtils::TYPE(ASR::make_Array_t(al, loc, int4, dims.p, dims.n,
        ASR::array_physical_typeType::FixedSizeArray,
        ASR::memory_spaceType::Global));
}

// Appends a dummy argument of type `type` to `fn`, keeping its signature in
// step.
ASR::symbol_t* append_dummy(Allocator &al, const Location &loc,
        ASR::Function_t &fn, const std::string &name, ASR::ttype_t *type,
        ASR::ttype_t *signature_type) {
    ASR::symbol_t *sym = gpu_new_variable(al, loc, fn.m_symtab,
        fn.m_symtab->get_unique_name(name), type, ASR::intentType::InOut);
    Vec<ASR::expr_t*> args;
    args.reserve(al, fn.n_args + 1);
    for (size_t i = 0; i < fn.n_args; i++) args.push_back(al, fn.m_args[i]);
    args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym)));
    fn.m_args = args.p;
    fn.n_args = args.n;

    // The signature may be shared with the procedure this one was copied
    // from, so a new one is made rather than the old one changed.
    ASR::FunctionType_t *t = ASRUtils::get_FunctionType(fn);
    Vec<ASR::ttype_t*> types;
    types.reserve(al, t->n_arg_types + 1);
    for (size_t i = 0; i < t->n_arg_types; i++) {
        types.push_back(al, t->m_arg_types[i]);
    }
    types.push_back(al, signature_type);
    fn.m_function_signature = ASRUtils::TYPE(ASR::make_FunctionType_t(al,
        t->base.base.loc, types.p, types.n, t->m_return_var_type, t->m_abi,
        t->m_deftype, t->m_bindc_name, t->m_elemental, t->m_pure,
        t->m_module, t->m_inline, t->m_static, t->m_restrictions,
        t->n_restrictions, t->m_is_restriction, t->m_exec_space));
    return sym;
}

} // namespace

GpuDecline gpu_lower_device_stops(Allocator &al, const Location &loc,
        ASR::Function_t &kernel, SymbolTable *host_scope,
        GpuDeviceStops &stops) {
    // Every procedure the kernel reaches.
    std::vector<ASR::Function_t*> reached;
    {
        std::set<ASR::Function_t*> seen;
        std::deque<ASR::Function_t*> work;
        work.push_back(&kernel);
        seen.insert(&kernel);
        while (!work.empty()) {
            ASR::Function_t *fn = work.front();
            work.pop_front();
            for (ASR::Function_t *callee : gpu_callees(fn->m_body,
                    fn->n_body, false)) {
                if (!seen.insert(callee).second) continue;
                reached.push_back(callee);
                work.push_back(callee);
            }
        }
    }

    // The procedures that can stop: those holding a `stop`, and those
    // calling one that can.
    std::set<ASR::Function_t*> stopping;
    for (ASR::Function_t *fn : reached) {
        if (!stops_of(*fn).empty()) stopping.insert(fn);
    }
    for (bool added = true; added;) {
        added = false;
        for (ASR::Function_t *fn : reached) {
            if (stopping.count(fn) > 0) continue;
            for (ASR::Function_t *callee : gpu_callees(fn->m_body,
                    fn->n_body, false)) {
                if (stopping.count(callee) > 0) {
                    stopping.insert(fn);
                    added = true;
                    break;
                }
            }
        }
    }
    if (stopping.empty() && stops_of(kernel).empty()) return GpuDecline();

    // What cannot be lowered is turned down before anything is changed.
    std::vector<ASR::Function_t*> rewritten = {&kernel};
    for (ASR::Function_t *fn : reached) {
        if (stopping.count(fn) > 0) rewritten.push_back(fn);
    }
    for (ASR::Function_t *fn : rewritten) {
        std::string routine = fn == &kernel ? "" : fn->m_name;
        for (const ASR::stmt_t *s : stops_of(*fn)) {
            ASR::expr_t *value = stop_code(*s);
            if (value == nullptr) continue;
            ASR::ttype_t *type = ASRUtils::expr_type(value);
            if (ASRUtils::is_integer(*type)) continue;
            if (ASRUtils::expr_value(value) != nullptr) continue;
            return GpuDecline(GpuDeclineReason::StopCodeNotConstant, routine);
        }
        if (fn != &kernel && !owned_by(*fn, kernel)) {
            return GpuDecline(GpuDeclineReason::StoppingProcedureShared,
                fn->m_name);
        }
    }
    for (ASR::Function_t *fn : rewritten) {
        ProcedureValueFinder values(stopping);
        for (size_t i = 0; i < fn->n_body; i++) {
            values.visit_stmt(*fn->m_body[i]);
        }
        if (values.found != nullptr) {
            return GpuDecline(GpuDeclineReason::StoppingProcedureShared,
                values.found->m_name);
        }
    }

    ASRUtils::ASRBuilder b(al, loc);
    ASR::ttype_t *int4 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));

    // The buffer is the last parameter of the kernel. Its extent is only
    // known once every site has been numbered, and is set below.
    ASR::symbol_t *status = append_dummy(al, loc, kernel,
        "__gpu_stop_status", status_type(al, loc, 1),
        status_type(al, loc, 1));
    StopLowering lowering(al, loc, stopping, status);

    for (ASR::Function_t *fn : rewritten) {
        if (fn == &kernel) continue;
        ASR::symbol_t *site = append_dummy(al, loc, *fn, "__gpu_stop_site",
            int4, int4);
        ASR::symbol_t *code = append_dummy(al, loc, *fn, "__gpu_stop_code",
            int4, int4);
        lowering.lower_procedure(*fn, site, code);
    }
    // The kernel holds the site and the code a procedure it calls hands
    // back, when it calls one that can stop.
    ASR::symbol_t *site = nullptr, *code = nullptr;
    if (!stopping.empty()) {
        site = gpu_new_variable(al, loc, kernel.m_symtab,
            kernel.m_symtab->get_unique_name("__gpu_stop_site"), int4);
        code = gpu_new_variable(al, loc, kernel.m_symtab,
            kernel.m_symtab->get_unique_name("__gpu_stop_code"), int4);
    }
    lowering.lower_kernel(kernel, site, code);
    if (site != nullptr) {
        Vec<ASR::stmt_t*> body;
        body.reserve(al, kernel.n_body + 2);
        body.push_back(al, b.Assignment(b.Var(site), b.i32(0)));
        body.push_back(al, b.Assignment(b.Var(code), b.i32(0)));
        for (size_t i = 0; i < kernel.n_body; i++) {
            body.push_back(al, kernel.m_body[i]);
        }
        kernel.m_body = body.p;
        kernel.n_body = body.n;
    }

    int64_t n_sites = (int64_t)lowering.sites.size();
    ASR::down_cast<ASR::Variable_t>(status)->m_type =
        status_type(al, loc, n_sites + 1);
    // `append_dummy` made the kernel a signature of its own, so its last
    // parameter is updated in place.
    ASR::FunctionType_t *kernel_type = ASRUtils::get_FunctionType(kernel);
    kernel_type->m_arg_types[kernel_type->n_arg_types - 1] =
        status_type(al, loc, n_sites + 1);

    // The host side: the statement a thread recorded is run once the host
    // has waited for the launch. The status is cleared once, on entry to
    // the procedure that launches the kernel (see gpu_launch_sync.cpp).
    stops.host_status = gpu_new_variable(al, loc, host_scope,
        host_scope->get_unique_name("__gpu_stop_status"),
        status_type(al, loc, n_sites + 1));
    auto host_item = [&](int64_t index) {
        Vec<ASR::array_index_t> args;
        args.reserve(al, 1);
        ASR::array_index_t ai;
        ai.loc = loc;
        ai.m_left = nullptr;
        ai.m_right = b.i32(index);
        ai.m_step = nullptr;
        args.push_back(al, ai);
        return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
            b.Var(stops.host_status), args.p, args.n, int4,
            ASR::arraystorageType::ColMajor, nullptr, false));
    };

    ASR::stmt_t *replay = nullptr;
    for (int64_t k = n_sites; k >= 1; k--) {
        const ASR::stmt_t *s = lowering.sites[k - 1];
        ASR::expr_t *value = stop_code(*s);
        ASR::expr_t *host_value = nullptr;
        if (value != nullptr) {
            if (ASRUtils::is_integer(*ASRUtils::expr_type(value))) {
                host_value = host_item(k + 1);
            } else {
                ASRUtils::ExprStmtDuplicator dup(al);
                host_value = dup.duplicate_expr(ASRUtils::expr_value(value));
            }
        }
        ASR::stmt_t *stmt = ASR::is_a<ASR::Stop_t>(*s)
            ? ASRUtils::STMT(ASR::make_Stop_t(al, loc, host_value))
            : ASRUtils::STMT(ASR::make_ErrorStop_t(al, loc, host_value));
        std::vector<ASR::stmt_t*> orelse;
        if (replay != nullptr) orelse.push_back(replay);
        replay = b.If(b.Eq(host_item(1), b.i32(k)), {stmt}, orelse);
    }
    if (replay != nullptr) stops.after_launch.push_back(replay);
    return GpuDecline();
}

namespace {

// `Var` == or /= constant `n`: the variable compared, nullptr otherwise.
ASR::expr_t* compared_with(ASR::expr_t *test, ASR::cmpopType op, int64_t n) {
    if (!ASR::is_a<ASR::IntegerCompare_t>(*test)) return nullptr;
    ASR::IntegerCompare_t *c = ASR::down_cast<ASR::IntegerCompare_t>(test);
    int64_t value = 0;
    if (c->m_op != op || !ASR::is_a<ASR::IntegerConstant_t>(*c->m_right)
            || !ASRUtils::extract_value(c->m_right, value) || value != n) {
        return nullptr;
    }
    return c->m_left;
}

ASR::symbol_t* array_of_item(ASR::expr_t *e) {
    if (!ASR::is_a<ASR::ArrayItem_t>(*e)) return nullptr;
    ASR::expr_t *v = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
    if (!ASR::is_a<ASR::Var_t>(*v)) return nullptr;
    return ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(v)
        ->m_v);
}

} // namespace

ASR::symbol_t* gpu_stop_record_status(const ASR::stmt_t &s) {
    if (!ASR::is_a<ASR::If_t>(s)) return nullptr;
    const ASR::If_t &x = *ASR::down_cast<ASR::If_t>(&s);
    if (x.n_orelse != 0 || x.n_body == 0) return nullptr;
    ASR::expr_t *first = compared_with(x.m_test, ASR::cmpopType::Eq, 0);
    if (first == nullptr) return nullptr;
    ASR::symbol_t *status = array_of_item(first);
    if (status == nullptr) return nullptr;
    for (size_t i = 0; i < x.n_body; i++) {
        if (!ASR::is_a<ASR::Assignment_t>(*x.m_body[i]) || array_of_item(
                ASR::down_cast<ASR::Assignment_t>(x.m_body[i])->m_target)
                != status) {
            return nullptr;
        }
    }
    return status;
}

bool gpu_stopped_check_scalars(const ASR::stmt_t &s, ASR::symbol_t **site,
        ASR::symbol_t **code) {
    if (!ASR::is_a<ASR::If_t>(s)) return false;
    const ASR::If_t &x = *ASR::down_cast<ASR::If_t>(&s);
    if (x.n_orelse != 0 || x.n_body == 0
            || !ASR::is_a<ASR::Return_t>(*x.m_body[x.n_body - 1])) {
        return false;
    }
    ASR::expr_t *tested = compared_with(x.m_test, ASR::cmpopType::NotEq, 0);
    if (tested == nullptr || !ASR::is_a<ASR::Var_t>(*tested)) return false;
    *site = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(tested)->m_v);
    *code = nullptr;
    for (size_t i = 0; i + 1 < x.n_body; i++) {
        if (gpu_stop_record_status(*x.m_body[i]) == nullptr) return false;
        const ASR::If_t *record = ASR::down_cast<ASR::If_t>(x.m_body[i]);
        for (size_t j = 0; j < record->n_body; j++) {
            ASR::expr_t *value = ASR::down_cast<ASR::Assignment_t>(
                record->m_body[j])->m_value;
            if (ASR::is_a<ASR::Var_t>(*value)
                    && ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(value)->m_v) != *site) {
                *code = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(value)->m_v);
            }
        }
    }
    return true;
}

} // namespace LCompilers

#include <tests/doctest.h>

#include <algorithm>
#include <fstream>
#include <functional>
#include <sstream>

#include <libasr/asr_text.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/pass_manager.h>
#include <libasr/pass/gpu_memory_space.h>
#include <libasr/pass/gpu_loop_utils.h>
#include <libasr/serialization.h>
#include <lfortran/fortran_evaluator.h>
#include <lfortran/parser/source_offset.h>

using namespace LCompilers;

TEST_CASE("Entry inquiries require metadata but not element values") {
    const std::string source = R"(
subroutine inquiries(a, n)
    integer, intent(in) :: a(:), n
    integer :: local(n), selector
    integer, pointer :: p(:)
    integer, allocatable :: allocated(:)
    character(:), allocatable :: text
    character(n) :: local_text
    integer :: pointer_size, pointer_lower, pointer_upper, pointer_shape(1)
    integer :: allocated_size, deferred_len, section_size
    integer :: dummy_size, local_size, local_len, local_shape(1)
    pointer_size = size(p)
    pointer_lower = lbound(p, 1)
    pointer_upper = ubound(p, 1)
    pointer_shape = shape(p)
    allocated_size = size(allocated)
    deferred_len = len(text)
    section_size = size(local(:selector))
    dummy_size = size(a)
    local_size = size(local)
    local_len = len(local_text)
    local_shape = shape(local)
end subroutine
)";
    CompilerOptions options;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = "entry_inquiries.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(LFortran::source_offset(source.size()));
    diag::Diagnostics diagnostics;
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(parsed.ok);
    auto* function = ASR::down_cast<ASR::Function_t>(
        parsed.result->m_symtab->get_symbol("inquiries"));
    std::map<std::string, std::string> expected = {
        {"pointer_size", "p"}, {"pointer_lower", "p"},
        {"pointer_upper", "p"}, {"pointer_shape", "p"},
        {"allocated_size", "allocated"}, {"deferred_len", "text"},
        {"section_size", "selector"}, {"dummy_size", ""},
        {"local_size", ""}, {"local_len", ""}, {"local_shape", ""}
    };
    size_t checked = 0;
    for (size_t i = 0; i < function->n_body; i++) {
        if (!ASR::is_a<ASR::Assignment_t>(*function->m_body[i])) continue;
        auto* assignment = ASR::down_cast<ASR::Assignment_t>(function->m_body[i]);
        std::string name = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(assignment->m_target)->m_v);
        INFO(name);
        REQUIRE(expected.count(name) == 1);
        const auto* unavailable = ASRUtils::undefined_local_on_entry(
            assignment->m_value, function->m_symtab);
        CHECK((unavailable ? std::string(unavailable->m_name) : "") == expected.at(name));
        checked++;
    }
    CHECK(checked == expected.size());
}

TEST_CASE("GPU workspace predicates must be total at the capture point") {
    const std::string source = R"(
subroutine predicates(active, maybe, control, divisor)
    logical, intent(in) :: active
    logical, optional, intent(in) :: maybe
    real, allocatable, intent(in) :: control(:)
    integer, intent(in) :: divisor
    logical :: result
    result = active
    result = maybe
    result = size(control) > 0
    result = 10 / divisor > 0
end subroutine
)";
    CompilerOptions options;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = "predicates.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(LFortran::source_offset(source.size()));
    diag::Diagnostics diagnostics;
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    REQUIRE(parsed.ok);
    auto *function = ASR::down_cast<ASR::Function_t>(
        parsed.result->m_symtab->get_symbol("predicates"));
    std::set<ASR::symbol_t*> arguments;
    for (size_t i = 0; i < function->n_args; i++) {
        arguments.insert(ASR::down_cast<ASR::Var_t>(function->m_args[i])->m_v);
    }
    REQUIRE(function->n_body == 4);
    for (size_t i = 0; i < function->n_body; i++) {
        auto *assignment = ASR::down_cast<ASR::Assignment_t>(function->m_body[i]);
        CHECK(gpu_uniform_workspace_condition(assignment->m_value, arguments,
            function->m_symtab) == (i == 0));
    }
    auto *active = ASR::down_cast<ASR::Variable_t>(
        ASR::down_cast<ASR::Var_t>(function->m_args[0])->m_v);
    active->m_is_volatile = true;
    CHECK_FALSE(gpu_uniform_workspace_condition(function->m_args[0],
        arguments, function->m_symtab));
}

TEST_CASE("Every decline of the GPU offloading pipeline is an error") {
    Allocator allocator(1024 * 1024);
    Location loc{0, 0};
    ASR::ttype_t *real8 = ASRUtils::TYPE(ASR::make_Real_t(allocator, loc, 8));
    ASR::ttype_t *logical8 = ASRUtils::TYPE(ASR::make_Logical_t(allocator, loc, 8));
    GpuDecline wide_real(GpuDeclineReason::SymbolTypeNotRepresentable, "x", real8);
    GpuDecline wide_logical(GpuDeclineReason::ArrayElementTypeWidth, "x", logical8);

    // A missing lowering is an error, even though the loop could run on the
    // CPU.
    PassOptions options;
    options.gpu_offload_cuda = true;
    diag::Diagnostics missing;
    options.diagnostics = &missing;
    report_gpu_decline(options, loc, wide_logical);
    CHECK(missing.has_error());
    CHECK(missing.diagnostics.size() == 1);

    // So is a width the device has no type for, and the flag that softens
    // the unsupported list does not change that: the decline comes after the
    // loop was committed to the device, so the message suggests no flag.
    diag::Diagnostics limited;
    options.diagnostics = &limited;
    options.gpu_offload_cuda = false;
    options.gpu_offload_metal = true;
    options.gpu_allow_cpu_fallback = true;
    report_gpu_decline(options, loc, wide_real);
    CHECK(limited.has_error());
    REQUIRE(limited.diagnostics.size() == 1);
    CHECK(limited.diagnostics[0].message.find(
        gpu_decline_message(wide_real)) != std::string::npos);
    CHECK(limited.diagnostics[0].message.find("--gpu-allow-cpu-fallback")
        == std::string::npos);

    // So is showing the kernels: there is no host loop left to show instead.
    diag::Diagnostics shown;
    options.diagnostics = &shown;
    options.gpu_allow_cpu_fallback = false;
    options.gpu_offload_metal = false;
    options.gpu_offload_cuda = true;
    options.gpu_kernel_source_only = true;
    report_gpu_decline(options, loc, wide_logical);
    CHECK(shown.has_error());
    CHECK(shown.diagnostics.size() == 1);
}

// A loop that uses a construct on the unsupported list is reported for the
// construct the source wrote, before any lowering: a `print` lowers to an
// `iomsg` buffer of a type no device has, and naming that buffer would name
// something the source never mentions. Without the flag it is an error that
// suggests the flag; with it, the loop runs on the CPU with a warning.
TEST_CASE("An unsupported construct is reported where the source wrote it") {
    const std::string source = R"(
program p
    implicit none
    integer :: i
    real :: a(4)
    a = 0.0
    do concurrent (i = 1:4)
        a(i) = real(i)
        print *, a(i)
    end do
end program
)";
    auto compile = [&](bool allow_cpu_fallback) {
        CompilerOptions options;
        options.gpu_backend = "metal";
        options.po.gpu_offload_metal = true;
        options.po.gpu_allow_cpu_fallback = allow_cpu_fallback;
        options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
        FortranEvaluator evaluator(options);
        LocationManager lm;
        LocationManager::FileLocations file;
        file.in_filename = "gpu_unsupported_io.f90";
        lm.files.push_back(file);
        lm.file_ends.push_back(LCompilers::LFortran::source_offset(source.size()));
        diag::Diagnostics diagnostics;
        auto parsed = evaluator.get_asr2(source, lm, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE(parsed.ok);
        Allocator allocator(32 * 1024 * 1024);
        PassManager passes;
        std::string pass = "gpu_kernel_finalize", skip;
        passes.parse_pass_arg(pass, skip);
        options.po.pass_cumulative = true;
        passes.apply_passes(allocator, parsed.result, options.po, diagnostics);
        return diagnostics;
    };

    diag::Diagnostics strict = compile(false);
    std::string rendered = strict.render2();
    INFO(rendered);
    CHECK(strict.has_error());
    CHECK(rendered.find("input/output") != std::string::npos);
    CHECK(rendered.find("--gpu-allow-cpu-fallback") != std::string::npos);
    CHECK(rendered.find("iomsg") == std::string::npos);

    diag::Diagnostics allowed = compile(true);
    rendered = allowed.render2();
    INFO(rendered);
    CHECK_FALSE(allowed.has_error());
    CHECK(rendered.find("runs on the CPU") != std::string::npos);
    CHECK(rendered.find("input/output") != std::string::npos);
}

TEST_CASE("GPU layouts preserve identity, extents and device closure") {
    const std::string source = R"(
program p
    implicit none
    real :: a(2,3)
    call fill(3, a)
contains
    pure integer function twice(i)
        integer, intent(in) :: i
        twice = 2*i
    end function
    subroutine fill(n, a)
        integer, intent(in) :: n
        real, intent(out) :: a(2,3)
        integer :: i
        do concurrent (i=1:3)
            block
                real :: work(n)
                work = real(twice(i))
                a(1,i) = sum(work)
            end block
            block
                real :: work(n+1)
                work = real(i)
                a(2,i) = sum(work)
            end block
        end do
    end subroutine
end program
)";
    CompilerOptions options;
    options.gpu_backend = "cuda";
    options.po.gpu_offload_cuda = true;
    options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = "gpu_layout.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(LCompilers::LFortran::source_offset(source.size()));
    diag::Diagnostics diagnostics;
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(parsed.ok);
    ASR::TranslationUnit_t &unit = *parsed.result;
    Allocator allocator(32 * 1024 * 1024);
    PassManager passes;
    std::string pass = "gpu_kernel_finalize", skip;
    passes.parse_pass_arg(pass, skip);
    options.po.pass_cumulative = true;
    passes.apply_passes(allocator, &unit, options.po, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE_FALSE(diagnostics.has_error());
    REQUIRE(asr_verify(unit, true, diagnostics));
    ASR::Function_t *kernel = nullptr;
    for (auto &entry : unit.m_symtab->get_scope()) {
        if (ASRUtils::is_device_kernel(entry.second)) {
            REQUIRE(kernel == nullptr);
            kernel = ASR::down_cast<ASR::Function_t>(entry.second);
        }
    }
    REQUIRE(kernel != nullptr);
    REQUIRE(kernel->m_gpu != nullptr);
    auto &layout = *kernel->m_gpu;
    REQUIRE(layout.n_workspaces == 2);
    REQUIRE(layout.n_device_functions > 0);
    CHECK(layout.m_workspaces[0].m_variable != layout.m_workspaces[1].m_variable);
    CHECK(std::string(ASRUtils::symbol_name(layout.m_workspaces[0].m_variable)) ==
        ASRUtils::symbol_name(layout.m_workspaces[1].m_variable));
    CHECK(layout.m_workspaces[0].m_buffer_index !=
        layout.m_workspaces[1].m_buffer_index);
    CHECK(kernel->n_args == (size_t)layout.m_source_argument_count + 2);

    for (auto form : {ASRTextForm::Named, ASRTextForm::Positional}) {
        ASRTextOptions text_options;
        text_options.form = form;
        text_options.indent = false;
        std::string text = asr_to_text(unit, text_options);
        CHECK(text.find("gpu_kernel_layout") != std::string::npos);
        Allocator restored_allocator(32 * 1024 * 1024);
        LocationManager restored_lm;
        diag::Diagnostics restored_diagnostics;
        auto restored = asr_from_text(restored_allocator, text, "gpu.asr",
            restored_lm, restored_diagnostics);
        INFO(restored_diagnostics.render2());
        REQUIRE(restored.ok);
        REQUIRE(asr_verify(*restored.result, true, restored_diagnostics));
        CHECK(asr_to_text(*restored.result, text_options) == text);
    }
    {
        Allocator restored_allocator(32 * 1024 * 1024);
        auto *restored = ASR::down_cast2<ASR::TranslationUnit_t>(
            deserialize_asr(restored_allocator, serialize(unit), true, 0));
        fix_external_symbols(*restored, *restored->m_symtab);
        diag::Diagnostics restored_diagnostics;
        INFO(restored_diagnostics.render2());
        CHECK(asr_verify(*restored, true, restored_diagnostics));
    }
    auto rejects = [&](const char *code) {
        diag::Diagnostics invalid;
        CHECK_FALSE(asr_verify(unit, true, invalid));
        REQUIRE_FALSE(invalid.diagnostics.empty());
        CHECK(invalid.diagnostics.back().code == code);
    };
    auto &workspace = layout.m_workspaces[0];
    workspace.m_buffer_index++;
    rejects("asr.verify.gpu_layout.workspace_slot");
    workspace.m_buffer_index--;

    REQUIRE(layout.n_buffers > 0);
    auto role = layout.m_buffers[0].m_kind;
    layout.m_buffers[0].m_kind = ASR::gpu_argument_kindType::GpuScalar;
    rejects("asr.verify.gpu_layout.argument_role");
    layout.m_buffers[0].m_kind = role;

    size_t functions = layout.n_device_functions;
    layout.n_device_functions = 0;
    rejects("asr.verify.gpu_layout.device_call");
    layout.n_device_functions = functions;

    auto &dimension = workspace.m_dims[0];
    ASR::symbol_t *parameter = dimension.m_parameter;
    dimension.m_parameter = ASR::down_cast<ASR::Var_t>(kernel->m_args[0])->m_v;
    rejects("asr.verify.gpu_layout.extent_parameter");
    dimension.m_parameter = parameter;

    ASR::expr_t *extent = dimension.m_extent;
    // The DO CONCURRENT index-name `i` has construct scope, so semantics
    // gives it its own variable `i__dc`.
    ASR::symbol_t *index = kernel->m_symtab->get_symbol("i__dc");
    REQUIRE(index != nullptr);
    dimension.m_extent = ASRUtils::EXPR(
        ASR::make_Var_t(allocator, extent->base.loc, index));
    rejects("asr.verify.gpu_layout.host_evaluable_extent");
    dimension.m_extent = extent;
    diag::Diagnostics valid;
    CHECK(asr_verify(unit, true, valid));
}

TEST_CASE("GPU specialization preserves imported host interfaces") {
    const std::string source = R"(
module gpu_test_interface
    interface
        pure function c_abs(i) bind(c, name="abs") result(r)
            integer(4), value :: i
            integer(4) :: r
        end function
    end interface
end module
subroutine invoke(x)
    use gpu_test_interface
    integer, intent(inout) :: x
    x = c_abs(x)
end subroutine
)";
    CompilerOptions options;
    options.po.gpu_offload_cuda = true;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = "gpu_interface.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(LCompilers::LFortran::source_offset(source.size()));
    diag::Diagnostics diagnostics;
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(parsed.ok);
    auto &unit = *parsed.result;
    auto *kernel = ASR::down_cast<ASR::Function_t>(
        unit.m_symtab->get_symbol("invoke"));
    auto *module = ASR::down_cast<ASR::Module_t>(
        unit.m_symtab->get_symbol("gpu_test_interface"));
    auto *host = ASR::down_cast<ASR::Function_t>(
        module->m_symtab->get_symbol("c_abs"));
    ASRUtils::get_FunctionType(kernel)->m_exec_space = ASR::exec_spaceType::Kernel;
    Allocator allocator(1024 * 1024);
    pass_gpu_memory_space(allocator, unit, options.po);
    CHECK(ASRUtils::get_exec_space(*host) == ASR::exec_spaceType::Host);
    auto callees = ASRUtils::get_called_functions(kernel->m_body, kernel->n_body);
    REQUIRE(callees.size() == 1);
    auto *device = *callees.begin();
    CHECK(device != host);
    CHECK(ASRUtils::get_exec_space(*device) == ASR::exec_spaceType::Device);
    CHECK(std::string(ASRUtils::get_FunctionType(device)->m_bindc_name) == "abs");
    CHECK(asr_verify(unit, true, diagnostics));
}

namespace {

class GuardedGpuWork : public ASR::BaseWalkVisitor<GuardedGpuWork> {
public:
    std::vector<ASR::expr_t*> guards;
    std::vector<ASR::GpuKernelLaunch_t*> launches;
    std::map<ASR::symbol_t*, ASR::expr_t*> assignments;
    std::map<ASR::symbol_t*, size_t> assignment_counts;
    size_t guarded_allocations = 0, source_allocations = 0;
    size_t runtime_launches = 0, runtime_releases = 0;
    size_t empty_lane_guards = 0;

    void visit_Assignment(const ASR::Assignment_t &x) {
        if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
            auto *symbol = ASR::down_cast<ASR::Var_t>(x.m_target)->m_v;
            assignments[symbol] = x.m_value;
            assignment_counts[symbol]++;
        }
    }

    void visit_If(const ASR::If_t &x) {
        ASR::expr_t *count = nullptr;
        if (ASR::is_a<ASR::IntegerCompare_t>(*x.m_test)) {
            auto &test = *ASR::down_cast<ASR::IntegerCompare_t>(x.m_test);
            int64_t zero = -1;
            if (test.m_op == ASR::cmpopType::Gt
                    && ASRUtils::extract_value(
                        ASRUtils::expr_value(test.m_right), zero)
                    && zero == 0) {
                count = test.m_left;
            }
        }
        if (count && ASR::is_a<ASR::Var_t>(*count)) {
            auto found = assignments.find(ASR::down_cast<ASR::Var_t>(count)->m_v);
            if (found != assignments.end()
                    && ASR::is_a<ASR::IntegerBinOp_t>(*found->second)) {
                auto &value = *ASR::down_cast<ASR::IntegerBinOp_t>(found->second);
                int64_t lanes = -1;
                if (value.m_op == ASR::binopType::Mul
                        && ASRUtils::extract_value(
                            ASRUtils::expr_value(value.m_right), lanes)
                        && lanes == 0) {
                    empty_lane_guards++;
                }
            }
        }
        if (count) guards.push_back(count);
        for (size_t i = 0; i < x.n_body; i++) visit_stmt(*x.m_body[i]);
        if (count) guards.pop_back();
        for (size_t i = 0; i < x.n_orelse; i++) visit_stmt(*x.m_orelse[i]);
    }

    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
        REQUIRE(ASR::is_a<ASR::Var_t>(*x.m_thread_count));
        CHECK(assignment_counts[
            ASR::down_cast<ASR::Var_t>(x.m_thread_count)->m_v] == 1);
        bool guarded = false;
        for (ASR::expr_t *count : guards) {
            guarded |= ASR::is_a<ASR::Var_t>(*count)
                && ASR::down_cast<ASR::Var_t>(count)->m_v
                    == ASR::down_cast<ASR::Var_t>(x.m_thread_count)->m_v;
        }
        CHECK(guarded);
        launches.push_back(const_cast<ASR::GpuKernelLaunch_t*>(&x));
    }

    void visit_Allocate(const ASR::Allocate_t &) {
        if (guards.empty()) source_allocations++;
        else guarded_allocations++;
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        std::string name = ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(x.m_name));
        if (name == "lfortran_gpu_launch") runtime_launches++;
        if (name == "lfortran_gpu_release_kernel") runtime_releases++;
        if (name == "lfortran_gpu_launch"
                || name == "lfortran_gpu_release_kernel"
                || name == "lfortran_gpu_set_buffer_arg"
                || name == "lfortran_gpu_set_scalar_arg"
                || name == "lfortran_gpu_set_scratch_arg"
                || name == "lfortran_gpu_read_scratch_arg") {
            // The logical iteration guard encloses all offload lowering;
            // expansion also guards the effective thread count with lanes.
            CHECK(guards.size() >= 2);
        }
    }
};

// Runs the integer assignments the host makes before each launch, as far
// as they are made of constants and earlier ones, and records each launch's
// thread count and the value of its argument that selects tiles.
class GpuLaunchGeometry : public ASR::BaseWalkVisitor<GpuLaunchGeometry> {
public:
    std::map<ASR::symbol_t*, int64_t> values;
    std::vector<std::pair<int64_t, int64_t>> launches;

    bool evaluate(ASR::expr_t *x, int64_t &result) {
        if (ASRUtils::extract_value(ASRUtils::expr_value(x), result)) {
            return true;
        }
        int64_t left = 0, right = 0;
        switch (x->type) {
            case ASR::exprType::Var: {
                auto found = values.find(ASR::down_cast<ASR::Var_t>(x)->m_v);
                if (found == values.end()) return false;
                result = found->second;
                return true;
            }
            case ASR::exprType::Cast: {
                auto &cast = *ASR::down_cast<ASR::Cast_t>(x);
                return cast.m_kind == ASR::cast_kindType::IntegerToInteger
                    && evaluate(cast.m_arg, result);
            }
            case ASR::exprType::IntegerBinOp: {
                auto &op = *ASR::down_cast<ASR::IntegerBinOp_t>(x);
                if (!evaluate(op.m_left, left)
                        || !evaluate(op.m_right, right)) return false;
                switch (op.m_op) {
                    case ASR::binopType::Add: result = left + right; break;
                    case ASR::binopType::Sub: result = left - right; break;
                    case ASR::binopType::Mul: result = left * right; break;
                    case ASR::binopType::Div:
                        if (right == 0) return false;
                        result = left / right;
                        break;
                    default: return false;
                }
                return true;
            }
            case ASR::exprType::IntegerCompare: {
                auto &compare = *ASR::down_cast<ASR::IntegerCompare_t>(x);
                if (!evaluate(compare.m_left, left)
                        || !evaluate(compare.m_right, right)) return false;
                switch (compare.m_op) {
                    case ASR::cmpopType::Eq: result = left == right; break;
                    case ASR::cmpopType::NotEq: result = left != right; break;
                    case ASR::cmpopType::Lt: result = left < right; break;
                    case ASR::cmpopType::LtE: result = left <= right; break;
                    case ASR::cmpopType::Gt: result = left > right; break;
                    case ASR::cmpopType::GtE: result = left >= right; break;
                }
                return true;
            }
            case ASR::exprType::LogicalBinOp: {
                auto &op = *ASR::down_cast<ASR::LogicalBinOp_t>(x);
                if (!evaluate(op.m_left, left)
                        || !evaluate(op.m_right, right)) return false;
                switch (op.m_op) {
                    case ASR::logicalbinopType::And:
                        result = left && right;
                        break;
                    case ASR::logicalbinopType::Or:
                        result = left || right;
                        break;
                    default: return false;
                }
                return true;
            }
            case ASR::exprType::IfExp: {
                auto &choice = *ASR::down_cast<ASR::IfExp_t>(x);
                int64_t test = 0;
                return evaluate(choice.m_test, test) && evaluate(
                    test ? choice.m_body : choice.m_orelse, result);
            }
            default: return false;
        }
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        int64_t value = 0;
        if (ASR::is_a<ASR::Var_t>(*x.m_target)
                && ASRUtils::is_integer(*ASRUtils::expr_type(x.m_target))
                && evaluate(x.m_value, value)) {
            values[ASR::down_cast<ASR::Var_t>(x.m_target)->m_v] = value;
        }
    }

    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
        int64_t count = -1, tiled = -1;
        CHECK(evaluate(x.m_thread_count, count));
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::expr_t *arg = x.m_args[i].m_value;
            if (arg && ASR::is_a<ASR::Var_t>(*arg)
                    && startswith(ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(arg)->m_v), "__gpu_tiled")) {
                CHECK(evaluate(arg, tiled));
            }
        }
        launches.push_back({tiled, count});
    }
};

} // namespace

TEST_CASE("GPU zero work guards setup and effective launch geometry") {
    const std::string source = R"(
program p
    implicit none
    real :: a(32,4), s, g(5,7)
    real, allocatable :: outside(:)
    integer :: lo, hi, i, j
    lo = 1
    hi = 3
    allocate(outside(0))
    a = 0
    do concurrent (i = lo:hi)
        a(:,i) = a(:,i) + 1
    end do
    do concurrent (j = lo:7, i = 1:hi)
        g(i,j) = real(i + j)
    end do
    lo = 3
    hi = 1
    s = 5
    do concurrent (i = lo:hi) reduce(+:s)
        s = s + a(1,i)
    end do
end program
)";
    for (bool metal : {false, true}) {
        CompilerOptions options;
        options.gpu_backend = metal ? "metal" : "cuda";
        options.po.gpu_offload_metal = metal;
        options.po.gpu_offload_cuda = !metal;
        options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
        FortranEvaluator evaluator(options);
        LocationManager lm;
        LocationManager::FileLocations file;
        file.in_filename = "gpu_zero_work.f90";
        lm.files.push_back(file);
        lm.file_ends.push_back(LFortran::source_offset(source.size()));
        diag::Diagnostics diagnostics;
        auto parsed = evaluator.get_asr2(source, lm, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE(parsed.ok);
        ASR::TranslationUnit_t &unit = *parsed.result;
        Allocator allocator(32 * 1024 * 1024);
        PassManager passes;
        std::string pass = "gpu_vector_lanes", skip;
        passes.parse_pass_arg(pass, skip);
        options.po.pass_cumulative = true;
        passes.apply_passes(allocator, &unit, options.po, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE_FALSE(diagnostics.has_error());
        auto *program = ASR::down_cast<ASR::Program_t>(
            unit.m_symtab->get_symbol("p"));
        GuardedGpuWork work;
        for (size_t i = 0; i < program->n_body; i++) {
            work.visit_stmt(*program->m_body[i]);
        }
        REQUIRE(work.launches.size() == 3);
        CHECK(work.source_allocations == 1);
        CHECK(work.guarded_allocations >= 1);
        bool has_lanes = false;
        size_t wide_counts = 0;
        for (auto *launch : work.launches) {
            // The host counts the iterations of a nest, and the whole tiles
            // CUDA launches a two-dimensional one in, in integer(8) on every
            // device: the count can exceed the kind of the loop indices. A
            // Metal kernel still numbers its threads in 32 bits, and runs a
            // launch of more iterations in chunks.
            if (ASRUtils::extract_kind_from_ttype_t(
                    ASRUtils::expr_type(launch->m_thread_count)) == 8) {
                wide_counts++;
            }
            auto *kernel = ASR::down_cast<ASR::Function_t>(launch->m_kernel);
            if (kernel->m_gpu->m_lanes) {
                has_lanes = true;
                // Exercise the expansion contract independently of today's
                // lane selector, which chooses either one or 32 lanes.
                launch->m_args[launch->n_args - 1].m_value =
                    ASRUtils::EXPR(ASR::make_IntegerConstant_t(allocator,
                        launch->base.base.loc, 0,
                        ASRUtils::expr_type(
                            launch->m_args[launch->n_args - 1].m_value),
                        ASR::integerbozType::Decimal));
            }
        }
        CHECK(has_lanes == metal);
        CHECK(wide_counts == 3);
        pass_gpu_launch_sync(allocator, unit, options.po);
        pass_device_launch_expand(allocator, unit, options.po);
        REQUIRE(asr_verify(unit, true, diagnostics));
        GuardedGpuWork expanded;
        for (size_t i = 0; i < program->n_body; i++) {
            expanded.visit_stmt(*program->m_body[i]);
        }
        CHECK(expanded.launches.empty());
        // On both devices the three launches may run in chunks
        // (GpuKernelLaunch.chunk_dimensions): each launches its kernel once
        // when it fits in a chunk, and once per chunk in a loop otherwise.
        CHECK(expanded.runtime_launches == 6);
        CHECK(expanded.runtime_releases == 3);
        CHECK(expanded.source_allocations == 1);
        CHECK(expanded.empty_lane_guards == (metal ? 1 : 0));
    }
}

namespace {

// Runs the CUDA pipeline on the program `name` in `source`, with launches
// of more than `chunk_limit` iterations run in chunks, and hands `visit`
// each statement of its body.
template <class Visit>
void visit_cuda_program(const std::string &source,
        const std::string &filename, const std::string &name, Visit visit,
        int64_t chunk_limit = 2147483647, bool warps = false) {
    CompilerOptions options;
    options.gpu_backend = "cuda";
    options.po.gpu_offload_cuda = true;
    options.po.gpu_cuda_warps = warps;
    options.po.gpu_chunk_limit = chunk_limit;
    options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = filename;
    lm.files.push_back(file);
    lm.file_ends.push_back(LFortran::source_offset(source.size()));
    diag::Diagnostics diagnostics;
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(parsed.ok);
    ASR::TranslationUnit_t &unit = *parsed.result;
    Allocator allocator(32 * 1024 * 1024);
    PassManager passes;
    std::string pass = "gpu_vector_lanes", skip;
    passes.parse_pass_arg(pass, skip);
    options.po.pass_cumulative = true;
    passes.apply_passes(allocator, &unit, options.po, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE_FALSE(diagnostics.has_error());
    auto *program = ASR::down_cast<ASR::Program_t>(
        unit.m_symtab->get_symbol(name));
    for (size_t i = 0; i < program->n_body; i++) {
        visit(*program->m_body[i]);
    }
}

// The (tiled, thread count) pair of every CUDA launch of the program
// `name` in `source`, from the host's launch prelude.
std::vector<std::pair<int64_t, int64_t>> cuda_launch_geometry(
        const std::string &source, const std::string &filename,
        const std::string &name) {
    GpuLaunchGeometry geometry;
    visit_cuda_program(source, filename, name,
        [&](ASR::stmt_t &s) { geometry.visit_stmt(s); });
    return geometry.launches;
}

// How many arrays every CUDA launch of the program `name` in `source` says
// the kernel overwrites (write-discard).
std::vector<size_t> cuda_launch_overwrites(const std::string &source,
        const std::string &filename, const std::string &name) {
    struct Overwrites : public ASR::BaseWalkVisitor<Overwrites> {
        std::vector<size_t> launches;
        void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
            launches.push_back(x.n_overwritten);
        }
    } overwrites;
    visit_cuda_program(source, filename, name,
        [&](ASR::stmt_t &s) { overwrites.visit_stmt(s); });
    return overwrites.launches;
}

void check_launch_geometry(
        const std::vector<std::pair<int64_t, int64_t>> &launches,
        const std::vector<std::pair<int64_t, int64_t>> &expected) {
    REQUIRE(launches.size() == expected.size());
    for (size_t i = 0; i < launches.size(); i++) {
        INFO("launch " << i);
        CHECK(launches[i].first == expected[i].first);
        CHECK(launches[i].second == expected[i].second);
    }
}

// The WASM test binary runs under node without the source tree, so it
// cannot read the integration tests.
#ifndef HAVE_BUILD_TO_WASM
std::string read_project_file(const std::string &path) {
    std::ifstream in(std::string(LFORTRAN_PROJECT_SOURCE_DIR) + "/" + path);
    INFO(path);
    REQUIRE(in.good());
    std::stringstream contents;
    contents << in.rdbuf();
    return contents.str();
}
#endif

} // namespace

// The dimensions every CUDA launch of the program `name` in `source` splits
// into chunks of at most `chunk_limit` iterations, as the names of their
// bound parameters, and the iterations of the dimensions it covers whole
// (GpuKernelLaunch.chunk_dimensions and chunk_fixed_iterations).
std::vector<std::pair<std::vector<std::string>, int64_t>> cuda_launch_chunks(
        const std::string &source, const std::string &filename,
        const std::string &name, int64_t chunk_limit) {
    struct Chunks : public ASR::BaseWalkVisitor<Chunks> {
        std::vector<std::pair<std::vector<std::string>, int64_t>> launches;
        void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
            std::vector<std::string> bounds;
            for (size_t i = 0; i < x.n_chunk_dimensions; i++) {
                bounds.push_back(ASRUtils::symbol_name(
                    x.m_chunk_dimensions[i].m_start_parameter));
                bounds.push_back(ASRUtils::symbol_name(
                    x.m_chunk_dimensions[i].m_end_parameter));
            }
            launches.push_back({bounds, x.m_chunk_fixed_iterations});
        }
    } chunks;
    visit_cuda_program(source, filename, name,
        [&](ASR::stmt_t &s) { chunks.visit_stmt(s); }, chunk_limit);
    return chunks.launches;
}

TEST_CASE("GPU launches split only what can exceed the chunk limit") {
    // Dimensions with constant extents are covered whole by every chunk,
    // wherever they are numbered, while their extents fit the limit; a
    // nest the kinds of its indices and its constant bounds keep within
    // the limit is never split.
    const std::string source = R"(
program p
    implicit none
    integer :: n, m, i, j, k, c
    integer(8) :: n8, l
    integer :: a(8, 2, 2), x(100), w(0:100), v(100, 3), z(4, 5, 6)
    integer :: b(10, 10, 10), d(10, 10, 4), y(10)
    n = 2
    m = 100
    n8 = 10
    do concurrent (i = 1:8, j = 1:n, k = 1:n)
        a(i, j, k) = i + j + k
    end do
    do concurrent (i = 1:m)
        x(i) = i
    end do
    do concurrent (i = 0:m)
        w(i) = i
    end do
    do concurrent (i = 1:m, c = 1:3)
        v(i, c) = i * c
    end do
    do concurrent (i = 1:4, j = 1:5, k = 1:6)
        z(i, j, k) = i + j + k
    end do
    do concurrent (l = 1:n8)
        y(l) = int(l)
    end do
    do concurrent (i = 1:10, j = 1:10, k = 1:10)
        b(i, j, k) = i + j + k
    end do
    do concurrent (i = 1:10, j = 1:10, k = 1:4)
        d(i, j, k) = i + j + k
    end do
    print *, a(1, 1, 1), x(1), w(0), v(1, 1), z(1, 1, 1), y(1), b(1, 1, 1), &
        d(1, 1, 1)
end program
)";
    using Launches = std::vector<std::pair<std::vector<std::string>,
        int64_t>>;
    auto check = [](const Launches &launches, const Launches &expected) {
        REQUIRE(launches.size() == expected.size());
        for (size_t i = 0; i < launches.size(); i++) {
            INFO("launch " << i);
            CHECK(launches[i].first == expected[i].first);
            CHECK(launches[i].second == expected[i].second);
        }
    };
    std::vector<std::string> none;
    std::vector<std::string> first = {"__loop_start_0", "__loop_end_0"};
    std::vector<std::string> second = {"__loop_start_1", "__loop_end_1"};
    std::vector<std::string> second_third = {"__loop_start_1",
        "__loop_end_1", "__loop_start_2", "__loop_end_2"};
    check(cuda_launch_chunks(source, "gpu_chunks.f90", "p", 2147483647), {
        // `i` (8 values) is covered whole; `j` and `k` are split.
        {second_third, 8},
        // An integer(4) index from 1 takes at most huge(0) values.
        {none, 0},
        // From 0 it can take huge(0) + 1.
        {first, 1},
        // `c` (3 values), numbered after `i`, is still covered whole.
        {first, 3},
        // 120 constant iterations.
        {none, 0},
        // An integer(8) index.
        {first, 1},
        {none, 0},
        {none, 0},
    });
    check(cuda_launch_chunks(source, "gpu_chunks.f90", "p", 50), {
        {second_third, 8},
        {first, 1},
        {first, 1},
        {first, 3},
        // 4 * 5 = 20 iterations fit beside each other, 6 more do not.
        {{"__loop_start_2", "__loop_end_2"}, 20},
        {first, 1},
        // 10 values of `i` fit, and 100 of `i` and `j` do not.
        {second_third, 10},
        // The 4 values of `k` fit beside the 10 of `i`; `j` is split.
        {second, 40},
    });
}

TEST_CASE("GPU launches with reductions run in chunks") {
    // A launch with a reduction is split like any other, and its kernel
    // then takes the flag that tells it whether it runs the first chunk,
    // the only one whose threads start the accumulators. A launch that is
    // never split, or that has no accumulators, takes none.
    const std::string source = R"(
program p
    implicit none
    integer :: n, lo, i, j, s, x(0:10)
    real :: t
    n = 10
    lo = 0
    s = 0
    t = 0
    do concurrent (i = lo:n) reduce(+:s)
        s = s + i
    end do
    do concurrent (i = 1:n) reduce(+:s)
        s = s + i
    end do
    do concurrent (i = 1:4, j = 1:n) reduce(*:t)
        t = t * real(i + j)
    end do
    do concurrent (i = lo:n)
        x(i) = i
    end do
    print *, s, t, x(0)
end program
)";
    std::vector<std::pair<size_t, std::string>> launches;
    struct Launches : public ASR::BaseWalkVisitor<Launches> {
        std::vector<std::pair<size_t, std::string>> &launches;
        Launches(std::vector<std::pair<size_t, std::string>> &launches)
            : launches(launches) {}
        void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
            launches.push_back({x.n_chunk_dimensions,
                x.m_first_chunk_parameter == nullptr ? std::string()
                    : std::string(ASRUtils::symbol_name(
                        x.m_first_chunk_parameter))});
        }
    } visitor(launches);
    visit_cuda_program(source, "gpu_chunk_reductions.f90", "p",
        [&](ASR::stmt_t &s) { visitor.visit_stmt(s); });
    std::vector<std::pair<size_t, std::string>> expected = {
        // With a run-time start an integer(4) index can take more than
        // huge(0) values.
        {1, "__gpu_first_chunk"},
        // From 1 it takes at most huge(0).
        {0, ""},
        {1, "__gpu_first_chunk"},
        // No reduction.
        {1, ""},
    };
    REQUIRE(launches.size() == expected.size());
    for (size_t i = 0; i < launches.size(); i++) {
        INFO("launch " << i);
        CHECK(launches[i] == expected[i]);
    }
}

TEST_CASE("GPU launches with lanes are not tiled") {
    // The lanes of an iteration share a thread index, which a tiled kernel
    // reads as the place in its tile: only a fraction of each tile would
    // run. Where a launch gets lanes, the host passes __tiled = 0.
    const std::string source = R"(
program p
    implicit none
    integer, parameter :: hv = 48, dv = 128
    real :: state(dv, dv, hv), v(dv, hv), tot(dv, hv)
    integer :: h, row
    do concurrent (integer :: h = 1:hv, row = 1:dv)
        block
            state(:, row, h) = state(:, row, h)*2
            tot(row, h) = sum(state(:, row, h)*v(:, h))
        end block
    end do
    print *, tot(1, 1)
end program
)";
    struct Launches : public ASR::BaseWalkVisitor<Launches> {
        size_t tiled_with_lanes = 0, resets = 0, seen_resets = 0;
        void visit_If(const ASR::If_t &x) {
            // `if (__gpu_lanes > 1) __gpu_tiled = 0`
            if (x.n_body == 1 && x.n_orelse == 0
                    && ASR::is_a<ASR::Assignment_t>(*x.m_body[0])) {
                ASR::expr_t *target =
                    ASR::down_cast<ASR::Assignment_t>(x.m_body[0])->m_target;
                if (ASR::is_a<ASR::Var_t>(*target) && std::string(
                        ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(
                            target)->m_v)).rfind("__gpu_tiled", 0) == 0) {
                    seen_resets++;
                }
            }
            ASR::BaseWalkVisitor<Launches>::visit_If(x);
        }
        void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
            auto *kernel = ASR::down_cast<ASR::Function_t>(x.m_kernel);
            bool tiled = false;
            for (size_t i = 0; i < kernel->n_args; i++) {
                tiled = tiled || std::string(ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(kernel->m_args[i])->m_v))
                    .rfind("__tiled", 0) == 0;
            }
            if (tiled && kernel->m_gpu->m_lanes != nullptr) {
                tiled_with_lanes++;
                // The reset comes before the launch that uses it.
                if (seen_resets > resets) resets++;
            }
        }
    } launches;
    visit_cuda_program(source, "gpu_lanes_tiled.f90", "p",
        [&](ASR::stmt_t &s) { launches.visit_stmt(s); }, 2147483647, true);
    CHECK(launches.tiled_with_lanes == 1);
    CHECK(launches.resets == launches.tiled_with_lanes);
}

TEST_CASE("GPU tiled launches pad a nest by at most a quarter") {
    // CI has no GPU to time a launch, so pin the geometry the host picks:
    // whether a nest runs in tiles of 32 by 8, and how many threads.
    const std::string source = R"(
program p
    implicit none
    real :: a(4096,1), b(3,2,100), c(64,64,1), d(64,64), e(33,64)
    integer :: i, j, k
    do concurrent (j = 1:1, i = 1:4096)
        a(i,j) = 1
    end do
    do concurrent (i = 1:3, j = 1:2, k = 1:100)
        b(i,j,k) = 1
    end do
    do concurrent (k = 1:1, j = 1:64, i = 1:64)
        c(i,j,k) = 1
    end do
    do concurrent (j = 2:63, i = 2:63)
        d(i,j) = 1
    end do
    do concurrent (j = 1:64, i = 1:33)
        e(i,j) = 1
    end do
end program
)";
    std::vector<std::pair<int64_t, int64_t>> launches =
        cuda_launch_geometry(source, "gpu_tile_geometry.f90", "p");
    std::vector<std::pair<int64_t, int64_t>> expected = {
        // A second dimension of one would pad 8 times: flat.
        {0, 4096},
        // Both tiled dimensions short (3 by 2): flat.
        {0, 600},
        // The tile's height runs along j, the dimension the store steps
        // through next, not along k, the next in loop order.
        {1, 4096},
        // A stencil whose extents are not whole tiles: 2 by 8 tiles.
        {1, 4096},
        // 33 is padded to 64: flat.
        {0, 33 * 64},
    };
    check_launch_geometry(launches, expected);
}

TEST_CASE("GPU constant nests decide tiles and chunks at compile time") {
    // A nest whose extents are all constants runs in tiles only where its
    // whole tiles come to at most the chunk limit; otherwise its kernel
    // numbers the iterations flat (no tile flag, -1 below), so a nest of at
    // most the limit iterations runs as one launch, without chunks.
    const std::string source = R"(
program p
    implicit none
    real, allocatable :: y(:,:)
    real :: a(64, 7), b(64, 8), c(32, 8), d(64, 64)
    integer :: i, j
    allocate(y(64, 33554431))
    do concurrent (i = 1:64, j = 1:33554431)
        y(i, j) = 1
    end do
    do concurrent (i = 1:64, j = 1:7)
        a(i, j) = 1
    end do
    do concurrent (i = 1:64, j = 1:8)
        b(i, j) = 1
    end do
    do concurrent (i = 1:32, j = 1:8)
        c(i, j) = 1
    end do
    do concurrent (i = 1:64, j = 1:64)
        d(i, j) = 1
    end do
end program
)";
    struct Chunks : public ASR::BaseWalkVisitor<Chunks> {
        std::vector<size_t> dimensions;
        void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
            dimensions.push_back(x.n_chunk_dimensions);
        }
    };
    auto check = [&](int64_t chunk_limit,
            const std::vector<std::pair<int64_t, int64_t>> &geometry,
            const std::vector<size_t> &chunk_dimensions) {
        INFO("chunk limit " << chunk_limit);
        GpuLaunchGeometry launches;
        Chunks chunks;
        visit_cuda_program(source, "gpu_constant_tiles.f90", "p",
            [&](ASR::stmt_t &s) {
                launches.visit_stmt(s);
                chunks.visit_stmt(s);
            }, chunk_limit);
        check_launch_geometry(launches.launches, geometry);
        CHECK(chunks.dimensions == chunk_dimensions);
    };
    check(2147483647, {
        // 2147483584 iterations, at most huge(0); tiles would start 2**31.
        {-1, int64_t(64) * 33554431},
        // 64 by 7 is padded to 64 by 8 within a quarter: tiled.
        {1, 64 * 8},
        {1, 64 * 8},
        {1, 32 * 8},
        {1, 64 * 64},
    }, {0, 0, 0, 0, 0});
    check(480, {
        {-1, int64_t(64) * 33554431},
        // 448 iterations fit, 512 threads in tiles do not: flat, one launch.
        {-1, 64 * 7},
        // 512 iterations do not fit: flat, in chunks of `j`.
        {-1, 64 * 8},
        // One whole tile of 256 threads.
        {1, 32 * 8},
        {-1, 64 * 64},
    }, {1, 0, 1, 0, 1});
    check(512, {
        {-1, int64_t(64) * 33554431},
        // Whole tiles of exactly the limit stay tiled, and unsplit.
        {1, 64 * 8},
        {1, 64 * 8},
        {1, 32 * 8},
        {-1, 64 * 64},
    }, {1, 0, 0, 0, 1});
}

TEST_CASE("GPU tiled launches never start more threads than the chunk limit") {
    // `j` has a run-time end, but its kind and constant start leave it at
    // most 127 values, so neither nest can have more iterations than a
    // limit of 4000 and neither runs in chunks. Whole tiles of the first, 30 by at
    // most 127, start 32 by 128 threads: within the default limit it runs
    // tiled, past a limit of 4000 it runs flat, one thread per iteration
    // (gpu_metal_706).
    const std::string source = R"(
program p
    implicit none
    integer :: a(30, 127), b(31, 64), i, j, n
    n = huge(0)
    do concurrent (i = 1:30, j = huge(0) - 126:n)
        a(i, j - huge(0) + 127) = i
    end do
    n = huge(0) - 63
    do concurrent (i = 1:31, j = huge(0) - 126:n)
        b(i, j - huge(0) + 127) = i
    end do
end program
)";
    auto check = [&](int64_t chunk_limit,
            const std::vector<std::pair<int64_t, int64_t>> &geometry) {
        INFO("chunk limit " << chunk_limit);
        GpuLaunchGeometry launches;
        size_t chunked = 0;
        visit_cuda_program(source, "gpu_tile_limit.f90", "p",
            [&](ASR::stmt_t &s) {
                launches.visit_stmt(s);
                struct Chunks : public ASR::BaseWalkVisitor<Chunks> {
                    size_t n = 0;
                    void visit_GpuKernelLaunch(
                            const ASR::GpuKernelLaunch_t &x) {
                        n += x.n_chunk_dimensions;
                    }
                } chunks;
                chunks.visit_stmt(s);
                chunked += chunks.n;
            }, chunk_limit);
        check_launch_geometry(launches.launches, geometry);
        CHECK(chunked == 0);
    };
    check(2147483647, {{1, 32 * 128}, {1, 32 * 64}});
    // The second nest, 31 by 64, still runs in tiles of 32 by 64 threads.
    check(4000, {{0, 30 * 127}, {1, 32 * 64}});
}

#ifndef HAVE_BUILD_TO_WASM
TEST_CASE("A private map feeds an ordered reduction without a workspace") {
    std::string source = read_project_file(
        "integration_tests/gpu_private_reduction_01.f90");
    for (const std::string transforms : {"all", "none"}) {
        INFO(transforms);
        CompilerOptions options;
        options.gpu_backend = "metal";
        options.po.gpu_offload_metal = true;
        options.po.gpu_loop_transforms = transforms;
        options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
        FortranEvaluator evaluator(options);
        LocationManager lm;
        LocationManager::FileLocations file;
        file.in_filename = "gpu_private_reduction_01.f90";
        lm.files.push_back(file);
        lm.file_ends.push_back(LFortran::source_offset(source.size()));
        diag::Diagnostics diagnostics;
        auto parsed = evaluator.get_asr2(source, lm, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE(parsed.ok);
        Allocator allocator(32 * 1024 * 1024);
        PassManager passes;
        passes.use_default_passes();
        passes.apply_passes(allocator, parsed.result, options.po, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE_FALSE(diagnostics.has_error());
        REQUIRE(asr_verify(*parsed.result, true, diagnostics));
        size_t kernels = 0;
        for (const auto &entry: parsed.result->m_symtab->get_scope()) {
            if (!ASRUtils::is_device_kernel(entry.second)) continue;
            auto* kernel = ASR::down_cast<ASR::Function_t>(entry.second);
            REQUIRE(kernel->m_gpu != nullptr);
            CHECK(kernel->m_gpu->n_workspaces == (transforms == "all" ? 0 : 1));
            kernels++;
        }
        CHECK(kernels == 1);
    }
}

TEST_CASE("Private reduction fusion retains escaping mutated and reused maps") {
    // A map that the forwarding pass recomputes at its reads keeps no
    // workspace, which is what the escaping copy and the section fold are.
    auto check = [](const std::string &extent, const std::string &consumer,
            const std::string &attributes = "", size_t workspaces = 1) {
        INFO(extent << " / " << consumer << " / " << attributes);
        std::string source = R"(
subroutine map(input,x,y,saved)
    real, contiguous, intent(in) :: input(:,:), x(:)
    real, contiguous, intent(out) :: y(:,:), saved(:,:)
    do concurrent (integer :: n = 1:size(y,2))
        block
            real)" + attributes + R"( :: codes()" + extent + R"()
            do concurrent (integer :: d = 1:)" + extent + R"()
                codes(d) = input(d,n)
            end do
            )" + consumer + R"(
        end block
    end do
end subroutine
)";
        CompilerOptions options;
        options.gpu_backend = "metal";
        options.po.gpu_offload_metal = true;
        options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
        FortranEvaluator evaluator(options);
        LocationManager lm;
        LocationManager::FileLocations file;
        file.in_filename = "private_reduction_fallback.f90";
        lm.files.push_back(file);
        lm.file_ends.push_back(LFortran::source_offset(source.size()));
        diag::Diagnostics diagnostics;
        auto parsed = evaluator.get_asr2(source, lm, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE(parsed.ok);
        Allocator allocator(32 * 1024 * 1024);
        PassManager passes;
        passes.use_default_passes();
        passes.apply_passes(allocator, parsed.result, options.po, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE_FALSE(diagnostics.has_error());
        REQUIRE(asr_verify(*parsed.result, true, diagnostics));
        size_t kernels = 0;
        for (const auto &entry: parsed.result->m_symtab->get_scope()) {
            if (!ASRUtils::is_device_kernel(entry.second)) continue;
            auto* kernel = ASR::down_cast<ASR::Function_t>(entry.second);
            REQUIRE(kernel->m_gpu != nullptr);
            CHECK(kernel->m_gpu->n_workspaces == workspaces);
            kernels++;
        }
        CHECK(kernels == 1);
    };
    check("size(x)", "saved(:,n)=codes\ny(1,n)=sum(codes*x)", "", 0);
    check("size(x)", "codes(1)=codes(1)+1\ny(1,n)=sum(codes*x)");
    check("size(x)", "do concurrent(integer :: m=1:2)\ny(m,n)=sum(codes*x)\nend do");
    check("size(x)+4", "y(1,n)=sum(codes(:size(x))*x)", "", 0);
    check("size(x)", "y(1,n)=sum(codes*x)", ", target");
    check("size(x)", "y(1,n)=sum(codes*x)", ", volatile");
    check("size(x)", "y(1,n)=sum(codes*x)+sum(codes*x)");
    check("size(x)", "y(1,n)=sum(codes*x)\nblock\nreal :: shape_user(size(codes))\n"
        "shape_user=0\ny(2,n)=sum(shape_user)\nend block");
}

TEST_CASE("Scalar call actual reductions do not reserve a product heap") {
    std::string source = read_project_file(
        "integration_tests/gpu_scalar_reduction_actual_01.f90");
    CompilerOptions options;
    options.gpu_backend = "metal";
    options.po.gpu_offload_metal = true;
    options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = "gpu_scalar_reduction_actual_01.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(LFortran::source_offset(source.size()));
    diag::Diagnostics diagnostics;
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(parsed.ok);
    Allocator allocator(32 * 1024 * 1024);
    PassManager passes;
    passes.use_default_passes();
    passes.apply_passes(allocator, parsed.result, options.po, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE_FALSE(diagnostics.has_error());
    REQUIRE(asr_verify(*parsed.result, true, diagnostics));
    struct HeapReservations: ASR::BaseWalkVisitor<HeapReservations> {
        size_t count = 0;
        void visit_GpuAtomicFetchAdd(const ASR::GpuAtomicFetchAdd_t&) { count++; }
    } reservations;
    reservations.visit_TranslationUnit(*parsed.result);
    CHECK(reservations.count == 0);
    size_t kernels = 0;
    for (const auto &entry: parsed.result->m_symtab->get_scope()) {
        if (!ASRUtils::is_device_kernel(entry.second)) continue;
        auto* kernel = ASR::down_cast<ASR::Function_t>(entry.second);
        REQUIRE(kernel->m_gpu != nullptr);
        // Codes and weights remain distinct workspaces; terms and
        // partial_values, which only a sum reads, need none.
        CHECK(kernel->m_gpu->n_workspaces == 2);
        kernels++;
    }
    CHECK(kernels == 1);
}

TEST_CASE("Scalar reduction actual normalization respects evaluation domains") {
    auto check = [](const std::string& attributes,
            const std::string& statement, bool materialized) {
        INFO(attributes << " / " << statement);
        const std::string source = R"(
module scalar_actual_domains
contains
    pure elemental real function f(x) result(r)
        real, intent(in) :: x
        r = x
    end function
    real function effect(x) result(r)
        real, intent(inout) :: x
        x = x + 1
        r = x
    end function
    subroutine test(n,a,out,state)
        integer, intent(in) :: n
        real, intent(in))" + attributes + R"( :: a(n,2)
        real, intent(inout) :: out(2)
        real, intent(inout) :: state
        )" + statement + R"(
    end subroutine
end module
)";
        CompilerOptions options;
        FortranEvaluator evaluator(options);
        LocationManager lm;
        LocationManager::FileLocations file;
        file.in_filename = "scalar_actual_domains.f90";
        lm.files.push_back(file);
        lm.file_ends.push_back(LFortran::source_offset(source.size()));
        diag::Diagnostics diagnostics;
        auto parsed = evaluator.get_asr2(source, lm, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE(parsed.ok);
        Allocator allocator(32 * 1024 * 1024);
        PassManager passes;
        std::string pass = "array_struct_temporary", skip;
        passes.parse_pass_arg(pass, skip);
        passes.apply_passes(allocator, parsed.result, options.po, diagnostics);
        REQUIRE_FALSE(diagnostics.has_error());
        REQUIRE(asr_verify(*parsed.result, true, diagnostics));
        auto* module = ASR::down_cast<ASR::Module_t>(
            parsed.result->m_symtab->get_symbol("scalar_actual_domains"));
        auto* function = ASR::down_cast<ASR::Function_t>(
            module->m_symtab->get_symbol("test"));
        struct Allocations: ASRUtils::BlockBodyWalkVisitor<Allocations> {
            size_t count = 0;
            void visit_Allocate(const ASR::Allocate_t&) { count++; }
        } allocations;
        for (size_t i = 0; i < function->n_body; i++) {
            allocations.visit_stmt(*function->m_body[i]);
        }
        CHECK((allocations.count > 0) == materialized);
    };
    check("", "out(1)=f(sum(a(:,1)*2))", false);
    check("", "out(1)=f(sum(a*2))", false);
    check("", "out(1)=f(sum(a(:0,1)*2,mask=a(:0,1)>0))", false);
    check(", volatile", "out(1)=f(sum(a(:,1)*2))", true);
    check(", asynchronous", "out(1)=f(sum(a(:,1)*2))", true);
    check("", "block\nvolatile :: a\nout(1)=f(sum(a(:,1)*2))\nend block", true);
    check("", "block\nvolatile :: a\nend block\nout(1)=f(sum(a(:,1)*2))", false);
    check("", "out(1)=f(sum(a(:,1)*2))+effect(state)", true);
    check("", "out=f(sum(a*2,dim=1))", true);
    check("", "where(out>0) out=f(sum(a(:,1)*2))", true);
}

TEST_CASE("GPU workspace guards preserve shapes and reject private predicates") {
    bool polarity = false, nested = false;
    SUBCASE("data, bounds and conservative fallbacks") {}
    SUBCASE("equivalent predicates with reversed polarity") { polarity = true; }
    SUBCASE("nested concurrent domains and unguarded data uses") { nested = true; }
    std::string filename = nested ? "gpu_workspace_guard_03.f90"
        : polarity ? "gpu_workspace_guard_02.f90" : "gpu_workspace_guard_01.f90";
    const std::string source = read_project_file("integration_tests/" + filename);
    CompilerOptions options;
    options.gpu_backend = "cuda";
    options.po.gpu_offload_cuda = true;
    options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = filename;
    lm.files.push_back(file);
    lm.file_ends.push_back(LFortran::source_offset(source.size()));
    diag::Diagnostics diagnostics;
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(parsed.ok);
    Allocator allocator(32 * 1024 * 1024);
    PassManager passes;
    passes.use_default_passes();
    passes.apply_passes(allocator, parsed.result, options.po, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE_FALSE(diagnostics.has_error());
    REQUIRE(asr_verify(*parsed.result, true, diagnostics));
    size_t workspaces = 0, guarded = 0, negative = 0;
    for (const auto &entry : parsed.result->m_symtab->get_scope()) {
        if (!ASRUtils::is_device_kernel(entry.second)) continue;
        auto *kernel = ASR::down_cast<ASR::Function_t>(entry.second);
        REQUIRE(kernel->m_gpu != nullptr);
        for (size_t w = 0; w < kernel->m_gpu->n_workspaces; w++) {
            auto &workspace = kernel->m_gpu->m_workspaces[w];
            workspaces++;
            REQUIRE(workspace.n_dims == 1);
            CHECK(ASRUtils::extract_kind_from_ttype_t(
                ASRUtils::expr_type(workspace.m_dims[0].m_extent)) == 4);
            if (nested) {
                std::string name = ASRUtils::symbol_name(workspace.m_variable);
                INFO(name);
                CHECK((workspace.m_allocation_if != nullptr) ==
                    (name == "nested_work" || name == "inherited_work"));
            }
            if (!workspace.m_allocation_if) continue;
            guarded++;
            negative += !workspace.m_allocation_on_true;
            REQUIRE(workspace.m_allocation_parameter != nullptr);
            CHECK(ASR::is_a<ASR::Var_t>(*workspace.m_allocation_if));
            workspace.m_allocation_on_true = !workspace.m_allocation_on_true;
            diag::Diagnostics invalid;
            CHECK_FALSE(asr_verify(*parsed.result, true, invalid));
            REQUIRE_FALSE(invalid.diagnostics.empty());
            CHECK(invalid.diagnostics.back().code ==
                "asr.verify.gpu_layout.allocation_dominance");
            workspace.m_allocation_on_true = !workspace.m_allocation_on_true;
        }
    }
    CHECK(workspaces == (nested ? 5 : polarity ? 1 : 4));
    CHECK(guarded == (nested ? 2 : polarity ? 1 : 3));
    CHECK(negative == (nested || polarity ? 1 : 0));
    for (auto form : {ASRTextForm::Named, ASRTextForm::Positional}) {
        ASRTextOptions text_options;
        text_options.form = form;
        std::string text = asr_to_text(*parsed.result, text_options);
        Allocator restored_allocator(32 * 1024 * 1024);
        LocationManager restored_lm;
        diag::Diagnostics restored_diagnostics;
        auto restored = asr_from_text(restored_allocator, text, "guards.asr",
            restored_lm, restored_diagnostics);
        INFO(restored_diagnostics.render2());
        REQUIRE(restored.ok);
        CHECK(asr_verify(*restored.result, true, restored_diagnostics));
        CHECK(asr_to_text(*restored.result, text_options) == text);
    }
}

TEST_CASE("GPU tiled launch tests run tiled nests") {
    // gpu_metal_485, gpu_metal_490 and gpu_metal_491 check the tiled index
    // computation only if their nests do tile; pin which ones do, so that a
    // change of the padding threshold cannot silently move them to flat
    // numbering.
    std::vector<std::pair<int64_t, int64_t>> expected = {
        {1, 64 * 64},           // stencil 62 x 62
        {0, 3 * 1000},          // b(3, 1000)
        {1, 64 * 32 * 4},       // c: -3:56, 0:29, 3:6
        {1, 64 * 32},           // d: 3:64 x 3:33
        {1, 0},                 // empty nest
        {0, 300},               // f(300, 1)
        {0, 600},               // g(300, 2)
        {0, 40 * 37},           // h(40, 37, 1)
        {0, 3 * 2 * 500},       // p(3, 2, 500)
        {1, 64 * 32 * 4},       // contiguous dimension in the middle
        {1, 64 * 32 * 4},       // stores disagree: k, not j (4), down
        {1, 32 * 64},           // transposed store
        {1, 0},                 // empty dimension outside the tile
    };
    check_launch_geometry(cuda_launch_geometry(
        read_project_file("integration_tests/gpu_metal_485.f90"),
        "gpu_metal_485.f90", "gpu_metal_485"), expected);
    expected = {
        {1, 256 * 32},          // integer(8) indices, -5:250 x 1:31
    };
    check_launch_geometry(cuda_launch_geometry(
        read_project_file("integration_tests/gpu_metal_491.f90"),
        "gpu_metal_491.f90", "gpu_metal_491"), expected);
    expected = {
        {1, 64 * 64},           // workspace, 62 x 62
        {1, 64 * 32 * 3},       // workspace, 60 x 30 x 3
        {0, 37 * 11},           // workspace, 37 x 11
    };
    check_launch_geometry(cuda_launch_geometry(
        read_project_file("integration_tests/gpu_metal_490.f90"),
        "gpu_metal_490.f90", "gpu_metal_490"), expected);
    // Index-set splitting splits the first two nests, whose bodies test
    // a loop index against its lower bound: each becomes the split nest
    // and the original nest, which runs when the domain has no interior.
    // Both must tile.
    expected = {
        {1, 64 * 64},           // high lower bound along the tile width
        {1, 64 * 64},           // the same nest, unsplit
        {1, 64 * 64},           // high lower bound along the tile height
        {1, 64 * 64},           // the same nest, unsplit
        {1, 64 * 32},           // negative lower bounds, partial tiles
        {0, 33 * 31},           // high lower bound, flat fallback
    };
    for (const std::string kind : {"i4", "i8"}) {
        std::string name = "gpu_tile_high_bounds_" + kind;
        check_launch_geometry(cuda_launch_geometry(
            read_project_file("integration_tests/" + name + ".f90"),
            name + ".f90", name), expected);
    }
}

TEST_CASE("Time skewing leaves the procedures parallel loops call") {
    // `smooth` runs inside the parallel loop of the program, where its own
    // parallel loops run serially: its time loop is not skewed (the skewed
    // host loop would call the runtime from device code).
    const std::string source = R"(
module skew_callee_m
implicit none
contains
    pure subroutine smooth(u, d, n, nt)
        integer, intent(in) :: n, nt
        real, intent(inout) :: u(n), d(n)
        integer :: it, k
        do it = 1, nt
            do concurrent (k = 2:n - 1)
                d(k) = 0.25 * (u(k - 1) + 2 * u(k) + u(k + 1))
            end do
            do concurrent (k = 2:n - 1)
                u(k) = d(k)
            end do
        end do
    end subroutine
end module

program skew_callee
use skew_callee_m, only: smooth
implicit none
integer, parameter :: n = 3000, m = 4
real :: u(n, m), d(n, m)
integer :: j
u = 1
d = 0
call smooth(u(:, 1), d(:, 1), n, 5)
do concurrent (j = 1:m)
    call smooth(u(:, j), d(:, j), n, 5)
end do
print *, sum(u)
end program
)";
    CompilerOptions options;
    options.gpu_backend = "cuda";
    options.po.gpu_offload_cuda = true;
    options.po.gpu_cache_bytes = 4096;
    options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = "skew_callee.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(LFortran::source_offset(source.size()));
    diag::Diagnostics diagnostics;
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(parsed.ok);
    ASR::TranslationUnit_t &unit = *parsed.result;
    Allocator allocator(32 * 1024 * 1024);
    PassManager passes;
    std::string pass = "gpu_loop_transforms", skip;
    passes.parse_pass_arg(pass, skip);
    options.po.pass_cumulative = true;
    passes.apply_passes(allocator, &unit, options.po, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE_FALSE(diagnostics.has_error());
    auto *module = ASR::down_cast<ASR::Module_t>(
        unit.m_symtab->get_symbol("skew_callee_m"));
    auto *smooth = ASR::down_cast<ASR::Function_t>(
        module->m_symtab->get_symbol("smooth"));
    for (auto &item : smooth->m_symtab->get_scope()) {
        INFO(item.first);
        CHECK_FALSE(startswith(item.first, "__gpu_skew"));
    }
}

TEST_CASE("GPU reduction accumulators are overwritten, not copied in") {
    // Every kernel of a reduction sets each of its accumulators before it
    // reads it, so every launch hands them over write-discard: the first
    // loop's one, then the second loop's two, and nothing else.
    struct Accumulators : public ASR::BaseWalkVisitor<Accumulators> {
        std::vector<std::vector<std::string>> launches;
        void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
            std::vector<std::string> names;
            for (size_t i = 0; i < x.n_overwritten; i++) {
                REQUIRE(ASR::is_a<ASR::Var_t>(*x.m_overwritten[i]));
                names.push_back(ASRUtils::symbol_name(ASR::down_cast<
                    ASR::Var_t>(x.m_overwritten[i])->m_v));
                CHECK(ASR::is_a<ASR::LogicalConstant_t>(
                    *x.m_overwritten_if[i]));
            }
            launches.push_back(names);
        }
    } accumulators;
    visit_cuda_program(
        read_project_file("integration_tests/gpu_reduction_overwrite_01.f90"),
        "gpu_reduction_overwrite_01.f90", "gpu_reduction_overwrite_01",
        [&](ASR::stmt_t &s) { accumulators.visit_stmt(s); });
    REQUIRE(accumulators.launches.size() == 2);
    std::vector<size_t> expected = {1, 2};
    for (size_t i = 0; i < expected.size(); i++) {
        INFO("launch " << i);
        REQUIRE(accumulators.launches[i].size() == expected[i]);
        for (const std::string &name : accumulators.launches[i]) {
            INFO(name);
            CHECK(startswith(name, "__gpu_red_"));
        }
    }
}

TEST_CASE("GPU split nests keep the write-discard of the loops they split") {
    // gpu_metal_527 splits a 1-D stencil (the split nest, then the original
    // nest for a domain without an interior) and a 2-D one (the jammed
    // nest, the plain split nest, the original nest). Each overwrites one
    // array, which every launch must say.
    std::vector<size_t> expected = {1, 1, 1, 1, 1};
    std::vector<size_t> launches = cuda_launch_overwrites(
        read_project_file("integration_tests/gpu_metal_527.f90"),
        "gpu_metal_527.f90", "gpu_metal_527");
    REQUIRE(launches.size() == expected.size());
    for (size_t i = 0; i < launches.size(); i++) {
        INFO("launch " << i);
        CHECK(launches[i] == expected[i]);
    }
}

TEST_CASE("GPU optional views preserve known contiguity without host copies") {
    std::string source = read_project_file(
        "integration_tests/gpu_optional_launch_04.f90");
    for (bool metal : {false, true}) {
        CompilerOptions options;
        options.gpu_backend = metal ? "metal" : "cuda";
        options.po.gpu_offload_metal = metal;
        options.po.gpu_offload_cuda = !metal;
        options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
        FortranEvaluator evaluator(options);
        LocationManager lm;
        LocationManager::FileLocations file;
        file.in_filename = "gpu_optional_launch_04.f90";
        lm.files.push_back(file);
        lm.file_ends.push_back(LFortran::source_offset(source.size()));
        diag::Diagnostics diagnostics;
        auto parsed = evaluator.get_asr2(source, lm, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE(parsed.ok);
        Allocator allocator(32 * 1024 * 1024);
        PassManager passes;
        std::string pass = "device_launch_expand", skip;
        passes.parse_pass_arg(pass, skip);
        options.po.pass_cumulative = true;
        passes.apply_passes(allocator, parsed.result, options.po, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE_FALSE(diagnostics.has_error());
        REQUIRE(asr_verify(*parsed.result, true, diagnostics));
        auto *module = ASR::down_cast<ASR::Module_t>(
            parsed.result->m_symtab->get_symbol("gpu_optional_launch_04_m"));
        for (auto name : {"explicit_input", "contiguous_input", "strided_update"}) {
            INFO(name);
            // The body is in the deferring twin gpu_launch_sync makes.
            auto *function = ASR::down_cast<ASR::Function_t>(
                module->m_symtab->get_symbol(std::string(name)
                    + "__gpu_deferred"));
            struct Allocations : public ASR::BaseWalkVisitor<Allocations> {
                size_t count = 0;
                void visit_Allocate(const ASR::Allocate_t &x) {
                    count += x.n_args;
                }
            } allocations;
            for (size_t i = 0; i < function->n_body; i++) {
                allocations.visit_stmt(*function->m_body[i]);
            }
            CHECK(allocations.count == (std::string(name) == "strided_update" ? 1 : 0));
        }
    }
}
namespace {

// The GpuSync statements in the body of the `do` loop over `var`.
class LoopSyncs : public ASR::BaseWalkVisitor<LoopSyncs> {
public:
    std::string var;
    int depth = 0;
    size_t syncs = 0;
    size_t launches = 0;
    void visit_DoLoop(const ASR::DoLoop_t &x) {
        bool inner = ASR::is_a<ASR::Var_t>(*x.m_head.m_v)
            && ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(
                x.m_head.m_v)->m_v) == var;
        if (inner) depth++;
        BaseWalkVisitor::visit_DoLoop(x);
        if (inner) depth--;
    }
    void visit_GpuSync(const ASR::GpuSync_t &/*x*/) {
        if (depth > 0) syncs++;
    }
    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &/*x*/) {
        if (depth > 0) launches++;
    }
};

}

TEST_CASE("GPU launches are not synced for a call handed their arrays") {
    // `sum(a)` is a call to a procedure whose dummy is neither a pointer nor
    // a target, which cannot keep a pointer to `a` once it returns. The
    // later calls of the time loop (the one `mod` is) therefore cannot
    // reach `a` through a pointer, and the launches of the loop, which
    // write it, run without a sync between them.
    const std::string source = R"(
program sync_call_args
implicit none
integer :: n, it, k
real(8), allocatable :: a(:), b(:)
real(8) :: chk
integer :: i
n = 100000
allocate(a(n), b(n))
a = 0
b = 1
do k = 1, 2
    do it = 1, 8
        if (mod(it, 2) == 1) then
            do concurrent (i = 1:n)
                a(i) = b(i) + 1
            end do
        else
            do concurrent (i = 1:n)
                b(i) = a(n + 1 - i) + 1
            end do
        end if
    end do
    if (k == 1) chk = sum(a)
end do
print *, chk, a(1)
end program
)";
    CompilerOptions options;
    options.gpu_backend = "cuda";
    options.po.gpu_offload_cuda = true;
    options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = "sync_call_args.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(LFortran::source_offset(source.size()));
    diag::Diagnostics diagnostics;
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(parsed.ok);
    ASR::TranslationUnit_t &unit = *parsed.result;
    Allocator allocator(32 * 1024 * 1024);
    PassManager passes;
    std::string pass = "gpu_vector_lanes", skip;
    passes.parse_pass_arg(pass, skip);
    options.po.pass_cumulative = true;
    passes.apply_passes(allocator, &unit, options.po, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE_FALSE(diagnostics.has_error());
    pass_gpu_launch_sync(allocator, unit, options.po);
    REQUIRE(asr_verify(unit, true, diagnostics));
    auto *program = ASR::down_cast<ASR::Program_t>(
        unit.m_symtab->get_symbol("sync_call_args"));
    LoopSyncs loop;
    loop.var = "it";
    for (size_t i = 0; i < program->n_body; i++) {
        loop.visit_stmt(*program->m_body[i]);
    }
    CHECK(loop.launches == 2);
    CHECK(loop.syncs == 0);
}

namespace {

// The arrays each GpuKeep names, and how many GpuWait statements there are,
// in the body it is handed.
class Keeps : public ASR::BaseWalkVisitor<Keeps> {
public:
    std::vector<std::string> kept;
    size_t waits = 0;
    void visit_GpuKeep(const ASR::GpuKeep_t &x) {
        for (size_t i = 0; i < x.n_vars; i++) {
            kept.push_back(ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(x.m_vars[i])->m_v));
        }
    }
    void visit_GpuWait(const ASR::GpuWait_t &/*x*/) {
        waits++;
    }
};

Keeps keeps_in(ASR::stmt_t **body, size_t n_body) {
    Keeps k;
    for (size_t i = 0; i < n_body; i++) k.visit_stmt(*body[i]);
    std::sort(k.kept.begin(), k.kept.end());
    return k;
}

}

TEST_CASE("GPU waits keep the copies of arrays the host does not change") {
    const std::string source = R"(
module gpu_keep_m
implicit none
contains

    ! The host reads the sum at every iteration, and changes neither array:
    ! both are kept. The procedure returns after the loop, so it waits before
    ! it returns, which keeps nothing.
    subroutine both(a, b, n, s)
        integer, intent(in) :: n
        real(8), intent(in) :: a(n), b(n)
        real(8), intent(out) :: s
        real(8) :: t
        integer :: i, it
        s = 0
        do it = 1, 4
            t = 0
            do concurrent (i = 1:n) reduce(+:t)
                t = t + a(i)*b(i)
            end do
            s = s + t
        end do
    end subroutine

    ! The host changes `a` between the launches: only `b` is kept.
    subroutine host_changes(a, b, n, s)
        integer, intent(in) :: n
        real(8), intent(inout) :: a(n)
        real(8), intent(in) :: b(n)
        real(8), intent(out) :: s
        real(8) :: t
        integer :: i, it
        s = 0
        do it = 1, 4
            t = 0
            do concurrent (i = 1:n) reduce(+:t)
                t = t + a(i)*b(i)
            end do
            s = s + t
            a(1) = a(1) + t
        end do
    end subroutine

    ! A pointer may point to a section with gaps: not kept.
    subroutine pointer_dummy(a, s)
        real(8), pointer, intent(in) :: a(:)
        real(8), intent(out) :: s
        real(8) :: t
        integer :: i, it
        s = 0
        do it = 1, 4
            t = 0
            do concurrent (i = 1:size(a)) reduce(+:t)
                t = t + a(i)
            end do
            s = s + t
        end do
    end subroutine

    ! A contained procedure changes `a`, another only reads a scalar: only
    ! `b` is kept.
    subroutine contained_changes(a, b, n, s)
        integer, intent(in) :: n
        real(8), intent(inout) :: a(n)
        real(8), intent(in) :: b(n)
        real(8), intent(out) :: s
        real(8) :: t
        integer :: i, it
        s = 0
        do it = 1, 4
            t = 0
            do concurrent (i = 1:n) reduce(+:t)
                t = t + a(i)*b(i)
            end do
            s = s + twice(t)
            call bump()
        end do
    contains
        real(8) function twice(x)
            real(8), intent(in) :: x
            twice = 2*x
        end function
        subroutine bump()
            a(n) = a(n) + 1
        end subroutine
    end subroutine

end module
)";
    CompilerOptions options;
    options.gpu_backend = "cuda";
    options.po.gpu_offload_cuda = true;
    options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = "gpu_keep.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(LFortran::source_offset(source.size()));
    diag::Diagnostics diagnostics;
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(parsed.ok);
    ASR::TranslationUnit_t &unit = *parsed.result;
    Allocator allocator(32 * 1024 * 1024);
    PassManager passes;
    std::string pass = "gpu_vector_lanes", skip;
    passes.parse_pass_arg(pass, skip);
    options.po.pass_cumulative = true;
    passes.apply_passes(allocator, &unit, options.po, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE_FALSE(diagnostics.has_error());
    pass_gpu_launch_sync(allocator, unit, options.po);
    REQUIRE(asr_verify(unit, true, diagnostics));
    auto *module = ASR::down_cast<ASR::Module_t>(
        unit.m_symtab->get_symbol("gpu_keep_m"));
    // A procedure may be specialized under another name
    // (`<name>_real____0`).
    auto keeps = [&](const std::string &name) {
        ASR::Function_t *fn = nullptr;
        for (auto &item : module->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)
                    && (item.first == name
                        || item.first.rfind(name + "_", 0) == 0)) {
                fn = ASR::down_cast<ASR::Function_t>(item.second);
            }
        }
        REQUIRE(fn != nullptr);
        return keeps_in(fn->m_body, fn->n_body);
    };
    using names = std::vector<std::string>;
    Keeps k = keeps("both");
    CHECK(k.kept == names{"a", "b"});
    CHECK(k.waits == 1);
    // The body is in the deferring twin, entered with launches of its
    // caller perhaps pending on `a` and `b`: before the host changes `a`
    // it waits, on the path that launched nothing in this iteration too,
    // so the wait after the launch keeps both until that one.
    k = keeps("host_changes");
    CHECK(k.kept == names{"a", "b", "b"});
    k = keeps("pointer_dummy");
    CHECK(k.kept.empty());
    CHECK(k.waits == 0);
    k = keeps("contained_changes");
    CHECK(k.kept == names{"b", "b"});
}

namespace {

// The launches (`L`), syncs (`S`), waits (`W`), discards (`D`) and
// subroutine calls (`C`) of a procedure body after gpu_launch_sync, in
// order, the two branches of an `if` in brackets (a launch is in one: it is
// skipped when it has no iterations).
class SyncTrace : public ASR::BaseWalkVisitor<SyncTrace> {
public:
    std::string trace;
    std::set<ASR::symbol_t*> blocks;
    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &/*x*/) {
        trace += "L";
    }
    void visit_GpuSync(const ASR::GpuSync_t &/*x*/) { trace += "S"; }
    void visit_GpuWait(const ASR::GpuWait_t &/*x*/) { trace += "W"; }
    void visit_GpuDiscard(const ASR::GpuDiscard_t &/*x*/) { trace += "D"; }
    void visit_SubroutineCall(const ASR::SubroutineCall_t &/*x*/) {
        trace += "C";
    }
    void visit_If(const ASR::If_t &x) {
        trace += "[";
        for (size_t i = 0; i < x.n_body; i++) visit_stmt(*x.m_body[i]);
        trace += "|";
        for (size_t i = 0; i < x.n_orelse; i++) visit_stmt(*x.m_orelse[i]);
        trace += "]";
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks.insert(b).second) return;
        ASR::AssociateBlock_t *block = ASR::down_cast<ASR::AssociateBlock_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// The SyncTrace of procedure `name` of module `module` (or of the program
// `name` when `module` is empty), or of its deferring twin, after the Metal
// pipeline up to gpu_launch_sync.
std::string gpu_sync_trace(const std::string &source,
        const std::string &module, const std::string &name,
        bool loaded_from_mod = false) {
    CompilerOptions options;
    options.gpu_backend = "metal";
    options.po.gpu_offload_metal = true;
    options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = "gpu_sync_trace.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(LFortran::source_offset(source.size()));
    diag::Diagnostics diagnostics;
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(parsed.ok);
    ASR::TranslationUnit_t &unit = *parsed.result;
    if (loaded_from_mod) {
        // As if the modules came from their module files, whose procedures
        // this unit compiles (the default multi-file mode).
        for (auto &item : unit.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Module_t>(*item.second)) {
                ASR::down_cast<ASR::Module_t>(item.second)
                    ->m_loaded_from_mod = true;
            }
        }
    }
    Allocator allocator(32 * 1024 * 1024);
    PassManager passes;
    std::string pass = "gpu_vector_lanes", skip;
    passes.parse_pass_arg(pass, skip);
    options.po.pass_cumulative = true;
    passes.apply_passes(allocator, &unit, options.po, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE_FALSE(diagnostics.has_error());
    pass_gpu_launch_sync(allocator, unit, options.po);
    REQUIRE(asr_verify(unit, true, diagnostics));
    SymbolTable *scope = unit.m_symtab;
    if (!module.empty()) {
        scope = ASR::down_cast<ASR::Module_t>(
            unit.m_symtab->get_symbol(module))->m_symtab;
    }
    // pass_array_by_data may have replaced the procedure by its variant
    // `<name>_<suffix>`.
    ASR::symbol_t *sym = scope->get_symbol(name);
    std::string names;
    for (auto &item : scope->get_scope()) {
        names += item.first + " ";
        if (sym == nullptr && item.first.rfind(name + "_", 0) == 0
                && ASR::is_a<ASR::Function_t>(*item.second)) {
            sym = item.second;
        }
    }
    INFO(names);
    REQUIRE(sym != nullptr);
    // A procedure gpu_launch_sync gave a deferring twin calls it.
    if (ASR::symbol_t *twin = scope->get_symbol(
            ASRUtils::symbol_name(sym) + std::string("__gpu_deferred"))) {
        sym = twin;
    }
    SyncTrace trace;
    if (ASR::is_a<ASR::Program_t>(*sym)) {
        ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(sym);
        for (size_t i = 0; i < p->n_body; i++) trace.visit_stmt(*p->m_body[i]);
    } else {
        ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym);
        for (size_t i = 0; i < f->n_body; i++) trace.visit_stmt(*f->m_body[i]);
    }
    return trace.trace;
}

}

TEST_CASE("GPU twins of modules loaded from module files leave only their launches pending") {
    // `project` launches on y and counts the call on the host: a caller that
    // chains projections does not wait between them, whether the module is
    // compiled in this unit or came from its module file.
    const std::string source = R"(
module sync_loaded
implicit none
type :: tally
    integer :: calls = 0
end type
contains
subroutine step(y)
    real, intent(inout) :: y(:)
    integer :: i
    do concurrent (i = 1:size(y))
        y(i) = y(i) + 1
    end do
end subroutine
subroutine project(y, t)
    real, intent(inout) :: y(:)
    type(tally), intent(inout) :: t
    call step(y)
    t%calls = t%calls + 1
end subroutine
subroutine chain(a, b, t)
    real, intent(inout) :: a(:), b(:)
    type(tally), intent(inout) :: t
    call project(a, t)
    call project(b, t)
end subroutine
end module
)";
    CHECK(gpu_sync_trace(source, "sync_loaded", "chain") == "CC");
    CHECK(gpu_sync_trace(source, "sync_loaded", "chain", true) == "CC");
}

TEST_CASE("GPU calls reach the locals a contained procedure names only when it can run elsewhere") {
    // `show` reads the local w. While it is only called by name nothing but
    // `by_name` runs it, so the call of the unrelated twin `bump` does not wait
    // for the launch that writes w; once its name is handed to another
    // procedure (`by_value`) that may call it, and the call of `bump` waits.
    const std::string source = R"(
module sync_frames
implicit none
contains
subroutine bump(y)
    real, intent(inout) :: y(:)
    integer :: i
    do concurrent (i = 1:size(y))
        y(i) = y(i) + 1
    end do
end subroutine
subroutine run(f)
    interface
        subroutine f()
        end subroutine
    end interface
    call f()
end subroutine
subroutine by_name(b, r)
    real, intent(inout) :: b(:)
    real, intent(out) :: r
    real :: w(64)
    integer :: i
    do concurrent (i = 1:64)
        w(i) = i
    end do
    call bump(b)
    call show()
    r = w(2)
contains
    subroutine show()
        if (w(1) /= 1.0) error stop "stale"
    end subroutine
end subroutine
subroutine by_value(b, r)
    real, intent(inout) :: b(:)
    real, intent(out) :: r
    real :: w(64)
    integer :: i
    do concurrent (i = 1:64)
        w(i) = i
    end do
    call bump(b)
    call run(show)
    r = w(2)
contains
    subroutine show()
        if (w(1) /= 1.0) error stop "stale"
    end subroutine
end subroutine
end module
)";
    CHECK(gpu_sync_trace(source, "sync_frames", "by_name") == "[L|]CSC");
    CHECK(gpu_sync_trace(source, "sync_frames", "by_value").substr(0, 5)
        == "[L|]S");
}

TEST_CASE("GPU calls do not access the local targets nothing else names") {
    // `a` is a target of a local pointer, which makes it aliasable, but the
    // pointer is local: the call of the unrelated twin `bump` cannot reach
    // `a`, so it does not wait for the launch that writes `a`. When the
    // pointer is handed to a procedure that stores it (`publish`), a later
    // call may access `a`, and the host waits.
    const std::string source = R"(
module sync_targets
implicit none
real, pointer :: shared(:) => null()
contains
subroutine bump(y)
    real, intent(inout) :: y(:)
    integer :: i
    do concurrent (i = 1:size(y))
        y(i) = y(i) + 1
    end do
end subroutine
subroutine publish(x)
    real, target, intent(in) :: x(:)
    shared => x
end subroutine
subroutine local_only(b, r)
    real, intent(inout) :: b(:)
    real, intent(out) :: r
    real, target :: a(64)
    real, pointer :: p(:)
    integer :: i
    p => a(1:32)
    do concurrent (i = 1:64)
        a(i) = i
    end do
    call bump(b)
    r = p(2)
end subroutine
subroutine published(b, r)
    real, intent(inout) :: b(:)
    real, intent(out) :: r
    real, target :: a(64)
    integer :: i
    call publish(a)
    do concurrent (i = 1:64)
        a(i) = i
    end do
    call bump(b)
    r = a(2)
end subroutine
end module
)";
    CHECK(gpu_sync_trace(source, "sync_targets", "local_only")
        == "[L|]CS");
    CHECK(gpu_sync_trace(source, "sync_targets", "published")
        == "C[L|]SC");
}

TEST_CASE("GPU allocatable actuals of contiguous optional dummies are not synced") {
    // The allocatable actual reaches the CONTIGUOUS optional dummy through a
    // pointer to it or to an explicit-shape placeholder, which is contiguous:
    // the calls are not checked and copied on the host, which would sync the
    // launches before them.
    std::string source = read_project_file(
        "integration_tests/gpu_optional_launch_05.f90");
    for (auto name : {"forward", "forward_whole"}) {
        std::string trace = gpu_sync_trace(source, "gpu_optional_launch_05_m", name);
        INFO(name << ": " << trace);
        CHECK(trace.find('S') == std::string::npos);
        CHECK(trace.find('W') == std::string::npos);
    }
}

TEST_CASE("GPU calls access only what the body of the callee accesses") {
    // `check` only asks for the sizes of its arguments, and `first` reads
    // an element: the host waits for the first launch before `first` only.
    const std::string source = R"(
module sync_summaries
implicit none
contains
pure subroutine check(x, y)
    real, intent(in) :: x(:), y(:)
    if (size(x) /= size(y)) error stop "size mismatch"
end subroutine
pure real function first(x)
    real, intent(in) :: x(:)
    first = x(1)
end function
subroutine shapes_only(x, y)
    real, intent(inout) :: x(:), y(:)
    integer :: i
    do concurrent (i = 1:size(x))
        y(i) = x(i) + 1
    end do
    call check(x, y)
    do concurrent (i = 1:size(x))
        x(i) = 2*y(i)
    end do
end subroutine
subroutine reads_value(x, y)
    real, intent(inout) :: x(:), y(:)
    integer :: i
    do concurrent (i = 1:size(x))
        y(i) = x(i) + 1
    end do
    if (first(y) > 0) then
        do concurrent (i = 1:size(x))
            x(i) = 2*y(i)
        end do
    end if
end subroutine
end module
)";
    CHECK(gpu_sync_trace(source, "sync_summaries", "shapes_only")
        == "[L|]C[L|]");
    // The twin returns with the second launch pending, and waits for the
    // reads it kept copies for past the first wait (GpuKeep).
    CHECK(gpu_sync_trace(source, "sync_summaries", "reads_value")
        == "[L|]S[[L|]|]W");
}

TEST_CASE("GPU calls test the early return of their subroutine") {
    // `dm` returns at once unless a directory is given: its call tests
    // that first, so the wait for the launch before the call, which hands
    // `dm` the array the launch writes, is in the branch that dumps.
    // `keep` defines its intent(out) dummy even when it returns at once,
    // and `count` evaluates an impure function: neither call is skipped.
    const std::string source = R"(
module sync_hooks
implicit none
integer :: calls = 0
contains
integer function next()
    calls = calls + 1
    next = calls
end function
subroutine two_steps(x, directory)
    real, intent(inout) :: x(:)
    character(*), intent(in) :: directory
    real :: y(2)
    integer :: i
    do concurrent (i = 1:size(x))
        x(i) = x(i) + 1
    end do
    call dm('after_first', x)
    do concurrent (i = 1:size(x))
        x(i) = x(i) + 1
    end do
    call keep(y)
    call count(next())
contains
    subroutine dm(name, values)
        character(*), intent(in) :: name
        real, intent(in) :: values(:)
        if (len_trim(directory) == 0) return
        print *, name, values(1)
    end subroutine
    subroutine keep(values)
        real, intent(out) :: values(:)
        if (len_trim(directory) == 0) return
        values = 0
    end subroutine
    subroutine count(k)
        integer, intent(in) :: k
        if (len_trim(directory) == 0) return
        print *, k
    end subroutine
end subroutine
end module
)";
    CHECK(gpu_sync_trace(source, "sync_hooks", "two_steps")
        == "[L|][SC|][L|]SCC");
}

TEST_CASE("GPU launches are not waited for to disassociate a pointer") {
    // `nullify(p)` changes only `p`, not the array it points to, which the
    // first launch writes: the host waits once, before `print`.
    const std::string source = R"(
program sync_nullify
implicit none
real, target :: x(1000)
real, pointer :: p(:)
integer :: i
x = 1
p => x
do concurrent (i = 1:1000)
    x(i) = x(i) + 1
end do
nullify(p)
do concurrent (i = 1:1000)
    x(i) = 2*x(i)
end do
print *, x(1)
end program
)";
    CHECK(gpu_sync_trace(source, "", "sync_nullify") == "[L|][L|]S");
}

TEST_CASE("GPU launches are not waited for because a local pointer "
        "points to their arrays") {
    // Handing `x(:, 2)` to `show` makes a local pointer to the section,
    // which no other procedure can name, and asking whether `x` is
    // allocated associates no pointer with it: `note` cannot reach `x`, and
    // the host waits for the launch only before `print`.
    const std::string source = R"(
module sync_escape_m
implicit none
integer :: total = 0
contains
subroutine show(v)
    real, intent(in) :: v(:)
    total = total + int(v(1))
end subroutine
subroutine note(k)
    integer, intent(in) :: k
    total = total + k
end subroutine
end module
program sync_escape
use sync_escape_m
implicit none
real, allocatable :: x(:, :)
integer :: i, j
allocate(x(100, 4))
x = 1
call show(x(:, 2))
do concurrent (i = 1:100, j = 1:4)
    x(i, j) = 2*x(i, j)
end do
if (allocated(x)) call note(3)
do concurrent (i = 1:100, j = 1:4)
    x(i, j) = x(i, j) + 1
end do
print *, x(1, 1), total
end program
)";
    CHECK(gpu_sync_trace(source, "", "sync_escape") == "C[L|][C|][L|]S");
}

TEST_CASE("GPU module procedures return with their launches pending") {
    // The twin of `both` leaves the launches of the twins of `add_one`
    // pending, and the program waits for them before `print`. A twin
    // waits before returning when a launch writes a module variable, or
    // may have recorded a stop. A call through a procedure pointer reaches
    // `add_one` itself, which waits.
    const std::string source = R"(
module sync_twins
implicit none
real :: shared(8)
contains
subroutine add_one(x)
    real, intent(inout) :: x(:)
    integer :: i
    do concurrent (i = 1:size(x))
        x(i) = x(i) + 1
    end do
end subroutine
subroutine both(x, y)
    real, intent(inout) :: x(:), y(:)
    call add_one(x)
    call add_one(y)
end subroutine
subroutine to_shared(x)
    real, intent(in) :: x(:)
    integer :: i
    do concurrent (i = 1:8)
        shared(i) = x(i)
    end do
end subroutine
subroutine checked(x, n)
    real, intent(inout) :: x(:)
    integer, intent(in) :: n
    integer :: i
    do concurrent (i = 1:size(x))
        if (n < 0) error stop "negative"
        x(i) = x(i) + n
    end do
end subroutine
end module
program sync_twins_main
use sync_twins
implicit none
real :: x(100), y(100)
procedure(add_one), pointer :: p
x = 1
y = 2
call both(x, y)
p => add_one
call p(x)
call both(x, y)
print *, x(1), y(1)
end program
)";
    CHECK(gpu_sync_trace(source, "sync_twins", "add_one") == "[L|]");
    CHECK(gpu_sync_trace(source, "sync_twins", "both") == "CC");
    CHECK(gpu_sync_trace(source, "sync_twins", "to_shared") == "[L|]S");
    CHECK(gpu_sync_trace(source, "sync_twins", "checked") == "[L|]S[|]");
    CHECK(gpu_sync_trace(source, "", "sync_twins_main") == "CSCCS");
}

TEST_CASE("GPU twins return without waiting for the launches that read "
        "their locals") {
    // The second launch of `two_steps` reads the local `t` that the first
    // writes, and the launch of `shifted` reads the optional `bias` through
    // the pointer the lowering of optional arguments associates with
    // `bias` or a local placeholder. Where those locals end, the runtime is
    // handed them to wait for a launch that uses their storage in place
    // (GpuDiscard), so neither twin waits for every launch that reads host
    // memory in place, such as the ones that read `x`.
    const std::string source = R"(
module sync_locals
implicit none
contains
subroutine two_steps(x, y)
    real, intent(in) :: x(:)
    real, intent(out) :: y(:)
    real :: t(size(x))
    integer :: i
    do concurrent (i = 1:size(x))
        t(i) = 2*x(i)
    end do
    do concurrent (i = 1:size(x))
        y(i) = t(i) + x(i)
    end do
end subroutine
subroutine shifted(x, y, bias)
    real, contiguous, intent(in) :: x(:)
    real, contiguous, intent(out) :: y(:)
    real, contiguous, intent(in), optional :: bias(:)
    integer :: i
    do concurrent (i = 1:size(x))
        y(i) = x(i)
        if (present(bias)) y(i) = y(i) + bias(i)
    end do
end subroutine
end module
)";
    CHECK(gpu_sync_trace(source, "sync_locals", "two_steps") == "[L|][L|]D");
    CHECK(gpu_sync_trace(source, "sync_locals", "shifted") == "[[|]L|]D");
}

// The buffer arguments of every kernel of `source` after the Metal pipeline
// up to gpu_launch_sync, as `name:W` when the kernel may write it and
// `name:R` otherwise, sorted.
static std::vector<std::string> gpu_buffer_access(const std::string &source) {
    CompilerOptions options;
    options.gpu_backend = "metal";
    options.po.gpu_offload_metal = true;
    options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = "gpu_buffer_access.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(LFortran::source_offset(source.size()));
    diag::Diagnostics diagnostics;
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(parsed.ok);
    ASR::TranslationUnit_t &unit = *parsed.result;
    Allocator allocator(32 * 1024 * 1024);
    PassManager passes;
    std::string pass = "gpu_vector_lanes", skip;
    passes.parse_pass_arg(pass, skip);
    options.po.pass_cumulative = true;
    passes.apply_passes(allocator, &unit, options.po, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE_FALSE(diagnostics.has_error());
    std::vector<std::string> access;
    std::function<void(SymbolTable*)> walk = [&](SymbolTable *scope) {
        for (auto &item : scope->get_scope()) {
            if (!ASR::is_a<ASR::Function_t>(*item.second)
                    && !ASR::is_a<ASR::Module_t>(*item.second)) {
                continue;
            }
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(
                    item.second);
                for (size_t i = 0; f->m_gpu && i < f->m_gpu->n_buffers;
                        i++) {
                    const ASR::gpu_kernel_argument_t &b =
                        f->m_gpu->m_buffers[i];
                    access.push_back(std::string(ASRUtils::symbol_name(
                        b.m_variable)) + (b.m_written ? ":W" : ":R"));
                }
            }
            walk(ASRUtils::symbol_symtab(item.second));
        }
    };
    walk(unit.m_symtab);
    std::sort(access.begin(), access.end());
    return access;
}

TEST_CASE("A GPU kernel that binds a view of an array only reads it") {
    // The elemental call on a section of `k` in the inner loop gets a
    // pointer bound to the section in the kernel; nothing is written through
    // it, so `k` stays an argument the kernel only reads, which the runtime
    // need not keep as one the kernel writes (nor wait for when a launch
    // wrote part of it).
    const std::string source = R"(
module kernel_views
use, intrinsic :: iso_fortran_env, only: int16, real32
implicit none
contains
pure elemental function widen(x) result(y)
    integer(int16), intent(in) :: x
    real(real32) :: y
    y = real(x, real32)
end function
pure subroutine scores_of(q, k, s)
    integer(int16), contiguous, intent(in) :: q(:,:), k(:,:)
    real(real32), contiguous, intent(out) :: s(:,:)
    integer :: h, t
    do concurrent (h = 1:size(q,2))
        block
            real(real32) :: qv(size(q,1))
            qv = widen(q(:,h))*2
            do concurrent (t = 1:size(k,2))
                s(t,h) = sum(qv*widen(k(:,t)))
            end do
        end block
    end do
end subroutine
end module
)";
    std::vector<std::string> access = gpu_buffer_access(source);
    std::string joined;
    for (const std::string &a : access) joined += a + " ";
    INFO(joined);
    CHECK(std::find(access.begin(), access.end(), "k:R") != access.end());
    CHECK(std::find(access.begin(), access.end(), "k:W") == access.end());
    CHECK(std::find(access.begin(), access.end(), "s:W") != access.end());
}

TEST_CASE("GPU host reads of what pending launches only read do not wait") {
    // The host reads `x(1)` while the launch that reads `x` is pending, and
    // `use_twice` hands `m`, whose component a pending launch reads, to an
    // intent(in) dummy: neither changes what the launches read, so neither
    // waits. Writing `x(2)` does.
    const std::string source = R"(
module sync_reads
implicit none
type :: matrix
    real, allocatable :: w(:)
end type
contains
subroutine apply(m, x, y)
    type(matrix), intent(in) :: m
    real, contiguous, intent(in) :: x(:)
    real, contiguous, intent(out) :: y(:)
    integer :: i
    do concurrent (i = 1:size(x))
        y(i) = m%w(i)*x(i)
    end do
end subroutine
subroutine use_twice(m, x, y, z)
    type(matrix), intent(in) :: m
    real, contiguous, intent(in) :: x(:)
    real, contiguous, intent(inout) :: y(:), z(:)
    call apply(m, x, y)
    call apply(m, y, z)
end subroutine
end module
program sync_reads_main
use sync_reads
implicit none
real :: x(100000), y(100000), z(100000), s
integer :: i
x = 1
do concurrent (i = 1:size(x))
    y(i) = 2*x(i)
end do
s = x(1) + 1
do concurrent (i = 1:size(x))
    z(i) = 3*x(i)
end do
x(2) = s
print *, y(1), z(1)
end program
)";
    CHECK(gpu_sync_trace(source, "sync_reads", "use_twice") == "CC");
    CHECK(gpu_sync_trace(source, "", "sync_reads_main") == "[L|][L|]WS");
    // An assignment whose value calls a function that may change its
    // argument, here what the pending launch reads, waits as before.
    const std::string impure = R"(
module sync_impure
implicit none
contains
real function bump(a)
    real, intent(inout) :: a(:)
    a(1) = a(1) + 1
    bump = a(1)
end function
end module
program sync_impure_main
use sync_impure
implicit none
real :: x(100000), y(100000), s
integer :: i
x = 1
do concurrent (i = 1:size(x))
    y(i) = 2*x(i)
end do
s = bump(x)
print *, s, y(1)
end program
)";
    CHECK(gpu_sync_trace(impure, "", "sync_impure_main") == "[L|]WS");
}

#endif

TEST_CASE("A scalar every iteration assigns first is not read back when "
        "nothing reads it after the loop") {
    // POT3D's set_boundary_points: `t` is assigned first in every iteration
    // of both loops over `i`, so the second loop does not read the value the
    // first leaves in it, and nothing after the second reads it either.
    // Neither launch then needs the host to wait for `t`, which would copy
    // back every array the launches wrote. With `local(t)` the program says
    // so itself; it waits for the launches as often either way: once, for
    // the check at the end.
    auto syncs = [](const std::string &locality) {
        std::string source = R"(
program p
    implicit none
    integer, parameter :: nr = 50, np = 400
    real(8) :: x(nr,2,np), s0(nr), s1(nr), dph(np)
    real(8) :: t
    integer :: it, i, k
    x = 1
    dph = 1
    do it = 1, 10
        do concurrent (k = 1:np, i = 1:nr)
            x(i,1,k) = x(i,2,k) + 1
        end do
        do concurrent (i = 1:nr))" + locality + R"(
            t = 0
            do concurrent (k = 1:np) reduce(+:t)
                t = t + x(i,1,k)*dph(k)
            end do
            s0(i) = t
        end do
        do concurrent (i = 1:nr))" + locality + R"(
            t = 0
            do concurrent (k = 1:np) reduce(+:t)
                t = t + x(i,2,k)*dph(k)
            end do
            s1(i) = t
        end do
        do concurrent (k = 1:np, i = 1:nr)
            x(i,2,k) = s0(i) + s1(i)
        end do
    end do
    if (x(1,2,1) < 0) error stop
end program
)";
        INFO(locality);
        CompilerOptions options;
        options.gpu_backend = "cuda";
        options.po.gpu_offload_cuda = true;
        options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
        FortranEvaluator evaluator(options);
        LocationManager lm;
        LocationManager::FileLocations file;
        file.in_filename = "gpu_assigned_first.f90";
        lm.files.push_back(file);
        lm.file_ends.push_back(LFortran::source_offset(source.size()));
        diag::Diagnostics diagnostics;
        auto parsed = evaluator.get_asr2(source, lm, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE(parsed.ok);
        Allocator allocator(32 * 1024 * 1024);
        PassManager passes;
        std::string pass = "gpu_launch_sync", skip;
        passes.parse_pass_arg(pass, skip);
        options.po.pass_cumulative = true;
        passes.apply_passes(allocator, parsed.result, options.po, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE_FALSE(diagnostics.has_error());
        REQUIRE(asr_verify(*parsed.result, true, diagnostics));
        struct Syncs : public ASR::BaseWalkVisitor<Syncs> {
            size_t launches = 0, syncs = 0;
            void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &) {
                launches++;
            }
            void visit_GpuSync(const ASR::GpuSync_t &) { syncs++; }
        } counts;
        auto *program = ASR::down_cast<ASR::Program_t>(
            parsed.result->m_symtab->get_symbol("p"));
        for (size_t i = 0; i < program->n_body; i++) {
            counts.visit_stmt(*program->m_body[i]);
        }
        CHECK(counts.launches == 4);
        return counts.syncs;
    };
    size_t local = syncs(" local(t)");
    CHECK(local == 1);
    CHECK(syncs("") == local);
}

#ifndef HAVE_BUILD_TO_WASM
TEST_CASE("Device powers with small integer exponents are products") {
    // gpu_integer_powers_01 raises an element, a function result and an
    // expression to the powers 0 to 8: no device routine is left with a
    // real power whose exponent is an integer constant.
    std::string filename = "gpu_integer_powers_01.f90";
    std::string source = read_project_file("integration_tests/" + filename);
    CompilerOptions options;
    options.gpu_backend = "metal";
    options.po.gpu_offload_metal = true;
    options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = filename;
    lm.files.push_back(file);
    lm.file_ends.push_back(LFortran::source_offset(source.size()));
    diag::Diagnostics diagnostics;
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(parsed.ok);
    Allocator allocator(32 * 1024 * 1024);
    PassManager passes;
    std::string pass = "gpu_kernel_finalize", skip;
    passes.parse_pass_arg(pass, skip);
    options.po.pass_cumulative = true;
    passes.apply_passes(allocator, parsed.result, options.po, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE_FALSE(diagnostics.has_error());
    REQUIRE(asr_verify(*parsed.result, true, diagnostics));
    struct Powers : public ASR::BaseWalkVisitor<Powers> {
        size_t powers = 0, products = 0;
        void visit_RealBinOp(const ASR::RealBinOp_t &x) {
            int64_t k;
            if (x.m_op == ASR::binopType::Pow
                    && ASRUtils::extract_value(x.m_right, k)) {
                powers++;
            }
            if (x.m_op == ASR::binopType::Mul) products++;
            ASR::BaseWalkVisitor<Powers>::visit_RealBinOp(x);
        }
    } device;
    size_t kernels = 0;
    for (const auto &entry: parsed.result->m_symtab->get_scope()) {
        if (!ASRUtils::is_device_kernel(entry.second)) continue;
        kernels++;
        device.visit_symbol(*entry.second);
    }
    CHECK(kernels == 1);
    CHECK(device.powers == 0);
    // The 0 + 0 + 1 + 2 + 3 + 4 + 5 + 6 + 7 products of x**0 to x**8, one
    // of half(x)**2, two of (x + 1)**3, and the one in the kernel's copy of
    // half.
    CHECK(device.products == 32);
}

TEST_CASE("A device check every call satisfies does not make a kernel stop") {
    // gpu_guard_fold_01 checks arguments that every call passes in range,
    // gpu_guard_fold_02 one that a call passes out of range: only its
    // launch keeps a status buffer, and only its copy of the checking
    // function the hidden stop arguments.
    for (const auto &test : {std::make_pair("gpu_guard_fold_01", 0),
            std::make_pair("gpu_guard_fold_02", 1)}) {
        INFO(test.first);
        std::string filename = std::string(test.first) + ".f90";
        std::string source = read_project_file(
            "integration_tests/" + filename);
        CompilerOptions options;
        options.gpu_backend = "metal";
        options.po.gpu_offload_metal = true;
        options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
        FortranEvaluator evaluator(options);
        LocationManager lm;
        LocationManager::FileLocations file;
        file.in_filename = filename;
        lm.files.push_back(file);
        lm.file_ends.push_back(LFortran::source_offset(source.size()));
        diag::Diagnostics diagnostics;
        auto parsed = evaluator.get_asr2(source, lm, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE(parsed.ok);
        Allocator allocator(32 * 1024 * 1024);
        // Up to the pass that places the waits, before the launches are
        // expanded into runtime calls.
        PassManager passes;
        std::string pass = "gpu_launch_sync", skip;
        passes.parse_pass_arg(pass, skip);
        options.po.pass_cumulative = true;
        passes.apply_passes(allocator, parsed.result, options.po, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE_FALSE(diagnostics.has_error());
        REQUIRE(asr_verify(*parsed.result, true, diagnostics));
        struct Stops : public ASR::BaseWalkVisitor<Stops> {
            size_t launches = 0, stopping_launches = 0;
            void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
                launches++;
                if (x.m_stop_status != nullptr) stopping_launches++;
            }
        } stops;
        stops.visit_TranslationUnit(*parsed.result);
        CHECK(stops.launches == (test.second == 0 ? 3 : 1));
        CHECK(stops.stopping_launches == (size_t)test.second);
        size_t kernels = 0, stopping_copies = 0;
        for (const auto &entry: parsed.result->m_symtab->get_scope()) {
            if (!ASRUtils::is_device_kernel(entry.second)) continue;
            kernels++;
            auto *kernel = ASR::down_cast<ASR::Function_t>(entry.second);
            for (const auto &local: kernel->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Function_t>(*local.second)) continue;
                if (ASR::down_cast<ASR::Function_t>(local.second)->m_symtab
                        ->get_symbol("__gpu_stop_site") != nullptr) {
                    stopping_copies++;
                }
            }
        }
        CHECK(kernels == stops.launches);
        CHECK(stopping_copies == (size_t)test.second);
    }
}
#endif
#ifndef HAVE_BUILD_TO_WASM
TEST_CASE("A kernel's call of a small function that runs a loop is inlined") {
    // gpu_inline_loop_calls_01 sums the squares of a column through
    // sum_squares and a weighted column through weighted: both calls are
    // inlined, so the lanes of an iteration share their loops and combine
    // the sums, and no kernel calls either function.
    std::string filename = "gpu_inline_loop_calls_01.f90";
    std::string source = read_project_file("integration_tests/" + filename);
    CompilerOptions options;
    options.gpu_backend = "metal";
    options.po.gpu_offload_metal = true;
    options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = filename;
    lm.files.push_back(file);
    lm.file_ends.push_back(LFortran::source_offset(source.size()));
    diag::Diagnostics diagnostics;
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(parsed.ok);
    Allocator allocator(32 * 1024 * 1024);
    PassManager passes;
    std::string pass = "gpu_vector_lanes", skip;
    passes.parse_pass_arg(pass, skip);
    options.po.pass_cumulative = true;
    passes.apply_passes(allocator, parsed.result, options.po, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE_FALSE(diagnostics.has_error());
    REQUIRE(asr_verify(*parsed.result, true, diagnostics));
    struct Kernel : public ASR::BaseWalkVisitor<Kernel> {
        size_t reductions = 0, calls = 0;
        void visit_GpuLaneReduce(const ASR::GpuLaneReduce_t &x) {
            reductions++;
            ASR::BaseWalkVisitor<Kernel>::visit_GpuLaneReduce(x);
        }
        void visit_FunctionCall(const ASR::FunctionCall_t &x) {
            std::string name = ASRUtils::symbol_name(
                ASRUtils::symbol_get_past_external(x.m_name));
            if (name.rfind("sum_squares", 0) == 0
                    || name.rfind("weighted", 0) == 0) {
                calls++;
            }
            ASR::BaseWalkVisitor<Kernel>::visit_FunctionCall(x);
        }
    } kernel;
    for (const auto &entry: parsed.result->m_symtab->get_scope()) {
        if (ASRUtils::is_device_kernel(entry.second)) {
            kernel.visit_symbol(*entry.second);
        }
    }
    CHECK(kernel.calls == 0);
    CHECK(kernel.reductions == 2);
}
#endif

#ifndef HAVE_BUILD_TO_WASM
namespace {

// What the launches of a procedure look like after the Metal pipeline up to
// gpu_vector_lanes, with --fast.
struct GpuLaunchSummary {
    // The most iterations a launch gets lanes for: the constant its lanes
    // selection compares the iteration count with, or -1 without lanes.
    int64_t lanes_cap = -1;
    // The most workspaces a launched kernel has.
    size_t workspaces = 0;
    // The lane reductions of the kernels it launches.
    size_t reductions = 0;
};

// The GpuLaunchSummary of each procedure of module `module` in `source`
// that launches a kernel, by the procedure's name up to the variant suffix
// pass_array_by_data adds.
std::map<std::string, GpuLaunchSummary> gpu_launch_summaries(
        const std::string &source, const std::string &filename,
        const std::string &module) {
    CompilerOptions options;
    options.gpu_backend = "metal";
    options.po.gpu_offload_metal = true;
    options.po.fast = true;
    options.po.bounds_checking = false;
    options.po.strict_bounds_checking = false;
    options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = filename;
    lm.files.push_back(file);
    lm.file_ends.push_back(LFortran::source_offset(source.size()));
    diag::Diagnostics diagnostics;
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(parsed.ok);
    Allocator allocator(32 * 1024 * 1024);
    PassManager passes;
    std::string pass = "gpu_vector_lanes", skip;
    passes.parse_pass_arg(pass, skip);
    options.po.pass_cumulative = true;
    passes.apply_passes(allocator, parsed.result, options.po, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE_FALSE(diagnostics.has_error());
    REQUIRE(asr_verify(*parsed.result, true, diagnostics));
    // `if (count <= cap) __gpu_lanes = 32` (VectorLanes), and the launches.
    struct Launches : public ASR::BaseWalkVisitor<Launches> {
        GpuLaunchSummary summary;
        bool launches = false;
        void visit_If(const ASR::If_t &x) {
            if (x.n_body == 1 && ASR::is_a<ASR::Assignment_t>(*x.m_body[0])
                    && ASR::is_a<ASR::IntegerCompare_t>(*x.m_test)) {
                auto *a = ASR::down_cast<ASR::Assignment_t>(x.m_body[0]);
                auto *c = ASR::down_cast<ASR::IntegerCompare_t>(x.m_test);
                int64_t value = 0;
                if (ASR::is_a<ASR::Var_t>(*a->m_target)
                        && std::string(ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(a->m_target)->m_v))
                            .rfind("__gpu_lanes", 0) == 0
                        && c->m_op == ASR::cmpopType::LtE
                        && ASRUtils::extract_value(c->m_right, value)) {
                    summary.lanes_cap = std::max(summary.lanes_cap, value);
                }
            }
            BaseWalkVisitor::visit_If(x);
        }
        std::set<ASR::Function_t*> kernels;
        void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
            launches = true;
            auto *kernel = ASR::down_cast<ASR::Function_t>(x.m_kernel);
            summary.workspaces = std::max(summary.workspaces,
                kernel->m_gpu->n_workspaces);
            if (!kernels.insert(kernel).second) return;
            struct Reductions : public ASR::BaseWalkVisitor<Reductions> {
                size_t found = 0;
                void visit_GpuLaneReduce(const ASR::GpuLaneReduce_t &x) {
                    found++;
                    BaseWalkVisitor::visit_GpuLaneReduce(x);
                }
            } reductions;
            reductions.visit_symbol(kernel->base);
            summary.reductions += reductions.found;
        }
    };
    std::map<std::string, GpuLaunchSummary> summaries;
    auto *m = ASR::down_cast<ASR::Module_t>(
        parsed.result->m_symtab->get_symbol(module));
    for (auto &entry : m->m_symtab->get_scope()) {
        if (!ASR::is_a<ASR::Function_t>(*entry.second)) continue;
        Launches l;
        l.visit_symbol(*entry.second);
        if (!l.launches) continue;
        std::string name = entry.first.substr(0, entry.first.find('_'));
        GpuLaunchSummary &s = summaries[name];
        s.lanes_cap = std::max(s.lanes_cap, l.summary.lanes_cap);
        s.workspaces = std::max(s.workspaces, l.summary.workspaces);
        s.reductions = std::max(s.reductions, l.summary.reductions);
    }
    return summaries;
}

}

TEST_CASE("GPU lanes share the parts of a split sum one part at a time") {
    // gpu_lanes_parts_01: the lanes share out the sum of each part, not
    // the parts, of which there may be fewer than the lanes: the kernel
    // reduces each part across the lanes, and then the parts.
    std::string source = read_project_file(
        "integration_tests/gpu_lanes_parts_01.f90");
    auto launches = gpu_launch_summaries(source, "gpu_lanes_parts_01.f90",
        "gpu_lanes_parts_01_m");
    REQUIRE(launches.count("split"));
    CHECK(launches["split"].reductions == 2);
}

TEST_CASE("GPU lanes sum along columns while stepping an index of their own") {
    // gpu_lanes_columns_02, with --fast: the sums of both procedures step a
    // second index into another array, and the lanes share them out at any
    // launch size (above the 1024 iterations other kernels get lanes up
    // to): the strided sum, and the branch of the quantized product that
    // sums at once, whose other branch shares out its groups.
    std::string source = read_project_file(
        "integration_tests/gpu_lanes_columns_02.f90");
    auto launches = gpu_launch_summaries(source, "gpu_lanes_columns_02.f90",
        "gpu_lanes_columns_02_m");
    REQUIRE(launches.count("strided"));
    CHECK(launches["strided"].lanes_cap > 1024);
    CHECK(launches["strided"].reductions == 1);
    REQUIRE(launches.count("packed"));
    CHECK(launches["packed"].lanes_cap > 1024);
    CHECK(launches["packed"].reductions >= 1);
}

TEST_CASE("GPU private maps that only a sum reads need no workspace") {
    // gpu_private_reduction_02: the group terms of `grouped` (in a branch
    // of an `if`) and of `counts` are added to their sums as they are
    // computed; `reread` reads its map again and keeps its workspace.
    std::string source = read_project_file(
        "integration_tests/gpu_private_reduction_02.f90");
    auto launches = gpu_launch_summaries(source, "gpu_private_reduction_02.f90",
        "gpu_private_reduction_02_m");
    REQUIRE(launches.count("grouped"));
    CHECK(launches["grouped"].workspaces == 0);
    REQUIRE(launches.count("counts"));
    CHECK(launches["counts"].workspaces == 0);
    REQUIRE(launches.count("reread"));
    CHECK(launches["reread"].workspaces == 1);
}
#endif

#include <tests/doctest.h>

#include <fstream>
#include <sstream>

#include <libasr/asr_text.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/pass_manager.h>
#include <libasr/pass/gpu_memory_space.h>
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
            // CUDA launches the two-dimensional nest in whole tiles, whose
            // count can exceed the kind of the loop indices.
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
        CHECK(wide_counts == (metal ? 0 : 1));
        pass_gpu_launch_sync(allocator, unit, options.po);
        pass_device_launch_expand(allocator, unit, options.po);
        REQUIRE(asr_verify(unit, true, diagnostics));
        GuardedGpuWork expanded;
        for (size_t i = 0; i < program->n_body; i++) {
            expanded.visit_stmt(*program->m_body[i]);
        }
        CHECK(expanded.launches.empty());
        CHECK(expanded.runtime_launches == 3);
        CHECK(expanded.runtime_releases == 3);
        CHECK(expanded.source_allocations == 1);
        CHECK(expanded.empty_lane_guards == (metal ? 1 : 0));
    }
}

namespace {

// Runs the CUDA pipeline on the program `name` in `source` and hands
// `visit` each statement of its body.
template <class Visit>
void visit_cuda_program(const std::string &source,
        const std::string &filename, const std::string &name, Visit visit) {
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

#ifndef HAVE_BUILD_TO_WASM
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
#endif

#include <tests/doctest.h>

#include <libasr/asr_text.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/pass_manager.h>
#include <libasr/pass/gpu_memory_space.h>
#include <libasr/serialization.h>
#include <lfortran/fortran_evaluator.h>

using namespace LCompilers;

TEST_CASE("GPU decline policy distinguishes lowering gaps from backend limits") {
    Allocator allocator(1024 * 1024);
    Location loc{0, 0};
    ASR::ttype_t *real8 = ASRUtils::TYPE(ASR::make_Real_t(allocator, loc, 8));
    ASR::ttype_t *logical8 = ASRUtils::TYPE(ASR::make_Logical_t(allocator, loc, 8));
    auto metal = gpu_device_capabilities(GpuDevice::Metal);
    auto cuda = gpu_device_capabilities(GpuDevice::Cuda);
    GpuDecline wide_real(GpuDeclineReason::SymbolTypeNotRepresentable, "x", real8);
    GpuDecline wide_logical(GpuDeclineReason::ArrayElementTypeWidth, "x", logical8);
    CHECK(gpu_decline_class(wide_real, metal) == GpuDeclineClass::BackendCannot);
    CHECK(gpu_decline_class(wide_real, cuda) == GpuDeclineClass::NotImplemented);
    CHECK(gpu_decline_class(wide_logical, cuda) == GpuDeclineClass::NotImplemented);
    CHECK(gpu_decline_class(GpuDecline(GpuDeclineReason::ScalarNotNumeric),
        cuda) == GpuDeclineClass::NotImplemented);

    PassOptions options;
    options.gpu_offload_cuda = true;
    diag::Diagnostics strict;
    options.diagnostics = &strict;
    report_gpu_decline(options, loc, wide_logical);
    CHECK(strict.has_error());

    diag::Diagnostics waived;
    options.diagnostics = &waived;
    options.gpu_allow_cpu_fallback = true;
    report_gpu_decline(options, loc, wide_logical);
    CHECK_FALSE(waived.has_error());
    CHECK(waived.diagnostics.size() == 1);

    diag::Diagnostics limited;
    options.diagnostics = &limited;
    options.gpu_allow_cpu_fallback = false;
    options.gpu_offload_cuda = false;
    options.gpu_offload_metal = true;
    report_gpu_decline(options, loc, wide_real);
    CHECK_FALSE(limited.has_error());
    CHECK(limited.diagnostics.size() == 1);
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
    lm.file_ends.push_back(source.size());
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
        auto *restored = ASR::down_cast<ASR::TranslationUnit_t>(
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
    ASR::symbol_t *index = kernel->m_symtab->get_symbol("i");
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
    use iso_c_binding, only: c_int
    interface
        pure function c_abs(i) bind(c, name="abs") result(r)
            import c_int
            integer(c_int), value :: i
            integer(c_int) :: r
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
    options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator evaluator(options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = "gpu_interface.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(source.size());
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

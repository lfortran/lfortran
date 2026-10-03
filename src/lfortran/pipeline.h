#ifndef LFORTRAN_PIPELINE_H
#define LFORTRAN_PIPELINE_H

#include <string>

#include <libasr/asr.h>
#include <libasr/diagnostics.h>
#include <libasr/exception.h>
#include <libasr/location.h>
#include <libasr/pass/pass_manager.h>
#include <libasr/utils.h>
#include <lfortran/fortran_evaluator.h>

namespace LCompilers {

enum class ASRPipelinePhase {
    None,
    InitialVerify,
    PassesAndLLVM,
    ObjectEmission
};

struct ASRObjectResult {
    bool ok = false;
    ASRPipelinePhase phase = ASRPipelinePhase::None;
    int status = 0;
    int optimization_time_us = 0;
    int object_time_us = 0;
};

Result<ASR::TranslationUnit_t*> load_input_asr(
    const std::string &input, const std::string &infile, bool from_asr,
    Allocator &asr_text_allocator, FortranEvaluator &evaluator,
    LocationManager &lm, diag::Diagnostics &diagnostics,
    bool require_main_program=false);

ASRObjectResult compile_asr_to_object(
    ASR::TranslationUnit_t &asr, const std::string &infile,
    const std::string &outfile, bool assembly,
    CompilerOptions &compiler_options, PassManager &pass_manager,
    FortranEvaluator &evaluator, LocationManager &lm,
    diag::Diagnostics &diagnostics);

} // namespace LCompilers

#endif // LFORTRAN_PIPELINE_H

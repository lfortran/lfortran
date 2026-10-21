#ifndef LFORTRAN_EVALUATION_H
#define LFORTRAN_EVALUATION_H

#include <lfortran/fortran_evaluator.h>

namespace LCompilers::LFortran {

// Compiler execution is serialized by the caller. Sinks run on that thread;
// OS stream capture and protocol/session bookkeeping belong to the frontend.
class EvaluationEvents {
public:
    virtual ~EvaluationEvents() = default;
    virtual void stream(const std::string &, const std::string &) {}
    virtual void display(const std::string &, const std::string &) {}
    virtual void clear_output() {}
};

struct EvaluationSettings {
    bool verbose = false;
};

struct EvaluationOutcome {
    bool ok = false;
    FortranEvaluator::EvalResult value{};
    bool has_value = false;
    std::string text;
    std::string type_name;
    std::string diagnostics;
    std::string error_name;
    std::string error_value;
    std::string exception_report;
};

class EvaluationService {
    // FortranEvaluator borrows these options for its entire lifetime.
    CompilerOptions options;
    FortranEvaluator evaluator;
public:
    explicit EvaluationService(CompilerOptions options = {});
    EvaluationService(const EvaluationService &) = delete;
    EvaluationService &operator=(const EvaluationService &) = delete;
    EvaluationOutcome execute(const std::string &code,
        EvaluationEvents &events, const EvaluationSettings &settings = {});
    static void format_result(EvaluationOutcome &out);
};

} // namespace LCompilers::LFortran

extern "C" {
#ifdef _WIN32
__declspec(dllexport) void lfortran_display_data(const char *mime_type, const char *data);
__declspec(dllexport) void lfortran_clear_output();
#else
__attribute__((visibility("default"))) void lfortran_display_data(
    const char *mime_type, const char *data);
__attribute__((visibility("default"))) void lfortran_clear_output();
#endif
}

#endif

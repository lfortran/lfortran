#include <lfortran/evaluation.h>
#include <lfortran/utils.h>
#include <libasr/exception.h>
#include <iomanip>
#include <sstream>

namespace {
using LCompilers::LFortran::EvaluationEvents;
// Runtime/JIT state is process-local too. No concurrent service executions.
EvaluationEvents *current_events = nullptr;
bool event_failed = false;

class ExecutionScope {
    EvaluationEvents *previous;
    LCompilers::LFortran::EvaluationSettings settings;
public:
    ExecutionScope(EvaluationEvents &events,
            const LCompilers::LFortran::EvaluationSettings &settings)
        : previous(current_events), settings(settings) {
        LCOMPILERS_ASSERT(!previous);
        event_failed = false;
        current_events = &events;
        _lfortran_stdin_install(settings.input ? input : nullptr,
            &this->settings);
    }
    ~ExecutionScope() {
        _lfortran_stdin_reset();
        current_events = previous;
    }
private:
    static int input(void *context, const char **record, size_t *length) noexcept {
        const auto &settings =
            *static_cast<const LCompilers::LFortran::EvaluationSettings *>(context);
        try {
            return settings.input(settings.input_context, record, length);
        } catch (...) {
            // Never unwind through generated code or C runtime frames.
            return 1;
        }
    }
};
}

// Keep these exports available to native JIT and WASM side modules alike.
extern "C" void lfortran_display_data(const char *mime_type, const char *data) {
    if (current_events && mime_type && data) {
        try {
            current_events->display(mime_type, data);
        } catch (...) {
            event_failed = true;
        }
    }
}

extern "C" void lfortran_clear_output() {
    if (current_events) {
        try {
            current_events->clear_output();
        } catch (...) {
            event_failed = true;
        }
    }
}

namespace LCompilers::LFortran {

static CompilerOptions interactive_options(CompilerOptions options) {
    options.interactive = true;
    if (options.po.runtime_library_dir.empty()) {
        options.po.runtime_library_dir = get_runtime_library_dir();
    }
    return options;
}

EvaluationService::EvaluationService(CompilerOptions options)
    : options(interactive_options(std::move(options))), evaluator(this->options) {}

void EvaluationService::format_result(EvaluationOutcome &out) {
    using R = FortranEvaluator::EvalResult;
    auto &r = out.value;
    std::ostringstream text;
    out.has_value = true;
    switch (r.type) {
        case R::integer4: out.type_name = "integer"; text << r.i32; break;
        case R::integer8: out.type_name = "integer(8)"; text << r.i64; break;
        case R::real4:
            out.type_name = "real"; text << std::setprecision(8) << r.f32; break;
        case R::real8:
            out.type_name = "real(8)"; text << std::setprecision(17) << r.f64; break;
        case R::complex4:
            out.type_name = "complex";
            text << std::setprecision(8) << "(" << r.c32.re << ", " << r.c32.im << ")";
            break;
        case R::complex8:
            out.type_name = "complex(8)";
            text << std::setprecision(17) << "(" << r.c64.re << ", " << r.c64.im << ")";
            break;
        case R::boolean: out.type_name = "logical"; text << (r.b ? "True" : "False"); break;
        case R::character: out.type_name = "character"; text << r.str; break;
        case R::statement:
            out.type_name = "none"; text << "(statement)"; out.has_value = false; break;
        case R::none:
            out.type_name = "none"; text << "(nothing to execute)"; out.has_value = false; break;
    }
    out.text = text.str();
}

EvaluationOutcome EvaluationService::execute(const std::string &code,
        EvaluationEvents &events, const EvaluationSettings &settings) {
    EvaluationOutcome out;
    out.value.type = FortranEvaluator::EvalResult::none;
    ExecutionScope scope(events, settings);
    LocationManager lm;
    diag::Diagnostics diagnostics;
    PassManager passes;
    passes.use_default_passes();
    try {
        const auto newline = code.find('\n');
        const auto magic = code.substr(0, newline);
        const bool inspection = magic == "%%showast" || magic == "%%showasr" ||
            magic == "%%showllvm" || magic == "%%showasm" ||
            magic == "%%showcpp" || magic == "%%showfmt";
        if (inspection) {
            const auto source = newline == std::string::npos ? "" : code.substr(newline + 1);
            Result<std::string> result{Error{}};
            if (magic == "%%showast") result = evaluator.get_ast(source, lm, diagnostics);
            else if (magic == "%%showasr") result = evaluator.get_asr(source, lm, diagnostics);
            else if (magic == "%%showllvm") result = evaluator.get_llvm(source, lm, passes, diagnostics);
            else if (magic == "%%showasm") result = evaluator.get_asm(source, lm, passes, diagnostics);
            else if (magic == "%%showcpp") result = evaluator.get_cpp(source, lm, diagnostics, 1);
            else result = evaluator.get_fmt(source, lm, diagnostics);
            out.ok = result.ok;
            if (out.ok) events.stream("stdout", result.result);
        } else {
            auto result = evaluator.evaluate(code, settings.verbose, lm, passes, diagnostics);
            out.ok = result.ok;
            if (out.ok) {
                out.value = std::move(result.result);
                format_result(out);
            }
        }
        out.diagnostics = diagnostics.render(lm, options);
        if (!out.ok) {
            out.error_name = "CompilerError";
            out.error_value = out.diagnostics;
        }
        if (event_failed) {
            out.ok = false;
            out.error_name = "DisplayError";
            out.error_value = "display event delivery failed";
        }
    } catch (const LCompilersException &error) {
        out.ok = false;
        out.error_name = error.name();
        out.error_value = error.msg();
        auto trace = error.stacktrace_addresses();
        get_local_addresses(trace);
        get_local_info(trace);
        out.exception_report = "Internal Compiler Error: Unhandled exception\n" +
            stacktrace2str(trace, stacktrace_depth) + error.name() + ": " + error.msg() + "\n";
    }
    return out;
}

} // namespace LCompilers::LFortran

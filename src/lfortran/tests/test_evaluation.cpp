#include <lfortran/evaluation.h>
#include <iostream>
#include <stdexcept>
#include <vector>

using namespace LCompilers;
using namespace LCompilers::LFortran;

static void require(bool condition, const std::string &message) {
    if (!condition) throw std::runtime_error(message);
}

struct Events : EvaluationEvents {
    std::string output, mime, data;
    int clears = 0;
    void stream(const std::string &, const std::string &text) override { output += text; }
    void display(const std::string &type, const std::string &text) override {
        mime = type; data = text;
    }
    void clear_output() override { clears++; }
};

static CompilerOptions options() {
    CompilerOptions options;
    options.use_colors = false;
    options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    return options;
}

static EvaluationOutcome run(EvaluationService &service, const std::string &code,
        Events &events) {
    auto outcome = service.execute(code, events);
    require(outcome.ok, code + "\n" + outcome.diagnostics + outcome.exception_report);
    return outcome;
}

static void results_and_cells() {
    EvaluationService service(options());
    Events events;
    const std::vector<std::pair<std::string, std::string>> examples = {
        {"42", "42"}, {"42_8", "42"}, {"1.25", "1.25"},
        {"1.2345678901234567_8", "1.2345678901234567"},
        {"(1.25, -2.5)", "(1.25, -2.5)"},
        {"(1.25_8, -2.5_8)", "(1.25, -2.5)"},
        {".true.", "True"}, {".false.", "False"}, {"'hello'", "hello"}
    };
    for (auto &example : examples) {
        auto outcome = run(service, example.first, events);
        require(outcome.has_value && outcome.text == example.second,
            example.first + ": expected " + example.second + ", got " +
            (outcome.has_value ? outcome.text : "no value"));
    }
    require(!run(service, "integer :: n\nn = 41", events).has_value, "statement result");
    require(run(service, "n+1", events).text == "42", "persistent symbols");
    require(!run(service, "", events).has_value, "empty cell");
    auto error = service.execute("integer :: broken(", events);
    require(!error.ok && error.error_name == "CompilerError" &&
        error.diagnostics.find("<cell ") != std::string::npos, "cell diagnostics");
    require(run(service, "n", events).text == "41", "state after parse error");
    for (const std::string magic : {"ast", "asr", "llvm", "asm", "cpp", "fmt"}) {
        EvaluationService inspection(options());
        events.output.clear();
        run(inspection, "%%show" + magic + "\ninteger :: x", events);
        require(!events.output.empty(), magic + " inspection");
    }
}

static void display_events() {
    EvaluationService service(options());
    Events events;
    run(service, R"(
use lfortran_display, only: display_data, clear_output
call display_data('text/html', '<b>hello</b>')
call clear_output()
)", events);
    require(events.mime == "text/html" && events.data == "<b>hello</b>" &&
        events.clears == 1, "display C ABI");
    lfortran_display_data("text/plain", "outside execution");
    lfortran_clear_output();
    require(events.clears == 1, "event sink must be execution scoped");
    class ThrowingEvents : public Events {
        void clear_output() override { throw std::runtime_error("display failure"); }
    } throwing;
    auto failed = service.execute("call clear_output()", throwing);
    require(!failed.ok && failed.error_name == "DisplayError",
        "display exceptions must not cross generated code");
}

int main() {
    try {
        results_and_cells();
        display_events();
        std::cout << "shared evaluation checks passed\n";
        return 0;
    } catch (const std::exception &error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}

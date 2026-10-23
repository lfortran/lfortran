#ifndef LFORTRAN_FORTRAN_KERNEL_H
#define LFORTRAN_FORTRAN_KERNEL_H

#include <functional>
#include <string>

#include <libasr/config.h>
#include <libasr/string_utils.h>
#include <lfortran/fortran_evaluator.h>

namespace LCompilers::LFortran {

// Runs the Jupyter kernel of a connection file with `options`. The kernel is
// the Xeus one with WITH_XEUS, else the built-in one (jupyter.cpp), which
// needs no third-party library. If the file does not exist, the built-in
// kernel listens on free ports and writes it. A `managed` kernel, the one the
// prompt starts, stops when its last client disconnects and removes the file.
int run_kernel(const std::string &connection_filename, CompilerOptions &options,
    bool managed);

// What the two kernels share: everything they do besides moving messages.
namespace kernel {

// Text in a Jupyter message is JSON, so bytes that are not UTF-8 are shown as
// U+FFFD.
inline std::string utf8(const std::string &s)
{
    if (is_valid_utf8(s)) return s;
    std::string out;
    size_t i = 0;
    while (i < s.size()) {
        const unsigned char c = static_cast<unsigned char>(s[i]);
        const size_t n = c >= 0xf0 ? 4 : c >= 0xe0 ? 3 : c >= 0xc0 ? 2 : 1;
        if (i + n <= s.size() && is_valid_utf8(s.data() + i, n)) {
            out.append(s, i, n);
            i += n;
        } else {
            out += "\xef\xbf\xbd";
            i++;
        }
    }
    return out;
}

// The kernel_info_reply.
const char *const implementation = "LFortran";
const char *const language = "fortran";
const char *const language_version = "2018";
const char *const mimetype = "text/x-fortran";
const char *const file_extension = ".f90";
inline std::string banner()
{
    return "LFortran " + std::string(LFORTRAN_VERSION) + "\nJupyter kernel for Fortran";
}

// Runs the code of an execute_request: what the cell writes, its diagnostics
// and its error go to `stream` ("stdout" or "stderr"), its value to `result`
// as text/plain. `io` supplies the display, clear_output and input hooks.
// `verbose` adds the cell's AST, ASR and LLVM IR (`lfortran -v`).
inline FortranEvaluator::CellResult execute(FortranEvaluator &e,
    const std::string &code, FortranEvaluator::CellIO io,
    const std::function<void(const std::string &name, const std::string &text)> &stream,
    const std::function<void(const std::string &text)> &result, bool verbose = false)
{
    io.output = [&](int fd, const std::string &text) {
        stream(fd == 1 ? "stdout" : "stderr", utf8(text));
    };
    FortranEvaluator::CellResult cell = e.execute(code, verbose, io);
    if (!cell.output.empty()) {
        stream("stdout", utf8(cell.output.back() == '\n' ? cell.output : cell.output + "\n"));
    }
    if (!cell.ok) {
        stream("stderr", utf8(cell.error_text()));
        return cell;
    }
    if (!cell.diagnostics.empty()) stream("stderr", utf8(cell.diagnostics));
    if (verbose && !cell.inspection) {
        const bool colors = e.compiler_options.use_colors;
        auto section = [&](const std::string &title) {
            return colors ? "\033[1m\033[34m" + title + "\033[0m\033[39m\n" : title + "\n";
        };
        const FortranEvaluator::EvalResult &r = cell.value;
        stream("stdout", utf8(section("AST:") + r.ast + "\n" + section("ASR:") + r.asr
            + "\n" + section("LLVM IR:") + r.llvm_ir + "\n" + "Return type: "
            + r.type_name() + "\n" + section("Result:")));
    }
    if (cell.value.has_value()) result(utf8(cell.value.text()));
    return cell;
}

} // namespace kernel

} // namespace LCompilers::LFortran

#endif // LFORTRAN_FORTRAN_KERNEL_H

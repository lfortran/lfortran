#ifndef LFORTRAN_FORTRAN_KERNEL_H
#define LFORTRAN_FORTRAN_KERNEL_H

#include <functional>
#include <string>

#include <libasr/config.h>
#include <libasr/string_utils.h>
#include <lfortran/fortran_evaluator.h>
#include <lfortran/jupyter.h>

namespace LCompilers::LFortran {

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

// Capture chunks can split a code point. Keep only a valid incomplete suffix,
// independently for stdout and stderr; malformed bytes retain utf8()'s policy.
class StreamText {
    std::string carry[2];
    const std::function<void(const std::string &, const std::string &)> &stream;
public:
    explicit StreamText(const std::function<void(const std::string &,
        const std::string &)> &stream) : stream(stream) {}
    void write(int fd, const std::string &bytes) {
        auto &tail = carry[fd - 1];
        std::string text = std::move(tail);
        text += bytes;
        size_t end = text.size(), start = end;
        while (start && end - start < 3 &&
                (static_cast<unsigned char>(text[start - 1]) & 0xc0) == 0x80) start--;
        if (start) {
            start--;
            const auto c = static_cast<unsigned char>(text[start]);
            size_t n = c >= 0xc2 && c <= 0xdf ? 2 :
                c >= 0xe0 && c <= 0xef ? 3 : c >= 0xf0 && c <= 0xf4 ? 4 : 0;
            bool prefix = n && end - start < n;
            for (size_t j = start + 1; prefix && j < end; j++) {
                auto b = static_cast<unsigned char>(text[j]);
                prefix = (b & 0xc0) == 0x80;
                if (j == start + 1)
                    prefix = prefix && !(c == 0xe0 && b < 0xa0) &&
                        !(c == 0xed && b >= 0xa0) && !(c == 0xf0 && b < 0x90) &&
                        !(c == 0xf4 && b >= 0x90);
            }
            if (prefix) end = start;
        }
        tail = text.substr(end);
        if (end) stream(fd == 1 ? "stdout" : "stderr", utf8(text.substr(0, end)));
    }
    void finish() {
        for (int i = 0; i < 2; i++) {
            if (!carry[i].empty()) {
                auto text = std::move(carry[i]);
                carry[i].clear();
                stream(i == 0 ? "stdout" : "stderr", utf8(text));
            }
        }
    }
};

// The kernel_info_reply.
const char *const implementation = "LFortran";
const char *const language = "fortran";
const char *const language_version = "2018";
const char *const mimetype = "text/x-fortran";
const char *const file_extension = ".f90";
// `version` is the LFortran version, which only .cpp files expand, as it
// changes with every commit (ci/test_ci.py).
inline std::string banner(const std::string &version)
{
    return "LFortran " + version + "\nJupyter kernel for Fortran";
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
    StreamText text(stream);
    const auto on_exit = io.flush_on_exit;
    io.flush_on_exit = [&]() {
        try { text.finish(); }
        catch (...) {
            if (on_exit) on_exit();
            throw;
        }
        if (on_exit) on_exit();
    };
    const auto display = io.display;
    if (display) io.display = [&](const std::string &mime, const std::string &data) {
        text.finish();
        display(mime, data);
    };
    const auto clear = io.clear_output;
    if (clear) io.clear_output = [&]() { text.finish(); clear(); };
    const auto input = io.input;
    if (input) io.input = [&](std::string &record) {
        text.finish();
        return input(record);
    };
    io.output = [&](int fd, const std::string &bytes) { text.write(fd, bytes); };
    FortranEvaluator::CellResult cell = e.execute(code, verbose, io);
    text.finish();
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

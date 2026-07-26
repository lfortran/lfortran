#include <iostream>

#include <stdio.h>
#include <stdlib.h>

#ifdef _WIN32
#    include <io.h>
#    define fileno _fileno
#    define dup _dup
#    define dup2 _dup2
#    define close _close
#    include <fcntl.h>
#else
#    include <unistd.h>
#endif

#include <xeus/xinterpreter.hpp>
#include <xeus/xkernel.hpp>
#include <xeus/xkernel_configuration.hpp>
#include <xeus/xhelper.hpp>
#ifdef __EMSCRIPTEN__
#include <emscripten/bind.h>
#include <xeus/xembind.hpp>
#else
#include <xeus-zmq/xzmq_context.hpp>
#include <xeus-zmq/xserver_zmq.hpp>
#endif

#include <nlohmann/json.hpp>

#include <lfortran/fortran_kernel.h>
#include <lfortran/parser/parser.h>
#include <lfortran/semantics/ast_to_asr.h>
#include <libasr/codegen/asr_to_llvm.h>
#include <lfortran/fortran_evaluator.h>
#include <lfortran/utils.h>
#include <libasr/asr_utils.h>
#include <libasr/string_utils.h>

namespace nl = nlohmann;

// ── Jupyter display_data bridge ──────────────────────────────────────────────
// These C-linkage symbols are called from JIT'd Fortran code via bind(C)
// declarations injected at REPL startup in configure_impl().
// Native (ORC JIT): resolved from the host process's exported symbol table.
// WASM: defined in the MAIN_MODULE; side modules import via --allow-undefined
// and RTLD_DEFAULT resolves them at dlopen time.

#ifdef _WIN32
#define LFORTRAN_KERNEL_API __declspec(dllexport)
#else
#define LFORTRAN_KERNEL_API __attribute__((visibility("default")))
#endif

extern "C" {

// Generic display bridge: publish arbitrary MIME type + data to Jupyter.
// Users can send HTML, SVG, LaTeX, Markdown, base64-encoded images, etc.
// This is the extensibility point - all format-specific encoding (BMP, PNG, etc.)
// should be done in Fortran user code, not hardcoded here.
LFORTRAN_KERNEL_API void lfortran_display_data(const char* mime_type, const char* data) {
    if (!mime_type || !data) return;
    nl::json bundle = nl::json::object();
    bundle[mime_type] = std::string(data);
    xeus::get_interpreter().display_data(
        std::move(bundle), nl::json::object(), nl::json::object());
}

} // extern "C"

// ── Display bootstrap module ──────────────────────────────────────────────────
// Evaluated once in configure_impl() before any user cell.
// Provides the single generic display primitive: display_data(mime_type, data).
// All format-specific encoding should be done in user Fortran code.

static constexpr const char* kDisplaySetupCode = R"fortran(
module lfortran_display
  use iso_c_binding, only: c_char, c_null_char
  implicit none

  interface
    subroutine lf_display_data(mime, payload) bind(C, name="lfortran_display_data")
      import :: c_char
      character(kind=c_char), intent(in) :: mime(*), payload(*)
    end subroutine
  end interface

contains

  ! Generic display: send any MIME type + data to Jupyter
  ! Examples:
  !   call display_data("text/html", "<h1>Hello</h1>")
  !   call display_data("image/svg+xml", svg_string)
  !   call display_data("image/bmp", base64_bmp_string)
  subroutine display_data(mime_type, data)
    character(len=*), intent(in) :: mime_type, data
    call lf_display_data(trim(mime_type)//c_null_char, trim(data)//c_null_char)
  end subroutine

end module lfortran_display
)fortran";

namespace LCompilers::LFortran {


    class RedirectStdout
    {
    public:
        RedirectStdout(std::string &out) : _out{out} {
            stdout_fileno = fileno(stdout);
            std::cout << std::flush;
            fflush(stdout);
            saved_stdout = dup(stdout_fileno);
#ifdef _WIN32
            if (_pipe(out_pipe, 65536, O_BINARY) != 0) {
#else
            if (pipe(out_pipe) != 0) {
#endif
                throw LCompilersException("pipe() failed");
            }
            dup2(out_pipe[1], stdout_fileno);
            close(out_pipe[1]);
            printf("X");
        }

        ~RedirectStdout() {
            fflush(stdout);
            read(out_pipe[0], buffer, MAX_LEN);
            dup2(saved_stdout, stdout_fileno);
            _out = std::string(&buffer[1]);
        }
    private:
        std::string &_out;
        static const size_t MAX_LEN=1024;
        char buffer[MAX_LEN+1] = {0};
        int out_pipe[2];
        int saved_stdout;
        int stdout_fileno;
    };

    class custom_interpreter : public xeus::xinterpreter
    {
    private:
        CompilerOptions compiler_options;
        FortranEvaluator e;

    public:
        custom_interpreter() : compiler_options{}, e{compiler_options} {
            e.compiler_options.interactive = true;
            e.compiler_options.po.runtime_library_dir =
                LCompilers::LFortran::get_runtime_library_dir();
        }
        virtual ~custom_interpreter() = default;

    private:

        void configure_impl() override;

        void execute_request_impl(send_reply_callback cb,
                                  int execution_counter,
                                  const std::string& code,
                                  //bool silent,
                                  //bool store_history,
                                  xeus::execute_request_config config,
                                  nl::json user_expressions) override;

        nl::json complete_request_impl(const std::string& code,
                                       int cursor_pos) override;

        nl::json inspect_request_impl(const std::string& code,
                                      int cursor_pos,
                                      int detail_level) override;

        nl::json is_complete_request_impl(const std::string& code) override;

        nl::json kernel_info_request_impl() override;

        nl::json shutdown_request_impl(bool restart) override;
        nl::json interrupt_request_impl() override;
    };

    
    void custom_interpreter::execute_request_impl(send_reply_callback cb,
                                                  int execution_counter, // Typically the cell number
                                                  const std::string& code, // Code to execute
                                                  xeus::execute_request_config, //config
                                                  nl::json /*user_expressions*/)
    {
        FortranEvaluator::EvalResult r;
        std::string std_out;
        std::string code0;
        CompilerOptions cu;
        try {
            if (startswith(code, "%%showast")) {
                code0 = code.substr(code.find("\n")+1);
                LocationManager lm;
                {
                    LocationManager::FileLocations fl;
                    fl.in_filename = "input";
                    std::ofstream out("input");
                    out << code0;
                    lm.files.push_back(fl);
                }
                diag::Diagnostics diagnostics;
                Result<std::string>
                    res = e.get_ast(code0, lm, diagnostics);
                nl::json result;
                if (res.ok) {
                    publish_stream("stdout", res.result);
                    result["status"] = "ok";
                    result["payload"] = nl::json::array();
                    result["user_expressions"] = nl::json::object();
                } else {
                    std::string msg = diagnostics.render(lm, cu);
                    publish_stream("stderr", msg);
                    result["status"] = "error";
                    result["ename"] = "CompilerError";
                    result["evalue"] = msg;
                    result["traceback"] = nl::json::array();
                }
                cb(result);
                return;
            }
            if (startswith(code, "%%showasr")) {
                code0 = code.substr(code.find("\n")+1);
                LocationManager lm;
                {
                    LocationManager::FileLocations fl;
                    fl.in_filename = "input";
                    std::ofstream out("input");
                    out << code0;
                    lm.files.push_back(fl);
                }
                diag::Diagnostics diagnostics;
                Result<std::string>
                res = e.get_asr(code0, lm, diagnostics);
                nl::json result;
                if (res.ok) {
                    publish_stream("stdout", res.result);
                    result["status"] = "ok";
                    result["payload"] = nl::json::array();
                    result["user_expressions"] = nl::json::object();
                } else {
                    std::string msg = diagnostics.render(lm, cu);
                    publish_stream("stderr", msg);
                    result["status"] = "error";
                    result["ename"] = "CompilerError";
                    result["evalue"] = msg;
                    result["traceback"] = nl::json::array();
                }
                cb(result);
                return;
            }
            if (startswith(code, "%%showllvm")) {
                code0 = code.substr(code.find("\n")+1);
                LocationManager lm;
                {
                    LocationManager::FileLocations fl;
                    fl.in_filename = "input";
                    std::ofstream out("input");
                    out << code0;
                    lm.files.push_back(fl);
                }
                LCompilers::PassManager lpm;
                lpm.use_default_passes();
                diag::Diagnostics diagnostics;
                Result<std::string>
                res = e.get_llvm(code0, lm, lpm, diagnostics);
                nl::json result;
                if (res.ok) {
                    publish_stream("stdout", res.result);
                    result["status"] = "ok";
                    result["payload"] = nl::json::array();
                    result["user_expressions"] = nl::json::object();
                } else {
                    std::string msg = diagnostics.render(lm, cu);
                    publish_stream("stderr", msg);
                    result["status"] = "error";
                    result["ename"] = "CompilerError";
                    result["evalue"] = msg;
                    result["traceback"] = nl::json::array();
                }
                cb(result);
                return;
            }
            if (startswith(code, "%%showasm")) {
                code0 = code.substr(code.find("\n")+1);
                LocationManager lm;
                {
                    LocationManager::FileLocations fl;
                    fl.in_filename = "input";
                    std::ofstream out("input");
                    out << code0;
                    lm.files.push_back(fl);
                }
                LCompilers::PassManager lpm;
                lpm.use_default_passes();
                diag::Diagnostics diagnostics;
                Result<std::string>
                res = e.get_asm(code0, lm, lpm, diagnostics);
                nl::json result;
                if (res.ok) {
                    publish_stream("stdout", res.result);
                    result["status"] = "ok";
                    result["payload"] = nl::json::array();
                    result["user_expressions"] = nl::json::object();
                } else {
                    std::string msg = diagnostics.render(lm, cu);
                    publish_stream("stderr", msg);
                    result["status"] = "error";
                    result["ename"] = "CompilerError";
                    result["evalue"] = msg;
                    result["traceback"] = nl::json::array();
                }
                cb(result);
                return;
            }
            if (startswith(code, "%%showcpp")) {
                code0 = code.substr(code.find("\n")+1);
                LocationManager lm;
                {
                    LocationManager::FileLocations fl;
                    fl.in_filename = "input";
                    std::ofstream out("input");
                    out << code0;
                    lm.files.push_back(fl);
                }
                diag::Diagnostics diagnostics;
                Result<std::string>
                res = e.get_cpp(code0, lm, diagnostics, 1);
                nl::json result;
                if (res.ok) {
                    publish_stream("stdout", res.result);
                    result["status"] = "ok";
                    result["payload"] = nl::json::array();
                    result["user_expressions"] = nl::json::object();
                } else {
                    std::string msg = diagnostics.render(lm, cu);
                    publish_stream("stderr", msg);
                    result["status"] = "error";
                    result["ename"] = "CompilerError";
                    result["evalue"] = msg;
                    result["traceback"] = nl::json::array();
                }
                cb(result);
                return;
            }
            if (startswith(code, "%%showfmt")) {
                code0 = code.substr(code.find("\n")+1);
                LocationManager lm;
                {
                    LocationManager::FileLocations fl;
                    fl.in_filename = "input";
                    std::ofstream out("input");
                    out << code0;
                    lm.files.push_back(fl);
                }
                diag::Diagnostics diagnostics;
                Result<std::string>
                res = e.get_fmt(code0, lm, diagnostics);
                nl::json result;
                if (res.ok) {
                    publish_stream("stdout", res.result);
                    result["status"] = "ok";
                    result["payload"] = nl::json::array();
                    result["user_expressions"] = nl::json::object();
                } else {
                    std::string msg = diagnostics.render(lm, cu);
                    publish_stream("stderr", msg);
                    result["status"] = "error";
                    result["ename"] = "CompilerError";
                    result["evalue"] = msg;
                    result["traceback"] = nl::json::array();
                }
                cb(result);
                return;
            }

            RedirectStdout s(std_out);
            code0 = code;
            LocationManager lm;
            {
                LocationManager::FileLocations fl;
                fl.in_filename = "input";
                std::ofstream out("input");
                out << code0;
                lm.files.push_back(fl);
            }
            LCompilers::PassManager lpm;
            lpm.use_default_passes();
            diag::Diagnostics diagnostics;
            Result<FortranEvaluator::EvalResult>
            res = e.evaluate(code0, false, lm, lpm, diagnostics);
            if (res.ok) {
                r = res.result;
            } else {
                std::string msg = diagnostics.render(lm, cu);
                publish_stream("stderr", msg);
                nl::json result;
                result["status"] = "error";
                result["ename"] = "CompilerError";
                result["evalue"] = msg;
                result["traceback"] = nl::json::array();
                cb(result);
                return;
            }
        } catch (const LCompilersException &e) {
            publish_stream("stderr", "LFortran Exception: " + e.msg());
            nl::json result;
            result["status"] = "error";
            result["ename"] = "LCompilersException";
            result["evalue"] = e.msg();
            result["traceback"] = nl::json::array();
            cb(result);
            return;
        }

        if (std_out.size() > 0) {
            publish_stream("stdout", std_out);
        }

        switch (r.type) {
            case (LCompilers::FortranEvaluator::EvalResult::integer4) : {
                nl::json pub_data;
                pub_data["text/plain"] = std::to_string(r.i32);
                publish_execution_result(execution_counter, std::move(pub_data), nl::json::object());
                break;
            }
            case (LCompilers::FortranEvaluator::EvalResult::integer8) : {
                nl::json pub_data;
                pub_data["text/plain"] = std::to_string(r.i64);
                publish_execution_result(execution_counter, std::move(pub_data), nl::json::object());
                break;
            }
            case (LCompilers::FortranEvaluator::EvalResult::real4) : {
                nl::json pub_data;
                pub_data["text/plain"] = std::to_string(r.f32);
                publish_execution_result(execution_counter, std::move(pub_data), nl::json::object());
                break;
            }
            case (LCompilers::FortranEvaluator::EvalResult::real8) : {
                nl::json pub_data;
                pub_data["text/plain"] = std::to_string(r.f64);
                publish_execution_result(execution_counter, std::move(pub_data), nl::json::object());
                break;
            }
            case (LCompilers::FortranEvaluator::EvalResult::complex4) : {
                nl::json pub_data;
                pub_data["text/plain"] = "(" + std::to_string(r.c32.re) + ", " + std::to_string(r.c32.im) + ")";
                publish_execution_result(execution_counter, std::move(pub_data), nl::json::object());
                break;
            }
            case (LCompilers::FortranEvaluator::EvalResult::complex8) : {
                nl::json pub_data;
                pub_data["text/plain"] = "(" + std::to_string(r.c64.re) + ", " + std::to_string(r.c64.im) + ")";
                publish_execution_result(execution_counter, std::move(pub_data), nl::json::object());
                break;
            }
            case (LCompilers::FortranEvaluator::EvalResult::statement) : {
                break;
            }
            case (LCompilers::FortranEvaluator::EvalResult::none) : {
                break;
            }
            default : throw LCompilersException("Return type not supported");
        }

        nl::json result;
        result["status"] = "ok";
        result["payload"] = nl::json::array();
        result["user_expressions"] = nl::json::object();
        cb(result);
        return;
    }
    
    void custom_interpreter::configure_impl()
    {
        xeus::register_interpreter(this);

        // Inject the lfortran_display module before the first user cell so that
        // display_html(), display_image(), display_svg() etc. are available in
        // every subsequent cell.
        LocationManager lm;
        {
            LocationManager::FileLocations fl;
            fl.in_filename = "input";
            std::ofstream out("input");
            out << kDisplaySetupCode;
            lm.files.push_back(fl);
        }
        LCompilers::PassManager lpm;
        lpm.use_default_passes();
        diag::Diagnostics diagnostics;
        CompilerOptions cu;
        auto res = e.evaluate(kDisplaySetupCode, false, lm, lpm, diagnostics);
        if (!res.ok) {
            std::string msg = diagnostics.render(lm, cu);
            std::cerr << "[xlfortran] Warning: display setup failed: " << msg << "\n";
        }
    }

    nl::json custom_interpreter::complete_request_impl(const std::string& code,
                                                       int cursor_pos)
    {
        nl::json result;

        // Code starts with 'H', it could be the following completion
        if (code[0] == 'H')
        {
            result["status"] = "ok";
            result["matches"] = {"Hello", "Hey", "Howdy"};
            result["cursor_start"] = 5;
            result["cursor_end"] = cursor_pos;
        }
        // No completion result
        else
        {
            result["status"] = "ok";
            result["matches"] = nl::json::array();
            result["cursor_start"] = cursor_pos;
            result["cursor_end"] = cursor_pos;
        }

        return result;
    }

    nl::json custom_interpreter::inspect_request_impl(const std::string& code,
                                                      int /*cursor_pos*/,
                                                      int /*detail_level*/)
    {
        nl::json result;

        if (code.compare("print") == 0)
        {
            result["found"] = true;
            result["text/plain"] = "Print objects to the text stream file, [...]";
        }
        else
        {
            result["found"] = false;
        }

        result["status"] = "ok";
        return result;
    }

    nl::json custom_interpreter::is_complete_request_impl(const std::string& /*code*/)
    {
        nl::json result;

        // if (is_complete(code))
        // {
            result["status"] = "complete";
        // }
        // else
        // {
        //    result["status"] = "incomplete";
        //    result["indent"] = 4;
        //}

        return result;
    }

    nl::json custom_interpreter::kernel_info_request_impl()
    {
        nl::json result;
        std::string version = LFORTRAN_VERSION;
        std::string banner = ""
            "LFortran " + version + "\n"
            "Jupyter kernel for Fortran";
        result["banner"] = banner;
        result["implementation"] = "LFortran";
        result["implementation_version"] = version;
        result["language_info"]["name"] = "fortran";
        result["language_info"]["version"] = "2018";
        result["language_info"]["mimetype"] = "text/x-fortran";
        result["language_info"]["file_extension"] = ".f90";
        return result;
    }

    nl::json custom_interpreter::shutdown_request_impl(bool restart) {
        std::cout << "Bye!!" << std::endl;
        return xeus::create_shutdown_reply(restart);
    }

    nl::json custom_interpreter::interrupt_request_impl() {
        return xeus::create_interrupt_reply();
    }

    int run_kernel(const std::string &connection_filename)
    {
#ifdef __EMSCRIPTEN__
        (void)connection_filename;
        throw LCompilersException("run_kernel() is not available in the WASM build");
        return 1;
#else
        std::unique_ptr<xeus::xcontext> context = xeus::make_zmq_context();

        // Create interpreter instance
        using interpreter_ptr = std::unique_ptr<custom_interpreter>;
        interpreter_ptr interpreter = interpreter_ptr(new custom_interpreter());

        using history_manager_ptr = std::unique_ptr<xeus::xhistory_manager>;
        history_manager_ptr hist = xeus::make_in_memory_history_manager();

        nl::json debugger_config;

        // Load configuration file
        xeus::xconfiguration config = xeus::load_configuration(connection_filename);

        // Create kernel instance and start it
        xeus::xkernel kernel(config,
                             xeus::get_user_name(),
                             std::move(context),
                             std::move(interpreter),
                             xeus::make_xserver_default,
                             std::move(hist),
                             xeus::make_console_logger(xeus::xlogger::msg_type,
                                                       xeus::make_file_logger(xeus::xlogger::content, "xeus.log")),
                             xeus::make_null_debugger,
                             debugger_config);

        std::cout <<
            "Starting xeus-fortran kernel...\n\n"
            "If you want to connect to this kernel from an other client, you can use"
            " the " + connection_filename + " file."
            << std::endl;

        kernel.start();

        return 0;
#endif // __EMSCRIPTEN__
    }

} // namespace LCompilers::LFortran

#ifdef __EMSCRIPTEN__
namespace {
    LCompilers::LFortran::custom_interpreter* make_interpreter(emscripten::val /*js_args*/)
    {
        return new LCompilers::LFortran::custom_interpreter();
    }
}

EMSCRIPTEN_BINDINGS(my_module)
{
    xeus::export_core();
    xeus::export_kernel<LCompilers::LFortran::custom_interpreter,
                        &make_interpreter>("xkernel");
}
#endif // __EMSCRIPTEN__

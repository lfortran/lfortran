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
            auto n = read(out_pipe[0], buffer, MAX_LEN);
            dup2(saved_stdout, stdout_fileno);
            close(saved_stdout);
            close(out_pipe[0]);
            // The first byte is the "X" written by the constructor.
            _out = n > 1 ? std::string(&buffer[1], n - 1) : std::string();
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
#if XEUS_VERSION_MAJOR > 5
        nl::json shutdown_request_impl(bool restart) override;
        nl::json interrupt_request_impl() override;
#else
	void shutdown_request_impl() override;
#endif
    };

    
    void custom_interpreter::execute_request_impl(send_reply_callback cb,
                                                  int execution_counter,
                                                  const std::string& code,
                                                  xeus::execute_request_config config,
                                                  nl::json /*user_expressions*/)
    {
        const bool silent = config.silent;
        FortranEvaluator::CellIO io;
        io.display = [&](const std::string &mime, const std::string &data) {
            if (!silent) display_data(nl::json{{mime, data}}, nl::json::object(),
                nl::json::object());
        };
        io.clear_output = [&]() { if (!silent) clear_output(false); };
        FortranEvaluator::CellResult cell;
        std::string std_out;
        try {
            RedirectStdout capture(std_out);
            cell = e.execute(code, false, io);
        } catch (const LCompilersException &error) {
            // Capturing stdout failed; the request still needs a reply.
            cell.ok = false;
            cell.error_name = error.name();
            cell.error_value = error.msg();
        }
        if (!silent) {
            if (!std_out.empty()) publish_stream("stdout", std_out);
            if (!cell.output.empty()) publish_stream("stdout", cell.output);
            if (!cell.ok) publish_stream("stderr", cell.error_value);
            else if (!cell.diagnostics.empty()) publish_stream("stderr", cell.diagnostics);
        }
        nl::json result;
        if (cell.ok) {
            if (cell.value.has_value() && !silent) {
                publish_execution_result(execution_counter,
                    nl::json{{"text/plain", cell.value.text()}}, nl::json::object());
            }
            result["status"] = "ok";
            result["payload"] = nl::json::array();
            result["user_expressions"] = nl::json::object();
        } else {
            result["status"] = "error";
            result["ename"] = cell.error_name;
            result["evalue"] = cell.error_value;
            result["traceback"] = nl::json::array();
        }
        cb(result);
    }

    void custom_interpreter::configure_impl()
    {
        xeus::register_interpreter(this);
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
        std::string version = LFORTRAN_VERSION;
        std::string banner = ""
            "LFortran " + version + "\n"
            "Jupyter kernel for Fortran";
        return xeus::create_info_reply(
            "LFortran",
            version,
            "fortran",
            "2018",
            "text/x-fortran",
            ".f90",
            "",
            std::string("text/x-fortran"),
            "",
            banner
        );
    }

#if XEUS_VERSION_MAJOR > 5
    nl::json custom_interpreter::shutdown_request_impl(bool restart) {
        std::cout << "Bye!!" << std::endl;
        return xeus::create_shutdown_reply(restart);
    }

    nl::json custom_interpreter::interrupt_request_impl() {
        return xeus::create_interrupt_reply();
    }
#else
    void custom_interpreter::shutdown_request_impl() {
        std::cout << "Bye!!" << std::endl;
    }
#endif

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

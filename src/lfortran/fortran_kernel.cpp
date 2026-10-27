#include <iostream>
#include <sstream>

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
#include <xeus/xinput.hpp>
#endif

#include <nlohmann/json.hpp>

#include <lfortran/fortran_kernel.h>
#include <lfortran/fortran_evaluator.h>
#include <lfortran/jupyter.h>
#include <lfortran/utils.h>

namespace nl = nlohmann;

namespace LCompilers::LFortran {


    // nlohmann::json throws on a string that is not UTF-8, which would leave
    // the request without a reply.
    using kernel::utf8;

    class custom_interpreter : public xeus::xinterpreter
    {
    private:
        CompilerOptions compiler_options;
        FortranEvaluator e;

    public:
        explicit custom_interpreter(const CompilerOptions &options = {})
                : compiler_options{options}, e{compiler_options} {
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
            if (!silent) display_data(nl::json{{utf8(mime), utf8(data)}},
                nl::json::object(), nl::json::object());
        };
        io.clear_output = [&]() { if (!silent) clear_output(false); };
        // A disabled input channel must not read the host terminal.
        if (!config.allow_stdin) io.input = [](std::string &) { return 1; };
#ifndef __EMSCRIPTEN__
        // Xeus's own stdin channel. It blocks until an input_reply arrives,
        // including after the client that was asked has gone, so the clients
        // behind this cell wait with it. "\x04" is end of file.
        else io.input = [](std::string &record) {
            record = xeus::blocking_input_request("", false);
            return record == "\x04" ? -1 : 0;
        };
#endif
        // `lfortran -v` is the execute_request field lfortran_verbose. Xeus
        // does not give the content to the interpreter, so the client also
        // puts that field in the header.
        const nl::json &header = parent_header();
        const auto field = header.find("lfortran_verbose");
        const bool verbose = field != header.end() && field->is_boolean()
            && field->get<bool>();
        FortranEvaluator::CellResult cell = kernel::execute(e, code, io,
            [&](const std::string &name, const std::string &text) {
                if (!silent) publish_stream(name, text);
            },
            [&](const std::string &text) {
                if (!silent) publish_execution_result(execution_counter,
                    nl::json{{"text/plain", text}}, nl::json::object());
            }, verbose);
        nl::json result;
        if (cell.ok) {
            result["status"] = "ok";
            result["payload"] = nl::json::array();
            result["user_expressions"] = nl::json::object();
        } else {
            result["status"] = "error";
            result["ename"] = cell.error_name;
            result["evalue"] = utf8(cell.error_value);
            nl::json traceback = nl::json::array();
            std::istringstream lines(cell.error_traceback);
            for (std::string line; std::getline(lines, line);) {
                traceback.push_back(utf8(line));
            }
            result["traceback"] = traceback;
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
        return xeus::create_info_reply(
            kernel::implementation,
            LFORTRAN_VERSION,
            kernel::language,
            kernel::language_version,
            kernel::mimetype,
            kernel::file_extension,
            "",
            std::string(kernel::mimetype),
            "",
            kernel::banner(LFORTRAN_VERSION)
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

    int run_kernel(const std::string &connection_filename, CompilerOptions &options,
        bool managed)
    {
#ifdef __EMSCRIPTEN__
        (void)connection_filename;
        (void)options;
        (void)managed;
        throw LCompilersException("run_kernel() is not available in the WASM build");
        return 1;
#else
        // `lfortran kernel --managed` is the built-in kernel. Xeus cannot
        // tell when its last client disconnects. The prompt does not pass
        // this flag: it ends the kernel it started.
        if (managed) return jupyter::run_builtin_kernel(connection_filename, options, true);

        std::unique_ptr<xeus::xcontext> context = xeus::make_zmq_context();

        // Create interpreter instance
        using interpreter_ptr = std::unique_ptr<custom_interpreter>;
        interpreter_ptr interpreter = interpreter_ptr(new custom_interpreter(options));

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
        jupyter::kernel_started();

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

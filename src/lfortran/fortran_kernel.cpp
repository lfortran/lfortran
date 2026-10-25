#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <variant>

#include <xeus/xinterpreter.hpp>
#include <xeus/xkernel.hpp>
#include <xeus/xkernel_configuration.hpp>
#include <xeus/xhelper.hpp>
#include <xeus/xinput.hpp>
#ifdef __EMSCRIPTEN__
#include <emscripten/bind.h>
#include <xeus/xembind.hpp>
#else
#include <xeus/xmessage.hpp>
#include <xeus-zmq/xzmq_context.hpp>
#include <xeus-zmq/xserver_zmq.hpp>
#endif

#include <nlohmann/json.hpp>

#include <lfortran/fortran_kernel.h>
#include <lfortran/fortran_evaluator.h>
#include <lfortran/jupyter.h>
#include <lfortran/utils.h>
#include <libasr/string_utils.h>

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

        // Set by the server: the `lfortran_verbose` of the execute_request
        // (`lfortran -v`), and whether the client asked for input has gone.
        bool verbose = false;
        bool input_lost = false;

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
        // A READ asks the client that ran the cell; "\x04" is end of file.
        else io.input = [this](std::string &record) {
            input_lost = false;
            record = xeus::blocking_input_request("", false);
            if (input_lost) return 1;
            return record == "\x04" ? -1 : 0;
        };
#endif
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

#if !defined(__EMSCRIPTEN__) && XEUS_VERSION_MAJOR > 5
    // Xeus's ZeroMQ server, but for its stdin channel, which cannot tell when
    // the client that a READ asks disconnects: the kernel would wait for it,
    // and every other client with it. jupyter::InputChannel listens on the
    // stdin port instead. The server also passes on `lfortran_verbose`, an
    // LFortran-specific field of execute_request, which Xeus does not give
    // the interpreter, and aborts requests as the protocol says.
    class kernel_server final : public xeus::xserver_zmq
    {
        xeus::xkernel_configuration config;
        custom_interpreter &interpreter;
        std::unique_ptr<jupyter::InputChannel> input;
        const std::string user = xeus::get_user_name(), session = xeus::new_xguid().c_str();
        // Set while the execute_requests that wait behind a failed cell are
        // aborted
        bool aborting = false;

        // Xeus's own stdin socket listens on a free port, which no client
        // knows.
        static xeus::xkernel_configuration without_stdin(xeus::xkernel_configuration config)
        {
            config.m_stdin_port = "";
            return config;
        }

    public:
        // `input` listens on the stdin port of `config`, which it serves.
        kernel_server(xeus::xcontext &context, const xeus::xkernel_configuration &config,
            nl::json::error_handler_t eh, custom_interpreter &interpreter,
            std::unique_ptr<jupyter::InputChannel> input)
            : xserver_zmq(context, without_stdin(config), eh), config(config),
              interpreter(interpreter), input(std::move(input)) {}

        // Whether jupyter::InputChannel can serve the stdin channel of
        // `config`: IPv4 over TCP, with HMAC-SHA256 signatures
        static bool serves(const xeus::xkernel_configuration &config)
        {
            return config.m_transport == "tcp" && !config.m_stdin_port.empty()
                && (config.m_key.empty() || config.m_signature_scheme == "hmac-sha256")
                && jupyter::InputChannel::serves(config.m_ip);
        }

    private:
        // As Xeus's default server
        void start_impl(xeus::xpub_message message) override
        {
            start_publisher_thread();
            start_heartbeat_thread();
            publish(std::move(message), xeus::channel::SHELL);
            while (!is_stopped()) {
                auto received = poll_channels(-1);
                if (received) dispatch(std::move(*received));
            }
            stop_channels();
        }

        void dispatch(message_channel received)
        {
            if (aborting && received.second == xeus::channel::SHELL
                    && received.first.header().value("msg_type", "") == "execute_request") {
                abort(received.first);
            } else if (received.second == xeus::channel::SHELL) {
                const nl::json &content = received.first.content();
                auto verbose = content.find("lfortran_verbose");
                interpreter.verbose = verbose != content.end() && verbose->is_boolean()
                    && verbose->get<bool>();
                notify_shell_listener(std::move(received.first));
            } else {
                notify_control_listener(std::move(received.first));
            }
        }

        // After a cell fails with stop_on_error. Xeus would abort the
        // requests that wait (and, once it has aborted one, those that come
        // in the next 50 ms) with an error reply and no idle status, for
        // which a client waits forever.
        // Instead the execute_requests that wait are aborted as by the
        // built-in kernel, and the other requests handled.
        void abort_queue_impl(const listener &, long) override
        {
            struct Aborting {
                bool &flag;
                Aborting(bool &flag) : flag(flag) { flag = true; }
                ~Aborting() { flag = false; }
            } guard(aborting);
            while (auto received = poll_channels(0)) dispatch(std::move(*received));
        }

        // An aborted reply between busy and idle, without running the cell
        void abort(const xeus::xmessage &request)
        {
            auto status = [&](const char *state) {
                publish(xeus::xpub_message("kernel_core.lfortran.status",
                    xeus::make_header("status", user, session), request.header(),
                    nl::json::object(), nl::json{{"execution_state", state}},
                    xeus::buffer_sequence()), xeus::channel::SHELL);
            };
            status("busy");
            send_shell_message(xeus::xmessage(request.identities(),
                xeus::make_header("execute_reply", user, session), request.header(),
                nl::json::object(), nl::json{{"status", "aborted"}}, xeus::buffer_sequence()));
            status("idle");
        }

        void stop_impl() override
        {
            set_request_stop(true);
        }

        void send_stdin_impl(xeus::xmessage message) override
        {
            auto dump = [](const nl::json &part) {
                return part.dump(-1, ' ', false, nl::json::error_handler_t::replace);
            };
            std::string parts[4] = {dump(message.header()), dump(message.parent_header()),
                dump(message.metadata()), dump(message.content())};
            const auto &identities = message.identities();
            std::string value;
            interpreter.input_lost = identities.empty()
                || !input->request(identities.front(), parts, value);
            if (interpreter.input_lost) return;
            notify_stdin_listener(xeus::xmessage(identities, nl::json::object(),
                message.header(), nl::json::object(), nl::json{{"value", value}},
                xeus::buffer_sequence()));
        }

        void update_config_impl(xeus::xkernel_configuration &c) const override
        {
            c.m_control_port = config.m_control_port;
            c.m_shell_port = config.m_shell_port;
            c.m_stdin_port = config.m_stdin_port;
            c.m_iopub_port = config.m_iopub_port;
            c.m_hb_port = config.m_hb_port;
        }
    };
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
        // Xeus does not tell when its last client disconnects, so the kernel
        // the prompt starts is the built-in one, as is a kernel started
        // without a connection file, which only the built-in one writes.
        if (managed || !std::ifstream(connection_filename).good()) {
            return jupyter::run_builtin_kernel(connection_filename, options, managed);
        }

        std::unique_ptr<xeus::xcontext> context = xeus::make_zmq_context();

        // Create interpreter instance
        using interpreter_ptr = std::unique_ptr<custom_interpreter>;
        interpreter_ptr interpreter = interpreter_ptr(new custom_interpreter(options));
        custom_interpreter &kernel_interpreter = *interpreter;

        using history_manager_ptr = std::unique_ptr<xeus::xhistory_manager>;
        history_manager_ptr hist = xeus::make_in_memory_history_manager();

        nl::json debugger_config;

        // Load configuration file
        xeus::xconfiguration config;
        try {
            config = xeus::load_configuration(connection_filename);
        } catch (const std::exception &error) {
            std::cerr << "error: " << connection_filename << ": " << error.what() << std::endl;
            return 1;
        }

#if XEUS_VERSION_MAJOR > 5
        auto make_server = [&kernel_interpreter](xeus::xcontext &context,
                const xeus::xconfiguration &config, nl::json::error_handler_t eh)
                -> std::unique_ptr<xeus::xserver> {
            auto kernel_config = std::get_if<xeus::xkernel_configuration>(&config);
            // Else Xeus's own server, whose READ waits even after the client
            // asked has gone, and which does not pass on `lfortran -v`.
            if (!kernel_config || !kernel_server::serves(*kernel_config)) {
                return xeus::make_xserver_default(context, config, eh);
            }
            // Before Xeus's stdin socket, which takes a free port, takes this one
            int port = std::stoi(kernel_config->m_stdin_port);
            auto input = std::make_unique<jupyter::InputChannel>(kernel_config->m_ip, port,
                kernel_config->m_key);
            return std::make_unique<kernel_server>(context, *kernel_config, eh,
                kernel_interpreter, std::move(input));
        };
#else
        // Xeus 5 has only its own server: a READ waits even after the client
        // asked has gone, `lfortran -v` is not passed on, and a failed cell
        // aborts the requests that wait as Xeus does.
        (void)kernel_interpreter;
        auto make_server = xeus::make_xserver_default;
#endif

        // Create kernel instance and start it
        std::unique_ptr<xeus::xkernel> kernel;
        try {
            kernel = std::make_unique<xeus::xkernel>(config,
                             xeus::get_user_name(),
                             std::move(context),
                             std::move(interpreter),
                             make_server,
                             std::move(hist),
                             xeus::make_console_logger(xeus::xlogger::msg_type,
                                                       xeus::make_file_logger(xeus::xlogger::content, "xeus.log")),
                             xeus::make_null_debugger,
                             debugger_config);
        } catch (const std::exception &error) {
            std::cerr << "error: " << error.what() << std::endl;
            return 1;
        }

        std::cout <<
            "Starting xeus-fortran kernel...\n\n"
            "If you want to connect to this kernel from an other client, you can use"
            " the " + connection_filename + " file."
            << std::endl;

        kernel->start();

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

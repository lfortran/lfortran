#include <iostream>
#include <sstream>
#include <csignal>
#include <deque>
#include <chrono>
#include <thread>

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
#include <xeus-zmq/xmiddleware.hpp>
#include <xeus/xinput.hpp>
#include <zmq.h>
#include <openssl/hmac.h>
#endif

#include <nlohmann/json.hpp>

#include <lfortran/fortran_kernel.h>
#include <lfortran/fortran_evaluator.h>
#include <lfortran/jupyter.h>
#include <lfortran/utils.h>

namespace nl = nlohmann;

namespace LCompilers::LFortran {

#ifndef __EMSCRIPTEN__
    // Unlike the split server, this keeps all request sockets on the
    // publishing thread. Only interrupts run during a cell; everything that
    // could re-enter the interpreter is retained until its worker has joined.
    class polling_server : public xeus::xserver_zmq {
        std::deque<xeus::xmessage> shell, control;

        void start_impl(xeus::xpub_message message) override {
            start_publisher_thread();
            start_heartbeat_thread();
            publish(std::move(message), xeus::channel::SHELL);
            while (!is_stopped()) {
                if (!control.empty()) {
                    auto next = std::move(control.front());
                    control.pop_front();
                    notify_control_listener(std::move(next));
                } else if (!shell.empty()) {
                    auto next = std::move(shell.front());
                    shell.pop_front();
                    notify_shell_listener(std::move(next));
                } else if (auto next = poll_channels(-1)) {
                    if (next->second == xeus::channel::SHELL) {
                        notify_shell_listener(std::move(next->first));
                    } else {
                        notify_control_listener(std::move(next->first));
                    }
                }
            }
            stop_channels();
        }

        void stop_impl() override { set_request_stop(true); }

    public:
        void abort_pending(const listener &listen) {
            while (!shell.empty()) {
                auto next = std::move(shell.front());
                shell.pop_front();
                listen(std::move(next));
            }
        }

        polling_server(xeus::xcontext &context, const xeus::xconfiguration &config,
                nl::json::error_handler_t eh) : xeus::xserver_zmq(context, config, eh) {}

        void poll_control() {
            for (int i = 0; i < 64; i++) {
                auto next = poll_channels(0);
                if (!next) break;
                auto &message = next->first;
                const auto type = message.header().value("msg_type", "");
                if (next->second == xeus::channel::SHELL) {
                    shell.push_back(std::move(message));
                } else if (type == "interrupt_request") {
                    FortranEvaluator::request_interrupt();
                    // Do not enter Xeus's control dispatcher here: its status
                    // publication would change the active cell's parent/state.
                    send_control_message(xeus::xmessage(message.identities(),
                        xeus::make_header("interrupt_reply",
                            message.header().value("username", ""),
                            message.header().value("session", "")),
                        message.header(), nl::json::object(),
                        nl::json{{"status", "ok"}}, {}));
                } else {
                    if (type == "shutdown_request") FortranEvaluator::request_interrupt();
                    control.push_back(std::move(message));
                }
            }
        }
    };

    class streaming_server;
    static thread_local streaming_server *active_streaming_server = nullptr;

#if XEUS_VERSION_MAJOR > 5
    using connection_configuration = xeus::xkernel_configuration;
#else
    using connection_configuration = xeus::xconfiguration;
#endif

    // Xeus-ZMQ forwards IOPub through an inproc PUB/SUB queue and then an
    // external XPUB socket. Both silently drop at their high-water marks.
    // Keep its request channels, but publish directly on a caller-owned
    // XPUB with backpressure, including status/idle and display messages.
    class streaming_server : public xeus::xserver {
        std::unique_ptr<void, decltype(&zmq_ctx_term)> context{nullptr, zmq_ctx_term};
        std::unique_ptr<void, decltype(&zmq_close)> publisher{nullptr, zmq_close};
        std::unique_ptr<polling_server> server;
        connection_configuration configuration;
        const EVP_MD *digest;
        nl::json::error_handler_t error_handler;
        std::string endpoint;

        static void checked(int result) {
            if (result < 0) throw LCompilersException(
                std::string("cannot publish kernel output: ") + zmq_strerror(zmq_errno()));
        }

        void send(const xeus::xpub_message &message) {
            // Consume subscription notifications; ZeroMQ applies the
            // subscriptions itself, even when evaluation is running.
            while (zmq_recv(publisher.get(), nullptr, 0, ZMQ_DONTWAIT) >= 0) {}
            std::string parts[] = {
                message.header().dump(-1, ' ', false, error_handler),
                message.parent_header().dump(-1, ' ', false, error_handler),
                message.metadata().dump(-1, ' ', false, error_handler),
                message.content().dump(-1, ' ', false, error_handler)
            };
            std::string signature;
            if (digest) {
                std::string data;
                for (auto &part : parts) data += part;
                unsigned char hash[EVP_MAX_MD_SIZE];
                unsigned int length = 0;
                if (!HMAC(digest, configuration.m_key.data(),
                        configuration.m_key.size(),
                        reinterpret_cast<const unsigned char*>(data.data()),
                        data.size(), hash, &length)) {
                    throw LCompilersException("cannot sign kernel output");
                }
                const char *hex = "0123456789abcdef";
                for (unsigned int i = 0; i < length; i++) {
                    signature += hex[hash[i] >> 4];
                    signature += hex[hash[i] & 15];
                }
            }
            auto frame = [&](const char *data, size_t size, bool more) {
                for (;;) {
                    int result = zmq_send(publisher.get(), data, size,
                        ZMQ_DONTWAIT | (more ? ZMQ_SNDMORE : 0));
                    if (result >= 0) return true;
                    if (zmq_errno() == EINTR) continue;
                    if (zmq_errno() != EAGAIN) checked(result);
                    poll_control();
                    if (FortranEvaluator::is_interrupt_requested()) {
                        // A stalled subscriber must not hold cancellation or
                        // join hostage. Drop this transport's undelivered
                        // backlog and disconnect subscribers, which reconnect
                        // normally; never claim complete output for that peer.
                        const int linger = 0;
                        checked(zmq_setsockopt(publisher.get(), ZMQ_LINGER,
                            &linger, sizeof(linger)));
                        publisher.reset();
                        open_publisher();
                        return false;
                    }
                    std::this_thread::sleep_for(std::chrono::milliseconds(2));
                }
            };
            if (!frame(message.topic().data(), message.topic().size(), true)) return;
            if (!frame("<IDS|MSG>", 9, true)) return;
            if (!frame(signature.data(), signature.size(), true)) return;
            for (int i = 0; i < 4; i++) {
                if (!frame(parts[i].data(), parts[i].size(),
                        i != 3 || !message.buffers().empty())) return;
            }
            for (size_t i = 0; i < message.buffers().size(); i++) {
                auto &buffer = message.buffers()[i];
                if (!frame(buffer.data(), buffer.size(), i + 1 < message.buffers().size())) return;
            }
        }

        xeus::xcontrol_messenger &get_control_messenger_impl() override {
            return server->get_control_messenger();
        }
        void send_shell_impl(xeus::xmessage message) override {
            server->send_shell(std::move(message));
        }
        void send_control_impl(xeus::xmessage message) override {
            server->send_control(std::move(message));
        }
        void send_stdin_impl(xeus::xmessage message) override {
            server->send_stdin(std::move(message));
        }
        void publish_impl(xeus::xpub_message message, xeus::channel) override {
            send(message);
        }
        void start_impl(xeus::xpub_message message) override {
            active_streaming_server = this;
            struct Reset {
                ~Reset() { active_streaming_server = nullptr; }
            } reset;
            send(message);
            server->start(std::move(message));
        }
        void abort_queue_impl(const listener &listen, long interval) override {
            server->abort_pending(listen);
            server->abort_queue(listen, interval);
        }
        void stop_impl() override {
            server->stop();
        }
        void update_config_impl(connection_configuration &config) const override {
            server->update_config(config);
            config.m_iopub_port = configuration.m_iopub_port;
        }

    public:
        void poll_control() { server->poll_control(); }

    private:
        void open_publisher() {
            publisher.reset(zmq_socket(context.get(), ZMQ_XPUB));
            if (!publisher) checked(-1);
            const int no_drop = 1, linger = 1000;
            checked(zmq_setsockopt(publisher.get(), ZMQ_XPUB_NODROP, &no_drop, sizeof(no_drop)));
            checked(zmq_setsockopt(publisher.get(), ZMQ_LINGER, &linger, sizeof(linger)));
            checked(zmq_bind(publisher.get(), endpoint.c_str()));
        }

    public:
        streaming_server(xeus::xcontext &xeus_context,
                const connection_configuration &config, nl::json::error_handler_t eh)
            : configuration(config), error_handler(eh) {
            const std::string prefix = "hmac-";
            digest = config.m_signature_scheme.compare(0, prefix.size(), prefix) == 0
                ? EVP_get_digestbyname(config.m_signature_scheme.substr(prefix.size()).c_str())
                : nullptr;
            if (!digest && config.m_signature_scheme != "none") {
                throw LCompilersException("unsupported kernel signature scheme");
            }
            context.reset(zmq_ctx_new());
            if (!context) checked(-1);
            const std::string port = config.m_iopub_port.empty()
                ? (config.m_transport == "tcp" ? "*" : xeus::find_free_port())
                : config.m_iopub_port;
            endpoint = xeus::get_end_point(config.m_transport, config.m_ip, port);
            open_publisher();
            char address[1024];
            size_t size = sizeof(address);
            checked(zmq_getsockopt(publisher.get(), ZMQ_LAST_ENDPOINT, address, &size));
            endpoint = address;
            const char separator = config.m_transport == "tcp" ? ':' : '-';
            configuration.m_iopub_port = endpoint.substr(endpoint.rfind(separator) + 1);
            auto request_config = config;
            request_config.m_iopub_port.clear();
            server = std::make_unique<polling_server>(xeus_context, request_config, eh);
            server->register_shell_listener([this](xeus::xmessage message) {
                notify_shell_listener(std::move(message));
            });
            server->register_control_listener([this](xeus::xmessage message) {
                notify_control_listener(std::move(message));
            });
            server->register_stdin_listener([this](xeus::xmessage message) {
                notify_stdin_listener(std::move(message));
            });
            server->register_internal_listener([this](nl::json message) {
                return notify_internal_listener(std::move(message));
            });
        }
    };

    static std::unique_ptr<xeus::xserver> make_streaming_server(xeus::xcontext &context,
            const xeus::xconfiguration &config, nl::json::error_handler_t eh) {
#if XEUS_VERSION_MAJOR > 5
        if (const auto *connection = std::get_if<connection_configuration>(&config)) {
            return std::make_unique<streaming_server>(context, *connection, eh);
        }
        return xeus::make_xserver_default(context, config, eh);
#else
        return std::make_unique<streaming_server>(context, config, eh);
#endif
    }
#endif

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
#ifndef __EMSCRIPTEN__
        io.poll_interrupt = []() {
            if (active_streaming_server) active_streaming_server->poll_control();
        };
#endif
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
        FortranEvaluator::request_interrupt();
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
                             make_streaming_server,
                             std::move(hist),
                             xeus::make_console_logger(xeus::xlogger::msg_type,
                                                       xeus::make_file_logger(xeus::xlogger::content, "xeus.log")),
                             xeus::make_null_debugger,
                             debugger_config);
        jupyter::kernel_started();
        std::signal(SIGINT, [](int) { FortranEvaluator::request_interrupt(); });

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

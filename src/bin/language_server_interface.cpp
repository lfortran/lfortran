#include <exception>
#include <filesystem>
#include <random>
#include <regex>
#include <stdexcept>
#include <thread>

#include <libasr/config.h>
#include <libasr/exception.h>

#include <server/logger.h>
#include <server/lsp_message_stream.h>
#include <server/lsp_specification.h>

#ifndef CLI11_HAS_FILESYSTEM
#define CLI11_HAS_FILESYSTEM 0
#endif // CLI11_HAS_FILESYSTEM
#include <bin/CLI11.hpp>

#include <bin/concurrent_lfortran_lsp_language_server.h>
#include <bin/language_server_interface.h>
#include <bin/parallel_lfortran_lsp_language_server.h>

namespace LCompilers::LLanguageServer::Interface {
    namespace fs = std::filesystem;

    namespace lc = LCompilers;
    namespace lsp = LCompilers::LanguageServerProtocol;

    auto validateConfigSection(const std::string &configSection) -> std::string {
        if (!std::regex_match(configSection, RE_CONFIG_SECTION)) {
            return ("Configuration section is not a valid sequence of dot-separated, "
                    "ECMAScript Ids: " + configSection) ;
        }

        if (std::regex_match(configSection, RE_RESERVED_CONFIG_IDS)) {
            return ("Configuration section contains a reserved Id for ECMAScript: " +
                    configSection);
        }

        return "";
    }

    auto existsAndIsWritable(const std::string &pathString) -> std::string {
        fs::path path = fs::absolute(pathString).lexically_normal();

        fs::path dir = path.parent_path();
        if (!fs::exists(dir) && !fs::create_directories(dir)) {
            throw lc::LCompilersException(
                "Cannot create log directory: " + dir.string()
            );
        }

        std::ofstream ofs(path);
        if (!ofs.is_open()) {
            throw lc::LCompilersException(
                "Path is not writable: " + path.string()
            );
        }
        ofs.close();

        return path.string();
    }

    auto existsAndIsExecutable(const std::string &pathString) -> std::string {
        fs::path path = fs::absolute(pathString).lexically_normal();

        if (!fs::exists(path)) {
            throw lc::LCompilersException(
                "Path does not exist: " + path.string()
            );
        }

        std::error_code ec;
        fs::file_status status = std::filesystem::status(path, ec);

        if (ec) {
            throw lc::LCompilersException(
                "Error getting file status: " + ec.message()
            );
        }

        fs::perms permissions = status.permissions();

#ifdef _WIN32
        if ((permissions & fs::perms::owner_exec) == fs::perms::none)
#else
        if ((permissions & (fs::perms::owner_exec |
                            fs::perms::group_exec |
                            fs::perms::others_exec)) == fs::perms::none)
#endif
        {
            throw lc::LCompilersException(
                "Path is not executable: " + path.string()
            );
        }

        return path.string();
    }

    const std::map<Language, std::string> LanguageNames = {
        {Language::FORTRAN, "FORTRAN"},
    };

    const std::map<Language, std::string> LanguageValues = {
        {Language::FORTRAN, "fortran"},
    };

    auto languageByValue(const std::string &value) -> Language {
        for (const auto &[lang_key, lang_value] : LanguageValues) {
            if (lang_value == value) {
                return lang_key;
            }
        }
        throw std::invalid_argument(
            "Invalid Language value: \"" + value + "\""
        );
    }

    const std::map<DataFormat, std::string> DataFormatNames = {
        {DataFormat::JSON_RPC, "JSON_RPC"},
    };

    const std::map<DataFormat, std::string> DataFormatValues = {
        {DataFormat::JSON_RPC, "json-rpc"},
    };

    auto dataFormatByValue(const std::string &value) -> DataFormat {
        for (const auto &[fmt_key, fmt_value] : DataFormatValues) {
            if (fmt_value == value) {
                return fmt_key;
            }
        }
        throw std::invalid_argument(
            "Invalid DataFormat value: \"" + value + "\""
        );
    }

    const std::map<CommunicationProtocol, std::string> CommunicationProtocolNames = {
        {CommunicationProtocol::STDIO, "STDIO"},
    };

    const std::map<CommunicationProtocol, std::string> CommunicationProtocolValues = {
        {CommunicationProtocol::STDIO, "stdio"},
    };

    auto communicationProtocolByValue(const std::string &value) -> CommunicationProtocol {
        for (const auto &[protocol_key, protocol_value] : CommunicationProtocolValues) {
            if (protocol_value == value) {
                return protocol_key;
            }
        }
        throw std::invalid_argument(
            "Invalid CommunicationProtocol value: \"" + value + "\""
        );
    }

    const std::map<ServerProtocol, std::string> ServerProtocolNames = {
        {ServerProtocol::LSP, "LSP"},
    };

    const std::map<ServerProtocol, std::string> ServerProtocolValues = {
        {ServerProtocol::LSP, "lsp"},
    };

    auto serverProtocolByValue(const std::string &value) -> ServerProtocol {
        for (const auto &[protocol_key, protocol_value] : ServerProtocolValues) {
            if (protocol_value == value) {
                return protocol_key;
            }
        }
        throw std::invalid_argument(
            "Invalid ServerProtocol value: \"" + value + "\""
        );
    }

    const std::map<ExecutionStrategy, std::string> ExecutionStrategyNames = {
        {ExecutionStrategy::PARALLEL, "PARALLEL"},
        {ExecutionStrategy::CONCURRENT, "CONCURRENT"},
    };

    const std::map<ExecutionStrategy, std::string> ExecutionStrategyValues {
        {ExecutionStrategy::PARALLEL, "parallel"},
        {ExecutionStrategy::CONCURRENT, "concurrent"},
    };

    auto executionStrategyByValue(const std::string &value) -> ExecutionStrategy {
        for (const auto &[strategy_key, strategy_value] : ExecutionStrategyValues) {
            if (strategy_value == value) {
                return strategy_key;
            }
        }
        throw std::invalid_argument(
            "Invalid ExecutionStrategy value: \"" + value + "\""
        );
    }

    LanguageServerInterface::LanguageServerInterface()
        : workspaceConfig(std::make_shared<lsc::LFortranLspConfig>())
    {
        // empty
    }

    auto LanguageServerInterface::prepare(CLI::App &app) -> CLI::App * {
        CLI::App *server = app.add_subcommand(
            "server",
            "LCompilers Language Server: Serves requests from language extensions in supported editors."
        );

        opts.language = Language::FORTRAN;
        server->add_option(
            "--language", opts.language,
            "Specifies the language to serve."
        )->capture_default_str()->transform(
            CLI::CheckedTransformer(
                transpose(LanguageValues),
                CLI::ignore_case
            )
        );

        opts.dataFormat = DataFormat::JSON_RPC;
        server->add_option(
            "--data-format", opts.dataFormat,
            "Specifies the data exchange format for requests."
        )->capture_default_str()->transform(
            CLI::CheckedTransformer(
                transpose(DataFormatValues),
                CLI::ignore_case
            )
        );

        opts.communicationProtocol = CommunicationProtocol::STDIO;
        server->add_option(
            "--communication-protocol", opts.communicationProtocol,
            "Specifies the communication protocol over which to serve requests."
        )->capture_default_str()->transform(
            CLI::CheckedTransformer(
                transpose(CommunicationProtocolValues),
                CLI::ignore_case
            )
        );

        opts.serverProtocol = ServerProtocol::LSP;
        server->add_option(
            "--server-protocol", opts.serverProtocol,
            "Specifies the language server protocol that defines how to serve requests."
        )->capture_default_str()->transform(
            CLI::CheckedTransformer(
                transpose(ServerProtocolValues),
                CLI::ignore_case
            )
        );

        opts.executionStrategy = ExecutionStrategy::CONCURRENT;
        server->add_option(
            "--execution-strategy", opts.executionStrategy,
            "Specifies the execution strategy for handling messages. The `parallel` strategy implies multiple messages may be processed alongside each other, while the `concurrent` strategy implies multiple messages may be processed but only one processor will be active at a time (they will yield control to each other)."
        )->capture_default_str()->transform(
            CLI::CheckedTransformer(
                transpose(ExecutionStrategyValues),
                CLI::ignore_case
            )
        );

        opts.numRequestThreads = 5;
        server->add_option(
            "--num-request-threads", opts.numRequestThreads,
            "Number of threads that serve requests."
        )->capture_default_str();

        opts.numWorkerThreads = std::thread::hardware_concurrency();
        server->add_option(
            "--num-worker-threads", opts.numWorkerThreads,
            "Number of threads that will handle sub-tasks of requests."
        )->capture_default_str();

        opts.enableLogging = false;
        server->add_flag(
            "--enable-logging", opts.enableLogging,
            "Enable logging to a file."
        )->capture_default_str();

        opts.configSection = "LFortran";
        server->add_option(
            "--config-section", opts.configSection,
            ("Identifies the server's configuration section within the "
             "workspace configuration.")
        )->capture_default_str()->check(validateConfigSection);

        workspaceConfig->openIssueReporterOnError = true;
        server->add_option(
            "--open-issue-reporter-on-error",
            workspaceConfig->openIssueReporterOnError,
            "Open a bug report if an internal error occurs."
        )->capture_default_str();

        workspaceConfig->maxNumberOfProblems = 100;
        server->add_option(
            "--max-number-of-problems",
            workspaceConfig->maxNumberOfProblems,
            "Maximum number of errors and warnings to report."
        )->capture_default_str();

        workspaceConfig->trace.server = lsp::TraceValues::Off;
        server->add_option(
            "--trace-server", workspaceConfig->trace.server,
            "Traces the communication between the language client and server."
        )->capture_default_str()->transform(
            CLI::CheckedTransformer(
                transpose(lsp::TraceValuesValues),
                CLI::ignore_case
            )
        );

        server->add_option(
            "--compiler-path", workspaceConfig->compiler.path,
            "Path to the LFortran compiler executable."
        )->transform(existsAndIsExecutable);

        server->add_option(
            "compiler_flags", workspaceConfig->compiler.flags,
            "Additional flags to pass to the LFortran compiler."
        );

        if (opts.enableLogging) {
            workspaceConfig->log.path = existsAndIsWritable("lfortran-language-server.log");
        } else {
            workspaceConfig->log.path = "";
        }
        server->add_option(
            "--log-path", workspaceConfig->log.path,
            "Path to where logs should be written."
        )->capture_default_str()->transform(existsAndIsWritable);

        workspaceConfig->log.level = lsl::Level::LOG_LEVEL_INFO;
        server->add_option(
            "--log-level", workspaceConfig->log.level,
            "Verbosity of log output"
        )->capture_default_str()->transform(
            CLI::CheckedTransformer(
                transpose(lsl::LevelValues),
                CLI::ignore_case
            )
        );

        workspaceConfig->log.prettyPrint = true;
        server->add_option(
            "--log-pretty-print", workspaceConfig->log.prettyPrint,
            "Whether to pretty-print JSON objects and arrays."
        )->capture_default_str();

        workspaceConfig->indentSize = 4;
        server->add_option(
            "--indent-size", workspaceConfig->indentSize,
            "Number of spaces per level of indentation."
        )->capture_default_str();

        workspaceConfig->timeoutMs = 100;
        server->add_option(
            "--timeout-ms", workspaceConfig->timeoutMs,
            "Number of milliseconds to await requests from server-to-client."
        )->capture_default_str();

        workspaceConfig->retry.maxAttempts = 5;
        server->add_option(
            "--max-retry-attempts", workspaceConfig->retry.maxAttempts,
            "Maximum number of times to attempt a request before giving up."
        )->capture_default_str();

        workspaceConfig->retry.minSleepTimeMs = 10;
        server->add_option(
            "--min-retry-sleep-time-ms", workspaceConfig->retry.minSleepTimeMs,
            "Minimum number of milliseconds to wait between request attempts."
        )->capture_default_str();

        workspaceConfig->retry.maxSleepTimeMs = 10;
        server->add_option(
            "--max-retry-sleep-time-ms", workspaceConfig->retry.maxSleepTimeMs,
            "Maximum number of milliseconds to wait between request attempts."
        )->capture_default_str();

        workspaceConfig->telemetry.enabled = false;
        server->add_option(
            "--telemetry-enabled", workspaceConfig->telemetry.enabled,
            "Whether to enable telemetry events (may require a restart)."
        )->capture_default_str();

        workspaceConfig->telemetry.frequencyMs = 1000;
        server->add_option(
            "--telemetry-frequency-ms", workspaceConfig->telemetry.frequencyMs,
            "Number of milliseconds to wait between telemetry events."
        )->capture_default_str();

        opts.extensionId = "lcompilers.lfortran-lsp";
        server->add_option(
            "--extension-id", opts.extensionId,
            "Identifies the language client extension that interacts with this server."
        )->capture_default_str();

        opts.parentProcessId = -1;
        server->add_option(
            "--parent-process-id", opts.parentProcessId,
            "Process ID (PID) of the process that has spawned this language server."
        )->capture_default_str();

        return server;
    }

    auto LanguageServerInterface::buildMessageStream(
        lsl::Logger &logger
    ) -> std::unique_ptr<ls::MessageStream> {
        switch (opts.serverProtocol) {
        case ServerProtocol::LSP: {
            return std::make_unique<lsp::LspMessageStream>(std::cin, logger);
        }
        default: {
            throw lc::LCompilersException(
                ("Unsupported server protocol: ServerProtocol::" +
                 ServerProtocolNames.at(opts.serverProtocol))
            );
        }
        }
    }

    auto LanguageServerInterface::buildLanguageServer(
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        lsl::Logger &logger,
        std::atomic_bool &start,
        std::condition_variable &startChanged,
        std::mutex &startMutex
    ) -> std::unique_ptr<ls::LanguageServer> {
        if (opts.language == Language::FORTRAN) {
            if (opts.dataFormat == DataFormat::JSON_RPC) {
                if (opts.serverProtocol == ServerProtocol::LSP) {
                    switch (opts.executionStrategy) {
                    case ExecutionStrategy::PARALLEL: {
                        std::random_device randomSeed;
                        return std::make_unique<lsp::ParallelLFortranLspLanguageServer>(
                            incomingMessages,
                            outgoingMessages,
                            opts.numRequestThreads,
                            opts.numWorkerThreads,
                            logger,
                            opts.configSection,
                            opts.extensionId,
                            LFORTRAN_VERSION,
                            opts.parentProcessId,
                            randomSeed(),
                            workspaceConfig,
                            start,
                            startChanged,
                            startMutex
                        );
                        break;
                    }
                    case ExecutionStrategy::CONCURRENT: {
                        return std::make_unique<lsp::ConcurrentLFortranLspLanguageServer>(
                            incomingMessages,
                            outgoingMessages,
                            logger,
                            opts.configSection,
                            opts.extensionId,
                            LFORTRAN_VERSION,
                            opts.parentProcessId,
                            workspaceConfig,
                            start,
                            startChanged,
                            startMutex
                        );
                        break;
                    }
                    default: {
                        throw lc::LCompilersException(
                            ("Unsupported execution strategy: " +
                             ExecutionStrategyValues.at(opts.executionStrategy))
                        );
                    }
                    }
                } else {
                    throw lc::LCompilersException(
                        ("Unsupported server protocol for fortran: " +
                         ServerProtocolValues.at(opts.serverProtocol))
                    );
                }
            } else {
                throw lc::LCompilersException(
                    ("Unsupported data format for fortran: " +
                     DataFormatValues.at(opts.dataFormat))
                );
            }
        } else {
            throw lc::LCompilersException(
                ("Unsupported language: " + LanguageValues.at(opts.language))
            );
        }
    }

    auto LanguageServerInterface::buildCommunicationProtocol(
        ls::LanguageServer &languageServer,
        ls::MessageStream &messageStream,
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        lsl::Logger &logger,
        std::atomic_bool &start,
        std::condition_variable &startChanged,
        std::mutex &startMutex
    ) -> std::unique_ptr<ls::CommunicationProtocol> {
        switch (opts.communicationProtocol) {
        case CommunicationProtocol::STDIO: {
            return std::make_unique<ls::CommunicationProtocol>(
                languageServer,
                messageStream,
                incomingMessages,
                outgoingMessages,
                logger,
                start,
                startChanged,
                startMutex
            );
        }
        default: {
            throw lc::LCompilersException(
                ("Unsupported communication protocol: " +
                 std::to_string(static_cast<int>(opts.communicationProtocol)))
            );
        }
        }
    }

    auto LanguageServerInterface::serve() -> void {
        try {
            lsl::Logger logger(opts.enableLogging ? workspaceConfig->log.path : "", "LanguageServerInterface");
            logger.setLevel(workspaceConfig->log.level);
            logger.threadName("main");

            std::atomic_bool start{false};
            std::condition_variable startChanged;
            std::mutex startMutex;

            std::unique_ptr<ls::MessageStream> messageStream =
                buildMessageStream(logger);
            ls::MessageQueue communicatorToServer(logger, "communicator-to-server");
            ls::MessageQueue serverToCommunicator(logger, "server-to-communicator");
            std::unique_ptr<ls::LanguageServer> languageServer =
                buildLanguageServer(
                    communicatorToServer,
                    serverToCommunicator,
                    logger,
                    start,
                    startChanged,
                    startMutex
                );
            std::unique_ptr<ls::CommunicationProtocol> communicationProtocol =
                buildCommunicationProtocol(
                    *languageServer,
                    *messageStream,
                    serverToCommunicator,
                    communicatorToServer,
                    logger,
                    start,
                    startChanged,
                    startMutex
                );
            {
                std::unique_lock<std::mutex> startLock(startMutex);
                start = true;
            }
            startChanged.notify_all();
            communicationProtocol->serve();
            logger.info()
                << "Language server terminated cleanly."
                << std::endl;
        } catch(const LCompilers::LCompilersException &e) {
            std::cerr << "Caught unhandled exception" << std::endl;
            std::vector<LCompilers::StacktraceItem> d = e.stacktrace_addresses();
            get_local_addresses(d);
            get_local_info(d);
            std::cerr << stacktrace2str(d, LCompilers::stacktrace_depth, false);
            std::cerr << e.name() + ": " << e.msg() << std::endl;
            throw e;
        } catch (const std::exception &e) {
            std::string buffer = "Caught unhandled exception: ";
            buffer.append(e.what());
            throw lc::LCompilersException(buffer);
        } catch (...) {
            std::cerr << "Unknown Exception" << std::endl;
        }
    }

} // LCompilers::LLanguageServer::Interface

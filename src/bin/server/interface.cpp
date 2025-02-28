#include "bin/CLI11.hpp"
#include "server/lsp_specification.h"
#include <filesystem>
#include <regex>
#include <stdexcept>
#include <thread>

#include <server/logger.h>
#include <server/lsp_message_stream.h>

#include <bin/server/interface.h>
#include <bin/server/lfortran_lsp_language_server.h>

namespace LCompilers::LLanguageServer::Interface {
    namespace fs = std::filesystem;

    namespace lsp = LCompilers::LanguageServerProtocol;

    ExitError::ExitError(ExitCode code, const std::string &message)
        : std::logic_error(message)
        , _code{code}
    {
        // empty
    }

    auto ExitError::code() const -> ExitCode {
        return _code;
    }

    auto validateConfigSection(const std::string &configSection) -> std::string {
        if (!std::regex_match(configSection, RE_CONFIG_SECTION)) {
            std::string buffer = "Configuration section is not a valid sequence of dot-separated, ECMAScript Ids: ";
            buffer.append(configSection);
            throw ExitError(ExitCode::INVALID_VALUE, buffer);
        }

        if (std::regex_match(configSection, RE_RESERVED_CONFIG_IDS)) {
            std::string buffer = "Configuration section contains a reserved Id for ECMAScript: ";
            buffer.append(configSection);
            throw ExitError(ExitCode::INVALID_VALUE, buffer);
        }

        return configSection;
    }

    auto existsAndIsWritable(const std::string &pathString) -> std::string {
        fs::path path = fs::absolute(pathString).lexically_normal();

        fs::path dir = path.parent_path();
        if (!fs::exists(dir) && !fs::create_directories(dir)) {
            throw ExitError(
                ExitCode::INVALID_VALUE,
                "Cannot create log directory: " + dir.string()
            );
        }

        std::ofstream ofs(path);
        if (!ofs.is_open()) {
            throw ExitError(
                ExitCode::INVALID_VALUE,
                "Path is not writable: " + path.string()
            );
        }
        ofs.close();

        return path.string();
    }

    auto existsAndIsExecutable(const std::string &pathString) -> std::string {
        fs::path path = fs::absolute(pathString).lexically_normal();

        if (!fs::exists(path)) {
            throw ExitError(
                ExitCode::INVALID_VALUE,
                "Path does not exist: " + path.string()
            );
        }

        std::error_code ec;
        fs::file_status status = std::filesystem::status(path, ec);

        if (ec) {
            throw ExitError(
                ExitCode::INVALID_VALUE,
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
            throw ExitError(
                ExitCode::INVALID_VALUE,
                "Path is not executable: " + path.string()
            );
        }

        return path.string();
    }

    const std::map<ExitCode, std::string> ExitCodeNames = {
        {ExitCode::SUCCESS, "SUCCESS"},
        {ExitCode::INVALID_VALUE, "INVALID_VALUE"},
        {ExitCode::BAD_ARG_COMBO, "BAD_ARG_COMBO"},
        {ExitCode::UNHANDLED_EXCEPTION, "UNHANDLED_EXCEPTION"},
    };

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

    CommandLineInterface::CommandLineInterface()
        : workspaceConfig(std::make_shared<lsc::LFortranLspConfig>())
    {
        // empty
    }

    auto CommandLineInterface::prepare(CLI::App &app) -> CLI::App & {
        CLI::App &server = *app.add_subcommand(
            "server",
            "LCompilers Language Server: Serves requests from language extensions in supported editors."
        );

        opts.language = Language::FORTRAN;
        server.add_option(
            "--language", opts.language,
            "Specifies the language to serve."
        )->capture_default_str()->transform(
            CLI::CheckedTransformer(
                transpose(LanguageValues),
                CLI::ignore_case
            )
        );

        opts.dataFormat = DataFormat::JSON_RPC;
        server.add_option(
            "--data-format", opts.dataFormat,
            "Specifies the data exchange format for requests."
        )->capture_default_str()->transform(
            CLI::CheckedTransformer(
                transpose(DataFormatValues),
                CLI::ignore_case
            )
        );

        opts.communicationProtocol = CommunicationProtocol::STDIO;
        server.add_option(
            "--communication-protocol", opts.communicationProtocol,
            "Specifies the communication protocol over which to serve requests."
        )->capture_default_str()->transform(
            CLI::CheckedTransformer(
                transpose(CommunicationProtocolValues),
                CLI::ignore_case
            )
        );

        opts.serverProtocol = ServerProtocol::LSP;
        server.add_option(
            "--server-protocol", opts.serverProtocol,
            "Specifies the language server protocol that defines how to serve requests."
        )->capture_default_str()->transform(
            CLI::CheckedTransformer(
                transpose(ServerProtocolValues),
                CLI::ignore_case
            )
        );

        opts.numRequestThreads = 5;
        server.add_option(
            "--num-request-threads", opts.numRequestThreads,
            "Number of threads that serve requests."
        )->capture_default_str();

        opts.numWorkerThreads = std::thread::hardware_concurrency();
        server.add_option(
            "--num-worker-threads", opts.numWorkerThreads,
            "Number of threads that will handle sub-tasks of requests."
        )->capture_default_str();

        opts.configSection = "LFortranLanguageServer";
        server.add_option(
            "--config-section", opts.configSection,
            ("Identifies the server's configuration section within the "
             "workspace configuration.")
        )->capture_default_str()->check(validateConfigSection);

        workspaceConfig->openIssueReporterOnError = true;
        server.add_option(
            "--open-issue-reporter-on-error",
            workspaceConfig->openIssueReporterOnError,
            "Open a bug report if an internal error occurs."
        )->capture_default_str();

        workspaceConfig->maxNumberOfProblems = 100;
        server.add_option(
            "--max-number-of-problems",
            workspaceConfig->maxNumberOfProblems,
            "Maximum number of errors and warnings to report."
        )->capture_default_str();

        workspaceConfig->trace.server = lsp::TraceValues::OFF;
        server.add_option(
            "--trace-server", workspaceConfig->trace.server,
            "Traces the communication between the language client and server."
        )->capture_default_str()->transform(
            CLI::CheckedTransformer(
                transpose(lsp::TraceValuesValues),
                CLI::ignore_case
            )
        );

        server.add_option(
            "--compiler-path", workspaceConfig->compiler.path,
            "Path to the LFortran compiler executable."
        )->transform(existsAndIsExecutable);

        server.add_option(
            "compiler_flags", workspaceConfig->compiler.flags,
            "Additional flags to pass to the LFortran compiler."
        );

        workspaceConfig->log.path = existsAndIsWritable("lfortran-language-server.log");
        server.add_option(
            "--log-path", workspaceConfig->log.path,
            "Path to where logs should be written."
        )->capture_default_str()->transform(existsAndIsWritable);

        workspaceConfig->log.level = lsl::Level::LOG_LEVEL_INFO;
        server.add_option(
            "--log-level", workspaceConfig->log.level,
            "Verbosity of log output"
        )->capture_default_str()->transform(
            CLI::CheckedTransformer(
                transpose(lsl::LevelValues),
                CLI::ignore_case
            )
        );

        workspaceConfig->log.prettyPrint = true;
        server.add_option(
            "--log-pretty-print", workspaceConfig->log.prettyPrint,
            "Whether to pretty-print JSON objects and arrays."
        )->capture_default_str();

        workspaceConfig->log.indentSize = 4;
        server.add_option(
            "--log-indent-size", workspaceConfig->log.indentSize,
            "Number of spaces to indent the pretty-printed JSON."
        )->capture_default_str();

        return server;
    }

    auto CommandLineInterface::buildMessageStream(
        lsl::Logger &logger
    ) -> std::unique_ptr<ls::MessageStream> {
        switch (opts.serverProtocol) {
        case ServerProtocol::LSP: {
            return std::make_unique<lsp::LspMessageStream>(std::cin, logger);
        }
        default: {
            std::string buffer;
            buffer.append("Unsupported server protocol: ServerProtocol::");
            buffer.append(ServerProtocolNames.at(opts.serverProtocol));
            throw ExitError(ExitCode::INVALID_VALUE, buffer);
        }
        }
    }

    auto CommandLineInterface::buildLanguageServer(
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        lsl::Logger &logger
    ) -> std::unique_ptr<ls::LanguageServer> {
        if (opts.language == Language::FORTRAN) {
            if (opts.dataFormat == DataFormat::JSON_RPC) {
                if (opts.serverProtocol == ServerProtocol::LSP) {
                    return std::make_unique<lsp::LFortranLspLanguageServer>(
                        incomingMessages,
                        outgoingMessages,
                        opts.numRequestThreads,
                        opts.numWorkerThreads,
                        logger,
                        opts.configSection,
                        workspaceConfig
                    );
                } else {
                    std::string buffer = "Unsupported server protocol for fortran: ";
                    buffer.append(std::to_string(static_cast<int>(opts.serverProtocol)));
                    throw ExitError(ExitCode::INVALID_VALUE, buffer);
                }
            } else {
                std::string buffer = "Unsupported data format for fortran: ";
                buffer.append(std::to_string(static_cast<int>(opts.dataFormat)));
                throw ExitError(ExitCode::INVALID_VALUE, buffer);
            }
        } else {
            std::string buffer = "Unsupported language: ";
            buffer.append(std::to_string(static_cast<int>(opts.language)));
            throw ExitError(ExitCode::INVALID_VALUE, buffer);
        }
    }

    auto CommandLineInterface::buildCommunicationProtocol(
        ls::LanguageServer &languageServer,
        ls::MessageStream &messageStream,
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        lsl::Logger &logger
    ) -> std::unique_ptr<ls::CommunicationProtocol> {
        switch (opts.communicationProtocol) {
        case CommunicationProtocol::STDIO: {
            return std::make_unique<ls::CommunicationProtocol>(
                languageServer,
                messageStream,
                incomingMessages,
                outgoingMessages,
                logger
            );
        }
        default: {
            std::string buffer = "Unsupported communication protocol: ";
            buffer.append(
                std::to_string(static_cast<int>(opts.communicationProtocol))
            );
            throw ExitError(ExitCode::INVALID_VALUE, buffer);
        }
        }
    }

    auto CommandLineInterface::buildMessageQueue(
        lsl::Logger &logger
    ) -> std::unique_ptr<ls::MessageQueue> {
        return std::make_unique<ls::MessageQueue>(logger);
    }

    auto CommandLineInterface::serve() -> void {
        try {
            lsl::Logger logger(workspaceConfig->log.path);
            try {
                std::unique_ptr<ls::MessageStream> messageStream =
                    buildMessageStream(logger);
                ls::MessageQueue communicatorToServer(logger);
                ls::MessageQueue serverToCommunicator(logger);
                std::unique_ptr<ls::LanguageServer> languageServer =
                    buildLanguageServer(
                        communicatorToServer,
                        serverToCommunicator,
                        logger);
                std::unique_ptr<ls::CommunicationProtocol> communicationProtocol =
                    buildCommunicationProtocol(
                        *languageServer,
                        *messageStream,
                        serverToCommunicator,
                        communicatorToServer,
                        logger
                    );
                communicationProtocol->serve();
                logger.debug()
                    << "[CommandLineInterface] Communication protocol terminated."
                    << std::endl;
                logger.info() << "Language server terminated cleanly.";
            } catch (std::exception &e) {
                std::string buffer = "Language server terminated erroneously: ";
                buffer.append(e.what());
                throw ExitError(ExitCode::UNHANDLED_EXCEPTION, buffer);
            }
        } catch (std::exception &e) {
            std::string buffer = "Caught unhandled exception: ";
            buffer.append(e.what());
            throw ExitError(ExitCode::UNHANDLED_EXCEPTION, buffer);
        }
    }

} // LCompilers::LLanguageServer::Interface

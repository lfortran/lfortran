#pragma once

#include <cstddef>
#include <map>
#include <memory>
#include <regex>
#include <stdexcept>
#include <string>

#ifndef CLI11_HAS_FILESYSTEM
#define CLI11_HAS_FILESYSTEM 0
#endif // CLI11_HAS_FILESYSTEM

#include <bin/CLI11.hpp>

#include <server/communication_protocol.h>
#include <server/language_server.h>
#include <server/logger.h>
#include <server/message_stream.h>
#include <server/thread_pool.h>

#include <bin/lfortran_lsp_config.h>

namespace LCompilers::LLanguageServer::Interface {
    namespace ls = LCompilers::LLanguageServer;
    namespace lsl = LCompilers::LLanguageServer::Logging;
    namespace lsc = LCompilers::LanguageServerProtocol::Config;

    const std::regex RE_CONFIG_SECTION(
        "^(?:[a-z_$][a-z_$0-9]*)(?:\\.(?:[a-z_$][a-z_$0-9]*))*$",
        std::regex_constants::ECMAScript | std::regex_constants::icase
    );

    const std::regex RE_RESERVED_CONFIG_IDS(
        "^(?:(?:[a-z_$][a-z_$0-9]*)\\.)*(?:break|case|catch|class|const|continue|debugger|default|delete|do|else|enum|export|extends|finally|for|function|if|implements|import|in|instanceof|interface|let|new|package|private|protected|public|return|static|super|switch|this|throw|try|typeof|var|void|while|with|yield)(?:\\.(?:[a-z_$][a-z_$0-9]*))*$",
        std::regex_constants::ECMAScript
    );

    auto validateConfigSection(const std::string &configSection) -> std::string;

    auto existsAndIsWritable(const std::string &pathString) -> std::string;

    auto existsAndIsExecutable(const std::string &pathString) -> std::string;

    enum class Language {
        FORTRAN,
    };

    extern const std::map<Language, std::string> LanguageNames;

    extern const std::map<Language, std::string> LanguageValues;

    auto languageByValue(const std::string &value) -> Language;

    enum class DataFormat {
        JSON_RPC,
    };

    extern const std::map<DataFormat, std::string> DataFormatNames;

    extern const std::map<DataFormat, std::string> DataFormatValues;

    auto dataFormatByValue(const std::string &value) -> DataFormat;

    enum class CommunicationProtocol {
        STDIO,
    };

    extern const std::map<CommunicationProtocol, std::string> CommunicationProtocolNames;

    extern const std::map<CommunicationProtocol, std::string> CommunicationProtocolValues;

    auto communicationProtocolByValue(const std::string &value) -> CommunicationProtocol;

    enum class ServerProtocol {
        LSP,
    };

    extern const std::map<ServerProtocol, std::string> ServerProtocolNames;

    extern const std::map<ServerProtocol, std::string> ServerProtocolValues;

    auto serverProtocolByValue(const std::string &value) -> ServerProtocol;

    template <typename T, typename U>
    auto transpose(const std::map<T, U> &map) -> std::map<U, T> {
        std::map<U, T> transposed;
        for (const auto &[key, value] : map) {
            transposed.emplace(value, key);
        }
        return transposed;
    }

    /**
     * Parsed argument values from the command line.
     */
    struct CommandLineOptions {
        Language language;
        DataFormat dataFormat;
        CommunicationProtocol communicationProtocol;
        ServerProtocol serverProtocol;
        std::size_t numRequestThreads;
        std::size_t numWorkerThreads;
        std::string configSection;
        std::string extensionId;
        int parentProcessId;
    };

    class LanguageServerInterface {
    public:
        LanguageServerInterface();
        auto prepare(CLI::App &app) -> CLI::App *;
        auto serve() -> void;
    private:
        CommandLineOptions opts;
        std::shared_ptr<lsc::LFortranLspConfig> workspaceConfig;

        auto buildMessageStream(
            lsl::Logger &logger
        ) -> std::unique_ptr<ls::MessageStream>;

        auto buildLanguageServer(
            ls::MessageQueue &incomingMessages,
            ls::MessageQueue &outgoingMessages,
            lsl::Logger &logger
        ) -> std::unique_ptr<ls::LanguageServer>;

        auto buildCommunicationProtocol(
            ls::LanguageServer &languageServer,
            ls::MessageStream &messageStream,
            ls::MessageQueue &incomingMessages,
            ls::MessageQueue &outgoingMessages,
            lsl::Logger &logger
        ) -> std::unique_ptr<ls::CommunicationProtocol>;

        auto buildMessageQueue(
            lsl::Logger &logger
        ) -> std::unique_ptr<ls::MessageQueue>;
    };

} // namespace LCompilers::LLanguageServer::Interface

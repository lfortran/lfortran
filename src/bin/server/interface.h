#pragma once

#include <cstddef>
#include <filesystem>
#include <map>
#include <memory>
#include <regex>
#include <string>

#define CLI11_HAS_FILESYSTEM 0
#include <bin/CLI11.hpp>

#include <server/communication_protocol.h>
#include <server/language_server.h>
#include <server/logger.h>
#include <server/message_queue.h>
#include <server/message_stream.h>
#include <server/thread_pool.h>

namespace LCompilers::LLanguageServer::Interface {
  namespace fs = std::filesystem;

  namespace ls = LCompilers::LLanguageServer;
  namespace lsl = LCompilers::LLanguageServer::Logging;

  const std::regex RE_CONFIG_SECTION(
    "^(?:[a-z_$][a-z_$0-9]*)(?:\\.(?:[a-z_$][a-z_$0-9]*))*$",
    std::regex_constants::ECMAScript | std::regex_constants::icase
  );

  const std::regex RE_RESERVED_CONFIG_IDS(
    "^(?:(?:[a-z_$][a-z_$0-9]*)\\.)*(?:break|case|catch|class|const|continue|debugger|default|delete|do|else|export|extends|finally|for|function|if|import|in|instanceof|new|return|super|switch|this|throw|try|typeof|var|void|while|with|yield|enum|implements|interface|let|package|private|protected|public|static)(?:\\.(?:[a-z_$][a-z_$0-9]*))*$",
    std::regex_constants::ECMAScript
  );

  auto isValidConfigSection(const std::string &configSection) -> bool;

  enum class ExitCode {
    SUCCESS = 0,
    INVALID_VALUE = 1,
    BAD_ARG_COMBO = 2,
    UNHANDLED_EXCEPTION = 3,
  };

  extern std::map<ExitCode, std::string> ExitCodeNames;

  enum class Language {
    FORTRAN,
  };

  extern std::map<Language, std::string> LanguageNames;

  extern std::map<Language, std::string> LanguageValues;

  auto languageByValue(const std::string &value) -> Language;

  enum class DataFormat {
    JSON_RPC,
  };

  extern std::map<DataFormat, std::string> DataFormatNames;

  extern std::map<DataFormat, std::string> DataFormatValues;

  auto dataFormatByValue(const std::string &value) -> DataFormat;

  enum class CommunicationProtocol {
    STDIO,
  };

  extern std::map<CommunicationProtocol, std::string> CommunicationProtocolNames;

  extern std::map<CommunicationProtocol, std::string> CommunicationProtocolValues;

  auto communicationProtocolByValue(const std::string &value) -> CommunicationProtocol;

  enum class ServerProtocol {
    LSP,
  };

  extern std::map<ServerProtocol, std::string> ServerProtocolNames;

  extern std::map<ServerProtocol, std::string> ServerProtocolValues;

  auto serverProtocolByValue(const std::string &value) -> ServerProtocol;

  /**
   * Literal argument values from the command line.
   */
  struct CommandLineArguments {
    std::string language = LanguageValues.at(Language::FORTRAN);
    std::string dataFormat = DataFormatValues.at(DataFormat::JSON_RPC);
    std::string communicationProtocol =
      CommunicationProtocolValues.at(CommunicationProtocol::STDIO);
    std::string serverProtocol = ServerProtocolValues.at(ServerProtocol::LSP);
    std::size_t numRequestThreads = 5;
    std::size_t numWorkerThreads;
    std::string logPath = "llanguage-server.log";
    std::string configSection = "LFortranLanguageServer";
  };

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
    fs::path logPath;
    std::string configSection;
  };

  class CommandLineInterface {
  public:
    auto prepare(CLI::App &app) -> CLI::App &;
    auto validate() -> ExitCode;
    auto serve() -> ExitCode;
  private:
    CommandLineArguments args;
    CommandLineOptions opts;

    auto validateAndSetLogPath() -> ExitCode;
    auto validateAndSetLanguage() -> ExitCode;
    auto validateAndSetDataFormat() -> ExitCode;
    auto validateAndSetCommunicationProtocol() -> ExitCode;
    auto validateAndSetServerProtocol() -> ExitCode;
    auto validateAndSetInteractive() -> ExitCode;
    auto validateAndSetNumRequestThreads() -> ExitCode;
    auto validateAndSetNumWorkerThreads() -> ExitCode;
    auto validateAndSetConfigSection() -> ExitCode;

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

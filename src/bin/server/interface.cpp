#include <regex>
#include <sstream>
#include <stdexcept>
#include <thread>

#include <server/lsp_message_stream.h>

#include "bin/server/interface.h"
#include "bin/server/lfortran_lsp_language_server.h"

namespace  LCompilers::LLanguageServer::Interface {
  namespace fs = std::filesystem;

  namespace ls = LCompilers::LLanguageServer;
  namespace lsp = LCompilers::LanguageServerProtocol;
  namespace lsl = LCompilers::LLanguageServer::Logging;

  auto isValidConfigSection(const std::string &configSection) -> bool {
    if (!std::regex_match(configSection, RE_CONFIG_SECTION)) {
      std::cerr
        << "Configuration section is not a valid sequence of dot-separated, ECMAScript Ids: "
        << configSection
        << std::endl;
      return false;
    }

    if (std::regex_match(configSection, RE_RESERVED_CONFIG_IDS)) {
      std::cerr
        << "Configuration section contains a reserved Id for ECMAScript: "
        << configSection
        << std::endl;
      return false;
    }

    return true;
  }

  std::map<ExitCode, std::string> ExitCodeNames = {
    {ExitCode::SUCCESS, "SUCCESS"},
    {ExitCode::INVALID_VALUE, "INVALID_VALUE"},
    {ExitCode::BAD_ARG_COMBO, "BAD_ARG_COMBO"},
    {ExitCode::UNHANDLED_EXCEPTION, "UNHANDLED_EXCEPTION"},
  };

  std::map<Language, std::string> LanguageNames = {
    {Language::FORTRAN, "FORTRAN"},
  };

  std::map<Language, std::string> LanguageValues = {
    {Language::FORTRAN, "fortran"},
  };

  auto languageByValue(const std::string &value) -> Language {
    for (const auto &[lang_key, lang_value] : LanguageValues) {
      if (lang_value == value) {
        return lang_key;
      }
    }
    std::stringstream ss;
    ss << "Invalid Language value: \"" << value << "\"";
    throw std::invalid_argument(ss.str());
  }

  std::map<DataFormat, std::string> DataFormatNames = {
    {DataFormat::JSON_RPC, "JSON_RPC"},
  };

  std::map<DataFormat, std::string> DataFormatValues = {
    {DataFormat::JSON_RPC, "json-rpc"},
  };

  auto dataFormatByValue(const std::string &value) -> DataFormat {
    for (const auto &[fmt_key, fmt_value] : DataFormatValues) {
      if (fmt_value == value) {
        return fmt_key;
      }
    }
    std::stringstream ss;
    ss << "Invalid DataFormat value: \"" << value << "\"";
    throw std::invalid_argument(ss.str());
  }

  std::map<CommunicationProtocol, std::string> CommunicationProtocolNames = {
    {CommunicationProtocol::STDIO, "STDIO"},
  };

  std::map<CommunicationProtocol, std::string> CommunicationProtocolValues = {
    {CommunicationProtocol::STDIO, "stdio"},
  };

  auto communicationProtocolByValue(const std::string &value) -> CommunicationProtocol {
    for (const auto &[protocol_key, protocol_value] : CommunicationProtocolValues) {
      if (protocol_value == value) {
        return protocol_key;
      }
    }
    std::stringstream ss;
    ss << "Invalid CommunicationProtocol value: \"" << value << "\"";
    throw std::invalid_argument(ss.str());
  }

  std::map<ServerProtocol, std::string> ServerProtocolNames = {
    {ServerProtocol::LSP, "LSP"},
  };

  std::map<ServerProtocol, std::string> ServerProtocolValues = {
    {ServerProtocol::LSP, "lsp"},
  };

  auto serverProtocolByValue(const std::string &value) -> ServerProtocol {
    for (const auto &[protocol_key, protocol_value] : ServerProtocolValues) {
      if (protocol_value == value) {
        return protocol_key;
      }
    }
    std::stringstream ss;
    ss << "Invalid ServerProtocol value: \"" << value << "\"";
    throw std::invalid_argument(ss.str());
  }

  auto CommandLineInterface::validateAndSetLogPath() -> ExitCode {
    fs::path logPath = fs::absolute(args.logPath).lexically_normal();
    fs::path logDir = logPath.parent_path();
    if (!fs::exists(logDir) && !fs::create_directories(logDir)) {
      std::cerr
        << "Cannot create log directory: " << logDir
        << std::endl;
      return ExitCode::INVALID_VALUE;
    }

    std::ofstream ofs(logPath);
    if (ofs.is_open()) {
      ofs.close();
      opts.logPath = logPath;
      std::cerr
        << "Logging to: " << logPath
        << std::endl;
      return ExitCode::SUCCESS;
    }

    std::cerr
      << "Log path is not writable: " << logPath
      << std::endl;
    return ExitCode::INVALID_VALUE;
  }

  auto CommandLineInterface::validateAndSetLanguage() -> ExitCode {
    try {
      opts.language = languageByValue(args.language);
      return ExitCode::SUCCESS;
    } catch (std::invalid_argument &e) {
      std::cerr
        << "Unsupported value for --language (\""
        << args.language
        << "\"), it must be one of: ";
      auto iter = LanguageValues.begin();
      for (int index = 0; iter != LanguageValues.end(); ++index, ++iter) {
        if (index > 0) {
          std::cerr << "; ";
        }
        std::cerr << iter->second;
      }
      std::cerr << std::endl;
      return ExitCode::INVALID_VALUE;
    }
  }

  auto CommandLineInterface::validateAndSetDataFormat() -> ExitCode {
    try {
      opts.dataFormat = dataFormatByValue(args.dataFormat);
      return ExitCode::SUCCESS;
    } catch (std::invalid_argument &e) {
      std::cerr
        << "Unsupported value for --data-format: (\""
        << args.dataFormat
        << "\"), it must be one of: ";
      auto iter = DataFormatValues.begin();
      for (int index = 0; iter != DataFormatValues.end(); ++index, ++iter) {
        if (index > 0) {
          std::cerr << "; ";
        }
        std::cerr << iter->second;
      }
      std::cerr << std::endl;
      return ExitCode::INVALID_VALUE;
    }
  }

  auto CommandLineInterface::validateAndSetCommunicationProtocol() -> ExitCode {
    try {
      opts.communicationProtocol =
        communicationProtocolByValue(args.communicationProtocol);
      return ExitCode::SUCCESS;
    } catch (std::invalid_argument &e) {
      std::cerr
        << "Unsupported value for --communication-protocol: (\""
        << args.communicationProtocol
        << "\"), it must be one of: ";
      auto iter = CommunicationProtocolValues.begin();
      for (int index = 0; iter != CommunicationProtocolValues.end(); ++index, ++iter) {
        if (index > 0) {
          std::cerr << "; ";
        }
        std::cerr << iter->second;
      }
      std::cerr << std::endl;
      return ExitCode::INVALID_VALUE;
    }
  }

  auto CommandLineInterface::validateAndSetServerProtocol() -> ExitCode {
    try {
      opts.serverProtocol = serverProtocolByValue(args.serverProtocol);
    } catch (std::invalid_argument &e) {
      std::cerr
        << "Unsupported value for --server-protocol: (\""
        << args.serverProtocol
        << "\"), it must be one of: "
        << std::endl;
      auto iter = ServerProtocolValues.begin();
      for (int index = 0; iter != ServerProtocolValues.end(); ++index, ++iter) {
        if (index > 0) {
          std::cerr << "; ";
        }
        std::cerr << iter->second;
      }
      std::cerr << std::endl;
      return ExitCode::INVALID_VALUE;
    }
    if (opts.serverProtocol == ServerProtocol::LSP) {
      if (opts.dataFormat != DataFormat::JSON_RPC) {
        std::cerr
          << "Only JSON-RPC is supported with LSP; must set --data-format=json-rpc."
          << std::endl;
        return ExitCode::BAD_ARG_COMBO;
      }
    }
    return ExitCode::SUCCESS;
  }

  auto CommandLineInterface::validateAndSetNumRequestThreads() -> ExitCode {
    opts.numRequestThreads = args.numRequestThreads;
    return ExitCode::SUCCESS;
  }

  auto CommandLineInterface::validateAndSetNumWorkerThreads() -> ExitCode {
    opts.numWorkerThreads = args.numWorkerThreads;
    return ExitCode::SUCCESS;
  }

  auto CommandLineInterface::validateAndSetConfigSection() -> ExitCode {
    if (isValidConfigSection(args.configSection)) {
      opts.configSection = args.configSection;
      return ExitCode::SUCCESS;
    }
    return ExitCode::INVALID_VALUE;
  }

  auto CommandLineInterface::prepare(CLI::App &app) -> CLI::App & {
    CLI::App &server = *app.add_subcommand(
      "server",
      "LCompilers Language Server: Serves requests from language extensions in supported editors."
    );

    std::stringstream ss;

    {
      ss.str("");
      ss << "Specifies the language to serve (";
      auto iter = LanguageValues.begin();
      for (int i = 0; iter != LanguageValues.end(); ++iter, ++i) {
        if (i > 0) {
          ss << "; ";
        }
        ss << iter->second;
      }
      ss << ").";
      server.add_option(
        "--language", args.language,
        ss.str()
      )->capture_default_str();
    }

    {
      ss.str("");
      ss << "Specifies the data exchange format for requests (";
      auto iter = DataFormatValues.begin();
      for (int i = 0; iter != DataFormatValues.end(); ++iter, ++i) {
        if (i > 0) {
          ss << "; ";
        }
        ss << iter->second;
      }
      ss << ").";
      server.add_option(
        "--data-format", args.dataFormat,
        ss.str()
      )->capture_default_str();
    }

    {
      ss.str("");
      ss << "Specifies the communication protocol over which to serve requests (";
      auto iter = CommunicationProtocolValues.begin();
      for (int i = 0; iter != CommunicationProtocolValues.end(); ++iter, ++i) {
        if (i > 0) {
          ss << "; ";
        }
        ss << iter->second;
      }
      ss << ").";
      server.add_option(
        "--communication-protocol", args.communicationProtocol,
        ss.str()
      )->capture_default_str();
    }

    {
      ss.str("");
      ss << "Specifies the language server protocol that defines how to serve requests (";
      auto iter = ServerProtocolValues.begin();
      for (int i = 0; iter != ServerProtocolValues.end(); ++iter, ++i) {
        if (i > 0) {
          ss << "; ";
        }
        ss << iter->second;
      }
      ss << ").";
      server.add_option(
        "--server-protocol", args.serverProtocol,
        ss.str()
      )->capture_default_str();
    }

    {
      ss.str("");
      ss << "Number of threads that serve requests (default: "
         << args.numRequestThreads
         << ").";
      server.add_option(
        "--num-request-threads", args.numRequestThreads,
        ss.str()
      )->capture_default_str();
    }

    {
      args.numWorkerThreads = std::thread::hardware_concurrency();
      ss.str("");
      ss << "Number of threads that will handle sub-tasks of requests (default: "
         << args.numWorkerThreads
         << ").";
      server.add_option(
        "--num-worker-threads", args.numWorkerThreads,
        ss.str()
      )->capture_default_str();
    }

    {
      ss.str("");
      ss << "Path to where logs should be written (default: \""
         << args.logPath
         << "\").";
      server.add_option(
        "--log-path", args.logPath,
        ss.str()
      )->capture_default_str();
    }

    {
      ss.str("");
      ss << "Identifies the server's configuration section within the workspace configuration (default: \""
         << args.configSection
         << "\").";
      server.add_option(
        "--config-section", args.configSection,
        ss.str()
      )->capture_default_str();
    }

    return server;
  }

  auto CommandLineInterface::validate() -> ExitCode {
    ExitCode exitCode = ExitCode::SUCCESS;
    if (exitCode == ExitCode::SUCCESS) {
      exitCode = validateAndSetLanguage();
    }
    if (exitCode == ExitCode::SUCCESS) {
      exitCode = validateAndSetDataFormat();
    }
    if (exitCode == ExitCode::SUCCESS) {
      exitCode = validateAndSetCommunicationProtocol();
    }
    if (exitCode == ExitCode::SUCCESS) {
      exitCode = validateAndSetServerProtocol();
    }
    if (exitCode == ExitCode::SUCCESS) {
      exitCode = validateAndSetNumRequestThreads();
    }
    if (exitCode == ExitCode::SUCCESS) {
      exitCode = validateAndSetNumWorkerThreads();
    }
    if (exitCode == ExitCode::SUCCESS) {
      exitCode = validateAndSetLogPath();
    }
    if (exitCode == ExitCode::SUCCESS) {
      exitCode = validateAndSetConfigSection();
    }
    return exitCode;
  }

  auto CommandLineInterface::buildMessageStream(
    lsl::Logger &logger
  ) -> std::unique_ptr<ls::MessageStream> {
    switch (opts.serverProtocol) {
    case ServerProtocol::LSP: {
      return std::make_unique<lsp::LspMessageStream>(std::cin, logger);
    }
    default: {
      std::stringstream ss;
      ss << "Unsupported server protocol: "
         << static_cast<int>(opts.serverProtocol);
      throw std::runtime_error(ss.str());
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
            opts.configSection
          );
        } else {
          std::stringstream ss;
          ss << "Unsupported server protocol for fortran: "
             << static_cast<int>(opts.serverProtocol);
          throw std::runtime_error(ss.str());
        }
      } else {
        std::stringstream ss;
        ss << "Unsupported data format for fortran: "
           << static_cast<int>(opts.dataFormat);
        throw std::runtime_error(ss.str());
      }
    } else {
      std::stringstream ss;
      ss << "Unsupported language: " << static_cast<int>(opts.language);
      throw std::runtime_error(ss.str());
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
      std::stringstream ss;
      ss << "Unsupported communication protocol: "
         << static_cast<int>(opts.communicationProtocol);
      throw std::runtime_error(ss.str());
    }
    }
  }

  auto CommandLineInterface::buildMessageQueue(
    lsl::Logger &logger
  ) -> std::unique_ptr<ls::MessageQueue> {
    return std::make_unique<ls::MessageQueue>(logger);
  }

  auto CommandLineInterface::serve() -> ExitCode {
    try {
      lsl::Logger logger(opts.logPath);
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
      return ExitCode::SUCCESS;
    } catch (std::exception &e) {
      std::cerr
        << "Caught unhandled exception: "
        << e.what()
        << std::endl;
      return ExitCode::UNHANDLED_EXCEPTION;
    }
  }

} // LCompilers::LLanguageServer::Interface

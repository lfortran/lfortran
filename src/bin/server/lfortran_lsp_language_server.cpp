#include <cstring>

#include <server/lsp_language_server.h>
#include <server/specification.h>

#include "bin/utils.h"
#include "bin/server/utils.h"
#include "bin/server/lfortran_lsp_language_server.h"

namespace LCompilers::LanguageServerProtocol {
  namespace lcli = LCompilers::CommandLineInterface;

  LFortranLspLanguageServer::LFortranLspLanguageServer(
    ls::MessageQueue &incomingMessages,
    ls::MessageQueue &outgoingMessages,
    std::size_t numRequestThreads,
    std::size_t numWorkerThreads,
    lsl::Logger &logger,
    const std::string &configSection
  ) : BaseLspLanguageServer(
      incomingMessages,
      outgoingMessages,
      numRequestThreads,
      numWorkerThreads,
      logger,
      configSection
    )
  {
    // empty
  }

  auto LFortranLspLanguageServer::invalidateConfigCache() -> void {
    BaseLspLanguageServer::invalidateConfigCache();
    {
      std::unique_lock<std::shared_mutex> writeLock(optionMutex);
      optionsByUri.clear();
    }
    logger.debug() << "Invalidated compiler options cache." << std::endl;
  }

  auto LFortranLspLanguageServer::getCompilerOptions(
    const DocumentUri &uri
  ) -> const CompilerOptions & {
    std::shared_lock<std::shared_mutex> readLock(optionMutex);
    auto optionIter = optionsByUri.find(uri);
    if (optionIter != optionsByUri.end()) {
      return optionIter->second;
    }

    readLock.unlock();

    CompilerOptions compiler_options;
    compiler_options.continue_compilation = true;

    const LSPAny &config = getConfig(uri);
    LSPAnyType configType = static_cast<LSPAnyType>(config.index());
    switch (configType) {
    case LSPAnyType::OBJECT_TYPE: {
      const LSPObject &object = std::get<LSPObject>(config);
      auto iter = object.find("compiler");
      if (iter != object.end()) {
        const LSPAny &compilerAny = *iter->second;
        LSPAnyType compilerType = static_cast<LSPAnyType>(compilerAny.index());
        switch (compilerType) {
        case LSPAnyType::OBJECT_TYPE: {
          const LSPObject &compiler = std::get<LSPObject>(compilerAny);
          if ((iter = compiler.find("flags")) != compiler.end()) {
            const LSPAny &flagsAny = *iter->second;
            LSPAnyType flagsType = static_cast<LSPAnyType>(flagsAny.index());
            switch (flagsType) {
            case LSPAnyType::ARRAY_TYPE: {
              const LSPArray &flags = std::get<LSPArray>(flagsAny);
              int argc = 1 + flags.size();
              char **argv = new char*[argc];
              argv[0] = new char[source.length() + 1];
              std::strcpy(argv[0], source.c_str());
              for (std::size_t i = 0; i < flags.size(); ++i) {
                const std::unique_ptr<LSPAny> &flag = flags[i];
                LSPAnyType flagType = static_cast<LSPAnyType>(flag->index());
                switch (flagType) {
                case LSPAnyType::OBJECT_TYPE: // fallthrough
                case LSPAnyType::ARRAY_TYPE: {
                  string_t stringValue = serializer.serialize(*flag);
                  argv[i + 1] = new char[stringValue.size() + 1];
                  std::strcpy(argv[i + 1], stringValue.c_str());
                  break;
                }
                case LSPAnyType::STRING_TYPE: {
                  string_t stringValue = transformer.anyToString(*flag);
                  argv[i + 1] = new char[stringValue.size() + 1];
                  std::strcpy(argv[i + 1], stringValue.c_str());
                  break;
                }
                case LSPAnyType::INTEGER_TYPE: {
                  integer_t integerValue = transformer.anyToInteger(*flag);
                  std::string stringValue = std::to_string(integerValue);
                  argv[i + 1] = new char[stringValue.size() + 1];
                  std::strcpy(argv[i + 1], stringValue.c_str());
                  break;
                }
                case LSPAnyType::UINTEGER_TYPE: {
                  uinteger_t uintegerValue = transformer.anyToUInteger(*flag);
                  std::string stringValue = std::to_string(uintegerValue);
                  argv[i + 1] = new char[stringValue.size() + 1];
                  std::strcpy(argv[i + 1], stringValue.c_str());
                  break;
                }
                case LSPAnyType::DECIMAL_TYPE: {
                  decimal_t decimalValue = transformer.anyToDecimal(*flag);
                  std::string stringValue = std::to_string(decimalValue);
                  argv[i + 1] = new char[stringValue.size() + 1];
                  std::strcpy(argv[i + 1], stringValue.c_str());
                  break;
                }
                case LSPAnyType::BOOLEAN_TYPE: {
                  boolean_t booleanValue = transformer.anyToBoolean(*flag);
                  std::string stringValue = std::to_string(booleanValue);
                  argv[i + 1] = new char[stringValue.size() + 1];
                  std::strcpy(argv[i + 1], stringValue.c_str());
                  break;
                }
                case LSPAnyType::NULL_TYPE: {
                  std::string stringValue = "";
                  argv[i + 1] = new char[stringValue.size() + 1];
                  std::strcpy(argv[i + 1], stringValue.c_str());
                  break;
                }
                }
              }
              int exitCode = lcli::init_compiler_options(compiler_options, argc, argv);
              if (exitCode != 0) {
                std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());
                logger
                  << "Failed to initialize compiler options for document with uri: " << uri
                  << std::endl;
                logger
                  << "init_compiler_options(...) returned with status: " << exitCode
                  << std::endl;
              }
              for (int i = 0; i < argc; ++i) {
                delete[] argv[i];
              }
              delete[] argv;
              break;
            }
            default: {
              std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());
              logger
                << "Unsupported type of compiler flags: LSPAnyType::"
                << LSPAnyTypeNames.at(flagsType)
                << std::endl;
            }
            }
          }
          break;
        }
        default: {
          std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());
          logger
            << "Unsupported type of compiler options: LSPAnyType::"
            << LSPAnyTypeNames.at(compilerType)
            << std::endl;
        }
        }
      }
      break;
    }
    default: {
      std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());
      logger
        << "Unsupported type of config: LSPAnyType::"
        << LSPAnyTypeNames.at(configType)
        << std::endl;
    }
    }

    std::unique_lock<std::shared_mutex> writeLock(configMutex);
    optionIter = optionsByUri.find(uri);
    if (optionIter != optionsByUri.end()) {
      return optionIter->second;
    }

    auto record = optionsByUri.emplace(uri, std::move(compiler_options));
    return record.first->second;
  }

  auto LFortranLspLanguageServer::validate(
    TextDocument &document
  ) -> void {
    workerPool.execute([this, &document](
      const std::string &threadName,
      const std::size_t threadId
    ) {
      try {
        // NOTE: These value may have been updated since the validation was
        // requested, but that's okay because we want to validate the latest
        // version anyway:
        std::unique_lock<std::shared_mutex> readLock(document.mutex());
        const std::string &uri = document.uri();
        const std::string &path = document.path().string();
        const std::string &text = document.text();
        int version = document.version();
        try {
          CompilerOptions compiler_options = getCompilerOptions(uri);
          std::vector<LCompilers::error_highlight> highlights =
            lfortran.showErrors(path, text, compiler_options);

          std::vector<std::unique_ptr<Diagnostic>> diagnostics;
          for (const LCompilers::error_highlight &highlight : highlights) {
            std::unique_ptr<Position> start = std::make_unique<Position>();
            start->line = highlight.first_line - 1;
            start->character = highlight.first_column - 1;

            std::unique_ptr<Position> end = std::make_unique<Position>();
            end->line = highlight.last_line - 1;
            end->character = highlight.last_column;

            std::unique_ptr<Range> range = std::make_unique<Range>();
            range->start = std::move(start);
            range->end = std::move(end);

            std::unique_ptr<Diagnostic> diagnostic = std::make_unique<Diagnostic>();
            diagnostic->range = std::move(range);
            diagnostic->severity =
              diagnostic_level_to_lsp_severity(highlight.severity);
            diagnostic->message = highlight.message;
            diagnostic->source = source;

            diagnostics.push_back(std::move(diagnostic));
          }

          PublishDiagnosticsParams params;
          params.uri = uri;
          params.version = version;
          params.diagnostics = std::move(diagnostics);
          sendTextDocument_publishDiagnostics(params);
        } catch (std::exception &e) {
          logger.error()
            << "[" << threadName << "_" << threadId << "] "
            << "Failed to validate document (uri=\""
            << uri << "\"): " << e.what()
            << std::endl;
        }
      } catch (std::exception &e) {
        logger.error()
          << "[" << threadName << "_" << threadId << "] "
          << "Failed to read document attributes: " << e.what()
          << std::endl;
      }
    });
  }

  // request: "initialize"
  auto LFortranLspLanguageServer::receiveInitialize(
    InitializeParams &params
  ) -> InitializeResult {
    InitializeResult result = BaseLspLanguageServer::receiveInitialize(params);
    // add additional function for lfortran
    return result;
  }

  // notification: "workspace/didDeleteFiles"
  auto LFortranLspLanguageServer::receiveWorkspace_didDeleteFiles(
    DeleteFilesParams &/*params*/
  ) -> void {
    std::shared_lock<std::shared_mutex> readLock(documentMutex);
    for (auto &[uri, document] : documentsByUri) {
      validate(document);
    }
  }

  // notification: "workspace/didChangeConfiguration"
  auto LFortranLspLanguageServer::receiveWorkspace_didChangeConfiguration(
    DidChangeConfigurationParams &params
  ) -> void {
    BaseLspLanguageServer::receiveWorkspace_didChangeConfiguration(params);
    std::shared_lock<std::shared_mutex> readLock(documentMutex);
    for (auto &[uri, document] : documentsByUri) {
      validate(document);
    }
  }

  // notification: "textDocument/didOpen"
  auto LFortranLspLanguageServer::receiveTextDocument_didOpen(
    DidOpenTextDocumentParams &params
  ) -> void {
    BaseLspLanguageServer::receiveTextDocument_didOpen(params);
    {
      std::shared_lock<std::shared_mutex> readLock(documentMutex);
      const DocumentUri &uri = params.textDocument->uri;
      TextDocument &document = documentsByUri.at(uri);
      validate(document);
    }
  }

  // notification: "textDocument/didChange"
  auto LFortranLspLanguageServer::receiveTextDocument_didChange(
    DidChangeTextDocumentParams &params
  ) -> void {
    BaseLspLanguageServer::receiveTextDocument_didChange(params);
    {
      std::shared_lock<std::shared_mutex> readLock(documentMutex);
      const DocumentUri &uri = params.textDocument->uri;
      TextDocument &document = documentsByUri.at(uri);
      validate(document);
    }
  }

  // notification: "workspace/didChangeWatchedFiles"
  auto LFortranLspLanguageServer::receiveWorkspace_didChangeWatchedFiles(
    DidChangeWatchedFilesParams &/*params*/
  ) -> void {
    std::shared_lock<std::shared_mutex> readLock(documentMutex);
    for (auto &[uri, document] : documentsByUri) {
      validate(document);
    }
  }

} // namespace LCompilers::LanguageServerProtocol

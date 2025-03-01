#pragma once

#include <cstddef>
#include <filesystem>
#include <shared_mutex>
#include <regex>
#include <string>
#include <vector>

#include <server/logger.h>
#include <server/lsp_specification.h>

namespace LCompilers::LanguageServerProtocol {
    namespace fs = std::filesystem;

    namespace lsl = LCompilers::LLanguageServer::Logging;

    const std::regex RE_FILE_URI(
        "^file:(?://)?",
        std::regex_constants::ECMAScript | std::regex_constants::icase
    );

    class LspTextDocument {
    public:
        LspTextDocument(
            const std::string &uri,
            const std::string &languageId,
            int version,
            const std::string &text,
            lsl::Logger &logger
        );
        LspTextDocument(LspTextDocument &&other) noexcept;    // move constructor

        inline auto uri() const -> const DocumentUri & {
            return _uri;
        }

        inline auto path() const -> const fs::path & {
            return _path;
        }

        inline auto languageId() const -> const std::string & {
            return _languageId;
        }

        inline auto version() const -> int {
            return _version;
        }

        inline auto text() const -> const std::string & {
            return _text;
        }

        inline auto mutex() -> std::shared_mutex & {
            return _mutex;
        }

        auto apply(
            std::vector<TextDocumentContentChangeEvent> &changes,
            int version
        ) -> void;
    private:
        DocumentUri _uri;
        std::string _languageId;
        int _version;
        std::string _text;
        lsl::Logger &logger;
        fs::path _path;
        std::string buffer;
        std::vector<std::size_t> lineIndices;
        std::shared_mutex _mutex;

        auto validateUriAndSetPath() -> void;
        auto indexLines() -> void;

        auto from(
            const TextDocumentContentChangeEvent &event
        ) const -> std::size_t;
        auto from(
            const TextDocumentContentChangeEvent_0 &event
        ) const -> std::size_t;
        auto from(
            const TextDocumentContentChangeEvent_1 &event
        ) const -> std::size_t;

        auto decompose(
            const TextDocumentContentChangeEvent &event,
            std::size_t &j,
            std::size_t &k,
            std::string &patch
        ) -> void;
        auto decompose(
            const TextDocumentContentChangeEvent_0 &event,
            std::size_t &j,
            std::size_t &k,
            std::string &patch
        ) -> void;
        auto decompose(
            const TextDocumentContentChangeEvent_1 &event,
            std::size_t &j,
            std::size_t &k,
            std::string &patch
        ) -> void;
    };

} // namespace LCompilers::LanguageServerProtocol

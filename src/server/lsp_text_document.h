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

    // NOTE: File URIs follow one of the following schemes:
    // 1. `file:/path` (no hostname)
    // 2. `file:///path` (empty hostname)
    // 3. `file://hostname/path`
    // NOTE: All we are interested in is the `/path` portion
    const std::regex RE_FILE_URI(
        "^file:(?://[^/]*)?",
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
        LspTextDocument(
            const std::string &uri,
            lsl::Logger &logger
        );
        LspTextDocument(LspTextDocument &&other) noexcept;    // move constructor

        auto documentId() const -> std::size_t;
        auto uri() const -> const DocumentUri &;
        auto setUri(const DocumentUri &uri) -> void;
        auto path() const -> const fs::path &;
        auto languageId() const -> const std::string &;
        auto version() const -> int;
        auto text() const -> const std::string &;
        auto mutex() -> std::shared_mutex &;

        auto update(
            const std::string &languageId,
            int version,
            const std::string &text
        ) -> void;

        auto apply(
            std::vector<TextDocumentContentChangeEvent> &changes,
            int version
        ) -> void;

        auto toPosition(
            std::size_t line,
            std::size_t column
        ) const -> std::size_t;

        auto fromPosition(
            std::size_t &line,
            std::size_t &column,
            std::size_t position
        ) const -> void;
    private:
        // NOTE: The document's URI might change but its documentId will remain
        // the same.
        const std::size_t _documentId;
        DocumentUri _uri;
        std::string _languageId;
        int _version;
        std::string _text;
        lsl::Logger logger;
        fs::path _path;
        std::string buffer;
        std::vector<std::size_t> posByLine;
        std::vector<std::size_t> lenByLine;
        std::shared_mutex _mutex;

        auto indexLines() -> void;
        auto loadText() -> void;

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

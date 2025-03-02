#include <algorithm>
#include <stdexcept>

#include <server/lsp_exception.h>
#include <server/lsp_text_document.h>

namespace LCompilers::LanguageServerProtocol {

    LspTextDocument::LspTextDocument(
        const DocumentUri &uri,
        const std::string &languageId,
        int version,
        const std::string &text,
        lsl::Logger &logger
    ) : _uri(uri)
      , _languageId(languageId)
      , _version(version)
      , _text(text)
      , logger(logger)
    {
        buffer.reserve(8196);
        validateUriAndSetPath();
        indexLines();
    }

    LspTextDocument::LspTextDocument(LspTextDocument &&other) noexcept
        : _uri(std::move(other._uri))
        , _languageId(std::move(other._languageId))
        , _version(other._version)
        , _text(std::move(other._text))
        , logger(other.logger)
        , _path(std::move(other._path))
        , buffer(std::move(other.buffer))
        , lineIndices(std::move(other.lineIndices))
    {
        // empty
    }

    auto LspTextDocument::validateUriAndSetPath() -> void {
        std::string path = std::regex_replace(_uri, RE_FILE_URI, "");
        _path = fs::canonical(path);
    }

    auto LspTextDocument::apply(
        std::vector<TextDocumentContentChangeEvent> &changes,
        int version
    ) -> void {
        std::sort(
            changes.begin(),
            changes.end(),
            [this](auto &a, auto &b) {
                return from(a) < from(b);
            }
        );

        std::unique_lock<std::shared_mutex> writeLock(_mutex);

        buffer.clear();
        std::size_t i = 0;
        for (const auto &change : changes) {
            std::size_t j;
            std::size_t k;
            std::string patch;
            decompose(change, j, k, patch);
            if (i < _text.length()) {
                buffer.append(_text.substr(i, (j - i)));
            }
            buffer.append(patch);
            i = k;
        }
        if (i < _text.length()) {
            buffer.append(_text.substr(i, (_text.length() - i)));
        }
        _text = buffer;
        indexLines();
        _version = version;
    }

    auto LspTextDocument::indexLines() -> void {
        lineIndices.clear();
        lineIndices.push_back(0);
        for (std::size_t index = 0; index < _text.length(); ++index) {
            unsigned char c = _text[index];
            switch (c) {
            case '\r': {
                if (((index + 1) < _text.length()) && (_text[index + 1] == '\n')) {
                    ++index;
                }
            } // fallthrough
            case '\n': {
                lineIndices.push_back(index + 1);
            }
            }
        }
    }

    auto LspTextDocument::from(
        const TextDocumentContentChangeEvent &event
    ) const -> std::size_t {
        switch (event.type()) {
        case TextDocumentContentChangeEventType::TextDocumentContentChangeEvent_0: {
            return from(event.textDocumentContentChangeEvent_0());
        }
        case TextDocumentContentChangeEventType::TextDocumentContentChangeEvent_1: {
            return from(event.textDocumentContentChangeEvent_1());
        }
        case TextDocumentContentChangeEventType::Uninitialized: {
            throw std::invalid_argument("event has not been initialized!");
        }
        }
        throw std::runtime_error("This should be unreachable.");
    }

    auto LspTextDocument::from(
        const TextDocumentContentChangeEvent_0 &event
    ) const -> std::size_t {
        const Range &range = event.range;
        const Position &start = range.start;
        std::size_t index = lineIndices[start.line] + start.character;
        return index;
    }

    auto LspTextDocument::from(
        const TextDocumentContentChangeEvent_1 &/*event*/
    ) const -> std::size_t {
        return 0;
    }

    auto LspTextDocument::decompose(
        const TextDocumentContentChangeEvent &event,
        std::size_t &j,
        std::size_t &k,
        std::string &patch
    ) -> void {
        switch (event.type()) {
        case TextDocumentContentChangeEventType::TextDocumentContentChangeEvent_0: {
            const TextDocumentContentChangeEvent_0 &partial = event.textDocumentContentChangeEvent_0();
            decompose(partial, j, k, patch);
            break;
        }
        case TextDocumentContentChangeEventType::TextDocumentContentChangeEvent_1: {
            const TextDocumentContentChangeEvent_1 &whole = event.textDocumentContentChangeEvent_1();
            decompose(whole, j, k, patch);
            break;
        }
        case TextDocumentContentChangeEventType::Uninitialized: {
            throw std::invalid_argument("event has not been initialized!");
        }
        }
    }

    auto LspTextDocument::decompose(
        const TextDocumentContentChangeEvent_0 &event,
        std::size_t &j,
        std::size_t &k,
        std::string &patch
    ) -> void {
        const Range &range = event.range;
        const Position &start = range.start;
        const Position &end = range.end;

        if (start.line > end.line) {
            buffer.clear();
            buffer.append("start.line must be <= end.line, but ");
            buffer.append(std::to_string(start.line));
            buffer.append(" > ");
            buffer.append(std::to_string(end.line));
            throw LSP_EXCEPTION(ErrorCodes::InvalidParams, buffer);
        }

        if ((start.line == end.line) && (start.character > end.character)) {
            buffer.clear();
            buffer.append("start.character must be <= end.character when colinear, but ");
            buffer.append(std::to_string(start.character));
            buffer.append(" > ");
            buffer.append(std::to_string(end.character));
            throw LSP_EXCEPTION(ErrorCodes::InvalidParams, buffer);
        }

        if (start.line < lineIndices.size()) {
            j = lineIndices[start.line] + start.character;
        } else if (start.line == lineIndices[lineIndices.size() - 1] + 1) {
            j = _text.length();
        } else {
            buffer.clear();
            buffer.append("start.line must be <= ");
            buffer.append(std::to_string(lineIndices[lineIndices.size() - 1] + 1));
            buffer.append(" but was: ");
            buffer.append(std::to_string(start.line));
            throw LSP_EXCEPTION(ErrorCodes::InvalidParams, buffer);
        }

        if (end.line < lineIndices.size()) {
            k = lineIndices[end.line] + end.character;
        } else {
            k = j + event.text.length();
        }

        patch = event.text;
    }

    auto LspTextDocument::decompose(
        const TextDocumentContentChangeEvent_1 &event,
        std::size_t &j,
        std::size_t &k,
        std::string &patch
    ) -> void {
        j = 0;
        k = _text.length();
        patch = event.text;
    }

} // namespace LCompilers::LanguageServerProtocol

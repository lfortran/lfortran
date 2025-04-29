#include <algorithm>
#include <atomic>
#include <cctype>
#include <filesystem>
#include <fstream>
#include <stdexcept>
#include <string>
#include <sstream>

#include <server/lsp_exception.h>
#include <server/lsp_text_document.h>

namespace LCompilers::LanguageServerProtocol {

    auto nextId() -> std::size_t {
        static std::atomic_size_t serialId{0};
        return serialId++;
    }

    LspTextDocument::LspTextDocument(
        const DocumentUri &uri,
        const std::string &languageId,
        int version,
        const std::string &text,
        lsl::Logger &logger
    ) : _id(nextId())
      , _languageId(languageId)
      , _version(version)
      , _text(text)
      , logger(logger.having("LspTextDocument"))
    {
        buffer.reserve(8196);
        setUri(uri);
        indexLines();
    }

    LspTextDocument::LspTextDocument(
        const std::string &uri,
        lsl::Logger &logger
    ) : _id(nextId())
      , _languageId("")
      , _version(-1)
      , _text("")
      , logger(logger.having("LspTextDocument"))
    {
        buffer.reserve(8196);
        setUri(uri);
        loadText();
        indexLines();
    }

    LspTextDocument::LspTextDocument(LspTextDocument &&other) noexcept
        : _id(nextId())
        , _uri(std::move(other._uri))
        , _languageId(std::move(other._languageId))
        , _version(other._version)
        , _text(std::move(other._text))
        , logger(std::move(other.logger))
        , _path(std::move(other._path))
        , buffer(std::move(other.buffer))
        , posByLine(std::move(other.posByLine))
        , lenByLine(std::move(other.lenByLine))
    {
        // empty
    }

    auto LspTextDocument::loadText() -> void {
        std::ifstream fs(_path);
        if (fs.is_open()) {
            std::stringstream ss;
            ss << fs.rdbuf();
            _text = ss.str();
        }
    }

    auto LspTextDocument::id() const -> std::size_t {
        return _id;
    }

    auto LspTextDocument::uri() const -> const DocumentUri & {
        return _uri;
    }

    auto LspTextDocument::setUri(const DocumentUri &uri) -> void {
        _uri = uri;
        std::string path = std::regex_replace(uri, RE_FILE_URI, "");
        _path = fs::absolute(path).lexically_normal();
    }

    auto LspTextDocument::path() const -> const fs::path & {
        return _path;
    }

    auto LspTextDocument::languageId() const -> const std::string & {
        return _languageId;
    }

    auto LspTextDocument::version() const -> int {
        return _version;
    }

    auto LspTextDocument::text() const -> const std::string & {
        return _text;
    }

    auto LspTextDocument::mutex() -> std::shared_mutex & {
        return _mutex;
    }

    auto LspTextDocument::numLines() const -> std::size_t {
        return lenByLine.size();
    }

    auto LspTextDocument::lastLine() const -> std::size_t {
        return numLines() - 1;
    }

    inline auto isIndent(unsigned char c) -> bool {
        return (c == ' ') || (c == '\t');
    }

    // might include mixed tabs and spaces ...
    auto LspTextDocument::leadingIndentation(std::size_t line) -> std::string_view {
        std::size_t start = toPosition(line, 0);
        std::size_t stop = start;
        while ((stop < _text.length()) && isIndent(_text[stop])) {
            ++stop;
        }
        std::size_t length = stop - start;
        return std::string_view(_text.data() + start, length);
    }

    auto LspTextDocument::slice(
        std::size_t startLine,
        std::size_t startColumn,
        std::size_t endLine,
        std::size_t endColumn
    ) const -> std::string {
        std::size_t start = toPosition(startLine, startColumn);
        std::size_t stop = toPosition(endLine, endColumn);
        std::size_t length = stop - start;
        return _text.substr(start, length);
    }

    auto LspTextDocument::numColumns(std::size_t line) const -> std::size_t {
        if (line < numLines()) {
            return lenByLine.at(line);
        }
        throw std::invalid_argument(
            ("line=" + std::to_string(line) +
             " is out-of-bounds for document with " +
             std::to_string(numLines()) + " lines.")
        );
    }

    auto LspTextDocument::lastColumn(std::size_t line) const -> std::size_t {
        return numColumns(line) - 1;
    }

    auto LspTextDocument::update(
        const std::string &languageId,
        int version,
        const std::string &text
    ) -> void {
        std::unique_lock<std::shared_mutex> writeLock(_mutex);
        _languageId = languageId;
        _version = version;
        _text = text;
        indexLines();
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
        posByLine.clear();
        lenByLine.clear();
        posByLine.push_back(0);
        std::size_t column = 0;
        for (std::size_t index = 0; index < _text.length(); ++index) {
            unsigned char c = _text[index];
            switch (c) {
            case '\r': {
                if (((index + 1) < _text.length()) && (_text[index + 1] == '\n')) {
                    ++index;
                    ++column;
                }
            } // fallthrough
            case '\n': {
                posByLine.push_back(index + 1);
                lenByLine.push_back(column + 1);
                column = 0;
                break;
            }
            default: {
                ++column;
            }
            }
        }
        lenByLine.push_back(column + 1);
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
        std::size_t index = posByLine[start.line] + start.character;
        return index;
    }

    auto LspTextDocument::toPosition(
        std::size_t line,
        std::size_t column
    ) const -> std::size_t {
        if (line >= posByLine.size()) {
            throw std::invalid_argument(
                ("line=" + std::to_string(line) +
                 " is greater than the number of lines: " +
                 std::to_string(posByLine.size()))
            );
        }
        if (column >= lenByLine[line]) {
            throw std::invalid_argument(
                ("column=" + std::to_string(column) +
                 " is greater than the number of columns on line=" +
                 std::to_string(line) + ": " + std::to_string(lenByLine[line]))
            );
        }
        std::size_t position = posByLine[line] + column;
        return position;
    }

    auto LspTextDocument::fromPosition(
        std::size_t &line,
        std::size_t &column,
        std::size_t position
    ) const -> void {
        if (position >= _text.length()) {
            throw std::invalid_argument(
                ("position=" + std::to_string(position) +
                " is out-of-bounds for text of length=" +
                std::to_string(_text.length()))
            );
        }
        std::size_t lower = 0;
        std::size_t upper = posByLine.size() - 1;
        line = (lower + upper + 1) >> 1;
        long long scol = position - posByLine[line];
        while ((scol < 0)
               || (static_cast<std::size_t>(scol) >= lenByLine[line])) {
            if (scol < 0) {
                upper = line - 1;
            } else {
                lower = line;
            }
            line = (lower + upper + 1) >> 1;
            scol = position - posByLine[line];
        }
        column = static_cast<std::size_t>(scol);
    }

    inline bool isIdentifier(unsigned char c) {
        return std::isalnum(c) || (c == '_');
    }

    auto LspTextDocument::symbolAt(
        std::size_t line,
        std::size_t column
    ) const -> std::string_view {
        std::size_t lower = toPosition(line, column);
        std::size_t upper = lower;
        while ((lower > 0) && isIdentifier(_text[lower - 1])) {
            --lower;
        }
        while ((upper < _text.length()) && isIdentifier(upper + 1)) {
            ++upper;
        }
        std::size_t length = upper - lower;
        return std::string_view(_text.data() + lower, length);
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

        if (start.line < posByLine.size()) {
            j = posByLine[start.line] + start.character;
        } else if (start.line == posByLine[posByLine.size() - 1] + 1) {
            j = _text.length();
        } else {
            buffer.clear();
            buffer.append("start.line must be <= ");
            buffer.append(std::to_string(posByLine[posByLine.size() - 1] + 1));
            buffer.append(" but was: ");
            buffer.append(std::to_string(start.line));
            throw LSP_EXCEPTION(ErrorCodes::InvalidParams, buffer);
        }

        if (end.line < posByLine.size()) {
            k = posByLine[end.line] + end.character;
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

#include <cctype>

#include <server/lsp_exception.h>
#include <server/lsp_json_parser.h>
#include <server/lsp_specification.h>

namespace LCompilers::LanguageServerProtocol {

    LspJsonParser::LspJsonParser(const std::string &message)
        : message(message)
    {
        buffer.reserve(16384);
    }

    auto LspJsonParser::parse() -> std::unique_ptr<LSPAny> {
        dropWhitespace();
        if (hasNext()) {
            switch (nextChar()) {
            case '{': {
                std::unique_ptr<LSPAny> object = std::make_unique<LSPAny>();
                (*object) = parseObject();
                dropWhitespace();
                if (hasNext()) {
                    throw LSP_EXCEPTION(
                        ErrorCodes::ParseError,
                        "Not a valid JSON object (trailing characters): '" + message + "'"
                    );
                }
                return object;
            }
            case '[': {
                std::unique_ptr<LSPAny> array = std::make_unique<LSPAny>();
                (*array) = parseArray();
                dropWhitespace();
                if (hasNext()) {
                    throw LSP_EXCEPTION(
                        ErrorCodes::ParseError,
                        "Not a valid JSON array (trailing characters): '" + message + "'"
                    );
                }
                return array;
            }
            default: {
                throw LSP_EXCEPTION(
                    ErrorCodes::ParseError,
                    "Not a valid JSON message: '" + message + "'"
                );
            }
            }
        }
        throw LSP_EXCEPTION(
            ErrorCodes::ParseError,
            "Cannot parse empty message."
        );
    }

    auto LspJsonParser::dropWhitespace() -> void {
        do {
            switch (peekChar()) {
            case ' ': // fallthrough
            case '\t': // fallthrough
            case '\r': // fallthrough
            case '\n': {
                advance();
                break;
            }
            default: {
                return;
            }
            }
        } while (hasNext());
    }

    auto LspJsonParser::parseObject() -> LSPObject {
        LSPObject object;
        bool hasAttribute = false;
        dropWhitespace();
        while (hasNext()) {
            unsigned char c = nextChar();
            switch (c) {
            case '"': {
                const string_t key = parseString();
                dropWhitespace();
                if (hasNext()) {
                    unsigned char c = nextChar();
                    if (c == ':') {
                        dropWhitespace();
                        std::unique_ptr<LSPAny> value = parseValue();
                        object.emplace(key, std::move(value));
                        hasAttribute = true;
                    } else {
                        buffer.clear();
                        buffer.append("Expected ':' to follow object attribute name, not: '");
                        escapeAndBuffer(c);
                        buffer.push_back('\'');
                        throw LSP_EXCEPTION(ErrorCodes::ParseError, buffer);
                    }
                } else {
                    throw LSP_EXCEPTION(
                        ErrorCodes::ParseError,
                        "Reached end-of-message while parsing object attribute."
                    );
                }
                break;
            }
            case ',': {
                if (!hasAttribute) {
                    buffer.clear();
                    buffer.append("Found out-of-sequence separator (',') at position ");
                    buffer.append(std::to_string(index));
                    buffer.append(" while parsing object: ");
                    for (std::size_t i = 0; i <= index; ++i) {
                        escapeAndBuffer(message[i]);
                    }
                    throw LSP_EXCEPTION(ErrorCodes::ParseError, buffer);
                }
                hasAttribute = false;
                dropWhitespace();
                break;
            }
            case '}': {
                return object;
            }
            default: {
                buffer.clear();
                buffer.append("Found invalid character while parsing object: '");
                escapeAndBuffer(c);
                buffer.push_back('\'');
                throw LSP_EXCEPTION(ErrorCodes::ParseError, buffer);
            }
            }
        }
        throw LSP_EXCEPTION(
            ErrorCodes::ParseError,
            "Reached end-of-message while parsing object."
        );
    }

    auto LspJsonParser::parseArray() -> LSPArray {
        LSPArray array;
        bool hasValue = false;
        dropWhitespace();
        while (hasNext()) {
            unsigned char c = peekChar();
            switch (c) {
            case ',': {
                if (!hasValue) {
                    buffer.clear();
                    buffer.append("Found out-of-sequence separator (',') at position ");
                    buffer.append(std::to_string(index));
                    buffer.append(" while parsing array: ");
                    for (std::size_t i = 0; i <= index; ++i) {
                        escapeAndBuffer(message[i]);
                    }
                    throw LSP_EXCEPTION(ErrorCodes::ParseError, buffer);
                }
                advance();
                hasValue = false;
                break;
            }
            case ']': {
                advance();
                return array;
            }
            default: {
                std::unique_ptr<LSPAny> value = parseValue();
                array.push_back(std::move(value));
                hasValue = true;
            }
            }
        }
        throw LSP_EXCEPTION(
            ErrorCodes::ParseError,
            "Reached end-of-message while parsing array."
        );
    }

    auto LspJsonParser::parseValue() -> std::unique_ptr<LSPAny> {
        dropWhitespace();
        if (hasNext()) {
            std::unique_ptr<LSPAny> value;
            switch (peekChar()) {
            case '"': {
                advance();
                value = std::make_unique<LSPAny>();
                (*value) = parseString();
                break;
            }
            case '-': // passthrough
            case '0': // passthrough
            case '1': // passthrough
            case '2': // passthrough
            case '3': // passthrough
            case '4': // passthrough
            case '5': // passthrough
            case '6': // passthrough
            case '7': // passthrough
            case '8': // passthrough
            case '9': {
                value = parseNumber();
                break;
            }
            case '{': {
                advance();
                value = std::make_unique<LSPAny>();
                (*value) = parseObject();
                break;
            }
            case '[': {
                advance();
                value = std::make_unique<LSPAny>();
                (*value) = parseArray();
                break;
            }
            case 't': {
                advance();
                value = parseTrue();
                break;
            }
            case 'f': {
                advance();
                value = parseFalse();
                break;
            }
            case 'n': {
                advance();
                value = parseNull();
                break;
            }
            }
            dropWhitespace();
            return value;
        }
        throw LSP_EXCEPTION(
            ErrorCodes::ParseError,
            "Reached end-of-message while parsing value."
        );
    }

    auto LspJsonParser::parseString() -> string_t {
        buffer.clear();
        bool escaped = false;
        while (hasNext()) {
            unsigned char c = nextChar();
            switch (c) {
            case '\\': {
                if (escaped) {
                    buffer.push_back(c);
                }
                escaped = !escaped;
                break;
            }
            case 'n': {
                if (!escaped) {
                    buffer.push_back(c);
                } else {
                    buffer.push_back('\n');
                    escaped = false;
                }
                break;
            }
            case '\n': {
                buffer.push_back(c);
                escaped = false;
                break;
            }
            case 't': {
                if (!escaped) {
                    buffer.push_back(c);
                } else {
                    buffer.push_back('\t');
                    escaped = false;
                }
                break;
            }
            case '\t': {
                buffer.push_back(c);
                escaped = false;
                break;
            }
            case 'b': {
                if (!escaped) {
                    buffer.push_back(c);
                } else {
                    buffer.push_back('\b');
                    escaped = false;
                }
                break;
            }
            case '\b': {
                buffer.push_back('\b');
                escaped = false;
                break;
            }
            case 'r': {
                if (!escaped) {
                    buffer.push_back(c);
                } else {
                    buffer.push_back('\r');
                    escaped = false;
                }
                break;
            }
            case '\r': {
                buffer.push_back(c);
                escaped = false;
                break;
            }
            case 'f': {
                if (!escaped) {
                    buffer.push_back(c);
                } else {
                    buffer.push_back('\f');
                    escaped = false;
                }
                break;
            }
            case '\f': {
                buffer.push_back(c);
                escaped = false;
                break;
            }
            case '\'': {
                buffer.push_back(c);
                escaped = false;
                break;
            }
            case 'u': {
                if (!escaped) {
                    buffer.push_back(c);
                } else {
                    buffer.append("\\u");
                    std::size_t i;
                    for (i = 0; (i < 4) && hasNext(); ++i) {
                        unsigned char c = nextChar();
                        if (std::isxdigit(static_cast<int>(c))) {
                            buffer.push_back(c);
                        } else {
                            buffer.clear();
                            buffer.append("Found non-hex digit while parsing unicode character: '");
                            escapeAndBuffer(c);
                            buffer.push_back('\'');
                            throw LSP_EXCEPTION(ErrorCodes::ParseError, buffer);
                        }
                    }
                    if (i < 4) {
                        throw LSP_EXCEPTION(
                            ErrorCodes::ParseError,
                            "Reached end-of-message while parsing unicode literal."
                        );
                    }
                    escaped = false;
                }
                break;
            }
            case '"': {
                if (escaped) {
                    buffer.push_back(c);
                    escaped = false;
                } else {
                    return buffer;
                }
                break;
            }
            default: {
                buffer.push_back(c);
                escaped = false;
            }
            }
        }
        throw LSP_EXCEPTION(
            ErrorCodes::ParseError,
            "Cannot parse string without closing parenthesis."
        );
    }

    auto LspJsonParser::escapeAndBuffer(unsigned char c) -> void {
        switch (c) {
        case '\n': {
            buffer.append("\\n");
            break;
        }
        case '\t': {
            buffer.append("\\t");
            break;
        }
        case '\b': {
            buffer.append("\\b");
            break;
        }
        case '\r': {
            buffer.append("\\r");
            break;
        }
        case '\f': {
            buffer.append("\\f");
            break;
        }
        default: {
            buffer.push_back(c);
        }
        }
    }

    auto LspJsonParser::parseNumber() -> std::unique_ptr<LSPAny> {
        bool isNegated = false;
        bool hasDigit = false;
        buffer.clear();
        while (hasNext()) {
            unsigned char c = peekChar();
            switch (c) {
            case '-': {
                if (isNegated) {
                    throw LSP_EXCEPTION(
                        ErrorCodes::ParseError,
                        "Found double-negative while parsing number"
                    );
                }
                isNegated = true;
                buffer.push_back(c);
                advance();
                break;
            }
            case '0': {
                hasDigit = true;
                buffer.push_back(c);
                advance();
                switch (peekChar()) {
                case '.': {
                    buffer.push_back(c);
                    advance();
                    return parseFraction();
                }
                case 'e': // fallthrough
                case 'E': {
                    buffer.push_back(c);
                    advance();
                    return parseExponent();
                }
                }
                break;
            }
            case '1': // fallthrough
            case '2': // fallthrough
            case '3': // fallthrough
            case '4': // fallthrough
            case '5': // fallthrough
            case '6': // fallthrough
            case '7': // fallthrough
            case '8': // fallthrough
            case '9': {
                hasDigit = true;
                buffer.push_back(c);
                advance();
                break;
            }
            default: {
                if (hasDigit) {
                    std::unique_ptr<LSPAny> number = std::make_unique<LSPAny>();
                    (*number) = std::stoi(buffer);
                    return number;
                }
                throw LSP_EXCEPTION(
                    ErrorCodes::ParseError,
                    "Failed to parse number: no digits."
                );
            }
            }
        }
        throw LSP_EXCEPTION(
            ErrorCodes::ParseError,
            "Reached end-of-message while parsing number."
        );
    }

    auto LspJsonParser::parseFraction() -> std::unique_ptr<LSPAny> {
        bool hasDigit = false;
        while (hasNext()) {
            unsigned char c = peekChar();
            switch (c) {
            case '0': // fallthrough
            case '1': // fallthrough
            case '2': // fallthrough
            case '3': // fallthrough
            case '4': // fallthrough
            case '5': // fallthrough
            case '6': // fallthrough
            case '7': // fallthrough
            case '8': // fallthrough
            case '9': {
                hasDigit = true;
                buffer.push_back(c);
                advance();
                break;
            }
            case 'e': // fallthrough
            case 'E': {
                buffer.push_back(c);
                advance();
                return parseExponent();
            }
            default: {
                if (hasDigit) {
                    std::unique_ptr<LSPAny> number = std::make_unique<LSPAny>();
                    (*number) = std::stod(buffer);
                    return number;
                }
                throw LSP_EXCEPTION(
                    ErrorCodes::ParseError,
                    "Failed to parse fraction: no digits."
                );
            }
            }
        }
        throw LSP_EXCEPTION(
            ErrorCodes::ParseError,
            "Reached end-of-message while parsing decimal."
        );
    }

    auto LspJsonParser::parseExponent() -> std::unique_ptr<LSPAny> {
        bool hasDigit = false;
        bool hasSign = false;
        while (hasNext()) {
            unsigned char c = peekChar();
            switch (c) {
            case '-': // fallthrough
            case '+': {
                if (hasSign) {
                    throw LSP_EXCEPTION(
                        ErrorCodes::ParseError,
                        "Found multiple signs for exponent."
                    );
                }
                hasSign = true;
                buffer.push_back(c);
                advance();
                break;
            }
            case '0': // fallthrough
            case '1': // fallthrough
            case '2': // fallthrough
            case '3': // fallthrough
            case '4': // fallthrough
            case '5': // fallthrough
            case '6': // fallthrough
            case '7': // fallthrough
            case '8': // fallthrough
            case '9': {
                hasDigit = true;
                buffer.push_back(c);
                advance();
                break;
            }
            default: {
                if (hasDigit) {
                    std::unique_ptr<LSPAny> number = std::make_unique<LSPAny>();
                    (*number) = std::stod(buffer);
                    return number;
                }
                throw LSP_EXCEPTION(
                    ErrorCodes::ParseError,
                    "Failed to parse exponent: no digits."
                );
            }
            }
        }
        throw LSP_EXCEPTION(
            ErrorCodes::ParseError,
            "Reached end-of-message while parsing exponent."
        );
    }

    auto LspJsonParser::parseTrue() -> std::unique_ptr<LSPAny> {
        // if (hasNext() && ('t' == nextChar())) {
            if (hasNext() && ('r' == nextChar())) {
                if (hasNext() && ('u' == nextChar())) {
                    if (hasNext() && ('e' == nextChar())) {
                        std::unique_ptr<LSPAny> literal = std::make_unique<LSPAny>();
                        (*literal) = true;
                        return literal;
                    }
                }
            }
        // }
        throw LSP_EXCEPTION(
            ErrorCodes::ParseError,
            "Failed to parse literal: true"
        );
    }

    auto LspJsonParser::parseFalse() -> std::unique_ptr<LSPAny> {
        // if (hasNext() && ('f' == nextChar())) {
            if (hasNext() && ('a' == nextChar())) {
                if (hasNext() && ('l' == nextChar())) {
                    if (hasNext() && ('s' == nextChar())) {
                        if (hasNext() && ('e' == nextChar())) {
                            std::unique_ptr<LSPAny> literal = std::make_unique<LSPAny>();
                            (*literal) = false;
                            return literal;
                        }
                    }
                }
            }
        // }
        throw LSP_EXCEPTION(
            ErrorCodes::ParseError,
            "Failed to parse literal: false"
        );
    }

    auto LspJsonParser::parseNull() -> std::unique_ptr<LSPAny> {
        // if (hasNext() && ('n' == nextChar())) {
            if (hasNext() && ('u' == nextChar())) {
                if (hasNext() && ('l' == nextChar())) {
                    if (hasNext() && ('l' == nextChar())) {
                        std::unique_ptr<LSPAny> literal = std::make_unique<LSPAny>();
                        (*literal) = nullptr;
                        return literal;
                    }
                }
            }
        // }
        throw LSP_EXCEPTION(
            ErrorCodes::ParseError,
            "Failed to parse literal: true"
        );
    }

} // namespace LCompilers::LanguageServerProtocol

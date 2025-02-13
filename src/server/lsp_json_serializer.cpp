#include <utility>

#include <server/specification.h>
#include <server/lsp_exception.h>
#include <server/lsp_json_serializer.h>

namespace LCompilers::LanguageServerProtocol {

    auto LspJsonSerializer::serialize(const LSPAny &any) const -> std::string {
        std::string buffer;
        buffer.reserve(1024);
        serializeValue(buffer, any);
        return buffer;
    }

    auto LspJsonSerializer::pprint(
        const LSPAny &any,
        const std::string &indent
    ) const -> std::string {
        std::string buffer;
        buffer.reserve(2048);
        pprintValue(buffer, any, indent, 0);
        return buffer;
    }

    auto LspJsonSerializer::pprint(
        const LSPArray &array,
        const std::string &indent
    ) const -> std::string {
        std::string buffer;
        buffer.reserve(2048);
        pprintArray(buffer, array, indent, 0);
        return buffer;
    }

    auto LspJsonSerializer::pprint(
        const LSPObject &object,
        const std::string &indent
    ) const -> std::string {
        std::string buffer;
        buffer.reserve(2048);
        pprintObject(buffer, object, indent, 0);
        return buffer;
    }

    void LspJsonSerializer::newlineIndent(
        std::string &buffer,
        const std::string &indent,
        std::size_t level
    ) const {
        buffer.push_back('\n');
        for (std::size_t i = 0; i < level; ++i) {
            buffer.append(indent);
        }
    }

    void LspJsonSerializer::serializeArray(
        std::string &buffer,
        const LSPArray &array
    ) const {
        buffer.push_back('[');
        LSPArray::const_iterator iter = array.begin();
        if (iter != array.end()) {
            serializeValue(buffer, **iter++);
            while (iter != array.end()) {
                buffer.push_back(',');
                serializeValue(buffer, **iter++);
            }
        }
        buffer.push_back(']');
    }

    void LspJsonSerializer::pprintArray(
        std::string &buffer,
        const LSPArray &array,
        const std::string &indent,
        std::size_t level
    ) const {
        buffer.push_back('[');
        LSPArray::const_iterator iter = array.begin();
        if (iter != array.end()) {
            std::size_t nextLevel = level + 1;
            newlineIndent(buffer, indent, nextLevel);
            pprintValue(buffer, **iter++, indent, nextLevel);
            while (iter != array.end()) {
                buffer.push_back(',');
                newlineIndent(buffer, indent, nextLevel);
                pprintValue(buffer, **iter++, indent, nextLevel);
            }
            newlineIndent(buffer, indent, level);
        }
        buffer.push_back(']');
    }

    void LspJsonSerializer::serializeObject(
        std::string &buffer,
        const LSPObject &object
    ) const {
        buffer.push_back('{');
        LSPObject::const_iterator iter = object.begin();
        if (iter != object.end()) {
            serializeString(buffer, iter->first);
            buffer.push_back(':');
            serializeValue(buffer, *iter->second);
            while ((++iter) != object.end()) {
                buffer.push_back(',');
                serializeString(buffer, iter->first);
                buffer.push_back(':');
                serializeValue(buffer, *iter->second);
            }
        }
        buffer.push_back('}');
    }

    void LspJsonSerializer::pprintObject(
        std::string &buffer,
        const LSPObject &object,
        const std::string &indent,
        std::size_t level
    ) const {
        buffer.push_back('{');
        LSPObject::const_iterator iter = object.begin();
        if (iter != object.end()) {
            std::size_t nextLevel = level + 1;
            newlineIndent(buffer, indent, nextLevel);
            serializeString(buffer, iter->first);
            buffer.append(": ");
            pprintValue(buffer, *iter->second, indent, nextLevel);
            while ((++iter) != object.end()) {
                buffer.push_back(',');
                newlineIndent(buffer, indent, nextLevel);
                serializeString(buffer, iter->first);
                buffer.push_back(':');
                pprintValue(buffer, *iter->second, indent, nextLevel);
            }
            newlineIndent(buffer, indent, level);
        }
        buffer.push_back('}');
    }

    void LspJsonSerializer::serializeValue(
        std::string &buffer,
        const LSPAny &value
    ) const {
        switch (static_cast<LSPAnyType>(value.index())) {
        case LSPAnyType::OBJECT_TYPE: {
            serializeObject(buffer, std::get<LSPObject>(value));
            break;
        }
        case LSPAnyType::ARRAY_TYPE: {
            serializeArray(buffer, std::get<LSPArray>(value));
            break;
        }
        case LSPAnyType::STRING_TYPE: {
            serializeString(buffer, value);
            break;
        }
        case LSPAnyType::INTEGER_TYPE: // fallthrough
        case LSPAnyType::UINTEGER_TYPE: // fallthrough
        case LSPAnyType::DECIMAL_TYPE: {
            serializeNumber(buffer, value);
            break;
        }
        case LSPAnyType::BOOLEAN_TYPE: {
            serializeBoolean(buffer, value);
            break;
        }
        case LSPAnyType::NULL_TYPE: {
            serializeNull(buffer, value);
            break;
        }
        }
    }

    void LspJsonSerializer::pprintValue(
        std::string &buffer,
        const LSPAny &value,
        const std::string &indent,
        std::size_t level
    ) const {
        switch (static_cast<LSPAnyType>(value.index())) {
        case LSPAnyType::OBJECT_TYPE: {
            pprintObject(buffer, std::get<LSPObject>(value), indent, level);
            break;
        }
        case LSPAnyType::ARRAY_TYPE: {
            pprintArray(buffer, std::get<LSPArray>(value), indent, level);
            break;
        }
        case LSPAnyType::STRING_TYPE: {
            serializeString(buffer, value);
            break;
        }
        case LSPAnyType::INTEGER_TYPE: // fallthrough
        case LSPAnyType::UINTEGER_TYPE: // fallthrough
        case LSPAnyType::DECIMAL_TYPE: {
            serializeNumber(buffer, value);
            break;
        }
        case LSPAnyType::BOOLEAN_TYPE: {
            serializeBoolean(buffer, value);
            break;
        }
        case LSPAnyType::NULL_TYPE: {
            serializeNull(buffer, value);
            break;
        }
        }
    }

    void LspJsonSerializer::serializeString(
        std::string &buffer,
        const LSPAny &value
    ) const {
        LSPAnyType valueType = static_cast<LSPAnyType>(value.index());
        switch (valueType) {
        case LSPAnyType::STRING_TYPE: {
            const string_t &string = std::get<string_t>(value);
            buffer.push_back('"');
            for (std::size_t i = 0, k = string.length(); i < k; ++i) {
                unsigned char c = string[i];
                switch (c) {
                case '"': {
                    buffer.append("\\\"");
                    break;
                }
                case '\\': {
                    buffer.append("\\\\");
                    break;
                }
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
            buffer.push_back('"');
            break;
        }
        default: {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                ("Cannot serialize JSON string of type " +
                 LSPAnyTypeNames.at(valueType))
            );
        }
        }
    }

    void LspJsonSerializer::serializeNumber(
        std::string &buffer,
        const LSPAny &value
    ) const {
        LSPAnyType valueType = static_cast<LSPAnyType>(value.index());
        switch (valueType) {
        case LSPAnyType::INTEGER_TYPE: {
            buffer.append(std::to_string(std::get<integer_t>(value)));
            break;
        }
        case LSPAnyType::UINTEGER_TYPE: {
            buffer.append(std::to_string(std::get<uinteger_t>(value)));
            break;
        }
        case LSPAnyType::DECIMAL_TYPE: {
            buffer.append(std::to_string(std::get<decimal_t>(value)));
            break;
        }
        default: {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                ("Cannot serialize JSON number of type " +
                 LSPAnyTypeNames.at(valueType))
            );
        }
        }
    }

    void LspJsonSerializer::serializeBoolean(
        std::string &buffer,
        const LSPAny &value
    ) const {
        LSPAnyType valueType = static_cast<LSPAnyType>(value.index());
        switch (valueType) {
        case LSPAnyType::BOOLEAN_TYPE: {
            buffer.append(std::to_string(std::get<boolean_t>(value)));
            break;
        }
        default: {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                ("Cannot serialize JSON boolean of type " +
                 LSPAnyTypeNames.at(valueType))
            );
        }
        }
    }

    void LspJsonSerializer::serializeNull(
        std::string &buffer,
        const LSPAny &value
    ) const {
        LSPAnyType valueType = static_cast<LSPAnyType>(value.index());
        switch (valueType) {
        case LSPAnyType::NULL_TYPE: {
            buffer.append("null");
            break;
        }
        default: {
            throw LSP_EXCEPTION(
                ErrorCodes::INVALID_PARAMS,
                ("Cannot serialize JSON null of type " +
                 LSPAnyTypeNames.at(valueType))
            );
        }
        }
    }

} // namespace LCompilers::LanguageServerProtocol

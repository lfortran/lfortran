#include <utility>

#include <server/lsp_exception.h>
#include <server/lsp_json_serializer.h>
#include <server/lsp_specification.h>

namespace LCompilers::LanguageServerProtocol {

    LspJsonSerializer::LspJsonSerializer()
        : _indent(DEFAULT_INDENT_PATTERN)
    {
        // empty
    }

    LspJsonSerializer::LspJsonSerializer(const std::string &indent)
        : _indent(indent)
    {
        // empty
    }

    auto LspJsonSerializer::serialize(const LSPAny &any) const -> std::string {
        std::string buffer;
        buffer.reserve(1024);
        serializeValue(buffer, any);
        return buffer;
    }

    auto LspJsonSerializer::serialize(const LSPObject &object) const -> std::string {
        std::string buffer;
        buffer.reserve(1024);
        serializeObject(buffer, object);
        return buffer;
    }

    auto LspJsonSerializer::serialize(const LSPArray &array) const -> std::string {
        std::string buffer;
        buffer.reserve(1024);
        serializeArray(buffer, array);
        return buffer;
    }

    auto LspJsonSerializer::pprint(
        const LSPAny &any
    ) const -> std::string {
        std::string buffer;
        buffer.reserve(2048);
        pprintValue(buffer, any, 0);
        return buffer;
    }

    auto LspJsonSerializer::pprint(
        const LSPObject &object
    ) const -> std::string {
        std::string buffer;
        buffer.reserve(2048);
        pprintObject(buffer, object, 0);
        return buffer;
    }

    auto LspJsonSerializer::pprint(
        const LSPArray &array
    ) const -> std::string {
        std::string buffer;
        buffer.reserve(2048);
        pprintArray(buffer, array, 0);
        return buffer;
    }

    void LspJsonSerializer::newlineIndent(
        std::string &buffer,
        std::size_t level
    ) const {
        buffer.push_back('\n');
        for (std::size_t i = 0; i < level; ++i) {
            buffer.append(_indent);
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
        std::size_t level
    ) const {
        buffer.push_back('[');
        LSPArray::const_iterator iter = array.begin();
        if (iter != array.end()) {
            std::size_t nextLevel = level + 1;
            newlineIndent(buffer, nextLevel);
            pprintValue(buffer, **iter++, nextLevel);
            while (iter != array.end()) {
                buffer.push_back(',');
                newlineIndent(buffer, nextLevel);
                pprintValue(buffer, **iter++, nextLevel);
            }
            newlineIndent(buffer, level);
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
        std::size_t level
    ) const {
        buffer.push_back('{');
        LSPObject::const_iterator iter = object.begin();
        if (iter != object.end()) {
            std::size_t nextLevel = level + 1;
            newlineIndent(buffer, nextLevel);
            serializeString(buffer, iter->first);
            buffer.append(": ");
            pprintValue(buffer, *iter->second, nextLevel);
            while ((++iter) != object.end()) {
                buffer.push_back(',');
                newlineIndent(buffer, nextLevel);
                serializeString(buffer, iter->first);
                buffer.push_back(':');
                pprintValue(buffer, *iter->second, nextLevel);
            }
            newlineIndent(buffer, level);
        }
        buffer.push_back('}');
    }

    void LspJsonSerializer::serializeValue(
        std::string &buffer,
        const LSPAny &value
    ) const {
        switch (value.type()) {
        case LSPAnyType::Object: {
            serializeObject(buffer, value.object());
            break;
        }
        case LSPAnyType::Array: {
            serializeArray(buffer, value.array());
            break;
        }
        case LSPAnyType::String: {
            serializeString(buffer, value);
            break;
        }
        case LSPAnyType::Integer: // fallthrough
        case LSPAnyType::UInteger: // fallthrough
        case LSPAnyType::Decimal: {
            serializeNumber(buffer, value);
            break;
        }
        case LSPAnyType::Boolean: {
            serializeBoolean(buffer, value);
            break;
        }
        case LSPAnyType::Null: {
            serializeNull(buffer, value);
            break;
        }
        case LSPAnyType::Uninitialized: {
            throw LSP_EXCEPTION(
                ErrorCodes::InternalError,
                "Value was not initialized."
            );
        }
        }
    }

    void LspJsonSerializer::pprintValue(
        std::string &buffer,
        const LSPAny &value,
        std::size_t level
    ) const {
        switch (value.type()) {
        case LSPAnyType::Object: {
            pprintObject(buffer, value.object(), level);
            break;
        }
        case LSPAnyType::Array: {
            pprintArray(buffer, value.array(), level);
            break;
        }
        case LSPAnyType::String: {
            serializeString(buffer, value);
            break;
        }
        case LSPAnyType::Integer: // fallthrough
        case LSPAnyType::UInteger: // fallthrough
        case LSPAnyType::Decimal: {
            serializeNumber(buffer, value);
            break;
        }
        case LSPAnyType::Boolean: {
            serializeBoolean(buffer, value);
            break;
        }
        case LSPAnyType::Null: {
            serializeNull(buffer, value);
            break;
        }
        case LSPAnyType::Uninitialized: {
            throw LSP_EXCEPTION(
                ErrorCodes::InternalError,
                "Value was not initialized."
            );
        }
        }
    }

    void LspJsonSerializer::serializeString(
        std::string &buffer,
        const LSPAny &value
    ) const {
        switch (value.type()) {
        case LSPAnyType::String: {
            const string_t &string = value.string();
            serializeString(buffer, string);
            break;
        }
        default: {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                ("Cannot serialize JSON string of type " +
                 LSPAnyTypeNames.at(value.type()))
            );
        }
        }
    }

    void LspJsonSerializer::serializeString(
        std::string &buffer,
        const std::string &string
    ) const {
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
    }

    void LspJsonSerializer::serializeNumber(
        std::string &buffer,
        const LSPAny &value
    ) const {
        switch (value.type()) {
        case LSPAnyType::Integer: {
            buffer.append(std::to_string(value.integer()));
            break;
        }
        case LSPAnyType::UInteger: {
            buffer.append(std::to_string(value.uinteger()));
            break;
        }
        case LSPAnyType::Decimal: {
            buffer.append(std::to_string(value.decimal()));
            break;
        }
        default: {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                ("Cannot serialize JSON number of type " +
                 LSPAnyTypeNames.at(value.type()))
            );
        }
        }
    }

    void LspJsonSerializer::serializeBoolean(
        std::string &buffer,
        const LSPAny &value
    ) const {
        switch (value.type()) {
        case LSPAnyType::Boolean: {
            buffer.append(std::to_string(value.boolean()));
            break;
        }
        default: {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                ("Cannot serialize JSON boolean of type " +
                 LSPAnyTypeNames.at(value.type()))
            );
        }
        }
    }

    void LspJsonSerializer::serializeNull(
        std::string &buffer,
        const LSPAny &value
    ) const {
        switch (value.type()) {
        case LSPAnyType::Null: {
            buffer.append("null");
            break;
        }
        default: {
            throw LSP_EXCEPTION(
                ErrorCodes::InvalidParams,
                ("Cannot serialize JSON null of type " +
                 LSPAnyTypeNames.at(value.type()))
            );
        }
        }
    }

} // namespace LCompilers::LanguageServerProtocol

#pragma once

#include <server/specification.h>

namespace LCompilers::LanguageServerProtocol {

    class LspJsonSerializer {
    public:
        auto serialize(const LSPAny &any) const -> std::string;

        void serializeArray(
            std::string &buffer,
            const LSPArray &array
        ) const;

        void serializeObject(
            std::string &buffer,
            const LSPObject &object
        ) const;

        void serializeValue(
            std::string &buffer,
            const LSPAny &value
        ) const;

        void serializeString(
            std::string &buffer,
            const LSPAny &value
        ) const;

        void serializeNumber(
            std::string &buffer,
            const LSPAny &value
        ) const;

        void serializeBoolean(
            std::string &buffer,
            const LSPAny &value
        ) const;

        void serializeNull(
            std::string &buffer,
            const LSPAny &value
        ) const;

        auto pprint(
            const LSPArray &array,
            const std::string &indent
        ) const -> std::string;

        auto pprint(
            const LSPObject &object,
            const std::string &indent
        ) const -> std::string;

        auto pprint(
            const LSPAny &any,
            const std::string &indent
        ) const -> std::string;

        void newlineIndent(
            std::string &buffer,
            const std::string &indent,
            std::size_t level
        ) const;

        void pprintArray(
            std::string &buffer,
            const LSPArray &array,
            const std::string &indent,
            std::size_t level
        ) const;

        void pprintObject(
            std::string &buffer,
            const LSPObject &object,
            const std::string &indent,
            std::size_t level
        ) const;

        void pprintValue(
            std::string &buffer,
            const LSPAny &value,
            const std::string &indent,
            std::size_t level
        ) const;
    };

} // namespace LCompilers::LanguageServerProtocol

#pragma once

#include <string>

#include <server/lsp_specification.h>

namespace LCompilers::LanguageServerProtocol {

    const std::string DEFAULT_INDENT_PATTERN = "    ";

    class LspJsonSerializer {
    public:
        LspJsonSerializer();
        LspJsonSerializer(const std::string &indent);

        auto serialize(const LSPAny &any) const -> std::string;

        auto pprint(
            const LSPArray &array
        ) const -> std::string;

        auto pprint(
            const LSPObject &object
        ) const -> std::string;

        auto pprint(
            const LSPAny &any
        ) const -> std::string;

        inline auto indent() const -> const std::string & {
            return _indent;
        }

        inline void setIndent(const std::string &indent) {
            _indent = indent;
        }

    private:
        std::string _indent;

        void newlineIndent(
            std::string &buffer,
            std::size_t level
        ) const;

        void serializeArray(
            std::string &buffer,
            const LSPArray &array
        ) const;

        void pprintArray(
            std::string &buffer,
            const LSPArray &array,
            std::size_t level
        ) const;

        void serializeObject(
            std::string &buffer,
            const LSPObject &object
        ) const;

        void pprintObject(
            std::string &buffer,
            const LSPObject &object,
            std::size_t level
        ) const;

        void serializeValue(
            std::string &buffer,
            const LSPAny &value
        ) const;

        void pprintValue(
            std::string &buffer,
            const LSPAny &value,
            std::size_t level
        ) const;

        void serializeString(
            std::string &buffer,
            const LSPAny &value
        ) const;

        void serializeString(
            std::string &buffer,
            const std::string &string
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
    };

} // namespace LCompilers::LanguageServerProtocol

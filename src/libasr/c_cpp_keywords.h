#ifndef LIBASR_C_CPP_KEYWORDS_H
#define LIBASR_C_CPP_KEYWORDS_H

#include <string>
#include <unordered_set>

namespace LCompilers {

inline bool is_c_keyword(const std::string &name) {
    static const std::unordered_set<std::string> keywords = {
        "_Alignas", "_Alignof", "_Atomic", "_Bool", "_Complex", "_Generic",
        "_Imaginary", "_Noreturn", "_Static_assert", "_Thread_local", "auto",
        "break", "case", "char", "const", "continue", "default", "do",
        "double", "else", "enum", "extern", "float", "for", "goto", "if", "int",
        "long", "register", "return", "short", "signed", "sizeof", "static",
        "struct", "switch", "typedef", "union", "unsigned", "void", "volatile",
        "while"
    };
    return keywords.count(name) != 0;
}

inline bool is_cpp_keyword(const std::string &name) {
    static const std::unordered_set<std::string> keywords = {
        "alignas", "alignof", "and", "and_eq", "asm", "bitand", "bitor",
        "bool", "catch", "char16_t", "char32_t", "class", "compl", "constexpr",
        "const_cast", "decltype", "delete", "dynamic_cast", "explicit",
        "export", "false", "friend", "inline", "mutable", "namespace", "new",
        "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq",
        "private", "protected", "public", "reinterpret_cast", "static_assert",
        "static_cast", "template", "this", "thread_local", "throw", "true",
        "try", "typeid", "typename", "using", "virtual", "wchar_t", "xor",
        "xor_eq"
    };
    return is_c_keyword(name) || keywords.count(name) != 0;
}

} // namespace LCompilers

#endif

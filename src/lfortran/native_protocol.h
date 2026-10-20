#ifndef LFORTRAN_NATIVE_PROTOCOL_H
#define LFORTRAN_NATIVE_PROTOCOL_H

// Private to the native adapters. Do not include from compiler/backend headers.
#include <llvm/Support/JSON.h>
#include <llvm/Support/raw_ostream.h>
#include <lfortran/native_session.h>
#include <deque>
#include <stdexcept>

namespace LCompilers::LFortran::Local {
using Object = llvm::json::Object;
using Array = llvm::json::Array;
using Value = llvm::json::Value;
constexpr size_t max_frame = 16 * 1024 * 1024;
constexpr size_t max_output = 32 * 1024 * 1024;
constexpr size_t max_requests = 128;
constexpr size_t max_peers = 64;

inline std::string encode(const Value &value) {
    std::string result;
    llvm::raw_string_ostream out(result);
    out << value;
    return out.str();
}
inline Value parse(const std::string &text) {
    if (text.size() > max_frame || !llvm::json::isUTF8(text))
        throw std::runtime_error("invalid UTF-8 or oversized local message");
    // Bound recursion before calling LLVM's recursive JSON reader.
    size_t depth = 0;
    bool quoted = false, escape = false;
    for (char c : text) {
        if (quoted) {
            if (escape) escape = false;
            else if (c == '\\') escape = true;
            else if (c == '"') quoted = false;
        } else if (c == '"') quoted = true;
        else if (c == '{' || c == '[') {
            if (++depth > 64) throw std::runtime_error("local JSON nesting limit");
        } else if (c == '}' || c == ']') {
            if (!depth) throw std::runtime_error("invalid local JSON nesting");
            --depth;
        }
    }
    auto value = llvm::json::parse(text);
    if (!value) throw std::runtime_error(llvm::toString(value.takeError()));
    return std::move(*value);
}
inline const Object &object(const Value &value) {
    auto result = value.getAsObject();
    if (!result) throw std::runtime_error("expected local JSON object");
    return *result;
}
inline std::string string(const Object &value, llvm::StringRef name) {
    auto result = value.getString(name);
    if (!result) throw std::runtime_error("missing string: " + name.str());
    return result->str();
}
inline std::string optional_string(const Object &value, llvm::StringRef name) {
    auto result = value.getString(name);
    return result ? result->str() : "";
}
inline bool boolean(const Object &value, llvm::StringRef name, bool fallback) {
    auto found = value.find(name);
    if (found == value.end()) return fallback;
    auto result = found->second.getAsBoolean();
    if (!result) throw std::runtime_error("expected boolean: " + name.str());
    return *result;
}
inline const Object &member(const Object &value, llvm::StringRef name) {
    auto result = value.getObject(name);
    if (!result) throw std::runtime_error("missing object: " + name.str());
    return *result;
}
Object header(const std::string &type, const std::string &session);
Value message(const std::string &channel, const std::string &type,
    const std::string &session, const Object &parent, Object content,
    Object metadata = Object{});
void validate_message(const Object &message);
ClientEvent client_event(const std::string &envelope);

class Frames {
    std::string input;
public:
    std::deque<std::string> ready;
    void append(const char *data, size_t size) {
        input.append(data, size);
        while (input.size() >= 4) {
            size_t length = 0;
            for (size_t i = 0; i < 4; ++i)
                length = (length << 8) | static_cast<unsigned char>(input[i]);
            if (!length || length > max_frame)
                throw std::runtime_error("invalid local frame length");
            if (input.size() < length + 4) break;
            if (ready.size() >= max_requests)
                throw std::runtime_error("too many buffered local frames");
            ready.push_back(input.substr(4, length));
            input.erase(0, length + 4);
        }
    }
    static std::string pack(const std::string &text) {
        if (text.empty() || text.size() > max_frame)
            throw std::runtime_error("invalid outgoing local frame length");
        std::string result(4, '\0');
        for (size_t i = 0; i < 4; ++i)
            result[i] = static_cast<char>((text.size() >> ((3-i)*8)) & 255);
        return result + text;
    }
};
} // namespace LCompilers::LFortran::Local
#endif

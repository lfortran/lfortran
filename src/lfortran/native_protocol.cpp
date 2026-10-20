#include <lfortran/native_protocol.h>
#include <atomic>
#include <chrono>
#include <ctime>
#include <iomanip>
#include <random>
#include <sstream>

namespace LCompilers::LFortran {
std::string new_session_id() {
    static std::atomic<uint64_t> sequence{0};
    static const uint64_t seed = [] {
        std::random_device random;
        return (uint64_t(random()) << 32) ^ random();
    }();
    std::ostringstream out;
    out << std::hex << seed << "-" <<
        std::chrono::high_resolution_clock::now().time_since_epoch().count()
        << "-" << ++sequence;
    return out.str();
}
std::string json_string(const std::string &text) {
    return Local::encode(llvm::json::fixUTF8(text));
}
namespace Local {
Object header(const std::string &type, const std::string &session) {
    auto now = std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());
    std::tm utc{};
#ifdef _WIN32
    gmtime_s(&utc, &now);
#else
    gmtime_r(&now, &utc);
#endif
    char date[32];
    std::strftime(date, sizeof(date), "%Y-%m-%dT%H:%M:%SZ", &utc);
    return Object{{"msg_id", new_session_id()}, {"session", session},
        {"username", "lfortran"}, {"date", std::string(date)}, {"msg_type", type},
        {"version", "5.4"}};
}
Value message(const std::string &channel, const std::string &type,
        const std::string &session, const Object &parent, Object content,
        Object metadata) {
    return Object{{"channel", channel}, {"header", header(type, session)},
        {"parent_header", Object(parent)}, {"metadata", std::move(metadata)},
        {"content", std::move(content)}, {"buffers", Array{}}};
}
static bool base64(const std::string &value) {
    if (value.size() % 4) return false;
    size_t padding = 0;
    for (size_t i = 0; i < value.size(); ++i) {
        const unsigned char c = value[i];
        if (c == '=') {
            if (++padding > 2 || i < value.size()-2) return false;
        } else if (padding || !((c >= 'A' && c <= 'Z') ||
                (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') ||
                c == '+' || c == '/')) return false;
    }
    return true;
}
void validate_message(const Object &value) {
    auto channel = string(value, "channel");
    if (channel != "shell" && channel != "control" && channel != "stdin" &&
            channel != "iopub")
        throw std::runtime_error("invalid local channel");
    const auto &h = member(value, "header");
    for (const auto *field : {"msg_id", "session", "username", "date",
            "msg_type", "version"}) {
        auto text = string(h, field);
        if (text.size() > 1024 || (text.empty() &&
                std::string(field) != "username"))
            throw std::runtime_error("invalid local header");
    }
    auto version = string(h, "version");
    if (version.size() < 3 || version.substr(0, 2) != "5." ||
            version.find_first_not_of("0123456789", 2) != std::string::npos)
        throw std::runtime_error("local binding requires Jupyter protocol 5.x");
    member(value, "parent_header");
    member(value, "metadata");
    member(value, "content");
    auto buffers = value.getArray("buffers");
    if (!buffers || buffers->size() > 1024)
        throw std::runtime_error("invalid local buffers");
    for (const auto &buffer : *buffers) {
        auto text = buffer.getAsString();
        if (!text || !base64(text->str()))
            throw std::runtime_error("invalid base64 buffer");
    }
}
ClientEvent client_event(const std::string &envelope) {
    auto value = parse(envelope);
    const auto &m = object(value);
    if (m.getString("connection_error"))
        throw std::runtime_error(string(m, "connection_error"));
    validate_message(m);
    const auto &h = member(m, "header");
    const auto &p = member(m, "parent_header");
    const auto &c = member(m, "content");
    ClientEvent result;
    result.envelope = envelope;
    result.channel = string(m, "channel");
    result.type = string(h, "msg_type");
    result.id = string(h, "msg_id");
    result.session = string(h, "session");
    result.parent_id = optional_string(p, "msg_id");
    result.parent_session = optional_string(p, "session");
    result.content = encode(Object(c));
    result.text = optional_string(c, "text");
    result.stream_name = optional_string(c, "name");
    result.code = optional_string(c, "code");
    result.prompt = optional_string(c, "prompt");
    result.password = boolean(c, "password", false);
    result.status = optional_string(c, "execution_state");
    if (result.type == "is_complete_reply") result.status = string(c, "status");
    if (result.type == "execute_result" || result.type == "display_data") {
        if (auto data = c.getObject("data"))
            result.text = optional_string(*data, "text/plain");
    }
    if (result.type == "error")
        result.text = optional_string(c, "ename") + ": " + optional_string(c, "evalue");
    if (result.type == "execute_reply") {
        result.text.clear();
        if (optional_string(c, "status") == "error")
            result.text = optional_string(c, "ename") + ": " + optional_string(c, "evalue");
    }
    if (result.type == "kernel_info_reply")
        result.text = optional_string(c, "banner");
    return result;
}
} // namespace Local
} // namespace LCompilers::LFortran

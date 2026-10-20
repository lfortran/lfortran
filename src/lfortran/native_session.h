#ifndef LFORTRAN_NATIVE_SESSION_H
#define LFORTRAN_NATIVE_SESSION_H

#include <libasr/utils.h>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace LCompilers::LFortran {

// JSON is an adapter detail, not a dependency of the compiler or this API.
// Envelopes are Jupyter logical messages (the local binding encodes buffers
// individually as base64). Peer numbers are transport identities, not sessions.
struct SessionDelivery {
    uint64_t peer;
    std::string envelope;
    bool disconnect = false;
    // An origin's admission reservation follows its completion through every
    // transport queue, and is released only after sending (or real disconnect).
    std::shared_ptr<void> retention = nullptr;
    bool best_effort = false;
};

class SessionEndpoint {
public:
    virtual ~SessionEndpoint() = default;
    virtual const std::string &id() const = 0;
    virtual void receive(uint64_t peer, const std::string &envelope) = 0;
    virtual void disconnect(uint64_t peer) = 0;
    virtual std::vector<SessionDelivery> take_deliveries() = 0;
    virtual bool shutdown_requested() const = 0;
    virtual bool executing() const = 0;
    virtual size_t peer_count() const = 0;
};

class NativeSession : public SessionEndpoint {
    struct Impl;
    std::unique_ptr<Impl> impl;
public:
    explicit NativeSession(CompilerOptions options);
    ~NativeSession();
    NativeSession(const NativeSession &) = delete;
    NativeSession &operator=(const NativeSession &) = delete;
    const std::string &id() const override;
    void receive(uint64_t peer, const std::string &envelope) override;
    void disconnect(uint64_t peer) override;
    std::vector<SessionDelivery> take_deliveries() override;
    bool shutdown_requested() const override;
    // Called only after the worker is idle; blocked/fatal kernels are ended
    // by the owning server process, never by asynchronous C++ cancellation.
    bool executing() const override;
    size_t peer_count() const override;
};

// Optional adapters share the reactor thread and the same endpoint. Local
// peers use the low half of the identity space; adapters use the high half.
constexpr uint64_t first_adapter_peer = uint64_t{1} << 63;
class SessionAdapter {
public:
    virtual ~SessionAdapter() = default;
    virtual void start(SessionEndpoint &) = 0;
    virtual void poll(SessionEndpoint &) = 0; // nonblocking
    virtual bool deliver(const SessionDelivery &) = 0; // true if routed
    virtual bool pending() const = 0;
    virtual void stop() = 0;
};

struct ClientEvent {
    std::string channel, type, id, session, parent_id, parent_session;
    std::string envelope;
    std::string content;
    std::string text, stream_name, code, prompt, status;
    bool password = false;
};

// The terminal uses this interface only. A foreign-kernel adapter implements
// the same interface; completeness and completion fences are protocol decisions.
class SessionClient {
public:
    virtual ~SessionClient() = default;
    virtual std::string request(const std::string &type,
        const std::string &content, const std::string &channel = "shell",
        const std::string &parent = "{}") = 0;
    virtual bool next(ClientEvent &event, int timeout_ms) = 0;
    virtual const std::string &session_id() const = 0;
    virtual void send_envelope(const std::string &envelope) = 0;
    virtual void input_reply(const ClientEvent &prompt,
        const std::string &record, bool eof) = 0;
};

std::string new_session_id();
std::string json_string(const std::string &text);
bool session_input_available();
std::unique_ptr<SessionClient> connect_local_session(const std::string &file);
// Shared private-file/ACL checks for native and optional classic descriptors.
std::string read_private_connection_file(const std::string &file);
// The server owns endpoint/descriptor cleanup. ready_handle is an inherited
// startup pipe, or -1 for an explicitly launched persistent server.
int serve_local_session(CompilerOptions options, const std::string &file,
    bool managed, intptr_t ready_handle = -1, SessionAdapter *adapter = nullptr);
// A local-to-foreign bridge supplies an endpoint without constructing an
// EvaluationService. This also owns all local IPC/discovery lifecycle rules.
int serve_local_endpoint(SessionEndpoint &endpoint, const std::string &file,
    bool managed, intptr_t ready_handle = -1, SessionAdapter *adapter = nullptr);
std::string launch_local_session(const std::vector<std::string> &arguments,
    const std::string &file);

} // namespace LCompilers::LFortran
#endif

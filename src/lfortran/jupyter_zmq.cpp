#include <lfortran/jupyter_zmq.h>
#include <lfortran/native_protocol.h>
#include <zmq.h>
#include <openssl/crypto.h>
#include <openssl/evp.h>
#include <openssl/hmac.h>
#include <algorithm>
#include <array>
#include <chrono>
#include <cstdio>
#include <cstring>
#include <map>
#include <set>

namespace LCompilers::LFortran {
namespace {
using namespace Local;
using Clock = std::chrono::steady_clock;
using Parts = std::vector<std::string>;
constexpr const char *delimiter = "<IDS|MSG>";
constexpr std::array<const char *, 3> channels{"shell", "control", "stdin"};

struct Configuration {
    std::string ip, key;
    std::map<std::string, int> ports;
    explicit Configuration(const std::string &file) {
        auto value = parse(read_private_connection_file(file));
        const auto &c = object(value);
        for (const auto &entry : c) {
            auto name = entry.first.str();
            if (name.find("curve") != std::string::npos ||
                    name == "encryption" || name == "supported_encryption")
                throw std::runtime_error("Curve/encrypted Jupyter connections are not supported; "
                    "refusing a plaintext downgrade");
            if (name == "registration_ip" || name == "registration_port" ||
                    name == "registration_transport" || name == "registration_url")
                throw std::runtime_error("Jupyter registration handshake is not supported; "
                    "use a classic five-port connection file");
        }
        if (string(c, "transport") != "tcp")
            throw std::runtime_error("Jupyter adapter supports classic TCP connections only");
        ip = string(c, "ip");
        if (ip.empty()) ip = "127.0.0.1";
        if (ip.size() > 255 || ip.find_first_of("/\\; \r\n\t") != std::string::npos)
            throw std::runtime_error("invalid Jupyter TCP address");
        if (string(c, "signature_scheme") != "hmac-sha256")
            throw std::runtime_error("unsupported Jupyter signature_scheme; use hmac-sha256");
        key = string(c, "key");
        std::set<int> used;
        for (auto channel : {"shell", "control", "stdin", "iopub", "hb"}) {
            auto port = c.getInteger(std::string(channel) + "_port");
            if (!port || *port < 1 || *port > 65535 ||
                    !used.insert(static_cast<int>(*port)).second)
                throw std::runtime_error("classic Jupyter ports must be distinct integers in 1..65535");
            ports[channel] = static_cast<int>(*port);
        }
    }
    std::string address(const std::string &channel) const {
        auto host = ip.find(':') == std::string::npos ? ip : "[" + ip + "]";
        return "tcp://" + host + ":" + std::to_string(ports.at(channel));
    }
};

std::string base64_encode(const std::string &data) {
    if (data.empty()) return "";
    std::string result(4 * ((data.size() + 2) / 3) + 1, '\0');
    auto size = EVP_EncodeBlock(reinterpret_cast<unsigned char *>(result.data()),
        reinterpret_cast<const unsigned char *>(data.data()), static_cast<int>(data.size()));
    result.resize(size);
    return result;
}
std::string base64_decode(const std::string &data) {
    if (data.empty()) return "";
    std::string result(data.size() / 4 * 3, '\0');
    int size = EVP_DecodeBlock(reinterpret_cast<unsigned char *>(result.data()),
        reinterpret_cast<const unsigned char *>(data.data()), static_cast<int>(data.size()));
    if (size < 0) throw std::runtime_error("invalid Jupyter buffer encoding");
    if (data.back() == '=') --size;
    if (data.size() > 1 && data[data.size()-2] == '=') --size;
    result.resize(size);
    return result;
}
std::string signature(const std::string &key, const Parts &parts, size_t begin) {
    if (key.empty()) return "";
    std::string bytes;
    for (size_t i = begin; i < begin + 4; ++i) bytes += parts.at(i);
    unsigned char digest[EVP_MAX_MD_SIZE];
    unsigned int size = 0;
    if (!HMAC(EVP_sha256(), key.data(), static_cast<int>(key.size()),
            reinterpret_cast<const unsigned char *>(bytes.data()), bytes.size(), digest, &size))
        throw std::runtime_error("cannot sign Jupyter message");
    static constexpr char hex[] = "0123456789abcdef";
    std::string result;
    for (unsigned int i = 0; i < size; ++i) {
        result += hex[digest[i] >> 4];
        result += hex[digest[i] & 15];
    }
    return result;
}
struct WireMessage {
    Parts route;
    std::string envelope;
};
class Codec {
    std::string key;
    std::set<std::string> seen;
    std::deque<std::string> order;
public:
    explicit Codec(const std::string &key) : key(key) {}
    WireMessage decode(const Parts &parts, const std::string &channel) {
        auto mark = std::find(parts.begin(), parts.end(), delimiter);
        size_t offset = static_cast<size_t>(mark - parts.begin());
        if (mark == parts.end() || offset > 32 || parts.size() < offset + 6 ||
                parts.size() > offset + 6 + 1024)
            throw std::runtime_error("malformed Jupyter multipart");
        auto expected = signature(key, parts, offset + 2);
        const auto &actual = parts[offset + 1];
        if (actual.size() != expected.size() ||
                CRYPTO_memcmp(actual.data(), expected.data(), expected.size()) != 0)
            throw std::runtime_error("invalid Jupyter signature");
        if (!key.empty()) {
            if (!seen.insert(actual).second)
                throw std::runtime_error("replayed Jupyter signature");
            order.push_back(actual);
            if (order.size() > 4096) {
                seen.erase(order.front());
                order.pop_front();
            }
        }
        Object message{{"channel", channel}};
        size_t index = offset + 2;
        for (auto field : {"header", "parent_header", "metadata", "content"}) {
            auto json = parse(parts[index++]);
            object(json);
            message[field] = std::move(json);
        }
        Array buffers;
        for (; index < parts.size(); ++index)
            buffers.push_back(base64_encode(parts[index]));
        message["buffers"] = std::move(buffers);
        validate_message(message);
        return {Parts(parts.begin(), mark), encode(std::move(message))};
    }
    Parts encode_message(const std::string &text, Parts route = {}) const {
        auto value = parse(text);
        const auto &m = object(value);
        validate_message(m);
        Parts json;
        for (auto field : {"header", "parent_header", "metadata", "content"})
            json.push_back(encode(Object(member(m, field))));
        route.push_back(delimiter);
        route.push_back(signature(key, json, 0));
        route.insert(route.end(), json.begin(), json.end());
        for (const auto &buffer : *m.getArray("buffers"))
            route.push_back(base64_decode(buffer.getAsString()->str()));
        return route;
    }
};

class Context {
public:
    void *handle = zmq_ctx_new();
    Context() {
        if (!handle) throw std::runtime_error("cannot create ZeroMQ context");
    }
    ~Context() { zmq_ctx_term(handle); }
};
class Socket {
    Parts incoming;
    size_t bytes = 0;
    bool discarding = false;
public:
    void *handle;
    std::deque<Parts> output;
    size_t output_bytes = 0;
    int source = -1;
    Socket(Context &context, int type) : handle(zmq_socket(context.handle, type)) {
        if (!handle) throw std::runtime_error("cannot create ZeroMQ socket");
        try {
            option(ZMQ_LINGER, 0);
            option(ZMQ_SNDHWM, 256);
            option(ZMQ_RCVHWM, 256);
            int64_t limit = max_frame;
            if (zmq_setsockopt(handle, ZMQ_MAXMSGSIZE, &limit, sizeof(limit)))
                throw std::runtime_error("cannot bound ZeroMQ message size");
            option(ZMQ_IPV6, 1);
        } catch (...) { zmq_close(handle); throw; }
    }
    ~Socket() { zmq_close(handle); }
    Socket(const Socket &) = delete;
    void option(int name, int value) {
        if (zmq_setsockopt(handle, name, &value, sizeof(value)))
            throw std::runtime_error("cannot configure ZeroMQ socket");
    }
    void identity(const std::string &id) {
        if (zmq_setsockopt(handle, ZMQ_ROUTING_ID, id.data(), id.size()))
            throw std::runtime_error("cannot set ZeroMQ routing identity");
    }
    void attach(const std::string &address, bool server) {
        int status = server ? zmq_bind(handle, address.c_str())
                            : zmq_connect(handle, address.c_str());
        if (status) throw std::runtime_error("cannot " + std::string(server ? "bind " : "connect ") +
            "Jupyter channel: " + zmq_strerror(zmq_errno()));
    }
    // Multipart input is resumed between polls, including bounded draining of
    // malformed multipart messages. One peer cannot monopolize the reactor.
    bool receive(Parts &result) {
        for (size_t n = 0; n < 128; ++n) {
            zmq_msg_t frame;
            zmq_msg_init(&frame);
            if (zmq_msg_recv(&frame, handle, ZMQ_DONTWAIT) < 0) {
                int error = zmq_errno();
                zmq_msg_close(&frame);
                if (error == EAGAIN || error == EINTR) return false;
                throw std::runtime_error("Jupyter receive failed");
            }
            if (incoming.empty() && !discarding) source = zmq_msg_get(&frame, ZMQ_SRCFD);
            if (!discarding) bytes += zmq_msg_size(&frame);
            if (bytes > max_frame || incoming.size() >= 1062) {
                incoming.clear();
                discarding = true;
            }
            if (!discarding)
                incoming.emplace_back(static_cast<const char *>(zmq_msg_data(&frame)),
                    zmq_msg_size(&frame));
            bool more = zmq_msg_more(&frame);
            zmq_msg_close(&frame);
            if (!more) {
                bytes = 0;
                if (discarding) { discarding = false; continue; }
                result = std::move(incoming);
                incoming.clear();
                return true;
            }
        }
        return false;
    }
    void queue(Parts parts) {
        size_t size = 0;
        for (const auto &p : parts) size += p.size();
        if (output.size() >= max_requests || output_bytes + size > max_output)
            throw std::runtime_error("Jupyter output queue limit reached");
        output_bytes += size;
        output.push_back(std::move(parts));
    }
    enum class Sent { sent, blocked, disconnected };
    Sent send(const Parts &parts) {
        for (size_t i = 0; i < parts.size(); ++i) {
            int flags = ZMQ_DONTWAIT | (i + 1 == parts.size() ? 0 : ZMQ_SNDMORE);
            if (zmq_send(handle, parts[i].data(), parts[i].size(), flags) < 0) {
                int error = zmq_errno();
                if (i == 0 && (error == EAGAIN || error == EINTR)) return Sent::blocked;
                if (i == 0 && error == EHOSTUNREACH) return Sent::disconnected;
                throw std::runtime_error("Jupyter multipart send failed; outcome unknown");
            }
        }
        return Sent::sent;
    }
    void flush() {
        while (!output.empty()) {
            const auto &parts = output.front();
            auto result = send(parts);
            if (result == Sent::blocked) return;
            if (result == Sent::disconnected)
                throw std::runtime_error("Jupyter route disconnected; execution outcome unknown");
            for (const auto &part : parts) output_bytes -= part.size();
            output.pop_front();
        }
    }
};

struct MonitoredRouter {
    Socket socket;
    Socket monitor;
    explicit MonitoredRouter(Context &context)
        : socket(context, ZMQ_ROUTER), monitor(context, ZMQ_PAIR) {
        socket.option(ZMQ_ROUTER_MANDATORY, 1);
        auto address = "inproc://lfortran-monitor-" + new_session_id();
        if (zmq_socket_monitor(socket.handle, address.c_str(), ZMQ_EVENT_DISCONNECTED))
            throw std::runtime_error("cannot monitor Jupyter routes");
        monitor.attach(address, false);
    }
    std::vector<int> disconnected() {
        std::vector<int> result;
        Parts event;
        for (int i = 0; i < 64 && monitor.receive(event); ++i) {
            if (event.size() != 2 || event[0].size() < 6) continue;
            uint16_t type;
            uint32_t value;
            std::memcpy(&type, event[0].data(), sizeof(type));
            std::memcpy(&value, event[0].data() + sizeof(type), sizeof(value));
            if (type == ZMQ_EVENT_DISCONNECTED) result.push_back(static_cast<int>(value));
        }
        return result;
    }
};
std::string route_key(const Parts &route) {
    std::string key;
    for (const auto &frame : route) {
        for (size_t i = 0; i < 4; ++i)
            key += static_cast<char>((frame.size() >> ((3-i)*8)) & 255);
        key += frame;
    }
    return key;
}

class Server final : public SessionAdapter {
    Configuration config;
    Codec codec;
    Context context;
    std::array<std::unique_ptr<MonitoredRouter>, 3> routers;
    std::unique_ptr<Socket> iopub, heartbeat;
    struct Peer {
        uint64_t id;
        std::string session;
        Parts route;
        std::array<int, 3> sources{-1, -1, -1};
        std::string input;
        std::array<std::deque<Parts>, 3> output;
        size_t bytes = 0;
    };
    std::map<std::string, Peer> peers;
    uint64_t next_peer = first_adapter_peer;
    std::set<std::string> publications;
    std::deque<std::string> publication_order;
    SessionEndpoint *endpoint = nullptr;
    void remove(uint64_t peer) {
        if (endpoint) endpoint->disconnect(peer);
        for (auto i = peers.begin(); i != peers.end(); ) {
            if (i->second.id == peer) i = peers.erase(i); else ++i;
        }
    }
public:
    explicit Server(const std::string &file) : config(file), codec(config.key) {}
    ~Server() override { stop(); }
    void start(SessionEndpoint &session) override {
        endpoint = &session;
        for (size_t i = 0; i < channels.size(); ++i) {
            routers[i].reset(new MonitoredRouter(context));
            routers[i]->socket.option(ZMQ_LINGER, 100);
            routers[i]->socket.attach(config.address(channels[i]), true);
        }
        iopub.reset(new Socket(context, ZMQ_PUB));
        iopub->option(ZMQ_LINGER, 100);
        iopub->attach(config.address("iopub"), true);
        heartbeat.reset(new Socket(context, ZMQ_REP));
        heartbeat->attach(config.address("hb"), true);
    }
    void poll(SessionEndpoint &session) override {
        Parts parts;
        heartbeat->flush();
        if (heartbeat->output.empty() && heartbeat->receive(parts)) {
            heartbeat->queue(std::move(parts));
            heartbeat->flush();
        }
        for (size_t i = 0; i < channels.size(); ++i) {
            auto &router = *routers[i];
            for (int fd : router.disconnected()) {
                std::vector<uint64_t> gone;
                for (const auto &p : peers)
                    if (p.second.sources[i] == fd) gone.push_back(p.second.id);
                for (auto peer : gone) remove(peer);
            }
            for (size_t n = 0; n < 32 && router.socket.receive(parts); ++n) {
                try {
                    auto wire = codec.decode(parts, channels[i]);
                    if (wire.route.empty()) continue;
                    auto value = parse(wire.envelope);
                    auto &m = *value.getAsObject();
                    const auto &h = member(m, "header");
                    auto session_id = string(h, "session");
                    auto key = route_key(wire.route);
                    auto found = peers.find(key);
                    if (found == peers.end()) {
                        if (i == 2 || peers.size() >= max_peers) continue;
                        Peer p{next_peer++, session_id, wire.route, {-1,-1,-1}, "", {}, 0};
                        found = peers.emplace(key, std::move(p)).first;
                        session.receive(found->second.id, encode(Object{
                            {"lfortran_local_protocol", 1}, {"client_session", session_id},
                            {"features", Array{}}}));
                    }
                    auto &peer = found->second;
                    if (peer.session != session_id) continue;
                    peer.sources[i] = router.socket.source;
                    if (auto subshell = h.get("subshell_id"))
                        if (!subshell->getAsNull())
                            throw std::runtime_error("Jupyter subshells are not supported");
                    if (string(h, "msg_type") == "input_reply") {
                        if (i != 2 || peer.input.empty()) continue;
                        auto parent = parse(peer.input);
                        // Common clients (including BlockingKernelClient.input)
                        // omit the parent. The authenticated transport route and
                        // its sole pending prompt supply the correlation.
                        if (member(m, "parent_header").empty())
                            m["parent_header"] = Object(object(parent));
                        auto &metadata = *m.getObject("metadata");
                        if (boolean(metadata, "lfortran_stdin_eof", false))
                            throw std::runtime_error("standard Jupyter input has no EOF extension");
                        peer.input.clear();
                    }
                    session.receive(peer.id, encode(value));
                } catch (const std::exception &) {
                    // Unauthenticated/malformed/replayed frames are discarded
                    // without reflecting attacker data or killing other peers.
                }
            }
            std::vector<uint64_t> gone;
            for (auto &entry : peers) {
                auto &peer = entry.second;
                auto &output = peer.output[i];
                for (size_t n = 0; n < 32 && !output.empty(); ++n) {
                    auto result = router.socket.send(output.front());
                    if (result == Socket::Sent::blocked) break;
                    if (result == Socket::Sent::disconnected) {
                        gone.push_back(peer.id);
                        break;
                    }
                    for (const auto &frame : output.front()) peer.bytes -= frame.size();
                    output.pop_front();
                }
            }
            for (auto peer : gone) remove(peer);
        }
        iopub->flush();
    }
    bool deliver(const SessionDelivery &delivery) override {
        bool routed = delivery.peer >= first_adapter_peer;
        if (delivery.disconnect) {
            if (routed) remove(delivery.peer);
            return routed;
        }
        auto value = parse(delivery.envelope);
        const auto &m = object(value);
        auto channel = optional_string(m, "channel");
        if (channel == "iopub") {
            auto id = string(member(m, "header"), "msg_id");
            // NativeSession broadcasts per peer; one wire publication only.
            if (publications.insert(id).second) {
                publication_order.push_back(id);
                if (publication_order.size() > 4096) {
                    publications.erase(publication_order.front());
                    publication_order.pop_front();
                }
                iopub->queue(codec.encode_message(delivery.envelope,
                    Parts{string(member(m, "header"), "msg_type")}));
            }
        } else if (routed && !channel.empty()) {
            auto found = std::find_if(peers.begin(), peers.end(),
                [&](const auto &p) { return p.second.id == delivery.peer; });
            if (found == peers.end()) return true;
            size_t i = channel == "shell" ? 0 : channel == "control" ? 1 : 2;
            if (channel == "stdin") found->second.input = encode(Object(member(m, "header")));
            auto &peer = found->second;
            auto parts = codec.encode_message(delivery.envelope, peer.route);
            size_t bytes = 0;
            for (const auto &part : parts) bytes += part.size();
            if (peer.bytes + bytes > max_output || peer.output[i].size() >= max_requests) {
                remove(peer.id);
                return routed;
            }
            peer.bytes += bytes;
            peer.output[i].push_back(std::move(parts));
        }
        return routed;
    }
    bool pending() const override {
        if (iopub && !iopub->output.empty()) return true;
        if (heartbeat && !heartbeat->output.empty()) return true;
        for (const auto &p : peers) if (p.second.bytes) return true;
        return false;
    }
    void stop() noexcept override {
        peers.clear();
        for (auto &r : routers) r.reset();
        heartbeat.reset();
        iopub.reset();
        endpoint = nullptr;
    }
};

struct ClientRoute {
    std::string identity = new_session_id();
    std::array<std::unique_ptr<Socket>, 3> sockets;
    ClientRoute(Context &context, const Configuration &config) {
        for (size_t i = 0; i < channels.size(); ++i) {
            sockets[i].reset(new Socket(context, ZMQ_DEALER));
            sockets[i]->identity(identity);
            sockets[i]->attach(config.address(channels[i]), false);
        }
    }
};
// One subscription and heartbeat per upstream kernel, even in a many-peer
// bridge. DEALER identities, including stdin, are separate per local origin.
class Upstream {
    Configuration config;
    Codec codec;
    Context context;
    Socket iopub;
    std::unique_ptr<Socket> heartbeat;
    std::string probe;
    bool awaiting_heartbeat = false;
    Clock::time_point last_heartbeat = Clock::now(), sent_heartbeat{};
    std::map<uint64_t, std::unique_ptr<ClientRoute>> routes;
public:
    std::string kernel;
    std::deque<std::pair<uint64_t, std::string>> events;
    size_t event_bytes = 0;
    explicit Upstream(const std::string &file)
        : config(file), codec(config.key), iopub(context, ZMQ_SUB) {
        if (zmq_setsockopt(iopub.handle, ZMQ_SUBSCRIBE, "", 0))
            throw std::runtime_error("cannot subscribe to Jupyter output");
        iopub.attach(config.address("iopub"), false);
        reset_heartbeat();
    }
    void reset_heartbeat() {
        heartbeat.reset(new Socket(context, ZMQ_REQ));
        heartbeat->attach(config.address("hb"), false);
        awaiting_heartbeat = false;
    }
    void add(uint64_t peer) {
        if (routes.size() >= max_peers) throw std::runtime_error("Jupyter peer limit reached");
        routes.emplace(peer, std::make_unique<ClientRoute>(context, config));
    }
    void remove(uint64_t peer) { routes.erase(peer); }
    void send(uint64_t peer, const std::string &text) {
        auto value = parse(text);
        const auto &m = object(value);
        auto channel = string(m, "channel");
        if (channel != "shell" && channel != "control" && channel != "stdin")
            throw std::runtime_error("cannot send on Jupyter IOPub");
        auto &socket = routes.at(peer)->sockets[channel == "shell" ? 0 :
            channel == "control" ? 1 : 2];
        socket->queue(codec.encode_message(text));
        socket->flush();
    }
    void poll(int timeout) {
        std::vector<zmq_pollitem_t> wait{{iopub.handle, 0, ZMQ_POLLIN, 0},
            {heartbeat->handle, 0, ZMQ_POLLIN, 0}};
        for (auto &route : routes)
            for (auto &socket : route.second->sockets) {
                socket->flush();
                wait.push_back({socket->handle, 0, ZMQ_POLLIN, 0});
            }
        if (zmq_poll(wait.data(), static_cast<int>(wait.size()), timeout) < 0 &&
                zmq_errno() != EINTR)
            throw std::runtime_error("Jupyter polling failed");
        Parts parts;
        if (awaiting_heartbeat && heartbeat->receive(parts)) {
            if (parts.size() == 1 && parts[0] == probe) last_heartbeat = Clock::now();
            awaiting_heartbeat = false;
        }
        auto now = Clock::now();
        if (awaiting_heartbeat && now - sent_heartbeat > std::chrono::seconds(2))
            reset_heartbeat();
        if (!awaiting_heartbeat && now - sent_heartbeat >= std::chrono::seconds(1)) {
            probe = new_session_id();
            heartbeat->queue(Parts{probe});
            heartbeat->flush();
            sent_heartbeat = now;
            awaiting_heartbeat = true;
        }
        if (now - last_heartbeat > std::chrono::seconds(10))
            throw std::runtime_error("Jupyter heartbeat lost; execution outcome unknown "
                "(requests are never automatically retried)");
        auto receive = [&](Socket &socket, const std::string &channel, uint64_t peer) {
            for (size_t n = 0; n < 32 && socket.receive(parts); ++n) {
                WireMessage wire;
                try { wire = codec.decode(parts, channel); }
                catch (const std::exception &) { continue; }
                auto event = client_event(wire.envelope);
                if (!kernel.empty() && event.session != kernel)
                    throw std::runtime_error("Jupyter kernel restarted; execution outcome unknown");
                if (event_bytes + wire.envelope.size() > max_output ||
                        events.size() >= 4096)
                    throw std::runtime_error("upstream Jupyter event queue limit reached");
                event_bytes += wire.envelope.size();
                events.emplace_back(peer, std::move(wire.envelope));
            }
        };
        receive(iopub, "iopub", 0);
        for (auto &route : routes)
            for (size_t i = 0; i < channels.size(); ++i)
                receive(*route.second->sockets[i], channels[i], route.first);
    }
    std::pair<uint64_t, std::string> pop() {
        auto event = std::move(events.front());
        events.pop_front();
        event_bytes -= event.second.size();
        return event;
    }
    void ready(uint64_t peer, const std::string &session) {
        const auto deadline = Clock::now() + std::chrono::seconds(20);
        auto next = Clock::time_point{};
        std::set<std::string> requests, replies, publications;
        while (Clock::now() < deadline) {
            if (Clock::now() >= next) {
                auto request = message("shell", "kernel_info_request", session, Object{}, Object{});
                requests.insert(string(member(object(request), "header"), "msg_id"));
                send(peer, encode(request));
                next = Clock::now() + std::chrono::milliseconds(250);
            }
            poll(50);
            while (!events.empty()) {
                auto pair = pop();
                auto event = client_event(pair.second);
                if (event.parent_session != session || !requests.count(event.parent_id)) continue;
                if (event.type == "kernel_info_reply") {
                    auto value = parse(event.content);
                    auto protocol = string(object(value), "protocol_version");
                    if (protocol.substr(0, 2) != "5.")
                        throw std::runtime_error("external kernel must support Jupyter protocol 5.x");
                    replies.insert(event.parent_id);
                    kernel = event.session;
                }
                if (event.channel == "iopub" && event.type == "status")
                    publications.insert(event.parent_id);
                if (replies.count(event.parent_id) && publications.count(event.parent_id))
                    return;
            }
        }
        throw std::runtime_error("Jupyter kernel_info/IOPub subscription readiness timed out");
    }
    bool pending() const {
        for (const auto &route : routes)
            for (const auto &socket : route.second->sockets)
                if (!socket->output.empty()) return true;
        return false;
    }
};

class Client final : public SessionClient {
    Upstream upstream;
    std::string session = new_session_id();
public:
    explicit Client(const std::string &file) : upstream(file) {
        upstream.add(1);
        upstream.ready(1, session);
    }
    const std::string &session_id() const override { return session; }
    void send_envelope(const std::string &envelope) override { upstream.send(1, envelope); }
    std::string request(const std::string &type, const std::string &content,
            const std::string &channel, const std::string &parent) override {
        auto c = parse(content), p = parse(parent);
        auto value = message(channel, type, session, object(p), Object(object(c)));
        auto id = string(member(object(value), "header"), "msg_id");
        send_envelope(encode(value));
        return id;
    }
    bool next(ClientEvent &event, int timeout) override {
        if (upstream.events.empty()) upstream.poll(timeout);
        if (upstream.events.empty()) return false;
        event = client_event(upstream.pop().second);
        return true;
    }
    void input_reply(const ClientEvent &prompt, const std::string &record, bool eof) override {
        if (eof) throw std::runtime_error("standard Jupyter stdin does not support EOF/cancel; "
            "no fake input value was sent (the external kernel may still be waiting)");
        auto value = parse(prompt.envelope);
        request("input_reply", encode(Object{{"value", record}}), "stdin",
            encode(Object(member(object(value), "header"))));
    }
};

class Bridge final : public JupyterBridge {
    std::unique_ptr<Upstream> upstream;
    std::string kernel;
    struct Peer {
        std::string session, input;
        std::set<std::string> pending;
        size_t bytes = 0;
    };
    std::map<uint64_t, Peer> peers;
    std::vector<SessionDelivery> output;
    std::set<std::string> known;
    std::deque<std::string> order;
    bool stopping = false;
    void send(uint64_t peer, const std::string &envelope, bool close = false) {
        auto found = peers.find(peer);
        if (found == peers.end()) return;
        if (found->second.bytes + envelope.size() > max_output) {
            disconnect(peer);
            return;
        }
        found->second.bytes += envelope.size();
        output.push_back({peer, envelope, close});
    }
public:
    explicit Bridge(const std::string &file) : upstream(new Upstream(file)) {
        upstream->add(first_adapter_peer);
        upstream->ready(first_adapter_peer, new_session_id());
        kernel = upstream->kernel;
        upstream->remove(first_adapter_peer);
    }
    const std::string &id() const override { return kernel; }
    void receive(uint64_t peer, const std::string &envelope) override {
        try {
            if (stopping) throw std::runtime_error("bridge is shutting down");
            auto value = parse(envelope);
            const auto &m = object(value);
            if (!peers.count(peer)) {
                if (peers.size() >= max_peers ||
                        m.getInteger("lfortran_local_protocol").getValueOr(-1) != 1)
                    throw std::runtime_error("unsupported bridge hello or peer limit");
                auto session = string(m, "client_session");
                if (session.empty() || session.size() > 1024)
                    throw std::runtime_error("invalid bridge client session");
                upstream->add(peer);
                peers.emplace(peer, Peer{session, "", {}, 0});
                send(peer, encode(Object{{"lfortran_local_protocol", 1},
                    {"kernel_session", kernel}, {"jupyter_protocol", "5.4"},
                    {"features", Array{"stdin", "rich_output"}},
                    {"max_frame_bytes", static_cast<int64_t>(max_frame)},
                    {"lfortran_bridge", true}}));
                return;
            }
            validate_message(m);
            const auto &h = member(m, "header");
            auto &p = peers.at(peer);
            if (string(h, "session") != p.session)
                throw std::runtime_error("bridge header session disagrees with hello");
            auto request = string(h, "msg_id"), type = string(h, "msg_type");
            for (const auto &other : peers)
                if (other.second.pending.count(request))
                    throw std::runtime_error("duplicate outstanding bridge request");
            if (!known.insert(request).second)
                throw std::runtime_error("duplicate bridge request; no automatic execute retry");
            order.push_back(request);
            if (order.size() > 4096) { known.erase(order.front()); order.pop_front(); }
            if (type == "input_reply") {
                if (p.input.empty() || string(m, "channel") != "stdin")
                    throw std::runtime_error("unexpected bridge input_reply");
                if (boolean(member(m, "metadata"), "lfortran_stdin_eof", false))
                    throw std::runtime_error("standard Jupyter stdin has no EOF/cancel; "
                        "no fake input value was sent; upstream may still be waiting");
                auto prompt = parse(p.input);
                if (optional_string(member(m, "parent_header"), "msg_id") !=
                        string(object(prompt), "msg_id") ||
                        optional_string(member(m, "parent_header"), "session") != kernel)
                    throw std::runtime_error("wrongly correlated bridge input_reply");
                p.input.clear();
            } else if (type == "shutdown_request") {
                if (boolean(member(m, "content"), "restart", false))
                    throw std::runtime_error("bridge cannot restart a foreign kernel");
                auto reply = message(string(m, "channel"), "shutdown_reply", kernel, h,
                    Object{{"restart", false}}, Object{{"lfortran_bridge_local", true}});
                send(peer, encode(reply));
                stopping = true; // Never shut down a kernel owned by someone else.
                return;
            } else {
                if (string(m, "channel") != "shell" && string(m, "channel") != "control")
                    throw std::runtime_error("invalid bridge request channel");
                if (h.get("subshell_id") && !h.get("subshell_id")->getAsNull())
                    throw std::runtime_error("bridge subshells are not supported");
                // Optional queries may legitimately never receive a reply.
                // Only executions retain a slot beyond the bounded send queue.
                if (type == "execute_request") {
                    if (p.pending.size() >= max_requests)
                        throw std::runtime_error("bridge pending execution limit reached");
                    p.pending.insert(request);
                }
            }
            upstream->send(peer, envelope);
        } catch (const std::exception &e) {
            if (!peers.count(peer)) peers.emplace(peer, Peer{});
            send(peer, encode(Object{{"connection_error", e.what()}}), true);
        }
    }
    void disconnect(uint64_t peer) override {
        auto p = peers.find(peer);
        if (p != peers.end() && !p->second.input.empty())
            std::fprintf(stderr, "bridge: input origin disconnected; standard Jupyter has "
                "no EOF/cancel; the foreign kernel may still be waiting\n");
        peers.erase(peer);
        if (upstream) upstream->remove(peer);
    }
    std::vector<SessionDelivery> take_deliveries() override {
        auto result = std::move(output);
        output.clear();
        for (auto &p : peers) p.second.bytes = 0;
        return result;
    }
    bool shutdown_requested() const override { return stopping; }
    bool executing() const override { return false; }
    size_t peer_count() const override { return peers.size(); }
    void start(SessionEndpoint &) override {}
    void poll(SessionEndpoint &) override {
        upstream->poll(0);
        while (!upstream->events.empty()) {
            auto pair = upstream->pop();
            auto event = client_event(pair.second);
            if (pair.first == 0) {
                std::vector<uint64_t> recipients;
                for (const auto &p : peers) recipients.push_back(p.first);
                for (auto peer : recipients) send(peer, pair.second);
            } else {
                auto p = peers.find(pair.first);
                if (p == peers.end()) continue;
                if (event.type == "input_request") {
                    auto value = parse(pair.second);
                    p->second.input = encode(Object(member(object(value), "header")));
                } else if (event.channel == "shell" || event.channel == "control") {
                    p->second.pending.erase(event.parent_id);
                }
                send(pair.first, pair.second);
            }
        }
    }
    bool deliver(const SessionDelivery &) override { return false; }
    bool pending() const override { return upstream && upstream->pending(); }
    void stop() noexcept override { upstream.reset(); }
};
} // namespace

std::unique_ptr<SessionAdapter> jupyter_server(const std::string &file) {
    return std::make_unique<Server>(file);
}
std::unique_ptr<SessionClient> connect_jupyter_session(const std::string &file) {
    return std::make_unique<Client>(file);
}
std::unique_ptr<JupyterBridge> jupyter_bridge(const std::string &file) {
    return std::make_unique<Bridge>(file);
}
bool is_local_connection_file(const std::string &file) {
    auto value = Local::parse(read_private_connection_file(file));
    return Local::object(value).get("lfortran_local_protocol") != nullptr;
}
} // namespace LCompilers::LFortran

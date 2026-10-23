// The parts of the Jupyter messaging protocol that LFortran needs, with no
// third-party library: a kernel (when not built with Xeus) and a client for
// the prompt. Any Jupyter client can use the kernel, and the client can use
// any Jupyter kernel.
//
// Transport: ZeroMQ's ZMTP 3.0 with the NULL mechanism over TCP (the "tcp"
// transport of a connection file), implementing only what a Jupyter kernel
// and client do with it: ROUTER sockets for shell, control and stdin, a PUB
// socket for IOPub (it sends every message to every subscriber) and a REP
// socket for the heartbeat; DEALER and SUB sockets on the client side.
// Messages are signed with HMAC-SHA256 and encoded with LLVM's JSON library.
//
// The kernel runs everything on one thread, but for the heartbeat, which
// clients expect to answer while a cell runs: a long cell holds the other
// requests back (they wait in their sockets), and its output is sent when it
// reads input, displays or ends.

#include <lfortran/jupyter.h>
#include <lfortran/fortran_kernel.h>

#include <iostream>
#include <stdexcept>

#ifndef __EMSCRIPTEN__

#include <atomic>
#include <cctype>
#include <cerrno>
#include <chrono>
#include <csignal>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <ctime>
#include <deque>
#include <filesystem>
#include <fstream>
#include <random>
#include <sstream>
#include <thread>
#include <vector>

#include <llvm/Support/JSON.h>
#include <llvm/Support/raw_ostream.h>

#include <lfortran/utils.h>

// After every other header: <winsock2.h> defines macros such as min and max.
#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#include <process.h>
#else
#include <arpa/inet.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <spawn.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

extern char **environ;
#endif

namespace LCompilers::LFortran::jupyter {

namespace {

using llvm::json::Array;
using llvm::json::Object;

// Sockets

#ifdef _WIN32
using socket_t = SOCKET;
const socket_t no_socket = INVALID_SOCKET;

void close_socket(socket_t s) { closesocket(s); }

int poll_sockets(std::vector<pollfd> &fds, int timeout)
{
    return WSAPoll(fds.data(), static_cast<ULONG>(fds.size()), timeout);
}

std::string socket_error() { return "socket error " + std::to_string(WSAGetLastError()); }

bool interrupted() { return false; }

bool would_block() { return WSAGetLastError() == WSAEWOULDBLOCK; }

void set_nonblocking(socket_t s)
{
    u_long yes = 1;
    ioctlsocket(s, FIONBIO, &yes);
}
#else
using socket_t = int;
const socket_t no_socket = -1;

void close_socket(socket_t s) { close(s); }

int poll_sockets(std::vector<pollfd> &fds, int timeout)
{
    return poll(fds.data(), fds.size(), timeout);
}

std::string socket_error() { return std::strerror(errno); }

bool interrupted() { return errno == EINTR; }

bool would_block() { return errno == EAGAIN || errno == EWOULDBLOCK; }

void set_nonblocking(socket_t s)
{
    fcntl(s, F_SETFL, fcntl(s, F_GETFL) | O_NONBLOCK);
}
#endif

// Starts Winsock. A write to a closed connection fails rather than raising
// SIGPIPE.
void start_sockets()
{
#ifdef _WIN32
    static WSADATA data;
    static int started = WSAStartup(MAKEWORD(2, 2), &data);
    (void)started;
#else
    std::signal(SIGPIPE, SIG_IGN);
#endif
}

// At most 1 GiB, which fits the int that Winsock takes.
int chunk(size_t size)
{
    return static_cast<int>(size < (size_t(1) << 30) ? size : (size_t(1) << 30));
}

bool send_all(socket_t s, const char *data, size_t size)
{
    while (size > 0) {
        auto n = ::send(s, data, chunk(size), 0);
        if (n < 0 && interrupted()) continue;
        if (n <= 0) return false;
        data += n;
        size -= static_cast<size_t>(n);
    }
    return true;
}

bool read_all(socket_t s, char *data, size_t size)
{
    while (size > 0) {
        auto n = ::recv(s, data, chunk(size), 0);
        if (n < 0 && interrupted()) continue;
        if (n <= 0) return false;
        data += n;
        size -= static_cast<size_t>(n);
    }
    return true;
}

void set_no_delay(socket_t s)
{
    int yes = 1;
    setsockopt(s, IPPROTO_TCP, TCP_NODELAY, reinterpret_cast<const char *>(&yes),
        sizeof(yes));
}

sockaddr_in address(const std::string &ip, int port)
{
    sockaddr_in a{};
    a.sin_family = AF_INET;
    a.sin_port = htons(static_cast<uint16_t>(port));
    if (ip == "*" || ip == "0.0.0.0") {
        a.sin_addr.s_addr = htonl(INADDR_ANY);
    } else if (inet_pton(AF_INET, ip.c_str(), &a.sin_addr) != 1) {
        throw std::runtime_error("unsupported IP address " + ip
            + " (only IPv4 addresses are supported)");
    }
    return a;
}

// Listens on `port` of `ip`, or on a free port if it is 0, which `port` then
// becomes.
socket_t listen_on(const std::string &ip, int &port)
{
    sockaddr_in a = address(ip, port);
    socket_t s = socket(AF_INET, SOCK_STREAM, 0);
    if (s == no_socket) throw std::runtime_error("cannot create a socket: " + socket_error());
#ifndef _WIN32
    int yes = 1;
    setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(yes));
#endif
    if (bind(s, reinterpret_cast<sockaddr *>(&a), sizeof(a)) != 0 || listen(s, 16) != 0) {
        std::string reason = socket_error();
        close_socket(s);
        throw std::runtime_error("cannot listen on " + ip + ":" + std::to_string(port)
            + ": " + reason);
    }
    socklen_t size = sizeof(a);
    getsockname(s, reinterpret_cast<sockaddr *>(&a), &size);
    port = ntohs(a.sin_port);
    return s;
}

socket_t connect_to(const std::string &ip, int port)
{
    sockaddr_in a = address(ip == "*" || ip == "0.0.0.0" ? "127.0.0.1" : ip, port);
    socket_t s = socket(AF_INET, SOCK_STREAM, 0);
    if (s == no_socket) throw std::runtime_error("cannot create a socket: " + socket_error());
    if (connect(s, reinterpret_cast<sockaddr *>(&a), sizeof(a)) != 0) {
        std::string reason = socket_error();
        close_socket(s);
        throw std::runtime_error("cannot connect to the kernel at " + ip + ":"
            + std::to_string(port) + ": " + reason);
    }
    return s;
}

// HMAC-SHA256 (FIPS 180-4, RFC 2104), the signature scheme of Jupyter
// messages.

class Sha256 {
    uint32_t h[8] = {0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
        0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19};
    unsigned char block[64];
    size_t used = 0;
    uint64_t length = 0;

    static uint32_t rotate(uint32_t x, int n) { return (x >> n) | (x << (32 - n)); }

    void compress()
    {
        static const uint32_t k[64] = {
            0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
            0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
            0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
            0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
            0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
            0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
            0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
            0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2};
        uint32_t w[64];
        for (int i = 0; i < 16; i++) {
            w[i] = uint32_t(block[4 * i]) << 24 | uint32_t(block[4 * i + 1]) << 16
                | uint32_t(block[4 * i + 2]) << 8 | block[4 * i + 3];
        }
        for (int i = 16; i < 64; i++) {
            uint32_t s0 = rotate(w[i - 15], 7) ^ rotate(w[i - 15], 18) ^ (w[i - 15] >> 3);
            uint32_t s1 = rotate(w[i - 2], 17) ^ rotate(w[i - 2], 19) ^ (w[i - 2] >> 10);
            w[i] = w[i - 16] + s0 + w[i - 7] + s1;
        }
        uint32_t a = h[0], b = h[1], c = h[2], d = h[3], e = h[4], f = h[5], g = h[6], hh = h[7];
        for (int i = 0; i < 64; i++) {
            uint32_t t1 = hh + (rotate(e, 6) ^ rotate(e, 11) ^ rotate(e, 25))
                + ((e & f) ^ (~e & g)) + k[i] + w[i];
            uint32_t t2 = (rotate(a, 2) ^ rotate(a, 13) ^ rotate(a, 22))
                + ((a & b) ^ (a & c) ^ (b & c));
            hh = g; g = f; f = e; e = d + t1; d = c; c = b; b = a; a = t1 + t2;
        }
        h[0] += a; h[1] += b; h[2] += c; h[3] += d;
        h[4] += e; h[5] += f; h[6] += g; h[7] += hh;
    }

public:
    void update(const std::string &data)
    {
        for (char ch : data) {
            block[used++] = static_cast<unsigned char>(ch);
            if (used == 64) {
                compress();
                used = 0;
            }
        }
        length += data.size();
    }

    // The 32 bytes of the digest.
    std::string digest()
    {
        uint64_t bits = length * 8;
        update(std::string(1, '\x80'));
        while (used != 56) update(std::string(1, '\0'));
        std::string size(8, '\0');
        for (int i = 0; i < 8; i++) size[i] = char(bits >> (56 - 8 * i));
        update(size);
        std::string out(32, '\0');
        for (int i = 0; i < 32; i++) out[i] = char(h[i / 4] >> (24 - 8 * (i % 4)));
        return out;
    }
};

// The signature of a message's four JSON parts, in lowercase hex; empty if
// there is no key.
std::string sign(const std::string &key, const std::string *parts)
{
    if (key.empty()) return "";
    std::string k = key;
    if (k.size() > 64) {
        Sha256 hash;
        hash.update(k);
        k = hash.digest();
    }
    k.resize(64, '\0');
    std::string inner_pad = k, outer_pad = k;
    for (int i = 0; i < 64; i++) {
        inner_pad[i] ^= 0x36;
        outer_pad[i] ^= 0x5c;
    }
    Sha256 inner;
    inner.update(inner_pad);
    for (int i = 0; i < 4; i++) inner.update(parts[i]);
    Sha256 outer;
    outer.update(outer_pad);
    outer.update(inner.digest());
    std::string digest = outer.digest(), hex;
    const char *digits = "0123456789abcdef";
    for (char c : digest) {
        hex += digits[static_cast<unsigned char>(c) >> 4];
        hex += digits[c & 0xf];
    }
    return hex;
}

// JSON

std::string dump(const llvm::json::Value &value)
{
    std::string text;
    llvm::raw_string_ostream os(text);
    os << value;
    return os.str();
}

std::string get_string(const Object &object, llvm::StringRef key)
{
    if (auto value = object.getString(key)) return value->str();
    return "";
}

std::string random_hex()
{
    static std::random_device device;
    std::string hex;
    for (int i = 0; i < 4; i++) {
        char part[9];
        std::snprintf(part, sizeof(part), "%08x", static_cast<unsigned>(device()));
        hex += part;
    }
    return hex;
}

std::string now()
{
    auto time = std::chrono::system_clock::now();
    std::time_t seconds = std::chrono::system_clock::to_time_t(time);
    long long micro = std::chrono::duration_cast<std::chrono::microseconds>(
        time.time_since_epoch()).count() % 1000000;
    std::tm utc;
#ifdef _WIN32
    gmtime_s(&utc, &seconds);
#else
    gmtime_r(&seconds, &utc);
#endif
    char text[40];
    size_t n = std::strftime(text, sizeof(text), "%Y-%m-%dT%H:%M:%S", &utc);
    std::snprintf(text + n, sizeof(text) - n, ".%06lldZ", micro);
    return text;
}

// Connection files

const char *const port_names[5] = {"shell_port", "control_port", "stdin_port",
    "iopub_port", "hb_port"};
enum Channel { shell, control, stdin_channel, iopub, heartbeat };

struct Connection {
    std::string ip = "127.0.0.1";
    std::string key;
    int ports[5] = {0, 0, 0, 0, 0};
};

Connection read_connection(const std::string &path)
{
    std::ifstream file(path);
    if (!file) throw std::runtime_error("cannot read the connection file " + path);
    std::stringstream text;
    text << file.rdbuf();
    auto value = llvm::json::parse(text.str());
    if (!value) {
        llvm::consumeError(value.takeError());
        throw std::runtime_error(path + " is not a Jupyter connection file");
    }
    const Object *object = value->getAsObject();
    if (!object) throw std::runtime_error(path + " is not a Jupyter connection file");
    Connection c;
    std::string transport = get_string(*object, "transport");
    if (!transport.empty() && transport != "tcp") {
        throw std::runtime_error("the " + transport + " transport is not supported, only tcp");
    }
    c.key = get_string(*object, "key");
    std::string scheme = get_string(*object, "signature_scheme");
    if (!c.key.empty() && !scheme.empty() && scheme != "hmac-sha256") {
        throw std::runtime_error("the " + scheme + " signature scheme is not supported, only hmac-sha256");
    }
    if (object->getString("ip")) c.ip = get_string(*object, "ip");
    for (int i = 0; i < 5; i++) {
        if (auto port = object->getInteger(port_names[i])) c.ports[i] = static_cast<int>(*port);
    }
    return c;
}

void write_connection(const std::string &path, const Connection &c)
{
    Object object{{"transport", "tcp"}, {"ip", c.ip}, {"key", c.key},
        {"signature_scheme", "hmac-sha256"}, {"kernel_name", "fortran"}};
    for (int i = 0; i < 5; i++) object[port_names[i]] = c.ports[i];
    // Written whole under another name and then renamed, so that a client
    // never reads part of it.
    const std::string partial = path + ".partial";
    {
#ifndef _WIN32
        // The key lets whoever reads the file run code as this user.
        mode_t mask = umask(077);
#endif
        std::ofstream file(partial);
#ifndef _WIN32
        umask(mask);
#endif
        file << dump(llvm::json::Value(std::move(object))) << "\n";
        if (!file) throw std::runtime_error("cannot write the connection file " + path);
    }
    if (std::rename(partial.c_str(), path.c_str()) != 0) {
        std::remove(partial.c_str());
        throw std::runtime_error("cannot write the connection file " + path);
    }
}

// Messages

struct Message {
    std::vector<std::string> identities;
    Object header, parent, metadata, content;

    std::string type() const { return get_string(header, "msg_type"); }
    std::string id() const { return get_string(header, "msg_id"); }
    std::string parent_id() const { return get_string(parent, "msg_id"); }
};

Message new_message(const std::string &type, const Object &parent, Object content,
    const std::string &session)
{
    Message m;
    m.header = Object{{"msg_id", random_hex()}, {"session", session},
        {"username", "lfortran"}, {"date", now()}, {"msg_type", type},
        {"version", "5.3"}};
    m.parent = parent;
    m.content = std::move(content);
    return m;
}

std::vector<std::string> encode(const Message &m, const std::string &key)
{
    std::vector<std::string> frames = m.identities;
    std::string parts[4] = {dump(Object(m.header)), dump(Object(m.parent)),
        dump(Object(m.metadata)), dump(Object(m.content))};
    frames.push_back("<IDS|MSG>");
    frames.push_back(sign(key, parts));
    for (auto &part : parts) frames.push_back(std::move(part));
    return frames;
}

bool decode(const std::vector<std::string> &frames, const std::string &key, Message &m)
{
    size_t d = 0;
    while (d < frames.size() && frames[d] != "<IDS|MSG>") d++;
    if (d + 6 > frames.size()) return false;
    if (!key.empty() && sign(key, &frames[d + 2]) != frames[d + 1]) return false;
    m.identities.assign(frames.begin(), frames.begin() + d);
    Object *parts[4] = {&m.header, &m.parent, &m.metadata, &m.content};
    for (int i = 0; i < 4; i++) {
        auto value = llvm::json::parse(frames[d + 2 + i]);
        if (!value) {
            llvm::consumeError(value.takeError());
            return false;
        }
        Object *object = value->getAsObject();
        if (!object) return false;
        *parts[i] = std::move(*object);
    }
    return true;
}

} // namespace

// One ZeroMQ connection: ZMTP 3.0 (https://rfc.zeromq.org/spec/23/) with the
// NULL mechanism over a TCP socket.
class Peer {
public:
    socket_t s;
    // The peer's Identity: Jupyter clients give their shell and stdin
    // sockets the same one, so that a READ asks the client that ran the cell.
    std::string identity;
    bool closed = false;

    // Exchanges greetings and READY commands as a socket of `type`; closes
    // `s` and throws if that fails.
    Peer(socket_t fd, const std::string &type, const std::string &own_identity = "") : s(fd)
    {
        set_no_delay(s);
        if (!handshake(type, own_identity)) {
            close_socket(s);
            throw std::runtime_error("the ZeroMQ handshake failed");
        }
    }
    ~Peer() { close_socket(s); }
    Peer(const Peer &) = delete;
    Peer &operator=(const Peer &) = delete;

    bool send(const std::vector<std::string> &frames)
    {
        std::string data = encode_frames(frames);
        if (!closed && !send_all(s, data.data(), data.size())) closed = true;
        return !closed;
    }

    // An IOPub subscriber does not block the kernel: one that does not read,
    // such as a prompt waiting for keys, gets its messages from an outbox,
    // which drops them beyond 64 MiB, as ZeroMQ does at its high-water mark.
    void make_subscriber()
    {
        set_nonblocking(s);
    }

    void post(const std::vector<std::string> &frames)
    {
        if (closed || outbox_bytes > (size_t(64) << 20)) return;
        outbox.push_back(encode_frames(frames));
        outbox_bytes += outbox.back().size();
        flush();
    }

    bool pending() const { return !closed && !outbox.empty(); }

    // Sends what the subscriber's socket takes now.
    void flush()
    {
        while (pending()) {
            const std::string &data = outbox.front();
            auto n = ::send(s, data.data() + sent, chunk(data.size() - sent), 0);
            if (n < 0 && interrupted()) continue;
            if (n < 0 && would_block()) return;
            if (n <= 0) {
                closed = true;
                return;
            }
            sent += static_cast<size_t>(n);
            if (sent == data.size()) {
                outbox_bytes -= data.size();
                outbox.pop_front();
                sent = 0;
            }
        }
    }

    // Reads and ignores what a subscriber sends (its subscriptions); false
    // once it has disconnected.
    bool discard_input()
    {
        char buffer[4096];
        auto n = ::recv(s, buffer, sizeof(buffer), 0);
        if (n == 0 || (n < 0 && !interrupted() && !would_block())) closed = true;
        return !closed;
    }

    // Reads one message, skipping commands.
    bool receive(std::vector<std::string> &frames)
    {
        frames.clear();
        unsigned char flags;
        std::string body;
        do {
            if (!read_frame(flags, body)) {
                closed = true;
                return false;
            }
            if (!(flags & 4)) frames.push_back(std::move(body));
        } while ((flags & 1) || (flags & 4));
        return true;
    }

private:
    std::deque<std::string> outbox;
    size_t outbox_bytes = 0, sent = 0;

    static std::string encode_frames(const std::vector<std::string> &frames)
    {
        std::string data;
        for (size_t i = 0; i < frames.size(); i++) {
            data += frame(i + 1 < frames.size() ? 1 : 0, frames[i]);
        }
        return data;
    }

    // flags: 1 more frames follow, 2 long size, 4 command
    static std::string frame(int flags, const std::string &body)
    {
        std::string out;
        if (body.size() > 255) {
            out += char(flags | 2);
            for (int i = 7; i >= 0; i--) out += char(uint64_t(body.size()) >> (8 * i));
        } else {
            out += char(flags);
            out += char(body.size());
        }
        return out + body;
    }

    bool read_frame(unsigned char &flags, std::string &body)
    {
        if (!read_all(s, reinterpret_cast<char *>(&flags), 1)) return false;
        unsigned char size[8];
        int n = (flags & 2) ? 8 : 1;
        if (!read_all(s, reinterpret_cast<char *>(size), n)) return false;
        uint64_t length = 0;
        for (int i = 0; i < n; i++) length = length << 8 | size[i];
        body.resize(length);
        return length == 0 || read_all(s, &body[0], length);
    }

    static std::string property(const std::string &name, const std::string &value)
    {
        std::string out(1, char(name.size()));
        out += name;
        for (int i = 3; i >= 0; i--) out += char(uint32_t(value.size()) >> (8 * i));
        return out + value;
    }

    bool handshake(const std::string &type, const std::string &own_identity)
    {
        std::string greeting(64, '\0');
        greeting[0] = '\xff';
        greeting[9] = '\x7f';
        greeting[10] = 3;
        greeting.replace(12, 4, "NULL");
        std::string peer(64, '\0');
        if (!send_all(s, greeting.data(), greeting.size())
                || !read_all(s, &peer[0], peer.size())) {
            return false;
        }
        if (peer[0] != '\xff' || peer[9] != '\x7f' || peer[10] < 3
                || peer.compare(12, 5, std::string("NULL\0", 5)) != 0) {
            return false;
        }
        std::string ready = "\x05READY" + property("Socket-Type", type);
        if (!own_identity.empty()) ready += property("Identity", own_identity);
        ready = frame(4, ready);
        if (!send_all(s, ready.data(), ready.size())) return false;
        unsigned char flags;
        std::string body;
        if (!read_frame(flags, body) || !(flags & 4) || body.compare(0, 6, "\x05READY") != 0) {
            return false;
        }
        for (size_t i = 6; i < body.size();) {
            size_t name_size = static_cast<unsigned char>(body[i]);
            if (i + 1 + name_size + 4 > body.size()) return false;
            std::string name = body.substr(i + 1, name_size);
            i += 1 + name_size;
            uint32_t value_size = 0;
            for (int k = 0; k < 4; k++) {
                value_size = value_size << 8 | static_cast<unsigned char>(body[i + k]);
            }
            i += 4;
            if (i + value_size > body.size()) return false;
            for (char &c : name) c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
            if (name == "identity") identity = body.substr(i, value_size);
            i += value_size;
        }
        return true;
    }
};

namespace {

bool send_message(Peer &peer, const Message &m, const std::string &key)
{
    return peer.send(encode(m, key));
}

} // namespace

// The client

Client::Client(const std::string &connection_file)
{
    start_sockets();
    Connection c = read_connection(connection_file);
    key = c.key;
    session = random_hex();
    iopub = std::make_unique<Peer>(connect_to(c.ip, c.ports[jupyter::iopub]), "SUB");
    // Subscribe to every message (in ZMTP 3.0, a message starting with 1).
    iopub->send({std::string(1, '\x01')});
    shell = std::make_unique<Peer>(connect_to(c.ip, c.ports[jupyter::shell]), "DEALER", session);
    stdin_ = std::make_unique<Peer>(connect_to(c.ip, c.ports[stdin_channel]), "DEALER", session);
    // As jupyter_client's wait_for_ready(): send kernel_info_requests until
    // IOPub delivers, as a kernel may start publishing to a new subscriber a
    // little later.
    for (int attempt = 0; attempt < 50; attempt++) {
        Message request = new_message("kernel_info_request", Object{}, Object{}, session);
        if (!send_message(*shell, request, key)) break;
        auto deadline = std::chrono::steady_clock::now() + std::chrono::milliseconds(200);
        while (std::chrono::steady_clock::now() < deadline) {
            std::vector<pollfd> fds{{shell->s, POLLIN, 0}, {iopub->s, POLLIN, 0}};
            if (poll_sockets(fds, 50) <= 0) continue;
            std::vector<std::string> frames;
            Message m;
            if (fds[0].revents && !shell->receive(frames)) {
                throw std::runtime_error("the kernel closed the connection");
            }
            if (fds[1].revents) {
                if (!iopub->receive(frames)) throw std::runtime_error("the kernel closed the connection");
                if (decode(frames, key, m) && m.parent_id() == request.id()) return;
            }
        }
    }
    throw std::runtime_error("the kernel does not answer");
}

Client::~Client() = default;

namespace {

// Prints an IOPub message as a terminal shows it: plain text only. A cell of
// another client is shown with its code.
void show(const Message &m, bool other)
{
    const std::string type = m.type();
    const Object *data = m.content.getObject("data");
    if (type == "execute_input" && other) {
        std::istringstream lines(get_string(m.content, "code"));
        for (std::string line; std::getline(lines, line);) {
            std::cout << "[other client] " << line << "\n";
        }
        std::cout << std::flush;
    } else if (type == "stream") {
        (get_string(m.content, "name") == "stderr" ? std::cerr : std::cout)
            << get_string(m.content, "text") << std::flush;
    } else if (type == "execute_result" && data) {
        std::cout << get_string(*data, "text/plain") << std::endl;
    } else if (type == "display_data" && data) {
        if (data->getString("text/plain")) {
            std::cout << get_string(*data, "text/plain") << std::endl;
        } else {
            for (const auto &item : *data) {
                std::cout << "[" << llvm::StringRef(item.first).str() << " output]" << std::endl;
                break;
            }
        }
    } else if (type == "error") {
        if (const Array *traceback = m.content.getArray("traceback")) {
            for (const auto &line : *traceback) {
                if (auto text = line.getAsString()) std::cerr << text->str() << "\n";
            }
            std::cerr << std::flush;
        }
    }
}

} // namespace

bool Client::execute(const std::string &code, bool verbose)
{
    Object content{{"code", kernel::utf8(code)}, {"silent", false},
        {"store_history", true}, {"user_expressions", Object{}},
        {"allow_stdin", true}, {"stop_on_error", true}};
    if (verbose) content["lfortran_verbose"] = true;
    Message request = new_message("execute_request", Object{}, std::move(content), session);
    if (!send_message(*shell, request, key)) return false;
    bool replied = false, idle = false;
    while (!replied || !idle) {
        std::vector<pollfd> fds{{iopub->s, POLLIN, 0}, {shell->s, POLLIN, 0},
            {stdin_->s, POLLIN, 0}};
        if (poll_sockets(fds, -1) < 0) {
            if (interrupted()) continue;
            return false;
        }
        Peer *peers[3] = {iopub.get(), shell.get(), stdin_.get()};
        for (int i = 0; i < 3; i++) {
            if (!fds[i].revents) continue;
            std::vector<std::string> frames;
            if (!peers[i]->receive(frames)) return false;
            Message m;
            if (!decode(frames, key, m)) continue;
            const std::string type = m.type();
            if (i == 2 && type == "input_request") {
                // A line of the terminal, ended by Enter (CR or LF); Ctrl-D
                // on an empty line is end of file, which Jupyter clients
                // send as "\x04".
                std::cout << get_string(m.content, "prompt") << std::flush;
                std::string record;
                int c;
                while ((c = getchar()) != EOF && c != '\n' && c != '\r' && c != 4) {
                    record += static_cast<char>(c);
                }
                if ((c == EOF || c == 4) && record.empty()) record = "\x04";
                clearerr(stdin);
                Message input = new_message("input_reply", m.header,
                    Object{{"value", kernel::utf8(record)}}, session);
                if (!send_message(*stdin_, input, key)) return false;
                continue;
            }
            if (m.parent_id() != request.id()) {
                // Another client's cell, which ran before this one
                if (i == 0 && get_string(m.parent, "session") != session) show(m, true);
                continue;
            }
            if (i == 1) {
                if (type == "execute_reply") replied = true;
            } else if (type == "status") {
                if (get_string(m.content, "execution_state") == "idle") idle = true;
            } else {
                show(m, false);
            }
        }
    }
    return true;
}

void Client::show_others()
{
    for (;;) {
        std::vector<pollfd> fds{{iopub->s, POLLIN, 0}};
        if (poll_sockets(fds, 0) <= 0) return;
        std::vector<std::string> frames;
        Message m;
        if (!iopub->receive(frames)) return;
        if (decode(frames, key, m) && get_string(m.parent, "session") != session) {
            show(m, true);
        }
    }
}

void Client::shutdown()
{
    Message request = new_message("shutdown_request", Object{},
        Object{{"restart", false}}, session);
    if (!send_message(*shell, request, key)) return;
    // The reply, or the kernel closing the connection
    std::vector<pollfd> fds{{shell->s, POLLIN, 0}};
    std::vector<std::string> frames;
    if (poll_sockets(fds, 5000) > 0) shell->receive(frames);
}

std::unique_ptr<Client> Client::start(const std::string &executable,
    const std::vector<std::string> &arguments, bool managed, std::string &connection_file)
{
    start_sockets();
    // Free ports, which the kernel then listens on, as jupyter_client does
    Connection c;
    c.key = random_hex();
    for (int &port : c.ports) {
        port = 0;
        close_socket(listen_on(c.ip, port));
    }
    std::error_code error;
    std::filesystem::path directory = std::filesystem::temp_directory_path(error);
    if (error) directory = ".";
    connection_file = (directory / ("lfortran-" + random_hex() + ".json")).string();
    write_connection(connection_file, c);

    std::vector<std::string> args{executable};
    args.insert(args.end(), arguments.begin(), arguments.end());
    args.insert(args.end(), {"kernel", "-f", connection_file});
    if (managed) args.push_back("--managed");
#ifdef _WIN32
    std::vector<std::string> quoted;
    for (const std::string &arg : args) {
        quoted.push_back(arg.find_first_of(" \t\"") == std::string::npos ? arg
            : "\"" + arg + "\"");
    }
    std::vector<const char *> argv;
    for (const std::string &arg : quoted) argv.push_back(arg.c_str());
    argv.push_back(nullptr);
    intptr_t child = _spawnv(_P_NOWAIT, executable.c_str(), argv.data());
    if (child == -1) {
        std::remove(connection_file.c_str());
        throw std::runtime_error("cannot start the kernel " + executable);
    }
    auto exited = [&]() {
        return WaitForSingleObject(reinterpret_cast<HANDLE>(child), 0) == WAIT_OBJECT_0;
    };
#else
    std::vector<char *> argv;
    for (std::string &arg : args) argv.push_back(&arg[0]);
    argv.push_back(nullptr);
    // The kernel does not use the terminal: it may outlive the prompt, whose
    // output must then end. In its own process group, it does not get the
    // prompt's Ctrl-C.
    posix_spawn_file_actions_t actions;
    posix_spawn_file_actions_init(&actions);
    posix_spawn_file_actions_addopen(&actions, 0, "/dev/null", O_RDONLY, 0);
    posix_spawn_file_actions_addopen(&actions, 1, "/dev/null", O_WRONLY, 0);
    posix_spawn_file_actions_addopen(&actions, 2, "/dev/null", O_WRONLY, 0);
    posix_spawnattr_t attributes;
    posix_spawnattr_init(&attributes);
    posix_spawnattr_setflags(&attributes, POSIX_SPAWN_SETPGROUP);
    posix_spawnattr_setpgroup(&attributes, 0);
    pid_t child;
    int failed = posix_spawn(&child, executable.c_str(), &actions, &attributes,
        argv.data(), environ);
    posix_spawn_file_actions_destroy(&actions);
    posix_spawnattr_destroy(&attributes);
    if (failed != 0) {
        std::remove(connection_file.c_str());
        throw std::runtime_error("cannot start the kernel " + executable + ": "
            + std::strerror(failed));
    }
    auto exited = [&]() {
        int status;
        return waitpid(child, &status, WNOHANG) == child;
    };
#endif
    // Connect once the kernel listens.
    auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(60);
    for (;;) {
        try {
            return std::make_unique<Client>(connection_file);
        } catch (const std::runtime_error &) {
            if (exited() || std::chrono::steady_clock::now() > deadline) {
                std::remove(connection_file.c_str());
                throw std::runtime_error("the kernel did not start");
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(20));
        }
    }
}

#ifndef HAVE_LFORTRAN_XEUS

namespace {

bool get_bool(const Object &object, llvm::StringRef key, bool otherwise)
{
    if (auto value = object.getBoolean(key)) return *value;
    return otherwise;
}

volatile std::sig_atomic_t stop_requested = 0;

extern "C" void request_stop(int)
{
    stop_requested = 1;
}

const char *const socket_types[5] = {"ROUTER", "ROUTER", "ROUTER", "PUB", "REP"};

// Answers heartbeats on its own thread until `stop` is set.
void answer_heartbeats(socket_t listener, const std::atomic<bool> &stop)
{
    std::vector<std::unique_ptr<Peer>> peers;
    while (!stop) {
        std::vector<pollfd> fds{{listener, POLLIN, 0}};
        for (auto &peer : peers) fds.push_back({peer->s, POLLIN, 0});
        if (poll_sockets(fds, 250) <= 0) continue;
        for (size_t i = 1; i < fds.size(); i++) {
            std::vector<std::string> frames;
            if (fds[i].revents && peers[i - 1]->receive(frames)) peers[i - 1]->send(frames);
        }
        for (size_t i = peers.size(); i-- > 0;) {
            if (peers[i]->closed) peers.erase(peers.begin() + i);
        }
        if (fds[0].revents) {
            socket_t s = accept(listener, nullptr, nullptr);
            try {
                if (s != no_socket) peers.push_back(std::make_unique<Peer>(s, "REP"));
            } catch (const std::runtime_error &) {
            }
        }
    }
}

class Kernel;

// The kernel whose IOPub output is sent when the process exits.
Kernel *exiting_kernel = nullptr;

class Kernel {
    FortranEvaluator &e;
    std::string key, session = random_hex();
    socket_t *listeners;
    bool managed;
    std::vector<std::unique_ptr<Peer>> peers[4];
    int execution_count = 0;
    bool stopping = false;

public:
    Kernel(FortranEvaluator &e, const std::string &key, socket_t *listeners, bool managed)
        : e(e), key(key), listeners(listeners), managed(managed) {}

    void run()
    {
        // A cell that ends the process (STOP, a fatal error) has its output
        // passed on as the process exits; this sends it to the subscribers.
        static const int registered = std::atexit([]() {
            if (exiting_kernel) exiting_kernel->flush_subscribers(1000);
        });
        (void)registered;
        exiting_kernel = this;
        bool had_client = false;
        auto started = std::chrono::steady_clock::now();
        while (!stopping && !stop_requested) {
            std::vector<pollfd> fds;
            std::vector<std::pair<int, Peer *>> owners;
            for (int c = 0; c < 4; c++) {
                fds.push_back({listeners[c], POLLIN, 0});
                owners.push_back({c, nullptr});
                for (auto &peer : peers[c]) {
                    short events = POLLIN;
                    if (peer->pending()) events |= POLLOUT;
                    fds.push_back({peer->s, events, 0});
                    owners.push_back({c, peer.get()});
                }
            }
            if (poll_sockets(fds, 250) > 0) {
                for (size_t i = 0; i < fds.size() && !stopping; i++) {
                    if (!fds[i].revents) continue;
                    int c = owners[i].first;
                    Peer *peer = owners[i].second;
                    if (!peer) {
                        accept_peer(c);
                        continue;
                    }
                    if (peer->closed) continue;
                    if (c == iopub) {
                        if (fds[i].revents & POLLOUT) peer->flush();
                        if (fds[i].revents & ~POLLOUT) peer->discard_input();
                        continue;
                    }
                    // Replies on stdin come only while a cell waits for one.
                    std::vector<std::string> frames;
                    Message request;
                    if (peer->receive(frames) && (c == shell || c == control)
                            && decode(frames, key, request)) {
                        handle(*peer, request);
                    }
                }
            }
            remove_closed();
            // A managed kernel stops when its last client disconnects, or if
            // none connects within a minute.
            if (managed) {
                if (!peers[shell].empty()) had_client = true;
                if (had_client ? peers[shell].empty()
                        : std::chrono::steady_clock::now() - started > std::chrono::minutes(1)) {
                    stopping = true;
                }
            }
        }
        flush_subscribers(1000);
        exiting_kernel = nullptr;
    }

    // Sends the subscribers' outboxes, waiting at most `timeout` ms.
    void flush_subscribers(int timeout)
    {
        auto deadline = std::chrono::steady_clock::now() + std::chrono::milliseconds(timeout);
        while (std::chrono::steady_clock::now() < deadline) {
            std::vector<pollfd> fds;
            for (auto &peer : peers[iopub]) {
                peer->flush();
                if (peer->pending()) fds.push_back({peer->s, POLLOUT, 0});
            }
            if (fds.empty()) return;
            poll_sockets(fds, 50);
        }
    }

private:
    void accept_peer(int c)
    {
        socket_t s = accept(listeners[c], nullptr, nullptr);
        if (s == no_socket) return;
        try {
            peers[c].push_back(std::make_unique<Peer>(s, socket_types[c]));
            if (c == iopub) peers[c].back()->make_subscriber();
        } catch (const std::runtime_error &) {
        }
    }

    // Peers are removed only here, between requests, as the requests being
    // handled refer to them.
    void remove_closed()
    {
        for (auto &list : peers) {
            for (size_t i = list.size(); i-- > 0;) {
                if (list[i]->closed) list.erase(list.begin() + i);
            }
        }
    }

    void publish(const std::string &type, const Object &parent, Object content)
    {
        Message m = new_message(type, parent, std::move(content), session);
        m.identities = {"kernel." + session + "." + type};
        std::vector<std::string> frames = encode(m, key);
        for (auto &peer : peers[iopub]) peer->post(frames);
    }

    void reply(Peer &peer, const Message &request, const std::string &type, Object content)
    {
        Message m = new_message(type, request.header, std::move(content), session);
        m.identities = request.identities;
        send_message(peer, m, key);
    }

    void handle(Peer &peer, const Message &request)
    {
        const std::string type = request.type();
        publish("status", request.header, Object{{"execution_state", "busy"}});
        if (type == "execute_request") {
            execute(peer, request);
        } else if (type == "kernel_info_request") {
            reply(peer, request, "kernel_info_reply", Object{{"status", "ok"},
                {"protocol_version", "5.3"},
                {"implementation", kernel::implementation},
                {"implementation_version", LFORTRAN_VERSION},
                {"language_info", Object{{"name", kernel::language},
                    {"version", kernel::language_version},
                    {"mimetype", kernel::mimetype},
                    {"file_extension", kernel::file_extension},
                    {"codemirror_mode", kernel::mimetype}}},
                {"banner", kernel::banner()}, {"help_links", Array{}}});
        } else if (type == "is_complete_request") {
            reply(peer, request, "is_complete_reply", Object{{"status", "complete"}});
        } else if (type == "complete_request") {
            int64_t cursor = 0;
            if (auto position = request.content.getInteger("cursor_pos")) cursor = *position;
            reply(peer, request, "complete_reply", Object{{"status", "ok"},
                {"matches", Array{}}, {"cursor_start", cursor}, {"cursor_end", cursor},
                {"metadata", Object{}}});
        } else if (type == "inspect_request") {
            reply(peer, request, "inspect_reply", Object{{"status", "ok"},
                {"found", false}, {"data", Object{}}, {"metadata", Object{}}});
        } else if (type == "history_request") {
            reply(peer, request, "history_reply", Object{{"status", "ok"},
                {"history", Array{}}});
        } else if (type == "comm_info_request") {
            reply(peer, request, "comm_info_reply", Object{{"status", "ok"},
                {"comms", Object{}}});
        } else if (type == "interrupt_request") {
            // A cell cannot be interrupted yet; between cells there is
            // nothing to interrupt.
            reply(peer, request, "interrupt_reply", Object{{"status", "ok"}});
        } else if (type == "shutdown_request") {
            bool restart = get_bool(request.content, "restart", false);
            reply(peer, request, "shutdown_reply", Object{{"status", "ok"}, {"restart", restart}});
            publish("shutdown_reply", request.header, Object{{"status", "ok"}, {"restart", restart}});
            stopping = true;
        }
        publish("status", request.header, Object{{"execution_state", "idle"}});
    }

    void execute(Peer &peer, const Message &request)
    {
        const Object &content = request.content;
        const std::string code = get_string(content, "code");
        const bool silent = get_bool(content, "silent", false);
        const bool allow_stdin = get_bool(content, "allow_stdin", true);
        // An LFortran extension: the cell's AST, ASR and LLVM IR (`lfortran -v`)
        const bool verbose = get_bool(content, "lfortran_verbose", false);
        if (!silent && get_bool(content, "store_history", true)) execution_count++;
        if (!silent) {
            publish("execute_input", request.header,
                Object{{"code", code}, {"execution_count", execution_count}});
        }
        FortranEvaluator::CellIO io;
        io.display = [&](const std::string &mime, const std::string &data) {
            if (!silent) publish("display_data", request.header,
                Object{{"data", Object{{kernel::utf8(mime), kernel::utf8(data)}}},
                    {"metadata", Object{}}, {"transient", Object{}}});
        };
        io.clear_output = [&]() {
            if (!silent) publish("clear_output", request.header, Object{{"wait", false}});
        };
        const std::string identity = peer.identity;
        io.input = [&](std::string &record) {
            return allow_stdin ? read_input(identity, request.header, record) : 1;
        };
        FortranEvaluator::CellResult cell = kernel::execute(e, code, io,
            [&](const std::string &name, const std::string &text) {
                if (!silent) publish("stream", request.header,
                    Object{{"name", name}, {"text", text}});
            },
            [&](const std::string &text) {
                if (!silent) publish("execute_result", request.header,
                    Object{{"execution_count", execution_count},
                        {"data", Object{{"text/plain", text}}}, {"metadata", Object{}}});
            }, verbose);
        Object result{{"status", cell.ok ? "ok" : "error"}, {"execution_count", execution_count}};
        if (cell.ok) {
            result["user_expressions"] = Object{};
            result["payload"] = Array{};
        } else {
            result["ename"] = kernel::utf8(cell.error_name);
            result["evalue"] = kernel::utf8(cell.error_value);
            Array traceback;
            std::istringstream lines(cell.error_traceback);
            for (std::string line; std::getline(lines, line);) {
                traceback.push_back(kernel::utf8(line));
            }
            result["traceback"] = std::move(traceback);
        }
        reply(peer, request, "execute_reply", std::move(result));
    }

    Peer *stdin_peer(const std::string &identity)
    {
        for (auto &peer : peers[stdin_channel]) {
            if (!peer->closed && peer->identity == identity) return peer.get();
        }
        return nullptr;
    }

    // A READ asks the client that ran the cell, through its stdin socket of
    // the same identity, and waits for its answer; other requests wait in
    // their sockets. Returns as the CellIO input hook does.
    int read_input(const std::string &identity, const Object &parent, std::string &record)
    {
        Peer *target = identity.empty() ? nullptr : stdin_peer(identity);
        if (!target) return 1;
        Message request = new_message("input_request", parent,
            Object{{"prompt", ""}, {"password", false}}, session);
        if (!send_message(*target, request, key)) return 1;
        while (!stop_requested && stdin_peer(identity)) {
            std::vector<pollfd> fds{{listeners[stdin_channel], POLLIN, 0}};
            std::vector<Peer *> owners{nullptr};
            for (auto &peer : peers[stdin_channel]) {
                if (peer->closed) continue;
                fds.push_back({peer->s, POLLIN, 0});
                owners.push_back(peer.get());
            }
            for (auto &peer : peers[iopub]) peer->flush();
            if (poll_sockets(fds, 250) <= 0) continue;
            for (size_t i = 0; i < fds.size(); i++) {
                if (!fds[i].revents) continue;
                if (!owners[i]) {
                    accept_peer(stdin_channel);
                    continue;
                }
                std::vector<std::string> frames;
                Message m;
                if (!owners[i]->receive(frames) || owners[i]->identity != identity
                        || !decode(frames, key, m) || m.type() != "input_reply") {
                    continue;
                }
                record = get_string(m.content, "value");
                return record == "\x04" ? -1 : 0;
            }
        }
        return 1;
    }
};

} // namespace

#endif // HAVE_LFORTRAN_XEUS

} // namespace LCompilers::LFortran::jupyter

namespace LCompilers::LFortran {

#ifndef HAVE_LFORTRAN_XEUS
int run_kernel(const std::string &connection_filename, CompilerOptions &options,
    bool managed)
{
    using namespace jupyter;
    start_sockets();
    Connection c;
    const bool existing = std::ifstream(connection_filename).good();
    socket_t listeners[5];
    int opened = 0;
    try {
        if (existing) {
            c = read_connection(connection_filename);
        } else {
            c.key = random_hex();
        }
        for (; opened < 5; opened++) listeners[opened] = listen_on(c.ip, c.ports[opened]);
        if (!existing) write_connection(connection_filename, c);
    } catch (const std::runtime_error &error) {
        for (int i = 0; i < opened; i++) close_socket(listeners[i]);
        std::cerr << "error: " << error.what() << std::endl;
        return 1;
    }
    std::signal(SIGINT, request_stop);
    std::signal(SIGTERM, request_stop);

    options.interactive = true;
    if (options.po.runtime_library_dir.empty()) {
        options.po.runtime_library_dir = get_runtime_library_dir();
    }
    FortranEvaluator e(options);
    if (!managed) {
        std::cout << "LFortran Jupyter kernel. Connect with `lfortran --connect "
            << connection_filename << "` or `jupyter console --existing "
            << connection_filename << "`." << std::endl;
    }

    std::atomic<bool> stop{false};
    std::thread beats(answer_heartbeats, listeners[jupyter::heartbeat], std::cref(stop));
    Kernel(e, c.key, listeners, managed).run();
    stop = true;
    beats.join();
    for (socket_t listener : listeners) close_socket(listener);
    if (!existing || managed) std::remove(connection_filename.c_str());
    return 0;
}
#endif

} // namespace LCompilers::LFortran

#else // __EMSCRIPTEN__

namespace LCompilers::LFortran::jupyter {

class Peer {};

Client::Client(const std::string &)
{
    throw std::runtime_error("Jupyter clients are not supported in this build");
}

Client::~Client() = default;

bool Client::execute(const std::string &)
{
    return false;
}

} // namespace LCompilers::LFortran::jupyter

#ifndef HAVE_LFORTRAN_XEUS
namespace LCompilers::LFortran {
int run_kernel(const std::string &, CompilerOptions &, bool)
{
    std::cerr << "error: the Jupyter kernel is not supported in this build" << std::endl;
    return 1;
}
}
#endif

#endif // __EMSCRIPTEN__

// The parts of the Jupyter messaging protocol that LFortran needs, with no
// third-party library: a kernel, which `lfortran kernel` runs when not built
// with Xeus, a client for the prompt, and the stdin channel of the built-in
// kernel. The prompt starts that kernel, or the Xeus one in a build with
// Xeus, and ends it when the prompt exits. Any Jupyter client can use the
// kernel, and the client can use any Jupyter kernel.
//
// Transport: ZeroMQ's ZMTP 3.0 with the NULL mechanism over TCP (the "tcp"
// transport of a connection file), implementing only what a Jupyter kernel
// and client do with it: ROUTER sockets for shell, control and stdin, a PUB
// socket for IOPub (it sends every message to every subscriber) and a REP
// socket for the heartbeat; DEALER and SUB sockets on the client side.
// Messages are signed with HMAC-SHA256 and encoded with LLVM's JSON library.
//
// A cell worker holds other requests back (they wait in their sockets).
// The calling thread streams output and services the cell's hooks.
// Two threads keep connections that must not
// wait for a cell: the heartbeat, which clients expect to answer while a cell
// runs, and the stdin channel. A third watches the clients while a cell runs.
// No connection holds the others back: the kernel's connections do not block.

#include <lfortran/jupyter.h>
#include <libasr/config.h>

#include <iostream>
#include <stdexcept>

#ifndef __EMSCRIPTEN__

#include <lfortran/fortran_kernel.h>
#include <atomic>
#include <cctype>
#include <cerrno>
#include <chrono>
#include <csignal>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <condition_variable>
#include <cstring>
#include <ctime>
#include <deque>
#include <filesystem>
#include <fstream>
#include <initializer_list>
#include <limits>
#include <mutex>
#include <optional>
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
#include <tlhelp32.h>
#else
#include <arpa/inet.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <poll.h>
#include <spawn.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>
#ifdef __APPLE__
#include <sys/sysctl.h>
#endif

extern char **environ;
#endif

namespace LCompilers::LFortran::jupyter {

namespace {

using llvm::json::Array;
using llvm::json::Object;

// Sockets

// A kernel's connection whose ZeroMQ handshake has not finished after the
// kernel has listened that long is closed.
constexpr auto handshake_timeout = std::chrono::seconds(10);

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
    if (bind(s, reinterpret_cast<sockaddr *>(&a), sizeof(a)) != 0
            || listen(s, SOMAXCONN) != 0) {
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

// Whether `signature` is that of `parts`, compared in a time that does not
// tell how much of it is right.
bool signed_by(const std::string &key, const std::string *parts, const std::string &signature)
{
    const std::string expected = sign(key, parts);
    if (signature.size() != expected.size()) return false;
    unsigned char difference = 0;
    for (size_t i = 0; i < expected.size(); i++) {
        difference |= static_cast<unsigned char>(expected[i] ^ signature[i]);
    }
    return difference == 0;
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

// The four JSON parts of a message, which its signature covers
void message_parts(const Message &m, std::string parts[4])
{
    parts[0] = dump(Object(m.header));
    parts[1] = dump(Object(m.parent));
    parts[2] = dump(Object(m.metadata));
    parts[3] = dump(Object(m.content));
}

std::vector<std::string> encode(const Message &m, const std::string &key)
{
    std::vector<std::string> frames = m.identities;
    std::string parts[4];
    message_parts(m, parts);
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
    if (!key.empty() && !signed_by(key, &frames[d + 2], frames[d + 1])) return false;
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
// NULL mechanism over a TCP socket. The kernel's connections do not block:
// each keeps what arrives until it makes a whole frame, and what it sends in
// an outbox, so that a connection that stalls, in its handshake or in a
// message, or that does not read, holds nothing else back. The client's
// connections block.
class Peer {
public:
    socket_t s;
    // The peer's Identity: Jupyter clients give their shell and stdin
    // sockets the same one, so that a READ asks the client that ran the cell.
    std::string identity;
    bool closed = false;

    // What a kernel does with a message for a peer that does not read.
    enum class Delivery {
        // It keeps it, as losing a reply or a status would leave the peer
        // waiting for it, but disconnects a peer that leaves 16 MiB of them.
        reliable,
        // It drops it once 64 MiB wait, as ZeroMQ does at its high-water
        // mark: the output of cells, for a subscriber such as a prompt
        // waiting for keys.
        droppable
    };

    // Starts the handshake as a socket of `type`. A blocking peer finishes it
    // here, waiting while the kernel runs a cell, else closes `fd` and throws.
    Peer(socket_t fd, const std::string &type, bool blocking,
        const std::string &own_identity = "")
        : s(fd), blocking(blocking), type(type), own_identity(own_identity)
    {
        set_no_delay(s);
        if (!blocking) set_nonblocking(s);
        std::string greeting(64, '\0');
        greeting[0] = '\xff';
        greeting[9] = '\x7f';
        greeting[10] = 3;
        greeting.replace(12, 4, "NULL");
        write(std::move(greeting), Delivery::reliable);
        while (blocking && !closed && !ready()) read();
        if (blocking && !ready()) {
            close_socket(s);
            throw std::runtime_error("the ZeroMQ handshake failed");
        }
    }
    ~Peer() { close_socket(s); }
    Peer(const Peer &) = delete;
    Peer &operator=(const Peer &) = delete;

    bool ready() const { return state == State::open; }

    // Whether the handshake should have finished by now
    bool expired() const
    {
        return !ready() && std::chrono::steady_clock::now() > deadline;
    }

    // Gives the handshake `time` more, which the kernel spent not listening.
    void postpone(std::chrono::steady_clock::duration time) { deadline += time; }

    // Sends a message: at once if the peer blocks, else through the outbox.
    // False once the connection is closed.
    bool send(const std::vector<std::string> &frames,
        Delivery delivery = Delivery::reliable)
    {
        std::string data;
        for (size_t i = 0; i < frames.size(); i++) {
            data += frame(i + 1 < frames.size() ? 1 : 0, frames[i]);
        }
        write(std::move(data), delivery);
        return !closed;
    }

    bool pending() const { return !closed && !outbox.empty(); }

    // Sends what the socket takes now.
    void flush(size_t budget = SIZE_MAX)
    {
        while (budget && pending()) {
            Outgoing &out = outbox.front();
            const size_t remaining = out.data.size() - sent;
            auto n = ::send(s, out.data.data() + sent,
                chunk(remaining < budget ? remaining : budget), 0);
            if (n < 0 && interrupted()) continue;
            if (n < 0 && would_block()) return;
            if (n <= 0) {
                closed = true;
                return;
            }
            sent += static_cast<size_t>(n);
            budget -= static_cast<size_t>(n);
            if (sent == out.data.size()) {
                queued -= sent;
                if (out.reliable) queued_reliable -= sent;
                outbox.pop_front();
                sent = 0;
            }
        }
    }

    // Reads what has arrived; a blocking peer waits for something.
    void read()
    {
        char buffer[65536];
        while (!closed) {
            auto n = ::recv(s, buffer, chunk(sizeof(buffer)), 0);
            if (n < 0 && interrupted()) continue;
            if (n < 0 && would_block()) break;
            if (n <= 0) {
                closed = true;
                break;
            }
            input.append(buffer, static_cast<size_t>(n));
            if (blocking) break;
        }
        parse();
    }

    // Takes the next message read, if any.
    bool next(std::vector<std::string> &frames)
    {
        if (received.empty()) return false;
        frames = std::move(received.front());
        received.pop_front();
        return true;
    }

private:
    enum class State { greeting, ready_command, open };
    struct Outgoing {
        std::string data;
        bool reliable;
    };
    const bool blocking;
    const std::string type, own_identity;
    std::chrono::steady_clock::time_point deadline
        = std::chrono::steady_clock::now() + handshake_timeout;
    State state = State::greeting;
    // What was read, of which the part from `parsed` on is not a whole frame
    std::string input;
    size_t parsed = 0;
    // The frames of the message being read, and the messages read
    std::vector<std::string> message;
    std::deque<std::vector<std::string>> received;
    std::deque<Outgoing> outbox;
    // The bytes in the outbox, those of reliable messages, and those of its
    // first message already sent
    size_t queued = 0, queued_reliable = 0, sent = 0;

    void write(std::string data, Delivery delivery)
    {
        if (closed) return;
        if (blocking) {
            if (!send_all(s, data.data(), data.size())) closed = true;
            return;
        }
        const bool reliable = delivery == Delivery::reliable;
        if (reliable && queued_reliable > (size_t(16) << 20)) {
            closed = true;
            return;
        }
        if (!reliable && queued > (size_t(64) << 20)) return;
        queued += data.size();
        if (reliable) queued_reliable += data.size();
        outbox.push_back({std::move(data), reliable});
        flush();
    }

    // Takes the greeting, the READY command and the messages out of `input`.
    void parse()
    {
        for (;;) {
            const size_t available = input.size() - parsed;
            const char *p = input.data() + parsed;
            if (state == State::greeting) {
                if (available > 0 && p[0] != '\xff') return fail();
                if (available < 64) break;
                if (p[9] != '\x7f' || static_cast<unsigned char>(p[10]) < 3
                        || std::string(p + 12, 5) != std::string("NULL\0", 5)) {
                    return fail();
                }
                parsed += 64;
                state = State::ready_command;
                std::string ready = "\x05READY" + property("Socket-Type", type);
                if (!own_identity.empty()) ready += property("Identity", own_identity);
                write(frame(4, ready), Delivery::reliable);
                continue;
            }
            // flags: 1 more frames follow, 2 long size, 4 command
            if (available < 2) break;
            const unsigned char flags = static_cast<unsigned char>(p[0]);
            const size_t header = (flags & 2) ? 9 : 2;
            if (available < header) break;
            uint64_t length = 0;
            for (size_t i = 1; i < header; i++) {
                length = length << 8 | static_cast<unsigned char>(p[i]);
            }
            if (length > available - header) break;
            std::string body(p + header, static_cast<size_t>(length));
            parsed += header + static_cast<size_t>(length);
            if (state == State::ready_command) {
                if (!(flags & 4) || !read_ready(body)) return fail();
                state = State::open;
            } else if (!(flags & 4)) {
                // Other commands, such as PING, are ignored.
                message.push_back(std::move(body));
                if (!(flags & 1)) {
                    received.push_back(std::move(message));
                    message.clear();
                }
            }
        }
        if (parsed == input.size()) {
            input.clear();
            parsed = 0;
        } else if (parsed > (size_t(1) << 20)) {
            input.erase(0, parsed);
            parsed = 0;
        }
    }

    void fail() { closed = true; }

    // The peer's READY command: its Identity, if it has one
    bool read_ready(const std::string &body)
    {
        if (body.compare(0, 6, "\x05READY") != 0) return false;
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

    static std::string property(const std::string &name, const std::string &value)
    {
        std::string out(1, char(name.size()));
        out += name;
        for (int i = 3; i >= 0; i--) out += char(uint32_t(value.size()) >> (8 * i));
        return out + value;
    }
};

namespace {

bool send_message(Peer &peer, const Message &m, const std::string &key)
{
    return peer.send(encode(m, key));
}

// The kernel's side of a channel: a listening socket and the connections it
// accepts, as ZeroMQ sockets of `type`.
struct Endpoint {
    socket_t listener;
    std::string type;
    std::vector<std::unique_ptr<Peer>> peers;
    // When the kernel last listened
    std::chrono::steady_clock::time_point watched = std::chrono::steady_clock::now();

    Endpoint(socket_t listener, const std::string &type) : listener(listener), type(type)
    {
        set_nonblocking(listener);
    }
    ~Endpoint() { close_socket(listener); }
    Endpoint(const Endpoint &) = delete;
    Endpoint &operator=(const Endpoint &) = delete;

    // Adds the listener and the connections to what `fds` polls, and whose
    // they are to `owners`. Closes the connections whose handshake has not
    // finished in time, not counting the time the kernel spent running a
    // cell, in which it does not listen.
    void watch(std::vector<pollfd> &fds, std::vector<std::pair<Endpoint *, Peer *>> &owners)
    {
        const auto now = std::chrono::steady_clock::now();
        if (now - watched > std::chrono::seconds(1)) {
            for (auto &peer : peers) peer->postpone(now - watched);
        }
        watched = now;
        fds.push_back({listener, POLLIN, 0});
        owners.push_back({this, nullptr});
        for (auto &peer : peers) {
            if (peer->expired()) peer->closed = true;
            if (peer->closed) continue;
            short events = POLLIN;
            if (peer->pending()) events |= POLLOUT;
            fds.push_back({peer->s, events, 0});
            owners.push_back({this, peer.get()});
        }
    }

    // Accepts a connection, or sends and reads what `fd` (of `peer`) can.
    void progress(const pollfd &fd, Peer *peer)
    {
        if (!peer) {
            socket_t s = accept(listener, nullptr, nullptr);
            if (s != no_socket) peers.push_back(std::make_unique<Peer>(s, type, false));
            return;
        }
        if (fd.revents & POLLOUT) peer->flush();
        if (fd.revents & ~POLLOUT) peer->read();
    }

    // Removes the closed connections; only between requests, which refer to
    // them.
    void prune()
    {
        for (size_t i = peers.size(); i-- > 0;) {
            if (peers[i]->closed) peers.erase(peers.begin() + i);
        }
    }

    Peer *find(const std::string &identity) const
    {
        for (auto &peer : peers) {
            if (!peer->closed && peer->ready() && peer->identity == identity) return peer.get();
        }
        return nullptr;
    }
};

using Owners = std::vector<std::pair<Endpoint *, Peer *>>;

// Accepts, sends and reads, waiting at most `timeout` ms.
void progress(std::initializer_list<Endpoint *> endpoints, int timeout)
{
    std::vector<pollfd> fds;
    Owners owners;
    for (Endpoint *endpoint : endpoints) endpoint->watch(fds, owners);
    if (poll_sockets(fds, timeout) <= 0) return;
    for (size_t i = 0; i < fds.size(); i++) {
        if (fds[i].revents) owners[i].first->progress(fds[i], owners[i].second);
    }
}

} // namespace

// The stdin channel has its own thread: ZeroMQ clients drop a connection
// whose handshake has not finished in 30 seconds, and the client that a READ
// asks may disconnect while the cell waits.
struct InputChannel::Impl {
    enum Status { none, waiting, answered, gone };
    std::string key;
    Endpoint endpoint;
    std::mutex mutex;
    std::condition_variable changed;
    std::atomic<bool> stop{false};
    // The input_request, for the client of `identity`, and its answer
    Status status = none;
    std::string identity, value, request_id;
    std::vector<std::string> request;
    Peer *target = nullptr;
    std::chrono::steady_clock::time_point deadline;
    std::thread thread;

    Impl(socket_t listener, const std::string &key) : key(key), endpoint(listener, "ROUTER") {}

    // Sends the request once its client has connected, or ends it if that
    // client has gone. With `mutex` locked.
    void update()
    {
        if (status != waiting) return;
        if (!target) {
            // The client's connection may not be accepted yet.
            target = endpoint.find(identity);
            if (target) {
                target->send(request);
            } else if (std::chrono::steady_clock::now() > deadline) {
                status = gone;
            }
        }
        if (target && target->closed) status = gone;
        if (status != waiting) {
            target = nullptr;
            changed.notify_all();
        }
    }

    void run()
    {
        while (!stop) {
            std::vector<pollfd> fds;
            Owners owners;
            {
                std::lock_guard<std::mutex> lock(mutex);
                endpoint.watch(fds, owners);
            }
            const int n = poll_sockets(fds, 100);
            std::lock_guard<std::mutex> lock(mutex);
            for (size_t i = 0; n > 0 && i < fds.size(); i++) {
                if (fds[i].revents) endpoint.progress(fds[i], owners[i].second);
            }
            // Replies come only to a request; anything else is ignored.
            for (auto &peer : endpoint.peers) {
                std::vector<std::string> frames;
                Message m;
                while (peer->next(frames)) {
                    if (status == waiting && peer.get() == target && decode(frames, key, m)
                            && m.type() == "input_reply"
                            && (m.parent_id().empty() || m.parent_id() == request_id)) {
                        value = get_string(m.content, "value");
                        status = answered;
                        target = nullptr;
                        changed.notify_all();
                    }
                }
            }
            update();
            endpoint.prune();
        }
    }
};

InputChannel::InputChannel(const std::string &ip, int &port, const std::string &key,
    bool resolve_host)
{
    start_sockets();
    std::string host = ip;
    if (resolve_host && !serves(host)) {
        addrinfo hints{}, *result = nullptr;
        hints.ai_family = AF_INET;
        hints.ai_socktype = SOCK_STREAM;
        if (getaddrinfo(host.c_str(), nullptr, &hints, &result) != 0)
            throw std::runtime_error("cannot resolve stdin address: " + host);
        char numeric[INET_ADDRSTRLEN];
        const auto *socket_address = reinterpret_cast<const sockaddr_in *>(result->ai_addr);
        const char *resolved = inet_ntop(AF_INET, &socket_address->sin_addr,
            numeric, sizeof(numeric));
        if (resolved) host = numeric;
        freeaddrinfo(result);
        if (!resolved) throw std::runtime_error("cannot resolve stdin address: " + ip);
    }
    impl = std::make_unique<Impl>(listen_on(host, port), key);
    impl->thread = std::thread([this]() { impl->run(); });
}

InputChannel::~InputChannel()
{
    impl->stop = true;
    impl->thread.join();
}

void InputChannel::ask(const std::string &identity, const std::string parts[4])
{
    std::lock_guard<std::mutex> lock(impl->mutex);
    impl->request = {"<IDS|MSG>", sign(impl->key, parts)};
    impl->request.insert(impl->request.end(), parts, parts + 4);
    impl->identity = identity;
    auto header = llvm::json::parse(parts[0]);
    impl->request_id = header && header->getAsObject()
        ? get_string(*header->getAsObject(), "msg_id") : "";
    if (!header) llvm::consumeError(header.takeError());
    impl->status = Impl::waiting;
    impl->target = nullptr;
    impl->deadline = std::chrono::steady_clock::now() + std::chrono::seconds(1);
    impl->update();
}

int InputChannel::wait(std::string &value, int timeout)
{
    std::unique_lock<std::mutex> lock(impl->mutex);
    impl->changed.wait_for(lock, std::chrono::milliseconds(timeout),
        [&]() { return impl->status != Impl::waiting; });
    const Impl::Status status = impl->status;
    if (status == Impl::waiting) return 0;
    impl->status = Impl::none;
    if (status != Impl::answered) return -1;
    value = std::move(impl->value);
    return 1;
}

void InputChannel::cancel()
{
    std::lock_guard<std::mutex> lock(impl->mutex);
    impl->status = Impl::none;
    impl->target = nullptr;
    impl->request.clear();
    impl->changed.notify_all();
}

bool InputChannel::request(const std::string &identity, const std::string parts[4],
    std::string &value)
{
    ask(identity, parts);
    int answer;
    while ((answer = wait(value, 1000)) == 0) {}
    return answer > 0;
}

bool InputChannel::serves(const std::string &ip)
{
    try {
        address(ip, 0);
        return true;
    } catch (const std::runtime_error &) {
        return false;
    }
}

// The client

namespace {

// Waits at most `timeout` ms (-1: no limit) for a message from one of
// `peers`, the client's connections to the kernel; returns its index, -1 if
// none comes in time, or -2 once the kernel has closed a connection.
template <size_t N>
int next_message(Peer *const (&peers)[N], const std::string &key, Message &m, int timeout)
{
    for (;;) {
        for (size_t i = 0; i < N; i++) {
            std::vector<std::string> frames;
            while (peers[i]->next(frames)) {
                if (decode(frames, key, m)) return i;
            }
        }
        for (Peer *peer : peers) {
            if (peer->closed) return -2;
        }
        std::vector<pollfd> fds;
        for (Peer *peer : peers) fds.push_back({peer->s, POLLIN, 0});
        const int n = poll_sockets(fds, timeout);
        if (n < 0 && interrupted()) continue;
        if (n < 0) return -2;
        if (n == 0) return -1;
        for (size_t i = 0; i < N; i++) {
            if (fds[i].revents) peers[i]->read();
        }
    }
}

} // namespace

Client::Client(const std::string &connection_file)
{
    start_sockets();
    Connection c = read_connection(connection_file);
    key = c.key;
    session = random_hex();
    iopub = std::make_unique<Peer>(connect_to(c.ip, c.ports[jupyter::iopub]), "SUB", true);
    // Subscribe to every message (in ZMTP 3.0, a message starting with 1).
    iopub->send({std::string(1, '\x01')});
    shell = std::make_unique<Peer>(connect_to(c.ip, c.ports[jupyter::shell]), "DEALER", true,
        session);
    stdin_ = std::make_unique<Peer>(connect_to(c.ip, c.ports[stdin_channel]), "DEALER", true,
        session);
    control = std::make_unique<Peer>(connect_to(c.ip, c.ports[jupyter::control]), "DEALER", true,
        session);
    Peer *const peers[3] = {iopub.get(), shell.get(), stdin_.get()};
    // As jupyter_client's wait_for_ready(): send kernel_info_requests until
    // IOPub delivers, as a kernel may start publishing to a new subscriber a
    // little later. A kernel that runs a cell replies after it.
    for (int attempt = 0; attempt < 50; attempt++) {
        Message request = new_message("kernel_info_request", Object{}, Object{}, session);
        if (!send_message(*shell, request, key)) break;
        auto deadline = std::chrono::steady_clock::now() + std::chrono::milliseconds(200);
        bool replied = false;
        while (!replied || std::chrono::steady_clock::now() < deadline) {
            Message m;
            const int i = next_message(peers, key, m, 50);
            if (i == -2) throw std::runtime_error("the kernel closed the connection");
            if (m.parent_id() != request.id()) continue;
            if (i == 0) return;
            if (i == 1) replied = true;
        }
    }
    throw std::runtime_error("the kernel does not answer");
}

namespace {

// The kernel this prompt started. The destructor ends it on the way out.
// Ctrl-C ends it too, and that handler does not run destructors, so it can
// use only a sig_atomic_t and this fixed buffer. A client that only joined
// leaves both empty.
#ifdef _WIN32
volatile PVOID owned_process = nullptr;
#else
volatile std::sig_atomic_t owned_pid = 0;
#endif
char owned_connection[4096] = {};
volatile std::sig_atomic_t interrupt_installed = 0;
#ifdef _WIN32
std::atomic<bool> executing_prompt{false}, prompt_interrupt{false};
#else
volatile std::sig_atomic_t executing_prompt = 0;
volatile std::sig_atomic_t prompt_interrupt = 0;
#endif
// The terminal as it was when the prompt started, which the Ctrl-C handler
// restores: Ctrl-C at `>>>` arrives while the line editor has it in raw mode.
volatile std::sig_atomic_t terminal_saved = 0;
#ifdef _WIN32
HANDLE terminal_input = nullptr;
DWORD terminal_mode = 0;
#else
struct termios terminal_state {};
#endif

// Removes the connection file this prompt created. Safe to call twice.
// On POSIX it only uses unlink, so the Ctrl-C handler may call it too.
void release_owned_connection()
{
    if (owned_connection[0] == '\0') return;
#ifdef _WIN32
    std::remove(owned_connection);
#else
    unlink(owned_connection);
#endif
    owned_connection[0] = '\0';
}

#ifdef _WIN32
HANDLE take_owned_process()
{
    return static_cast<HANDLE>(InterlockedExchangePointer(&owned_process, nullptr));
}

BOOL WINAPI interrupt_prompt(DWORD event)
{
    if (event != CTRL_C_EVENT && event != CTRL_BREAK_EVENT) return FALSE;
    if (executing_prompt) {
        prompt_interrupt = 1;
        return TRUE;
    }
    release_owned_connection();
    HANDLE handle = take_owned_process();
    if (handle) {
        TerminateProcess(handle, 1);
        WaitForSingleObject(handle, 2000);
        CloseHandle(handle);
    }
    if (terminal_saved) SetConsoleMode(terminal_input, terminal_mode);
    // The default handler then ends the prompt, as without this handler.
    return FALSE;
}
#else
extern "C" void interrupt_prompt(int)
{
    if (executing_prompt) {
        prompt_interrupt = 1;
        return;
    }
    release_owned_connection();
    const pid_t pid = static_cast<pid_t>(owned_pid);
    owned_pid = 0;
    if (pid > 0) {
        // The kernel is in another process group, so it has not seen this
        // Ctrl-C, and it may be inside a cell that does not return.
        kill(pid, SIGKILL);
        waitpid(pid, nullptr, 0);
    }
    // TCSANOW: the output may never drain, as when nothing reads it.
    if (terminal_saved) tcsetattr(STDIN_FILENO, TCSANOW, &terminal_state);
    // End by SIGINT, as without this handler, so that a shell knows that
    // Ctrl-C ended the prompt (and, say, stops a loop that runs it).
    signal(SIGINT, SIG_DFL);
    sigset_t interrupt;
    sigemptyset(&interrupt);
    sigaddset(&interrupt, SIGINT);
    sigprocmask(SIG_UNBLOCK, &interrupt, nullptr);
    raise(SIGINT);
    _Exit(130);
}
#endif

// Removes the connection file of the kernel this prompt started and ends the
// kernel. The file goes first, as this prompt may not live to remove it
// after the kernel has gone. `wait_ms` is how long a SIGTERM may take to
// finish, including the built-in kernel flushing output, before the process
// is killed. Safe to call twice: the first call takes the process.
void end_owned_kernel(int wait_ms)
{
#ifdef _WIN32
    release_owned_connection();
    HANDLE handle = take_owned_process();
    if (handle) {
        TerminateProcess(handle, 1);
        WaitForSingleObject(handle, static_cast<DWORD>(wait_ms));
        CloseHandle(handle);
    }
#else
    sigset_t blocked, previous;
    sigemptyset(&blocked);
    sigaddset(&blocked, SIGINT);
    sigprocmask(SIG_BLOCK, &blocked, &previous);
    release_owned_connection();
    const pid_t pid = static_cast<pid_t>(owned_pid);
    owned_pid = 0;
    if (pid > 0) {
        kill(pid, SIGTERM);
        const auto deadline = std::chrono::steady_clock::now()
            + std::chrono::milliseconds(wait_ms);
        for (;;) {
            int status = 0;
            const pid_t got = waitpid(pid, &status, WNOHANG);
            if (got == pid || got < 0) break;
            if (std::chrono::steady_clock::now() >= deadline) {
                kill(pid, SIGKILL);
                waitpid(pid, &status, 0);
                break;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
    }
    sigprocmask(SIG_SETMASK, &previous, nullptr);
#endif
}

// Records the kernel `process` (a pid, or a process handle on Windows) for
// the destructor and the Ctrl-C handler. `connection_file` fits in
// owned_connection.
void remember_owned_kernel(intptr_t process, const std::string &connection_file)
{
    std::memcpy(owned_connection, connection_file.c_str(), connection_file.size() + 1);
#ifdef _WIN32
    owned_process = reinterpret_cast<HANDLE>(process);
#else
    owned_pid = static_cast<std::sig_atomic_t>(process);
#endif
}

} // namespace

void handle_interrupt()
{
    if (interrupt_installed) return;
#ifdef _WIN32
    terminal_input = GetStdHandle(STD_INPUT_HANDLE);
    if (GetConsoleMode(terminal_input, &terminal_mode)) terminal_saved = 1;
    if (SetConsoleCtrlHandler(interrupt_prompt, TRUE)) interrupt_installed = 1;
#else
    struct sigaction current {};
    sigaction(SIGINT, nullptr, &current);
    // Ctrl-C is ignored, as for a background job of a shell without job
    // control. It must not end the prompt then.
    if (current.sa_handler == SIG_IGN) return;
    if (isatty(STDIN_FILENO) && tcgetattr(STDIN_FILENO, &terminal_state) == 0) {
        terminal_saved = 1;
    }
    struct sigaction action {};
    action.sa_handler = interrupt_prompt;
    sigemptyset(&action.sa_mask);
    // During execution the client loop must observe the interrupt marker.
    action.sa_flags = 0;
    if (sigaction(SIGINT, &action, nullptr) == 0) interrupt_installed = 1;
#endif
}

Client::~Client()
{
    // A client that only joined did not start a kernel.
    if (process == 0 && owned_file.empty()) return;
    // The built-in kernel notices SIGTERM within about 250 ms, then may
    // spend up to a second flushing what a cell printed. Wait long enough
    // that a normal exit is not cut off.
    end_owned_kernel(2000);
}

namespace {

void terminal_output(const std::string &text, bool error)
{
    (error ? std::cerr : std::cout) << text << std::flush;
}

// Shows an IOPub message as plain text. A cell of another client includes
// its code; the sink can be the terminal itself or its active line editor.
void show(const Message &m, bool other,
    const Client::DisplaySink &sink = terminal_output)
{
    const std::string type = m.type();
    const Object *data = m.content.getObject("data");
    if (type == "execute_input" && other) {
        std::istringstream lines(get_string(m.content, "code"));
        for (std::string line; std::getline(lines, line);) {
            sink("[other client] " + line + "\n", false);
        }
    } else if (type == "stream") {
        sink(get_string(m.content, "text"),
            get_string(m.content, "name") == "stderr");
    } else if (type == "execute_result" && data) {
        sink(get_string(*data, "text/plain") + "\n", false);
    } else if (type == "display_data" && data) {
        if (data->getString("text/plain")) {
            sink(get_string(*data, "text/plain") + "\n", false);
        } else {
            for (const auto &item : *data) {
                sink("[" + llvm::StringRef(item.first).str() + " output]\n", false);
                break;
            }
        }
    } else if (type == "error") {
        if (const Array *traceback = m.content.getArray("traceback")) {
            for (const auto &line : *traceback) {
                if (auto text = line.getAsString()) sink(text->str() + "\n", true);
            }
        }
    }
}

} // namespace

int Client::stdin_character()
{
#ifdef _WIN32
    HANDLE input = GetStdHandle(STD_INPUT_HANDLE);
    if (GetFileType(input) == FILE_TYPE_PIPE) {
        DWORD available = 0;
        if (!PeekNamedPipe(input, nullptr, 0, nullptr, &available, nullptr)) return EOF;
        if (!available) return -2;
    } else {
        DWORD mode = 0;
        if (GetConsoleMode(input, &mode)) {
            static std::string line;
            static size_t position = 0;
            if (position == line.size()) {
                DWORD count = 0;
                if (!GetNumberOfConsoleInputEvents(input, &count) || !count) return -2;
                std::vector<INPUT_RECORD> events(count);
                if (!PeekConsoleInput(input, events.data(), count, &count)) return EOF;
                bool complete = false;
                for (DWORD i = 0; i < count; i++) {
                    if (events[i].EventType == KEY_EVENT &&
                            events[i].Event.KeyEvent.bKeyDown &&
                            events[i].Event.KeyEvent.wVirtualKeyCode == VK_RETURN)
                        complete = true;
                }
                if (!complete) return -2;
                char buffer[4096];
                DWORD size = 0;
                if (!ReadFile(input, buffer, sizeof(buffer), &size, nullptr))
                    return prompt_interrupt ? -2 : EOF;
                line.assign(buffer, size);
                position = 0;
                if (line.empty()) return -2;
            }
            return static_cast<unsigned char>(line[position++]);
        }
    }
#else
    pollfd input{STDIN_FILENO, POLLIN, 0};
    if (::poll(&input, 1, 0) <= 0) return -2;
#endif
    return getchar();
}

bool Client::execute(const std::string &code, bool verbose, const InputSource &input_source)
{
    struct Executing {
        Executing() { prompt_interrupt = 0; executing_prompt = 1; }
        ~Executing() { executing_prompt = 0; prompt_interrupt = 0; }
    } executing;
    Object content{{"code", kernel::utf8(code)}, {"silent", false},
        {"store_history", true}, {"user_expressions", Object{}},
        {"allow_stdin", true}, {"stop_on_error", true}};
    if (verbose) content["lfortran_verbose"] = true;
    Message request = new_message("execute_request", Object{}, std::move(content), session);
    // Also in the header, which Xeus passes to the interpreter, unlike the
    // rest of the content.
    if (verbose) request.header["lfortran_verbose"] = true;
    if (!send_message(*shell, request, key)) return false;
    Peer *const peers[4] = {iopub.get(), shell.get(), stdin_.get(), control.get()};
    bool replied = false, idle = false;
    std::optional<Message> reading;
    std::string record;
    auto interrupt = [&]() {
        if (!prompt_interrupt) return true;
        prompt_interrupt = 0;
        if (reading) {
            const auto interruptible = reading->content.getBoolean("lfortran_interruptible");
            if (!interruptible || *interruptible) reading.reset();
        }
        record.clear();
        clearerr(stdin);
        return send_message(*control, new_message("interrupt_request", Object{},
            Object{}, session), key);
    };
    while (!replied || !idle) {
        if (!interrupt()) return false;
        if (reading) {
            const int c = input_source ? input_source() : stdin_character();
            if (prompt_interrupt) continue;
            if (c == EOF || c == '\n' || c == '\r' || c == 4) {
                if ((c == EOF || c == 4) && record.empty()) record = "\x04";
                clearerr(stdin);
                Message input = new_message("input_reply", reading->header,
                    Object{{"value", kernel::utf8(record)}}, session);
                if (!send_message(*stdin_, input, key)) return false;
                reading.reset();
                record.clear();
            } else if (c != -2) {
                record += static_cast<char>(c);
            }
        }
        Message m;
        const int i = next_message(peers, key, m, reading ? 1 : 50);
        if (i == -1) continue;
        if (i < 0) {
            show_last(request.id());
            return false;
        }
        const std::string type = m.type();
        if (i == 2 && type == "input_request" && m.parent_id() == request.id()) {
            // A line of the terminal, ended by Enter (CR or LF); Ctrl-D
            // on an empty line is end of file, which Jupyter clients
            // send as "\x04".
            std::cout << get_string(m.content, "prompt") << std::flush;
            reading.emplace(std::move(m));
            record.clear();
            continue;
        }
        if (m.parent_id() != request.id()) {
            // Another client's cell, which ran before this one
            if (i == 0 && get_string(m.parent, "session") != session) show(m, true);
            continue;
        }
        if (i == 1) {
            if (type == "execute_reply") {
                replied = true;
                reading.reset();
                if (get_string(m.content, "status") == "aborted") {
                    std::cerr << "The cell did not run, as a cell before it failed."
                        << std::endl;
                }
            }
        } else if (type == "status") {
            if (get_string(m.content, "execution_state") == "idle") {
                idle = true;
                reading.reset();
            }
        } else {
            show(m, false);
        }
    }
    return true;
}

Client::PollResult Client::poll_others(const DisplaySink &sink)
{
    Peer *const peers[3] = {iopub.get(), shell.get(), stdin_.get()};
    PollResult result = PollResult::none;
    const DisplaySink display = [&](const std::string &text, bool error) {
        if (text.empty()) return;
        result = PollResult::updates;
        sink(text, error);
    };
    // Yield to the keyboard even when a publisher never stops. Do not retain
    // a second output queue: the sink consumes each message as it is decoded.
    auto deadline = std::chrono::steady_clock::now() + std::chrono::milliseconds(2);
    for (int n = 0; n < 32; n++) {
        Message m;
        const int i = next_message(peers, key, m, 0);
        if (i == -2) return PollResult::disconnected;
        if (i == -1) break;
        if (i == 0 && get_string(m.parent, "session") != session) show(m, true, display);
        if (std::chrono::steady_clock::now() >= deadline) break;
    }
    return result;
}

void Client::show_last(const std::string &request_id)
{
    auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(1);
    while (!iopub->closed && std::chrono::steady_clock::now() < deadline) {
        std::vector<pollfd> fds{{iopub->s, POLLIN, 0}};
        if (poll_sockets(fds, 50) > 0) iopub->read();
    }
    std::vector<std::string> frames;
    while (iopub->next(frames)) {
        Message m;
        if (!decode(frames, key, m)) continue;
        if (m.parent_id() == request_id) {
            show(m, false);
        } else if (get_string(m.parent, "session") != session) {
            show(m, true);
        }
    }
}

int Client::kernel_status(int timeout)
{
    if (!process) return -1;
#ifdef _WIN32
    HANDLE handle = reinterpret_cast<HANDLE>(process);
    DWORD code = 0;
    if (WaitForSingleObject(handle, static_cast<DWORD>(timeout)) != WAIT_OBJECT_0
            || !GetExitCodeProcess(handle, &code)) {
        return -1;
    }
    // The process has exited. Drop the handle, but leave the connection
    // file for the destructor: this client still removes it.
    process = 0;
    HANDLE claimed = take_owned_process();
    if (claimed) CloseHandle(claimed);
    return static_cast<int>(code);
#else
    sigset_t blocked, previous;
    sigemptyset(&blocked);
    sigaddset(&blocked, SIGINT);
    auto deadline = std::chrono::steady_clock::now() + std::chrono::milliseconds(timeout);
    for (;;) {
        int status = 0;
        // Block Ctrl-C across the reap. Otherwise the handler can observe
        // the pid after it has been reused.
        sigprocmask(SIG_BLOCK, &blocked, &previous);
        const pid_t child = waitpid(static_cast<pid_t>(process), &status, WNOHANG);
        if (child == static_cast<pid_t>(process)) {
            process = 0;
            owned_pid = 0;
            sigprocmask(SIG_SETMASK, &previous, nullptr);
            return WIFEXITED(status) ? WEXITSTATUS(status) : -1;
        }
        sigprocmask(SIG_SETMASK, &previous, nullptr);
        if (child < 0 || std::chrono::steady_clock::now() > deadline) return -1;
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
#endif
}

namespace {

#ifdef _WIN32
// `arg` as a command line gives it to a program: quoted if it has spaces or
// quotes, which are escaped, as are the backslashes before them or before
// the closing quote, as CommandLineToArgvW and the C runtime split it.
std::string quote_argument(const std::string &arg)
{
    if (!arg.empty() && arg.find_first_of(" \t\n\v\"") == std::string::npos) return arg;
    std::string quoted = "\"";
    size_t backslashes = 0;
    for (char c : arg) {
        if (c == '\\') {
            backslashes++;
            continue;
        }
        quoted.append(c == '"' ? 2 * backslashes + 1 : backslashes, '\\');
        backslashes = 0;
        quoted += c;
    }
    quoted.append(2 * backslashes, '\\');
    return quoted + "\"";
}

// Starts `args` (the program first) with NUL as its standard input, output
// and error, which are the only handles it inherits: it does not hold the
// prompt's pipes open. It gets a console of its own, without a window, for
// the programs that its cells run, and so it does not get the prompt's
// Ctrl-C. Returns its process handle, or null.
HANDLE spawn(const std::vector<std::string> &args)
{
    std::string command_line;
    for (const std::string &arg : args) {
        if (!command_line.empty()) command_line += ' ';
        command_line += quote_argument(arg);
    }
    SECURITY_ATTRIBUTES inherited{sizeof(SECURITY_ATTRIBUTES), nullptr, TRUE};
    HANDLE input = CreateFileA("NUL", GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
        &inherited, OPEN_EXISTING, 0, nullptr);
    HANDLE output = CreateFileA("NUL", GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
        &inherited, OPEN_EXISTING, 0, nullptr);
    HANDLE handles[2] = {input, output};
    SIZE_T size = 0;
    InitializeProcThreadAttributeList(nullptr, 1, 0, &size);
    std::vector<char> list(size);
    auto attributes = reinterpret_cast<LPPROC_THREAD_ATTRIBUTE_LIST>(list.data());
    PROCESS_INFORMATION process{};
    bool started = false;
    if (input != INVALID_HANDLE_VALUE && output != INVALID_HANDLE_VALUE
            && InitializeProcThreadAttributeList(attributes, 1, 0, &size)) {
        STARTUPINFOEXA startup{};
        startup.StartupInfo.cb = sizeof(startup);
        startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
        startup.StartupInfo.hStdInput = input;
        startup.StartupInfo.hStdOutput = output;
        startup.StartupInfo.hStdError = output;
        startup.lpAttributeList = attributes;
        started = UpdateProcThreadAttribute(attributes, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
                handles, sizeof(handles), nullptr, nullptr)
            && CreateProcessA(args[0].c_str(), &command_line[0], nullptr, nullptr, TRUE,
                CREATE_NEW_PROCESS_GROUP | CREATE_NO_WINDOW | EXTENDED_STARTUPINFO_PRESENT,
                nullptr, nullptr, &startup.StartupInfo, &process);
        DeleteProcThreadAttributeList(attributes);
    }
    for (HANDLE handle : handles) {
        if (handle != INVALID_HANDLE_VALUE) CloseHandle(handle);
    }
    if (!started) return nullptr;
    CloseHandle(process.hThread);
    return process.hProcess;
}
#endif

} // namespace

std::unique_ptr<Client> Client::start(const std::string &executable,
    const std::vector<std::string> &arguments, std::string &connection_file)
{
    start_sockets();
    // Free ports, which the kernel then listens on, as jupyter_client does:
    // all are taken before any is released, so that no two are the same.
    Connection c;
    c.key = random_hex();
    std::vector<socket_t> taken;
    for (int &port : c.ports) {
        port = 0;
        taken.push_back(listen_on(c.ip, port));
    }
    for (socket_t s : taken) close_socket(s);
    std::error_code error;
    std::filesystem::path directory = std::filesystem::temp_directory_path(error);
    if (error) directory = ".";
    connection_file = (directory / ("lfortran-" + random_hex() + ".json")).string();
    // The Ctrl-C handler keeps this path in a fixed buffer. Reject a longer
    // one before starting the kernel, so a failure leaves no child behind.
    if (connection_file.size() >= sizeof(owned_connection)) {
        throw std::runtime_error("the connection file path is too long");
    }
    write_connection(connection_file, c);

    std::vector<std::string> args{executable};
    args.insert(args.end(), arguments.begin(), arguments.end());
#ifdef _WIN32
    const std::string parent = std::to_string(GetCurrentProcessId());
#else
    const std::string parent = std::to_string(getpid());
#endif
    args.insert(args.end(), {"kernel", "-f", connection_file, "--parent", parent});
#ifdef _WIN32
    HANDLE child = spawn(args);
    if (!child) {
        std::remove(connection_file.c_str());
        throw std::runtime_error("cannot start the kernel " + executable);
    }
    auto exited = [&]() { return WaitForSingleObject(child, 0) == WAIT_OBJECT_0; };
    auto abandon = [&]() {
        TerminateProcess(child, 1);
        CloseHandle(child);
    };
#else
    std::vector<char *> argv;
    for (std::string &arg : args) argv.push_back(&arg[0]);
    argv.push_back(nullptr);
    // The kernel does not use the terminal. It is in its own process group,
    // so the prompt's Ctrl-C does not reach it; the prompt handles Ctrl-C
    // and then ends the kernel.
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
    auto abandon = [&]() {
        kill(child, SIGKILL);
        waitpid(child, nullptr, 0);
    };
#endif
    // Connect once the kernel listens.
    auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(60);
    for (;;) {
        try {
            auto client = std::make_unique<Client>(connection_file);
#ifdef _WIN32
            client->process = reinterpret_cast<intptr_t>(child);
#else
            client->process = static_cast<intptr_t>(child);
#endif
            client->owned_file = connection_file;
            remember_owned_kernel(client->process, connection_file);
            return client;
        } catch (const std::runtime_error &) {
            const bool gone = exited();
            if (gone || std::chrono::steady_clock::now() > deadline) {
                if (!gone) abandon();
                std::remove(connection_file.c_str());
                throw std::runtime_error("the kernel did not start");
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(20));
        }
    }
}

namespace {

// The connection file that the kernel removes when it stops, also if a cell
// ends the process (STOP, a fatal error). Never freed, as a thread may use it
// while the process exits.
const std::string *owned_connection_file = nullptr;

void remove_owned_connection_file()
{
    if (owned_connection_file) std::remove(owned_connection_file->c_str());
}

void own_connection_file(const std::string &connection_file)
{
    if (owned_connection_file) return;
    owned_connection_file = new std::string(connection_file);
    std::atexit(remove_owned_connection_file);
}

// The connection file of a kernel that stops with its parent. The kernel
// owns it once it has started on it (kernel_started), not if it fails to.
const std::string *parent_connection_file = nullptr;

// Removes the connection file that the kernel owns, then ends the process
// as SIGTERM does by default. The Xeus kernel, which does not handle
// SIGTERM, then removes the file too. The built-in kernel replaces this
// handler with its own.
extern "C" void terminate_kernel(int)
{
#ifdef _WIN32
    remove_owned_connection_file();
#else
    if (owned_connection_file) unlink(owned_connection_file->c_str());
#endif
    std::signal(SIGTERM, SIG_DFL);
#ifndef _WIN32
    sigset_t terminate;
    sigemptyset(&terminate);
    sigaddset(&terminate, SIGTERM);
    sigprocmask(SIG_UNBLOCK, &terminate, nullptr);
#endif
    std::raise(SIGTERM);
}

#ifdef _WIN32
// The ID of the process that created this one, which may have exited since.
// 0 if it cannot be told.
DWORD parent_id()
{
    DWORD parent = 0;
    HANDLE snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if (snapshot == INVALID_HANDLE_VALUE) return 0;
    PROCESSENTRY32 entry;
    entry.dwSize = sizeof(entry);
    for (BOOL more = Process32First(snapshot, &entry); more;
            more = Process32Next(snapshot, &entry)) {
        if (entry.th32ProcessID == GetCurrentProcessId()) {
            parent = entry.th32ParentProcessID;
            break;
        }
    }
    CloseHandle(snapshot);
    return parent;
}

// The process `id` if it is running and was created before this one, which
// its parent was; else null, also if a later process reused the ID of a
// parent that has exited.
HANDLE open_earlier_process(DWORD id)
{
    HANDLE handle = OpenProcess(SYNCHRONIZE | PROCESS_QUERY_LIMITED_INFORMATION, FALSE, id);
    if (!handle) return nullptr;
    FILETIME created, self, unused1, unused2, unused3;
    if (WaitForSingleObject(handle, 0) != WAIT_TIMEOUT
            || (GetProcessTimes(handle, &created, &unused1, &unused2, &unused3)
                && GetProcessTimes(GetCurrentProcess(), &self, &unused1, &unused2,
                    &unused3)
                && CompareFileTime(&created, &self) > 0)) {
        CloseHandle(handle);
        return nullptr;
    }
    return handle;
}
#else
// Whether the process `id` has exited, also if its parent has not reaped it
// yet: a zombie still takes signals.
bool exited(pid_t id)
{
    if (kill(id, 0) != 0 && errno != EPERM) return true;
#if defined(__APPLE__)
    int name[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PID, id};
    struct kinfo_proc info {};
    size_t size = sizeof(info);
    return sysctl(name, 4, &info, &size, nullptr, 0) == 0 && size == sizeof(info)
        && info.kp_proc.p_stat == SZOMB;
#elif defined(__linux__)
    // The state follows the command, in parentheses that it may contain too
    std::ifstream stat("/proc/" + std::to_string(id) + "/stat");
    std::string line;
    std::getline(stat, line);
    const size_t command_end = line.rfind(')');
    return command_end != std::string::npos && command_end + 2 < line.size()
        && line[command_end + 2] == 'Z';
#else
    return false;
#endif
}
#endif

} // namespace

bool stop_with_parent(long long parent, const std::string &connection_file)
{
#ifdef _WIN32
    if (parent > MAXDWORD) return false;
    HANDLE handle = open_earlier_process(static_cast<DWORD>(parent));
    const DWORD actual = parent_id();
    if (handle && actual != 0 && actual != parent) {
        CloseHandle(handle);
        return false;
    }
    const bool gone = !handle;
#else
    if (parent > std::numeric_limits<pid_t>::max()) return false;
    // A parent that has exited already, before this kernel got here, is no
    // longer the parent either. Another process that runs is an error.
    const bool gone = getppid() != parent;
    if (gone && !exited(static_cast<pid_t>(parent))) return false;
#endif
    if (gone) {
        // Stop at once, as for a parent that exits later, before this
        // kernel starts on the file
        std::remove(connection_file.c_str());
        terminate_kernel(SIGTERM);
    }
    parent_connection_file = new std::string(connection_file);
#ifndef _WIN32
    // Stop on SIGTERM also if the parent blocked it, which this process
    // inherited, as do the threads it starts from here on.
    sigset_t terminate;
    sigemptyset(&terminate);
    sigaddset(&terminate, SIGTERM);
    sigprocmask(SIG_UNBLOCK, &terminate, nullptr);
#endif
    // Also if the parent ignored it
    std::signal(SIGTERM, terminate_kernel);
    std::thread([=]() {
#ifdef _WIN32
        if (handle) {
            WaitForSingleObject(handle, INFINITE);
            CloseHandle(handle);
        }
#else
        // As ipykernel does: once the parent has exited, this process has
        // another parent.
        while (getppid() == static_cast<pid_t>(parent)) {
            std::this_thread::sleep_for(std::chrono::milliseconds(250));
        }
#endif
        std::remove(connection_file.c_str());
        // The built-in kernel stops as it does on SIGTERM, also during a
        // cell. The Xeus kernel terminates.
        std::raise(SIGTERM);
    }).detach();
    return true;
}

void kernel_started()
{
    if (parent_connection_file) own_connection_file(*parent_connection_file);
}

// The built-in kernel

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

// Whether one of `clients`, connections that the kernel does not read while
// it runs a cell, is still open, or a new one waits on `listener`
bool connected(socket_t listener, const std::vector<socket_t> &clients)
{
#ifdef POLLRDHUP
    const short hangup = POLLRDHUP;
#else
    const short hangup = 0;
#endif
    std::vector<pollfd> fds{{listener, POLLIN, 0}};
    for (socket_t s : clients) fds.push_back({s, static_cast<short>(POLLIN | hangup), 0});
    if (poll_sockets(fds, 0) < 0) return true;
    if (fds[0].revents & POLLIN) return true;
    for (size_t i = 1; i < fds.size(); i++) {
        const short events = fds[i].revents;
        if (events & (POLLHUP | POLLERR | POLLNVAL | hangup)) continue;
        if (!(events & POLLIN)) return true;
        // What the client sent and the kernel has not read yet, or else the
        // end of the connection
        char c;
        if (::recv(clients[i - 1], &c, 1, MSG_PEEK) > 0) return true;
    }
    return false;
}

// Shell requests wait while a cell runs; the caller pumps control and IOPub.
// A thread watches the shell connections meanwhile, and ends the process if
// the kernel is asked to stop (SIGINT, SIGTERM) or, for a managed kernel, once
// its last client has gone. The prompt ends its kernel with SIGTERM, including
// during a cell that does not return; this is what makes that cell stop.
class CellWatch {
    std::mutex mutex;
    std::condition_variable changed;
    bool done = false;
    std::thread thread;

public:
    CellWatch(const Endpoint &shell, bool managed)
    {
        std::vector<socket_t> clients;
        for (auto &peer : shell.peers) {
            if (!peer->closed) clients.push_back(peer->s);
        }
        thread = std::thread([this, listener = shell.listener, clients, managed]() {
            std::unique_lock<std::mutex> lock(mutex);
            while (!changed.wait_for(lock, std::chrono::milliseconds(250),
                    [this]() { return done; })) {
                if (stop_requested || (managed && !connected(listener, clients))) {
                    remove_owned_connection_file();
                    std::_Exit(0);
                }
            }
        });
    }
    ~CellWatch()
    {
        {
            std::lock_guard<std::mutex> lock(mutex);
            done = true;
        }
        changed.notify_all();
        thread.join();
    }
    CellWatch(const CellWatch &) = delete;
    CellWatch &operator=(const CellWatch &) = delete;
};

// Answers heartbeats on its own thread until `stop` is set.
void answer_heartbeats(Endpoint &beats, const std::atomic<bool> &stop)
{
    while (!stop) {
        progress({&beats}, 250);
        for (auto &peer : beats.peers) {
            std::vector<std::string> frames;
            while (peer->next(frames)) peer->send(frames);
        }
        beats.prune();
    }
}

class Kernel;

// The kernel whose IOPub output is sent when the process exits.
Kernel *exiting_kernel = nullptr;

class Kernel {
    FortranEvaluator &e;
    std::string key, session = random_hex();
    Endpoint &shell_, &control_, &iopub_;
    InputChannel &stdin_;
    bool managed;
    int execution_count = 0;
    bool stopping = false;
    // Set while the execute_requests that wait behind a failed cell are
    // aborted
    bool aborting = false;
    bool interrupted_cell = false;
    std::vector<std::pair<Peer *, Message>> deferred_control;
    const std::thread::id owner = std::this_thread::get_id();

public:
    Kernel(FortranEvaluator &e, const std::string &key, Endpoint &shell, Endpoint &control,
        Endpoint &iopub, InputChannel &input, bool managed)
        : e(e), key(key), shell_(shell), control_(control), iopub_(iopub), stdin_(input),
          managed(managed) {}

    void run()
    {
        // A cell that ends the process (STOP, a fatal error) has its output
        // passed on as the process exits; this sends it to the subscribers.
        static const int registered = std::atexit([]() {
            if (exiting_kernel && std::this_thread::get_id() == exiting_kernel->owner)
                exiting_kernel->flush_subscribers(1000);
        });
        (void)registered;
        exiting_kernel = this;
        bool had_client = false;
        auto started = std::chrono::steady_clock::now();
        while (!stopping && !stop_requested) {
            progress({&shell_, &control_, &iopub_}, 250);
            // A subscriber sends only its subscriptions; it gets every message.
            for (auto &peer : iopub_.peers) {
                std::vector<std::string> frames;
                while (peer->next(frames)) {}
            }
            handle_received(control_);
            handle_received(shell_);
            for (Endpoint *endpoint : {&shell_, &control_, &iopub_}) endpoint->prune();
            // A managed kernel stops when its last client disconnects, or if
            // none connects within a minute.
            if (managed) {
                if (!shell_.peers.empty()) had_client = true;
                if (had_client ? shell_.peers.empty()
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
            for (auto &peer : iopub_.peers) {
                peer->flush();
                if (peer->pending()) fds.push_back({peer->s, POLLOUT, 0});
            }
            if (fds.empty()) return;
            poll_sockets(fds, 50);
        }
    }

private:
    bool pump_interrupt()
    {
        progress({&control_, &iopub_}, 0);
        for (auto &peer : control_.peers) {
            std::vector<std::string> frames;
            for (int n = 0; n < 32 && peer->next(frames); n++) {
                Message request;
                if (!decode(frames, key, request)) continue;
                if (request.type() == "interrupt_request") {
                    interrupted_cell = true;
                    handle(*peer, request);
                } else {
                    deferred_control.emplace_back(peer.get(), std::move(request));
                }
            }
        }
        for (auto &subscriber : iopub_.peers) subscriber->flush(65536);
        return interrupted_cell;
    }

    // Handles the requests read from the connections of `endpoint`.
    void handle_received(Endpoint &endpoint)
    {
        for (size_t i = 0; i < endpoint.peers.size() && !stopping; i++) {
            Peer &peer = *endpoint.peers[i];
            std::vector<std::string> frames;
            while (!stopping && peer.next(frames)) {
                Message request;
                if (decode(frames, key, request)) handle(peer, request);
            }
        }
    }

    // A cell that fails with stop_on_error aborts the execute_requests that
    // wait (in any client), as their cells may need it, as ipykernel and Xeus
    // do.
    void abort_waiting()
    {
        aborting = true;
        for (auto &peer : shell_.peers) peer->read();
        handle_received(shell_);
        aborting = false;
    }

    void publish(const std::string &type, const Object &parent, Object content)
    {
        Message m = new_message(type, parent, std::move(content), session);
        m.identities = {"kernel." + session + "." + type};
        std::vector<std::string> frames = encode(m, key);
        // A client waits for the idle status that ends its request.
        const Peer::Delivery delivery = type == "status" || type == "shutdown_reply"
            ? Peer::Delivery::reliable : Peer::Delivery::droppable;
        for (auto &peer : iopub_.peers) {
            if (peer->ready()) {
                peer->send(frames, delivery);
                // execute() pumps callbacks without returning to run().
                // Make nonblocking progress without servicing new requests.
                peer->flush();
            }
        }
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
        bool failed = false;
        publish("status", request.header, Object{{"execution_state", "busy"}});
        if (type == "execute_request") {
            if (aborting) {
                reply(peer, request, "execute_reply", Object{{"status", "aborted"}});
            } else {
                failed = execute(peer, request);
            }
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
                {"banner", kernel::banner(LFORTRAN_VERSION)}, {"help_links", Array{}}});
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
            reply(peer, request, "interrupt_reply", Object{{"status", "ok"}});
        } else if (type == "shutdown_request") {
            bool restart = get_bool(request.content, "restart", false);
            reply(peer, request, "shutdown_reply", Object{{"status", "ok"}, {"restart", restart}});
            publish("shutdown_reply", request.header, Object{{"status", "ok"}, {"restart", restart}});
            stopping = true;
        }
        // Before the idle, after which the client may send its next cell
        if (failed) abort_waiting();
        publish("status", request.header, Object{{"execution_state", "idle"}});
        // Never re-enter execution or shutdown while the evaluator is active.
        if (type == "execute_request") {
            auto deferred = std::move(deferred_control);
            deferred_control.clear();
            for (auto &item : deferred) handle(*item.first, item.second);
        }
    }

    // Runs the cell of an execute_request; true if it failed and the
    // requests that wait are to be aborted.
    bool execute(Peer &peer, const Message &request)
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
        interrupted_cell = false;
        io.cancel_requested = [&]() { return pump_interrupt(); };
        io.progress_output = [&]() {
            for (auto &subscriber : iopub_.peers) subscriber->flush(65536);
        };
        io.flush_on_exit = [&]() { flush_subscribers(1000); };
        io.display = [&](const std::string &mime, const std::string &data) {
            if (!silent) publish("display_data", request.header,
                Object{{"data", Object{{kernel::utf8(mime), kernel::utf8(data)}}},
                    {"metadata", Object{}}, {"transient", Object{}}});
        };
        io.clear_output = [&]() {
            if (!silent) publish("clear_output", request.header, Object{{"wait", false}});
        };
        const std::string identity = peer.identity;
        io.input = [&](std::string &record, bool interruptible) {
            return allow_stdin ? read_input(identity, request.header, record, interruptible) : 1;
        };
        FortranEvaluator::CellResult cell;
        {
            CellWatch watch(shell_, managed);
            cell = kernel::execute(e, code, io,
                [&](const std::string &name, const std::string &text) {
                    if (!silent) publish("stream", request.header,
                        Object{{"name", name}, {"text", text}});
                },
                [&](const std::string &text) {
                    if (!silent) publish("execute_result", request.header,
                        Object{{"execution_count", execution_count},
                            {"data", Object{{"text/plain", text}}}, {"metadata", Object{}}});
                }, verbose);
        }
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
            if (!silent && cell.error_name == "KeyboardInterrupt")
                publish("error", request.header, Object{{"ename", cell.error_name},
                    {"evalue", kernel::utf8(cell.error_value)}, {"traceback", Array{}}});
        }
        reply(peer, request, "execute_reply", std::move(result));
        return !cell.ok && !silent && get_bool(content, "stop_on_error", true);
    }

    // A READ asks the client that ran the cell, through its stdin socket of
    // the same identity, and waits for its answer, sending the subscribers
    // what the cell wrote before; other requests wait in their sockets.
    // Returns as the CellIO input hook does: > 0 (an I/O error) if that
    // client is not connected or disconnects.
    int read_input(const std::string &identity, const Object &parent, std::string &record,
        bool interruptible)
    {
        if (identity.empty()) return 1;
        Message request = new_message("input_request", parent,
            Object{{"prompt", ""}, {"password", false},
                {"lfortran_interruptible", interruptible}}, session);
        std::string parts[4];
        message_parts(request, parts);
        stdin_.ask(identity, parts);
        for (;;) {
            if (pump_interrupt() && interruptible) {
                stdin_.cancel();
                return -1;
            }
            std::vector<pollfd> fds;
            for (auto &peer : iopub_.peers) {
                peer->flush();
                if (peer->pending()) fds.push_back({peer->s, POLLOUT, 0});
            }
            const int answer = stdin_.wait(record, fds.empty() ? 250 : 0);
            if (answer > 0) return record == "\x04" ? -1 : 0;
            if (answer < 0) return 1;
            if (!fds.empty()) poll_sockets(fds, 50);
        }
    }
};

} // namespace

int run_builtin_kernel(const std::string &connection_filename, CompilerOptions &options,
    bool managed)
{
    start_sockets();
    Connection c;
    const bool existing = std::ifstream(connection_filename).good();
    std::unique_ptr<Endpoint> shell, control, iopub, beats;
    std::unique_ptr<InputChannel> input;
    try {
        if (existing) {
            c = read_connection(connection_filename);
        } else {
            c.key = random_hex();
        }
        shell = std::make_unique<Endpoint>(listen_on(c.ip, c.ports[jupyter::shell]), "ROUTER");
        control = std::make_unique<Endpoint>(listen_on(c.ip, c.ports[jupyter::control]),
            "ROUTER");
        input = std::make_unique<InputChannel>(c.ip, c.ports[stdin_channel], c.key);
        iopub = std::make_unique<Endpoint>(listen_on(c.ip, c.ports[jupyter::iopub]), "PUB");
        beats = std::make_unique<Endpoint>(listen_on(c.ip, c.ports[heartbeat]), "REP");
        if (!existing) write_connection(connection_filename, c);
    } catch (const std::runtime_error &error) {
        std::cerr << "error: " << error.what() << std::endl;
        return 1;
    }
    if (!existing || managed) own_connection_file(connection_filename);
    kernel_started();
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
    std::thread answering(answer_heartbeats, std::ref(*beats), std::cref(stop));
    Kernel(e, c.key, *shell, *control, *iopub, *input, managed).run();
    stop = true;
    answering.join();
    remove_owned_connection_file();
    return 0;
}

} // namespace LCompilers::LFortran::jupyter

#ifndef HAVE_LFORTRAN_XEUS
namespace LCompilers::LFortran {

int run_kernel(const std::string &connection_filename, CompilerOptions &options,
    bool managed)
{
    return jupyter::run_builtin_kernel(connection_filename, options, managed);
}

} // namespace LCompilers::LFortran
#endif

#else // __EMSCRIPTEN__

namespace LCompilers::LFortran::jupyter {

class Peer {};

Client::Client(const std::string &)
{
    throw std::runtime_error("Jupyter clients are not supported in this build");
}

Client::~Client() = default;

bool Client::execute(const std::string &, bool, const InputSource &)
{
    return false;
}

Client::PollResult Client::poll_others(const DisplaySink &)
{
    return PollResult::disconnected;
}

int Client::stdin_character() { return EOF; }

int Client::kernel_status(int)
{
    return -1;
}

std::unique_ptr<Client> Client::start(const std::string &,
    const std::vector<std::string> &, std::string &)
{
    throw std::runtime_error("Jupyter clients are not supported in this build");
}

bool stop_with_parent(long long, const std::string &)
{
    return true;
}

void kernel_started() {}

void handle_interrupt() {}

} // namespace LCompilers::LFortran::jupyter

#ifndef HAVE_LFORTRAN_XEUS
int LCompilers::LFortran::run_kernel(const std::string &, CompilerOptions &, bool)
{
    std::cerr << "error: the Jupyter kernel is not supported in this build" << std::endl;
    return 1;
}
#endif

#endif // __EMSCRIPTEN__

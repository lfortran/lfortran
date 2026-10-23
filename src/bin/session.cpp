#include <bin/session.h>

#include <iostream>
#include <stdexcept>

#if !defined(_WIN32) && !defined(__EMSCRIPTEN__)
#include <cerrno>
#include <csignal>
#include <cstdio>
#include <cstring>
#include <vector>

#include <poll.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <unistd.h>

#include <lfortran/fortran_evaluator.h>

namespace LCompilers::LFortran {

namespace {

// A message is a frame: the length of what follows as 4 big-endian bytes, a
// kind byte and the payload.
//
// Client to server:
//   'x' run the cell in the payload
//   'i' the record (one line, without the newline) a READ asked for
//   'e' end of file for the READ that asked for input
// Server to client, while its cell runs:
//   '1', '2' text the cell wrote to stdout, stderr
//   'd' display data: MIME type, a NUL byte, the data
//   'c' clear the output
//   '?' a READ asks for a record; answer with 'i' or 'e'
//   'r' the cell's value, as text
//   '.' the cell is done; the payload is "ok" or "error"

bool write_all(int fd, const char *data, size_t size) {
    while (size > 0) {
        ssize_t n = send(fd, data, size, 0);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) return false;
        data += n;
        size -= n;
    }
    return true;
}

bool read_all(int fd, char *data, size_t size) {
    while (size > 0) {
        ssize_t n = recv(fd, data, size, 0);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) return false;
        data += n;
        size -= n;
    }
    return true;
}

bool send_frame(int fd, char kind, const std::string &payload) {
    uint32_t size = payload.size() + 1;
    char header[5] = {char(size >> 24), char(size >> 16), char(size >> 8),
        char(size), kind};
    return write_all(fd, header, sizeof(header))
        && write_all(fd, payload.data(), payload.size());
}

bool read_frame(int fd, char &kind, std::string &payload) {
    unsigned char header[4];
    if (!read_all(fd, reinterpret_cast<char *>(header), sizeof(header))) return false;
    uint32_t size = uint32_t(header[0]) << 24 | uint32_t(header[1]) << 16
        | uint32_t(header[2]) << 8 | header[3];
    if (size == 0) return false;
    if (!read_all(fd, &kind, 1)) return false;
    payload.resize(size - 1);
    return read_all(fd, &payload[0], payload.size());
}

// While a cell runs, its stdout and stderr go to temporary files rather than
// pipes: a file never fills up, so nothing has to drain it concurrently, and
// what the cell wrote so far is sent whenever the cell waits (for input) or
// finishes.
class Capture {
    FILE *files[2];
    int saved[2];
public:
    Capture() {
        fflush(stdout);
        fflush(stderr);
        for (int i = 0; i < 2; i++) {
            files[i] = tmpfile();
            if (!files[i]) throw std::runtime_error("cannot create a temporary file");
            saved[i] = dup(1 + i);
            dup2(fileno(files[i]), 1 + i);
        }
    }
    ~Capture() {
        fflush(stdout);
        fflush(stderr);
        for (int i = 0; i < 2; i++) {
            dup2(saved[i], 1 + i);
            close(saved[i]);
            fclose(files[i]);
        }
    }
    // What the cell has written to stdout (0) or stderr (1) since last time.
    std::string take(int i) {
        fflush(i == 0 ? stdout : stderr);
        int fd = fileno(files[i]);
        std::string text;
        char buffer[4096];
        lseek(fd, 0, SEEK_SET);
        ssize_t n;
        while ((n = read(fd, buffer, sizeof(buffer))) > 0) text.append(buffer, n);
        // The descriptors 1 or 2 share this file offset, so the cell's next
        // write starts again at the beginning.
        if (ftruncate(fd, 0) != 0) throw std::runtime_error("cannot truncate a temporary file");
        lseek(fd, 0, SEEK_SET);
        return text;
    }
};

void run_cell(FortranEvaluator &e, int client, const std::string &code) {
    bool connected = true;
    auto send = [&](char kind, const std::string &payload) {
        if (connected && !send_frame(client, kind, payload)) connected = false;
    };
    FortranEvaluator::CellResult cell;
    {
        Capture capture;
        auto flush = [&]() {
            std::string out = capture.take(0), err = capture.take(1);
            if (!out.empty()) send('1', out);
            if (!err.empty()) send('2', err);
        };
        FortranEvaluator::CellIO io;
        io.display = [&](const std::string &mime, const std::string &data) {
            flush();
            send('d', mime + '\0' + data);
        };
        io.clear_output = [&]() {
            flush();
            send('c', "");
        };
        // A READ waits here for this client's answer; the other clients'
        // requests wait in their sockets until the cell is done.
        io.input = [&](std::string &record) {
            flush();
            send('?', "");
            char kind;
            if (!connected || !read_frame(client, kind, record)) {
                connected = false;
                return 1;
            }
            return kind == 'i' ? 0 : -1;
        };
        cell = e.execute(code, false, io);
        flush();
    }
    if (!cell.ok) {
        send('2', cell.error_text());
    } else {
        if (!cell.diagnostics.empty()) send('2', cell.diagnostics);
        if (cell.inspection && !cell.output.empty()) {
            send('1', cell.output.back() == '\n' ? cell.output : cell.output + "\n");
        }
        if (cell.value.has_value()) send('r', cell.value.text());
    }
    send('.', cell.ok ? "ok" : "error");
}

volatile sig_atomic_t stop_requested = 0;

extern "C" void request_stop(int) {
    stop_requested = 1;
}

sockaddr_un socket_address(const std::string &path) {
    sockaddr_un address{};
    address.sun_family = AF_UNIX;
    if (path.size() >= sizeof(address.sun_path)) {
        throw std::runtime_error("the socket path is too long: " + path);
    }
    std::strcpy(address.sun_path, path.c_str());
    return address;
}

} // namespace

int serve_session(CompilerOptions &options, const std::string &path) {
    sockaddr_un address;
    try {
        address = socket_address(path);
    } catch (const std::runtime_error &error) {
        std::cerr << "error: " << error.what() << std::endl;
        return 1;
    }
    int listener = socket(AF_UNIX, SOCK_STREAM, 0);
    // Whoever can connect can run code as this user: only the user can.
    mode_t mask = umask(077);
    int bound = bind(listener, reinterpret_cast<sockaddr *>(&address), sizeof(address));
    umask(mask);
    if (listener < 0 || bound != 0 || listen(listener, 16) != 0) {
        std::cerr << "error: cannot listen on " << path << ": "
            << std::strerror(errno) << std::endl;
        return 1;
    }
    // A client that has gone is noticed by the failed send, not a signal.
    signal(SIGPIPE, SIG_IGN);
    // SIGINT and SIGTERM stop the server, which then removes the socket.
    struct sigaction action{};
    action.sa_handler = request_stop;
    sigaction(SIGINT, &action, nullptr);
    sigaction(SIGTERM, &action, nullptr);

    options.interactive = true;
    FortranEvaluator e(options);
    std::vector<int> clients;
    while (!stop_requested) {
        std::vector<pollfd> fds{{listener, POLLIN, 0}};
        for (int client : clients) fds.push_back({client, POLLIN, 0});
        if (poll(fds.data(), fds.size(), -1) < 0) {
            if (errno == EINTR) continue;
            break;
        }
        // Requests that arrived together run in the order of the clients.
        for (size_t i = 1; i < fds.size(); i++) {
            if (!fds[i].revents) continue;
            char kind;
            std::string payload;
            if (!read_frame(fds[i].fd, kind, payload)) {
                close(fds[i].fd);
                fds[i].fd = -1;
            } else if (kind == 'x') {
                run_cell(e, fds[i].fd, payload);
            }
        }
        clients.clear();
        for (size_t i = 1; i < fds.size(); i++) {
            if (fds[i].fd >= 0) clients.push_back(fds[i].fd);
        }
        if (fds[0].revents & POLLIN) {
            int client = accept(listener, nullptr, nullptr);
            if (client >= 0) clients.push_back(client);
        }
    }
    for (int client : clients) close(client);
    close(listener);
    unlink(path.c_str());
    return 0;
}

SessionClient::SessionClient(const std::string &path) {
    sockaddr_un address = socket_address(path);
    signal(SIGPIPE, SIG_IGN);
    fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0 || connect(fd, reinterpret_cast<sockaddr *>(&address), sizeof(address)) != 0) {
        std::string reason = std::strerror(errno);
        if (fd >= 0) close(fd);
        throw std::runtime_error("cannot connect to " + path + ": " + reason);
    }
}

SessionClient::~SessionClient() {
    close(fd);
}

bool SessionClient::execute(const std::string &code) {
    if (!send_frame(fd, 'x', code)) return false;
    for (;;) {
        char kind;
        std::string payload;
        if (!read_frame(fd, kind, payload)) return false;
        switch (kind) {
            case '1': std::cout << payload << std::flush; break;
            case '2': std::cerr << payload << std::flush; break;
            case 'r': std::cout << payload << std::endl; break;
            case 'd': {
                // A terminal shows plain text only.
                size_t end = payload.find('\0');
                if (payload.compare(0, end, "text/plain") == 0) {
                    std::cout << payload.substr(end + 1) << std::endl;
                } else {
                    std::cout << "[" << payload.substr(0, end) << " output]" << std::endl;
                }
                break;
            }
            case '?': {
                // A line of the terminal, ended by Enter (CR or LF); Ctrl-D on
                // an empty line is end of file, for this READ only.
                std::string record;
                int c;
                while ((c = getchar()) != EOF && c != '\n' && c != '\r' && c != 4) {
                    record += static_cast<char>(c);
                }
                bool eof = (c == EOF || c == 4) && record.empty();
                clearerr(stdin);
                if (!send_frame(fd, eof ? 'e' : 'i', record)) return false;
                break;
            }
            case '.': return true;
        }
    }
}

} // namespace LCompilers::LFortran

#else

namespace LCompilers::LFortran {

int serve_session(CompilerOptions &, const std::string &) {
    std::cerr << "error: sessions are not supported on this platform yet" << std::endl;
    return 1;
}

SessionClient::SessionClient(const std::string &) : fd(-1) {
    throw std::runtime_error("sessions are not supported on this platform yet");
}

SessionClient::~SessionClient() {}

bool SessionClient::execute(const std::string &) {
    return false;
}

} // namespace LCompilers::LFortran

#endif

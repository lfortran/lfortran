#include <lfortran/native_protocol.h>
#include <algorithm>
#include <atomic>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <map>
#include <mutex>
#include <thread>
#ifdef _WIN32
#include <windows.h>
#include <aclapi.h>
#include <io.h>
#include <fcntl.h>
#include <conio.h>
#else
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <cerrno>
#endif

namespace LCompilers::LFortran {
using namespace Local;
namespace {
using Clock = std::chrono::steady_clock;
namespace fs = std::filesystem;
#ifdef _WIN32
using Handle = HANDLE;
const Handle invalid = INVALID_HANDLE_VALUE;
std::wstring wide(const std::string &text) { return fs::u8path(text).wstring(); }
void close_handle(Handle fd) {
    if (fd != invalid) CloseHandle(fd);
}
struct PrivateSecurity {
    std::vector<char> token;
    SECURITY_DESCRIPTOR descriptor{};
    SECURITY_ATTRIBUTES attributes{sizeof(SECURITY_ATTRIBUTES), &descriptor, FALSE};
    PACL acl = nullptr;
    PrivateSecurity() {
        HANDLE process_token;
        if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &process_token))
            throw std::runtime_error("cannot query local session owner");
        DWORD size = 0;
        GetTokenInformation(process_token, TokenUser, nullptr, 0, &size);
        token.resize(size);
        bool ok = GetTokenInformation(process_token, TokenUser, token.data(), size, &size);
        CloseHandle(process_token);
        if (!ok) throw std::runtime_error("cannot query local session SID");
        auto sid = reinterpret_cast<TOKEN_USER *>(token.data())->User.Sid;
        EXPLICIT_ACCESSW access{};
        access.grfAccessPermissions = GENERIC_ALL;
        access.grfAccessMode = SET_ACCESS;
        access.grfInheritance = SUB_CONTAINERS_AND_OBJECTS_INHERIT;
        access.Trustee.TrusteeForm = TRUSTEE_IS_SID;
        access.Trustee.TrusteeType = TRUSTEE_IS_USER;
        access.Trustee.ptstrName = static_cast<LPWSTR>(sid);
        if (SetEntriesInAclW(1, &access, nullptr, &acl) != ERROR_SUCCESS ||
                !InitializeSecurityDescriptor(&descriptor, SECURITY_DESCRIPTOR_REVISION) ||
                !SetSecurityDescriptorOwner(&descriptor, sid, FALSE) ||
                !SetSecurityDescriptorDacl(&descriptor, TRUE, acl, FALSE) ||
                !SetSecurityDescriptorControl(&descriptor, SE_DACL_PROTECTED, SE_DACL_PROTECTED))
            throw std::runtime_error("cannot secure local session ACL");
    }
    ~PrivateSecurity() { if (acl) LocalFree(acl); }
    void check(const std::string &path) {
        PSID owner = nullptr;
        PACL dacl = nullptr;
        PSECURITY_DESCRIPTOR security = nullptr;
        auto name = wide(path);
        DWORD error = GetNamedSecurityInfoW(name.data(), SE_FILE_OBJECT,
            OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION,
            &owner, nullptr, &dacl, nullptr, &security);
        auto sid = reinterpret_cast<TOKEN_USER *>(token.data())->User.Sid;
        bool ok = error == ERROR_SUCCESS && owner && EqualSid(owner, sid) && dacl;
        if (ok) {
            for (DWORD i = 0; i < dacl->AceCount; ++i) {
                void *ace;
                if (!GetAce(dacl, i, &ace)) { ok = false; break; }
                auto entry = static_cast<ACCESS_ALLOWED_ACE *>(ace);
                if (entry->Header.AceType != ACCESS_ALLOWED_ACE_TYPE ||
                        !EqualSid(&entry->SidStart, sid)) { ok = false; break; }
            }
        }
        if (security) LocalFree(security);
        if (!ok) throw std::runtime_error("local session path must have an owner-only ACL");
        DWORD flags = GetFileAttributesW(name.c_str());
        if (flags == INVALID_FILE_ATTRIBUTES || (flags & FILE_ATTRIBUTE_REPARSE_POINT))
            throw std::runtime_error("local session paths must not be reparse points");
    }
};
PrivateSecurity &security() { static PrivateSecurity result; return result; }
int read_some(Handle fd, char *data, size_t length) {
    DWORD available = 0, count = 0;
    if (!PeekNamedPipe(fd, nullptr, 0, nullptr, &available, nullptr)) return -1;
    if (!available) return 0;
    if (!ReadFile(fd, data, static_cast<DWORD>(std::min<size_t>(length, available)),
            &count, nullptr)) return -1;
    return count ? static_cast<int>(count) : -1;
}
int write_some(Handle fd, const char *data, size_t length) {
    DWORD count = 0;
    if (!WriteFile(fd, data, static_cast<DWORD>(std::min<size_t>(length, 65536)),
            &count, nullptr)) {
        if (GetLastError() == ERROR_NO_DATA) return 0;
        return -1;
    }
    return static_cast<int>(count);
}
void wait_io(const std::vector<Handle> &, int timeout) {
    // PIPE_NOWAIT keeps each reactor operation bounded. No request depends on
    // this polling interval for ordering or readiness.
    HANDLE event = CreateEventA(nullptr, FALSE, FALSE, nullptr);
    WaitForSingleObject(event, static_cast<DWORD>(timeout));
    CloseHandle(event);
}
void private_directory(const std::string &path) {
    if (!CreateDirectoryW(wide(path).c_str(), &security().attributes) &&
            GetLastError() != ERROR_ALREADY_EXISTS)
        throw std::runtime_error("cannot create private local session directory");
    security().check(path);
    if (!(GetFileAttributesW(wide(path).c_str()) & FILE_ATTRIBUTE_DIRECTORY))
        throw std::runtime_error("local session parent is not a directory");
}
void check_private(const std::string &path) { security().check(path); }
void write_private(const std::string &path, const std::string &text) {
    HANDLE fd = CreateFileW(wide(path).c_str(), GENERIC_WRITE, 0, &security().attributes,
        CREATE_NEW, FILE_ATTRIBUTE_NORMAL, nullptr);
    if (fd == invalid) throw std::runtime_error("cannot exclusively create connection file");
    DWORD n = 0;
    bool ok = WriteFile(fd, text.data(), static_cast<DWORD>(text.size()), &n, nullptr)
        && n == text.size();
    CloseHandle(fd);
    if (!ok) { DeleteFileW(wide(path).c_str()); throw std::runtime_error("cannot write connection file"); }
}
int64_t process_id() { return GetCurrentProcessId(); }
#else
using Handle = int;
constexpr Handle invalid = -1;
void close_handle(Handle fd) { if (fd >= 0) close(fd); }
void nonblocking(Handle fd) {
    if (fcntl(fd, F_SETFL, O_NONBLOCK) < 0 || fcntl(fd, F_SETFD, FD_CLOEXEC) < 0)
        throw std::runtime_error("cannot configure local transport");
}
int read_some(Handle fd, char *data, size_t length) {
    ssize_t n;
    do { n = read(fd, data, length); } while (n < 0 && errno == EINTR);
    if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) return 0;
    return n > 0 ? static_cast<int>(n) : -1;
}
int write_some(Handle fd, const char *data, size_t length) {
    ssize_t n;
#ifdef MSG_NOSIGNAL
    do { n = send(fd, data, length, MSG_NOSIGNAL); } while (n < 0 && errno == EINTR);
#else
    do { n = write(fd, data, length); } while (n < 0 && errno == EINTR);
#endif
    if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) return 0;
    return n > 0 ? static_cast<int>(n) : -1;
}
void wait_io(const std::vector<Handle> &handles, int timeout) {
    std::vector<pollfd> fds;
    for (auto fd : handles) fds.push_back({fd, POLLIN, 0});
    int result;
    do { result = poll(fds.data(), fds.size(), timeout); }
    while (result < 0 && errno == EINTR);
    if (result < 0) throw std::runtime_error("local transport poll failed");
}
void check_private(const std::string &path) {
    struct stat st{};
    if (lstat(path.c_str(), &st) || st.st_uid != getuid() ||
            (st.st_mode & 077) || S_ISLNK(st.st_mode))
        throw std::runtime_error("local session paths must be owned by you and private (0700/0600)");
}
void private_directory(const std::string &path) {
    if (mkdir(path.c_str(), 0700) && errno != EEXIST)
        throw std::runtime_error("cannot create private local session directory");
    check_private(path);
    struct stat st{};
    if (lstat(path.c_str(), &st) || !S_ISDIR(st.st_mode))
        throw std::runtime_error("local session parent is not a directory");
}
void write_private(const std::string &path, const std::string &text) {
    int fd = open(path.c_str(), O_WRONLY | O_CREAT | O_EXCL | O_NOFOLLOW, 0600);
    if (fd < 0) throw std::runtime_error("cannot exclusively create connection file: " + path);
    size_t offset = 0;
    while (offset < text.size()) {
        auto n = write(fd, text.data()+offset, text.size()-offset);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) { close(fd); unlink(path.c_str()); throw std::runtime_error("cannot write connection file"); }
        offset += n;
    }
    close(fd);
}
int64_t process_id() { return getpid(); }
void same_user(Handle fd) {
#if defined(__linux__)
    struct ucred credentials{};
    socklen_t length = sizeof(credentials);
    if (getsockopt(fd, SOL_SOCKET, SO_PEERCRED, &credentials, &length) ||
            credentials.uid != getuid())
        throw std::runtime_error("local transport peer has a different owner");
#elif defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__)
    uid_t uid;
    gid_t gid;
    if (getpeereid(fd, &uid, &gid) || uid != getuid())
        throw std::runtime_error("local transport peer has a different owner");
#else
    (void)fd; // Owner-only socket directory is the portable admission boundary.
#endif
}
#endif
struct Connection {
    Handle fd = invalid;
    Frames frames;
    std::deque<std::string> output;
    size_t offset = 0, bytes = 0;
    bool closing = false, remote_closed = false, received = false;
    Clock::time_point opened = Clock::now();
    explicit Connection(Handle fd) : fd(fd) {}
    ~Connection() { close_handle(fd); }
    Connection(const Connection &) = delete;
    Connection &operator=(const Connection &) = delete;
    void send(const std::string &text) {
        auto frame = Frames::pack(text);
        if (bytes + frame.size() > max_output || output.size() >= 4096)
            throw std::runtime_error("slow local peer");
        bytes += frame.size();
        output.push_back(std::move(frame));
    }
    bool pump() {
        if (remote_closed) return !frames.ready.empty();
        size_t budget = 256 * 1024;
        while (!output.empty() && budget) {
            auto &front = output.front();
            int n = write_some(fd, front.data()+offset,
                std::min<size_t>(budget, front.size()-offset));
            if (n < 0) {
                remote_closed = true;
                return !frames.ready.empty();
            }
            if (!n) break;
            offset += n;
            bytes -= n;
            budget -= n;
            if (offset == front.size()) { output.pop_front(); offset = 0; }
        }
        if (closing) return !output.empty();
        char buffer[8192];
        budget = 256 * 1024;
        while (budget) {
            int n = read_some(fd, buffer, std::min<size_t>(sizeof(buffer), budget));
            if (n < 0) {
                remote_closed = true;
                return !frames.ready.empty();
            }
            if (!n) break;
            frames.append(buffer, n);
            budget -= n;
        }
        return true;
    }
    std::string receive(int timeout) {
        auto deadline = Clock::now() + std::chrono::milliseconds(timeout);
        for (;;) {
            if (!frames.ready.empty()) {
                auto text = std::move(frames.ready.front());
                frames.ready.pop_front();
                return text;
            }
            if (!pump()) throw std::runtime_error(
                "kernel connection lost; active execution outcome is unknown");
            if (!frames.ready.empty()) continue;
            auto left = std::chrono::duration_cast<std::chrono::milliseconds>(
                deadline-Clock::now()).count();
            if (left <= 0) return "";
            wait_io({fd}, static_cast<int>(std::min<int64_t>(left, 10)));
        }
    }
};
std::string user_root() {
#ifdef _WIN32
    DWORD size = GetEnvironmentVariableW(L"USERPROFILE", nullptr, 0);
    std::wstring home(size, L'\0');
    if (!size || !GetEnvironmentVariableW(L"USERPROFILE", home.data(), size))
        throw std::runtime_error("a private home directory is required");
    home.resize(size-1);
    auto path = (fs::path(home) / ".lfortran").u8string();
#else
    const char *home = std::getenv("HOME");
    if (!home || !*home) throw std::runtime_error("a private home directory is required");
    auto path = (fs::path(home) / ".lfortran").string();
#endif
    private_directory(path);
    return path;
}
std::string read_descriptor(const std::string &path) {
    check_private(fs::absolute(fs::u8path(path)).parent_path().u8string());
    check_private(path);
    std::ifstream in(fs::u8path(path), std::ios::binary);
    if (!in) throw std::runtime_error("cannot read connection file");
    std::string text;
    char bytes[4096];
    while (in.read(bytes, sizeof(bytes)) || in.gcount()) {
        text.append(bytes, static_cast<size_t>(in.gcount()));
        if (text.size() > 65536) throw std::runtime_error("oversized connection descriptor");
    }
    return text;
}
struct Artifacts;
std::atomic<Artifacts *> exit_artifacts{nullptr};
struct Artifacts {
    std::mutex cleanup_mutex;
    std::string directory, endpoint, descriptor, id;
    bool wrote_descriptor = false, made_directory = false, made_endpoint = false;
    ~Artifacts() {
        auto expected = this;
        exit_artifacts.compare_exchange_strong(expected, nullptr);
        cleanup();
    }
    void cleanup() {
        std::lock_guard<std::mutex> lock(cleanup_mutex);
        // Never remove a replacement descriptor/session. A private directory
        // prevents a different user racing these checks.
        if (wrote_descriptor) {
            try {
                auto value = parse(read_descriptor(descriptor));
                if (optional_string(object(value), "kernel_session") == id)
                    fs::remove(fs::u8path(descriptor));
            } catch (...) {}
            wrote_descriptor = false;
        }
#ifndef _WIN32
        if (made_endpoint) { std::error_code error; fs::remove(fs::u8path(endpoint), error); }
#endif
        if (made_directory) { std::error_code error; fs::remove(fs::u8path(directory), error); }
        made_endpoint = made_directory = false;
    }
};
std::atomic<bool> terminated{false};
#ifndef _WIN32
void terminate_server(int) { terminated.store(true); }
#endif
struct Listener {
    Handle fd = invalid;
    std::string endpoint;
    bool first = true;
    explicit Listener(std::string endpoint) : endpoint(std::move(endpoint)) {
#ifndef _WIN32
        fd = socket(AF_UNIX, SOCK_STREAM, 0);
        if (fd < 0) throw std::runtime_error("cannot create Unix session socket");
        sockaddr_un address{};
        address.sun_family = AF_UNIX;
        if (this->endpoint.size() >= sizeof(address.sun_path)) {
            close_handle(fd);
            throw std::runtime_error("private Unix socket path is too long");
        }
        std::copy(this->endpoint.begin(), this->endpoint.end(), address.sun_path);
        if (bind(fd, reinterpret_cast<sockaddr *>(&address), sizeof(address))) {
            close_handle(fd);
            throw std::runtime_error("cannot bind private Unix session socket");
        }
        if (chmod(this->endpoint.c_str(), 0600) || listen(fd, 64)) {
            close_handle(fd);
            unlink(this->endpoint.c_str());
            throw std::runtime_error("cannot secure/listen on Unix session socket");
        }
        nonblocking(fd);
#else
        create();
#endif
    }
#ifdef _WIN32
    void create() {
        fd = CreateNamedPipeA(endpoint.c_str(),
            PIPE_ACCESS_DUPLEX | (first ? FILE_FLAG_FIRST_PIPE_INSTANCE : 0),
            PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_NOWAIT | PIPE_REJECT_REMOTE_CLIENTS,
            PIPE_UNLIMITED_INSTANCES, 65536, 65536, 0, &security().attributes);
        first = false;
        if (fd == invalid) throw std::runtime_error("cannot create private session named pipe");
    }
#endif
    ~Listener() { close_handle(fd); }
    Handle accept() {
#ifdef _WIN32
        if (!ConnectNamedPipe(fd, nullptr)) {
            DWORD error = GetLastError();
            if (error == ERROR_NO_DATA) {
                DisconnectNamedPipe(fd);
                return invalid;
            }
            if (error != ERROR_PIPE_CONNECTED) {
                if (error == ERROR_PIPE_LISTENING) return invalid;
                throw std::runtime_error("cannot accept local named-pipe connection");
            }
        }
        Handle result = fd;
        fd = invalid;
        try { create(); } catch (...) { close_handle(result); throw; }
        return result;
#else
        Handle result = ::accept(fd, nullptr, nullptr);
        if (result < 0) {
            if (errno == EAGAIN || errno == EWOULDBLOCK || errno == EINTR) return invalid;
            throw std::runtime_error("cannot accept local connection");
        }
        try { nonblocking(result); same_user(result); }
        catch (...) { close_handle(result); throw; }
        return result;
#endif
    }
};
class LocalClient : public SessionClient {
    std::unique_ptr<Connection> connection;
    std::string session = new_session_id(), kernel;
    bool eof_supported = false;
public:
    explicit LocalClient(const std::string &file) {
        auto value = parse(read_descriptor(file));
        const auto &descriptor = object(value);
        if (descriptor.getInteger("lfortran_local_protocol").getValueOr(-1) != 1)
            throw std::runtime_error("standard Jupyter connections require the optional "
                "ZeroMQ adapter (WITH_JUPYTER_ZMQ); this build supports native descriptors");
        kernel = string(descriptor, "kernel_session");
        Handle fd = invalid;
#ifdef _WIN32
        if (string(descriptor, "transport") != "named_pipe")
            throw std::runtime_error("not a Windows named-pipe descriptor");
        auto endpoint = string(descriptor, "pipe");
        auto deadline = Clock::now() + std::chrono::seconds(15);
        do {
            fd = CreateFileA(endpoint.c_str(), GENERIC_READ | GENERIC_WRITE,
                0, nullptr, OPEN_EXISTING, 0, nullptr);
            if (fd != invalid || GetLastError() != ERROR_PIPE_BUSY) break;
            if (!WaitNamedPipeA(endpoint.c_str(), 1000) &&
                    GetLastError() != ERROR_SEM_TIMEOUT) break;
        } while (Clock::now() < deadline);
        if (fd == invalid) throw std::runtime_error("cannot attach to local kernel pipe");
        DWORD mode = PIPE_READMODE_BYTE | PIPE_NOWAIT;
        if (!SetNamedPipeHandleState(fd, &mode, nullptr, nullptr)) {
            close_handle(fd);
            throw std::runtime_error("cannot configure local kernel pipe");
        }
#else
        if (string(descriptor, "transport") != "unix")
            throw std::runtime_error("not a Unix socket descriptor");
        auto endpoint = string(descriptor, "socket");
        check_private(fs::path(endpoint).parent_path().string());
        check_private(endpoint);
        sockaddr_un address{};
        address.sun_family = AF_UNIX;
        if (endpoint.size() >= sizeof(address.sun_path))
            throw std::runtime_error("Unix socket path is too long");
        std::copy(endpoint.begin(), endpoint.end(), address.sun_path);
        fd = socket(AF_UNIX, SOCK_STREAM, 0);
        if (fd < 0) throw std::runtime_error("cannot create local client socket");
        if (::connect(fd, reinterpret_cast<sockaddr *>(&address), sizeof(address))) {
            close_handle(fd);
            throw std::runtime_error("cannot attach to local kernel (dead or unavailable)");
        }
        try { same_user(fd); nonblocking(fd); }
        catch (...) { close_handle(fd); throw; }
#endif
        connection.reset(new Connection(fd));
        connection->send(encode(Object{{"lfortran_local_protocol", 1},
            {"client_session", session}, {"features", Array{"stdin_eof"}}}));
        auto response = connection->receive(15000);
        if (response.empty()) throw std::runtime_error("local kernel hello timed out");
        auto hello = parse(response);
        const auto &h = object(hello);
        if (h.getInteger("lfortran_local_protocol").getValueOr(-1) != 1 ||
                optional_string(h, "kernel_session") != kernel ||
                optional_string(h, "jupyter_protocol") != "5.4")
            throw std::runtime_error("local kernel hello/descriptor mismatch");
        if (auto features = h.getArray("features"))
            for (const auto &feature : *features)
                if (feature.getAsString().getValueOr("") == "stdin_eof")
                    eof_supported = true;
    }
    const std::string &session_id() const override { return session; }
    void send_envelope(const std::string &envelope) override {
        auto value = parse(envelope);
        validate_message(object(value));
        connection->send(envelope);
        if (!connection->pump()) throw std::runtime_error("kernel connection lost");
    }
    std::string request(const std::string &type, const std::string &content,
            const std::string &channel, const std::string &parent) override {
        auto value = message(channel, type, session, object(parse(parent)),
            Object(object(parse(content))));
        auto id = string(member(object(value), "header"), "msg_id");
        connection->send(encode(value));
        if (!connection->pump()) throw std::runtime_error("kernel connection lost");
        return id;
    }
    bool next(ClientEvent &event, int timeout) override {
        auto text = connection->receive(timeout);
        if (text.empty()) return false;
        event = client_event(text);
        if (event.session != kernel)
            throw std::runtime_error("unexpected kernel session in local message");
        return true;
    }
    void input_reply(const ClientEvent &prompt, const std::string &record,
            bool eof) override {
        if (eof && !eof_supported)
            throw std::runtime_error("this session does not support stdin EOF/cancel; "
                "no fake input value was sent (the external kernel may still be waiting)");
        auto original = parse(prompt.envelope);
        Object metadata;
        if (eof) metadata["lfortran_stdin_eof"] = true;
        auto value = message("stdin", "input_reply", session,
            Object(member(object(original), "header")),
            Object{{"value", record}}, std::move(metadata));
        connection->send(encode(value));
        if (!connection->pump()) throw std::runtime_error("kernel connection lost");
    }
};
void ready_write(intptr_t ready, const std::string &text) {
    if (ready == -1) return;
#ifdef _WIN32
    DWORD count;
    WriteFile(reinterpret_cast<HANDLE>(ready), text.data(),
        static_cast<DWORD>(text.size()), &count, nullptr);
    CloseHandle(reinterpret_cast<HANDLE>(ready));
#else
    size_t offset = 0;
    while (offset < text.size()) {
        auto n = write(static_cast<int>(ready), text.data()+offset, text.size()-offset);
        if (n < 0 && errno == EINTR) continue;
        if (n <= 0) break;
        offset += n;
    }
    close(static_cast<int>(ready));
#endif
}
}
bool session_input_available() {
#ifdef _WIN32
    HANDLE input = GetStdHandle(STD_INPUT_HANDLE);
    DWORD mode;
    if (GetConsoleMode(input, &mode)) return _kbhit() != 0;
    if (GetFileType(input) == FILE_TYPE_PIPE) {
        DWORD available = 0;
        return !PeekNamedPipe(input, nullptr, 0, nullptr, &available, nullptr)
            || available != 0;
    }
    return true;
#else
    pollfd input{STDIN_FILENO, POLLIN, 0};
    int result;
    do { result = poll(&input, 1, 0); } while (result < 0 && errno == EINTR);
    if (result < 0) throw std::runtime_error("cannot poll terminal input");
    return result != 0;
#endif
}
std::unique_ptr<SessionClient> connect_local_session(const std::string &file) {
    return std::unique_ptr<SessionClient>(new LocalClient(file));
}
std::string read_private_connection_file(const std::string &file) {
    return read_descriptor(file);
}
int serve_local_endpoint(SessionEndpoint &session, const std::string &file,
        bool managed, intptr_t ready, SessionAdapter *adapter) {
    Artifacts artifacts;
    try {
#ifdef _WIN32
        if (!std::freopen("NUL", "r", stdin))
#else
        if (!std::freopen("/dev/null", "r", stdin))
#endif
            throw std::runtime_error("cannot isolate kernel stdin");
#ifndef _WIN32
        signal(SIGPIPE, SIG_IGN);
        signal(SIGTERM, terminate_server);
        signal(SIGINT, terminate_server);
#endif
        // C/runtime writes are records as well as C++ iostream writes.
        setvbuf(stdout, nullptr, _IOLBF, 0);
        setvbuf(stderr, nullptr, _IONBF, 0);
        artifacts.directory = (fs::u8path(user_root()) /
            ("s-" + new_session_id().substr(0, 16))).u8string();
#ifdef _WIN32
        if (!CreateDirectoryW(wide(artifacts.directory).c_str(), &security().attributes))
#else
        if (mkdir(artifacts.directory.c_str(), 0700))
#endif
            throw std::runtime_error("cannot exclusively create session directory");
        artifacts.made_directory = true;
        private_directory(artifacts.directory);
        artifacts.descriptor = file.empty()
            ? (fs::u8path(artifacts.directory) / "connection.json").u8string()
            : fs::absolute(fs::u8path(file)).u8string();
        check_private(fs::u8path(artifacts.descriptor).parent_path().u8string());
        if (fs::exists(fs::u8path(artifacts.descriptor)))
            throw std::runtime_error("connection file already exists; refusing to replace it");
        artifacts.id = session.id();
#ifdef _WIN32
        artifacts.endpoint = "\\\\.\\pipe\\lfortran-" + session.id();
#else
        artifacts.endpoint = (fs::path(artifacts.directory) / "kernel.sock").string();
#endif
        Listener listener(artifacts.endpoint);
        artifacts.made_endpoint = true;
        if (adapter) adapter->start(session);
        Object descriptor{{"lfortran_local_protocol", 1},
            {"kernel_session", session.id()}, {"pid", process_id()}, {"managed", managed}};
#ifdef _WIN32
        descriptor["transport"] = "named_pipe";
        descriptor["pipe"] = artifacts.endpoint;
#else
        descriptor["transport"] = "unix";
        descriptor["socket"] = artifacts.endpoint;
#endif
        write_private(artifacts.descriptor, encode(std::move(descriptor)));
        artifacts.wrote_descriptor = true;
        static const bool exit_hook = [] {
            std::atexit([] {
                auto owned = exit_artifacts.exchange(nullptr);
                if (owned) owned->cleanup();
            });
            return true;
        }();
        (void)exit_hook;
        exit_artifacts.store(&artifacts);
        ready_write(ready, artifacts.descriptor + "\n");
        ready = -1;
        std::map<uint64_t, std::unique_ptr<Connection>> peers;
        uint64_t sequence = 0;
        bool attached = false, ending = false;
        const auto startup_deadline = Clock::now() + std::chrono::seconds(15);
        auto shutdown_deadline = Clock::time_point::max();
        for (;;) {
            if (adapter) adapter->poll(session);
            if (!ending) {
                for (size_t accepted = 0; accepted < max_peers; ++accepted) {
                    auto fd = listener.accept();
                    if (fd == invalid) break;
                    if (peers.size() >= max_peers) { close_handle(fd); continue; }
                    peers.emplace(++sequence, std::unique_ptr<Connection>(new Connection(fd)));
                }
            }
            for (auto p = peers.begin(); p != peers.end();) {
                bool alive = true;
                try {
                    alive = p->second->pump();
                    while (alive && !p->second->frames.ready.empty() && !ending) {
                        auto frame = std::move(p->second->frames.ready.front());
                        p->second->frames.ready.pop_front();
                        session.receive(p->first, frame);
                        p->second->received = true;
                    }
                    if (!p->second->received &&
                            Clock::now()-p->second->opened > std::chrono::seconds(10))
                        alive = false;
                } catch (const std::exception &) { alive = false; }
                if (!alive) {
                    session.disconnect(p->first);
                    p = peers.erase(p);
                } else ++p;
            }
            for (auto &delivery : session.take_deliveries()) {
                if (adapter && adapter->deliver(delivery)) continue;
                auto p = peers.find(delivery.peer);
                if (p == peers.end()) continue;
                try {
                    p->second->send(delivery.envelope);
                    p->second->closing = delivery.disconnect;
                } catch (const std::exception &) {
                    session.disconnect(p->first);
                    peers.erase(p);
                }
            }
            attached |= session.peer_count() != 0;
            if (!ending && (terminated.load() || session.shutdown_requested() ||
                    (managed && session.peer_count() == 0 && peers.empty() &&
                        (attached || Clock::now() > startup_deadline)))) {
                ending = true;
                shutdown_deadline = Clock::now() + std::chrono::seconds(2);
                for (auto &p : peers) p.second->closing = true;
            }
            if (ending) {
                bool pending = adapter && adapter->pending();
                for (const auto &p : peers) pending |= !p.second->output.empty();
                if (!pending || Clock::now() >= shutdown_deadline) {
                    artifacts.cleanup();
                    if (adapter) adapter->stop();
                    // JIT may be waiting in READ or arbitrary user code.
                    // Process isolation is the cancellation boundary.
                    if (session.executing()) std::_Exit(0);
                    return 0;
                }
            }
            std::vector<Handle> handles;
            if (!ending) handles.push_back(listener.fd);
            for (const auto &p : peers) handles.push_back(p.second->fd);
            wait_io(handles, 5);
        }
    } catch (const std::exception &error) {
        if (adapter) adapter->stop();
        ready_write(ready, std::string("error: ") + error.what() + "\n");
        std::fprintf(stderr, "local session: %s\n", error.what());
        return 1;
    }
}
int serve_local_session(CompilerOptions options, const std::string &file,
        bool managed, intptr_t ready, SessionAdapter *adapter) {
    NativeSession session(std::move(options));
    return serve_local_endpoint(session, file, managed, ready, adapter);
}
std::string launch_local_session(const std::vector<std::string> &arguments,
        const std::string &file) {
    if (arguments.empty()) throw std::runtime_error("missing kernel executable");
    // The child receives the original compiler options and cwd. Only session
    // transport options are replaced; no source or JIT state is forked.
    std::vector<std::string> args{arguments[0]};
    for (size_t i = 1; i < arguments.size(); ++i) {
        if (arguments[i] == "--connection-file") { ++i; continue; }
        if (arguments[i].rfind("--connection-file=", 0) == 0) continue;
        args.push_back(arguments[i]);
    }
    args.push_back("--serve");
    args.push_back("--session-managed");
    if (!file.empty()) { args.push_back("--connection-file"); args.push_back(file); }
#ifdef _WIN32
    SECURITY_ATTRIBUTES attributes{sizeof(attributes), nullptr, TRUE};
    HANDLE read_pipe, write_pipe;
    if (!CreatePipe(&read_pipe, &write_pipe, &attributes, 4096))
        throw std::runtime_error("cannot create kernel startup pipe");
    SetHandleInformation(read_pipe, HANDLE_FLAG_INHERIT, 0);
    args.push_back("--session-ready");
    args.push_back(std::to_string(reinterpret_cast<intptr_t>(write_pipe)));
    auto quote = [](const std::string &arg) {
        std::string result = "\"";
        size_t slashes = 0;
        for (char c : arg) {
            if (c == '\\') { ++slashes; continue; }
            result.append(c == '"' ? 2*slashes+1 : slashes, '\\');
            slashes = 0;
            result += c;
        }
        result.append(2*slashes, '\\');
        return result + "\"";
    };
    std::string command;
    for (const auto &arg : args) command += quote(arg) + " ";
    STARTUPINFOEXW startup{};
    startup.StartupInfo.cb = sizeof(startup);
    startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
    HANDLE null = CreateFileW(L"NUL", GENERIC_READ | GENERIC_WRITE,
        FILE_SHARE_READ | FILE_SHARE_WRITE, &attributes, OPEN_EXISTING, 0, nullptr);
    startup.StartupInfo.hStdInput = null;
    startup.StartupInfo.hStdOutput = null;
    startup.StartupInfo.hStdError = null;
    SIZE_T attribute_size = 0;
    InitializeProcThreadAttributeList(nullptr, 1, 0, &attribute_size);
    std::vector<char> attribute_storage(attribute_size);
    startup.lpAttributeList = reinterpret_cast<LPPROC_THREAD_ATTRIBUTE_LIST>(
        attribute_storage.data());
    bool attribute_ready = InitializeProcThreadAttributeList(
        startup.lpAttributeList, 1, 0, &attribute_size);
    HANDLE inherited[]{write_pipe, null};
    bool inherited_ready = attribute_ready && UpdateProcThreadAttribute(
        startup.lpAttributeList, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
        inherited, sizeof(inherited), nullptr, nullptr);
    PROCESS_INFORMATION process{};
    auto wide_command = wide(command);
    bool ok = inherited_ready && CreateProcessW(wide(arguments[0]).c_str(),
        wide_command.data(), nullptr, nullptr, TRUE,
        CREATE_NO_WINDOW | EXTENDED_STARTUPINFO_PRESENT, nullptr, nullptr,
        &startup.StartupInfo, &process);
    if (attribute_ready) DeleteProcThreadAttributeList(startup.lpAttributeList);
    CloseHandle(write_pipe);
    CloseHandle(null);
    if (!ok) { CloseHandle(read_pipe); throw std::runtime_error("cannot launch local kernel"); }
    CloseHandle(process.hThread);
#else
    int ready[2];
    if (pipe(ready)) throw std::runtime_error("cannot create kernel startup pipe");
    args.push_back("--session-ready");
    args.push_back(std::to_string(ready[1]));
    std::vector<char *> argv;
    for (auto &arg : args) argv.push_back(arg.data());
    argv.push_back(nullptr);
    pid_t launcher = fork();
    if (launcher < 0) {
        close(ready[0]); close(ready[1]);
        throw std::runtime_error("cannot launch local kernel");
    }
    if (launcher == 0) {
        close(ready[0]);
        if (setsid() < 0) _exit(126);
        pid_t child = fork();
        if (child < 0) _exit(126);
        if (child > 0) _exit(0);
        int null = open("/dev/null", O_RDWR);
        if (null < 0) _exit(126);
        for (int fd = 0; fd < 3; ++fd) if (dup2(null, fd) < 0) _exit(126);
        if (null > 2) close(null);
        char identity[64];
        int length = std::snprintf(identity, sizeof(identity), "pid:%lld\n",
            static_cast<long long>(getpid()));
        if (write(ready[1], identity, static_cast<size_t>(length)) != length)
            _exit(126);
        execv(argv[0], argv.data());
        _exit(127);
    }
    close(ready[1]);
    int status;
    while (waitpid(launcher, &status, 0) < 0 && errno == EINTR) {}
    nonblocking(ready[0]);
#endif
    std::string response;
#ifndef _WIN32
    pid_t owned_child = -1;
#endif
    auto deadline = Clock::now() + std::chrono::seconds(30);
    bool ended = false;
    while (Clock::now() < deadline && response.find('\n') == std::string::npos) {
        char buffer[4096];
#ifdef _WIN32
        DWORD available = 0, size = 0;
        if (!PeekNamedPipe(read_pipe, nullptr, 0, nullptr, &available, nullptr)) { ended = true; break; }
        if (available && ReadFile(read_pipe, buffer, sizeof(buffer), &size, nullptr))
            response.append(buffer, size);
        wait_io({}, 5);
#else
        auto n = read(ready[0], buffer, sizeof(buffer));
        if (n > 0) response.append(buffer, n);
        else if (!n) { ended = true; break; }
        else if (errno != EAGAIN && errno != EINTR) { ended = true; break; }
        auto newline = response.find('\n');
        if (owned_child == -1 && newline != std::string::npos &&
                response.rfind("pid:", 0) == 0) {
            owned_child = static_cast<pid_t>(std::stoll(response.substr(4, newline-4)));
            response.erase(0, newline+1);
        }
        wait_io({ready[0]}, 100);
#endif
        if (response.size() > 65536) break;
    }
#ifdef _WIN32
    CloseHandle(read_pipe);
    if (response.empty() || response.rfind("error: ", 0) == 0 || ended ||
            response.find('\n') == std::string::npos)
        TerminateProcess(process.hProcess, 1);
    CloseHandle(process.hProcess);
#else
    close(ready[0]);
    if (!ended && owned_child > 0 && response.find('\n') == std::string::npos)
        kill(owned_child, SIGKILL);
#endif
    if (response.empty() || response.rfind("error: ", 0) == 0 || ended ||
            response.find('\n') == std::string::npos)
        throw std::runtime_error("managed kernel startup failed: " + response);
    response.resize(response.find('\n'));
    return response;
}
}

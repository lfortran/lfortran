#ifdef _WIN32
#define NOMINMAX
#include <windows.h>
#else
#include <dirent.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/stat.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/syscall.h>
#endif
#endif

#include <cctype>
#include <cerrno>
#include <cfloat>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <fstream>
#include <filesystem>
#include <random>
#include <sstream>

#include <libasr/exception.h>
#include <libasr/utils.h>
#include <libasr/string_utils.h>
#include <libasr/temp_dir.h>

namespace LCompilers {

std::string get_unique_ID() {
    static std::random_device dev;
    static std::mt19937 rng(dev());
    std::uniform_int_distribution<int> dist(0, 61);
    const std::string v =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    std::string res;
    for (int i = 0; i < 22; i++) {
        res += v[dist(rng)];
    }
    return res;
}

bool present(Vec<char*> &v, const char* name) {
    for (auto &a : v) {
        if (std::string(a) == std::string(name)) {
            return true;
        }
    }
    return false;
}

bool present(char** const v, size_t n, const std::string name) {
    for (size_t i = 0; i < n; i++) {
        if (std::string(v[i]) == name) {
            return true;
        }
    }
    return false;
}

int visualize_json(std::string &astr_data_json, LCompilers::Platform os) {
    using namespace LCompilers;
    std::hash<std::string> hasher;
    std::string file_name = "visualize" + std::to_string(hasher(astr_data_json)) + ".html";
    std::ofstream out;
    out.open(file_name);
    out << LCompilers::generate_visualize_html(astr_data_json);
    out.close();
    std::string open_cmd = "";
    switch (os) {
        case Linux: open_cmd = "xdg-open"; break;
        case Windows: open_cmd = "start"; break;
        case macOS_Intel:
        case macOS_ARM: open_cmd = "open"; break;
        default:
            std::cerr << "Unsupported Platform " << pf2s(os) <<std::endl;
            std::cerr << "Please open file " << file_name << " manually" <<std::endl;
            return 11;
    }
    std::string cmd = open_cmd + " " + file_name;
    int err = system(cmd.data());
    if (err) {
        std::cout << "The command '" + cmd + "' failed." << std::endl;
        return 11;
    }
    return 0;
}

std::string generate_visualize_html(std::string &astr_data_json) {
    std::stringstream out;
    out << R"(<!DOCTYPE html>
<html>
<head>
    <title>LCompilers AST/R Visualization</title>
    <script crossorigin src="https://unpkg.com/react@18/umd/react.production.min.js"></script>
    <script crossorigin src="https://unpkg.com/react-dom@18/umd/react-dom.production.min.js"></script>

    <script src="https://unpkg.com/@babel/standalone/babel.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/react-flow-renderer/10.3.17/umd/index.js"></script>
    <script src="https://dagrejs.github.io/project/dagre/latest/dagre.min.js"></script>
    <script> )";
    out << "var astr_data = " << astr_data_json << "; </script>\n";
    out << R"(</head>

<body style="margin: 0px;">
    <script type="text/babel" data-type="module">
function TreeNode({ node }) {
    if (node.literals.length === 0) return <p><b>{node.node}</b></p>;
    return (
        <div>
            <p><b>{node.node}</b></p>
            <div style={{ backgroundColor: "#FBBD23", padding: "2px" }}>
                {
                    node.literals.map((val, idx) => <p style={{ margin: "0px", padding: "1px" }} key={idx}>{val[0]}: {val[1]}</p>)
                }
            </div>
        </div>
    );
}

const getLayoutedElements = (nodes, edges, direction = 'TB') => {
    const nodeWidth = 180;
    const isHorizontal = direction === 'LR';

    const dagreGraph = new dagre.graphlib.Graph();
    dagreGraph.setDefaultEdgeLabel(() => ({}));
    dagreGraph.setGraph({ rankdir: direction });

    nodes.forEach(node => dagreGraph.setNode(node.id, { width: nodeWidth, height: node.nodeHeight }));
    edges.forEach(edge => dagreGraph.setEdge(edge.source, edge.target));

    dagre.layout(dagreGraph);

    nodes.forEach((node) => {
        const nodeWithPosition = dagreGraph.node(node.id);
        node.targetPosition = isHorizontal ? 'left' : 'top';
        node.sourcePosition = isHorizontal ? 'right' : 'bottom';
        // Shifting the dagre node position (anchor=center center) to the top left
        // so it matches the React Flow node anchor point (top left).
        node.position = {
            x: nodeWithPosition.x - nodeWidth / 2,
            y: nodeWithPosition.y - node.nodeHeight / 2,
        };
        return node;
    });

    return [nodes, edges];
};

class Graph {
    constructor() {
        this.nodes = [];
        this.edges = [];
        this.idx = 1;
        return this;
    }

    createNode(cur_node) {
        cur_node.idx = this.idx++;
        cur_node.literals = [];
        let obj = cur_node.fields;
        for (let prop in obj) {
            let neigh = obj[prop];
            if (typeof neigh === 'object') {
                if (neigh.hasOwnProperty("node")) {
                    this.createEdge(cur_node.idx, neigh, prop);
                } else {
                    if (neigh.length > 0) {
                        for (let i in neigh) {
                            let arrayElement = neigh[i];
                            if (typeof arrayElement === 'object') {
                                if (arrayElement.hasOwnProperty("node")) {
                                    this.createEdge(cur_node.idx, arrayElement, `${prop}[${i}]`);
                                } else {
                                    console.log("ERROR: Unexpected 2D Array found");
                                }
                            } else {
                                cur_node.literals.push([`${prop}[${i}]`, `${arrayElement}`]);
                            }
                        }
                    } else {
                        // 0 length array, show as literal
                        cur_node.literals.push([prop, "[]"]);
                    }
                }
            } else {
                cur_node.literals.push([prop, `${neigh}`]);
            }
        }

        this.nodes.push({ id: `${cur_node.idx}`, data: { label: <TreeNode node={cur_node} /> }, nodeHeight: 70 + 20 * (cur_node.literals.length) });
    }

    createEdge(parent_idx, cur_node, edge_label) {
        this.edges.push({
            id: `${parent_idx}-${this.idx}`,
            source: `${parent_idx}`,
            target: `${this.idx}`,
            label: edge_label,
            labelStyle: { fontWeight: 700 },
            labelBgPadding: [8, 4],
            labelBgStyle: { fill: '#FBBD23' },
        });
        this.createNode(cur_node);
    }
}

function Flow({ nodes, edges }) {
    return (
        <div style={{ height: '100vh' }}>
            <ReactFlow.default
                defaultNodes={nodes}
                defaultEdges={edges}
                style={{ backgroundColor: '#e5e7eb' }}
            >
                <ReactFlow.Background />
                <ReactFlow.Controls />
                <ReactFlow.MiniMap />
            </ReactFlow.default>
        </div>
    );
}

function MyApp() {
    var g = new Graph();
    g.createNode(astr_data);
    var [layoutedNodes, layoutedEdges] = getLayoutedElements(g.nodes, g.edges);
    return (<Flow nodes={layoutedNodes} edges={layoutedEdges} />);
}

ReactDOM.render(<MyApp />, document.body);
    </script>
</body>

</html>)";
    return out.str();
}

std::string pf2s(Platform p) {
    switch (p) {
        case (Platform::Linux) : return "Linux";
        case (Platform::macOS_Intel) : return "macOS Intel";
        case (Platform::macOS_ARM) : return "macOS ARM";
        case (Platform::macOS_PowerPC) : return "macOS PowerPC";
        case (Platform::Windows) : return "Windows";
        case (Platform::FreeBSD) : return "FreeBSD";
        case (Platform::OpenBSD) : return "OpenBSD";
    }
    return "Unsupported Platform";
}

Platform get_platform()
{
#if defined(_WIN32)
    return Platform::Windows;
#elif defined(__APPLE__)
#    ifdef __aarch64__
    return Platform::macOS_ARM;
#    elif defined(__POWERPC__)
    return Platform::macOS_PowerPC;
#    else
    return Platform::macOS_Intel;
#    endif
#elif defined(__FreeBSD__)
    return Platform::FreeBSD;
#elif defined(__OpenBSD__)
    return Platform::OpenBSD;
#else
    return Platform::Linux;
#endif
}

int get_c_pointer_kind(const std::string &target_triple)
{
    // The size of a pointer, of `size_t` and of `ptrdiff_t`: 4 on the
    // targets with 32-bit addresses, 8 on the others. As in llvm::Triple, the
    // components after the architecture may come in any order.
    std::vector<std::string> c = string_split(target_triple, "-", false);
    if (c.empty() || c[0].empty()) return sizeof(void*);
    std::string arch = c[0];
    for (size_t i = 1; i < c.size(); i++) {
        // x32: the x86-64 instruction set with 32-bit pointers
        if (endswith(c[i], "x32")) return 4;
    }
    bool i386 = arch.size() == 4 && arch[0] == 'i' && endswith(arch, "86");
    bool arm32 = (startswith(arch, "arm") && !startswith(arch, "arm64"))
        || startswith(arch, "thumb");
    if (i386 || arm32 || arch == "arm64_32" || arch == "wasm32"
            || arch == "riscv32" || arch == "mips" || arch == "mipsel"
            || arch == "powerpc" || arch == "ppc" || arch == "sparc") {
        return 4;
    }
    return 8;
}

int get_c_long_double_kind(const std::string &target_triple)
{
    // The values of clang. As in llvm::Triple, the components after the
    // architecture may come in any order and the vendor may be omitted.
    std::vector<std::string> c = string_split(target_triple, "-", false);
    auto has = [&](const std::string &prefix) {
        for (size_t i = 1; i < c.size(); i++) {
            if (startswith(c[i], prefix)) return true;
        }
        return false;
    };
    std::string arch = c.empty() ? "" : c[0];
    bool apple = has("apple") || has("darwin") || has("macos");
    bool windows = has("windows") || has("win32") || has("mingw32")
        || has("cygwin");
    bool x86_64 = startswith(arch, "x86_64") || arch == "amd64";
    bool x86 = x86_64
        || (arch.size() == 4 && arch[0] == 'i' && endswith(arch, "86"));
    if (x86) {
        // 80-bit x87 extended precision, except for MSVC (also the default
        // environment on Windows) and Android
        if (has("android")) return x86_64 ? 16 : 8;
        bool windows_gnu = has("gnu") || has("mingw32") || has("cygwin")
            || has("cygnus");
        return (windows && !windows_gnu) ? 8 : 10;
    }
    if (startswith(arch, "aarch64") || startswith(arch, "arm64")) {
        // Apple and Windows use double, the others IEEE quadruple precision.
        // arm64ec exists only on Windows and arm64_32 only on Apple.
        bool double_only = arch == "arm64ec" || arch == "arm64_32";
        return (apple || windows || double_only) ? 8 : 16;
    }
    if (arch == "wasm32" || arch == "wasm64") return 16;
    // The host, which is also the default target
    switch (LDBL_MANT_DIG) {
        case 53: return 8;
        case 113: return 16;
        default: return 10;
    }
}

// -1 until a compilation sets it from its PassOptions. The host kind is
// the same value those options start with.
static int active_c_long_double_kind = -1;

void set_active_c_long_double_kind(int kind)
{
    active_c_long_double_kind = kind;
}

int get_active_c_long_double_kind()
{
    if (active_c_long_double_kind < 0) {
        return get_c_long_double_kind("");
    }
    return active_c_long_double_kind;
}

// Platform-specific initialization
// On Windows, enable colors in terminal. On other systems, do nothing.
// Return value: 0 on success, negative number on failure.
int initialize()
{
#ifdef _WIN32
    HANDLE h_stdout = GetStdHandle(STD_OUTPUT_HANDLE);
    if (h_stdout == INVALID_HANDLE_VALUE)
        return -1;

    DWORD mode;
    if (! GetConsoleMode(h_stdout, &mode))
        return -2;

    mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    if (! SetConsoleMode(h_stdout, mode))
        return -3;

    return 0;
#else
    return 0;
#endif
}

std::string TempDir::system_temp_dir() {
#ifdef _WIN32
    char buffer[MAX_PATH + 1];
    DWORD len = GetTempPathA(sizeof(buffer), buffer);
    if (len > 0 && len < sizeof(buffer)) return std::string(buffer, len);
    return ".";
#else
    const char *tmp = std::getenv("TMPDIR");
    if (tmp && *tmp) return tmp;
    return "/tmp";
#endif
}

namespace {

// The name of this machine, as it can appear in a file name
std::string temp_dir_host_name() {
    char name[256] = {};
#ifdef _WIN32
    DWORD size = sizeof(name);
    if (!GetComputerNameA(name, &size)) return "host";
#else
    if (gethostname(name, sizeof(name) - 1) != 0) return "host";
#endif
    std::string host;
    for (const char *c = name; *c && host.size() < 64; c++) {
        bool ok = std::isalnum(static_cast<unsigned char>(*c))
            || *c == '.' || *c == '-' || *c == '_';
        host += ok ? *c : '_';
    }
    return host.empty() ? "host" : host;
}

// The directory that the signal handlers remove
TempDir *signal_temp_dir = nullptr;

#ifdef _WIN32
// Runs in a new thread when the console is closed or Ctrl+C or Ctrl+Break
// is pressed; returning FALSE lets the default handler end the process
BOOL WINAPI remove_temp_dir_on_console_event(DWORD) {
    TempDir *dir = signal_temp_dir;
    if (dir) dir->remove_in_signal_handler();
    return FALSE;
}
#else
// Interrupts, crashes, and the limits of the CPU time and the file size
constexpr int temp_dir_signals[] = {
    SIGHUP, SIGINT, SIGPIPE, SIGQUIT, SIGTERM,
    SIGABRT, SIGBUS, SIGFPE, SIGILL, SIGSEGV,
    SIGXCPU, SIGXFSZ
};
constexpr size_t n_temp_dir_signals
    = sizeof(temp_dir_signals) / sizeof(temp_dir_signals[0]);
struct sigaction previous_signal_actions[n_temp_dir_signals];

// The directory of the signal handlers and its subdirectories, opened when
// they are created: the handlers cannot open them, opendir() allocates
constexpr size_t max_signal_dirs = 4;
DIR *signal_dirs[max_signal_dirs] = {};

void open_signal_dir(const std::string &path) {
    for (DIR *&dir : signal_dirs) {
        if (!dir) {
            dir = opendir(path.c_str());
            return;
        }
    }
}

void close_signal_dirs() {
    for (DIR *&dir : signal_dirs) {
        if (dir) closedir(dir);
        dir = nullptr;
    }
}

// Removes the files and the empty subdirectories in the open directory
// `dir` in one pass over its entries, without allocating: in a signal
// handler. Returns how many it removed.
size_t remove_entries_once_in_signal_handler(DIR *dir) {
    int fd = dirfd(dir);
    size_t removed = 0;
    auto remove_entry = [fd, &removed](const char *name) {
        if (name[0] == '.' && (name[1] == '\0'
                || (name[1] == '.' && name[2] == '\0'))) {
            return;
        }
        if (unlinkat(fd, name, 0) == 0
                || unlinkat(fd, name, AT_REMOVEDIR) == 0) {
            removed++;
        }
    };
#ifdef __linux__
    // The getdents64 system call reads the entries into this buffer.
    // The layout of an entry is the kernel's struct linux_dirent64.
    struct LinuxDirent64 {
        std::uint64_t d_ino;
        std::int64_t d_off;
        unsigned short d_reclen;
        unsigned char d_type;
        char d_name[1];
    };
    alignas(LinuxDirent64) char buffer[4096];
    long n;
    while ((n = syscall(SYS_getdents64, fd, buffer, sizeof(buffer))) > 0) {
        for (long pos = 0; pos < n;) {
            unsigned short reclen;
            std::memcpy(&reclen, buffer + pos + offsetof(LinuxDirent64, d_reclen),
                sizeof(reclen));
            remove_entry(buffer + pos + offsetof(LinuxDirent64, d_name));
            pos += reclen;
        }
    }
#else
    // readdir() of an open directory reads into the buffer of its DIR
    while (struct dirent *entry = readdir(dir)) remove_entry(entry->d_name);
#endif
    return removed;
}

// Removes the files and the empty subdirectories in the open directory
// `dir`, without allocating. A file system may skip entries of a directory
// that changes while it is read, so it is read again from the start until a
// pass removes nothing, at most a few times.
void remove_entries_in_signal_handler(DIR *dir) {
    for (int pass = 0; pass < 8; pass++) {
#ifdef __linux__
        lseek(dirfd(dir), 0, SEEK_SET);
#else
        rewinddir(dir);
#endif
        if (remove_entries_once_in_signal_handler(dir) == 0) break;
    }
}

void temp_dir_signal_set(sigset_t *set) {
    sigemptyset(set);
    for (int sig : temp_dir_signals) sigaddset(set, sig);
}

void remove_temp_dir_on_signal(int sig) {
    int saved_errno = errno;
    TempDir *dir = signal_temp_dir;
    if (dir) dir->remove_in_signal_handler();
    // Hand the signal to the previous handler, which is the default action
    // (the process ends with the status of the signal, 128+sig in a shell)
    // or, for a crash, the stacktrace printer. It is blocked here, so it is
    // delivered when this handler returns.
    for (size_t i = 0; i < n_temp_dir_signals; i++) {
        if (temp_dir_signals[i] == sig) {
            sigaction(sig, &previous_signal_actions[i], nullptr);
        }
    }
    raise(sig);
    errno = saved_errno;
}
#endif

// Holds the signals of the handlers while it exists, so that they see the
// directory either before it is created or removed, or after
class BlockTempDirSignals {
public:
#ifdef _WIN32
    BlockTempDirSignals() {}
#else
    BlockTempDirSignals() {
        sigset_t signals;
        temp_dir_signal_set(&signals);
        sigprocmask(SIG_BLOCK, &signals, &previous_mask_);
    }
    ~BlockTempDirSignals() { sigprocmask(SIG_SETMASK, &previous_mask_, nullptr); }
private:
    sigset_t previous_mask_;
#endif
};

void install_temp_dir_signal_handlers(TempDir *dir) {
    if (signal_temp_dir) return;
    signal_temp_dir = dir;
#ifdef _WIN32
    SetConsoleCtrlHandler(remove_temp_dir_on_console_event, TRUE);
#else
    struct sigaction action {};
    action.sa_handler = remove_temp_dir_on_signal;
    // One handler at a time
    temp_dir_signal_set(&action.sa_mask);
    action.sa_flags = SA_RESTART;
    for (size_t i = 0; i < n_temp_dir_signals; i++) {
        int sig = temp_dir_signals[i];
        if (sigaction(sig, nullptr, &previous_signal_actions[i]) != 0) continue;
        // E.g. SIGINT in a background job, SIGHUP under nohup
        if (previous_signal_actions[i].sa_handler == SIG_IGN) continue;
        sigaction(sig, &action, nullptr);
    }
#endif
}

bool process_exists(unsigned long pid) {
#ifdef _WIN32
    HANDLE process = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE,
        static_cast<DWORD>(pid));
    if (!process) return GetLastError() != ERROR_INVALID_PARAMETER;
    DWORD code = 0;
    bool exists = !GetExitCodeProcess(process, &code) || code == STILL_ACTIVE;
    CloseHandle(process);
    return exists;
#else
    // EPERM: it exists, but belongs to another user
    return !(kill(static_cast<pid_t>(pid), 0) == -1 && errno == ESRCH);
#endif
}

// The process ID in the name `<start><pid>-<6 letters or digits>` of a
// temporary directory, else 0
unsigned long temp_dir_pid(const std::string &name, const std::string &start) {
    if (name.compare(0, start.size(), start) != 0) return 0;
    size_t begin = start.size(), end = begin;
    while (end < name.size() && std::isdigit(static_cast<unsigned char>(name[end]))) {
        end++;
    }
    if (end == begin || end - begin > 9 || name.size() != end + 7
            || name[end] != '-') {
        return 0;
    }
    for (size_t i = end + 1; i < name.size(); i++) {
        if (!std::isalnum(static_cast<unsigned char>(name[i]))) return 0;
    }
    return std::stoul(name.substr(begin, end - begin));
}

} // namespace

std::vector<std::string> TempDir::remove_stale(const std::string &prefix) {
    std::vector<std::string> removed;
    std::string start = prefix + "-" + temp_dir_host_name() + "-";
    std::error_code ec;
    std::filesystem::directory_iterator it(system_temp_dir(), ec), end;
    for (; !ec && it != end; it.increment(ec)) {
        const std::filesystem::path &dir = it->path();
        unsigned long pid = temp_dir_pid(dir.filename().string(), start);
        if (pid == 0) continue;
#ifdef _WIN32
        // The user's temporary directory is private on Windows
        std::error_code status_ec;
        if (!std::filesystem::is_directory(
                std::filesystem::symlink_status(dir, status_ec))) {
            continue;
        }
#else
        // A directory of this user, not a symbolic link to one
        struct stat st;
        if (lstat(dir.c_str(), &st) != 0 || !S_ISDIR(st.st_mode)
                || st.st_uid != geteuid()) {
            continue;
        }
#endif
        if (process_exists(pid)) continue;
        std::error_code remove_ec;
        std::filesystem::remove_all(dir, remove_ec);
        if (!remove_ec) removed.push_back(dir.string());
    }
    return removed;
}

TempDir::~TempDir() {
    remove();
    if (signal_temp_dir == this) signal_temp_dir = nullptr;
}

const std::string &TempDir::path() {
    if (!path_.empty()) return path_;
    std::filesystem::path base = system_temp_dir();
    BlockTempDirSignals block_signals;
#ifdef _WIN32
    std::string name = prefix_ + "-" + temp_dir_host_name() + "-"
        + std::to_string(GetCurrentProcessId()) + "-";
    // The user's temporary directory is already private on Windows
    std::error_code ec;
    for (int i = 0; i < 100 && path_.empty(); i++) {
        std::filesystem::path dir = base / (name + get_unique_ID().substr(0, 6));
        if (std::filesystem::create_directory(dir, ec)) {
            path_ = dir.string();
        } else if (ec) {
            break;
        }
    }
    if (path_.empty()) {
        throw TempDirError("cannot create a temporary directory in '"
            + base.string() + "': " + (ec ? ec.message() : "it exists"));
    }
#else
    // mkdtemp() creates the directory with mode 0700 and a unique name
    std::string name = (base / (prefix_ + "-" + temp_dir_host_name() + "-"
        + std::to_string(getpid()) + "-XXXXXX")).string();
    if (!mkdtemp(&name[0])) {
        throw TempDirError("cannot create a temporary directory in '"
            + base.string() + "': " + std::strerror(errno));
    }
    path_ = name;
#endif
    if (path_.size() < sizeof(c_path_)) {
        std::memcpy(c_path_, path_.c_str(), path_.size() + 1);
    }
    std::string message = prefix_ + ": temporary files kept in " + path_ + "\n";
    if (message.size() <= sizeof(kept_message_)) {
        std::memcpy(kept_message_, message.data(), message.size());
        kept_message_size_ = message.size();
    }
    if (remove_on_signal_) install_temp_dir_signal_handlers(this);
#ifndef _WIN32
    if (signal_temp_dir == this) open_signal_dir(path_);
#endif
    return path_;
}

std::string TempDir::file(const std::string &name) {
    return (std::filesystem::path(path()) / name).string();
}

std::string TempDir::file(const std::string &subdir, const std::string &name) {
    std::filesystem::path dir = std::filesystem::path(path()) / subdir;
    BlockTempDirSignals block_signals;
    std::error_code ec;
    [[maybe_unused]] bool created = std::filesystem::create_directory(dir, ec);
    if (ec) {
        throw TempDirError("cannot create the temporary directory '"
            + dir.string() + "': " + ec.message());
    }
#ifndef _WIN32
    if (created && signal_temp_dir == this) open_signal_dir(dir.string());
#endif
    return (dir / name).string();
}

void TempDir::remove() noexcept {
    if (path_.empty() || keep_) return;
    BlockTempDirSignals block_signals;
#ifndef _WIN32
    if (signal_temp_dir == this) close_signal_dirs();
#endif
    try {
        std::error_code ec;
        std::filesystem::remove_all(path_, ec);
    } catch (...) {
    }
    c_path_[0] = '\0';
    kept_message_size_ = 0;
    path_.clear();
}

void TempDir::report_kept() noexcept {
    if (!keep_ || kept_message_size_ == 0 || kept_reported_) return;
    kept_reported_ = 1;
#ifdef _WIN32
    std::fwrite(kept_message_, 1, kept_message_size_, stderr);
    std::fflush(stderr);
#else
    // write() is async-signal-safe
    for (std::size_t done = 0; done < kept_message_size_;) {
        ssize_t n = write(STDERR_FILENO, kept_message_ + done,
            kept_message_size_ - done);
        if (n > 0) {
            done += static_cast<std::size_t>(n);
        } else if (n < 0 && errno == EINTR) {
            continue;
        } else {
            break;
        }
    }
#endif
}

void TempDir::remove_in_signal_handler() noexcept {
    if (keep_) {
        report_kept();
        return;
    }
    if (c_path_[0] == '\0') return;
#ifdef _WIN32
    try {
        std::error_code ec;
        std::filesystem::remove_all(c_path_, ec);
    } catch (...) {
    }
#else
    // Without allocating: the signal may have interrupted malloc() or free().
    // The subdirectories, opened after the directory, first.
    if (signal_temp_dir == this) {
        for (size_t i = max_signal_dirs; i-- > 0;) {
            if (signal_dirs[i]) remove_entries_in_signal_handler(signal_dirs[i]);
        }
    }
    rmdir(c_path_);
#endif
    c_path_[0] = '\0';
}


} // namespace LCompilers

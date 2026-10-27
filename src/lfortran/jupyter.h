#ifndef LFORTRAN_JUPYTER_H
#define LFORTRAN_JUPYTER_H

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace LCompilers {
struct CompilerOptions;
}

namespace LCompilers::LFortran {

// Runs the Jupyter kernel of a connection file with `options`. The kernel is
// the Xeus one with WITH_XEUS, else the built-in one (jupyter.cpp), which
// needs no third-party library. If the file does not exist, the built-in
// kernel listens on free ports and writes it. A `managed` kernel
// (`lfortran kernel --managed`) is always the built-in one: it stops when its
// last client disconnects and removes the file. The prompt does not use that
// flag. It ends the kernel it started, which also stops with the prompt
// (stop_with_parent).
int run_kernel(const std::string &connection_filename, CompilerOptions &options,
    bool managed);

} // namespace LCompilers::LFortran

namespace LCompilers::LFortran::jupyter {

class Peer;

// The prompt's Jupyter client: it runs cells in a kernel, prints their output
// and the other clients' cells and output, and answers their READs from the
// terminal.
class Client {
    std::string key, session;
    std::unique_ptr<Peer> shell, stdin_, iopub;
    // The kernel that start() started: its process ID, or its process
    // handle on Windows. Empty for a client that only joined.
    intptr_t process = 0;
    // The connection file start() created. Removed when this client ends
    // the kernel.
    std::string owned_file;

    // Shows what the kernel published before it closed its connections.
    void show_last(const std::string &request_id);
public:
    // Connects to the kernel of a connection file.
    explicit Client(const std::string &connection_file);
    ~Client();
    Client(const Client &) = delete;
    Client &operator=(const Client &) = delete;

    // Starts `executable kernel` with `arguments` (the prompt's own) on a new
    // connection file in the temporary directory, which it returns in
    // `connection_file` for other clients, and connects to it. This client
    // ends that kernel when it is destroyed, and on Ctrl-C.
    static std::unique_ptr<Client> start(const std::string &executable,
        const std::vector<std::string> &arguments, std::string &connection_file);

    // Runs a cell; `verbose` also shows its AST, ASR and LLVM IR. Returns
    // false if the kernel has gone.
    bool execute(const std::string &code, bool verbose = false);
    // Prints what the other clients ran since the last call.
    void show_others();
    // Waits at most `timeout` ms for the kernel that start() started to
    // exit; returns its exit status, or -1 if it has not exited, did not end
    // normally or was not started by this client.
    int kernel_status(int timeout);
};

// Stops this process, a kernel, once the process `parent` has exited, and
// removes `connection_file`, which `parent` wrote (`lfortran kernel
// --parent`); the kernel also removes it when it exits by itself. The prompt
// starts its kernel so, which then stops with the prompt however the prompt
// ends: also if it is killed or its terminal closes, when it cannot end the
// kernel itself. A parent that has already exited stops the kernel at once.
// Returns false, and does nothing, if `parent` is not a process ID or is a
// running process other than this process's parent.
bool stop_with_parent(long long parent, const std::string &connection_file);

// Makes Ctrl-C end the prompt, after it ends the kernel that the prompt
// started (Client::start), removes the kernel's connection file and restores
// the terminal. The prompt still ends as Ctrl-C ends it by default. Does
// nothing if Ctrl-C is ignored, as for a background job.
void handle_interrupt();

#ifndef __EMSCRIPTEN__

// The built-in kernel of a connection file (see run_kernel).
int run_builtin_kernel(const std::string &connection_file, CompilerOptions &options,
    bool managed);

// A kernel's stdin channel, on which a READ asks the client that ran the
// cell: Jupyter clients connect it with the identity of their shell socket.
// It keeps its connections on a thread of its own, and tells when the client
// asked disconnects, which the stdin channel of Xeus does not.
class InputChannel {
    struct Impl;
    std::unique_ptr<Impl> impl;
public:
    // Listens on `port` of `ip`, or on a free port if it is 0, which `port`
    // then becomes; `key` signs the messages.
    InputChannel(const std::string &ip, int &port, const std::string &key);
    ~InputChannel();
    InputChannel(const InputChannel &) = delete;
    InputChannel &operator=(const InputChannel &) = delete;

    // Sends the client of `identity` an input_request of four JSON `parts`
    // (header, parent header, metadata and content).
    void ask(const std::string &identity, const std::string parts[4]);
    // Waits at most `timeout` ms for the answer: 1 with its value, -1 if that
    // client is not connected or has disconnected, 0 if it is still awaited.
    int wait(std::string &value, int timeout);
    // Asks and waits as long as the client is connected; false if it is not.
    bool request(const std::string &identity, const std::string parts[4],
        std::string &value);
    // Whether it can listen on `ip`: an IPv4 address
    static bool serves(const std::string &ip);
};

#endif

} // namespace LCompilers::LFortran::jupyter

#endif // LFORTRAN_JUPYTER_H

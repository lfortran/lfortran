#ifndef LFORTRAN_JUPYTER_H
#define LFORTRAN_JUPYTER_H

#include <memory>
#include <string>
#include <vector>

namespace LCompilers {
struct CompilerOptions;
}

namespace LCompilers::LFortran {

// Runs the Jupyter kernel of a connection file with `options`. The kernel is
// the Xeus one with WITH_XEUS, else the built-in one (jupyter.cpp), which
// needs no third-party library. If the file does not exist, the kernel is the
// built-in one, which listens on free ports and writes it. A `managed`
// kernel, the one the prompt starts, is always the built-in one: it stops
// when its last client disconnects and removes the file.
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
public:
    // Connects to the kernel of a connection file.
    explicit Client(const std::string &connection_file);
    ~Client();
    Client(const Client &) = delete;
    Client &operator=(const Client &) = delete;

    // Starts `executable kernel` with `arguments` (the prompt's own) on a new
    // connection file in the temporary directory, which it returns in
    // `connection_file` for other clients, and connects to it. A `managed`
    // kernel stops when its last client disconnects.
    static std::unique_ptr<Client> start(const std::string &executable,
        const std::vector<std::string> &arguments, bool managed,
        std::string &connection_file);

    // Runs a cell; `verbose` also shows its AST, ASR and LLVM IR. Returns
    // false if the kernel has gone.
    bool execute(const std::string &code, bool verbose = false);
    // Prints what the other clients ran since the last call.
    void show_others();
};

#ifndef __EMSCRIPTEN__

// The built-in kernel of a connection file (see run_kernel).
int run_builtin_kernel(const std::string &connection_file, CompilerOptions &options,
    bool managed);

// A kernel's stdin channel, on which a READ asks the client that ran the
// cell: Jupyter clients connect it with the identity of their shell socket.
// It keeps its connections on a thread of its own, and tells when the client
// asked disconnects.
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
};

#endif

} // namespace LCompilers::LFortran::jupyter

#endif // LFORTRAN_JUPYTER_H

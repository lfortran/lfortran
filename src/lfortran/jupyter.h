#ifndef LFORTRAN_JUPYTER_H
#define LFORTRAN_JUPYTER_H

#include <memory>
#include <string>
#include <vector>

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
    // Asks the kernel to stop.
    void shutdown();
};

} // namespace LCompilers::LFortran::jupyter

#endif // LFORTRAN_JUPYTER_H

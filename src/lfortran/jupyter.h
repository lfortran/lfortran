#ifndef LFORTRAN_JUPYTER_H
#define LFORTRAN_JUPYTER_H

#include <memory>
#include <string>

namespace LCompilers {
struct CompilerOptions;
}

namespace LCompilers::LFortran {

// Runs the Jupyter kernel of a connection file with `options`. The kernel is
// the Xeus one with WITH_XEUS, else the built-in one (jupyter.cpp), which
// needs no third-party library. If the file does not exist, the built-in
// kernel listens on free ports and writes it. A `managed` kernel, started with
// `--managed`, is always the built-in one: it stops when its last client
// disconnects and removes the file.
int run_kernel(const std::string &connection_filename, CompilerOptions &options,
    bool managed);

} // namespace LCompilers::LFortran

namespace LCompilers::LFortran::jupyter {

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

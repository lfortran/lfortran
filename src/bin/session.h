#ifndef LFORTRAN_SESSION_H
#define LFORTRAN_SESSION_H

#include <string>

#include <libasr/utils.h>

namespace LCompilers::LFortran {

// Serves one interactive session on a Unix socket at `path`: the clients that
// connect share one FortranEvaluator, and their cells run one at a time, in
// the order they arrive, on this single thread. See doc/src/usage.md.
int serve_session(CompilerOptions &options, const std::string &path);

// A terminal's connection to a session server.
class SessionClient {
    int fd;
public:
    explicit SessionClient(const std::string &path);
    ~SessionClient();
    SessionClient(const SessionClient &) = delete;
    SessionClient &operator=(const SessionClient &) = delete;
    // Runs `code` in the session, prints its output and result and answers
    // its READs from stdin. Returns false if the server has gone.
    bool execute(const std::string &code);
};

} // namespace LCompilers::LFortran

#endif // LFORTRAN_SESSION_H

#ifndef LIBASR_TEMP_DIR_H
#define LIBASR_TEMP_DIR_H

#include <csignal>
#include <cstddef>
#include <stdexcept>
#include <string>
#include <vector>

namespace LCompilers {

// The directory could not be created
class TempDirError : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

// A private directory for the intermediate files of one compiler invocation,
// `<prefix>-<hostname>-<pid>-<random>` in the system temporary directory
// (TMPDIR, else /tmp; GetTempPath on Windows), readable only by its owner.
// It is created when a file in it is first needed, and removed with
// everything in it by remove() or the destructor, unless kept.
class TempDir {
public:
    explicit TempDir(const std::string &prefix) : prefix_(prefix) {}
    ~TempDir();
    TempDir(const TempDir &) = delete;
    TempDir &operator=(const TempDir &) = delete;

    // The directory, created on the first call; throws TempDirError
    const std::string &path();
    // The path of the file `name` in the directory, which is created
    std::string file(const std::string &name);
    // The path of the file `name` in the subdirectory `subdir` of the
    // directory, which are created. The signal handlers remove the files
    // of up to three subdirectories.
    std::string file(const std::string &subdir, const std::string &name);
    bool created() const { return !path_.empty(); }
    // Keep the directory instead of removing it
    void keep(bool keep) { keep_ = keep; }
    bool kept() const { return keep_; }
    // If the directory was created and is kept, write `<prefix>: temporary
    // files kept in <directory>` to standard error, once. It does not
    // allocate, so the signal handlers call it too.
    void report_kept() noexcept;
    // Remove the directory and its contents, unless kept. Never throws.
    void remove() noexcept;
    // Also remove the directory, unless kept, when the process is ended by
    // a signal (SIGHUP, SIGINT, SIGPIPE, SIGQUIT, SIGTERM, a crash: SIGABRT,
    // SIGBUS, SIGFPE, SIGILL, SIGSEGV, or a limit: SIGXCPU, SIGXFSZ) or, on
    // Windows, by a console control event. SIGKILL cannot be caught. The
    // handlers are installed when the directory is created, so a process
    // that never creates it keeps its signal handling. Signals that were
    // ignored stay ignored. Only one TempDir can do this.
    void remove_on_signal() { remove_on_signal_ = true; }
    // Remove the directory from a signal handler, without allocating; or,
    // if it is kept, report_kept()
    void remove_in_signal_handler() noexcept;
    // The directory once created, else "", in a buffer that never moves,
    // for a signal handler that cannot allocate
    const char *c_path() const noexcept { return c_path_; }

    // The system temporary directory
    static std::string system_temp_dir();
    // Remove the directories `<prefix>-<this host>-<pid>-<random>` in the
    // system temporary directory that belong to this user and to processes
    // that no longer exist: those of a process that was killed (SIGKILL).
    // Symbolic links are left alone. Returns the removed directories.
    static std::vector<std::string> remove_stale(const std::string &prefix);

private:
    std::string prefix_;
    std::string path_;
    bool keep_ = false;
    bool remove_on_signal_ = false;
    char c_path_[4096] = {};
    // The message of report_kept(), made when the directory is created
    char kept_message_[4200] = {};
    std::size_t kept_message_size_ = 0;
    volatile std::sig_atomic_t kept_reported_ = 0;
};

} // namespace LCompilers

#endif // LIBASR_TEMP_DIR_H

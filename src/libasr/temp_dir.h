#ifndef LIBASR_TEMP_DIR_H
#define LIBASR_TEMP_DIR_H

#include <stdexcept>
#include <string>

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
    ~TempDir() { remove(); }
    TempDir(const TempDir &) = delete;
    TempDir &operator=(const TempDir &) = delete;

    // The directory, created on the first call; throws TempDirError
    const std::string &path();
    // The path of the file `name` in the directory, which is created
    std::string file(const std::string &name);
    bool created() const { return !path_.empty(); }
    // Keep the directory instead of removing it
    void keep(bool keep) { keep_ = keep; }
    // Remove the directory and its contents, unless kept. Never throws.
    void remove() noexcept;
    // The directory once created, else "", in a buffer that never moves,
    // for a signal handler that cannot allocate
    const char *c_path() const noexcept { return c_path_; }

    // The system temporary directory
    static std::string system_temp_dir();

private:
    std::string prefix_;
    std::string path_;
    bool keep_ = false;
    char c_path_[4096] = {};
};

} // namespace LCompilers

#endif // LIBASR_TEMP_DIR_H

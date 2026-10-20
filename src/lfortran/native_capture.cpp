#include <lfortran/native_capture.h>
#include <llvm/Support/JSON.h>
#include <condition_variable>
#include <algorithm>
#include <chrono>
#include <cstdio>
#include <iostream>
#include <mutex>
#include <thread>
#include <stdexcept>
#ifdef _WIN32
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#else
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <cerrno>
#endif

namespace LCompilers::LFortran {
namespace {
int duplicate_fd(int fd) {
#ifdef _WIN32
    return _dup(fd);
#else
    return dup(fd);
#endif
}
void replace_fd(int from, int to) {
#ifdef _WIN32
    if (_dup2(from, to)) throw std::runtime_error("cannot redirect kernel stream");
#else
    if (dup2(from, to) < 0) throw std::runtime_error("cannot redirect kernel stream");
#endif
}
void close_fd(int fd) {
    if (fd < 0) return;
#ifdef _WIN32
    _close(fd);
#else
    close(fd);
#endif
}
}
struct NativeCapture::Impl {
    std::function<void(const std::string &, const std::string &)> sink;
    int pipes[2][2]{{-1,-1},{-1,-1}}, saved[2]{-1,-1};
#ifndef _WIN32
    int wake[2]{-1,-1};
#endif
    std::thread reader;
    std::mutex mutex;
    std::condition_variable changed;
    uint64_t requested = 0, completed = 0;
    bool stop = false, finish_utf8 = false;
    std::string pending[2];

    void emit(int index, bool final) {
        auto &bytes = pending[index];
        size_t size = bytes.size();
        if (!final && size) {
            // Retain only a possibly incomplete final codepoint. Invalid
            // complete sequences are replaced, never passed into LLVM JSON.
            size_t start = size - 1;
            while (start && (static_cast<unsigned char>(bytes[start]) & 0xc0) == 0x80
                    && size-start < 4) --start;
            unsigned char lead = bytes[start];
            size_t needed = lead >= 0xf0 && lead <= 0xf4 ? 4 :
                lead >= 0xe0 && lead <= 0xef ? 3 :
                lead >= 0xc2 && lead <= 0xdf ? 2 : 1;
            if (size-start < needed) size = start;
        }
        if (size) {
            sink(index ? "stderr" : "stdout",
                llvm::json::fixUTF8(bytes.substr(0, size)));
            bytes.erase(0, size);
        }
    }
    void drain(int index) {
        char bytes[8192];
        for (;;) {
#ifdef _WIN32
            DWORD available = 0;
            HANDLE handle = reinterpret_cast<HANDLE>(_get_osfhandle(pipes[index][0]));
            if (!PeekNamedPipe(handle, nullptr, 0, nullptr, &available, nullptr)
                    || !available) break;
            int n = _read(pipes[index][0], bytes,
                static_cast<unsigned>(std::min<size_t>(sizeof(bytes), available)));
#else
            auto n = read(pipes[index][0], bytes, sizeof(bytes));
            if (n < 0 && errno == EINTR) continue;
#endif
            if (n <= 0) break;
            pending[index].append(bytes, static_cast<size_t>(n));
            emit(index, false);
        }
    }
    void run() {
        for (;;) {
#ifndef _WIN32
            pollfd fds[3]{{pipes[0][0], POLLIN, 0},
                {pipes[1][0], POLLIN, 0}, {wake[0], POLLIN, 0}};
            int result;
            do { result = poll(fds, 3, -1); } while (result < 0 && errno == EINTR);
            if (result < 0) std::terminate();
            char bytes[64];
            while (read(wake[0], bytes, sizeof(bytes)) > 0) {}
#else
            // Anonymous CRT pipes cannot be waited on as overlapped handles.
            // The condition wakes fences immediately; timed polling drains
            // ordinary writes continuously, independently of evaluation.
            std::unique_lock<std::mutex> wait(mutex);
            changed.wait_for(wait, std::chrono::milliseconds(5),
                [&] { return requested != completed || stop; });
            wait.unlock();
#endif
            drain(0);
            drain(1);
            std::lock_guard<std::mutex> lock(mutex);
            if (requested != completed) {
                // The writer flushed and is paused until this acknowledgment.
                drain(0);
                drain(1);
                if (finish_utf8) { emit(0, true); emit(1, true); }
                completed = requested;
                changed.notify_all();
            }
            if (stop) break;
        }
    }
    void signal() {
#ifndef _WIN32
        char byte = 0;
        if (write(wake[1], &byte, 1) < 0 && errno != EAGAIN) std::terminate();
#endif
        changed.notify_all();
    }
    explicit Impl(std::function<void(const std::string &,
            const std::string &)> callback) : sink(std::move(callback)) {
        std::cout.flush();
        std::cerr.flush();
        std::fflush(nullptr);
        try {
            for (int i = 0; i < 2; ++i) {
#ifdef _WIN32
                if (_pipe(pipes[i], 65536, _O_BINARY | _O_NOINHERIT))
#else
                if (pipe(pipes[i]))
#endif
                    throw std::runtime_error("cannot create kernel capture pipe");
#ifndef _WIN32
                fcntl(pipes[i][0], F_SETFL, O_NONBLOCK);
                for (int fd : pipes[i]) fcntl(fd, F_SETFD, FD_CLOEXEC);
#endif
                saved[i] = duplicate_fd(i+1);
                if (saved[i] < 0) throw std::runtime_error("cannot save kernel stream");
                replace_fd(pipes[i][1], i+1);
            }
#ifndef _WIN32
            if (pipe(wake)) throw std::runtime_error("cannot create capture wake pipe");
            for (int fd : wake) {
                fcntl(fd, F_SETFL, O_NONBLOCK);
                fcntl(fd, F_SETFD, FD_CLOEXEC);
            }
#endif
            reader = std::thread([this] { run(); });
        } catch (...) { restore(); throw; }
    }
    void restore() {
        for (int i = 0; i < 2; ++i) {
            if (saved[i] >= 0) {
                replace_fd(saved[i], i+1);
                close_fd(saved[i]);
            }
            for (int fd : pipes[i]) close_fd(fd);
        }
#ifndef _WIN32
        for (int fd : wake) close_fd(fd);
#endif
    }
    void fence(bool final) {
        std::cout.flush();
        std::cerr.flush();
        std::fflush(nullptr);
        std::unique_lock<std::mutex> lock(mutex);
        finish_utf8 = final;
        const auto target = ++requested;
        signal();
        changed.wait(lock, [&] { return completed >= target; });
    }
    ~Impl() {
        fence(true);
        {
            std::lock_guard<std::mutex> lock(mutex);
            stop = true;
            signal();
        }
        reader.join();
        restore();
    }
};
NativeCapture::NativeCapture(std::function<void(const std::string &,
        const std::string &)> sink) : impl(new Impl(std::move(sink))) {}
NativeCapture::~NativeCapture() = default;
void NativeCapture::fence(bool final) { impl->fence(final); }
}

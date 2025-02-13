#pragma once

#include <atomic>
#include <condition_variable>
#include <cstddef>
#include <functional>
#include <mutex>
#include <queue>
#include <string>
#include <thread>
#include <vector>

#include <server/logger.h>

namespace LCompilers::LLanguageServer::Threading {
    namespace lsl = LCompilers::LLanguageServer::Logging;

    typedef std::function<void(
        const std::string &name,
        const std::size_t threadId
    )> Task;

    class ThreadPool {
    public:
        ThreadPool(
            const std::string &name,
            std::size_t numThreads,
            lsl::Logger &logger
        );

        inline auto name() const -> const std::string & {
            return _name;
        }

        inline auto numThreads() const -> std::size_t {
            return _numThreads;
        }

        inline auto isRunning() const -> bool {
            return running;
        }

        auto execute(Task task) -> bool;
        auto stop() -> void;
        auto stopNow() -> void;
        auto join() -> void;
    protected:
        const std::string _name;
        std::size_t _numThreads;
        lsl::Logger &logger;
        std::vector<std::thread> workers;
        std::atomic_bool running = true;
        std::atomic_bool stopRunning = false;
        std::atomic_bool stopRunningNow = false;
        std::mutex taskMutex;
        std::condition_variable taskAvailable;
        std::queue<Task> tasks;
        std::atomic_size_t size;

        auto run(const std::size_t threadId) -> void;
    };

} // namespace LCompilers::LLanguageServer::Threading

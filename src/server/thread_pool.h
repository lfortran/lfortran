#pragma once

#include <atomic>
#include <cstddef>
#include <functional>
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <utility>
#include <vector>

#include <server/logger.h>
#include <server/queue.hpp>

namespace LCompilers::LLanguageServer::Threading {
    namespace lsl = LCompilers::LLanguageServer::Logging;

    const std::size_t TASK_QUEUE_CAPACITY = 256;

    typedef std::function<void(
        std::shared_ptr<std::atomic_bool> running
    )> Task;

    typedef std::pair<Task, std::shared_ptr<std::atomic_bool>> QueueElem;

    template class Queue<QueueElem, TASK_QUEUE_CAPACITY>;

    typedef Queue<QueueElem, TASK_QUEUE_CAPACITY> TaskQueue;

    class ThreadPool {
    public:
        ThreadPool(
            const std::string &name,
            std::size_t numThreads,
            lsl::Logger &logger
        );

        auto name() const -> const std::string &;
        auto numThreads() -> std::size_t;
        auto numActive() const -> std::size_t;
        auto numPending() const -> std::size_t;
        auto numExecuted() const -> std::size_t;
        auto isRunning() const -> bool;
        auto hasCapacity() -> bool;

        auto ensureCapacity() -> void;
        auto grow(std::size_t size) -> std::size_t;
        auto execute(Task task) -> std::shared_ptr<std::atomic_bool>;
        auto stop() -> void;
        auto stopNow() -> void;
        auto join() -> void;
    protected:
        const std::string _name;
        lsl::Logger logger;
        TaskQueue tasks;
        std::vector<std::thread> workers;
        std::recursive_mutex workerMutex;
        std::atomic_bool running = true;
        std::atomic_bool stopRunning = false;
        std::atomic_bool stopRunningNow = false;
        std::atomic_size_t activeCount = 0;
        std::atomic_size_t nextGrowSize = 1;
        std::atomic_size_t m_executed = 0;

        auto run(const std::size_t threadId) -> void;
    };

} // namespace LCompilers::LLanguageServer::Threading

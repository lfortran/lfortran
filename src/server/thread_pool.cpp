#include <iostream>
#include <exception>
#include <memory>
#include <mutex>

#include <server/thread_pool.h>

namespace LCompilers::LLanguageServer::Threading {
    using namespace std::literals::chrono_literals;

    ThreadPool::ThreadPool(
        const std::string &name,
        std::size_t numThreads,
        lsl::Logger &logger
    ) : _name(name)
      , logger(logger.having("ThreadPool", {name}))
      , tasks(this->logger, name + "-queue")
    {
        grow(numThreads);
    }

    auto ThreadPool::name() const -> const std::string & {
        return _name;
    }

    auto ThreadPool::numThreads() -> std::size_t {
        std::unique_lock<std::recursive_mutex> lock(workerMutex);
        return workers.size();
    }

    auto ThreadPool::numActive() const -> std::size_t {
        return activeCount;
    }

    auto ThreadPool::numPending() const -> std::size_t {
        return tasks.size();
    }

    auto ThreadPool::numExecuted() const -> std::size_t {
        return m_executed;
    }

    auto ThreadPool::isRunning() const -> bool {
        return running;
    }

    auto ThreadPool::ensureCapacity() -> void {
        // Doubles the number of threads allocated each successive time this
        // method is called while the capacity remains insufficient. Once a
        // sufficient capacity is obtained the momentum parameter `nextGrowSize`
        // is reset to 1.
        if (!hasCapacity()) {
            std::unique_lock<std::recursive_mutex> lock(workerMutex);
            if (!hasCapacity()) {
                logger.debug()
                    << "Growing by " << nextGrowSize << " thread(s)."
                    << std::endl;
                grow(nextGrowSize);
                nextGrowSize = (nextGrowSize << 1);
            }
        } else {
            nextGrowSize = 1;
        }
    }

    auto ThreadPool::hasCapacity() -> bool {
        return activeCount < numThreads();
    }

    auto ThreadPool::grow(std::size_t size) -> std::size_t {
        std::unique_lock<std::recursive_mutex> lock(workerMutex);
        workers.reserve(workers.size() + size);
        for (std::size_t i = 0; i < size; ++i) {
            std::size_t threadId = workers.size();
            logger.debug()
                << "Starting thread " << _name << "_" << threadId
                << std::endl;
            workers.emplace_back([this, threadId]() {
                run(threadId);
            });
        }
        return size;
    }

    auto ThreadPool::execute(Task task) -> std::shared_ptr<std::atomic_bool> {
        if (!stopRunning) {
            QueueElem *elem = tasks.enqueue(
                std::make_pair(task, std::make_shared<std::atomic_bool>(true))
            );
            if (elem) {
                return elem->second;
            }
        }
        return nullptr;
    }

    auto ThreadPool::stop() -> void {
        logger.debug()
            << "Thread pool will no longer accept new tasks and "
               "will shut down once those pending have returned."
            << std::endl;
        stopRunning = true;
        tasks.stop();
    }

    auto ThreadPool::stopNow() -> void {
        logger.debug()
            << "Stopping thread pool as quickly as possible."
            << std::endl;
        stopRunning = true;
        stopRunningNow = true;
        tasks.stopNow();
    }

    auto ThreadPool::join() -> void {
        std::unique_lock<std::recursive_mutex> lock(workerMutex);
        for (std::thread &worker : workers) {
            if (worker.joinable()) {
                worker.join();
                logger.debug() << "Terminated thread." << std::endl;
            }
        }
        running = false;
    }

    auto ThreadPool::run(const std::size_t threadId) -> void {
        try {
            logger.threadName(_name + "_" + std::to_string(threadId));
            while (!stopRunningNow && (!stopRunning || (tasks.size() > 0))) {
                QueueElem elem = tasks.dequeue();
                Task &task = elem.first;
                std::shared_ptr<std::atomic_bool> taskIsRunning = elem.second;
                if (!stopRunningNow && *taskIsRunning) {
                    ++activeCount;
                    try {
                        task(std::move(taskIsRunning));
                    } catch (std::exception &e) {
                        logger.error() << "Failed to execute task: " << e.what() << std::endl;
                    }
                    --activeCount;
                    ++m_executed;
                }
            }
            logger.trace() << "Terminated successfully." << std::endl;
        } catch (std::exception &e) {
            if (e.what() != DEQUEUE_FAILED_MESSAGE) {
                logger.error()
                    << "Unhandled exception caught: " << e.what()
                    << std::endl;
            } else {
                logger.trace()
                    << "Interrupted while dequeuing messages: " << e.what()
                    << std::endl;
            }
        } catch (...) {
            logger.error()
                << "Unhandled exception caught: unknown"
                << std::endl;
        }
    }

} // namespace LCompilers::LLanguageServer::Threading

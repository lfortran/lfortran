#include <chrono>
#include <iostream>
#include <exception>

#include <server/thread_pool.h>

namespace LCompilers::LLanguageServer::Threading {
    using namespace std::literals::chrono_literals;

    ThreadPool::ThreadPool(
        const std::string &name,
        std::size_t numThreads,
        lsl::Logger &logger
    ) : _name(name)
        , _numThreads(numThreads)
        , logger(logger)
    {
        for (std::size_t threadId = 0; threadId < numThreads; ++threadId) {
            logger.debug()
                << "Starting thread " << name << "_" << threadId
                << std::endl;
            workers.emplace_back([this, threadId]() {
                run(threadId);
            });
        }
    }

    auto ThreadPool::execute(Task task) -> bool {
        if (!stopRunning) {
            std::unique_lock<std::mutex> taskLock(taskMutex);
            if (!stopRunning) {
                tasks.push(task);
                ++size;
                taskAvailable.notify_one();
                return true;
            }
        }
        return false;
    }

    auto ThreadPool::stop() -> void {
        logger.debug()
            << "Thread pool [" << _name << "] will no longer accept new tasks and "
            << "will shut down once those pending have returned."
            << std::endl;
        stopRunning = true;
    }

    auto ThreadPool::stopNow() -> void {
        logger.debug()
            << "Stopping thread pool [" << _name << "] as quickly as possible."
            << std::endl;
        stopRunning = true;
        stopRunningNow = true;
        {
            std::unique_lock<std::mutex> taskLock(taskMutex);
            taskAvailable.notify_all();
        }
    }

    auto ThreadPool::join() -> void {
        for (std::size_t threadId = 0; threadId < _numThreads; ++threadId) {
            std::thread &worker = workers[threadId];
            if (worker.joinable()) {
                worker.join();
            }
        }
        running = false;
    }

    auto ThreadPool::run(const std::size_t threadId) -> void {
        try {
            while (!stopRunningNow && (!stopRunning || (size > 0))) {
                std::unique_lock<std::mutex> taskLock(taskMutex);
                taskAvailable.wait(taskLock, [this]{
                    return (size > 0) || stopRunningNow;
                });
                if ((size > 0) && !stopRunningNow) {
                    Task task = tasks.front();
                    tasks.pop();
                    --size;
                    taskLock.unlock();
                    try {
                        task(_name, threadId);
                    } catch (std::exception &e) {
                        logger.error()
                            << "[" << _name << "_" << threadId << "] "
                            << "Failed to execute task: " << e.what()
                            << std::endl;
                    }
                }
            }
            logger.debug()
                << "[" << _name << "_" << threadId << "] "
                << "Terminated."
                << std::endl;
        } catch (std::exception &e) {
            logger.error()
                << "[" << _name << "_" << threadId << "] "
                << "Unhandled exception caught: " << e.what()
                << std::endl;
        }
    }

} // namespace LCompilers::LLanguageServer::Threading

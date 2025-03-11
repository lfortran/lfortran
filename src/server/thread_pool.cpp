#include "server/queue.hpp"
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
      , tasks(logger)
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
            tasks.enqueue(task);
            return true;
        }
        return false;
    }

    auto ThreadPool::stop() -> void {
        logger.debug()
            << "Thread pool [" << _name << "] will no longer accept new tasks and "
            "will shut down once those pending have returned."
            << std::endl;
        stopRunning = true;
        tasks.stop();
    }

    auto ThreadPool::stopNow() -> void {
        logger.debug()
            << "Stopping thread pool [" << _name << "] as quickly as possible."
            << std::endl;
        stopRunning = true;
        stopRunningNow = true;
        tasks.stopNow();
    }

    auto ThreadPool::join() -> void {
        for (std::size_t threadId = 0; threadId < _numThreads; ++threadId) {
            std::thread &worker = workers[threadId];
            if (worker.joinable()) {
                worker.join();
                logger.debug()
                    << "Thread " << _name << "_" << threadId << " terminated."
                    << std::endl;
            }
        }
        running = false;
    }

    auto ThreadPool::run(const std::size_t threadId) -> void {
        try {
            while (!stopRunningNow && (!stopRunning || (tasks.size() > 0))) {
                Task task = tasks.dequeue();
                if (!stopRunningNow) {
                    try {
                        task(_name, threadId);
                    } catch (std::exception &e) {
                        logger.error()
                            << "[" << _name << "_" << threadId << "] "
                            "Failed to execute task: " << e.what()
                            << std::endl;
                    }
                }
            }
            logger.trace()
                << "[" << _name << "_" << threadId << "] "
                "Terminated successfully."
                << std::endl;
        } catch (std::exception &e) {
            if (e.what() != DEQUEUE_FAILED_MESSAGE) {
                logger.error()
                    << "[" << _name << "_" << threadId << "] "
                    "Unhandled exception caught: " << e.what()
                    << std::endl;
            } else {
                logger.trace()
                    << "[" << _name << "_" << threadId << "] "
                    "Interrupted while dequeuing messages: " << e.what()
                    << std::endl;
            }
        }
    }

} // namespace LCompilers::LLanguageServer::Threading

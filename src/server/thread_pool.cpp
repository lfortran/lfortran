#include <iostream>
#include <exception>

#include <memory>
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
                    try {
                        task(std::move(taskIsRunning));
                    } catch (std::exception &e) {
                        logger.error() << "Failed to execute task: " << e.what() << std::endl;
                    }
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
        }
    }

} // namespace LCompilers::LLanguageServer::Threading

#pragma once

#include <chrono>
#include <map>
#include <mutex>
#include <shared_mutex>
#include <string>
#include <unordered_map>

#include <server/logger.h>
#include <server/queue.hpp>
#include <server/thread_pool.h>
#ifdef DEBUG
#include <server/observable_lock.hpp>
#endif // DEBUG

namespace LCompilers::LLanguageServer {
    namespace lsl = LCompilers::LLanguageServer::Logging;
    namespace lst = LCompilers::LLanguageServer::Threading;

    typedef std::chrono::system_clock::time_point time_point_t;

    class LanguageServer;

#ifdef DEBUG
    typedef lst::ObservableLock<LanguageServer, std::shared_lock<std::shared_mutex>> ReadLock;
    typedef lst::ObservableLock<LanguageServer, std::unique_lock<std::shared_mutex>> WriteLock;
    typedef lst::ObservableLock<LanguageServer, std::unique_lock<std::mutex>> MutexLock;
#else
    typedef std::shared_lock<std::shared_mutex> ReadLock;
    typedef std::unique_lock<std::shared_mutex> WriteLock;
    typedef std::unique_lock<std::mutex> MutexLock;
#endif // DEBUG

    const std::size_t MESSAGE_QUEUE_CAPACITY = 64;

    template class lst::Queue<std::string, MESSAGE_QUEUE_CAPACITY>;

    typedef lst::Queue<std::string, MESSAGE_QUEUE_CAPACITY> MessageQueue;

#ifdef DEBUG
    struct OwnerRecord {
        std::string thread;
        const char *file;
        int line;
        time_point_t timestamp;
        OwnerRecord(
            const std::string &thread,
            const char *file,
            int line,
            time_point_t timestamp
        );
    }; // struct OwnerRecord
#endif // DEBUG

    class LanguageServer {
    public:
        virtual ~LanguageServer();
        virtual bool isTerminated() const = 0;
        virtual void join();

        virtual auto formatException(
            const std::string &heading,
            const std::exception_ptr &exception_ptr
        ) const -> std::string;
    protected:
        LanguageServer(
            MessageQueue &incomingMessages,
            MessageQueue &outgoingMessages,
            lsl::Logger &logger
        );
        MessageQueue &incomingMessages;
        MessageQueue &outgoingMessages;
        lsl::Logger logger;

#ifdef DEBUG
        std::unordered_map<
            std::string,
            std::map<std::string, OwnerRecord>
        > waitingById;
        std::unordered_map<std::string, OwnerRecord> ownersById;
        std::shared_mutex ownerMutex;
#endif // DEBUG

        auto send(const std::string &request) -> void;

        virtual void prepare(
            std::string &buffer,
            const std::string &message
        ) const = 0;

        auto readLock(
            const char *file,
            int line,
            std::shared_mutex &mutex,
            std::string &&identifier
        ) -> ReadLock;
        auto writeLock(
            const char *file,
            int line,
            std::shared_mutex &mutex,
            std::string &&identifier
        ) -> WriteLock;
        auto mutexLock(
            const char *file,
            int line,
            std::mutex &mutex,
            std::string &&identifier
        ) -> MutexLock;

#ifdef DEBUG
        template <typename LockType>
        auto acquire(lst::ObservableLock<LanguageServer, LockType> &observable) -> void {
            const std::string &identifier = observable.identifier();
            const std::string &threadName = lsl::Logger::threadName();
            std::unique_lock<std::shared_mutex> writeLock(ownerMutex);
            auto waitingIter = waitingById.find(identifier);
            if (waitingIter == waitingById.end()) {
                waitingById.emplace_hint(
                    waitingIter,
                    std::piecewise_construct,
                    std::forward_as_tuple(identifier),
                    std::forward_as_tuple()
                )->second.emplace(
                    std::piecewise_construct,
                    std::forward_as_tuple(threadName),
                    std::forward_as_tuple(
                        threadName,
                        observable.file(),
                        observable.line(),
                        std::chrono::system_clock::now()
                    )
                );
            } else {
                auto &waitingThreads = waitingIter->second;
                auto threadIter = waitingThreads.find(threadName);
                if (threadIter == waitingThreads.end()) {
                    waitingThreads.emplace_hint(
                        threadIter,
                        std::piecewise_construct,
                        std::forward_as_tuple(threadName),
                        std::forward_as_tuple(
                            threadName,
                            observable.file(),
                            observable.line(),
                            std::chrono::system_clock::now()
                        )
                    );
                } else {
                    OwnerRecord &record = threadIter->second;
                    record.thread = threadName;
                    record.file = observable.file();
                    record.line = observable.line();
                    record.timestamp = std::chrono::system_clock::now();
                }
            }
            writeLock.unlock();
            observable->lock();
            writeLock.lock();
            waitingIter = waitingById.find(identifier);
            if (waitingIter != waitingById.end()) {
                auto &waitingThreads = waitingIter->second;
                auto threadIter = waitingThreads.find(threadName);
                if (threadIter != waitingThreads.end()) {
                    waitingThreads.erase(threadIter);
                    if (waitingThreads.empty()) {
                        waitingById.erase(waitingIter);
                    }
                } else {
                    logger.warn()
                        << "waitingById.find(\"" << identifier
                        << "\")->second.find(\"" << threadName << "\") failed."
                        << std::endl;
                }
            } else {
                logger.warn()
                    << "waitingById.find(\"" << identifier << "\") failed."
                    << std::endl;
            }
            auto ownerIter = ownersById.find(identifier);
            if (ownerIter == ownersById.end()) {
                ownersById.emplace_hint(
                    ownerIter,
                    std::piecewise_construct,
                    std::forward_as_tuple(identifier),
                    std::forward_as_tuple(
                        threadName,
                        observable.file(),
                        observable.line(),
                        std::chrono::system_clock::now()
                    )
                );
            } else {
                OwnerRecord &record = ownerIter->second;
                record.thread = threadName;
                record.file = observable.file();
                record.line = observable.line();
                record.timestamp = std::chrono::system_clock::now();
            }
        }

        template <typename LockType>
        auto release(lst::ObservableLock<LanguageServer, LockType> &observable) -> void {
            if (observable->owns_lock()) {
                std::unique_lock<std::shared_mutex> writeLock(ownerMutex);
                ownersById.erase(ownersById.find(observable.identifier()));
                observable->unlock();
            }
        }

        template <typename ObserverType, typename LockType>
        friend class lst::ObservableLock;
#endif // DEBUG
    };

} // namespace LCompilers::LLanguageServer

#ifdef DEBUG
namespace LCompilers::LLanguageServer::Threading {
    namespace ls = LCompilers::LLanguageServer;
    template class ObservableLock<ls::LanguageServer, std::shared_lock<std::shared_mutex>>;
    template class ObservableLock<ls::LanguageServer, std::unique_lock<std::shared_mutex>>;
    template class ObservableLock<ls::LanguageServer, std::unique_lock<std::mutex>>;
} // namespace LCompilers::LLanguageServer::Threading
#endif // DEBUG

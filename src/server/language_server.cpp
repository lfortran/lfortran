#include <exception>
#include <ostream>

#include <server/language_server.h>

namespace LCompilers::LLanguageServer {

#ifdef DEBUG
    OwnerRecord::OwnerRecord(
        const std::string &thread,
        const char *file,
        int line,
        time_point_t timestamp
    ) : thread(thread)
      , file(file)
      , line(line)
      , timestamp(timestamp)
    {
        // empty
    }
#endif // DEBUG

    LanguageServer::LanguageServer(
        MessageQueue &incomingMessages,
        MessageQueue &outgoingMessages,
        lsl::Logger &logger
    ) : incomingMessages(incomingMessages)
      , outgoingMessages(outgoingMessages)
      , logger(logger.having("LanguageServer"))
    {
#ifdef DEBUG
        waitingById.reserve(32);
        ownersById.reserve(32);
#endif // DEBUG
    }

    LanguageServer::~LanguageServer() {
        // empty
    }

    auto LanguageServer::join() -> void {
        // empty
    }

    auto LanguageServer::send(const std::string &message) -> void {
        static thread_local std::string buffer;
        buffer.clear();
        prepare(buffer, message);
        if (outgoingMessages.enqueue(buffer) == nullptr) {
            logger.error() << "Failed to enqueue message:" << std::endl
                           << buffer << std::endl;
        }
    }

    auto LanguageServer::formatException(
        const std::string &heading,
        const std::exception_ptr &exception_ptr
    ) const -> std::string {
        try {
            if (exception_ptr) {
                std::rethrow_exception(exception_ptr);
            } else {
                return std::string{heading}.append(": ").append("unknown");
            }
        } catch (const std::exception &exception) {
            return std::string{heading}.append(": ").append(exception.what());
        } catch (...) {
            return std::string{heading}.append(": ").append("unknown");
        }
    }

#ifdef DEBUG
    auto LanguageServer::readLock(
        const char *file,
        int line,
        std::shared_mutex &mutex,
        std::string &&identifier
    ) -> ReadLock {
        ReadLock lock(
            *this, file, line,
            // NOTE: A read lock may be acquired by multiple threads,
            // simultaneously, so the identifier must be unique to each to avoid
            // collisions:
            "read:" + lsl::Logger::threadName() + ":" + identifier,
            std::shared_lock<std::shared_mutex>(
                mutex,
                std::defer_lock
            )
        );
        acquire(lock);
        return lock;
    }

    auto LanguageServer::writeLock(
        const char *file,
        int line,
        std::shared_mutex &mutex,
        std::string &&identifier
    ) -> WriteLock {
        WriteLock lock(
            *this, file, line,
            "write:" + identifier,
            std::unique_lock<std::shared_mutex>(
                mutex,
                std::defer_lock
            )
        );
        acquire(lock);
        return lock;
    }

    auto LanguageServer::mutexLock(
        const char *file,
        int line,
        std::mutex &mutex,
        std::string &&identifier
    ) -> MutexLock {
        MutexLock lock(
            *this, file, line,
            "mutex:" + identifier,
            std::unique_lock<std::mutex>(
                mutex,
                std::defer_lock
            )
        );
        acquire(lock);
        return lock;
    }
#endif // DEBUG

} // namespace LCompilers::LLanguageServer

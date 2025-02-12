#pragma once

#include <atomic>
#include <condition_variable>
#include <cstddef>
#include <mutex>
#include <string>

#include <server/logger.h>

namespace LCompilers::LLanguageServer {
  namespace lsl = LCompilers::LLanguageServer::Logging;

  const std::size_t MESSAGE_QUEUE_CAPACITY = 64;

  class MessageQueue {
  public:
    MessageQueue(lsl::Logger &logger);

    inline auto size() const -> std::size_t {
      return _size;
    }

    inline auto isRunning() const -> bool {
      return sending || receiving;
    }

    inline auto isStopped() const -> bool {
      return !isRunning();
    }

    auto enqueue(const std::string &message) -> bool;
    auto dequeue() -> const std::string;
    auto stop() -> void;
    auto stopNow() -> void;
  private:
    lsl::Logger &logger;
    std::string buffer[MESSAGE_QUEUE_CAPACITY];
    std::atomic_bool sending = true;
    std::atomic_bool receiving = true;
    std::atomic_size_t head = 0;
    std::atomic_size_t tail = 0;
    std::atomic_size_t _size = 0;
    std::mutex mutex;
    std::condition_variable enqueued;
    std::condition_variable dequeued;
  };

} // namespace LCompilers::LLanguageServer

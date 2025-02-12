#include <stdexcept>

#include <server/message_queue.h>

namespace LCompilers::LLanguageServer {

  MessageQueue::MessageQueue(lsl::Logger &logger)
    : logger(logger)
  {
    // empty
  }

  auto MessageQueue::enqueue(const std::string &message) -> bool {
    if (receiving) {
      std::unique_lock<std::mutex> lock(mutex);
      dequeued.wait(lock, [this]{
        return (_size < MESSAGE_QUEUE_CAPACITY) || !receiving;
      });
      if ((_size < MESSAGE_QUEUE_CAPACITY) && receiving) {
        buffer[tail] = message;
        tail = (tail + 1) % MESSAGE_QUEUE_CAPACITY;
        ++_size;
        enqueued.notify_one();
        return true;
      }
    }
    return false;
  }

  auto MessageQueue::dequeue() -> const std::string {
    if (sending) {
      std::unique_lock<std::mutex> lock(mutex);
      enqueued.wait(lock, [this]{
        return (_size > 0) || !sending;
      });
      if ((_size > 0) && sending) {
        const std::string &message = buffer[head];
        head = (head + 1) % MESSAGE_QUEUE_CAPACITY;
        --_size;
        dequeued.notify_one();
        return message;
      }
    }
    throw std::runtime_error("Failed to read message from queue.");
  }

  auto MessageQueue::stop() -> void {
    bool expected = true;
    if (receiving.compare_exchange_strong(expected, false)) {
      std::unique_lock<std::mutex> lock(mutex);
      dequeued.notify_all();
    } else {
      throw std::runtime_error("MessageQueue has already been stopped!");
    }
  }

  auto MessageQueue::stopNow() -> void {
    bool expected;

    expected = true;
    if (receiving.compare_exchange_strong(expected, false)) {
      std::unique_lock<std::mutex> lock(mutex);
      dequeued.notify_all();
    }

    expected = true;
    if (sending.compare_exchange_strong(expected, false)) {
      std::unique_lock<std::mutex> lock(mutex);
      enqueued.notify_all();
    }
  }

} // namespace LCompilers::LLanguageServer

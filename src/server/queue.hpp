#pragma once

#include <atomic>
#include <condition_variable>
#include <cstddef>
#include <mutex>

#include <server/logger.h>

namespace LCompilers::LLanguageServer::Threading {
    namespace lsl = LCompilers::LLanguageServer::Logging;

    const std::string DEQUEUE_FAILED_MESSAGE =
        "Failed to read message from queue.";

    template <typename T, std::size_t N>
    class Queue {
    public:
        Queue(lsl::Logger &logger);

        inline auto size() const -> std::size_t {
            return _size;
        }

        inline auto isRunning() const -> bool {
            return sending || receiving;
        }

        inline auto isStopped() const -> bool {
            return !isRunning();
        }

        auto enqueue(T value) -> bool;
        auto dequeue() -> T;
        auto stop() -> void;
        auto stopNow() -> void;
    private:
        lsl::Logger &logger;
        T buffer[N];
        std::atomic_bool sending = true;
        std::atomic_bool receiving = true;
        std::atomic_size_t head = 0;
        std::atomic_size_t tail = 0;
        std::atomic_size_t _size = 0;
        std::mutex mutex;
        std::condition_variable enqueued;
        std::condition_variable dequeued;
    };

    template <typename T, std::size_t N>
    Queue<T,N>::Queue(lsl::Logger &logger)
        : logger(logger)
    {
        // empty
    }

    template <typename T, std::size_t N>
    auto Queue<T,N>::enqueue(T value) -> bool {
        if (receiving) {
            std::unique_lock<std::mutex> lock(mutex);
            dequeued.wait(lock, [this]{
                return (_size < N) || !receiving;
            });
            if ((_size < N) && receiving) {
                buffer[tail] = value;
                tail = (tail + 1) % N;
                ++_size;
                enqueued.notify_one();
                return true;
            }
        }
        return false;
    }

    template <typename T, std::size_t N>
    auto Queue<T,N>::dequeue() -> T {
        if (sending) {
            std::unique_lock<std::mutex> lock(mutex);
            enqueued.wait(lock, [this]{
                return (_size > 0) || !sending;
            });
            if ((_size > 0) && sending) {
                T value = buffer[head];
                head = (head + 1) % N;
                --_size;
                dequeued.notify_one();
                return value;
            }
        }
        throw std::runtime_error(DEQUEUE_FAILED_MESSAGE);
    }

    template <typename T, std::size_t N>
    auto Queue<T,N>::stop() -> void {
        bool expected = true;
        if (receiving.compare_exchange_strong(expected, false)) {
            std::unique_lock<std::mutex> lock(mutex);
            dequeued.notify_all();
        } else {
            throw std::runtime_error("Queue has already been stopped!");
        }
    }

    template <typename T, std::size_t N>
    auto Queue<T,N>::stopNow() -> void {
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

} // namespace LCompilers::LLanguageServer::Threading

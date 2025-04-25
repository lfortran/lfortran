#pragma once

#include <atomic>
#include <condition_variable>
#include <cstddef>
#include <mutex>
#include <string>

#include <server/logger.h>

namespace LCompilers::LLanguageServer::Threading {
    namespace lsl = LCompilers::LLanguageServer::Logging;

    const std::string DEQUEUE_FAILED_MESSAGE =
        "Failed to read message from queue.";

    template <typename T, std::size_t N>
    class Queue {
    public:
        Queue(lsl::Logger &logger, const std::string &name);
        auto size() const -> std::size_t;
        auto seen() const -> std::size_t;
        auto name() const -> const std::string &;
        auto isRunning() const -> bool;
        auto isStopped() const -> bool;
        auto enqueue(T value) -> T *;
        auto dequeue() -> T;
        auto stop() -> void;
        auto stopNow() -> void;
    private:
        lsl::Logger logger;
        const std::string m_name;
        T buffer[N];
        std::atomic_bool sending = true;
        std::atomic_bool receiving = true;
        std::atomic_size_t head = 0;
        std::atomic_size_t tail = 0;
        std::atomic_size_t _size = 0;
        std::atomic_size_t m_seen = 0;
        std::mutex mutex;
        std::condition_variable enqueued;
        std::condition_variable dequeued;
    };

    template <typename T, std::size_t N>
    Queue<T,N>::Queue(lsl::Logger &logger, const std::string &name)
        : logger(logger.having("Queue", {name}))
        , m_name(name)
    {
        // empty
    }

    template <typename T, std::size_t N>
    auto Queue<T,N>::size() const -> std::size_t {
        return _size;
    }

    template <typename T, std::size_t N>
    auto Queue<T,N>::seen() const -> std::size_t {
        return m_seen;
    }

    template <typename T, std::size_t N>
    auto Queue<T,N>::name() const -> const std::string & {
        return m_name;
    }

    template <typename T, std::size_t N>
    auto Queue<T,N>::isRunning() const -> bool {
        return sending || receiving;
    }

    template <typename T, std::size_t N>
    auto Queue<T,N>::isStopped() const -> bool {
        return !isRunning();
    }

    template <typename T, std::size_t N>
    auto Queue<T,N>::enqueue(T value) -> T * {
        if (receiving) {
            std::unique_lock<std::mutex> lock(mutex);
            dequeued.wait(lock, [this]{
                return (_size < N) || !receiving;
            });
            if ((_size < N) && receiving) {
                T *elem = &buffer[tail];
                (*elem) = value;
                tail = (tail + 1) % N;
                ++_size;
                ++m_seen;
                lock.unlock();
                enqueued.notify_one();
                return elem;
            }
            logger.warn()
                << "Failed to add element to queue of size=" << _size
                << ", capacity=" << N << std::endl;
        } else {
            logger.warn() << "Queue is no longer adding values." << std::endl;
        }
        return nullptr;
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
                lock.unlock();
                dequeued.notify_one();
                return value;
            }
            logger.warn()
                << "Failed to return element from queue of size=" << _size
                << ", capacity=" << N << std::endl;
        } else {
            logger.warn() << "Queue is no longer returning values." << std::endl;
        }
        throw std::runtime_error(DEQUEUE_FAILED_MESSAGE);
    }

    template <typename T, std::size_t N>
    auto Queue<T,N>::stop() -> void {
        logger.debug() << "Stopping queue ..." << std::endl;

        bool expected = true;
        if (receiving.compare_exchange_strong(expected, false)) {
            dequeued.notify_all();
        } else {
            throw std::runtime_error("Queue has already been stopped!");
        }
    }

    template <typename T, std::size_t N>
    auto Queue<T,N>::stopNow() -> void {
        logger.trace() << "Stopping queue now!" << std::endl;

        bool expected = true;
        if (receiving.compare_exchange_strong(expected, false)) {
            dequeued.notify_all();
        }

        expected = true;
        if (sending.compare_exchange_strong(expected, false)) {
            enqueued.notify_all();
        }
    }

} // namespace LCompilers::LLanguageServer::Threading

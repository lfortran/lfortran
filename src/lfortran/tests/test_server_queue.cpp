// Regression test for the language server's message queue shutdown.
//
// Queue::stop()/stopNow() set the flags that are used as predicates of the
// queue's condition variables. If those flags are updated without holding
// `mutex`, a notification can be lost: a waiting thread may have already
// evaluated its predicate but not yet registered on the condition variable
// when the notification is sent, after which it sleeps forever. That is what
// made the communication listener (and therefore `lfortran server`) hang on
// shutdown, so the LSP test client timed out while waiting for it to exit.
//
// Each iteration races a blocked waiter against the stop flag and checks that
// the waiter is actually woken up. The delay before stopping is swept with
// fine granularity so the notification lands at different points relative to
// the waiter.
#include <server/logger.h>
#include <server/queue.hpp>

#include <atomic>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <thread>

namespace lsl = LCompilers::LLanguageServer::Logging;
namespace lst = LCompilers::LLanguageServer::Threading;

namespace {

constexpr long NUM_ITERATIONS = 10000;
constexpr unsigned MAX_SPIN = 10000;
constexpr std::size_t QUEUE_CAPACITY = 4;

void spin(unsigned numSpins)
{
    volatile unsigned long accumulator = 0;
    for (unsigned i = 0; i < numSpins; ++i) {
        accumulator += i;
    }
    (void)accumulator;
}

// Returns false if `done` is not set within the timeout, i.e. the waiter never
// woke up. The process is exited directly in that case since the stuck thread
// cannot be recovered.
bool await_done(
    const std::atomic_bool &done,
    std::chrono::milliseconds timeout
)
{
    const auto deadline = std::chrono::steady_clock::now() + timeout;
    while (!done.load(std::memory_order_acquire)) {
        if (std::chrono::steady_clock::now() > deadline) {
            return false;
        }
        std::this_thread::yield();
    }
    return true;
}

} // namespace

int main()
{
    lsl::Logger root("", "test_server_queue");
    root.setLevel(lsl::Level::LOG_LEVEL_OFF);
    lsl::Logger queueLogger = root.having("queue");

    for (long i = 0; i < NUM_ITERATIONS; ++i) {
        const unsigned numSpins =
            static_cast<unsigned>(i % MAX_SPIN);

        // A consumer blocked in dequeue() must be woken by stopNow().
        {
            lst::Queue<int, QUEUE_CAPACITY> queue(queueLogger, "dequeue");
            std::atomic_bool done{false};
            std::thread consumer([&queue, &done]() {
                try {
                    (void)queue.dequeue();
                } catch (...) {
                    // Expected once the queue has been stopped.
                }
                done.store(true, std::memory_order_release);
            });

            spin(numSpins);
            queue.stopNow();

            if (!await_done(done, std::chrono::seconds(5))) {
                std::fprintf(
                    stderr,
                    "FAIL: dequeue() missed the stopNow() wakeup "
                    "(iteration=%ld, spins=%u)\n",
                    i, numSpins
                );
                std::fflush(stderr);
                std::_Exit(1);
            }
            consumer.join();
        }

        // A producer blocked in enqueue() on a full queue must be woken by
        // stop().
        {
            lst::Queue<int, QUEUE_CAPACITY> queue(queueLogger, "enqueue");
            for (std::size_t k = 0; k < QUEUE_CAPACITY; ++k) {
                (void)queue.enqueue(static_cast<int>(k));
            }
            std::atomic_bool done{false};
            std::thread producer([&queue, &done]() {
                (void)queue.enqueue(42);
                done.store(true, std::memory_order_release);
            });

            spin(numSpins);
            queue.stop();

            if (!await_done(done, std::chrono::seconds(5))) {
                std::fprintf(
                    stderr,
                    "FAIL: enqueue() missed the stop() wakeup "
                    "(iteration=%ld, spins=%u)\n",
                    i, numSpins
                );
                std::fflush(stderr);
                std::_Exit(1);
            }
            producer.join();
        }
    }

    std::printf(
        "PASS: %ld iterations without a missed queue-shutdown wakeup\n",
        NUM_ITERATIONS
    );
    return 0;
}

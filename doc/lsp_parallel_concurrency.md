# LSP Parallel Communication Audit

This note summarizes every place in the LFortran LSP stack that relies on
threads, atomics, or explicit synchronization, together with an assessment of
whether the current usage appears correct or potentially buggy.

## CI Guard Rails

- `.github/workflows/Quick-Checks-CI.yml:180-218` runs the LSP pytest suite in
  concurrent mode once under `set -ex`. Any timeout or pytest failure exits the
  shell immediately, so the CI job fails as soon as the concurrent run times
  out (the `timeout` process propagates its non-zero exit status).
- The parallel run underneath the same step already retries three times
  (controlled via the `MAX_ATTEMPTS=3` environment variable) before `exit
  $EXIT_CODE`, so a flaky failure still causes the step (and CI) to fail if all
  attempts exceed the timeout or pytest fails for a logical reason.

## Entry Points & Protocol Plumbing

| Component | Path / Lines | Primitives | Assessment |
|-----------|--------------|------------|------------|
| Launch gating | `src/bin/language_server_interface.cpp:531-569` | `std::atomic_bool start`, `std::condition_variable startChanged`, `std::mutex startMutex` | Safe – the main thread sets `start=true` under the mutex before broadcasting, and both the protocol and server constructors wait on the same condition. |
| STDIO protocol loop | `src/server/communication_protocol.cpp:12-112` | Background `listener` thread, atomic `running`, queue stop signals | Safe – the listener thread waits for `startChanged`, then only touches `incomingMessages`; shutdown toggles `running=false`, stops queues, and joins threads so a timeout or crash fails fast. |
| Message Queue | `src/server/queue.hpp:24-133` | `std::mutex mutex`, two `std::condition_variable`s, atomics guarding producer/consumer state | Safe – both enqueue/dequeue paths hold the same mutex and use the condition-variable predicate form, so spurious wakeups get ignored. `stop()` throws on double-stop, which is useful to catch logic errors. |

## Shared Infrastructure

| Component | Path / Lines | Locks / Atomics | Assessment |
|-----------|--------------|-----------------|------------|
| Logger | `src/server/logger.{h,cpp}:53-146` | `std::recursive_mutex _mutex`, `std::atomic<Level> _level` | Safe – all formatting helpers grab the recursive mutex before writing to file/stderr. Direct `logger <<` calls outside the formatter would need manual locking, but the code base does not do that. |
| LSP message stream logging | `src/server/lsp_message_stream.cpp:80-205` | reuses `Logger::mutex()` | Safe – every warning branch wraps logging in a `std::unique_lock<std::recursive_mutex>`, so dumps cannot interleave. |
| Thread pool | `src/server/thread_pool.{h,cpp}:18-124` | `std::recursive_mutex workerMutex`, `TaskQueue` (which carries its own mutex/condvars), atomics for `running`, `stopRunning`, `activeCount` | Safe – only `ensureCapacity/grow/join` hold the recursive mutex, and tasks are guarded by the queue. No lock-ordering issues were observed. |
| Request bookkeeping | `src/server/lsp_language_server.h:33-75` | `std::shared_mutex requestMutex`, `std::atomic_int serialRequestId`, `std::map<int,std::shared_ptr<RequestMessage>> requestsById` | Safe – every consumer uses the `LSP_READ_LOCK`/`LSP_WRITE_LOCK` macros, so readers/writers coordinate properly. |
| Debug owner tracking | `src/server/language_server.h:24-140` | `std::shared_mutex ownerMutex` | Safe and debug-only: ObservableLock records lock owners/waiters. |

## Base Server State (`src/server/base_lsp_language_server.*`)

- Atomics: `_initialized`, `_shutdown`, `_exit`, `_initialized`, `trace`,
  `serialSendId`, `pendingSendId` (`Base LSP header:34-82`). They correctly
  track lifecycle without extra locks.
- Global mutexes:
  - `documentMutex` protects `documentsByUri` for open/update/close
    (`base_lsp_language_server.cpp:270-335`); readers use shared locks.
  - `configMutex`, `workspaceMutex`, `lspConfigMutex`, and `runningMutex`
    guard config caches, workspace config data, generated configs, and the
    telemetry histogram respectively (`base_lsp_language_server.h:66-120`).
  - `activeMutex` protects `activeRequests` (`handleRequest`,
    `receiveCancelRequest` at `base_lsp_language_server.cpp:567-616` and
    `1502-1528`). Each map entry stores a `shared_ptr<std::atomic_bool>` that
    gets flipped to cancel work. This looks safe.
  - `requestMutex` remains owned by the generated LSP base and is only locked
    via the macros.
- Condition variables: the base class uses the `sent` condition only in the
  parallel strategy (see below).
- Reading/writing documents: all document lookups first grab the shared lock,
  release it before taking an exclusive one, and only re-check state afterward
  (`base_lsp_language_server.cpp:320-412`), so no deadlocks were spotted.

## Document-Level Synchronization

- `src/server/lsp_text_document.h:32-104` exposes `std::shared_mutex &_mutex`
  through `mutex()`. Writers (e.g. `update`, `apply`) take a unique lock
  (`lsp_text_document.cpp:150-193`). Read-only helpers rely on the caller to
  hold a read lock, which the language server does through the `LSP_READ_LOCK`
  macros. This convention is safe today; it would become buggy if any caller
  accessed `_text` without locking.

## Fortran-Specific Extensions

- `src/bin/lfortran_lsp_language_server.h:44-111` introduces
  `optionMutex`, `validationMutex`, and `highlightsMutex` (all shared mutexes)
  plus several `std::atomic_bool` flags describing client capabilities.
  Accessors in `lfortran_lsp_language_server.cpp:144-241` use the expected
  read/write locks, so the caches remain consistent.
- `src/bin/lfortran_accessor.h:88-98` wraps the compiler entry points in a
  `std::mutex mutex`. Every public accessor method grabs a `std::unique_lock`
  before manipulating shared compiler instances (`src/bin/lfortran_accessor.cpp`
  uses the lock in each public method). This prevents re-entrancy crashes when
  the same accessor is called from multiple request threads.

## Execution Strategies

### Concurrent (`src/server/concurrent_lsp_language_server.*`)

- Uses no explicit mutexes around `pendingMessages` or `responsesById`
  (`concurrent_lsp_language_server.cpp:73-211`). The implementation relies on
  having exactly one listener thread that calls `handle` synchronously, so the
  priority queue and map never experience concurrent writers. This is correct
  as long as we keep the execution strategy single-threaded, but would become
  buggy (data races) if we ever dispatch messages through a pool.

### Parallel (`src/server/parallel_lsp_language_server.*`)

- Thread pools: `requestPool` and `workerPool` (header lines 95-104) run tasks
  concurrently.
- Ordering barrier: `sentMutex` plus `sent` condition variable guarantee that
  responses are flushed to the client in send-id order
  (`parallel_lsp_language_server.cpp:171-210`). Every request thread waits for
  its turn, increments `pendingSendId`, and notifies all waiters. This is
  correct but sensitive to missed `notify_all`; tests now cover it under timeouts.
- Retry & TTL caches:
  - `recentMutex` guards throttling of completed request IDs
    (`expireCaches` at `parallel_lsp_language_server.cpp:389-417`).
  - `retryMutex` protects the retry priority queue; scheduled tasks reacquire
    the write lock before mutating the queue (`430-507`). Enqueued retry tasks
    upgrade from the request cache (`requestMutex`) under a read lock before
    resending, so they do not contend with writers.
- Cron scheduling: `scheduleMutex` guards the cron schedule map while
  `cronMutex` protects the priority queue (`parallel_lsp_language_server.cpp:239-332`).
  Locks are always acquired in that order (schedule -> cron) to avoid
  deadlocks.
- Cron thread: `cron` runs in the background, wakes up every 40 ms, and submits
  tasks back to the worker pool. The loop ensures `workerPool.ensureCapacity()`
  executes without holding other locks, so the design is safe.
- Request cancellation: `retryRequests` calls `cancelRequest` before resending.
  `cancelRequest` only touches `activeMutex`, and the scheduling path never
  holds `recentMutex` or `retryMutex` while calling it, so no circular waits
  were found.

## Additional Atomics and Locks

- `src/server/base_lsp_language_server.h:98-112` tracks telemetry owners using
  `std::shared_mutex runningMutex`.
- `src/server/base_lsp_language_server.h:116-118` includes atomics that capture
  whether the client can handle workspace config notifications; they are
  written in initialize responses and read elsewhere without locking – this is
  safe because they are atomic flags.
- `src/server/base_lsp_language_server.cpp:1497-1529` toggles per-request
  atomic flags during `$ /cancelRequest`, ensuring that long-running work sees
  the cancellation.

## Potential Weak Spots

1. **Concurrent strategy relies on single-threaded listeners.** If we ever move
   `handle()` calls to a pool, `pendingMessages` and `responsesById` must gain
   a mutex; otherwise data races will surface.
2. **Document helpers assume external locking.** Most getters in
   `LspTextDocument` read `_text` without taking `_mutex`; the callers do lock
   today, but adding helper sites in the future without following the pattern
   would introduce data races. Consider enforcing the lock inside the class.
3. **Queue stop semantics.** `Queue::stop()` throws if called twice
   (`queue.hpp:96-122`). That is intentional debugging help, but any new code
   that might stop the same queue from multiple places must guard against the
   exception or use `stopNow()`.

No other obvious data races or deadlocks were observed while walking through
the synchronization sites listed above.

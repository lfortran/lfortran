#include "server/base_lsp_language_server.h"
#include <algorithm>
#include <cctype>
#include <chrono>
#include <exception>
#include <memory>
#include <mutex>
#include <string>
#include <tuple>

#include <server/parallel_lsp_language_server.h>
#include <server/lsp_exception.h>
#include <server/lsp_issue_reporter.h>
#include <server/lsp_json_parser.h>
#include <server/lsp_language_server.h>
#include <server/lsp_specification.h>
#include <server/lsp_text_document.h>

namespace LCompilers::LanguageServerProtocol {
    using namespace std::chrono_literals;
    auto now = std::chrono::system_clock::now;

    auto CronComparator::operator()(
        const CronElem &lhs,
        const CronElem &rhs
    ) -> bool {
        return std::get<2>(lhs) > std::get<2>(rhs);
    }

    ParallelLspLanguageServer::ParallelLspLanguageServer(
        ls::MessageQueue &incomingMessages,
        ls::MessageQueue &outgoingMessages,
        std::size_t numRequestThreads,
        std::size_t numWorkerThreads,
        lsl::Logger &logger,
        const std::string &configSection,
        const std::string &extensionId,
        const std::string &compilerVersion,
        int parentProcessId,
        unsigned int seed,
        std::shared_ptr<lsc::LspConfigTransformer> lspConfigTransformer,
        std::shared_ptr<lsc::LspConfig> workspaceConfig,
        std::atomic_bool &start,
        std::condition_variable &startChanged,
        std::mutex &startMutex
    ) : BaseLspLanguageServer(
        incomingMessages,
        outgoingMessages,
        logger,
        configSection,
        extensionId,
        compilerVersion,
        parentProcessId,
        lspConfigTransformer,
        workspaceConfig,
        start,
        startChanged,
        startMutex
      )
      , logger(logger.having("ParallelLspLanguageServer"))
      , requestPool("request", numRequestThreads, logger)
      , workerPool("worker", numWorkerThreads, logger)
      , randomEngine(seed)
      , cron([this, &logger, &start, &startChanged, &startMutex]{
          logger.threadName("cron");
          if (!start) {
              std::unique_lock<std::mutex> startLock(startMutex);
              startChanged.wait(startLock, [&start]{
                  return start.load();
              });
          }
          chronicle();
      })
    {
        // -----------------------
        // Schedule the cron jobs:
        // -----------------------
        schedule(
            [this](std::shared_ptr<std::atomic_bool> taskIsRunning) {
                if (*taskIsRunning) {
                    checkParentProcessId();
                }
            },
            [this]{ return ttl(1000ms); }
        );
        schedule(
            [this](std::shared_ptr<std::atomic_bool> taskIsRunning) {
                expireCaches(std::move(taskIsRunning));
            },
            [this]{ return ttl(1000ms); }
        );
        if (this->workspaceConfig->timeoutMs > 0) {
            schedule(
                [this](std::shared_ptr<std::atomic_bool> taskIsRunning) {
                    retryRequests(std::move(taskIsRunning));
                },
                [this]{ return ttl(100ms); }
            );
        } else {
            this->logger.warn() << "Request time-outs are disabled." << std::endl;
        }
        schedule(
            [this](std::shared_ptr<std::atomic_bool> taskIsRunning) {
                if (*taskIsRunning) {
                    sendTelemetry();
                }
            },
            [this]{ return ttl(1000ms); }
        );
    }

    auto ParallelLspLanguageServer::join() -> void {
        if (listener.joinable()) {
            listener.join();
            logger.debug()
                << "Incoming-message listener terminated."
                << std::endl;
        }
        requestPool.join();
        logger.debug() << "Request thread-pool terminated." << std::endl;
        workerPool.join();
        logger.debug() << "Worker thread-pool terminated." << std::endl;
    }

    auto ParallelLspLanguageServer::listen() -> void {
        try {
            while (!_exit) {
                const std::string message = incomingMessages.dequeue();
                if (!_exit) {
                    std::size_t sendId = nextSendId();
                    std::shared_ptr<std::atomic_bool> taskIsRunning =
                        requestPool.execute([this, message, sendId](
                            std::shared_ptr<std::atomic_bool> taskIsRunning
                        ) {
                            try {
                                if (*taskIsRunning) {
                                    handle(message, sendId, std::move(taskIsRunning));
                                } else {
                                    logger.debug()
                                        << "Canceled before message could be handled."
                                        << std::endl;
                                }
                            } catch (...) {
                                std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());
                                logger.error()
                                    << "Failed to handle message: " << message
                                    << std::endl;
                                logger.error()
                                    << formatException(
                                        "Caught unhandled exception",
                                        std::current_exception()
                                    )
                                    << std::endl;
                            }
                            {
                                std::unique_lock<std::mutex> sentLock(sentMutex);
                                sent.wait(sentLock, [this, sendId]{
                                    return (pendingSendId == sendId) || _exit;
                                });
                            }
                            ++pendingSendId;
                            sent.notify_all();
                        });
                }
            }
        } catch (std::exception &e) {
            if (e.what() != lst::DEQUEUE_FAILED_MESSAGE) {
                logger.error()
                    << formatException(
                        "Unhandled exception caught",
                        std::current_exception()
                    )
                    << std::endl;
            } else {
                logger.trace()
                    << "Interrupted while dequeuing messages: " << e.what()
                    << std::endl;
            }
        } catch (...) {
            logger.error()
                << formatException(
                    "Unhandled exception caught",
                    std::current_exception()
                )
                << std::endl;
        }
    }

    auto ParallelLspLanguageServer::nextCronId() -> std::size_t {
        return ++serialCronId;
    }

    auto ParallelLspLanguageServer::schedule(
        lst::Task cronJob,
        CronSchedule schedule
    ) -> std::size_t {
        std::size_t cronId = nextCronId();
        {
            auto writeLock = this->writeLock(__FILE__, __LINE__, scheduleMutex, "schedule");
            cronSchedules.emplace(cronId, schedule);
        }
        time_point_t nextTimePoint = schedule();
        {
            auto cronLock = this->mutexLock(__FILE__, __LINE__, cronMutex, "cron");
            cronJobs.push(std::make_tuple(cronId, cronJob, nextTimePoint));
        }
        return cronId;
    }

    auto ParallelLspLanguageServer::unschedule(std::size_t cronId) -> bool {
        auto readLock = this->readLock(__FILE__, __LINE__, scheduleMutex, "schedule");
        auto iter = cronSchedules.find(cronId);
        if (iter != cronSchedules.end()) {
            readLock.unlock();
            auto writeLock = this->writeLock(__FILE__, __LINE__, scheduleMutex, "schedule");
            iter = cronSchedules.find(cronId);
            if (iter != cronSchedules.end()) {
                cronSchedules.erase(iter);
                return true;
            }
        }
        return false;
    }

    auto ParallelLspLanguageServer::chronicle() -> void {
        try {
            while (!_exit) {
                requestPool.ensureCapacity();
                workerPool.ensureCapacity();
                bool changed;
                do {
                    changed = false;
                    auto cronLock = this->mutexLock(__FILE__, __LINE__, cronMutex, "cron");
                    if (cronJobs.size() > 0) {
                        const CronElem &elem = cronJobs.top();
                        if (std::get<2>(elem) < now()) {
                            std::size_t cronId = std::get<0>(elem);
                            lst::Task cronJob = std::get<1>(elem);
                            cronJobs.pop();
                            cronLock.unlock();
                            workerPool.execute([this, cronId, cronJob](
                                std::shared_ptr<std::atomic_bool> taskIsRunning
                            ) {
                                if (!_exit) {
                                    if (*taskIsRunning) {
                                        try {
                                            cronJob(std::move(taskIsRunning));
                                        } catch (...) {
                                            logger.error()
                                                << formatException(
                                                    ("Caught unhandled exception "
                                                     "while executing cron job "
                                                     "with id=" + std::to_string(cronId)),
                                                    std::current_exception()
                                                )
                                                << std::endl;
                                        }
                                    } else {
                                        logger.debug()
                                            << "Cron job with id=" << cronId << " canceled before execution."
                                            << std::endl;
                                    }

                                    auto readLock = this->readLock(__FILE__, __LINE__, scheduleMutex, "schedule");
                                    auto iter = cronSchedules.find(cronId);
                                    if (iter != cronSchedules.end()) {
                                        CronSchedule schedule = iter->second;
                                        readLock.unlock();
                                        time_point_t nextTimePoint = schedule();
                                        auto cronLock = this->mutexLock(__FILE__, __LINE__, cronMutex, "cron");
                                        cronJobs.push(std::make_tuple(cronId, cronJob, nextTimePoint));
                                    }
                                }
                            });
                            changed = true;
                        }
                    }
                } while (!_exit && changed);

                // NOTE: Wait a short period of time before running the cron jobs again:
                std::this_thread::sleep_for(40ms);
            }
        } catch (...) {
            logger.error()
                << formatException(
                    "Unhandled exception caught",
                    std::current_exception()
                )
                << std::endl;
        }
    }

    auto ParallelLspLanguageServer::send(const RequestMessage &request) -> void {
        int requestId = request.id.integer();
        BaseLspLanguageServer::send(request);
        {
            auto retryLock = this->writeLock(__FILE__, __LINE__, retryMutex, "retry-requests");
            retryAttempts.push(
                std::make_pair(
                    std::make_tuple(
                        requestId,
                        0,  // attempt
                        milliseconds_t(
                            workspaceConfig->retry.minSleepTimeMs
                        )
                    ),
                    ttl(milliseconds_t(workspaceConfig->timeoutMs))
                )
            );
        }
    }

    auto ParallelLspLanguageServer::send(
        const std::string &message,
        std::size_t sendId
    ) -> void {
        // -------------------------------------------------------------------------
        // NOTE: The LSP spec requires responses to be returned in roughly the same
        // order of receipt of their corresponding requests. Some types of responses
        // may be returned out-of-order, but in order to support those we will need
        // to implement a sort of dependency graph. Without knowledge of their
        // dependencies, we must respond to all requests in order of receipt.
        // -------------------------------------------------------------------------
        {
            std::unique_lock<std::mutex> sentLock(sentMutex);
            sent.wait(sentLock, [this, sendId]{
                return (pendingSendId == sendId) || _exit;
            });
        }
        if ((pendingSendId == sendId) && !_exit) {
            ls::LanguageServer::send(message);
        }
    }

    auto ParallelLspLanguageServer::ttl(
        const milliseconds_t &timeout
    ) const -> time_point_t {
        return now() + timeout;
    }

    auto ParallelLspLanguageServer::expireCaches(
        std::shared_ptr<std::atomic_bool> taskIsRunning
    ) -> void {
        bool changed = true;
        while (!_exit && *taskIsRunning && changed) {
            changed = false;
            auto readLock = this->readLock(__FILE__, __LINE__, recentMutex, "recent-requests");
            if ((recentRequests.size() > 0) && (recentRequests.top().second < now())) {
                readLock.unlock();
                auto writeLock = this->writeLock(__FILE__, __LINE__, recentMutex, "recent-requests");
                if (recentRequests.size() > 0) {
                    const TTLRecord<std::string> &record = recentRequests.top();
                    if (record.second < now()) {
                        std::string requestId = record.first;
                        recentRequests.pop();
                        writeLock.unlock();
                        {
                            auto requestLock = this->mutexLock(__FILE__, __LINE__, activeMutex, "active-requests");
                            auto iter = activeRequests.find(requestId);
                            if (iter != activeRequests.end()) {
                                activeRequests.erase(iter);  // timeout
                            }
                        }
                        changed = true;
                    }
                }
            }
        }
    }

    auto ParallelLspLanguageServer::randomBetween(
        const milliseconds_t &lower,
        const milliseconds_t &upper
    ) -> milliseconds_t {
        std::uniform_int_distribution<long long> distribution(
            lower.count(),
            upper.count()
        );
        return milliseconds_t(distribution(randomEngine));
    }

    auto ParallelLspLanguageServer::retryRequests(
        std::shared_ptr<std::atomic_bool> taskIsRunning
    ) -> void {
        bool changed = true;
        while (!_exit && *taskIsRunning && changed) {
            changed = false;
            auto readLock = this->readLock(__FILE__, __LINE__, retryMutex, "retry-requests");
            if ((retryAttempts.size() > 0) && retryAttempts.top().second < now()) {
                readLock.unlock();
                auto writeLock = this->writeLock(__FILE__, __LINE__, retryMutex, "retry-requests");
                const TTLRecord<RetryRecord> &record = retryAttempts.top();
                if (record.second < now()) {
                    int requestId = std::get<0>(record.first);
                    cancelRequest(requestId);
                    unsigned int attempt = std::get<1>(record.first);
                    retryAttempts.pop();
                    writeLock.unlock();
                    if (attempt < workspaceConfig->retry.maxAttempts) {
                        // NOTE: See the section on "Decorrelated Jitter" in the following article:
                        // https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/
                        milliseconds_t lastSleepTimeMs =
                            std::get<2>(record.first);
                        milliseconds_t nextSleepTimeMs =
                            std::min<milliseconds_t>(
                                milliseconds_t(workspaceConfig->retry.maxSleepTimeMs),
                                randomBetween(
                                    milliseconds_t(workspaceConfig->retry.minSleepTimeMs),
                                    lastSleepTimeMs * 3
                                )
                            );
                        // NOTE: Release the worker thread to execute more tasks
                        // while we wait to retry the request. Do not sleep from
                        // the current thread or we may run out of available
                        // workers!
                        schedule([this, requestId, attempt, &nextSleepTimeMs](
                            std::shared_ptr<std::atomic_bool> taskIsRunning
                        ) {
                            if (*taskIsRunning) {
                                auto requestLock = this->readLock(__FILE__, __LINE__, requestMutex, "requests");
                                auto iter = requestsById.find(requestId);
                                if (iter != requestsById.end()) {
                                    std::shared_ptr<RequestMessage> request = iter->second;
                                    requestLock.unlock();
                                    LspLanguageServer::send(*request);
                                    auto writeLock = this->writeLock(__FILE__, __LINE__, retryMutex, "retry-requests");
                                    retryAttempts.push(
                                        std::make_pair(
                                            std::make_tuple(
                                                requestId,
                                                attempt + 1,
                                                nextSleepTimeMs
                                            ),
                                            ttl(milliseconds_t(workspaceConfig->timeoutMs))
                                        )
                                    );
                                }
                            }
                        }, nextSleepTimeMs);
                        logger.trace()
                            << "Request with id=" << requestId
                            << " timed-out. Retrying after "
                            << static_cast<long>(nextSleepTimeMs.count())
                            << " ms." << std::endl;
                    } else {
                        logger.error()
                            << "Request with id=" << requestId
                            << " failed after " << attempt << " attempts."
                            << std::endl;
                    }
                }
                changed = true;
            }
        }
    }

    auto ParallelLspLanguageServer::dispatch(
        ResponseMessage &response,
        RequestMessage &request
    ) -> void {
        BaseLspLanguageServer::dispatch(response, request);
        std::string requestId = to_string(request.id);
        {
            auto writeLock = this->writeLock(__FILE__, __LINE__, recentMutex, "recent-requests");
            recentRequests.push(std::make_pair(requestId, ttl(RECENT_REQUEST_TIMEOUT)));
        }
    }

    auto ParallelLspLanguageServer::getConfig(
        const DocumentUri &uri,
        const std::string &configSection
    ) -> const std::shared_ptr<LSPAny> {
        auto readLock = this->readLock(__FILE__, __LINE__, configMutex, "config:" + uri);
        auto configIter = configsByUri.find(uri);
        if (configIter != configsByUri.end()) {
            return configIter->second;
        }

        readLock.unlock();

        ConfigurationItem item;
        item.scopeUri = uri;
        item.section = configSection;

        ConfigurationParams params;
        params.items.push_back(std::move(item));

        auto writeLock = this->writeLock(__FILE__, __LINE__, configMutex, "config:" + uri);
        configIter = configsByUri.find(uri);
        if (configIter != configsByUri.end()) {
            return configIter->second;
        }

        std::shared_future<std::shared_ptr<LSPAny>> future;

        auto pendingIter = pendingConfigsByUri.find(uri);
        if (pendingIter != pendingConfigsByUri.end()) {
            future = pendingIter->second.second;
        } else {
            int requestId = sendWorkspace_configuration(params);
            std::promise<std::shared_ptr<LSPAny>> promise;
            future = promise.get_future().share();
            {
                auto &pairs = pendingConfigsById.emplace(
                    std::piecewise_construct,
                    std::forward_as_tuple(requestId),
                    std::forward_as_tuple()
                ).first->second;
                auto &pair = pairs.emplace_back();
                pair.first = uri;
                pair.second = std::move(promise);
            }
            pendingConfigsByUri.emplace(
                std::piecewise_construct,
                std::forward_as_tuple(uri),
                std::forward_as_tuple(requestId, future)
            );
        }

        writeLock.unlock();

        if (future.valid()) {
            future.wait();
            if (future.valid()) {
                return future.get();
            }
        }

        throw std::runtime_error(
            "Future config became invalid while waiting for it."
        );
    }

    auto ParallelLspLanguageServer::exit() -> bool {
        if (BaseLspLanguageServer::exit()) {
            requestPool.stopNow();
            workerPool.stopNow();
            return true;
        }
        return false;
    }

} // namespace LCompilers::LanguageServerProtocol

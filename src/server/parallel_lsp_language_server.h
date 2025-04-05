#pragma once

#include <atomic>
#include <chrono>
#include <condition_variable>
#include <functional>
#include <memory>
#include <mutex>
#include <queue>
#include <random>
#include <shared_mutex>
#include <string>
#include <thread>
#include <tuple>
#include <utility>

#include <server/base_lsp_language_server.h>
#include <server/logger.h>
#include <server/lsp_config.h>
#include <server/lsp_language_server.h>
#include <server/lsp_specification.h>
#include <server/lsp_text_document.h>

namespace LCompilers::LanguageServerProtocol {
    namespace lsc = LCompilers::LanguageServerProtocol::Config;
    namespace lst = LCompilers::LLanguageServer::Threading;

    using namespace std::chrono_literals;
    typedef std::chrono::system_clock::time_point time_point_t;
    typedef std::chrono::milliseconds milliseconds_t;

    /**
     * Returns the next time_point_t, relative to now, at which to execute the
     * associated cron job.
     */
    typedef std::function<time_point_t()> CronSchedule;

    typedef std::tuple<std::size_t, lst::Task, time_point_t> CronElem;

    struct CronComparator {
        auto operator()(const CronElem &lhs, const CronElem &rhs) -> bool;
    };

    typedef std::priority_queue<
        CronElem,
        std::vector<CronElem>,
        CronComparator
    > CronQueue;

    template <typename T>
    using TTLRecord = std::pair<T, time_point_t>;

    template <typename T>
    struct TTLComparator {
        auto operator()(const TTLRecord<T> &lhs, const TTLRecord<T> &rhs) {
            return lhs.second > rhs.second;
        }
    };

    template <typename T>
    using TTLCache = std::priority_queue<
        TTLRecord<T>,
        std::vector<TTLRecord<T>>,
        TTLComparator<T>
    >;

    typedef std::tuple<
        int,  // requestId
        unsigned int,  // attempt
        milliseconds_t  // last sleep time between attempts
    > RetryRecord;

    class ParallelLspLanguageServer : virtual public BaseLspLanguageServer {
    protected:
        ParallelLspLanguageServer(
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
            std::shared_ptr<lsc::LspConfig> workspaceConfig
        );

        lsl::Logger logger;
        lst::ThreadPool requestPool;
        lst::ThreadPool workerPool;
        std::condition_variable sent;
        std::mutex sentMutex;
        std::default_random_engine randomEngine;

        const milliseconds_t RECENT_REQUEST_TIMEOUT = 1000ms;
        TTLCache<std::string> recentRequests;
        std::shared_mutex recentMutex;
        auto expireCaches(std::shared_ptr<std::atomic_bool> taskIsRunning) -> void;

        auto ttl(const milliseconds_t &timeout) const -> time_point_t;
        auto randomBetween(
            const milliseconds_t &lower,
            const milliseconds_t &upper
        ) -> milliseconds_t;

        TTLCache<RetryRecord> retryAttempts;
        std::shared_mutex retryMutex;
        auto retryRequests(std::shared_ptr<std::atomic_bool> taskIsRunning) -> void;

        std::atomic_size_t serialCronId = 0;
        std::map<std::size_t, CronSchedule> cronSchedules;
        std::shared_mutex scheduleMutex;
        CronQueue cronJobs;
        std::mutex cronMutex;
        auto nextCronId() -> std::size_t;
        auto schedule(lst::Task cronJob, CronSchedule schedule) -> std::size_t;
        auto unschedule(std::size_t cronId) -> bool;
        auto chronicle() -> void;

        template <class Rep, class Period = std::ratio<1>>
        auto schedule(
            lst::Task cronJob,
            std::chrono::duration<Rep, Period> timeout
        ) -> void {
            std::size_t cronId = nextCronId();
            auto now = std::chrono::system_clock::now();
            time_point_t nextTimePoint = now + timeout;
            {
                std::unique_lock<std::mutex> cronLock(cronMutex);
                cronJobs.push(std::make_tuple(cronId, cronJob, nextTimePoint));
            }
        }

        // NOTE: By convention and to encourage proper initialization order,
        // move all std::thread declarations to the bottom of the members!
        // See: https://github.com/lfortran/lfortran/issues/6756
        std::thread cron;

        auto join() -> void override;
        auto listen() -> void override;

        auto send(const RequestMessage &request) -> void override;
        auto send(const std::string &request, std::size_t sendId) -> void override;

        auto dispatch(
            ResponseMessage &response,
            RequestMessage &request
        ) -> void override;

        auto getConfig(
            const DocumentUri &uri,
            const std::string &configSection
        ) -> const std::shared_ptr<LSPAny> override;

        auto exit() -> bool override;
    }; // class ParallelLspLanguageServer

} // namespace LCompilers::LanguageServerProtocol

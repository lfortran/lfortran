#pragma once

#include <cstddef>

#include <server/logger.h>

namespace LCompilers::LLanguageServer {
    namespace lsl = LCompilers::LLanguageServer::Logging;

    /**
     * Contains methods for measuring various resources used by the current
     * process such as memory utilization and CPU usage. Please note that many
     * of the methods may be platform-specific. No method should raise an
     * exception but if the current platform is not supported then they will
     * return stubbed responses and `isValid()` will return `false`.
     */
    class ProcessUsage {
    public:
        ProcessUsage(lsl::Logger &logger);
        auto memoryUtilization() -> std::size_t;
        auto cpuUsage() -> double;
        auto isValid() const -> bool;
    private:
        lsl::Logger logger;
        double m_lastCpuTime;
        double m_lastUptime;
        int m_numCores;
        bool m_isValid{true};

        double uptime();
        double cpuTime();
        int numCores();
    }; // class CpuUsageTracker

} // namespace LCompilers::LLanguageServer

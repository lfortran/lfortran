#pragma once

#include <cstddef>

#include <server/logger.h>

namespace LCompilers::LLanguageServer {
    namespace lsl = LCompilers::LLanguageServer::Logging;

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

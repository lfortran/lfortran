#pragma once

#include <cstddef>

namespace LCompilers::LLanguageServer {

    auto memoryUtilization() -> std::size_t;

    class CpuUsageTracker {
    public:
        CpuUsageTracker();
        double getInstantaneousCpuUsage();
    private:
        double m_lastCpuTime;
        double m_lastUptime;
        int m_numCores;

        double getUptime();
        double getCpuTime();
        int getNumCores();
    }; // class CpuUsageTracker

} // namespace LCompilers::LLanguageServer

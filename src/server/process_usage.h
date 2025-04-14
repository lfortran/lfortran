#pragma once

#include <cstddef>

namespace LCompilers::LLanguageServer {

    struct ProcessUsage {
        std::size_t memoryUtilization;  // Resident memory usage in bytes
        double cpuUtilization;          // CPU usage as percentage
        static auto sample() -> ProcessUsage;
    };

} // namespace LCompilers::LLanguageServer

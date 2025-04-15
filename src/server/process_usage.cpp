#include <chrono>
#include <cstdint>
#include <iostream>
#include <string>
#include <thread>

#if defined(_WIN32)
#include <windows.h>
#include <psapi.h>
#elif defined(__linux__)
#include <fstream>
#include <sstream>
#include <unistd.h>
#include <sys/resource.h>
#include <sys/times.h>
#elif defined(__APPLE__)
#include <mach/mach.h>
#include <mach/task.h>
#include <mach/thread_info.h>
#include <sys/resource.h>
#include <sys/sysctl.h>
#endif

#include <server/process_usage.h>

namespace LCompilers::LLanguageServer {

    auto memoryUtilization() -> std::size_t {
        std::size_t numBytes = 0;

#if defined(_WIN32)
        // --- Windows Memory ---
        PROCESS_MEMORY_COUNTERS mem_info = {0};
        mem_info.cb = sizeof(mem_info);
        HANDLE process = GetCurrentProcess();
        if (GetProcessMemoryInfo(process, &mem_info, sizeof(mem_info))) {
            numBytes = mem_info.WorkingSetSize;
        }

#elif defined(__linux__)
        // --- Linux Memory ---
        std::ifstream status("/proc/self/status");
        if (status.is_open()) {
            std::string line;
            // uint64_t vm_size = 0;
            while (std::getline(status, line)) {
                std::istringstream iss(line);
                std::string key, value;
                if (std::getline(iss, key, ':')
                    && std::getline(iss >> std::ws, value)) {
                    if (key == "VmRSS") {
                        numBytes = std::stoull(value) * 1024; // Convert kB to bytes
                        break;
                    }
                }
            }
            status.close();
        }

#elif defined(__APPLE__)
        // --- macOS CPU Cores ---
        int ncpu;
        size_t len = sizeof(ncpu);
        sysctlbyname("hw.ncpu", &ncpu, &len, nullptr, 0);

        // --- macOS Memory ---
        struct task_basic_info info;
        mach_msg_type_number_t count = TASK_BASIC_INFO_COUNT;
        if (task_info(
                mach_task_self(),
                TASK_BASIC_INFO,
                (task_info_t)&info,
                &count
            ) == KERN_SUCCESS
        ) {
            numBytes = info.resident_size;
        }
#endif

        return numBytes;
    }

    CpuUsageTracker::CpuUsageTracker()
        : m_lastCpuTime(0.0)
        , m_lastUptime(0.0)
    {
        m_numCores = getNumCores();
        m_lastCpuTime = getCpuTime();
        m_lastUptime = getUptime();
    }

    double CpuUsageTracker::getUptime() {
#ifdef __linux__
        std::ifstream file("/proc/uptime");
        if (!file) throw std::runtime_error("Failed to open /proc/uptime");
        double uptime;
        file >> uptime;
        return uptime;
#elif defined(_WIN32)
        FILETIME system_time;
        GetSystemTimeAsFileTime(&system_time);
        ULARGE_INTEGER time;
        time.LowPart = system_time.dwLowDateTime;
        time.HighPart = system_time.dwHighDateTime;
        return time.QuadPart / 10'000'000.0; // Convert 100-nanosecond units to seconds
#else // macOS
        struct timeval boottime;
        size_t len = sizeof(boottime);
        if (sysctlbyname("kern.boottime", &boottime, &len, nullptr, 0) != 0) {
            throw std::runtime_error("Failed to get system uptime");
        }
        auto now = std::chrono::system_clock::now().time_since_epoch();
        return std::chrono::duration<double>(now).count()
            - (boottime.tv_sec + boottime.tv_usec / 1'000'000.0);
#endif
    }

    double CpuUsageTracker::getCpuTime() {
#ifdef __linux__
        std::ifstream file("/proc/self/stat");
        if (!file) throw std::runtime_error("Failed to open /proc/self/stat");
        std::string line;
        std::getline(file, line);
        std::istringstream iss(line);
        std::string token;
        for (int i = 0; i < 13; ++i) iss >> token;
        unsigned long utime, stime;
        iss >> utime >> stime;
        return (utime + stime) / (double)sysconf(_SC_CLK_TCK); // Convert ticks to seconds
#elif defined(_WIN32)
        FILETIME creationTime, exitTime, kernelTime, userTime;
        if (!GetProcessTimes(
                GetCurrentProcess(),
                &creationTime,
                &exitTime,
                &kernelTime,
                &userTime
            )
        ) {
            throw std::runtime_error("Failed to get process times");
        }
        ULARGE_INTEGER kernel, user;
        kernel.LowPart = kernelTime.dwLowDateTime;
        kernel.HighPart = kernelTime.dwHighDateTime;
        user.LowPart = userTime.dwLowDateTime;
        user.HighPart = userTime.dwHighDateTime;
        // Convert 100-nanosecond units to seconds:
        return (kernel.QuadPart + user.QuadPart) / 10'000'000.0;
#else // macOS
        task_t task = mach_task_self();
        thread_act_array_t threadList;
        mach_msg_type_number_t threadCount;
        kern_return_t kr = task_threads(task, &threadList, &threadCount);
        if (kr != KERN_SUCCESS) {
            throw std::runtime_error("Failed to get thread list");
        }

        double totalCpuTime = 0.0;
        for (mach_msg_type_number_t i = 0; i < threadCount; ++i) {
            thread_basic_info_data_t threadInfo;
            mach_msg_type_number_t threadInfoCount = THREAD_BASIC_INFO_COUNT;
            kr = thread_info(
                threadList[i],
                THREAD_BASIC_INFO,
                (thread_info_t)&threadInfo,
                &threadInfoCount
            );
            if (kr == KERN_SUCCESS && !(threadInfo.flags & TH_FLAGS_IDLE)) {
                totalCpuTime += threadInfo.user_time.seconds
                    + (threadInfo.user_time.microseconds / 1'000'000.0)
                    + threadInfo.system_time.seconds
                    + (threadInfo.system_time.microseconds / 1'000'000.0);
            }
        }

        for (mach_msg_type_number_t i = 0; i < threadCount; ++i) {
            mach_port_deallocate(mach_task_self(), threadList[i]);
        }

        vm_deallocate(
            mach_task_self(),
            (vm_address_t)threadList,
            threadCount * sizeof(thread_act_t)
        );

        return totalCpuTime;
#endif
    }

    int CpuUsageTracker::getNumCores() {
#ifdef __linux__
        return sysconf(_SC_NPROCESSORS_ONLN);
#elif defined(_WIN32)
        SYSTEM_INFO sysinfo;
        GetSystemInfo(&sysinfo);
        return sysinfo.dwNumberOfProcessors;
#else // macOS
        int cores;
        size_t len = sizeof(cores);
        if (sysctlbyname("hw.ncpu", &cores, &len, nullptr, 0) != 0) {
            throw std::runtime_error("Failed to get number of CPU cores");
        }
        return cores;
#endif
    }

    double CpuUsageTracker::getInstantaneousCpuUsage() {
        double currentCpuTime = getCpuTime();
        double currentUptime = getUptime();

        double cpuTimeDiff = currentCpuTime - m_lastCpuTime;
        double uptimeDiff = currentUptime - m_lastUptime;

        if (uptimeDiff < 0.001) {
            return 0.0; // Too short an interval
        }

        double cpuUsage = (cpuTimeDiff / uptimeDiff) * 100.0 / m_numCores;

        m_lastCpuTime = currentCpuTime;
        m_lastUptime = currentUptime;

        return (cpuUsage > 0.0) ? cpuUsage : 0.0; // Ensure non-negative
    }

} // namespace LCompilers::LLanguageServer

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
// --------------------------------------------------------------------
// TODO: Uncomment the BSD-related sections and test them on BSD before
// commiting them to main.
// --------------------------------------------------------------------
/*
#elif defined(__FreeBSD__) \
    || defined(__OpenBSD__) \
    || defined(__NetBSD__) \
    || defined(__DragonFly__) \
    || defined(__BSD__)
#include <sys/resource.h>
#include <sys/sysctl.h>
#include <sys/types.h>
#include <unistd.h>
*/
#endif

#include <server/process_usage.h>

namespace LCompilers::LLanguageServer {

    ProcessUsage::ProcessUsage(lsl::Logger &logger)
        : logger(logger.having("ProcessUsage"))
    {
        m_numCores = numCores();
        m_lastCpuTime = cpuTime();
        m_lastUptime = uptime();
    }

    auto ProcessUsage::isValid() const -> bool {
        return m_isValid;
    }

    auto ProcessUsage::memoryUtilization() -> std::size_t {
#if defined(_WIN32)
        PROCESS_MEMORY_COUNTERS pmc;
        if (GetProcessMemoryInfo(GetCurrentProcess(), &pmc, sizeof(pmc))) {
            return pmc.WorkingSetSize;
        }
        logger.error() << "Failed to get process memory information" << std::endl;
#elif defined(__linux__)
        std::ifstream statm("/proc/self/statm");
        if (statm.is_open()) {
            size_t value;
            statm >> value; // vm_size
            statm >> value; // resident
            return value * getpagesize();
        }
        logger.error() << "Failed to open /proc/self/statm" << std::endl;
#elif defined(__APPLE__)
        task_t task = MACH_PORT_NULL;
        struct task_basic_info info;
        mach_msg_type_number_t count = TASK_BASIC_INFO_COUNT;
        if ((task_for_pid(mach_task_self(), getpid(), &task) == KERN_SUCCESS)
            && (task_info(task, TASK_BASIC_INFO, (task_info_t)&info, &count) == KERN_SUCCESS)) {
            return info.resident_size;
        }
        logger.error() << "Failed to get task information" << std::endl;
/*
#elif defined(__FreeBSD__) \
    || defined(__OpenBSD__) \
    || defined(__NetBSD__) \
    || defined(__DragonFly__) \
    || defined(__BSD__)
        int mib[] = {CTL_KERN, KERN_PROC, KERN_PROC_PID, getpid()};
        struct kinfo_proc kp;
        size_t len = sizeof(kp);
        if (sysctl(mib, 4, &kp, &len, NULL, 0) == 0) {
            return kp.kp_vm_rssize * getpagesize();
        }
        logger.error() << "Failed to get process information" << std::endl;
*/
#else
        logger.error() << "Operating system is not supported" << std::endl;
#endif
        m_isValid = false;
        return 0;
    }

    double ProcessUsage::uptime() {
#ifdef __linux__
        std::ifstream file("/proc/uptime");
        if (file) {
            double uptime;
            file >> uptime;
            return uptime;
        }
        logger.error() << "Failed to open /proc/uptime" << std::endl;
#elif defined(_WIN32)
        FILETIME system_time;
        GetSystemTimeAsFileTime(&system_time);
        ULARGE_INTEGER time;
        time.LowPart = system_time.dwLowDateTime;
        time.HighPart = system_time.dwHighDateTime;
        return time.QuadPart / 10'000'000.0; // Convert 100-nanosecond units to seconds
#elif defined(__APPLE__)
        struct timeval boottime;
        size_t len = sizeof(boottime);
        if (sysctlbyname("kern.boottime", &boottime, &len, nullptr, 0) == 0) {
            auto now = std::chrono::system_clock::now().time_since_epoch();
            return std::chrono::duration<double>(now).count()
                - (boottime.tv_sec + boottime.tv_usec / 1'000'000.0);
        }
        logger.error() << "Failed to get system uptime" << std::endl;
/*
#elif defined(__FreeBSD__) \
    || defined(__OpenBSD__) \
    || defined(__NetBSD__) \
    || defined(__DragonFly__) \
    || defined(__BSD__)
        struct timeval boottime;
        size_t len = sizeof(boottime);
        int mib[2] = { CTL_KERN, KERN_BOOTTIME };
        if (sysctl(mib, 2, &boottime, &len, nullptr, 0) == 0) {
            auto now = std::chrono::system_clock::now().time_since_epoch();
            return std::chrono::duration<double>(now).count()
                - (boottime.tv_sec + boottime.tv_usec / 1'000'000.0);
        }
        logger.error() << "Failed to get system uptime" << std::endl;
*/
#else
        logger.error() << "Operating system is not supported" << std::endl;
#endif
        m_isValid = false;
        return -1.0;
    }

    double ProcessUsage::cpuTime() {
#ifdef __linux__
        std::ifstream file("/proc/self/stat");
        if (file) {
            std::string line;
            std::getline(file, line);
            std::istringstream iss(line);
            std::string token;
            for (int i = 0; i < 13; ++i) iss >> token;
            unsigned long utime, stime;
            iss >> utime >> stime;
            // Convert ticks to seconds:
            return (utime + stime) / (double)sysconf(_SC_CLK_TCK);
        }
        logger.error() << "Failed to open /proc/self/stat" << std::endl;
#elif defined(_WIN32)
        FILETIME creationTime, exitTime, kernelTime, userTime;
        if (GetProcessTimes(
                GetCurrentProcess(),
                &creationTime,
                &exitTime,
                &kernelTime,
                &userTime
            )
        ) {
            ULARGE_INTEGER kernel, user;
            kernel.LowPart = kernelTime.dwLowDateTime;
            kernel.HighPart = kernelTime.dwHighDateTime;
            user.LowPart = userTime.dwLowDateTime;
            user.HighPart = userTime.dwHighDateTime;
            // Convert 100-nanosecond units to seconds:
            return (kernel.QuadPart + user.QuadPart) / 10'000'000.0;
        }
        logger.error() << "Failed to get process times" << std::endl;
#elif defined(__APPLE__)
        task_t task = mach_task_self();
        thread_act_array_t threadList;
        mach_msg_type_number_t threadCount;
        kern_return_t kr = task_threads(task, &threadList, &threadCount);
        if (kr == KERN_SUCCESS) {
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
        }
        logger.error() << "Failed to get thread list" << std::endl;
/*
#elif defined(__FreeBSD__) \
    || defined(__OpenBSD__) \
    || defined(__NetBSD__) \
    || defined(__DragonFly__) \
    || defined(__BSD__)
        struct rusage usage;
        if (getrusage(RUSAGE_SELF, &usage) == 0) {
            // Total CPU time in seconds:
            return (usage.ru_utime.tv_sec + usage.ru_utime.tv_usec / 1'000'000.0)
                + (usage.ru_stime.tv_sec + usage.ru_stime.tv_usec / 1'000'000.0);
        }
        logger.error() << "Failed to get rusage" << std::endl;
*/
#else
        logger.error() << "Operating system is not supported" << std::endl;
#endif
        m_isValid = false;
        return -1.0;
    }

    int ProcessUsage::numCores() {
#ifdef __linux__
        return sysconf(_SC_NPROCESSORS_ONLN);
#elif defined(_WIN32)
        SYSTEM_INFO sysinfo;
        GetSystemInfo(&sysinfo);
        return sysinfo.dwNumberOfProcessors;
#elif defined(__APPLE__)
        int cores;
        size_t len = sizeof(cores);
        if (sysctlbyname("hw.ncpu", &cores, &len, nullptr, 0) == 0) {
            return cores;
        }
        logger.error() << "Failed to get number of CPU cores" << std::endl;
/*
#elif defined(__FreeBSD__) \
    || defined(__OpenBSD__) \
    || defined(__NetBSD__) \
    || defined(__DragonFly__) \
    || defined(__BSD__)
        int cores;
        size_t len = sizeof(cores);
        if (sysctlbyname("hw.ncpu", &cores, &len, nullptr, 0) == 0) {
            return cores;
        }
        logger.error() << "Failed to get number of CPU cores" << std::endl;
*/
#else
        logger.error() << "Operating system is not supported" << std::endl;
#endif
        m_isValid = false;
        return -1;
    }

    double ProcessUsage::cpuUsage() {
        double currentCpuTime = cpuTime();
        double currentUptime = uptime();

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

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
#include <sys/sysctl.h>
#include <sys/resource.h>
#endif

#include <server/process_usage.h>

namespace LCompilers::LLanguageServer {

    auto ProcessUsage::sample() -> ProcessUsage {
        ProcessUsage usage = {0, 0.0};

#if defined(_WIN32)
        // --- Windows Memory ---
        PROCESS_MEMORY_COUNTERS mem_info = {0};
        mem_info.cb = sizeof(mem_info);
        HANDLE process = GetCurrentProcess();
        if (GetProcessMemoryInfo(process, &mem_info, sizeof(mem_info))) {
            usage.memoryUtilization = mem_info.WorkingSetSize;
        }

        // --- Windows CPU Usage ---
        FILETIME creation_time, exit_time, kernel_time1, user_time1;
        if (GetProcessTimes(
                process,
                &creation_time,
                &exit_time,
                &kernel_time1,
                &user_time1
            )
        ) {
            uint64_t kernel1 =
                (static_cast<uint64_t>(kernel_time1.dwHighDateTime) << 32)
                | kernel_time1.dwLowDateTime;
            uint64_t user1 =
                (static_cast<uint64_t>(user_time1.dwHighDateTime) << 32)
                | user_time1.dwLowDateTime;
            uint64_t cpu1 = kernel1 + user1;

            // Sleep to measure difference
            std::this_thread::sleep_for(std::chrono::milliseconds(100));

            FILETIME kernel_time2, user_time2;
            if (GetProcessTimes(
                    process,
                    &creation_time,
                    &exit_time,
                    &kernel_time2,
                    &user_time2
                )
            ) {
                uint64_t kernel2 =
                    (static_cast<uint64_t>(kernel_time2.dwHighDateTime) << 32)
                    | kernel_time2.dwLowDateTime;
                uint64_t user2 =
                    (static_cast<uint64_t>(user_time2.dwHighDateTime) << 32)
                    | user_time2.dwLowDateTime;
                uint64_t cpu2 = kernel2 + user2;

                uint64_t cpu_diff = cpu2 - cpu1;
                // Convert to seconds (FILETIME is in 100-nanosecond intervals)
                double seconds = 0.1; // 100ms
                usage.cpuUtilization = (cpu_diff / (seconds * 10000000.0)) * 100.0;
            }
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
                        usage.memoryUtilization = std::stoull(value) * 1024; // Convert kB to bytes
                        break;
                    }
                }
            }
            status.close();
        }

        // --- Linux CPU Usage ---
        struct tms time1, time2;
        clock_t ticks1 = times(&time1);
        std::ifstream stat("/proc/self/stat");
        if (stat.is_open()) {
            std::string line;
            std::getline(stat, line);
            std::istringstream iss(line);
            std::string temp;
            uint64_t utime1, stime1;
            // Skip first 13 fields to reach utime (14) and stime (15)
            for (int i = 0; i < 13; ++i) iss >> temp;
            iss >> utime1 >> stime1;
            uint64_t cpu1 = utime1 + stime1;
            stat.close();

            // Sleep to measure difference
            std::this_thread::sleep_for(std::chrono::milliseconds(100));

            stat.open("/proc/self/stat");
            if (stat.is_open()) {
                std::getline(stat, line);
                iss.str(line);
                iss.clear();
                for (int i = 0; i < 13; ++i) iss >> temp;
                uint64_t utime2, stime2;
                iss >> utime2 >> stime2;
                uint64_t cpu2 = utime2 + stime2;

                ticks1 = times(&time1);
                clock_t ticks2 = times(&time2);
                if (ticks2 > ticks1) {
                    uint64_t cpu_diff = cpu2 - cpu1;
                    double seconds = (ticks2 - ticks1)
                        / static_cast<double>(sysconf(_SC_CLK_TCK));
                    usage.cpuUtilization = (cpu_diff / seconds) * 100.0;
                }
                stat.close();
            }
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
                static_cast<task_info_t>(&info),
                &count
            ) == KERN_SUCCESS
        ) {
            usage.memoryUtilization = info.resident_size;
        }

        // --- macOS CPU Usage ---
        struct rusage usage1, usage2;
        if (getrusage(RUSAGE_SELF, &usage1) == 0) {
            double cpu1 =
                usage1.ru_utime.tv_sec + usage1.ru_stime.tv_sec
                + (usage1.ru_utime.tv_usec + usage1.ru_stime.tv_usec) / 1000000.0;

            // Sleep to measure difference
            std::this_thread::sleep_for(std::chrono::milliseconds(100));

            if (getrusage(RUSAGE_SELF, &usage2) == 0) {
                double cpu2 =
                    usage2.ru_utime.tv_sec + usage2.ru_stime.tv_sec
                    + (usage2.ru_utime.tv_usec + usage2.ru_stime.tv_usec) / 1000000.0;
                double seconds = 0.1; // 100ms
                usage.cpuUtilization = ((cpu2 - cpu1) / seconds) * 100.0;
            }
        }
#endif

        return usage;
    }

} // namespace LCompilers::LLanguageServer

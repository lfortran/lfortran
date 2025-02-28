#pragma once

#include <atomic>
#include <cstddef>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <map>
#include <mutex>
#include <string>
#include <string_view>

namespace LCompilers::LLanguageServer::Logging {
    namespace fs = std::filesystem;

    enum class Level {
        LOG_LEVEL_OFF,
        LOG_LEVEL_FATAL,
        LOG_LEVEL_ERROR,
        LOG_LEVEL_WARN,
        LOG_LEVEL_INFO,
        LOG_LEVEL_DEBUG,
        LOG_LEVEL_TRACE,
        LOG_LEVEL_ALL,
    };

    extern const std::map<Level, std::string> LevelNames;
    extern const std::map<Level, std::string> LevelValues;

    auto levelByName(const std::string &name) -> Level;
    auto levelByValue(const std::string &value) -> Level;
    auto levelByValue(int value) -> Level;

    class Logger;

    class Formatter {
    public:
        Formatter(Logger &logger, Level level, const std::string &prompt);
        auto operator<<(bool boolean) -> Formatter &;
        auto operator<<(unsigned char c) -> Formatter &;
        auto operator<<(char c) -> Formatter &;
        auto operator<<(short unsigned int value) -> Formatter &;
        auto operator<<(int value) -> Formatter &;
        auto operator<<(unsigned int value) -> Formatter &;
        auto operator<<(std::size_t value) -> Formatter &;
        auto operator<<(const char *str) -> Formatter &;
        auto operator<<(const std::string &str) -> Formatter &;
        auto operator<<(const std::string_view &view) -> Formatter &;
        auto operator<<(std::ostream& (*manip)(std::ostream&)) -> Formatter &;
    private:
        Logger &logger;
        bool enabled = false;
        std::unique_lock<std::recursive_mutex> lock;
    };

    class Logger {
    public:
        Logger(const fs::path &logPath);
        ~Logger();

        auto mutex() -> std::recursive_mutex &;

        auto logPath() const -> const fs::path &;
        auto isOpen() const -> bool;
        auto close() -> void;

        auto setLevel(Level level) -> void;
        auto level() const -> Level;

        auto isOff() const -> bool;
        auto isFatalEnabled() const -> bool;
        auto isErrorEnabled() const -> bool;
        auto isWarnEnabled() const -> bool;
        auto isInfoEnabled() const -> bool;
        auto isDebugEnabled() const -> bool;
        auto isTraceEnabled() const -> bool;
        auto areAllEnabled() const -> bool;

        auto fatal() -> Formatter;
        auto error() -> Formatter;
        auto warn() -> Formatter;
        auto info() -> Formatter;
        auto debug() -> Formatter;
        auto trace() -> Formatter;

        auto operator<<(bool boolean) -> Logger &;
        auto operator<<(unsigned char c) -> Logger &;
        auto operator<<(char c) -> Logger &;
        auto operator<<(short unsigned int value) -> Logger &;
        auto operator<<(int value) -> Logger &;
        auto operator<<(unsigned int value) -> Logger &;
        auto operator<<(std::size_t value) -> Logger &;
        auto operator<<(const char *str) -> Logger &;
        auto operator<<(const std::string &str) -> Logger &;
        auto operator<<(const std::string_view &view) -> Logger &;
        auto operator<<(std::ostream& (*manip)(std::ostream&)) -> Logger &;
    private:
        fs::path _logPath;
        std::ofstream logFile;
        std::recursive_mutex _mutex;
        std::atomic<Level> _level{Level::LOG_LEVEL_INFO};
    };

} // namespace LCompilers::LLanguageServer::Logging

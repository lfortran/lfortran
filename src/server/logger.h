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
        OFF,
        FATAL,
        ERROR,
        WARN,
        INFO,
        DEBUG,
        TRACE,
        ALL,
    };

    extern std::map<Level, std::string> LevelNames;
    extern std::map<Level, std::string> LevelValues;

    auto levelByName(const std::string &name) -> Level;
    auto levelByValue(const std::string &value) -> Level;
    auto levelByValue(int value) -> Level;

    class Logger;

    class Formatter {
    public:
        Formatter(Logger &logger, Level level, const std::string &prompt);
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

        inline auto mutex() -> std::recursive_mutex & {
            return _mutex;
        }

        auto logPath() const -> const fs::path &;
        auto isOpen() const -> bool;
        auto close() -> void;

        auto inline setLevel(Level level) -> void {
            _level = level;
        }

        inline auto level() const -> Level {
            return _level;
        }

        inline auto isOff() const -> bool {
            return _level == Level::OFF;
        }

        inline auto isFatalEnabled() const -> bool {
            return _level >= Level::FATAL;
        }

        inline auto isErrorEnabled() const -> bool {
            return _level >= Level::ERROR;
        }

        inline auto isWarnEnabled() const -> bool {
            return _level >= Level::WARN;
        }

        inline auto isInfoEnabled() const -> bool {
            return _level >= Level::INFO;
        }

        inline auto isDebugEnabled() const -> bool {
            return _level >= Level::DEBUG;
        }

        inline auto isTraceEnabled() const -> bool {
            return _level >= Level::TRACE;
        }

        inline auto areAllEnabled() const -> bool {
            return _level == Level::ALL;
        }

        inline auto fatal() -> Formatter {
            return Formatter(*this, Level::FATAL, "FATAL");
        }

        inline auto error() -> Formatter {
            return Formatter(*this, Level::ERROR, "ERROR");
        }

        inline auto warn() -> Formatter {
            return Formatter(*this, Level::WARN, "WARN");
        }

        inline auto info() -> Formatter {
            return Formatter(*this, Level::INFO, "INFO");
        }

        inline auto debug() -> Formatter {
            return Formatter(*this, Level::DEBUG, "DEBUG");
        }

        inline auto trace() -> Formatter {
            return Formatter(*this, Level::TRACE, "TRACE");
        }

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
        std::atomic<Level> _level{Level::INFO};
    };

} // namespace LCompilers::LLanguageServer::Logging

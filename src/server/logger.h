#pragma once

#include <atomic>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <map>
#include <mutex>
#include <string>
#include <vector>

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

    auto formatTimePoint(
        const std::chrono::system_clock::time_point& tp
    ) -> std::string;

    class Formatter {
    public:
        Formatter(Logger &logger, Level level, const std::string &prompt);

        auto operator<<(std::ostream& (*manip)(std::ostream&)) -> Formatter &;

        template <typename T>
        auto operator<<(const T &value) -> Formatter &;

        template <typename T>
        auto operator<<(const T &&value) -> Formatter &;
    private:
        Logger &logger;
        bool enabled = false;
        std::unique_lock<std::recursive_mutex> lock;
    };

    template <typename T>
    auto Formatter::operator<<(const T &value) -> Formatter & {
        if (enabled) {
            logger << value;
        }
        return *this;
    }

    template <typename T>
    auto Formatter::operator<<(const T &&value) -> Formatter & {
        if (enabled) {
            logger << std::move(value);
        }
        return *this;
    }

    class Logger {
    public:
        Logger(const fs::path &logPath, const std::string &typeName);
        Logger(
            Logger *parent,
            const std::string &typeName
        );
        Logger(
            Logger *parent,
            const std::string &typeName,
            const std::vector<std::string> &&attributes
        );
        Logger(Logger &&logger) noexcept;
        ~Logger();

        auto having(const std::string &typeName) -> Logger;
        auto having(
            const std::string &typeName,
            const std::vector<std::string> &&attributes
        ) -> Logger;

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

        auto threadName(const std::string &threadName) -> void;
        auto threadName() const -> const std::string &;

        auto typeName() const -> const std::string &;
        auto attributes() const -> const std::vector<std::string> &;

        auto operator<<(bool boolean) -> Logger &;
        auto operator<<(const fs::path &path) -> Logger &;
        auto operator<<(std::ostream& (*manip)(std::ostream&)) -> Logger &;

        template <typename T>
        auto operator<<(const T &value) -> Logger &;

        template <typename T>
        auto operator<<(const T &&value) -> Logger &;
    private:
        fs::path _logPath;
        std::ofstream logFile;
        std::recursive_mutex _mutex;
        std::atomic<Level> _level{Level::LOG_LEVEL_INFO};
        Logger *parent;
        std::string m_typeName;
        std::vector<std::string> m_attributes;
    };

    template <typename T>
    auto Logger::operator<<(const T &value) -> Logger & {
        if (parent) {
            parent->operator<<(value);
        } else {
            logFile << value;
            std::cerr << value;
        }
        return *this;
    }

    template <typename T>
    auto Logger::operator<<(const T &&value) -> Logger & {
        if (parent) {
            parent->operator<<(std::move(value));
        } else {
            logFile << value;
            std::cerr << value;
        }
        return *this;
    }
} // namespace LCompilers::LLanguageServer::Logging

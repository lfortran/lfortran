#include <stdexcept>

#include <server/logger.h>

namespace LCompilers::LLanguageServer::Logging {

    Logger::Logger(const fs::path &logPath)
        : _logPath(logPath)
        , logFile(logPath, std::ios::out | std::ios::trunc)
    {
        if (!logFile.is_open()) {
            throw std::invalid_argument(
                "Failed to open log file for writing: " + logPath.string()
            );
        }
    }

    Logger::~Logger() {
        if (isOpen()) {
            close();
        }
    }

    auto Logger::mutex() -> std::recursive_mutex & {
        return _mutex;
    }

    auto Logger::setLevel(Level level) -> void {
        _level = level;
    }

    auto Logger::level() const -> Level {
        return _level;
    }

    auto Logger::isOff() const -> bool {
        return _level == Level::OFF;
    }

    auto Logger::isFatalEnabled() const -> bool {
        return _level >= Level::FATAL;
    }

    auto Logger::isErrorEnabled() const -> bool {
        return _level >= Level::ERROR;
    }

    auto Logger::isWarnEnabled() const -> bool {
        return _level >= Level::WARN;
    }

    auto Logger::isInfoEnabled() const -> bool {
        return _level >= Level::INFO;
    }

    auto Logger::isDebugEnabled() const -> bool {
        return _level >= Level::DEBUG;
    }

    auto Logger::isTraceEnabled() const -> bool {
        return _level >= Level::TRACE;
    }

    auto Logger::areAllEnabled() const -> bool {
        return _level == Level::ALL;
    }

    auto Logger::fatal() -> Formatter {
        return Formatter(*this, Level::FATAL, "FATAL");
    }

    auto Logger::error() -> Formatter {
        return Formatter(*this, Level::ERROR, "ERROR");
    }

    auto Logger::warn() -> Formatter {
        return Formatter(*this, Level::WARN, "WARN");
    }

    auto Logger::info() -> Formatter {
        return Formatter(*this, Level::INFO, "INFO");
    }

    auto Logger::debug() -> Formatter {
        return Formatter(*this, Level::DEBUG, "DEBUG");
    }

    auto Logger::trace() -> Formatter {
        return Formatter(*this, Level::TRACE, "TRACE");
    }

    std::map<Level, std::string> LevelNames = {
        {Level::OFF, "OFF"},
        {Level::FATAL, "FATAL"},
        {Level::ERROR, "ERROR"},
        {Level::WARN, "WARN"},
        {Level::INFO, "INFO"},
        {Level::DEBUG, "DEBUG"},
        {Level::TRACE, "TRACE"},
        {Level::ALL, "ALL"},
    };

    std::map<Level, std::string> LevelValues = {
        {Level::OFF, "off"},
        {Level::FATAL, "fatal"},
        {Level::ERROR, "error"},
        {Level::WARN, "warn"},
        {Level::INFO, "info"},
        {Level::DEBUG, "debug"},
        {Level::TRACE, "trace"},
        {Level::ALL, "all"},
    };

    auto levelByName(const std::string &name) -> Level {
        for (const auto &[enum_name, enum_value] : LevelNames) {
            if (name == enum_value) {
                return enum_name;
            }
        }
        throw std::invalid_argument("Invalid Level name: " + name);
    }

    auto levelByValue(const std::string &value) -> Level {
        for (const auto &[enum_name, enum_value] : LevelValues) {
            if (value == enum_value) {
                return enum_name;
            }
        }
        throw std::invalid_argument("Invalid Level value: " + value);
    }

    auto levelByValue(int value) -> Level {
        for (const auto &[enum_name, enum_value] : LevelNames) {
            if (value == static_cast<int>(enum_name)) {
                return enum_name;
            }
        }
        throw std::invalid_argument(
            "Invalid Level value: " + std::to_string(value)
        );
    }

    Formatter::Formatter(Logger &logger, Level level, const std::string &prompt)
        : logger(logger)
        , lock(logger.mutex(), std::defer_lock)
    {
        if (logger.level() >= level) {
            lock.lock();
            if (logger.level() >= level) {
                enabled = true;
                logger << '[' << prompt << "] ";
            } else {
                lock.unlock();
            }
        }
    }

    auto Formatter::operator<<(unsigned char c) -> Formatter & {
        if (enabled) {
            logger << c;
        }
        return *this;
    }

    auto Formatter::operator<<(char c) -> Formatter & {
        if (enabled) {
            logger << c;
        }
        return *this;
    }

    auto Formatter::operator<<(short unsigned int value) -> Formatter & {
        if (enabled) {
            logger << value;
        }
        return *this;
    }

    auto Formatter::operator<<(int value) -> Formatter & {
        if (enabled) {
            logger << value;
        }
        return *this;
    }

    auto Formatter::operator<<(unsigned int value) -> Formatter & {
        if (enabled) {
            logger << value;
        }
        return *this;
    }

    auto Formatter::operator<<(std::size_t value) -> Formatter & {
        if (enabled) {
            logger << value;
        }
        return *this;
    }

    auto Formatter::operator<<(const char *str) -> Formatter & {
        if (enabled) {
            logger << str;
        }
        return *this;
    }

    auto Formatter::operator<<(const std::string &str) -> Formatter & {
        if (enabled) {
            logger << str;
        }
        return *this;
    }

    auto Formatter::operator<<(const std::string_view &view) -> Formatter & {
        if (enabled) {
            logger << view;
        }
        return *this;
    }

    auto Formatter::operator<<(std::ostream& (*manip)(std::ostream&)) -> Formatter & {
        if (enabled) {
            logger << manip;
        }
        return *this;
    }

    auto Logger::logPath() const -> const fs::path & {
        return _logPath;
    }

    auto Logger::isOpen() const -> bool {
        return logFile.is_open();
    }

    auto Logger::close() -> void {
        if (logFile.is_open()) {
            logFile.flush();
            logFile.close();
        } else {
            throw std::runtime_error("Logger has already been closed.");
        }
    }

    auto Logger::operator<<(unsigned char c) -> Logger & {
        logFile << c;
        std::cerr << c;
        return *this;
    }

    auto Logger::operator<<(char c) -> Logger & {
        logFile << c;
        std::cerr << c;
        return *this;
    }

    auto Logger::operator<<(short unsigned int value) -> Logger & {
        logFile << value;
        std::cerr << value;
        return *this;
    }

    auto Logger::operator<<(int value) -> Logger & {
        logFile << value;
        std::cerr << value;
        return *this;
    }

    auto Logger::operator<<(unsigned int value) -> Logger & {
        logFile << value;
        std::cerr << value;
        return *this;
    }

    auto Logger::operator<<(std::size_t value) -> Logger & {
        logFile << value;
        std::cerr << value;
        return *this;
    }

    auto Logger::operator<<(const char *str) -> Logger & {
        logFile << str;
        std::cerr << str;
        return *this;
    }

    auto Logger::operator<<(const std::string &str) -> Logger & {
        logFile << str;
        std::cerr << str;
        return *this;
    }

    auto Logger::operator<<(const std::string_view &view) -> Logger & {
        logFile << view;
        std::cerr << view;
        return *this;
    }

    auto Logger::operator<<(std::ostream& (*manip)(std::ostream&)) -> Logger & {
        logFile << manip;
        std::cerr << manip;
        return *this;
    }

} // namespace LCompilers::LLanguageServer::Logging

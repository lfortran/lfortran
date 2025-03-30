#include <chrono>
#include <iomanip>
#include <stdexcept>

#include <server/logger.h>

namespace LCompilers::LLanguageServer::Logging {

    const std::map<Level, std::string> LevelNames = {
        {Level::LOG_LEVEL_OFF, "LOG_LEVEL_OFF"},
        {Level::LOG_LEVEL_FATAL, "LOG_LEVEL_FATAL"},
        {Level::LOG_LEVEL_ERROR, "LOG_LEVEL_ERROR"},
        {Level::LOG_LEVEL_WARN, "LOG_LEVEL_WARN"},
        {Level::LOG_LEVEL_INFO, "LOG_LEVEL_INFO"},
        {Level::LOG_LEVEL_DEBUG, "LOG_LEVEL_DEBUG"},
        {Level::LOG_LEVEL_TRACE, "LOG_LEVEL_TRACE"},
        {Level::LOG_LEVEL_ALL, "LOG_LEVEL_ALL"},
    };

    const std::map<Level, std::string> LevelValues = {
        {Level::LOG_LEVEL_OFF, "off"},
        {Level::LOG_LEVEL_FATAL, "fatal"},
        {Level::LOG_LEVEL_ERROR, "error"},
        {Level::LOG_LEVEL_WARN, "warn"},
        {Level::LOG_LEVEL_INFO, "info"},
        {Level::LOG_LEVEL_DEBUG, "debug"},
        {Level::LOG_LEVEL_TRACE, "trace"},
        {Level::LOG_LEVEL_ALL, "all"},
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

                auto now = std::chrono::system_clock::now();
                std::time_t time_now = std::chrono::system_clock::to_time_t(now);
                std::tm utc_tm = *std::gmtime(&time_now);
                auto duration = now.time_since_epoch();
                auto milliseconds = std::chrono::duration_cast<std::chrono::milliseconds>(duration).count() % 1000;
                logger << '[' << std::put_time(&utc_tm, "%Y-%m-%d %H:%M:%S")
                       << "." << std::setfill('0') << std::setw(3) << milliseconds << " UTC]";

                logger << '[' << logger.typeName() << ']';
                for (const std::string &attribute : logger.attributes()) {
                    logger << '[' << attribute << ']';
                }
                if (logger.threadName().length() > 0) {
                    logger << '[' << logger.threadName() << ']';
                }
                logger << '[' << prompt << ']';
                logger << ' ';
            } else {
                lock.unlock();
            }
        }
    }

    auto Formatter::operator<<(std::ostream& (*manip)(std::ostream&)) -> Formatter & {
        if (enabled) {
            logger << manip;
        }
        return *this;
    }

    Logger::Logger(const fs::path &logPath, const std::string &typeName)
        : _logPath(logPath)
        , logFile(logPath, std::ios::out | std::ios::trunc)
        , parent(nullptr)
        , m_typeName(typeName)
    {
        if (!logFile.is_open()) {
            throw std::invalid_argument(
                "Failed to open log file for writing: " + logPath.string()
            );
        }
        info() << "Logging to: " << logPath << std::endl;
    }

    Logger::Logger(
        Logger *parent,
        const std::string &typeName
    ) : parent(parent)
      , m_typeName(typeName)
    {
        // empty
    }

    Logger::Logger(
        Logger *parent,
        const std::string &typeName,
        const std::vector<std::string> &&attributes
    ) : parent(parent)
      , m_typeName(typeName)
      , m_attributes(attributes)
    {
        // empty
    }

    Logger::Logger(Logger &&logger) noexcept
        : _logPath(std::move(logger._logPath))
        , logFile(std::move(logger.logFile))
        , _level(logger._level.load())
        , parent(logger.parent)
        , m_typeName(std::move(logger.m_typeName))
        , m_attributes(std::move(logger.m_attributes))
    {
        // empty
    }

    Logger::~Logger() {
        if (!parent && isOpen()) {
            close();
        }
    }

    auto Logger::having(const std::string &typeName) -> Logger {
        return Logger(this, typeName);
    }

    auto Logger::having(
        const std::string &typeName,
        const std::vector<std::string> &&attributes
    ) -> Logger {
        return Logger(this, typeName, std::move(attributes));
    }

    auto Logger::mutex() -> std::recursive_mutex & {
        if (parent) {
            return parent->mutex();
        }
        return _mutex;
    }

    auto Logger::setLevel(Level level) -> void {
        if (parent) {
            parent->setLevel(level);
        } else {
            _level = level;
        }
    }

    auto Logger::level() const -> Level {
        if (parent) {
            return parent->level();
        }
        return _level;
    }

    auto Logger::isOff() const -> bool {
        if (parent) {
            return parent->isOff();
        }
        return _level == Level::LOG_LEVEL_OFF;
    }

    auto Logger::isFatalEnabled() const -> bool {
        if (parent) {
            return parent->isFatalEnabled();
        }
        return _level >= Level::LOG_LEVEL_FATAL;
    }

    auto Logger::isErrorEnabled() const -> bool {
        if (parent) {
            return parent->isErrorEnabled();
        }
        return _level >= Level::LOG_LEVEL_ERROR;
    }

    auto Logger::isWarnEnabled() const -> bool {
        if (parent) {
            return parent->isWarnEnabled();
        }
        return _level >= Level::LOG_LEVEL_WARN;
    }

    auto Logger::isInfoEnabled() const -> bool {
        if (parent) {
            return parent->isInfoEnabled();
        }
        return _level >= Level::LOG_LEVEL_INFO;
    }

    auto Logger::isDebugEnabled() const -> bool {
        if (parent) {
            return parent->isDebugEnabled();
        }
        return _level >= Level::LOG_LEVEL_DEBUG;
    }

    auto Logger::isTraceEnabled() const -> bool {
        if (parent) {
            return parent->isTraceEnabled();
        }
        return _level >= Level::LOG_LEVEL_TRACE;
    }

    auto Logger::areAllEnabled() const -> bool {
        if (parent) {
            return parent->areAllEnabled();
        }
        return _level == Level::LOG_LEVEL_ALL;
    }

    auto Logger::fatal() -> Formatter {
        return Formatter(*this, Level::LOG_LEVEL_FATAL, "FATAL");
    }

    auto Logger::error() -> Formatter {
        return Formatter(*this, Level::LOG_LEVEL_ERROR, "ERROR");
    }

    auto Logger::warn() -> Formatter {
        return Formatter(*this, Level::LOG_LEVEL_WARN, "WARN");
    }

    auto Logger::info() -> Formatter {
        return Formatter(*this, Level::LOG_LEVEL_INFO, "INFO");
    }

    auto Logger::debug() -> Formatter {
        return Formatter(*this, Level::LOG_LEVEL_DEBUG, "DEBUG");
    }

    auto Logger::trace() -> Formatter {
        return Formatter(*this, Level::LOG_LEVEL_TRACE, "TRACE");
    }

    auto Logger::logPath() const -> const fs::path & {
        if (parent) {
            return parent->logPath();
        }
        return _logPath;
    }

    auto Logger::isOpen() const -> bool {
        if (parent) {
            return parent->isOpen();
        }
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

    static thread_local std::string m_threadName;

    auto Logger::threadName(const std::string &threadName) -> void {
        m_threadName = threadName;
    }

    auto Logger::threadName() const -> const std::string & {
        return m_threadName;
    }

    auto Logger::typeName() const -> const std::string & {
        return m_typeName;
    }

    auto Logger::attributes() const -> const std::vector<std::string> & {
        return m_attributes;
    }

    auto Logger::operator<<(bool boolean) -> Logger & {
        return this->operator<<(boolean ? "true" : "false");
    }

    auto Logger::operator<<(const fs::path &path) -> Logger & {
        return this->operator<<(path.string());
    }

    auto Logger::operator<<(std::ostream& (*manip)(std::ostream&)) -> Logger & {
        if (parent) {
            parent->operator<<(manip);
        } else {
            logFile << manip;
            std::cerr << manip;
        }
        return *this;
    }

} // namespace LCompilers::LLanguageServer::Logging

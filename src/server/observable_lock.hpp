#pragma once

#include <string>

namespace LCompilers::LLanguageServer::Threading {

    template <typename ObserverType, typename LockType>
    class ObservableLock {
    public:
        ObservableLock(
            ObserverType &observer,
            const char *file,
            int line,
            std::string &&identifier,
            LockType &&lock
        );
        ObservableLock(ObservableLock &&other) noexcept;
        ~ObservableLock();
        auto identifier() const -> const std::string &;
        auto file() const -> const char *;
        auto line() const -> int;
        auto operator*() -> LockType &;
        auto operator->() -> LockType *;
        auto lock() -> void;
        auto unlock() -> void;
    private:
        ObserverType &m_observer;
        const char *m_file;
        int m_line;
        std::string m_identifier;
        LockType m_lock;
    };

    template <typename ObserverType, typename LockType>
    ObservableLock<ObserverType, LockType>::ObservableLock(
        ObserverType &observer,
        const char *file,
        int line,
        std::string &&identifier,
        LockType &&lock
    ) : m_observer(observer)
      , m_file(file)
      , m_line(line)
      , m_identifier(std::move(identifier))
      , m_lock(std::move(lock))
    {
        // empty
    }

    template <typename ObserverType, typename LockType>
    ObservableLock<ObserverType, LockType>::ObservableLock(
        ObservableLock &&other
    ) noexcept
        : m_observer(other.m_observer)
        , m_file(other.m_file)
        , m_line(other.m_line)
        , m_identifier(std::move(other.m_identifier))
        , m_lock(std::move(other.m_lock))
    {
        // empty
    }

    template <typename ObserverType, typename LockType>
    ObservableLock<ObserverType, LockType>::~ObservableLock() {
        unlock();
    }

    template <typename ObserverType, typename LockType>
    auto ObservableLock<ObserverType, LockType>::identifier() const -> const std::string & {
        return m_identifier;
    }

    template <typename ObserverType, typename LockType>
    auto ObservableLock<ObserverType, LockType>::file() const -> const char * {
        return m_file;
    }

    template <typename ObserverType, typename LockType>
    auto ObservableLock<ObserverType, LockType>::line() const -> int {
        return m_line;
    }

    template <typename ObserverType, typename LockType>
    auto ObservableLock<ObserverType, LockType>::operator*() -> LockType & {
        return m_lock;
    }

    template <typename ObserverType, typename LockType>
    auto ObservableLock<ObserverType, LockType>::operator->() -> LockType * {
        return &m_lock;
    }

    template <typename ObserverType, typename LockType>
    auto ObservableLock<ObserverType, LockType>::lock() -> void {
        m_observer.acquire(*this);
    }

    template <typename ObserverType, typename LockType>
    auto ObservableLock<ObserverType, LockType>::unlock() -> void {
        m_observer.release(*this);
    }

} // namespace LCompilers::LLanguageServer::Threading

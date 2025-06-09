#ifndef LFORTRAN_EXCEPTION_H
#define LFORTRAN_EXCEPTION_H

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    LFORTRAN_NO_EXCEPTION    = 0,
    LFORTRAN_RUNTIME_ERROR   = 1,
    LFORTRAN_EXCEPTION       = 2,
    LFORTRAN_PARSER_ERROR    = 4,
    LFORTRAN_ASSERT_FAILED   = 7,
    LFORTRAN_ASSEMBLER_ERROR = 8,
} lfortran_exceptions_t;

#ifdef __cplusplus
}
#endif


#ifdef __cplusplus

#include <exception>
#include <string>
#include <libasr/location.h>
#include <libasr/config.h>
#include <libasr/stacktrace.h>
#include <libasr/diagnostics.h>

namespace LCompilers {

/*
    This Error structure is returned in Result when failure happens.

    The diagnostic messages contain warnings and error(s). In the past when
    just one error was returned, it made sense to return it as part of the
    Error structure in Result. However now when warnings are also reported,
    those we want to return in any case. Also since multiple errors can be
    reported, they are now storted independently of the exception that is
    internally used to abort the analysis (typically a visitor pattern).

    For the above reasons the diagnostic messages are now returned as an
    argument, independently of the Result<T>, and they can contain messages on
    success also. On failure they contain at least one error message (and in
    addition can contain warnings and more error messages).

    Consequently, we do not currently store anything in the Error structure
    below.
*/
struct Error {
};

struct ErrorMessage {
    std::string message;

    ErrorMessage(const std::string& msg)
        : message(msg)
    {}
};

template<typename T, typename E = Error>
struct Result {
    bool ok;
    union {
        T result;
        E error;
    };
    // Default constructor
    Result() = delete;
    // Success result constructor
    Result(const T &result) : ok{true}, result{result} {}
    // Error result constructor
    Result(const Error &error) : ok{false}, error{error} {}
    // Destructor
    ~Result() {
        if (!ok) {
            error.~E();
        }
    }
    // Copy constructor
    Result(const Result& other)
        : ok{ other.ok } {
        if (ok) {
            new (&result) T(other.result);
        } else {
            new (&error) E(other.error);
        }
    }
    // Copy assignment
    Result& operator=(const Result& other) {
        if (this != &other) {
            this->~Result();
            new (this) Result(other);
        }
        return *this;
    }
    // Move constructor for success
    Result(T&& res) : ok(true), result{std::move(res)} {}
    // Move constructor for error (optional)
    Result(E&& err) : ok(false), error{std::move(err)} {}
    // Move constructor
    Result(Result&& other)
        : ok(other.ok) {
        if (ok) {
            new (&result) T(std::move(other.result));
        } else {
            new (&error) E(std::move(other.error));
        }
    }
    // Move assignment
    Result<T>&& operator=(T&& other) = delete;
};


const int stacktrace_depth = 4;

class LCompilersException : public std::exception
{
    std::string m_msg;
    lfortran_exceptions_t ec;
    std::vector<StacktraceItem> m_stacktrace_addresses;
public:
    LCompilersException(const std::string &msg, lfortran_exceptions_t error)
        : m_msg{msg}, ec{error}, m_stacktrace_addresses{get_stacktrace_addresses()}
    { }
    LCompilersException(const std::string &msg)
        : LCompilersException(msg, LFORTRAN_EXCEPTION)
    {
    }
    const char *what() const throw()
    {
        return m_msg.c_str();
    }
    std::string msg() const
    {
        return m_msg;
    }
    std::string name() const
    {
        switch (ec) {
            case (lfortran_exceptions_t::LFORTRAN_EXCEPTION) :
                return "LCompilersException";
            case (lfortran_exceptions_t::LFORTRAN_ASSERT_FAILED) :
                return "AssertFailed";
            default : return "Unknown Exception";
        }
    }
    std::vector<StacktraceItem> stacktrace_addresses() const
    {
        return m_stacktrace_addresses;
    }
    lfortran_exceptions_t error_code()
    {
        return ec;
    }
};

class AssertFailed : public LCompilersException
{
public:
    AssertFailed(const std::string &msg)
        : LCompilersException(msg, LFORTRAN_ASSERT_FAILED)
    {
    }
};

class AssemblerError : public LCompilersException
{
public:
    AssemblerError(const std::string &msg)
        : LCompilersException(msg, LFORTRAN_ASSEMBLER_ERROR)
    {
    }
};

template<typename T>
static inline T TRY(Result<T> result) {
    if (result.ok) {
        return result.result;
    } else {
        throw LCompilersException("Internal Compiler Error: TRY failed, error was not handled explicitly");
    }
}


} // namespace LCompilers

#endif // __cplusplus
#endif // LFORTRAN_EXCEPTION_H

#ifndef LFORTRAN_STRING_UTILS_H
#define LFORTRAN_STRING_UTILS_H

#include <string>
#include <vector>
#include <cctype>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <cstdlib>
#include <cerrno>
#include <cstdint>
#include <cstring>

#include <libasr/alloc.h>
#include <libasr/containers.h>

namespace LCompilers {

// Converts a double to string preserving full precision.
// Uses setprecision(17) currently which is enough for 64-bit real(8) values. We can set this to 34 for 128-bit real(16) values in the future.
// For NaN values, the exact bit pattern is preserved using the
// format "nan:XXXXXXXXXXXXXXXX" (16 hex digits) so that signaling NaN
// payloads survive round-tripping through string storage.
inline std::string double_to_str_precision(double x) {
    if (std::isnan(x)) {
        uint64_t bits;
        std::memcpy(&bits, &x, sizeof(bits));
        std::ostringstream oss;
        oss << "nan:";
        oss << std::hex << std::setfill('0') << std::setw(16) << bits;
        return oss.str();
    }
    if (std::isinf(x)) return x > 0 ? "inf" : "-inf";
    std::ostringstream oss;
    oss << std::scientific << std::setprecision(17) << x;
    return oss.str();
}

// Safe string-to-double conversion that handles subnormals, special values,
// and NaN bit patterns encoded as "nan:XXXXXXXXXXXXXXXX".
// std::stod throws out_of_range for subnormals (e.g., 4.9e-324), while
// strtod from <cstdlib> handles them correctly by returning the denormalized
// value and setting errno to ERANGE without losing the value.
inline double str_to_double(const std::string &s) {
    // Recover exact NaN bit pattern
    if (s.size() > 4 && s.substr(0, 4) == "nan:") {
        uint64_t bits = std::stoull(s.substr(4), nullptr, 16);
        double result;
        std::memcpy(&result, &bits, sizeof(result));
        return result;
    }
    char *end;
    errno = 0;
    double result = std::strtod(s.c_str(), &end);
    if (end == s.c_str()) {
        throw std::invalid_argument("str_to_double: no conversion: " + s);
    }
    // errno == ERANGE is OK for subnormals — strtod still returns
    // the best representable value.
    return result;
}


bool startswith(const std::string &s, const std::string &e);
bool endswith(const std::string &s, const std::string &e);
std::string to_lower(const std::string &s);
std::vector<std::string> string_split(const std::string &s,
    const std::string &split_string, bool strs_to_lower=true);
std::vector<std::string> string_split_avoid_parentheses(const std::string &s,
    bool strs_to_lower=true);
std::vector<std::string> split(const std::string &s);
std::string join(const std::string j, const std::vector<std::string> &v);
std::vector<std::string> slice(const std::vector<std::string> &v,
        int start=0, int end=-1);
char *s2c(Allocator &al, const std::string &s);

// Replaces all occurrences of `regex` (a regular expression, must escape
// special characters) with `replace`
std::string replace(const std::string &s,
    const std::string &regex, const std::string &replace);

// Reads a file, returns success/fail as a result
bool read_file(const std::string &filename, std::string &text);
// Reads a file, aborts on failure
std::string read_file_ok(const std::string &filename);

// Returns the parent path to the given path
std::string parent_path(const std::string &path);
// Returns true if the path is relative
bool is_relative_path(const std::string &path);
// Joins paths (paths can be empty)
std::string join_paths(const std::vector<std::string> &paths);

// Escapes special characters from the given string
// using C style escaping
std::string str_escape_c(const std::string &s);
char* str_unescape_c(Allocator &al, LCompilers::Str &s);

// Escapes double quote characters from the given string
// given string must be enclosed in double quotes
std::string str_escape_fortran_double_quote(const std::string &s);
char* str_unescape_fortran(Allocator &al, LCompilers::Str &s, char ch);

bool str_compare(const unsigned char *pos, std::string s);
void rtrim(std::string& str);

} // namespace LCompilers

#endif // LFORTRAN_STRING_UTILS_H

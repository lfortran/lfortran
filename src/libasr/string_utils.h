#ifndef LFORTRAN_STRING_UTILS_H
#define LFORTRAN_STRING_UTILS_H

#include <string>
#include <vector>
#include <cstdint>
#include <cctype>

#include <libasr/alloc.h>
#include <libasr/containers.h>

namespace LCompilers {


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

// UTF-8 helpers ------------------------------------------------------------
//
// A CHARACTER value of kind > 1 is stored in ASR as UTF-8 in
// `StringConstant::m_s` (a `char*` cannot carry the embedded null bytes that
// raw UCS-4 would produce), while its ASR length is a count of characters.
// These helpers convert between the two.

// Returns true if `value` is well-formed UTF-8. Overlong forms, surrogates and
// out-of-range code points are rejected.
bool is_valid_utf8(const char *value, size_t size);
bool is_valid_utf8(const std::string &value);

// Decodes well-formed UTF-8 into code points. The input must already have
// passed `is_valid_utf8`.
std::vector<uint32_t> utf8_decode(const std::string &s);

// Number of Unicode code points in `s`, i.e. the Fortran length of the value
// `s` encodes. Malformed trailing bytes count as one character each, so this
// never reads past the end of `s`.
size_t utf8_codepoint_count(const std::string &s);
size_t utf8_codepoint_count(const char *s);

// Appends the UTF-8 encoding of a single code point to `out`.
void utf8_encode_codepoint(std::string &out, uint32_t code_point);

// Decodes UTF-8 and serialises each code point as `kind` little-endian bytes:
// kind 2 gives UCS-2, kind 4 gives UCS-4. This is the in-memory form the
// backends use.
std::vector<uint8_t> utf8_to_unicode_bytes(const std::string &s, int kind);

// The inverse of `utf8_to_unicode_bytes`: reads `count` code points of `kind`
// little-endian bytes each and returns their UTF-8 encoding.
std::string unicode_bytes_to_utf8(const uint8_t *bytes, size_t count, int kind);

// Splits UTF-8 into one string per code point. Malformed trailing bytes are
// returned as single byte entries rather than dropped.
std::vector<std::string> utf8_split(const std::string &s);

} // namespace LCompilers

#endif // LFORTRAN_STRING_UTILS_H

#include <cctype>
#include <regex>
#include <algorithm>
#include <string>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>

#include <libasr/string_utils.h>
#include <libasr/containers.h>

namespace LCompilers {


bool startswith(const std::string &s, const std::string &e)
{
    if (s.size() < e.size()) return false;
    return s.substr(0, e.size()) == e;
}

bool endswith(const std::string &s, const std::string &e)
{
    if (s.size() < e.size()) return false;
    return s.substr(s.size()-e.size()) == e;
}

std::string to_lower(const std::string &s) {
    std::string res = s;
    std::transform(res.begin(), res.end(), res.begin(),
        [](unsigned char c){ return std::tolower(c); });
    return res;
}

char *s2c(Allocator &al, const std::string &s) {
    Str x; x.from_str_view(s);
    return x.c_str(al);
}

// Splits the string `s` using the separator `split_string`
std::vector<std::string> string_split(const std::string &s,
    const std::string &split_string, bool strs_to_lower)
{
    std::vector<std::string> result;
    size_t old_pos = 0;
    size_t new_pos;
    std::string substr;
    while ((new_pos = s.find(split_string, old_pos)) != std::string::npos) {
        substr = s.substr(old_pos, new_pos-old_pos);
        if (substr.size() > 0)
            result.push_back(strs_to_lower ? to_lower(substr) : substr);
        old_pos = new_pos+split_string.size();
    }
    substr = s.substr(old_pos);
    result.push_back(strs_to_lower ? to_lower(substr) : substr);
    return result;
}

std::vector<std::string> string_split_avoid_parentheses(const std::string &str, bool strs_to_lower) {
    std::vector<std::string> result;
    std::string word;
    bool in_brackets = false;
    for (char ch : str) {
        if (ch == ' ' && !in_brackets) {
            if (!word.empty()) {
                result.push_back(strs_to_lower ? LCompilers::to_lower(word) : word);
                word.clear();
            }
        } else {
            if (ch == '(') in_brackets = true;
            if (ch == ')') in_brackets = false;
            word += ch;
        }
    }
    if (!word.empty()) result.push_back(strs_to_lower ? LCompilers::to_lower(word) : word);
    return result;
}

// Splits the string `s` using any space or newline
std::vector<std::string> split(const std::string &s)
{
    std::vector<std::string> result;
    std::string split_chars = " \n";
    size_t old_pos = 0;
    size_t new_pos;
    while ((new_pos = s.find_first_of(split_chars, old_pos)) != std::string::npos) {
        std::string substr = s.substr(old_pos, new_pos-old_pos);
        if (substr.size() > 0) result.push_back(substr);
        old_pos = new_pos+1;
    }
    result.push_back(s.substr(old_pos));
    return result;
}

std::string join(const std::string j, const std::vector<std::string> &l)
{
    std::string result;
    for (size_t i=0; i<l.size(); i++) {
        result += l[i];
        if (i < l.size()-1) result += j;
    }
    return result;
}

std::vector<std::string> slice(const std::vector<std::string>& v, int start, int end)
{
    int oldlen = v.size();
    int newlen;

    if ((end == -1) || (end >= oldlen)) {
        newlen = oldlen-start;
    } else {
        newlen = end-start;
    }

    std::vector<std::string> nv(newlen);

    for (int i=0; i<newlen; i++) {
        nv[i] = v[start+i];
    }
    return nv;
}

std::string replace(const std::string &s,
    const std::string &regex, const std::string &replace)
{
    return std::regex_replace(s, std::regex(regex), replace);
}

bool read_file(const std::string &filename, std::string &text)
{
    if (filename.empty()) return false;
    std::ifstream ifs(filename.c_str(), std::ios::in | std::ios::binary
            | std::ios::ate);
    if (!ifs.is_open()) return false;

    std::ifstream::pos_type filesize = ifs.tellg();
    if (filesize < 0) return false;
    const std::size_t size = static_cast<std::size_t>(filesize);
    if (size == 0) {
        text.clear();
        return true;
    }

    ifs.seekg(0, std::ios::beg);

    std::vector<char> bytes(size);
    ifs.read(bytes.data(), filesize);

    text.assign(bytes.data(), size);
    return true;
}

std::string read_file_ok(const std::string &filename) {
    std::string text;
    if (read_file(filename, text)) {
        return text;
    } else {
        std::cerr << "File '" + filename + "' cannot be opened." << std::endl;
        abort();
    }
}


std::string parent_path(const std::string &path) {
    int pos = path.size()-1;
    while (pos >= 0 && path[pos] != '/') pos--;
    if (pos == -1) {
        return "";
    } else {
        return path.substr(0, pos);
    }
}

bool is_relative_path(const std::string &path) {
    return !startswith(path, "/");
}

std::string join_paths(const std::vector<std::string> &paths) {
    std::string p;
    std::string delim = "/";
    for (auto &path : paths) {
        if (path.size() > 0) {
            if (p.size() > 0 && !endswith(p, delim)) {
                p.append(delim);
            }
            p.append(path);
        }
    }
    return p;
}

std::string str_escape_c(const std::string &s) {
    std::ostringstream o;
    for (auto c = s.cbegin(); c != s.cend(); c++) {
        switch (*c) {
            case '"': o << "\\\""; break;
            case '\\': o << "\\\\"; break;
            case '\b': o << "\\b"; break;
            case '\f': o << "\\f"; break;
            case '\n': o << "\\n"; break;
            case '\r': o << "\\r"; break;
            case '\t': o << "\\t"; break;
            case '\v': o << "\\v"; break;
            default:
                if ('\x00' <= *c && *c <= '\x1f') {
                    o << "\\u"
                    << std::hex << std::setw(4) << std::setfill('0') << static_cast<int>(*c);
                } else {
                    o << *c;
                }
        }
    }
    return o.str();
}

char* str_unescape_c(Allocator &al, LCompilers::Str &s) {
    std::string x = "";
    size_t idx = 0;
    for (; idx + 1 < s.size(); idx++) {
        if (s[idx] == '\\' && s[idx+1] == '\n') { // continuation character
            idx++;
        } else if (s[idx] == '\\' && s[idx+1] == 'n') {
            x += "\n";
            idx++;
        } else if (s[idx] == '\\' && s[idx+1] == 't') {
            x += "\t";
            idx++;
        } else if (s[idx] == '\\' && s[idx+1] == 'r') {
            x += "\r";
            idx++;
        } else if (s[idx] == '\\' && s[idx+1] == 'b') {
            x += "\b";
            idx++;
        } else if (s[idx] == '\\' && s[idx+1] == 'v') {
            x += "\v";
            idx++;
        } else if (s[idx] == '\\' && s[idx + 1] == 'f') {
            x += "\f";
            idx++;
        } else if (s[idx] == '\\' && s[idx+1] == '\\') {
            x += "\\";
            idx++;
        } else if (s[idx] == '\\' && s[idx+1] == '"') {
            x += '"';
            idx++;
        } else if (s[idx] == '\\' && s[idx+1] == '\'') {
            x += '\'';
            idx++;
        } else {
            x += s[idx];
        }
    }
    if (idx < s.size()) {
        x += s[idx];
    }
    return LCompilers::s2c(al, x);
}

std::string str_escape_fortran_double_quote(const std::string &s) {
    std::ostringstream o;
    for (auto c = s.cbegin(); c != s.cend(); c++) {
        switch (*c) {
            case '"': o << "\"\""; break;
        }
    }
    return o.str();
}

char* str_unescape_fortran(Allocator &al, LCompilers::Str &s, char ch) {
    std::string x = "";
    size_t idx = 0;
    for (; idx + 1 < s.size(); idx++) {
        if (s[idx] == ch && s[idx + 1] == ch) {
            x += s[idx];
            idx++;
        } else {
            x += s[idx];
        }
    }
    if (idx < s.size()) {
        x += s[idx];
    }
    return LCompilers::s2c(al, x);
}

bool str_compare(const unsigned char *pos, std::string s) {
    for (size_t i = 0; i < s.size(); i++) {
        if (pos[i] == '\0') {
            return false;
        }

        if (pos[i] != s[i]) {
            return false;
        }
    }
    return true;
}

// trim trailing whitespace from a string in-place
void rtrim(std::string& str) {
    str.erase(std::find_if_not(str.rbegin(), str.rend(), ::isspace).base(), str.end());
}

// UTF-8 helpers ------------------------------------------------------------

namespace {

// Length in bytes of the UTF-8 sequence a lead byte starts, or 0 if `c` is not
// a valid lead byte.
size_t utf8_sequence_length(unsigned char c) {
    if (c < 0x80) return 1;
    if ((c & 0xe0) == 0xc0) return 2;
    if ((c & 0xf0) == 0xe0) return 3;
    if ((c & 0xf8) == 0xf0) return 4;
    return 0;
}

// The code point bits a lead byte contributes.
uint32_t utf8_lead_bits(unsigned char c, size_t length) {
    switch (length) {
        case 1: return c;
        case 2: return c & 0x1f;
        case 3: return c & 0x0f;
        default: return c & 0x07;
    }
}

} // anonymous namespace

bool is_valid_utf8(const char *value, size_t size) {
    size_t i = 0;
    while (i < size) {
        const unsigned char c = static_cast<unsigned char>(value[i]);
        const size_t length = utf8_sequence_length(c);
        if (length == 0) return false;
        if (length == 1) {
            i++;
            continue;
        }
        if (i + length > size) return false;
        uint32_t code_point = utf8_lead_bits(c, length);
        for (size_t k = 1; k < length; k++) {
            const unsigned char cont =
                static_cast<unsigned char>(value[i + k]);
            if ((cont & 0xc0) != 0x80) return false;
            code_point = (code_point << 6) | (cont & 0x3f);
        }
        if (length == 2 && code_point < 0x80) return false;
        if (length == 3 && code_point < 0x800) return false;
        if (length == 4 && code_point < 0x10000) return false;
        if (code_point > 0x10ffff) return false;
        if (code_point >= 0xd800 && code_point <= 0xdfff) return false;
        i += length;
    }
    return true;
}

bool is_valid_utf8(const std::string &value) {
    return is_valid_utf8(value.data(), value.size());
}

std::vector<uint32_t> utf8_decode(const std::string &s) {
    std::vector<uint32_t> code_points;
    size_t i = 0;
    while (i < s.size()) {
        const unsigned char c = static_cast<unsigned char>(s[i]);
        size_t length = utf8_sequence_length(c);
        if (length == 0 || i + length > s.size()) {
            // Not well-formed: pass the byte through so callers that did not
            // validate still make progress instead of running off the end.
            code_points.push_back(c);
            i++;
            continue;
        }
        uint32_t code_point = utf8_lead_bits(c, length);
        for (size_t k = 1; k < length; k++) {
            code_point = (code_point << 6) |
                (static_cast<unsigned char>(s[i + k]) & 0x3f);
        }
        code_points.push_back(code_point);
        i += length;
    }
    return code_points;
}

size_t utf8_codepoint_count(const std::string &s) {
    size_t count = 0;
    size_t i = 0;
    while (i < s.size()) {
        const size_t length =
            utf8_sequence_length(static_cast<unsigned char>(s[i]));
        i += (length == 0 || i + length > s.size()) ? 1 : length;
        count++;
    }
    return count;
}

size_t utf8_codepoint_count(const char *s) {
    return utf8_codepoint_count(std::string(s));
}

void utf8_encode_codepoint(std::string &out, uint32_t code_point) {
    if (code_point <= 0x7f) {
        out += static_cast<char>(code_point);
    } else if (code_point <= 0x7ff) {
        out += static_cast<char>(0xc0 | (code_point >> 6));
        out += static_cast<char>(0x80 | (code_point & 0x3f));
    } else if (code_point <= 0xffff) {
        out += static_cast<char>(0xe0 | (code_point >> 12));
        out += static_cast<char>(0x80 | ((code_point >> 6) & 0x3f));
        out += static_cast<char>(0x80 | (code_point & 0x3f));
    } else {
        out += static_cast<char>(0xf0 | (code_point >> 18));
        out += static_cast<char>(0x80 | ((code_point >> 12) & 0x3f));
        out += static_cast<char>(0x80 | ((code_point >> 6) & 0x3f));
        out += static_cast<char>(0x80 | (code_point & 0x3f));
    }
}

std::vector<uint8_t> utf8_to_unicode_bytes(const std::string &s, int kind) {
    std::vector<uint32_t> code_points = utf8_decode(s);
    std::vector<uint8_t> bytes(code_points.size() * kind);
    for (size_t idx = 0; idx < code_points.size(); idx++) {
        const uint32_t cp = code_points[idx];
        for (int b = 0; b < kind; b++) {
            bytes[idx * kind + b] = (cp >> (8 * b)) & 0xff;
        }
    }
    return bytes;
}

std::vector<std::string> utf8_split(const std::string &s) {
    std::vector<std::string> characters;
    size_t i = 0;
    while (i < s.size()) {
        size_t length = utf8_sequence_length(static_cast<unsigned char>(s[i]));
        if (length == 0 || i + length > s.size()) length = 1;
        characters.push_back(s.substr(i, length));
        i += length;
    }
    return characters;
}

std::string unicode_bytes_to_utf8(const uint8_t *bytes, size_t count, int kind) {
    std::string out;
    for (size_t idx = 0; idx < count; idx++) {
        uint32_t cp = 0;
        for (int b = 0; b < kind; b++) {
            cp |= static_cast<uint32_t>(bytes[idx * kind + b]) << (8 * b);
        }
        utf8_encode_codepoint(out, cp);
    }
    return out;
}

} // namespace LCompilers

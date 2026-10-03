#include <cctype>
#include <cerrno>
#include <cmath>
#include <cstdlib>
#include <sstream>
#include <iomanip>
#include <locale>
#include <set>

#include <libasr/asr_text_parser.h>
#include <libasr/string_utils.h>

namespace LCompilers::ASRText {

namespace {

// Maximum nesting depth of lists/vectors/maps/tagged literals. Chosen well
// above any realistic ASR document while still bounding recursion depth (and
// therefore native stack usage) for pathological/adversarial input.
constexpr size_t max_nesting_depth = 512;

// Floor for the maximum number of `Value` nodes a document may allocate.
// The effective limit also scales with input size (see `Parser::Parser`),
// so legitimate large documents are never rejected, while inputs crafted to
// allocate a huge number of nodes from a small amount of text are.
constexpr size_t min_max_nodes = 1u << 16; // 65536

bool is_ascii_space(unsigned char c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v';
}

// Characters that always terminate a bare token (symbol/keyword/tag name or
// number), whether or not they are otherwise meaningful. `#`, `\`, `'`, `^`,
// backtick and `~` are reserved reader characters we do not support as part
// of bare tokens (tags are read separately, starting at the `#`).
bool is_reserved_delimiter(unsigned char c) {
    switch (c) {
        case '(': case ')': case '[': case ']': case '{': case '}':
        case '"': case ';': case '#': case '\\': case '\'': case '^':
        case '`': case '~':
            return true;
        default:
            return false;
    }
}

bool is_token_terminator(unsigned char c) {
    return is_ascii_space(c) || c == ',' || is_reserved_delimiter(c);
}

bool is_ascii_digit(unsigned char c) {
    return c >= '0' && c <= '9';
}

bool hex_digit_value(char c, int &value) {
    if (c >= '0' && c <= '9') { value = c - '0'; return true; }
    if (c >= 'a' && c <= 'f') { value = c - 'a' + 10; return true; }
    if (c >= 'A' && c <= 'F') { value = c - 'A' + 10; return true; }
    return false;
}

// Canonical string used to detect duplicate scalar map keys. Two scalar
// values are considered equal keys iff they have the same kind and the same
// canonical representation.
std::string canonical_scalar_key(const Value &v) {
    std::ostringstream out;
    out.imbue(std::locale::classic());
    switch (v.kind) {
        case ValueKind::Nil:
            out << "n";
            break;
        case ValueKind::Bool:
            out << "b:" << (v.bool_value ? "1" : "0");
            break;
        case ValueKind::Integer:
            out << "i:" << v.int_value;
            break;
        case ValueKind::Float:
            // 17 significant decimal digits round-trip any double exactly,
            // so two floats compare equal here iff they compare equal as
            // `double`s.
            out << "f:" << std::setprecision(17) << v.float_value;
            break;
        case ValueKind::String:
            out << "s:" << v.text;
            break;
        case ValueKind::Symbol:
            out << "y:" << v.text;
            break;
        case ValueKind::Keyword:
            out << "k:" << v.text;
            break;
        default:
            // Not a scalar kind; callers must not invoke this for composite
            // (List/Vector/Map/Tagged) keys.
            break;
    }
    return out.str();
}

bool is_scalar_kind(ValueKind kind) {
    switch (kind) {
        case ValueKind::Nil:
        case ValueKind::Bool:
        case ValueKind::Integer:
        case ValueKind::Float:
        case ValueKind::String:
        case ValueKind::Symbol:
        case ValueKind::Keyword:
            return true;
        default:
            return false;
    }
}

// Recursive-descent reader for the EDN subset. Produces a `Document` owning
// every `Value` node, or fails by populating `diagnostics` with exactly one
// `ASRParser` error diagnostic (the first one encountered).
class Parser {
public:
    Parser(const std::string &src, diag::Diagnostics &diagnostics)
        : src{src}, n{src.size()}, diagnostics{diagnostics},
          max_nodes{std::max(min_max_nodes, 2 * src.size() + 64)}
    { }

    Result<std::unique_ptr<Document>> parse_document() {
        doc = std::make_unique<Document>();
        Value *root = nullptr;
        if (!parse_value(root)) {
            return Error();
        }
        skip_ws_and_comments();
        if (pos != n) {
            fail("unexpected trailing input after the top-level value",
                point_loc(pos), "trailing data starts here");
            return Error();
        }
        doc->root = root;
        return std::move(doc);
    }

private:
    const std::string &src;
    size_t n;
    diag::Diagnostics &diagnostics;
    size_t max_nodes;
    size_t pos = 0;
    size_t depth = 0;
    size_t node_count = 0;
    bool failed = false;
    std::unique_ptr<Document> doc;

    // A single-byte location at `p` (clamped so it never points past the
    // last valid byte, which keeps diagnostic rendering well defined even
    // when `p == n`, i.e. end of input).
    Location point_loc(size_t p) const {
        Location loc;
        if (n == 0) {
            loc.first = 0;
            loc.last = 0;
        } else {
            size_t clamped = (p < n) ? p : n - 1;
            loc.first = static_cast<uint32_t>(clamped);
            loc.last = static_cast<uint32_t>(clamped);
        }
        return loc;
    }

    Location span_loc(size_t first, size_t last) const {
        Location loc;
        loc.first = static_cast<uint32_t>(first);
        loc.last = static_cast<uint32_t>(last);
        return loc;
    }

    bool fail(const std::string &message, const Location &loc,
            const std::string &label) {
        if (!failed) {
            failed = true;
            diagnostics.asr_parser_error_label(message, {loc}, label);
        }
        return false;
    }

    bool check_depth(const Location &loc) {
        if (depth > max_nesting_depth) {
            return fail("exceeded the maximum allowed nesting depth ("
                    + std::to_string(max_nesting_depth) + ")", loc,
                    "too deeply nested here");
        }
        return true;
    }

    Value *new_value(ValueKind kind, const Location &loc, bool &ok) {
        if (node_count >= max_nodes) {
            ok = fail("document is too large or complex to parse "
                    "(exceeded the maximum number of values)", loc,
                    "while parsing this value");
            return nullptr;
        }
        node_count++;
        ok = true;
        return doc->new_value(kind, loc);
    }

    void skip_ws_and_comments() {
        while (pos < n) {
            unsigned char c = static_cast<unsigned char>(src[pos]);
            if (is_ascii_space(c) || c == ',') {
                pos++;
            } else if (c == ';') {
                while (pos < n && src[pos] != '\n') {
                    pos++;
                }
            } else {
                break;
            }
        }
    }

    // Reads a bare token (symbol/keyword name, tag name, or number) starting
    // at `pos`, stopping before whitespace/comma/any reserved delimiter or
    // end of input. Always advances `pos` by at least one byte.
    std::string read_token() {
        size_t start = pos;
        while (pos < n && !is_token_terminator(static_cast<unsigned char>(src[pos]))) {
            pos++;
        }
        return src.substr(start, pos - start);
    }

    bool looks_numeric(const std::string &tok) const {
        if (tok.empty()) return false;
        size_t i = 0;
        if (tok[0] == '+' || tok[0] == '-') i = 1;
        return i < tok.size() && is_ascii_digit(static_cast<unsigned char>(tok[i]));
    }

    bool matches_integer(const std::string &tok) const {
        size_t i = 0;
        if (tok[i] == '+' || tok[i] == '-') i++;
        if (i >= tok.size()) return false;
        for (; i < tok.size(); i++) {
            if (!is_ascii_digit(static_cast<unsigned char>(tok[i]))) return false;
        }
        return true;
    }

    // `int frac? exp?` with at least one of `frac`/`exp` present, matching
    // EDN's floating-point literal grammar.
    bool matches_float(const std::string &tok) const {
        size_t i = 0;
        if (tok[i] == '+' || tok[i] == '-') i++;
        size_t int_start = i;
        while (i < tok.size() && is_ascii_digit(static_cast<unsigned char>(tok[i]))) i++;
        if (i == int_start) return false;
        bool has_frac = false, has_exp = false;
        if (i < tok.size() && tok[i] == '.') {
            size_t frac_start = i + 1;
            size_t j = frac_start;
            while (j < tok.size() && is_ascii_digit(static_cast<unsigned char>(tok[j]))) j++;
            if (j == frac_start) return false;
            has_frac = true;
            i = j;
        }
        if (i < tok.size() && (tok[i] == 'e' || tok[i] == 'E')) {
            size_t j = i + 1;
            if (j < tok.size() && (tok[j] == '+' || tok[j] == '-')) j++;
            size_t exp_digits_start = j;
            while (j < tok.size() && is_ascii_digit(static_cast<unsigned char>(tok[j]))) j++;
            if (j == exp_digits_start) return false;
            has_exp = true;
            i = j;
        }
        return (has_frac || has_exp) && i == tok.size();
    }

    bool parse_atom(Value *&out) {
        size_t start = pos;
        std::string tok = read_token();
        Location loc = span_loc(start, pos - 1);
        if (tok == "nil") {
            bool ok;
            Value *v = new_value(ValueKind::Nil, loc, ok);
            if (!ok) return false;
            v->text = tok;
            out = v;
            return true;
        }
        if (tok == "true" || tok == "false") {
            bool ok;
            Value *v = new_value(ValueKind::Bool, loc, ok);
            if (!ok) return false;
            v->text = tok;
            v->bool_value = (tok == "true");
            out = v;
            return true;
        }
        if (looks_numeric(tok)) {
            if (matches_integer(tok)) {
                errno = 0;
                char *endptr = nullptr;
                long long val = std::strtoll(tok.c_str(), &endptr, 10);
                if (errno == ERANGE || endptr != tok.c_str() + tok.size()) {
                    return fail("integer literal `" + tok + "` does not fit "
                            "in a 64-bit signed integer", loc,
                            "integer out of range here");
                }
                bool ok;
                Value *v = new_value(ValueKind::Integer, loc, ok);
                if (!ok) return false;
                v->text = tok;
                v->int_value = static_cast<int64_t>(val);
                out = v;
                return true;
            }
            if (matches_float(tok)) {
                std::istringstream value_stream(tok);
                value_stream.imbue(std::locale::classic());
                double val;
                value_stream >> val;
                if (!value_stream ||
                        value_stream.peek() != std::char_traits<char>::eof() ||
                        !std::isfinite(val)) {
                    return fail("float literal `" + tok + "` is out of range "
                            "or not a finite number", loc,
                            "invalid float here");
                }
                bool ok;
                Value *v = new_value(ValueKind::Float, loc, ok);
                if (!ok) return false;
                v->text = tok;
                v->float_value = val;
                out = v;
                return true;
            }
            return fail("malformed numeric literal `" + tok + "`", loc,
                    "invalid number here");
        }
        bool ok;
        Value *v = new_value(ValueKind::Symbol, loc, ok);
        if (!ok) return false;
        v->text = tok;
        out = v;
        return true;
    }

    bool parse_keyword(Value *&out) {
        size_t start = pos;
        pos++; // consume ':'
        if (pos >= n || is_token_terminator(static_cast<unsigned char>(src[pos]))) {
            return fail("keyword is missing a name after `:`",
                    span_loc(start, pos - 1), "empty keyword here");
        }
        std::string tok = read_token();
        Location loc = span_loc(start, pos - 1);
        bool ok;
        Value *v = new_value(ValueKind::Keyword, loc, ok);
        if (!ok) return false;
        v->text = tok;
        out = v;
        return true;
    }

    bool parse_tag(Value *&out) {
        size_t start = pos;
        pos++; // consume '#'
        if (pos >= n || is_token_terminator(static_cast<unsigned char>(src[pos]))) {
            return fail("tag is missing a name after `#`",
                    span_loc(start, pos - 1), "empty tag here");
        }
        std::string tag_name = read_token();
        skip_ws_and_comments();
        if (pos >= n || src[pos] == ')' || src[pos] == ']' || src[pos] == '}') {
            return fail("tag `#" + tag_name + "` is missing its value",
                    span_loc(start, pos > start ? pos - 1 : start),
                    "expected a value after this tag");
        }
        depth++;
        if (!check_depth(span_loc(start, pos))) return false;
        Value *child = nullptr;
        bool child_ok = parse_value(child);
        depth--;
        if (!child_ok) return false;
        Location loc = span_loc(start, child->loc.last);
        bool ok;
        Value *v = new_value(ValueKind::Tagged, loc, ok);
        if (!ok) return false;
        v->tag = tag_name;
        v->tagged_value = child;
        out = v;
        return true;
    }

    bool parse_string(Value *&out) {
        size_t start = pos;
        pos++; // consume opening quote
        std::string buffer;
        while (true) {
            if (pos >= n) {
                return fail("unterminated string literal",
                        span_loc(start, start), "string opened here");
            }
            unsigned char c = static_cast<unsigned char>(src[pos]);
            if (c == '"') {
                pos++;
                break;
            }
            if (c == '\\') {
                size_t esc_start = pos;
                pos++;
                if (pos >= n) {
                    return fail("unterminated escape sequence in string literal",
                            span_loc(esc_start, esc_start), "escape starts here");
                }
                char e = src[pos];
                switch (e) {
                    case '\\': buffer += '\\'; pos++; break;
                    case '"':  buffer += '"';  pos++; break;
                    case 'n':  buffer += '\n'; pos++; break;
                    case 'r':  buffer += '\r'; pos++; break;
                    case 't':  buffer += '\t'; pos++; break;
                    case 'u': {
                        pos++; // consume 'u'
                        if (pos + 4 > n) {
                            return fail("malformed `\\u` unicode escape: "
                                    "expected 4 hexadecimal digits",
                                    span_loc(esc_start, n - 1),
                                    "incomplete unicode escape here");
                        }
                        uint32_t cp = 0;
                        for (int k = 0; k < 4; k++) {
                            int hv;
                            if (!hex_digit_value(src[pos + k], hv)) {
                                return fail("malformed `\\u` unicode escape: "
                                        "expected 4 hexadecimal digits",
                                        span_loc(esc_start, pos + k),
                                        "invalid hex digit here");
                            }
                            cp = (cp << 4) | static_cast<uint32_t>(hv);
                        }
                        pos += 4;
                        if (cp >= 0xD800 && cp <= 0xDFFF) {
                            return fail("unicode escape encodes a surrogate "
                                    "code point",
                                    span_loc(esc_start, pos - 1),
                                    "surrogate escapes are not valid UTF-8");
                        }
                        utf8_encode_codepoint(buffer, cp);
                        break;
                    }
                    default:
                        return fail(std::string("invalid escape sequence `\\") +
                                e + "` in string literal",
                                span_loc(esc_start, pos), "invalid escape here");
                }
            } else {
                buffer += static_cast<char>(c);
                pos++;
            }
        }
        Location loc = span_loc(start, pos - 1);
        if (!is_valid_utf8(buffer)) {
            return fail("string literal is not well-formed UTF-8",
                    loc, "invalid UTF-8 in this string");
        }
        bool ok;
        Value *v = new_value(ValueKind::String, loc, ok);
        if (!ok) return false;
        v->text = buffer;
        out = v;
        return true;
    }

    // Parses a `(` ... `)` list or `[` ... `]` vector.
    bool parse_seq(Value *&out, ValueKind kind, char open, char close) {
        size_t start = pos;
        pos++; // consume open delimiter
        depth++;
        if (!check_depth(span_loc(start, start))) return false;
        std::vector<Value *> elements;
        while (true) {
            skip_ws_and_comments();
            if (pos >= n) {
                depth--;
                return fail(std::string("unclosed `") + open +
                        "`: reached end of input before the matching `" +
                        close + "`", span_loc(start, start),
                        "unclosed delimiter here");
            }
            if (src[pos] == close) {
                pos++;
                break;
            }
            Value *child = nullptr;
            if (!parse_value(child)) {
                depth--;
                return false;
            }
            elements.push_back(child);
        }
        depth--;
        Location loc = span_loc(start, pos - 1);
        bool ok;
        Value *v = new_value(kind, loc, ok);
        if (!ok) return false;
        v->elements = std::move(elements);
        out = v;
        return true;
    }

    bool parse_map(Value *&out) {
        size_t start = pos;
        pos++; // consume '{'
        depth++;
        if (!check_depth(span_loc(start, start))) return false;
        std::vector<Value *> forms;
        while (true) {
            skip_ws_and_comments();
            if (pos >= n) {
                depth--;
                return fail("unclosed `{`: reached end of input before the "
                        "matching `}`", span_loc(start, start),
                        "unclosed delimiter here");
            }
            if (src[pos] == '}') {
                pos++;
                break;
            }
            Value *child = nullptr;
            if (!parse_value(child)) {
                depth--;
                return false;
            }
            forms.push_back(child);
        }
        depth--;
        Location loc = span_loc(start, pos - 1);
        if (forms.size() % 2 != 0) {
            return fail("map literal has an odd number of forms; expected "
                    "key/value pairs", loc, "unpaired key in this map");
        }
        std::vector<MapEntry> entries;
        entries.reserve(forms.size() / 2);
        std::set<std::string> seen_scalar_keys;
        for (size_t i = 0; i < forms.size(); i += 2) {
            Value *key = forms[i];
            Value *value = forms[i + 1];
            if (is_scalar_kind(key->kind)) {
                std::string canon = canonical_scalar_key(*key);
                if (!seen_scalar_keys.insert(canon).second) {
                    return fail("duplicate map key", key->loc,
                            "this key already appears earlier in the map");
                }
            }
            entries.push_back(MapEntry{key, value});
        }
        bool ok;
        Value *v = new_value(ValueKind::Map, loc, ok);
        if (!ok) return false;
        v->entries = std::move(entries);
        out = v;
        return true;
    }

    bool parse_value(Value *&out) {
        skip_ws_and_comments();
        if (pos >= n) {
            return fail("expected a value but reached end of input",
                    point_loc(pos), "expected a value here");
        }
        unsigned char c = static_cast<unsigned char>(src[pos]);
        switch (c) {
            case '(': return parse_seq(out, ValueKind::List, '(', ')');
            case '[': return parse_seq(out, ValueKind::Vector, '[', ']');
            case '{': return parse_map(out);
            case '"': return parse_string(out);
            case '#': return parse_tag(out);
            case ':': return parse_keyword(out);
            case ')': case ']': case '}':
                return fail(std::string("unexpected closing delimiter `") +
                        static_cast<char>(c) + "`", point_loc(pos),
                        "no matching opening delimiter");
            case '\\': case '\'': case '^': case '`': case '~':
                return fail(std::string("unsupported syntax `") +
                        static_cast<char>(c) + "` (quote/metadata/reader "
                        "macros are not supported)", point_loc(pos),
                        "unsupported character here");
            default:
                return parse_atom(out);
        }
    }
};

} // namespace

Result<std::unique_ptr<Document>> parse(const std::string &text,
        diag::Diagnostics &diagnostics) {
    Parser parser(text, diagnostics);
    return parser.parse_document();
}

Result<std::unique_ptr<Document>> parse(const std::string &text,
        const std::string &filename, LocationManager &lm,
        diag::Diagnostics &diagnostics) {
    {
        LocationManager::FileLocations fl;
        fl.in_filename = filename;
        lm.files.push_back(fl);
        lm.init_simple(text);
        lm.file_ends.push_back(static_cast<uint32_t>(text.size()));
    }
    return parse(text, diagnostics);
}

} // namespace LCompilers::ASRText

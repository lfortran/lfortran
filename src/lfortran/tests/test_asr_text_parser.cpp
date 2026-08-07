#include <tests/doctest.h>

#include <cstdint>
#include <limits>
#include <memory>
#include <string>

#include <libasr/asr_text_parser.h>
#include <libasr/diagnostics.h>
#include <libasr/utils.h>

using LCompilers::Location;
using LCompilers::LocationManager;
using LCompilers::Result;
using LCompilers::diag::Diagnostics;
using LCompilers::diag::Level;
using LCompilers::diag::Stage;
using LCompilers::ASRText::Document;
using LCompilers::ASRText::MapEntry;
using LCompilers::ASRText::Value;
using LCompilers::ASRText::ValueKind;

namespace {

// Parses `text` and asserts that it succeeds with no reported errors,
// returning the owning `Document` so the test can inspect its tree.
std::unique_ptr<Document> parse_ok(const std::string &text) {
    Diagnostics diagnostics;
    Result<std::unique_ptr<Document>> res = LCompilers::ASRText::parse(text, diagnostics);
    INFO("input: ", text);
    REQUIRE(res.ok);
    CHECK_FALSE(diagnostics.has_error());
    REQUIRE(res.result->root != nullptr);
    return std::move(res.result);
}

// Parses `text` and asserts that it fails with at least one ASRParser error.
void parse_fails(const std::string &text) {
    Diagnostics diagnostics;
    Result<std::unique_ptr<Document>> res = LCompilers::ASRText::parse(text, diagnostics);
    INFO("input: ", text);
    CHECK_FALSE(res.ok);
    REQUIRE(diagnostics.has_error());
    bool found_asr_parser_stage = false;
    bool found_primary_label = false;
    for (auto &d : diagnostics.diagnostics) {
        if (d.stage == Stage::ASRParser && d.level == Level::Error) {
            found_asr_parser_stage = true;
            for (auto &l : d.labels) {
                if (l.primary) found_primary_label = true;
            }
        }
    }
    CHECK(found_asr_parser_stage);
    CHECK(found_primary_label);
}

}

TEST_CASE("ASRText parser: nil/true/false") {
    auto d = parse_ok("nil");
    CHECK(d->root->kind == ValueKind::Nil);

    d = parse_ok("true");
    CHECK(d->root->kind == ValueKind::Bool);
    CHECK(d->root->bool_value == true);

    d = parse_ok("false");
    CHECK(d->root->kind == ValueKind::Bool);
    CHECK(d->root->bool_value == false);
}

TEST_CASE("ASRText parser: integers") {
    auto d = parse_ok("42");
    CHECK(d->root->kind == ValueKind::Integer);
    CHECK(d->root->int_value == 42);

    d = parse_ok("-17");
    CHECK(d->root->kind == ValueKind::Integer);
    CHECK(d->root->int_value == -17);

    d = parse_ok("0");
    CHECK(d->root->kind == ValueKind::Integer);
    CHECK(d->root->int_value == 0);

    d = parse_ok("+5");
    CHECK(d->root->kind == ValueKind::Integer);
    CHECK(d->root->int_value == 5);
}

TEST_CASE("ASRText parser: integer boundaries") {
    auto d = parse_ok("9223372036854775807"); // INT64_MAX
    CHECK(d->root->kind == ValueKind::Integer);
    CHECK(d->root->int_value == std::numeric_limits<int64_t>::max());

    d = parse_ok("-9223372036854775808"); // INT64_MIN
    CHECK(d->root->kind == ValueKind::Integer);
    CHECK(d->root->int_value == std::numeric_limits<int64_t>::min());

    // One past INT64_MAX must be rejected as overflow.
    parse_fails("9223372036854775808");
    // One past INT64_MIN (in magnitude) must be rejected as overflow.
    parse_fails("-9223372036854775809");
}

TEST_CASE("ASRText parser: floats") {
    auto d = parse_ok("3.14");
    CHECK(d->root->kind == ValueKind::Float);
    CHECK(d->root->float_value == doctest::Approx(3.14));

    d = parse_ok("1.0e10");
    CHECK(d->root->kind == ValueKind::Float);
    CHECK(d->root->float_value == doctest::Approx(1.0e10));

    d = parse_ok("-2.5e-3");
    CHECK(d->root->kind == ValueKind::Float);
    CHECK(d->root->float_value == doctest::Approx(-2.5e-3));

    // Exponent without a fractional part is still a valid EDN float.
    d = parse_ok("2e3");
    CHECK(d->root->kind == ValueKind::Float);
    CHECK(d->root->float_value == doctest::Approx(2e3));
}

TEST_CASE("ASRText parser: malformed numbers are rejected") {
    parse_fails("1.");       // missing fractional digits
    parse_fails("1e");       // missing exponent digits
    parse_fails("1.2.3");    // two decimal points
    parse_fails("1e+");      // missing exponent digits after sign
    parse_fails("12abc");    // digits followed by invalid trailing characters
}

TEST_CASE("ASRText parser: float overflow is rejected") {
    parse_fails("1e400"); // overflows a double to +inf
}

TEST_CASE("ASRText parser: bare sign is a symbol, not a number") {
    auto d = parse_ok("-");
    CHECK(d->root->kind == ValueKind::Symbol);
    CHECK(d->root->text == "-");

    d = parse_ok("+");
    CHECK(d->root->kind == ValueKind::Symbol);
    CHECK(d->root->text == "+");
}

TEST_CASE("ASRText parser: symbols and keywords") {
    auto d = parse_ok("foo");
    CHECK(d->root->kind == ValueKind::Symbol);
    CHECK(d->root->text == "foo");

    d = parse_ok("my.ns/Foo");
    CHECK(d->root->kind == ValueKind::Symbol);
    CHECK(d->root->text == "my.ns/Foo");

    d = parse_ok(":kw");
    CHECK(d->root->kind == ValueKind::Keyword);
    CHECK(d->root->text == "kw");

    d = parse_ok(":my.ns/kw");
    CHECK(d->root->kind == ValueKind::Keyword);
    CHECK(d->root->text == "my.ns/kw");
}

TEST_CASE("ASRText parser: empty keyword is rejected") {
    parse_fails(":");
    parse_fails(": ");
}

TEST_CASE("ASRText parser: strings, plain and with escapes") {
    auto d = parse_ok("\"hello\"");
    CHECK(d->root->kind == ValueKind::String);
    CHECK(d->root->text == "hello");

    d = parse_ok(R"("a\nb\tc\r\\d\"e\bf\fg")");
    CHECK(d->root->kind == ValueKind::String);
    CHECK(d->root->text == std::string("a\nb\tc\r\\d\"e\bf\fg"));
}

TEST_CASE("ASRText parser: string preserves UTF-8 bytes as-is") {
    // Embedded raw (already UTF-8 encoded) multi-byte characters must be
    // preserved byte-for-byte.
    std::string input = "\"caf\xC3\xA9\""; // "café" with é as raw UTF-8
    auto d = parse_ok(input);
    CHECK(d->root->kind == ValueKind::String);
    CHECK(d->root->text == "caf\xC3\xA9");
}

TEST_CASE("ASRText parser: unicode escapes, including embedded NUL") {
    auto d = parse_ok(R"("\u0041")"); // \u0041 == 'A'
    CHECK(d->root->text == "A");

    // U+00E9 (é) must be encoded as the 2-byte UTF-8 sequence 0xC3 0xA9.
    d = parse_ok(R"("\u00e9")");
    REQUIRE(d->root->text.size() == 2);
    CHECK(static_cast<unsigned char>(d->root->text[0]) == 0xC3);
    CHECK(static_cast<unsigned char>(d->root->text[1]) == 0xA9);

    // \u0000 must produce a real embedded NUL byte, and the string must
    // retain everything that follows it.
    d = parse_ok(R"("a\u0000b")");
    REQUIRE(d->root->text.size() == 3);
    CHECK(d->root->text[0] == 'a');
    CHECK(d->root->text[1] == '\0');
    CHECK(d->root->text[2] == 'b');
}

TEST_CASE("ASRText parser: invalid escapes are rejected") {
    parse_fails("\"\\v\"");     // \v is a C escape, not an EDN escape
    parse_fails("\"\\x41\"");   // \x is not an EDN escape
    parse_fails("\"\\q\"");     // unknown escape
    parse_fails(R"("\u12")");   // truncated unicode escape
    parse_fails(R"("\u12zz")"); // invalid hex digits
    parse_fails("\"abc");       // unterminated string
    parse_fails("\"abc\\");     // unterminated escape at end of input
}

TEST_CASE("ASRText parser: lists") {
    auto d = parse_ok("(1 2 3)");
    REQUIRE(d->root->kind == ValueKind::List);
    REQUIRE(d->root->elements.size() == 3);
    CHECK(d->root->elements[0]->int_value == 1);
    CHECK(d->root->elements[1]->int_value == 2);
    CHECK(d->root->elements[2]->int_value == 3);

    d = parse_ok("()");
    REQUIRE(d->root->kind == ValueKind::List);
    CHECK(d->root->elements.empty());
}

TEST_CASE("ASRText parser: vectors") {
    auto d = parse_ok("[1 2 3]");
    REQUIRE(d->root->kind == ValueKind::Vector);
    REQUIRE(d->root->elements.size() == 3);
    CHECK(d->root->elements[1]->int_value == 2);

    d = parse_ok("[]");
    REQUIRE(d->root->kind == ValueKind::Vector);
    CHECK(d->root->elements.empty());
}

TEST_CASE("ASRText parser: maps preserve entry order") {
    auto d = parse_ok("{:a 1 :b 2 :c 3}");
    REQUIRE(d->root->kind == ValueKind::Map);
    REQUIRE(d->root->entries.size() == 3);
    CHECK(d->root->entries[0].key->text == "a");
    CHECK(d->root->entries[0].value->int_value == 1);
    CHECK(d->root->entries[1].key->text == "b");
    CHECK(d->root->entries[1].value->int_value == 2);
    CHECK(d->root->entries[2].key->text == "c");
    CHECK(d->root->entries[2].value->int_value == 3);

    d = parse_ok("{}");
    REQUIRE(d->root->kind == ValueKind::Map);
    CHECK(d->root->entries.empty());
}

TEST_CASE("ASRText parser: map with odd number of forms is rejected") {
    parse_fails("{:a 1 :b}");
}

TEST_CASE("ASRText parser: duplicate scalar map keys are rejected") {
    parse_fails("{:a 1 :a 2}");
    parse_fails("{1 :x 1 :y}");
    parse_fails(R"({"k" 1 "k" 2})");
    parse_fails("{nil 1 nil 2}");
}

TEST_CASE("ASRText parser: distinct-kind keys that look alike are not duplicates") {
    // 1 (Integer) and 1.0 (Float) are different kinds, so this is not a
    // duplicate key even though they compare numerically equal.
    auto d = parse_ok("{1 :int 1.0 :float}");
    REQUIRE(d->root->entries.size() == 2);

    // :a (Keyword) and "a" (String) and a (Symbol) are all different kinds.
    d = parse_ok(R"({:a 1 "a" 2 a 3})");
    REQUIRE(d->root->entries.size() == 3);
}

TEST_CASE("ASRText parser: comments and commas are whitespace") {
    auto d = parse_ok("; leading comment\n(1, 2 ,3) ; trailing comment");
    REQUIRE(d->root->kind == ValueKind::List);
    REQUIRE(d->root->elements.size() == 3);
    CHECK(d->root->elements[0]->int_value == 1);
    CHECK(d->root->elements[2]->int_value == 3);
}

TEST_CASE("ASRText parser: tagged literals") {
    auto d = parse_ok("#Foo 1");
    REQUIRE(d->root->kind == ValueKind::Tagged);
    CHECK(d->root->tag == "Foo");
    REQUIRE(d->root->tagged_value != nullptr);
    CHECK(d->root->tagged_value->int_value == 1);

    // Namespaced tag.
    d = parse_ok("#my.ns/Foo (1 2)");
    REQUIRE(d->root->kind == ValueKind::Tagged);
    CHECK(d->root->tag == "my.ns/Foo");
    REQUIRE(d->root->tagged_value != nullptr);
    CHECK(d->root->tagged_value->kind == ValueKind::List);

    // Unknown tag names are accepted at this generic layer; validating
    // known/expected tags is the schema layer's responsibility.
    d = parse_ok("#totally/unknown-tag 42");
    CHECK(d->root->tag == "totally/unknown-tag");

    // No whitespace is required between the tag and a value that starts
    // with a delimiter.
    d = parse_ok("#Foo(1 2)");
    CHECK(d->root->tag == "Foo");
    CHECK(d->root->tagged_value->kind == ValueKind::List);

    // Nested tags.
    d = parse_ok("#A #B 1");
    CHECK(d->root->tag == "A");
    CHECK(d->root->tagged_value->tag == "B");
    CHECK(d->root->tagged_value->tagged_value->int_value == 1);
}

TEST_CASE("ASRText parser: tag without a name or value is rejected") {
    parse_fails("#");
    parse_fails("# 1");
    parse_fails("#Foo");
    parse_fails("#Foo)");
}

TEST_CASE("ASRText parser: unsupported reader syntax is rejected") {
    parse_fails("'foo");     // quote
    parse_fails("^{:a 1} x"); // metadata
    parse_fails("`foo");     // syntax-quote / backtick
    parse_fails("~foo");     // unquote
}

TEST_CASE("ASRText parser: mismatched and unclosed delimiters") {
    parse_fails("(1 2");     // unclosed list
    parse_fails("[1 2");     // unclosed vector
    parse_fails("{:a 1");    // unclosed map
    parse_fails("(1 2]");    // mismatched closing delimiter
    parse_fails(")");        // stray closing delimiter
    parse_fails("(]");       // mismatched inside a list
}

TEST_CASE("ASRText parser: trailing top-level values are rejected") {
    parse_fails("1 2");
    parse_fails("(1) (2)");
    parse_fails("nil nil");
}

TEST_CASE("ASRText parser: empty input is rejected") {
    parse_fails("");
    parse_fails("   ; only a comment\n");
}

TEST_CASE("ASRText parser: excessive nesting is rejected") {
    std::string deep_open(2000, '(');
    std::string deep_close(2000, ')');
    parse_fails(deep_open + "1" + deep_close);
}

TEST_CASE("ASRText parser: nested lists/vectors/maps mixed together") {
    auto d = parse_ok("(#T {:a [1 2 (3 4)] :b nil})");
    REQUIRE(d->root->kind == ValueKind::List);
    REQUIRE(d->root->elements.size() == 1);
    Value *tagged = d->root->elements[0];
    CHECK(tagged->kind == ValueKind::Tagged);
    Value *map = tagged->tagged_value;
    REQUIRE(map->kind == ValueKind::Map);
    REQUIRE(map->entries.size() == 2);
    Value *vec = map->entries[0].value;
    REQUIRE(vec->kind == ValueKind::Vector);
    REQUIRE(vec->elements.size() == 3);
    CHECK(vec->elements[2]->kind == ValueKind::List);
}

TEST_CASE("ASRText parser: exact byte spans") {
    // "  (12 foo)" -- the list starts at byte 2 and ends at byte 9 (the
    // closing ')'); "12" spans bytes 3-4; "foo" spans bytes 6-8.
    std::string input = "  (12 foo)";
    auto d = parse_ok(input);
    REQUIRE(d->root->kind == ValueKind::List);
    CHECK(d->root->loc.first == 2);
    CHECK(d->root->loc.last == 9);
    REQUIRE(d->root->elements.size() == 2);
    CHECK(d->root->elements[0]->loc.first == 3);
    CHECK(d->root->elements[0]->loc.last == 4);
    CHECK(d->root->elements[1]->loc.first == 6);
    CHECK(d->root->elements[1]->loc.last == 8);
}

TEST_CASE("ASRText parser: exact diagnostic rendering for a syntax error") {
    Diagnostics diagnostics;
    std::string input = "(1 2";
    Result<std::unique_ptr<Document>> res = LCompilers::ASRText::parse(input, diagnostics);
    REQUIRE_FALSE(res.ok);
    REQUIRE(diagnostics.diagnostics.size() >= 1);
    auto &d = diagnostics.diagnostics[0];
    CHECK(d.level == Level::Error);
    CHECK(d.stage == Stage::ASRParser);
    auto rendered = LCompilers::diag::diag_level_to_str(d, false);
    std::string message_type = std::get<0>(rendered);
    CHECK(message_type == "ASR syntax error");
}

TEST_CASE("ASRText parser: parse(text, filename, lm, diagnostics) renders against that file") {
    LocationManager lm;
    Diagnostics diagnostics;
    std::string input = "(1 2";
    Result<std::unique_ptr<Document>> res =
        LCompilers::ASRText::parse(input, "asr_text_input", lm, diagnostics);
    REQUIRE_FALSE(res.ok);
    LCompilers::CompilerOptions co;
    std::string rendered = diagnostics.render(lm, co);
    CHECK(rendered.find("asr_text_input") != std::string::npos);
    CHECK(rendered.find("ASR syntax error") != std::string::npos);
}

#ifndef LFORTRAN_ASR_TEXT_PARSER_H
#define LFORTRAN_ASR_TEXT_PARSER_H

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include <libasr/diagnostics.h>
#include <libasr/exception.h>
#include <libasr/location.h>

// ASRText is a generic, non-evaluating reader for the EDN subset used by the
// textual ASR codec. It parses the source text into an owned tree of `Value`
// nodes (a `Document`) that a generated schema decoder can later walk to
// build actual `ASR::asr_t` nodes. This layer knows nothing about ASR: it
// only understands the generic data syntax (lists, vectors, maps, scalars,
// and tagged literals). Tag validation (which tags are known/expected, and
// what shape their payload must have) is entirely the responsibility of the
// schema layer built on top of this reader.
namespace LCompilers::ASRText {

// The kind of a parsed `Value` node.
enum class ValueKind {
    List, Vector, Map, String, Symbol, Keyword, Integer, Float, Bool, Nil,
    Tagged
};

struct Value;

// A single ordered key/value pair inside a `{...}` map literal. Both the
// key and the value are themselves generic `Value` nodes (owned by the
// enclosing `Document`), so maps can be keyed by any value, not just
// strings/keywords.
struct MapEntry {
    Value *key;
    Value *value;
};

// A single node of the generic value tree. Only the fields relevant to
// `kind` are meaningful; the rest are left default-initialized. All
// pointers refer to other nodes owned by the same `Document`, so a schema
// decoder can walk the tree without copying anything.
struct Value {
    ValueKind kind;

    // Exact byte span (in the original input text) this value was parsed
    // from, following the same [first, last] inclusive convention used
    // elsewhere in the compiler (see e.g. `Tokenizer`).
    Location loc;

    // List (`(...)`) / Vector (`[...]`) elements, in source order.
    std::vector<Value *> elements;

    // Map (`{...}`) entries, in source order (insertion order is
    // preserved; this is not a hash map).
    std::vector<MapEntry> entries;

    // Decoded text payload:
    //  - String:  the decoded bytes (escapes already resolved), may
    //             contain embedded NUL and arbitrary UTF-8 bytes.
    //  - Symbol:  the raw symbol text (e.g. "foo/bar").
    //  - Keyword: the text after the leading ':' (e.g. "foo/bar" for
    //             ":foo/bar").
    std::string text;

    // Tagged literal (`#tag value`): `tag` is the tag text without the
    // leading '#' (e.g. "Function" or "my.ns/Foo"); `tagged_value` is the
    // (mandatory) value the tag was applied to.
    std::string tag;
    Value *tagged_value = nullptr;

    // Integer literal value.
    int64_t int_value = 0;

    // Float literal value (always finite; see parser diagnostics for
    // rejection of overflowing/non-finite literals).
    double float_value = 0.0;

    // Boolean literal value (`true`/`false`).
    bool bool_value = false;
};

// Owns every `Value` node produced while parsing a single document, so
// nodes can safely be referenced by raw pointer (no reference counting,
// no copies) for the lifetime of the `Document`.
struct Document {
    std::vector<std::unique_ptr<Value>> nodes;

    // The single top-level value of the document (never null on success).
    Value *root = nullptr;

    Value *new_value(ValueKind kind, const Location &loc) {
        nodes.push_back(std::make_unique<Value>());
        Value *v = nodes.back().get();
        v->kind = kind;
        v->loc = loc;
        return v;
    }
};

// Parses `text` as a single EDN-subset value (per the grammar documented
// above `ValueKind`). On success, returns the owning `Document`. On
// failure, at least one `diag::Stage::ASRParser` error (with a primary
// label) is appended to `diagnostics` and an error `Result` is returned.
//
// This overload does not touch any `LocationManager`; `Location` spans in
// the returned tree are byte offsets into `text` itself. It is the
// preferred entry point when the caller does not need `diagnostics` to be
// immediately renderable against a source file (e.g. schema decoder unit
// tests), keeping this reader independent from any ASR/LocationManager
// bookkeeping.
Result<std::unique_ptr<Document>> parse(const std::string &text,
        diag::Diagnostics &diagnostics);

// Convenience overload: initializes `lm` (a single file named `filename`
// with contents `text`) before parsing, so that
// `diagnostics.render(lm, ...)` renders human-readable diagnostics against
// that file. Use this when the parsed text corresponds to a real file (or
// file-like buffer) whose diagnostics should point back at it.
Result<std::unique_ptr<Document>> parse(const std::string &text,
        const std::string &filename, LocationManager &lm,
        diag::Diagnostics &diagnostics);

} // namespace LCompilers::ASRText

#endif // LFORTRAN_ASR_TEXT_PARSER_H

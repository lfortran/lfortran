#ifndef LFORTRAN_ASR_TEXT_PARSER_H
#define LFORTRAN_ASR_TEXT_PARSER_H

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include <libasr/diagnostics.h>
#include <libasr/exception.h>
#include <libasr/location.h>

// Generic EDN-subset reader. Owns a `Document` of `Value` nodes; it does
// not know about ASR. `Location` spans are inclusive byte offsets into the
// input. The two-argument `parse` leaves locations as raw offsets; the
// four-argument overload initializes `lm` so diagnostics can be rendered.
namespace LCompilers::ASRText {

enum class ValueKind {
    List, Vector, Map, String, Symbol, Keyword, Integer, Float, Bool, Nil,
    Tagged
};

struct Value;

struct MapEntry {
    Value *key;
    Value *value;
};

struct Value {
    ValueKind kind;
    Location loc;
    std::vector<Value *> elements;
    std::vector<MapEntry> entries;
    std::string text;
    std::string tag;
    Value *tagged_value = nullptr;
    int64_t int_value = 0;
    double float_value = 0.0;
    bool bool_value = false;
};

struct Document {
    std::vector<std::unique_ptr<Value>> nodes;
    Value *root = nullptr;

    Value *new_value(ValueKind kind, const Location &loc) {
        nodes.push_back(std::make_unique<Value>());
        Value *v = nodes.back().get();
        v->kind = kind;
        v->loc = loc;
        return v;
    }
};

Result<std::unique_ptr<Document>> parse(const std::string &text,
        diag::Diagnostics &diagnostics);

Result<std::unique_ptr<Document>> parse(const std::string &text,
        const std::string &filename, LocationManager &lm,
        diag::Diagnostics &diagnostics);

} // namespace LCompilers::ASRText

#endif // LFORTRAN_ASR_TEXT_PARSER_H

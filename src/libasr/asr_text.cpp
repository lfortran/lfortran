#include <libasr/asr_text.h>

#include <algorithm>
#include <cmath>
#include <cstring>
#include <iomanip>
#include <initializer_list>
#include <limits>
#include <locale>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <utility>
#include <vector>

#include <libasr/asr_symbol_visitor.h>
#include <libasr/asr_text_deserialization_visitor.h>
#include <libasr/asr_text_parser.h>
#include <libasr/asr_text_visitor.h>
#include <libasr/asr_utils.h>

namespace LCompilers {

namespace {

std::string byte_hex(const uint8_t *bytes, size_t size) {
    static const char digits[] = "0123456789abcdef";
    std::string result;
    result.resize(size * 2);
    for (size_t i = 0; i < size; i++) {
        result[2 * i] = digits[bytes[i] >> 4];
        result[2 * i + 1] = digits[bytes[i] & 0x0f];
    }
    return result;
}

std::string uint64_hex(uint64_t value) {
    static const char digits[] = "0123456789abcdef";
    std::string result(16, '0');
    for (size_t i = 0; i < result.size(); i++) {
        size_t shift = (result.size() - i - 1) * 4;
        result[i] = digits[(value >> shift) & 0x0f];
    }
    return result;
}

class ASRTextSymbolTableCollector
    : public ASR::TextSymbolTableCollectorBaseVisitor<
        ASRTextSymbolTableCollector> {
public:
    std::map<const SymbolTable *, size_t> ids;

    void register_symbol_table(const SymbolTable *symtab) {
        if (ids.find(symtab) == ids.end()) {
            ids.emplace(symtab, ids.size());
        }
    }
};

class ASRTextWriter
    : public ASR::TextSerializationBaseVisitor<ASRTextWriter> {
    ASRTextOptions options;
    std::map<const SymbolTable *, size_t> symtab_ids;
    std::string output;
    size_t indent_level = 0;
    std::vector<size_t> form_sizes;
    std::vector<size_t> vector_sizes;
    std::vector<size_t> map_sizes;

    void append_indent() {
        output.append(indent_level * 2, ' ');
    }

    void append_separator() {
        if (options.indent) {
            output.push_back('\n');
            append_indent();
        } else {
            output.push_back(' ');
        }
    }

    void write_escaped_string(const char *value, size_t size) {
        output.push_back('"');
        for (size_t i = 0; i < size; i++) {
            const unsigned char c = static_cast<unsigned char>(value[i]);
            switch (c) {
                case '\\': output += "\\\\"; break;
                case '"': output += "\\\""; break;
                case '\n': output += "\\n"; break;
                case '\r': output += "\\r"; break;
                case '\t': output += "\\t"; break;
                case '\b': output += "\\b"; break;
                case '\f': output += "\\f"; break;
                default:
                    if (c < 0x20 || c == 0x7f) {
                        std::ostringstream hex;
                        hex << "\\u" << std::hex << std::setw(4)
                            << std::setfill('0') << static_cast<unsigned int>(c);
                        output += hex.str();
                    } else {
                        output.push_back(static_cast<char>(c));
                    }
            }
        }
        output.push_back('"');
    }

    size_t symbol_table_id(const SymbolTable &symtab) {
        auto found = symtab_ids.find(&symtab);
        if (found != symtab_ids.end()) {
            return found->second;
        }
        size_t id = symtab_ids.size();
        symtab_ids.emplace(&symtab, id);
        return id;
    }

public:
    ASRTextWriter(const ASRTextOptions &options,
        std::map<const SymbolTable *, size_t> ids)
        : options(options), symtab_ids(std::move(ids)) {
        output.reserve(100000);
    }

    std::string take_output() {
        return std::move(output);
    }

    void begin_document() {
        begin_form("ASRText", 2);
        begin_field("version");
        write_int(1);
        begin_field("value");
    }

    void end_document() {
        end_form();
    }

    void begin_form(const char *name, size_t field_count) {
        output.push_back('(');
        output += name;
        form_sizes.push_back(field_count);
        indent_level++;
    }

    void begin_field(const char *name) {
        append_separator();
        if (options.form == ASRTextForm::Named) {
            output.push_back(':');
            output += name;
            output.push_back(' ');
        }
    }

    void end_form() {
        LCOMPILERS_ASSERT(!form_sizes.empty());
        size_t field_count = form_sizes.back();
        form_sizes.pop_back();
        indent_level--;
        if (options.indent && field_count > 0) {
            output.push_back('\n');
            append_indent();
        }
        output.push_back(')');
    }

    void begin_vector(size_t size) {
        output.push_back('[');
        vector_sizes.push_back(size);
        indent_level++;
    }

    void begin_element(size_t index) {
        if (index > 0 || options.indent) {
            append_separator();
        }
    }

    void end_vector() {
        LCOMPILERS_ASSERT(!vector_sizes.empty());
        size_t size = vector_sizes.back();
        vector_sizes.pop_back();
        indent_level--;
        if (options.indent && size > 0) {
            output.push_back('\n');
            append_indent();
        }
        output.push_back(']');
    }

    void begin_map(size_t size) {
        output.push_back('{');
        map_sizes.push_back(size);
        indent_level++;
    }

    void begin_map_entry(const std::string &key, size_t index) {
        if (index > 0 || options.indent) {
            append_separator();
        }
        write_escaped_string(key.data(), key.size());
        output.push_back(' ');
    }

    void end_map() {
        LCOMPILERS_ASSERT(!map_sizes.empty());
        size_t size = map_sizes.back();
        map_sizes.pop_back();
        indent_level--;
        if (options.indent && size > 0) {
            output.push_back('\n');
            append_indent();
        }
        output.push_back('}');
    }

    void write_nil() {
        output += "nil";
    }

    void write_bool(bool value) {
        output += value ? "true" : "false";
    }

    void write_int(int64_t value) {
        output += std::to_string(value);
    }

    void write_float(double value) {
        if (!std::isfinite(value)) {
            uint64_t bits;
            std::memcpy(&bits, &value, sizeof(bits));
            output += "#asr/float64 \"";
            output += uint64_hex(bits);
            output.push_back('"');
            return;
        }
        if (value == 0.0 && std::signbit(value)) {
            output += "-0.0";
            return;
        }
        std::ostringstream value_stream;
        value_stream.imbue(std::locale::classic());
        value_stream << std::setprecision(
            std::numeric_limits<double>::max_digits10) << value;
        std::string text = value_stream.str();
        if (text.find_first_of(".eE") == std::string::npos) {
            text += ".0";
        }
        output += text;
    }

    void write_keyword(const char *value) {
        output.push_back(':');
        output += value;
    }

    void write_string(const char *value) {
        write_escaped_string(value, std::strlen(value));
    }

    void write_string_constant(const ASR::StringConstant_t &value) {
        size_t size = std::strlen(value.m_s);
        if (value.m_type && ASR::is_a<ASR::String_t>(*value.m_type)) {
            ASR::String_t *string_type =
                ASR::down_cast<ASR::String_t>(value.m_type);
            int64_t length = 0;
            if (string_type->m_len &&
                    ASRUtils::extract_value(string_type->m_len, length) &&
                    length >= 0) {
                size = static_cast<size_t>(length);
            }
        }
        write_escaped_string(value.m_s, size);
    }

    void write_bytes(const void *data, size_t size) {
        output += "#asr/bytes \"";
        output += byte_hex(static_cast<const uint8_t *>(data), size);
        output.push_back('"');
    }

    void write_real_constant(const ASR::RealConstant_t &value) {
        if (ASRUtils::extract_kind_from_ttype_t(value.m_type) == 16) {
            output += "#asr/real128 \"";
            output += byte_hex(
                ASRUtils::real_constant_unpack_r16(value.m_r), 16);
            output.push_back('"');
        } else {
            write_float(value.m_r);
        }
    }

    void write_symbol_table_ref(const SymbolTable &symtab) {
        output += std::to_string(symbol_table_id(symtab));
    }

    void write_symbol_ref(const ASR::symbol_t &symbol) {
        output += "(SymbolRef ";
        output += std::to_string(
            symbol_table_id(*ASRUtils::symbol_parent_symtab(&symbol)));
        output.push_back(' ');
        std::string name = ASRUtils::symbol_name(&symbol);
        write_escaped_string(name.data(), name.size());
        output.push_back(')');
    }
};

bool parse_hex(const std::string &text, std::vector<uint8_t> &bytes) {
    if (text.size() % 2 != 0) {
        return false;
    }
    bytes.resize(text.size() / 2);
    for (size_t i = 0; i < bytes.size(); i++) {
        auto hex_value = [](char value, uint8_t &result) {
            if (value >= '0' && value <= '9') {
                result = static_cast<uint8_t>(value - '0');
                return true;
            }
            if (value >= 'a' && value <= 'f') {
                result = static_cast<uint8_t>(value - 'a' + 10);
                return true;
            }
            if (value >= 'A' && value <= 'F') {
                result = static_cast<uint8_t>(value - 'A' + 10);
                return true;
            }
            return false;
        };
        uint8_t high, low;
        if (!hex_value(text[2 * i], high) ||
                !hex_value(text[2 * i + 1], low)) {
            return false;
        }
        bytes[i] = static_cast<uint8_t>((high << 4) | low);
    }
    return true;
}

class ASRTextDecoder
    : public ASR::TextDeserializationBaseVisitor<
        ASRTextDecoder, ASRText::Value> {
public:
    using TextValue = ASRText::Value;

private:
    Allocator &al;
    diag::Diagnostics &diagnostics;
    std::map<int64_t, SymbolTable *> symbol_tables;
    std::map<int64_t, const TextValue *> symbol_table_forms;
    std::set<int64_t> decoded_symbol_tables;
    bool failed = false;

    bool decode_symbol_table_id(const TextValue &value, int64_t &id) {
        if (value.kind != ASRText::ValueKind::Integer ||
                value.int_value < 0) {
            schema_error(value,
                "expected a nonnegative integer symbol table ID");
            return false;
        }
        id = value.int_value;
        return true;
    }

    bool unwrap_location(const TextValue &value,
            const TextValue *&unwrapped) {
        unwrapped = &value;
        if (value.kind != ASRText::ValueKind::Tagged ||
                value.tag != "asr/loc") {
            return true;
        }
        if (value.tagged_value == nullptr ||
                value.tagged_value->kind != ASRText::ValueKind::Vector ||
                value.tagged_value->elements.size() != 2) {
            schema_error(value,
                "#asr/loc expects [[first last] value]");
            return false;
        }
        const TextValue &span = *value.tagged_value->elements[0];
        if (span.kind != ASRText::ValueKind::Vector ||
                span.elements.size() != 2 ||
                span.elements[0]->kind != ASRText::ValueKind::Integer ||
                span.elements[1]->kind != ASRText::ValueKind::Integer ||
                span.elements[0]->int_value < 0 ||
                span.elements[1]->int_value < span.elements[0]->int_value ||
                static_cast<uint64_t>(span.elements[1]->int_value) >
                    std::numeric_limits<uint32_t>::max()) {
            schema_error(span,
                "#asr/loc span must contain ordered uint32 byte offsets");
            return false;
        }
        unwrapped = value.tagged_value->elements[1];
        return true;
    }

    bool collect_symbol_tables(const TextValue &value) {
        if (value.kind == ASRText::ValueKind::List) {
            std::string name;
            if (!decode_form_name(value, name)) {
                return false;
            }
            if (name == "SymbolTable") {
                std::vector<const TextValue *> fields;
                if (!decode_form(value, "SymbolTable",
                        {"id", "symbols"}, fields)) {
                    return false;
                }
                int64_t id;
                if (!decode_symbol_table_id(*fields[0], id)) {
                    return false;
                }
                if (symbol_tables.find(id) != symbol_tables.end()) {
                    schema_error(*fields[0],
                        "duplicate symbol table ID " + std::to_string(id));
                    return false;
                }
                symbol_tables[id] = al.make_new<SymbolTable>(nullptr);
                symbol_table_forms[id] = &value;
            }
            for (const TextValue *element : value.elements) {
                if (!collect_symbol_tables(*element)) {
                    return false;
                }
            }
        } else if (value.kind == ASRText::ValueKind::Vector) {
            for (const TextValue *element : value.elements) {
                if (!collect_symbol_tables(*element)) {
                    return false;
                }
            }
        } else if (value.kind == ASRText::ValueKind::Map) {
            for (const ASRText::MapEntry &entry : value.entries) {
                if (!collect_symbol_tables(*entry.key) ||
                        !collect_symbol_tables(*entry.value)) {
                    return false;
                }
            }
        } else if (value.kind == ASRText::ValueKind::Tagged) {
            return collect_symbol_tables(*value.tagged_value);
        }
        return true;
    }

    bool predeclare_symbols() {
        for (const auto &table_item : symbol_table_forms) {
            std::vector<const TextValue *> fields;
            if (!decode_form(*table_item.second, "SymbolTable",
                    {"id", "symbols"}, fields)) {
                return false;
            }
            if (fields[1]->kind != ASRText::ValueKind::Map) {
                schema_error(*fields[1],
                    "symbol table contents must be a map");
                return false;
            }
            SymbolTable *symtab = symbol_tables[table_item.first];
            for (const ASRText::MapEntry &entry : fields[1]->entries) {
                if (entry.key->kind != ASRText::ValueKind::String) {
                    schema_error(*entry.key,
                        "symbol table names must be strings");
                    return false;
                }
                std::string constructor;
                if (!decode_form_name(*entry.value, constructor)) {
                    return false;
                }
                ASR::symbolType type;
                if (!ASR::symbol_type_from_name(constructor, type)) {
                    schema_error(*entry.value, "'" + constructor +
                        "' is not an ASR symbol constructor");
                    return false;
                }
                if (symtab->get_symbol(entry.key->text) != nullptr) {
                    schema_error(*entry.key, "duplicate symbol '" +
                        entry.key->text + "'");
                    return false;
                }
                symtab->add_symbol(entry.key->text,
                    ASR::make_symbol_stub(
                        al, type, node_location(*entry.value)));
            }
        }
        return true;
    }

public:
    ASRTextDecoder(Allocator &al, diag::Diagnostics &diagnostics)
        : al(al), diagnostics(diagnostics) {}

    Allocator &allocator() {
        return al;
    }

    void schema_error(const TextValue &value, const std::string &message) {
        if (!failed) {
            failed = true;
            diagnostics.asr_parser_error_label(
                message, {value.loc}, "invalid ASR text here");
        }
    }

    bool decode_form_name(const TextValue &value, std::string &name) {
        const TextValue *form;
        if (!unwrap_location(value, form)) {
            return false;
        }
        if (form->kind != ASRText::ValueKind::List ||
                form->elements.empty() ||
                form->elements[0]->kind != ASRText::ValueKind::Symbol) {
            schema_error(value, "expected an ASR constructor list");
            return false;
        }
        name = form->elements[0]->text;
        return true;
    }

    bool decode_form(const TextValue &value, const char *expected_name,
            std::initializer_list<const char *> expected_fields,
            std::vector<const TextValue *> &fields) {
        std::string name;
        if (!decode_form_name(value, name)) {
            return false;
        }
        if (name != expected_name) {
            schema_error(value, "expected '" + std::string(expected_name) +
                "', found '" + name + "'");
            return false;
        }
        const TextValue *form;
        if (!unwrap_location(value, form)) {
            return false;
        }
        const size_t expected_count = expected_fields.size();
        fields.assign(expected_count, nullptr);
        std::vector<std::string> names;
        names.reserve(expected_count);
        for (const char *field : expected_fields) {
            names.emplace_back(field);
        }
        const bool named = form->elements.size() > 1 &&
            form->elements[1]->kind == ASRText::ValueKind::Keyword &&
            std::find(names.begin(), names.end(),
                form->elements[1]->text) != names.end();
        if (!named && form->elements.size() == expected_count + 1) {
            for (size_t i = 0; i < expected_count; i++) {
                fields[i] = form->elements[i + 1];
            }
            return true;
        }
        if (!named || form->elements.size() != 1 + 2 * expected_count) {
            schema_error(value, "'" + name + "' expects " +
                std::to_string(expected_count) +
                " positional fields or exactly " +
                std::to_string(expected_count) + " named fields");
            return false;
        }
        for (size_t i = 1; i < form->elements.size(); i += 2) {
            const TextValue &key = *form->elements[i];
            if (key.kind != ASRText::ValueKind::Keyword) {
                schema_error(key,
                    "cannot mix positional and named ASR fields");
                return false;
            }
            auto found = std::find(names.begin(), names.end(), key.text);
            if (found == names.end()) {
                schema_error(key, "unknown field :" + key.text +
                    " in '" + name + "'");
                return false;
            }
            const size_t index =
                static_cast<size_t>(std::distance(names.begin(), found));
            if (fields[index] != nullptr) {
                schema_error(key, "duplicate field :" + key.text);
                return false;
            }
            fields[index] = form->elements[i + 1];
        }
        for (size_t i = 0; i < fields.size(); i++) {
            if (fields[i] == nullptr) {
                schema_error(value, "missing field :" + names[i] +
                    " in '" + name + "'");
                return false;
            }
        }
        return true;
    }

    bool decode_keyword(const TextValue &value, std::string &keyword) {
        if (value.kind != ASRText::ValueKind::Keyword) {
            schema_error(value, "expected an enum keyword");
            return false;
        }
        keyword = value.text;
        return true;
    }

    bool decode_vector(const TextValue &value,
            std::vector<const TextValue *> &elements) {
        if (value.kind != ASRText::ValueKind::Vector) {
            schema_error(value, "expected a vector");
            return false;
        }
        elements.assign(value.elements.begin(), value.elements.end());
        return true;
    }

    bool decode_string(const TextValue &value, char *&result) {
        if (value.kind != ASRText::ValueKind::String) {
            schema_error(value, "expected a string");
            return false;
        }
        result = static_cast<char *>(al.alloc(value.text.size() + 1));
        std::memcpy(result, value.text.data(), value.text.size());
        result[value.text.size()] = '\0';
        return true;
    }

    bool decode_int(const TextValue &value, int64_t &result) {
        if (value.kind != ASRText::ValueKind::Integer) {
            schema_error(value, "expected an integer");
            return false;
        }
        result = value.int_value;
        return true;
    }

    bool decode_float(const TextValue &value, double &result) {
        if (value.kind == ASRText::ValueKind::Float) {
            result = value.float_value;
            return true;
        }
        if (value.kind != ASRText::ValueKind::Tagged ||
                value.tagged_value == nullptr ||
                value.tagged_value->kind != ASRText::ValueKind::String) {
            schema_error(value, "expected a floating-point value");
            return false;
        }
        std::vector<uint8_t> bytes;
        if (value.tag == "asr/float64") {
            if (!parse_hex(value.tagged_value->text, bytes) ||
                    bytes.size() != 8) {
                schema_error(value, "invalid #asr/float64 payload");
                return false;
            }
            uint64_t bits = 0;
            for (uint8_t byte : bytes) {
                bits = (bits << 8) | byte;
            }
            std::memcpy(&result, &bits, sizeof(result));
            return true;
        }
        if (value.tag == "asr/real128") {
            if (!parse_hex(value.tagged_value->text, bytes) ||
                    bytes.size() != 16) {
                schema_error(value, "invalid #asr/real128 payload");
                return false;
            }
            uint8_t *payload = static_cast<uint8_t *>(al.alloc(16));
            std::memcpy(payload, bytes.data(), bytes.size());
            result = ASRUtils::real_constant_pack_r16(payload);
            return true;
        }
        schema_error(value, "unknown floating-point tag #" + value.tag);
        return false;
    }

    bool decode_bool(const TextValue &value, bool &result) {
        if (value.kind != ASRText::ValueKind::Bool) {
            schema_error(value, "expected true or false");
            return false;
        }
        result = value.bool_value;
        return true;
    }

    bool decode_bytes(const TextValue &value, void *&result,
            size_t expected_size) {
        if (value.kind != ASRText::ValueKind::Tagged ||
                value.tag != "asr/bytes" ||
                value.tagged_value == nullptr ||
                value.tagged_value->kind != ASRText::ValueKind::String) {
            schema_error(value, "expected an #asr/bytes payload");
            return false;
        }
        std::vector<uint8_t> bytes;
        if (!parse_hex(value.tagged_value->text, bytes) ||
                bytes.size() != expected_size) {
            schema_error(value, "#asr/bytes payload size does not match n_data");
            return false;
        }
        result = al.alloc(expected_size);
        if (expected_size > 0) {
            std::memcpy(result, bytes.data(), expected_size);
        }
        return true;
    }

    bool is_nil(const TextValue &value) const {
        return value.kind == ASRText::ValueKind::Nil;
    }

    Location node_location(const TextValue &value) {
        if (value.kind == ASRText::ValueKind::Tagged &&
                value.tag == "asr/loc" && value.tagged_value != nullptr &&
                value.tagged_value->kind == ASRText::ValueKind::Vector &&
                value.tagged_value->elements.size() == 2) {
            const TextValue &span = *value.tagged_value->elements[0];
            if (span.kind == ASRText::ValueKind::Vector &&
                    span.elements.size() == 2 &&
                    span.elements[0]->kind == ASRText::ValueKind::Integer &&
                    span.elements[1]->kind == ASRText::ValueKind::Integer) {
                return {
                    static_cast<uint32_t>(span.elements[0]->int_value),
                    static_cast<uint32_t>(span.elements[1]->int_value)
                };
            }
        }
        const TextValue *form = &value;
        if (value.kind == ASRText::ValueKind::Tagged &&
                value.tag == "asr/loc" && value.tagged_value != nullptr &&
                value.tagged_value->kind == ASRText::ValueKind::Vector &&
                value.tagged_value->elements.size() == 2) {
            form = value.tagged_value->elements[1];
        }
        if (form->kind == ASRText::ValueKind::List &&
                !form->elements.empty()) {
            return form->elements[0]->loc;
        }
        return value.loc;
    }

    Location *make_location(const Location &loc) {
        Location *result = al.make_new<Location>();
        *result = loc;
        return result;
    }

    bool decode_symbol_table_ref(const TextValue &value,
            SymbolTable *&result) {
        int64_t id;
        if (!decode_symbol_table_id(value, id)) {
            return false;
        }
        auto found = symbol_tables.find(id);
        if (found == symbol_tables.end()) {
            schema_error(value, "unknown symbol table ID " +
                std::to_string(id));
            return false;
        }
        result = found->second;
        return true;
    }

    bool decode_symbol_ref(const TextValue &value,
            ASR::symbol_t *&result) {
        std::vector<const TextValue *> fields;
        if (!decode_form(value, "SymbolRef",
                {"symtab", "name"}, fields)) {
            return false;
        }
        SymbolTable *symtab;
        if (!decode_symbol_table_ref(*fields[0], symtab)) {
            return false;
        }
        const TextValue &name = *fields[1];
        if (name.kind != ASRText::ValueKind::String) {
            schema_error(name, "symbol name must be a string");
            return false;
        }
        result = symtab->get_symbol(name.text);
        if (result == nullptr) {
            schema_error(name, "unknown symbol '" + name.text + "'");
            return false;
        }
        return true;
    }

    bool decode_symbol_table(const TextValue &value,
            SymbolTable *&result) {
        std::vector<const TextValue *> fields;
        if (!decode_form(value, "SymbolTable", {"id", "symbols"}, fields)) {
            return false;
        }
        int64_t id;
        if (!decode_symbol_table_id(*fields[0], id)) {
            return false;
        }
        auto found = symbol_tables.find(id);
        if (found == symbol_tables.end()) {
            schema_error(*fields[0], "unknown symbol table ID " +
                std::to_string(id));
            return false;
        }
        result = found->second;
        if (decoded_symbol_tables.find(id) != decoded_symbol_tables.end()) {
            return true;
        }
        decoded_symbol_tables.insert(id);
        if (fields[1]->kind != ASRText::ValueKind::Map) {
            schema_error(*fields[1], "symbol table contents must be a map");
            return false;
        }
        for (const ASRText::MapEntry &entry : fields[1]->entries) {
            if (entry.key->kind != ASRText::ValueKind::String) {
                schema_error(*entry.key,
                    "symbol table names must be strings");
                return false;
            }
            ASR::symbol_t *decoded;
            if (!deserialize_symbol(*entry.value, decoded)) {
                return false;
            }
            ASR::symbol_t *stub = result->get_symbol(entry.key->text);
            if (stub == nullptr || stub->type != decoded->type) {
                schema_error(*entry.value,
                    "symbol definition does not match its declaration");
                return false;
            }
            ASR::fill_symbol_stub(stub, decoded);
        }
        return true;
    }

    Result<ASR::TranslationUnit_t *> decode(const TextValue &root) {
        std::vector<const TextValue *> document_fields;
        if (!decode_form(root, "ASRText",
                {"version", "value"}, document_fields)) {
            return Error();
        }
        int64_t version;
        if (!decode_int(*document_fields[0], version)) {
            return Error();
        }
        if (version != 1) {
            schema_error(*document_fields[0],
                "unsupported ASR text format version " +
                std::to_string(version));
            return Error();
        }
        const TextValue &value = *document_fields[1];
        if (!collect_symbol_tables(value) ||
                !predeclare_symbols()) {
            return Error();
        }
        ASR::unit_t *unit;
        if (!deserialize_unit(value, unit)) {
            return Error();
        }
        if (!ASR::is_a<ASR::TranslationUnit_t>(*unit)) {
            schema_error(value,
                "ASR text root must be a TranslationUnit");
            return Error();
        }
        ASR::TranslationUnit_t *translation_unit =
            ASR::down_cast<ASR::TranslationUnit_t>(unit);
        ASR::fix_symbol_table_parents(*translation_unit);
        return translation_unit;
    }
};

} // namespace

std::string asr_to_text(const ASR::asr_t &asr,
        const ASRTextOptions &options) {
    ASRTextSymbolTableCollector collector;
    if (asr.type == ASR::asrType::unit) {
        collector.visit_TranslationUnit(
            *ASR::down_cast2<ASR::TranslationUnit_t>(&asr));
    }
    ASRTextWriter writer(options, std::move(collector.ids));
    writer.begin_document();
    writer.visit_asr(asr);
    writer.end_document();
    return writer.take_output();
}

std::string asr_to_text(const ASR::TranslationUnit_t &asr,
        const ASRTextOptions &options) {
    return asr_to_text(
        (const ASR::asr_t &)asr, options);
}

Result<ASR::TranslationUnit_t *> asr_from_text(
        Allocator &al, const std::string &text, const std::string &filename,
        LocationManager &lm, diag::Diagnostics &diagnostics) {
    Result<std::unique_ptr<ASRText::Document>> parsed =
        ASRText::parse(text, filename, lm, diagnostics);
    if (!parsed.ok) {
        return Error();
    }
    ASRTextDecoder decoder(al, diagnostics);
    return decoder.decode(*parsed.result->root);
}

} // namespace LCompilers

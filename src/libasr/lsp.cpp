#include <iostream>
#include <stdint.h>
#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/asr_builder.h>
#include <libasr/location.h>
#include <lfortran/fortran_evaluator.h>
#include <libasr/lsp_interface.h>
#include <libasr/asr_lookup_name.h>

namespace LCompilers {

enum LFortranJSONType {
    kArrayType, kObjectType
};

class LFortranJSON {
    public:
        std::string value;
        LFortranJSONType type;

        LFortranJSON(LFortranJSONType type) : type(type) {}

        void SetObject() {
            if (type == LFortranJSONType::kArrayType) {
                value = "[";
            } else {
                value = "{";
            }
        }

        void SetArray() {
            value = "[";
        }

        void AddMember(std::string key, int v) {
            // find `{` and add key value pair
            int end_pos = value.find_last_of("}");
            // If '}' is not found, then add key value pair at the end and add '}'
            // If '}' is found, then add `, "key": value` before '}'
            if (end_pos == -1) {
                value.insert(value.length(), "\"" + key + "\":" + std::to_string(v) + "}");
            } else {
                value.insert(end_pos, ",\"" + key + "\":" + std::to_string(v));
            }
        }

        void AddMember(std::string key, uint32_t v) {
            // find `{` and add key value pair
            int end_pos = value.find_last_of("}");
            // If '}' is not found, then add key value pair at the end and add '}'
            // If '}' is found, then add `, "key": value` before '}'
            if (end_pos == -1) {
                value.insert(value.length(), "\"" + key + "\":" + std::to_string(v) + "}");
            } else {
                value.insert(end_pos, ",\"" + key + "\":" + std::to_string(v));
            }
        }

        void AddMember(std::string key, std::string v) {
            // find `{` and add key value pair
            int end_pos = value.find_last_of("}");
            // If '}' is not found, then add key value pair at the end and add '}'
            // If '}' is found, then add `, "key": value` before '}'
            if (end_pos == -1) {
                value.insert(value.length(), "\"" + key + "\":\"" + v + "\"}");
            } else {
                value.insert(end_pos, ",\"" + key + "\":\"" + v + "\"");
            }
        }

        void AddMember(std::string key, LFortranJSON v) {
            // find `{` and add key value pair
            int end_pos = value.find_last_of("}");
            // If '}' is not found, then add key value pair at the end and add '}'
            // If '}' is found, then add `, "key": value` before '}'
            if (end_pos == -1) {
                value.insert(value.length(), "\"" + key + "\":" + v.GetValue() + "}");
            } else {
                value.insert(end_pos, ",\"" + key + "\":" + v.GetValue());
            }
        }

        void PushBack(LFortranJSON v) {
            // find `[` and add value
            int end_pos = value.find_last_of("]");
            // If ']' is not found, then add value at the end and add ']'
            // If ']' is found, then add `, value` before ']'
            if (end_pos == -1) {
                value.insert(value.length(), v.GetValue() + "]");
            } else {
                value.insert(end_pos, "," + v.GetValue());
            }
        }

        std::string GetValue() {
            return value;
        }


};

template <typename T>
void populate_symbol_lists(T* x, LCompilers::LocationManager lm, std::vector<LCompilers::document_symbols> &symbol_lists) {
    LCompilers::document_symbols loc;
    for (auto &a : x->m_symtab->get_scope()) {
        std::string symbol_name = a.first;
        uint32_t first_line;
        uint32_t last_line;
        uint32_t first_column;
        uint32_t last_column;
        std::string filename;
        lm.pos_to_linecol(a.second->base.loc.first, first_line,
            first_column, filename);
        lm.pos_to_linecol(a.second->base.loc.last, last_line,
            last_column, filename);
        loc.first_column = first_column;
        loc.last_column = last_column;
        loc.first_line = first_line-1;
        loc.last_line = last_line-1;
        loc.symbol_name = symbol_name;
        loc.filename = filename;
        symbol_lists.push_back(loc);
        if ( LCompilers::ASR::is_a<LCompilers::ASR::Module_t>(*a.second) ) {
            LCompilers::ASR::Module_t *m = LCompilers::ASR::down_cast<LCompilers::ASR::Module_t>(a.second);
            populate_symbol_lists(m, lm, symbol_lists);
        } else if ( LCompilers::ASR::is_a<LCompilers::ASR::Function_t>(*a.second) ) {
            LCompilers::ASR::Function_t *f = LCompilers::ASR::down_cast<LCompilers::ASR::Function_t>(a.second);
            populate_symbol_lists(f, lm, symbol_lists);
        } else if ( LCompilers::ASR::is_a<LCompilers::ASR::Program_t>(*a.second) ) {
            LCompilers::ASR::Program_t *p = LCompilers::ASR::down_cast<LCompilers::ASR::Program_t>(a.second);
            populate_symbol_lists(p, lm, symbol_lists);
        }
    }
}

int get_symbols(const std::string &infile, CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);
    LCompilers::FortranEvaluator fe(compiler_options);
    std::vector<LCompilers::document_symbols> symbol_lists;

    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    {
        LCompilers::diag::Diagnostics diagnostics;
        LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
            x = fe.get_asr2(input, lm, diagnostics);
        if (x.ok) {
            populate_symbol_lists(x.result, lm, symbol_lists);
        } else {
            std::cout << "{}";
            return 0;
        }
    }

    LFortranJSON test_output(LFortranJSONType::kArrayType);
    LFortranJSON range_object(LFortranJSONType::kObjectType);
    LFortranJSON start_detail(LFortranJSONType::kObjectType);
    LFortranJSON end_detail(LFortranJSONType::kObjectType);
    LFortranJSON location_object(LFortranJSONType::kObjectType);
    LFortranJSON test_capture(LFortranJSONType::kObjectType);

    test_output.SetArray();

    for (auto symbol : symbol_lists) {
        uint32_t start_character = symbol.first_column;
        uint32_t start_line = symbol.first_line;
        uint32_t end_character = symbol.last_column;
        uint32_t end_line = symbol.last_line;
        std::string name = symbol.symbol_name;

        range_object.SetObject();

        start_detail.SetObject();
        start_detail.AddMember("character", start_character);
        start_detail.AddMember("line", start_line);
        range_object.AddMember("start", start_detail);

        end_detail.SetObject();
        end_detail.AddMember("character", end_character);
        end_detail.AddMember("line", end_line);
        range_object.AddMember("end", end_detail);

        location_object.SetObject();
        location_object.AddMember("range", range_object);
        location_object.AddMember("uri", "uri");

        test_capture.SetObject();
        test_capture.AddMember("kind", 1);
        test_capture.AddMember("location", location_object);
        test_capture.AddMember("name", name);
        test_output.PushBack(test_capture);
    }
    std::cout << test_output.GetValue();

    return 0;
}

int get_errors(const std::string &infile, CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);
    LCompilers::FortranEvaluator fe(compiler_options);

    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    LCompilers::diag::Diagnostics diagnostics;
    {
        LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
            result = fe.get_asr2(input, lm, diagnostics);
    }

    std::vector<LCompilers::error_highlight> diag_lists;
    LCompilers::error_highlight h;
    for (auto &d : diagnostics.diagnostics) {
        if (compiler_options.no_warnings && d.level != LCompilers::diag::Level::Error) {
            continue;
        }
        h.message = d.message;
        h.severity = d.level;
        for (auto label : d.labels) {
            for (auto span : label.spans) {
                uint32_t first_line;
                uint32_t first_column;
                uint32_t last_line;
                uint32_t last_column;
                std::string filename;
                lm.pos_to_linecol(span.loc.first, first_line, first_column,
                    filename);
                lm.pos_to_linecol(span.loc.last, last_line, last_column,
                    filename);
                h.first_column = first_column;
                h.last_column = last_column;
                h.first_line = first_line-1;
                h.last_line = last_line-1;
                h.filename = filename;
                diag_lists.push_back(h);
            }
        }
    }

    LFortranJSON range_obj(LFortranJSONType::kObjectType);
    LFortranJSON start_detail(LFortranJSONType::kObjectType);
    LFortranJSON end_detail(LFortranJSONType::kObjectType);
    LFortranJSON diag_results(LFortranJSONType::kArrayType);
    LFortranJSON diag_capture(LFortranJSONType::kObjectType);
    LFortranJSON message_send(LFortranJSONType::kObjectType);

    for (auto diag : diag_lists) {
        uint32_t start_line = diag.first_line;
        uint32_t start_column = diag.first_column;
        uint32_t end_line = diag.last_line;
        uint32_t end_column = diag.last_column;
        uint32_t severity = diag.severity;
        std::string msg = diag.message;

        range_obj.SetObject();

        start_detail.SetObject();
        start_detail.AddMember("line", start_line);
        start_detail.AddMember("character", start_column);
        range_obj.AddMember("start", start_detail);

        end_detail.SetObject();
        end_detail.AddMember("line", end_line);
        end_detail.AddMember("character", end_column);
        range_obj.AddMember("end", end_detail);

        diag_results.SetArray();

        diag_capture.SetObject();
        diag_capture.AddMember("source", "lpyth");
        diag_capture.AddMember("range", range_obj);
        diag_capture.AddMember("message", msg);
        diag_capture.AddMember("severity", severity);
        diag_results.PushBack(diag_capture);

        message_send.SetObject();
        message_send.AddMember("uri", "uri");
        message_send.AddMember("diagnostics", diag_results);
    }
    std::cout << message_send.GetValue();

    return 0;
}

int get_definitions(const std::string &infile, LCompilers::CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);
    LCompilers::FortranEvaluator fe(compiler_options);
    std::vector<LCompilers::document_symbols> symbol_lists;

    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    {
        LCompilers::diag::Diagnostics diagnostics;
        LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
            x = fe.get_asr2(input, lm, diagnostics);
        if (x.ok) {
            // populate_symbol_lists(x.result, lm, symbol_lists);
            uint16_t l = std::stoi(compiler_options.line);
            uint16_t c = std::stoi(compiler_options.column);
            uint64_t pos = lm.linecol_to_pos(l, c);
            LCompilers::ASR::asr_t* asr = fe.handle_lookup_name(x.result, pos);
            LCompilers::document_symbols loc;
            if (!ASR::is_a<ASR::symbol_t>(*asr)) {
                std::cout << "[]";
                return 0;
            }
            ASR::symbol_t* s = ASR::down_cast<ASR::symbol_t>(asr);
            std::string symbol_name = ASRUtils::symbol_name( s );
            uint32_t first_line;
            uint32_t last_line;
            uint32_t first_column;
            uint32_t last_column;
            std::string filename;
            lm.pos_to_linecol(asr->loc.first, first_line,
                first_column, filename);
            lm.pos_to_linecol(asr->loc.last, last_line,
                last_column, filename);
            loc.first_column = first_column;
            loc.last_column = last_column;
            loc.first_line = first_line-1;
            loc.last_line = last_line-1;
            loc.symbol_name = symbol_name;
            loc.filename = filename;
            symbol_lists.push_back(loc);
        } else {
            std::cout << "[]";
            return 0;
        }
    }

    LFortranJSON start_detail(LFortranJSONType::kObjectType);
    LFortranJSON range_object(LFortranJSONType::kObjectType);
    LFortranJSON end_detail(LFortranJSONType::kObjectType);
    LFortranJSON location_object(LFortranJSONType::kObjectType);
    LFortranJSON test_capture(LFortranJSONType::kObjectType);
    LFortranJSON test_output(LFortranJSONType::kArrayType);

    test_output.SetArray();

    for (auto symbol : symbol_lists) {
        uint32_t start_character = symbol.first_column;
        uint32_t start_line = symbol.first_line;
        uint32_t end_character = symbol.last_column;
        uint32_t end_line = symbol.last_line;
        std::string name = symbol.symbol_name;

        range_object.SetObject();

        start_detail.SetObject();
        start_detail.AddMember("character", start_character);
        start_detail.AddMember("line", start_line);
        range_object.AddMember("start", start_detail);

        end_detail.SetObject();
        end_detail.AddMember("character", end_character);
        end_detail.AddMember("line", end_line);
        range_object.AddMember("end", end_detail);

        location_object.SetObject();
        location_object.AddMember("range", range_object);
        location_object.AddMember("uri", "uri");

        test_capture.SetObject();
        test_capture.AddMember("kind", 1);
        test_capture.AddMember("location", location_object);
        test_capture.AddMember("name", name);
        test_output.PushBack(test_capture);
    }

    std::cout << test_output.GetValue();

    return 0;
}

int get_all_occurences(const std::string &infile, LCompilers::CompilerOptions &compiler_options)
{
    std::string input = read_file(infile);
    LCompilers::FortranEvaluator fe(compiler_options);
    std::vector<LCompilers::document_symbols> symbol_lists;

    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = infile;
        lm.files.push_back(fl);
        lm.file_ends.push_back(input.size());
    }
    {
        LCompilers::diag::Diagnostics diagnostics;
        LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
            x = fe.get_asr2(input, lm, diagnostics);
        if (x.ok) {
            // populate_symbol_lists(x.result, lm, symbol_lists);
            uint16_t l = std::stoi(compiler_options.line);
            uint16_t c = std::stoi(compiler_options.column);
            uint64_t pos = lm.linecol_to_pos(l, c);
            LCompilers::ASR::asr_t* asr = fe.handle_lookup_name(x.result, pos);
            LCompilers::document_symbols loc;
            if (!ASR::is_a<ASR::symbol_t>(*asr)) {
                std::cout << "[]";
                return 0;
            }
            ASR::symbol_t* s = ASR::down_cast<ASR::symbol_t>(asr);
            std::string symbol_name = ASRUtils::symbol_name( s );
            LCompilers::LFortran::OccurenceCollector occ(symbol_name, symbol_lists, lm);
            occ.visit_TranslationUnit(*x.result);
        } else {
            std::cout << "[]";
            return 0;
        }
    }

    LFortranJSON start_detail(LFortranJSONType::kObjectType);
    LFortranJSON range_object(LFortranJSONType::kObjectType);
    LFortranJSON end_detail(LFortranJSONType::kObjectType);
    LFortranJSON location_object(LFortranJSONType::kObjectType);
    LFortranJSON test_capture(LFortranJSONType::kObjectType);
    LFortranJSON test_output(LFortranJSONType::kArrayType);

    test_output.SetArray();

    for (auto symbol : symbol_lists) {
        uint32_t start_character = symbol.first_column;
        uint32_t start_line = symbol.first_line;
        uint32_t end_character = symbol.last_column;
        uint32_t end_line = symbol.last_line;
        std::string name = symbol.symbol_name;

        range_object.SetObject();

        start_detail.SetObject();
        start_detail.AddMember("character", start_character);
        start_detail.AddMember("line", start_line);
        range_object.AddMember("start", start_detail);

        end_detail.SetObject();
        end_detail.AddMember("character", end_character);
        end_detail.AddMember("line", end_line);
        range_object.AddMember("end", end_detail);

        location_object.SetObject();
        location_object.AddMember("range", range_object);
        location_object.AddMember("uri", "uri");

        test_capture.SetObject();
        test_capture.AddMember("kind", 1);
        test_capture.AddMember("location", location_object);
        test_capture.AddMember("name", name);
        test_output.PushBack(test_capture);
    }

    std::cout << test_output.GetValue();

    return 0;
}

}


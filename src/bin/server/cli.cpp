#include <iostream>

#include <libasr/asr_builder.h>
#include <libasr/asr.h>
#include <libasr/asr_lookup_name.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/containers.h>
#include <libasr/diagnostics.h>
#include <libasr/exception.h>
#include <libasr/location.h>
#include <libasr/lsp_interface.h>

#include <server/specification.h>

#include <bin/server/utils.h>
#include <bin/server/cli.h>
#include <bin/server/lfortran_accessor.h>

namespace LCompilers {
    namespace lsp = LCompilers::LanguageServerProtocol;

    LFortranJSON::LFortranJSON(LFortranJSONType type)
        : type(type)
        , rebuild_needed(true)
    {
        if (type == kArrayType) {
            json_value = "[]";
        } else {
            json_value = "{}";
        }
    }

    void LFortranJSON::SetObject() {
        type = kObjectType;
        object_members.clear();
        json_value = "{}";
        rebuild_needed = false;
    }

    void LFortranJSON::SetArray() {
        type = kArrayType;
        array_values.clear();
        json_value = "[]";
        rebuild_needed = false;
    }

    void LFortranJSON::AddMember(std::string key, int v) {
        object_members.push_back({key, std::to_string(v)});
        rebuild_needed = true;
    }

    void LFortranJSON::AddMember(std::string key, uint32_t v) {
        object_members.push_back({key, std::to_string(v)});
        rebuild_needed = true;
    }

    void LFortranJSON::AddMember(std::string key, std::string v) {
        object_members.push_back({key, "\"" + v + "\""});
        rebuild_needed = true;
    }

    void LFortranJSON::AddMember(std::string key, LFortranJSON v) {
        object_members.push_back({key, v.GetValue()});
        rebuild_needed = true;
    }

    void LFortranJSON::PushBack(LFortranJSON v) {
        array_values.push_back(v.GetValue());
        rebuild_needed = true;
    }

    std::string LFortranJSON::GetValue() {
        if (rebuild_needed) {
            RebuildJSON();
            rebuild_needed = false;
        }
        return json_value;
    }

    void LFortranJSON::RebuildJSON() {
        if (type == kObjectType) {
            json_value = "{";
            for (size_t i = 0; i < object_members.size(); i++) {
                json_value += "\"" + object_members[i].first + "\":" + object_members[i].second;
                if (i < object_members.size() - 1) {
                    json_value += ",";
                }
            }
            json_value += "}";
        } else if (type == kArrayType) {
            json_value = "[";
            for (size_t i = 0; i < array_values.size(); i++) {
                json_value += array_values[i];
                if (i < array_values.size() - 1) {
                    json_value += ",";
                }
            }
            json_value += "]";
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
            lsp::SymbolKind kind =
                lsp::asr_symbol_type_to_lsp_symbol_kind(symbol.symbol_type);

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
            test_capture.AddMember("kind", static_cast<int>(kind));
            test_capture.AddMember("location", location_object);
            test_capture.AddMember("name", name);
            test_capture.AddMember("filename", symbol.filename);
            test_output.PushBack(test_capture);
        }
        std::cout << test_output.GetValue();

        return 0;
    }

    int get_errors(const std::string &infile, CompilerOptions &compiler_options)
    {
        std::string input = read_file(infile);
        LCompilers::LLanguageServer::LFortranAccessor lfortran_accessor;
        std::vector<LCompilers::error_highlight> diag_lists =
            lfortran_accessor.showErrors(infile, input, compiler_options);

        LFortranJSON range_obj(LFortranJSONType::kObjectType);
        LFortranJSON start_detail(LFortranJSONType::kObjectType);
        LFortranJSON end_detail(LFortranJSONType::kObjectType);
        LFortranJSON diag_results(LFortranJSONType::kArrayType);
        LFortranJSON diag_capture(LFortranJSONType::kObjectType);
        LFortranJSON message_send(LFortranJSONType::kObjectType);
        LFortranJSON all_errors(LFortranJSONType::kArrayType);
        all_errors.SetArray();

        message_send.SetObject();
        message_send.AddMember("uri", "file://" + infile);

        for (auto diag : diag_lists) {
            uint32_t start_line = diag.first_line;
            uint32_t start_column = diag.first_column;
            uint32_t end_line = diag.last_line;
            uint32_t end_column = diag.last_column;
            lsp::DiagnosticSeverity severity =
                lsp::diagnostic_level_to_lsp_severity(diag.severity);
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

            diag_capture.SetObject();
            diag_capture.AddMember("source", "lpyth");
            diag_capture.AddMember("range", range_obj);
            diag_capture.AddMember("message", msg);
            diag_capture.AddMember("severity", static_cast<int>(severity));

            all_errors.PushBack(diag_capture);
        }
        message_send.AddMember("diagnostics", all_errors);
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
                uint64_t input_pos = lm.linecol_to_pos(l, c);
                if (c > 0 && input_pos > 0 && !is_id_chr(input[input_pos]) && is_id_chr(input[input_pos - 1])) {
                    // input_pos is to the right of the word boundary
                    input_pos--;
                }
                uint64_t output_pos = lm.input_to_output_pos(input_pos, false);
                LCompilers::ASR::asr_t* asr = fe.handle_lookup_name(x.result, output_pos);
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
                lm.pos_to_linecol(lm.output_to_input_pos(asr->loc.first, false), first_line,
                    first_column, filename);
                lm.pos_to_linecol(lm.output_to_input_pos(asr->loc.last, true), last_line,
                    last_column, filename);
                loc.first_column = first_column;
                loc.last_column = last_column;
                loc.first_line = first_line;
                loc.last_line = last_line;
                loc.symbol_name = symbol_name;
                loc.filename = filename;
                loc.symbol_type = s->type;
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
            lsp::SymbolKind kind =
                lsp::asr_symbol_type_to_lsp_symbol_kind(symbol.symbol_type);

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
            location_object.AddMember("uri", symbol.filename);

            test_capture.SetObject();
            test_capture.AddMember("kind", static_cast<int>(kind));
            test_capture.AddMember("location", location_object);
            test_capture.AddMember("name", name);
            test_capture.AddMember("filename", symbol.filename);
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
                uint64_t input_pos = lm.linecol_to_pos(l, c);
                uint64_t output_pos = lm.input_to_output_pos(input_pos, false);
                LCompilers::ASR::asr_t* asr = fe.handle_lookup_name(x.result, output_pos);
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
            lsp::SymbolKind kind =
                lsp::asr_symbol_type_to_lsp_symbol_kind(symbol.symbol_type);

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
            test_capture.AddMember("kind", static_cast<int>(kind));
            test_capture.AddMember("location", location_object);
            test_capture.AddMember("name", name);
            test_output.PushBack(test_capture);
        }

        std::cout << test_output.GetValue();

        return 0;
    }

} // namespace LCompilers


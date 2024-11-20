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


#ifdef HAVE_LFORTRAN_RAPIDJSON
    #include <libasr/lsp_interface.h>
    #include <rapidjson/document.h>
    #include <rapidjson/stringbuffer.h>
    #include <rapidjson/writer.h>
#endif

namespace LCompilers {
#ifdef HAVE_LFORTRAN_RAPIDJSON
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

    rapidjson::Document test_output(rapidjson::kArrayType);
    rapidjson::Document range_object(rapidjson::kObjectType);
    rapidjson::Document start_detail(rapidjson::kObjectType);
    rapidjson::Document end_detail(rapidjson::kObjectType);
    rapidjson::Document location_object(rapidjson::kObjectType);
    rapidjson::Document test_capture(rapidjson::kObjectType);

    test_output.SetArray();

    for (auto symbol : symbol_lists) {
        uint32_t start_character = symbol.first_column;
        uint32_t start_line = symbol.first_line;
        uint32_t end_character = symbol.last_column;
        uint32_t end_line = symbol.last_line;
        std::string name = symbol.symbol_name;

        range_object.SetObject();
        rapidjson::Document::AllocatorType &allocator = range_object.GetAllocator();

        start_detail.SetObject();
        start_detail.AddMember("character", rapidjson::Value().SetInt(start_character), allocator);
        start_detail.AddMember("line", rapidjson::Value().SetInt(start_line), allocator);
        range_object.AddMember("start", start_detail, allocator);

        end_detail.SetObject();
        end_detail.AddMember("character", rapidjson::Value().SetInt(end_character), allocator);
        end_detail.AddMember("line", rapidjson::Value().SetInt(end_line), allocator);
        range_object.AddMember("end", end_detail, allocator);

        location_object.SetObject();
        location_object.AddMember("range", range_object, allocator);
        location_object.AddMember("uri", rapidjson::Value().SetString("uri", allocator), allocator);

        test_capture.SetObject();
        test_capture.AddMember("kind", rapidjson::Value().SetInt(1), allocator);
        test_capture.AddMember("location", location_object, allocator);
        test_capture.AddMember("name", rapidjson::Value().SetString(name.c_str(), allocator), allocator);
        test_output.PushBack(test_capture, test_output.GetAllocator());
    }
    rapidjson::StringBuffer buffer;
    buffer.Clear();
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    test_output.Accept(writer);
    std::string resp_str( buffer.GetString() );

    std::cout << resp_str;

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

    rapidjson::Document range_obj(rapidjson::kObjectType);
    rapidjson::Document start_detail(rapidjson::kObjectType);
    rapidjson::Document end_detail(rapidjson::kObjectType);
    rapidjson::Document diag_results(rapidjson::kArrayType);
    rapidjson::Document diag_capture(rapidjson::kObjectType);
    rapidjson::Document message_send(rapidjson::kObjectType);

    for (auto diag : diag_lists) {
        uint32_t start_line = diag.first_line;
        uint32_t start_column = diag.first_column;
        uint32_t end_line = diag.last_line;
        uint32_t end_column = diag.last_column;
        uint32_t severity = diag.severity;
        std::string msg = diag.message;

        range_obj.SetObject();
        rapidjson::Document::AllocatorType &allocator = range_obj.GetAllocator();

        start_detail.SetObject();
        start_detail.AddMember("line", rapidjson::Value().SetInt(start_line), allocator);
        start_detail.AddMember("character", rapidjson::Value().SetInt(start_column), allocator);
        range_obj.AddMember("start", start_detail, allocator);

        end_detail.SetObject();
        end_detail.AddMember("line", rapidjson::Value().SetInt(end_line), allocator);
        end_detail.AddMember("character", rapidjson::Value().SetInt(end_column), allocator);
        range_obj.AddMember("end", end_detail, allocator);

        diag_results.SetArray();

        diag_capture.AddMember("source", rapidjson::Value().SetString("lpyth", allocator), allocator);
        diag_capture.AddMember("range", range_obj, allocator);
        diag_capture.AddMember("message", rapidjson::Value().SetString(msg.c_str(), allocator), allocator);
        diag_capture.AddMember("severity", rapidjson::Value().SetInt(severity), allocator);
        diag_results.PushBack(diag_capture, allocator);

        message_send.SetObject();
        message_send.AddMember("uri", rapidjson::Value().SetString("uri", allocator), allocator);
        message_send.AddMember("diagnostics", diag_results, allocator);
    }

    rapidjson::StringBuffer buffer;
    buffer.Clear();
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    message_send.Accept(writer);
    std::string resp_str( buffer.GetString() );
    std::cout << resp_str;

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
            std::cout << "{}";
            return 0;
        }
    }

    rapidjson::Document test_output(rapidjson::kArrayType);
    rapidjson::Document range_object(rapidjson::kObjectType);
    rapidjson::Document start_detail(rapidjson::kObjectType);
    rapidjson::Document end_detail(rapidjson::kObjectType);
    rapidjson::Document location_object(rapidjson::kObjectType);
    rapidjson::Document test_capture(rapidjson::kObjectType);

    test_output.SetArray();

    for (auto symbol : symbol_lists) {
        uint32_t start_character = symbol.first_column;
        uint32_t start_line = symbol.first_line;
        uint32_t end_character = symbol.last_column;
        uint32_t end_line = symbol.last_line;
        std::string name = symbol.symbol_name;

        range_object.SetObject();
        rapidjson::Document::AllocatorType &allocator = range_object.GetAllocator();

        start_detail.SetObject();
        start_detail.AddMember("character", rapidjson::Value().SetInt(start_character), allocator);
        start_detail.AddMember("line", rapidjson::Value().SetInt(start_line), allocator);
        range_object.AddMember("start", start_detail, allocator);

        end_detail.SetObject();
        end_detail.AddMember("character", rapidjson::Value().SetInt(end_character), allocator);
        end_detail.AddMember("line", rapidjson::Value().SetInt(end_line), allocator);
        range_object.AddMember("end", end_detail, allocator);

        location_object.SetObject();
        location_object.AddMember("range", range_object, allocator);
        location_object.AddMember("uri", rapidjson::Value().SetString("uri", allocator), allocator);

        test_capture.SetObject();
        test_capture.AddMember("kind", rapidjson::Value().SetInt(1), allocator);
        test_capture.AddMember("location", location_object, allocator);
        test_capture.AddMember("name", rapidjson::Value().SetString(name.c_str(), allocator), allocator);
        test_output.PushBack(test_capture, test_output.GetAllocator());
    }
    rapidjson::StringBuffer buffer;
    buffer.Clear();
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    test_output.Accept(writer);
    std::string resp_str( buffer.GetString() );

    std::cout << resp_str;

    return 0;
}
#endif
}


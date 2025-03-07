#include <cstdint>

#include <libasr/asr.h>
#include <libasr/asr_lookup_name.h>
#include <libasr/diagnostics.h>
#include <libasr/exception.h>
#include <libasr/location.h>

#include <lfortran/ast_to_src.h>
#include <lfortran/fortran_evaluator.h>

#include <bin/lfortran_accessor.h>

namespace LCompilers::LLanguageServer {

    auto LFortranAccessor::showErrors(
        const std::string &filename,
        const std::string &text,
        const CompilerOptions &compiler_options
    ) const -> std::vector<LCompilers::error_highlight> {
        LCompilers::FortranEvaluator fe(compiler_options);

        LCompilers::LocationManager lm;
        {
                LCompilers::LocationManager::FileLocations fl;
                fl.in_filename = filename;
                lm.files.push_back(fl);
                lm.file_ends.push_back(text.size());
        }

        LCompilers::diag::Diagnostics diagnostics;
        {
                LCompilers::Result<LCompilers::ASR::TranslationUnit_t*> result =
                    fe.get_asr2(text, lm, diagnostics);
        }

        std::vector<LCompilers::error_highlight> diag_lists;
        diag_lists.reserve(diagnostics.diagnostics.size());
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
                    h.first_line = first_line;
                    h.last_line = last_line;
                    h.filename = filename;
                    diag_lists.push_back(h);
                }
            }
        }

        return diag_lists;
    }

    auto LFortranAccessor::lookupName(
        const std::string &filename,
        const std::string &text,
        const CompilerOptions &compiler_options
    ) const -> std::vector<LCompilers::document_symbols> {
        LCompilers::FortranEvaluator fe(compiler_options);
        std::vector<LCompilers::document_symbols> symbol_lists;

        LCompilers::LocationManager lm;
        {
            LCompilers::LocationManager::FileLocations fl;
            fl.in_filename = filename;
            lm.files.push_back(fl);
            lm.file_ends.push_back(text.size());
        }
        {
            LCompilers::diag::Diagnostics diagnostics;
            LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
                x = fe.get_asr2(text, lm, diagnostics);
            if (x.ok) {
                // populate_symbol_lists(x.result, lm, symbol_lists);
                uint16_t l = std::stoi(compiler_options.line);
                uint16_t c = std::stoi(compiler_options.column);
                uint64_t input_pos = lm.linecol_to_pos(l, c);
                if (c > 0 && input_pos > 0 && !is_id_chr(text[input_pos]) &&
                    is_id_chr(text[input_pos - 1])) {
                    // input_pos is to the right of the word boundary
                    --input_pos;
                }
                uint64_t output_pos = lm.input_to_output_pos(input_pos, false);
                LCompilers::ASR::asr_t* asr =
                    fe.handle_lookup_name(x.result, output_pos);
                if (ASR::is_a<ASR::symbol_t>(*asr)) {
                    ASR::symbol_t* s = ASR::down_cast<ASR::symbol_t>(asr);
                    std::string symbol_name = ASRUtils::symbol_name( s );
                    LCompilers::document_symbols &loc = symbol_lists.emplace_back();
                    loc.symbol_name = symbol_name;
                    loc.first_pos = lm.output_to_input_pos(asr->loc.first, false);
                    lm.pos_to_linecol(
                        loc.first_pos,
                        loc.first_line,
                        loc.first_column,
                        loc.filename
                    );
                    loc.last_pos = lm.output_to_input_pos(asr->loc.last, true);
                    lm.pos_to_linecol(
                        loc.last_pos,
                        loc.last_line,
                        loc.last_column,
                        loc.filename
                    );
                    loc.symbol_type = s->type;
                }
            }
        }

        return symbol_lists;
    }

    auto LFortranAccessor::getAllOccurrences(
        const std::string &filename,
        const std::string &text,
        const CompilerOptions &compiler_options
    ) const -> std::vector<LCompilers::document_symbols> {
        LCompilers::FortranEvaluator fe(compiler_options);
        std::vector<LCompilers::document_symbols> symbol_lists;

        LCompilers::LocationManager lm;
        {
            LCompilers::LocationManager::FileLocations fl;
            fl.in_filename = filename;
            lm.files.push_back(fl);
            lm.file_ends.push_back(text.size());
        }
        {
            LCompilers::diag::Diagnostics diagnostics;
            LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
                x = fe.get_asr2(text, lm, diagnostics);
            if (x.ok) {
                // populate_symbol_lists(x.result, lm, symbol_lists);
                uint16_t l = std::stoi(compiler_options.line);
                uint16_t c = std::stoi(compiler_options.column);
                uint64_t input_pos = lm.linecol_to_pos(l, c);
                uint64_t output_pos = lm.input_to_output_pos(input_pos, false);
                LCompilers::ASR::asr_t* asr = fe.handle_lookup_name(x.result, output_pos);
                LCompilers::document_symbols loc;
                if (ASR::is_a<ASR::symbol_t>(*asr)) {
                    ASR::symbol_t* s = ASR::down_cast<ASR::symbol_t>(asr);
                    std::string symbol_name = ASRUtils::symbol_name( s );
                    LCompilers::LFortran::OccurenceCollector occ(symbol_name, symbol_lists, lm);
                    occ.visit_TranslationUnit(*x.result);
                }
            }
        }

        return symbol_lists;
    }

    auto LFortranAccessor::format(
        const std::string &filename,
        const std::string &text,
        const CompilerOptions &compiler_options,
        bool color,
        int indent,
        bool indent_unit
    ) const -> LCompilers::Result<std::string> {
        LCompilers::FortranEvaluator fe(compiler_options);
        LCompilers::LocationManager lm;
        LCompilers::diag::Diagnostics diagnostics;
        {
            LCompilers::LocationManager::FileLocations fl;
            fl.in_filename = filename;
            lm.files.push_back(fl);
            lm.file_ends.push_back(text.size());
        }
        LCompilers::Result<LCompilers::LFortran::AST::TranslationUnit_t*>
            r = fe.get_ast2(text, lm, diagnostics);
        std::cerr << diagnostics.render(lm, compiler_options);
        if (!r.ok) {
            LCOMPILERS_ASSERT(diagnostics.has_error())
            return LCompilers::Error();
        }
        LCompilers::LFortran::AST::TranslationUnit_t* ast = r.result;

        // AST -> Source
        std::string source = LCompilers::LFortran::ast_to_src(*ast, color,
            indent, indent_unit);

        return source;
    }

    auto LFortranAccessor::getSymbols(
        const std::string &filename,
        const std::string &text,
        const CompilerOptions &compiler_options
    ) const -> std::vector<LCompilers::document_symbols> {
        LCompilers::FortranEvaluator fe(compiler_options);
        std::vector<LCompilers::document_symbols> symbol_lists;

        LCompilers::LocationManager lm;
        {
            LCompilers::LocationManager::FileLocations fl;
            fl.in_filename = filename;
            lm.files.push_back(fl);
            lm.file_ends.push_back(text.size());
        }
        {
            LCompilers::diag::Diagnostics diagnostics;
            LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
                x = fe.get_asr2(text, lm, diagnostics);
            if (x.ok) {
              populateSymbolLists(x.result, lm, symbol_lists, -1);
            }
        }

        return symbol_lists;
    }

} // namespace LCompilers::LLanguageServer

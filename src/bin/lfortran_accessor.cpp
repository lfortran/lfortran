#include <cstdint>

#include <libasr/asr.h>
#include <libasr/diagnostics.h>
#include <libasr/exception.h>
#include <libasr/location.h>

#include <lfortran/fortran_evaluator.h>

#include <bin/lfortran_accessor.h>

namespace LCompilers::LLanguageServer {

    auto LFortranAccessor::showErrors(
        const std::string &filename,
        const std::string &text,
        const CompilerOptions &compiler_options
    ) -> std::vector<LCompilers::error_highlight> {
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
    ) -> std::vector<LCompilers::document_symbols> {
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
                    uint32_t first_line;
                    uint32_t last_line;
                    uint32_t first_column;
                    uint32_t last_column;
                    std::string filename;
                    lm.pos_to_linecol(
                        lm.output_to_input_pos(asr->loc.first, false),
                        first_line, first_column, filename);
                    lm.pos_to_linecol(
                        lm.output_to_input_pos(asr->loc.last, true),
                        last_line, last_column, filename);
                    LCompilers::document_symbols loc;
                    loc.first_column = first_column;
                    loc.last_column = last_column;
                    loc.first_line = first_line;
                    loc.last_line = last_line;
                    loc.symbol_name = symbol_name;
                    loc.filename = filename;
                    loc.symbol_type = s->type;
                    symbol_lists.push_back(loc);
                }
            }
        }

        return symbol_lists;
    }

} // namespace LCompilers::LLanguageServer

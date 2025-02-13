#include <cstdint>

#include <libasr/asr.h>
#include <libasr/diagnostics.h>
#include <libasr/exception.h>
#include <libasr/location.h>

#include <lfortran/fortran_evaluator.h>

#include <bin/server/lfortran_accessor.h>

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

} // namespace LCompilers::LLanguageServer

#pragma once

#include <mutex>
#include <string>
#include <utility>
#include <vector>

#include <libasr/exception.h>
#include <libasr/lsp_interface.h>
#include <libasr/asr_utils.h>
#include <libasr/utils.h>
#include <libasr/pass/global_init.h>
#include <lfortran/utils.h>

namespace LCompilers::LLanguageServer {

    inline bool is_id_chr(unsigned char c) {
        return std::isalnum(c) || (c == '_');
    }

    // Whether `s` is a symbol the compiler declared, which is not shown to
    // the user.
    inline bool is_hidden_symbol(LCompilers::ASR::symbol_t *s) {
        return LCompilers::LFortran::is_generated_symbol_name(
                LCompilers::ASRUtils::symbol_name(s))
            || LCompilers::ASRUtils::is_global_init_symbol(s);
    }

    class LFortranAccessor {
    public:
        auto showErrors(
            const std::string &filename,
            const std::string &text,
            CompilerOptions &compiler_options
        ) -> std::vector<LCompilers::error_highlight>;

        auto lookupName(
            const std::string &filename,
            const std::string &text,
            CompilerOptions &compiler_options
        ) -> std::vector<LCompilers::document_symbols>;

        auto previewSymbol(
            const std::string &filename,
            const std::string &text,
            CompilerOptions &compiler_options
        ) -> std::vector<std::pair<LCompilers::document_symbols, std::string>>;

        auto getAllOccurrences(
            const std::string &filename,
            const std::string &text,
            CompilerOptions &compiler_options
        ) -> std::vector<LCompilers::document_symbols>;

        auto getSymbols(
            const std::string &filename,
            const std::string &text,
            CompilerOptions &compiler_options
        ) -> std::vector<LCompilers::document_symbols>;

        template <typename T>
        auto populateSymbolLists(
            T* x,
            LCompilers::LocationManager &lm,
            std::vector<LCompilers::document_symbols> &symbol_lists,
            int parent_index
        ) -> void {
            for (auto &a : x->m_symtab->get_scope()) {
                if (is_hidden_symbol(a.second)) {
                    continue;
                }
                std::size_t index = symbol_lists.size();
                LCompilers::document_symbols &loc = symbol_lists.emplace_back();
                loc.parent_index = parent_index;
                loc.symbol_name = a.first;
                loc.symbol_type = a.second->type;
                lm.pos_to_linecol(
                    a.second->base.loc.first,
                    loc.first_line,
                    loc.first_column,
                    loc.filename
                );
                lm.pos_to_linecol(
                    a.second->base.loc.last,
                    loc.last_line,
                    loc.last_column,
                    loc.filename
                );
                if ( LCompilers::ASR::is_a<LCompilers::ASR::Module_t>(*a.second) ) {
                    LCompilers::ASR::Module_t *m = LCompilers::ASR::down_cast<LCompilers::ASR::Module_t>(a.second);
                    populateSymbolLists(m, lm, symbol_lists, index);
                } else if ( LCompilers::ASR::is_a<LCompilers::ASR::Function_t>(*a.second) ) {
                    LCompilers::ASR::Function_t *f = LCompilers::ASR::down_cast<LCompilers::ASR::Function_t>(a.second);
                    populateSymbolLists(f, lm, symbol_lists, index);
                } else if ( LCompilers::ASR::is_a<LCompilers::ASR::Program_t>(*a.second) ) {
                    LCompilers::ASR::Program_t *p = LCompilers::ASR::down_cast<LCompilers::ASR::Program_t>(a.second);
                    populateSymbolLists(p, lm, symbol_lists, index);
                }
            }
        }

        auto format(
            const std::string &filename,
            const std::string &text,
            CompilerOptions &compiler_options,
            bool color,
            int indent,
            bool indent_unit
        ) -> LCompilers::Result<std::string>;
    private:
        std::mutex mutex;
    };

} // namespace LCompilers::LLanguageServer

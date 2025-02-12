#pragma once

#include <cctype>
#include <cstdint>
#include <string>
#include <vector>

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

#include <lfortran/fortran_evaluator.h>

namespace LCompilers {

  enum LFortranJSONType {
    kArrayType, kObjectType
  };

  class LFortranJSON {
  private:
    LFortranJSONType type;
    std::string json_value;
    std::vector<std::pair<std::string, std::string>> object_members;
    std::vector<std::string> array_values;
    bool rebuild_needed;

  public:
    LFortranJSON(LFortranJSONType type);
    void SetObject();
    void SetArray();
    void AddMember(std::string key, int v);
    void AddMember(std::string key, uint32_t v);
    void AddMember(std::string key, std::string v);
    void AddMember(std::string key, LFortranJSON v);
    void PushBack(LFortranJSON v);
    std::string GetValue();

  private:
    void RebuildJSON();
  };

  template <typename T>
  void populate_symbol_lists(
    T* x,
    LCompilers::LocationManager lm,
    std::vector<LCompilers::document_symbols> &symbol_lists
  ) {
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
      loc.first_line = first_line;
      loc.last_line = last_line;
      loc.symbol_name = symbol_name;
      loc.filename = filename;
      loc.symbol_type = a.second->type;
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

  int get_symbols(const std::string &infile, CompilerOptions &compiler_options);

  int get_errors(const std::string &infile, CompilerOptions &compiler_options);

  inline bool is_id_chr(unsigned char c)
  {
    return std::isalnum(c) || (c == '_');
  }

  int get_definitions(const std::string &infile, LCompilers::CompilerOptions &compiler_options);

  int get_all_occurences(const std::string &infile, LCompilers::CompilerOptions &compiler_options);

} // namespace LCompilers


#pragma once

#include <cctype>
#include <cstdint>
#include <string>
#include <utility>
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

    int get_symbols(const std::string &infile, CompilerOptions &compiler_options);

    int get_errors(const std::string &infile, CompilerOptions &compiler_options);

    inline bool is_id_chr(unsigned char c) {
        return std::isalnum(c) || (c == '_');
    }

    int get_definitions(const std::string &infile, LCompilers::CompilerOptions &compiler_options);

    int get_all_occurences(const std::string &infile, LCompilers::CompilerOptions &compiler_options);

} // namespace LCompilers


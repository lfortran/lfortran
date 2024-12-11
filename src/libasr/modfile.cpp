#include <string>
#include <libasr/config.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/modfile.h>
#include <libasr/serialization.h>
#include <libasr/bwriter.h>
#include <iostream>

namespace LCompilers {

const std::string lfortran_modfile_type_string = "LCompilers Modfile";

inline void save_asr(const ASR::TranslationUnit_t &m, std::string& asr_string) {
    #ifdef WITH_LFORTRAN_BINARY_MODFILES
    BinaryWriter b;
    #else
    TextWriter b;
    #endif
    b.write_string(lfortran_modfile_type_string);
    b.write_string(LFORTRAN_VERSION);
    b.write_string(serialize(m));
    asr_string = b.get_str();
}

std::string save_modfile(const ASR::TranslationUnit_t &m) {
    if (m.m_symtab->get_scope().size() != 1) {
        std::cerr << "Error: Modfile scope size is not 1" << std::endl;
        std::exit(EXIT_FAILURE);
    }
    for (auto &a : m.m_symtab->get_scope()) {
        if (!ASR::is_a<ASR::Module_t>(*a.second)) {
            std::cerr << "Error: Expected ASR::Module_t" << std::endl;
            std::cerr << "Modfile name: " << a.first << ", Modfile path: <Path to modfile>" << std::endl;
            std::exit(EXIT_FAILURE);
        }
    }

    std::string asr_string;
    save_asr(m, asr_string);
    return asr_string;
}

std::string save_pycfile(const ASR::TranslationUnit_t &m) {
    std::string asr_string;
    save_asr(m, asr_string);
    return asr_string;
}

inline void load_serialised_asr(const std::string &s, std::string& asr_binary) {
    #ifdef WITH_LFORTRAN_BINARY_MODFILES
    BinaryReader b(s);
    #else
    TextReader b(s);
    #endif
    std::string file_type = b.read_string();
    if (file_type != lfortran_modfile_type_string) {
        throw LCompilersException("LCompilers Modfile format not recognized");
    }
    std::string version = b.read_string();
    if (version != LFORTRAN_VERSION) {
        throw LCompilersException("Incompatible format: LFortran Modfile was generated using version '" + version + "', but current LFortran version is '" + LFORTRAN_VERSION + "'");
    }
    asr_binary = b.read_string();
}

ASR::TranslationUnit_t* load_modfile(Allocator &al, const std::string &s, bool load_symtab_id, SymbolTable &symtab) {
    std::string asr_binary;
    load_serialised_asr(s, asr_binary);
    ASR::asr_t *asr = deserialize_asr(al, asr_binary, load_symtab_id, symtab);
    ASR::TranslationUnit_t *tu = ASR::down_cast2<ASR::TranslationUnit_t>(asr);
    return tu;
}

ASR::TranslationUnit_t* load_pycfile(Allocator &al, const std::string &s, bool load_symtab_id) {
    std::string asr_binary;
    load_serialised_asr(s, asr_binary);
    ASR::asr_t *asr = deserialize_asr(al, asr_binary, load_symtab_id);
    ASR::TranslationUnit_t *tu = ASR::down_cast2<ASR::TranslationUnit_t>(asr);
    return tu;
}

} // namespace LCompilers

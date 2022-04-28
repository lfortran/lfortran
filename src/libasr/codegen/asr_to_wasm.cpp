#include <iostream>
#include <memory>
#include <chrono>
#include <iomanip>
#include <fstream>

#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/codegen/asr_to_wasm.h>
#include <libasr/codegen/wasm_assembler.h>
#include <libasr/pass/do_loops.h>
#include <libasr/pass/global_stmts.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>


namespace LFortran {

namespace {

    // Local exception that is only used in this file to exit the visitor
    // pattern and caught later (not propagated outside)
    class CodeGenError
    {
    public:
        diag::Diagnostic d;
    public:
        CodeGenError(const std::string &msg)
            : d{diag::Diagnostic(msg, diag::Level::Error, diag::Stage::CodeGen)}
        { }
    };

}

class ASRToWASMVisitor : public ASR::BaseVisitor<ASRToWASMVisitor>
{
public:
    Allocator &m_al;

    Vec<uint8_t> m_preamble;
    Vec<uint8_t> m_type_section;
    Vec<uint8_t> m_func_section;
    Vec<uint8_t> m_export_section;
    Vec<uint8_t> m_code_section;


    std::map<std::string, int32_t> m_var_name_idx_map;
    std::map<std::string, int32_t> m_func_name_idx_map;
public:

    ASRToWASMVisitor(Allocator &al) : m_al{al} {}

};


Result<int> asr_to_wasm(ASR::TranslationUnit_t &asr, Allocator &al,
        const std::string &filename, bool time_report)
{
    int time_pass_global=0;
    int time_pass_do_loops=0;
    int time_visit_asr=0;
    int time_verify=0;
    int time_save=0;

    ASRToWASMVisitor v(al);

    {
        auto t1 = std::chrono::high_resolution_clock::now();
        pass_wrap_global_stmts_into_function(al, asr, "f");
        auto t2 = std::chrono::high_resolution_clock::now();
        time_pass_global = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();
    }

    {
        auto t1 = std::chrono::high_resolution_clock::now();
        pass_replace_do_loops(al, asr);
        auto t2 = std::chrono::high_resolution_clock::now();
        time_pass_do_loops = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();
    }

    {
        auto t1 = std::chrono::high_resolution_clock::now();
        try {
            v.visit_asr((ASR::asr_t &)asr);
        } catch (const CodeGenError &e) {
            Error error;
            return error;
        }
        auto t2 = std::chrono::high_resolution_clock::now();
        time_visit_asr = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();
    }

    // {
    //     auto t1 = std::chrono::high_resolution_clock::now();
    //     v.m_a.verify();
    //     auto t2 = std::chrono::high_resolution_clock::now();
    //     time_verify = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();
    // }

    {
        auto t1 = std::chrono::high_resolution_clock::now();
        // v.m_a.save_binary(filename);
        FILE* fp = fopen(filename.c_str(), "wb");
        fwrite(v.m_preamble.data(), sizeof(uint8_t), v.m_preamble.size(), fp);
        fwrite(v.m_type_section.data(), sizeof(uint8_t), v.m_type_section.size(), fp);
        fwrite(v.m_func_section.data(), sizeof(uint8_t), v.m_func_section.size(), fp);
        fwrite(v.m_export_section.data(), sizeof(uint8_t), v.m_export_section.size(), fp);
        fwrite(v.m_code_section.data(), sizeof(uint8_t), v.m_code_section.size(), fp);
        auto t2 = std::chrono::high_resolution_clock::now();
        time_save = std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count();
    }

    if (time_report) {
        std::cout << "Codegen Time report:" << std::endl;
        std::cout << "Global:     " << std::setw(5) << time_pass_global << std::endl;
        std::cout << "Do loops:   " << std::setw(5) << time_pass_do_loops << std::endl;
        std::cout << "ASR -> wasm: " << std::setw(5) << time_visit_asr << std::endl;
        std::cout << "Verify:     " << std::setw(5) << time_verify << std::endl;
        std::cout << "Save:       " << std::setw(5) << time_save << std::endl;
        int total = time_pass_global + time_pass_do_loops + time_visit_asr + time_verify + time_verify + time_save;
        std::cout << "Total:      " << std::setw(5) << total << std::endl;
    }
    return 0;
}

} // namespace LFortran

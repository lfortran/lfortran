#include <iostream>
#include <memory>
#include <chrono>

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
public:

    ASRToWASMVisitor(Allocator &al) : m_al{al} {}

};


Result<int> asr_to_wasm(ASR::TranslationUnit_t &asr, Allocator &al,
        const std::string &filename, bool time_report)
{
    return 0;
}

} // namespace LFortran

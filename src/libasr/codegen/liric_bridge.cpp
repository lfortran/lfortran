// Liric backend bridge.
// Compiled with -DLCompilers=LCompilers_Liric and liric compat headers.
// Types from the main binary (LCompilers::*) are passed as void* and
// reinterpret_cast to the liric-namespace equivalents (LCompilers_Liric::*).
// This is safe because the types are layout-identical (same headers, same ABI).

#include <libasr/codegen/asr_to_llvm.h>
#include <libasr/codegen/evaluator.h>
#include <libasr/alloc.h>
#include <llvm/IR/Module.h>

#include <string>
#include <vector>
#include <iostream>

#define LIRIC_EXPORT __attribute__((visibility("default")))

extern "C" {

LIRIC_EXPORT int liric_compile_to_object(
    void *asr_ptr, void *diag_ptr, void *al_ptr, void *lpm_ptr,
    void *co_ptr, const char *infile, void *lm_ptr, const char *outfile)
{
    // reinterpret_cast: types are ABI-identical between LCompilers and
    // LCompilers_Liric namespaces (same headers, only namespace differs).
    auto &asr = *reinterpret_cast<LCompilers::ASR::TranslationUnit_t*>(asr_ptr);
    auto &diag = *reinterpret_cast<LCompilers::diag::Diagnostics*>(diag_ptr);
    auto &al = *reinterpret_cast<Allocator*>(al_ptr);
    auto &lpm = *reinterpret_cast<LCompilers::PassManager*>(lpm_ptr);
    auto &co = *reinterpret_cast<LCompilers::CompilerOptions*>(co_ptr);
    auto &lm = *reinterpret_cast<LCompilers::LocationManager*>(lm_ptr);

    LCompilers::LLVMEvaluator e(co.target);

    if (!LCompilers::ASRUtils::main_program_present(asr)
        && !LCompilers::ASRUtils::global_function_present(asr)
        && !co.separate_compilation
        && !co.generate_code_for_global_procedures) {
        e.create_empty_object_file(outfile);
        liric_llvm::Module::writeEmptyObjectCompanionFiles(outfile);
        return 0;
    }

    auto res = LCompilers::asr_to_llvm(asr, diag, e.get_context(),
        al, lpm, co, "", "", infile ? infile : "", lm);
    if (!res.ok) return 1;

    try {
        e.save_object_file(*(res.result->m_m), outfile);
        res.result->m_m->emitObjectCompanionFiles(outfile);
    } catch (const std::exception &ex) {
        std::cerr << "liric object emission failed: " << ex.what() << std::endl;
        return 1;
    }
    return 0;
}

LIRIC_EXPORT int liric_link_executable(const char *const *object_files, int nobjects,
    const char *outfile)
{
    std::vector<std::string> objects;
    objects.reserve(nobjects);
    for (int i = 0; i < nobjects; i++) {
        objects.push_back(object_files[i]);
    }
    try {
        liric_llvm::Module::emitExecutableFromObjects(objects, outfile);
    } catch (const std::exception &ex) {
        std::cerr << "liric link failed: " << ex.what() << std::endl;
        return 1;
    }
    return 0;
}

LIRIC_EXPORT const char *liric_get_version()
{
    static std::string v = LCompilers::LLVMEvaluator::llvm_version();
    return v.c_str();
}

LIRIC_EXPORT const char *liric_get_default_target()
{
    static std::string t = LCompilers::LLVMEvaluator::get_default_target_triple();
    return t.c_str();
}

} // extern "C"

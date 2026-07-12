#ifndef LFORTRAN_EVALUATOR_H
#define LFORTRAN_EVALUATOR_H

#include <complex>
#include <iostream>
#include <memory>

#include <libasr/alloc.h>
#include <libasr/asr_scopes.h>
#include <libasr/asr.h>
#include <libasr/utils.h>

// Forward declare all needed LLVM classes without importing any LLVM header
// files. Those are only imported in evaluator.cpp and nowhere else, to speed
// up compilation.
namespace llvm {
    class ExecutionEngine;
    class LLVMContext;
    class Module;
    class Function;
    class GlobalVariable;
    class TargetMachine;
    class DataLayout;
    namespace orc {
        class KaleidoscopeJIT;
    }
}

namespace mlir {
    class MLIRContext;
    class ModuleOp;
}

namespace LCompilers {

class LLVMModule
{
public:
    std::unique_ptr<llvm::Module> m_m;
    LLVMModule(std::unique_ptr<llvm::Module> m);
    ~LLVMModule();
    std::string str();
    // Return a function return type as a string (real / integer)
    std::string get_return_type(const std::string &fn_name);
    llvm::Function *get_function(const std::string &fn_name);
    llvm::GlobalVariable *get_global(const std::string &global_name);
};

class MLIRModule {
public:
    std::unique_ptr<mlir::ModuleOp> mlir_m;
    std::unique_ptr<mlir::MLIRContext> mlir_ctx;
    std::unique_ptr<llvm::Module> llvm_m;
    std::unique_ptr<llvm::LLVMContext> llvm_ctx;
    MLIRModule(std::unique_ptr<mlir::ModuleOp> m,
        std::unique_ptr<mlir::MLIRContext> ctx);
    ~MLIRModule();
    std::string mlir_str();
    std::string llvm_str();
    void mlir_to_llvm(llvm::LLVMContext &ctx);
};

class LLVMEvaluator
{
private:
    std::unique_ptr<llvm::orc::KaleidoscopeJIT> jit;
    std::unique_ptr<llvm::LLVMContext> context;
    std::string target_triple;
    llvm::TargetMachine *TM;
public:
    LLVMEvaluator(const std::string &t = "");
    ~LLVMEvaluator();
    std::unique_ptr<llvm::Module> parse_module(const std::string &source, const std::string &filename);
    std::unique_ptr<LLVMModule> parse_module2(const std::string &source, const std::string &filename);
    void add_module(const std::string &source);
    void add_module(std::unique_ptr<llvm::Module> mod);
    void add_module(std::unique_ptr<LLVMModule> m);
    intptr_t get_symbol_address(const std::string &name);
    std::string get_asm(llvm::Module &m);
    void save_asm_file(llvm::Module &m, const std::string &filename);
    void save_object_file(llvm::Module &m, const std::string &filename);
    void create_empty_object_file(const std::string &filename);
    void opt(llvm::Module &m);
    static std::string module_to_string(llvm::Module &m);
    static void print_version_message();
    static std::string llvm_version();
    llvm::LLVMContext &get_context();
    const llvm::DataLayout &get_jit_data_layout();
    static void print_targets();
    static std::string get_default_target_triple();

    template<class T>
    T execfn(const std::string &name) {
        intptr_t addr = get_symbol_address(name);
        T (*f)() = (T (*)())addr;
        return f();
    }
};


#ifdef __EMSCRIPTEN__

// WASM executor: replaces ORC JIT for incremental Fortran evaluation in the
// browser. Each cell is compiled to a wasm32 .o, linked into a side module via
// wasm-ld (lld), then loaded with dlopen so its symbols become globally visible.
class WasmLFortranExecutor
{
public:
    WasmLFortranExecutor();
    ~WasmLFortranExecutor();

    void add_module(std::unique_ptr<LLVMModule> m, int eval_count);
    std::unique_ptr<LLVMModule> parse_module2(const std::string &source, const std::string &filename);
    intptr_t get_symbol_address(const std::string &name);
    llvm::LLVMContext &get_context();

    template<class T>
    T execfn(const std::string &name) {
        intptr_t addr = get_symbol_address(name);
        T (*f)() = (T (*)())addr;
        return f();
    }

private:
    std::unique_ptr<llvm::LLVMContext> context;
    std::string TempDir;
    // Each executor instance gets a unique ID so that __lfortran_evaluate_N
    // function names are globally unique across all loaded side modules.
    // This allows dlsym(RTLD_DEFAULT) to always find the right symbol without
    // needing to track per-module dlopen handles.
    int m_id;
};

#endif // __EMSCRIPTEN__

} // namespace LCompilers

#endif // LFORTRAN_EVALUATOR_H

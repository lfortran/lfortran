#ifndef LFORTRAN_EVALUATOR_H
#define LFORTRAN_EVALUATOR_H

#include <complex>
#include <iostream>
#include <map>
#include <memory>
#include <set>

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

// When `MACOSX_DEPLOYMENT_TARGET` is set, a macOS triple names that release
// instead of the host OS. Object files and the link then record one minimum,
// which is what Apple's clang does. Other triples, and an unset variable,
// are returned unchanged.
std::string apply_macos_deployment_target(const std::string &triple);

// The macOS version a triple records, such as "15.0.0". Empty when `triple`
// is not a macOS or Darwin triple.
std::string macos_platform_version(const std::string &triple);

struct LLVMTargetConfig {
    std::string triple;
    std::string cpu = "generic";
    std::string tune_cpu;
    std::string features;
    std::string data_layout;
    bool emit_cpu_attribute = false;
    bool host_target = false;
    bool fast = false;
    // `--fast` without `--no-fast-math`: the backend may fuse a multiply and
    // an add into an FMA, like `-ffp-contract=fast` in clang and flang.
    bool fast_math = false;

    void apply_target_attributes(llvm::Module &module) const;
};

LLVMTargetConfig resolve_llvm_target_config(
    const CompilerOptions &compiler_options);

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
    LLVMTargetConfig target_config;
    std::unique_ptr<llvm::TargetMachine> TM;
    // What each module added to the JIT refers to and defines, so that what
    // running code would link can be checked without linking anything (see
    // undefined_symbols()).
    struct JITModule {
        std::vector<std::string> refs;
        // Everything it refers to, directly or through other modules, is
        // known to be defined.
        bool linkable;
    };
    std::vector<JITModule> jit_modules;
    // The module (index into `jit_modules`) that defines each symbol.
    std::map<std::string, size_t> jit_symbols;
    std::set<std::string> process_symbols;
    LLVMEvaluator(LLVMTargetConfig target_config);
    void configure_module(llvm::Module &module) const;
    bool is_process_symbol(const std::string &name);
public:
    LLVMEvaluator(const std::string &t = "");
    LLVMEvaluator(const CompilerOptions &compiler_options);
    ~LLVMEvaluator();
    std::unique_ptr<llvm::Module> parse_module(const std::string &source, const std::string &filename);
    std::unique_ptr<LLVMModule> parse_module2(const std::string &source, const std::string &filename);
    void add_module(const std::string &source);
    void add_module(std::unique_ptr<llvm::Module> mod);
    void add_module(std::unique_ptr<LLVMModule> m);
    // The symbols that running code of `m` would need and that are defined
    // nowhere, neither by a module added earlier nor by the host process. The
    // JIT links a module as a whole the first time a symbol of it is looked
    // up, and with it every module it refers to, so these include what such
    // a module refers to. If linking fails, the symbols involved stay failed
    // for the rest of the session, so nothing is looked up in the modules
    // here, and `m` is not added.
    std::vector<std::string> undefined_symbols(LLVMModule &m);
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
    const LLVMTargetConfig &get_target_config() const {
        return target_config;
    }
    const llvm::TargetMachine &get_target_machine() const {
        return *TM;
    }
    static void print_targets();
    static std::string get_default_target_triple();

    template<class T, class... Args>
    T execfn(const std::string &name, Args... args) {
        intptr_t addr = get_symbol_address(name);
        T (*f)(Args...) = (T (*)(Args...))addr;
        return f(args...);
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

    template<class T, class... Args>
    T execfn(const std::string &name, Args... args) {
        intptr_t addr = get_symbol_address(name);
        T (*f)(Args...) = (T (*)(Args...))addr;
        return f(args...);
    }

private:
    std::unique_ptr<llvm::LLVMContext> context;
    std::string TempDir;
    // Definitions belong to this executor; imports of them from later modules
    // use the same names. Runtime imports remain in the process namespace.
    std::map<std::string, std::string> symbols;
    int m_id;
};

#endif // __EMSCRIPTEN__

} // namespace LCompilers

#endif // LFORTRAN_EVALUATOR_H

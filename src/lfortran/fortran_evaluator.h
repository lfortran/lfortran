#ifndef LFORTRAN_FORTRAN_EVALUATOR_H
#define LFORTRAN_FORTRAN_EVALUATOR_H

#include <memory>
#include <set>

#include <libasr/alloc.h>
#include <lfortran/parser/parser.h>
#include <libasr/asr_scopes.h>
#include <lfortran/ast.h>
#include <libasr/asr.h>
#include <lfortran/utils.h>
#include <libasr/config.h>
#include <libasr/diagnostics.h>
#include <libasr/pass/pass_manager.h>
#include <libasr/utils.h>

namespace LCompilers {

class LLVMModule;
class MLIRModule;
class LLVMEvaluator;
#ifdef __EMSCRIPTEN__
class WasmLFortranExecutor;
#endif

/*
   FortranEvaluator is the main class to access the Fortran compiler.

   This class is used for both interactive (.evaluate()) and non-interactive
   (.get_llvm2()) compilation. The methods return diagnostic messages (errors,
   warnings, style suggestions, ...) as an argument. One can use
   Diagnostic::render to render them.

   One can use get_asr2() to obtain the ASR and then hand it over to other
   backends by hand.
*/
class FortranEvaluator
{
public:
    CompilerOptions& compiler_options;

    FortranEvaluator(CompilerOptions& compiler_options);
    ~FortranEvaluator();

    struct EvalResult {
        enum {
            integer4, integer8, real4, real8, complex4, complex8, boolean,
            character, statement, none
        } type;
        union {
            bool b;
            int32_t i32;
            int64_t i64;
            float f32;
            double f64;
            struct {float re, im;} c32;
            struct {double re, im;} c64;
        };
        std::string str;
        std::string ast;
        std::string asr;
        std::string llvm_ir;
    };

    // Evaluates `code`.
    // If `verbose=true`, it saves ast, asr and llvm_ir in Result.
    Result<EvalResult> evaluate(const std::string &code, bool verbose,
        LocationManager &lm, LCompilers::PassManager& pass_manager,
        diag::Diagnostics &diagnostics);
    Result<EvalResult> evaluate2(const std::string &code);

    Result<std::string> get_ast(const std::string &code,
        LocationManager &lm, diag::Diagnostics &diagnostics);
    Result<LCompilers::LFortran::AST::TranslationUnit_t*> get_ast2(
        const std::string &code, LocationManager &lm,
        diag::Diagnostics &diagnostics);
    Result<std::string> get_asr(const std::string &code,
        LocationManager &lm, diag::Diagnostics &diagnostics);
    ASR::asr_t* handle_lookup_name(LCompilers::ASR::TranslationUnit_t* tu, uint64_t pos);
    Result<ASR::TranslationUnit_t*> get_asr2(const std::string &code,
        LocationManager &lm, diag::Diagnostics &diagnostics);
    Result<ASR::TranslationUnit_t*> get_asr3(
        LCompilers::LFortran::AST::TranslationUnit_t &ast,
        diag::Diagnostics &diagnostics, LCompilers::LocationManager &lm);
    // Copy of a cell's symbols, taken before the ASR passes rewrite them, to
    // serve as the parent scope of the next cell.
    SymbolTable* snapshot_cell_scope(ASR::TranslationUnit_t &asr);
    SymbolTable* copy_cell_scope(SymbolTable *scope, SymbolTable *parent,
        const Location &loc);
    SymbolTable* copy_cell_chain(SymbolTable *chain, const Location &loc);
#ifdef HAVE_LFORTRAN_LLVM
    // Turn definitions the JIT already holds into declarations.
    void drop_redefinitions(LLVMModule &m);
#endif
    Result<std::string> get_llvm(const std::string &code,
        LocationManager &lm, LCompilers::PassManager& pass_manager,
        diag::Diagnostics &diagnostics);
    Result<std::unique_ptr<LLVMModule>> get_llvm2(const std::string &code,
        LocationManager &lm, LCompilers::PassManager& pass_manager,
        diag::Diagnostics &diagnostics);
    Result<std::unique_ptr<LLVMModule>> get_llvm3(ASR::TranslationUnit_t &asr,
        LCompilers::PassManager& pass_manager,
        diag::Diagnostics &diagnostics, LocationManager &lm, const std::string &infile,
        int* time_opt);
    Result<std::string> get_asm(const std::string &code,
        LocationManager &lm,
        LCompilers::PassManager& pass_manager,
        diag::Diagnostics &diagnostics);
    Result<Vec<uint8_t>> get_wasm(const std::string &code, LocationManager &lm,
        diag::Diagnostics &diagnostics);
    Result<std::string> get_wat(const std::string &code, LocationManager &lm,
        diag::Diagnostics &diagnostics);
    Result<std::string> get_cpp(const std::string &code, LocationManager &lm,
        diag::Diagnostics &diagnostics, int64_t default_lower_bound);
    Result<std::string> get_cpp2(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, int64_t default_lower_bound);
    Result<std::string> get_c(const std::string &code, LocationManager &lm,
        diag::Diagnostics &diagnostics, int64_t default_lower_bound);
    Result<std::string> get_c2(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, int64_t default_lower_bound);
    Result<std::string> get_c3(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, LCompilers::PassManager& pass_manager,
        int64_t default_lower_bound);
    // GPU kernel source for the backend selected by --gpu, with no host-side
    // kernel-registration shim, so that external toolchains can consume it.
    Result<std::string> get_gpu_kernel_source(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, LCompilers::PassManager& pass_manager);
    Result<std::string> get_julia(const std::string &code,
        LocationManager &lm, diag::Diagnostics &diagnostics);
    Result<std::unique_ptr<MLIRModule>> get_mlir(
        ASR::asr_t &asr, diag::Diagnostics &diagnostics);
    Result<std::string> get_fortran(const std::string &code,
        LocationManager &lm, diag::Diagnostics &diagnostics,
        LCompilers::PassManager& pass_manager);
    Result<std::string> get_fmt(const std::string &code, LocationManager &lm,
        diag::Diagnostics &diagnostics);
    Allocator &get_al() { return al; };
#ifdef HAVE_LFORTRAN_LLVM
    LLVMEvaluator &get_llvm_evaluator();
#endif
#ifdef __EMSCRIPTEN__
    WasmLFortranExecutor &get_wasm_executor();
#endif

private:
    Allocator al;
#ifdef HAVE_LFORTRAN_LLVM
    std::unique_ptr<LLVMEvaluator> e;
    int eval_count;
    // Functions already defined by an earlier evaluation. Compiler-generated
    // helpers (intrinsic lowerings, procedure specialisations) are recreated
    // by the passes on every evaluation; redefining them would be rejected by
    // the JIT, so later modules only declare them. See drop_redefinitions().
    std::set<std::string> defined_symbols;
#endif
#ifdef __EMSCRIPTEN__
    std::unique_ptr<WasmLFortranExecutor> wasm_exec;
#endif
    SymbolTable *symbol_table;
    std::string run_fn;
    // One entry per cell evaluated so far, each covering a range of its own,
    // so that a location taken from an earlier cell still points at that
    // cell's text. Only used in interactive mode.
    std::vector<LocationManager::FileLocations> cell_files;
    std::vector<uint32_t> cell_ends;
    // Where the cell being compiled starts.
    uint32_t cell_start = 0;

    // Puts the chain of cells, this one last, into `lm` and returns the
    // position this cell starts at.
    uint32_t open_cell(const std::string &code, LocationManager &lm);
    // Moves this cell's intervals to where the cell starts and records it.
    void close_cell(const std::string &code, LocationManager &lm);
};

} // namespace LCompilers

#endif // LFORTRAN_FORTRAN_EVALUATOR_H

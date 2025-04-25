#include <fstream>

#include <lfortran/fortran_evaluator.h>
#include <libasr/codegen/asr_to_cpp.h>
#include <libasr/codegen/asr_to_c.h>
#include <libasr/codegen/asr_to_wasm.h>
#include <libasr/codegen/asr_to_julia.h>
#include <libasr/codegen/asr_to_fortran.h>
#include <libasr/codegen/wasm_to_wat.h>
#include <lfortran/ast_to_src.h>
#include <libasr/exception.h>
#include <lfortran/ast.h>
#include <libasr/asr.h>
#include <lfortran/semantics/ast_to_asr.h>
#include <lfortran/parser/parser.h>
#include <lfortran/parser/preprocessor.h>
#include <lfortran/pickle.h>
#include <libasr/pickle.h>
#include <libasr/utils.h>
#include <libasr/asr_lookup_name.h>


#ifdef HAVE_LFORTRAN_LLVM
#include <libasr/codegen/evaluator.h>
#include <libasr/codegen/asr_to_llvm.h>
#ifdef HAVE_LFORTRAN_MLIR
#include <libasr/codegen/asr_to_mlir.h>
#endif
#else
namespace LCompilers {
    class LLVMEvaluator {};
}
#endif

namespace LCompilers {


/* ------------------------------------------------------------------------- */
// FortranEvaluator

FortranEvaluator::FortranEvaluator(CompilerOptions& compiler_options)
    :
    compiler_options{compiler_options},
    al{1024*1024},
#ifdef HAVE_LFORTRAN_LLVM
    e{std::make_unique<LLVMEvaluator>()},
    eval_count{0},
#endif
    symbol_table{nullptr}
{
}

FortranEvaluator::~FortranEvaluator() = default;

Result<FortranEvaluator::EvalResult> FortranEvaluator::evaluate2(const std::string &code) {
    LocationManager lm;
    LCompilers::PassManager lpm;
    lpm.use_default_passes();
    {
        LocationManager::FileLocations fl;
        fl.in_filename = "input";
        std::ofstream out("input");
        out << code;
        lm.files.push_back(fl);
    }
    diag::Diagnostics diagnostics;
    return evaluate(code, false, lm, lpm, diagnostics);
}

Result<FortranEvaluator::EvalResult> FortranEvaluator::evaluate(
#ifdef HAVE_LFORTRAN_LLVM
            const std::string &code_orig, bool verbose, LocationManager &lm,
            LCompilers::PassManager& pass_manager, diag::Diagnostics &diagnostics
#else
            const std::string &/*code_orig*/, bool /*verbose*/,
                LocationManager &/*lm*/, LCompilers::PassManager& /*pass_manager*/,
                diag::Diagnostics &/*diagnostics*/
#endif
            )
{
#ifdef HAVE_LFORTRAN_LLVM
    EvalResult result;

    // Src -> AST
    Result<LFortran::AST::TranslationUnit_t*> res = get_ast2(
        code_orig, lm, diagnostics);
    LFortran::AST::TranslationUnit_t* ast;
    if (res.ok) {
        ast = res.result;
    } else {
        return res.error;
    }

    if (verbose) {
        result.ast = LFortran::pickle(*ast, true);
    }

    // AST -> ASR
    Result<ASR::TranslationUnit_t*> res2 = get_asr3(*ast, diagnostics, lm);
    ASR::TranslationUnit_t* asr;
    if (res2.ok) {
        asr = res2.result;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return res2.error;
    }

    if (verbose) {
        result.asr = pickle(*asr, true);
    }

    // ASR -> LLVM
    Result<std::unique_ptr<LLVMModule>> res3 = get_llvm3(*asr,
        pass_manager, diagnostics, lm.files.back().in_filename);
    std::unique_ptr<LCompilers::LLVMModule> m;
    if (res3.ok) {
        m = std::move(res3.result);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return res3.error;
    }

    if (verbose) {
        result.llvm_ir = m->str();
    }

    std::string return_type = m->get_return_type(run_fn);

    // LLVM -> Machine code -> Execution
    e->add_module(std::move(m));
    if (return_type == "integer4") {
        int32_t r = e->execfn<int32_t>(run_fn);
        result.type = EvalResult::integer4;
        result.i32 = r;
    } else if (return_type == "integer8") {
        int64_t r = e->execfn<int64_t>(run_fn);
        result.type = EvalResult::integer8;
        result.i64 = r;
    } else if (return_type == "real4") {
        float r = e->execfn<float>(run_fn);
        result.type = EvalResult::real4;
        result.f32 = r;
    } else if (return_type == "real8") {
        double r = e->execfn<double>(run_fn);
        result.type = EvalResult::real8;
        result.f64 = r;
    } else if (return_type == "complex4") {
        std::complex<float> r = e->execfn<std::complex<float>>(run_fn);
        result.type = EvalResult::complex4;
        result.c32.re = r.real();
        result.c32.im = r.imag();
    } else if (return_type == "complex8") {
        std::complex<double> r = e->execfn<std::complex<double>>(run_fn);
        result.type = EvalResult::complex8;
        result.c64.re = r.real();
        result.c64.im = r.imag();
    } else if (return_type == "logical") {
        bool r = e->execfn<bool>(run_fn);
        result.type = EvalResult::boolean;
        result.b = r;
    } else if (return_type == "void") {
        e->execfn<void>(run_fn);
        result.type = EvalResult::statement;
    } else if (return_type == "none") {
        result.type = EvalResult::none;
    } else {
        throw LCompilersException("FortranEvaluator::evaluate(): Return type not supported");
    }
    return result;
#else
    throw LCompilersException("LLVM is not enabled");
#endif
}

Result<std::string> FortranEvaluator::get_ast(const std::string &code,
    LocationManager &lm, diag::Diagnostics &diagnostics)
{
    Result<LFortran::AST::TranslationUnit_t*> ast = get_ast2(code, lm,
        diagnostics);
    if (ast.ok) {
        if (compiler_options.po.tree) {
            return LFortran::pickle_tree(*ast.result, compiler_options.use_colors);
        } else if (compiler_options.po.json || compiler_options.po.visualize) {
            return LFortran::pickle_json(*ast.result, lm, compiler_options.po.no_loc);
        }
        return LFortran::pickle(*ast.result, compiler_options.use_colors,
            compiler_options.indent);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return ast.error;
    }
}

Result<LFortran::AST::TranslationUnit_t*> FortranEvaluator::get_ast2(
            const std::string &code_orig, LocationManager &lm,
            diag::Diagnostics &diagnostics)
{
    // Src -> AST
    const std::string *code=&code_orig;
    std::string tmp;
    if (compiler_options.c_preprocessor) {
        // Preprocessor
        LFortran::CPreprocessor cpp(compiler_options);
        Result<std::string> res = cpp.run(code_orig, lm, cpp.macro_definitions, diagnostics);
        if (res.ok) {
            tmp = res.result;
        } else {
            LCOMPILERS_ASSERT(diagnostics.has_error())
            return res.error;
        }
        code = &tmp;
    }
    if (compiler_options.prescan || compiler_options.fixed_form) {
        std::vector<std::filesystem::path> include_dirs;
        include_dirs.push_back(parent_path(lm.files.back().in_filename));
        include_dirs.insert(include_dirs.end(),
                            compiler_options.po.include_dirs.begin(),
                            compiler_options.po.include_dirs.end());
        tmp = LFortran::prescan(*code, lm, compiler_options.fixed_form, include_dirs);
        code = &tmp;
    }
    Result<LFortran::AST::TranslationUnit_t*>
        res = LFortran::parse(al, *code, diagnostics, compiler_options);
    if (res.ok) {
        return res.result;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return res.error;
    }
}

Result<std::string> FortranEvaluator::get_asr(const std::string &code,
    LocationManager &lm, diag::Diagnostics &diagnostics)
{
    Result<ASR::TranslationUnit_t*> asr = get_asr2(code, lm, diagnostics);
    if (asr.ok) {
        if (compiler_options.po.tree) {
            return pickle_tree(*asr.result, compiler_options.use_colors);
        } else if (compiler_options.po.json) {
            return pickle_json(*asr.result, lm, compiler_options.po.no_loc, false);
        }
        return pickle(*asr.result,
            compiler_options.use_colors, compiler_options.indent);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return asr.error;
    }
}

LCompilers::ASR::asr_t* FortranEvaluator::handle_lookup_name(LCompilers::ASR::TranslationUnit_t* tu, uint64_t pos) {
    LCompilers::LFortran::LookupNameVisitor lnv(pos);
    lnv.visit_TranslationUnit(*tu);
    if (lnv.node_to_return != nullptr) {
        return lnv.node_to_return;
    } else {
        return ( LCompilers::ASR::asr_t*) tu;
    }
}

Result<ASR::TranslationUnit_t*> FortranEvaluator::get_asr2(
            const std::string &code_orig, LocationManager &lm,
            diag::Diagnostics &diagnostics)
{
    // Src -> AST
    Result<LFortran::AST::TranslationUnit_t*>
        res = get_ast2(code_orig, lm, diagnostics);
    LFortran::AST::TranslationUnit_t* ast;
    if (res.ok) {
        ast = res.result;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return res.error;
    }
    // AST -> ASR
    Result<ASR::TranslationUnit_t*> res2 = get_asr3(*ast, diagnostics, lm);
    if (res2.ok) {
        return res2.result;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return res2.error;
    }
}

Result<ASR::TranslationUnit_t*> FortranEvaluator::get_asr3(
    LFortran::AST::TranslationUnit_t &ast, diag::Diagnostics &diagnostics, LCompilers::LocationManager &lm)
{
    ASR::TranslationUnit_t* asr;
    // AST -> ASR
    // Remove the old execution function if it exists
    if (symbol_table) {
        if (symbol_table->get_symbol(run_fn) != nullptr) {
            symbol_table->erase_symbol(run_fn);
        }
        symbol_table->mark_all_variables_external(al);
    }
    auto res = LFortran::ast_to_asr(al, ast, diagnostics, symbol_table,
        compiler_options.symtab_only, compiler_options, lm);
    if (res.ok) {
        asr = res.result;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return res.error;
    }
    if (!symbol_table) symbol_table = asr->m_symtab;

    return asr;
}

Result<std::string> FortranEvaluator::get_llvm(
    const std::string &code, LocationManager &lm, LCompilers::PassManager& pass_manager,
    diag::Diagnostics &diagnostics
    )
{
    Result<std::unique_ptr<LLVMModule>> res = get_llvm2(code, lm, pass_manager, diagnostics);
    if (res.ok) {
#ifdef HAVE_LFORTRAN_LLVM
        return res.result->str();
#else
        throw LCompilersException("LLVM is not enabled");
#endif
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return res.error;
    }
}

Result<std::unique_ptr<LLVMModule>> FortranEvaluator::get_llvm2(
    const std::string &code, LocationManager &lm, LCompilers::PassManager& pass_manager,
    diag::Diagnostics &diagnostics)
{
    Result<ASR::TranslationUnit_t*> asr = get_asr2(code, lm, diagnostics);
    if (!asr.ok) {
        return asr.error;
    }
    Result<std::unique_ptr<LLVMModule>> res = get_llvm3(*asr.result, pass_manager,
        diagnostics, lm.files.back().in_filename);
    if (res.ok) {
#ifdef HAVE_LFORTRAN_LLVM
        std::unique_ptr<LLVMModule> m = std::move(res.result);
        return m;
#else
        throw LCompilersException("LLVM is not enabled");
#endif
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return res.error;
    }
}

Result<std::unique_ptr<LLVMModule>> FortranEvaluator::get_llvm3(
#ifdef HAVE_LFORTRAN_LLVM
    ASR::TranslationUnit_t &asr, LCompilers::PassManager& pass_manager,
    diag::Diagnostics &diagnostics
#else
    ASR::TranslationUnit_t &/*asr*/, LCompilers::PassManager &/*pass_manager*/,
    diag::Diagnostics &/*diagnostics*/
#endif
, [[maybe_unused]] const std::string &infile)
{
#ifdef HAVE_LFORTRAN_LLVM
    eval_count++;
    run_fn = "__lfortran_evaluate_" + std::to_string(eval_count);

    if (compiler_options.separate_compilation) {
        compiler_options.po.intrinsic_symbols_mangling = true;
    }

    if (compiler_options.emit_debug_info) {
        if (!compiler_options.emit_debug_line_column) {
            diagnostics.add(LCompilers::diag::Diagnostic(
                "The `emit_debug_line_column` is not enabled; please use the "
                "`--debug-with-line-column` option to get the correct "
                "location information",
                LCompilers::diag::Level::Error,
                LCompilers::diag::Stage::Semantic, {})
            );
            Error err;
            return err;
        }
    }
    // ASR -> LLVM
    std::unique_ptr<LCompilers::LLVMModule> m;
    Result<std::unique_ptr<LCompilers::LLVMModule>> res
        = asr_to_llvm(asr, diagnostics,
            e->get_context(), al, pass_manager,
            compiler_options, run_fn, "", infile);
    if (res.ok) {
        m = std::move(res.result);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return res.error;
    }

    if (compiler_options.po.fast) {
        auto t1 = std::chrono::high_resolution_clock::now();
        e->opt(*m->m_m);
        auto t2 = std::chrono::high_resolution_clock::now();
        auto time_opt = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count();
    }

    return m;
#else
    throw LCompilersException("LLVM is not enabled");
#endif
}

Result<std::string> FortranEvaluator::get_asm(
#ifdef HAVE_LFORTRAN_LLVM
    const std::string &code, LocationManager &lm,
    LCompilers::PassManager& lpm,
    diag::Diagnostics &diagnostics
#else
    const std::string &/*code*/,
    LocationManager &/*lm*/,
    LCompilers::PassManager&/*lpm*/,
    diag::Diagnostics &/*diagnostics*/
#endif
    )
{
#ifdef HAVE_LFORTRAN_LLVM
    Result<std::unique_ptr<LLVMModule>> res = get_llvm2(code, lm, lpm, diagnostics);
    if (res.ok) {
        return e->get_asm(*res.result->m_m);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return res.error;
    }
#else
    throw LCompilersException("LLVM is not enabled");
#endif
}

Result<Vec<uint8_t>> FortranEvaluator::get_wasm(const std::string &code,
    LocationManager &lm, diag::Diagnostics &diagnostics)
{
    // Src -> AST -> ASR -> WASM
    SymbolTable *old_symbol_table = symbol_table;
    symbol_table = nullptr;
    Result<ASR::TranslationUnit_t*> asr = get_asr2(code, lm, diagnostics);
    symbol_table = old_symbol_table;
    if (asr.ok) {
        return asr_to_wasm_bytes_stream(*asr.result, al, diagnostics, compiler_options);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return asr.error;
    }
}

Result<std::string> FortranEvaluator::get_wat(const std::string &code,
    LocationManager &lm, diag::Diagnostics &diagnostics)
{
    // Src -> AST -> ASR -> WASM -> WAT
    SymbolTable *old_symbol_table = symbol_table;
    symbol_table = nullptr;
    Result<Vec<uint8_t>> wasm = get_wasm(code, lm, diagnostics);
    symbol_table = old_symbol_table;
    if (wasm.ok) {
            return wasm_to_wat(wasm.result, al, diagnostics);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return wasm.error;
    }
}

Result<std::string> FortranEvaluator::get_cpp(const std::string &code,
    LocationManager &lm, diag::Diagnostics &diagnostics, int64_t default_lower_bound)
{
    // Src -> AST -> ASR
    SymbolTable *old_symbol_table = symbol_table;
    symbol_table = nullptr;
    Result<ASR::TranslationUnit_t*> asr = get_asr2(code, lm, diagnostics);
    symbol_table = old_symbol_table;
    if (asr.ok) {
        return get_cpp2(*asr.result, diagnostics, default_lower_bound);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return asr.error;
    }
}

Result<std::string> FortranEvaluator::get_cpp2(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, int64_t default_lower_bound)
{
    // ASR -> C++
    return asr_to_cpp(al, asr, diagnostics, compiler_options,
                      default_lower_bound);
}

Result<std::string> FortranEvaluator::get_c(const std::string &code,
    LocationManager &lm, diag::Diagnostics &diagnostics,
    int64_t default_lower_bound)
{
    // Src -> AST -> ASR
    SymbolTable *old_symbol_table = symbol_table;
    symbol_table = nullptr;
    Result<ASR::TranslationUnit_t*> asr = get_asr2(code, lm, diagnostics);
    symbol_table = old_symbol_table;
    if (asr.ok) {
        return get_c2(*asr.result, diagnostics, default_lower_bound);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return asr.error;
    }
}

Result<std::string> FortranEvaluator::get_c2(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, int64_t default_lower_bound)
{
    // ASR -> C
    return asr_to_c(al, asr, diagnostics, compiler_options,
                    default_lower_bound);
}

Result<std::string> FortranEvaluator::get_c3(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, LCompilers::PassManager& pass_manager, int64_t default_lower_bound)
{
    // ASR -> ASR pass
    Allocator al(64*1024*1024);
    compiler_options.po.always_run = false;
    compiler_options.po.run_fun = "f";
    pass_manager.skip_c_passes();
    pass_manager.apply_passes(al, &asr, compiler_options.po, diagnostics);
    // ASR pass -> C
    return asr_to_c(al, asr, diagnostics, compiler_options, default_lower_bound);
}

Result<std::string> FortranEvaluator::get_julia(const std::string &code,
    LocationManager &lm, diag::Diagnostics &diagnostics)
{
    // Src -> AST -> ASR -> Julia
    SymbolTable *old_symbol_table = symbol_table;
    symbol_table = nullptr;
    Result<ASR::TranslationUnit_t*> asr = get_asr2(code, lm, diagnostics);
    symbol_table = old_symbol_table;
    if (asr.ok) {
        return asr_to_julia(al, *asr.result, diagnostics);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return asr.error;
    }
}

// asr_t &asr accepts only TranslationUnit and Module's type for now
Result<std::unique_ptr<MLIRModule>> FortranEvaluator::get_mlir(
#ifdef HAVE_LFORTRAN_MLIR
        ASR::asr_t &asr, diag::Diagnostics &diagnostics
#else
        ASR::asr_t &/*asr*/, diag::Diagnostics &/*diagnostics*/
#endif
) {
#ifdef HAVE_LFORTRAN_MLIR
    // ASR -> MLIR
    std::unique_ptr<LCompilers::MLIRModule> m;
    LCompilers::PassManager pass_manager;
    if (ASR::is_a<ASR::unit_t>(asr)) {
        pass_manager.use_default_passes();
        pass_manager.apply_passes(al, (ASR::TranslationUnit_t *)&asr,
            compiler_options.po, diagnostics);
    }
    Result<std::unique_ptr<MLIRModule>> res = asr_to_mlir(al,
        (ASR::asr_t &)asr, diagnostics);
    if (res.ok) {
        m = std::move(res.result);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return res.error;
    }

    // MLIR -> LLVM
    m->mlir_to_llvm(*m->llvm_ctx);
    return m;
#else
    throw LCompilersException("MLIR is not enabled");
#endif
}

Result<std::string> FortranEvaluator::get_fortran(const std::string &code,
    LocationManager &lm, diag::Diagnostics &diagnostics)
{
    // SRC -> AST -> ASR -> Fortran
    SymbolTable *old_symbol_table = symbol_table;
    symbol_table = nullptr;
    Result<ASR::TranslationUnit_t*> asr = get_asr2(code, lm, diagnostics);
    symbol_table = old_symbol_table;
    if (asr.ok) {
        LCompilers::PassManager pass_manager;
        pass_manager.use_fortran_passes();
        pass_manager.apply_passes(al, asr.result, compiler_options.po, diagnostics);
        return asr_to_fortran(*asr.result, diagnostics, false, 4);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return asr.error;
    }
}

Result<std::string> FortranEvaluator::get_fmt(const std::string &code,
    LocationManager &lm, diag::Diagnostics &diagnostics)
{
    // Src -> AST
    Result<LFortran::AST::TranslationUnit_t*> ast = get_ast2(code, lm, diagnostics);
    if (ast.ok) {
        // AST -> Fortran
        return LFortran::ast_to_src(*ast.result, true);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return ast.error;
    }
}

} // namespace LCompilers

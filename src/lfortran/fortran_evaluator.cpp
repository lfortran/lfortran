#include <array>
#include <cstring>
#include <fstream>
#include <set>

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
#include <libasr/asr_utils.h>
#include <lfortran/semantics/ast_to_asr.h>
#include <lfortran/parser/parser.h>
#include <lfortran/parser/preprocessor.h>
#include <lfortran/pickle.h>
#include <libasr/pickle.h>
#include <libasr/utils.h>
#include <libasr/asr_lookup_name.h>
#include <libasr/serialization.h>


#ifdef HAVE_LFORTRAN_LLVM
#include <libasr/codegen/evaluator.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <libasr/codegen/asr_to_llvm.h>
#ifdef HAVE_LFORTRAN_MLIR
#include <libasr/codegen/asr_to_mlir.h>
#endif
#else
namespace LCompilers {
    class LLVMEvaluator {};
#ifdef __EMSCRIPTEN__
    class WasmLFortranExecutor {};
#endif
}
#endif

namespace LCompilers {

class StringDescriptor {
    std::array<unsigned char, sizeof(char *) + sizeof(int64_t)> storage{};

public:
    void *pointer() {
        return storage.data();
    }

    char *data() const {
        char *data;
        std::memcpy(&data, storage.data(), sizeof(data));
        return data;
    }

    int64_t length() const {
        int64_t length;
        std::memcpy(&length, storage.data() + sizeof(char *), sizeof(length));
        return length;
    }
};


/* ------------------------------------------------------------------------- */
// FortranEvaluator

FortranEvaluator::FortranEvaluator(CompilerOptions& compiler_options)
    :
    compiler_options{compiler_options},
    al{1024*1024},
#ifdef HAVE_LFORTRAN_LLVM
    e{nullptr},
    eval_count{0},
#endif
    symbol_table{nullptr}
{
}

FortranEvaluator::~FortranEvaluator() = default;

#ifdef HAVE_LFORTRAN_LLVM
LLVMEvaluator &FortranEvaluator::get_llvm_evaluator() {
    if (!e) {
        e = std::make_unique<LLVMEvaluator>(compiler_options.target);
    }
    return *e;
}

#ifdef __EMSCRIPTEN__
WasmLFortranExecutor &FortranEvaluator::get_wasm_executor() {
    if (!wasm_exec) {
        wasm_exec = std::make_unique<WasmLFortranExecutor>();
    }
    return *wasm_exec;
}
#endif
#endif

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
        result.asr = pickle(*asr, true, false, false, false);
    }

    bool character_result = asr->n_items > 0
        && asr->m_items[asr->n_items - 1]->type == ASR::asrType::expr
        && ASRUtils::is_character(*ASRUtils::expr_type(
            ASRUtils::EXPR(asr->m_items[asr->n_items - 1])));

    // ASR -> LLVM
    //
    // In interactive mode `asr` *is* the session state: its global symbol table
    // persists across evaluations. ASR passes rewrite symbols in place --
    // pass_array_by_data, for example, replaces a module procedure that takes
    // an assumed-shape array with a specialised one under a mangled name --
    // so running them on the session ASR corrupts it for later cells, which
    // then fail with "Function '<name>' not found".
    //
    // Compile a throwaway copy instead and leave the session ASR pristine.
    // Codegen still resolves to the code emitted by earlier cells: a procedure
    // from an earlier evaluation is marked ExternalUndefined (see
    // SymbolTable::mark_all_variables_external), the pass propagates that ABI
    // to the specialisation it derives, and generate_function() then emits a
    // declaration rather than a definition, which the JIT binds to the
    // definition the earlier cell compiled.
    //
    // Only interactive mode pays for the copy; batch compilation runs the
    // passes directly on its own ASR, as before.
    ASR::TranslationUnit_t* asr_to_compile = asr;
    if (compiler_options.interactive) {
        Result<ASR::TranslationUnit_t*> asr_copy = copy_asr(*asr, diagnostics);
        if (asr_copy.ok) {
            asr_to_compile = asr_copy.result;
        } else {
            LCOMPILERS_ASSERT(diagnostics.has_error())
            return asr_copy.error;
        }
    }

    Result<std::unique_ptr<LLVMModule>> res3 = get_llvm3(*asr_to_compile,
        pass_manager, diagnostics, lm, lm.files.back().in_filename,
        nullptr);
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
    if (character_result) {
        return_type = "character";
    }
    // A cell may contain a program unit. In interactive mode it is compiled
    // into `run_fn + "_program"` (see visit_Program) and has to be called from
    // here; otherwise the program is compiled and never runs, and the cell
    // silently produces no output at all.
    std::string program_fn = run_fn + "_program";
    bool has_program = (m->get_return_type(program_fn) == "void");

    // With full-width logical types, logicals are now i32/i64 in LLVM
    // (same as integers). Check the ASR to distinguish logical from integer.
    // This has to look at the tree that was compiled: `run_fn` does not exist
    // in the session ASR, it is created by the wrap-global-statements pass, so
    // in interactive mode it only ever appears in the copy.
    if (asr_to_compile->m_symtab->get_symbol(run_fn) != nullptr) {
        ASR::symbol_t *fn_sym = asr_to_compile->m_symtab->get_symbol(run_fn);
        if (ASR::is_a<ASR::Function_t>(*fn_sym)) {
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(fn_sym);
            if (fn->m_return_var) {
                ASR::ttype_t *ret_type = ASRUtils::expr_type(fn->m_return_var);
                if (ASRUtils::is_logical(*ret_type)) {
                    return_type = "logical";
                }
            }
        }
    }

    if (compiler_options.interactive) {
        drop_redefinitions(*m);
    }

    // LLVM -> Machine code -> Execution
#ifdef __EMSCRIPTEN__
    WasmLFortranExecutor &e = get_wasm_executor();
    e.add_module(std::move(m), eval_count);
#else
    LLVMEvaluator &e = get_llvm_evaluator();
    e.add_module(std::move(m));
#endif
    if (has_program) {
        e.execfn<void>(program_fn);
    }
    if (return_type == "integer4") {
        int32_t r = e.execfn<int32_t>(run_fn);
        result.type = EvalResult::integer4;
        result.i32 = r;
    } else if (return_type == "integer8") {
        int64_t r = e.execfn<int64_t>(run_fn);
        result.type = EvalResult::integer8;
        result.i64 = r;
    } else if (return_type == "real4") {
        float r = e.execfn<float>(run_fn);
        result.type = EvalResult::real4;
        result.f32 = r;
    } else if (return_type == "real8") {
        double r = e.execfn<double>(run_fn);
        result.type = EvalResult::real8;
        result.f64 = r;
    } else if (return_type == "complex4") {
        std::complex<float> r = e.execfn<std::complex<float>>(run_fn);
        result.type = EvalResult::complex4;
        result.c32.re = r.real();
        result.c32.im = r.imag();
    } else if (return_type == "complex8") {
        std::complex<double> r = e.execfn<std::complex<double>>(run_fn);
        result.type = EvalResult::complex8;
        result.c64.re = r.real();
        result.c64.im = r.imag();
    } else if (return_type == "logical") {
        int32_t r = e.execfn<int32_t>(run_fn);
        result.type = EvalResult::boolean;
        result.b = (r != 0);
    } else if (return_type == "character") {
        StringDescriptor descriptor;
        e.execfn<void>(run_fn, descriptor.pointer());
        result.type = EvalResult::character;
        char *data = descriptor.data();
        int64_t length = descriptor.length();
        if (data && length > 0) {
            result.str.assign(data, length);
        }
        if (data) {
            const std::string allocator_fn = compiler_options.detect_leaks
                ? "_lfortran_get_compiler_mem_dbg_allocator"
                : "_lfortran_get_default_allocator";
            void *allocator = e.execfn<void *>(allocator_fn);
            e.execfn<void>("_lfortran_free_alloc", allocator, data);
        }
    } else if (return_type == "void") {
        e.execfn<void>(run_fn);
        result.type = EvalResult::statement;
    } else if (return_type == "none") {
        result.type = has_program ? EvalResult::statement : EvalResult::none;
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
        const std::string *cpp_input = &code_orig;
        std::string cpp_input_with_newline;
        if (!code_orig.empty() && code_orig.back() != '\n') {
            cpp_input_with_newline = code_orig;
            cpp_input_with_newline.push_back('\n');
            cpp_input = &cpp_input_with_newline;
        }
        Result<std::string> res = cpp.run(*cpp_input, lm, cpp.macro_definitions, diagnostics);
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
        Result<std::string> prescan_res = LFortran::prescan(*code, lm,
            compiler_options.fixed_form, include_dirs, diagnostics);
        if (prescan_res.ok) {
            tmp = prescan_res.result;
        } else {
            LCOMPILERS_ASSERT(diagnostics.has_error())
            return prescan_res.error;
        }
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
            return pickle_tree(*asr.result, compiler_options.use_colors, false);
        } else if (compiler_options.po.json) {
            return pickle_json(*asr.result, lm, compiler_options.po.no_loc, false);
        }
        return pickle(*asr.result,
            compiler_options.use_colors, compiler_options.indent, false, false);
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


#ifdef HAVE_LFORTRAN_LLVM
void FortranEvaluator::drop_redefinitions(LLVMModule &m)
{
    // Each interactive evaluation compiles a fresh copy of the session ASR, so
    // the passes regenerate their helper procedures every time: intrinsic
    // lowerings such as __lcompilers_optimization_mod_i32, or the
    // pass_array_by_data specialisation of a procedure that lives in the
    // session. Adding a second definition of a symbol the JIT already holds is
    // an error, and the existing definition is equivalent, so keep the
    // declaration and drop the body.
    for (llvm::Function &fn : *m.m_m) {
        if (fn.isDeclaration()) continue;
        std::string name = fn.getName().str();
        if (!defined_symbols.insert(name).second) {
            fn.deleteBody();
        }
    }
}
#endif

Result<ASR::TranslationUnit_t*> FortranEvaluator::copy_asr(
    ASR::TranslationUnit_t &asr, diag::Diagnostics &diagnostics)
{
    // Round-trip through the ASR serializer, the same mechanism modfiles use.
    // It copies every node, so the passes run on memory the session never
    // looks at again.
    try {
        std::string binary = serialize(asr);
        // Fresh symbol table ids: reusing the originals' would re-register them
        // in the global symtab map, so lookups against the session ASR would
        // land in the copy instead.
        ASR::asr_t* copy = deserialize_asr(al, binary,
            /* load_symtab_id */ false, /* offset */ 0);
        ASR::TranslationUnit_t* tu = ASR::down_cast2<ASR::TranslationUnit_t>(copy);
        fix_external_symbols(*tu, *tu->m_symtab);
        return tu;
    } catch (const LCompilersException &e) {
        diagnostics.diagnostics.push_back(diag::Diagnostic(
            "Failed to copy the interactive ASR: " + e.msg(),
            diag::Level::Error, diag::Stage::ASRPass));
        return Error();
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
        // Remove program units left by earlier evaluations. A program cannot be
        // referenced from a later cell, and keeping it would make re-running a
        // cell that defines one fail with "symbol already declared", which is
        // the ordinary edit-and-run-again loop in a notebook.
        std::vector<std::string> old_programs;
        for (auto &item : symbol_table->get_scope()) {
            if (ASR::is_a<ASR::Program_t>(*item.second)) {
                old_programs.push_back(item.first);
            }
        }
        for (auto &name : old_programs) {
            symbol_table->erase_symbol(name);
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
        diagnostics, lm, lm.files.back().in_filename, nullptr);
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

/*
    time_opt: keeps track of time taken by using `--fast` flag
        i.e. time taken by optimizations, and used when
        `--time-report` flag is used
*/
Result<std::unique_ptr<LLVMModule>> FortranEvaluator::get_llvm3(
#ifdef HAVE_LFORTRAN_LLVM
    ASR::TranslationUnit_t &asr, LCompilers::PassManager& pass_manager,
    diag::Diagnostics &diagnostics, LocationManager& lm
#else
    ASR::TranslationUnit_t &/*asr*/, LCompilers::PassManager &/*pass_manager*/,
    diag::Diagnostics &/*diagnostics*/, LocationManager &/*lm*/
#endif
, [[maybe_unused]] const std::string &infile,
  [[maybe_unused]] int* time_opt=nullptr)
{
#ifdef HAVE_LFORTRAN_LLVM
    eval_count++;
    run_fn = "__lfortran_evaluate_" + std::to_string(eval_count);

    if (compiler_options.generate_code_for_global_procedures) {
        compiler_options.po.intrinsic_symbols_mangling = true;
    }

#ifdef __EMSCRIPTEN__
    llvm::LLVMContext &ctx = get_wasm_executor().get_context();
#else
    llvm::LLVMContext &ctx = get_llvm_evaluator().get_context();
#endif

    // ASR -> LLVM
    std::unique_ptr<LCompilers::LLVMModule> m;
    Result<std::unique_ptr<LCompilers::LLVMModule>> res
        = asr_to_llvm(asr, diagnostics,
            ctx, al, pass_manager,
            compiler_options, run_fn, "", infile, lm);
    if (res.ok) {
        m = std::move(res.result);
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return res.error;
    }

    if (compiler_options.po.fast) {
#ifndef __EMSCRIPTEN__
        auto t1 = std::chrono::high_resolution_clock::now();
        get_llvm_evaluator().opt(*m->m_m);
        auto t2 = std::chrono::high_resolution_clock::now();
        if (compiler_options.po.time_report && time_opt) {
            *time_opt = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count();
        }
#endif
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
        return get_llvm_evaluator().get_asm(*res.result->m_m);
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
    LocationManager &lm, diag::Diagnostics &diagnostics,
    LCompilers::PassManager& pass_manager)
{
    // SRC -> AST -> ASR -> Fortran
    SymbolTable *old_symbol_table = symbol_table;
    symbol_table = nullptr;
    Result<ASR::TranslationUnit_t*> asr = get_asr2(code, lm, diagnostics);
    symbol_table = old_symbol_table;
    if (asr.ok) {
        if (!pass_manager.has_user_defined_passes()) {
            pass_manager.use_fortran_passes();
        }
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

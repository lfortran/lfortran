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

// Each cell is compiled on its own but a later cell refers to the symbols of
// an earlier one, and a diagnostic about such a symbol has to quote the cell it
// came from. So the cells are laid out one after another in one address space:
// this cell parses at an offset past every cell before it, and `lm` carries all
// of them, each with its own text. Without that, a location from an earlier
// cell is read as a position in this one and quotes whatever text happens to
// be there.
uint32_t FortranEvaluator::open_cell(const std::string &code,
    LocationManager &lm)
{
    uint32_t start = cell_ends.empty() ? 0 : cell_ends.back();
    LocationManager::FileLocations fl;
    // A cell has no file of its own, so it is named after its place in the
    // session. Interactive mode is also how a file holding statements outside
    // any program unit is compiled; that has a name already, and it is the one
    // to report.
    fl.in_filename = lm.files.empty() ? "" : lm.files.back().in_filename;
    if (fl.in_filename.empty()) {
        fl.in_filename = "<cell " + std::to_string(cell_files.size() + 1) + ">";
    }
    fl.source = code;
    lm.files = cell_files;
    lm.file_ends = cell_ends;
    lm.files.push_back(fl);
    // Provisional, so that a diagnostic raised before the cell is closed still
    // resolves to this cell. close_cell() corrects it once the text the parser
    // is given is known.
    lm.file_ends.push_back(start + code.size());
    return start;
}

// The prescanner fills in the intervals of the file it is given, counting from
// zero. This cell starts further along, so they are moved up to where it is.
void FortranEvaluator::close_cell(const std::string &code, LocationManager &lm)
{
    if (lm.files.empty() || lm.file_ends.empty()) return;
    LocationManager::FileLocations &fl = lm.files.back();
    if (fl.out_start.empty()) {
        fl.out_start = {cell_start, cell_start + (uint32_t)code.size()};
        fl.in_start = {0, (uint32_t)code.size()};
        lm.get_newlines(fl.source, fl.in_newlines);
    } else {
        for (size_t i = 0; i < fl.out_start.size(); i++) {
            fl.out_start[i] += cell_start;
        }
    }
    lm.file_ends.back() = cell_start + code.size();
    cell_files.push_back(fl);
    cell_ends.push_back(lm.file_ends.back());
}

Result<FortranEvaluator::EvalResult> FortranEvaluator::evaluate2(const std::string &code) {
    LocationManager lm;
    LCompilers::PassManager lpm;
    lpm.use_default_passes();
    if (!compiler_options.interactive) {
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

    // ASR -> LLVM. The passes rewrite this tree; that is fine, it is this
    // cell's own and is discarded afterwards. What later cells resolve
    // against is the snapshot taken in get_asr3().
    Result<std::unique_ptr<LLVMModule>> res3 = get_llvm3(*asr,
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
    if (asr->m_symtab->get_symbol(run_fn) != nullptr) {
        ASR::symbol_t *fn_sym = asr->m_symtab->get_symbol(run_fn);
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
    // Every interactive entry point comes through here, so this is where a
    // cell is opened: the kernel calls get_ast() and get_asr() directly for
    // its magics, and closing a cell that was never opened reads off the end
    // of an empty vector.
    if (compiler_options.interactive) {
        cell_start = open_cell(code_orig, lm);
    }
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
            if (compiler_options.interactive) close_cell(*cpp_input, lm);
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
            if (compiler_options.interactive) close_cell(*code, lm);
            return prescan_res.error;
        }
        code = &tmp;
    }
    if (compiler_options.interactive) close_cell(*code, lm);
    Result<LFortran::AST::TranslationUnit_t*>
        res = LFortran::parse(al, *code, diagnostics, compiler_options,
            compiler_options.interactive ? cell_start : 0);
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

namespace {

// A generic procedure and a custom operator name the specific procedures they
// resolve to. A later cell that resolves one would otherwise call the original
// specific procedure, which is not the one this cell compiled and which code
// generation never declares. The list is shared with the original, so it is
// replaced, not written to.
static void relink_procs(Allocator &al, SymbolTable *scope,
    ASR::symbol_t **&m_procs, size_t &n_procs)
{
    Vec<ASR::symbol_t*> procs;
    procs.reserve(al, n_procs);
    for (size_t i = 0; i < n_procs; i++) {
        ASR::symbol_t *copy = scope->resolve_symbol(
            ASRUtils::symbol_name(m_procs[i]));
        procs.push_back(al, copy != nullptr ? copy : m_procs[i]);
    }
    m_procs = procs.p;
    n_procs = procs.size();
}

// The duplicated symbols still point into the scopes they were copied from: an
// ExternalSymbol at the module it names, a generic procedure at the specific
// procedures it resolves to. A later cell resolves a name and finds the copy,
// so anything still pointing at an original does not match it. Point them at
// the copies.
static void relink_copied_symbols(Allocator &al, SymbolTable *scope,
    SymbolTable *tu)
{
    for (auto &item : scope->get_scope()) {
        ASR::symbol_t *s = item.second;
        if (ASR::is_a<ASR::ExternalSymbol_t>(*s)) {
            ASR::ExternalSymbol_t *es = ASR::down_cast<ASR::ExternalSymbol_t>(s);
            ASR::symbol_t *mod = tu->resolve_symbol(es->m_module_name);
            if (mod == nullptr || !ASR::is_a<ASR::Module_t>(*mod)) continue;
            ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(mod);
            ASR::symbol_t *target = m->m_symtab->find_scoped_symbol(
                es->m_original_name, es->n_scope_names, es->m_scope_names);
            if (target != nullptr) es->m_external = target;
        } else if (ASR::is_a<ASR::GenericProcedure_t>(*s)) {
            ASR::GenericProcedure_t *gp
                = ASR::down_cast<ASR::GenericProcedure_t>(s);
            relink_procs(al, scope, gp->m_procs, gp->n_procs);
        } else if (ASR::is_a<ASR::CustomOperator_t>(*s)) {
            ASR::CustomOperator_t *co
                = ASR::down_cast<ASR::CustomOperator_t>(s);
            relink_procs(al, scope, co->m_procs, co->n_procs);
        } else if (ASR::is_a<ASR::StructMethodDeclaration_t>(*s)) {
            // A type-bound procedure names the procedure it binds to, and a
            // later cell calls whatever it names. Left pointing at the
            // original, that is a procedure code generation never declares.
            ASR::StructMethodDeclaration_t *m
                = ASR::down_cast<ASR::StructMethodDeclaration_t>(s);
            // From the scope holding the derived type, not from the type's
            // own scope: the binding carries the name of the procedure, and
            // looking it up there finds the binding itself.
            SymbolTable *outer = scope->parent;
            ASR::symbol_t *copy = outer == nullptr ? nullptr
                : outer->resolve_symbol(ASRUtils::symbol_name(m->m_proc));
            if (copy != nullptr && ASR::is_a<ASR::Function_t>(
                    *ASRUtils::symbol_get_past_external(copy))) {
                m->m_proc = copy;
            }
        } else if (ASR::is_a<ASR::Variable_t>(*s)) {
            // A variable of a derived type names the type it was declared
            // with. Left pointing at the original, a dummy argument of a
            // copied procedure is of a different type than the one a later
            // cell declares from the copy, and the call does not type check.
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
            if (v->m_type_declaration != nullptr) {
                ASR::symbol_t *copy = scope->resolve_symbol(
                    ASRUtils::symbol_name(v->m_type_declaration));
                if (copy != nullptr) v->m_type_declaration = copy;
            }
        } else if (ASR::is_a<ASR::Module_t>(*s)
                || ASR::is_a<ASR::Function_t>(*s)
                || ASR::is_a<ASR::Struct_t>(*s)
                || ASR::is_a<ASR::Enum_t>(*s)
                || ASR::is_a<ASR::Union_t>(*s)
                || ASR::is_a<ASR::Block_t>(*s)
                || ASR::is_a<ASR::AssociateBlock_t>(*s)) {
            // Only the symbols that own a scope: asking any other kind for one
            // is an error, not an empty answer.
            relink_copied_symbols(al, ASRUtils::symbol_symtab(s), tu);
        }
    }
}

} // namespace

SymbolTable* FortranEvaluator::snapshot_cell_scope(ASR::TranslationUnit_t &asr)
{
    // ASR passes rewrite what they are given: pass_array_by_data replaces a
    // procedure taking an assumed-shape array with a specialisation under a
    // mangled name, for instance. This cell's scope is the parent of the next
    // cell's, so if later cells resolved names against the tree the passes
    // just rewrote, they would see lowered signatures instead of the ones the
    // user wrote.
    //
    // So the cell is compiled from the tree semantic analysis produced -- the
    // passes may do as they like to it, it is thrown away afterwards -- while
    // the chain gets a copy of its symbols taken beforehand. Only symbols are
    // copied: later cells resolve names and read signatures, they never re-run
    // this cell's statements. The copy carries the same names at the same
    // depth, so it mangles to the same symbols this cell just compiled.
    //
    // The earlier cells are copied too. Passes do not only add symbols, they
    // also rewrite the ones they are given in place: the pass that replaces
    // optional arguments with presence flags rewrites the procedure itself, so
    // an earlier cell's procedure that this cell's passes touched would be
    // presented to the next cell with its optional arguments already gone, and
    // a call omitting them would no longer compile. Each cell therefore hands
    // the next one a chain that no pass has seen.
    SymbolTable* parent = copy_cell_chain(asr.m_symtab->parent,
        asr.base.base.loc);
    SymbolTable* snapshot = copy_cell_scope(asr.m_symtab, parent,
        asr.base.base.loc);
    return snapshot;
}

// One TranslationUnit scope's symbols, copied into a fresh scope. Only symbols
// are copied: later cells resolve names and read signatures, they never re-run
// an earlier cell's statements. The copy carries the same names at the same
// depth, so it mangles to the same symbols that cell compiled to.
SymbolTable* FortranEvaluator::copy_cell_scope(SymbolTable *scope,
    SymbolTable *parent, const Location &loc)
{
    SymbolTable* copy = al.make_new<SymbolTable>(parent);
    ASR::asr_t* owner = ASR::make_TranslationUnit_t(al, loc, copy, nullptr, 0);
    copy->asr_owner = owner;
    ASRUtils::SymbolDuplicator duplicator(al);
    for (auto &item : scope->get_scope()) {
        // A program unit is executed by the cell that declares it and its name
        // is not referenceable from Fortran, so later cells have no use for it.
        if (ASR::is_a<ASR::Program_t>(*item.second)) continue;
        duplicator.duplicate_symbol(item.second, copy);
    }
    relink_copied_symbols(al, copy, copy);
    return copy;
}

// The chain of earlier cells, oldest first, copied scope by scope.
SymbolTable* FortranEvaluator::copy_cell_chain(SymbolTable *chain,
    const Location &loc)
{
    if (chain == nullptr) return nullptr;
    SymbolTable *parent = copy_cell_chain(chain->parent, loc);
    return copy_cell_scope(chain, parent, loc);
}

Result<ASR::TranslationUnit_t*> FortranEvaluator::get_asr3(
    LFortran::AST::TranslationUnit_t &ast, diag::Diagnostics &diagnostics, LCompilers::LocationManager &lm)
{
    ASR::TranslationUnit_t* asr;
    // AST -> ASR
    //
    // Each interactive evaluation gets its own TranslationUnit, whose scope is
    // parented to the previous one. Cells are therefore purely additive: a cell
    // sees everything declared before it, and nothing can reach forward into a
    // later cell. Redeclaring a name shadows the earlier one the way a nested
    // scope does, so code compiled earlier keeps using what it resolved to
    // then, while later cells resolve to the new declaration. That is the
    // behaviour of a Python notebook, where re-running a cell rebinds the name
    // and objects created earlier keep the old one.
    //
    // Ordinary compilation has none of this: one translation unit, no chaining
    // and no copying, so that it is not slowed down by it.
    SymbolTable *cell_scope = symbol_table;
    if (compiler_options.interactive) {
        if (symbol_table) {
            // Everything declared before this cell is already compiled, so it
            // is referenced rather than emitted again.
            symbol_table->mark_all_variables_external(al);
        }
        cell_scope = al.make_new<SymbolTable>(symbol_table);
    }
    auto res = LFortran::ast_to_asr(al, ast, diagnostics, cell_scope,
        compiler_options.symtab_only, compiler_options, lm);
    if (res.ok) {
        asr = res.result;
    } else {
        LCOMPILERS_ASSERT(diagnostics.has_error())
        return res.error;
    }
    if (compiler_options.interactive) {
        // The next cell is parented to a snapshot of this one, not to the tree
        // about to be handed to the passes.
        symbol_table = snapshot_cell_scope(*asr);
    }

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

    if (compiler_options.interactive) {
        // Nothing in this cell may call a procedure the user declared, but a
        // later cell still can, so it has to be emitted. Dropping it here
        // leaves the later cell's call with no definition to link to.
        pass_manager.skip_pass("unused_functions");
    }

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

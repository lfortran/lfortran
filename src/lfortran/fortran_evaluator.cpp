#include <cstdio>
#include <algorithm>
#include <atomic>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <optional>
#include <set>
#include <sstream>
#include <stdexcept>
#include <type_traits>
#include <cerrno>
#include <exception>
#ifndef __EMSCRIPTEN__
#include <chrono>
#include <condition_variable>
#include <mutex>
#include <thread>
#endif

#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
// The SDK's COM shorthand must not rewrite compiler identifiers.
#undef interface
#else
#include <unistd.h>
#endif
#include <sys/stat.h>

#include <lfortran/fortran_evaluator.h>
#include <libasr/codegen/asr_to_cpp.h>
#include <libasr/codegen/asr_to_c.h>
#include <libasr/codegen/asr_to_wasm.h>
#include <libasr/codegen/asr_to_julia.h>
#include <libasr/codegen/asr_to_cuda.h>
#include <libasr/codegen/asr_to_metal.h>
#include <libasr/codegen/asr_to_fortran.h>
#include <libasr/codegen/wasm_to_wat.h>
#include <lfortran/ast_to_src.h>
#include <libasr/exception.h>
#include <libasr/stacktrace.h>
#include <libasr/runtime/lfortran_stdin.h>
#include <libasr/runtime/lfortran_cancellation.h>
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

#ifndef __EMSCRIPTEN__
namespace {
struct CellCancellation {
    std::atomic<bool> requested{false};
    bool observed = false; // Worker-owned, read by the caller only after join.
    unsigned protected_depth = 0;
};
thread_local CellCancellation *active_cancellation = nullptr;

#ifdef HAVE_LFORTRAN_LLVM
int poll_cell_cancellation(bool propagate_only) {
    auto *state = active_cancellation;
    if (!state || state->protected_depth) return 0;
    if (!propagate_only && state->requested.load(std::memory_order_relaxed)) {
        state->observed = true;
    }
    return state->observed;
}

void protect_cell_cancellation(bool begin) {
    if (!active_cancellation) return;
    if (begin) active_cancellation->protected_depth++;
    else {
        LCOMPILERS_ASSERT(active_cancellation->protected_depth > 0);
        active_cancellation->protected_depth--;
    }
}
#endif
}
#endif


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

// Defined in the runtime library (lfortran_intrinsics.c, declared in
// lfortran_intrinsics.h, a C header), whose lfortran_display_data() and
// lfortran_clear_output() the lfortran_display module calls; the JIT
// resolves them to the copy linked into this process.
extern "C" void *_lfortran_set_display_hooks(
    void (*display_data)(void *context, const char *mime_type, const char *data),
    void (*clear_output)(void *context),
    void *context);

namespace {
// The context of the display hooks while FortranEvaluator::execute() runs a
// cell.
struct DisplayContext {
    const LCompilers::FortranEvaluator::CellIO &io;
    std::string error; // why a hook failed; empty if none has
};

// The context of the runtime's stdin provider while execute() runs a cell
// whose CellIO has an input hook. The runtime copies `record` before it
// asks for the next one.
struct InputContext {
    const LCompilers::FortranEvaluator::CellIO &io;
    std::string record;
};

#ifdef _WIN32
int dup_fd(int fd) { return _dup(fd); }
int dup2_fd(int from, int to) { return _dup2(from, to); }
void close_fd(int fd) { _close(fd); }
#else
int dup_fd(int fd) { return dup(fd); }
int dup2_fd(int from, int to) { return dup2(from, to); }
void close_fd(int fd) { close(fd); }
#endif

// While a cell runs with CellIO::output set, its stdout and stderr (file
// descriptors 1 and 2) go to temporary files rather than pipes: a file never
// fills up. Readers must not change the writer's file position.
class OutputCapture {
    FILE *files[2] = {nullptr, nullptr};
    int saved[2] = {-1, -1};
    int64_t consumed[2] = {0, 0};
#ifdef _WIN32
    int readers[2] = {-1, -1};
#endif
    void restore() noexcept {
        for (int i = 0; i < 2; i++) {
            if (saved[i] >= 0) {
                dup2_fd(saved[i], 1 + i);
                close_fd(saved[i]);
            }
#ifdef _WIN32
            if (readers[i] >= 0) close_fd(readers[i]);
#endif
            if (files[i]) fclose(files[i]);
        }
    }
public:
    OutputCapture() {
        try {
            fflush(stdout);
            fflush(stderr);
            for (int i = 0; i < 2; i++) {
                files[i] = tmpfile();
                if (!files[i]) throw std::runtime_error("cannot create cell output capture");
#ifdef _WIN32
                // Unlike dup(), ReOpenFile creates an independent file position.
                HANDLE reader = ReOpenFile((HANDLE)_get_osfhandle(fileno(files[i])),
                    GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, 0);
                if (reader == INVALID_HANDLE_VALUE)
                    throw std::runtime_error("cannot open cell output reader");
                readers[i] = _open_osfhandle((intptr_t)reader, _O_BINARY);
                if (readers[i] < 0) {
                    CloseHandle(reader);
                    throw std::runtime_error("cannot open cell output descriptor");
                }
#endif
                saved[i] = dup_fd(1 + i);
                if (saved[i] < 0 || dup2_fd(fileno(files[i]), 1 + i) < 0)
                    throw std::runtime_error("cannot redirect cell output");
            }
        } catch (...) {
            restore();
            throw;
        }
    }
    ~OutputCapture() {
        fflush(stdout);
        fflush(stderr);
        restore();
    }
    OutputCapture(const OutputCapture &) = delete;
    OutputCapture &operator=(const OutputCapture &) = delete;
    // Passes what the cell has written since the last call to `output`.
    void pump(const std::function<void(int, const std::string &)> &output,
            bool barrier = false) {
        // Formatted/nonadvancing WRITE may still be in the C stdio buffer.
        // fflush serializes with stdio writers; it never moves the read offset.
        if (fflush(stdout) != 0 || fflush(stderr) != 0)
            throw std::runtime_error("cannot flush cell output capture");
        for (int i = 0; i < 2; i++) {
            int fd = fileno(files[i]);
            char buffer[4096];
#ifdef _WIN32
            struct _stat64 info;
            if (_fstat64(fd, &info) != 0)
#else
            struct stat info;
            if (fstat(fd, &info) != 0)
#endif
                throw std::runtime_error("cannot inspect cell output capture");
            // Snapshot at barriers; bounded turns otherwise. Separate spools
            // preserve per-stream order, not stdout/stderr interleaving.
            int64_t end = barrier ? info.st_size
                : std::min<int64_t>(info.st_size, consumed[i] + 65536);
            while (consumed[i] < end) {
                unsigned size = (unsigned)std::min<int64_t>(sizeof(buffer), end - consumed[i]);
#ifdef _WIN32
                if (_lseeki64(readers[i], consumed[i], SEEK_SET) < 0)
                    throw std::runtime_error("cannot position cell output reader");
                int n = _read(readers[i], buffer, size);
#else
                auto n = pread(fd, buffer, size, consumed[i]);
#endif
                if (n < 0 && errno == EINTR) continue;
                if (n <= 0) throw std::runtime_error("cannot read cell output capture");
                consumed[i] += n;
                output(1 + i, std::string(buffer, n));
            }
        }
    }
};

// What the cell running with CellIO::output has written but not yet passed
// on, for when it ends the process.
std::function<void()> *flush_at_exit = nullptr;

#ifndef __EMSCRIPTEN__
// One synchronous hook at a time; no output queue or frontend work on the
// worker. In particular no mutex is held while a callback waits for stdin.
class CellMailbox {
    std::mutex mutex;
    std::condition_variable changed;
    std::function<void()> pending;
    std::exception_ptr failure;
    bool done = false;
public:
    void call(const std::function<void()> &action) {
        std::unique_lock<std::mutex> lock(mutex);
        pending = action;
        changed.notify_one();
        changed.wait(lock, [&]() { return !pending; });
        if (failure) std::rethrow_exception(failure);
    }
    void finish() {
        std::lock_guard<std::mutex> lock(mutex);
        done = true;
        changed.notify_one();
    }
    [[noreturn]] void await_process_exit() {
        std::unique_lock<std::mutex> lock(mutex);
        for (;;) changed.wait(lock);
    }
    bool service() {
        std::unique_lock<std::mutex> lock(mutex);
        if (!pending && !done) changed.wait_for(lock, std::chrono::milliseconds(10));
        if (pending) {
            lock.unlock();
            std::exception_ptr error;
            try { pending(); } catch (...) { error = std::current_exception(); }
            lock.lock();
            failure = error;
            pending = {};
            changed.notify_one();
        }
        return done;
    }
};
#endif

// A hook is called from generated code, so its exception must not unwind
// through it.
template <typename Call>
void run_hook(DisplayContext &context, const Call &call) {
    try {
        call();
    } catch (const std::exception &error) {
        if (context.error.empty()) context.error = error.what();
    } catch (...) {
        if (context.error.empty()) context.error = "unknown exception";
    }
}
}

extern "C" {
static void display_data_hook(void *context, const char *mime_type,
        const char *data) {
    DisplayContext &c = *static_cast<DisplayContext *>(context);
    run_hook(c, [&]() { if (c.io.display) c.io.display(mime_type, data); });
}

static void clear_output_hook(void *context) {
    DisplayContext &c = *static_cast<DisplayContext *>(context);
    run_hook(c, [&]() { if (c.io.clear_output) c.io.clear_output(); });
}

static int read_input(void *context, const char **record, size_t *length) {
    InputContext &c = *static_cast<InputContext *>(context);
    try {
        c.record.clear();
        bool interruptible = true;
#ifndef __EMSCRIPTEN__
        interruptible = !active_cancellation || active_cancellation->protected_depth == 0;
#endif
        int status = c.io.input(c.record, interruptible);
        *record = c.record.data();
        *length = c.record.size();
        return status;
    } catch (...) {
        // An exception must not unwind through generated code or the C
        // runtime; the cell sees unavailable input instead.
        return 1;
    }
}
}

namespace LCompilers {

class StringDescriptor {
    struct Storage {
        char *data;
        int64_t length;
    };
    Storage storage{};

public:
    void *pointer() {
        return &storage;
    }

    char *data() const {
        return storage.data;
    }

    int64_t length() const {
        return storage.length;
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
        e = std::make_unique<LLVMEvaluator>(compiler_options);
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
    lm.file_ends.push_back(LFortran::source_offset(start + code.size()));
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
    lm.file_ends.back() = LFortran::source_offset(cell_start + code.size());
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
        fl.source = code;
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
    const JITCancellation *cancellation = nullptr;
#ifndef __EMSCRIPTEN__
    const JITCancellation native_cancellation{
        poll_cell_cancellation, protect_cell_cancellation,
        [&](const std::string &name) {
            // The owned-command runtime returns normally after cancellation,
            // just like a checkpointed JIT procedure.
            return defined_symbols.count(name) || name == "_lfortran_exec_command";
        }};
    cancellation = &native_cancellation;
#endif
    Result<std::unique_ptr<LLVMModule>> res3 = get_llvm3(*asr,
        pass_manager, diagnostics, lm, lm.files.back().in_filename,
        nullptr, cancellation);
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
    // The globals the cell declares that need executable code to set up are
    // set up in `run_fn + "_setup"` (see emit_interactive_global_setup),
    // which has to run before anything of the cell can use them.
    std::string setup_fn = run_fn + "_setup";
    bool has_setup = (m->get_return_type(setup_fn) == "void");

    // Logical and Windows complex results share LLVM return types with
    // integers or subroutines. Use the ASR result kind to select their ABI.
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
                } else if (compiler_options.platform == Platform::Windows &&
                        ASRUtils::is_complex(*ret_type)) {
                    int kind = ASRUtils::extract_kind_from_ttype_t(ret_type);
                    LCOMPILERS_ASSERT(kind == 4 || kind == 8);
                    return_type = kind == 4 ? "complex4" : "complex8";
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
    if (has_setup) {
        e.execfn<void>(setup_fn);
    }
    if (has_program) {
        e.execfn<void>(program_fn);
#ifndef __EMSCRIPTEN__
        if (active_cancellation && active_cancellation->observed) return result;
#endif
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
        result.type = EvalResult::complex4;
        if (compiler_options.platform == Platform::Windows) {
            // The JIT returns the two floats in an i64, not a C++ class.
            uint64_t r = e.execfn<uint64_t>(run_fn);
            float parts[2];
            static_assert(sizeof(parts) == sizeof(r));
            std::memcpy(parts, &r, sizeof(r));
            result.c32.re = parts[0];
            result.c32.im = parts[1];
        } else {
#if defined(__aarch64__) && defined(__linux__)
            // On Linux/AArch64 the JIT function returns complex_4 as <2 x float>
            // in V0 (see get_function_type() in llvm_utils.cpp), while
            // std::complex<float> follows the HFA convention (S0+S1), so the
            // imaginary part came back as garbage. Read the vector lanes.
            typedef float v2f32 __attribute__((vector_size(8)));
            v2f32 r = e.execfn<v2f32>(run_fn);
            result.c32.re = r[0];
            result.c32.im = r[1];
#else
            std::complex<float> r = e.execfn<std::complex<float>>(run_fn);
            result.c32.re = r.real();
            result.c32.im = r.imag();
#endif
        }
    } else if (return_type == "complex8") {
        result.type = EvalResult::complex8;
        if (compiler_options.platform == Platform::Windows) {
            double parts[2];
            e.execfn<void>(run_fn, parts);
            result.c64.re = parts[0];
            result.c64.im = parts[1];
        } else {
            std::complex<double> r = e.execfn<std::complex<double>>(run_fn);
            result.c64.re = r.real();
            result.c64.im = r.imag();
        }
    } else if (return_type == "logical") {
        int32_t r = e.execfn<int32_t>(run_fn);
        result.type = EvalResult::boolean;
        result.b = (r != 0);
    } else if (return_type == "character") {
        StringDescriptor descriptor;
        e.execfn<void>(run_fn, descriptor.pointer());
#ifndef __EMSCRIPTEN__
        if (active_cancellation && active_cancellation->observed) return result;
#endif
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

std::string FortranEvaluator::EvalResult::type_name() const {
    switch (type) {
        case integer4: return "integer";
        case integer8: return "integer(8)";
        case real4: return "real";
        case real8: return "real(8)";
        case complex4: return "complex";
        case complex8: return "complex(8)";
        case boolean: return "logical";
        case character: return "character";
        default: return "none";
    }
}

namespace {
// The shortest text, with at most `max_digits` significant digits, that reads
// back as `x`, and always with a decimal point or an exponent, so that a real
// such as 1.0 does not look like the integer 1.
template <typename T>
std::string real_text(T x, int max_digits) {
    std::string text;
    for (int digits = 1; digits <= max_digits; digits++) {
        std::ostringstream s;
        s << std::setprecision(digits) << x;
        text = s.str();
        T y = std::is_same<T, float>::value ? std::strtof(text.c_str(), nullptr)
            : std::strtod(text.c_str(), nullptr);
        if (y == x) break;
    }
    if (text.find_first_not_of("-0123456789") == std::string::npos) {
        text += ".0";
    }
    return text;
}
}

std::string FortranEvaluator::EvalResult::text() const {
    std::ostringstream s;
    switch (type) {
        case integer4: s << i32; break;
        case integer8: s << i64; break;
        case real4: s << real_text(f32, 9); break;
        case real8: s << real_text(f64, 17); break;
        case complex4:
            s << "(" << real_text(c32.re, 9) << ", " << real_text(c32.im, 9) << ")";
            break;
        case complex8:
            s << "(" << real_text(c64.re, 17) << ", " << real_text(c64.im, 17) << ")";
            break;
        case boolean: s << (b ? "True" : "False"); break;
        case character: s << str; break;
        case statement: s << "(statement)"; break;
        case none: s << "(nothing to execute)"; break;
    }
    return s.str();
}

std::string FortranEvaluator::CellResult::error_text() const {
    if (internal_error) {
        return "Internal Compiler Error: Unhandled exception\n" + error_traceback
            + error_name + ": " + error_value + "\n";
    }
    if (!error_value.empty() && error_value.back() != '\n') {
        return error_value + "\n";
    }
    return error_value;
}

FortranEvaluator::CellResult FortranEvaluator::execute(const std::string &code,
        bool verbose, const CellIO &io) {
    LCOMPILERS_ASSERT(compiler_options.interactive);
    CellResult out;
    // What the cell writes goes to io.output before anything it displays,
    // clears or reads.
    std::optional<OutputCapture> capture;
    std::exception_ptr output_error;
    auto drain_output = [&](bool barrier) {
        if (output_error) std::rethrow_exception(output_error);
        if (capture) {
            try {
                capture->pump(io.output, barrier);
                if (io.progress_output) io.progress_output();
            }
            catch (...) {
                output_error = std::current_exception();
                throw;
            }
        }
    };
#ifndef __EMSCRIPTEN__
    CellMailbox mailbox;
    CellCancellation cancellation;
    const bool threaded = bool(io.output) || bool(io.cancel_requested);
    const auto owner = std::this_thread::get_id();
    bool process_exiting = false; // Only accessed by the caller.
    auto poll_cancellation = [&]() {
        try {
            if (io.cancel_requested && io.cancel_requested()) {
                cancellation.requested.store(true, std::memory_order_relaxed);
            }
        } catch (...) {
            output_error = std::current_exception();
            cancellation.requested.store(true, std::memory_order_relaxed);
        }
    };
#endif
    auto dispatch = [&](const std::function<void()> &action) {
#ifndef __EMSCRIPTEN__
        if (threaded && std::this_thread::get_id() != owner) {
            mailbox.call([&]() {
                action();
                poll_cancellation();
            });
            return;
        }
#endif
        action();
    };
    std::function<void()> flush_output = [&]() {
        dispatch([&]() {
#ifndef __EMSCRIPTEN__
            process_exiting = true;
#endif
            // STOP runs atexit on the worker. Drain and flush the transport
            // here, before letting it continue process termination.
            try { drain_output(true); }
            catch (...) {
                if (io.flush_on_exit) io.flush_on_exit();
                throw;
            }
            if (io.flush_on_exit) io.flush_on_exit();
        });
    };
    // A cell that ends the process (STOP, a fatal error) still passes on
    // what it wrote.
    struct FlushAtExit {
        FlushAtExit(std::function<void()> *flush) {
            static const int registered = std::atexit([]() {
                try { if (flush_at_exit) (*flush_at_exit)(); }
                catch (...) {} // An atexit failure cannot escape exit().
            });
            (void)registered;
            flush_at_exit = flush;
        }
        ~FlushAtExit() { flush_at_exit = nullptr; }
    };
    std::optional<FlushAtExit> flush_at_exit_guard;
    CellIO hooks_io = io;
    if (io.output
#ifndef __EMSCRIPTEN__
            || threaded
#endif
            ) {
        flush_at_exit_guard.emplace(&flush_output);
        if (io.display) {
            hooks_io.display = [&](const std::string &mime, const std::string &data) {
                dispatch([&]() {
                    drain_output(true);
                    io.display(mime, data);
                });
            };
        }
        if (io.clear_output) {
            hooks_io.clear_output = [&]() {
                dispatch([&]() {
                    drain_output(true);
                    io.clear_output();
                });
            };
        }
        if (io.input) {
            hooks_io.input = [&](std::string &record, bool interruptible) {
                int status = 1;
                dispatch([&]() {
                    drain_output(true);
                    status = io.input(record, interruptible);
                });
                return status;
            };
        }
    }
    DisplayContext display{hooks_io, ""};
    InputContext input{hooks_io, ""};
    struct Hooks {
        Hooks(DisplayContext &display, InputContext &input) {
            [[maybe_unused]] void *previous = _lfortran_set_display_hooks(display_data_hook,
                clear_output_hook, &display);
            LCOMPILERS_ASSERT(previous == nullptr);
            _lfortran_stdin_install(input.io.input ? read_input : nullptr, &input);
        }
        ~Hooks() {
            _lfortran_stdin_reset();
            _lfortran_set_display_hooks(nullptr, nullptr, nullptr);
        }
    } hooks(display, input);
    LocationManager lm;
    diag::Diagnostics diagnostics;
    PassManager passes;
    passes.use_default_passes();
    auto execute_cell = [&]() {
#ifndef __EMSCRIPTEN__
        struct ActiveCancellation {
            CellCancellation *previous = active_cancellation;
            lfortran_cancel_poll previous_poll = nullptr;
            ActiveCancellation(CellCancellation &state, [[maybe_unused]] bool enabled) {
                active_cancellation = &state;
#ifdef HAVE_LFORTRAN_LLVM
                previous_poll = _lfortran_set_cancel_poll(enabled
                    ? +[]() { return poll_cell_cancellation(false); } : nullptr);
#endif
            }
            ~ActiveCancellation() {
#ifdef HAVE_LFORTRAN_LLVM
                _lfortran_set_cancel_poll(previous_poll);
#endif
                active_cancellation = previous;
            }
        } active(cancellation, bool(io.cancel_requested));
#endif
        try {
            const auto newline = code.find('\n');
            auto magic = code.substr(0, newline);
            // Trailing blanks and the \r of a CRLF line ending are not part of it.
            magic.erase(magic.find_last_not_of(" \t\r") + 1);
            const auto source = newline == std::string::npos ? "" : code.substr(newline + 1);
            Result<std::string> shown{Error{}};
            // An inspection cell shows how its code compiles in the session, but
            // it does not run, so its declarations must not join the session.
            struct KeepSession {
                SymbolTable *&scope;
                SymbolTable *const saved;
                ~KeepSession() { scope = saved; }
            };
            out.inspection = true;
            if (magic == "%%showast") shown = get_ast(source, lm, diagnostics);
            else if (magic == "%%showasr") {
                KeepSession keep{symbol_table, symbol_table};
                shown = get_asr(source, lm, diagnostics);
            } else if (magic == "%%showllvm") {
                KeepSession keep{symbol_table, symbol_table};
                shown = get_llvm(source, lm, passes, diagnostics);
            } else if (magic == "%%showasm") {
                KeepSession keep{symbol_table, symbol_table};
                shown = get_asm(source, lm, passes, diagnostics);
            } else if (magic == "%%showcpp") shown = get_cpp(source, lm, diagnostics, 1);
            else if (magic == "%%showfmt") shown = get_fmt(source, lm, diagnostics);
            else {
                out.inspection = false;
                auto result = evaluate(code, verbose, lm, passes, diagnostics);
                out.ok = result.ok;
                if (out.ok) out.value = std::move(result.result);
            }
            if (shown.ok) {
                out.ok = true;
                out.output = shown.result;
            }
            out.diagnostics = diagnostics.render(lm, compiler_options);
            if (!out.ok) {
                LCOMPILERS_ASSERT(diagnostics.has_error());
                out.error_name = "CompilerError";
                out.error_value = out.diagnostics;
            } else if (!display.error.empty()) {
                out.ok = false;
                out.error_name = "DisplayError";
                out.error_value = "display failed: " + display.error;
            }
        } catch (const LCompilersException &error) {
            out.ok = false;
            out.internal_error = true;
            out.error_name = error.name();
            out.error_value = error.msg();
            auto trace = error.stacktrace_addresses();
            get_local_addresses(trace);
            get_local_info(trace);
            out.error_traceback = stacktrace2str(trace, stacktrace_depth);
        }
    };
    auto execution_error = [&](const std::string &message) {
        out.ok = false;
        out.internal_error = false;
        out.error_traceback.clear();
        out.error_name = "ExecutionError";
        out.error_value = message;
    };
    try {
        if (io.output) capture.emplace();
#ifndef __EMSCRIPTEN__
        if (threaded) {
            std::exception_ptr worker_error;
            std::thread worker([&]() {
                try { execute_cell(); }
                catch (...) { worker_error = std::current_exception(); }
                mailbox.finish();
            });
            bool done;
            do {
                poll_cancellation();
                try { drain_output(false); }
                catch (...) {} // Saved above; keep servicing hooks until join.
                done = mailbox.service();
                if (process_exiting) mailbox.await_process_exit();
            } while (!done);
            worker.join();
            drain_output(true);
            if (worker_error) std::rethrow_exception(worker_error);
        } else
#endif
        {
            execute_cell();
        }
        drain_output(true);
#ifndef __EMSCRIPTEN__
        if (cancellation.observed) {
            out.ok = false;
            out.value = {};
            out.internal_error = false;
            out.error_traceback.clear();
            out.error_name = "KeyboardInterrupt";
            out.error_value = "cell execution interrupted";
        }
#endif
    } catch (const std::exception &error) {
        execution_error(error.what());
    } catch (...) {
        execution_error("unknown cell execution failure");
    }
    return out;
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

LCompilers::ASR::asr_t* FortranEvaluator::handle_lookup_name(LCompilers::ASR::TranslationUnit_t* tu, uint32_t pos) {
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
        // Cleanup helpers with local linkage belong to this module; the JIT
        // cannot resolve a declaration of one against a previous cell.
        if (fn.isDeclaration() || fn.hasLocalLinkage()) continue;
        std::string name = fn.getName().str();
        if (!defined_symbols.insert(name).second) {
            fn.deleteBody();
        }
    }
}
#endif

namespace {

// Whether `scope`, or a scope nested in it, has a view of a common block.
static bool has_common_block(SymbolTable *scope) {
    for (auto &item : scope->get_scope()) {
        if (ASR::is_a<ASR::CommonBlock_t>(*item.second)) {
            return true;
        }
        SymbolTable *nested = ASRUtils::symbol_symtab(item.second);
        if (nested != nullptr && has_common_block(nested)) {
            return true;
        }
    }
    return false;
}

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
            // An ExternalSymbol has to name a definition. The module's own
            // entry for the name may be an import of its own -- an intrinsic
            // module re-exporting a derived type -- and naming that would
            // build a chain no consumer of the symbol can follow.
            if (target != nullptr &&
                    !ASR::is_a<ASR::ExternalSymbol_t>(*target)) {
                es->m_external = target;
            }
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
                || ASR::is_a<ASR::Program_t>(*s)
                || ASR::is_a<ASR::BlockData_t>(*s)
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
    ASR::asr_t* owner = ASR::make_TranslationUnit_t(al, loc, copy, nullptr, 0, nullptr);
    copy->asr_owner = owner;
    ASRUtils::SymbolDuplicator duplicator(al);
    for (auto &item : scope->get_scope()) {
        // A program unit is executed by the cell that declares it and its name
        // is not referenceable from Fortran, so later cells have no use for
        // its statements. The same holds for a BLOCK DATA, whose initial
        // values the storage of its common blocks already carries. What a
        // later cell needs of either is its views of common blocks, which
        // tell it that the storage of those blocks exists already, so one
        // that has views is copied without its statements.
        if (ASR::is_a<ASR::Program_t>(*item.second)) {
            ASR::Program_t *program = ASR::down_cast<ASR::Program_t>(item.second);
            if (has_common_block(program->m_symtab)) {
                SymbolTable *symtab = al.make_new<SymbolTable>(copy);
                duplicator.duplicate_SymbolTable(program->m_symtab, symtab);
                ASR::asr_t *program_copy = ASR::make_Program_t(al,
                    program->base.base.loc, symtab, program->m_name,
                    program->m_dependencies, program->n_dependencies,
                    nullptr, 0, nullptr, program->m_start_name,
                    program->m_end_name);
                symtab->asr_owner = program_copy;
                copy->add_symbol(item.first, ASR::down_cast<ASR::symbol_t>(program_copy));
            }
            continue;
        }
        if (ASR::is_a<ASR::BlockData_t>(*item.second)) {
            ASR::BlockData_t *block_data = ASR::down_cast<ASR::BlockData_t>(item.second);
            SymbolTable *symtab = al.make_new<SymbolTable>(copy);
            duplicator.duplicate_SymbolTable(block_data->m_symtab, symtab);
            ASR::asr_t *block_data_copy = ASR::make_BlockData_t(al,
                block_data->base.base.loc, symtab, block_data->m_name,
                block_data->m_block_data_name, block_data->m_dependencies,
                block_data->n_dependencies, block_data->m_start_name,
                block_data->m_end_name);
            symtab->asr_owner = block_data_copy;
            copy->add_symbol(item.first, ASR::down_cast<ASR::symbol_t>(block_data_copy));
            continue;
        }
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
    std::map<std::string, ASR::ttype_t*> implicit_types;
    if (compiler_options.interactive) {
        auto previous = cell_implicit_types.find(symbol_table);
        if (previous != cell_implicit_types.end()) {
            implicit_types = previous->second;
        }
        if (symbol_table) {
            // Everything declared before this cell is already compiled, so it
            // is referenced rather than emitted again.
            symbol_table->mark_all_variables_external(al);
        }
        cell_scope = al.make_new<SymbolTable>(symbol_table);
    }
    auto res = LFortran::ast_to_asr(al, ast, diagnostics, cell_scope,
        compiler_options.symtab_only, compiler_options, lm,
        compiler_options.interactive ? &implicit_types : nullptr);
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
        cell_implicit_types[symbol_table] = std::move(implicit_types);
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
  [[maybe_unused]] int64_t* time_opt,
  [[maybe_unused]] const JITCancellation *cancellation)
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
    LLVMTargetConfig target_config
        = resolve_llvm_target_config(compiler_options);
#else
    LLVMEvaluator &llvm_evaluator = get_llvm_evaluator();
    llvm::LLVMContext &ctx = llvm_evaluator.get_context();
    LLVMTargetConfig target_config = llvm_evaluator.get_target_config();
#endif

    // ASR -> LLVM
    std::unique_ptr<LCompilers::LLVMModule> m;
    Result<std::unique_ptr<LCompilers::LLVMModule>> res
        = asr_to_llvm(asr, diagnostics,
            ctx, target_config, al, pass_manager,
            compiler_options, run_fn, "", infile, lm, cancellation);
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

Result<std::string> FortranEvaluator::get_gpu_kernel_source(
        ASR::TranslationUnit_t &asr, diag::Diagnostics &diagnostics,
        LCompilers::PassManager& pass_manager)
{
    // The kernels only exist once gpu_offload has run, so the passes have to
    // be applied before either backend is asked for source.
    Allocator al(64*1024*1024);
    compiler_options.po.always_run = false;
    compiler_options.po.run_fun = "f";
    if (!pass_manager.has_user_defined_passes()) {
        pass_manager.use_default_passes();
    }
    // A loop that uses a construct on the unsupported list is shown as a
    // loop that runs on the CPU rather than made fatal, so that the kernels
    // of the other loops are still shown. A loop the offloading pipeline
    // fails on is an error here as everywhere else.
    compiler_options.po.gpu_kernel_source_only = true;
    pass_manager.apply_passes(al, &asr, compiler_options.po, diagnostics);
    if (diagnostics.has_error()) return Error();
    if (compiler_options.gpu_backend == "cuda") {
        return asr_to_cuda(al, asr, diagnostics, compiler_options, false);
    } else if (compiler_options.gpu_backend == "metal") {
        return asr_to_metal(al, asr, diagnostics, compiler_options);
    }
    diagnostics.add(diag::Diagnostic(
        "no GPU backend selected, pass --gpu=cuda or --gpu=metal",
        diag::Level::Error, diag::Stage::CodeGen));
    return Error();
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
    // The MLIR backend formats and transfers only scalars; the array
    // arguments of a StringFormat and the array items of a READ or WRITE
    // are lowered for the LLVM backend alone.
    pass_manager.skip_pass("string_format_array_args");
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
        // A pass reports a hard error by adding it to the diagnostics, and
        // then there is no program to print.
        bool had_error_before_passes = diagnostics.has_error();
        pass_manager.apply_passes(al, asr.result, compiler_options.po, diagnostics);
        if (!had_error_before_passes && diagnostics.has_error()) {
            return Error();
        }
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

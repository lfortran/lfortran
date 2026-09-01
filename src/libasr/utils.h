#ifndef LIBASR_UTILS_H
#define LIBASR_UTILS_H

#include <string>
#include <vector>
#include <filesystem>
#include <libasr/containers.h>

namespace LCompilers {

struct LocationManager;

enum Platform {
    Linux,
    macOS_Intel,
    macOS_ARM,
    macOS_PowerPC,
    Windows,
    FreeBSD,
    OpenBSD,
};

std::string pf2s(Platform);
Platform get_platform();

std::string get_unique_ID();
int visualize_json(std::string &astr_data_json, LCompilers::Platform os);
std::string generate_visualize_html(std::string &astr_data_json);

namespace diag {
    struct Diagnostics;
}

struct PassOptions {
    std::filesystem::path mod_files_dir;
    std::vector<std::filesystem::path> include_dirs;

    int default_integer_kind = 4;

    std::string run_fun; // for global_stmts pass
    // TODO: Convert to std::filesystem::path (also change find_and_load_module())
    std::string runtime_library_dir;
    bool always_run = false; // for unused_functions pass
    bool inline_external_symbol_calls = true; // for inline_function_calls pass
    int64_t unroll_factor = 32; // for loop_unroll pass
    bool fast = false; // is fast flag enabled.
    bool no_fast_math = false; // disable fast-math optimizations (NaN, Inf, etc.)
    bool verbose = false; // For developer debugging
    bool dump_all_passes = false; // For developer debugging
    bool dump_fortran = false; // For developer debugging
    bool pass_cumulative = false; // Apply passes cumulatively
    bool verify_all_passes = false; // Verify ASR after every pass
    bool disable_main = false;
    bool use_loop_variable_after_loop = false;
    bool realloc_lhs_arrays = false;
    std::vector<int64_t> skip_optimization_func_instantiation;
    bool module_name_mangling = false;
    bool intrinsic_module_name_mangling = false;
    bool global_symbols_mangling = false;
    bool intrinsic_symbols_mangling = false;
    bool all_symbols_mangling = false;
    bool bindc_mangling = false;
    bool fortran_mangling = false;
    bool mangle_underscore = false;
    bool mangle_underscore_external = false;
    bool json = false;
    bool clojure = false;
    bool no_member_names = false;
    bool no_loc = false;
    bool visualize = false;
    bool tree = false;
    bool with_intrinsic_mods = false;
    bool c_mangling = false;
    bool enable_cpython = false;
    bool c_skip_bindpy_pass = false;
    bool openmp = false;
    bool gpu_offload_metal = false;
    bool gpu_offload_cuda = false;
    // `!$omp parallel do` asks for host threads. Offloading one onto a device
    // is a choice the user has to make, so it is off unless asked for.
    bool gpu_offload_omp_loops = false;
    // `--gpu-offload-report`: print one line per `do concurrent` that does
    // not become a GPU kernel of its own, saying why.
    bool gpu_offload_report = false;
    // Resolves a `Location` to file/line/column for that report only. Not
    // owned, and null whenever the caller has no LocationManager to hand.
    const LocationManager *loc_manager = nullptr;
    bool time_report = false;
    bool skip_removal_of_unused_procedures_in_pass_array_by_data = false;
    bool bounds_checking = true;
    bool strict_bounds_checking = false;
    // Short-circuit evaluation of logical .and./.or. (the standard permits
    // but does not require it); off by default.
    bool logical_short_circuit = false;
    bool descriptor_index_64 = false; // Use 64-bit indices in array descriptors
    bool coarray = false;
    std::vector<std::string> vector_of_time_report;
    // Set by the pass manager so that a pass can report a diagnostic. It is
    // null when the passes are run outside the pass manager.
    diag::Diagnostics *diagnostics = nullptr;
};

struct CompilerOptions {
    std::vector<std::string> runtime_linker_paths;

    // TODO: Convert to std::filesystem::path (also change find_and_load_module())
    PassOptions po;

    bool fixed_form = false;
    bool interactive = false;
    bool c_preprocessor = false;
    std::vector<std::string> c_preprocessor_defines;
    bool prescan = true;
    bool disable_main = false;
    bool symtab_only = false;
    bool show_stacktrace = false;
    bool use_colors = true;
    bool use_runtime_colors = false;
    bool indent = true;
    bool json = false;
    bool tree = false;
    bool visualize = false;
    bool fast = false;
    bool openmp = false;
    std::string gpu_backend = "";
    std::string gpu_metal_source = "";
    std::string gpu_cuda_source = "";
    // Compile the generated device code as ordinary host code and run the
    // kernels on the CPU, so the GPU path is testable without a GPU.
    bool gpu_cpu_emulation = false;
    // Toolchain driver used to compile and link GPU device code.
    std::string device_compiler = "nvcc";
    std::string openmp_lib_dir = "";
    bool lookup_name = false;
    bool rename_symbol = false;
    std::string line = "";
    std::string column = "";
    bool continue_compilation = false;
    bool semantics_only = false;
    bool new_classes = true;
    /*
        Generates object code for modules as well as global procedures ( subroutines / functions )
        avialable in ASR. This needs to be explicity set to true.
    */
    bool separate_compilation = false;
    /*
        Generates object code *only* for global procedures ( subroutines / functions ) *if present* in ASR
        by marking modules as external. We have a utility that identifies global procedures and hence this
        option is not exposed to user. It gets set to true if there are any global procedures in ASR.
        This is the default behaviour.

        It is overridden by `generate_object_code` option.
    */
    bool generate_code_for_global_procedures = false;
    bool show_warnings = true;
    bool show_style_suggestions = true;
    bool logical_casting = false;
    bool show_error_banner = true;
    bool bounds_checking = true;
    std::string error_format = "human";
    bool new_parser = false;
    bool implicit_typing = false;
    bool implicit_interface = false;
    bool implicit_argument_casting = false;
    bool infer_mode = false;
    bool print_leading_space = false;
    bool rtlib = false;
    bool use_loop_variable_after_loop = false;
    std::string target = "";
    std::string march = "";
    std::string mcpu = "";
    std::string mtune = "";
    std::string arg_o = "";
    bool emit_debug_info = false;
    bool enable_cpython = false;
    bool enable_symengine = false;
    bool link_numpy = false;
    bool run = false;
    bool legacy_array_sections = false;
    bool ignore_pragma = false;
    bool stack_arrays = false;
    bool internal_alloc_check = false;
    bool descriptor_index_64 = false; // Use 64-bit indices in array descriptors (implied by -fdefault-integer-8)
    bool wasm_html = false;
    bool time_report = false;
    int32_t fpe_traps = 0; // Bitmask of LCOMPILERS_FE_* flags
    std::string emcc_embed;
    std::vector<std::string> import_paths;
    Platform platform;
    bool detect_leaks = false;

    CompilerOptions () : platform{get_platform()} {};
};

bool present(Vec<char*> &v, const char* name);
bool present(char** const v, size_t n, const std::string name);
int initialize();

// Floating point exception trap flags (bitmask)
const int32_t LCOMPILERS_FE_INVALID   = 1;
const int32_t LCOMPILERS_FE_ZERO      = 2;
const int32_t LCOMPILERS_FE_OVERFLOW  = 4;
const int32_t LCOMPILERS_FE_UNDERFLOW = 8;
const int32_t LCOMPILERS_FE_INEXACT   = 16;
const int32_t LCOMPILERS_FE_DENORMAL  = 32;

} // namespace LCompilers

#endif // LIBASR_UTILS_H

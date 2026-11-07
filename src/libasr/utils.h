#ifndef LIBASR_UTILS_H
#define LIBASR_UTILS_H

#include <string>
#include <vector>
#include <filesystem>
#include <libasr/containers.h>

namespace LCompilers {

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
// The kind of the real type that is C `long double` on the target given by
// its triple, or on the host when the triple is empty or not recognized
int get_c_long_double_kind(const std::string &target_triple);

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
    // The kind of C `long double` on the target, which the intrinsic
    // iso_c_binding module gives as `c_long_double`
    int c_long_double_kind = get_c_long_double_kind("");

    std::string run_fun; // for global_stmts pass
    // TODO: Convert to std::filesystem::path (also change find_and_load_module())
    std::string runtime_library_dir;
    bool always_run = false; // for unused_functions pass
    bool inline_external_symbol_calls = true; // for inline_function_calls pass
    bool fast = false; // is fast flag enabled.
    bool no_fast_math = false; // disable fast-math optimizations (NaN, Inf, etc.)
    bool verbose = false; // For developer debugging
    bool dump_all_passes = false; // For developer debugging
    bool dump_fortran = false; // For developer debugging
    bool pass_cumulative = false; // Apply passes cumulatively
    bool verify_all_passes = false; // Verify ASR after every pass
    bool disable_main = false;
    bool realloc_lhs_arrays = false;
    bool stack_arrays = false;
    // Each module is compiled into an object file of its own, so a module
    // read back from a `.mod` file is defined elsewhere and this translation
    // unit only refers to it.
    bool separate_compilation = false;
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
    // The CUDA offload is compiled for an AMD GPU with HIP (`--gpu=hip`).
    bool gpu_offload_hip = false;
    // The CUDA device code runs on an NVIDIA GPU (--gpu=cuda), rather than
    // in the CPU emulation or on an AMD GPU through HIP: its threads run
    // together in warps, which the lanes of an iteration need
    // (gpu_vector_lanes.cpp).
    bool gpu_cuda_warps = false;
    // `!$omp parallel do` asks for host threads. Offloading one onto a device
    // is a choice the user has to make, so it is off unless asked for.
    bool gpu_offload_omp_loops = false;
    // A parallel loop that uses a construct on the unsupported list of the
    // selected device (see gpu_unsupported_check.h) runs on the CPU with a
    // warning instead of being a compile-time error. It never affects a
    // loop the offloading pipeline fails on.
    bool gpu_allow_cpu_fallback = false;
    // The device kernels are only being shown, not built. A loop that uses a
    // construct on the unsupported list then runs on the CPU with a warning,
    // so that the kernels of the other loops are still produced.
    bool gpu_kernel_source_only = false;
    // The loop transformations applied to parallel loops assigned to the
    // device before they are offloaded (gpu_loop_fusion.cpp and the passes
    // after it): "all", "none", or a comma separated list of `fuse`, `swap`,
    // `split`, `jam` and `skew`.
    std::string gpu_loop_transforms = "all";
    // The size in bytes of the device's last-level cache that the loop
    // transformations assume, or -1 to ask the device at run time.
    int64_t gpu_cache_bytes = -1;
    // The fewest iterations of a device loop that index-set splitting
    // splits at run time; 0 splits every one that has an interior.
    int64_t gpu_split_min_iterations = 131072;
    // The most iterations one launch of a kernel runs. A launch of more runs
    // its kernel once per chunk of at most this many (device_launch_expand).
    // Lowered only by tests, to run many chunks over small loops.
    int64_t gpu_chunk_limit = 2147483647;
    bool time_report = false;
    bool skip_removal_of_unused_procedures_in_pass_array_by_data = false;
    // Whether the target can lower descriptor-to-data views in retained entries.
    bool supports_descriptor_entry_wrappers = true;
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
    // The number of activations of a host that can hold a procedure value of
    // one of its internal procedures at the same time (nested_vars)
    int64_t internal_procedure_thunks = 8;
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
    // Compile the generated device code as HIP and run it on an AMD GPU
    // through ROCm (`--gpu=hip`), with the CUDA runtime API of the GPU
    // runtime implemented with HIP.
    bool gpu_hip = false;
    // Toolchain driver used to compile and link GPU device code.
    std::string device_compiler = "nvcc";
    // GPU architecture the device code is compiled for, such as sm_120 or,
    // with --gpu=hip, gfx942. Empty means the GPUs of the machine that
    // compiles.
    std::string gpu_arch = "";
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
    // Diagnose requirements of Fortran 2023 that earlier standards relax
    bool std_f23_conformance = false;
    bool ignore_pragma = false;
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

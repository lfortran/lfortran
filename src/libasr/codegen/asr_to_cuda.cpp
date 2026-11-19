#include <libasr/codegen/asr_to_cuda.h>
#include <libasr/codegen/asr_to_gpu_c.h>
#include <libasr/pass/gpu_decline.h>

namespace LCompilers {

// CUDA C++, the CPU emulation of it that `--gpu=cuda_cpu` compiles as
// ordinary host code, and HIP (`--gpu=hip`), which spells everything this
// dialect emits as CUDA C++ does. The spellings the shared device code
// generator asks a dialect for; everything else it derives from the ASR
// itself.
struct CudaDialect {
    // Whether to emit the trailing kernel-registration shim, which is only
    // meaningful to the runtime's lookup-by-name launch path.
    bool emit_registrar = true;
    // Whether to emit the extra shims the CPU emulation needs. The real CUDA
    // output is left untouched.
    bool emulate_cpu = false;
    // Whether the source is compiled as HIP for an AMD GPU.
    bool hip = false;
    // The integer kind element offsets are computed in
    // (GpuDeviceCapabilities::offset_kind).
    int element_offset_kind = 4;
    // `--gpu-overlap`: every kernel waits for the grid before it in the
    // stream first thing (see emit_kernel_entry).
    bool overlap = false;

    CudaDialect(bool emit_registrar_, bool emulate_cpu_, bool hip_,
            int offset_kind_, bool overlap_ = false)
        : emit_registrar(emit_registrar_), emulate_cpu(emulate_cpu_),
          hip(hip_), element_offset_kind(offset_kind_), overlap(overlap_) {}

    // The runtime launches with programmatic stream serialization, so a
    // kernel may start (its blocks being resident) before the previous grid
    // of the stream has finished and flushed its writes. The kernel does
    // not touch memory until `griddepcontrol.wait` returns, which waits for
    // that grid to complete; it is a no-op if the kernel was launched
    // without a predecessor or without the attribute. A grid lets its
    // dependents start after its main loop (emit_dependents_trigger), or
    // they launch when its blocks exit. The memory clobber keeps the
    // compiler from moving a load above the wait.
    void emit_kernel_entry(std::stringstream &src,
            const std::string &indent) const {
        if (!overlaps_kernels()) return;
        src << "#if defined(__CUDA_ARCH__) && __CUDA_ARCH__ >= 900\n"
            << indent << "asm volatile(\"griddepcontrol.wait;\" ::: "
            << "\"memory\");\n"
            << "#endif\n";
    }

    // Whether kernels overlap the kernel before them (--gpu-overlap).
    bool overlaps_kernels() const {
        return overlap && !emulate_cpu && !hip;
    }

    // Lets the next kernel of the stream start (asr_to_gpu_c.h,
    // kernel_main_loop): it does not touch memory before it waits for this
    // one, which has waited for the one before.
    void emit_dependents_trigger(std::stringstream &src,
            const std::string &indent) const {
        if (!overlaps_kernels()) return;
        src << "#if defined(__CUDA_ARCH__) && __CUDA_ARCH__ >= 900\n"
            << indent << "asm volatile(\"griddepcontrol.launch_dependents;\""
            << " ::: \"memory\");\n"
            << "#endif\n";
    }

    // CUDA has one flat address space for global memory, so a pointer into it
    // needs no qualifier at all; only the two special spaces are named.
    std::string addr_space(ASR::memory_spaceType space) const {
        switch (space) {
            case ASR::memory_spaceType::Global: return "";
            case ASR::memory_spaceType::Shared: return "__shared__";
            case ASR::memory_spaceType::Constant: return "__constant__";
            case ASR::memory_spaceType::Thread: return "";
        }
        return "";
    }

    std::string real_type(int kind) const {
        return (kind == 8) ? "double" : "float";
    }

    // The integer kind an element's offset in an array is computed in:
    // 8 with --descriptor-index-64, the flag that selects 64-bit array
    // indexing, and 4 otherwise, which is cheaper to compute in.
    int offset_kind() const {
        return element_offset_kind;
    }

    void emit_prologue(std::stringstream &src) const {
        src << "#include <stdint.h>\n";
        if (hip) {
            // nvcc includes the CUDA runtime header itself; the HIP driver
            // needs the HIP one named.
            src << "#include <hip/hip_runtime.h>\n";
        }
        if (emulate_cpu) {
            // Turns the CUDA execution space qualifiers and the built-in
            // thread coordinates into plain host C++.
            src << "#include \"cuda_cpu_device.h\"\n";
            src << "#include <algorithm>\n";
            src << "#include <cmath>\n";
            // The shared generator spells the maths unqualified, the way a
            // shading language does; on the host they live in namespace std.
            for (const char *fn : {"abs", "exp", "sqrt", "sin", "cos",
                    "fmod", "fma", "pow", "min", "max", "round", "trunc"}) {
                src << "using std::" << fn << ";\n";
            }
        }
        src << "\n";
    }

    std::string device_fn_qualifier() const {
        return "__device__ inline ";
    }

    // The most threads of a block the launches use (the device's
    // threads_per_block). For a kernel whose threads stream down the rows of
    // a matrix (streams_strided_rows), also that one block per
    // multiprocessor is enough: without it, ptxas keeps the kernel to the
    // registers that let eight blocks share a multiprocessor, and then
    // cannot keep the loads of an unrolled loop in flight together (a
    // matrix-vector product ran at a third of the memory bandwidth). Other
    // kernels keep the occupancy, which a gather needs to hide its loads (a
    // sparse matrix-vector product lost a quarter of its speed with the
    // bound). The CPU emulation's host compiler knows no such attribute, and
    // HIP, tuned on AMD GPUs without it, is left as it was.
    //
    // A kernel whose lanes read vectors of 16 bytes (`lane_vectors`, the
    // most vectors an iteration of one of its loops reads) is held to the
    // registers that let as many of its blocks share a multiprocessor as
    // keep those loads in flight together: each vector lands in 4
    // registers and is decoded into about as many more, and the addresses,
    // counters and accumulators take about 8, so a thread needs
    // 8 + 8*vectors of the 65536 registers of a multiprocessor. That is at
    // most the 1536 of its 2048 threads, six blocks of 256: a bound that
    // lets ptxas cut the registers to run all 2048 leaves the loads too few
    // registers, and one block per multiprocessor too few threads to hide
    // them. On an H200, a q8 matrix-vector product of 34816 by 5120 that
    // takes 1, 2 or 4 rows per iteration (5, 6 or 8 vectors) runs fastest
    // with 4 to 6, 4 and 3 blocks (5, 4 and 3 here), the 4-row one in
    // 61 us with 3 blocks and 92 us with 6, where ptxas spills registers to
    // fit 40; one that reads 4 vectors (two chunks of a row per iteration)
    // keeps six.
    std::string kernel_qualifier(int64_t threads_per_block,
            bool few_blocks, int64_t lane_vectors = 0) const {
        if (emulate_cpu || hip) return "extern \"C\" __global__ void ";
        std::string min_blocks;
        if (few_blocks) {
            min_blocks = ", 1";
        } else if (lane_vectors > 0 && threads_per_block > 0
                && threads_per_block <= 1536) {
            min_blocks = ", " + std::to_string(lane_vector_blocks(
                threads_per_block, lane_vectors));
        }
        return launch_bounds(threads_per_block, min_blocks);
    }

    // The blocks of a kernel whose lanes read vectors of 16 bytes that
    // kernel_qualifier lets share a multiprocessor.
    static int64_t lane_vector_blocks(int64_t threads_per_block,
            int64_t lane_vectors) {
        int64_t registers = 8 + 8*lane_vectors;
        return std::max<int64_t>(1, std::min<int64_t>(
            1536 / threads_per_block,
            65536 / (threads_per_block*registers)));
    }

    static std::string launch_bounds(int64_t threads_per_block,
            const std::string &min_blocks) {
        return "extern \"C\" __global__ void __launch_bounds__("
            + std::to_string(threads_per_block) + min_blocks + ") ";
    }

    // The qualifier of a second version of a kernel whose lanes read
    // vectors of 16 bytes, held to the registers of the next step of
    // occupancy, which lets more of its blocks share a multiprocessor, and
    // the blocks: empty and zero when there is none. A thread is given
    // registers in steps of 8, so the next step has 8 fewer than the
    // bound of kernel_qualifier leaves. The runtime launches whichever
    // version fills the waves of the multiprocessors better for the
    // blocks of the launch (lfortran_gpu_cuda.c): on an H200, the q8
    // matrix-vector product of 16,480 rows (4 per block, 14 or 16 blocks
    // per multiprocessor, 2.23 or 1.95 waves of 132) took 28.0 us with 14
    // and 27.0 with 16, while one of 34,816 rows (4.7 or 4.1 waves) took
    // 52 and 55.
    std::pair<std::string, int64_t> occupancy_variant(
            int64_t threads_per_block, bool few_blocks,
            int64_t lane_vectors) const {
        if (emulate_cpu || hip || few_blocks || lane_vectors <= 0
                || threads_per_block <= 0 || threads_per_block > 1536) {
            return {"", 0};
        }
        int64_t blocks = lane_vector_blocks(threads_per_block, lane_vectors);
        int64_t registers = 65536 / (threads_per_block*blocks) / 8 * 8 - 8;
        if (registers < 32) return {"", 0};
        int64_t more = std::min<int64_t>(1536 / threads_per_block,
            65536 / (threads_per_block*registers));
        if (more <= blocks) return {"", 0};
        return {launch_bounds(threads_per_block, ", " + std::to_string(more)),
            more};
    }

    // A kernel parameter is bound by position, not to a named buffer slot.
    std::string buffer_attr(int /*index*/) const {
        return "";
    }

    // A buffer no other buffer of the launch aliases. nvcc then reads one
    // the kernel does not store to through the read-only data path, and
    // keeps a value it loaded across a store to another buffer.
    std::string noalias_qualifier() const {
        return "__restrict__ ";
    }

    // The grid position is a built-in rather than a parameter, so the kernel
    // takes none and derives it in its first statement instead.
    void emit_thread_id_param(std::stringstream & /*src*/,
            bool /*has_prev*/) const {}

    // A kernel whose iterations each run on several lanes (see
    // gpu_vector_lanes.cpp; only on an NVIDIA GPU, whose warps run them)
    // takes no parameters for it: the lanes of an iteration are
    // consecutive threads of a warp, `lanes` (a power of two up to 32)
    // dividing the block, and the prologue numbers the iterations by
    // dividing the thread index and the block size by `lanes`. With fewer
    // than 32 lanes a warp runs several iterations, so a barrier or a
    // shuffle names the lanes of its own iteration (__gpu_lane_mask) and
    // does not wait for the others.
    void emit_lane_thread_params(std::stringstream & /*src*/,
            bool /*has_prev*/) const {}
    void emit_lane_prologue(std::stringstream &src, const std::string &indent,
            const std::string &lanes, int64_t fixed_lanes = 0) const {
        // A kernel that every launch gives the same lanes (fixed_lanes,
        // gpu_vector_lanes.cpp) names them as a constant: its loops step
        // by a constant, which nvcc turns into constant address increments.
        if (fixed_lanes > 0) {
            src << indent << lanes << " = " << fixed_lanes << ";\n";
        }
        src << indent << "int __gpu_lanes_n = " << lanes << ";\n";
        src << indent << "unsigned __gpu_lane_mask = __gpu_lanes_n >= 32 ? "
            "0xffffffffu : (((1u << __gpu_lanes_n) - 1u) << "
            "(threadIdx.x & 31u & ~(unsigned)(__gpu_lanes_n - 1)));\n";
        src << indent << "int __gpu_lane = (int)(threadIdx.x % "
            "(unsigned)__gpu_lanes_n);\n";
        src << indent << "unsigned __gpu_iteration_thread = threadIdx.x / "
            "(unsigned)__gpu_lanes_n;\n";
        src << indent << "unsigned __gpu_iteration_block = blockDim.x / "
            "(unsigned)__gpu_lanes_n;\n";
    }
    // The test of the lanes most launches give a kernel whose lanes are a
    // run-time value (gpu_vector_lanes.cpp): a warp, or all of them when
    // it never gets as many.
    std::string common_lanes_test(const std::string &lanes,
            int64_t lanes_max) const {
        if (emulate_cpu || hip) return "";
        return "__scalar_args." + lanes + " == "
            + std::to_string(std::min<int64_t>(lanes_max, 32));
    }
    std::string lane_index() const { return "__gpu_lane"; }
    std::string lane_thread_index() const { return "__gpu_iteration_thread"; }
    std::string lane_block_size() const { return "__gpu_iteration_block"; }
    // The value in the first lane of the iteration. With one lane per
    // iteration the threads of a warp run different iterations, which need
    // not reach the same shuffle, and the value is the thread's own.
    std::string lane_broadcast(const std::string &value, bool block) const {
        return std::string(block ? "__lfortran_lane_broadcast_block("
            : "__lfortran_lane_broadcast(") + value
            + ", __gpu_lanes_n, __gpu_lane_mask)";
    }
    // The lanes wait for each other: the threads of a warp, or, when the
    // lanes may be the whole block (more than a warp), the block.
    void emit_lane_barrier(std::stringstream &src, const std::string &indent,
            const std::string & /*lanes*/, bool block) const {
        src << indent << (block ? "__lfortran_lane_barrier_block("
            : "__lfortran_lane_barrier(") << "__gpu_lanes_n, "
            "__gpu_lane_mask);\n";
    }
    // The threads of the block wait for each other, in the kernel's own
    // code: a kernel whose lanes are all the threads of a block at every
    // launch (gpu_vector_lanes.cpp) keeps the elements of a column in
    // registers across its barriers, which a call to
    // __lfortran_block_barrier would have the device compiler keep apart
    // from it (an update of the state of a gated delta rule took 5% longer).
    void emit_block_barrier(std::stringstream &src,
            const std::string &indent) const {
        src << indent << "__syncthreads();\n";
    }
    // What the lanes of the iteration hold, combined, in every lane
    // (__lfortran_lane_reduce in the prologue of the kernel source).
    // In a kernel without lanes, the lanes are the 32 threads of the warp.
    std::string lane_reduce(const std::string &value, int op,
            bool has_lanes, bool block) const {
        return std::string(has_lanes && block ? "__lfortran_lane_reduce_block<"
            : "__lfortran_lane_reduce<") + std::to_string(op) + ">(" + value
            + ", " + (has_lanes ? "__gpu_lanes_n, __gpu_lane_mask"
                : "32, 0xffffffffu") + ")";
    }
    // What each group of `width` consecutive lanes of a warp hold,
    // combined, in each of them: all the threads of the warp run it.
    std::string lane_group_reduce(const std::string &value, int op,
            int64_t width) const {
        return "__lfortran_lane_reduce<" + std::to_string(op) + ">(" + value
            + ", " + std::to_string(width) + ", 0xffffffffu)";
    }
    // Whether consecutive combinations over the lanes of a whole block
    // share one barrier (emit_stmts): not in the CPU emulation or on HIP,
    // whose blocks have no lanes of more than a warp.
    bool batched_block_reduce() const {
        return !emulate_cpu && !hip;
    }
    // `targets` are assigned the elements that start at `source` (`a[i]`),
    // read with __lfortran_wide_load in the prologue of the kernel source.
    // A `pinned` vector is read where the statement is: nvcc otherwise
    // spreads the reads of the rows of a step of a matrix-vector product
    // across its arithmetic, where fewer of them are in flight at a time
    // (the gate and up projections of Qwen3.8-27B took 3% longer).
    std::string wide_load(const std::string &indent,
            const std::vector<std::string> &targets,
            const std::string &source, const std::string &array,
            const std::string &aligned, bool assume, bool pinned) const {
        std::string out = indent + "__lfortran_wide_load"
            + (pinned && !emulate_cpu && !hip ? "<true>" : "") + "("
            + (assume ? std::string("true") : wide_aligned(array, aligned))
            + ", &(" + source + ")";
        for (const std::string &t : targets) out += ", " + t;
        return out + ");\n";
    }
    // Whether `array` starts at a multiple of 16 bytes and `aligned` holds.
    std::string wide_aligned(const std::string &array,
            const std::string &aligned) const {
        return "((" + aligned + ") && (reinterpret_cast<size_t>(" + array
            + ") & 15u) == 0)";
    }
    std::string wide_load_helper() const {
        return
            "// The 16 bytes that start at `p`: read at once when `ok` says `p` is a\n"
            "// multiple of 16, element by element, put together, when it is not.\n"
            "// The elements are taken out of the 16 bytes, so that the two ways\n"
            "// meet before the elements are separated. A `pinned` vector is read\n"
            "// in the order of the statements (asm volatile).\n"
            "template <bool pinned = false, typename T>\n"
            "__device__ __forceinline__ uint4 __lfortran_wide_vector(bool ok, const T* p) {\n"
            "    if (ok) {\n"
            + std::string(emulate_cpu || hip ? "" :
            "        if (pinned) {\n"
            "            uint4 v;\n"
            "            asm volatile(\"ld.global.v4.u32 {%0,%1,%2,%3}, [%4];\"\n"
            "                : \"=r\"(v.x), \"=r\"(v.y), \"=r\"(v.z), \"=r\"(v.w) : \"l\"(p));\n"
            "            return v;\n"
            "        }\n")
            + "        return *reinterpret_cast<const uint4*>(p);\n"
            "    }\n"
            "    T t[16 / sizeof(T)];\n"
            "    #pragma unroll\n"
            "    for (int i = 0; i < 16 / (int)sizeof(T); i++) t[i] = p[i];\n"
            "    uint4 v;\n"
            "    __builtin_memcpy(&v, t, 16);\n"
            "    return v;\n"
            "}\n\n"
            "template <bool pinned = false, typename T>\n"
            "__device__ __forceinline__ void __lfortran_wide_load(bool ok, const T* p, T &a, T &b) {\n"
            "    uint4 v = __lfortran_wide_vector<pinned>(ok, p);\n"
            "    const T* e = reinterpret_cast<const T*>(&v);\n"
            "    a = e[0]; b = e[1];\n"
            "}\n\n"
            "template <bool pinned = false, typename T>\n"
            "__device__ __forceinline__ void __lfortran_wide_load(bool ok, const T* p, T &a, T &b, T &c, T &d) {\n"
            "    uint4 v = __lfortran_wide_vector<pinned>(ok, p);\n"
            "    const T* e = reinterpret_cast<const T*>(&v);\n"
            "    a = e[0]; b = e[1]; c = e[2]; d = e[3];\n"
            "}\n\n"
            "template <bool pinned = false, typename T>\n"
            "__device__ __forceinline__ void __lfortran_wide_load(bool ok, const T* p, T &a0, T &a1, T &a2, T &a3, T &a4, T &a5, T &a6, T &a7) {\n"
            "    uint4 v = __lfortran_wide_vector<pinned>(ok, p);\n"
            "    const T* e = reinterpret_cast<const T*>(&v);\n"
            "    a0 = e[0]; a1 = e[1]; a2 = e[2]; a3 = e[3]; a4 = e[4]; a5 = e[5]; a6 = e[6]; a7 = e[7];\n"
            "}\n\n";
    }
    // The definition lane_reduce names, written once ahead of the kernels.
    std::string lane_reduce_helper() const {
        return
            "// The lanes of an iteration are consecutive threads of a warp, or,\n"
            "// with more than 32 of them, all the threads of a block.\n"
            "template <int op, typename T>\n"
            "__device__ inline T __lfortran_lane_combine(T v, T w) {\n"
            "    return op == 0 ? v + w : op == 1 ? v * w\n"
            "        : op == 2 ? (w < v ? w : v) : (w > v ? w : v);\n"
            "}\n\n"
            "// The lanes are the warps of a block: each combines its own lanes,\n"
            "// and every lane then folds the partial results of the warps in the\n"
            "// same order.\n"
            "template <int op, typename T>\n"
            "__device__ __noinline__ T __lfortran_block_reduce(T v, int lanes) {\n"
            "    #pragma unroll\n"
            "    for (int offset = 16; offset > 0; offset >>= 1) {\n"
            "        v = __lfortran_lane_combine<op>(v,\n"
            "            __shfl_xor_sync(0xffffffffu, v, offset, 32));\n"
            "    }\n"
            "    __shared__ T partial[32];\n"
            "    if ((threadIdx.x & 31u) == 0) partial[threadIdx.x >> 5] = v;\n"
            "    __syncthreads();\n"
            "    T r = partial[0];\n"
            "    for (int i = 1; i < (lanes >> 5); i++) {\n"
            "        r = __lfortran_lane_combine<op>(r, partial[i]);\n"
            "    }\n"
            "    __syncthreads();\n"
            "    return r;\n"
            "}\n\n"
            "// The combination of what the `lanes` lanes of an iteration hold,\n"
            "// in every lane: 0 adds, 1 multiplies, 2 takes the smallest, 3 the\n"
            "// largest. With one lane, the lane's own value.\n"
            "template <int op, typename T>\n"
            "__device__ inline T __lfortran_lane_reduce(T v, int lanes, unsigned mask) {\n"
            "    if (lanes == 32) {\n"
            "        // A whole warp, named as a constant: with the mask in a\n"
            "        // register nvcc tests every shuffle for diverged lanes.\n"
            "        #pragma unroll\n"
            "        for (int offset = 16; offset > 0; offset >>= 1) {\n"
            "            v = __lfortran_lane_combine<op>(v,\n"
            "                __shfl_xor_sync(0xffffffffu, v, offset, 32));\n"
            "        }\n"
            "        return v;\n"
            "    }\n"
            "    if (lanes == 16) {\n"
            "        // The common sizes, with the shuffles unrolled.\n"
            "        #pragma unroll\n"
            "        for (int offset = 16; offset > 0; offset >>= 1) {\n"
            "            if (offset < lanes) {\n"
            "                v = __lfortran_lane_combine<op>(v,\n"
            "                    __shfl_xor_sync(mask, v, offset, lanes));\n"
            "            }\n"
            "        }\n"
            "        return v;\n"
            "    }\n"
            "    for (int offset = lanes >> 1; offset > 0; offset >>= 1) {\n"
            "        v = __lfortran_lane_combine<op>(v,\n"
            "            __shfl_xor_sync(mask, v, offset, lanes));\n"
            "    }\n"
            "    return v;\n"
            "}\n\n"
            "// For a kernel whose iterations may have the lanes of a block. Its\n"
            "// barriers keep the other kernels from reordering their loads, so\n"
            "// the others do not call these.\n"
            "template <int op, typename T>\n"
            "__device__ inline T __lfortran_lane_reduce_block(T v, int lanes, unsigned mask) {\n"
            "    if (lanes > 32) return __lfortran_block_reduce<op>(v, lanes);\n"
            "    return __lfortran_lane_reduce<op>(v, lanes, mask);\n"
            "}\n\n"
            "__device__ __noinline__ void __lfortran_block_barrier() {\n"
            "    __syncthreads();\n"
            "}\n\n"
            "// Waits for the other lanes of the iteration.\n"
            "__device__ inline void __lfortran_lane_barrier(int lanes, unsigned mask) {\n"
            "    if (lanes > 1) __syncwarp(mask);\n"
            "}\n\n"
            "__device__ inline void __lfortran_lane_barrier_block(int lanes, unsigned mask) {\n"
            "    if (lanes > 32) __lfortran_block_barrier();\n"
            "    else if (lanes > 1) __syncwarp(mask);\n"
            "}\n\n"
            "template <typename T>\n"
            "__device__ __noinline__ T __lfortran_block_broadcast(T v, int lanes) {\n"
            "    __shared__ T first;\n"
            "    if ((threadIdx.x % (unsigned)lanes) == 0) first = v;\n"
            "    __syncthreads();\n"
            "    T r = first;\n"
            "    __syncthreads();\n"
            "    return r;\n"
            "}\n\n"
            "// The value of the first lane of the iteration, in every lane.\n"
            "template <typename T>\n"
            "__device__ inline T __lfortran_lane_broadcast(T v, int lanes, unsigned mask) {\n"
            "    return lanes > 1 ? __shfl_sync(mask, v, 0, lanes) : v;\n"
            "}\n\n"
            "template <typename T>\n"
            "__device__ inline T __lfortran_lane_broadcast_block(T v, int lanes, unsigned mask) {\n"
            "    if (lanes > 32) return __lfortran_block_broadcast(v, lanes);\n"
            "    return __lfortran_lane_broadcast(v, lanes, mask);\n"
            "}\n\n";
    }

    // The definitions a GpuReduceCombine names, written once ahead of the
    // kernels.
    std::string reduce_combine_helper() const {
        std::string h =
            "// The operation of a reduction of elements of arrays: 0 adds, 1\n"
            "// multiplies, 2 takes the smaller, 3 the larger.\n"
            "template <int op, typename T>\n"
            "__device__ inline T __lfortran_reduce_op(T v, T w) {\n"
            "    return op == 0 ? v + w : op == 1 ? v * w\n"
            "        : op == 2 ? (w < v ? w : v) : (w > v ? w : v);\n"
            "}\n\n";
        std::string arrive =
            "// Whether this is the last of the `units` arrivals at the counter,\n"
            "// the first element of the scratch buffer, which it then leaves\n"
            "// zero for the next launch.\n";
        if (emulate_cpu) {
            return h + arrive +
                "// The threads run one after another.\n"
                "__device__ inline bool __lfortran_reduce_arrive(void *scratch,\n"
                "        long long units) {\n"
                "    unsigned int *counter = (unsigned int*)scratch;\n"
                "    if ((long long)++*counter < units) return false;\n"
                "    *counter = 0u;\n"
                "    return true;\n"
                "}\n\n";
        }
        return "#include <cooperative_groups.h>\n"
            "#include <cooperative_groups/reduce.h>\n\n" + h + arrive +
            "// What a unit wrote before it arrives is seen by the last one\n"
            "// after it arrives: the add releases and acquires.\n"
            "__device__ inline bool __lfortran_reduce_arrive(void *scratch,\n"
            "        long long units) {\n"
            "    unsigned int *counter = (unsigned int*)scratch;\n"
            "    unsigned int done;\n"
            "#if defined(__CUDA_ARCH__) && __CUDA_ARCH__ >= 700\n"
            "    asm volatile(\"atom.add.acq_rel.gpu.u32 %0, [%1], 1;\"\n"
            "        : \"=r\"(done) : \"l\"(counter) : \"memory\");\n"
            "#else\n"
            "    __threadfence();\n"
            "    done = atomicAdd(counter, 1u);\n"
            "    __threadfence();\n"
            "#endif\n"
            "    if ((long long)done < units - 1) return false;\n"
            "    *counter = 0u;\n"
            "    return true;\n"
            "}\n\n"
            "// `v` combined over the 32 threads of a warp, in every one.\n"
            "template <int op, typename T>\n"
            "__device__ inline T __lfortran_reduce_warp(T v) {\n"
            "    #pragma unroll\n"
            "    for (int offset = 16; offset > 0; offset >>= 1) {\n"
            "        v = __lfortran_reduce_op<op>(v,\n"
            "            __shfl_xor_sync(0xffffffffu, v, offset, 32));\n"
            "    }\n"
            "    return v;\n"
            "}\n\n"
            "// The target combined with `v` by an atomic update, for the\n"
            "// operations whose result does not depend on the order: an integer\n"
            "// sum, and the larger or the smaller of integers (natively) or of\n"
            "// reals (by compare and swap of their bits).\n"
            "template <int op>\n"
            "__device__ inline void __lfortran_reduce_atomic(int *p, int v) {\n"
            "    if (op == 0) atomicAdd(p, v);\n"
            "    else if (op == 2) atomicMin(p, v);\n"
            "    else atomicMax(p, v);\n"
            "}\n\n"
            "template <int op>\n"
            "__device__ inline void __lfortran_reduce_atomic(long long *p, long long v) {\n"
            "    if (op == 0) atomicAdd((unsigned long long*)p, (unsigned long long)v);\n"
            "    else if (op == 2) atomicMin(p, v);\n"
            "    else atomicMax(p, v);\n"
            "}\n\n"
            "template <int op>\n"
            "__device__ inline void __lfortran_reduce_atomic(long *p, long v) {\n"
            "    __lfortran_reduce_atomic<op>((long long*)p, (long long)v);\n"
            "}\n\n"
            "template <int op, typename T, typename U>\n"
            "__device__ inline void __lfortran_reduce_atomic_bits(T *p, T v) {\n"
            "    U *q = (U*)p;\n"
            "    U old = *(volatile U*)q, assumed;\n"
            "    do {\n"
            "        assumed = old;\n"
            "        T current;\n"
            "        __builtin_memcpy(&current, &assumed, sizeof(T));\n"
            "        T next = __lfortran_reduce_op<op>(current, v);\n"
            "        U bits;\n"
            "        __builtin_memcpy(&bits, &next, sizeof(T));\n"
            "        if (bits == assumed) return;\n"
            "        old = atomicCAS(q, assumed, bits);\n"
            "    } while (old != assumed);\n"
            "}\n\n"
            "template <int op>\n"
            "__device__ inline void __lfortran_reduce_atomic(float *p, float v) {\n"
            "    __lfortran_reduce_atomic_bits<op, float, unsigned int>(p, v);\n"
            "}\n\n"
            "template <int op>\n"
            "__device__ inline void __lfortran_reduce_atomic(double *p, double v) {\n"
            "    __lfortran_reduce_atomic_bits<op, double, unsigned long long>(p, v);\n"
            "}\n\n"
            "// The threads of a warp that call it together combine their `v`, and\n"
            "// the first of them updates the target: one atomic update per warp.\n"
            "template <int op, typename T>\n"
            "__device__ inline void __lfortran_reduce_atomic_group(T *p, T v) {\n"
            "    namespace cg = cooperative_groups;\n"
            "    cg::coalesced_group g = cg::coalesced_threads();\n"
            "    T r;\n"
            "    if (op == 0) r = cg::reduce(g, v, cg::plus<T>());\n"
            "    else if (op == 2) r = cg::reduce(g, v, cg::less<T>());\n"
            "    else r = cg::reduce(g, v, cg::greater<T>());\n"
            "    if (g.thread_rank() == 0) __lfortran_reduce_atomic<op>(p, r);\n"
            "}\n\n"
            "// `v` combined over the threads of the block, in every one: each warp\n"
            "// combines its own, and the warps' meet in shared memory in their\n"
            "// order.\n"
            "template <int op, typename T>\n"
            "__device__ __noinline__ T __lfortran_reduce_block(T v) {\n"
            "    v = __lfortran_reduce_warp<op>(v);\n"
            "    __shared__ T partial[32];\n"
            "    if ((threadIdx.x & 31u) == 0) partial[threadIdx.x >> 5] = v;\n"
            "    __syncthreads();\n"
            "    T r = partial[0];\n"
            "    for (unsigned i = 1; i < (blockDim.x >> 5); i++) {\n"
            "        r = __lfortran_reduce_op<op>(r, partial[i]);\n"
            "    }\n"
            "    __syncthreads();\n"
            "    return r;\n"
            "}\n\n";
    }

    // A GpuReduceCombine: the threads numbered below `threads` (as the
    // kernel numbers them: of a kernel with `lanes`, its iterations, whose
    // first lane holds the values) combine their `items` into the targets.
    //
    // An item whose operation gives the same result in any order
    // (gpu_reduce_item_atomic: an integer sum, a maximum, a minimum) is
    // combined over the threads of a warp that have a value and reach it
    // together, and the first of them adds it to the target with one atomic
    // update: one per warp, or per iteration where an iteration has the
    // lanes of a whole block. The result is the same whatever the order of
    // the updates, and nothing else is needed: the threads that have no
    // iterations need not reach it, as in a kernel that runs one iteration
    // per thread (gpu_offload.cpp).
    //
    // For the others, the unit is a block: its threads combine their
    // values in a fixed order (__lfortran_reduce_block; a block that runs
    // one iteration on all its threads has the value in its first thread
    // already), its first thread writes them to its part of `scratch`,
    // after the counter, and arrives at the counter; the block that arrives
    // last has its first warp combine the parts of all blocks, each lane
    // every 32nd in the order of the blocks and the lanes in a fixed order,
    // and apply them. Every thread of the launch reaches it, so the
    // barriers of a block are met; a block whose threads are all past
    // `threads` has nothing to combine and does not count. The parts are
    // read past the cache of the multiprocessor (__ldcg), which may hold
    // what another launch left there. On an H200, a launch of 1280 blocks
    // that finish together takes about 2 us longer for it than one that
    // only stores its results: the arrivals queue at the counter, and the
    // last block reads the parts after its own arrival.
    //
    // The CPU emulation, which runs the threads one after another and has
    // no barriers, combines each thread's values into an atomic item's
    // target directly, and makes each thread a unit of the others.
    void emit_reduce_combine(std::stringstream &src, const std::string &in,
            const std::vector<GpuReduceCombineItem> &all_items,
            const std::string &threads, const std::string &scratch,
            bool lanes) const {
        std::vector<GpuReduceCombineItem> atomic, items;
        for (const GpuReduceCombineItem &item : all_items) {
            (item.atomic ? atomic : items).push_back(item);
        }
        std::string m = std::to_string(items.size());
        auto part = [&](const std::string &unit, size_t j) {
            return "((" + items[j].type + "*)(" + scratch + " + 1 + ("
                + unit + ") * " + m + " + " + std::to_string(j) + "))";
        };
        auto op_of = [](const GpuReduceCombineItem &item) {
            return "__lfortran_reduce_op<" + std::to_string(item.op) + ">(";
        };
        auto combine = [&](size_t j, const std::string &into,
                const std::string &value) {
            return into + " = " + op_of(items[j]) + into + ", " + value
                + ");\n";
        };
        // The target, from what it starts from.
        auto apply = [&](size_t j) {
            return items[j].target + " = " + op_of(items[j])
                + (items[j].start.empty() ? items[j].target : items[j].start)
                + ", __rc_total" + std::to_string(j) + ");\n";
        };
        std::string i1 = in + "    ", i2 = i1 + "    ", i3 = i2 + "    ",
            i4 = i3 + "    ";
        src << in << "{\n";
        src << i1 << "const long long __rc_threads = (long long)(" << threads
            << ");\n";
        if (emulate_cpu) {
            src << i1 << "const long long __rc_slot = (long long)blockIdx.x "
                "* blockDim.x + threadIdx.x;\n";
            src << i1 << "if (__rc_slot < __rc_threads) {\n";
            for (const GpuReduceCombineItem &item : atomic) {
                src << i2 << item.target << " = " << op_of(item)
                    << item.target << ", " << item.value << ");\n";
            }
            if (!items.empty()) {
                for (size_t j = 0; j < items.size(); j++) {
                    src << i2 << "*" << part("__rc_slot", j) << " = "
                        << items[j].value << ";\n";
                }
                src << i2 << "if (__lfortran_reduce_arrive(" << scratch
                    << ", __rc_threads)) {\n";
                for (size_t j = 0; j < items.size(); j++) {
                    src << i3 << items[j].type << " __rc_total" << j << " = "
                        << items[j].identity << ";\n";
                }
                src << i3 << "for (long long __rc_u = 0; __rc_u < "
                    "__rc_threads; __rc_u++) {\n";
                for (size_t j = 0; j < items.size(); j++) {
                    src << i4 << combine(j, "__rc_total" + std::to_string(j),
                        "*" + part("__rc_u", j));
                }
                src << i3 << "}\n";
                for (size_t j = 0; j < items.size(); j++) {
                    src << i3 << apply(j);
                }
                src << i2 << "}\n";
            }
            src << i1 << "}\n" << in << "}\n";
            return;
        }
        src << i1 << "const bool __rc_take = (long long)"
            << (lanes ? lane_global_thread_id() : global_thread_id())
            << " < __rc_threads" << (lanes ? " && " + lane_index() + " == 0"
                : std::string()) << ";\n";
        if (!atomic.empty()) {
            // An iteration on a warp of lanes or more is alone in its warp
            // to have a value: its first lane updates the target itself.
            auto updates = [&](const std::string &indent,
                    const std::string &helper) {
                for (const GpuReduceCombineItem &item : atomic) {
                    src << indent << helper << "<" << item.op << ">(&("
                        << item.target << "), " << item.value << ");\n";
                }
            };
            src << i1 << "if (__rc_take) {\n";
            if (lanes) {
                src << i2 << "if (__gpu_lanes_n >= 32) {\n";
                updates(i3, "__lfortran_reduce_atomic");
                src << i2 << "} else {\n";
                updates(i3, "__lfortran_reduce_atomic_group");
                src << i2 << "}\n";
            } else {
                updates(i2, "__lfortran_reduce_atomic_group");
            }
            src << i1 << "}\n";
        }
        if (items.empty()) {
            src << in << "}\n";
            return;
        }
        std::string i5 = i4 + "    ";
        src << i1 << "const long long __rc_per_block = (long long)"
            << (lanes ? lane_block_size() : block_size()) << ";\n";
        src << i1 << "const long long __rc_units = (__rc_threads + "
            "__rc_per_block - 1) / __rc_per_block;\n";
        src << i1 << "if ((long long)blockIdx.x < __rc_units) {\n";
        for (size_t j = 0; j < items.size(); j++) {
            src << i2 << items[j].type << " __rc_value" << j
                << " = __rc_take ? (" << items[j].value << ") : "
                << items[j].identity << ";\n";
        }
        // A block of one iteration, whose lanes are all its threads, has
        // its value in its first thread already.
        src << i2 << "if (__rc_per_block > 1) {\n";
        for (size_t j = 0; j < items.size(); j++) {
            src << i3 << "__rc_value" << j << " = __lfortran_reduce_block<"
                << items[j].op << ">(__rc_value" << j << ");\n";
        }
        src << i2 << "}\n";
        src << i2 << "if (threadIdx.x < 32u) {\n";
        src << i3 << "int __rc_last = 0;\n";
        src << i3 << "if (threadIdx.x == 0) {\n";
        for (size_t j = 0; j < items.size(); j++) {
            src << i4 << "*" << part("(long long)blockIdx.x", j)
                << " = __rc_value" << j << ";\n";
        }
        src << i4 << "__rc_last = __lfortran_reduce_arrive(" << scratch
            << ", __rc_units);\n";
        src << i3 << "}\n";
        // The first warp of the last block combines the parts of all
        // blocks; the warp barrier orders its loads after the arrival.
        src << i3 << "__syncwarp();\n";
        src << i3 << "if (__shfl_sync(0xffffffffu, __rc_last, 0)) {\n";
        for (size_t j = 0; j < items.size(); j++) {
            src << i4 << items[j].type << " __rc_total" << j << " = "
                << items[j].identity << ";\n";
        }
        src << i4 << "for (long long __rc_u = threadIdx.x; __rc_u < "
            "__rc_units; __rc_u += 32) {\n";
        for (size_t j = 0; j < items.size(); j++) {
            src << i5 << combine(j, "__rc_total" + std::to_string(j),
                "__ldcg(" + part("__rc_u", j) + ")");
        }
        src << i4 << "}\n";
        for (size_t j = 0; j < items.size(); j++) {
            src << i4 << "__rc_total" << j << " = __lfortran_reduce_warp<"
                << items[j].op << ">(__rc_total" << j << ");\n";
        }
        src << i4 << "if (threadIdx.x == 0) {\n";
        for (size_t j = 0; j < items.size(); j++) {
            src << i5 << apply(j);
        }
        src << i4 << "}\n" << i3 << "}\n" << i2 << "}\n" << i1 << "}\n"
            << in << "}\n";
    }

    // Small innermost loops are unrolled by 16, as nvfortran does, so that
    // a thread keeps that many iterations' loads in flight. The CPU
    // emulation's host compiler has its own unrolling, and HIP unrolls such
    // loops four times in the gpu_sequential_unroll pass instead.
    // A thread that streams down a row of a matrix the threads of the launch
    // read a column of together (`deep`) has few threads to hide each load:
    // a matrix-vector product of 16,384 rows reads 2.7 TB/s on an H200 with 16
    // iterations in flight, 3.9 TB/s with 64.
    std::string unroll_pragma(bool deep = false) const {
        return emulate_cpu || hip ? std::string()
            : std::string(deep ? "#pragma unroll 64" : "#pragma unroll 16");
    }
    // A loop whose iterations read vectors of 16 bytes (gpu_wide_loads.cpp)
    // has the loads of 16 elements in each already: unrolled by 2, a loop
    // of a quantized matrix-vector product keeps its loads in flight with
    // few registers; by 4 it takes 64 and the SM runs a third fewer warps
    // (the product of 17408 rows takes 15% longer).
    // A kernel whose lanes read such vectors (`lane_vectors`) does not
    // unroll it: its lanes have the next vectors to load in flight with the
    // registers of the bound of kernel_qualifier, which an unrolled loop
    // takes for its copies (a quantized matrix-vector product read 3%
    // faster over its launches, together with the bound).
    std::string unroll_wide_pragma(bool lane_vectors = false) const {
        return emulate_cpu || hip ? std::string()
            : std::string(lane_vectors ? "#pragma unroll 1"
                                       : "#pragma unroll 2");
    }
    // A loop that indexes a private array by its variable, of few
    // iterations known at compile time, is unrolled whole, so that the
    // array lives in registers.
    std::string unroll_whole_pragma() const {
        return emulate_cpu || hip ? std::string()
            : std::string("#pragma unroll");
    }
    // A loop that combines the values of the lanes of an iteration in each
    // of its iterations is unrolled by four: the shuffles of one iteration
    // wait for the loads of the next otherwise.
    std::string unroll_few_pragma() const {
        return emulate_cpu || hip ? std::string()
            : std::string("#pragma unroll 4");
    }
    // A bare literal is double precision, which would pick the wrong
    // overload of an intrinsic taking single precision arguments.
    std::string real_literal(double value, int kind) const {
        std::string literal = double_to_scientific(value);
        return (kind == 8) ? literal : literal + "f";
    }

    std::string thread_index() const { return "threadIdx.x"; }
    std::string block_index() const { return "blockIdx.x"; }
    std::string block_size() const { return "blockDim.x"; }

    // The grid divides into blocks, so a thread's position in the grid has to
    // be worked out from all three.
    std::string global_thread_id() const {
        return "(blockIdx.x * blockDim.x + threadIdx.x)";
    }
    // The same for the lanes of an iteration, which all have the number of
    // the iteration.
    std::string lane_global_thread_id() const {
        return "(blockIdx.x * __gpu_iteration_block + __gpu_iteration_thread)";
    }

    std::string uint_type() const {
        return "unsigned int";
    }

    // The bits of `value` read as a `type` of the same width: a real of
    // kind `kind` when `to_real`, else an integer of that kind. The CPU
    // emulation defines these intrinsics in cuda_cpu_device.h.
    std::string bit_cast(const std::string & /*type*/, bool to_real,
            int kind, const std::string &value) const {
        if (kind == 8) {
            return std::string(to_real ? "__longlong_as_double("
                : "__double_as_longlong(") + value + ")";
        }
        return std::string(to_real ? "__int_as_float(" : "__float_as_int(")
            + value + ")";
    }

    // A relaxed atomic add to an `int` in device memory, giving the old
    // value. The CPU emulation defines atomicAdd in cuda_cpu_device.h.
    std::string atomic_fetch_add(const std::string &address,
            const std::string &increment) const {
        return "atomicAdd(" + address + ", " + increment + ")";
    }

    // Every scalar arrives in one struct, passed by value, which is what the
    // runtime's argument array holds a pointer to.
    void emit_scalar_args_param(std::stringstream &src,
            const std::string &struct_name, int /*buffer_index*/) const {
        src << "    " << struct_name << " __scalar_args";
    }

    // On the CPU there is no portable way to call a function pointer with a
    // dynamically built argument list, so every kernel gets a thunk that
    // unpacks the argument array the runtime passes.
    void emit_kernel_epilogue(std::stringstream &src,
            const std::string &kernel_name,
            const std::vector<GpuKernelParam> &params) const {
        if (!emulate_cpu) return;
        src << "extern \"C\" void __lf_thunk_" << kernel_name
            << "(void **a) {\n";
        src << "    " << kernel_name << "(";
        for (size_t slot = 0; slot < params.size(); slot++) {
            const GpuKernelParam &p = params[slot];
            if (slot > 0) src << ", ";
            switch (p.kind) {
                case GpuKernelParamKind::Buffer:
                    src << "*(" << p.type << " **)a[" << slot << "]";
                    break;
                case GpuKernelParamKind::StructReference:
                    src << "**(" << p.type << " **)a[" << slot << "]";
                    break;
                case GpuKernelParamKind::ScalarStruct:
                    src << "*(" << p.type << " *)a[" << slot << "]";
                    break;
            }
        }
        src << ");\n";
        src << "}\n\n";
    }

    void emit_translation_unit_epilogue(std::stringstream &src,
            const std::vector<std::string> &kernel_names,
            const std::vector<std::pair<std::string, std::string>>
                &variants) const {
        if (!emit_registrar) return;
        src << "\n// Auto-generated kernel registration\n";
        if (emulate_cpu) {
            if (kernel_names.empty()) return;
            // The thunks are registered instead of the kernels, with the
            // type of a kernel, and the emulated cudaLaunchKernel calls them.
            // A plain constructor attribute keeps the file free of any C++
            // runtime dependency. Under separate compilation the per-object
            // device sources are concatenated, so the registrar is named after
            // the first kernel to keep it unique per translation unit.
            src << "typedef void (*kernel_func_t)(void);\n";
            src << "extern \"C\" void lfortran_gpu_register_kernel("
                   "const char *name, kernel_func_t func);\n\n";
            src << "__attribute__((constructor)) static void "
                   "_lfortran_cuda_cpu_registrar_" << kernel_names[0]
                << "(void) {\n";
            for (auto &kname : kernel_names) {
                src << "    lfortran_gpu_register_kernel(\"" << kname
                    << "\", (kernel_func_t)__lf_thunk_" << kname << ");\n";
            }
            src << "}\n";
            return;
        }
        if (kernel_names.empty()) return;
        src << "typedef void (*kernel_func_t)(void);\n";
        src << "extern \"C\" void lfortran_gpu_register_kernel("
               "const char *name, kernel_func_t func);\n";
        if (!variants.empty()) {
            src << "extern \"C\" void lfortran_gpu_register_kernel_variant("
                   "const char *name, kernel_func_t func);\n";
        }
        src << "\n";
        src << "namespace {\n";
        src << "struct _lfortran_cuda_registrar {\n";
        src << "    _lfortran_cuda_registrar() {\n";
        for (auto &kname : kernel_names) {
            src << "        lfortran_gpu_register_kernel(\""
                << kname << "\", (kernel_func_t)" << kname << ");\n";
        }
        // A second version of a kernel, after the kernel it is one of.
        for (auto &v : variants) {
            src << "        lfortran_gpu_register_kernel_variant(\""
                << v.first << "\", (kernel_func_t)" << v.second << ");\n";
        }
        src << "    }\n";
        src << "} _lfortran_cuda_reg;\n";
        src << "}\n";
    }
};

Result<std::string> asr_to_cuda(Allocator & /*al*/, ASR::TranslationUnit_t &asr,
    diag::Diagnostics &diagnostics, CompilerOptions &co, bool emit_registrar)
{
    ASRToGpuCVisitor<CudaDialect> v(co,
        CudaDialect(emit_registrar, co.gpu_cpu_emulation, co.gpu_hip,
            gpu_device_capabilities(co.po).offset_kind,
            co.gpu_overlap && gpu_overlap_arch_ok(co.gpu_arch)));
    try {
        v.visit_TranslationUnit(asr);
    } catch (const CodeGenError &e) {
        diagnostics.diagnostics.push_back(e.d);
        return Error();
    } catch (const Abort &) {
        return Error();
    }
    return v.src.str();
}

} // namespace LCompilers

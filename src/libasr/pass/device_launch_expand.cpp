#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/assert.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/device_launch_expand.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_kernel_abi.h>
#include <libasr/pass/gpu_offload_designator.h>
#include <libasr/pass/gpu_data_layout.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/pass_utils.h>

#include <functional>
#include <iostream>
#include <map>
#include <string>
#include <vector>

namespace LCompilers {

/*
Expands the high level GpuKernelLaunch, GpuSync and GpuDiscard statements
into explicit ASR that calls the `lfortran_gpu_*` runtime, so that the host side of a
kernel launch is ordinary ASR: it shows up in --show-asr, every backend can
lower it, and later passes can optimise it.

The generated sequence mirrors the argument layout that the device code
generators (asr_to_metal.cpp and asr_to_cuda.cpp) expect:

    ctx    = lfortran_gpu_init()
    kernel = lfortran_gpu_load_kernel(ctx, "<kernel name>", <name length>)
    call lfortran_gpu_set_buffer_arg(kernel, 0, c_loc(a), size_in_bytes(a),
                                     access)    ! does the kernel write `a`,
                                                ! or all of it before reading?
    ...
    scalars%x = x                       ! one struct holding every scalar
    call lfortran_gpu_set_scalar_arg(kernel, n, c_loc(scalars), sizeof(scalars))
    grid = [(thread_count * lanes + block_size - 1) / block_size, 1, 1]
    block = [block_size, 1, 1]
    call lfortran_gpu_launch(ctx, kernel, c_loc(grid), c_loc(block))
    call lfortran_gpu_release_kernel(kernel)

`lanes` is the kernel's last argument when its layout has lanes
(gpu_vector_lanes.cpp), and one otherwise.

The launch does not wait for the kernel. An argument the kernel may write
stays on the device until a GpuSync (lfortran_gpu_sync) copies it back; the
`gpu_launch_sync` pass, which runs before this one, places those. Only the
copies this pass makes itself -- a contiguous copy of a strided argument, the
components of an array of derived type, the combined buffer of a packed
launch -- are waited for right after the launch, when the kernel may write
them, before they are copied back. A run-time sized local of the kernel (a
workspace) is a scratch buffer.

A scratch buffer of the launch (GpuKernelLaunch.scratch), which only the
device holds, is set with lfortran_gpu_set_scratch_arg and read back
after the launch with lfortran_gpu_read_scratch_arg when the launch says so.
Per-thread workspaces use the same interface but are never read back. The
runtime owns this storage and may reuse it across launches.

The finalized Function.gpu layout fixes the buffer order, scalar fields and
workspace bindings. This pass expands that contract without deciding whether
the loop can be offloaded or deriving workspace extents again.

The logical count is evaluated once and normalized before any launch setup.
The entire sequence above, including its cleanup, runs only when the effective
count (including lanes) is positive. The common runtime rejects empty geometry.
*/
class DeviceLaunchExpandVisitor :
        public PassUtils::PassVisitor<DeviceLaunchExpandVisitor>
{
    public:

        DeviceLaunchExpandVisitor(Allocator &al_,
                ASR::TranslationUnit_t &unit_) :
            PassVisitor(al_, nullptr), unit(unit_) {}

        void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
            const auto *kernel = ASR::down_cast<ASR::Function_t>(x.m_kernel);
            LCOMPILERS_ASSERT(kernel->m_gpu != nullptr);
            const Location &loc = x.base.base.loc;
            ASRUtils::ASRBuilder b(al, loc);
            ASR::expr_t *threads = declare_local(loc, "gpu_threads", int64);
            ASR::expr_t *started = threads;
            if (kernel->m_gpu->m_lanes) {
                started = declare_local(loc, "gpu_started", int64);
            }
            Vec<ASR::stmt_t*> stmts;
            stmts.reserve(al, 8);
            if (!expand_launch(x, threads, started, stmts)) {
                remove_original_stmt = true;
                return;
            }
            pass_result.reserve(al, 4);
            pass_result.push_back(al, b.Assignment(threads,
                b.i2i_t(x.m_thread_count, int64)));
            pass_result.push_back(al, b.If(b.Lt(threads, b.i64(0)),
                {b.Assignment(threads, b.i64(0))}, {}));
            if (kernel->m_gpu->m_lanes) {
                pass_result.push_back(al, b.Assignment(started,
                    b.Mul(threads,
                        b.i2i_t(x.m_args[x.n_args - 1].m_value, int64))));
            }
            pass_result.push_back(al, ASRUtils::STMT(ASR::make_If_t(al,
                loc, nullptr, b.Gt(started, b.i64(0)), stmts.p, stmts.n,
                nullptr, 0)));
        }

        void visit_GpuSync(const ASR::GpuSync_t &x) {
            const Location &loc = x.base.base.loc;
            ASRUtils::ASRBuilder b(al, loc);
            ASR::expr_t *ctx = declare_local(loc, "gpu_ctx", b.CPtr());
            pass_result.reserve(al, 2);
            pass_result.push_back(al, b.Assignment(ctx, gpu_init_call(loc)));
            Vec<ASR::call_arg_t> args;
            args.reserve(al, 1);
            args.push_back(al, call_arg(loc, ctx));
            pass_result.push_back(al, b.SubroutineCall(
                runtime_subroutine(loc, "lfortran_gpu_sync", {b.CPtr()},
                    {true}), args));
        }

        // lfortran_gpu_discard(ctx, c_loc(v), size_in_bytes(v)) for each
        // variable: the host is about to free it, and what a pending launch
        // writes into it is not needed.
        void visit_GpuDiscard(const ASR::GpuDiscard_t &x) {
            const Location &loc = x.base.base.loc;
            ASRUtils::ASRBuilder b(al, loc);
            ASR::expr_t *ctx = declare_local(loc, "gpu_ctx", b.CPtr());
            pass_result.reserve(al, x.n_vars + 1);
            pass_result.push_back(al, b.Assignment(ctx, gpu_init_call(loc)));
            ASR::symbol_t *discard = runtime_subroutine(loc,
                "lfortran_gpu_discard", {b.CPtr(), b.CPtr(), int64},
                {true, true, true});
            for (size_t i = 0; i < x.n_vars; i++) {
                Vec<ASR::call_arg_t> args;
                args.reserve(al, 3);
                args.push_back(al, call_arg(loc, ctx));
                args.push_back(al, call_arg(loc, address_of(loc,
                    x.m_vars[i])));
                args.push_back(al, call_arg(loc, buffer_byte_size(loc,
                    x.m_vars[i])));
                pass_result.push_back(al, b.SubroutineCall(discard, args));
            }
        }

    private:

        ASR::TranslationUnit_t &unit;
        std::map<ASR::Function_t*, ASR::symbol_t*> scalar_arg_structs;
        // Sizes buffer of a decomposed member, so a workspace can be
        // counted from the same element the device strides by.
        std::map<GpuStructMemberKey, ASR::expr_t*> member_sizes_bufs;

        // A launch this pass cannot lay out, found once the loop it came
        // from is gone. `gpu_offload` answers the same question while the
        // loop is still there and leaves it on the host; there is nothing
        // left to leave it on here, so this is an error whatever the reason
        // for the decline. The wording is the one every decline
        // is phrased in, so that the two stages cannot describe the same
        // limitation differently.


        ASR::call_arg_t call_arg(const Location &loc, ASR::expr_t *value) {
            ASR::call_arg_t arg;
            arg.loc = loc;
            arg.m_value = value;
            return arg;
        }

        ASR::expr_t* declare_local(const Location &loc,
                const std::string &name, ASR::ttype_t *type,
                ASR::symbol_t *type_declaration = nullptr) {
            ASRUtils::ASRBuilder b(al, loc);
            return b.Variable(current_scope,
                current_scope->get_unique_name("__" + name, false), type,
                ASR::intentType::Local, type_declaration,
                ASR::abiType::BindC);
        }

        // Declares (once) an interface to a `lfortran_gpu_*` runtime entry
        // point in the global scope.
        ASR::symbol_t* runtime_symbol(const Location &loc,
                const std::string &name,
                const std::vector<ASR::ttype_t*> &arg_types,
                const std::vector<bool> &by_value,
                ASR::ttype_t *return_type,
                const std::string &c_name = "") {
            SymbolTable *global_scope = unit.m_symtab;
            if (ASR::symbol_t *existing = global_scope->get_symbol(name)) {
                return existing;
            }
            ASRUtils::ASRBuilder b(al, loc);
            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            Vec<ASR::expr_t*> args;
            args.reserve(al, arg_types.size());
            for (size_t i = 0; i < arg_types.size(); i++) {
                args.push_back(al, b.Variable(fn_symtab,
                    "arg" + std::to_string(i), arg_types[i],
                    ASR::intentType::In, nullptr, ASR::abiType::BindC,
                    by_value[i]));
            }
            ASR::expr_t *return_var = nullptr;
            if (return_type) {
                return_var = b.Variable(fn_symtab, name, return_type,
                    ASRUtils::intent_return_var, nullptr,
                    ASR::abiType::BindC, false);
            }
            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, name), nullptr, 0,
                args.p, args.n, nullptr, 0, return_var,
                ASR::abiType::BindC, ASR::accessType::Public,
                ASR::deftypeType::Interface,
                s2c(al, c_name.empty() ? name : c_name),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);
            ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(fn);
            global_scope->add_symbol(name, sym);
            return sym;
        }

        ASR::symbol_t* runtime_subroutine(const Location &loc,
                const std::string &name,
                const std::vector<ASR::ttype_t*> &arg_types,
                const std::vector<bool> &by_value) {
            return runtime_symbol(loc, name, arg_types, by_value, nullptr);
        }

        ASR::expr_t* gpu_init_call(const Location &loc) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::symbol_t *sym = runtime_symbol(loc, "lfortran_gpu_init",
                {}, {}, b.CPtr());
            Vec<ASR::call_arg_t> args;
            args.reserve(al, 1);
            return b.Call(sym, args, b.CPtr());
        }

        // c_loc(x): the address of the first element for an array, and the
        // address of the variable itself otherwise.
        ASR::expr_t* address_of(const Location &loc, ASR::expr_t *x) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *type = ASRUtils::type_get_past_allocatable(
                ASRUtils::type_get_past_pointer(ASRUtils::expr_type(x)));
            if (ASRUtils::is_array(type)) {
                // A pointer to an array is required to have deferred shape.
                type = ASRUtils::duplicate_type_with_empty_dims(al, type);
            }
            ASR::ttype_t *ptr_type = ASRUtils::TYPE(
                ASR::make_Pointer_t(al, loc, type));
            return b.PointerToCPtr(ASRUtils::EXPR(
                ASR::make_GetPointer_t(al, loc, x, ptr_type, nullptr)),
                b.CPtr());
        }

        // One argument the runtime is handed as a block of bytes.
        struct BufferArg {
            ASR::expr_t *arg;
            ASR::expr_t *address;
            ASR::expr_t *byte_size;
            // A buffer only the device holds (see GpuKernelLaunch.scratch).
            const ASR::gpu_scratch_buffer_t *scratch = nullptr;
            // False for a copy of this pass that nothing is copied back
            // from: the host never reads what the kernel writes there, so
            // it is handed over as read only, and the runtime does not keep
            // it on the device past the launch, when the copy may be gone.
            bool host_reads_writes = true;
        };

        // One array argument inside the combined buffer of a packed launch.
        struct PackedBuffer {
            ASR::expr_t *arg;
            ASR::expr_t *offset;
            ASR::expr_t *byte_size;
        };

        ASR::stmt_t* allocate_bytes(const Location &loc, ASR::expr_t *buffer,
                ASR::expr_t *n_bytes) {
            ASRUtils::ASRBuilder b(al, loc);
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, 1);
            ASR::dimension_t dim;
            dim.loc = loc;
            dim.m_start = b.i64(1);
            dim.m_length = n_bytes;
            dims.push_back(al, dim);
            return b.Allocate(buffer, dims.p, dims.n);
        }

        ASR::stmt_t* memcpy_call(const Location &loc, ASR::expr_t *dest,
                ASR::expr_t *source, ASR::expr_t *n_bytes) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::symbol_t *sym = runtime_symbol(loc, "_lfortran_gpu_memcpy",
                {b.CPtr(), b.CPtr(), int64}, {true, true, true}, b.CPtr(),
                "memcpy");
            Vec<ASR::call_arg_t> args;
            args.reserve(al, 3);
            args.push_back(al, call_arg(loc, dest));
            args.push_back(al, call_arg(loc, source));
            args.push_back(al, call_arg(loc, n_bytes));
            return ASRUtils::STMT(ASR::make_Expr_t(al, loc,
                b.Call(sym, args, b.CPtr())));
        }

        // Number of bytes the runtime has to copy for one buffer argument.
        ASR::expr_t* buffer_byte_size(const Location &loc, ASR::expr_t *arg) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *type = ASRUtils::type_get_past_allocatable(
                ASRUtils::type_get_past_pointer(ASRUtils::expr_type(arg)));
            if (!ASRUtils::is_array(type) ||
                    ASRUtils::get_fixed_size_of_array(type) > 0) {
                return ASRUtils::EXPR(ASR::make_SizeOfType_t(al, loc,
                    gpu_size_of_type_argument(al, arg, type), int64, nullptr));
            }
            ASR::ttype_t *element = gpu_size_of_type_argument(al, arg,
                ASRUtils::type_get_past_array(type));
            ASR::expr_t *size = b.Mul(
                b.i2i_t(b.ArraySize(arg, nullptr, int32), int64),
                ASRUtils::EXPR(ASR::make_SizeOfType_t(al, loc, element,
                    int64, nullptr)));
            // An allocatable the loop only uses when it is allocated -- a
            // local allocated when an optional argument is present -- is
            // handed over as no storage at all when it is not.
            if (ASRUtils::is_allocatable(ASRUtils::expr_type(arg))) {
                size = ASRUtils::EXPR(ASR::make_IfExp_t(al, loc,
                    is_allocated(loc, arg), size, b.i64(0), int64, nullptr));
            }
            return size;
        }

        // Builds, once per kernel, the struct that carries every scalar
        // argument. Its layout has to match the `__ScalarArgs_*` struct the
        // device code generator emits, so the members are created in the same
        // order.
        ASR::symbol_t* get_scalar_args_struct(const Location &loc,
                ASR::Function_t *kernel,
                const std::vector<std::pair<std::string, ASR::ttype_t*>> &fields) {
            auto it = scalar_arg_structs.find(kernel);
            if (it != scalar_arg_structs.end()) return it->second;

            SymbolTable *global_scope = unit.m_symtab;
            std::string struct_name = global_scope->get_unique_name(
                "__ScalarArgs_" + std::string(kernel->m_name), false);
            SymbolTable *struct_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);
            SetChar members;
            members.reserve(al, fields.size());
            for (auto &field : fields) {
                b.VariableDeclaration(struct_symtab, field.first, field.second,
                    ASR::intentType::Local, nullptr, ASR::abiType::BindC);
                members.push_back(al, s2c(al, field.first));
            }
            ASR::symbol_t *struct_sym = ASR::down_cast<ASR::symbol_t>(
                ASR::make_Struct_t(al, loc, struct_symtab,
                    s2c(al, struct_name), nullptr, nullptr, 0,
                    members.p, members.n, nullptr, 0,
                    ASR::abiType::BindC, ASR::accessType::Public,
                    false, false, false, nullptr, 0, nullptr, nullptr,
                    nullptr, 0));
            ASR::down_cast<ASR::Struct_t>(struct_sym)->m_struct_signature =
                ASRUtils::make_StructType_t_util(al, loc, struct_sym, true);
            global_scope->add_symbol(struct_name, struct_sym);
            scalar_arg_structs[kernel] = struct_sym;
            return struct_sym;
        }

        // Splits every allocatable array member of an array of structs into
        // the three flat buffers the device code reads: the elements' data
        // laid out end to end, and their offsets and sizes. The data is
        // copied back into the members after the launch, because the kernel
        // may have written to it.
        void decompose_struct_members(const Location &loc,
                Vec<ASR::stmt_t*> &out, ASR::expr_t *arg,
                ASR::symbol_t *parameter,
                std::vector<BufferArg> &buffers,
                std::vector<ASR::stmt_t*> &writebacks,
                std::vector<ASR::stmt_t*> &cleanups,
                const ASR::Function_t &kernel, bool written) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::Struct_t *st = gpu_struct_definition(
                ASRUtils::get_struct_sym_from_struct_expr(arg));
            if (!st) return;
            std::string arg_name = ASRUtils::symbol_name(parameter);
            // A member inherited from a type this one extends is stored
            // and handed over exactly like one of its own.
            for (const GpuComponentLayout &component :
                    gpu_component_layouts(*kernel.m_gpu, parameter, st)) {
                std::string member_name = component.name();
                ASR::symbol_t *member = component.component;
                ASR::ttype_t *element_type = component.element_type;
                bool element_is_empty = component.element_is_empty;
                ASR::expr_t *element_bytes = element_is_empty
                    ? b.i64(1)
                    : ASRUtils::EXPR(ASR::make_SizeOfType_t(al, loc,
                        element_type, int64, nullptr));

                GpuStructMemberKey key{arg_name, member_name};
                // The component of an element may not be allocated: the
                // offload pass gave storage only to those the loop writes
                // (see build_component_fit). One that is not holds nothing
                // to hand over.
                bool may_be_unallocated = ASRUtils::is_allocatable(
                    ASRUtils::symbol_type(member));

                ASR::expr_t *n = declare_local(loc, "gpu_struct_count", int32);
                ASR::expr_t *total = declare_local(loc, "gpu_member_total",
                    int32);
                ASR::expr_t *k = declare_local(loc, "gpu_struct_index", int32);
                ASR::expr_t *sizes = declare_local(loc, "gpu_member_sizes",
                    b.allocatable(b.Array({-1}, int32)));
                ASR::expr_t *offsets = declare_local(loc, "gpu_member_offsets",
                    b.allocatable(b.Array({-1}, int32)));
                ASR::expr_t *data = declare_local(loc, "gpu_member_data",
                    b.allocatable(b.Array({-1}, int8)));

                size_t rank = component.rank;
                out.push_back(al, b.Assignment(n,
                    b.ArraySize(arg, nullptr, int32)));
                Vec<ASR::dimension_t> dims;
                dims.reserve(al, 1);
                ASR::dimension_t dim;
                dim.loc = loc;
                dim.m_start = b.i32(1);
                dim.m_length = n;
                dims.push_back(al, dim);
                out.push_back(al, b.Allocate(offsets, dims.p, dims.n));
                // Two entries per dimension per element: the extent and
                // the lower bound.
                Vec<ASR::dimension_t> size_dims;
                size_dims.reserve(al, 1);
                ASR::dimension_t size_dim;
                size_dim.loc = loc;
                size_dim.m_start = b.i32(1);
                size_dim.m_length = b.Mul(n, b.i32(2 * (int)rank));
                size_dims.push_back(al, size_dim);
                out.push_back(al, b.Allocate(sizes, size_dims.p,
                    size_dims.n));
                out.push_back(al, b.Assignment(total, b.i32(0)));
                std::vector<ASR::stmt_t*> measure;
                measure.push_back(b.Assignment(b.ArrayItem_01(offsets, {k}),
                    total));
                std::vector<ASR::stmt_t*> extents, no_extents;
                for (size_t d = 0; d < rank; d++) {
                    extents.push_back(b.Assignment(
                        member_extent(loc, sizes, k, rank, d),
                        b.ArraySize(struct_member(loc, arg, k, member),
                            rank > 1 ? b.i32((int)d + 1) : nullptr,
                            int32)));
                    extents.push_back(b.Assignment(
                        member_lower_bound(loc, sizes, k, rank, d),
                        b.GetLBound(struct_member(loc, arg, k, member),
                            (int)d + 1)));
                    no_extents.push_back(b.Assignment(
                        member_extent(loc, sizes, k, rank, d), b.i32(0)));
                    no_extents.push_back(b.Assignment(
                        member_lower_bound(loc, sizes, k, rank, d),
                        b.i32(1)));
                }
                if (may_be_unallocated) {
                    measure.push_back(b.If(is_allocated(loc,
                        struct_member(loc, arg, k, member)), extents,
                        no_extents));
                } else {
                    measure.insert(measure.end(), extents.begin(),
                        extents.end());
                }
                measure.push_back(b.Assignment(total, b.Add(total,
                    member_element_count(loc, sizes, k, rank))));
                out.push_back(al, b.DoLoop(k, b.i32(1), n, measure));
                member_sizes_bufs[key] = sizes;
                // A member that is allocated but holds no elements in any
                // of them -- `allocate(x%m(0,3))` -- leaves nothing to hand
                // over, but the buffer still has to have a byte in it: the
                // launch takes the address of its first element, and the
                // runtime has no buffer of no bytes to give the kernel.
                ASR::expr_t *data_bytes = declare_local(loc,
                    "gpu_member_bytes", int64);
                out.push_back(al, b.Assignment(data_bytes,
                    b.Mul(b.i2i_t(total, int64), element_bytes)));
                out.push_back(al, b.If(b.Lt(data_bytes, b.i64(1)),
                    {b.Assignment(data_bytes, b.i64(1))}, {}));
                out.push_back(al, allocate_bytes(loc, data, data_bytes));
                if (!element_is_empty) {
                    out.push_back(al, b.DoLoop(k, b.i32(1), n, {
                        copy_if_allocated(loc, may_be_unallocated,
                            struct_member(loc, arg, k, member),
                            memcpy_call(loc,
                                member_data_address(loc, data, offsets, k,
                                    element_bytes),
                                address_of(loc,
                                    struct_member(loc, arg, k, member)),
                                member_byte_size(loc, sizes, k, rank,
                                    element_bytes)))}));
                }

                ASR::expr_t *index_bytes = b.Mul(b.i2i_t(n, int64), b.i64(4));
                ASR::expr_t *sizes_bytes =
                    b.Mul(index_bytes, b.i64(2 * (int64_t)rank));
                buffers.push_back({data, address_of(loc, data), data_bytes});
                buffers.push_back({offsets, address_of(loc, offsets),
                    index_bytes});
                buffers.push_back({sizes, address_of(loc, sizes),
                    sizes_bytes});

                // Copied back only when the kernel may have written it.
                if (!element_is_empty && written) {
                    writebacks.push_back(b.DoLoop(k, b.i32(1), n, {
                        copy_if_allocated(loc, may_be_unallocated,
                            struct_member(loc, arg, k, member),
                            memcpy_call(loc,
                                address_of(loc,
                                    struct_member(loc, arg, k, member)),
                                member_data_address(loc, data, offsets, k,
                                    element_bytes),
                                member_byte_size(loc, sizes, k, rank,
                                    element_bytes)))}));
                }
                cleanups.push_back(b.Deallocate(data));
                cleanups.push_back(b.Deallocate(offsets));
                cleanups.push_back(b.Deallocate(sizes));
            }
        }

        // `copy`, only when `component` is allocated if it may not be.
        ASR::stmt_t* copy_if_allocated(const Location &loc,
                bool may_be_unallocated, ASR::expr_t *component,
                ASR::stmt_t *copy) {
            if (!may_be_unallocated) return copy;
            ASRUtils::ASRBuilder b(al, loc);
            return b.If(is_allocated(loc, component), {copy}, {});
        }

        ASR::expr_t* is_allocated(const Location &loc, ASR::expr_t *x) {
            Vec<ASR::expr_t*> args;
            args.reserve(al, 1);
            args.push_back(al, x);
            return ASRUtils::EXPR(ASR::make_IntrinsicImpureFunction_t(al, loc,
                static_cast<int64_t>(
                    ASRUtils::IntrinsicImpureFunctions::Allocated),
                args.p, args.n, 0,
                ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 4)), nullptr));
        }

    private:

        // The subscripts of the element of `arg` at column-major position
        // `index`, counting from one. The flattened component buffers are
        // laid out and read by that position, so an array of a rank above
        // one is walked in the same order the device reads it back in
        // rather than declined: subscript d is
        // lbound_d + mod((index - 1) / (e_0 * ... * e_{d-1}), e_d).
        std::vector<ASR::expr_t*> element_subscripts(const Location &loc,
                ASR::expr_t *arg, ASR::expr_t *index) {
            ASRUtils::ASRBuilder b(al, loc);
            std::vector<ASR::expr_t*> subscripts;
            int rank = ASRUtils::extract_n_dims_from_ttype(
                ASRUtils::expr_type(arg));
            if (rank <= 1) {
                subscripts.push_back(index);
                return subscripts;
            }
            ASR::expr_t *flat = b.Sub(index, b.i32(1));
            ASR::expr_t *stride = nullptr;
            for (int d = 0; d < rank; d++) {
                ASR::expr_t *extent = b.ArraySize(arg, b.i32(d + 1), int32);
                ASR::expr_t *pos = stride == nullptr
                    ? flat : b.Div(flat, stride);
                if (d + 1 < rank) {
                    // mod(pos, extent), spelled out so no intrinsic has to
                    // survive the passes that run after this one.
                    pos = b.Sub(pos, b.Mul(b.Div(pos, extent), extent));
                }
                subscripts.push_back(b.Add(b.GetLBound(arg, d + 1), pos));
                stride = stride == nullptr ? extent : b.Mul(stride, extent);
            }
            return subscripts;
        }

        ASR::expr_t* struct_member(const Location &loc, ASR::expr_t *arg,
                ASR::expr_t *index, ASR::symbol_t *member) {
            ASRUtils::ASRBuilder b(al, loc);
            return ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, loc,
                b.ArrayItem_01(arg, element_subscripts(loc, arg, index)),
                member, ASRUtils::symbol_type(member), nullptr));
        }

        ASR::expr_t* member_data_address(const Location &loc,
                ASR::expr_t *data, ASR::expr_t *offsets, ASR::expr_t *index,
                ASR::expr_t *element_bytes) {
            ASRUtils::ASRBuilder b(al, loc);
            return address_of(loc, b.ArrayItem_01(data, {b.Add(
                b.Mul(b.i2i_t(b.ArrayItem_01(offsets, {index}), int64),
                    element_bytes), b.i64(1))}));
        }

        // Where the extent of dimension `d` of element `index` sits in the
        // sizes buffer: the buffer carries `2 * rank` entries per element,
        // the extents in dimension order and then the lower bounds.
        ASR::expr_t* member_extent(const Location &loc, ASR::expr_t *sizes,
                ASR::expr_t *index, size_t rank, size_t d) {
            ASRUtils::ASRBuilder b(al, loc);
            return b.ArrayItem_01(sizes, {b.Add(
                b.Mul(b.Sub(index, b.i32(1)), b.i32(2 * (int)rank)),
                b.i32((int)d + 1))});
        }

        // Where the lower bound of dimension `d` of element `index` sits in
        // the sizes buffer.
        ASR::expr_t* member_lower_bound(const Location &loc,
                ASR::expr_t *sizes, ASR::expr_t *index, size_t rank,
                size_t d) {
            return member_extent(loc, sizes, index, rank, rank + d);
        }

        // Number of elements of one element's component: the product of its
        // extents.
        ASR::expr_t* member_element_count(const Location &loc,
                ASR::expr_t *sizes, ASR::expr_t *index, size_t rank) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::expr_t *count = member_extent(loc, sizes, index, rank, 0);
            for (size_t d = 1; d < rank; d++) {
                count = b.Mul(count,
                    member_extent(loc, sizes, index, rank, d));
            }
            return count;
        }

        ASR::expr_t* member_byte_size(const Location &loc, ASR::expr_t *sizes,
                ASR::expr_t *index, size_t rank,
                ASR::expr_t *element_bytes) {
            ASRUtils::ASRBuilder b(al, loc);
            return b.Mul(b.i2i_t(
                member_element_count(loc, sizes, index, rank), int64),
                element_bytes);
        }

        // Copy every element between a strided array and its contiguous
        // stand-in, one at a time. A whole-array assignment would do, but
        // this pass runs after the one that lowers those, so what it wrote
        // would reach the backend as a block copy -- which is exactly the
        // assumption the copy is here to avoid.
        void copy_elementwise(const Location &loc, Vec<ASR::stmt_t*> &out,
                ASR::expr_t *tmp, ASR::expr_t *arg, int rank,
                bool back) {
            ASRUtils::ASRBuilder b(al, loc);
            std::vector<ASR::expr_t*> idx;
            for (int d = 0; d < rank; d++) {
                idx.push_back(declare_local(loc,
                    "gpu_copy_i" + std::to_string(d), int32));
            }
            std::vector<ASR::expr_t*> arg_subs;
            for (int d = 0; d < rank; d++) {
                // The stand-in is 1-based; the argument keeps its own
                // lower bound.
                arg_subs.push_back(b.Add(b.Sub(idx[d], b.i32(1)),
                    b.ArrayLBound(arg, d + 1)));
            }
            ASR::expr_t *tmp_el = b.ArrayItem_01(tmp, idx);
            ASR::expr_t *arg_el = b.ArrayItem_01(arg, arg_subs);
            std::vector<ASR::stmt_t*> body;
            body.push_back(back ? b.Assignment(arg_el, tmp_el)
                                : b.Assignment(tmp_el, arg_el));
            for (int d = 0; d < rank; d++) {
                body = {b.DoLoop(idx[d], b.i32(1),
                    b.ArraySize(arg, b.i32(d + 1), int32), body)};
            }
            out.push_back(al, body[0]);
        }

        // The parts of one value of a derived type that the device reads
        // through the struct it is handed, copied component by component
        // into `to`, with the copy back into `from` collected in `back`.
        //
        // An allocatable or a pointer component is not one of those parts:
        // it reaches the kernel as its own flat buffers, and the field the
        // device lays out in its place is never read through. It is also the
        // one component a copy here must not touch -- this pass runs after
        // the deep copy rewrites, so an assignment of it reaches the backend
        // as a block copy of the array descriptor, after which the local and
        // the argument name the same descriptor and the same storage, and
        // the local's finalization frees what the argument still points at.
        // So it is skipped, and the field keeps the value the local was
        // declared with, while the components around it keep their offsets.
        //
        // Returns false when a component has no copy at all.
        bool copy_plain_parts(const Location &loc, ASR::expr_t *to,
                ASR::expr_t *from, ASR::symbol_t *struct_sym,
                std::vector<ASR::stmt_t*> &out,
                std::vector<ASR::stmt_t*> &back) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::Struct_t *st = gpu_struct_definition(struct_sym);
            if (st == nullptr) return false;
            std::vector<ASR::symbol_t*> data_members;
            gpu_collect_data_members(st, data_members);
            for (ASR::symbol_t *member : data_members) {
                if (member == nullptr
                        || !ASR::is_a<ASR::Variable_t>(*member)) {
                    return false;
                }
                ASR::ttype_t *mt = ASRUtils::symbol_type(member);
                if (ASRUtils::is_allocatable_or_pointer(mt)) continue;
                ASR::expr_t *mfrom = ASRUtils::EXPR(
                    ASR::make_StructInstanceMember_t(al, loc, from, member,
                        mt, nullptr));
                ASR::expr_t *mto = ASRUtils::EXPR(
                    ASR::make_StructInstanceMember_t(al, loc, to, member,
                        mt, nullptr));
                ASR::symbol_t *decl = ASR::down_cast<ASR::Variable_t>(
                    member)->m_type_declaration;
                if (ASR::is_a<ASR::StructType_t>(
                            *ASRUtils::type_get_past_array(mt))
                        && gpu_struct_has_allocatable_parts(decl)) {
                    if (!copy_plain_parts_of_value(loc, mto, mfrom, mt, decl,
                            out, back)) {
                        return false;
                    }
                    continue;
                }
                out.push_back(b.Assignment(mto, mfrom));
                back.push_back(b.Assignment(mfrom, mto));
            }
            return true;
        }

        // The same, for a value that may be an array of such a type: every
        // element is copied on its own, because the whole array has no copy
        // that is not the block copy of descriptors this is here to avoid.
        bool copy_plain_parts_of_value(const Location &loc, ASR::expr_t *to,
                ASR::expr_t *from, ASR::ttype_t *type, ASR::symbol_t *decl,
                std::vector<ASR::stmt_t*> &out,
                std::vector<ASR::stmt_t*> &back) {
            ASRUtils::ASRBuilder b(al, loc);
            if (!ASRUtils::is_array(type)) {
                return copy_plain_parts(loc, to, from, decl, out, back);
            }
            ASR::dimension_t *dims = nullptr;
            int rank = checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(type, dims));
            if (rank <= 0) return false;
            std::vector<ASR::expr_t*> idx;
            for (int d = 0; d < rank; d++) {
                if (dims[d].m_start == nullptr
                        || dims[d].m_length == nullptr) {
                    return false;
                }
                idx.push_back(declare_local(loc,
                    "gpu_part_i" + std::to_string(d), int32));
            }
            std::vector<ASR::stmt_t*> body, body_back;
            if (!copy_plain_parts(loc, b.ArrayItem_01(to, idx),
                    b.ArrayItem_01(from, idx), decl, body, body_back)) {
                return false;
            }
            for (int d = 0; d < rank; d++) {
                ASR::expr_t *start = dims[d].m_start;
                ASR::expr_t *end = b.Sub(b.Add(start, dims[d].m_length),
                    b.i32(1));
                if (!body.empty()) {
                    body = {b.DoLoop(idx[d], start, end, body)};
                }
                if (!body_back.empty()) {
                    body_back = {b.DoLoop(idx[d], start, end, body_back)};
                }
            }
            for (ASR::stmt_t *stmt : body) out.push_back(stmt);
            for (ASR::stmt_t *stmt : body_back) back.push_back(stmt);
            return true;
        }

        // A polymorphic argument reaches the device as the class container
        // it is represented by -- a type descriptor beside the data -- and
        // the kernel is generated against the declared type, so reading a
        // component of it would read the descriptor. Copy the declared
        // type's own components into a plain local and hand that over.
        // The local is copied back right after the launch when the kernel
        // may write it; one with nothing to copy back -- every component
        // of the type is an allocatable array the device is handed on its
        // own -- leaves `written` false, since what the kernel writes there
        // is never read.
        ASR::expr_t* plain_struct_argument(const Location &loc,
                Vec<ASR::stmt_t*> &out, ASR::expr_t *arg,
                ASR::Variable_t *kparam, bool &written,
                std::vector<ASR::stmt_t*> &writebacks) {
            if (!gpu_argument_is_polymorphic_value(arg)) return arg;
            ASR::symbol_t *struct_sym = ASRUtils::symbol_get_past_external(
                ASRUtils::get_struct_sym_from_struct_expr(arg));
            ASR::ttype_t *plain_type = ASRUtils::make_StructType_t_util(al,
                loc, struct_sym, true);
            ASR::expr_t *tmp = declare_local(loc, "gpu_plain_arg",
                plain_type, struct_sym);
            std::vector<ASR::stmt_t*> forward, back;
            if (!copy_plain_parts(loc, tmp, arg, struct_sym, forward, back)) {
                // The finalized layout requires a plain copy of each
                // component, as checked by the kernel planner.
                // Handing the container over instead would have the kernel
                // read the type descriptor as the declared type's data, so
                // the two walks disagreeing is reported, not compiled.
                throw LCompilersException("the gpu backend cannot copy the "
                    "components of the polymorphic argument passed as '"
                    + std::string(kparam->m_name) + "' to a gpu kernel");
            }
            for (ASR::stmt_t *stmt : forward) out.push_back(al, stmt);
            written = written && !back.empty();
            if (written) {
                for (ASR::stmt_t *stmt : back) writebacks.push_back(stmt);
            }
            return tmp;
        }

        // `arg` itself when the device can read it as it stands, or a
        // contiguous copy of it when it cannot.
        ASR::expr_t* contiguous_argument(const Location &loc,
                Vec<ASR::stmt_t*> &out, ASR::expr_t *arg, bool written,
                std::vector<ASR::stmt_t*> &writebacks) {
            ASR::ttype_t *arg_type = ASRUtils::expr_type(arg);
            if (!ASRUtils::is_array(arg_type)) return arg;
            if (ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(arg_type))) {
                return arg;
            }
            if (!gpu_argument_may_be_strided(arg)) return arg;
            ASRUtils::ASRBuilder b(al, loc);
            ASR::dimension_t *dims = nullptr;
            int rank = checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(
                ASRUtils::type_get_past_allocatable_pointer(arg_type), dims));
            if (rank <= 0) return arg;
            std::vector<int64_t> deferred((size_t)rank, -1);
            ASR::expr_t *tmp = declare_local(loc, "gpu_contiguous_arg",
                b.allocatable(b.Array(deferred,
                    ASRUtils::extract_type(arg_type))));
            Vec<ASR::dimension_t> alloc_dims;
            alloc_dims.reserve(al, rank);
            for (int d = 0; d < rank; d++) {
                ASR::dimension_t dd;
                dd.loc = loc;
                dd.m_start = b.i32(1);
                dd.m_length = b.ArraySize(arg, b.i32(d + 1), int32);
                alloc_dims.push_back(al, dd);
            }
            out.push_back(al, b.Allocate(tmp, alloc_dims.p, alloc_dims.n));
            copy_elementwise(loc, out, tmp, arg, rank, false);
            // Copied back only when the kernel writes it and the caller's
            // own argument can be written: the copy stands in for the
            // argument, it is not a licence to assign to something the
            // caller may not.
            bool arg_is_writable = true;
            if (ASR::is_a<ASR::Var_t>(*arg)) {
                ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(arg)->m_v);
                if (ASR::is_a<ASR::Variable_t>(*sym)) {
                    arg_is_writable = ASR::down_cast<ASR::Variable_t>(sym)
                        ->m_intent != ASR::intentType::In;
                }
            }
            if (written && arg_is_writable) {
                Vec<ASR::stmt_t*> back;
                back.reserve(al, 1);
                copy_elementwise(loc, back, tmp, arg, rank, true);
                for (size_t k = 0; k < back.n; k++) {
                    writebacks.push_back(back.p[k]);
                }
            }
            // Every launch allocates the copy afresh, so it is released
            // once copied back, ready for the next launch of the loop.
            writebacks.push_back(b.Deallocate(tmp));
            return tmp;
        }

        // False when a shape only the passes after `gpu_offload` create
        // stops the layout part way; the caller then drops the launch and
        // the reported error fails the build.
        bool expand_launch(const ASR::GpuKernelLaunch_t &x,
                ASR::expr_t *threads, ASR::expr_t *started,
                Vec<ASR::stmt_t*> &out) {
            const Location &loc = x.base.base.loc;
            ASRUtils::ASRBuilder b(al, loc);

            ASR::Function_t *kernel =
                ASR::down_cast<ASR::Function_t>(x.m_kernel);
            std::string kernel_name(kernel->m_name);
            const ASR::gpu_kernel_layout_t &layout = *kernel->m_gpu;

            std::vector<BufferArg> buffers;
            std::vector<std::pair<std::string, ASR::ttype_t*>> scalar_fields;
            std::vector<ASR::expr_t*> scalar_values;
            // What the host does with what the kernel wrote, and the
            // temporaries it frees, after the launch.
            std::vector<ASR::stmt_t*> writebacks;
            std::vector<ASR::stmt_t*> cleanups;

            ASR::expr_t *ctx = declare_local(loc, "gpu_ctx", b.CPtr());
            ASR::expr_t *gpu_kernel = declare_local(loc, "gpu_kernel",
                b.CPtr());
            out.push_back(al, b.Assignment(ctx, gpu_init_call(loc)));

            // kernel = lfortran_gpu_load_kernel(ctx, "<name>", len)
            ASR::ttype_t *c_string = b.UnboundedArray(
                b.String(b.i32(1), ASR::ExpressionLength, ASR::CChar), 1);
            ASR::symbol_t *load_sym = runtime_symbol(loc,
                "lfortran_gpu_load_kernel",
                {b.CPtr(), c_string, int32}, {true, false, true}, b.CPtr());
            ASR::ttype_t *name_type = b.String(
                b.i32(kernel_name.size()), ASR::ExpressionLength);
            Vec<ASR::call_arg_t> load_args;
            load_args.reserve(al, 3);
            load_args.push_back(al, call_arg(loc, ctx));
            load_args.push_back(al, call_arg(loc,
                ASRUtils::create_string_physical_cast(al,
                    b.StringConstant(kernel_name, name_type), ASR::CChar)));
            load_args.push_back(al, call_arg(loc, b.i32(kernel_name.size())));
            out.push_back(al, b.Assignment(gpu_kernel,
                b.Call(load_sym, load_args, b.CPtr())));

            std::map<int64_t, const ASR::gpu_scratch_buffer_t*> scratch;
            for (size_t i = 0; i < x.n_scratch; i++) {
                scratch[x.m_scratch[i].m_argument_index] = &x.m_scratch[i];
            }
            // The bytes of `n` elements of the array `arg`.
            auto element_bytes = [&](ASR::expr_t *arg, ASR::expr_t *n) {
                ASR::ttype_t *element = ASRUtils::duplicate_type(al,
                    ASRUtils::extract_type(ASRUtils::expr_type(arg)));
                return b.Mul(b.i2i_t(n, int64), ASRUtils::EXPR(
                    ASR::make_SizeOfType_t(al, loc, element, int64,
                        nullptr)));
            };
            // The scratch buffers a packed launch holds in its combined
            // buffer, which the host does allocate.
            std::vector<ASR::expr_t*> host_scratch;

            for (size_t i = 0; i < layout.n_buffers; i++) {
                const auto &entry = layout.m_buffers[i];
                if (entry.m_member) continue;
                ASR::expr_t *arg = x.m_args[entry.m_argument_index].m_value;
                auto sc = scratch.find(entry.m_argument_index);
                if (sc != scratch.end()) {
                    // Nothing of a scratch buffer is copied in. A packed
                    // launch hands every array over in one buffer the host
                    // fills, so there the host holds it as well.
                    ASR::expr_t *bytes = element_bytes(arg,
                        sc->second->m_size);
                    if (layout.m_packed) {
                        Vec<ASR::dimension_t> dims;
                        dims.reserve(al, 1);
                        ASR::dimension_t dim;
                        dim.loc = loc;
                        dim.m_start = b.i32(1);
                        dim.m_length = sc->second->m_size;
                        dims.push_back(al, dim);
                        out.push_back(al, b.Allocate(arg, dims.p, dims.n));
                        buffers.push_back({arg, address_of(loc, arg), bytes});
                        if (sc->second->m_read_back == nullptr) {
                            host_scratch.push_back(arg);
                        }
                    } else {
                        buffers.push_back({arg, nullptr, bytes, sc->second});
                    }
                    continue;
                }
                ASR::ttype_t *arg_type = ASRUtils::expr_type(arg);
                ASR::Variable_t *kparam = gpu_argument_variable(entry);
                {
                    // An array the caller only knows through a descriptor
                    // may be a section of something larger, with a stride
                    // between its elements. The device is handed a block of
                    // bytes, so such an argument is copied into a
                    // contiguous temporary first, and copied back after
                    // when the kernel writes it.
                    bool written = entry.m_written;
                    ASR::expr_t *buffer_arg = plain_struct_argument(loc,
                        out, arg, kparam, written, writebacks);
                    buffer_arg = contiguous_argument(loc, out,
                        buffer_arg, written, writebacks);
                    buffers.push_back({buffer_arg,
                        address_of(loc, buffer_arg),
                        buffer_byte_size(loc, buffer_arg)});
                    buffers.back().host_reads_writes = written;
                    if (ASRUtils::is_array(arg_type)) {
                        // The buffers the device reads a component through
                        // are named after the kernel's own parameter, so
                        // the actual need not be a plain variable: a
                        // component chain names one array just as well.
                        decompose_struct_members(loc, out, arg,
                            entry.m_variable, buffers, writebacks, cleanups,
                            *kernel, entry.m_written);
                    }
                }
            }
            LCOMPILERS_ASSERT(buffers.size() == layout.n_buffers);

            // How the kernel uses each buffer, which decides whether the
            // device keeps it after the launch (lfortran_gpu_runtime.h).
            auto access = [&](bool written) {
                return b.i32(written ? 1 : 0);
            };
            // An array the kernel assigns whole before it reads it, when
            // the launch says so, need not be copied to the device first.
            auto buffer_access = [&](size_t i) {
                const auto &entry = layout.m_buffers[i];
                if (!buffers[i].host_reads_writes) return access(false);
                if (!entry.m_written || entry.m_member
                        || entry.m_kind != ASR::gpu_argument_kindType::GpuArray) {
                    return access(entry.m_written);
                }
                ASR::expr_t *actual = x.m_args[entry.m_argument_index].m_value;
                for (size_t j = 0; j < x.n_overwritten; j++) {
                    if (!gpu_same_designator(x.m_overwritten[j], actual)) {
                        continue;
                    }
                    ASRUtils::ExprStmtDuplicator dup(al);
                    dup.success = true;
                    return ASRUtils::EXPR(ASR::make_IfExp_t(al, loc,
                        dup.duplicate_expr(x.m_overwritten_if[j]),
                        b.i32(2), b.i32(1), int32, nullptr));
                }
                return access(true);
            };
            bool any_written = false;
            for (size_t i = 0; i < layout.n_buffers; i++) {
                any_written |= layout.m_buffers[i].m_written;
            }
            ASR::symbol_t *set_buffer_sym = runtime_subroutine(loc,
                "lfortran_gpu_set_buffer_arg",
                {b.CPtr(), int32, b.CPtr(), int64, int32},
                {true, true, true, true, true});
            ASR::symbol_t *set_scratch_sym = runtime_subroutine(loc,
                "lfortran_gpu_set_scratch_arg", {b.CPtr(), int32, int64},
                {true, true, true});
            int buffer_idx = 0;
            std::vector<std::pair<BufferArg, int>> read_backs;
            std::vector<PackedBuffer> packed_buffers;
            ASR::expr_t *packed = nullptr;
            ASR::expr_t *packed_size = nullptr;
            if (layout.m_packed) {
                // Metal binds at most 31 buffers, so past that the device
                // code generator puts every array into one combined buffer
                // and reads each one at an offset handed over as a scalar.
                packed_size = declare_local(loc, "gpu_packed_size", int64);
                out.push_back(al, b.Assignment(packed_size, b.i64(0)));
                for (size_t i = 0; i < buffers.size(); i++) {
                    ASR::expr_t *size = declare_local(loc, "gpu_buffer_size",
                        int64);
                    ASR::expr_t *offset = declare_local(loc, "gpu_offset",
                        int64);
                    out.push_back(al, b.Assignment(size,
                        buffers[i].byte_size));
                    // Round the running total up to the buffer alignment.
                    out.push_back(al, b.Assignment(offset, b.Mul(
                        b.Div(b.Add(packed_size, b.i64(PACKED_BUFFER_ALIGN - 1)),
                            b.i64(PACKED_BUFFER_ALIGN)),
                        b.i64(PACKED_BUFFER_ALIGN))));
                    out.push_back(al, b.Assignment(packed_size,
                        b.Add(offset, size)));
                    packed_buffers.push_back({buffers[i].arg, offset, size});
                }
                packed = declare_local(loc, "gpu_packed",
                    b.allocatable(b.Array({-1}, int8)));
                out.push_back(al, allocate_bytes(loc, packed, packed_size));
                for (auto &buffer : packed_buffers) {
                    out.push_back(al, memcpy_call(loc,
                        address_of(loc, b.ArrayItem_01(packed,
                            {b.Add(buffer.offset, b.i64(1))})),
                        address_of(loc, buffer.arg), buffer.byte_size));
                }
                Vec<ASR::call_arg_t> args;
                args.reserve(al, 5);
                args.push_back(al, call_arg(loc, gpu_kernel));
                args.push_back(al, call_arg(loc, b.i32(buffer_idx++)));
                args.push_back(al, call_arg(loc, address_of(loc, packed)));
                args.push_back(al, call_arg(loc, packed_size));
                args.push_back(al, call_arg(loc, access(any_written)));
                out.push_back(al, b.SubroutineCall(set_buffer_sym, args));
            } else {
                for (size_t i = 0; i < buffers.size(); i++) {
                    BufferArg &buffer = buffers[i];
                    Vec<ASR::call_arg_t> args;
                    args.reserve(al, 5);
                    args.push_back(al, call_arg(loc, gpu_kernel));
                    if (buffer.scratch != nullptr) {
                        read_backs.push_back({buffer, buffer_idx});
                        args.push_back(al, call_arg(loc,
                            b.i32(buffer_idx++)));
                        args.push_back(al, call_arg(loc, buffer.byte_size));
                        out.push_back(al, b.SubroutineCall(set_scratch_sym,
                            args));
                        continue;
                    }
                    args.push_back(al, call_arg(loc, b.i32(buffer_idx++)));
                    args.push_back(al, call_arg(loc, buffer.address));
                    args.push_back(al, call_arg(loc, buffer.byte_size));
                    args.push_back(al, call_arg(loc, buffer_access(i)));
                    out.push_back(al, b.SubroutineCall(set_buffer_sym, args));
                }
            }

            for (size_t i = 0; i < layout.n_scalars; i++) {
                const auto &entry = layout.m_scalars[i];
                ASR::expr_t *value = x.m_args[entry.m_argument_index].m_value;
                if (entry.m_kind == ASR::gpu_argument_kindType::GpuArrayExtent
                        && scratch.count(entry.m_argument_index)) {
                    value = b.i2i_t(scratch[entry.m_argument_index]->m_size,
                        entry.m_type);
                } else if (entry.m_kind ==
                        ASR::gpu_argument_kindType::GpuArrayExtent) {
                    value = b.ArraySize(value, b.i32(entry.m_dimension + 1),
                        entry.m_type);
                } else if (entry.m_kind ==
                        ASR::gpu_argument_kindType::GpuPackedOffset) {
                    value = b.i2i_t(packed_buffers[entry.m_dimension].offset,
                        entry.m_type);
                }
                scalar_fields.push_back({gpu_argument_name(entry, layout),
                    entry.m_type});
                scalar_values.push_back(value);
            }
            if (!scalar_fields.empty()) {
                ASR::symbol_t *struct_sym = get_scalar_args_struct(loc,
                    kernel, scalar_fields);
                ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(struct_sym);
                ASR::ttype_t *struct_type = ASRUtils::make_StructType_t_util(
                    al, loc, struct_sym, true);
                ASR::expr_t *scalars = declare_local(loc, "gpu_scalar_args",
                    struct_type, struct_sym);
                for (size_t i = 0; i < scalar_fields.size(); i++) {
                    ASR::symbol_t *member = st->m_symtab->get_symbol(
                        scalar_fields[i].first);
                    ASR::expr_t *target = ASRUtils::EXPR(
                        ASR::make_StructInstanceMember_t(al, loc, scalars,
                            member, ASRUtils::symbol_type(member), nullptr));
                    out.push_back(al, b.Assignment(target,
                        scalar_values[i]));
                }
                Vec<ASR::call_arg_t> args;
                args.reserve(al, 4);
                args.push_back(al, call_arg(loc, gpu_kernel));
                args.push_back(al, call_arg(loc, b.i32(buffer_idx++)));
                args.push_back(al, call_arg(loc, address_of(loc, scalars)));
                args.push_back(al, call_arg(loc, ASRUtils::EXPR(
                    ASR::make_SizeOfType_t(al, loc, struct_type, int64,
                        nullptr))));
                out.push_back(al, b.SubroutineCall(runtime_subroutine(loc,
                    "lfortran_gpu_set_scalar_arg",
                    {b.CPtr(), int32, b.CPtr(), int64},
                    {true, true, true, true}), args));
            }

            // The threads that run the body are numbered from zero, and
            // the runtime starts whole blocks of them, given as int[3]: the
            // kernel sends the threads past the last back before its body.
            ASR::expr_t *block_size = b.i2i_t(x.m_block_size, int64);
            ASR::ttype_t *dim3 = b.Array({3}, int32);
            ASR::expr_t *grid = declare_local(loc, "gpu_grid", dim3);
            ASR::expr_t *block = declare_local(loc, "gpu_block", dim3);
            fill_geometry(loc, out, grid, b.Div(b.Sub(b.Add(started,
                block_size), b.i64(1)), block_size));
            fill_geometry(loc, out, block, x.m_block_size);

            // A block local variable length array in the kernel becomes an
            // extra device buffer holding one instance per thread that runs
            // the body, because the device languages have no variable
            // length arrays. What a thread finds in its instance is
            // undefined, as for any local, and nothing reads it after the
            // launch, so it is a scratch buffer: only the device holds it.
            for (size_t w = 0; w < layout.n_workspaces; w++) {
                const ASR::gpu_workspace_t &workspace = layout.m_workspaces[w];
                ASR::expr_t *n_elements = threads;
                for (size_t d = 0; d < workspace.n_dims; d++) {
                    const auto &dim = workspace.m_dims[d];
                    ASR::expr_t *extent = dim.m_parameter
                        ? x.m_args[gpu_parameter_index(*kernel,
                            dim.m_parameter)].m_value
                        : dim.m_extent;
                    n_elements = b.Mul(n_elements, b.i2i_t(extent, int64));
                }
                Vec<ASR::call_arg_t> args;
                args.reserve(al, 3);
                args.push_back(al, call_arg(loc, gpu_kernel));
                args.push_back(al, call_arg(loc,
                    b.i32(workspace.m_buffer_index)));
                args.push_back(al, call_arg(loc, b.Mul(n_elements,
                    b.i64(workspace.m_element_size))));
                out.push_back(al, b.SubroutineCall(set_scratch_sym, args));
            }

            Vec<ASR::call_arg_t> launch_args;
            launch_args.reserve(al, 4);
            launch_args.push_back(al, call_arg(loc, ctx));
            launch_args.push_back(al, call_arg(loc, gpu_kernel));
            launch_args.push_back(al, call_arg(loc, address_of(loc, grid)));
            launch_args.push_back(al, call_arg(loc, address_of(loc, block)));
            out.push_back(al, b.SubroutineCall(runtime_subroutine(loc,
                "lfortran_gpu_launch",
                {b.CPtr(), b.CPtr(), b.CPtr(), b.CPtr()},
                {true, true, true, true}), launch_args));
            // The launch does not wait for the kernel. What the host reads
            // back below -- the copies it made of its own, and the part of
            // a scratch buffer the kernel used, which a value the kernel
            // wrote counts -- needs the kernel to be done: wait for it
            // here. The arguments the device keeps are waited for where the
            // host next uses them (gpu_launch_sync.cpp).
            bool reads_back = false;
            for (auto &read_back : read_backs) {
                reads_back |= read_back.first.scratch->m_read_back != nullptr;
            }
            if ((!packed_buffers.empty() && any_written)
                    || !writebacks.empty() || reads_back) {
                Vec<ASR::call_arg_t> sync_args;
                sync_args.reserve(al, 1);
                sync_args.push_back(al, call_arg(loc, ctx));
                out.push_back(al, b.SubroutineCall(runtime_subroutine(loc,
                    "lfortran_gpu_sync", {b.CPtr()}, {true}), sync_args));
            }
            // The host reads back the leading part of a scratch buffer the
            // launch says it reads, into the array it allocates for it.
            for (auto &read_back : read_backs) {
                const BufferArg &buffer = read_back.first;
                if (buffer.scratch->m_read_back == nullptr) continue;
                ASR::expr_t *n = declare_local(loc, "gpu_read_back", int32);
                out.push_back(al, b.Assignment(n, b.i2i_t(
                    buffer.scratch->m_read_back, int32)));
                Vec<ASR::dimension_t> dims;
                dims.reserve(al, 1);
                ASR::dimension_t dim;
                dim.loc = loc;
                dim.m_start = b.i32(1);
                dim.m_length = n;
                dims.push_back(al, dim);
                out.push_back(al, b.Allocate(buffer.arg, dims.p, dims.n));
                Vec<ASR::call_arg_t> args;
                args.reserve(al, 4);
                args.push_back(al, call_arg(loc, gpu_kernel));
                args.push_back(al, call_arg(loc, b.i32(read_back.second)));
                args.push_back(al, call_arg(loc, address_of(loc,
                    buffer.arg)));
                args.push_back(al, call_arg(loc, element_bytes(buffer.arg,
                    n)));
                out.push_back(al, b.SubroutineCall(runtime_subroutine(loc,
                    "lfortran_gpu_read_scratch_arg",
                    {b.CPtr(), int32, b.CPtr(), int64},
                    {true, true, true, true}), args));
            }
            // Release this launch's handle after any requested scratch
            // readback. The runtime manages buffer lifetimes, including
            // storage cached for later launches.
            Vec<ASR::call_arg_t> release_args;
            release_args.reserve(al, 1);
            release_args.push_back(al, call_arg(loc, gpu_kernel));
            out.push_back(al, b.SubroutineCall(runtime_subroutine(loc,
                "lfortran_gpu_release_kernel", {b.CPtr()}, {true}),
                release_args));
            for (size_t i = 0; i < packed_buffers.size(); i++) {
                if (!layout.m_buffers[i].m_written) continue;
                const PackedBuffer &buffer = packed_buffers[i];
                out.push_back(al, memcpy_call(loc,
                    address_of(loc, buffer.arg),
                    address_of(loc, b.ArrayItem_01(packed,
                        {b.Add(buffer.offset, b.i64(1))})),
                    buffer.byte_size));
            }
            if (packed) out.push_back(al, b.Deallocate(packed));
            for (ASR::expr_t *arg : host_scratch) {
                out.push_back(al, b.Deallocate(arg));
            }
            for (ASR::stmt_t *writeback : writebacks) {
                out.push_back(al, writeback);
            }
            for (ASR::stmt_t *cleanup : cleanups) {
                out.push_back(al, cleanup);
            }
            return true;
        }

        void fill_geometry(const Location &loc, Vec<ASR::stmt_t*> &out,
                ASR::expr_t *dims, ASR::expr_t *size) {
            ASRUtils::ASRBuilder b(al, loc);
            out.push_back(al, b.Assignment(b.ArrayItem_01(dims, {b.i32(1)}),
                b.i2i_t(size, int32)));
            out.push_back(al, b.Assignment(b.ArrayItem_01(dims, {b.i32(2)}),
                b.i32(1)));
            out.push_back(al, b.Assignment(b.ArrayItem_01(dims, {b.i32(3)}),
                b.i32(1)));
        }

};





ASR::expr_t* gpu_associate_selector(ASR::symbol_t *name) {
    name = ASRUtils::symbol_get_past_external(name);
    if (!ASR::is_a<ASR::Variable_t>(*name)) return nullptr;
    SymbolTable *scope = ASR::down_cast<ASR::Variable_t>(name)
        ->m_parent_symtab;
    if (scope == nullptr || scope->asr_owner == nullptr
            || !ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) {
        return nullptr;
    }
    ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
    if (!ASR::is_a<ASR::AssociateBlock_t>(*owner)) return nullptr;
    ASR::AssociateBlock_t *block = ASR::down_cast<ASR::AssociateBlock_t>(
        owner);
    ASR::expr_t *selector = nullptr;
    for (size_t i = 0; i < block->n_body; i++) {
        if (!ASR::is_a<ASR::Associate_t>(*block->m_body[i])) continue;
        ASR::Associate_t *a = ASR::down_cast<ASR::Associate_t>(
            block->m_body[i]);
        if (!ASR::is_a<ASR::Var_t>(*a->m_target)
                || ASR::down_cast<ASR::Var_t>(a->m_target)->m_v != name) {
            continue;
        }
        if (selector != nullptr) return nullptr;
        selector = a->m_value;
    }
    return selector;
}

// Whether `e` designates array elements laid out end to end.
static bool designates_contiguous(ASR::expr_t *e) {
    switch (e->type) {
        case ASR::exprType::Var:
            return !gpu_argument_may_be_strided(e);
        case ASR::exprType::ArrayPhysicalCast:
            return designates_contiguous(
                ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg);
        case ASR::exprType::StructInstanceMember: {
            ASR::StructInstanceMember_t *m =
                ASR::down_cast<ASR::StructInstanceMember_t>(e);
            ASR::ttype_t *type = ASRUtils::symbol_type(m->m_m);
            if (ASRUtils::is_pointer(type)) return false;
            // A component of one value: an allocatable one is its own
            // block of memory, and one of a fixed shape is part of the
            // value.
            return !ASRUtils::is_array(ASRUtils::expr_type(m->m_v));
        }
        default:
            return false;
    }
}

bool gpu_argument_may_be_strided(ASR::expr_t *arg) {
    if (!ASR::is_a<ASR::Var_t>(*arg)) return false;
    ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(arg)->m_v);
    if (!ASR::is_a<ASR::Variable_t>(*sym)) return false;
    ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
    if (ASRUtils::is_pointer(var->m_type)) {
        // An associate name is what it is bound to.
        ASR::expr_t *selector = gpu_associate_selector(sym);
        return selector == nullptr || !designates_contiguous(selector);
    }
    if (var->m_intent == ASR::intentType::Local) return false;
    ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(
        var->m_type);
    if (!ASR::is_a<ASR::Array_t>(*t)) return false;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(t);
    // Assumed shape: no extent of its own to lay out.
    for (size_t d = 0; d < arr->n_dims; d++) {
        if (arr->m_dims[d].m_length == nullptr) return true;
    }
    return false;
}

bool gpu_argument_is_polymorphic_value(ASR::expr_t *arg) {
    ASR::ttype_t *arg_type = ASRUtils::expr_type(arg);
    if (ASRUtils::is_array(arg_type)) return false;
    ASR::ttype_t *bare = ASRUtils::extract_type(arg_type);
    if (!ASR::is_a<ASR::StructType_t>(*bare)) return false;
    if (!ASRUtils::is_class_type(bare)) return false;
    if (ASRUtils::is_unlimited_polymorphic_type(arg_type)) return false;
    ASR::symbol_t *struct_sym = ASRUtils::symbol_get_past_external(
        ASRUtils::get_struct_sym_from_struct_expr(arg));
    return struct_sym != nullptr && ASR::is_a<ASR::Struct_t>(*struct_sym);
}

GpuBufferHandling gpu_launch_buffer_handling(
        const ASR::GpuKernelLaunch_t &launch,
        const ASR::gpu_kernel_argument_t &entry) {
    for (size_t i = 0; i < launch.n_scratch; i++) {
        if (launch.m_scratch[i].m_argument_index == entry.m_argument_index) {
            return GpuBufferHandling::Scratch;
        }
    }
    const ASR::Function_t *kernel = ASR::down_cast<ASR::Function_t>(
        ASRUtils::symbol_get_past_external(launch.m_kernel));
    if (kernel->m_gpu->m_packed || entry.m_member != nullptr) {
        return GpuBufferHandling::HostCopy;
    }
    ASR::expr_t *arg = launch.m_args[entry.m_argument_index].m_value;
    if (gpu_argument_is_polymorphic_value(arg)) {
        return GpuBufferHandling::HostCopyShallow;
    }
    ASR::ttype_t *type = ASRUtils::expr_type(arg);
    if (ASRUtils::is_array(type)
            && !ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(type))
            && gpu_argument_may_be_strided(arg)) {
        return GpuBufferHandling::HostCopy;
    }
    return GpuBufferHandling::Storage;
}

void pass_device_launch_expand(Allocator &al, ASR::TranslationUnit_t &unit,
                               const LCompilers::PassOptions &pass_options) {
    if (!gpu_device_capabilities(pass_options).device_selected()) {
        return;
    }
    DeviceLaunchExpandVisitor v(al, unit);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers

#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/assert.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/device_launch_expand.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_kernel_abi.h>
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
Expands the high level GpuKernelLaunch and GpuSync statements into explicit
ASR that calls the `lfortran_gpu_*` runtime, so that the host side of a
kernel launch is ordinary ASR: it shows up in --show-asr, every backend can
lower it, and later passes can optimise it.

The generated sequence mirrors the argument layout that the device code
generators (asr_to_metal.cpp and asr_to_cuda.cpp) expect:

    ctx    = lfortran_gpu_init()
    kernel = lfortran_gpu_load_kernel(ctx, "<kernel name>", <name length>)
    call lfortran_gpu_set_buffer_arg(kernel, 0, c_loc(a), size_in_bytes(a))
    ...
    scalars%x = x                       ! one struct holding every scalar
    call lfortran_gpu_set_scalar_arg(kernel, n, c_loc(scalars), sizeof(scalars))
    grid = [grid_size, 1, 1]
    block = [block_size, 1, 1]
    call lfortran_gpu_launch(ctx, kernel, c_loc(grid), c_loc(block))

The finalized Function.gpu layout fixes the buffer order, scalar fields and
workspace bindings. This pass expands that contract without deciding whether
the loop can be offloaded or deriving workspace extents again.
*/
class DeviceLaunchExpandVisitor :
        public PassUtils::PassVisitor<DeviceLaunchExpandVisitor>
{
    public:

        DeviceLaunchExpandVisitor(Allocator &al_,
                ASR::TranslationUnit_t &unit_,
                const PassOptions &pass_options_) :
            PassVisitor(al_, nullptr), unit(unit_),
            pass_options(pass_options_) {}

        void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
            LCOMPILERS_ASSERT(ASR::down_cast<ASR::Function_t>(
                x.m_kernel)->m_gpu != nullptr);
            Vec<ASR::stmt_t*> stmts;
            stmts.reserve(al, 8);
            if (!expand_launch(x, stmts)) {
                remove_original_stmt = true;
                return;
            }
            pass_result.reserve(al, stmts.size());
            for (size_t i = 0; i < stmts.size(); i++) {
                pass_result.push_back(al, stmts[i]);
            }
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

    private:

        ASR::TranslationUnit_t &unit;
        const PassOptions &pass_options;
        std::map<ASR::Function_t*, ASR::symbol_t*> scalar_arg_structs;
        // Size of the first element of a decomposed struct member. A
        // member sized at run time from another one, and a workspace sized
        // from a member, both read it.
        std::map<GpuStructMemberKey, ASR::expr_t*> member_first_sizes;
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
            return b.Mul(b.i2i_t(b.ArraySize(arg, nullptr, int32), int64),
                ASRUtils::EXPR(ASR::make_SizeOfType_t(al, loc, element,
                    int64, nullptr)));
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
                const ASR::Function_t &kernel,
                ASR::call_arg_t *launch_args, size_t n_launch_args) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::Struct_t *st = gpu_struct_definition(
                ASRUtils::get_struct_sym_from_struct_expr(arg));
            if (!st) return;
            std::string arg_name = ASRUtils::symbol_name(parameter);
            std::map<GpuStructMemberKey, int64_t> write_sizes =
                find_struct_member_vla_write_sizes(kernel,
                    gpu_kernel_workspaces(kernel));
            std::map<GpuStructMemberKey, GpuStructMemberKey> runtime_sources =
                find_struct_member_vla_runtime_sources(kernel);
            std::map<std::string, const GpuVlaWorkspace*> no_workspaces;
            std::map<GpuStructMemberKey, GpuMemberShape> shapes =
                kernel_struct_member_shapes(kernel, no_workspaces);
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

                // The kernel gives the component of the elements it writes a
                // size: the same for every element, or one the kernel works
                // out in each iteration from what `shape` says.
                GpuStructMemberKey key{arg_name, member_name};
                auto write_size = write_sizes.find(key);
                auto source = runtime_sources.find(key);
                auto shape = shapes.find(key);
                const GpuMemberShape *written_shape =
                    shape != shapes.end() ? &shape->second : nullptr;
                std::vector<ASR::expr_t*> uniform_extents;
                if (write_size != write_sizes.end()) {
                    uniform_extents.push_back(b.i32(write_size->second));
                } else if (source != runtime_sources.end()) {
                    auto first = member_first_sizes.find(source->second);
                    uniform_extents.push_back(first != member_first_sizes.end()
                        ? first->second : b.i32(1));
                }

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
                // One entry per dimension per element.
                Vec<ASR::dimension_t> size_dims;
                size_dims.reserve(al, 1);
                ASR::dimension_t size_dim;
                size_dim.loc = loc;
                size_dim.m_start = b.i32(1);
                size_dim.m_length = rank > 1
                    ? b.Mul(n, b.i32((int)rank)) : n;
                size_dims.push_back(al, size_dim);
                out.push_back(al, b.Allocate(sizes, size_dims.p,
                    size_dims.n));
                out.push_back(al, b.Assignment(total, b.i32(0)));
                std::vector<ASR::stmt_t*> measure;
                if (written_shape || !uniform_extents.empty()) {
                    fit_written_component(loc, out, measure, arg, member,
                        arg_name, member_name, rank, k, kernel, launch_args,
                        n_launch_args, written_shape, uniform_extents);
                }
                measure.push_back(b.Assignment(b.ArrayItem_01(offsets, {k}),
                    total));
                // The component of an element the loop does not write need
                // not be allocated; it holds nothing to hand over.
                bool may_be_unallocated = ASRUtils::is_allocatable(
                    ASRUtils::symbol_type(member));
                std::vector<ASR::stmt_t*> extents, no_extents;
                for (size_t d = 0; d < rank; d++) {
                    extents.push_back(b.Assignment(
                        member_extent(loc, sizes, k, rank, d),
                        b.ArraySize(struct_member(loc, arg, k, member),
                            rank > 1 ? b.i32((int)d + 1) : nullptr,
                            int32)));
                    no_extents.push_back(b.Assignment(
                        member_extent(loc, sizes, k, rank, d), b.i32(0)));
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
                member_first_sizes[key] = member_element_count(loc, sizes,
                    b.i32(1), rank);
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
                ASR::expr_t *sizes_bytes = rank > 1
                    ? b.Mul(index_bytes, b.i64((int64_t)rank))
                    : index_bytes;
                buffers.push_back({data, address_of(loc, data), data_bytes});
                buffers.push_back({offsets, address_of(loc, offsets),
                    index_bytes});
                buffers.push_back({sizes, address_of(loc, sizes),
                    sizes_bytes});

                if (!element_is_empty) {
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
                writebacks.push_back(b.Deallocate(data));
                writebacks.push_back(b.Deallocate(offsets));
                writebacks.push_back(b.Deallocate(sizes));
            }
        }

        // Makes the component `member` of the elements of `arg` the loop
        // writes fit what the kernel writes into it: the size is
        // `uniform_extents` for every element, or the extents `shape` gives
        // each one in the iteration that writes it.
        //
        // Assigning to an allocatable component allocates it, or allocates
        // it again with another size, only with --realloc-lhs-arrays, as for
        // any assignment; only then does the host do so before the launch.
        // Without it the component has to fit already, which bounds checking
        // checks with the check an assignment gets.
        //
        // Only the elements the loop writes are looked at, in the iterations
        // that write them: the host replays the iterations and the `if`
        // tests around the write, and maps the loop indices through the
        // element's subscripts. Any other element is left as it is.
        //
        // What the host cannot work out before the loop runs it does not
        // guess:
        // * When the component can get more than one size, or its size reads
        //   something the host cannot evaluate, the host cannot allocate it.
        //   Bounds checking then only checks that a component every write
        //   gives a size is allocated.
        // * When the host cannot tell which elements the loop writes, it
        //   changes no allocated component and checks nothing. With the
        //   option it gives a component that is not allocated a size that is
        //   the same for every element.
        void fit_written_component(const Location &loc,
                Vec<ASR::stmt_t*> &out, std::vector<ASR::stmt_t*> &measure,
                ASR::expr_t *arg, ASR::symbol_t *member,
                const std::string &arg_name, const std::string &member_name,
                size_t rank, ASR::expr_t *k, const ASR::Function_t &kernel,
                ASR::call_arg_t *launch_args, size_t n_launch_args,
                const GpuMemberShape *shape,
                const std::vector<ASR::expr_t*> &uniform_extents) {
            bool realloc = pass_options.realloc_lhs_arrays;
            bool checks = pass_options.bounds_checking;
            if (!realloc && !checks) return;
            ASRUtils::ASRBuilder b(al, loc);
            auto extents_for = [&](
                    const std::map<ASR::symbol_t*, ASR::expr_t*> &indices) {
                std::vector<ASR::expr_t*> extents;
                // A component that can get more than one size has no one
                // size to give it.
                if (shape && !shape->single) return extents;
                if (!uniform_extents.empty()) {
                    ASRUtils::ExprStmtDuplicator duplicator(al);
                    for (ASR::expr_t *extent : uniform_extents) {
                        extents.push_back(duplicator.duplicate_expr(extent));
                    }
                } else if (shape) {
                    extents = gpu_host_member_extents(al, kernel, launch_args,
                        n_launch_args, *shape, indices);
                }
                return extents;
            };
            GpuHostIterations iterations;
            auto new_local = [&](const std::string &name,
                    ASR::ttype_t *type) {
                return declare_local(loc, name, type);
            };
            std::vector<ASR::expr_t*> subscripts, tests;
            bool written_known = shape && gpu_host_iterations(al, kernel,
                launch_args, n_launch_args, new_local, iterations);
            if (written_known) {
                subscripts = gpu_host_element_subscripts(al, kernel,
                    launch_args, n_launch_args, *shape, arg,
                    iterations.indices);
                written_known = !subscripts.empty() &&
                    gpu_host_write_conditions(al, kernel, launch_args,
                        n_launch_args, *shape, iterations.indices, tests);
            }
            if (!written_known) {
                std::vector<ASR::expr_t*> extents = extents_for({});
                if (realloc && extents.size() == rank) {
                    std::vector<ASR::stmt_t*> fit;
                    std::vector<ASR::expr_t*> lengths =
                        evaluate_extents(loc, extents, fit);
                    fit.push_back(b.If(b.Not(is_allocated(loc,
                        struct_member(loc, arg, k, member))),
                        {allocate_with(loc, struct_member(loc, arg, k,
                            member), lengths)}, {}));
                    measure.insert(measure.end(), fit.begin(), fit.end());
                }
                return;
            }
            ASR::expr_t *element = ASRUtils::EXPR(
                ASR::make_StructInstanceMember_t(al, loc,
                    b.ArrayItem_01(arg, subscripts), member,
                    ASRUtils::symbol_type(member), nullptr));
            std::vector<ASR::expr_t*> extents =
                extents_for(iterations.indices);
            std::vector<ASR::stmt_t*> fit;
            if (extents.size() == rank) {
                fit = realloc ? allocate_to_fit(loc, element, extents)
                    : check_fits(loc, element, extents);
            } else if (checks && shape->always_shaped) {
                if (realloc) {
                    fit.push_back(require_allocated(loc, element, arg_name,
                        member_name));
                } else {
                    ASRUtils::ExprStmtDuplicator duplicator(al);
                    fit.push_back(debug_check(loc, element,
                        duplicator.duplicate_expr(element)));
                }
            }
            if (fit.empty()) return;
            // Only an iteration the tests let through writes the element,
            // and a test is only evaluated where the ones around it hold.
            for (auto test = tests.rbegin(); test != tests.rend(); ++test) {
                fit = {b.If(*test, fit, {})};
            }
            std::vector<ASR::stmt_t*> body = iterations.prologue;
            body.insert(body.end(), fit.begin(), fit.end());
            ASR::ttype_t *counter_type =
                ASRUtils::expr_type(iterations.counter);
            out.push_back(al, b.DoLoop(iterations.counter,
                b.i_t(0, counter_type),
                b.Sub(iterations.count, b.i_t(1, counter_type)), body));
        }

        // Evaluates `extents`, never below zero, into new variables.
        std::vector<ASR::expr_t*> evaluate_extents(const Location &loc,
                const std::vector<ASR::expr_t*> &extents,
                std::vector<ASR::stmt_t*> &stmts) {
            ASRUtils::ASRBuilder b(al, loc);
            std::vector<ASR::expr_t*> lengths;
            for (ASR::expr_t *extent : extents) {
                ASR::expr_t *length = declare_local(loc,
                    "gpu_component_extent", int32);
                stmts.push_back(b.Assignment(length,
                    b.Max(b.i2i_t(extent, int32), b.i32(0))));
                lengths.push_back(length);
            }
            return lengths;
        }

        ASR::stmt_t* allocate_with(const Location &loc, ASR::expr_t *x,
                const std::vector<ASR::expr_t*> &lengths) {
            ASRUtils::ASRBuilder b(al, loc);
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, lengths.size());
            for (ASR::expr_t *length : lengths) {
                ASR::dimension_t dim;
                dim.loc = loc;
                dim.m_start = b.i32(1);
                dim.m_length = length;
                dims.push_back(al, dim);
            }
            return b.Allocate(x, dims.p, dims.n);
        }

        // Allocates `component` with `extents`, first deallocating it when
        // it is allocated with other extents, as an assignment with
        // --realloc-lhs-arrays does.
        std::vector<ASR::stmt_t*> allocate_to_fit(const Location &loc,
                ASR::expr_t *component,
                const std::vector<ASR::expr_t*> &extents) {
            ASRUtils::ASRBuilder b(al, loc);
            ASRUtils::ExprStmtDuplicator duplicator(al);
            auto copy = [&]() { return duplicator.duplicate_expr(component); };
            std::vector<ASR::stmt_t*> stmts;
            std::vector<ASR::expr_t*> lengths =
                evaluate_extents(loc, extents, stmts);
            size_t rank = lengths.size();
            ASR::expr_t *differs = nullptr;
            for (size_t d = 0; d < rank; d++) {
                ASR::expr_t *one = b.NotEq(b.ArraySize(copy(),
                    rank > 1 ? b.i32((int)d + 1) : nullptr, int32),
                    lengths[d]);
                differs = differs ? b.Or(differs, one) : one;
            }
            stmts.push_back(b.If(is_allocated(loc, copy()),
                {b.If(differs, {b.Deallocate(copy()),
                    allocate_with(loc, copy(), lengths)}, {})},
                {allocate_with(loc, component, lengths)}));
            return stmts;
        }

        // The check bounds checking makes of an assignment to `component`
        // from a value with `extents`: that it is allocated, with those
        // extents.
        std::vector<ASR::stmt_t*> check_fits(const Location &loc,
                ASR::expr_t *component,
                const std::vector<ASR::expr_t*> &extents) {
            ASRUtils::ASRBuilder b(al, loc);
            std::vector<ASR::stmt_t*> stmts;
            std::vector<ASR::expr_t*> lengths =
                evaluate_extents(loc, extents, stmts);
            ASR::expr_t *value = declare_local(loc, "gpu_component_shape",
                b.allocatable(b.Array(std::vector<int64_t>(lengths.size(),
                    -1), int8)));
            stmts.push_back(allocate_with(loc, value, lengths));
            stmts.push_back(debug_check(loc, component, value));
            stmts.push_back(b.Deallocate(value));
            return stmts;
        }

        // With --realloc-lhs-arrays the assignment would allocate
        // `component`, but the host cannot work out its size before the
        // loop runs, so the program has to have allocated it.
        ASR::stmt_t* require_allocated(const Location &loc,
                ASR::expr_t *component, const std::string &arg_name,
                const std::string &member_name) {
            ASRUtils::ASRBuilder b(al, loc);
            std::string message = "the size an offloaded loop gives the "
                "component '" + member_name + "' of '" + arg_name + "' "
                "cannot be determined before the loop runs, so it cannot be "
                "allocated automatically; allocate it before the loop";
            ASR::expr_t *code = b.StringConstant(message,
                b.String(b.i32(message.size()), ASR::ExpressionLength));
            return b.If(b.Not(is_allocated(loc, component)),
                {ASRUtils::STMT(ASR::make_ErrorStop_t(al, loc, code))}, {});
        }

        ASR::stmt_t* debug_check(const Location &loc, ASR::expr_t *target,
                ASR::expr_t *value) {
            Vec<ASR::expr_t*> components;
            components.reserve(al, 1);
            components.push_back(al, value);
            return ASRUtils::STMT(ASR::make_DebugCheckArrayBounds_t(al, loc,
                target, components.p, components.n, false));
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
        // sizes buffer: the buffer carries `rank` entries per element, in
        // dimension order.
        ASR::expr_t* member_extent(const Location &loc, ASR::expr_t *sizes,
                ASR::expr_t *index, size_t rank, size_t d) {
            ASRUtils::ASRBuilder b(al, loc);
            if (rank <= 1) return b.ArrayItem_01(sizes, {index});
            return b.ArrayItem_01(sizes, {b.Add(
                b.Mul(b.Sub(index, b.i32(1)), b.i32((int)rank)),
                b.i32((int)d + 1))});
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
            int rank = ASRUtils::extract_dimensions_from_ttype(type, dims);
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
        ASR::expr_t* plain_struct_argument(const Location &loc,
                Vec<ASR::stmt_t*> &out, ASR::expr_t *arg,
                ASR::Variable_t *kparam,
                std::vector<ASR::stmt_t*> &writebacks) {
            ASR::ttype_t *arg_type = ASRUtils::expr_type(arg);
            if (ASRUtils::is_array(arg_type)) return arg;
            ASR::ttype_t *bare = ASRUtils::extract_type(arg_type);
            if (!ASR::is_a<ASR::StructType_t>(*bare)) return arg;
            if (!ASRUtils::is_class_type(bare)) return arg;
            if (ASRUtils::is_unlimited_polymorphic_type(arg_type)) return arg;
            ASR::symbol_t *struct_sym = ASRUtils::symbol_get_past_external(
                ASRUtils::get_struct_sym_from_struct_expr(arg));
            if (struct_sym == nullptr
                    || !ASR::is_a<ASR::Struct_t>(*struct_sym)) {
                return arg;
            }
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
            if (kparam->m_intent != ASR::intentType::In) {
                for (ASR::stmt_t *stmt : back) writebacks.push_back(stmt);
            }
            return tmp;
        }

        // `arg` itself when the device can read it as it stands, or a
        // contiguous copy of it when it cannot.
        ASR::expr_t* contiguous_argument(const Location &loc,
                Vec<ASR::stmt_t*> &out, ASR::expr_t *arg,
                ASR::Variable_t *kparam,
                std::vector<ASR::stmt_t*> &writebacks) {
            ASR::ttype_t *arg_type = ASRUtils::expr_type(arg);
            if (!ASRUtils::is_array(arg_type)) return arg;
            if (ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(arg_type))) {
                return arg;
            }
            if (!may_be_strided(arg)) return arg;
            ASRUtils::ASRBuilder b(al, loc);
            ASR::dimension_t *dims = nullptr;
            int rank = ASRUtils::extract_dimensions_from_ttype(
                ASRUtils::type_get_past_allocatable_pointer(arg_type), dims);
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
            if (kparam->m_intent != ASR::intentType::In && arg_is_writable) {
                Vec<ASR::stmt_t*> back;
                back.reserve(al, 1);
                copy_elementwise(loc, back, tmp, arg, rank, true);
                for (size_t k = 0; k < back.n; k++) {
                    writebacks.push_back(back.p[k]);
                }
            }
            return tmp;
        }

        // Whether the elements of `arg` may not be laid out end to end.
        // Only an array the caller reaches through a descriptor can be: a
        // dummy declared assumed-shape, or a pointer, either of which may
        // be bound to a section of something larger.
        static bool may_be_strided(ASR::expr_t *arg) {
            if (!ASR::is_a<ASR::Var_t>(*arg)) return false;
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(arg)->m_v);
            if (!ASR::is_a<ASR::Variable_t>(*sym)) return false;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
            if (ASRUtils::is_pointer(var->m_type)) return true;
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

        // False when a shape only the passes after `gpu_offload` create
        // stops the layout part way; the caller then drops the launch and
        // the reported error fails the build.
        bool expand_launch(const ASR::GpuKernelLaunch_t &x,
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
            std::vector<ASR::stmt_t*> writebacks;

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

            for (size_t i = 0; i < layout.n_buffers; i++) {
                const auto &entry = layout.m_buffers[i];
                if (entry.m_member) continue;
                ASR::expr_t *arg = x.m_args[entry.m_argument_index].m_value;
                ASR::ttype_t *arg_type = ASRUtils::expr_type(arg);
                ASR::Variable_t *kparam = gpu_argument_variable(entry);
                {
                    // An array the caller only knows through a descriptor
                    // may be a section of something larger, with a stride
                    // between its elements. The device is handed a block of
                    // bytes, so such an argument is copied into a
                    // contiguous temporary first, and copied back after
                    // when the kernel writes it.
                    ASR::expr_t *buffer_arg = plain_struct_argument(loc,
                        out, arg, kparam, writebacks);
                    buffer_arg = contiguous_argument(loc, out,
                        buffer_arg, kparam, writebacks);
                    buffers.push_back({buffer_arg,
                        address_of(loc, buffer_arg),
                        buffer_byte_size(loc, buffer_arg)});
                    if (ASRUtils::is_array(arg_type)) {
                        // The buffers the device reads a component through
                        // are named after the kernel's own parameter, so
                        // the actual need not be a plain variable: a
                        // component chain names one array just as well.
                        decompose_struct_members(loc, out, arg,
                            entry.m_variable, buffers, writebacks, *kernel,
                            x.m_args, x.n_args);
                    }
                }
            }
            LCOMPILERS_ASSERT(buffers.size() == layout.n_buffers);

            ASR::symbol_t *set_buffer_sym = runtime_subroutine(loc,
                "lfortran_gpu_set_buffer_arg",
                {b.CPtr(), int32, b.CPtr(), int64},
                {true, true, true, true});
            int buffer_idx = 0;
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
                args.reserve(al, 4);
                args.push_back(al, call_arg(loc, gpu_kernel));
                args.push_back(al, call_arg(loc, b.i32(buffer_idx++)));
                args.push_back(al, call_arg(loc, address_of(loc, packed)));
                args.push_back(al, call_arg(loc, packed_size));
                out.push_back(al, b.SubroutineCall(set_buffer_sym, args));
            } else {
                for (auto &buffer : buffers) {
                    Vec<ASR::call_arg_t> args;
                    args.reserve(al, 4);
                    args.push_back(al, call_arg(loc, gpu_kernel));
                    args.push_back(al, call_arg(loc, b.i32(buffer_idx++)));
                    args.push_back(al, call_arg(loc, buffer.address));
                    args.push_back(al, call_arg(loc, buffer.byte_size));
                    out.push_back(al, b.SubroutineCall(set_buffer_sym, args));
                }
            }

            for (size_t i = 0; i < layout.n_scalars; i++) {
                const auto &entry = layout.m_scalars[i];
                ASR::expr_t *value = x.m_args[entry.m_argument_index].m_value;
                if (entry.m_kind == ASR::gpu_argument_kindType::GpuArrayExtent) {
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

            // The runtime takes the grid and block geometry as int[3].
            ASR::ttype_t *dim3 = b.Array({3}, int32);
            ASR::expr_t *grid = declare_local(loc, "gpu_grid", dim3);
            ASR::expr_t *block = declare_local(loc, "gpu_block", dim3);
            fill_geometry(loc, out, grid, x.m_grid_size);
            fill_geometry(loc, out, block, x.m_block_size);

            // A block local variable length array in the kernel becomes an
            // extra device buffer holding one instance per thread, because
            // the device languages have no variable length arrays.
            std::vector<ASR::expr_t*> workspaces;
            for (size_t w = 0; w < layout.n_workspaces; w++) {
                const ASR::gpu_workspace_t &workspace = layout.m_workspaces[w];
                ASR::expr_t *n_elements = b.Mul(
                    b.i2i_t(x.m_grid_size, int64),
                    b.i2i_t(x.m_block_size, int64));
                for (size_t d = 0; d < workspace.n_dims; d++) {
                    const auto &dim = workspace.m_dims[d];
                    ASR::expr_t *extent = dim.m_parameter
                        ? x.m_args[gpu_parameter_index(*kernel,
                            dim.m_parameter)].m_value
                        : dim.m_extent;
                    n_elements = b.Mul(n_elements, b.i2i_t(extent, int64));
                }
                ASR::expr_t *n_bytes = b.Mul(n_elements,
                    b.i64(workspace.m_element_size));
                ASR::expr_t *buffer = declare_local(loc, "gpu_workspace",
                    b.allocatable(b.Array({-1}, int8)));
                out.push_back(al, allocate_bytes(loc, buffer, n_bytes));
                Vec<ASR::call_arg_t> args;
                args.reserve(al, 4);
                args.push_back(al, call_arg(loc, gpu_kernel));
                args.push_back(al, call_arg(loc,
                    b.i32(workspace.m_buffer_index)));
                args.push_back(al, call_arg(loc, address_of(loc, buffer)));
                args.push_back(al, call_arg(loc, n_bytes));
                out.push_back(al, b.SubroutineCall(set_buffer_sym, args));
                workspaces.push_back(buffer);
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
            for (auto &buffer : packed_buffers) {
                out.push_back(al, memcpy_call(loc,
                    address_of(loc, buffer.arg),
                    address_of(loc, b.ArrayItem_01(packed,
                        {b.Add(buffer.offset, b.i64(1))})),
                    buffer.byte_size));
            }
            if (packed) out.push_back(al, b.Deallocate(packed));
            for (ASR::stmt_t *writeback : writebacks) {
                out.push_back(al, writeback);
            }
            for (ASR::expr_t *workspace : workspaces) {
                out.push_back(al, b.Deallocate(workspace));
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





void pass_device_launch_expand(Allocator &al, ASR::TranslationUnit_t &unit,
                               const LCompilers::PassOptions &pass_options) {
    if (!gpu_device_capabilities(pass_options).device_selected()) {
        return;
    }
    DeviceLaunchExpandVisitor v(al, unit, pass_options);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers

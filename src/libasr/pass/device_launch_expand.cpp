#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/device_launch_expand.h>
#include <libasr/pass/pass_utils.h>

#include <cstdlib>
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

A launch whose argument shape is not handled yet is left untouched, and the
LLVM backend expands it as before.
*/
static std::string launch_reason;
static bool no(const char *why) { launch_reason = why; return false; }
static bool no2(const char *why) { launch_reason = why; return false; }

// A struct passed to a kernel is sized with SizeOfType, which lays it out as
// an anonymous struct of its member types. Only structs whose members are
// plain scalars and fixed size arrays are laid out that way, so anything else
// (an extended type, character, allocatable or pointer members) is left to
// the LLVM backend, which resolves the layout through the struct symbol.
// Allocatable array members are also what the device code generators
// decompose into extra flat buffers, which this pass does not build yet.
static bool struct_is_plain(ASR::symbol_t *struct_sym) {
    if (!struct_sym) return no2("struct-unknown");
    ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(struct_sym);
    if (!ASR::is_a<ASR::Struct_t>(*sym)) return no2("struct-unknown");
    ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(sym);
    if (st->m_parent) return no2("struct-parent");
    for (size_t i = 0; i < st->n_members; i++) {
        ASR::symbol_t *member = st->m_symtab->get_symbol(st->m_members[i]);
        if (!member || !ASR::is_a<ASR::Variable_t>(*member)) return no2("struct-member");
        ASR::ttype_t *type = ASR::down_cast<ASR::Variable_t>(member)->m_type;
        if (ASRUtils::is_allocatable(type) || ASRUtils::is_pointer(type)) {
            return no2("struct-alloc-member");
        }
        if (ASRUtils::is_array(type) &&
                ASRUtils::get_fixed_size_of_array(type) <= 0) {
            return no2("struct-vla-member");
        }
        ASR::ttype_t *base = ASRUtils::type_get_past_array(type);
        if (ASR::is_a<ASR::StructType_t>(*base)) {
            if (!struct_is_plain(
                    ASR::down_cast<ASR::Variable_t>(member)
                        ->m_type_declaration)) {
                return false;
            }
            continue;
        }
        if (!ASR::is_a<ASR::Integer_t>(*base) &&
                !ASR::is_a<ASR::Real_t>(*base) &&
                !ASR::is_a<ASR::Logical_t>(*base)) {
            return no2("struct-member-type");
        }
    }
    return true;
}

// True when a value of this type can be handed to the runtime as a plain
// block of bytes whose size SizeOfType computes correctly.
static bool is_supported_buffer(ASR::expr_t *arg) {
    ASR::ttype_t *base = ASRUtils::type_get_past_array(
        ASRUtils::extract_type(ASRUtils::expr_type(arg)));
    if (ASR::is_a<ASR::StructType_t>(*base)) {
        return struct_is_plain(ASRUtils::get_struct_sym_from_struct_expr(arg));
    }
    if (ASR::is_a<ASR::Integer_t>(*base) || ASR::is_a<ASR::Real_t>(*base)
            || ASR::is_a<ASR::Logical_t>(*base)) {
        return true;
    }
    return no2("buffer-elem");
}

static bool is_supported_scalar(ASR::ttype_t *type) {
    ASR::ttype_t *t = ASRUtils::extract_type(type);
    return ASR::is_a<ASR::Integer_t>(*t) || ASR::is_a<ASR::Real_t>(*t);
}

static bool same_scalar_type(ASR::ttype_t *a, ASR::ttype_t *b) {
    ASR::ttype_t *ta = ASRUtils::extract_type(a);
    ASR::ttype_t *tb = ASRUtils::extract_type(b);
    return ta->type == tb->type &&
        ASRUtils::extract_kind_from_ttype_t(ta) ==
            ASRUtils::extract_kind_from_ttype_t(tb);
}

// True when every argument of this launch has a shape the pass can expand.
static bool launch_is_supported(const ASR::GpuKernelLaunch_t &x) {
    ASR::GpuKernelFunction_t *kernel =
        ASR::down_cast<ASR::GpuKernelFunction_t>(x.m_kernel);
    if (x.n_args != kernel->n_args) return no("arg-count");
    // Packing many buffers into one, and workspaces for variable-length
    // arrays, are not expanded here yet.
    for (auto &workspace : analyze_gpu_vla_workspaces(*kernel)) {
        for (auto &dim : workspace.dims) {
            // A workspace sized from a struct member needs the struct
            // decomposition, which this pass does not build yet.
            if (dim.is_struct_member_size) return no("vla-struct-member");
            if (!dim.is_constant &&
                    dim.call_arg_index >= x.n_args) return no("vla-dim");
        }
    }
    for (size_t i = 0; i < x.n_args; i++) {
        ASR::expr_t *arg = x.m_args[i].m_value;
        if (!arg) return no("null-arg");
        ASR::ttype_t *arg_type = ASRUtils::expr_type(arg);
        ASR::Variable_t *kparam = ASR::down_cast<ASR::Variable_t>(
            ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(kernel->m_args[i])->m_v));
        if (ASRUtils::is_array(arg_type) ||
                ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(arg_type))) {
            if (!is_supported_buffer(arg)) return false;
        } else {
            if (!is_supported_scalar(arg_type)) return no("scalar-type");
            if (!same_scalar_type(arg_type, kparam->m_type)) {
                return no("scalar-kind-mismatch");
            }
        }
    }
    return true;
}

// A translation unit is expanded all at once or not at all, so that the ASR
// calls and the calls the LLVM backend emits never meet in one module.
class LaunchSupportVisitor :
        public ASR::BaseWalkVisitor<LaunchSupportVisitor>
{
    public:
        bool all_supported = true;

        void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
            if (!launch_is_supported(x)) all_supported = false;
        }
};

class DeviceLaunchExpandVisitor :
        public PassUtils::PassVisitor<DeviceLaunchExpandVisitor>
{
    public:

        DeviceLaunchExpandVisitor(Allocator &al_,
                ASR::TranslationUnit_t &unit_) :
            PassVisitor(al_, nullptr), unit(unit_) {}

        // Number of launches whose argument shape this pass does not handle
        // yet, and which are therefore left to the LLVM backend.
        size_t n_not_expanded = 0;

        void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
            Vec<ASR::stmt_t*> stmts;
            stmts.reserve(al, 8);
            if (!expand_launch(x, stmts)) {
                n_not_expanded++;
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
        // Scalar argument struct created for each kernel, by kernel name.
        std::map<std::string, ASR::symbol_t*> scalar_arg_structs;

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
                return ASRUtils::EXPR(ASR::make_SizeOfType_t(al, loc, type,
                    int64, nullptr));
            }
            ASR::ttype_t *element = ASRUtils::type_get_past_array(type);
            return b.Mul(b.i2i_t(b.ArraySize(arg, nullptr, int32), int64),
                ASRUtils::EXPR(ASR::make_SizeOfType_t(al, loc, element,
                    int64, nullptr)));
        }

        // True when the kernel takes this argument as an assumed shape array,
        // in which case the device code reads its extents from scalars.
        static bool kernel_param_is_descriptor(ASR::Variable_t *kparam) {
            if (std::string(kparam->m_name).substr(0, 2) == "__") return false;
            ASR::ttype_t *type = ASRUtils::type_get_past_allocatable(
                kparam->m_type);
            if (!ASR::is_a<ASR::Array_t>(*type)) return false;
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
            for (size_t d = 0; d < arr->n_dims; d++) {
                if (!arr->m_dims[d].m_length) return true;
            }
            return false;
        }

        // Builds, once per kernel, the struct that carries every scalar
        // argument. Its layout has to match the `__ScalarArgs_*` struct the
        // device code generator emits, so the members are created in the same
        // order.
        ASR::symbol_t* get_scalar_args_struct(const Location &loc,
                const std::string &kernel_name,
                const std::vector<std::pair<std::string, ASR::ttype_t*>> &fields) {
            auto it = scalar_arg_structs.find(kernel_name);
            if (it != scalar_arg_structs.end()) return it->second;

            SymbolTable *global_scope = unit.m_symtab;
            std::string struct_name = global_scope->get_unique_name(
                "__ScalarArgs_" + kernel_name, false);
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
            scalar_arg_structs[kernel_name] = struct_sym;
            return struct_sym;
        }

        bool expand_launch(const ASR::GpuKernelLaunch_t &x,
                Vec<ASR::stmt_t*> &out) {
            const Location &loc = x.base.base.loc;
            ASRUtils::ASRBuilder b(al, loc);

            ASR::GpuKernelFunction_t *kernel =
                ASR::down_cast<ASR::GpuKernelFunction_t>(x.m_kernel);
            std::string kernel_name(kernel->m_name);
            if (!launch_is_supported(x)) return false;

            struct BufferArg {
                ASR::expr_t *arg;
                ASR::expr_t *address;
                ASR::expr_t *byte_size;
            };
            std::vector<BufferArg> buffers;
            std::vector<std::pair<std::string, ASR::ttype_t*>> scalar_fields;
            std::vector<ASR::expr_t*> scalar_values;

            for (size_t i = 0; i < x.n_args; i++) {
                ASR::expr_t *arg = x.m_args[i].m_value;
                if (!arg) return false;
                ASR::ttype_t *arg_type = ASRUtils::expr_type(arg);
                ASR::Variable_t *kparam = ASR::down_cast<ASR::Variable_t>(
                    ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(kernel->m_args[i])->m_v));
                if (ASRUtils::is_array(arg_type) ||
                        ASR::is_a<ASR::StructType_t>(
                            *ASRUtils::extract_type(arg_type))) {
                    buffers.push_back({arg, address_of(loc, arg),
                        buffer_byte_size(loc, arg)});
                } else {
                    scalar_fields.push_back({std::string(kparam->m_name),
                        ASRUtils::extract_type(arg_type)});
                    scalar_values.push_back(arg);
                }
            }

            // The device code reads the extents of an assumed shape array
            // argument from scalars appended after the value scalars.
            for (size_t i = 0; i < x.n_args; i++) {
                ASR::expr_t *arg = x.m_args[i].m_value;
                if (!ASRUtils::is_array(ASRUtils::expr_type(arg))) continue;
                ASR::Variable_t *kparam = ASR::down_cast<ASR::Variable_t>(
                    ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(kernel->m_args[i])->m_v));
                if (!kernel_param_is_descriptor(kparam)) continue;
                ASR::Array_t *kernel_arr = ASR::down_cast<ASR::Array_t>(
                    ASRUtils::type_get_past_allocatable(kparam->m_type));
                for (size_t d = 0; d < kernel_arr->n_dims; d++) {
                    scalar_fields.push_back({"__size_"
                        + std::string(kparam->m_name) + "_dim"
                        + std::to_string(d + 1), int32});
                    scalar_values.push_back(
                        b.ArraySize(arg, b.i32(d + 1), int32));
                }
            }

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

            ASR::symbol_t *set_buffer_sym = runtime_subroutine(loc,
                "lfortran_gpu_set_buffer_arg",
                {b.CPtr(), int32, b.CPtr(), int64},
                {true, true, true, true});
            int buffer_idx = 0;
            std::vector<PackedBuffer> packed_buffers;
            ASR::expr_t *packed = nullptr;
            ASR::expr_t *packed_size = nullptr;
            if (gpu_kernel_needs_buffer_packing(*kernel)) {
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
                for (size_t i = 0; i < packed_buffers.size(); i++) {
                    scalar_fields.push_back({"__offset_"
                        + std::to_string(i), int32});
                    scalar_values.push_back(
                        b.i2i_t(packed_buffers[i].offset, int32));
                }
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

            if (!scalar_fields.empty()) {
                ASR::symbol_t *struct_sym = get_scalar_args_struct(loc,
                    kernel_name, scalar_fields);
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
            for (auto &workspace : analyze_gpu_vla_workspaces(*kernel)) {
                ASR::expr_t *n_elements = b.Mul(
                    b.i2i_t(x.m_grid_size, int64),
                    b.i2i_t(x.m_block_size, int64));
                for (auto &dim : workspace.dims) {
                    n_elements = b.Mul(n_elements, dim.is_constant
                        ? b.i64(dim.constant_value)
                        : b.i2i_t(x.m_args[dim.call_arg_index].m_value,
                            int64));
                }
                ASR::expr_t *n_bytes = b.Mul(n_elements,
                    b.i64(workspace.elem_size));
                ASR::expr_t *buffer = declare_local(loc, "gpu_workspace",
                    b.allocatable(b.Array({-1}, int8)));
                out.push_back(al, allocate_bytes(loc, buffer, n_bytes));
                Vec<ASR::call_arg_t> args;
                args.reserve(al, 4);
                args.push_back(al, call_arg(loc, gpu_kernel));
                args.push_back(al, call_arg(loc,
                    b.i32(workspace.buffer_index)));
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
    if (!pass_options.gpu_offload_metal && !pass_options.gpu_offload_cuda) {
        return;
    }
    LaunchSupportVisitor support;
    support.visit_TranslationUnit(unit);
    if (std::getenv("LFORTRAN_GPU_LAUNCH_REPORT")) {
        std::fprintf(stderr, "device_launch_expand: %s\n",
            support.all_supported ? "expanded"
                : ("left to the llvm backend: " + launch_reason).c_str());
    }
    if (!support.all_supported) return;
    DeviceLaunchExpandVisitor v(al, unit);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers

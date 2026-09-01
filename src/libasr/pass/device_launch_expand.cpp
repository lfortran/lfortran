#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/device_launch_expand.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/pass_utils.h>

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

`gpu_offload` asks this pass, through gpu_launch_is_supported(), whether it
can lay out every argument of a launch the same way as the device code
generator, and keeps the loop on the host when it cannot, rather than
building a launch that would read the wrong bytes.
*/
// Why the last rejected launch could not be expanded, for the warning.
static std::string unsupported_reason;
static bool unsupported(const char *why) {
    unsupported_reason = why;
    return false;
}

static bool is_plain_scalar(ASR::ttype_t *type) {
    return ASR::is_a<ASR::Integer_t>(*type) || ASR::is_a<ASR::Real_t>(*type)
        || ASR::is_a<ASR::Logical_t>(*type);
}

// An allocatable rank one array member of a struct is not stored inline: the
// device code generators hand it over as three extra flat buffers holding
// every element's data, offset and size.
static bool struct_is_plain(ASR::symbol_t *struct_sym);
static ASR::Struct_t* get_struct(ASR::symbol_t *struct_sym);

static bool is_decomposed_member(ASR::symbol_t *member) {
    if (!member || !ASR::is_a<ASR::Variable_t>(*member)) return false;
    ASR::Variable_t *variable = ASR::down_cast<ASR::Variable_t>(member);
    if (!ASRUtils::is_allocatable(variable->m_type)) return false;
    ASR::ttype_t *inner = ASRUtils::type_get_past_allocatable(
        variable->m_type);
    if (!ASR::is_a<ASR::Array_t>(*inner)) return false;
    if (ASR::down_cast<ASR::Array_t>(inner)->n_dims != 1) return false;
    ASR::ttype_t *element = ASRUtils::type_get_past_array(inner);
    if (ASR::is_a<ASR::StructType_t>(*element)) {
        return struct_is_plain(variable->m_type_declaration);
    }
    return is_plain_scalar(element);
}

static ASR::Struct_t* get_struct(ASR::symbol_t *struct_sym) {
    if (!struct_sym) return nullptr;
    ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(struct_sym);
    if (!ASR::is_a<ASR::Struct_t>(*sym)) return nullptr;
    return ASR::down_cast<ASR::Struct_t>(sym);
}

// A struct passed to a kernel is sized with SizeOfType, which lays it out as
// an anonymous struct of its member types. Only structs whose members are
// plain scalars, fixed size arrays, nested plain structs and decomposed
// allocatable arrays are laid out that way; anything else (an extended type,
// character or pointer members) has no device layout at all, so a loop that
// needs it stays on the host.
static bool struct_is_plain(ASR::symbol_t *struct_sym) {
    ASR::Struct_t *st = get_struct(struct_sym);
    if (!st) {
        return unsupported("a derived type whose declaration is not known");
    }
    if (st->m_parent) return unsupported("an extended derived type");
    for (size_t i = 0; i < st->n_members; i++) {
        ASR::symbol_t *member = st->m_symtab->get_symbol(st->m_members[i]);
        if (!member || !ASR::is_a<ASR::Variable_t>(*member)) {
            return unsupported("a derived type with a non-data member");
        }
        if (is_decomposed_member(member)) continue;
        ASR::ttype_t *type = ASR::down_cast<ASR::Variable_t>(member)->m_type;
        if (ASRUtils::is_pointer(type)) {
            return unsupported("a derived type with a pointer member");
        }
        if (ASRUtils::is_allocatable(type)) {
            if (ASRUtils::is_array(type)) {
                return unsupported("a derived type with an allocatable array "
                    "member the gpu backend cannot decompose");
            }
            return unsupported(
                "a derived type with an allocatable scalar member");
        }
        if (ASRUtils::is_array(type) &&
                ASRUtils::get_fixed_size_of_array(type) <= 0) {
            return unsupported(
                "a derived type with an assumed shape array member");
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
        if (!is_plain_scalar(base)) {
            return unsupported(
                "a derived type with a member that is not a number");
        }
    }
    return true;
}

// True when a value of this type can be handed to the runtime as a plain
// block of bytes whose size SizeOfType computes correctly.
static bool is_supported_buffer(ASR::expr_t *arg) {
    ASR::ttype_t *arg_type = ASRUtils::expr_type(arg);
    ASR::ttype_t *base = ASRUtils::type_get_past_array(
        ASRUtils::extract_type(arg_type));
    if (ASR::is_a<ASR::StructType_t>(*base)) {
        ASR::symbol_t *struct_sym =
            ASRUtils::get_struct_sym_from_struct_expr(arg);
        if (!struct_is_plain(struct_sym)) return false;
        // Only an array of structs is decomposed; a scalar struct is passed
        // as it stands, with the members the kernel needs handed over as
        // separate arguments. Decomposing means walking the array element by
        // element, which needs a plain variable to subscript.
        if (!ASRUtils::is_array(arg_type) || ASR::is_a<ASR::Var_t>(*arg)) {
            return true;
        }
        ASR::Struct_t *st = get_struct(struct_sym);
        for (size_t i = 0; i < st->n_members; i++) {
            if (is_decomposed_member(st->m_symtab->get_symbol(
                    st->m_members[i]))) {
                return unsupported(
                    "an array of derived type that is not a plain variable");
            }
        }
        return true;
    }
    if (is_plain_scalar(base)) return true;
    return unsupported("an array whose elements are not numbers");
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
static bool launch_is_supported(ASR::symbol_t *kernel_sym,
        ASR::call_arg_t *call_args, size_t n_call_args) {
    ASR::Function_t *kernel = ASR::down_cast<ASR::Function_t>(kernel_sym);
    if (n_call_args != kernel->n_args) {
        return unsupported("a kernel that takes a different number of "
            "arguments");
    }
    for (auto &workspace : analyze_gpu_vla_workspaces(*kernel)) {
        for (auto &dim : workspace.dims) {
            if (dim.is_constant || dim.is_struct_member_size) continue;
            if (dim.is_host_expr) continue;
            if (dim.call_arg_index >= n_call_args) {
                return unsupported("a variable length array sized outside "
                    "the kernel arguments");
            }
        }
    }
    for (size_t i = 0; i < n_call_args; i++) {
        ASR::expr_t *arg = call_args[i].m_value;
        if (!arg) return unsupported("a missing argument");
        ASR::ttype_t *arg_type = ASRUtils::expr_type(arg);
        ASR::Variable_t *kparam = ASR::down_cast<ASR::Variable_t>(
            ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(kernel->m_args[i])->m_v));
        if (ASRUtils::is_array(arg_type) ||
                ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(arg_type))) {
            if (!is_supported_buffer(arg)) return false;
        } else {
            if (!is_supported_scalar(arg_type)) {
                return unsupported("a scalar that is not an integer or a "
                    "real");
            }
            if (!same_scalar_type(arg_type, kparam->m_type)) {
                return unsupported("a scalar whose kind differs from the "
                    "kernel parameter");
            }
        }
    }
    return true;
}

bool gpu_launch_is_supported(ASR::symbol_t *kernel, ASR::call_arg_t *args,
        size_t n_args, std::string &reason) {
    unsupported_reason.clear();
    if (launch_is_supported(kernel, args, n_args)) return true;
    reason = unsupported_reason;
    return false;
}

class DeviceLaunchExpandVisitor :
        public PassUtils::PassVisitor<DeviceLaunchExpandVisitor>
{
    public:

        DeviceLaunchExpandVisitor(Allocator &al_,
                ASR::TranslationUnit_t &unit_) :
            PassVisitor(al_, nullptr), unit(unit_) {}

        void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
            Vec<ASR::stmt_t*> stmts;
            stmts.reserve(al, 8);
            expand_launch(x, stmts);
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
        // Size of the first element of a decomposed struct member, by
        // "<array>.<member>". A member sized at run time from another one,
        // and a workspace sized from a member, both read it.
        std::map<std::string, ASR::expr_t*> member_first_sizes;

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

        // Splits every allocatable array member of an array of structs into
        // the three flat buffers the device code reads: the elements' data
        // laid out end to end, and their offsets and sizes. The data is
        // copied back into the members after the launch, because the kernel
        // may have written to it.
        void decompose_struct_members(const Location &loc,
                Vec<ASR::stmt_t*> &out, ASR::expr_t *arg,
                std::vector<BufferArg> &buffers,
                std::vector<ASR::stmt_t*> &writebacks,
                const ASR::Function_t &kernel) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::Struct_t *st = get_struct(
                ASRUtils::get_struct_sym_from_struct_expr(arg));
            if (!st) return;
            std::string arg_name = ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(arg)->m_v);
            std::map<std::string, int64_t> write_sizes =
                find_struct_member_vla_write_sizes(kernel,
                    analyze_gpu_vla_workspaces(kernel));
            std::map<std::string, std::string> runtime_sources =
                find_struct_member_vla_runtime_sources(kernel);
            for (size_t m = 0; m < st->n_members; m++) {
                ASR::symbol_t *member = st->m_symtab->get_symbol(
                    st->m_members[m]);
                if (!is_decomposed_member(member)) continue;
                ASR::ttype_t *member_type = ASRUtils::type_get_past_allocatable(
                    ASRUtils::symbol_type(member));
                ASR::ttype_t *element_type = ASRUtils::type_get_past_array(
                    member_type);
                // A struct with no data members occupies no bytes on the host
                // but one byte in the device language; size the buffer so
                // that every element stays addressable, and copy nothing,
                // because there is nothing to copy.
                ASR::Struct_t *element_struct = get_struct(
                    ASR::down_cast<ASR::Variable_t>(member)
                        ->m_type_declaration);
                bool element_is_empty = ASR::is_a<ASR::StructType_t>(
                    *element_type) && element_struct &&
                    element_struct->n_members == 0;
                ASR::expr_t *element_bytes = element_is_empty
                    ? b.i64(1)
                    : ASRUtils::EXPR(ASR::make_SizeOfType_t(al, loc,
                        element_type, int64, nullptr));

                // The kernel writes into a member the caller never allocated,
                // so the host has to give it storage first.
                std::string key = arg_name + "." + st->m_members[m];
                ASR::expr_t *missing_size = nullptr;
                auto write_size = write_sizes.find(key);
                if (write_size != write_sizes.end()) {
                    missing_size = b.i32(write_size->second);
                } else {
                    auto source = runtime_sources.find(key);
                    if (source != runtime_sources.end()) {
                        auto first = member_first_sizes.find(source->second);
                        missing_size = first != member_first_sizes.end()
                            ? first->second : b.i32(1);
                    }
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

                out.push_back(al, b.Assignment(n,
                    b.ArraySize(arg, nullptr, int32)));
                Vec<ASR::dimension_t> dims;
                dims.reserve(al, 1);
                ASR::dimension_t dim;
                dim.loc = loc;
                dim.m_start = b.i32(1);
                dim.m_length = n;
                dims.push_back(al, dim);
                out.push_back(al, b.Allocate(sizes, dims.p, dims.n));
                out.push_back(al, b.Allocate(offsets, dims.p, dims.n));
                out.push_back(al, b.Assignment(total, b.i32(0)));
                std::vector<ASR::stmt_t*> measure;
                if (missing_size) {
                    Vec<ASR::dimension_t> member_dims;
                    member_dims.reserve(al, 1);
                    ASR::dimension_t member_dim;
                    member_dim.loc = loc;
                    member_dim.m_start = b.i32(1);
                    member_dim.m_length = missing_size;
                    member_dims.push_back(al, member_dim);
                    measure.push_back(b.If(b.Not(is_allocated(loc,
                        struct_member(loc, arg, k, member))),
                        {b.Allocate(struct_member(loc, arg, k, member),
                            member_dims.p, member_dims.n)}, {}));
                }
                measure.push_back(b.Assignment(b.ArrayItem_01(offsets, {k}),
                    total));
                measure.push_back(b.Assignment(b.ArrayItem_01(sizes, {k}),
                    b.ArraySize(struct_member(loc, arg, k, member),
                        nullptr, int32)));
                measure.push_back(b.Assignment(total, b.Add(total,
                    b.ArrayItem_01(sizes, {k}))));
                out.push_back(al, b.DoLoop(k, b.i32(1), n, measure));
                member_first_sizes[key] = b.ArrayItem_01(sizes, {b.i32(1)});
                ASR::expr_t *data_bytes = b.Mul(b.i2i_t(total, int64),
                    element_bytes);
                out.push_back(al, allocate_bytes(loc, data, data_bytes));
                if (!element_is_empty) {
                    out.push_back(al, b.DoLoop(k, b.i32(1), n, {
                        memcpy_call(loc,
                            member_data_address(loc, data, offsets, k,
                                element_bytes),
                            address_of(loc,
                                struct_member(loc, arg, k, member)),
                            member_byte_size(loc, sizes, k,
                                element_bytes))}));
                }

                ASR::expr_t *index_bytes = b.Mul(b.i2i_t(n, int64), b.i64(4));
                buffers.push_back({data, address_of(loc, data), data_bytes});
                buffers.push_back({offsets, address_of(loc, offsets),
                    index_bytes});
                buffers.push_back({sizes, address_of(loc, sizes),
                    index_bytes});

                if (!element_is_empty) {
                    writebacks.push_back(b.DoLoop(k, b.i32(1), n, {
                        memcpy_call(loc,
                            address_of(loc,
                                struct_member(loc, arg, k, member)),
                            member_data_address(loc, data, offsets, k,
                                element_bytes),
                            member_byte_size(loc, sizes, k,
                                element_bytes))}));
                }
                writebacks.push_back(b.Deallocate(data));
                writebacks.push_back(b.Deallocate(offsets));
                writebacks.push_back(b.Deallocate(sizes));
            }
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

        // A designator the kernel writes in terms of its parameters, built
        // again over the actual arguments of this launch so the host can
        // read the same object: `self%points_(1,1,1,1)%values_` names one
        // array whichever side asks for it.
        ASR::expr_t* host_designator(const Location &loc,
                const ASR::GpuKernelLaunch_t &x,
                const ASR::Function_t *kernel, ASR::expr_t *e) {
            if (e == nullptr) return nullptr;
            ASRUtils::ASRBuilder b(al, loc);
            ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
            if (ASR::is_a<ASR::Var_t>(*v)) {
                std::string name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(v)->m_v);
                for (size_t i = 0; i < kernel->n_args; i++) {
                    std::string pname = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(kernel->m_args[i])->m_v);
                    if (pname != name) continue;
                    if (i >= x.n_args) break;
                    return x.m_args[i].m_value;
                }
                return nullptr;
            }
            if (ASR::is_a<ASR::StructInstanceMember_t>(*v)) {
                ASR::StructInstanceMember_t *sm =
                    ASR::down_cast<ASR::StructInstanceMember_t>(v);
                ASR::expr_t *base = host_designator(loc, x, kernel, sm->m_v);
                if (base == nullptr) return nullptr;
                ASR::symbol_t *st =
                    ASRUtils::get_struct_sym_from_struct_expr(base);
                if (st == nullptr) return nullptr;
                ASR::symbol_t *member = ASR::down_cast<ASR::Struct_t>(
                    ASRUtils::symbol_get_past_external(st))->m_symtab
                        ->get_symbol(ASRUtils::symbol_name(
                            ASRUtils::symbol_get_past_external(sm->m_m)));
                if (member == nullptr) return nullptr;
                return ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al,
                    loc, base, member, ASRUtils::symbol_type(member),
                    nullptr));
            }
            if (ASR::is_a<ASR::ArrayItem_t>(*v)) {
                ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(v);
                ASR::expr_t *base = host_designator(loc, x, kernel,
                    item->m_v);
                if (base == nullptr) return nullptr;
                std::vector<ASR::expr_t*> subs;
                for (size_t i = 0; i < item->n_args; i++) {
                    ASR::expr_t *sub = host_extent(loc, x, kernel,
                        item->m_args[i].m_right);
                    if (sub == nullptr) return nullptr;
                    subs.push_back(sub);
                }
                return b.ArrayItem_01(base, subs);
            }
            return nullptr;
        }

        // The extent expression of a workspace, rebuilt over the actual
        // arguments of this launch. The kernel writes an extent in terms of
        // its own parameters -- `op%m_ + 1` -- and the host has to compute
        // the same number before it dispatches, so every parameter the
        // expression names is replaced by the argument bound to it.
        // Returns nullptr when some part of it has no host counterpart.
        ASR::expr_t* host_extent(const Location &loc,
                const ASR::GpuKernelLaunch_t &x,
                const ASR::Function_t *kernel, ASR::expr_t *e) {
            if (e == nullptr) return nullptr;
            ASRUtils::ASRBuilder b(al, loc);
            ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
            if (ASR::is_a<ASR::Cast_t>(*v)) {
                return host_extent(loc, x, kernel,
                    ASR::down_cast<ASR::Cast_t>(v)->m_arg);
            }
            if (ASR::is_a<ASR::IntegerConstant_t>(*v)) return v;
            if (ASR::is_a<ASR::IntegerBinOp_t>(*v)) {
                ASR::IntegerBinOp_t *op =
                    ASR::down_cast<ASR::IntegerBinOp_t>(v);
                ASR::expr_t *l = host_extent(loc, x, kernel, op->m_left);
                ASR::expr_t *r = host_extent(loc, x, kernel, op->m_right);
                if (!l || !r) return nullptr;
                return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, l,
                    op->m_op, r, ASRUtils::expr_type(l), nullptr));
            }
            if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*v)) {
                ASR::expr_t *a = host_extent(loc, x, kernel,
                    ASR::down_cast<ASR::IntegerUnaryMinus_t>(v)->m_arg);
                if (!a) return nullptr;
                return ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al, loc,
                    a, ASRUtils::expr_type(a), nullptr));
            }
            std::vector<std::string> arg_names;
            for (size_t i = 0; i < kernel->n_args; i++) {
                arg_names.push_back(ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(kernel->m_args[i])->m_v));
            }
            // An extent of an array parameter: the actual argument has the
            // same shape, so ask it.
            std::string arr_name;
            size_t d = 0;
            if (gpu_extent_of_array_dim(v, arr_name, d)) {
                bool is_arg = false;
                for (size_t i = 0; i < arg_names.size(); i++) {
                    if (arg_names[i] != arr_name) continue;
                    is_arg = true;
                    if (i >= x.n_args || !x.m_args[i].m_value) break;
                    return b.ArraySize(x.m_args[i].m_value,
                        b.i32((int)d + 1), int32);
                }
                if (!is_arg) {
                    // A local of the kernel, whose own extent is written
                    // over the parameters. `size(t) + 1` is resolved by
                    // carrying on through `t`'s extent.
                    return host_extent(loc, x, kernel,
                        gpu_local_array_extent(kernel->m_symtab,
                            kernel->m_body, kernel->n_body, arr_name, d));
                }
            }
            size_t idx = 0;
            std::vector<std::string> path;
            if (resolve_extent_to_arg_member(v, arg_names, idx, path)) {
                if (idx >= x.n_args || !x.m_args[idx].m_value) {
                    return nullptr;
                }
                ASR::expr_t *out = x.m_args[idx].m_value;
                for (const std::string &m : path) {
                    ASR::symbol_t *st =
                        ASRUtils::get_struct_sym_from_struct_expr(out);
                    ASR::symbol_t *member = st
                        ? ASR::down_cast<ASR::Struct_t>(
                            ASRUtils::symbol_get_past_external(st))
                                ->m_symtab->get_symbol(m)
                        : nullptr;
                    if (member == nullptr) return nullptr;
                    out = ASRUtils::EXPR(ASR::make_StructInstanceMember_t(
                        al, loc, out, member,
                        ASRUtils::symbol_type(member), nullptr));
                }
                return out;
            }
            // `size(<designator>, d)` where the host can read the
            // designator: rebuild the designator over the actual and ask
            // its shape.
            if (ASR::is_a<ASR::ArraySize_t>(*v)) {
                ASR::ArraySize_t *sz = ASR::down_cast<ASR::ArraySize_t>(v);
                ASR::expr_t *host = host_designator(loc, x, kernel, sz->m_v);
                if (host != nullptr) {
                    ASR::expr_t *dim = sz->m_dim
                        ? host_extent(loc, x, kernel, sz->m_dim) : nullptr;
                    if (sz->m_dim == nullptr || dim != nullptr) {
                        return b.ArraySize(host, dim, int32);
                    }
                }
            }
            if (ASR::is_a<ASR::ArrayBound_t>(*v)) {
                ASR::ArrayBound_t *bd = ASR::down_cast<ASR::ArrayBound_t>(v);
                ASR::expr_t *host = host_designator(loc, x, kernel, bd->m_v);
                ASR::expr_t *dim = host
                    ? host_extent(loc, x, kernel, bd->m_dim) : nullptr;
                if (host != nullptr && dim != nullptr) {
                    return ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc,
                        host, dim, int32, bd->m_bound, nullptr));
                }
            }
            if (ASR::is_a<ASR::ArrayItem_t>(*v)) {
                ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(v);
                ASR::expr_t *base = ASRUtils::get_past_array_physical_cast(
                    item->m_v);
                if (!ASR::is_a<ASR::Var_t>(*base)) return nullptr;
                std::string name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(base)->m_v);
                for (size_t i = 0; i < arg_names.size(); i++) {
                    if (arg_names[i] != name) continue;
                    if (i >= x.n_args || !x.m_args[i].m_value) break;
                    std::vector<ASR::expr_t*> subs;
                    for (size_t k = 0; k < item->n_args; k++) {
                        ASR::expr_t *sub = host_extent(loc, x, kernel,
                            item->m_args[k].m_right);
                        if (sub == nullptr) return nullptr;
                        subs.push_back(sub);
                    }
                    return b.ArrayItem_01(x.m_args[i].m_value, subs);
                }
                return nullptr;
            }
            if (ASR::is_a<ASR::Var_t>(*v)) {
                std::string name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(v)->m_v);
                for (size_t i = 0; i < arg_names.size(); i++) {
                    if (arg_names[i] != name) continue;
                    if (i >= x.n_args) break;
                    return x.m_args[i].m_value;
                }
            }
            return nullptr;
        }

        ASR::expr_t* struct_member(const Location &loc, ASR::expr_t *arg,
                ASR::expr_t *index, ASR::symbol_t *member) {
            ASRUtils::ASRBuilder b(al, loc);
            return ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, loc,
                b.ArrayItem_01(arg, {index}), member,
                ASRUtils::symbol_type(member), nullptr));
        }

        ASR::expr_t* member_data_address(const Location &loc,
                ASR::expr_t *data, ASR::expr_t *offsets, ASR::expr_t *index,
                ASR::expr_t *element_bytes) {
            ASRUtils::ASRBuilder b(al, loc);
            return address_of(loc, b.ArrayItem_01(data, {b.Add(
                b.Mul(b.i2i_t(b.ArrayItem_01(offsets, {index}), int64),
                    element_bytes), b.i64(1))}));
        }

        ASR::expr_t* member_byte_size(const Location &loc, ASR::expr_t *sizes,
                ASR::expr_t *index, ASR::expr_t *element_bytes) {
            ASRUtils::ASRBuilder b(al, loc);
            return b.Mul(b.i2i_t(b.ArrayItem_01(sizes, {index}), int64),
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
            ASRUtils::ASRBuilder b(al, loc);
            ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(struct_sym);
            ASR::ttype_t *plain_type = ASRUtils::make_StructType_t_util(al,
                loc, struct_sym, true);
            ASR::expr_t *tmp = declare_local(loc, "gpu_plain_arg",
                plain_type, struct_sym);
            std::vector<ASR::stmt_t*> back;
            for (size_t m = 0; m < st->n_members; m++) {
                ASR::symbol_t *member = st->m_symtab->get_symbol(
                    st->m_members[m]);
                if (member == nullptr
                        || !ASR::is_a<ASR::Variable_t>(*member)) {
                    return arg;
                }
                ASR::ttype_t *mt = ASRUtils::symbol_type(member);
                if (ASRUtils::is_allocatable_or_pointer(mt)) return arg;
                ASR::expr_t *from = ASRUtils::EXPR(
                    ASR::make_StructInstanceMember_t(al, loc, arg, member,
                        mt, nullptr));
                ASR::expr_t *to = ASRUtils::EXPR(
                    ASR::make_StructInstanceMember_t(al, loc, tmp, member,
                        mt, nullptr));
                out.push_back(al, b.Assignment(to, from));
                back.push_back(b.Assignment(from, to));
            }
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

        void expand_launch(const ASR::GpuKernelLaunch_t &x,
                Vec<ASR::stmt_t*> &out) {
            const Location &loc = x.base.base.loc;
            ASRUtils::ASRBuilder b(al, loc);

            ASR::Function_t *kernel =
                ASR::down_cast<ASR::Function_t>(x.m_kernel);
            std::string kernel_name(kernel->m_name);

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

            for (size_t i = 0; i < x.n_args; i++) {
                ASR::expr_t *arg = x.m_args[i].m_value;
                ASR::ttype_t *arg_type = ASRUtils::expr_type(arg);
                ASR::Variable_t *kparam = ASR::down_cast<ASR::Variable_t>(
                    ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(kernel->m_args[i])->m_v));
                if (ASRUtils::is_array(arg_type) ||
                        ASR::is_a<ASR::StructType_t>(
                            *ASRUtils::extract_type(arg_type))) {
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
                    if (ASRUtils::is_array(arg_type) &&
                            ASR::is_a<ASR::Var_t>(*arg)) {
                        decompose_struct_members(loc, out, arg, buffers,
                            writebacks, *kernel);
                    }
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
                    ASR::expr_t *extent = nullptr;
                    if (dim.is_constant) {
                        extent = b.i64(dim.constant_value);
                    } else if (dim.is_struct_member_size) {
                        auto first = member_first_sizes.find(
                            dim.struct_member_key);
                        if (first == member_first_sizes.end()) continue;
                        extent = b.i2i_t(first->second, int64);
                    } else if (dim.is_host_expr) {
                        ASR::expr_t *host = host_extent(loc, x, kernel,
                            dim.dim_expr);
                        if (host == nullptr) continue;
                        extent = b.i2i_t(host, int64);
                    } else if (!dim.member_path.empty()) {
                        // A scalar component of a struct argument. The
                        // struct reaches the kernel as a buffer, so the
                        // host reads the component here instead.
                        ASR::expr_t *e =
                            x.m_args[dim.call_arg_index].m_value;
                        bool ok = true;
                        for (const std::string &m : dim.member_path) {
                            ASR::symbol_t *st =
                                ASRUtils::get_struct_sym_from_struct_expr(e);
                            ASR::symbol_t *member = st
                                ? ASR::down_cast<ASR::Struct_t>(
                                    ASRUtils::symbol_get_past_external(st))
                                        ->m_symtab->get_symbol(m)
                                : nullptr;
                            if (member == nullptr) { ok = false; break; }
                            e = ASRUtils::EXPR(
                                ASR::make_StructInstanceMember_t(al, loc, e,
                                    member, ASRUtils::symbol_type(member),
                                    nullptr));
                        }
                        if (!ok) continue;
                        extent = b.i2i_t(e, int64);
                    } else {
                        extent = b.i2i_t(
                            x.m_args[dim.call_arg_index].m_value, int64);
                    }
                    n_elements = b.Mul(n_elements, extent);
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
            for (ASR::stmt_t *writeback : writebacks) {
                out.push_back(al, writeback);
            }
            for (ASR::expr_t *workspace : workspaces) {
                out.push_back(al, b.Deallocate(workspace));
            }
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
    DeviceLaunchExpandVisitor v(al, unit);
    v.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers

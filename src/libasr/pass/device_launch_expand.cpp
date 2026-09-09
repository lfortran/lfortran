#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/assert.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/device_launch_expand.h>
#include <libasr/pass/gpu_decline.h>
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

`gpu_offload` asks this pass, through gpu_launch_is_supported(), whether it
can lay out every argument of a launch the same way as the device code
generator, and keeps the loop on the host when it cannot, rather than
building a launch that would read the wrong bytes.
*/
// Why the last rejected launch could not be expanded, for the diagnostic.
static GpuDecline unsupported_decline;
static bool unsupported(const GpuDecline &why) {
    unsupported_decline = why;
    return false;
}

// A number or a logical: what the device languages have a scalar type for at
// all, before their widths are considered.
static bool is_numeric_scalar(ASR::ttype_t *type) {
    return ASR::is_a<ASR::Integer_t>(*type) || ASR::is_a<ASR::Real_t>(*type)
        || ASR::is_a<ASR::Logical_t>(*type);
}

// ... and of a width the device has a type of its own for.  A width the
// device cannot match is not a layout the launch can hand over: the buffer is
// sized from the host element type, so the kernel would stride through it at
// the wrong size and quietly compute on the wrong elements.
static bool is_plain_scalar(ASR::ttype_t *type) {
    return is_numeric_scalar(type) && gpu_scalar_width_supported(type);
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
// allocatable arrays are laid out that way; anything else (character or
// pointer members) has no device layout at all, so a loop that needs it
// stays on the host.
//
// An extended type is laid out with the type it extends as its first field,
// on the device as on the host, so the type it extends has to be laid out
// that way too.
static bool struct_is_plain(ASR::symbol_t *struct_sym) {
    ASR::Struct_t *st = get_struct(struct_sym);
    if (!st) {
        return unsupported(
            GpuDecline(GpuDeclineReason::StructDeclarationUnknown));
    }
    if (st->m_parent && !struct_is_plain(st->m_parent)) return false;
    for (size_t i = 0; i < st->n_members; i++) {
        ASR::symbol_t *member = st->m_symtab->get_symbol(st->m_members[i]);
        if (!member || !ASR::is_a<ASR::Variable_t>(*member)) {
            return unsupported(
                GpuDecline(GpuDeclineReason::StructNonDataMember));
        }
        if (is_decomposed_member(member)) continue;
        ASR::ttype_t *type = ASR::down_cast<ASR::Variable_t>(member)->m_type;
        if (ASRUtils::is_pointer(type)) {
            return unsupported(
                GpuDecline(GpuDeclineReason::StructPointerMember));
        }
        if (ASRUtils::is_allocatable(type)) {
            if (ASRUtils::is_array(type)) {
                return unsupported(GpuDecline(
                    GpuDeclineReason::StructAllocatableArrayMember));
            }
            return unsupported(GpuDecline(
                GpuDeclineReason::StructAllocatableScalarMember));
        }
        if (ASRUtils::is_array(type) &&
                ASRUtils::get_fixed_size_of_array(type) <= 0) {
            return unsupported(GpuDecline(
                GpuDeclineReason::StructAssumedShapeArrayMember));
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
            if (is_numeric_scalar(base)) {
                return unsupported(GpuDecline(
                    GpuDeclineReason::StructMemberTypeWidth, "", base));
            }
            return unsupported(
                GpuDecline(GpuDeclineReason::StructMemberNotNumeric));
        }
    }
    return true;
}

// Every data member of `st`, the ones it inherits first, in layout order.
static void collect_data_members(ASR::Struct_t *st,
        std::vector<ASR::symbol_t*> &members) {
    if (!st) return;
    if (st->m_parent) collect_data_members(get_struct(st->m_parent), members);
    for (size_t i = 0; i < st->n_members; i++) {
        members.push_back(st->m_symtab->get_symbol(st->m_members[i]));
    }
}

// True when a value of this type carries an allocatable or a pointer
// component at any depth. Such a value cannot be copied by a single
// assignment here: the launch is expanded after the passes that turn an
// intrinsic assignment into a deep copy have run, so what reaches the
// backend is a block copy of the descriptors, and the copy would then own
// the original's storage and free it twice. It is copied part by part
// instead, leaving those components alone.
static bool struct_has_allocatable_parts(ASR::symbol_t *struct_sym) {
    ASR::Struct_t *st = get_struct(struct_sym);
    if (!st) return true;
    if (st->m_parent && struct_has_allocatable_parts(st->m_parent)) {
        return true;
    }
    for (size_t i = 0; i < st->n_members; i++) {
        ASR::symbol_t *member = st->m_symtab->get_symbol(st->m_members[i]);
        if (!member || !ASR::is_a<ASR::Variable_t>(*member)) return true;
        ASR::ttype_t *type = ASRUtils::symbol_type(member);
        if (ASRUtils::is_allocatable_or_pointer(type)) return true;
        ASR::ttype_t *base = ASRUtils::type_get_past_array(type);
        if (ASR::is_a<ASR::StructType_t>(*base) &&
                struct_has_allocatable_parts(
                    ASR::down_cast<ASR::Variable_t>(member)
                        ->m_type_declaration)) {
            return true;
        }
    }
    return false;
}

// A polymorphic argument reaches the device as the class container it is
// represented by, so the launch hands the kernel a plain copy of the declared
// type's own components instead of the container. That copy is only possible
// when every component either is copied by an assignment or is one the device
// never reads through the struct at all: an allocatable array component is
// handed over as its own flat buffers, so it is skipped, but any other
// allocatable or pointer component has no copy, and the loop stays on the
// host rather than handing the kernel a container it would read as the
// declared type.
//
// This walks the components in the same order, and asks the same questions
// of each one, as the copy that copy_plain_parts() builds once the launch is
// accepted. The two must agree: a launch accepted here whose copy cannot
// then be built would leave the kernel reading a class container as the
// declared type, which is the type descriptor read as data.
static bool class_argument_can_be_copied(ASR::symbol_t *struct_sym);

// The same question for one component, which may be an array of a derived
// type, each element of which is copied on its own.
static bool class_component_can_be_copied(ASR::ttype_t *type,
        ASR::symbol_t *decl) {
    if (!ASRUtils::is_array(type)) {
        return class_argument_can_be_copied(decl);
    }
    ASR::dimension_t *dims = nullptr;
    int rank = ASRUtils::extract_dimensions_from_ttype(type, dims);
    if (rank <= 0) {
        return unsupported(
            GpuDecline(GpuDeclineReason::ClassComponentArrayRank));
    }
    for (int d = 0; d < rank; d++) {
        if (dims[d].m_start == nullptr || dims[d].m_length == nullptr) {
            return unsupported(
                GpuDecline(GpuDeclineReason::ClassComponentArrayExtents));
        }
    }
    return class_argument_can_be_copied(decl);
}

static bool class_argument_can_be_copied(ASR::symbol_t *struct_sym) {
    ASR::Struct_t *st = get_struct(struct_sym);
    if (!st) {
        return unsupported(
            GpuDecline(GpuDeclineReason::ClassDeclarationUnknown));
    }
    std::vector<ASR::symbol_t*> members;
    collect_data_members(st, members);
    for (ASR::symbol_t *member : members) {
        if (!member || !ASR::is_a<ASR::Variable_t>(*member)) {
            return unsupported(
                GpuDecline(GpuDeclineReason::ClassNonDataComponent));
        }
        ASR::ttype_t *member_type = ASRUtils::symbol_type(member);
        if (ASRUtils::is_allocatable_or_pointer(member_type)) {
            if (is_decomposed_member(member)) continue;
            return unsupported(
                GpuDecline(GpuDeclineReason::ClassAllocatableComponent));
        }
        ASR::symbol_t *decl = ASR::down_cast<ASR::Variable_t>(
            member)->m_type_declaration;
        if (ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::type_get_past_array(member_type))
                && struct_has_allocatable_parts(decl)
                && !class_component_can_be_copied(member_type, decl)) {
            return false;
        }
    }
    return true;
}

static ASR::ttype_t* struct_layout_type(Allocator &al,
    ASR::symbol_t *struct_sym);

// The type one member occupies inside its struct. A member that is itself a
// derived type is laid out by that type's own layout, which is not what its
// StructType signature says when the type extends another one.
static ASR::ttype_t* member_layout_type(Allocator &al,
        ASR::Variable_t *member) {
    ASR::ttype_t *type = member->m_type;
    ASR::ttype_t *element = ASRUtils::type_get_past_array(type);
    if (!ASR::is_a<ASR::StructType_t>(*element)) return type;
    ASR::ttype_t *layout = struct_layout_type(al, member->m_type_declaration);
    if (!layout) return type;
    if (!ASR::is_a<ASR::Array_t>(*type)) return layout;
    ASR::Array_t *array = ASR::down_cast<ASR::Array_t>(type);
    return ASRUtils::TYPE(ASR::make_Array_t(al, type->base.loc, layout,
        array->m_dims, array->n_dims, array->m_physical_type,
        array->m_memory_space));
}

// The anonymous struct a value of `struct_sym` is laid out as: the type it
// extends first, then its own members. Both the host and the device put the
// inherited part of an extended type in front of the type's own, but a
// StructType signature lists only the members the type declares itself, so a
// launch that sized an extended type from its signature would copy only the
// tail of it. Returns nullptr when the type cannot be inspected.
static ASR::ttype_t* struct_layout_type(Allocator &al,
        ASR::symbol_t *struct_sym) {
    ASR::Struct_t *st = get_struct(struct_sym);
    if (!st) return nullptr;
    Vec<ASR::ttype_t*> members;
    members.reserve(al, st->n_members + 1);
    if (st->m_parent) {
        ASR::ttype_t *parent = struct_layout_type(al, st->m_parent);
        if (!parent) return nullptr;
        members.push_back(al, parent);
    }
    for (size_t i = 0; i < st->n_members; i++) {
        ASR::symbol_t *member = st->m_symtab->get_symbol(st->m_members[i]);
        if (!member || !ASR::is_a<ASR::Variable_t>(*member)) return nullptr;
        members.push_back(al, member_layout_type(al,
            ASR::down_cast<ASR::Variable_t>(member)));
    }
    return ASRUtils::TYPE(ASR::make_StructType_t(al, st->base.base.loc,
        members.p, members.n, nullptr, 0, true, false));
}

// The type to hand SizeOfType for a value of `type`, which is `type` itself
// unless it is a derived type whose layout its signature does not describe.
static ASR::ttype_t* size_of_type_arg(Allocator &al, ASR::expr_t *value,
        ASR::ttype_t *type) {
    ASR::ttype_t *element = ASRUtils::type_get_past_array(type);
    if (!ASR::is_a<ASR::StructType_t>(*element)) return type;
    ASR::ttype_t *layout = struct_layout_type(al,
        ASRUtils::get_struct_sym_from_struct_expr(value));
    if (!layout) return type;
    if (!ASR::is_a<ASR::Array_t>(*type)) return layout;
    ASR::Array_t *array = ASR::down_cast<ASR::Array_t>(type);
    return ASRUtils::TYPE(ASR::make_Array_t(al, type->base.loc, layout,
        array->m_dims, array->n_dims, array->m_physical_type,
        array->m_memory_space));
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
        if (ASRUtils::is_class_type(base)) {
            // The kernel is generated against the declared type, so the
            // launch has to hand over the declared type's own data rather
            // than the class container holding it.
            if (ASRUtils::is_unlimited_polymorphic_type(arg_type)) {
                return unsupported(GpuDecline(
                    GpuDeclineReason::UnlimitedPolymorphicArgument));
            }
            if (ASRUtils::is_array(arg_type)) {
                return unsupported(GpuDecline(
                    GpuDeclineReason::PolymorphicArrayArgument));
            }
            if (!class_argument_can_be_copied(struct_sym)) return false;
        }
        // An array of a derived type of any rank is handed over by the
        // column-major position of its elements, which is the order both
        // the host writes the flattened component buffers in and the
        // device reads them back in, so the rank itself is no obstacle.
        return true;
    }
    if (is_plain_scalar(base)) return true;
    if (is_numeric_scalar(base)) {
        return unsupported(GpuDecline(
            GpuDeclineReason::ArrayElementTypeWidth, "", base));
    }
    return unsupported(
        GpuDecline(GpuDeclineReason::ArrayElementNotNumeric));
}

static bool is_supported_scalar(ASR::ttype_t *type) {
    return is_plain_scalar(ASRUtils::extract_type(type));
}

static bool same_scalar_type(ASR::ttype_t *a, ASR::ttype_t *b) {
    ASR::ttype_t *ta = ASRUtils::extract_type(a);
    ASR::ttype_t *tb = ASRUtils::extract_type(b);
    return ta->type == tb->type &&
        ASRUtils::extract_kind_from_ttype_t(ta) ==
            ASRUtils::extract_kind_from_ttype_t(tb);
}

// True when the host can turn this workspace dimension into an extent
// expression at expand time. A dimension that cannot is not "already
// fine": skipping it would size the buffer short while the device still
// multiplies the extent in.
static bool workspace_dim_can_expand(const GpuVlaDim &dim,
        const ASR::Function_t *kernel) {
    if (dim.is_constant) return true;
    if (dim.is_struct_member_size) {
        if (dim.struct_member_key.empty()) return false;
        if (dim.struct_member_elem_index < 0) {
            return unsupported(GpuDecline(
                GpuDeclineReason::WorkspaceStructElementShape));
        }
        std::string::size_type dot = dim.struct_member_key.find('.');
        if (dot == std::string::npos) return false;
        std::string arr = dim.struct_member_key.substr(0, dot);
        std::string mem = dim.struct_member_key.substr(dot + 1);
        for (size_t i = 0; i < kernel->n_args; i++) {
            ASR::Variable_t *kparam = ASR::down_cast<ASR::Variable_t>(
                ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(kernel->m_args[i])->m_v));
            if (std::string(kparam->m_name) != arr) continue;
            if (!ASRUtils::is_array(kparam->m_type)) return false;
            ASR::Struct_t *st = get_struct(kparam->m_type_declaration);
            if (!st) return false;
            for (auto &m : ASRUtils::collect_allocatable_array_members(st)) {
                if (m.first == mem && is_decomposed_member(
                        &m.second->base)) {
                    return true;
                }
            }
            return false;
        }
        return false;
    }
    return dim.derived.ok();
}

// Defined after DeviceLaunchExpandVisitor so it can rebuild a host-evaluable
// workspace extent the same way expand does.
static bool launch_is_supported(Allocator &al, ASR::symbol_t *kernel_sym,
        ASR::call_arg_t *call_args, size_t n_call_args);

// The device lays a placeholder out in the field of an allocatable
// component and reads the component's data from a flat buffer of its own,
// named after the argument the component hangs off. Only a component of an
// argument itself, or of one of its elements, has such a buffer, so a
// component reached through another component would be read as the
// placeholder. Finds the first such read, so the loop stays on the host
// instead.
class NestedAllocatableReadFinder :
        public ASR::BaseWalkVisitor<NestedAllocatableReadFinder> {
public:
    bool found = false;
    std::string member_name;

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        ASR::symbol_t *member = ASRUtils::symbol_get_past_external(x.m_m);
        if (!found && member && ASR::is_a<ASR::Variable_t>(*member)
                && ASRUtils::is_allocatable_or_pointer(
                    ASRUtils::symbol_type(member))) {
            ASR::expr_t *base = x.m_v;
            while (true) {
                base = ASRUtils::get_past_array_physical_cast(base);
                if (ASR::is_a<ASR::ArrayItem_t>(*base)) {
                    base = ASR::down_cast<ASR::ArrayItem_t>(base)->m_v;
                    continue;
                }
                if (ASR::is_a<ASR::ArraySection_t>(*base)) {
                    base = ASR::down_cast<ASR::ArraySection_t>(base)->m_v;
                    continue;
                }
                break;
            }
            if (ASR::is_a<ASR::StructInstanceMember_t>(*base)) {
                found = true;
                member_name = ASRUtils::symbol_name(member);
            }
        }
        ASR::BaseWalkVisitor<NestedAllocatableReadFinder>
            ::visit_StructInstanceMember(x);
    }
};

// True when every argument of this launch has a shape the pass can expand.
static bool launch_is_supported_args(ASR::symbol_t *kernel_sym,
        ASR::call_arg_t *call_args, size_t n_call_args) {
    ASR::Function_t *kernel = ASR::down_cast<ASR::Function_t>(kernel_sym);
    if (n_call_args != kernel->n_args) {
        return unsupported(
            GpuDecline(GpuDeclineReason::KernelArgumentCountMismatch));
    }
    NestedAllocatableReadFinder nested;
    for (size_t i = 0; i < kernel->n_body; i++) {
        nested.visit_stmt(*kernel->m_body[i]);
    }
    if (nested.found) {
        return unsupported(GpuDecline(
            GpuDeclineReason::NestedAllocatableComponent,
            nested.member_name));
    }
    for (auto &workspace : analyze_gpu_vla_workspaces(*kernel)) {
        for (auto &dim : workspace.dims) {
            if (!workspace_dim_can_expand(dim, kernel)) {
                if (unsupported_decline.declined()) return false;
                return unsupported(GpuDecline(
                    GpuDeclineReason::LaunchVlaExtentNotRebuildable));
            }
        }
    }
    for (size_t i = 0; i < n_call_args; i++) {
        ASR::expr_t *arg = call_args[i].m_value;
        if (!arg) {
            return unsupported(GpuDecline(GpuDeclineReason::MissingArgument));
        }
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
                ASR::ttype_t *t = ASRUtils::extract_type(arg_type);
                if (is_numeric_scalar(t)) {
                    return unsupported(GpuDecline(
                        GpuDeclineReason::ScalarTypeWidth, "", t));
                }
                return unsupported(
                    GpuDecline(GpuDeclineReason::ScalarNotNumeric));
            }
            if (!same_scalar_type(arg_type, kparam->m_type)) {
                return unsupported(
                    GpuDecline(GpuDeclineReason::ScalarKindMismatch));
            }
        }
    }
    return true;
}

bool gpu_launch_is_supported(Allocator &al, ASR::symbol_t *kernel,
        ASR::call_arg_t *args, size_t n_args, GpuDecline &decline) {
    unsupported_decline = GpuDecline();
    if (launch_is_supported(al, kernel, args, n_args)) return true;
    decline = unsupported_decline;
    return false;
}

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
            // `gpu_offload` asked this same question before it committed the
            // loop, but it asked it of a draft kernel: the passes between
            // the two turn function results into temporaries, array
            // expressions into element loops and array extents into extra
            // arguments, so the kernel that reaches here is not the one the
            // decision was made about. Ask again, of the kernel the device
            // code generator will actually see, rather than lay out a launch
            // whose shape was never checked.
            GpuDecline decline;
            if (!gpu_launch_is_supported(al, x.m_kernel, x.m_args, x.n_args,
                    decline)) {
                report_launch_declined(x.base.base.loc, decline);
                remove_original_stmt = true;
                return;
            }
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
        // Scalar argument struct created for each kernel, by kernel name.
        std::map<std::string, ASR::symbol_t*> scalar_arg_structs;
        // Size of the first element of a decomposed struct member, by
        // "<array>.<member>". A member sized at run time from another one,
        // and a workspace sized from a member, both read it.
        std::map<std::string, ASR::expr_t*> member_first_sizes;
        // Sizes buffer of a decomposed member, so a workspace can be
        // counted from the same element the device strides by.
        std::map<std::string, ASR::expr_t*> member_sizes_bufs;

        // A launch this pass cannot lay out, found once the loop it came
        // from is gone. `gpu_offload` answers the same question while the
        // loop is still there and leaves it on the host; there is nothing
        // left to leave it on here, so this is an error whether or not the
        // CPU fallback was asked for. The wording is the one every decline
        // is phrased in, so that the two stages cannot describe the same
        // limitation differently.
        void report_launch_declined(const Location &where,
                const GpuDecline &decline) {
            if (pass_options.diagnostics == nullptr) return;
            std::string why = gpu_decline_message(decline);
            if (pass_options.gpu_decline_stats) {
                std::cerr << "gpu-decline: " << gpu_decline_class_name(
                    gpu_decline_class(decline,
                        gpu_device_capabilities(pass_options)))
                    << ": " << why << std::endl;
            }
            pass_options.diagnostics->message_label(
                "this parallel loop was offloaded to the gpu, but its "
                "launch cannot be laid out: " + why,
                {where}, why, diag::Level::Error, diag::Stage::ASRPass);
        }

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
                    size_of_type_arg(al, arg, type), int64, nullptr));
            }
            ASR::ttype_t *element = size_of_type_arg(al, arg,
                ASRUtils::type_get_past_array(type));
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
                const std::string &arg_name,
                std::vector<BufferArg> &buffers,
                std::vector<ASR::stmt_t*> &writebacks,
                const ASR::Function_t &kernel) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::Struct_t *st = get_struct(
                ASRUtils::get_struct_sym_from_struct_expr(arg));
            if (!st) return;
            std::map<std::string, int64_t> write_sizes =
                find_struct_member_vla_write_sizes(kernel,
                    analyze_gpu_vla_workspaces(kernel));
            std::map<std::string, std::string> runtime_sources =
                find_struct_member_vla_runtime_sources(kernel);
            // A member inherited from a type this one extends is stored
            // and handed over exactly like one of its own.
            for (auto &member_entry :
                    ASRUtils::collect_allocatable_array_members(st)) {
                const std::string &member_name = member_entry.first;
                ASR::symbol_t *member = &member_entry.second->base;
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
                std::string key = arg_name + "." + member_name;
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

                size_t rank = gpu_struct_member_rank(
                    ASR::down_cast<ASR::Variable_t>(member));
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
                for (size_t d = 0; d < rank; d++) {
                    measure.push_back(b.Assignment(
                        member_extent(loc, sizes, k, rank, d),
                        b.ArraySize(struct_member(loc, arg, k, member),
                            rank > 1 ? b.i32((int)d + 1) : nullptr,
                            int32)));
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
                        memcpy_call(loc,
                            member_data_address(loc, data, offsets, k,
                                element_bytes),
                            address_of(loc,
                                struct_member(loc, arg, k, member)),
                            member_byte_size(loc, sizes, k, rank,
                                element_bytes))}));
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
                        memcpy_call(loc,
                            address_of(loc,
                                struct_member(loc, arg, k, member)),
                            member_data_address(loc, data, offsets, k,
                                element_bytes),
                            member_byte_size(loc, sizes, k, rank,
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

    public:
        // A designator the kernel writes in terms of its parameters, built
        // again over the actual arguments of this launch so the host can
        // read the same object: `self%points_(1,1,1,1)%values_` names one
        // array whichever side asks for it.
        static ASR::expr_t* host_designator(Allocator &al, const Location &loc,
                const ASR::Function_t *kernel, ASR::call_arg_t *args,
                size_t n_args, ASR::expr_t *e) {
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
                    if (i >= n_args) break;
                    return args[i].m_value;
                }
                ASR::expr_t *bound = gpu_local_array_binding(
                    ASR::down_cast<ASR::Var_t>(v)->m_v, kernel->m_body,
                    kernel->n_body);
                if (bound != nullptr) {
                    return host_designator(al, loc, kernel, args, n_args,
                        bound);
                }
                return nullptr;
            }
            if (ASR::is_a<ASR::StructInstanceMember_t>(*v)) {
                ASR::StructInstanceMember_t *sm =
                    ASR::down_cast<ASR::StructInstanceMember_t>(v);
                ASR::expr_t *base = host_designator(al, loc, kernel, args,
                    n_args, sm->m_v);
                if (base == nullptr) return nullptr;
                ASR::symbol_t *st =
                    ASRUtils::get_struct_sym_from_struct_expr(base);
                ASR::symbol_t *member = gpu_struct_lookup_member(st,
                    ASRUtils::symbol_name(
                        ASRUtils::symbol_get_past_external(sm->m_m)));
                if (member == nullptr) return nullptr;
                return ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al,
                    loc, base, member, ASRUtils::symbol_type(member),
                    nullptr));
            }
            if (ASR::is_a<ASR::ArrayItem_t>(*v)) {
                ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(v);
                ASR::expr_t *base = host_designator(al, loc, kernel, args,
                    n_args, item->m_v);
                if (base == nullptr) return nullptr;
                std::vector<ASR::expr_t*> subs;
                GpuExtentScope scope = kernel_scope(kernel);
                for (size_t i = 0; i < item->n_args; i++) {
                    ASR::expr_t *sub = build_host_extent(al, loc, kernel,
                        args, n_args, gpu_derive_extent(
                            item->m_args[i].m_right, scope));
                    if (sub == nullptr) return nullptr;
                    subs.push_back(sub);
                }
                return b.ArrayItem_01(base, subs);
            }
            return nullptr;
        }

        // The scope a kernel's own extent expressions are read against.
        static GpuExtentScope kernel_scope(const ASR::Function_t *kernel) {
            GpuExtentScope scope;
            scope.kernel = kernel;
            for (size_t i = 0; i < kernel->n_args; i++) {
                scope.arg_names.push_back(ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(kernel->m_args[i])->m_v));
            }
            scope.symtab = kernel->m_symtab;
            scope.body = kernel->m_body;
            scope.n_body = kernel->n_body;
            return scope;
        }

        // The one derivation of a workspace extent, built again over the
        // actual arguments of this launch. The kernel writes the extent in
        // terms of its own parameters -- `op%m_ + 1` -- and the host has to
        // compute the same number before it dispatches, so every parameter
        // the derivation names is replaced by the argument bound to it.
        //
        // What the extent *is* was settled once, by gpu_derive_extent();
        // this only writes that answer in the caller's names. Returns
        // nullptr when a part of it has no host counterpart -- which is
        // also how a launch is declined, so an accepted launch is one whose
        // every workspace the host can size.
        static ASR::expr_t* build_host_extent(Allocator &al,
                const Location &loc, const ASR::Function_t *kernel,
                ASR::call_arg_t *args, size_t n_args, const GpuExtent &e) {
            ASRUtils::ASRBuilder b(al, loc);
            auto actual = [&](size_t i) -> ASR::expr_t* {
                if (i >= n_args) return nullptr;
                return args[i].m_value;
            };
            auto child = [&](size_t i) -> ASR::expr_t* {
                if (i >= e.children.size()) return nullptr;
                return build_host_extent(al, loc, kernel, args, n_args,
                    e.children[i]);
            };
            switch (e.kind) {
                case GpuExtentKind::None: {
                    return nullptr;
                }
                case GpuExtentKind::Constant: {
                    // A folded constant carries its own kind; a literal the
                    // derivation introduced is a plain default integer.
                    return e.expr != nullptr ? e.expr
                        : b.i32((int) e.int_value);
                }
                case GpuExtentKind::BinOp: {
                    ASR::expr_t *l = child(0), *r = child(1);
                    if (!l || !r) return nullptr;
                    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, l,
                        e.binop, r, ASRUtils::expr_type(l), nullptr));
                }
                case GpuExtentKind::Neg: {
                    ASR::expr_t *a = child(0);
                    if (!a) return nullptr;
                    return ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al,
                        loc, a, ASRUtils::expr_type(a), nullptr));
                }
                case GpuExtentKind::Compare: {
                    ASR::expr_t *l = child(0), *r = child(1);
                    if (!l || !r) return nullptr;
                    return ASRUtils::EXPR(ASR::make_IntegerCompare_t(al, loc,
                        l, e.cmpop, r, ASRUtils::expr_type(e.expr), nullptr));
                }
                case GpuExtentKind::Select: {
                    ASR::expr_t *t = child(0), *bdy = child(1),
                        *els = child(2);
                    if (!t || !bdy || !els) return nullptr;
                    return ASRUtils::EXPR(ASR::make_IfExp_t(al, loc, t, bdy,
                        els, ASRUtils::expr_type(bdy), nullptr));
                }
                case GpuExtentKind::Product: {
                    ASR::expr_t *out = nullptr;
                    for (size_t i = 0; i < e.children.size(); i++) {
                        ASR::expr_t *one = child(i);
                        if (one == nullptr) return nullptr;
                        out = out ? b.Mul(out, one) : one;
                    }
                    return out;
                }
                case GpuExtentKind::ArgScalar: {
                    return actual(e.arg_index);
                }
                case GpuExtentKind::ArgMember: {
                    // A struct reaches the kernel as a buffer, so the host
                    // reads the component out of the actual instead.
                    ASR::expr_t *out = actual(e.arg_index);
                    if (out == nullptr) return nullptr;
                    for (const std::string &m : e.member_path) {
                        ASR::symbol_t *st =
                            ASRUtils::get_struct_sym_from_struct_expr(out);
                        ASR::symbol_t *member = gpu_struct_lookup_member(st,
                            m);
                        if (member == nullptr) return nullptr;
                        out = ASRUtils::EXPR(
                            ASR::make_StructInstanceMember_t(al, loc, out,
                                member, ASRUtils::symbol_type(member),
                                nullptr));
                    }
                    return out;
                }
                case GpuExtentKind::ArgElement: {
                    ASR::expr_t *base = actual(e.arg_index);
                    if (base == nullptr) return nullptr;
                    std::vector<ASR::expr_t*> subs;
                    for (size_t i = 0; i < e.children.size(); i++) {
                        ASR::expr_t *sub = child(i);
                        if (sub == nullptr) return nullptr;
                        subs.push_back(sub);
                    }
                    return b.ArrayItem_01(base, subs);
                }
                case GpuExtentKind::ArrayDim: {
                    // The actual argument has the shape the parameter does,
                    // so the host asks it for the extent the kernel reads
                    // from its own dimension parameter.
                    ASR::expr_t *base = actual(e.arg_index);
                    if (base == nullptr) return nullptr;
                    return b.ArraySize(base, b.i32((int) e.int_value + 1),
                        int32);
                }
                case GpuExtentKind::Size: {
                    ASR::expr_t *host = host_designator(al, loc, kernel, args,
                        n_args, e.array);
                    if (host == nullptr) return nullptr;
                    ASR::expr_t *dim = nullptr;
                    if (!e.children.empty()) {
                        dim = child(0);
                        if (dim == nullptr) return nullptr;
                    }
                    return b.ArraySize(host, dim, int32);
                }
                case GpuExtentKind::Bound: {
                    ASR::expr_t *host = host_designator(al, loc, kernel, args,
                        n_args, e.array);
                    ASR::expr_t *dim = child(0);
                    if (host == nullptr || dim == nullptr) return nullptr;
                    return ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc,
                        host, dim, int32, e.bound, nullptr));
                }
            }
            return nullptr;
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
            ASR::Struct_t *st = get_struct(struct_sym);
            if (st == nullptr) return false;
            std::vector<ASR::symbol_t*> data_members;
            collect_data_members(st, data_members);
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
                        && struct_has_allocatable_parts(decl)) {
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
                // gpu_launch_is_supported() accepted this launch because
                // class_argument_can_be_copied() walks these same
                // components, so there is a copy for every one of them.
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
                    if (ASRUtils::is_array(arg_type)) {
                        // The buffers the device reads a component through
                        // are named after the kernel's own parameter, so
                        // the actual need not be a plain variable: a
                        // component chain names one array just as well.
                        decompose_struct_members(loc, out, arg,
                            kparam->m_name, buffers, writebacks, *kernel);
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
                    scalar_fields.push_back({GpuNames::dim_size(
                        kparam->m_name, d), int32});
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
                        auto sit = member_sizes_bufs.find(
                            dim.struct_member_key);
                        if (sit != member_sizes_bufs.end()) {
                            size_t rank = dim.struct_member_rank;
                            if (rank == 0) rank = 1;
                            // The element the extent names. An extent
                            // that names the loop variable instead has no
                            // element the host can point at, and the check
                            // this expansion runs first turns such a launch
                            // down, so what reaches here is an index the
                            // device strides by too.
                            extent = b.i2i_t(member_element_count(loc,
                                sit->second,
                                b.i32((int) dim.struct_member_elem_index + 1),
                                rank), int64);
                        }
                    } else {
                        ASR::expr_t *host = build_host_extent(al, loc, kernel,
                            x.m_args, x.n_args, dim.derived);
                        if (host != nullptr) {
                            extent = b.i2i_t(host, int64);
                        }
                    }
                    if (extent == nullptr) {
                        // Nothing is left to fall back on: the loop this
                        // launch came from is gone, so a workspace the host
                        // cannot size has to be reported rather than
                        // guessed at. The check above this expansion turns
                        // down every extent it can rebuild nothing for, so
                        // what reaches here is a workspace sized from a
                        // struct member with no sizes buffer of its own.
                        report_launch_declined(loc, GpuDecline(
                            GpuDeclineReason::LaunchVlaExtentNotRebuildable,
                            workspace.var_name));
                        return false;
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

static bool launch_is_supported(Allocator &al, ASR::symbol_t *kernel_sym,
        ASR::call_arg_t *call_args, size_t n_call_args) {
    if (!launch_is_supported_args(kernel_sym, call_args, n_call_args)) {
        return false;
    }
    ASR::Function_t *kernel = ASR::down_cast<ASR::Function_t>(kernel_sym);
    const Location &loc = kernel->base.base.loc;
    for (auto &workspace : analyze_gpu_vla_workspaces(*kernel)) {
        for (auto &dim : workspace.dims) {
            if (dim.is_constant) continue;
            if (dim.is_struct_member_size) continue;
            if (DeviceLaunchExpandVisitor::build_host_extent(al, loc,
                    kernel, call_args, n_call_args, dim.derived) == nullptr) {
                return unsupported(GpuDecline(
                    GpuDeclineReason::LaunchVlaExtentNotRebuildable));
            }
        }
    }
    return true;
}

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

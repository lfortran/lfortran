#include <set>

#include <libasr/asr_builder.h>
#include <libasr/pass/gpu_data_layout.h>
#include <libasr/pass/gpu_kernel_abi.h>
#include <libasr/pass/gpu_offload_preflight.h>

namespace LCompilers {

namespace {

class GpuLaunchSupport {
    GpuDecline &reason;
    bool unsupported(const GpuDecline &failure) {
        reason = failure;
        return false;
    }
public:
    explicit GpuLaunchSupport(GpuDecline &reason) : reason(reason) {
        reason = GpuDecline();
    }
    bool is_numeric_scalar(ASR::ttype_t *type) {
        return ASR::is_a<ASR::Integer_t>(*type) || ASR::is_a<ASR::Real_t>(*type)
            || ASR::is_a<ASR::Logical_t>(*type);
    }

    bool is_plain_scalar(ASR::ttype_t *type) {
        return is_numeric_scalar(type) && gpu_scalar_width_supported(type);
    }

    // Whether the layout hands this component over as a device buffer of
    // its own and the device can work with what is in it. Which components
    // are decomposed at all is not decided here -- it is one answer, in
    // gpu_component_is_decomposed -- so that the components this accepts
    // and the components the layout then goes on to describe are the same
    // ones.
    bool is_decomposed_member(ASR::symbol_t *member) {
        if (!gpu_component_is_decomposed(member)) return false;
        GpuComponentLayout component = gpu_component_layout(member);
        if (ASR::is_a<ASR::StructType_t>(*component.element_type)) {
            return struct_is_plain(ASR::down_cast<ASR::Variable_t>(member)
                ->m_type_declaration);
        }
        return is_plain_scalar(component.element_type);
    }

    bool struct_is_plain(ASR::symbol_t *struct_sym) {
        ASR::Struct_t *st = gpu_struct_definition(struct_sym);
        if (!st) {
            return unsupported(
                GpuDecline(GpuDeclineReason::StructDeclarationUnknown));
        }
        // A member of a real kind wider than the floor every device shares
        // keeps the type off every device, whichever member the walk below
        // would turn down first, so it is looked for before them.
        {
            GpuDeviceCapabilities shared_floor;
            std::set<ASR::Struct_t*> visited;
            ASR::ttype_t *member_type = nullptr;
            if (!gpu_struct_members_ok(struct_sym, visited, shared_floor,
                        &member_type)
                    && shared_floor.lacks_real_width(member_type)) {
                return unsupported(GpuDecline(
                    GpuDeclineReason::StructMemberTypeWidth, "",
                    member_type));
            }
        }
        if (st->m_parent && !struct_is_plain(st->m_parent)) return false;
        for (size_t i = 0; i < st->n_members; i++) {
            ASR::symbol_t *member = st->m_symtab->get_symbol(st->m_members[i]);
            if (!member || !ASR::is_a<ASR::Variable_t>(*member)) {
                return unsupported(
                    GpuDecline(GpuDeclineReason::StructNonDataMember));
            }
            if (is_decomposed_member(member)) continue;
            if (gpu_component_is_opaque(member)) continue;
            ASR::ttype_t *type = ASR::down_cast<ASR::Variable_t>(member)->m_type;
            if (ASRUtils::is_pointer(type)) {
                return unsupported(
                    GpuDecline(GpuDeclineReason::StructPointerMember));
            }
            if (ASRUtils::is_allocatable(type)) {
                return unsupported(GpuDecline(
                    GpuDeclineReason::StructAllocatableArrayMember));
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

    bool class_component_can_be_copied(ASR::ttype_t *type,
            ASR::symbol_t *decl) {
        if (!ASRUtils::is_array(type)) {
            return class_argument_can_be_copied(decl);
        }
        ASR::dimension_t *dims = nullptr;
        int rank = checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(type, dims));
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

    bool class_argument_can_be_copied(ASR::symbol_t *struct_sym) {
        ASR::Struct_t *st = gpu_struct_definition(struct_sym);
        if (!st) {
            return unsupported(
                GpuDecline(GpuDeclineReason::ClassDeclarationUnknown));
        }
        std::vector<ASR::symbol_t*> members;
        gpu_collect_data_members(st, members);
        for (ASR::symbol_t *member : members) {
            if (!member || !ASR::is_a<ASR::Variable_t>(*member)) {
                return unsupported(
                    GpuDecline(GpuDeclineReason::ClassNonDataComponent));
            }
            ASR::ttype_t *member_type = ASRUtils::symbol_type(member);
            if (ASRUtils::is_allocatable_or_pointer(member_type)) {
                if (is_decomposed_member(member)) continue;
                if (gpu_component_is_opaque(member)) continue;
                return unsupported(
                    GpuDecline(GpuDeclineReason::ClassAllocatableComponent));
            }
            ASR::symbol_t *decl = ASR::down_cast<ASR::Variable_t>(
                member)->m_type_declaration;
            if (ASR::is_a<ASR::StructType_t>(
                        *ASRUtils::type_get_past_array(member_type))
                    && gpu_struct_has_allocatable_parts(decl)
                    && !class_component_can_be_copied(member_type, decl)) {
                return false;
            }
        }
        return true;
    }

    bool is_supported_buffer(ASR::expr_t *arg) {
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

    bool is_supported_scalar(ASR::ttype_t *type) {
        return is_plain_scalar(ASRUtils::extract_type(type));
    }

    bool same_scalar_type(ASR::ttype_t *a, ASR::ttype_t *b) {
        ASR::ttype_t *ta = ASRUtils::extract_type(a);
        ASR::ttype_t *tb = ASRUtils::extract_type(b);
        return ta->type == tb->type &&
            ASRUtils::extract_kind_from_ttype_t(ta) ==
                ASRUtils::extract_kind_from_ttype_t(tb);
    }

    // Whether the device can read the extent of a workspace dimension.
    // Whether the host can size it is settled by the layout, which writes
    // out what the host evaluates.
    bool workspace_dim_can_expand(const GpuVlaDim &dim,
            const ASR::Function_t *kernel) {
        if (dim.is_constant) return true;
        if (dim.is_struct_member_size) {
            if (dim.struct_member_key.empty()) return false;
            const std::string &arr = dim.struct_member_key.base;
            const std::string &mem = dim.struct_member_key.member;
            for (size_t i = 0; i < kernel->n_args; i++) {
                ASR::Variable_t *kparam = ASR::down_cast<ASR::Variable_t>(
                    ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(kernel->m_args[i])->m_v));
                if (std::string(kparam->m_name) != arr) continue;
                if (!ASRUtils::is_array(kparam->m_type)) return false;
                ASR::Struct_t *st = gpu_struct_definition(kparam->m_type_declaration);
                if (!st) return false;
                for (auto &m : gpu_decomposed_components(st)) {
                    if (m.name() == mem && is_decomposed_member(m.component)) {
                        return true;
                    }
                }
                return false;
            }
            return false;
        }
        return true;
    }

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

    // The first allocatable scalar component of a derived type, at any
    // depth its value holds inline: its own components, those it inherits,
    // and those of a component of a derived type that is not itself
    // allocatable.
    static ASR::symbol_t* opaque_component(ASR::symbol_t *struct_sym,
            std::set<ASR::Struct_t*> &visited) {
        ASR::Struct_t *st = gpu_struct_definition(struct_sym);
        if (!st || !visited.insert(st).second) return nullptr;
        if (st->m_parent) {
            if (ASR::symbol_t *found = opaque_component(st->m_parent,
                    visited)) {
                return found;
            }
        }
        for (size_t i = 0; i < st->n_members; i++) {
            ASR::symbol_t *member = st->m_symtab->get_symbol(st->m_members[i]);
            if (gpu_component_is_opaque(member)) return member;
            if (!member || !ASR::is_a<ASR::Variable_t>(*member)) continue;
            ASR::Variable_t *variable = ASR::down_cast<ASR::Variable_t>(member);
            if (ASRUtils::is_allocatable_or_pointer(variable->m_type)) continue;
            if (!ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::type_get_past_array(variable->m_type))) {
                continue;
            }
            if (ASR::symbol_t *found = opaque_component(
                    variable->m_type_declaration, visited)) {
                return found;
            }
        }
        return nullptr;
    }

    // The device carries an allocatable scalar component as the host's
    // bytes, not as a value it can read (see `gpu_component_is_opaque`).
    // A kernel that names the component, or that stores a whole value of a
    // type holding one -- which would need the component allocated and
    // copied, not its host address duplicated -- is turned down here
    // rather than compiled into code that reads an address as data.
    class OpaqueComponentUseFinder : public
            ASRUtils::BlockBodyWalkVisitor<OpaqueComponentUseFinder> {
    public:
        GpuDecline found;

        void visit_body(ASR::stmt_t **body, size_t n_body) {
            for (size_t i = 0; i < n_body && !found.declined(); i++) {
                visit_stmt(*body[i]);
            }
        }

        void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
            ASR::symbol_t *member = ASRUtils::symbol_get_past_external(x.m_m);
            if (!found.declined() && gpu_component_is_opaque(member)) {
                found = GpuDecline(
                    GpuDeclineReason::AllocatableScalarComponentUse,
                    ASRUtils::symbol_name(member));
                return;
            }
            ASRUtils::BlockBodyWalkVisitor<OpaqueComponentUseFinder>
                ::visit_StructInstanceMember(x);
        }

        void visit_Assignment(const ASR::Assignment_t &x) {
            if (!found.declined() && ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(ASRUtils::expr_type(
                        x.m_target)))) {
                std::set<ASR::Struct_t*> visited;
                ASR::symbol_t *component = opaque_component(
                    ASRUtils::get_struct_sym_from_struct_expr(x.m_target),
                    visited);
                if (component) {
                    found = GpuDecline(
                        GpuDeclineReason::AllocatableScalarComponentCopy,
                        ASRUtils::symbol_name(component));
                    return;
                }
            }
            ASRUtils::BlockBodyWalkVisitor<OpaqueComponentUseFinder>
                ::visit_Assignment(x);
        }
    };

    bool supported(ASR::symbol_t *kernel_sym,
            ASR::call_arg_t *call_args, size_t n_call_args,
            const std::vector<GpuVlaWorkspace> &workspaces,
            const Vec<ASR::symbol_t*> &device_functions) {
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
        OpaqueComponentUseFinder opaque;
        opaque.visit_body(kernel->m_body, kernel->n_body);
        for (size_t i = 0; i < device_functions.n; i++) {
            ASR::Function_t *function =
                ASR::down_cast<ASR::Function_t>(device_functions[i]);
            opaque.visit_body(function->m_body, function->n_body);
        }
        if (opaque.found.declined()) return unsupported(opaque.found);
        for (auto &workspace : workspaces) {
            for (auto &dim : workspace.dims) {
                if (!workspace_dim_can_expand(dim, kernel)) {
                    if (reason.declined()) return false;
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


};

class GpuExtentBuilder {
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
            case GpuExtentKind::Opaque: {
                // A leaf only the device can spell. It reaches the host
                // builder when a section's bounds are values the kernel
                // alone holds, and there is nothing to build from it.
                return nullptr;
            }
            case GpuExtentKind::Cast: {
                ASR::expr_t *argument = child(0);
                if (!argument) return nullptr;
                auto *conversion = ASR::down_cast<ASR::Cast_t>(e.expr);
                return ASRUtils::EXPR(ASR::make_Cast_t(al, loc,
                    argument, conversion->m_kind, conversion->m_type,
                    nullptr, nullptr));
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
            case GpuExtentKind::Call: {
                ASR::FunctionCall_t *call =
                    ASR::down_cast<ASR::FunctionCall_t>(e.expr);
                ASR::Function_t *callee = ASR::down_cast<ASR::Function_t>(
                    ASRUtils::symbol_get_past_external(call->m_name));
                Vec<ASR::call_arg_t> call_args;
                call_args.reserve(al, e.children.size());
                for (size_t i = 0; i < e.children.size(); i++) {
                    ASR::call_arg_t arg;
                    arg.loc = loc;
                    arg.m_value = child(i);
                    if (arg.m_value == nullptr || i >= callee->n_args) {
                        return nullptr;
                    }
                    // The host hands an array over as the function takes
                    // it, which is not how the host stores it.
                    ASR::ttype_t *type = ASRUtils::expr_type(arg.m_value);
                    ASR::ttype_t *dummy = ASRUtils::expr_type(
                        callee->m_args[i]);
                    if (ASRUtils::is_array(type) && ASRUtils::is_array(dummy)
                            && ASRUtils::extract_physical_type(type) !=
                                ASRUtils::extract_physical_type(dummy)) {
                        ASR::array_physical_typeType physical =
                            ASRUtils::extract_physical_type(dummy);
                        arg.m_value = ASRUtils::EXPR(
                            ASRUtils::make_ArrayPhysicalCast_t_util(al, loc,
                                arg.m_value,
                                ASRUtils::extract_physical_type(type),
                                physical, ASRUtils::duplicate_type(al, type,
                                    nullptr, physical, true), nullptr));
                    }
                    call_args.push_back(al, arg);
                }
                return ASRUtils::EXPR(ASR::make_FunctionCall_t(al, loc,
                    &callee->base, nullptr, call_args.p, call_args.n,
                    call->m_type, nullptr, nullptr));
            }
            case GpuExtentKind::Designator: {
                return host_designator(al, loc, kernel, args, n_args,
                    e.array);
            }
            case GpuExtentKind::Element: {
                ASR::expr_t *base = host_designator(al, loc, kernel, args,
                    n_args, e.array);
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


};


// The parameters of a kernel that its body may write, directly or through a
// procedure it calls. The host reads back only what the kernel writes, and
// keeps it on the device until it does (see device_launch_expand.cpp), so
// a parameter is taken to be written unless every reference to it is
// known to only read it: an operand of an expression, a subscript, the
// value of an assignment, an inquiry about its shape, or an actual argument
// of an `intent(in)` dummy argument. Anything else -- the target of an
// assignment, what an `allocate` or `deallocate` names, an alias bound to
// it, its address, an argument of a dummy that may be defined, or a
// statement this does not know -- counts as a write.
class KernelWrites : public ASR::BaseWalkVisitor<KernelWrites> {
public:
    std::set<ASR::symbol_t*> written;
    std::set<ASR::symbol_t*> visited_blocks;

    void visit_stmt(const ASR::stmt_t &x) {
        switch (x.type) {
            case ASR::stmtType::Assignment: {
                const ASR::Assignment_t &a =
                    *ASR::down_cast<ASR::Assignment_t>(&x);
                mark(a.m_target);
                visit_expr(*a.m_target);
                visit_expr(*a.m_value);
                return;
            }
            case ASR::stmtType::If:
            case ASR::stmtType::DoLoop:
            case ASR::stmtType::DoConcurrentLoop:
            case ASR::stmtType::WhileLoop:
            case ASR::stmtType::Exit:
            case ASR::stmtType::Cycle:
            case ASR::stmtType::Return:
            case ASR::stmtType::Select:
            case ASR::stmtType::DebugCheckArrayBounds:
                BaseWalkVisitor::visit_stmt(x);
                return;
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *block = ASR::down_cast<ASR::BlockCall_t>(
                    &x)->m_m;
                if (visited_blocks.insert(block).second) {
                    ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(
                        ASRUtils::symbol_get_past_external(block));
                    for (size_t i = 0; i < b->n_body; i++) {
                        visit_stmt(*b->m_body[i]);
                    }
                }
                return;
            }
            case ASR::stmtType::Allocate:
            case ASR::stmtType::ReAlloc: {
                // What is allocated is written; its shape is read.
                ASR::alloc_arg_t *args = ASR::is_a<ASR::Allocate_t>(x)
                    ? ASR::down_cast<ASR::Allocate_t>(&x)->m_args
                    : ASR::down_cast<ASR::ReAlloc_t>(&x)->m_args;
                size_t n = ASR::is_a<ASR::Allocate_t>(x)
                    ? ASR::down_cast<ASR::Allocate_t>(&x)->n_args
                    : ASR::down_cast<ASR::ReAlloc_t>(&x)->n_args;
                for (size_t i = 0; i < n; i++) {
                    mark(args[i].m_a);
                    visit_expr(*args[i].m_a);
                    for (size_t d = 0; d < args[i].n_dims; d++) {
                        if (args[i].m_dims[d].m_start) {
                            visit_expr(*args[i].m_dims[d].m_start);
                        }
                        if (args[i].m_dims[d].m_length) {
                            visit_expr(*args[i].m_dims[d].m_length);
                        }
                    }
                    if (args[i].m_len_expr) visit_expr(*args[i].m_len_expr);
                }
                if (ASR::is_a<ASR::Allocate_t>(x)) {
                    const ASR::Allocate_t &a =
                        *ASR::down_cast<ASR::Allocate_t>(&x);
                    if (a.m_stat) mark(a.m_stat);
                    if (a.m_errmsg) mark(a.m_errmsg);
                    if (a.m_source) visit_expr(*a.m_source);
                }
                return;
            }
            case ASR::stmtType::ExplicitDeallocate:
            case ASR::stmtType::ImplicitDeallocate: {
                ASR::expr_t **vars = ASR::is_a<ASR::ExplicitDeallocate_t>(x)
                    ? ASR::down_cast<ASR::ExplicitDeallocate_t>(&x)->m_vars
                    : ASR::down_cast<ASR::ImplicitDeallocate_t>(&x)->m_vars;
                size_t n = ASR::is_a<ASR::ExplicitDeallocate_t>(x)
                    ? ASR::down_cast<ASR::ExplicitDeallocate_t>(&x)->n_vars
                    : ASR::down_cast<ASR::ImplicitDeallocate_t>(&x)->n_vars;
                for (size_t i = 0; i < n; i++) {
                    mark(vars[i]);
                    visit_expr(*vars[i]);
                }
                return;
            }
            case ASR::stmtType::AssociateBlockCall: {
                ASR::symbol_t *block = ASR::down_cast<
                    ASR::AssociateBlockCall_t>(&x)->m_m;
                if (visited_blocks.insert(block).second) {
                    ASR::AssociateBlock_t *b = ASR::down_cast<
                        ASR::AssociateBlock_t>(
                            ASRUtils::symbol_get_past_external(block));
                    for (size_t i = 0; i < b->n_body; i++) {
                        visit_stmt(*b->m_body[i]);
                    }
                }
                return;
            }
            case ASR::stmtType::SubroutineCall: {
                const ASR::SubroutineCall_t &c =
                    *ASR::down_cast<ASR::SubroutineCall_t>(&x);
                call_arguments(c.m_name, c.m_args, c.n_args, c.m_dt);
                return;
            }
            default: {
                // A statement this does not know may write anything it
                // names.
                AllNames names(written);
                names.visit_stmt(x);
                return;
            }
        }
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        call_arguments(x.m_name, x.m_args, x.n_args, x.m_dt);
    }

    void visit_GpuAtomicFetchAdd(const ASR::GpuAtomicFetchAdd_t &x) {
        mark(x.m_target);
        BaseWalkVisitor::visit_GpuAtomicFetchAdd(x);
    }

    void visit_GetPointer(const ASR::GetPointer_t &x) {
        mark(x.m_arg);
        BaseWalkVisitor::visit_GetPointer(x);
    }

    void visit_PointerToCPtr(const ASR::PointerToCPtr_t &x) {
        mark(x.m_arg);
        BaseWalkVisitor::visit_PointerToCPtr(x);
    }

private:
    class AllNames : public ASR::BaseWalkVisitor<AllNames> {
    public:
        std::set<ASR::symbol_t*> &written;
        std::set<ASR::symbol_t*> blocks;
        explicit AllNames(std::set<ASR::symbol_t*> &written_)
            : written(written_) {}
        void visit_Var(const ASR::Var_t &x) {
            written.insert(ASRUtils::symbol_get_past_external(x.m_v));
        }
        void visit_BlockCall(const ASR::BlockCall_t &x) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
            if (!blocks.insert(b).second) return;
            ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
            for (size_t i = 0; i < block->n_body; i++) {
                visit_stmt(*block->m_body[i]);
            }
        }
        void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
            if (!blocks.insert(b).second) return;
            ASR::AssociateBlock_t *block =
                ASR::down_cast<ASR::AssociateBlock_t>(b);
            for (size_t i = 0; i < block->n_body; i++) {
                visit_stmt(*block->m_body[i]);
            }
        }
    };

    // The variable a designator names: `a` of `a(i)%b(j)`.
    static ASR::symbol_t* root(ASR::expr_t *e) {
        while (e != nullptr) {
            switch (e->type) {
                case ASR::exprType::Var:
                    return ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(e)->m_v);
                case ASR::exprType::ArrayItem:
                    e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v; break;
                case ASR::exprType::ArraySection:
                    e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v; break;
                case ASR::exprType::StructInstanceMember:
                    e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
                    break;
                case ASR::exprType::ArrayPhysicalCast:
                    e = ASR::down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
                    break;
                case ASR::exprType::StringItem:
                    e = ASR::down_cast<ASR::StringItem_t>(e)->m_arg; break;
                case ASR::exprType::StringSection:
                    e = ASR::down_cast<ASR::StringSection_t>(e)->m_arg;
                    break;
                default:
                    return nullptr;
            }
        }
        return nullptr;
    }

    void mark(ASR::expr_t *e) {
        ASR::symbol_t *r = root(e);
        if (r != nullptr) {
            written.insert(r);
        } else if (e != nullptr) {
            AllNames names(written);
            names.visit_expr(*e);
        }
    }

    // Whether dummy `i` of `fn` only reads its actual argument.
    static bool reads_only(ASR::Function_t *fn, size_t i) {
        if (fn == nullptr || i >= fn->n_args
                || !ASR::is_a<ASR::Var_t>(*fn->m_args[i])) {
            return false;
        }
        ASR::symbol_t *dummy = ASR::down_cast<ASR::Var_t>(
            fn->m_args[i])->m_v;
        return ASR::is_a<ASR::Variable_t>(*dummy)
            && ASR::down_cast<ASR::Variable_t>(dummy)->m_intent
                == ASR::intentType::In
            && !ASRUtils::is_pointer(ASRUtils::symbol_type(dummy));
    }

    // `dt` is the object of a type-bound call. When the call does not pass
    // it among `args`, it is the dummy the binding passes it as.
    void call_arguments(ASR::symbol_t *name, ASR::call_arg_t *args,
            size_t n_args, ASR::expr_t *dt) {
        ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(name);
        ASR::StructMethodDeclaration_t *binding = nullptr;
        // A type-bound procedure is the procedure its binding names, whose
        // dummy arguments any procedure overriding it shares.
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*callee)) {
            binding = ASR::down_cast<ASR::StructMethodDeclaration_t>(callee);
            callee = ASRUtils::symbol_get_past_external(binding->m_proc);
        }
        ASR::Function_t *fn = ASR::is_a<ASR::Function_t>(*callee)
            ? ASR::down_cast<ASR::Function_t>(callee) : nullptr;
        // The dummy the first actual argument is passed for: the one after
        // the passed object when the call does not pass the object among
        // its arguments. A passed object that is not the first dummy, or a
        // call this cannot match up, writes everything it is handed.
        size_t first = 0;
        bool known = fn != nullptr;
        if (dt != nullptr) {
            bool apart = known && n_args + 1 == fn->n_args
                && (binding == nullptr || !binding->m_is_nopass);
            if (apart && binding != nullptr && binding->m_self_argument
                    && (!ASR::is_a<ASR::Var_t>(*fn->m_args[0])
                        || std::string(ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(fn->m_args[0])->m_v))
                            != binding->m_self_argument)) {
                known = false;
            }
            if (known && apart) {
                first = 1;
                if (!reads_only(fn, 0)) mark(dt);
            } else if (!known || n_args != fn->n_args) {
                known = false;
                mark(dt);
            }
            visit_expr(*dt);
        }
        for (size_t i = 0; i < n_args; i++) {
            ASR::expr_t *value = args[i].m_value;
            if (value == nullptr) continue;
            if (!known || !reads_only(fn, first + i)) mark(value);
            visit_expr(*value);
        }
    }
};

} // namespace

// The host calls every function a workspace extent calls, as well as the
// device.
class HostCalledFunctions : public ASR::BaseWalkVisitor<HostCalledFunctions> {
public:
    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_name);
        if (s != nullptr && ASR::is_a<ASR::Function_t>(*s)) {
            ASR::FunctionType_t *signature = ASRUtils::get_FunctionType(
                ASR::down_cast<ASR::Function_t>(s));
            if (signature->m_exec_space == ASR::exec_spaceType::Device) {
                signature->m_exec_space = ASR::exec_spaceType::HostDevice;
            }
        }
        ASR::BaseWalkVisitor<HostCalledFunctions>::visit_FunctionCall(x);
    }
};

ASR::expr_t* gpu_workspace_host_extent(Allocator &al,
        ASR::Function_t &kernel, const GpuVlaDim &dim) {
    const Location &loc = kernel.base.base.loc;
    ASRUtils::ASRBuilder b(al, loc);
    // An extent the host can evaluate once, for every thread, is written
    // as that; one that changes from thread to thread is written over the
    // thread index, and the launch sizes every thread's slice by its
    // largest value.
    ASR::expr_t *extent = nullptr;
    if (dim.is_constant) {
        extent = b.i32(dim.constant_value);
    } else if (dim.is_struct_member_size) {
        if (dim.struct_member_elem_index >= 0) {
            extent = dim.source_extent;
        }
    } else if (dim.derived.ok()) {
        Vec<ASR::call_arg_t> identity;
        identity.reserve(al, kernel.n_args);
        for (size_t i = 0; i < kernel.n_args; i++) {
            ASR::call_arg_t arg;
            arg.loc = loc;
            arg.m_value = kernel.m_args[i];
            identity.push_back(al, arg);
        }
        extent = GpuExtentBuilder::build_host_extent(al, loc,
            &kernel, identity.p, identity.n, dim.derived);
    }
    if (extent == nullptr) {
        ASR::expr_t *source = dim.source_extent;
        if (source == nullptr && dim.source_array != nullptr) {
            source = b.ArraySize(dim.source_array,
                dim.source_array_dim > 0
                    ? b.i32(dim.source_array_dim) : nullptr, int32);
        }
        extent = gpu_kernel_per_thread_extent(al, kernel, source);
    }
    return extent;
}

bool gpu_create_kernel_layout(Allocator &al, ASR::Function_t &kernel,
        ASR::call_arg_t *args, size_t n_args, GpuDecline &decline) {
    if (kernel.m_gpu) return true;
    std::string unresolved_workspace;
    if (!gpu_kernel_workspace_extents_resolvable(kernel, unresolved_workspace)) {
        decline = GpuDecline(GpuDeclineReason::WorkspaceNotSizeableOnHost,
            unresolved_workspace);
        return false;
    }
    Vec<ASR::symbol_t*> device_functions;
    device_functions.reserve(al, 4);
    std::set<ASR::Function_t*> visited, active;
    std::function<bool(ASR::Function_t*)> close_graph =
        [&](ASR::Function_t *function) {
        if (active.count(function)) {
            decline = GpuDecline(GpuDeclineReason::RecursiveDeviceFunction,
                function->m_name);
            return false;
        }
        if (!visited.insert(function).second) return true;
        const auto *signature = ASRUtils::get_FunctionType(function);
        bool missing_implementation =
            signature->m_deftype != ASR::deftypeType::Implementation &&
            signature->m_abi != ASR::abiType::BindC &&
            signature->m_abi != ASR::abiType::Intrinsic;
        if (!ASRUtils::runs_on_device(*function) || missing_implementation) {
            decline = GpuDecline(GpuDeclineReason::DeviceFunctionImplementation,
                function->m_name);
            return false;
        }
        active.insert(function);
        std::set<ASR::Function_t*> callees = gpu_callees(function->m_body,
            function->n_body, true);
        gpu_type_callees(function->m_symtab, callees);
        for (ASR::Function_t *callee : callees) {
            if (callee == function) continue;
            if (!close_graph(callee)) return false;
        }
        active.erase(function);
        if (function != &kernel) device_functions.push_back(al, &function->base);
        return true;
    };
    if (!close_graph(&kernel)) return false;
    GpuLaunchSupport support(decline);
    auto workspaces = collect_gpu_vla_workspaces(kernel, 0, true);
    if (!support.supported(&kernel.base, args, n_args, workspaces,
            device_functions)) {
        return false;
    }
    const Location &loc = kernel.base.base.loc;
    ASRUtils::ASRBuilder b(al, loc);
    Vec<ASR::gpu_workspace_t> planned;
    planned.reserve(al, workspaces.size());
    for (const auto &workspace : workspaces) {
        ASR::gpu_workspace_t ws;
        ws.loc = loc;
        ws.m_variable = workspace.var;
        ws.m_element_size = workspace.elem_size;
        ws.m_buffer_index = 0;
        Vec<ASR::gpu_workspace_dimension_t> dims;
        dims.reserve(al, workspace.dims.size());
        for (const auto &dim : workspace.dims) {
            ASR::expr_t *extent = gpu_workspace_host_extent(al, kernel, dim);
            if (!extent) {
                decline = GpuDecline(
                    GpuDeclineReason::WorkspaceNotSizeableOnHost,
                    workspace.var_name);
                return false;
            }
            HostCalledFunctions host_calls;
            host_calls.visit_expr(*extent);
            if (!gpu_bind_kernel_expression(al, kernel, args, n_args,
                    extent)) {
                decline = GpuDecline(
                    GpuDeclineReason::LaunchVlaExtentNotRebuildable,
                    workspace.var_name);
                return false;
            }
            ASR::gpu_workspace_dimension_t d;
            d.loc = loc;
            d.m_extent = extent;
            d.m_parameter = nullptr;
            dims.push_back(al, d);
        }
        ws.m_dims = dims.p;
        ws.n_dims = dims.n;
        planned.push_back(al, ws);
    }
    size_t source_count = kernel.n_args;
    Vec<ASR::expr_t*> parameters;
    parameters.reserve(al, source_count + planned.n);
    Vec<ASR::ttype_t*> types;
    types.reserve(al, source_count + planned.n);
    ASR::FunctionType_t *signature = ASRUtils::get_FunctionType(&kernel);
    for (size_t i = 0; i < source_count; i++) {
        parameters.push_back(al, kernel.m_args[i]);
        types.push_back(al, signature->m_arg_types[i]);
    }
    for (size_t w = 0; w < planned.n; w++) {
        for (size_t d = 0; d < planned[w].n_dims; d++) {
            auto &dim = planned[w].m_dims[d];
            if (gpu_folded_int_constant(dim.m_extent)) continue;
            ASR::ttype_t *type = ASRUtils::expr_type(dim.m_extent);
            ASR::expr_t *parameter = b.Variable(kernel.m_symtab,
                kernel.m_symtab->get_unique_name("__workspace_extent"),
                type, ASR::intentType::In);
            dim.m_parameter = ASR::down_cast<ASR::Var_t>(parameter)->m_v;
            parameters.push_back(al, parameter);
            types.push_back(al, type);
        }
    }
    kernel.m_args = parameters.p;
    kernel.n_args = parameters.n;
    signature->m_arg_types = types.p;
    signature->n_arg_types = types.n;

    KernelWrites writes;
    for (size_t i = 0; i < kernel.n_body; i++) {
        writes.visit_stmt(*kernel.m_body[i]);
    }
    // The device reaches an allocatable array component of a derived-type
    // parameter through a parameter of its own, which the kernel names as
    // the component (`net%nodes_`): a write through the parameter may
    // write any of them.
    for (size_t i = 0; i < parameters.n; i++) {
        auto *var = ASR::down_cast<ASR::Variable_t>(
            ASR::down_cast<ASR::Var_t>(parameters[i])->m_v);
        if (writes.written.count(&var->base) == 0
                || ASRUtils::is_array(var->m_type)) {
            continue;
        }
        ASR::Struct_t *st = gpu_struct_definition(var->m_type_declaration);
        if (st == nullptr) continue;
        for (auto &component : gpu_decomposed_components(st)) {
            ASR::symbol_t *data = kernel.m_symtab->get_symbol(
                GpuNames::member_data(var->m_name, component.name()));
            if (data != nullptr) writes.written.insert(data);
        }
    }
    Vec<ASR::gpu_kernel_argument_t> buffers, scalars;
    buffers.reserve(al, parameters.n);
    scalars.reserve(al, parameters.n);
    auto argument = [&](size_t index, ASR::gpu_argument_kindType kind,
            ASR::symbol_t *member = nullptr, int dimension = -1) {
        ASR::gpu_kernel_argument_t arg;
        arg.loc = loc;
        arg.m_variable = ASR::down_cast<ASR::Var_t>(parameters[index])->m_v;
        arg.m_member = member;
        arg.m_kind = kind;
        arg.m_argument_index = index;
        arg.m_dimension = dimension;
        // The offsets and sizes of a decomposed component are built by the
        // host for the launch and only read by the device.
        arg.m_written = kind != ASR::gpu_argument_kindType::GpuMemberOffsets
            && kind != ASR::gpu_argument_kindType::GpuMemberSizes
            && writes.written.count(arg.m_variable) > 0;
        arg.m_type = ASRUtils::extract_type(ASRUtils::symbol_type(
            member ? member : arg.m_variable));
        if (kind == ASR::gpu_argument_kindType::GpuMemberOffsets ||
                kind == ASR::gpu_argument_kindType::GpuMemberSizes ||
                kind == ASR::gpu_argument_kindType::GpuArrayExtent ||
                kind == ASR::gpu_argument_kindType::GpuPackedOffset) {
            arg.m_type = int32;
        }
        return arg;
    };
    for (size_t i = 0; i < parameters.n; i++) {
        auto *var = ASR::down_cast<ASR::Variable_t>(
            ASR::down_cast<ASR::Var_t>(parameters[i])->m_v);
        bool array = ASRUtils::is_array(var->m_type);
        bool structure = ASR::is_a<ASR::StructType_t>(
            *ASRUtils::extract_type(var->m_type));
        if (array || structure) {
            buffers.push_back(al, argument(i, array
                ? ASR::gpu_argument_kindType::GpuArray
                : ASRUtils::is_class_type(ASRUtils::extract_type(var->m_type))
                    ? ASR::gpu_argument_kindType::GpuClass
                    : ASR::gpu_argument_kindType::GpuStruct));
            ASR::Struct_t *st = array ? gpu_struct_definition(var->m_type_declaration)
                                     : nullptr;
            if (!st) continue;
            for (auto &member : gpu_decomposed_components(st)) {
                for (auto kind : {ASR::gpu_argument_kindType::GpuMemberData,
                        ASR::gpu_argument_kindType::GpuMemberOffsets,
                        ASR::gpu_argument_kindType::GpuMemberSizes}) {
                    buffers.push_back(al, argument(i, kind, member.component));
                }
            }
        } else {
            scalars.push_back(al, argument(i,
                ASR::gpu_argument_kindType::GpuScalar));
        }
    }
    for (size_t i = 0; i < parameters.n; i++) {
        auto *var = ASR::down_cast<ASR::Variable_t>(
            ASR::down_cast<ASR::Var_t>(parameters[i])->m_v);
        ASR::ttype_t *type = ASRUtils::type_get_past_allocatable_pointer(
            var->m_type);
        if (!ASR::is_a<ASR::Array_t>(*type)) continue;
        auto *array = ASR::down_cast<ASR::Array_t>(type);
        if (array->m_physical_type ==
                ASR::array_physical_typeType::UnboundedPointerArray) continue;
        bool deferred = false;
        for (size_t d = 0; d < array->n_dims; d++) {
            deferred |= array->m_dims[d].m_length == nullptr;
        }
        if (!deferred) continue;
        for (size_t d = 0; d < array->n_dims; d++) {
            scalars.push_back(al, argument(i,
                ASR::gpu_argument_kindType::GpuArrayExtent, nullptr, checked_integer_cast<int>(d)));
        }
    }
    bool packed = buffers.n + (scalars.n > 0) + planned.n > MAX_METAL_BUFFERS;
    if (packed) {
        for (size_t i = 0; i < buffers.n; i++) {
            scalars.push_back(al, argument(buffers[i].m_argument_index,
                ASR::gpu_argument_kindType::GpuPackedOffset,
                buffers[i].m_member, checked_integer_cast<int>(i)));
        }
    }
    size_t workspace_start = (packed ? 1 : buffers.n) + (scalars.n > 0);
    for (size_t i = 0; i < planned.n; i++) {
        planned.p[i].m_buffer_index = workspace_start + i;
    }
    auto *layout = al.make_new<ASR::gpu_kernel_layout_t>();
    layout->loc = loc;
    layout->m_buffers = buffers.p;
    layout->n_buffers = buffers.n;
    layout->m_scalars = scalars.p;
    layout->n_scalars = scalars.n;
    layout->m_workspaces = planned.p;
    layout->n_workspaces = planned.n;
    layout->m_packed = packed;
    layout->m_source_argument_count = source_count;
    layout->m_device_functions = device_functions.p;
    layout->n_device_functions = device_functions.n;
    layout->m_lanes = nullptr;
    kernel.m_gpu = layout;
    return true;
}

} // namespace LCompilers

#include <map>
#include <set>

#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/pass/gpu_data_layout.h>

namespace LCompilers {

static ASR::ttype_t* struct_layout_type(Allocator &al, ASR::symbol_t *symbol);

std::string GpuComponentLayout::name() const {
    return ASRUtils::symbol_name(component);
}

GpuComponentLayout gpu_component_layout(ASR::symbol_t *component) {
    ASR::Variable_t *variable = ASR::down_cast<ASR::Variable_t>(
        ASRUtils::symbol_get_past_external(component));
    GpuComponentLayout out;
    out.component = component;
    out.allocatable = ASRUtils::is_allocatable(variable->m_type);
    out.type = ASRUtils::type_get_past_allocatable(variable->m_type);
    out.element_type = ASRUtils::type_get_past_array(out.type);
    out.rank = gpu_struct_member_rank(variable);
    out.element_struct = gpu_struct_definition(variable->m_type_declaration);
    out.element_is_empty = ASR::is_a<ASR::StructType_t>(*out.element_type)
        && out.element_struct && out.element_struct->n_members == 0;
    return out;
}

bool gpu_component_is_decomposed(ASR::symbol_t *component) {
    if (!component || !ASR::is_a<ASR::Variable_t>(*component)) return false;
    ASR::ttype_t *type = ASR::down_cast<ASR::Variable_t>(component)->m_type;
    return ASRUtils::is_allocatable(type)
        && ASR::is_a<ASR::Array_t>(*ASRUtils::type_get_past_allocatable(type));
}

static void collect_decomposed_components(ASR::Struct_t *st,
        std::vector<GpuComponentLayout> &out,
        std::set<ASR::Struct_t*> &seen) {
    if (!st || !seen.insert(st).second) return;
    collect_decomposed_components(gpu_struct_definition(st->m_parent), out,
        seen);
    for (size_t i = 0; i < st->n_members; i++) {
        ASR::symbol_t *component = st->m_symtab->get_symbol(st->m_members[i]);
        if (!gpu_component_is_decomposed(component)) continue;
        out.push_back(gpu_component_layout(component));
    }
}

std::vector<GpuComponentLayout> gpu_decomposed_components(
        ASR::Struct_t *definition) {
    std::vector<GpuComponentLayout> components;
    std::set<ASR::Struct_t*> seen;
    collect_decomposed_components(definition, components, seen);
    return components;
}

std::vector<GpuComponentLayout> gpu_component_layouts(
        const ASR::gpu_kernel_layout_t &layout, ASR::symbol_t *variable,
        ASR::Struct_t *in_struct) {
    std::vector<GpuComponentLayout> components;
    if (!variable) return components;
    variable = ASRUtils::symbol_get_past_external(variable);
    std::map<std::string, size_t> found;
    for (size_t i = 0; i < layout.n_buffers; i++) {
        const ASR::gpu_kernel_argument_t &entry = layout.m_buffers[i];
        if (!entry.m_member) continue;
        if (ASRUtils::symbol_get_past_external(entry.m_variable) != variable) {
            continue;
        }
        std::string name = ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(entry.m_member));
        if (entry.m_kind == ASR::gpu_argument_kindType::GpuMemberData) {
            ASR::symbol_t *component = in_struct
                ? gpu_struct_lookup_member(&in_struct->base, name)
                : entry.m_member;
            if (!component) continue;
            GpuComponentLayout described = gpu_component_layout(component);
            described.data = &entry;
            found[name] = components.size();
            components.push_back(described);
            continue;
        }
        auto it = found.find(name);
        if (it == found.end()) continue;
        if (entry.m_kind == ASR::gpu_argument_kindType::GpuMemberOffsets) {
            components[it->second].offsets = &entry;
        } else if (entry.m_kind == ASR::gpu_argument_kindType::GpuMemberSizes) {
            components[it->second].sizes = &entry;
        }
    }
    return components;
}

ASR::Struct_t* gpu_struct_definition(ASR::symbol_t *struct_sym) {
    if (!struct_sym) return nullptr;
    ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(struct_sym);
    if (!ASR::is_a<ASR::Struct_t>(*sym)) return nullptr;
    return ASR::down_cast<ASR::Struct_t>(sym);
}

void gpu_collect_data_members(ASR::Struct_t *st,
        std::vector<ASR::symbol_t*> &members) {
    if (!st) return;
    if (st->m_parent) gpu_collect_data_members(gpu_struct_definition(st->m_parent), members);
    for (size_t i = 0; i < st->n_members; i++) {
        members.push_back(st->m_symtab->get_symbol(st->m_members[i]));
    }
}

bool gpu_struct_has_allocatable_parts(ASR::symbol_t *struct_sym) {
    ASR::Struct_t *st = gpu_struct_definition(struct_sym);
    if (!st) return true;
    if (st->m_parent && gpu_struct_has_allocatable_parts(st->m_parent)) {
        return true;
    }
    for (size_t i = 0; i < st->n_members; i++) {
        ASR::symbol_t *member = st->m_symtab->get_symbol(st->m_members[i]);
        if (!member || !ASR::is_a<ASR::Variable_t>(*member)) return true;
        ASR::ttype_t *type = ASRUtils::symbol_type(member);
        if (ASRUtils::is_allocatable_or_pointer(type)) return true;
        ASR::ttype_t *base = ASRUtils::type_get_past_array(type);
        if (ASR::is_a<ASR::StructType_t>(*base) &&
                gpu_struct_has_allocatable_parts(
                    ASR::down_cast<ASR::Variable_t>(member)
                        ->m_type_declaration)) {
            return true;
        }
    }
    return false;
}

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

static ASR::ttype_t* struct_layout_type(Allocator &al,
        ASR::symbol_t *struct_sym) {
    ASR::Struct_t *st = gpu_struct_definition(struct_sym);
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

ASR::ttype_t* gpu_size_of_type_argument(Allocator &al, ASR::expr_t *value,
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

} // namespace LCompilers

#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_data_layout.h>

namespace LCompilers {

static ASR::ttype_t* struct_layout_type(Allocator &al, ASR::symbol_t *symbol);

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

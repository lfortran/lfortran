#include <libasr/pass/procedure_argument_utils.h>

namespace LCompilers {

namespace ProcedureArguments {

ASR::symbol_t *scope_owner(SymbolTable *scope) {
    if (scope != nullptr && scope->asr_owner != nullptr &&
            ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) {
        return ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
    }
    return nullptr;
}

bool in_scope_chain(SymbolTable *scope, SymbolTable *s) {
    return scope == s || ASRUtils::is_parent(s, scope);
}

bool visible_from(SymbolTable *scope, ASR::symbol_t *sym) {
    SymbolTable *parent = ASRUtils::symbol_parent_symtab(sym);
    return in_scope_chain(scope, parent) &&
        parent->get_symbol(ASRUtils::symbol_name(sym)) == sym;
}

bool is_internal_procedure(ASR::symbol_t *sym) {
    if (!ASR::is_a<ASR::Function_t>(*sym)) {
        return false;
    }
    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym);
    ASR::symbol_t *owner = scope_owner(f->m_symtab->parent);
    return ASRUtils::get_FunctionType(f)->m_deftype ==
            ASR::deftypeType::Implementation && owner != nullptr &&
        (ASR::is_a<ASR::Function_t>(*owner) ||
         ASR::is_a<ASR::Program_t>(*owner));
}

ASR::symbol_t *top_level_unit(SymbolTable *scope) {
    ASR::symbol_t *unit = nullptr;
    for (; scope != nullptr; scope = scope->parent) {
        ASR::symbol_t *owner = scope_owner(scope);
        if (owner != nullptr) {
            unit = owner;
        }
    }
    return unit;
}

ASR::symbol_t *dummy_symbol(ASR::Function_t *f, size_t i) {
    if (f->m_args[i] == nullptr || !ASR::is_a<ASR::Var_t>(*f->m_args[i])) {
        return nullptr;
    }
    return ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v;
}

ASR::Function_t *removable_dummy_interface(ASR::Function_t *f, size_t i) {
    ASR::symbol_t *sym = dummy_symbol(f, i);
    if (sym == nullptr) {
        return nullptr;
    }
    ASR::Function_t *iface = nullptr;
    if (ASR::is_a<ASR::Function_t>(*sym)) {
        iface = ASR::down_cast<ASR::Function_t>(sym);
    } else if (ASR::is_a<ASR::Variable_t>(*sym)) {
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
        if (!ASR::is_a<ASR::FunctionType_t>(*v->m_type) ||
                v->m_presence != ASR::presenceType::Required ||
                v->m_type_declaration == nullptr) {
            return nullptr;
        }
        ASR::symbol_t *decl = ASRUtils::symbol_get_past_external(
            v->m_type_declaration);
        if (!ASR::is_a<ASR::Function_t>(*decl)) {
            return nullptr;
        }
        iface = ASR::down_cast<ASR::Function_t>(decl);
    } else {
        return nullptr;
    }
    ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(iface);
    if (ft->m_deftype == ASR::deftypeType::ImplicitInterface ||
            ft->m_abi != ASR::abiType::Source || ft->m_elemental) {
        return nullptr;
    }
    return iface;
}

bool same_dummy(ASR::expr_t *a, ASR::expr_t *b) {
    if (a == nullptr || b == nullptr || !ASR::is_a<ASR::Var_t>(*a) ||
            !ASR::is_a<ASR::Var_t>(*b)) {
        return false;
    }
    ASR::symbol_t *sa = ASR::down_cast<ASR::Var_t>(a)->m_v;
    ASR::symbol_t *sb = ASR::down_cast<ASR::Var_t>(b)->m_v;
    if (!ASR::is_a<ASR::Variable_t>(*sa) || !ASR::is_a<ASR::Variable_t>(*sb)) {
        return false;
    }
    ASR::Variable_t *va = ASR::down_cast<ASR::Variable_t>(sa);
    ASR::Variable_t *vb = ASR::down_cast<ASR::Variable_t>(sb);
    if (va->m_intent != vb->m_intent || va->m_presence != vb->m_presence ||
            va->m_value_attr != vb->m_value_attr ||
            va->m_target_attr != vb->m_target_attr ||
            va->m_contiguous_attr != vb->m_contiguous_attr) {
        return false;
    }
    ASR::ttype_t *ta = va->m_type, *tb = vb->m_type;
    if (ta->type != tb->type || ASRUtils::is_allocatable(ta) !=
                ASRUtils::is_allocatable(tb) ||
            ASRUtils::is_pointer(ta) != ASRUtils::is_pointer(tb)) {
        return false;
    }
    ASR::ttype_t *ea = ASRUtils::type_get_past_allocatable_pointer(ta);
    ASR::ttype_t *eb = ASRUtils::type_get_past_allocatable_pointer(tb);
    if (ea->type != eb->type || ASRUtils::is_array(ea) != ASRUtils::is_array(eb)) {
        return false;
    }
    if (ASRUtils::is_array(ea)) {
        if (ASRUtils::extract_n_dims_from_ttype(ea) !=
                    ASRUtils::extract_n_dims_from_ttype(eb) ||
                ASRUtils::extract_physical_type(ea) !=
                    ASRUtils::extract_physical_type(eb)) {
            return false;
        }
    }
    ASR::ttype_t *xa = ASRUtils::type_get_past_array(ea);
    ASR::ttype_t *xb = ASRUtils::type_get_past_array(eb);
    if (xa->type != xb->type) {
        return false;
    }
    switch (xa->type) {
        case ASR::ttypeType::Integer:
        case ASR::ttypeType::UnsignedInteger:
        case ASR::ttypeType::Real:
        case ASR::ttypeType::Complex:
        case ASR::ttypeType::Logical:
            break;
        case ASR::ttypeType::String: {
            ASR::String_t *a_str = ASR::down_cast<ASR::String_t>(xa);
            ASR::String_t *b_str = ASR::down_cast<ASR::String_t>(xb);
            if (a_str->m_len_kind != b_str->m_len_kind ||
                    a_str->m_physical_type != b_str->m_physical_type) {
                return false;
            }
            if (a_str->m_len_kind == ASR::string_length_kindType::ExpressionLength) {
                int64_t la = -1, lb = -1;
                if (a_str->m_len == nullptr || b_str->m_len == nullptr ||
                        !ASRUtils::extract_value(ASRUtils::expr_value(a_str->m_len), la) ||
                        !ASRUtils::extract_value(ASRUtils::expr_value(b_str->m_len), lb) ||
                        la != lb) {
                    return false;
                }
            }
            break;
        }
        case ASR::ttypeType::StructType: {
            if (ASRUtils::is_class_type(xa) || ASRUtils::is_class_type(xb)) {
                return false;
            }
            break;
        }
        default:
            return false;
    }
    return ASRUtils::types_equal(ta, tb, a, b, true) &&
        ASRUtils::types_equal(tb, ta, b, a, true) &&
        ASRUtils::extract_kind_from_ttype_t(xa) ==
            ASRUtils::extract_kind_from_ttype_t(xb);
}

bool same_interface(ASR::Function_t *iface, ASR::Function_t *actual) {
    ASR::FunctionType_t *at = ASRUtils::get_FunctionType(actual);
    if (at->m_abi != ASR::abiType::Source || at->m_elemental ||
            iface->n_args != actual->n_args ||
            (iface->m_return_var == nullptr) != (actual->m_return_var == nullptr)) {
        return false;
    }
    for (size_t i = 0; i < iface->n_args; i++) {
        if (!same_dummy(iface->m_args[i], actual->m_args[i])) {
            return false;
        }
    }
    if (iface->m_return_var != nullptr) {
        ASR::Variable_t *ra = ASRUtils::EXPR2VAR(iface->m_return_var);
        ASR::Variable_t *rb = ASRUtils::EXPR2VAR(actual->m_return_var);
        if (ASRUtils::is_array(ra->m_type) || ASRUtils::is_array(rb->m_type) ||
                ASRUtils::is_allocatable_or_pointer(ra->m_type) ||
                ASRUtils::is_allocatable_or_pointer(rb->m_type)) {
            return false;
        }
        switch (ra->m_type->type) {
            case ASR::ttypeType::Integer:
            case ASR::ttypeType::UnsignedInteger:
            case ASR::ttypeType::Real:
            case ASR::ttypeType::Complex:
            case ASR::ttypeType::Logical:
                break;
            default:
                return false;
        }
        if (ra->m_type->type != rb->m_type->type ||
                ASRUtils::extract_kind_from_ttype_t(ra->m_type) !=
                    ASRUtils::extract_kind_from_ttype_t(rb->m_type)) {
            return false;
        }
    }
    return true;
}

bool copyable_scope(SymbolTable *scope, bool internal_procedures) {
    for (auto &item: scope->get_scope()) {
        ASR::symbol_t *sym = item.second;
        switch (sym->type) {
            case ASR::symbolType::Variable: {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
                if (v->m_storage == ASR::storage_typeType::Save ||
                        v->m_storage == ASR::storage_typeType::Threadprivate ||
                        v->m_common_block != nullptr) {
                    return false;
                }
                break;
            }
            case ASR::symbolType::Function: {
                ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(sym);
                if (ft->m_deftype == ASR::deftypeType::Implementation &&
                        (!internal_procedures ||
                         ft->m_abi != ASR::abiType::Source ||
                         !copyable_scope(ASR::down_cast<ASR::Function_t>(
                            sym)->m_symtab, true))) {
                    return false;
                }
                break;
            }
            case ASR::symbolType::ExternalSymbol:
                break;
            case ASR::symbolType::Block:
                if (!copyable_scope(ASR::down_cast<ASR::Block_t>(sym)->m_symtab,
                        internal_procedures)) {
                    return false;
                }
                break;
            case ASR::symbolType::AssociateBlock:
                if (!copyable_scope(
                        ASR::down_cast<ASR::AssociateBlock_t>(sym)->m_symtab,
                        internal_procedures)) {
                    return false;
                }
                break;
            default:
                return false;
        }
    }
    return true;
}

bool copyable_procedure(ASR::Function_t *f, bool internal_procedures) {
    ASR::FunctionType_t *ft = ASRUtils::get_FunctionType(f);
    if (ft->m_deftype != ASR::deftypeType::Implementation ||
            ft->m_abi != ASR::abiType::Source || ft->m_elemental ||
            ft->m_static || ft->n_restrictions > 0 || ft->m_is_restriction ||
            ft->m_exec_space != ASR::exec_spaceType::Host ||
            f->m_gpu != nullptr || f->n_body == 0) {
        return false;
    }
    ASR::symbol_t *unit = top_level_unit(f->m_symtab->parent);
    if (unit != nullptr && ASR::is_a<ASR::Module_t>(*unit)) {
        ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(unit);
        if (m->m_loaded_from_mod || m->m_intrinsic) {
            return false;
        }
    }
    if (!copyable_scope(f->m_symtab, internal_procedures)) {
        return false;
    }
    NodeCounter counter;
    counter.visit_Function(*f);
    return counter.count <= max_copied_nodes;
}

ASR::Function_t *called_procedure(ASR::symbol_t *name, ASR::expr_t *dt) {
    if (dt != nullptr) {
        return nullptr;
    }
    ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(name);
    if (sym == nullptr || !ASR::is_a<ASR::Function_t>(*sym) ||
            ASRUtils::get_FunctionType(sym)->m_deftype !=
                ASR::deftypeType::Implementation) {
        return nullptr;
    }
    return ASR::down_cast<ASR::Function_t>(sym);
}

void collect_procedures(SymbolTable *scope,
        std::vector<ASR::Function_t*> &procedures) {
    for (auto &item: scope->get_scope()) {
        ASR::symbol_t *sym = item.second;
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym);
            if (ASRUtils::get_FunctionType(f)->m_deftype ==
                    ASR::deftypeType::Implementation) {
                procedures.push_back(f);
            }
        }
        SymbolTable *nested = ASRUtils::symbol_symtab(sym);
        if (nested != nullptr && (ASR::is_a<ASR::Function_t>(*sym) ||
                ASR::is_a<ASR::Program_t>(*sym) ||
                ASR::is_a<ASR::Module_t>(*sym) ||
                ASR::is_a<ASR::Block_t>(*sym) ||
                ASR::is_a<ASR::AssociateBlock_t>(*sym))) {
            collect_procedures(nested, procedures);
        }
    }
}

void add_module_dependency(Allocator &al, SymbolTable *scope,
        const std::string &module_name) {
    ASR::symbol_t *unit = top_level_unit(scope);
    if (unit == nullptr) {
        return;
    }
    char **deps;
    size_t n_deps;
    if (ASR::is_a<ASR::Module_t>(*unit)) {
        ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(unit);
        if (module_name == m->m_name) {
            return;
        }
        deps = m->m_dependencies;
        n_deps = m->n_dependencies;
    } else if (ASR::is_a<ASR::Program_t>(*unit)) {
        ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(unit);
        deps = p->m_dependencies;
        n_deps = p->n_dependencies;
    } else {
        return;
    }
    SetChar new_deps;
    new_deps.from_pointer_n_copy(al, deps, n_deps);
    new_deps.push_back(al, s2c(al, module_name));
    if (ASR::is_a<ASR::Module_t>(*unit)) {
        ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(unit);
        m->m_dependencies = new_deps.p;
        m->n_dependencies = new_deps.size();
    } else {
        ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(unit);
        p->m_dependencies = new_deps.p;
        p->n_dependencies = new_deps.size();
    }
}

} // namespace ProcedureArguments

} // namespace LCompilers

#ifndef LFORTRAN_SEMANTICS_TRAIT_PROCEDURES_H
#define LFORTRAN_SEMANTICS_TRAIT_PROCEDURES_H

#include <lfortran/ast.h>
#include <libasr/containers.h>

namespace LCompilers::LFortran {

// Use the existing templated-procedure pipeline without changing the source AST.
inline AST::program_unit_t *lower_trait_procedure(
        Allocator &al, const AST::TraitProcedure_t &x) {
    Vec<char*> names;
    names.reserve(al, x.n_parameters);
    Vec<AST::decl_stmt_t*> declarations;
    declarations.reserve(al, x.n_parameters);
    for (size_t i = 0; i < x.n_parameters; i++) {
        const auto &parameter = x.m_parameters[i];
        names.push_back(al, parameter.m_name);
        Vec<AST::decl_attribute_t*> attributes;
        attributes.reserve(al, 1);
        attributes.push_back(al, AST::down_cast<AST::decl_attribute_t>(
            AST::make_SimpleAttribute_t(al, parameter.loc,
                AST::simple_attributeType::AttrDeferred)));
        declarations.push_back(al, AST::down_cast<AST::decl_stmt_t>(
            AST::make_DerivedType_t(al, parameter.loc, parameter.m_name,
                nullptr, 0, nullptr, attributes.p, attributes.size(),
                nullptr, 0, nullptr, 0)));
    }
    auto fill = [&](auto *procedure) {
        Vec<AST::decl_stmt_t*> items;
        items.reserve(al, declarations.size() + procedure->n_items);
        for (auto *declaration : declarations) items.push_back(al, declaration);
        for (size_t i = 0; i < procedure->n_items; i++) {
            items.push_back(al, procedure->m_items[i]);
        }
        procedure->m_temp_args = names.p;
        procedure->n_temp_args = names.size();
        procedure->m_items = items.p;
        procedure->n_items = items.size();
        return &procedure->base;
    };
    if (AST::is_a<AST::Function_t>(*x.m_procedure)) {
        return fill(al.make_new<AST::Function_t>(
            *AST::down_cast<AST::Function_t>(x.m_procedure)));
    }
    LCOMPILERS_ASSERT(AST::is_a<AST::Subroutine_t>(*x.m_procedure));
    return fill(al.make_new<AST::Subroutine_t>(
        *AST::down_cast<AST::Subroutine_t>(x.m_procedure)));
}

} // namespace LCompilers::LFortran

#endif

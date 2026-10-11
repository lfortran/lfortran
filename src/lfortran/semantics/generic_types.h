#ifndef LFORTRAN_SEMANTICS_GENERIC_TYPES_H
#define LFORTRAN_SEMANTICS_GENERIC_TYPES_H

#include <map>
#include <set>
#include <string>
#include <tuple>
#include <vector>

#include <lfortran/ast.h>
#include <libasr/containers.h>
#include <libasr/string_utils.h>

namespace LCompilers::LFortran {

// A generic derived type of a module becomes a package: an AST Template named
// like the type whose deferred arguments are the type's parameters. It holds
// the definition, the implements statements of the same module that name the
// type with its parameter list, and the procedures bound to the type that
// depend on its parameters. Both semantic visitors lower a module this way,
// so they visit the same package scopes. The source AST is not changed.
struct GenericTypePackageAST {
    const AST::TraitDerivedType_t *definition;
    std::vector<const AST::Implements_t*> implements;
    // The procedures this type binds that the package of another generic
    // type owns: the procedure, that type and the binding's location.
    std::vector<std::tuple<std::string, std::string, Location>> foreign;
    // The procedures of the module this type binds that no package owns,
    // because they do not depend on the parameters, and the bindings'
    // locations.
    std::vector<std::pair<std::string, Location>> independent;
};

inline const AST::DerivedType_t &generic_type_definition(
        const AST::TraitDerivedType_t &x) {
    LCOMPILERS_ASSERT(AST::is_a<AST::DerivedType_t>(*x.m_derived_type));
    return *AST::down_cast<AST::DerivedType_t>(x.m_derived_type);
}

inline std::string generic_type_name(const AST::TraitDerivedType_t &x) {
    return to_lower(generic_type_definition(x).m_name);
}

// The generic type named as the implementing type of `x`, with or without a
// parameter list; empty for any other implements statement.
inline std::string generic_implements_target(const AST::Implements_t &x) {
    if (AST::is_a<AST::AttrGenericTypeParams_t>(*x.m_implementing_type)) {
        return to_lower(AST::down_cast<AST::AttrGenericTypeParams_t>(
            x.m_implementing_type)->m_name);
    }
    if (AST::is_a<AST::AttrType_t>(*x.m_implementing_type)) {
        auto *type = AST::down_cast<AST::AttrType_t>(x.m_implementing_type);
        if (type->m_type == AST::decl_typeType::TypeType && type->m_name) {
            return to_lower(type->m_name);
        }
    }
    return "";
}

inline const char *program_unit_name(const AST::program_unit_t &x) {
    if (AST::is_a<AST::TraitProcedure_t>(x)) {
        return program_unit_name(*AST::down_cast<AST::TraitProcedure_t>(&x)->m_procedure);
    }
    if (AST::is_a<AST::Function_t>(x)) return AST::down_cast<AST::Function_t>(&x)->m_name;
    if (AST::is_a<AST::Subroutine_t>(x)) return AST::down_cast<AST::Subroutine_t>(&x)->m_name;
    if (AST::is_a<AST::Procedure_t>(x)) return AST::down_cast<AST::Procedure_t>(&x)->m_name;
    return nullptr;
}

// The procedures named by the bindings `procedures` of a type or of an
// implements statement, with whether each is an initializer, and where each
// is bound first.
inline void generic_type_bound_procedures(AST::procedure_decl_t **procedures,
        size_t n_procedures, std::map<std::string, bool> &bound,
        std::map<std::string, Location> *locations = nullptr) {
    for (size_t i = 0; i < n_procedures; i++) {
        if (AST::is_a<AST::InitialProcedure_t>(*procedures[i])) {
            auto *initial = AST::down_cast<AST::InitialProcedure_t>(procedures[i]);
            for (size_t j = 0; j < initial->n_names; j++) {
                bound[to_lower(initial->m_names[j])] = true;
                if (locations) {
                    locations->emplace(to_lower(initial->m_names[j]), initial->base.base.loc);
                }
            }
        } else if (AST::is_a<AST::DerivedTypeProc_t>(*procedures[i])) {
            auto *binding = AST::down_cast<AST::DerivedTypeProc_t>(procedures[i]);
            for (size_t j = 0; j < binding->n_symbols; j++) {
                if (!AST::is_a<AST::UseSymbol_t>(*binding->m_symbols[j])) continue;
                std::string name = to_lower(binding->m_name ? binding->m_name
                    : AST::down_cast<AST::UseSymbol_t>(binding->m_symbols[j])->m_remote_sym);
                bound.emplace(name, false);
                if (locations) locations->emplace(name, binding->base.base.loc);
            }
        }
    }
}

// Whether a type specification names a parameter of the generic type `type`
// or applies `type` itself.
inline bool generic_type_attribute_dependent(const AST::decl_attribute_t *x,
        const std::set<std::string> &parameters, const std::string &type) {
    if (!x) return false;
    if (AST::is_a<AST::AttrType_t>(*x)) {
        auto *attr = AST::down_cast<AST::AttrType_t>(x);
        if ((attr->m_type == AST::decl_typeType::TypeType ||
                attr->m_type == AST::decl_typeType::TypeClass) && attr->m_name &&
                parameters.count(to_lower(attr->m_name))) {
            return true;
        }
        return generic_type_attribute_dependent(attr->m_attr, parameters, type);
    }
    if (AST::is_a<AST::AttrName_t>(*x)) {
        return parameters.count(to_lower(AST::down_cast<AST::AttrName_t>(x)->m_name)) > 0;
    }
    if (AST::is_a<AST::AttrKeyword_t>(*x)) {
        return generic_type_attribute_dependent(
            AST::down_cast<AST::AttrKeyword_t>(x)->m_value, parameters, type);
    }
    if (AST::is_a<AST::AttrGenericTypeArgs_t>(*x)) {
        auto *application = AST::down_cast<AST::AttrGenericTypeArgs_t>(x);
        if (to_lower(application->m_name) == type) return true;
        for (size_t i = 0; i < application->n_args; i++) {
            if (generic_type_attribute_dependent(application->m_args[i], parameters, type)) {
                return true;
            }
        }
        return false;
    }
    if (AST::is_a<AST::AttrExpr_t>(*x)) {
        auto *value = AST::down_cast<AST::AttrExpr_t>(x)->m_value;
        if (AST::is_a<AST::GenericProcedureValue_t>(*value)) {
            auto *application = AST::down_cast<AST::GenericProcedureValue_t>(value);
            if (to_lower(application->m_name) == type) return true;
            for (size_t i = 0; i < application->n_args; i++) {
                if (generic_type_attribute_dependent(application->m_args[i],
                        parameters, type)) return true;
            }
        }
    }
    return false;
}

inline bool generic_type_procedure_dependent(const AST::program_unit_t &x,
        const std::set<std::string> &parameters, const std::string &type);

inline bool generic_type_declarations_dependent(AST::decl_stmt_t **items,
        size_t n_items, AST::program_unit_t **contains, size_t n_contains,
        const std::set<std::string> &parameters, const std::string &type) {
    for (size_t i = 0; i < n_items; i++) {
        if (!AST::is_a<AST::Declaration_t>(*items[i])) continue;
        auto *declaration = AST::down_cast<AST::Declaration_t>(items[i]);
        if (generic_type_attribute_dependent(declaration->m_vartype, parameters, type)) {
            return true;
        }
    }
    for (size_t i = 0; i < n_contains; i++) {
        if (generic_type_procedure_dependent(*contains[i], parameters, type)) return true;
    }
    return false;
}

// Whether the declarations of a procedure (dummies, result, locals, internal
// procedures) name a parameter of the generic type `type` or apply `type`.
inline bool generic_type_procedure_dependent(const AST::program_unit_t &x,
        const std::set<std::string> &parameters, const std::string &type) {
    if (AST::is_a<AST::TraitProcedure_t>(x)) {
        return generic_type_procedure_dependent(
            *AST::down_cast<AST::TraitProcedure_t>(&x)->m_procedure, parameters, type);
    }
    if (AST::is_a<AST::Function_t>(x)) {
        auto *function = AST::down_cast<AST::Function_t>(&x);
        for (size_t i = 0; i < function->n_attributes; i++) {
            if (generic_type_attribute_dependent(function->m_attributes[i],
                    parameters, type)) return true;
        }
        return generic_type_declarations_dependent(function->m_items, function->n_items,
            function->m_contains, function->n_contains, parameters, type);
    }
    if (AST::is_a<AST::Subroutine_t>(x)) {
        auto *subroutine = AST::down_cast<AST::Subroutine_t>(&x);
        return generic_type_declarations_dependent(subroutine->m_items,
            subroutine->n_items, subroutine->m_contains, subroutine->n_contains,
            parameters, type);
    }
    return false;
}

// The module `x` with each generic derived type lowered to its package, or
// nullptr when it has none. `packages` receives each package Template.
inline AST::Module_t *lower_generic_type_module(Allocator &al, const AST::Module_t &x,
        std::map<const AST::Template_t*, GenericTypePackageAST> &packages) {
    std::map<std::string, GenericTypePackageAST> generic_types;
    for (size_t i = 0; i < x.n_items; i++) {
        if (AST::is_a<AST::TraitDerivedType_t>(*x.m_items[i])) {
            auto *definition = AST::down_cast<AST::TraitDerivedType_t>(x.m_items[i]);
            generic_types.emplace(generic_type_name(*definition),
                GenericTypePackageAST{definition, {}});
        }
    }
    if (generic_types.empty()) return nullptr;
    std::set<const AST::decl_stmt_t*> attached;
    for (size_t i = 0; i < x.n_items; i++) {
        if (!AST::is_a<AST::Implements_t>(*x.m_items[i])) continue;
        auto *implements = AST::down_cast<AST::Implements_t>(x.m_items[i]);
        auto found = generic_types.find(generic_implements_target(*implements));
        if (found == generic_types.end()) continue;
        found->second.implements.push_back(implements);
        attached.insert(x.m_items[i]);
    }
    // A procedure bound to a generic type moves into its package when it is
    // an initializer or depends on the type's parameters. A procedure that
    // does not depend on them stays an ordinary module procedure.
    std::map<std::string, std::string> owners;
    for (auto &entry : generic_types) {
        const auto &definition = generic_type_definition(*entry.second.definition);
        std::map<std::string, bool> bound;
        generic_type_bound_procedures(definition.m_contains, definition.n_contains, bound);
        for (auto *implements : entry.second.implements) {
            generic_type_bound_procedures(implements->m_procedures,
                implements->n_procedures, bound);
        }
        std::set<std::string> parameters;
        for (size_t i = 0; i < entry.second.definition->n_parameters; i++) {
            parameters.insert(to_lower(entry.second.definition->m_parameters[i].m_name));
        }
        for (size_t i = 0; i < x.n_contains; i++) {
            const char *name = program_unit_name(*x.m_contains[i]);
            if (!name) continue;
            auto binding = bound.find(to_lower(name));
            if (binding == bound.end()) continue;
            if (!binding->second && !generic_type_procedure_dependent(
                    *x.m_contains[i], parameters, entry.first)) continue;
            // The first type binding a dependent procedure owns it; the
            // semantic visitor diagnoses the others.
            owners.emplace(to_lower(name), entry.first);
        }
    }
    std::set<std::string> module_procedures;
    for (size_t i = 0; i < x.n_contains; i++) {
        const char *name = program_unit_name(*x.m_contains[i]);
        if (name) module_procedures.insert(to_lower(name));
    }
    // A procedure the package of another type owns cannot be bound here.
    for (auto &entry : generic_types) {
        const auto &definition = generic_type_definition(*entry.second.definition);
        std::map<std::string, bool> bound;
        std::map<std::string, Location> locations;
        generic_type_bound_procedures(definition.m_contains, definition.n_contains, bound,
            &locations);
        for (auto *implements : entry.second.implements) {
            generic_type_bound_procedures(implements->m_procedures,
                implements->n_procedures, bound, &locations);
        }
        for (const auto &binding : bound) {
            auto owner = owners.find(binding.first);
            if (owner != owners.end() && owner->second != entry.first) {
                entry.second.foreign.emplace_back(binding.first, owner->second,
                    locations.at(binding.first));
            } else if (owner == owners.end() && module_procedures.count(binding.first)) {
                entry.second.independent.emplace_back(binding.first,
                    locations.at(binding.first));
            }
        }
    }
    std::map<std::string, Vec<AST::program_unit_t*>> procedures;
    for (auto &entry : generic_types) {
        procedures[entry.first].reserve(al, 0);
    }
    Vec<AST::program_unit_t*> contains;
    contains.reserve(al, x.n_contains);
    for (size_t i = 0; i < x.n_contains; i++) {
        const char *name = program_unit_name(*x.m_contains[i]);
        auto owner = name ? owners.find(to_lower(name)) : owners.end();
        if (owner == owners.end()) {
            contains.push_back(al, x.m_contains[i]);
        } else {
            procedures[owner->second].push_back(al, x.m_contains[i]);
        }
    }
    Vec<AST::decl_stmt_t*> items;
    items.reserve(al, x.n_items);
    for (size_t i = 0; i < x.n_items; i++) {
        if (attached.count(x.m_items[i])) continue;
        if (!AST::is_a<AST::TraitDerivedType_t>(*x.m_items[i])) {
            items.push_back(al, x.m_items[i]);
            continue;
        }
        auto *definition = AST::down_cast<AST::TraitDerivedType_t>(x.m_items[i]);
        auto &package = generic_types.at(generic_type_name(*definition));
        Vec<char*> names;
        names.reserve(al, definition->n_parameters);
        for (size_t j = 0; j < definition->n_parameters; j++) {
            names.push_back(al, definition->m_parameters[j].m_name);
        }
        Vec<AST::decl_stmt_t*> package_items;
        package_items.reserve(al, 1 + package.implements.size());
        package_items.push_back(al, x.m_items[i]);
        for (auto *implements : package.implements) {
            package_items.push_back(al, const_cast<AST::decl_stmt_t*>(&implements->base));
        }
        auto &package_procedures = procedures[generic_type_name(*definition)];
        auto *lowered = AST::down_cast2<AST::Template_t>(AST::make_Template_t(al,
            definition->base.base.loc, generic_type_definition(*definition).m_name,
            names.p, names.size(), package_items.p, package_items.size(),
            package_procedures.p, package_procedures.size()));
        packages.emplace(lowered, package);
        items.push_back(al, &lowered->base);
    }
    auto *module = al.make_new<AST::Module_t>(x);
    module->m_items = items.p;
    module->n_items = items.size();
    module->m_contains = contains.p;
    module->n_contains = contains.size();
    return module;
}

} // namespace LCompilers::LFortran

#endif

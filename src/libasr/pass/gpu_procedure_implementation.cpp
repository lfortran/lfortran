#include <filesystem>
#include <set>
#include <string>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/modfile.h>
#include <libasr/serialization.h>
#include <libasr/string_utils.h>
#include <libasr/pass/gpu_procedure_implementation.h>
#include <libasr/pass/replace_implied_do_loops.h>

namespace LCompilers {

ASR::Function_t* GpuProcedureImplementations::implementation(
        ASR::Function_t *fn) {
    if (!fn) return nullptr;
    if (ASRUtils::get_FunctionType(fn)->m_deftype
            != ASR::deftypeType::Interface) {
        return fn;
    }
    auto cached = implementations.find(fn);
    if (cached != implementations.end()) return cached->second;
    ASR::Function_t *impl = find(fn);
    implementations[fn] = impl;
    return impl;
}

ASR::Function_t* GpuProcedureImplementations::find(
        ASR::Function_t *fn) {
    std::string pname = fn->m_name;
    for (auto &tu_item : tu.m_symtab->get_scope()) {
        if (!ASR::is_a<ASR::Module_t>(*tu_item.second)) continue;
        ASR::Module_t *mod = ASR::down_cast<ASR::Module_t>(
            tu_item.second);
        ASR::symbol_t *impl_sym = mod->m_symtab->get_symbol(pname);
        if (!impl_sym || !ASR::is_a<ASR::Function_t>(*impl_sym)) continue;
        ASR::Function_t *impl_func = ASR::down_cast<ASR::Function_t>(
            impl_sym);
        if (ASR::down_cast<ASR::FunctionType_t>(
                impl_func->m_function_signature)->m_deftype ==
                ASR::deftypeType::Implementation) {
            return impl_func;
        }
    }
    // Not in this translation unit: load the submodule from disk.
    SymbolTable *parent_st = fn->m_symtab->parent;
    if (!parent_st || !parent_st->asr_owner ||
            parent_st->asr_owner->type != ASR::asrType::symbol) {
        return fn;
    }
    ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
        parent_st->asr_owner);
    if (!ASR::is_a<ASR::Module_t>(*owner)) return fn;
    std::string smod_prefix = std::string(
        ASR::down_cast<ASR::Module_t>(owner)->m_name) + "@";
    std::vector<std::filesystem::path> mod_dirs;
    mod_dirs.push_back(pass_options.runtime_library_dir);
    mod_dirs.push_back(pass_options.mod_files_dir);
    mod_dirs.insert(mod_dirs.end(), pass_options.include_dirs.begin(),
        pass_options.include_dirs.end());
    for (auto &dir : mod_dirs) {
        if (dir.empty()) dir = ".";
        if (!std::filesystem::is_directory(dir)) continue;
        for (auto &file : std::filesystem::directory_iterator(dir)) {
            std::string fname = file.path().filename().string();
            if (!startswith(fname, smod_prefix) ||
                    !endswith(fname, ".smod")) continue;
            std::string content;
            if (!read_file(file.path().string(), content) ||
                    content.empty()) continue;
            LocationManager lm_tmp;
            auto res = load_modfile(al, content, false, *tu.m_symtab,
                lm_tmp);
            if (!res.ok) continue;
            load_submodule_deps(*res.result);
            fix_external_symbols(*res.result, *tu.m_symtab);
            lower_loaded_implied_do_loops(*res.result);
            ASR::Module_t *submod = ASRUtils::extract_module(
                *res.result);
            ASR::symbol_t *impl_sym = submod->m_symtab->get_symbol(
                pname);
            if (!impl_sym || !ASR::is_a<ASR::Function_t>(*impl_sym)) {
                continue;
            }
            ASR::Function_t *impl_func = ASR::down_cast<ASR::Function_t>(
                impl_sym);
            if (ASR::down_cast<ASR::FunctionType_t>(
                    impl_func->m_function_signature)->m_deftype !=
                    ASR::deftypeType::Implementation) continue;
            read_from_disk.insert(impl_func);
            return impl_func;
        }
    }
    return fn;
}

void GpuProcedureImplementations::load_submodule_deps(ASR::TranslationUnit_t &sub_tu) {
    std::vector<std::string> pending =
        ASRUtils::determine_module_dependencies(sub_tu);
    std::set<std::string> seen;
    bool loaded_any = false;

    while (!pending.empty()) {
        std::string dep_name = pending.back();
        pending.pop_back();
        if (seen.count(dep_name)) continue;
        seen.insert(dep_name);
        if (tu.m_symtab->get_symbol(dep_name) != nullptr)
            continue;
        if (sub_tu.m_symtab->get_symbol(dep_name) != nullptr)
            continue;
        bool is_intrinsic =
            startswith(dep_name, "lfortran_intrinsic");
        LocationManager lm_dep;
        auto dep_res = ASRUtils::find_and_load_module(
            al, dep_name, *tu.m_symtab, is_intrinsic,
            pass_options, lm_dep);
        if (!dep_res.ok && !is_intrinsic) {
            if (dep_name == "iso_c_binding" ||
                    dep_name == "iso_fortran_env") {
                LocationManager lm_dep2;
                auto dep_res2 =
                    ASRUtils::find_and_load_module(
                        al,
                        "lfortran_intrinsic_" + dep_name,
                        *tu.m_symtab, true, pass_options,
                        lm_dep2);
                if (dep_res2.ok) {
                    ASR::Module_t *dep_mod =
                        ASRUtils::extract_module(
                            *dep_res2.result);
                    tu.m_symtab->add_symbol(dep_name,
                        (ASR::symbol_t*)dep_mod);
                    dep_mod->m_symtab->parent =
                        tu.m_symtab;
                    dep_mod->m_loaded_from_mod = true;
                    loaded_any = true;
                    for (size_t i = 0;
                            i < dep_mod->n_dependencies; i++) {
                        pending.push_back(
                            dep_mod->m_dependencies[i]);
                    }
                }
                continue;
            }
        }
        if (!dep_res.ok) continue;
        ASR::Module_t *dep_mod =
            ASRUtils::extract_module(*dep_res.result);
        tu.m_symtab->add_symbol(dep_name,
            (ASR::symbol_t*)dep_mod);
        dep_mod->m_symtab->parent = tu.m_symtab;
        dep_mod->m_loaded_from_mod = true;
        // As `load_module` does: with separate compilation the module's own
        // object file defines its procedures and variables, so this
        // translation unit must only declare them.
        if (pass_options.separate_compilation &&
                !startswith(dep_mod->m_name, "lfortran_intrinsic")) {
            dep_mod->m_symtab->mark_all_variables_external(al);
        }
        loaded_any = true;
        for (size_t i = 0; i < dep_mod->n_dependencies; i++) {
            pending.push_back(dep_mod->m_dependencies[i]);
        }
    }

    if (loaded_any) {
        fix_external_symbols(tu, *tu.m_symtab);
    }
}

void GpuProcedureImplementations::lower_loaded_implied_do_loops(
        ASR::TranslationUnit_t &sub_tu) {
    pass_replace_implied_do_loops(al, sub_tu, pass_options);
}

} // namespace LCompilers

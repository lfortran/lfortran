#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/codegen/asr_to_metal.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/codegen/c_utils.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/string_utils.h>
#include <libasr/pass/intrinsic_functions.h>
#include <libasr/pass/intrinsic_function_registry.h>

#include <sstream>
#include <map>
#include <set>
#include <vector>

namespace LCompilers {

class GpuFuncCallCollector : public ASR::BaseWalkVisitor<GpuFuncCallCollector> {
public:
    std::set<std::string> called;
    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        called.insert(ASRUtils::symbol_name(x.m_name));
        ASR::BaseWalkVisitor<GpuFuncCallCollector>::visit_FunctionCall(x);
    }
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        called.insert(ASRUtils::symbol_name(x.m_name));
        ASR::BaseWalkVisitor<GpuFuncCallCollector>::visit_SubroutineCall(x);
    }
};

class ASRToMetalVisitor
{
public:
    std::stringstream src;
    int indent_level;
    CompilerOptions &co;

    // VLA info collected during kernel signature generation,
    // used by the BlockCall handler to emit device pointer offsets.
    std::vector<GpuVlaWorkspace> current_vla_infos;

    // Maps array parameter names to their synthesized size parameter
    // names within the current function being emitted. Populated by
    // emit_function_def for DescriptorArray parameters and consumed
    // by visit_expr when emitting ArraySize.
    std::map<std::string, std::string> func_array_size_params;

    // Maps "struct_var.member" to the device-pointer parameter name
    // for allocatable array members passed alongside the struct.
    std::map<std::string, std::string> func_array_data_params;

    // Maps "struct_arr.member" to the offsets-buffer parameter name
    // for allocatable array members of array-of-struct kernel arguments.
    std::map<std::string, std::string> struct_array_offset_params;

    // Maps "struct_arr.member" to the sizes-buffer parameter name
    // for allocatable array members of array-of-struct kernel arguments.
    std::map<std::string, std::string> struct_array_sizes_params;

    // Tracks local struct variables that were assigned from an element
    // of an array-of-struct. Maps local_var_name -> (array_name, index_expr_string).
    // Used by emit_struct_member_data_ptrs/sizes to offset into flat buffers.
    std::map<std::string, std::pair<std::string, std::string>> struct_from_array_elem;

    // Tracks allocatable out parameters emitted as thread pointers
    // in the current inline function. Used by the Assignment handler
    // to dereference the pointer when assigning to these params.
    std::set<std::string> alloc_pointer_params;

    // True when emitting an inline function body (not a kernel body).
    // Used to suppress array buffer indexing logic that only applies
    // to kernel-level device buffer parameters.
    bool in_inline_function = false;

    ASRToMetalVisitor(CompilerOptions &co_) : indent_level(0), co(co_) {}

    std::string get_indent() {
        return std::string(indent_level * 4, ' ');
    }

    std::string metal_type(ASR::ttype_t *type) {
        switch (type->type) {
            case ASR::ttypeType::Integer: {
                int kind = ASR::down_cast<ASR::Integer_t>(type)->m_kind;
                if (kind == 4) return "int";
                if (kind == 8) return "long";
                return "int";
            }
            case ASR::ttypeType::Real: {
                // Metal does not support double precision; use float for all
                return "float";
            }
            case ASR::ttypeType::Logical: {
                // Use int to match LLVM's i32 representation for Logical(4)
                int kind = ASR::down_cast<ASR::Logical_t>(type)->m_kind;
                if (kind == 1) return "char";
                if (kind == 2) return "short";
                if (kind == 4) return "int";
                return "int";
            }
            case ASR::ttypeType::Array: {
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
                return metal_type(arr->m_type);
            }
            case ASR::ttypeType::Allocatable: {
                ASR::Allocatable_t *alloc = ASR::down_cast<ASR::Allocatable_t>(type);
                return metal_type(alloc->m_type);
            }
            case ASR::ttypeType::StructType: {
                return "/* unsupported struct type */";
            }
            default:
                return "float";
        }
    }

    bool is_array_type(ASR::ttype_t *type) {
        return type->type == ASR::ttypeType::Array;
    }

    // Emit a local variable declaration, including array dimensions.
    // For scalars: `float x;`
    // For arrays:  `float x[3];` or `float x[n];` (VLA)
    void emit_local_var_decl(ASR::Variable_t *var) {
        ASR::ttype_t *type = var->m_type;
        if (ASR::is_a<ASR::Array_t>(*type)) {
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
            std::string elem_type;
            if (is_struct_type(arr->m_type)) {
                elem_type = get_struct_name(var);
            } else {
                elem_type = metal_type(arr->m_type);
            }
            src << get_indent() << elem_type << " "
                << var->m_name;
            for (size_t d = 0; d < arr->n_dims; d++) {
                src << "[";
                if (arr->m_dims[d].m_length) {
                    visit_expr(arr->m_dims[d].m_length);
                }
                src << "]";
            }
            src << ";\n";
        } else if (is_struct_type(type)) {
            src << get_indent() << get_struct_name(var) << " "
                << var->m_name << ";\n";
        } else {
            src << get_indent() << metal_type(type) << " "
                << var->m_name << ";\n";
        }
    }

    // Get total number of elements for a multi-dimensional array
    int64_t get_total_elements(ASR::ttype_t *type) {
        if (type->type != ASR::ttypeType::Array) return 1;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
        int64_t total = 1;
        for (size_t i = 0; i < arr->n_dims; i++) {
            if (arr->m_dims[i].m_length &&
                ASR::is_a<ASR::IntegerConstant_t>(*arr->m_dims[i].m_length)) {
                total *= ASR::down_cast<ASR::IntegerConstant_t>(
                    arr->m_dims[i].m_length)->m_n;
            }
        }
        return total;
    }

    // Emit the total size of an array expression (product of dimensions).
    // Used at function call sites to pass array sizes for DescriptorArray
    // parameters that are represented as device pointers in Metal.
    void emit_array_size_expr(ASR::expr_t *expr) {
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
            expr = ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr)->m_arg;
        }
        ASR::ttype_t *type = ASRUtils::expr_type(expr);
        if (ASR::is_a<ASR::Array_t>(*type)) {
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
            bool first_d = true;
            src << "(";
            for (size_t d = 0; d < arr->n_dims; d++) {
                if (arr->m_dims[d].m_length) {
                    if (!first_d) src << " * ";
                    first_d = false;
                    visit_expr(arr->m_dims[d].m_length);
                }
            }
            if (first_d) src << "0";
            src << ")";
        } else if (ASR::is_a<ASR::Var_t>(*expr)) {
            std::string name = ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(expr)->m_v);
            auto it = func_array_size_params.find(name);
            if (it != func_array_size_params.end()) {
                src << it->second;
            } else {
                src << "/* unknown array size */";
            }
        } else {
            src << "/* unknown array size */";
        }
    }

    // Emit extra size arguments for allocatable array members of a
    // struct-typed function call argument. The sizes are looked up
    // from the kernel's __size_<struct>_<member> scalar parameters,
    // or from __sizes_<arr>_<member>[idx] for array-of-struct elements.
    void emit_struct_member_sizes(ASR::expr_t *expr) {
        ASR::Variable_t *var = nullptr;
        std::string var_name;
        if (ASR::is_a<ASR::Var_t>(*expr)) {
            ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(expr)->m_v;
            sym = ASRUtils::symbol_get_past_external(sym);
            if (ASR::is_a<ASR::Variable_t>(*sym)) {
                var = ASR::down_cast<ASR::Variable_t>(sym);
                var_name = var->m_name;
            }
        }
        if (!var || !var->m_type_declaration) return;
        ASR::symbol_t *st_sym =
            ASRUtils::symbol_get_past_external(var->m_type_declaration);
        if (!ASR::is_a<ASR::Struct_t>(*st_sym)) return;
        ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(st_sym);

        // Check if this local struct came from an array-of-struct element
        auto arr_it = struct_from_array_elem.find(var_name);

        for (size_t m = 0; m < st->n_members; m++) {
            ASR::symbol_t *mem =
                st->m_symtab->get_symbol(st->m_members[m]);
            if (!mem || !ASR::is_a<ASR::Variable_t>(*mem)) continue;
            ASR::Variable_t *mv =
                ASR::down_cast<ASR::Variable_t>(mem);
            if (!ASRUtils::is_allocatable(mv->m_type)) continue;
            ASR::ttype_t *inner =
                ASRUtils::type_get_past_allocatable(mv->m_type);
            if (!ASR::is_a<ASR::Array_t>(*inner)) continue;

            // If this struct came from an array-of-struct element,
            // use the per-element sizes buffer
            if (arr_it != struct_from_array_elem.end()) {
                std::string arr_name = arr_it->second.first;
                std::string idx_str = arr_it->second.second;
                std::string key = arr_name + "." + st->m_members[m];
                auto sit = struct_array_sizes_params.find(key);
                if (sit != struct_array_sizes_params.end()) {
                    src << ", " << sit->second << "[" << idx_str << "]";
                    continue;
                }
            }

            // Try direct lookup first (var_name.member)
            std::string key = var_name + "." + st->m_members[m];
            auto it = func_array_size_params.find(key);
            if (it != func_array_size_params.end()) {
                src << ", " << it->second;
            } else {
                // Fallback: find any entry matching ".<member>" suffix
                // (for local struct copies that originated from a
                // kernel parameter)
                std::string suffix = std::string(".")
                    + st->m_members[m];
                bool found = false;
                for (auto &entry : func_array_size_params) {
                    if (entry.first.size() >= suffix.size() &&
                            entry.first.compare(
                                entry.first.size() - suffix.size(),
                                suffix.size(), suffix) == 0) {
                        src << ", " << entry.second;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    src << ", __size_" << var_name << "_"
                        << st->m_members[m];
                }
            }
        }
    }

    void emit_struct_member_data_ptrs(ASR::expr_t *expr) {
        ASR::Variable_t *var = nullptr;
        std::string var_name;
        if (ASR::is_a<ASR::Var_t>(*expr)) {
            ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(expr)->m_v;
            sym = ASRUtils::symbol_get_past_external(sym);
            if (ASR::is_a<ASR::Variable_t>(*sym)) {
                var = ASR::down_cast<ASR::Variable_t>(sym);
                var_name = var->m_name;
            }
        }
        if (!var || !var->m_type_declaration) return;
        ASR::symbol_t *st_sym =
            ASRUtils::symbol_get_past_external(var->m_type_declaration);
        if (!ASR::is_a<ASR::Struct_t>(*st_sym)) return;
        ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(st_sym);

        // Check if this local struct came from an array-of-struct element
        auto arr_it = struct_from_array_elem.find(var_name);

        for (size_t m = 0; m < st->n_members; m++) {
            ASR::symbol_t *mem =
                st->m_symtab->get_symbol(st->m_members[m]);
            if (!mem || !ASR::is_a<ASR::Variable_t>(*mem)) continue;
            ASR::Variable_t *mv =
                ASR::down_cast<ASR::Variable_t>(mem);
            if (!ASRUtils::is_allocatable(mv->m_type)) continue;
            ASR::ttype_t *inner =
                ASRUtils::type_get_past_allocatable(mv->m_type);
            if (!ASR::is_a<ASR::Array_t>(*inner)) continue;

            // If this struct came from an array-of-struct element,
            // offset the data pointer using the offsets buffer
            if (arr_it != struct_from_array_elem.end()) {
                std::string arr_name = arr_it->second.first;
                std::string idx_str = arr_it->second.second;
                std::string key = arr_name + "." + st->m_members[m];
                auto dit = func_array_data_params.find(key);
                auto oit = struct_array_offset_params.find(key);
                if (dit != func_array_data_params.end() &&
                        oit != struct_array_offset_params.end()) {
                    src << ", " << dit->second << " + "
                        << oit->second << "[" << idx_str << "]";
                    continue;
                }
            }

            std::string key = var_name + "." + st->m_members[m];
            auto it = func_array_data_params.find(key);
            if (it != func_array_data_params.end()) {
                src << ", " << it->second;
            } else {
                std::string suffix = std::string(".")
                    + st->m_members[m];
                bool found = false;
                for (auto &entry : func_array_data_params) {
                    if (entry.first.size() >= suffix.size() &&
                            entry.first.compare(
                                entry.first.size() - suffix.size(),
                                suffix.size(), suffix) == 0) {
                        src << ", " << entry.second;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    src << ", __data_" << var_name << "_"
                        << st->m_members[m];
                }
            }
        }
    }


    void visit_TranslationUnit(const ASR::TranslationUnit_t &tu) {
        src << "#include <metal_stdlib>\n";
        src << "using namespace metal;\n\n";

        // Collect and emit struct definitions from all kernels once,
        // before any kernel code, to avoid redefinition errors when
        // multiple kernels reference the same struct type.
        std::set<std::string> emitted_structs;
        std::vector<ASR::Struct_t*> ordered_structs;
        for (auto &item : tu.m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::GpuKernelFunction_t>(*item.second)) continue;
            ASR::GpuKernelFunction_t &kf =
                *ASR::down_cast<ASR::GpuKernelFunction_t>(item.second);
            for (auto &kitem : kf.m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Struct_t>(*kitem.second)) {
                    collect_structs_ordered(
                        ASR::down_cast<ASR::Struct_t>(kitem.second),
                        kf.m_symtab, emitted_structs, ordered_structs);
                }
            }
        }
        for (ASR::Struct_t *st : ordered_structs) {
            emit_struct_def(st);
        }

        for (auto &item : tu.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::GpuKernelFunction_t>(*item.second)) {
                visit_GpuKernelFunction(
                    *ASR::down_cast<ASR::GpuKernelFunction_t>(item.second));
            }
        }
    }

    // Get the Metal struct name for a struct-typed variable
    std::string get_struct_name(ASR::Variable_t *var) {
        if (var->m_type_declaration) {
            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                var->m_type_declaration);
            if (ASR::is_a<ASR::Struct_t>(*s)) {
                return ASR::down_cast<ASR::Struct_t>(s)->m_name;
            }
        }
        return "unknown_struct";
    }

    bool is_struct_type(ASR::ttype_t *type) {
        return ASR::is_a<ASR::StructType_t>(
            *ASRUtils::extract_type(type));
    }

    // Get the Struct_t for a variable that is either a struct or array-of-struct
    ASR::Struct_t* get_struct_decl(ASR::Variable_t *var) {
        if (!var->m_type_declaration) return nullptr;
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
            var->m_type_declaration);
        if (ASR::is_a<ASR::Struct_t>(*s)) {
            return ASR::down_cast<ASR::Struct_t>(s);
        }
        return nullptr;
    }

    // Check if a Struct_t has any allocatable array members
    bool struct_has_allocatable_members(ASR::Struct_t *st) {
        for (size_t m = 0; m < st->n_members; m++) {
            ASR::symbol_t *mem = st->m_symtab->get_symbol(st->m_members[m]);
            if (!mem || !ASR::is_a<ASR::Variable_t>(*mem)) continue;
            ASR::Variable_t *mv = ASR::down_cast<ASR::Variable_t>(mem);
            if (!ASRUtils::is_allocatable(mv->m_type)) continue;
            ASR::ttype_t *inner = ASRUtils::type_get_past_allocatable(mv->m_type);
            if (ASR::is_a<ASR::Array_t>(*inner)) return true;
        }
        return false;
    }

    // Emit a Metal struct definition for a Struct symbol
    void emit_struct_def(ASR::Struct_t *st) {
        src << "struct " << st->m_name << " {\n";
        for (size_t i = 0; i < st->n_members; i++) {
            ASR::symbol_t *mem = st->m_symtab->get_symbol(st->m_members[i]);
            if (mem && ASR::is_a<ASR::Variable_t>(*mem)) {
                ASR::Variable_t *mv = ASR::down_cast<ASR::Variable_t>(mem);
                if (ASRUtils::is_allocatable(mv->m_type)) {
                    // Allocatable members are passed as separate device
                    // buffers. Emit a long placeholder matching the host
                    // pointer size to keep the struct layout aligned.
                    src << "    long " << mv->m_name << ";\n";
                } else if (is_struct_type(mv->m_type)) {
                    src << "    " << get_struct_name(mv) << " "
                        << mv->m_name << ";\n";
                } else if (is_array_type(mv->m_type)) {
                    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(mv->m_type);
                    src << "    " << metal_type(arr->m_type) << " "
                        << mv->m_name;
                    int64_t total = get_total_elements(mv->m_type);
                    src << "[" << total << "]";
                    src << ";\n";
                } else {
                    src << "    " << metal_type(mv->m_type) << " "
                        << mv->m_name << ";\n";
                }
            }
        }
        src << "};\n\n";
    }

    // Collect struct definitions in dependency order (nested structs first)
    void collect_structs_ordered(ASR::Struct_t *st, SymbolTable *scope,
            std::set<std::string> &emitted,
            std::vector<ASR::Struct_t*> &ordered) {
        std::string name = st->m_name;
        if (emitted.count(name)) return;
        // Emit dependencies first
        for (size_t i = 0; i < st->n_members; i++) {
            ASR::symbol_t *mem = st->m_symtab->get_symbol(st->m_members[i]);
            if (mem && ASR::is_a<ASR::Variable_t>(*mem)) {
                ASR::Variable_t *mv = ASR::down_cast<ASR::Variable_t>(mem);
                if (is_struct_type(mv->m_type) && mv->m_type_declaration) {
                    ASR::symbol_t *inner = ASRUtils::symbol_get_past_external(
                        mv->m_type_declaration);
                    if (ASR::is_a<ASR::Struct_t>(*inner)) {
                        collect_structs_ordered(
                            ASR::down_cast<ASR::Struct_t>(inner),
                            scope, emitted, ordered);
                    }
                }
            }
        }
        emitted.insert(name);
        ordered.push_back(st);
    }

    // Resolve a FunctionCall symbol to the actual Function ASR node
    ASR::Function_t* resolve_function(ASR::symbol_t *sym) {
        sym = ASRUtils::symbol_get_past_external(sym);
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            return ASR::down_cast<ASR::Function_t>(sym);
        }
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*sym)) {
            ASR::StructMethodDeclaration_t *smd =
                ASR::down_cast<ASR::StructMethodDeclaration_t>(sym);
            return resolve_function(smd->m_proc);
        }
        return nullptr;
    }

    // Emit a Fortran function as a Metal inline function
    void emit_function_def(ASR::Function_t *fn, const std::string &metal_name) {
        func_array_size_params.clear();
        func_array_data_params.clear();
        ASR::FunctionType_t *ftype = ASR::down_cast<ASR::FunctionType_t>(
            fn->m_function_signature);
        std::string ret_type = "void";
        if (fn->m_return_var) {
            ret_type = metal_type(ASRUtils::expr_type(fn->m_return_var));
        }
        src << "inline " << ret_type << " " << metal_name << "(";
        bool first = true;
        for (size_t i = 0; i < fn->n_args; i++) {
            ASR::Variable_t *arg = ASR::down_cast<ASR::Variable_t>(
                ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
            // Struct-typed argument
            if (ftype->m_arg_types[i] &&
                ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(ftype->m_arg_types[i]))) {
                if (!first) src << ", ";
                first = false;
                // Out/InOut struct args are passed by reference
                // using device address space since they typically
                // originate from device buffer arrays in kernels
                if (arg->m_intent == ASR::intentType::Out ||
                    arg->m_intent == ASR::intentType::InOut) {
                    src << "device " << get_struct_name(arg) << "& "
                        << arg->m_name;
                } else {
                    src << get_struct_name(arg) << " " << arg->m_name;
                }
                // Add data pointer and size parameters for allocatable array members
                if (arg->m_type_declaration) {
                    ASR::symbol_t *st_sym =
                        ASRUtils::symbol_get_past_external(
                            arg->m_type_declaration);
                    if (ASR::is_a<ASR::Struct_t>(*st_sym)) {
                        ASR::Struct_t *st =
                            ASR::down_cast<ASR::Struct_t>(st_sym);
                        for (size_t m = 0; m < st->n_members; m++) {
                            ASR::symbol_t *mem =
                                st->m_symtab->get_symbol(
                                    st->m_members[m]);
                            if (!mem ||
                                !ASR::is_a<ASR::Variable_t>(*mem))
                                continue;
                            ASR::Variable_t *mv =
                                ASR::down_cast<ASR::Variable_t>(mem);
                            if (!ASRUtils::is_allocatable(mv->m_type))
                                continue;
                            ASR::ttype_t *inner =
                                ASRUtils::type_get_past_allocatable(
                                    mv->m_type);
                            if (!ASR::is_a<ASR::Array_t>(*inner))
                                continue;
                            std::string key =
                                std::string(arg->m_name) + "."
                                + st->m_members[m];
                            ASR::Array_t *mem_arr =
                                ASR::down_cast<ASR::Array_t>(inner);
                            std::string data_name =
                                "__data_" + std::string(arg->m_name)
                                + "_" + st->m_members[m];
                            std::string elem_type_str;
                            if (is_struct_type(mem_arr->m_type)) {
                                elem_type_str = get_struct_name(mv);
                            } else {
                                elem_type_str =
                                    metal_type(mem_arr->m_type);
                            }
                            src << ", device "
                                << elem_type_str
                                << "* " << data_name;
                            func_array_data_params[key] = data_name;
                            std::string size_name =
                                "__size_" + std::string(arg->m_name)
                                + "_" + st->m_members[m];
                            src << ", int " << size_name;
                            func_array_size_params[key] = size_name;
                        }
                    }
                }
                continue;
            }
            if (!first) src << ", ";
            first = false;
            if (is_array_type(arg->m_type)) {
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(arg->m_type);
                src << "device " << metal_type(arr->m_type) << "* " << arg->m_name;
                std::string size_name = std::string("__size_") + arg->m_name;
                src << ", int " << size_name;
                func_array_size_params[std::string(arg->m_name)] = size_name;
            } else if (ASRUtils::is_allocatable(arg->m_type)) {
                // Allocatable out parameter (from subroutine_from_function):
                // pass as thread-space pointer on Metal so both whole-array
                // assignment (*r = val) and element access (r[i] = val) work.
                src << "thread " << metal_type(arg->m_type) << "* "
                    << arg->m_name;
                alloc_pointer_params.insert(std::string(arg->m_name));
            } else {
                src << metal_type(arg->m_type) << " " << arg->m_name;
            }
        }
        src << ") {\n";
        indent_level++;
        // Declare return variable if present
        if (fn->m_return_var) {
            ASR::Variable_t *rv = ASR::down_cast<ASR::Variable_t>(
                ASR::down_cast<ASR::Var_t>(fn->m_return_var)->m_v);
            src << get_indent() << ret_type << " " << rv->m_name << ";\n";
        }
        // Declare Parameter (constant) variables from the function scope
        for (auto &item : fn->m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                item.second);
            if (var->m_storage != ASR::storage_typeType::Parameter) continue;
            if (!var->m_value) continue;
            src << get_indent() << "const " << metal_type(var->m_type)
                << " " << var->m_name << " = ";
            visit_expr(var->m_value);
            src << ";\n";
        }
        // Declare local variables (non-argument, non-return, non-parameter)
        {
            std::set<std::string> arg_names;
            for (size_t i = 0; i < fn->n_args; i++) {
                ASR::Variable_t *a = ASR::down_cast<ASR::Variable_t>(
                    ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
                arg_names.insert(std::string(a->m_name));
            }
            std::string ret_name;
            if (fn->m_return_var) {
                ASR::Variable_t *rv = ASR::down_cast<ASR::Variable_t>(
                    ASR::down_cast<ASR::Var_t>(fn->m_return_var)->m_v);
                ret_name = rv->m_name;
            }
            for (auto &item : fn->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                    item.second);
                if (var->m_storage == ASR::storage_typeType::Parameter)
                    continue;
                if (arg_names.count(std::string(var->m_name))) continue;
                if (std::string(var->m_name) == ret_name) continue;
                emit_local_var_decl(var);
            }
        }
        in_inline_function = true;
        for (size_t i = 0; i < fn->n_body; i++) {
            visit_stmt(fn->m_body[i]);
        }
        in_inline_function = false;
        if (fn->m_return_var) {
            ASR::Variable_t *rv = ASR::down_cast<ASR::Variable_t>(
                ASR::down_cast<ASR::Var_t>(fn->m_return_var)->m_v);
            src << get_indent() << "return " << rv->m_name << ";\n";
        }
        indent_level--;
        src << "}\n\n";
        func_array_size_params.clear();
        func_array_data_params.clear();
        alloc_pointer_params.clear();
    }

    void visit_GpuKernelFunction(const ASR::GpuKernelFunction_t &x) {
        std::string name(x.m_name);

        // Emit inline function definitions for type-bound procedures
        // referenced in this kernel
        std::set<std::string> emitted_funcs;
        for (auto &item : x.m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) continue;
            ASR::symbol_t *resolved = ASRUtils::symbol_get_past_external(
                item.second);
            ASR::Function_t *fn = resolve_function(resolved);
            if (!fn) continue;
            std::string fn_name(fn->m_name);
            if (emitted_funcs.count(fn_name)) continue;
            emitted_funcs.insert(fn_name);
            emit_function_def(fn, fn_name);
        }

        // Emit inline function definitions for internal functions
        // duplicated into the kernel scope by the gpu_offload pass.
        // Topologically sort so callees are emitted before callers.
        {
            std::map<std::string, ASR::Function_t*> kernel_funcs;
            for (auto &item : x.m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(
                    item.second);
                std::string fn_name(fn->m_name);
                if (emitted_funcs.count(fn_name)) continue;
                kernel_funcs[fn_name] = fn;
            }
            std::vector<std::string> sorted_funcs;
            std::set<std::string> visited, in_stack;
            std::function<void(const std::string&)> topo_sort =
                [&](const std::string &name) {
                if (visited.count(name)) return;
                visited.insert(name);
                in_stack.insert(name);
                auto it = kernel_funcs.find(name);
                if (it != kernel_funcs.end()) {
                    GpuFuncCallCollector call_collector;
                    ASR::Function_t *fn = it->second;
                    for (size_t i = 0; i < fn->n_body; i++) {
                        call_collector.visit_stmt(*fn->m_body[i]);
                    }
                    for (auto &callee : call_collector.called) {
                        if (kernel_funcs.count(callee) &&
                                !in_stack.count(callee)) {
                            topo_sort(callee);
                        }
                    }
                }
                in_stack.erase(name);
                sorted_funcs.push_back(name);
            };
            for (auto &[name, fn] : kernel_funcs) {
                topo_sort(name);
            }
            for (auto &fn_name : sorted_funcs) {
                emitted_funcs.insert(fn_name);
                emit_function_def(kernel_funcs[fn_name], fn_name);
            }
        }

        struct ArgInfo {
            std::string name;
            ASR::ttype_t *type;
            bool is_array;
            bool is_struct;
            std::string struct_name;
        };
        std::vector<ArgInfo> args;
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_args[i]);
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                ASRUtils::symbol_get_past_external(v->m_v));
            std::string arg_name(var->m_name);
            ASR::ttype_t *type = var->m_type;
            bool is_st = is_struct_type(type);
            // Get struct name for both struct-typed and array-of-struct
            // variables via m_type_declaration
            std::string sn = "";
            if (var->m_type_declaration) {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                    var->m_type_declaration);
                if (ASR::is_a<ASR::Struct_t>(*s)) {
                    sn = ASR::down_cast<ASR::Struct_t>(s)->m_name;
                }
            }
            args.push_back({arg_name, type, is_array_type(type), is_st, sn});
        }

        // Analyze blocks in the kernel body for VLAs (arrays with
        // non-constant dimensions) using the shared GPU utility.
        current_vla_infos = analyze_gpu_vla_workspaces(x);

        bool packed_mode = gpu_kernel_needs_buffer_packing(x);

        // Collect scalar args for packing into a single struct buffer
        struct ScalarArg {
            std::string name;
            std::string metal_type_str;
        };
        std::vector<ScalarArg> scalar_args;
        for (size_t i = 0; i < args.size(); i++) {
            if (!args[i].is_array && !args[i].is_struct) {
                scalar_args.push_back({args[i].name,
                    metal_type(args[i].type)});
            }
        }

        // In packed mode, collect info about arrays/structs that will
        // be packed into a single combined buffer
        struct PackedArrayInfo {
            std::string name;
            std::string elem_type;
            bool is_struct_ref;
            std::string struct_name;
            int64_t byte_size;
            int64_t offset;
        };
        std::vector<PackedArrayInfo> packed_arrays;

        if (packed_mode) {
            int64_t current_offset = 0;
            for (size_t i = 0; i < args.size(); i++) {
                if (!args[i].is_array && !args[i].is_struct) continue;

                std::string elem_type;
                int64_t byte_size = 0;
                if (args[i].is_array) {
                    elem_type = !args[i].struct_name.empty()
                        ? args[i].struct_name
                        : metal_type(args[i].type);
                    ASR::ttype_t *base_type =
                        ASRUtils::type_get_past_allocatable(args[i].type);
                    if (ASR::is_a<ASR::Array_t>(*base_type)) {
                        ASR::Array_t *arr =
                            ASR::down_cast<ASR::Array_t>(base_type);
                        int elem_size = 4;
                        if (ASR::is_a<ASR::Real_t>(*arr->m_type)) {
                            elem_size = ASR::down_cast<ASR::Real_t>(
                                arr->m_type)->m_kind;
                        } else if (ASR::is_a<ASR::Integer_t>(*arr->m_type)) {
                            elem_size = ASR::down_cast<ASR::Integer_t>(
                                arr->m_type)->m_kind;
                        }
                        int64_t total_elements = 1;
                        for (size_t d = 0; d < arr->n_dims; d++) {
                            if (arr->m_dims[d].m_length &&
                                ASR::is_a<ASR::IntegerConstant_t>(
                                    *arr->m_dims[d].m_length)) {
                                total_elements *=
                                    ASR::down_cast<ASR::IntegerConstant_t>(
                                        arr->m_dims[d].m_length)->m_n;
                            }
                        }
                        byte_size = total_elements * elem_size;
                    }
                }

                int align = PACKED_BUFFER_ALIGN;
                if (current_offset % align != 0) {
                    current_offset += align - (current_offset % align);
                }

                packed_arrays.push_back({
                    args[i].name, elem_type, args[i].is_struct,
                    args[i].struct_name, byte_size, current_offset
                });

                if (byte_size > 0) {
                    current_offset += byte_size;
                }
            }
        }

        // Emit scalar args struct definition (before kernel) if needed
        std::string scalar_struct_name = "__ScalarArgs_" + name;
        bool has_scalar_struct = !scalar_args.empty() ||
            (packed_mode && !packed_arrays.empty());

        if (has_scalar_struct) {
            src << "struct " << scalar_struct_name << " {\n";
            for (auto &sa : scalar_args) {
                src << "    " << sa.metal_type_str << " "
                    << sa.name << ";\n";
            }
            if (packed_mode) {
                for (auto &pa : packed_arrays) {
                    src << "    uint __offset_" << pa.name << ";\n";
                }
            }
            src << "};\n\n";
        }

        // Emit kernel signature
        src << "kernel void " << name << "(\n";

        int buffer_idx = 0;
        bool has_prev = false;

        if (packed_mode) {
            src << "    device char* __packed_arrays [[buffer("
                << buffer_idx++ << ")]]";
            has_prev = true;
        } else {
            for (size_t i = 0; i < args.size(); i++) {
                if (args[i].is_array) {
                    if (has_prev) src << ",\n";
                    src << "    ";
                    std::string elem_type = !args[i].struct_name.empty()
                        ? args[i].struct_name
                        : metal_type(args[i].type);
                    src << "device " << elem_type << "* "
                        << args[i].name << " [[buffer(" << buffer_idx++ << ")]]";
                    has_prev = true;
                    // For array-of-struct args, emit additional buffers
                    // for allocatable array members' data and offsets
                    if (!args[i].struct_name.empty()) {
                        ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(
                            x.m_args[i]);
                        ASR::Variable_t *var =
                            ASR::down_cast<ASR::Variable_t>(
                                ASRUtils::symbol_get_past_external(
                                    v->m_v));
                        ASR::Struct_t *st = get_struct_decl(var);
                        if (st) {
                            for (size_t m = 0; m < st->n_members; m++) {
                                ASR::symbol_t *mem =
                                    st->m_symtab->get_symbol(
                                        st->m_members[m]);
                                if (!mem ||
                                    !ASR::is_a<ASR::Variable_t>(*mem))
                                    continue;
                                ASR::Variable_t *mv =
                                    ASR::down_cast<ASR::Variable_t>(
                                        mem);
                                if (!ASRUtils::is_allocatable(mv->m_type))
                                    continue;
                                ASR::ttype_t *inner =
                                    ASRUtils::type_get_past_allocatable(
                                        mv->m_type);
                                if (!ASR::is_a<ASR::Array_t>(*inner))
                                    continue;
                                ASR::Array_t *mem_arr =
                                    ASR::down_cast<ASR::Array_t>(inner);
                                std::string et;
                                if (is_struct_type(mem_arr->m_type)) {
                                    et = get_struct_name(mv);
                                } else {
                                    et = metal_type(mem_arr->m_type);
                                }
                                std::string data_name =
                                    "__data_" + args[i].name + "_"
                                    + st->m_members[m];
                                src << ",\n    device " << et << "* "
                                    << data_name << " [[buffer("
                                    << buffer_idx++ << ")]]";
                                std::string off_name =
                                    "__offsets_" + args[i].name + "_"
                                    + st->m_members[m];
                                src << ",\n    device int* "
                                    << off_name << " [[buffer("
                                    << buffer_idx++ << ")]]";
                                std::string sizes_name =
                                    "__sizes_" + args[i].name + "_"
                                    + st->m_members[m];
                                src << ",\n    device int* "
                                    << sizes_name << " [[buffer("
                                    << buffer_idx++ << ")]]";
                            }
                        }
                    }
                } else if (args[i].is_struct) {
                    if (has_prev) src << ",\n";
                    src << "    ";
                    src << "device " << args[i].struct_name << "& "
                        << args[i].name << " [[buffer(" << buffer_idx++ << ")]]";
                    has_prev = true;
                }
            }
        }

        // Emit packed scalar struct as a single buffer
        if (has_scalar_struct) {
            if (has_prev) src << ",\n";
            src << "    constant " << scalar_struct_name
                << "& __scalar_args [[buffer(" << buffer_idx++ << ")]]";
            has_prev = true;
        }

        // Emit VLA workspace buffer parameters
        for (size_t v = 0; v < current_vla_infos.size(); v++) {
            LCOMPILERS_ASSERT(current_vla_infos[v].buffer_index == buffer_idx);
            ASR::Variable_t *vla_var = nullptr;
            // Look up the variable to get its Metal type string
            for (size_t bi = 0; bi < x.n_body; bi++) {
                if (!ASR::is_a<ASR::BlockCall_t>(*x.m_body[bi])) continue;
                ASR::BlockCall_t *bc = ASR::down_cast<ASR::BlockCall_t>(
                    x.m_body[bi]);
                if (!ASR::is_a<ASR::Block_t>(*bc->m_m)) continue;
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(bc->m_m);
                ASR::symbol_t *sym = block->m_symtab->resolve_symbol(
                    current_vla_infos[v].var_name);
                if (sym && ASR::is_a<ASR::Variable_t>(*sym)) {
                    vla_var = ASR::down_cast<ASR::Variable_t>(sym);
                    break;
                }
            }
            std::string elem_type_str = "float";
            if (vla_var && ASR::is_a<ASR::Array_t>(*vla_var->m_type)) {
                elem_type_str = metal_type(
                    ASR::down_cast<ASR::Array_t>(vla_var->m_type)->m_type);
            }

            if (has_prev) src << ",\n";
            src << "    device " << elem_type_str
                << "* __vla_" << current_vla_infos[v].var_name
                << " [[buffer(" << buffer_idx++ << ")]]";
            has_prev = true;
        }

        if (has_prev) src << ",\n";
        src << "    uint __thread_id [[thread_position_in_grid]]";

        src << ")\n{\n";
        indent_level++;

        // Unpack scalar args from the struct into local variables
        for (auto &sa : scalar_args) {
            src << get_indent() << sa.metal_type_str << " "
                << sa.name << " = __scalar_args." << sa.name << ";\n";
        }

        // In packed mode, unpack array pointers from the combined buffer
        if (packed_mode) {
            for (auto &pa : packed_arrays) {
                if (pa.is_struct_ref) {
                    src << get_indent() << "device " << pa.struct_name
                        << "& " << pa.name
                        << " = *(device " << pa.struct_name
                        << "*)(__packed_arrays + __scalar_args.__offset_"
                        << pa.name << ");\n";
                } else {
                    src << get_indent() << "device " << pa.elem_type
                        << "* " << pa.name
                        << " = (device " << pa.elem_type
                        << "*)(__packed_arrays + __scalar_args.__offset_"
                        << pa.name << ");\n";
                }
            }
        }

        // Populate func_array_size_params and func_array_data_params
        // for struct-typed kernel args so that emit_struct_member_sizes
        // and emit_struct_member_data_ptrs can find the right vars
        func_array_size_params.clear();
        func_array_data_params.clear();
        struct_array_offset_params.clear();
        struct_array_sizes_params.clear();
        struct_from_array_elem.clear();
        for (size_t i = 0; i < args.size(); i++) {
            if (args[i].is_struct && !args[i].is_array) {
                ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_args[i]);
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                    ASRUtils::symbol_get_past_external(v->m_v));
                if (var->m_type_declaration) {
                    ASR::symbol_t *st_sym =
                        ASRUtils::symbol_get_past_external(
                            var->m_type_declaration);
                    if (ASR::is_a<ASR::Struct_t>(*st_sym)) {
                        ASR::Struct_t *st =
                            ASR::down_cast<ASR::Struct_t>(st_sym);
                        for (size_t m = 0; m < st->n_members; m++) {
                            ASR::symbol_t *mem =
                                st->m_symtab->get_symbol(
                                    st->m_members[m]);
                            if (!mem ||
                                !ASR::is_a<ASR::Variable_t>(*mem))
                                continue;
                            ASR::Variable_t *mv =
                                ASR::down_cast<ASR::Variable_t>(mem);
                            if (!ASRUtils::is_allocatable(mv->m_type))
                                continue;
                            ASR::ttype_t *inner =
                                ASRUtils::type_get_past_allocatable(
                                    mv->m_type);
                            if (!ASR::is_a<ASR::Array_t>(*inner))
                                continue;
                            std::string key = args[i].name + "."
                                + st->m_members[m];
                            std::string size_name =
                                "__size_" + args[i].name + "_"
                                + st->m_members[m];
                            func_array_size_params[key] = size_name;
                            std::string data_name =
                                "__data_" + args[i].name + "_"
                                + st->m_members[m];
                            func_array_data_params[key] = data_name;
                        }
                    }
                }
            }
            // For array-of-struct args, register data and offset params
            // for allocatable members accessed via array indexing
            if (args[i].is_array && !args[i].struct_name.empty()) {
                ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_args[i]);
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                    ASRUtils::symbol_get_past_external(v->m_v));
                ASR::Struct_t *st = get_struct_decl(var);
                if (st) {
                    for (size_t m = 0; m < st->n_members; m++) {
                        ASR::symbol_t *mem =
                            st->m_symtab->get_symbol(st->m_members[m]);
                        if (!mem ||
                            !ASR::is_a<ASR::Variable_t>(*mem))
                            continue;
                        ASR::Variable_t *mv =
                            ASR::down_cast<ASR::Variable_t>(mem);
                        if (!ASRUtils::is_allocatable(mv->m_type))
                            continue;
                        ASR::ttype_t *inner =
                            ASRUtils::type_get_past_allocatable(
                                mv->m_type);
                        if (!ASR::is_a<ASR::Array_t>(*inner))
                            continue;
                        std::string key = args[i].name + "."
                            + st->m_members[m];
                        std::string data_name =
                            "__data_" + args[i].name + "_"
                            + st->m_members[m];
                        func_array_data_params[key] = data_name;
                        std::string off_name =
                            "__offsets_" + args[i].name + "_"
                            + st->m_members[m];
                        struct_array_offset_params[key] = off_name;
                        std::string sizes_name =
                            "__sizes_" + args[i].name + "_"
                            + st->m_members[m];
                        struct_array_sizes_params[key] = sizes_name;
                    }
                }
            }
        }

        // Declare local variables (non-argument variables in kernel scope)
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(item.second);
                bool is_arg = false;
                for (size_t i = 0; i < args.size(); i++) {
                    if (args[i].name == std::string(var->m_name)) {
                        is_arg = true;
                        break;
                    }
                }
                if (!is_arg) {
                    emit_local_var_decl(var);
                }
            }
        }

        for (size_t i = 0; i < x.n_body; i++) {
            visit_stmt(x.m_body[i]);
        }

        indent_level--;
        src << "}\n\n";
    }

    void visit_stmt(ASR::stmt_t *stmt) {
        switch (stmt->type) {
            case ASR::stmtType::Assignment: {
                ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(stmt);

                // Track struct-from-array-element assignments for
                // correct data pointer offsetting in function calls.
                // Pattern: local_struct = arr(i) where arr is array-of-struct
                if (ASR::is_a<ASR::Var_t>(*a->m_target) &&
                        ASR::is_a<ASR::ArrayItem_t>(*a->m_value)) {
                    ASR::ArrayItem_t *ai =
                        ASR::down_cast<ASR::ArrayItem_t>(a->m_value);
                    if (ASR::is_a<ASR::Var_t>(*ai->m_v)) {
                        std::string arr_name = ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(ai->m_v)->m_v);
                        std::string tgt_name = ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(
                                a->m_target)->m_v);
                        ASR::ttype_t *arr_type =
                            ASRUtils::expr_type(ai->m_v);
                        if (is_struct_type(arr_type) &&
                                is_array_type(arr_type) &&
                                ai->n_args == 1) {
                            // Capture the 0-based index expression
                            std::stringstream idx_ss;
                            ASR::expr_t *idx_expr =
                                ai->m_args[0].m_right
                                ? ai->m_args[0].m_right
                                : ai->m_args[0].m_left;
                            if (idx_expr) {
                                // Temporarily emit into a separate stream
                                std::stringstream saved;
                                saved.swap(src);
                                visit_expr(idx_expr);
                                idx_ss << "((int)(" << src.str()
                                       << ") - 1)";
                                saved.swap(src);
                            } else {
                                idx_ss << "0";
                            }
                            struct_from_array_elem[tgt_name] =
                                {arr_name, idx_ss.str()};
                        }
                    }
                }

                src << get_indent();
                // When assigning to an allocatable pointer param (e.g., r = val
                // where r is thread T*), dereference it: *r = val.
                // ArrayItem accesses (r[i] = val) are handled separately and
                // already work with pointers.
                bool deref_target = false;
                if (ASR::is_a<ASR::Var_t>(*a->m_target)) {
                    std::string tname = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(a->m_target)->m_v);
                    if (alloc_pointer_params.count(tname)) {
                        deref_target = true;
                    }
                }
                // When assigning a whole allocatable/array value to an
                // alloc_pointer_param (thread T*) or a device buffer,
                // the RHS may be a data pointer (device T*). In Metal,
                // copy the first element: *target = rhs[0] or
                // target[0] = rhs.
                ASR::ttype_t *rhs_type = ASRUtils::expr_type(a->m_value);
                bool rhs_is_array_or_alloc =
                    ASRUtils::is_allocatable(rhs_type)
                    || is_array_type(rhs_type)
                    || is_array_type(
                        ASRUtils::type_get_past_allocatable(rhs_type));
                ASR::ttype_t *tgt_type = ASRUtils::expr_type(a->m_target);
                bool target_is_array_buffer =
                    !deref_target
                    && !in_inline_function
                    && ASR::is_a<ASR::Var_t>(*a->m_target)
                    && is_array_type(tgt_type)
                    && !ASRUtils::is_allocatable(tgt_type);
                if (deref_target && rhs_is_array_or_alloc) {
                    src << "*";
                    visit_expr(a->m_target);
                    src << " = ";
                    visit_expr(a->m_value);
                    src << "[0];\n";
                } else if (target_is_array_buffer) {
                    visit_expr(a->m_target);
                    src << "[0] = ";
                    visit_expr(a->m_value);
                    src << ";\n";
                } else {
                    if (deref_target) {
                        src << "*";
                    }
                    visit_expr(a->m_target);
                    src << " = ";
                    visit_expr(a->m_value);
                    src << ";\n";
                }
                break;
            }
            case ASR::stmtType::If: {
                ASR::If_t *if_stmt = ASR::down_cast<ASR::If_t>(stmt);
                src << get_indent() << "if (";
                visit_expr(if_stmt->m_test);
                src << ") {\n";
                indent_level++;
                for (size_t i = 0; i < if_stmt->n_body; i++) {
                    visit_stmt(if_stmt->m_body[i]);
                }
                indent_level--;
                if (if_stmt->n_orelse > 0) {
                    src << get_indent() << "} else {\n";
                    indent_level++;
                    for (size_t i = 0; i < if_stmt->n_orelse; i++) {
                        visit_stmt(if_stmt->m_orelse[i]);
                    }
                    indent_level--;
                }
                src << get_indent() << "}\n";
                break;
            }
            case ASR::stmtType::Return: {
                src << get_indent() << "return;\n";
                break;
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *wl = ASR::down_cast<ASR::WhileLoop_t>(stmt);
                src << get_indent() << "while (";
                visit_expr(wl->m_test);
                src << ") {\n";
                indent_level++;
                for (size_t i = 0; i < wl->n_body; i++) {
                    visit_stmt(wl->m_body[i]);
                }
                indent_level--;
                src << get_indent() << "}\n";
                break;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
                src << get_indent() << "for (";
                visit_expr(dl->m_head.m_v);
                src << " = ";
                visit_expr(dl->m_head.m_start);
                src << "; ";
                visit_expr(dl->m_head.m_v);
                src << " <= ";
                visit_expr(dl->m_head.m_end);
                src << "; ";
                visit_expr(dl->m_head.m_v);
                if (dl->m_head.m_increment) {
                    src << " += ";
                    visit_expr(dl->m_head.m_increment);
                } else {
                    src << "++";
                }
                src << ") {\n";
                indent_level++;
                for (size_t i = 0; i < dl->n_body; i++) {
                    visit_stmt(dl->m_body[i]);
                }
                indent_level--;
                src << get_indent() << "}\n";
                break;
            }
            case ASR::stmtType::BlockCall: {
                ASR::BlockCall_t *bc = ASR::down_cast<ASR::BlockCall_t>(stmt);
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(bc->m_m);
                src << get_indent() << "{\n";
                indent_level++;
                for (auto &item : block->m_symtab->get_scope()) {
                    if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(
                        item.second);
                    // Check if this variable is a VLA backed by a workspace
                    // buffer (populated during kernel signature generation).
                    std::string vname(v->m_name);
                    auto vla_it = std::find_if(
                        current_vla_infos.begin(), current_vla_infos.end(),
                        [&](const GpuVlaWorkspace &ws) {
                            return ws.var_name == vname;
                        });
                    if (vla_it != current_vla_infos.end()) {
                        // Get the element type string from the variable
                        std::string elem_type_str = "float";
                        if (ASR::is_a<ASR::Array_t>(*v->m_type)) {
                            elem_type_str = metal_type(
                                ASR::down_cast<ASR::Array_t>(
                                    v->m_type)->m_type);
                        }
                        // Emit a device pointer into the per-thread slice
                        src << get_indent() << "device "
                            << elem_type_str << "* " << vname
                            << " = __vla_" << vname
                            << " + __thread_id * ";
                        if (vla_it->dims.size() == 1) {
                            visit_expr(vla_it->dims[0].dim_expr);
                        } else {
                            src << "(";
                            for (size_t d = 0;
                                    d < vla_it->dims.size(); d++) {
                                if (d > 0) src << " * ";
                                visit_expr(vla_it->dims[d].dim_expr);
                            }
                            src << ")";
                        }
                        src << ";\n";
                    } else {
                        emit_local_var_decl(v);
                    }
                }
                for (size_t i = 0; i < block->n_body; i++) {
                    visit_stmt(block->m_body[i]);
                }
                indent_level--;
                src << get_indent() << "}\n";
                break;
            }
            case ASR::stmtType::AssociateBlockCall: {
                ASR::AssociateBlockCall_t *abc =
                    ASR::down_cast<ASR::AssociateBlockCall_t>(stmt);
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(abc->m_m);
                src << get_indent() << "{\n";
                indent_level++;
                for (auto &item : ab->m_symtab->get_scope()) {
                    if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(
                        item.second);
                    emit_local_var_decl(v);
                }
                for (size_t i = 0; i < ab->n_body; i++) {
                    visit_stmt(ab->m_body[i]);
                }
                indent_level--;
                src << get_indent() << "}\n";
                break;
            }
            case ASR::stmtType::SubroutineCall: {
                ASR::SubroutineCall_t *sc =
                    ASR::down_cast<ASR::SubroutineCall_t>(stmt);
                ASR::Function_t *fn = resolve_function(sc->m_name);
                std::string call_name = fn
                    ? std::string(fn->m_name)
                    : std::string(ASRUtils::symbol_name(
                          ASRUtils::symbol_get_past_external(sc->m_name)));
                src << get_indent() << call_name << "(";
                bool first_arg = true;
                for (size_t i = 0; i < sc->n_args; i++) {
                    if (sc->m_args[i].m_value) {
                        if (!first_arg) src << ", ";
                        first_arg = false;
                        ASR::ttype_t *arg_type = ASRUtils::expr_type(
                            sc->m_args[i].m_value);
                        // Allocatable args correspond to pointer params
                        // in the target function; pass address of local var.
                        if (ASRUtils::is_allocatable(arg_type)) {
                            src << "&";
                        }
                        visit_expr(sc->m_args[i].m_value);
                        if (is_array_type(arg_type)
                                && !ASRUtils::is_allocatable(arg_type)) {
                            src << ", ";
                            emit_array_size_expr(sc->m_args[i].m_value);
                        } else if (is_struct_type(arg_type)) {
                            emit_struct_member_data_ptrs(
                                sc->m_args[i].m_value);
                            emit_struct_member_sizes(
                                sc->m_args[i].m_value);
                        }
                    }
                }
                src << ");\n";
                break;
            }
            case ASR::stmtType::ExplicitDeallocate: {
                // Skip deallocation on Metal GPU (memory managed by host)
                break;
            }
            default:
                break;
        }
    }

    void visit_expr(ASR::expr_t *expr) {
        switch (expr->type) {
            case ASR::exprType::Var: {
                ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(expr);
                src << ASRUtils::symbol_name(v->m_v);
                break;
            }
            case ASR::exprType::IntegerConstant: {
                ASR::IntegerConstant_t *c = ASR::down_cast<ASR::IntegerConstant_t>(expr);
                src << c->m_n;
                break;
            }
            case ASR::exprType::RealConstant: {
                ASR::RealConstant_t *c = ASR::down_cast<ASR::RealConstant_t>(expr);
                src << c->m_r;
                break;
            }
            case ASR::exprType::LogicalConstant: {
                ASR::LogicalConstant_t *c = ASR::down_cast<ASR::LogicalConstant_t>(expr);
                src << (c->m_value ? "1" : "0");
                break;
            }
            case ASR::exprType::IntegerBinOp: {
                ASR::IntegerBinOp_t *op = ASR::down_cast<ASR::IntegerBinOp_t>(expr);
                src << "(";
                visit_expr(op->m_left);
                src << " " << binop_str(op->m_op) << " ";
                visit_expr(op->m_right);
                src << ")";
                break;
            }
            case ASR::exprType::RealBinOp: {
                ASR::RealBinOp_t *op = ASR::down_cast<ASR::RealBinOp_t>(expr);
                if (op->m_op == ASR::binopType::Pow) {
                    src << "pow(";
                    visit_expr(op->m_left);
                    src << ", ";
                    visit_expr(op->m_right);
                    src << ")";
                } else {
                    src << "(";
                    visit_expr(op->m_left);
                    src << " " << binop_str(op->m_op) << " ";
                    visit_expr(op->m_right);
                    src << ")";
                }
                break;
            }
            case ASR::exprType::IntegerCompare: {
                ASR::IntegerCompare_t *op = ASR::down_cast<ASR::IntegerCompare_t>(expr);
                src << "(";
                visit_expr(op->m_left);
                src << " " << cmpop_str(op->m_op) << " ";
                visit_expr(op->m_right);
                src << ")";
                break;
            }
            case ASR::exprType::RealCompare: {
                ASR::RealCompare_t *op = ASR::down_cast<ASR::RealCompare_t>(expr);
                src << "(";
                visit_expr(op->m_left);
                src << " " << cmpop_str(op->m_op) << " ";
                visit_expr(op->m_right);
                src << ")";
                break;
            }
            case ASR::exprType::IntegerUnaryMinus: {
                ASR::IntegerUnaryMinus_t *u = ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr);
                src << "(-(";
                visit_expr(u->m_arg);
                src << "))";
                break;
            }
            case ASR::exprType::RealUnaryMinus: {
                ASR::RealUnaryMinus_t *u = ASR::down_cast<ASR::RealUnaryMinus_t>(expr);
                src << "(-(";
                visit_expr(u->m_arg);
                src << "))";
                break;
            }
            case ASR::exprType::LogicalBinOp: {
                ASR::LogicalBinOp_t *op = ASR::down_cast<ASR::LogicalBinOp_t>(expr);
                src << "((int)(";
                visit_expr(op->m_left);
                if (op->m_op == ASR::logicalbinopType::And) {
                    src << " && ";
                } else if (op->m_op == ASR::logicalbinopType::Or) {
                    src << " || ";
                } else {
                    src << " /* unsupported logical op */ ";
                }
                visit_expr(op->m_right);
                src << "))";
                break;
            }
            case ASR::exprType::LogicalNot: {
                ASR::LogicalNot_t *n = ASR::down_cast<ASR::LogicalNot_t>(expr);
                src << "((int)(!(";
                visit_expr(n->m_arg);
                src << ")))";
                break;
            }
            case ASR::exprType::LogicalCompare: {
                ASR::LogicalCompare_t *op = ASR::down_cast<ASR::LogicalCompare_t>(expr);
                src << "((int)(";
                visit_expr(op->m_left);
                src << " " << cmpop_str(op->m_op) << " ";
                visit_expr(op->m_right);
                src << "))";
                break;
            }
            case ASR::exprType::ArrayBound: {
                ASR::ArrayBound_t *ab = ASR::down_cast<ASR::ArrayBound_t>(expr);
                if (ab->m_value) {
                    visit_expr(ab->m_value);
                } else {
                    // For fixed-size arrays, compute from type info
                    ASR::ttype_t *arr_type = ASRUtils::type_get_past_allocatable_pointer(
                        ASRUtils::expr_type(ab->m_v));
                    if (ASR::is_a<ASR::Array_t>(*arr_type)) {
                        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(arr_type);
                        int dim_idx = 0;
                        if (ab->m_dim) {
                            if (ASR::is_a<ASR::IntegerConstant_t>(*ab->m_dim)) {
                                dim_idx = ASR::down_cast<ASR::IntegerConstant_t>(
                                    ab->m_dim)->m_n - 1;
                            }
                        }
                        if (dim_idx >= 0 && (size_t)dim_idx < arr->n_dims) {
                            if (ab->m_bound == ASR::arrayboundType::LBound) {
                                if (arr->m_dims[dim_idx].m_start) {
                                    visit_expr(arr->m_dims[dim_idx].m_start);
                                } else {
                                    src << "1";
                                }
                            } else {
                                // UBound = start + length - 1
                                if (arr->m_dims[dim_idx].m_length) {
                                    src << "(";
                                    if (arr->m_dims[dim_idx].m_start) {
                                        visit_expr(arr->m_dims[dim_idx].m_start);
                                    } else {
                                        src << "1";
                                    }
                                    src << " + ";
                                    visit_expr(arr->m_dims[dim_idx].m_length);
                                    src << " - 1)";
                                } else {
                                    src << "/* unknown ubound */";
                                }
                            }
                        } else {
                            src << "/* dim out of range */";
                        }
                    } else {
                        src << "/* non-array bound */";
                    }
                }
                break;
            }
            case ASR::exprType::Cast: {
                ASR::Cast_t *c = ASR::down_cast<ASR::Cast_t>(expr);
                std::string target_type = metal_type(c->m_type);
                src << "((" << target_type << ")";
                visit_expr(c->m_arg);
                src << ")";
                break;
            }
            case ASR::exprType::FunctionCall: {
                ASR::FunctionCall_t *fc = ASR::down_cast<ASR::FunctionCall_t>(expr);
                std::string fn_name(ASRUtils::symbol_name(
                    ASRUtils::symbol_get_past_external(fc->m_name)));
                // Handle lowered intrinsic functions
                if (fn_name.find("_lcompilers_real_") == 0 ||
                    fn_name.find("_lcompilers_dble_") == 0) {
                    std::string target = metal_type(fc->m_type);
                    src << "((" << target << ")";
                    if (fc->n_args > 0 && fc->m_args[0].m_value) {
                        visit_expr(fc->m_args[0].m_value);
                    }
                    src << ")";
                } else if (fn_name.find("_lcompilers_int_") == 0 ||
                           fn_name.find("_lcompilers_nint_") == 0) {
                    std::string target = metal_type(fc->m_type);
                    src << "((" << target << ")";
                    if (fc->n_args > 0 && fc->m_args[0].m_value) {
                        visit_expr(fc->m_args[0].m_value);
                    }
                    src << ")";
                } else if (fn_name.find("_lcompilers_sqrt_") == 0) {
                    src << "sqrt(";
                    if (fc->n_args > 0 && fc->m_args[0].m_value)
                        visit_expr(fc->m_args[0].m_value);
                    src << ")";
                } else if (fn_name.find("_lcompilers_abs_") == 0) {
                    src << "abs(";
                    if (fc->n_args > 0 && fc->m_args[0].m_value)
                        visit_expr(fc->m_args[0].m_value);
                    src << ")";
                } else if (fn_name.find("_lcompilers_sin_") == 0) {
                    src << "sin(";
                    if (fc->n_args > 0 && fc->m_args[0].m_value)
                        visit_expr(fc->m_args[0].m_value);
                    src << ")";
                } else if (fn_name.find("_lcompilers_cos_") == 0) {
                    src << "cos(";
                    if (fc->n_args > 0 && fc->m_args[0].m_value)
                        visit_expr(fc->m_args[0].m_value);
                    src << ")";
                } else if (fn_name.find("_lcompilers_exp_") == 0) {
                    src << "exp(";
                    if (fc->n_args > 0 && fc->m_args[0].m_value)
                        visit_expr(fc->m_args[0].m_value);
                    src << ")";
                } else if (fn_name.find("_lcompilers_mod_") == 0 ||
                           fn_name.find("_lcompilers_optimization_mod_") == 0) {
                    ASR::ttype_t *type = fc->m_type;
                    if (type && type->type == ASR::ttypeType::Real) {
                        src << "fmod(";
                    } else {
                        src << "((";
                    }
                    if (fc->n_args > 0 && fc->m_args[0].m_value)
                        visit_expr(fc->m_args[0].m_value);
                    if (type && type->type == ASR::ttypeType::Real) {
                        src << ", ";
                        if (fc->n_args > 1 && fc->m_args[1].m_value)
                            visit_expr(fc->m_args[1].m_value);
                        src << ")";
                    } else {
                        src << ") % (";
                        if (fc->n_args > 1 && fc->m_args[1].m_value)
                            visit_expr(fc->m_args[1].m_value);
                        src << "))";
                    }
                } else if (fn_name.find("_lcompilers_min_") == 0 ||
                           fn_name.find("_lcompilers_min0_") == 0) {
                    src << "min(";
                    if (fc->n_args > 0 && fc->m_args[0].m_value)
                        visit_expr(fc->m_args[0].m_value);
                    src << ", ";
                    if (fc->n_args > 1 && fc->m_args[1].m_value)
                        visit_expr(fc->m_args[1].m_value);
                    src << ")";
                } else if (fn_name.find("_lcompilers_max_") == 0 ||
                           fn_name.find("_lcompilers_max0_") == 0) {
                    src << "max(";
                    if (fc->n_args > 0 && fc->m_args[0].m_value)
                        visit_expr(fc->m_args[0].m_value);
                    src << ", ";
                    if (fc->n_args > 1 && fc->m_args[1].m_value)
                        visit_expr(fc->m_args[1].m_value);
                    src << ")";
                } else {
                    // User-defined function call — emit as fn(args)
                    // Resolve to actual function name
                    ASR::Function_t *fn = resolve_function(fc->m_name);
                    std::string call_name = fn ? std::string(fn->m_name) : fn_name;
                    src << call_name << "(";
                    bool first_arg = true;
                    for (size_t i = 0; i < fc->n_args; i++) {
                        if (fc->m_args[i].m_value) {
                            ASR::ttype_t *arg_type = ASRUtils::expr_type(
                                fc->m_args[i].m_value);
                            if (!first_arg) src << ", ";
                            first_arg = false;
                            visit_expr(fc->m_args[i].m_value);
                            if (is_array_type(arg_type)) {
                                src << ", ";
                                emit_array_size_expr(fc->m_args[i].m_value);
                            } else if (is_struct_type(arg_type)) {
                                emit_struct_member_data_ptrs(
                                    fc->m_args[i].m_value);
                                emit_struct_member_sizes(
                                    fc->m_args[i].m_value);
                            }
                        }
                    }
                    src << ")";
                }
                break;
            }
            case ASR::exprType::ArrayItem: {
                ASR::ArrayItem_t *ai = ASR::down_cast<ASR::ArrayItem_t>(expr);
                // For allocatable struct member access like s%a(i),
                // use the separate data pointer parameter instead of
                // the struct member (which is not a valid array in Metal).
                bool used_data_param = false;
                if (ASR::is_a<ASR::StructInstanceMember_t>(*ai->m_v)) {
                    ASR::StructInstanceMember_t *sm =
                        ASR::down_cast<ASR::StructInstanceMember_t>(ai->m_v);
                    std::string mem_name = ASRUtils::symbol_name(
                        ASRUtils::symbol_get_past_external(sm->m_m));
                    if (ASR::is_a<ASR::Var_t>(*sm->m_v)) {
                        // Single struct: s%member(j)
                        std::string struct_name = ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(sm->m_v)->m_v);
                        std::string key = struct_name + "." + mem_name;
                        auto it = func_array_data_params.find(key);
                        if (it != func_array_data_params.end()) {
                            src << it->second;
                            used_data_param = true;
                        }
                    } else if (ASR::is_a<ASR::ArrayItem_t>(*sm->m_v)) {
                        // Array-of-struct: arr(i)%member(j)
                        // Use separate data+offsets buffers:
                        // __data_arr_member[__offsets_arr_member[i-1] + (j-1)]
                        ASR::ArrayItem_t *arr_ai =
                            ASR::down_cast<ASR::ArrayItem_t>(sm->m_v);
                        if (ASR::is_a<ASR::Var_t>(*arr_ai->m_v)) {
                            std::string arr_name = ASRUtils::symbol_name(
                                ASR::down_cast<ASR::Var_t>(
                                    arr_ai->m_v)->m_v);
                            std::string key = arr_name + "." + mem_name;
                            auto dit = func_array_data_params.find(key);
                            auto oit = struct_array_offset_params.find(key);
                            if (dit != func_array_data_params.end() &&
                                    oit != struct_array_offset_params.end()) {
                                // Emit: data[offsets[arr_idx] + member_idx]
                                src << dit->second << "[" << oit->second
                                    << "[";
                                // Emit the struct array index (0-based)
                                if (arr_ai->n_args == 1) {
                                    ASR::expr_t *arr_idx =
                                        arr_ai->m_args[0].m_right
                                        ? arr_ai->m_args[0].m_right
                                        : arr_ai->m_args[0].m_left;
                                    if (arr_idx) {
                                        src << "((int)(";
                                        visit_expr(arr_idx);
                                        src << ") - 1)";
                                    } else {
                                        src << "0";
                                    }
                                } else {
                                    src << "0";
                                }
                                src << "] + ";
                                // Emit the member array index (0-based)
                                if (ai->n_args == 1) {
                                    ASR::expr_t *mem_idx =
                                        ai->m_args[0].m_right
                                        ? ai->m_args[0].m_right
                                        : ai->m_args[0].m_left;
                                    if (mem_idx) {
                                        src << "((int)(";
                                        visit_expr(mem_idx);
                                        src << ") - 1)";
                                    } else {
                                        src << "0";
                                    }
                                } else {
                                    src << "0";
                                }
                                src << "]";
                                // Skip the normal indexing path below
                                break;
                            }
                        }
                    }
                }
                if (!used_data_param) {
                    visit_expr(ai->m_v);
                }
                src << "[";
                // Compute linearized 0-based index for multi-dim arrays
                // Fortran: column-major, 1-based
                // Metal: flat 0-based buffer
                // For a(i,j) with dims (m,n): index = (j-1)*m + (i-1)
                ASR::ttype_t *arr_type = ASRUtils::expr_type(ai->m_v);

                if (ai->n_args == 1) {
                    // 1D: simple index - 1
                    ASR::expr_t *idx = ai->m_args[0].m_right ?
                        ai->m_args[0].m_right : ai->m_args[0].m_left;
                    if (idx) {
                        src << "((int)(";
                        visit_expr(idx);
                        src << ") - 1)";
                    } else {
                        src << "0";
                    }
                } else {
                    // Multi-dim: linearize column-major
                    // index = (i1-1) + dim[0]*((i2-1) + dim[1]*((i3-1) + ...))
                    // Build from innermost to outermost
                    emit_linearized_index(ai, arr_type);
                }
                src << "]";
                break;
            }
            case ASR::exprType::IntrinsicElementalFunction: {
                ASR::IntrinsicElementalFunction_t *f =
                    ASR::down_cast<ASR::IntrinsicElementalFunction_t>(expr);
                emit_intrinsic(f);
                break;
            }
            case ASR::exprType::GpuThreadIndex: {
                src << "__thread_id";
                break;
            }
            case ASR::exprType::GpuBlockIndex: {
                src << "0";
                break;
            }
            case ASR::exprType::GpuBlockSize: {
                src << "0";
                break;
            }
            case ASR::exprType::RealSqrt: {
                ASR::RealSqrt_t *rs = ASR::down_cast<ASR::RealSqrt_t>(expr);
                src << "sqrt(";
                visit_expr(rs->m_arg);
                src << ")";
                break;
            }
            case ASR::exprType::StructInstanceMember: {
                ASR::StructInstanceMember_t *sm =
                    ASR::down_cast<ASR::StructInstanceMember_t>(expr);
                ASR::symbol_t *mem = ASRUtils::symbol_get_past_external(sm->m_m);
                std::string mem_name = ASRUtils::symbol_name(mem);
                // For allocatable array members with a known data pointer
                // parameter, emit the data pointer instead of struct.member
                // (the struct field is just a descriptor, not the data).
                if (ASR::is_a<ASR::Var_t>(*sm->m_v)) {
                    std::string struct_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(sm->m_v)->m_v);
                    std::string key = struct_name + "." + mem_name;
                    auto it = func_array_data_params.find(key);
                    if (it != func_array_data_params.end()) {
                        src << it->second;
                        break;
                    }
                }
                visit_expr(sm->m_v);
                src << ".";
                src << mem_name;
                break;
            }
            case ASR::exprType::ArrayPhysicalCast: {
                ASR::ArrayPhysicalCast_t *apc =
                    ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr);
                visit_expr(apc->m_arg);
                break;
            }
            case ASR::exprType::ArraySize: {
                ASR::ArraySize_t *as = ASR::down_cast<ASR::ArraySize_t>(expr);
                if (as->m_value) {
                    visit_expr(as->m_value);
                } else if (ASR::is_a<ASR::Var_t>(*as->m_v)) {
                    std::string arr_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(as->m_v)->m_v);
                    auto it = func_array_size_params.find(arr_name);
                    if (it != func_array_size_params.end()) {
                        src << it->second;
                    } else {
                        ASR::ttype_t *arr_type = ASRUtils::expr_type(as->m_v);
                        if (ASR::is_a<ASR::Array_t>(*arr_type)) {
                            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(
                                arr_type);
                            bool first_dim = true;
                            src << "(";
                            for (size_t d = 0; d < arr->n_dims; d++) {
                                if (arr->m_dims[d].m_length) {
                                    if (!first_dim) src << " * ";
                                    first_dim = false;
                                    visit_expr(arr->m_dims[d].m_length);
                                }
                            }
                            if (first_dim) src << "0";
                            src << ")";
                        } else {
                            src << "/* unknown array size */";
                        }
                    }
                } else if (ASR::is_a<ASR::StructInstanceMember_t>(*as->m_v)) {
                    ASR::StructInstanceMember_t *sm =
                        ASR::down_cast<ASR::StructInstanceMember_t>(
                            as->m_v);
                    if (ASR::is_a<ASR::Var_t>(*sm->m_v)) {
                        std::string struct_name =
                            ASRUtils::symbol_name(
                                ASR::down_cast<ASR::Var_t>(
                                    sm->m_v)->m_v);
                        std::string mem_name =
                            ASRUtils::symbol_name(
                                ASRUtils::symbol_get_past_external(
                                    sm->m_m));
                        std::string key =
                            struct_name + "." + mem_name;
                        auto sit = func_array_size_params.find(key);
                        if (sit != func_array_size_params.end()) {
                            src << sit->second;
                        } else {
                            src << "/* unknown struct member array size */";
                        }
                    } else {
                        src << "/* unsupported ArraySize */";
                    }
                } else {
                    src << "/* unsupported ArraySize */";
                }
                break;
            }
            case ASR::exprType::IntrinsicImpureFunction: {
                ASR::IntrinsicImpureFunction_t *f =
                    ASR::down_cast<ASR::IntrinsicImpureFunction_t>(expr);
                if (f->m_impure_intrinsic_id == static_cast<int64_t>(
                        ASRUtils::IntrinsicImpureFunctions::Allocated)) {
                    // On Metal GPU, allocated() always returns true
                    // (data is pre-allocated on host before kernel launch)
                    src << "1";
                } else {
                    src << "/* unsupported IntrinsicImpureFunction */";
                }
                break;
            }
            default:
                src << "/* unsupported expr type " << expr->type << " */";
                break;
        }
    }

    void emit_linearized_index(ASR::ArrayItem_t *ai,
                               ASR::ttype_t *arr_type) {
        // Column-major linearization: index = sum_d( (idx_d - 1) * stride_d )
        // stride_0 = 1, stride_1 = dim[0], stride_2 = dim[0]*dim[1], ...
        // Strides are built as string expressions to handle variable dims.
        ASR::Array_t *arr = nullptr;
        ASR::ttype_t *inner = ASRUtils::type_get_past_allocatable(arr_type);
        if (ASR::is_a<ASR::Array_t>(*inner)) {
            arr = ASR::down_cast<ASR::Array_t>(inner);
        }
        bool first = true;
        std::string stride = "1";
        for (size_t d = 0; d < ai->n_args; d++) {
            ASR::expr_t *idx = ai->m_args[d].m_right ?
                ai->m_args[d].m_right : ai->m_args[d].m_left;
            if (!idx) continue;
            if (!first) src << " + ";
            first = false;
            if (stride == "1") {
                src << "((int)(";
                visit_expr(idx);
                src << ") - 1)";
            } else {
                src << "(" << stride << " * ((int)(";
                visit_expr(idx);
                src << ") - 1))";
            }
            if (arr && d < arr->n_dims) {
                ASR::expr_t *dim_len = arr->m_dims[d].m_length;
                std::string len_str = "0";
                if (dim_len) {
                    if (ASR::is_a<ASR::IntegerConstant_t>(*dim_len)) {
                        len_str = std::to_string(
                            ASR::down_cast<ASR::IntegerConstant_t>(
                                dim_len)->m_n);
                    } else if (ASR::is_a<ASR::Var_t>(*dim_len)) {
                        len_str = ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(dim_len)->m_v);
                    }
                }
                if (stride == "1") {
                    stride = len_str;
                } else {
                    stride = "(" + stride + " * " + len_str + ")";
                }
            }
        }
    }

    void emit_intrinsic(ASR::IntrinsicElementalFunction_t *f) {
        using IEF = ASRUtils::IntrinsicElementalFunctions;
        int64_t id = f->m_intrinsic_id;

        if (id == static_cast<int64_t>(IEF::Sqrt)) {
            src << "sqrt(";
            visit_expr(f->m_args[0]);
            src << ")";
        } else if (id == static_cast<int64_t>(IEF::Abs)) {
            src << "abs(";
            visit_expr(f->m_args[0]);
            src << ")";
        } else if (id == static_cast<int64_t>(IEF::Sin)) {
            src << "sin(";
            visit_expr(f->m_args[0]);
            src << ")";
        } else if (id == static_cast<int64_t>(IEF::Cos)) {
            src << "cos(";
            visit_expr(f->m_args[0]);
            src << ")";
        } else if (id == static_cast<int64_t>(IEF::Exp)) {
            src << "exp(";
            visit_expr(f->m_args[0]);
            src << ")";
        } else if (id == static_cast<int64_t>(IEF::Mod)) {
            // mod(a,b) in Fortran → a % b for integers, fmod for reals
            ASR::ttype_t *type = ASRUtils::expr_type(f->m_args[0]);
            if (type->type == ASR::ttypeType::Real) {
                src << "fmod(";
                visit_expr(f->m_args[0]);
                src << ", ";
                visit_expr(f->m_args[1]);
                src << ")";
            } else {
                src << "(";
                visit_expr(f->m_args[0]);
                src << " % ";
                visit_expr(f->m_args[1]);
                src << ")";
            }
        } else if (id == static_cast<int64_t>(IEF::Min)) {
            src << "min(";
            visit_expr(f->m_args[0]);
            src << ", ";
            visit_expr(f->m_args[1]);
            src << ")";
        } else if (id == static_cast<int64_t>(IEF::Max)) {
            src << "max(";
            visit_expr(f->m_args[0]);
            src << ", ";
            visit_expr(f->m_args[1]);
            src << ")";
        } else if (id == static_cast<int64_t>(IEF::Real)) {
            // Real cast: just emit the argument with a float cast
            src << "((float)(";
            visit_expr(f->m_args[0]);
            src << "))";
        } else {
            src << "/* unsupported intrinsic " << id << " */(";
            if (f->n_args > 0) visit_expr(f->m_args[0]);
            src << ")";
        }
    }

    std::string binop_str(ASR::binopType op) {
        switch (op) {
            case ASR::binopType::Add: return "+";
            case ASR::binopType::Sub: return "-";
            case ASR::binopType::Mul: return "*";
            case ASR::binopType::Div: return "/";
            default: return "?";
        }
    }

    std::string cmpop_str(ASR::cmpopType op) {
        switch (op) {
            case ASR::cmpopType::Eq: return "==";
            case ASR::cmpopType::NotEq: return "!=";
            case ASR::cmpopType::Lt: return "<";
            case ASR::cmpopType::LtE: return "<=";
            case ASR::cmpopType::Gt: return ">";
            case ASR::cmpopType::GtE: return ">=";
        }
        return "?";
    }
};

Result<std::string> asr_to_metal(Allocator & /*al*/, ASR::TranslationUnit_t &asr,
    diag::Diagnostics &diagnostics, CompilerOptions &co)
{
    ASRToMetalVisitor v(co);
    try {
        v.visit_TranslationUnit(asr);
    } catch (const CodeGenError &e) {
        diagnostics.diagnostics.push_back(e.d);
        return Error();
    } catch (const Abort &) {
        return Error();
    }
    return v.src.str();
}

} // namespace LCompilers

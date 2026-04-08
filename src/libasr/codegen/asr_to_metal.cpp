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

class GpuMathHelperScanner : public ASR::BaseWalkVisitor<GpuMathHelperScanner> {
public:
    bool needs_erf = false;
    bool needs_erfc = false;
    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        std::string fn_name(ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(x.m_name)));
        if (fn_name.find("_lcompilers_erf_") == 0) {
            needs_erf = true;
        } else if (fn_name.find("_lcompilers_erfc_") == 0 &&
                   fn_name.find("_lcompilers_erfc_scaled_") != 0) {
            needs_erfc = true;
        }
        ASR::BaseWalkVisitor<GpuMathHelperScanner>::visit_FunctionCall(x);
    }
    void visit_IntrinsicElementalFunction(
            const ASR::IntrinsicElementalFunction_t &x) {
        using IEF = ASRUtils::IntrinsicElementalFunctions;
        if (x.m_intrinsic_id == static_cast<int64_t>(IEF::Erf)) {
            needs_erf = true;
        } else if (x.m_intrinsic_id == static_cast<int64_t>(IEF::Erfc)) {
            needs_erfc = true;
        }
        ASR::BaseWalkVisitor<GpuMathHelperScanner>::
            visit_IntrinsicElementalFunction(x);
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

    // Tracks array parameters in the current inline function that are
    // emitted as thread pointers (thread T*). Used by the Assignment
    // handler to emit element-by-element copy for whole-array assignments.
    std::set<std::string> func_array_params;

    // Tracks local Allocatable(Array) variables in the current kernel
    // that have been declared as fixed-size arrays. Used to:
    // - skip '&' prefix in SubroutineCall (array decays to pointer)
    // - emit element-by-element copy in assignments
    std::set<std::string> local_alloc_arrays;

    // Maps local allocatable variable names to their computed total
    // element count, determined by pre-scanning Allocate statements
    // in the kernel's inline functions.
    std::map<std::string, int64_t> alloc_array_sizes;

    // Maps local allocatable variable names to Metal C expression strings
    // for runtime-dependent allocation sizes that cannot be resolved to
    // compile-time constants.
    std::map<std::string, std::string> alloc_array_size_exprs;

    // Maps pointer-to-section variable names to the Metal C expression
    // string for the section size, set when processing Associate stmts
    // that point into array sections.
    std::map<std::string, std::string> ptr_section_sizes;

    // Tracks pointer variables that are associated with local
    // (thread-space) arrays rather than device buffer arrays.
    std::set<std::string> ptr_to_local_alloc;

    // Kernel argument names (device buffer parameters). Used to
    // distinguish device-space arrays from thread-local arrays when
    // determining pointer address spaces.
    std::set<std::string> kernel_arg_names;

    // Names of allocatable array variables in Block scopes that have
    // non-constant dimensions (VLA workspaces). These are backed by
    // device buffers and must keep device address space.
    std::set<std::string> vla_workspace_names;

    // Tracks function names already emitted across all kernels in the
    // current translation unit, preventing duplicate definitions when
    // multiple kernels call the same module function.
    std::set<std::string> emitted_funcs;

    // True when emitting an inline function body (not a kernel body).
    // Used to suppress array buffer indexing logic that only applies
    // to kernel-level device buffer parameters.
    bool in_inline_function = false;

    // When >= 0, array/pointer Var nodes should be indexed with this
    // element index in visit_expr. Used for element-wise array
    // operations (comparisons, etc.) inside assignment loops.
    int64_t array_elem_index = -1;

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
        if (type->type == ASR::ttypeType::Array) return true;
        if (type->type == ASR::ttypeType::Pointer) {
            ASR::ttype_t *inner = ASR::down_cast<ASR::Pointer_t>(type)->m_type;
            return inner->type == ASR::ttypeType::Array;
        }
        return false;
    }

    // Emit a local variable declaration, including array dimensions.
    // For scalars: `float x;`
    // For arrays:  `float x[3];` or `float x[n];` (VLA)
    // For allocatable arrays: `float x[N];` (fixed-size from Allocate)
    void emit_local_var_decl(ASR::Variable_t *var) {
        ASR::ttype_t *type = var->m_type;
        ASR::ttype_t *base_type = ASRUtils::type_get_past_allocatable(type);
        bool is_alloc = ASRUtils::is_allocatable(type);
        // Pointer to array: emit as device or thread pointer depending
        // on whether it points to a device buffer or local array.
        if (ASR::is_a<ASR::Pointer_t>(*base_type)) {
            ASR::ttype_t *ptr_inner = ASR::down_cast<ASR::Pointer_t>(
                base_type)->m_type;
            if (ASR::is_a<ASR::Array_t>(*ptr_inner)) {
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(ptr_inner);
                std::string vname(var->m_name);
                if (ptr_to_local_alloc.count(vname)) {
                    src << get_indent() << "thread " << metal_type(arr->m_type)
                        << "* " << vname << ";\n";
                } else {
                    src << get_indent() << "device " << metal_type(arr->m_type)
                        << "* " << vname << ";\n";
                }
                return;
            }
        }
        if (ASR::is_a<ASR::Array_t>(*base_type)) {
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(base_type);
            std::string elem_type;
            if (is_struct_type(arr->m_type)) {
                elem_type = get_struct_name(var);
            } else {
                elem_type = metal_type(arr->m_type);
            }
            if (is_alloc) {
                // Allocatable array: check if a VLA workspace buffer
                // was allocated for this variable
                std::string vname(var->m_name);
                auto vla_it = std::find_if(
                    current_vla_infos.begin(), current_vla_infos.end(),
                    [&](const GpuVlaWorkspace &ws) {
                        return ws.var_name == vname;
                    });
                if (vla_it != current_vla_infos.end()) {
                    src << get_indent() << "device " << elem_type
                        << "* " << vname << " = __vla_" << vname
                        << " + __thread_id * (";
                    for (size_t d = 0; d < vla_it->dims.size(); d++) {
                        if (d > 0) src << " * ";
                        if (vla_it->dims[d].is_constant) {
                            src << vla_it->dims[d].constant_value;
                        } else {
                            visit_expr(vla_it->dims[d].dim_expr);
                        }
                    }
                    src << ");\n";
                    local_alloc_arrays.insert(vname);
                    return;
                }
            }
            src << get_indent() << elem_type << " "
                << var->m_name;
            if (is_alloc) {
                // Use pre-scanned compile-time size, runtime
                // expression, or fallback
                std::string vname(var->m_name);
                auto it = alloc_array_sizes.find(vname);
                if (it != alloc_array_sizes.end()) {
                    src << "[" << it->second << "]";
                } else {
                    auto eit = alloc_array_size_exprs.find(vname);
                    if (eit != alloc_array_size_exprs.end()) {
                        src << "[" << eit->second << "]";
                    } else {
                        src << "[1]";
                    }
                }
                local_alloc_arrays.insert(vname);
            } else if (arr->n_dims > 1) {
                // Multi-dimensional arrays are flattened to 1D because
                // ArrayItem uses linearized column-major indexing
                int64_t total = get_total_elements(base_type);
                src << "[" << total << "]";
            } else {
                src << "[";
                if (arr->m_dims[0].m_length) {
                    visit_expr(arr->m_dims[0].m_length);
                }
                src << "]";
            }
            src << ";\n";
        } else if (is_struct_type(base_type)) {
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

    // Compute the total allocation size from an Allocate statement's
    // alloc_arg dimensions. Returns -1 if any dimension is non-constant.
    int64_t compute_alloc_size(ASR::alloc_arg_t &arg) {
        int64_t total = 1;
        for (size_t d = 0; d < arg.n_dims; d++) {
            ASR::dimension_t &dim = arg.m_dims[d];
            if (!dim.m_length) return -1;
            if (!ASR::is_a<ASR::IntegerConstant_t>(*dim.m_length))
                return -1;
            total *= ASR::down_cast<ASR::IntegerConstant_t>(
                dim.m_length)->m_n;
        }
        return total;
    }

    // Compute the allocation size as a Metal C expression string.
    // Used when compile-time constant evaluation fails.
    std::string compute_alloc_size_expr(ASR::alloc_arg_t &arg) {
        std::stringstream save;
        save << src.str();
        std::string result;
        bool first = true;
        for (size_t d = 0; d < arg.n_dims; d++) {
            ASR::dimension_t &dim = arg.m_dims[d];
            if (!dim.m_length) return "";
            src.str("");
            visit_expr(dim.m_length);
            std::string dim_expr = src.str();
            if (dim_expr.empty()) return "";
            if (!first) result += " * ";
            first = false;
            result += "(" + dim_expr + ")";
        }
        src.str("");
        src << save.str();
        return result;
    }

    // Recursively scan a statement list for Allocate and Associate
    // statements, recursing into nested control flow.
    void scan_stmts_recursive(ASR::stmt_t **stmts, size_t n,
            const std::map<std::string,
                std::map<size_t, int64_t>> &func_allocs,
            const std::map<std::string,
                std::map<size_t, size_t>> &func_param_copies) {
        for (size_t i = 0; i < n; i++) {
            scan_stmt_for_alloc_sizes(stmts[i], func_allocs,
                func_param_copies);
            // Recurse into nested control flow
            switch (stmts[i]->type) {
                case ASR::stmtType::WhileLoop: {
                    ASR::WhileLoop_t *wl =
                        ASR::down_cast<ASR::WhileLoop_t>(stmts[i]);
                    scan_stmts_recursive(wl->m_body, wl->n_body,
                        func_allocs, func_param_copies);
                    break;
                }
                case ASR::stmtType::DoLoop: {
                    ASR::DoLoop_t *dl =
                        ASR::down_cast<ASR::DoLoop_t>(stmts[i]);
                    scan_stmts_recursive(dl->m_body, dl->n_body,
                        func_allocs, func_param_copies);
                    break;
                }
                case ASR::stmtType::If: {
                    ASR::If_t *if_s =
                        ASR::down_cast<ASR::If_t>(stmts[i]);
                    scan_stmts_recursive(if_s->m_body, if_s->n_body,
                        func_allocs, func_param_copies);
                    scan_stmts_recursive(if_s->m_orelse, if_s->n_orelse,
                        func_allocs, func_param_copies);
                    break;
                }
                default:
                    break;
            }
        }
    }

    // Recursively scan statements for assignment-based size propagation.
    void propagate_stmts_recursive(ASR::stmt_t **stmts, size_t n,
            bool &changed) {
        for (size_t i = 0; i < n; i++) {
            changed |= propagate_alloc_size_from_stmt(stmts[i]);
            switch (stmts[i]->type) {
                case ASR::stmtType::WhileLoop: {
                    ASR::WhileLoop_t *wl =
                        ASR::down_cast<ASR::WhileLoop_t>(stmts[i]);
                    propagate_stmts_recursive(wl->m_body, wl->n_body,
                        changed);
                    break;
                }
                case ASR::stmtType::DoLoop: {
                    ASR::DoLoop_t *dl =
                        ASR::down_cast<ASR::DoLoop_t>(stmts[i]);
                    propagate_stmts_recursive(dl->m_body, dl->n_body,
                        changed);
                    break;
                }
                case ASR::stmtType::If: {
                    ASR::If_t *if_s =
                        ASR::down_cast<ASR::If_t>(stmts[i]);
                    propagate_stmts_recursive(if_s->m_body, if_s->n_body,
                        changed);
                    propagate_stmts_recursive(if_s->m_orelse,
                        if_s->n_orelse, changed);
                    break;
                }
                default:
                    break;
            }
        }
    }

    // Pre-scan functions defined in the kernel scope to collect
    // allocation sizes. For each function, records which parameter
    // index is allocated and its compile-time size. Then scans
    // kernel body SubroutineCalls to map caller local variables
    // to those sizes, and propagates through assignments.
    void prescan_alloc_sizes(const ASR::GpuKernelFunction_t &kf) {
        alloc_array_sizes.clear();
        alloc_array_size_exprs.clear();
        ptr_to_local_alloc.clear();
        kernel_arg_names.clear();
        vla_workspace_names.clear();
        // Collect kernel argument names (device buffer parameters)
        for (size_t i = 0; i < kf.n_args; i++) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(kf.m_args[i]);
            kernel_arg_names.insert(ASRUtils::symbol_name(v->m_v));
        }
        // Collect VLA workspace variable names (allocatable arrays
        // with non-constant dimensions in Block scopes). These are
        // backed by device buffers and must keep device address space.
        for (size_t bi = 0; bi < kf.n_body; bi++) {
            if (kf.m_body[bi]->type != ASR::stmtType::BlockCall) continue;
            ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                ASR::down_cast<ASR::BlockCall_t>(kf.m_body[bi])->m_m);
            for (auto &item : block->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                    item.second);
                ASR::ttype_t *vtype = var->m_type;
                if (!ASR::is_a<ASR::Array_t>(*vtype)) continue;
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(vtype);
                for (size_t d = 0; d < arr->n_dims; d++) {
                    if (arr->m_dims[d].m_length &&
                            !ASR::is_a<ASR::IntegerConstant_t>(
                                *arr->m_dims[d].m_length)) {
                        vla_workspace_names.insert(
                            std::string(var->m_name));
                        break;
                    }
                }
            }
        }
        // Step 1: For each function in kernel scope, find Allocate
        // stmts and record param_index → size. Also detect assignments
        // of FixedSizeArray locals to output parameters (the passes may
        // lower allocatable array constants into a local FixedSizeArray
        // assigned to the out parameter, removing the Allocate).
        // Also detect out_param = in_param assignments (the out param
        // inherits the same size as the in param at the call site).
        std::map<std::string, std::map<size_t, int64_t>> func_allocs;
        std::map<std::string, std::map<size_t, size_t>> func_param_copies;
        for (auto &item : kf.m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(
                item.second);
            std::string fn_name(fn->m_name);
            // Build a set of parameter names for quick lookup
            std::set<std::string> param_names;
            for (size_t pi = 0; pi < fn->n_args; pi++) {
                ASR::Variable_t *pv =
                    ASR::down_cast<ASR::Variable_t>(
                        ASR::down_cast<ASR::Var_t>(
                            fn->m_args[pi])->m_v);
                param_names.insert(std::string(pv->m_name));
            }
            // Build a map of local FixedSizeArray variable names → sizes
            std::map<std::string, int64_t> local_fixed_sizes;
            for (auto &sym : fn->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*sym.second)) continue;
                ASR::Variable_t *var =
                    ASR::down_cast<ASR::Variable_t>(sym.second);
                ASR::ttype_t *vtype = var->m_type;
                if (!ASR::is_a<ASR::Array_t>(*vtype)) continue;
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(vtype);
                if (arr->m_physical_type !=
                        ASR::array_physical_typeType::FixedSizeArray)
                    continue;
                int64_t total = 1;
                bool all_const = true;
                for (size_t d = 0; d < arr->n_dims; d++) {
                    if (arr->m_dims[d].m_length &&
                            ASR::is_a<ASR::IntegerConstant_t>(
                                *arr->m_dims[d].m_length)) {
                        total *= ASR::down_cast<ASR::IntegerConstant_t>(
                            arr->m_dims[d].m_length)->m_n;
                    } else {
                        all_const = false;
                        break;
                    }
                }
                if (all_const && total > 0) {
                    local_fixed_sizes[std::string(var->m_name)] = total;
                }
            }
            for (size_t si = 0; si < fn->n_body; si++) {
                if (fn->m_body[si]->type == ASR::stmtType::Allocate) {
                    ASR::Allocate_t *alloc =
                        ASR::down_cast<ASR::Allocate_t>(fn->m_body[si]);
                    for (size_t ai = 0; ai < alloc->n_args; ai++) {
                        if (!alloc->m_args[ai].m_a) continue;
                        if (!ASR::is_a<ASR::Var_t>(
                                *alloc->m_args[ai].m_a))
                            continue;
                        std::string alloc_var = ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(
                                alloc->m_args[ai].m_a)->m_v);
                        int64_t sz = compute_alloc_size(
                            alloc->m_args[ai]);
                        if (sz <= 0) continue;
                        for (size_t pi = 0; pi < fn->n_args; pi++) {
                            ASR::Variable_t *pv =
                                ASR::down_cast<ASR::Variable_t>(
                                    ASR::down_cast<ASR::Var_t>(
                                        fn->m_args[pi])->m_v);
                            if (std::string(pv->m_name) == alloc_var) {
                                func_allocs[fn_name][pi] = sz;
                                break;
                            }
                        }
                    }
                } else if (fn->m_body[si]->type ==
                        ASR::stmtType::Assignment) {
                    ASR::Assignment_t *asgn =
                        ASR::down_cast<ASR::Assignment_t>(
                            fn->m_body[si]);
                    if (!ASR::is_a<ASR::Var_t>(*asgn->m_target)) continue;
                    if (!ASR::is_a<ASR::Var_t>(*asgn->m_value)) continue;
                    std::string tgt_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(
                            asgn->m_target)->m_v);
                    std::string val_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(
                            asgn->m_value)->m_v);
                    if (!param_names.count(tgt_name)) continue;
                    auto fit = local_fixed_sizes.find(val_name);
                    if (fit != local_fixed_sizes.end()) {
                        for (size_t pi = 0; pi < fn->n_args; pi++) {
                            ASR::Variable_t *pv =
                                ASR::down_cast<ASR::Variable_t>(
                                    ASR::down_cast<ASR::Var_t>(
                                        fn->m_args[pi])->m_v);
                            if (std::string(pv->m_name) == tgt_name) {
                                if (!func_allocs[fn_name].count(pi)) {
                                    func_allocs[fn_name][pi] =
                                        fit->second;
                                }
                                break;
                            }
                        }
                    } else if (param_names.count(val_name)) {
                        // out_param = in_param: the out param inherits
                        // the size of the in param at the call site.
                        size_t tgt_idx = SIZE_MAX, val_idx = SIZE_MAX;
                        for (size_t pi = 0; pi < fn->n_args; pi++) {
                            std::string pname(ASRUtils::symbol_name(
                                ASR::down_cast<ASR::Var_t>(
                                    fn->m_args[pi])->m_v));
                            if (pname == tgt_name) tgt_idx = pi;
                            if (pname == val_name) val_idx = pi;
                        }
                        if (tgt_idx != SIZE_MAX && val_idx != SIZE_MAX) {
                            func_param_copies[fn_name][tgt_idx] =
                                val_idx;
                        }
                    }
                }
            }
        }
        // Step 2: Recursively scan all kernel body statements
        // (including inside WhileLoops, DoLoops, If blocks)
        scan_stmts_recursive(kf.m_body, kf.n_body, func_allocs,
            func_param_copies);
        // Step 3: Scan blocks in kernel body for the same
        for (size_t bi = 0; bi < kf.n_body; bi++) {
            if (kf.m_body[bi]->type == ASR::stmtType::BlockCall) {
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
                    ASR::down_cast<ASR::BlockCall_t>(
                        kf.m_body[bi])->m_m);
                scan_stmts_recursive(block->m_body, block->n_body,
                    func_allocs, func_param_copies);
            } else if (kf.m_body[bi]->type ==
                    ASR::stmtType::AssociateBlockCall) {
                ASR::AssociateBlock_t *ab =
                    ASR::down_cast<ASR::AssociateBlock_t>(
                        ASR::down_cast<ASR::AssociateBlockCall_t>(
                            kf.m_body[bi])->m_m);
                scan_stmts_recursive(ab->m_body, ab->n_body,
                    func_allocs, func_param_copies);
            }
        }
        // Step 4: Propagate sizes through assignments
        // (if a = b and b has known size, a gets same size)
        bool changed = true;
        while (changed) {
            changed = false;
            propagate_stmts_recursive(kf.m_body, kf.n_body, changed);
            for (size_t bi = 0; bi < kf.n_body; bi++) {
                if (kf.m_body[bi]->type == ASR::stmtType::BlockCall) {
                    ASR::Block_t *block =
                        ASR::down_cast<ASR::Block_t>(
                            ASR::down_cast<ASR::BlockCall_t>(
                                kf.m_body[bi])->m_m);
                    propagate_stmts_recursive(block->m_body,
                        block->n_body, changed);
                } else if (kf.m_body[bi]->type ==
                        ASR::stmtType::AssociateBlockCall) {
                    ASR::AssociateBlock_t *ab =
                        ASR::down_cast<ASR::AssociateBlock_t>(
                            ASR::down_cast<ASR::AssociateBlockCall_t>(
                                kf.m_body[bi])->m_m);
                    propagate_stmts_recursive(ab->m_body,
                        ab->n_body, changed);
                }
            }
        }
    }

    void scan_stmt_for_alloc_sizes(ASR::stmt_t *stmt,
            const std::map<std::string,
                std::map<size_t, int64_t>> &func_allocs,
            const std::map<std::string,
                std::map<size_t, size_t>> &func_param_copies) {
        if (stmt->type == ASR::stmtType::SubroutineCall) {
            ASR::SubroutineCall_t *sc =
                ASR::down_cast<ASR::SubroutineCall_t>(stmt);
            std::string fn_name(ASRUtils::symbol_name(
                ASRUtils::symbol_get_past_external(sc->m_name)));
            auto it = func_allocs.find(fn_name);
            if (it != func_allocs.end()) {
                for (auto &[param_idx, sz] : it->second) {
                    if (param_idx >= sc->n_args) continue;
                    ASR::expr_t *actual = sc->m_args[param_idx].m_value;
                    if (!actual || !ASR::is_a<ASR::Var_t>(*actual)) continue;
                    std::string actual_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(actual)->m_v);
                    alloc_array_sizes[actual_name] = sz;
                }
            }
            // out_param = in_param: propagate size from actual in arg
            // to actual out arg.
            auto cit = func_param_copies.find(fn_name);
            if (cit != func_param_copies.end()) {
                for (auto &[out_idx, in_idx] : cit->second) {
                    if (out_idx >= sc->n_args || in_idx >= sc->n_args)
                        continue;
                    ASR::expr_t *out_actual =
                        sc->m_args[out_idx].m_value;
                    ASR::expr_t *in_actual =
                        sc->m_args[in_idx].m_value;
                    if (!out_actual || !in_actual) continue;
                    if (!ASR::is_a<ASR::Var_t>(*out_actual)) continue;
                    std::string out_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(out_actual)->m_v);
                    if (alloc_array_sizes.count(out_name)) continue;
                    // Try to get size from the in actual arg's type
                    ASR::ttype_t *in_type =
                        ASRUtils::type_get_past_allocatable_pointer(
                            ASRUtils::expr_type(in_actual));
                    if (ASR::is_a<ASR::Array_t>(*in_type)) {
                        ASR::Array_t *arr =
                            ASR::down_cast<ASR::Array_t>(in_type);
                        int64_t total = 1;
                        bool all_const = true;
                        for (size_t d = 0; d < arr->n_dims; d++) {
                            if (arr->m_dims[d].m_length &&
                                    ASR::is_a<ASR::IntegerConstant_t>(
                                        *arr->m_dims[d].m_length)) {
                                total *=
                                    ASR::down_cast<
                                        ASR::IntegerConstant_t>(
                                            arr->m_dims[d].m_length)
                                        ->m_n;
                            } else {
                                all_const = false;
                                break;
                            }
                        }
                        if (all_const && total > 0) {
                            alloc_array_sizes[out_name] = total;
                        }
                    }
                    // Also propagate from in actual's alloc size
                    if (!alloc_array_sizes.count(out_name)
                            && ASR::is_a<ASR::Var_t>(*in_actual)) {
                        std::string in_name = ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(
                                in_actual)->m_v);
                        auto sit = alloc_array_sizes.find(in_name);
                        if (sit != alloc_array_sizes.end()) {
                            alloc_array_sizes[out_name] = sit->second;
                        } else {
                            auto eit =
                                alloc_array_size_exprs.find(in_name);
                            if (eit !=
                                    alloc_array_size_exprs.end()) {
                                alloc_array_size_exprs[out_name] =
                                    eit->second;
                            }
                        }
                    }
                }
            }
        } else if (stmt->type == ASR::stmtType::Allocate) {
            ASR::Allocate_t *alloc =
                ASR::down_cast<ASR::Allocate_t>(stmt);
            for (size_t ai = 0; ai < alloc->n_args; ai++) {
                if (!alloc->m_args[ai].m_a) continue;
                if (!ASR::is_a<ASR::Var_t>(*alloc->m_args[ai].m_a))
                    continue;
                std::string vname = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(
                        alloc->m_args[ai].m_a)->m_v);
                int64_t sz = compute_alloc_size(alloc->m_args[ai]);
                if (sz > 0) {
                    alloc_array_sizes[vname] = sz;
                } else {
                    std::string expr =
                        compute_alloc_size_expr(alloc->m_args[ai]);
                    if (!expr.empty()) {
                        alloc_array_size_exprs[vname] = expr;
                    }
                }
            }
        } else if (stmt->type == ASR::stmtType::Associate) {
            ASR::Associate_t *assoc =
                ASR::down_cast<ASR::Associate_t>(stmt);
            if (ASR::is_a<ASR::Var_t>(*assoc->m_target)) {
                std::string tgt = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(assoc->m_target)->m_v);
                // Case 1: Associate target = Var (direct variable)
                if (ASR::is_a<ASR::Var_t>(*assoc->m_value)) {
                    std::string val = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(
                            assoc->m_value)->m_v);
                    // Propagate alloc size info to the pointer target
                    if (local_alloc_arrays.count(val) ||
                            alloc_array_sizes.count(val) ||
                            alloc_array_size_exprs.count(val)) {
                        auto sit = alloc_array_sizes.find(val);
                        if (sit != alloc_array_sizes.end()) {
                            alloc_array_size_exprs[tgt] =
                                std::to_string(sit->second);
                        } else {
                            auto eit =
                                alloc_array_size_exprs.find(val);
                            if (eit != alloc_array_size_exprs.end()) {
                                alloc_array_size_exprs[tgt] =
                                    eit->second;
                            }
                        }
                    }
                    // A non-allocatable, non-kernel-arg local array
                    // is in thread space (FixedSizeArray temporaries).
                    // Allocatable locals may be VLA workspaces backed
                    // by device buffers, so leave those as device.
                    if (!kernel_arg_names.count(val)) {
                        ASR::symbol_t *val_sym =
                            ASR::down_cast<ASR::Var_t>(
                                assoc->m_value)->m_v;
                        if (ASR::is_a<ASR::Variable_t>(*val_sym)) {
                            ASR::ttype_t *vt =
                                ASR::down_cast<ASR::Variable_t>(
                                    val_sym)->m_type;
                            if (!ASRUtils::is_allocatable(vt)) {
                                ptr_to_local_alloc.insert(tgt);
                            }
                        }
                    }
                }
                // Case 2: Associate target = ArraySection (pointer to
                // section of an array). If the base array is a local
                // (not a kernel arg) and not a VLA workspace, the
                // pointer is thread-space.
                if (ASR::is_a<ASR::ArraySection_t>(*assoc->m_value)) {
                    ASR::ArraySection_t *as =
                        ASR::down_cast<ASR::ArraySection_t>(
                            assoc->m_value);
                    if (ASR::is_a<ASR::Var_t>(*as->m_v)) {
                        std::string base = ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(
                                as->m_v)->m_v);
                        if (!kernel_arg_names.count(base)) {
                            ASR::symbol_t *base_sym =
                                ASR::down_cast<ASR::Var_t>(
                                    as->m_v)->m_v;
                            if (ASR::is_a<ASR::Variable_t>(
                                    *base_sym)) {
                                ASR::ttype_t *bt =
                                    ASR::down_cast<ASR::Variable_t>(
                                        base_sym)->m_type;
                                if (!ASRUtils::is_allocatable(bt) ||
                                        !vla_workspace_names.count(
                                            base)) {
                                    ptr_to_local_alloc.insert(tgt);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    bool propagate_alloc_size_from_stmt(ASR::stmt_t *stmt) {
        if (stmt->type != ASR::stmtType::Assignment) return false;
        ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(stmt);
        if (!ASR::is_a<ASR::Var_t>(*a->m_target)) return false;
        if (!ASR::is_a<ASR::Var_t>(*a->m_value)) return false;
        std::string tgt = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(a->m_target)->m_v);
        std::string val = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(a->m_value)->m_v);
        // Propagate compile-time sizes
        if (!alloc_array_sizes.count(tgt) && alloc_array_sizes.count(val)) {
            alloc_array_sizes[tgt] = alloc_array_sizes[val];
            return true;
        }
        // Propagate runtime size expressions
        if (!alloc_array_size_exprs.count(tgt)
                && alloc_array_size_exprs.count(val)) {
            alloc_array_size_exprs[tgt] = alloc_array_size_exprs[val];
            return true;
        }
        return false;
    }

    // Emit a pointer expression for an ArraySection. The result is a
    // device pointer to the first element of the section, suitable for
    // passing to a function that expects `device float*`.
    // For ArraySection(z, [1:2:1, i]) on a 2D (n1×n2) column-major
    // array, emits: z + linearized_index_of_section_start
    // Also records the section size in last_section_size for later use.
    void emit_array_section_pointer(ASR::expr_t *expr) {
        if (!ASR::is_a<ASR::ArraySection_t>(*expr)) {
            visit_expr(expr);
            return;
        }
        ASR::ArraySection_t *as = ASR::down_cast<ASR::ArraySection_t>(expr);
        std::string arr_name;
        if (ASR::is_a<ASR::Var_t>(*as->m_v)) {
            arr_name = ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(as->m_v)->m_v);
        }
        ASR::ttype_t *arr_type = ASRUtils::expr_type(as->m_v);
        ASR::Array_t *arr = nullptr;
        if (ASR::is_a<ASR::Array_t>(*arr_type)) {
            arr = ASR::down_cast<ASR::Array_t>(arr_type);
        }
        // Compute linearized offset of the section's first element
        // and the section size using column-major layout.
        // For range dims, use the lower bound as the index.
        // Section size is the product of range extents.
        std::stringstream size_ss;
        bool first_size = true;
        src << arr_name << " + ";
        bool first_dim = true;
        std::string stride = "1";
        for (size_t d = 0; d < as->n_args; d++) {
            bool is_range = as->m_args[d].m_left && as->m_args[d].m_right
                && as->m_args[d].m_step;
            ASR::expr_t *idx_expr = is_range
                ? as->m_args[d].m_left
                : (as->m_args[d].m_right
                    ? as->m_args[d].m_right : as->m_args[d].m_left);
            if (idx_expr) {
                if (!first_dim) src << " + ";
                first_dim = false;
                if (stride == "1") {
                    src << "((int)(";
                    visit_expr(idx_expr);
                    src << ") - 1)";
                } else {
                    src << "(" << stride << " * ((int)(";
                    visit_expr(idx_expr);
                    src << ") - 1))";
                }
            }
            // Compute section extent for range dims
            if (is_range) {
                std::stringstream save;
                save << src.str();
                src.str("");
                src << "((";
                visit_expr(as->m_args[d].m_right);
                src << ") - (";
                visit_expr(as->m_args[d].m_left);
                src << ") + 1)";
                std::string dim_size = src.str();
                src.str("");
                src << save.str();
                if (!first_size) size_ss << " * ";
                first_size = false;
                size_ss << dim_size;
            }
            // Update stride for next dimension
            if (arr && d < arr->n_dims) {
                ASR::expr_t *dim_len = arr->m_dims[d].m_length;
                std::string len_str = "0";
                if (dim_len) {
                    if (ASR::is_a<ASR::IntegerConstant_t>(*dim_len)) {
                        len_str = std::to_string(
                            ASR::down_cast<ASR::IntegerConstant_t>(
                                dim_len)->m_n);
                    } else {
                        std::stringstream save;
                        save << src.str();
                        src.str("");
                        visit_expr(dim_len);
                        len_str = src.str();
                        src.str("");
                        src << save.str();
                    }
                } else if (!arr_name.empty()) {
                    len_str = "__size_" + arr_name + "_dim"
                        + std::to_string(d + 1);
                }
                if (stride == "1") {
                    stride = len_str;
                } else {
                    stride = "(" + stride + " * " + len_str + ")";
                }
            }
        }
        if (first_dim) src << "0";
        if (!size_ss.str().empty()) {
            last_section_size = size_ss.str();
        }
    }

    std::string last_section_size;

    // Emit the total size of an array expression (product of dimensions).
    // Used at function call sites to pass array sizes for DescriptorArray
    // parameters that are represented as device pointers in Metal.
    void emit_array_size_expr(ASR::expr_t *expr) {
        if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
            expr = ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr)->m_arg;
        }
        ASR::ttype_t *type = ASRUtils::expr_type(expr);
        // Pointer(Array) — lookup the associated section size
        if (ASR::is_a<ASR::Pointer_t>(*type)) {
            ASR::ttype_t *inner = ASR::down_cast<ASR::Pointer_t>(type)->m_type;
            if (ASR::is_a<ASR::Array_t>(*inner)) {
                if (ASR::is_a<ASR::Var_t>(*expr)) {
                    std::string name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(expr)->m_v);
                    auto it = ptr_section_sizes.find(name);
                    if (it != ptr_section_sizes.end()) {
                        src << it->second;
                        return;
                    }
                    auto it2 = func_array_size_params.find(name);
                    if (it2 != func_array_size_params.end()) {
                        src << it2->second;
                        return;
                    }
                    auto it3 = alloc_array_size_exprs.find(name);
                    if (it3 != alloc_array_size_exprs.end()) {
                        src << it3->second;
                        return;
                    }
                }
                src << "/* unknown pointer-array size */";
                return;
            }
        }
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

    // Emit the array size for a StructInstanceMember expression whose
    // allocatable member has dynamic size. For array-of-struct elements
    // like t(i)%v, reads from __sizes_arr_member[idx]. For single
    // struct variables like s%v, reads from func_array_size_params.
    void emit_struct_member_array_size(ASR::expr_t *sim_expr) {
        ASR::StructInstanceMember_t *sm =
            ASR::down_cast<ASR::StructInstanceMember_t>(sim_expr);
        std::string mem_name = ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(sm->m_m));
        if (ASR::is_a<ASR::ArrayItem_t>(*sm->m_v)) {
            ASR::ArrayItem_t *arr_ai =
                ASR::down_cast<ASR::ArrayItem_t>(sm->m_v);
            if (ASR::is_a<ASR::Var_t>(*arr_ai->m_v)) {
                std::string arr_name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(arr_ai->m_v)->m_v);
                std::string key = arr_name + "." + mem_name;
                auto sit = struct_array_sizes_params.find(key);
                if (sit != struct_array_sizes_params.end()) {
                    src << sit->second << "[";
                    if (arr_ai->n_args == 1) {
                        ASR::expr_t *idx =
                            arr_ai->m_args[0].m_right
                            ? arr_ai->m_args[0].m_right
                            : arr_ai->m_args[0].m_left;
                        if (idx) {
                            src << "((int)(";
                            visit_expr(idx);
                            src << ") - 1)";
                        } else {
                            src << "0";
                        }
                    } else {
                        src << "0";
                    }
                    src << "]";
                    return;
                }
            }
        } else if (ASR::is_a<ASR::Var_t>(*sm->m_v)) {
            std::string struct_name = ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(sm->m_v)->m_v);
            std::string key = struct_name + "." + mem_name;
            auto sit = func_array_size_params.find(key);
            if (sit != func_array_size_params.end()) {
                src << sit->second;
                return;
            }
        }
        src << "/* unknown struct member size */";
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

    // Emit interleaved data pointer and size arguments for each
    // allocatable array member of a struct-typed function/subroutine
    // call argument, matching the parameter order in emit_function_def:
    // (data_member1, size_member1, data_member2, size_member2, ...)
    void emit_struct_member_args_interleaved(ASR::expr_t *expr) {
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

            // Emit data pointer for this member
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
                } else {
                    src << ", __data_" << var_name << "_"
                        << st->m_members[m];
                }
            } else {
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

            // Emit size for this member
            if (arr_it != struct_from_array_elem.end()) {
                std::string arr_name = arr_it->second.first;
                std::string idx_str = arr_it->second.second;
                std::string key = arr_name + "." + st->m_members[m];
                auto sit = struct_array_sizes_params.find(key);
                if (sit != struct_array_sizes_params.end()) {
                    src << ", " << sit->second << "[" << idx_str << "]";
                } else {
                    src << ", __size_" << var_name << "_"
                        << st->m_members[m];
                }
            } else {
                std::string key = var_name + "." + st->m_members[m];
                auto it = func_array_size_params.find(key);
                if (it != func_array_size_params.end()) {
                    src << ", " << it->second;
                } else {
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
    }


    bool needs_erf_helper = false;
    bool needs_erfc_helper = false;

    void scan_for_math_helpers(const ASR::TranslationUnit_t &tu) {
        GpuMathHelperScanner scanner;
        for (auto &item : tu.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::GpuKernelFunction_t>(*item.second)) {
                scanner.visit_symbol(*item.second);
            }
        }
        needs_erf_helper = scanner.needs_erf;
        needs_erfc_helper = scanner.needs_erfc;
    }

    void emit_math_helpers() {
        if (needs_erf_helper) {
            src << "// Abramowitz & Stegun approximation (7.1.26), max error ~1.5e-7\n";
            src << "inline float _lf_erf(float x) {\n";
            src << "    float ax = abs(x);\n";
            src << "    float t = 1.0f / (1.0f + 0.3275911f * ax);\n";
            src << "    float y = 1.0f - (((((1.061405429f * t + (-1.453152027f)) * t) + 1.421413741f) * t + (-0.284496736f)) * t + 0.254829592f) * t * exp(-(ax * ax));\n";
            src << "    return x < 0.0f ? -y : y;\n";
            src << "}\n\n";
        }
        if (needs_erfc_helper) {
            if (!needs_erf_helper) {
                src << "inline float _lf_erf(float x) {\n";
                src << "    float ax = abs(x);\n";
                src << "    float t = 1.0f / (1.0f + 0.3275911f * ax);\n";
                src << "    float y = 1.0f - (((((1.061405429f * t + (-1.453152027f)) * t) + 1.421413741f) * t + (-0.284496736f)) * t + 0.254829592f) * t * exp(-(ax * ax));\n";
                src << "    return x < 0.0f ? -y : y;\n";
                src << "}\n\n";
            }
            src << "inline float _lf_erfc(float x) {\n";
            src << "    return 1.0f - _lf_erf(x);\n";
            src << "}\n\n";
        }
    }

    void visit_TranslationUnit(const ASR::TranslationUnit_t &tu) {
        src << "#include <metal_stdlib>\n";
        src << "using namespace metal;\n\n";

        scan_for_math_helpers(tu);

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

        emit_math_helpers();

        emitted_funcs.clear();
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

    // Emit a Fortran function as a Metal inline function.
    // When out_addr_space is "thread", PointerArray Out params use thread.
    // When "device", they use device address space.
    void emit_function_def_impl(ASR::Function_t *fn,
            const std::string &metal_name,
            const std::string &out_addr_space) {
        func_array_size_params.clear();
        func_array_data_params.clear();
        func_array_params.clear();
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
            // Scalar struct-typed argument (not array-of-struct)
            if (ftype->m_arg_types[i] &&
                ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(ftype->m_arg_types[i]))
                && !is_array_type(arg->m_type)) {
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
                // FixedSizeArray and PointerArray out params (from
                // subroutine_from_function) receive local thread-space
                // temporaries; DescriptorArray params receive device
                // buffer pointers from the kernel.
                bool use_thread = (arr->m_physical_type
                    == ASR::array_physical_typeType::FixedSizeArray)
                    || (arr->m_physical_type
                    == ASR::array_physical_typeType::PointerArray
                    && (arg->m_intent == ASR::intentType::Out
                        || arg->m_intent == ASR::intentType::InOut));
                std::string addr_space = use_thread ? out_addr_space
                    : "device";
                std::string elem_type;
                if (is_struct_type(arr->m_type)) {
                    elem_type = get_struct_name(arg);
                } else {
                    elem_type = metal_type(arr->m_type);
                }
                src << addr_space << " "
                    << elem_type << "* " << arg->m_name;
                std::string size_name = std::string("__size_") + arg->m_name;
                src << ", int " << size_name;
                func_array_size_params[std::string(arg->m_name)] = size_name;
                if (use_thread) {
                    func_array_params.insert(std::string(arg->m_name));
                }
            } else if (ASRUtils::is_allocatable(arg->m_type)) {
                // Allocatable out parameter (from subroutine_from_function):
                // may be backed by thread-local stack (constant size) or
                // device VLA workspace (runtime size). Use out_addr_space
                // to generate both overloads.
                src << out_addr_space << " "
                    << metal_type(arg->m_type) << "* "
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
        // Pre-scan the inline function body for Allocate statements
        // and assignments from FixedSizeArray locals to allocatable
        // parameters, so we know sizes of function-internal allocatable
        // arrays (needed for array copy and broadcast assignments).
        {
            // Collect sizes of local FixedSizeArray variables
            std::map<std::string, int64_t> local_fixed_sizes;
            for (auto &sym : fn->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*sym.second)) continue;
                ASR::Variable_t *var =
                    ASR::down_cast<ASR::Variable_t>(sym.second);
                ASR::ttype_t *vtype = var->m_type;
                if (!ASR::is_a<ASR::Array_t>(*vtype)) continue;
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(vtype);
                if (arr->m_physical_type !=
                        ASR::array_physical_typeType::FixedSizeArray)
                    continue;
                int64_t total = 1;
                bool all_const = true;
                for (size_t d = 0; d < arr->n_dims; d++) {
                    if (arr->m_dims[d].m_length &&
                            ASR::is_a<ASR::IntegerConstant_t>(
                                *arr->m_dims[d].m_length)) {
                        total *= ASR::down_cast<ASR::IntegerConstant_t>(
                            arr->m_dims[d].m_length)->m_n;
                    } else {
                        all_const = false;
                        break;
                    }
                }
                if (all_const && total > 0) {
                    local_fixed_sizes[std::string(var->m_name)] = total;
                }
            }
            for (size_t i = 0; i < fn->n_body; i++) {
                if (fn->m_body[i]->type == ASR::stmtType::Allocate) {
                    ASR::Allocate_t *alloc =
                        ASR::down_cast<ASR::Allocate_t>(fn->m_body[i]);
                    for (size_t ai = 0; ai < alloc->n_args; ai++) {
                        if (!alloc->m_args[ai].m_a) continue;
                        if (!ASR::is_a<ASR::Var_t>(
                                *alloc->m_args[ai].m_a))
                            continue;
                        std::string vname = ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(
                                alloc->m_args[ai].m_a)->m_v);
                        int64_t sz = compute_alloc_size(
                            alloc->m_args[ai]);
                        if (sz > 0) {
                            alloc_array_sizes[vname] = sz;
                        }
                    }
                } else if (fn->m_body[i]->type ==
                        ASR::stmtType::Assignment) {
                    ASR::Assignment_t *asgn =
                        ASR::down_cast<ASR::Assignment_t>(
                            fn->m_body[i]);
                    if (!ASR::is_a<ASR::Var_t>(*asgn->m_target)) continue;
                    if (!ASR::is_a<ASR::Var_t>(*asgn->m_value)) continue;
                    std::string tgt_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(
                            asgn->m_target)->m_v);
                    std::string val_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(
                            asgn->m_value)->m_v);
                    auto fit = local_fixed_sizes.find(val_name);
                    if (fit != local_fixed_sizes.end() &&
                            !alloc_array_sizes.count(tgt_name)) {
                        alloc_array_sizes[tgt_name] = fit->second;
                    }
                }
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
        func_array_params.clear();
    }

    // Check if a function has any PointerArray Out/InOut parameter
    // that needs an address space overload (both thread and device).
    bool func_needs_device_overload(ASR::Function_t *fn) {
        for (size_t i = 0; i < fn->n_args; i++) {
            ASR::Variable_t *arg = ASR::down_cast<ASR::Variable_t>(
                ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
            if (is_array_type(arg->m_type)) {
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(
                    arg->m_type);
                if ((arr->m_physical_type
                        == ASR::array_physical_typeType::FixedSizeArray)
                    || (arr->m_physical_type
                        == ASR::array_physical_typeType::PointerArray
                        && (arg->m_intent == ASR::intentType::Out
                            || arg->m_intent == ASR::intentType::InOut))) {
                    return true;
                }
            } else if (ASRUtils::is_allocatable(arg->m_type)) {
                return true;
            }
        }
        return false;
    }

    // Emit a function with both thread and device overloads for
    // array out parameters when needed.
    void emit_function_def(ASR::Function_t *fn,
            const std::string &metal_name) {
        emit_function_def_impl(fn, metal_name, "thread");
        if (func_needs_device_overload(fn)) {
            emit_function_def_impl(fn, metal_name, "device");
        }
    }

    void visit_GpuKernelFunction(const ASR::GpuKernelFunction_t &x) {
        std::string name(x.m_name);
        local_alloc_arrays.clear();
        ptr_section_sizes.clear();

        // Pre-scan Allocate statements to determine sizes
        // for local allocatable array variables.
        prescan_alloc_sizes(x);

        // Collect function names actually called in the kernel body
        // so we only emit ExternalSymbol functions that are needed,
        // skipping struct method declarations that are merely referenced
        // by the struct type but never called.
        GpuFuncCallCollector kernel_call_collector;
        for (size_t i = 0; i < x.n_body; i++) {
            kernel_call_collector.visit_stmt(*x.m_body[i]);
        }

        // Collect all kernel functions (both ExternalSymbol-resolved TBPs
        // and duplicated Function_t entries) into a single map, then
        // topologically sort so callees are emitted before callers.
        {
            std::map<std::string, ASR::Function_t*> kernel_funcs;
            // Collect ExternalSymbol functions (type-bound procedures)
            for (auto &item : x.m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) continue;
                if (!kernel_call_collector.called.count(item.first)) continue;
                ASR::symbol_t *resolved = ASRUtils::symbol_get_past_external(
                    item.second);
                ASR::Function_t *fn = resolve_function(resolved);
                if (!fn) continue;
                std::string fn_name(fn->m_name);
                if (emitted_funcs.count(fn_name)) continue;
                kernel_funcs[fn_name] = fn;
            }
            // Collect internal functions duplicated into the kernel scope
            for (auto &item : x.m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(
                    item.second);
                std::string fn_name(fn->m_name);
                if (emitted_funcs.count(fn_name)) continue;
                if (kernel_funcs.count(fn_name)) continue;
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

        // Add per-dimension size scalars for DescriptorArray (assumed-shape)
        // kernel arguments so the kernel body can resolve ArrayBound UBound.
        for (size_t i = 0; i < args.size(); i++) {
            if (!args[i].is_array) continue;
            // Skip synthetic kernel args (__data_*, __size_*, etc.)
            if (args[i].name.substr(0, 2) == "__") continue;
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_args[i]);
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                ASRUtils::symbol_get_past_external(v->m_v));
            ASR::ttype_t *past_alloc =
                ASRUtils::type_get_past_allocatable(var->m_type);
            if (!ASR::is_a<ASR::Array_t>(*past_alloc)) continue;
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(past_alloc);
            bool has_null_dim = false;
            for (size_t d = 0; d < arr->n_dims; d++) {
                if (!arr->m_dims[d].m_length) {
                    has_null_dim = true;
                    break;
                }
            }
            if (!has_null_dim) continue;
            for (size_t d = 0; d < arr->n_dims; d++) {
                std::string dim_size_name = "__size_" + args[i].name
                    + "_dim" + std::to_string(d + 1);
                scalar_args.push_back({dim_size_name, "int"});
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
                    args[i].name, elem_type,
                    args[i].is_struct && !args[i].is_array,
                    args[i].struct_name, byte_size, current_offset
                });

                if (byte_size > 0) {
                    current_offset += byte_size;
                }

                // For array-of-struct args with allocatable members,
                // also pack the member data/offsets/sizes buffers
                if (args[i].is_array && !args[i].struct_name.empty()) {
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
                            packed_arrays.push_back({
                                data_name, et, false, "", 0, 0});
                            std::string off_name =
                                "__offsets_" + args[i].name + "_"
                                + st->m_members[m];
                            packed_arrays.push_back({
                                off_name, "int", false, "", 0, 0});
                            std::string sizes_name =
                                "__sizes_" + args[i].name + "_"
                                + st->m_members[m];
                            packed_arrays.push_back({
                                sizes_name, "int", false, "", 0, 0});
                        }
                    }
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
            // Look up the variable in block scopes
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
            // Also look up in kernel scope (for allocatable VLAs)
            if (!vla_var) {
                ASR::symbol_t *sym = x.m_symtab->resolve_symbol(
                    current_vla_infos[v].var_name);
                if (sym && ASR::is_a<ASR::Variable_t>(*sym)) {
                    vla_var = ASR::down_cast<ASR::Variable_t>(sym);
                }
            }
            std::string elem_type_str = "float";
            if (vla_var) {
                ASR::ttype_t *vla_type = ASRUtils::type_get_past_allocatable(
                    vla_var->m_type);
                if (ASR::is_a<ASR::Array_t>(*vla_type)) {
                    elem_type_str = metal_type(
                        ASR::down_cast<ASR::Array_t>(vla_type)->m_type);
                }
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

        // Register per-dimension size params for DescriptorArray
        // (assumed-shape) kernel arguments so ArrayBound UBound can
        // resolve them.
        for (size_t i = 0; i < args.size(); i++) {
            if (!args[i].is_array) continue;
            if (args[i].name.substr(0, 2) == "__") continue;
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_args[i]);
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                ASRUtils::symbol_get_past_external(v->m_v));
            ASR::ttype_t *past_alloc =
                ASRUtils::type_get_past_allocatable(var->m_type);
            if (!ASR::is_a<ASR::Array_t>(*past_alloc)) continue;
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(past_alloc);
            bool has_null_dim = false;
            for (size_t d = 0; d < arr->n_dims; d++) {
                if (!arr->m_dims[d].m_length) {
                    has_null_dim = true;
                    break;
                }
            }
            if (!has_null_dim) continue;
            for (size_t d = 0; d < arr->n_dims; d++) {
                std::string dim_key = args[i].name + "__dim"
                    + std::to_string(d + 1);
                std::string dim_var = "__size_" + args[i].name
                    + "_dim" + std::to_string(d + 1);
                func_array_size_params[dim_key] = dim_var;
            }
            // Also register the total flat size as the product of
            // per-dimension sizes so existing ArraySize lookups work.
            std::string total_expr = "__size_" + args[i].name
                + "_dim1";
            for (size_t d = 1; d < arr->n_dims; d++) {
                total_expr += " * __size_" + args[i].name
                    + "_dim" + std::to_string(d + 1);
            }
            func_array_size_params[args[i].name] = "("
                + total_expr + ")";
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
                bool target_is_local_alloc = false;
                bool target_is_func_array_param = false;
                if (ASR::is_a<ASR::Var_t>(*a->m_target)) {
                    std::string tname = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(a->m_target)->m_v);
                    if (alloc_pointer_params.count(tname)) {
                        deref_target = true;
                    }
                    if (local_alloc_arrays.count(tname)) {
                        target_is_local_alloc = true;
                    }
                    if (func_array_params.count(tname)) {
                        target_is_func_array_param = true;
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
                // Check if RHS is a local alloc array Var (not subscripted)
                bool rhs_is_local_alloc = false;
                if (ASR::is_a<ASR::Var_t>(*a->m_value)) {
                    std::string rname = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(a->m_value)->m_v);
                    rhs_is_local_alloc = local_alloc_arrays.count(rname) > 0;
                }
                if (target_is_local_alloc && rhs_is_array_or_alloc) {
                    // Copy between local allocatable arrays
                    std::string tname = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(a->m_target)->m_v);
                    auto sit = alloc_array_sizes.find(tname);
                    int64_t sz = (sit != alloc_array_sizes.end())
                        ? sit->second : 1;
                    bool rhs_is_simple_var = ASR::is_a<ASR::Var_t>(
                        *a->m_value);
                    for (int64_t ei = 0; ei < sz; ei++) {
                        visit_expr(a->m_target);
                        src << "[" << ei << "] = ";
                        if (rhs_is_simple_var) {
                            visit_expr(a->m_value);
                            src << "[" << ei << "]";
                        } else {
                            array_elem_index = ei;
                            visit_expr(a->m_value);
                            array_elem_index = -1;
                        }
                        src << ";\n";
                        if (ei + 1 < sz) src << get_indent();
                    }
                } else if (!target_is_local_alloc && rhs_is_local_alloc) {
                    if (target_is_array_buffer) {
                        // Target is a fixed-size array, RHS is a local
                        // alloc array: copy element-wise since C-style
                        // arrays are not assignable in Metal.
                        std::string rname = ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(a->m_value)->m_v);
                        auto sit = alloc_array_sizes.find(rname);
                        int64_t sz = (sit != alloc_array_sizes.end())
                            ? sit->second : 1;
                        for (int64_t ei = 0; ei < sz; ei++) {
                            visit_expr(a->m_target);
                            src << "[" << ei << "] = ";
                            visit_expr(a->m_value);
                            src << "[" << ei << "];\n";
                            if (ei + 1 < sz) src << get_indent();
                        }
                    } else {
                        // RHS is a local alloc array but target is a scalar
                        // (e.g., a(i) = alloc_var): index with [0]
                        visit_expr(a->m_target);
                        src << " = ";
                        visit_expr(a->m_value);
                        src << "[0];\n";
                    }
                } else if (deref_target && rhs_is_array_or_alloc) {
                    // Array copy to allocatable pointer param (e.g.,
                    // v = array_const where v is thread T*). Copy
                    // element-wise using the known size.
                    std::string tname = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(a->m_target)->m_v);
                    auto sit = alloc_array_sizes.find(tname);
                    if (sit != alloc_array_sizes.end() && sit->second > 1) {
                        for (int64_t ei = 0; ei < sit->second; ei++) {
                            visit_expr(a->m_target);
                            src << "[" << ei << "] = ";
                            visit_expr(a->m_value);
                            src << "[" << ei << "];\n";
                            if (ei + 1 < sit->second) src << get_indent();
                        }
                    } else if (ASR::is_a<ASR::Var_t>(*a->m_value)) {
                        // Target size unknown: try using the RHS
                        // array's size (e.g., r = a where a has a
                        // known func_array_size_params entry).
                        std::string rname = ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(a->m_value)->m_v);
                        auto rpit = func_array_size_params.find(rname);
                        if (rpit != func_array_size_params.end()) {
                            src << "for (int __copy_i = 0; __copy_i < "
                                << rpit->second
                                << "; __copy_i++) ";
                            visit_expr(a->m_target);
                            src << "[__copy_i] = ";
                            visit_expr(a->m_value);
                            src << "[__copy_i];\n";
                        } else {
                            auto rsit = alloc_array_sizes.find(rname);
                            if (rsit != alloc_array_sizes.end() &&
                                    rsit->second > 1) {
                                for (int64_t ei = 0;
                                        ei < rsit->second; ei++) {
                                    visit_expr(a->m_target);
                                    src << "[" << ei << "] = ";
                                    visit_expr(a->m_value);
                                    src << "[" << ei << "];\n";
                                    if (ei + 1 < rsit->second)
                                        src << get_indent();
                                }
                            } else {
                                src << "*";
                                visit_expr(a->m_target);
                                src << " = ";
                                visit_expr(a->m_value);
                                src << "[0];\n";
                            }
                        }
                    } else {
                        src << "*";
                        visit_expr(a->m_target);
                        src << " = ";
                        visit_expr(a->m_value);
                        src << "[0];\n";
                    }
                } else if (target_is_func_array_param && rhs_is_array_or_alloc) {
                    // Target is an array parameter (thread T*) and RHS is
                    // a fixed-size array (e.g., struct member). Copy
                    // element-by-element using the size parameter.
                    std::string tname = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(a->m_target)->m_v);
                    auto sit = func_array_size_params.find(tname);
                    std::string size_var = (sit != func_array_size_params.end())
                        ? sit->second : "1";
                    src << "for (int __copy_i = 0; __copy_i < "
                        << size_var << "; __copy_i++) ";
                    visit_expr(a->m_target);
                    src << "[__copy_i] = ";
                    visit_expr(a->m_value);
                    src << "[__copy_i];\n";
                } else if (target_is_array_buffer) {
                    // Compute the fixed size of the target array
                    int64_t fixed_sz = 1;
                    ASR::ttype_t *tgt_inner =
                        ASRUtils::type_get_past_allocatable_pointer(tgt_type);
                    if (ASR::is_a<ASR::Array_t>(*tgt_inner)) {
                        ASR::Array_t *tarr =
                            ASR::down_cast<ASR::Array_t>(tgt_inner);
                        int64_t total = 1;
                        bool all_const = true;
                        for (size_t d = 0; d < tarr->n_dims; d++) {
                            if (tarr->m_dims[d].m_length &&
                                    ASR::is_a<ASR::IntegerConstant_t>(
                                        *tarr->m_dims[d].m_length)) {
                                total *=
                                    ASR::down_cast<ASR::IntegerConstant_t>(
                                        tarr->m_dims[d].m_length)->m_n;
                            } else {
                                all_const = false;
                                break;
                            }
                        }
                        if (all_const && total > 0) fixed_sz = total;
                    }
                    bool rhs_is_simple_var =
                        ASR::is_a<ASR::Var_t>(*a->m_value);
                    for (int64_t ei = 0; ei < fixed_sz; ei++) {
                        visit_expr(a->m_target);
                        src << "[" << ei << "] = ";
                        if (rhs_is_array_or_alloc && !rhs_is_simple_var) {
                            array_elem_index = ei;
                            visit_expr(a->m_value);
                            array_elem_index = -1;
                        } else {
                            visit_expr(a->m_value);
                            if (rhs_is_array_or_alloc) {
                                src << "[" << ei << "]";
                            }
                        }
                        src << ";\n";
                        if (ei + 1 < fixed_sz) src << get_indent();
                    }
                } else if (deref_target && !rhs_is_array_or_alloc) {
                    // Scalar broadcast to allocatable array parameter:
                    // v = 1.0 where v is thread T* with known size
                    std::string tname = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(a->m_target)->m_v);
                    auto sit = alloc_array_sizes.find(tname);
                    if (sit != alloc_array_sizes.end() && sit->second > 1) {
                        for (int64_t ei = 0; ei < sit->second; ei++) {
                            visit_expr(a->m_target);
                            src << "[" << ei << "] = ";
                            visit_expr(a->m_value);
                            src << ";\n";
                            if (ei + 1 < sit->second) src << get_indent();
                        }
                    } else {
                        src << "*";
                        visit_expr(a->m_target);
                        src << " = ";
                        visit_expr(a->m_value);
                        src << ";\n";
                    }
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
                // On Metal, ArrayIsContiguous is always true.
                // If the condition is LogicalNot(ArrayIsContiguous),
                // skip the then-branch (dead code) and only emit
                // the else-branch to avoid generating invalid Metal
                // code from the non-contiguous copy path.
                bool cond_always_false = false;
                if (ASR::is_a<ASR::LogicalNot_t>(*if_stmt->m_test)) {
                    ASR::LogicalNot_t *ln =
                        ASR::down_cast<ASR::LogicalNot_t>(
                            if_stmt->m_test);
                    if (ASR::is_a<ASR::ArrayIsContiguous_t>(*ln->m_arg)) {
                        cond_always_false = true;
                    }
                }
                bool cond_always_true = false;
                if (ASR::is_a<ASR::ArrayIsContiguous_t>(
                        *if_stmt->m_test)) {
                    cond_always_true = true;
                }
                if (cond_always_false) {
                    for (size_t i = 0; i < if_stmt->n_orelse; i++) {
                        visit_stmt(if_stmt->m_orelse[i]);
                    }
                } else if (cond_always_true) {
                    for (size_t i = 0; i < if_stmt->n_body; i++) {
                        visit_stmt(if_stmt->m_body[i]);
                    }
                } else {
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
                }
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
                            if (vla_it->dims[0].is_constant) {
                                src << vla_it->dims[0].constant_value;
                            } else {
                                visit_expr(vla_it->dims[0].dim_expr);
                            }
                        } else {
                            src << "(";
                            for (size_t d = 0;
                                    d < vla_it->dims.size(); d++) {
                                if (d > 0) src << " * ";
                                if (vla_it->dims[d].is_constant) {
                                    src << vla_it->dims[d].constant_value;
                                } else {
                                    visit_expr(vla_it->dims[d].dim_expr);
                                }
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
                        // But if the local var is already a fixed-size array
                        // (local_alloc_arrays), it decays to a pointer
                        // automatically, so skip the '&'.
                        if (ASRUtils::is_allocatable(arg_type)) {
                            bool is_local_arr = false;
                            if (ASR::is_a<ASR::Var_t>(
                                    *sc->m_args[i].m_value)) {
                                std::string aname = ASRUtils::symbol_name(
                                    ASR::down_cast<ASR::Var_t>(
                                        sc->m_args[i].m_value)->m_v);
                                is_local_arr =
                                    local_alloc_arrays.count(aname) > 0;
                            }
                            if (!is_local_arr) {
                                src << "&";
                            }
                        }
                        visit_expr(sc->m_args[i].m_value);
                        if (is_array_type(arg_type)
                                && !ASRUtils::is_allocatable(arg_type)) {
                            src << ", ";
                            emit_array_size_expr(sc->m_args[i].m_value);
                        } else if (is_struct_type(arg_type)) {
                            emit_struct_member_args_interleaved(
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
            case ASR::stmtType::Allocate: {
                // Skip allocation on Metal GPU — local allocatable arrays
                // are declared as fixed-size arrays based on pre-scanned
                // Allocate dimensions.
                break;
            }
            case ASR::stmtType::ImplicitDeallocate: {
                // Skip implicit deallocation on Metal GPU
                break;
            }
            case ASR::stmtType::Associate: {
                ASR::Associate_t *assoc = ASR::down_cast<ASR::Associate_t>(stmt);
                last_section_size.clear();
                src << get_indent();
                visit_expr(assoc->m_target);
                src << " = ";
                emit_array_section_pointer(assoc->m_value);
                src << ";\n";
                if (ASR::is_a<ASR::Var_t>(*assoc->m_target)) {
                    std::string tgt_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(assoc->m_target)->m_v);
                    if (!last_section_size.empty()) {
                        ptr_section_sizes[tgt_name] = last_section_size;
                    }
                    // For pointer-to-local-alloc associations, record
                    // the allocatable's size for later use
                    if (ASR::is_a<ASR::Var_t>(*assoc->m_value)) {
                        std::string val_name = ASRUtils::symbol_name(
                            ASR::down_cast<ASR::Var_t>(
                                assoc->m_value)->m_v);
                        auto sit = alloc_array_sizes.find(val_name);
                        if (sit != alloc_array_sizes.end()) {
                            ptr_section_sizes[tgt_name] =
                                std::to_string(sit->second);
                        } else {
                            auto eit =
                                alloc_array_size_exprs.find(val_name);
                            if (eit != alloc_array_size_exprs.end()) {
                                ptr_section_sizes[tgt_name] =
                                    eit->second;
                            }
                        }
                    }
                }
                break;
            }
            default:
                break;
        }
    }

    void visit_expr(ASR::expr_t *expr) {
        switch (expr->type) {
            case ASR::exprType::IfExp: {
                ASR::IfExp_t *ie = ASR::down_cast<ASR::IfExp_t>(expr);
                if (ie->m_value) {
                    visit_expr(ie->m_value);
                } else {
                    src << "((";
                    visit_expr(ie->m_test);
                    src << ") ? (";
                    visit_expr(ie->m_body);
                    src << ") : (";
                    visit_expr(ie->m_orelse);
                    src << "))";
                }
                break;
            }
            case ASR::exprType::Var: {
                ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(expr);
                src << ASRUtils::symbol_name(v->m_v);
                if (array_elem_index >= 0) {
                    ASR::ttype_t *vtype = ASRUtils::expr_type(expr);
                    ASR::ttype_t *inner = ASRUtils::type_get_past_allocatable_pointer(vtype);
                    if (ASR::is_a<ASR::Array_t>(*inner)) {
                        src << "[" << array_elem_index << "]";
                    }
                }
                break;
            }
            case ASR::exprType::IntegerConstant: {
                ASR::IntegerConstant_t *c = ASR::down_cast<ASR::IntegerConstant_t>(expr);
                src << c->m_n;
                break;
            }
            case ASR::exprType::RealConstant: {
                ASR::RealConstant_t *c = ASR::down_cast<ASR::RealConstant_t>(expr);
                src << double_to_scientific(c->m_r);
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
                                } else if (ASR::is_a<ASR::StructInstanceMember_t>(*ab->m_v)) {
                                    emit_struct_member_array_size(ab->m_v);
                                } else if (ASR::is_a<ASR::Var_t>(*ab->m_v)) {
                                    std::string vname = ASRUtils::symbol_name(
                                        ASR::down_cast<ASR::Var_t>(ab->m_v)->m_v);
                                    auto pit = ptr_section_sizes.find(vname);
                                    if (pit != ptr_section_sizes.end()) {
                                        src << pit->second;
                                    } else {
                                        // Try per-dimension key first
                                        // (for assumed-shape kernel args)
                                        std::string dim_key = vname
                                            + "__dim"
                                            + std::to_string(dim_idx + 1);
                                        auto dpit =
                                            func_array_size_params.find(
                                                dim_key);
                                        if (dpit !=
                                                func_array_size_params
                                                    .end()) {
                                            src << dpit->second;
                                        } else {
                                        auto fpit = func_array_size_params.find(vname);
                                        if (fpit != func_array_size_params.end()) {
                                            src << fpit->second;
                                        } else {
                                            auto eit = alloc_array_size_exprs.find(vname);
                                            if (eit != alloc_array_size_exprs.end()) {
                                                src << eit->second;
                                            } else {
                                                auto sit = alloc_array_sizes.find(vname);
                                                if (sit != alloc_array_sizes.end()) {
                                                    src << sit->second;
                                                } else {
                                                    src << "/* unknown ubound for '" << vname << "' */";
                                                }
                                            }
                                        }
                                        }
                                    }
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
                } else if (fn_name.find("_lcompilers_erf_") == 0) {
                    src << "_lf_erf(";
                    if (fc->n_args > 0 && fc->m_args[0].m_value)
                        visit_expr(fc->m_args[0].m_value);
                    src << ")";
                } else if (fn_name.find("_lcompilers_erfc_") == 0 &&
                           fn_name.find("_lcompilers_erfc_scaled_") != 0) {
                    src << "_lf_erfc(";
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
                } else if (fn_name.find("_lcompilers_merge_") == 0) {
                    // merge(tsource, fsource, mask) -> (mask ? tsource : fsource)
                    src << "(";
                    if (fc->n_args > 2 && fc->m_args[2].m_value)
                        visit_expr(fc->m_args[2].m_value);
                    src << " ? ";
                    if (fc->n_args > 0 && fc->m_args[0].m_value)
                        visit_expr(fc->m_args[0].m_value);
                    src << " : ";
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
                                emit_struct_member_args_interleaved(
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
                    emit_struct_member_array_size(as->m_v);
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
            case ASR::exprType::ArrayIsContiguous: {
                src << "1";
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
        // Extract array variable name for assumed-shape __size_ parameters
        std::string arr_var_name;
        if (ASR::is_a<ASR::Var_t>(*ai->m_v)) {
            arr_var_name = ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(ai->m_v)->m_v);
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
                } else if (!arr_var_name.empty()) {
                    len_str = "__size_" + arr_var_name + "_dim"
                        + std::to_string(d + 1);
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
        } else if (id == static_cast<int64_t>(IEF::Erf)) {
            needs_erf_helper = true;
            src << "_lf_erf(";
            visit_expr(f->m_args[0]);
            src << ")";
        } else if (id == static_cast<int64_t>(IEF::Erfc)) {
            needs_erfc_helper = true;
            src << "_lf_erfc(";
            visit_expr(f->m_args[0]);
            src << ")";
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

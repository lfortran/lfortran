#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/codegen/asr_to_metal.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/codegen/c_utils.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/string_utils.h>
#include <libasr/pass/intrinsic_functions.h>

#include <sstream>
#include <map>
#include <set>
#include <vector>

namespace LCompilers {

class ASRToMetalVisitor
{
public:
    std::stringstream src;
    int indent_level;
    CompilerOptions &co;

    // VLA info collected during kernel signature generation,
    // used by the BlockCall handler to emit device pointer offsets.
    std::vector<GpuVlaWorkspace> current_vla_infos;

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
            src << get_indent() << metal_type(arr->m_type) << " "
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

    // Get dimension sizes for linearized indexing
    std::vector<int64_t> get_dim_sizes(ASR::ttype_t *type) {
        std::vector<int64_t> sizes;
        if (type->type != ASR::ttypeType::Array) return sizes;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
        for (size_t i = 0; i < arr->n_dims; i++) {
            if (arr->m_dims[i].m_length &&
                ASR::is_a<ASR::IntegerConstant_t>(*arr->m_dims[i].m_length)) {
                sizes.push_back(ASR::down_cast<ASR::IntegerConstant_t>(
                    arr->m_dims[i].m_length)->m_n);
            } else {
                sizes.push_back(0);
            }
        }
        return sizes;
    }

    void visit_TranslationUnit(const ASR::TranslationUnit_t &tu) {
        src << "#include <metal_stdlib>\n";
        src << "using namespace metal;\n\n";

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

    // Emit a Metal struct definition for a Struct symbol
    void emit_struct_def(ASR::Struct_t *st) {
        src << "struct " << st->m_name << " {\n";
        for (size_t i = 0; i < st->n_members; i++) {
            ASR::symbol_t *mem = st->m_symtab->get_symbol(st->m_members[i]);
            if (mem && ASR::is_a<ASR::Variable_t>(*mem)) {
                ASR::Variable_t *mv = ASR::down_cast<ASR::Variable_t>(mem);
                if (is_struct_type(mv->m_type)) {
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
            // Skip 'self'/'class' argument (pass attribute)
            if (ftype->m_arg_types[i] &&
                ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(ftype->m_arg_types[i]))) {
                continue;
            }
            if (!first) src << ", ";
            first = false;
            src << metal_type(arg->m_type) << " " << arg->m_name;
        }
        src << ") {\n";
        indent_level++;
        // Declare return variable if present
        if (fn->m_return_var) {
            ASR::Variable_t *rv = ASR::down_cast<ASR::Variable_t>(
                ASR::down_cast<ASR::Var_t>(fn->m_return_var)->m_v);
            src << get_indent() << ret_type << " " << rv->m_name << ";\n";
        }
        for (size_t i = 0; i < fn->n_body; i++) {
            visit_stmt(fn->m_body[i]);
        }
        if (fn->m_return_var) {
            ASR::Variable_t *rv = ASR::down_cast<ASR::Variable_t>(
                ASR::down_cast<ASR::Var_t>(fn->m_return_var)->m_v);
            src << get_indent() << "return " << rv->m_name << ";\n";
        }
        indent_level--;
        src << "}\n\n";
    }

    void visit_GpuKernelFunction(const ASR::GpuKernelFunction_t &x) {
        std::string name(x.m_name);

        // Emit struct type definitions in dependency order
        std::set<std::string> emitted_structs;
        std::vector<ASR::Struct_t*> ordered_structs;
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Struct_t>(*item.second)) {
                collect_structs_ordered(
                    ASR::down_cast<ASR::Struct_t>(item.second),
                    x.m_symtab, emitted_structs, ordered_structs);
            }
        }
        for (ASR::Struct_t *st : ordered_structs) {
            emit_struct_def(st);
        }

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
            std::string sn = is_st ? get_struct_name(var) : "";
            args.push_back({arg_name, type, is_array_type(type), is_st, sn});
        }

        // Analyze blocks in the kernel body for VLAs (arrays with
        // non-constant dimensions) using the shared GPU utility.
        current_vla_infos = analyze_gpu_vla_workspaces(x);

        // Emit kernel signature
        src << "kernel void " << name << "(\n";

        int buffer_idx = 0;
        bool has_prev = false;
        for (size_t i = 0; i < args.size(); i++) {
            if (has_prev) src << ",\n";
            src << "    ";
            if (args[i].is_array) {
                src << "device " << metal_type(args[i].type) << "* "
                    << args[i].name << " [[buffer(" << buffer_idx++ << ")]]";
            } else if (args[i].is_struct) {
                src << "device " << args[i].struct_name << "& "
                    << args[i].name << " [[buffer(" << buffer_idx++ << ")]]";
            } else {
                src << "constant " << metal_type(args[i].type) << "& "
                    << args[i].name << " [[buffer(" << buffer_idx++ << ")]]";
            }
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
                src << get_indent();
                visit_expr(a->m_target);
                src << " = ";
                visit_expr(a->m_value);
                src << ";\n";
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
                    ASR::ttype_t *arr_type = ASRUtils::expr_type(ab->m_v);
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
                            // Skip struct-typed arguments (self/class)
                            ASR::ttype_t *arg_type = ASRUtils::expr_type(
                                fc->m_args[i].m_value);
                            if (is_struct_type(arg_type)) continue;
                            if (!first_arg) src << ", ";
                            first_arg = false;
                            visit_expr(fc->m_args[i].m_value);
                        }
                    }
                    src << ")";
                }
                break;
            }
            case ASR::exprType::ArrayItem: {
                ASR::ArrayItem_t *ai = ASR::down_cast<ASR::ArrayItem_t>(expr);
                visit_expr(ai->m_v);
                src << "[";
                // Compute linearized 0-based index for multi-dim arrays
                // Fortran: column-major, 1-based
                // Metal: flat 0-based buffer
                // For a(i,j) with dims (m,n): index = (j-1)*m + (i-1)
                ASR::ttype_t *arr_type = ASRUtils::expr_type(ai->m_v);
                std::vector<int64_t> dim_sizes = get_dim_sizes(arr_type);

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
                    emit_linearized_index(ai, dim_sizes);
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
                visit_expr(sm->m_v);
                src << ".";
                ASR::symbol_t *mem = ASRUtils::symbol_get_past_external(sm->m_m);
                src << ASRUtils::symbol_name(mem);
                break;
            }
            case ASR::exprType::ArrayPhysicalCast: {
                ASR::ArrayPhysicalCast_t *apc =
                    ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr);
                visit_expr(apc->m_arg);
                break;
            }
            default:
                src << "/* unsupported expr type " << expr->type << " */";
                break;
        }
    }

    void emit_linearized_index(ASR::ArrayItem_t *ai,
                               std::vector<int64_t> &dim_sizes) {
        // Column-major linearization: index = sum_d( (idx_d - 1) * stride_d )
        // stride_0 = 1, stride_1 = dim[0], stride_2 = dim[0]*dim[1], ...
        bool first = true;
        int64_t stride = 1;
        for (size_t d = 0; d < ai->n_args; d++) {
            ASR::expr_t *idx = ai->m_args[d].m_right ?
                ai->m_args[d].m_right : ai->m_args[d].m_left;
            if (!idx) continue;
            if (!first) src << " + ";
            first = false;
            if (stride == 1) {
                src << "((int)(";
                visit_expr(idx);
                src << ") - 1)";
            } else {
                src << "(" << stride << " * ((int)(";
                visit_expr(idx);
                src << ") - 1))";
            }
            if (d < dim_sizes.size()) {
                stride *= dim_sizes[d];
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

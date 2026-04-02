#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/codegen/asr_to_metal.h>
#include <libasr/codegen/c_utils.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/string_utils.h>

#include <sstream>
#include <map>

namespace LCompilers {

class ASRToMetalVisitor
{
public:
    std::stringstream src;
    int indent_level;

    ASRToMetalVisitor() : indent_level(0) {}

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
                int kind = ASR::down_cast<ASR::Real_t>(type)->m_kind;
                if (kind == 4) return "float";
                if (kind == 8) return "double";
                return "float";
            }
            case ASR::ttypeType::Array: {
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
                return metal_type(arr->m_type);
            }
            default:
                return "float";
        }
    }

    bool is_array_type(ASR::ttype_t *type) {
        return type->type == ASR::ttypeType::Array;
    }

    ASR::ttype_t* get_element_type(ASR::ttype_t *type) {
        if (type->type == ASR::ttypeType::Array) {
            return ASR::down_cast<ASR::Array_t>(type)->m_type;
        }
        return type;
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

    void visit_GpuKernelFunction(const ASR::GpuKernelFunction_t &x) {
        std::string name(x.m_name);

        // Categorize args into buffers and scalars
        struct ArgInfo {
            std::string name;
            ASR::ttype_t *type;
            bool is_array;
        };
        std::vector<ArgInfo> args;
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_args[i]);
            std::string arg_name(ASRUtils::symbol_name(v->m_v));
            ASR::ttype_t *type = ASRUtils::symbol_type(v->m_v);
            args.push_back({arg_name, type, is_array_type(type)});
        }

        src << "kernel void " << name << "(\n";

        int buffer_idx = 0;
        for (size_t i = 0; i < args.size(); i++) {
            src << "    ";
            if (args[i].is_array) {
                src << "device " << metal_type(args[i].type) << "* "
                    << args[i].name << " [[buffer(" << buffer_idx++ << ")]]";
            } else {
                src << "constant " << metal_type(args[i].type) << "& "
                    << args[i].name << " [[buffer(" << buffer_idx++ << ")]]";
            }
            if (i < args.size() - 1) {
                src << ",\n";
            }
        }

        // Add thread_position_in_grid for 1D
        if (!args.empty()) src << ",\n";
        src << "    uint __thread_id [[thread_position_in_grid]]";

        src << ")\n{\n";
        indent_level++;

        // Declare local variables (non-argument variables in kernel scope)
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(item.second);
                // Skip arguments (already declared as kernel parameters)
                bool is_arg = false;
                for (size_t i = 0; i < args.size(); i++) {
                    if (args[i].name == std::string(var->m_name)) {
                        is_arg = true;
                        break;
                    }
                }
                if (!is_arg) {
                    src << get_indent() << metal_type(var->m_type) << " "
                        << var->m_name << ";\n";
                }
            }
        }

        // Emit kernel body (skip the loop variable assignment and bounds guard
        // since we handle them specially)
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
                src << get_indent() << "}\n";
                break;
            }
            case ASR::stmtType::Return: {
                src << get_indent() << "return;\n";
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
                src << "(";
                visit_expr(op->m_left);
                src << " " << binop_str(op->m_op) << " ";
                visit_expr(op->m_right);
                src << ")";
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
            case ASR::exprType::Cast: {
                ASR::Cast_t *c = ASR::down_cast<ASR::Cast_t>(expr);
                std::string target_type = metal_type(c->m_type);
                src << "((" << target_type << ")";
                visit_expr(c->m_arg);
                src << ")";
                break;
            }
            case ASR::exprType::ArrayItem: {
                ASR::ArrayItem_t *ai = ASR::down_cast<ASR::ArrayItem_t>(expr);
                visit_expr(ai->m_v);
                src << "[";
                // Metal uses 0-based indexing; Fortran uses 1-based
                // The index expression already includes the Fortran offset via
                // the loop variable computation, so we subtract 1 here
                src << "(int)(";
                if (ai->n_args > 0 && ai->m_args[0].m_right) {
                    visit_expr(ai->m_args[0].m_right);
                } else if (ai->n_args > 0 && ai->m_args[0].m_left) {
                    visit_expr(ai->m_args[0].m_left);
                }
                src << ") - 1]";
                break;
            }
            case ASR::exprType::GpuThreadIndex: {
                src << "__thread_id";
                break;
            }
            case ASR::exprType::GpuBlockIndex: {
                // Not used with thread_position_in_grid
                src << "0";
                break;
            }
            case ASR::exprType::GpuBlockSize: {
                src << "0";
                break;
            }
            default:
                src << "/* unsupported expr type " << expr->type << " */";
                break;
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
    diag::Diagnostics &diagnostics, CompilerOptions & /*co*/)
{
    ASRToMetalVisitor v;
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

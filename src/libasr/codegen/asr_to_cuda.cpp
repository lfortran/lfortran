#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/codegen/asr_to_cuda.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/codegen/c_utils.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/string_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>

#include <sstream>
#include <map>
#include <set>
#include <vector>

namespace LCompilers {

class ASRToCudaVisitor {
public:
    std::stringstream src;
    CompilerOptions &co;
    int indent_level = 0;

    // Maps for tracking array size parameters
    std::map<std::string, std::string> func_array_size_params;

    ASRToCudaVisitor(CompilerOptions &co) : co(co) {}

    std::string get_indent() {
        return std::string(indent_level * 4, ' ');
    }

    std::string cuda_type(ASR::ttype_t *type) {
        type = ASRUtils::type_get_past_allocatable(type);
        if (ASR::is_a<ASR::Array_t>(*type)) {
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
            return cuda_type(arr->m_type);
        }
        switch (type->type) {
            case ASR::ttypeType::Integer: {
                int kind = ASR::down_cast<ASR::Integer_t>(type)->m_kind;
                switch (kind) {
                    case 1: return "int8_t";
                    case 2: return "int16_t";
                    case 4: return "int";
                    case 8: return "long long";
                    default: return "int";
                }
            }
            case ASR::ttypeType::Real: {
                int kind = ASR::down_cast<ASR::Real_t>(type)->m_kind;
                return (kind == 8) ? "double" : "float";
            }
            case ASR::ttypeType::Logical:
                return "int";
            default:
                return "int";
        }
    }

    bool is_array_type(ASR::ttype_t *type) {
        type = ASRUtils::type_get_past_allocatable(type);
        return ASR::is_a<ASR::Array_t>(*type);
    }

    std::string binop_str(ASR::binopType op) {
        switch (op) {
            case ASR::binopType::Add: return "+";
            case ASR::binopType::Sub: return "-";
            case ASR::binopType::Mul: return "*";
            case ASR::binopType::Div: return "/";
            case ASR::binopType::Pow: return "**"; // handled specially
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

    void visit_TranslationUnit(const ASR::TranslationUnit_t &tu) {
        src << "#include <stdint.h>\n\n";

        // Collect kernel names for registration
        std::vector<std::string> kernel_names;

        for (auto &item : tu.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::GpuKernelFunction_t>(*item.second)) {
                ASR::GpuKernelFunction_t &kf =
                    *ASR::down_cast<ASR::GpuKernelFunction_t>(item.second);
                kernel_names.push_back(std::string(kf.m_name));
                visit_GpuKernelFunction(kf);
            }
        }

        // Emit kernel registration
        src << "\n// Auto-generated kernel registration\n";
        src << "typedef void (*kernel_func_t)(void);\n";
        src << "extern \"C\" void lfortran_gpu_register_kernel("
               "const char *name, kernel_func_t func);\n\n";
        src << "struct _lfortran_cuda_registrar {\n";
        src << "    _lfortran_cuda_registrar() {\n";
        for (auto &kname : kernel_names) {
            src << "        lfortran_gpu_register_kernel(\""
                << kname << "\", (kernel_func_t)" << kname << ");\n";
        }
        src << "    }\n";
        src << "} _lfortran_cuda_reg;\n";
    }

    void visit_GpuKernelFunction(const ASR::GpuKernelFunction_t &x) {
        std::string name(x.m_name);

        struct ArgInfo {
            std::string name;
            ASR::ttype_t *type;
            bool is_array;
        };
        std::vector<ArgInfo> args;
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_args[i]);
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                ASRUtils::symbol_get_past_external(v->m_v));
            std::string arg_name(var->m_name);
            ASR::ttype_t *type = var->m_type;
            args.push_back({arg_name, type, is_array_type(type)});
        }

        // Collect scalar args
        struct ScalarArg {
            std::string name;
            std::string cuda_type_str;
        };
        std::vector<ScalarArg> scalar_args;
        for (size_t i = 0; i < args.size(); i++) {
            if (!args[i].is_array) {
                scalar_args.push_back({args[i].name, cuda_type(args[i].type)});
            }
        }

        // Emit kernel function signature
        src << "extern \"C\" __global__ void " << name << "(";

        bool has_prev = false;
        //int buffer_idx = 0;
        for (size_t i = 0; i < args.size(); i++) {
            if (args[i].is_array) {
                if (has_prev) src << ", ";
                src << cuda_type(args[i].type) << " *" << args[i].name;
                has_prev = true;
                //buffer_idx++;
            }
        }
        // Scalar args as individual parameters
        for (auto &sa : scalar_args) {
            if (has_prev) src << ", ";
            src << sa.cuda_type_str << " " << sa.name;
            has_prev = true;
        }

        src << ") {\n";
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

    void emit_local_var_decl(ASR::Variable_t *var) {
        ASR::ttype_t *type = var->m_type;
        if (is_array_type(type)) {
            // Local arrays in kernels - determine size
            ASR::ttype_t *past_alloc = ASRUtils::type_get_past_allocatable(type);
            if (ASR::is_a<ASR::Array_t>(*past_alloc)) {
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(past_alloc);
                int64_t total = 1;
                for (size_t d = 0; d < arr->n_dims; d++) {
                    if (arr->m_dims[d].m_length &&
                        ASR::is_a<ASR::IntegerConstant_t>(*arr->m_dims[d].m_length)) {
                        total *= ASR::down_cast<ASR::IntegerConstant_t>(
                            arr->m_dims[d].m_length)->m_n;
                    }
                }
                src << get_indent() << cuda_type(arr->m_type) << " "
                    << var->m_name << "[" << total << "];\n";
            }
        } else {
            src << get_indent() << cuda_type(type) << " " << var->m_name << ";\n";
        }
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
                ASR::If_t *ifstmt = ASR::down_cast<ASR::If_t>(stmt);
                src << get_indent() << "if (";
                visit_expr(ifstmt->m_test);
                src << ") {\n";
                indent_level++;
                for (size_t i = 0; i < ifstmt->n_body; i++) {
                    visit_stmt(ifstmt->m_body[i]);
                }
                indent_level--;
                if (ifstmt->n_orelse > 0) {
                    src << get_indent() << "} else {\n";
                    indent_level++;
                    for (size_t i = 0; i < ifstmt->n_orelse; i++) {
                        visit_stmt(ifstmt->m_orelse[i]);
                    }
                    indent_level--;
                }
                src << get_indent() << "}\n";
                break;
            }
            case ASR::stmtType::Allocate: {
                break;
            }
            case ASR::stmtType::ImplicitDeallocate: {
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
            src << "((float)(";
            visit_expr(f->m_args[0]);
            src << "))";
        } else {
            src << "/* unsupported intrinsic " << id << " */(";
            if (f->n_args > 0) visit_expr(f->m_args[0]);
            src << ")";
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
                ASR::IntegerConstant_t *c =
                    ASR::down_cast<ASR::IntegerConstant_t>(expr);
                src << c->m_n;
                break;
            }
            case ASR::exprType::RealConstant: {
                ASR::RealConstant_t *c =
                    ASR::down_cast<ASR::RealConstant_t>(expr);
                src << c->m_r;
                break;
            }
            case ASR::exprType::LogicalConstant: {
                ASR::LogicalConstant_t *c =
                    ASR::down_cast<ASR::LogicalConstant_t>(expr);
                src << (c->m_value ? "1" : "0");
                break;
            }
            case ASR::exprType::IntegerBinOp: {
                ASR::IntegerBinOp_t *op =
                    ASR::down_cast<ASR::IntegerBinOp_t>(expr);
                src << "(";
                visit_expr(op->m_left);
                src << " " << binop_str(op->m_op) << " ";
                visit_expr(op->m_right);
                src << ")";
                break;
            }
            case ASR::exprType::RealBinOp: {
                ASR::RealBinOp_t *op =
                    ASR::down_cast<ASR::RealBinOp_t>(expr);
                src << "(";
                visit_expr(op->m_left);
                src << " " << binop_str(op->m_op) << " ";
                visit_expr(op->m_right);
                src << ")";
                break;
            }
            case ASR::exprType::IntegerCompare: {
                ASR::IntegerCompare_t *cmp =
                    ASR::down_cast<ASR::IntegerCompare_t>(expr);
                src << "(";
                visit_expr(cmp->m_left);
                src << " " << cmpop_str(cmp->m_op) << " ";
                visit_expr(cmp->m_right);
                src << ")";
                break;
            }
            case ASR::exprType::RealCompare: {
                ASR::RealCompare_t *cmp =
                    ASR::down_cast<ASR::RealCompare_t>(expr);
                src << "(";
                visit_expr(cmp->m_left);
                src << " " << cmpop_str(cmp->m_op) << " ";
                visit_expr(cmp->m_right);
                src << ")";
                break;
            }
            case ASR::exprType::IntegerUnaryMinus: {
                ASR::IntegerUnaryMinus_t *u =
                    ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr);
                src << "(-";
                visit_expr(u->m_arg);
                src << ")";
                break;
            }
            case ASR::exprType::RealUnaryMinus: {
                ASR::RealUnaryMinus_t *u =
                    ASR::down_cast<ASR::RealUnaryMinus_t>(expr);
                src << "(-";
                visit_expr(u->m_arg);
                src << ")";
                break;
            }
            case ASR::exprType::Cast: {
                ASR::Cast_t *c = ASR::down_cast<ASR::Cast_t>(expr);
                src << "((" << cuda_type(c->m_type) << ")";
                visit_expr(c->m_arg);
                src << ")";
                break;
            }
            case ASR::exprType::ArrayItem: {
                ASR::ArrayItem_t *ai = ASR::down_cast<ASR::ArrayItem_t>(expr);
                visit_expr(ai->m_v);
                src << "[";
                if (ai->n_args == 1 && ai->m_args[0].m_right) {
                    // Fortran 1-based → C 0-based
                    src << "(";
                    visit_expr(ai->m_args[0].m_right);
                    src << " - 1)";
                }
                src << "]";
                break;
            }
            case ASR::exprType::GpuThreadIndex: {
                src << "threadIdx.x";
                break;
            }
            case ASR::exprType::GpuBlockIndex: {
                src << "blockIdx.x";
                break;
            }
            case ASR::exprType::GpuBlockSize: {
                src << "blockDim.x";
                break;
            }
            case ASR::exprType::RealSqrt: {
                ASR::RealSqrt_t *rs = ASR::down_cast<ASR::RealSqrt_t>(expr);
                src << "sqrt(";
                visit_expr(rs->m_arg);
                src << ")";
                break;
            }
            case ASR::exprType::IntrinsicElementalFunction: {
                ASR::IntrinsicElementalFunction_t *f =
                    ASR::down_cast<ASR::IntrinsicElementalFunction_t>(expr);
                emit_intrinsic(f);
                break;
            }
            case ASR::exprType::FunctionCall: {
                ASR::FunctionCall_t *fc =
                    ASR::down_cast<ASR::FunctionCall_t>(expr);
                std::string fn_name(ASRUtils::symbol_name(
                    ASRUtils::symbol_get_past_external(fc->m_name)));
                // Handle lowered intrinsic functions (_lcompilers_* pattern)
                if (fn_name.find("_lcompilers_real_") == 0 ||
                    fn_name.find("_lcompilers_dble_") == 0) {
                    src << "((" << cuda_type(fc->m_type) << ")";
                    if (fc->n_args > 0 && fc->m_args[0].m_value)
                        visit_expr(fc->m_args[0].m_value);
                    src << ")";
                } else if (fn_name.find("_lcompilers_int_") == 0 ||
                           fn_name.find("_lcompilers_nint_") == 0) {
                    src << "((" << cuda_type(fc->m_type) << ")";
                    if (fc->n_args > 0 && fc->m_args[0].m_value)
                        visit_expr(fc->m_args[0].m_value);
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
                        if (fc->n_args > 0 && fc->m_args[0].m_value)
                            visit_expr(fc->m_args[0].m_value);
                        src << ", ";
                        if (fc->n_args > 1 && fc->m_args[1].m_value)
                            visit_expr(fc->m_args[1].m_value);
                        src << ")";
                    } else {
                        src << "((";
                        if (fc->n_args > 0 && fc->m_args[0].m_value)
                            visit_expr(fc->m_args[0].m_value);
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
                    src << fn_name << "(";
                    for (size_t i = 0; i < fc->n_args; i++) {
                        if (i > 0) src << ", ";
                        if (fc->m_args[i].m_value)
                            visit_expr(fc->m_args[i].m_value);
                    }
                    src << ")";
                }
                break;
            }
            default:
                throw CodeGenError("CUDA codegen: unsupported expression type "
                    + std::to_string(expr->type));
        }
    }
};

Result<std::string> asr_to_cuda(Allocator & /*al*/, ASR::TranslationUnit_t &asr,
    diag::Diagnostics &diagnostics, CompilerOptions &co)
{
    ASRToCudaVisitor v(co);
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

#include <mlir/IR/BuiltinOps.h>
#include <mlir/IR/BuiltinTypes.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/IR/Verifier.h>
#include <mlir/Target/LLVMIR/Dialect/LLVMIR/LLVMToLLVMIRTranslation.h>

#include <libasr/codegen/asr_to_mlir.h>
#include <libasr/containers.h>

using LCompilers::ASR::is_a;
using LCompilers::ASR::down_cast;

namespace LCompilers {

uint64_t static inline get_hash(ASR::asr_t *node) {
    return (uint64_t)node;
}

// Local exception that is only used in this file to exit the visitor
// pattern and caught later (not propagated outside)
class CodeGenError
{
public:
    diag::Diagnostic d;
public:
    CodeGenError(const std::string &msg)
        : d{diag::Diagnostic(msg, diag::Level::Error, diag::Stage::CodeGen)}
    { }

    CodeGenError(const std::string &msg, const Location &loc)
        : d{diag::Diagnostic(msg, diag::Level::Error, diag::Stage::CodeGen, {
            diag::Label("", {loc})
        })}
    { }
};


class ASRToMLIRVisitor : public ASR::BaseVisitor<ASRToMLIRVisitor>
{
public:
    Allocator &al;
    std::string src;

    std::unique_ptr<mlir::MLIRContext> context;
    std::unique_ptr<mlir::OpBuilder> builder;
    std::unique_ptr<mlir::ModuleOp> module;

    mlir::Location loc; // UnknownLoc for now
    mlir::Value tmp; // Used for temporary returning the value
    mlir::LLVM::LLVMPointerType llvmI8PtrTy; // character_type

    std::map<uint64_t, mlir::Value> mlir_symtab; // Used for variables

public:
    ASRToMLIRVisitor(Allocator &al)
        : al{al},
        context(std::make_unique<mlir::MLIRContext>()),
        builder(std::make_unique<mlir::OpBuilder>(context.get())),
        loc(builder->getUnknownLoc())
        {
            // Load MLIR Dialects
            context->getOrLoadDialect<mlir::LLVM::LLVMDialect>();

            // Initialize values
            llvmI8PtrTy = mlir::LLVM::LLVMPointerType::get(builder->getI8Type());
        }

    /********************************** Utils *********************************/
    mlir::Type getType(ASR::ttype_t *asr_type) {
        int kind = ASRUtils::extract_kind_from_ttype_t(asr_type);
        switch (asr_type->type) {
            case ASR::ttypeType::Integer: {
                if      (kind == 4) return builder->getI32Type();
                else if (kind == 8) return builder->getI64Type();
                else
                    throw LCompilersException("Unhandled Integer kind: " +
                        std::to_string(kind));
            } case ASR::ttypeType::Real: {
                if      (kind == 4) return builder->getF32Type();
                else if (kind == 8) return builder->getF64Type();
                else
                    throw LCompilersException("Unhandled Real kind: " +
                        std::to_string(kind));
            } case ASR::ttypeType::Array: {
                ASR::Array_t *arr_type = down_cast<ASR::Array_t>(asr_type);
                return mlir::LLVM::LLVMArrayType::get(getType(arr_type->m_type),
                    ASRUtils::get_fixed_size_of_array(asr_type));
            } default: {
                throw LCompilersException("Variable type '"+
                    ASRUtils::type_to_str_python(asr_type) +
                    "' is not supported yet");
            }
        }
    }

    std::string getUniqueName(std::string name = "") {
        static int itr = 0; ++itr;
        return name + std::to_string(itr);
    }

    mlir::Value createGlobalString(std::string value) {
        mlir::OpBuilder builder0(module->getBodyRegion());
        mlir::LLVM::LLVMArrayType arrayI8Ty = mlir::LLVM::LLVMArrayType::get(
            builder->getI8Type(), value.size()+1);

        llvm::SmallVector<char> vecValue(value.begin(), value.end());
        vecValue.push_back('\0');
        mlir::LLVM::GlobalOp globalStr = builder0.create<mlir::LLVM::GlobalOp>(
            loc, arrayI8Ty, false, mlir::LLVM::Linkage::Private,
            getUniqueName("char_const_"), builder0.getStringAttr(vecValue));
        return builder->create<mlir::LLVM::AddressOfOp>(loc, globalStr);
    }

    void visit_expr2(ASR::expr_t &x) {
        this->visit_expr(x);
        if (ASR::is_a<ASR::Var_t>(x)) {
            tmp = builder->create<mlir::LLVM::LoadOp>(loc, tmp);
        }
    }

    /******************************** Visitors ********************************/
    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        module = std::make_unique<mlir::ModuleOp>(builder->create<mlir::ModuleOp>(loc,
            llvm::StringRef("LFortran")));

        // Visit Program
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Program_t>(*item.second)) {
                visit_symbol(*item.second);
            }
        }
    }

    void visit_Program(const ASR::Program_t &x) {
        mlir::LLVM::LLVMFunctionType llvmFnType = mlir::LLVM::LLVMFunctionType::get(
            builder->getI32Type(), llvmI8PtrTy, true);
        mlir::OpBuilder builder0(module->getBodyRegion());
        mlir::LLVM::LLVMFuncOp function = builder0.create<mlir::LLVM::LLVMFuncOp>(
            loc, "main", llvmFnType);

        mlir::Block &entryBlock = *function.addEntryBlock();
        builder = std::make_unique<mlir::OpBuilder>(mlir::OpBuilder::atBlockBegin(
            &entryBlock));

        for (auto &item : x.m_symtab->get_scope()) {
            visit_symbol(*item.second);
        }

        for (size_t i = 0; i < x.n_body; i++) {
            visit_stmt(*x.m_body[i]);
        }

        mlir::LLVM::ConstantOp zero = builder->create<mlir::LLVM::ConstantOp>(
            loc, builder->getI32Type(), builder->getI32IntegerAttr(0));
        builder->create<mlir::LLVM::ReturnOp>(loc, zero.getResult());
    }

    void visit_Variable(const ASR::Variable_t &x) {
        uint32_t h = get_hash((ASR::asr_t*) &x);
        mlir::Value size = builder->create<mlir::LLVM::ConstantOp>(loc,
            builder->getI32Type(), builder->getI64IntegerAttr(1));
        mlir::Type var_type = mlir::LLVM::LLVMPointerType::get(getType(x.m_type));
        mlir_symtab[h] = builder->create<mlir::LLVM::AllocaOp>(loc, var_type,
            size);
        if (x.m_symbolic_value) {
            this->visit_expr2(*x.m_symbolic_value);
            builder->create<mlir::LLVM::StoreOp>(loc, tmp, mlir_symtab[h]);
        }
    }

    void visit_Var(const ASR::Var_t &x) {
        ASR::Variable_t *v = ASRUtils::EXPR2VAR(&x.base);
        uint32_t h = get_hash((ASR::asr_t*) v);
        tmp = mlir_symtab[h];
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        this->visit_expr(*x.m_target);
        mlir::Value target = tmp;
        this->visit_expr2(*x.m_value);
        mlir::Value value = tmp;
        builder->create<mlir::LLVM::StoreOp>(loc, value, target);
    }

    void visit_IntegerConstant(const ASR::IntegerConstant_t &x) {
        int kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
        mlir::Type type; mlir::IntegerAttr attr;
        switch (kind) {
            case 4: {
                type = builder->getI32Type();
                attr = builder->getI32IntegerAttr(x.m_n);
                break;
            } case 8: {
                type = builder->getI64Type();
                attr = builder->getI64IntegerAttr(x.m_n);
                break;
            }
            default:
                throw CodeGenError("Integer constant of kind: `"+
                    std::to_string(kind) +"` is not supported yet",
                    x.base.base.loc);
        }
        tmp = builder->create<mlir::LLVM::ConstantOp>(loc,
                type, attr).getResult();
    }

    void visit_RealConstant(const ASR::RealConstant_t &x) {
        int kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
        mlir::Type type; mlir::FloatAttr attr;
        switch (kind) {
            case 4: {
                type = builder->getF32Type();
                attr = builder->getF32FloatAttr(x.m_r);
                break;
            } case 8: {
                type = builder->getF64Type();
                attr = builder->getF64FloatAttr(x.m_r);
                break;
            }
            default:
                throw CodeGenError("Integer constant of kind: `"+
                    std::to_string(kind) +"` is not supported yet",
                    x.base.base.loc);
        }
        tmp = builder->create<mlir::LLVM::ConstantOp>(loc,
                type, attr).getResult();
    }

    void visit_StringConstant(const ASR::StringConstant_t &x) {
        tmp = createGlobalString(x.m_s);
    }

    void visit_IntegerBinOp(const ASR::IntegerBinOp_t &x) {
        this->visit_expr2(*x.m_left);
        mlir::Value left = tmp;
        this->visit_expr2(*x.m_right);
        mlir::Value right = tmp;
        switch (x.m_op) {
            case ASR::binopType::Add: {
                tmp = builder->create<mlir::LLVM::AddOp>(loc, left, right);
                break;
            } case ASR::binopType::Sub: {
                tmp = builder->create<mlir::LLVM::SubOp>(loc, left, right);
                break;
            } case ASR::binopType::Mul: {
                tmp = builder->create<mlir::LLVM::MulOp>(loc, left, right);
                break;
            } case ASR::binopType::Div: {
                tmp = builder->create<mlir::LLVM::SDivOp>(loc, left, right);
                break;
            }
            default:
                throw CodeGenError("BinOp operator not supported yet",
                    x.base.base.loc);
        }
    }

    void visit_RealBinOp(const ASR::RealBinOp_t &x) {
        this->visit_expr2(*x.m_left);
        mlir::Value left = tmp;
        this->visit_expr2(*x.m_right);
        mlir::Value right = tmp;
        switch (x.m_op) {
            case ASR::binopType::Add: {
                tmp = builder->create<mlir::LLVM::FAddOp>(loc, left, right);
                break;
            } case ASR::binopType::Sub: {
                tmp = builder->create<mlir::LLVM::FSubOp>(loc, left, right);
                break;
            } case ASR::binopType::Mul: {
                tmp = builder->create<mlir::LLVM::FMulOp>(loc, left, right);
                break;
            } case ASR::binopType::Div: {
                tmp = builder->create<mlir::LLVM::FDivOp>(loc, left, right);
                break;
            }
            default:
                throw CodeGenError("BinOp operator not supported yet",
                    x.base.base.loc);
        }
    }

    void visit_IntegerCompare(const ASR::IntegerCompare_t &x) {
        this->visit_expr2(*x.m_left);
        mlir::Value left = tmp;
        this->visit_expr2(*x.m_right);
        mlir::Value right = tmp;
        mlir::LLVM::ICmpPredicate op;
        switch (x.m_op) {
            case ASR::cmpopType::Eq: {
                op = mlir::LLVM::ICmpPredicate::eq; break;
            } case ASR::cmpopType::Lt: {
                op = mlir::LLVM::ICmpPredicate::slt; break;
            } case ASR::cmpopType::LtE: {
                op = mlir::LLVM::ICmpPredicate::sle; break;
            } case ASR::cmpopType::Gt: {
                op = mlir::LLVM::ICmpPredicate::sgt; break;
            } case ASR::cmpopType::GtE: {
                op = mlir::LLVM::ICmpPredicate::sge; break;
            } case ASR::cmpopType::NotEq: {
                op = mlir::LLVM::ICmpPredicate::ne; break;
            }
            default:
                throw CodeGenError("Compare operator not supported yet",
                    x.base.base.loc);
        }
        tmp = builder->create<mlir::LLVM::ICmpOp>(loc, op, left, right);
    }

    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        this->visit_expr(*x.m_v);
        mlir::Value m_v = tmp;

        LCOMPILERS_ASSERT(x.n_args == 1);
        this->visit_expr(*x.m_args[0].m_right);
        mlir::Value idx = tmp;

        if (ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(
                x.m_args[0].m_right)) != 8) {
            idx = builder->create<mlir::LLVM::SExtOp>(loc,
                builder->getI64Type(), idx);
        }
        mlir::LLVM::ConstantOp one = builder->create<mlir::LLVM::ConstantOp>(loc,
            builder->getI64Type(), builder->getIndexAttr(1));

        idx = builder->create<mlir::LLVM::SubOp>(loc, idx, one);
        mlir::Type basePtrType = mlir::LLVM::LLVMPointerType::get(getType(
            ASRUtils::extract_type(ASRUtils::expr_type(x.m_v))));
        tmp = builder->create<mlir::LLVM::GEPOp>(loc, basePtrType, m_v,
            mlir::ValueRange{idx});
    }

    void visit_ErrorStop(const ASR::ErrorStop_t &) {
        mlir::OpBuilder builder0(module->getBodyRegion());
        mlir::LLVM::LLVMFuncOp printf_fn =
            module->lookupSymbol<mlir::LLVM::LLVMFuncOp>("printf");
        if (!printf_fn) {
            mlir::LLVM::LLVMVoidType voidTy =
                mlir::LLVM::LLVMVoidType::get(context.get());
            mlir::LLVM::LLVMFunctionType llvmFnType =
                mlir::LLVM::LLVMFunctionType::get(voidTy, llvmI8PtrTy, true);
            printf_fn = builder0.create<mlir::LLVM::LLVMFuncOp>(
                loc, "printf", llvmFnType);
        }
        mlir::Value zero = builder->create<mlir::LLVM::ConstantOp>(loc,
            builder->getI64Type(), builder->getIndexAttr(0));
        tmp = builder->create<mlir::LLVM::GEPOp>(loc, llvmI8PtrTy,
            createGlobalString("ERROR STOP\n"), zero);
        builder->create<mlir::LLVM::CallOp>(loc, printf_fn, tmp);

        mlir::LLVM::LLVMFuncOp exit_fn =
            module->lookupSymbol<mlir::LLVM::LLVMFuncOp>("exit");
        if (!exit_fn) {
            mlir::LLVM::LLVMVoidType voidTy =
                mlir::LLVM::LLVMVoidType::get(context.get());
            mlir::LLVM::LLVMFunctionType llvmFnType =
                mlir::LLVM::LLVMFunctionType::get(voidTy, builder->getI32Type());
            exit_fn = builder0.create<mlir::LLVM::LLVMFuncOp>(
                loc, "exit", llvmFnType);
        }
        mlir::LLVM::ConstantOp one = builder->create<mlir::LLVM::ConstantOp>(
            loc, builder->getI32Type(), builder->getI32IntegerAttr(1));
        builder->create<mlir::LLVM::CallOp>(loc, exit_fn, one.getResult());

        builder->create<mlir::LLVM::UnreachableOp>(loc);
    }

    void handle_Print(const Location &l, ASR::expr_t *x) {
        std::string fmt = "";
        Vec<mlir::Value> args;
        if (ASR::is_a<ASR::StringFormat_t>(*x)) {
            ASR::StringFormat_t *sf = ASR::down_cast<ASR::StringFormat_t>(x);
            args.reserve(al, sf->n_args+1);
            args.push_back(al, nullptr); // Later used by `printf_fmt`
            for (size_t i=0; i<sf->n_args; i++) {
                ASR::ttype_t *t = ASRUtils::expr_type(sf->m_args[i]);
                this->visit_expr(*sf->m_args[i]);
                if (ASR::is_a<ASR::Var_t>(*sf->m_args[i]) ||
                        ASR::is_a<ASR::ArrayItem_t>(*sf->m_args[i])) {
                    tmp = builder->create<mlir::LLVM::LoadOp>(loc, tmp);
                }
                if (ASRUtils::is_integer(*t)) {
                    fmt += " %d";
                } else if (ASRUtils::is_real(*t)) {
                    tmp = builder->create<mlir::LLVM::FPExtOp>(loc,
                        builder->getF64Type(), tmp);
                    fmt += " %f";
                } else if (ASRUtils::is_character(*t)) {
                    fmt += " %s";
                } else {
                    throw CodeGenError("Unhandled type in print statement", l);
                }
                args.push_back(al, tmp);
            }
        } else {
            throw CodeGenError("Unsupported expression as formatter in print", l);
        }
        fmt += "\n";

        mlir::OpBuilder builder0(module->getBodyRegion());
        mlir::LLVM::LLVMFuncOp printf_fn =
            module->lookupSymbol<mlir::LLVM::LLVMFuncOp>("printf");
        if (!printf_fn) {
            mlir::LLVM::LLVMVoidType voidTy =
                mlir::LLVM::LLVMVoidType::get(context.get());
            mlir::LLVM::LLVMFunctionType llvmFnType =
                mlir::LLVM::LLVMFunctionType::get(voidTy, llvmI8PtrTy, true);
            printf_fn = builder0.create<mlir::LLVM::LLVMFuncOp>(
                loc, "printf", llvmFnType);
        }
        mlir::Value zero = builder->create<mlir::LLVM::ConstantOp>(loc,
            builder->getI64Type(), builder->getIndexAttr(0));
        args.p[0] = builder->create<mlir::LLVM::GEPOp>(loc,
            llvmI8PtrTy, createGlobalString(fmt), zero);
        builder->create<mlir::LLVM::CallOp>(loc, printf_fn,
            mlir::ValueRange{args.as_vector()});
    }

    void visit_Print(const ASR::Print_t &x) {
        handle_Print(x.base.base.loc, x.m_text);
    }

    void visit_FileWrite(const ASR::FileWrite_t &x) {
        if (!x.m_unit) {
            LCOMPILERS_ASSERT(x.n_values == 1);
            handle_Print(x.base.base.loc, x.m_values[0]);
        } else {
            throw CodeGenError("Only write(*, *) [...] is implemented for now",
                    x.base.base.loc);
        }
    }

};

Result<std::unique_ptr<MLIRModule>> asr_to_mlir(Allocator &al,
        ASR::TranslationUnit_t &asr, diag::Diagnostics &diagnostics) {
    ASRToMLIRVisitor v(al);
    try {
        v.visit_TranslationUnit(asr);
    } catch (const CodeGenError &e) {
        diagnostics.diagnostics.push_back(e.d);
        return Error();
    }

    mlir::registerLLVMDialectTranslation(*v.context);

    if (mlir::failed(mlir::verify(*v.module))) {
        std::string mlir_str;
        llvm::raw_string_ostream raw_os(mlir_str);
        v.module->print(raw_os);
        std::cout << "\n" << mlir_str << "\n";
        std::string msg = "asr_to_mlir: module verification failed";
        diagnostics.diagnostics.push_back(diag::Diagnostic(msg,
            diag::Level::Error, diag::Stage::CodeGen));
        Error error;
        return error;
    }
    return std::make_unique<MLIRModule>(std::move(v.module), std::move(v.context));
}

} // namespace LCompilers

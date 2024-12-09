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
        int kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
        if (kind != 4) {
            throw CodeGenError("Integer of kind: `"+ std::to_string(kind)
                +"` is not supported yet", x.base.base.loc);
        }
        // Used by scalar variables
        mlir::Value size = builder->create<mlir::LLVM::ConstantOp>(loc,
            builder->getI32Type(), builder->getI32IntegerAttr(1));
        mlir::Type var_type;
        switch (x.m_type->type) {
            case ASR::ttypeType::Integer: {
                if (kind == 4) {
                    var_type = mlir::LLVM::LLVMPointerType::get(
                        builder->getI32Type());
                }
                break;
            } case ASR::ttypeType::Real: {
                if (kind == 4) {
                    var_type = mlir::LLVM::LLVMPointerType::get(
                        builder->getF32Type());
                }
                break;
            }
            default:
                throw LCompilersException("Variable type '"+
                    ASRUtils::type_to_str_python(x.m_type) +
                    "' is not supported yet");
        }
        mlir_symtab[h] = builder->create<mlir::LLVM::AllocaOp>(loc, var_type,
            size);
        if (x.m_symbolic_value) {
            this->visit_expr(*x.m_symbolic_value);
            builder->create<mlir::LLVM::StoreOp>(loc, tmp, mlir_symtab[h]);
        }
    }

    void visit_Var(const ASR::Var_t &x) {
        ASR::Variable_t *v = ASRUtils::EXPR2VAR(&x.base);
        uint32_t h = get_hash((ASR::asr_t*) v);
        tmp = mlir_symtab[h];
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        ASR::Variable_t *m_target = ASRUtils::EXPR2VAR(x.m_target);
        uint32_t h = get_hash((ASR::asr_t*) m_target);
        mlir::Value target = mlir_symtab[h];
        this->visit_expr(*x.m_value);
        mlir::Value value = tmp;
        builder->create<mlir::LLVM::StoreOp>(loc, value, target);
    }

    void visit_IntegerConstant(const ASR::IntegerConstant_t &x) {
        int kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
        switch (kind) {
            case 4: {
                tmp = builder->create<mlir::LLVM::ConstantOp>(loc,
                    builder->getI32Type(),
                    builder->getI32IntegerAttr(x.m_n)).getResult();
                break;
            }
            default:
                throw CodeGenError("Integer constant of kind: `"+
                    std::to_string(kind) +"` is not supported yet",
                    x.base.base.loc);
        }
    }

    void visit_RealConstant(const ASR::RealConstant_t &x) {
        int kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
        switch (kind) {
            case 4: {
                tmp = builder->create<mlir::LLVM::ConstantOp>(loc,
                    builder->getF32Type(),
                    builder->getF32FloatAttr(x.m_r)).getResult();
                break;
            }
            default:
                throw CodeGenError("Integer constant of kind: `"+
                    std::to_string(kind) +"` is not supported yet",
                    x.base.base.loc);
        }
    }

    void visit_IntegerBinOp(const ASR::IntegerBinOp_t &x) {
        this->visit_expr(*x.m_left);
        mlir::Value left = tmp;
        this->visit_expr(*x.m_right);
        mlir::Value right = tmp;
        switch (x.m_op) {
            case ASR::binopType::Add: {
                tmp = builder->create<mlir::LLVM::AddOp>(loc, left, right);
                break;
            }
            case ASR::binopType::Mul: {
                tmp = builder->create<mlir::LLVM::MulOp>(loc, left, right);
                break;
            }
            default:
                throw CodeGenError("BinOp operator not supported yet",
                    x.base.base.loc);
        }
    }

    void visit_RealBinOp(const ASR::RealBinOp_t &x) {
        this->visit_expr(*x.m_left);
        mlir::Value left = tmp;
        this->visit_expr(*x.m_right);
        mlir::Value right = tmp;
        switch (x.m_op) {
            case ASR::binopType::Add: {
                tmp = builder->create<mlir::LLVM::FAddOp>(loc, left, right);
                break;
            }
            case ASR::binopType::Mul: {
                tmp = builder->create<mlir::LLVM::FMulOp>(loc, left, right);
                break;
            }
            default:
                throw CodeGenError("BinOp operator not supported yet",
                    x.base.base.loc);
        }
    }

    void visit_Print(const ASR::Print_t &x) {
        std::string fmt = "";
        Vec<mlir::Value> args;
        LCOMPILERS_ASSERT(x.m_text != nullptr &&
            ASR::is_a<ASR::String_t>(*ASRUtils::expr_type(x.m_text)));
        if (ASR::is_a<ASR::StringFormat_t>(*x.m_text)) {
            ASR::StringFormat_t *sf = ASR::down_cast<ASR::StringFormat_t>(
                x.m_text);
            args.reserve(al, sf->n_args+1);
            // Later used by `printf_fmt`
            args.push_back(al, nullptr);
            for (size_t i=0; i<sf->n_args; i++) {
                ASR::ttype_t *t = ASRUtils::expr_type(sf->m_args[i]);
                this->visit_expr(*sf->m_args[i]);
                if (ASRUtils::is_integer(*t)) {
                    fmt += " %d";
                    args.push_back(al, builder->create<mlir::LLVM::LoadOp>(loc,
                        tmp));
                } else if (ASRUtils::is_real(*t)) {
                    fmt += " %f";
                    args.push_back(al, builder->create<mlir::LLVM::LoadOp>(loc,
                        tmp));
                } else {
                    throw CodeGenError("Unhandled type in print statement",
                        x.base.base.loc);
                }
            }
        } else {
            throw CodeGenError("Unsupported expression as formatter in print",
                x.base.base.loc);
        }
        fmt += "\n";

        mlir::OpBuilder builder0(module->getBodyRegion());
        mlir::LLVM::LLVMArrayType arrayI8Ty = mlir::LLVM::LLVMArrayType::get(
            builder->getI8Type(), fmt.size());
        mlir::LLVM::GlobalOp global_str = builder0.create<mlir::LLVM::GlobalOp>(
            loc, arrayI8Ty, false, mlir::LLVM::Linkage::External, "printf_fmt",
            builder->getStringAttr(fmt));

        mlir::LLVM::LLVMVoidType voidTy = mlir::LLVM::LLVMVoidType::get(context.get());
        mlir::LLVM::LLVMFunctionType llvmFnType = mlir::LLVM::LLVMFunctionType::get(
            voidTy, llvmI8PtrTy, true);
        mlir::LLVM::LLVMFuncOp fn = builder0.create<mlir::LLVM::LLVMFuncOp>(
            loc, "printf", llvmFnType);

        mlir::Value zero = builder->create<mlir::LLVM::ConstantOp>(loc,
            builder->getI64Type(), builder->getIndexAttr(0));
        mlir::Value globalPtr = builder->create<mlir::LLVM::AddressOfOp>(
            loc, global_str);
        globalPtr = builder->create<mlir::LLVM::GEPOp>(loc, llvmI8PtrTy,
            globalPtr, mlir::ValueRange{zero, zero});
        args.p[0] = globalPtr;
        builder->create<mlir::LLVM::CallOp>(loc, fn,
            mlir::ValueRange{args.as_vector()});
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

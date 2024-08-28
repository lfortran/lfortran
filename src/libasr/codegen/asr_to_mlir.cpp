#include <mlir/IR/BuiltinOps.h>
#include <mlir/IR/BuiltinTypes.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
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
    std::unique_ptr<mlir::OpBuilder> b;
    std::unique_ptr<mlir::ModuleOp> module;

    mlir::Location loc; // UnknownLoc for now
    mlir::Value tmp; // Used for temporary returning the value
    mlir::LLVM::LLVMPointerType llvmI8PtrTy; // character_type

    std::map<uint64_t, mlir::Value> mlir_symtab; // Used for variables

public:
    ASRToMLIRVisitor(Allocator &al)
        : al{al},
        context(std::make_unique<mlir::MLIRContext>()),
        b(std::make_unique<mlir::OpBuilder>(context.get())),
        loc(b->getUnknownLoc())
        {
            // Load MLIR Dialects
            context->getOrLoadDialect<mlir::LLVM::LLVMDialect>();

            // Initialize values
            llvmI8PtrTy = mlir::LLVM::LLVMPointerType::get(b->getI8Type());
        }

    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        module = std::make_unique<mlir::ModuleOp>(b->create<mlir::ModuleOp>(loc,
            llvm::StringRef("LFortran")));

        // Visit Program
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Program_t>(*item.second)) {
                visit_symbol(*item.second);
            }
        }
    }

    void visit_Program(const ASR::Program_t &x) {
        // b->setInsertionPointToEnd(module->getBody());
        mlir::LLVM::LLVMFunctionType llvmFnType = mlir::LLVM::LLVMFunctionType::get(
            b->getI32Type(), llvmI8PtrTy, true);
        mlir::OpBuilder builder(module->getBodyRegion());
        mlir::LLVM::LLVMFuncOp function = builder.create<mlir::LLVM::LLVMFuncOp>(
            loc, "main", llvmFnType);

        mlir::Block &entryBlock = *function.addEntryBlock();
        b = std::make_unique<mlir::OpBuilder>(mlir::OpBuilder::atBlockBegin(
            &entryBlock));
        // b->setInsertionPointToStart(&entryBlock);

        for (auto &item : x.m_symtab->get_scope()) {
            visit_symbol(*item.second);
        }

        for (size_t i = 0; i < x.n_body; i++) {
            visit_stmt(*x.m_body[i]);
        }

        mlir::LLVM::ConstantOp zero = b->create<mlir::LLVM::ConstantOp>(
            loc, b->getI32Type(), b->getI32IntegerAttr(0));
        b->create<mlir::LLVM::ReturnOp>(loc, zero.getResult());
    }

    void visit_Variable(const ASR::Variable_t &x) {
        switch (x.m_type->type) {
            case ASR::ttypeType::Integer: {
                int kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
                if (kind == 4) {
                    auto one = b->create<mlir::LLVM::ConstantOp>(
                        loc, b->getI32Type(), b->getI32IntegerAttr(1));
                    mlir::LLVM::AllocaOp alloc = b->create<mlir::LLVM::AllocaOp>(
                        loc, mlir::LLVM::LLVMPointerType::get(b->getI32Type()), one, 4);
                    uint32_t h = get_hash((ASR::asr_t*) &x);
                    mlir_symtab[h] = alloc;
                    if (x.m_symbolic_value) {
                        this->visit_expr(*x.m_symbolic_value);
                        b->create<mlir::LLVM::StoreOp>(loc, tmp, alloc);
                    }
                } else {
                    throw CodeGenError("Integer of kind: `"+ std::to_string(kind)
                        +"` is not supported yet", x.base.base.loc);
                }
                break;
            }
            default:
                throw LCompilersException("Type not implemented");
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
        b->create<mlir::LLVM::StoreOp>(loc, value, target);
    }

    void visit_IntegerConstant(const ASR::IntegerConstant_t &x) {
        int kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
        switch (kind) {
            case 4: {
                tmp = b->create<mlir::LLVM::ConstantOp>(loc,
                    b->getI32Type(),
                    b->getI32IntegerAttr(x.m_n)).getResult();
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
                tmp = b->create<mlir::LLVM::AddOp>(loc, left, right);
                break;
            }
            case ASR::binopType::Mul: {
                tmp = b->create<mlir::LLVM::MulOp>(loc, left, right);
                break;
            }
            default:
                throw CodeGenError("Integer BinOp not supported yet",
                    x.base.base.loc);
        }
    }

    void visit_Print(const ASR::Print_t &x) {
        std::string fmt = "";
        Vec<mlir::Value> args; args.reserve(al, x.n_values);
        LCOMPILERS_ASSERT(x.n_values == 1)
        if (ASR::is_a<ASR::StringFormat_t>(*x.m_values[0])) {
            ASR::StringFormat_t *sf = ASR::down_cast<ASR::StringFormat_t>(
                x.m_values[0]);
            for (size_t i=0; i<sf->n_args; i++) {
                ASR::ttype_t *t = ASRUtils::expr_type(sf->m_args[i]);
                if (ASRUtils::is_integer(*t)) {
                    fmt += " %d";
                    this->visit_expr(*sf->m_args[i]);
                    args.push_back(al, b->create<mlir::LLVM::LoadOp>(loc, tmp));
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

        mlir::OpBuilder builder(module->getBodyRegion());
        mlir::LLVM::LLVMArrayType arrayI8Ty = mlir::LLVM::LLVMArrayType::get(
            b->getI8Type(), fmt.size());
        mlir::LLVM::GlobalOp glocal_str = builder.create<mlir::LLVM::GlobalOp>(
            loc, arrayI8Ty, false, mlir::LLVM::Linkage::External, "printf_fmt",
            b->getStringAttr(fmt));

        mlir::LLVM::LLVMVoidType voidTy = mlir::LLVM::LLVMVoidType::get(context.get());
        mlir::LLVM::LLVMFunctionType llvmFnType = mlir::LLVM::LLVMFunctionType::get(
            voidTy, llvmI8PtrTy, true);
        mlir::LLVM::LLVMFuncOp fn = builder.create<mlir::LLVM::LLVMFuncOp>(
            loc, "printf", llvmFnType);

        mlir::Value zero = b->create<mlir::LLVM::ConstantOp>(loc,
            b->getI64Type(), b->getIndexAttr(0));
        mlir::Value globalPtr = b->create<mlir::LLVM::AddressOfOp>(
            loc, glocal_str);
        globalPtr = b->create<mlir::LLVM::GEPOp>(loc, llvmI8PtrTy,
            globalPtr, mlir::ValueRange{zero, zero});
        b->create<mlir::LLVM::CallOp>(loc, fn, mlir::ValueRange{globalPtr, args[0]});
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
    return std::make_unique<MLIRModule>(std::move(v.module), std::move(v.context));
}

} // namespace LCompilers

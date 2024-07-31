#include <mlir/IR/MLIRContext.h>
#include <mlir/IR/Builders.h>
#include <mlir/Dialect/Arithmetic/IR/Arithmetic.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/IR/BuiltinTypes.h>
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/IR/OwningOpRef.h"



#include "mlir/Conversion/MemRefToLLVM/MemRefToLLVM.h"
#include "mlir/Conversion/ReconcileUnrealizedCasts/ReconcileUnrealizedCasts.h"
#include "mlir/Conversion/AffineToStandard/AffineToStandard.h"
// #include "mlir/Conversion/ArithToLLVM/ArithToLLVM.h"
#include "mlir/Conversion/MathToLLVM/MathToLLVM.h"
#include "mlir/Conversion/FuncToLLVM/ConvertFuncToLLVMPass.h"
#include "mlir/Conversion/SCFToControlFlow/SCFToControlFlow.h"
#include "mlir/Conversion/ControlFlowToLLVM/ControlFlowToLLVM.h"
#include "mlir/Dialect/Func/Transforms/Passes.h"
#include "mlir/Dialect/Affine/IR/AffineOps.h"
#include "mlir/Dialect/ControlFlow/IR/ControlFlowOps.h"
// #include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/ControlFlow/IR/ControlFlow.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/Math/IR/Math.h"
#include "mlir/Dialect/MemRef/IR/MemRef.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/Attributes.h"
#include "mlir/IR/Block.h"
#include "mlir/IR/OperationSupport.h"
#include "mlir/IR/TypeRange.h"
#include "mlir/IR/Value.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/OwningOpRef.h"
#include "mlir/IR/MLIRContext.h"
#include "mlir/IR/Verifier.h"
#include "mlir/IR/Builders.h"
#include "mlir/Parser/Parser.h"
#include "mlir/Pass/PassManager.h"
#include "mlir/Support/LogicalResult.h"
#include "mlir/Target/LLVMIR/Dialect/LLVMIR/LLVMToLLVMIRTranslation.h"
#include "mlir/ExecutionEngine/ExecutionEngine.h"
#include "mlir/ExecutionEngine/MemRefUtils.h"
#include "mlir/Dialect/Linalg/Passes.h"
#include "mlir/InitAllDialects.h"
#include "mlir/Transforms/Passes.h"

#include <libasr/codegen/asr_to_mlir.h>
#include <libasr/asr_utils.h>

using LCompilers::ASR::is_a;
using LCompilers::ASR::down_cast;

namespace LCompilers {

class ASRToMLIRVisitor : public ASR::BaseVisitor<ASRToMLIRVisitor>
{
public:
    Allocator &al;
    std::string src;
    int indent_level;
    std::string indent;
    int indent_spaces;

    mlir::MLIRContext context;
    // std::unique_ptr<mlir::Builder> b;
    mlir::Builder builder;
    mlir::OwningOpRef<mlir::ModuleOp> module;


public:
    ASRToMLIRVisitor(Allocator &al)
        : al{al}, indent_level{0}, indent_spaces{2}
        // context(mlir::MLIRContext())
        ,
        // b(std::make_unique<mlir::Builder>(context))
        // ,
        builder(mlir::Builder(&context))
        { }

    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        module = mlir::ModuleOp::create(builder.getUnknownLoc(), llvm::StringRef("LFortran"));
        // builder = std::make_unique<mlir::OpBuilder>(context);
        // builder->setInsertionPointToStart(module->getBody());

// std::cout << "TranslationUnit\n";
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Program_t>(*item.second)) {
                visit_symbol(*item.second);
            // r += src;
            }
        }
        // src = r;
    }

    /********************************** Symbol *********************************/
    void visit_Program(const ASR::Program_t &x) {
std::cout << "Program\n";
    // mlir::OpBuilder builder(&context);
    std::vector<mlir::Type> arg_types{};
    std::vector<mlir::Type> ret_types{};
    auto funcType = builder.getFunctionType(arg_types, ret_types);
    auto func = mlir::func::FuncOp::create(builder.getUnknownLoc(), llvm::StringRef("main"), funcType);
    // func.addEntryBlock();
    module->push_back(func);
        // auto *entryBlock = func.addEntryBlock();
    // mod.push_back(mod);
    // builder.setInsertionPointToStart(entryBlock);

    // firBuilder = std::make_unique<fir::FirOpBuilder>(mod, *kindMap);

//   auto funcOp = std::make_unique<mlir::func::FuncOp>(mlir::func::FuncOp::create(loc, name, funcType));
//   funcOp->setSymVisibility(visibility);
//   module_->push_back(*funcOp);

//   return new MLIRFunction(this, std::move(funcOp));

        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Variable_t>(*item.second)) {
                visit_symbol(*item.second);
            }
        }
    }

// MLIRFunction *MLIRModule::CreateFunction(
//     std::string name,
//     std::vector<mlir::Type> arg_types,
//     std::vector<mlir::Type> ret_types,
//     bool is_public) {
//   auto visibility = is_public ? "public" : "nested";

//   auto funcType = builder_->getFunctionType(arg_types, ret_types);
//   auto loc = builder_->getUnknownLoc();
//   auto funcOp = std::make_unique<mlir::func::FuncOp>(mlir::func::FuncOp::create(loc, name, funcType));
//   funcOp->setSymVisibility(visibility);
//   module_->push_back(*funcOp);
//   funcOp->addEntryBlock();
//   builder_->setInsertionPointToStart(&funcOp->getBody().front());

//   return new MLIRFunction(this, std::move(funcOp));
// }
    // }

    void visit_Variable(const ASR::Variable_t &x) {
        switch (x.m_type->type) {
            case ASR::ttypeType::Integer: {
                // mlir::TypedAttr();
// auto zero_attr = builder.getF32FloatAttr(0);
// auto zero = builder.create<mlir::arith::ConstantOp>(builder.getUnknownLoc(), zero_attr);
                // auto single_type = mlir::IntegerType::getI32(builder_.getContext());
                // auto type = mlir::MemRefType::get({}, single_type);
                // auto key = builder.create<mlir::memref::AllocOp>(Loc(), type);
                break;
            }
            default:
                throw LCompilersException("Type not implemented");
        }
    }
};

Result<std::string> asr_to_mlir(Allocator &al, ASR::TranslationUnit_t &asr) {
    ASRToMLIRVisitor v(al);
    v.visit_TranslationUnit(asr);
    v.module->dump();
    return v.src;
}

} // namespace LCompilers

#include <functional>

#include <mlir/IR/BuiltinOps.h>
#include <mlir/IR/BuiltinTypes.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/Dialect/OpenMP/OpenMPDialect.h>
#include <mlir/IR/Verifier.h>
#include <mlir/Target/LLVMIR/Dialect/LLVMIR/LLVMToLLVMIRTranslation.h>
#include <mlir/Target/LLVMIR/Dialect/OpenMP/OpenMPToLLVMIRTranslation.h>
#include <mlir/Target/LLVMIR/Dialect/Builtin/BuiltinToLLVMIRTranslation.h>

#include <libasr/codegen/asr_to_mlir.h>
#include <libasr/containers.h>
#include <libasr/pass/intrinsic_functions.h>
#include <libasr/pass/intrinsic_function_registry.h>

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
    mlir::LLVM::LLVMPointerType voidPtr; // ptr => void *

    std::map<uint64_t, mlir::Value> mlir_symtab; // Used for variables

    // The enclosing constructs that an EXIT or a CYCLE can name: an EXIT
    // branches to `exit`, a CYCLE to `cycle` (null for a construct that is
    // not a loop).
    struct ConstructFrame {
        const char *name;
        mlir::Block *exit;
        mlir::Block *cycle;
    };
    std::vector<ConstructFrame> construct_frames;

public:
    ASRToMLIRVisitor(Allocator &al)
        : al{al},
        context(std::make_unique<mlir::MLIRContext>()),
        builder(std::make_unique<mlir::OpBuilder>(context.get())),
        loc(builder->getUnknownLoc())
        {
            // Load MLIR Dialects
            context->getOrLoadDialect<mlir::LLVM::LLVMDialect>();
            context->getOrLoadDialect<mlir::omp::OpenMPDialect>();

            // Initialize values
            voidPtr = mlir::LLVM::LLVMPointerType::get(context.get());
        }

    /********************************** Utils *********************************/
    bool block_terminated() {
        mlir::Block *block = builder->getBlock();
        return !block->empty()
            && block->back().hasTrait<mlir::OpTrait::IsTerminator>();
    }

    // Statements after a branch or an ERROR STOP are unreachable, and a
    // block must not continue past its terminator.
    void visit_body(ASR::stmt_t **body, size_t n_body) {
        for (size_t i = 0; i < n_body && !block_terminated(); i++) {
            this->visit_stmt(*body[i]);
        }
    }

    void branch_to(mlir::Block *target) {
        if (!block_terminated()) {
            builder->create<mlir::LLVM::BrOp>(loc, mlir::ValueRange{}, target);
        }
    }

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
            } case ASR::ttypeType::Logical : {
                return builder->getI1Type();
            } case ASR::ttypeType::Array: {
                ASR::Array_t *arr_type = down_cast<ASR::Array_t>(asr_type);
                return mlir::LLVM::LLVMArrayType::get(getType(arr_type->m_type),
                    ASRUtils::get_fixed_size_of_array(asr_type));
            } default: {
                throw LCompilersException("Variable type '"+
                    ASRUtils::type_to_str_python_expr(asr_type, nullptr) +
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

    void visit_BindReference(const ASR::BindReference_t &x) {
        throw CodeGenError("host association is not supported by the MLIR backend",
            x.base.base.loc);
    }

    void visit_IfExp(const ASR::IfExp_t &x) {
        throw CodeGenError("conditional expressions are not supported by the MLIR backend",
            x.base.base.loc);
    }

    void visit_expr2(ASR::expr_t &x) {
        this->visit_expr(x);
        if (ASR::is_a<ASR::Var_t>(x) || ASR::is_a<ASR::ArrayItem_t>(x)) {
            mlir::Type type = getType(ASRUtils::expr_type(&x));
            tmp = builder->create<mlir::LLVM::LoadOp>(loc, type, tmp);
        }
    }

    /******************************** Visitors ********************************/
    void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::BlockData_t>(*item.second)) {
                throw CodeGenError("BLOCK DATA is not supported by the MLIR backend",
                    item.second->base.loc);
            }
        }
        module = std::make_unique<mlir::ModuleOp>(builder->create<mlir::ModuleOp>(loc,
            llvm::StringRef("LFortran")));

        // Visit all the Functions
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Function_t>(*item.second)) {
                visit_symbol(*item.second);
            }
        }
        // Visit all the Modules
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Module_t>(*item.second)) {
                visit_symbol(*item.second);
            }
        }
        // Finally, visit Program
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Program_t>(*item.second)) {
                visit_symbol(*item.second);
            }
        }
    }

    void visit_Module(const ASR::Module_t &x) {
        if (!module) {
            module = std::make_unique<mlir::ModuleOp>(
                builder->create<mlir::ModuleOp>(loc,
                llvm::StringRef("LFortran")));
        }
        // Visit all the Functions
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Function_t>(*item.second)) {
                visit_symbol(*item.second);
            }
        }
    }

    void visit_Function(const ASR::Function_t &x) {
        if (ASRUtils::is_bare_implicit_interface(x)) {
            return;
        }
        ASR::FunctionType_t *fnType = down_cast<ASR::FunctionType_t>(
            x.m_function_signature);
        if (fnType->m_deftype == ASR::deftypeType::Interface) {
            // Skip Interface function for now
            return;
        }
        Vec<mlir::Type> argsType; argsType.reserve(al, fnType->n_arg_types);
        // Collect all the arguments type
        for (size_t i=0; i<fnType->n_arg_types; i++) {
            argsType.push_back(al, voidPtr);
        }
        mlir::Type returnType;
        // Collect the return type
        if (fnType->m_return_var_type) {
            returnType = getType(fnType->m_return_var_type);
        } else {
            returnType = mlir::LLVM::LLVMVoidType::get(context.get());
        }

        mlir::LLVM::LLVMFunctionType llvmFnType = mlir::LLVM::LLVMFunctionType::get(
            returnType, argsType.as_vector(), false);
        mlir::OpBuilder builder0(module->getBodyRegion());
        // Create function
        mlir::LLVM::LLVMFuncOp fn = builder0.create<mlir::LLVM::LLVMFuncOp>(
            loc, x.m_name, llvmFnType);

        mlir::Block &entryBlock = *fn.addEntryBlock(*builder);
        builder = std::make_unique<mlir::OpBuilder>(mlir::OpBuilder::atBlockBegin(
            &entryBlock));

        // Store all the argument symbols in the mlir_symtab
        // Later, this is used in the function's body
        for (size_t i=0; i<x.n_args; i++) {
            ASR::Variable_t *v = ASRUtils::EXPR2VAR(x.m_args[i]);
            uint32_t h = get_hash((ASR::asr_t*) v);
            mlir_symtab[h] = fn.getArgument(i);
        }

        // Declare only the Local and ReturnVar symbols
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *v = down_cast<ASR::Variable_t>(item.second);
                if (v->m_intent == ASR::intentType::Local ||
                        v->m_intent == ASR::intentType::ReturnVar) {
                    visit_symbol(*item.second);
                }
            }
        }

        // Visit the function body
        for (size_t i = 0; i < x.n_body; i++) {
            visit_stmt(*x.m_body[i]);
        }
        if (x.m_return_var) {
            this->visit_expr2(*x.m_return_var);
            builder->create<mlir::LLVM::ReturnOp>(loc, tmp);
        } else {
            builder->create<mlir::LLVM::ReturnOp>(loc, mlir::ValueRange{});
        }
    }

    void visit_Program(const ASR::Program_t &x) {
        mlir::LLVM::LLVMFunctionType llvmFnType = mlir::LLVM::LLVMFunctionType::get(
            builder->getI32Type(), {}, false);
        mlir::OpBuilder builder0(module->getBodyRegion());
        mlir::LLVM::LLVMFuncOp function = builder0.create<mlir::LLVM::LLVMFuncOp>(
            loc, "main", llvmFnType);

        // Visit all the Functions
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Function_t>(*item.second)) {
                visit_symbol(*item.second);
            }
        }

        mlir::Block &entryBlock = *function.addEntryBlock(*builder);
        builder = std::make_unique<mlir::OpBuilder>(mlir::OpBuilder::atBlockBegin(
            &entryBlock));

        // Visit all the Variables
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Variable_t>(*item.second)) {
                visit_symbol(*item.second);
            }
        }

        for (size_t i = 0; i < x.n_body; i++) {
            visit_stmt(*x.m_body[i]);
        }

        mlir::LLVM::ConstantOp zero = builder->create<mlir::LLVM::ConstantOp>(
            loc, builder->getI32Type(), builder->getI32IntegerAttr(0));
        builder->create<mlir::LLVM::ReturnOp>(loc, zero.getResult());
    }
    void handle_string_variable(uint32_t h) { // Use i8* for now 
        auto i8ptr = mlir::LLVM::LLVMPointerType::get(builder->getContext());
        auto one = builder->create<mlir::LLVM::ConstantOp>(loc, builder->getI32Type(),
            builder->getI64IntegerAttr(1));// Allocate pointer-sized slot 
        mlir::Value ptr = builder->create<mlir::LLVM::AllocaOp>(loc, voidPtr, i8ptr, one);
        mlir_symtab[h] = ptr;
    }
    void visit_Variable(const ASR::Variable_t &x) {
        // A variable in a common block has no storage of its own, and this
        // backend does not lay out the storage of common blocks.
        if (ASR::CommonBlock_t *block = ASRUtils::get_common_block(&x)) {
            throw CodeGenError("COMMON blocks are not supported by the MLIR backend",
                block->base.base.loc);
        }
        if (ASRUtils::is_host_associated(x)) {
            throw CodeGenError("host association is not supported by the MLIR backend",
                x.base.base.loc);
        }
        uint32_t h = get_hash((ASR::asr_t*) &x);
        if (ASRUtils::is_character(*x.m_type)) {
            handle_string_variable(h);
            return;
        }
        mlir::Value size = builder->create<mlir::LLVM::ConstantOp>(loc,
            builder->getI32Type(), builder->getI64IntegerAttr(1));
        mlir_symtab[h] = builder->create<mlir::LLVM::AllocaOp>(loc,
            voidPtr, getType(x.m_type), size);
        if (x.m_symbolic_value) {
            this->visit_expr2(*x.m_symbolic_value);
            builder->create<mlir::LLVM::StoreOp>(loc, tmp, mlir_symtab[h]);
        }
    }

    void visit_Var(const ASR::Var_t &x) {
        ASR::Variable_t *v = ASRUtils::EXPR2VAR(&x.base);
        uint32_t h = get_hash((ASR::asr_t*) v);
        if (mlir_symtab.find(h) != mlir_symtab.end()) {
            tmp = mlir_symtab[h];
        } else {
            throw CodeGenError("Symbol '"+
                std::string(v->m_name)
                +"' not found", x.base.base.loc);
        }
    }

    template<typename T>
    void visit_Call(const T &x) {
        Vec<mlir::Value> args; args.reserve(al, x.n_args);
        Vec<mlir::Type> argTypes; argTypes.reserve(al, x.n_args);
        for (size_t i=0; i<x.n_args; i++) {
            this->visit_expr(*x.m_args[i].m_value);
            if (!is_a<ASR::Var_t>(*ASRUtils::get_past_array_physical_cast(
                    x.m_args[i].m_value))) {
                // Constant, BinOp, etc would have the type i32, but not i32*
                // So, We create an `alloca` here, store the value and
                // then, pass the alloca as an argument
                mlir::Type argType = getType(ASRUtils::extract_type(
                    ASRUtils::expr_type(x.m_args[i].m_value)));
                mlir::Value size = builder->create<mlir::LLVM::ConstantOp>(loc,
                    builder->getI32Type(), builder->getI64IntegerAttr(1));
                mlir::Value alloca = builder->create<mlir::LLVM::AllocaOp>(
                    loc, voidPtr, argType, size);
                builder->create<mlir::LLVM::StoreOp>(loc, tmp, alloca);
                args.push_back(al, alloca);
            } else {
                args.push_back(al, tmp);
            }
            argTypes.push_back(al, voidPtr);
        }
        mlir::LLVM::LLVMFuncOp fn = module->lookupSymbol<mlir::LLVM::LLVMFuncOp>(
            ASRUtils::symbol_name(x.m_name));
        if (!fn) {
            // Add function declaration
            ASR::FunctionType_t *fnType = down_cast<ASR::FunctionType_t>(
                down_cast<ASR::Function_t>(x.m_name)->m_function_signature);
            mlir::Type returnType;
            if (fnType->m_return_var_type) {
                returnType = getType(fnType->m_return_var_type);
            } else {
                returnType = mlir::LLVM::LLVMVoidType::get(context.get());
            }

            mlir::LLVM::LLVMFunctionType llvmFnType = mlir::LLVM::LLVMFunctionType::get(
                returnType, argTypes.as_vector(), false);
            mlir::OpBuilder builder0(module->getBodyRegion());
            fn = builder0.create<mlir::LLVM::LLVMFuncOp>(loc,
                ASRUtils::symbol_name(x.m_name), llvmFnType);
        }
        tmp = builder->create<mlir::LLVM::CallOp>(loc, fn,
            args.as_vector()).getResult();
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        visit_Call(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        visit_Call(x);
    }

    void visit_IntrinsicElementalFunction(const ASR::IntrinsicElementalFunction_t &x) {
        if (x.m_value) {
            this->visit_expr(*x.m_value);
            return;
        }

        switch (static_cast<ASRUtils::IntrinsicElementalFunctions>(x.m_intrinsic_id)) {
            case ASRUtils::IntrinsicElementalFunctions::Abs: {
                ASR::ttype_t *t = ASRUtils::expr_type(x.m_args[0]);
                this->visit_expr2(*x.m_args[0]);
                if (ASRUtils::is_real(*t)) {
                    tmp = builder->create<mlir::LLVM::FAbsOp>(loc, tmp);
                } else if (ASRUtils::is_integer(*t)) {
                    mlir::Type type = tmp.getType();
                    mlir::Value zero;
                    if (type == builder->getI32Type()) {
                        zero = builder->create<mlir::LLVM::ConstantOp>(loc, type,
                            builder->getI32IntegerAttr(0));
                    } else {
                        zero = builder->create<mlir::LLVM::ConstantOp>(loc, type,
                            builder->getI64IntegerAttr(0));
                    }
                    mlir::Value neg = builder->create<mlir::LLVM::SubOp>(loc, zero, tmp);
                    mlir::Value cmp = builder->create<mlir::LLVM::ICmpOp>(loc,
                        mlir::LLVM::ICmpPredicate::slt, tmp, zero);
                    tmp = builder->create<mlir::LLVM::SelectOp>(loc, cmp, neg, tmp);
                } else {
                    throw CodeGenError("Abs: unsupported type", x.base.base.loc);
                }
                break;
            }
            case ASRUtils::IntrinsicElementalFunctions::Max: {
                ASR::ttype_t *t = ASRUtils::expr_type(x.m_args[0]);
                this->visit_expr2(*x.m_args[0]);
                mlir::Value result = tmp;
                for (size_t i = 1; i < x.n_args; i++) {
                    this->visit_expr2(*x.m_args[i]);
                    if (ASRUtils::is_real(*t)) {
                        result = builder->create<mlir::LLVM::MaxNumOp>(loc, result, tmp);
                    } else if (ASRUtils::is_integer(*t)) {
                        result = builder->create<mlir::LLVM::SMaxOp>(loc, result, tmp);
                    } else {
                        throw CodeGenError("Max: unsupported type", x.base.base.loc);
                    }
                }
                tmp = result;
                break;
            }
            case ASRUtils::IntrinsicElementalFunctions::Min: {
                ASR::ttype_t *t = ASRUtils::expr_type(x.m_args[0]);
                this->visit_expr2(*x.m_args[0]);
                mlir::Value result = tmp;
                for (size_t i = 1; i < x.n_args; i++) {
                    this->visit_expr2(*x.m_args[i]);
                    if (ASRUtils::is_real(*t)) {
                        result = builder->create<mlir::LLVM::MinNumOp>(loc, result, tmp);
                    } else if (ASRUtils::is_integer(*t)) {
                        result = builder->create<mlir::LLVM::SMinOp>(loc, result, tmp);
                    } else {
                        throw CodeGenError("Min: unsupported type", x.base.base.loc);
                    }
                }
                tmp = result;
                break;
            }
            default: {
                throw CodeGenError("Either the '" + ASRUtils::IntrinsicElementalFunctionRegistry::
                        get_intrinsic_function_name(x.m_intrinsic_id) +
                        "' intrinsic is not implemented by MLIR backend or "
                        "the compile-time value is not available", x.base.base.loc);
            }
        }
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

    void visit_LogicalConstant(const ASR::LogicalConstant_t &x) {
        tmp = builder->create<mlir::LLVM::ConstantOp>(loc,
            getType(x.m_type), builder->getBoolAttr(x.m_value));
    }

    void visit_LogicalNot(const ASR::LogicalNot_t &x) {
        this->visit_expr2(*x.m_arg);
        mlir::Value one = builder->create<mlir::LLVM::ConstantOp>(loc,
            getType(x.m_type), builder->getBoolAttr(true));
        tmp = builder->create<mlir::LLVM::XOrOp>(loc, tmp, one);
    }

    void visit_LogicalBinOp(const ASR::LogicalBinOp_t &x) {
        this->visit_expr2(*x.m_left);
        mlir::Value left = tmp;
        this->visit_expr2(*x.m_right);
        mlir::Value right = tmp;
        switch (x.m_op) {
            case ASR::logicalbinopType::And: {
                tmp = builder->create<mlir::LLVM::AndOp>(loc, left, right);
                break;
            } case ASR::logicalbinopType::Or: {
                tmp = builder->create<mlir::LLVM::OrOp>(loc, left, right);
                break;
            } case ASR::logicalbinopType::Xor:
              case ASR::logicalbinopType::NEqv: {
                tmp = builder->create<mlir::LLVM::XOrOp>(loc, left, right);
                break;
            } case ASR::logicalbinopType::Eqv: {
                tmp = builder->create<mlir::LLVM::ICmpOp>(loc,
                    mlir::LLVM::ICmpPredicate::eq, left, right);
                break;
            }
        }
    }

    void visit_IntegerUnaryMinus(const ASR::IntegerUnaryMinus_t &x) {
        mlir::Type type = getType(x.m_type);
        int unaryMinus = 0;
        if (ASRUtils::extract_value(x.m_value, unaryMinus)) {
            mlir::IntegerAttr attr = builder->getIntegerAttr(type, unaryMinus);
            tmp = builder->create<mlir::LLVM::ConstantOp>(loc,
                            type, attr).getResult();
        } else {
            mlir::IntegerAttr attr = builder->getIntegerAttr(type, unaryMinus);
            mlir::Value zero = builder->create<mlir::LLVM::ConstantOp>(loc,
                type, attr);
            this->visit_expr2(*x.m_arg);
            tmp = builder->create<mlir::LLVM::SubOp>(loc, zero, tmp);
        }
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

    void visit_RealUnaryMinus(const ASR::RealUnaryMinus_t &x) {
        mlir::Type type = getType(x.m_type);
        double unaryMinus = 0.0;
        if (ASRUtils::extract_value(x.m_value, unaryMinus)) {
            mlir::FloatAttr attr = builder->getFloatAttr(type, unaryMinus);
            tmp = builder->create<mlir::LLVM::ConstantOp>(loc,
                            type, attr).getResult();
        } else {
            mlir::FloatAttr attr = builder->getFloatAttr(type, unaryMinus);
            mlir::Value zero = builder->create<mlir::LLVM::ConstantOp>(loc,
                type, attr);
            this->visit_expr2(*x.m_arg);
            tmp = builder->create<mlir::LLVM::FSubOp>(loc, zero, tmp);
        }
    }

    void visit_Cast(const ASR::Cast_t &x) {
        this->visit_expr2(*x.m_arg);
        int kind = ASRUtils::extract_kind_from_ttype_t(x.m_type);
        switch (x.m_kind) {
            case (ASR::cast_kindType::IntegerToReal): {
                mlir::Type type;
                switch (kind) {
                    case 4: {
                        type = builder->getF32Type(); break;
                    } case 8: {
                        type = builder->getF64Type(); break;
                    }
                    default:
                        throw CodeGenError("Integer constant of kind: `"+
                            std::to_string(kind) +"` is not supported yet",
                            x.base.base.loc);
                }
                tmp = builder->create<mlir::LLVM::SIToFPOp>(loc, type, tmp);
                break;
            } default: {
                throw CodeGenError("Cast of kind: `"+
                    std::to_string(x.m_kind) +"` is not supported yet",
                    x.base.base.loc);
            }
        }
    }

    void visit_StringConstant(const ASR::StringConstant_t &x) {
        tmp = createGlobalString(x.m_s);
    }

    void visit_ArrayPhysicalCast(const ASR::ArrayPhysicalCast_t &x) {
        this->visit_expr(*x.m_arg);
        switch (x.m_old) {
            case (ASR::array_physical_typeType::FixedSizeArray): {
                if (x.m_new == ASR::array_physical_typeType::PointerArray) {
                    mlir::Value zero = builder->create<mlir::LLVM::ConstantOp>(loc,
                        builder->getI64Type(), builder->getIndexAttr(0));
                    mlir::Type type = getType(x.m_type);
                    tmp = builder->create<mlir::LLVM::GEPOp>(loc, voidPtr, type,
                        tmp, mlir::ValueRange{zero, zero});
                } else {
                    throw CodeGenError("ArrayPhysicalCast to of kind: `"+
                        std::to_string(x.m_old) +"` is not supported yet",
                        x.base.base.loc);
                }
                break;
            } default: {
                throw CodeGenError("ArrayPhysicalCast from of kind: `"+
                    std::to_string(x.m_old) +"` is not supported yet",
                    x.base.base.loc);
            }
        }
    }

    void visit_ArrayBound(const ASR::ArrayBound_t &x) {
        int bound_value = -1;
        ASR::ttype_t *arr_type = ASRUtils::expr_type(x.m_v);
        if (is_a<ASR::Array_t>(*arr_type)) {
            ASR::Array_t *arr = down_cast<ASR::Array_t>(arr_type);
            int dim = -1;
            if (ASRUtils::extract_value(x.m_dim, dim)) {
                if (x.m_bound == ASR::arrayboundType::LBound) {
                    ASRUtils::extract_value(arr->m_dims[dim-1].m_start,
                        bound_value);
                } else {
                    ASRUtils::extract_value(arr->m_dims[dim-1].m_length,
                        bound_value);
                }
            } else {
                throw CodeGenError("Runtime `dim` in ArrayBound is not "
                    "supported yet", x.base.base.loc);
            }
        } else {
            throw CodeGenError("The type `"+
                ASRUtils::type_to_str_python_expr(arr_type, x.m_v)
                +"` is not supported yet", x.base.base.loc);
        }
        tmp = builder->create<mlir::LLVM::ConstantOp>(loc,
                    builder->getI32Type(),
                    builder->getI32IntegerAttr(bound_value)).getResult();
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

    void visit_RealCompare(const ASR::RealCompare_t &x) {
        this->visit_expr2(*x.m_left);
        mlir::Value left = tmp;
        this->visit_expr2(*x.m_right);
        mlir::Value right = tmp;
        mlir::LLVM::FCmpPredicate op;
        switch (x.m_op) {
            case ASR::cmpopType::Eq: {
                op = mlir::LLVM::FCmpPredicate::oeq; break;
            } case ASR::cmpopType::Lt: {
                op = mlir::LLVM::FCmpPredicate::olt; break;
            } case ASR::cmpopType::LtE: {
                op = mlir::LLVM::FCmpPredicate::ole; break;
            } case ASR::cmpopType::Gt: {
                op = mlir::LLVM::FCmpPredicate::ogt; break;
            } case ASR::cmpopType::GtE: {
                op = mlir::LLVM::FCmpPredicate::oge; break;
            } case ASR::cmpopType::NotEq: {
                op = mlir::LLVM::FCmpPredicate::one; break;
            }
            default:
                throw CodeGenError("Compare operator not supported yet",
                    x.base.base.loc);
        }
        tmp = builder->create<mlir::LLVM::FCmpOp>(loc, getType(x.m_type),
            op, left, right);
    }

    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        this->visit_expr(*x.m_v);
        mlir::Value m_v = tmp;

        LCOMPILERS_ASSERT(x.n_args == 1);
        this->visit_expr2(*x.m_args[0].m_right);
        mlir::Value idx = tmp;

        if (ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(
                x.m_args[0].m_right)) != 8) {
            idx = builder->create<mlir::LLVM::SExtOp>(loc,
                builder->getI64Type(), idx);
        }
        mlir::LLVM::ConstantOp one = builder->create<mlir::LLVM::ConstantOp>(loc,
            builder->getI64Type(), builder->getIndexAttr(1));

        idx = builder->create<mlir::LLVM::SubOp>(loc, idx, one);
        mlir::Type baseType;
        mlir::ValueRange gepIdx;
        if (ASRUtils::extract_physical_type(ASRUtils::expr_type(x.m_v))
                == ASR::array_physical_typeType::PointerArray) {
            gepIdx = {idx};
            baseType = getType(x.m_type);
        } else {
            mlir::Value zero = builder->create<mlir::LLVM::ConstantOp>(loc,
                builder->getI64Type(), builder->getIndexAttr(0));
            gepIdx = {zero, idx};
            baseType = getType(ASRUtils::expr_type(x.m_v));
        }
        tmp = builder->create<mlir::LLVM::GEPOp>(loc, voidPtr,
            baseType, m_v, gepIdx);

    }

    void visit_If(const ASR::If_t &x) {
        this->visit_expr(*x.m_test);
        mlir::Value test = tmp;

        mlir::Block *thisBlock = builder->getBlock();
        mlir::Block *thenBlock = builder->createBlock(thisBlock->getParent());
        mlir::Block *elseBlock = builder->createBlock(thisBlock->getParent());
        mlir::Block *contBlock = builder->createBlock(thisBlock->getParent());

        builder->setInsertionPointToEnd(thisBlock);
        builder->create<mlir::LLVM::CondBrOp>(loc, test, thenBlock, elseBlock);
        if (x.m_name) {
            construct_frames.push_back({x.m_name, contBlock, nullptr});
        }
        builder->setInsertionPointToStart(thenBlock);
        visit_body(x.m_body, x.n_body);
        branch_to(contBlock);

        builder->setInsertionPointToStart(elseBlock);
        visit_body(x.m_orelse, x.n_orelse);
        branch_to(contBlock);
        if (x.m_name) {
            construct_frames.pop_back();
        }

        builder->setInsertionPointToStart(contBlock);
    }

    void visit_WhileLoop(const ASR::WhileLoop_t &x) {
        mlir::Block *thisBlock = builder->getBlock();
        mlir::Block *headBlock = builder->createBlock(thisBlock->getParent());
        mlir::Block *bodyBlock = builder->createBlock(thisBlock->getParent());
        mlir::Block *contBlock = builder->createBlock(thisBlock->getParent());

        builder->setInsertionPointToEnd(thisBlock);
        builder->create<mlir::LLVM::BrOp>(loc, mlir::ValueRange{}, headBlock);

        builder->setInsertionPointToStart(headBlock);
        this->visit_expr(*x.m_test);
        builder->create<mlir::LLVM::CondBrOp>(loc, tmp, bodyBlock, contBlock);

        builder->setInsertionPointToStart(bodyBlock);
        construct_frames.push_back({x.m_name, contBlock, headBlock});
        visit_body(x.m_body, x.n_body);
        construct_frames.pop_back();
        branch_to(headBlock);

        builder->setInsertionPointToStart(contBlock);
    }

    // A counted DO loop, lowered by the rules for native CFG backends
    // (#16398). The start a, end b and step c are evaluated once:
    //
    //     i = a
    //     [c not constant: c == 0 -> "Runtime error: zero step in a DO loop"]
    //     cond_br (c > 0 ? a <= b : a >= b), body(n), orelse
    //   body(r):           // EXIT -> end, CYCLE -> latch
    //     ...
    //   latch:
    //     done = (i == b)  // step 1 or -1
    //     done = (r == 0)  // other steps
    //     i = i + c
    //     cond_br done, orelse, body(r - 1)
    //   orelse:
    //     loop-else
    //   end:
    //
    // The remaining trip count n = |b - a| / |c| is unsigned, so it is right
    // even where b - a overflows the kind of i.
    //
    // `body` writes the loop body.
    void emit_counted_loop(const ASR::do_loop_head_t &head, const char *name,
            const std::function<void()> &body, ASR::stmt_t **orelse,
            size_t n_orelse) {
        mlir::Region *region = builder->getBlock()->getParent();
        mlir::Block *endBlock = new mlir::Block();
        if (!head.m_v) {
            // A DO without loop control ends only by an EXIT, which skips
            // the loop-else.
            mlir::Block *bodyBlock = new mlir::Block();
            branch_to(bodyBlock);
            region->push_back(bodyBlock);
            builder->setInsertionPointToStart(bodyBlock);
            construct_frames.push_back({name, endBlock, bodyBlock});
            body();
            construct_frames.pop_back();
            branch_to(bodyBlock);
            region->push_back(endBlock);
            builder->setInsertionPointToStart(endBlock);
            return;
        }
        if (!ASRUtils::is_integer(*ASRUtils::expr_type(head.m_v))) {
            throw CodeGenError("the MLIR backend supports only an integer "
                "variable as the index of a DO loop", head.m_v->base.loc);
        }
        mlir::Type type = getType(ASRUtils::expr_type(head.m_v));
        this->visit_expr2(*head.m_start);
        mlir::Value start = tmp;
        this->visit_expr2(*head.m_end);
        mlir::Value end = tmp;
        this->visit_expr2(*head.m_increment);
        mlir::Value step = tmp;
        this->visit_expr(*head.m_v);
        mlir::Value v = tmp;
        builder->create<mlir::LLVM::StoreOp>(loc, start, v);

        int64_t step_value = 0;
        bool step_known = ASRUtils::extract_value(
            ASRUtils::expr_value(head.m_increment), step_value);
        if (step_known && step_value == 0) {
            throw CodeGenError("a DO loop with a zero step reached the MLIR "
                "backend", head.m_increment->base.loc);
        }
        bool unit_step = step_known && (step_value == 1 || step_value == -1);
        auto constant = [&](int64_t value) -> mlir::Value {
            return builder->create<mlir::LLVM::ConstantOp>(loc, type,
                builder->getIntegerAttr(type, value));
        };
        auto icmp = [&](mlir::LLVM::ICmpPredicate p, mlir::Value l,
                mlir::Value r) -> mlir::Value {
            return builder->create<mlir::LLVM::ICmpOp>(loc, p, l, r);
        };
        mlir::Value zero = constant(0);
        mlir::Value positive;
        if (!step_known) {
            mlir::Block *errorBlock = new mlir::Block();
            mlir::Block *okBlock = new mlir::Block();
            builder->create<mlir::LLVM::CondBrOp>(loc,
                icmp(mlir::LLVM::ICmpPredicate::eq, step, zero),
                errorBlock, okBlock);
            region->push_back(errorBlock);
            builder->setInsertionPointToStart(errorBlock);
            error_exit("Runtime error: zero step in a DO loop\n");
            region->push_back(okBlock);
            builder->setInsertionPointToStart(okBlock);
            positive = icmp(mlir::LLVM::ICmpPredicate::sgt, step, zero);
        }
        // `p` for a positive step, `n` for a negative one.
        auto by_step_sign = [&](mlir::Value p, mlir::Value n) -> mlir::Value {
            if (step_known) {
                return step_value > 0 ? p : n;
            }
            return builder->create<mlir::LLVM::SelectOp>(loc, positive, p, n);
        };
        auto sub = [&](mlir::Value l, mlir::Value r) -> mlir::Value {
            return builder->create<mlir::LLVM::SubOp>(loc, l, r);
        };
        mlir::Value enter = by_step_sign(
            icmp(mlir::LLVM::ICmpPredicate::sle, start, end),
            icmp(mlir::LLVM::ICmpPredicate::sge, start, end));

        mlir::Block *bodyBlock = new mlir::Block();
        mlir::Block *latchBlock = new mlir::Block();
        mlir::Block *orelseBlock = n_orelse > 0 ? new mlir::Block() : endBlock;
        llvm::SmallVector<mlir::Value, 1> count;
        if (!unit_step) {
            count.push_back(builder->create<mlir::LLVM::UDivOp>(loc,
                by_step_sign(sub(end, start), sub(start, end)),
                by_step_sign(step, sub(zero, step))));
            bodyBlock->addArgument(type, loc);
        }
        builder->create<mlir::LLVM::CondBrOp>(loc, enter, bodyBlock, count,
            orelseBlock, mlir::ValueRange{});

        region->push_back(bodyBlock);
        builder->setInsertionPointToStart(bodyBlock);
        construct_frames.push_back({name, endBlock, latchBlock});
        body();
        construct_frames.pop_back();
        branch_to(latchBlock);

        region->push_back(latchBlock);
        builder->setInsertionPointToStart(latchBlock);
        mlir::Value i = builder->create<mlir::LLVM::LoadOp>(loc, type, v);
        mlir::Value done;
        llvm::SmallVector<mlir::Value, 1> next_count;
        if (unit_step) {
            done = icmp(mlir::LLVM::ICmpPredicate::eq, i, end);
        } else {
            mlir::Value r = bodyBlock->getArgument(0);
            done = icmp(mlir::LLVM::ICmpPredicate::eq, r, zero);
            next_count.push_back(sub(r, constant(1)));
        }
        builder->create<mlir::LLVM::StoreOp>(loc,
            builder->create<mlir::LLVM::AddOp>(loc, i, step), v);
        builder->create<mlir::LLVM::CondBrOp>(loc, done, orelseBlock,
            mlir::ValueRange{}, bodyBlock, next_count);

        if (n_orelse > 0) {
            region->push_back(orelseBlock);
            builder->setInsertionPointToStart(orelseBlock);
            visit_body(orelse, n_orelse);
            branch_to(endBlock);
        }
        region->push_back(endBlock);
        builder->setInsertionPointToStart(endBlock);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        emit_counted_loop(x.m_head, x.m_name,
            [&]() { visit_body(x.m_body, x.n_body); },
            x.m_orelse, x.n_orelse);
    }

    // DO CONCURRENT runs its iterations in order, as nested counted loops
    // with the first index outermost. A CYCLE goes to the next iteration of
    // the innermost one, which is the next iteration of the construct.
    void emit_concurrent_loops(const ASR::DoConcurrentLoop_t &x, size_t i) {
        if (i == x.n_head) {
            visit_body(x.m_body, x.n_body);
            return;
        }
        emit_counted_loop(x.m_head[i], nullptr,
            [&]() { emit_concurrent_loops(x, i + 1); }, nullptr, 0);
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        emit_concurrent_loops(x, 0);
    }

    // An EXIT or a CYCLE without a name refers to the innermost loop.
    const ConstructFrame &construct_frame(const char *name,
            const Location &l) {
        for (auto f = construct_frames.rbegin();
                f != construct_frames.rend(); ++f) {
            if (name ? f->name && std::string(f->name) == name
                    : f->cycle != nullptr) {
                return *f;
            }
        }
        throw CodeGenError("the construct that EXIT or CYCLE refers to was "
            "not found", l);
    }

    void visit_Exit(const ASR::Exit_t &x) {
        branch_to(construct_frame(x.m_stmt_name, x.base.base.loc).exit);
    }

    void visit_Cycle(const ASR::Cycle_t &x) {
        branch_to(construct_frame(x.m_stmt_name, x.base.base.loc).cycle);
    }

    void visit_ErrorStop(const ASR::ErrorStop_t &) {
        error_exit("ERROR STOP\n");
    }

    // Print `msg` and exit with status 1.
    void error_exit(const std::string &msg) {
        mlir::OpBuilder builder0(module->getBodyRegion());
        mlir::LLVM::LLVMFuncOp printf_fn =
            module->lookupSymbol<mlir::LLVM::LLVMFuncOp>("printf");
        if (!printf_fn) {
            mlir::LLVM::LLVMVoidType voidTy =
                mlir::LLVM::LLVMVoidType::get(context.get());
            mlir::LLVM::LLVMFunctionType llvmFnType =
                mlir::LLVM::LLVMFunctionType::get(voidTy, voidPtr, true);
            printf_fn = builder0.create<mlir::LLVM::LLVMFuncOp>(
                loc, "printf", llvmFnType);
        }
        mlir::Value zero = builder->create<mlir::LLVM::ConstantOp>(loc,
            builder->getI64Type(), builder->getIndexAttr(0));
        tmp = builder->create<mlir::LLVM::GEPOp>(loc, voidPtr, voidPtr,
            createGlobalString(msg), zero);
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
            if (sf->n_dtio_bindings > 0) {
                throw CodeGenError("user-defined derived-type output is not "
                    "supported by the MLIR backend yet", l);
            }
            args.reserve(al, sf->n_args+1);
            args.push_back(al, nullptr); // Later used by `printf_fmt`
            for (size_t i=0; i<sf->n_args; i++) {
                ASR::ttype_t *t = ASRUtils::expr_type(sf->m_args[i]);
                this->visit_expr2(*sf->m_args[i]);
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
        } else if (ASRUtils::is_character(*ASRUtils::expr_type(x))) {
            this->visit_expr(*x);
            args.reserve(al, 2);
            args.push_back(al, nullptr); // Later used by `printf_fmt`
            args.push_back(al, tmp);
            fmt += " %s";
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
                mlir::LLVM::LLVMFunctionType::get(voidTy, voidPtr, true);
            printf_fn = builder0.create<mlir::LLVM::LLVMFuncOp>(
                loc, "printf", llvmFnType);
        }
        mlir::Value zero = builder->create<mlir::LLVM::ConstantOp>(loc,
            builder->getI64Type(), builder->getIndexAttr(0));
        args.p[0] = builder->create<mlir::LLVM::GEPOp>(loc,
            voidPtr, voidPtr, createGlobalString(fmt), zero);
        builder->create<mlir::LLVM::CallOp>(loc, printf_fn,
            mlir::ValueRange{args.as_vector()});
    }

    void visit_Print(const ASR::Print_t &x) {
        handle_Print(x.base.base.loc, x.m_text);
    }

    void visit_FileWrite(const ASR::FileWrite_t &x) {
        bool is_default_unit = false;
        if (x.m_unit) {
            int unit_value = -1;
            if (ASRUtils::extract_value(x.m_unit, unit_value)) {
                // In Fortran, default output unit is 6
                if (unit_value == 6) {
                    is_default_unit = true;
                }
            }
        }
        if (!is_default_unit) {
            throw CodeGenError(
                "MLIR backend currently supports only write(*,*) "
                "(default output unit)",
                x.base.base.loc
            );
        }
        LCOMPILERS_ASSERT(x.n_values == 1);
        handle_Print(x.base.base.loc, x.m_values[0]);
    }


};

Result<std::unique_ptr<MLIRModule>> asr_to_mlir(Allocator &al,
        ASR::asr_t &asr, diag::Diagnostics &diagnostics) {
    if ( !(ASR::is_a<ASR::unit_t>(asr) ||
            (ASR::is_a<ASR::Module_t>((ASR::symbol_t &)asr))) ) {
        diagnostics.diagnostics.push_back(diag::Diagnostic("Unhandled type "
            "passed as argument: 'asr' to asr_to_mlir(...)",
            diag::Level::Error, diag::Stage::CodeGen));
        Error error; return error;
    }
    ASRToMLIRVisitor v(al);
    try {
        v.visit_asr(asr);
    } catch (const CodeGenError &e) {
        diagnostics.diagnostics.push_back(e.d);
        return Error();
    }

    mlir::registerBuiltinDialectTranslation(*v.context);
    mlir::registerLLVMDialectTranslation(*v.context);
    mlir::registerOpenMPDialectTranslation(*v.context);

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

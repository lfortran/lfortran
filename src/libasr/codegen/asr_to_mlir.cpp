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
        // Visit all the Functions
        for (auto &item : x.m_symtab->get_scope()) {
            if (is_a<ASR::Function_t>(*item.second)) {
                visit_symbol(*item.second);
            }
        }
    }

    void visit_Function(const ASR::Function_t &x) {
        ASR::FunctionType_t *fnType = down_cast<ASR::FunctionType_t>(
            x.m_function_signature);
        Vec<mlir::Type> argsType; argsType.reserve(al, fnType->n_arg_types);
        // Collect all the arguments type
        for (size_t i=0; i<fnType->n_arg_types; i++) {
            mlir::Type argType = getType(fnType->m_arg_types[i]);
            argsType.push_back(al, mlir::LLVM::LLVMPointerType::get(argType));
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

        mlir::Block &entryBlock = *fn.addEntryBlock();
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

        mlir::Block &entryBlock = *function.addEntryBlock();
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
        mlir::LLVM::LLVMFuncOp fn = module->lookupSymbol<mlir::LLVM::LLVMFuncOp>(
            ASRUtils::symbol_name(x.m_name));
        if (!fn) {
            throw CodeGenError("Function '"+
                std::string(ASRUtils::symbol_name(x.m_name))
                +"' not found", x.base.base.loc);
        }
        Vec<mlir::Value> args; args.reserve(al, x.n_args);
        for (size_t i=0; i<x.n_args; i++) {
            this->visit_expr(*x.m_args[i].m_value);
            if (!is_a<ASR::Var_t>(*x.m_args[i].m_value)) {
                // Constant, BinOp, etc would have the type i32, but not i32*
                // So, We create an `alloca` here, store the value and
                // then, pass the alloca as an argument
                mlir::Type argType = mlir::LLVM::LLVMPointerType::get(
                    getType(ASRUtils::expr_type(x.m_args[i].m_value)));
                mlir::Value size = builder->create<mlir::LLVM::ConstantOp>(loc,
                    builder->getI32Type(), builder->getI64IntegerAttr(1));
                mlir::Value alloca = builder->create<mlir::LLVM::AllocaOp>(
                    loc, argType, size);
                builder->create<mlir::LLVM::StoreOp>(loc, tmp, alloca);
                args.push_back(al, alloca);
            } else {
                args.push_back(al, tmp);
            }
        }
        tmp = builder->create<mlir::LLVM::CallOp>(loc, fn,
            args.as_vector()).getResult(0);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        visit_Call(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        visit_Call(x);
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

    void visit_StringConstant(const ASR::StringConstant_t &x) {
        tmp = createGlobalString(x.m_s);
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
                ASRUtils::type_to_str_python(arr_type)
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
        mlir::Type baseType = mlir::LLVM::LLVMPointerType::get(getType(x.m_type));
        mlir::Value zero = builder->create<mlir::LLVM::ConstantOp>(loc,
            builder->getI64Type(), builder->getIndexAttr(0));
        tmp = builder->create<mlir::LLVM::GEPOp>(loc, baseType, m_v,
            mlir::ValueRange{zero, idx});
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
        builder->setInsertionPointToStart(thenBlock);
        for (size_t i=0; i<x.n_body; i++) {
            this->visit_stmt(*x.m_body[i]);
        }
        if (!(!thenBlock->empty() &&
                mlir::isa<mlir::LLVM::UnreachableOp>(thenBlock->back()))) {
            builder->create<mlir::LLVM::BrOp>(loc, mlir::ValueRange{}, contBlock);
        }

        builder->setInsertionPointToStart(elseBlock);
        for (size_t i=0; i<x.n_orelse; i++) {
            this->visit_stmt(*x.m_orelse[i]);
        }
        if (!(!elseBlock->empty() &&
            mlir::isa<mlir::LLVM::UnreachableOp>(elseBlock->back()))) {
            builder->create<mlir::LLVM::BrOp>(loc, mlir::ValueRange{}, contBlock);
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
        for (size_t i=0; i<x.n_body; i++) {
            this->visit_stmt(*x.m_body[i]);
        }
        builder->create<mlir::LLVM::BrOp>(loc, mlir::ValueRange{}, headBlock);

        builder->setInsertionPointToStart(contBlock);
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

#include "libasr/asr.h"
#include <libasr/assert.h>
#include <libasr/codegen/llvm_utils.h>
#include <libasr/codegen/llvm_array_utils.h>
#include <libasr/asr_utils.h>
#include <llvm/IR/Type.h>
#include "llvm_utils.h"

namespace LCompilers {

    namespace LLVM {

        llvm::Value* CreateStore(llvm::IRBuilder<> &builder, llvm::Value *x, llvm::Value *y) {
            LCOMPILERS_ASSERT(y->getType()->isPointerTy());
            return builder.CreateStore(x, y);
        }

        llvm::Value* lfortran_malloc(llvm::LLVMContext &context, llvm::Module &module,
                llvm::IRBuilder<> &builder, llvm::Value* arg_size) {
            std::string func_name = "_lfortran_malloc";
            arg_size = builder.CreateSExt(arg_size, llvm::Type::getInt64Ty(context));
            llvm::Function *fn = module.getFunction(func_name);
            if (!fn) {
                llvm::FunctionType *function_type = llvm::FunctionType::get(
                        llvm::Type::getInt8Ty(context)->getPointerTo(), {
                            llvm::Type::getInt64Ty(context)
                        }, false);
                fn = llvm::Function::Create(function_type,
                        llvm::Function::ExternalLinkage, func_name, module);
            }
            std::vector<llvm::Value*> args = {arg_size};
            return builder.CreateCall(fn, args);
        }

        llvm::Value* lfortran_calloc(llvm::LLVMContext &context, llvm::Module &module,
                llvm::IRBuilder<> &builder, llvm::Value* count, llvm::Value* type_size) {
            std::string func_name = "_lfortran_calloc";
            llvm::Function *fn = module.getFunction(func_name);
            if (!fn) {
                llvm::FunctionType *function_type = llvm::FunctionType::get(
                        llvm::Type::getInt8Ty(context)->getPointerTo(), {
                            llvm::Type::getInt32Ty(context),
                            llvm::Type::getInt32Ty(context)
                        }, false);
                fn = llvm::Function::Create(function_type,
                        llvm::Function::ExternalLinkage, func_name, module);
            }
            std::vector<llvm::Value*> args = {count, type_size};
            return builder.CreateCall(fn, args);
        }

        llvm::Value* lfortran_realloc(llvm::LLVMContext &context, llvm::Module &module,
                llvm::IRBuilder<> &builder, llvm::Value* ptr, llvm::Value* arg_size) {
            std::string func_name = "_lfortran_realloc";
            llvm::Function *fn = module.getFunction(func_name);
            if (!fn) {
                llvm::FunctionType *function_type = llvm::FunctionType::get(
                        llvm::Type::getInt8Ty(context)->getPointerTo(), {
                            llvm::Type::getInt8Ty(context)->getPointerTo(),
                            llvm::Type::getInt64Ty(context)
                        }, false);
                fn = llvm::Function::Create(function_type,
                        llvm::Function::ExternalLinkage, func_name, module);
            }
            std::vector<llvm::Value*> args = {
                builder.CreateBitCast(ptr, llvm::Type::getInt8Ty(context)->getPointerTo()),
                builder.CreateSExt(arg_size, llvm::Type::getInt64Ty(context))
            };
            return builder.CreateCall(fn, args);
        }

        llvm::Value* lfortran_free(llvm::LLVMContext &context, llvm::Module &module,
                                   llvm::IRBuilder<> &builder, llvm::Value* ptr) {
            std::string func_name = "_lfortran_free";
            llvm::Function *fn = module.getFunction(func_name);
            if (!fn) {
                llvm::FunctionType *function_type = llvm::FunctionType::get(
                        llvm::Type::getVoidTy(context), {
                            llvm::Type::getInt8Ty(context)->getPointerTo()
                        }, false);
                fn = llvm::Function::Create(function_type,
                        llvm::Function::ExternalLinkage, func_name, module);
            }
            std::vector<llvm::Value*> args = {
                builder.CreateBitCast(ptr, llvm::Type::getInt8Ty(context)->getPointerTo()),
            };
            return builder.CreateCall(fn, args);
        }
        bool is_llvm_pointer(const ASR::ttype_t& asr_type) {
            /*
                True : When Pointer or Allocatable, if and only if it's not a standalond string type.
            */
            return (ASR::is_a<ASR::Pointer_t>(asr_type) ||
                    ASR::is_a<ASR::Allocatable_t>(asr_type)) &&
                    !ASRUtils::is_string_only(const_cast<ASR::ttype_t*>(&asr_type));
        }
    } // namespace LLVM

    LLVMUtils::LLVMUtils(llvm::LLVMContext& context,
        llvm::IRBuilder<>* _builder, std::string& der_type_name_,
        std::map<std::string, llvm::StructType*>& name2dertype_,
        std::map<std::string, llvm::StructType*>& name2dercontext_,
        std::vector<std::string>& struct_type_stack_,
        std::map<std::string, std::string>& dertype2parent_,
        std::map<std::string, std::map<std::string, int>>& name2memidx_,
        CompilerOptions &compiler_options_,
        std::unordered_map<std::uint32_t, std::unordered_map<std::string, llvm::Type*>>& arr_arg_type_cache_,
        std::map<std::string, std::pair<llvm::Type*, llvm::Type*>>& fname2arg_type_,
        std::map<llvm::Value *, llvm::Type *> &ptr_type_, std::map<uint64_t, llvm::Value*> &llvm_symtab_):
        context(context), builder(std::move(_builder)), str_cmp_itr(nullptr), der_type_name(der_type_name_),
        name2dertype(name2dertype_), name2dercontext(name2dercontext_),
        struct_type_stack(struct_type_stack_), dertype2parent(dertype2parent_),
        name2memidx(name2memidx_), arr_arg_type_cache(arr_arg_type_cache_), fname2arg_type(fname2arg_type_),
        ptr_type(ptr_type_), dict_api_lp(nullptr), dict_api_sc(nullptr),
        set_api_lp(nullptr), set_api_sc(nullptr), compiler_options(compiler_options_), llvm_symtab(llvm_symtab_) {
            std::vector<llvm::Type*> els_4 = {
            llvm::Type::getFloatTy(context),
            llvm::Type::getFloatTy(context)};
            std::vector<llvm::Type*> els_8 = {
                llvm::Type::getDoubleTy(context),
                llvm::Type::getDoubleTy(context)};
            std::vector<llvm::Type*> els_4_ptr = {
                llvm::Type::getFloatTy(context)->getPointerTo(),
                llvm::Type::getFloatTy(context)->getPointerTo()};
            std::vector<llvm::Type*> els_8_ptr = {
                llvm::Type::getDoubleTy(context)->getPointerTo(),
                llvm::Type::getDoubleTy(context)->getPointerTo()};
            std::vector<llvm::Type*> string_descriptor_members {
                llvm::Type::getInt8Ty(context)->getPointerTo(), /* char* */
                llvm::Type::getInt64Ty(context)  /*length*/ };
            complex_type_4 = llvm::StructType::create(context, els_4, "complex_4", true);
            complex_type_8 = llvm::StructType::create(context, els_8, "complex_8", true);
            complex_type_4_ptr = llvm::StructType::create(context, els_4_ptr, "complex_4_ptr");
            complex_type_8_ptr = llvm::StructType::create(context, els_8_ptr, "complex_8_ptr");
            character_type = llvm::Type::getInt8Ty(context)->getPointerTo();
            string_descriptor = llvm::StructType::create(context,string_descriptor_members, "string_descriptor", true);
        }

    void LLVMUtils::set_module(llvm::Module* module_) {
        module = module_;
    }

    llvm::Type* LLVMUtils::getMemberType(ASR::ttype_t* mem_type, ASR::Variable_t* member,
        llvm::Module* module) {
        llvm::Type* llvm_mem_type = nullptr;
        switch( mem_type->type ) {
            case ASR::ttypeType::Integer: {
                int a_kind = ASR::down_cast<ASR::Integer_t>(mem_type)->m_kind;
                llvm_mem_type = getIntType(a_kind);
                break;
            }
            case ASR::ttypeType::Real: {
                int a_kind = ASR::down_cast<ASR::Real_t>(mem_type)->m_kind;
                llvm_mem_type = getFPType(a_kind);
                break;
            }
            case ASR::ttypeType::StructType: {
                llvm_mem_type = getStructType(ASR::down_cast<ASR::Struct_t>(member->m_type_declaration), module);
                break;
            }
            case ASR::ttypeType::EnumType: {
                llvm_mem_type = llvm::Type::getInt32Ty(context);
                break ;
            }
            case ASR::ttypeType::UnionType: {
                llvm_mem_type = getUnion(mem_type, module);
                break;
            }
            case ASR::ttypeType::Allocatable: {
                ASR::Allocatable_t* ptr_type = ASR::down_cast<ASR::Allocatable_t>(mem_type);
                llvm_mem_type = getMemberType(ptr_type->m_type, member, module)->getPointerTo();
                break;
            }
            case ASR::ttypeType::Pointer: {
                ASR::Pointer_t* ptr_type = ASR::down_cast<ASR::Pointer_t>(mem_type);
                llvm_mem_type = getMemberType(ptr_type->m_type, member, module)->getPointerTo();
                break;
            }
            case ASR::ttypeType::Complex: {
                int a_kind = ASR::down_cast<ASR::Complex_t>(mem_type)->m_kind;
                llvm_mem_type = getComplexType(a_kind);
                break;
            }
            case ASR::ttypeType::String: {
                llvm_mem_type = character_type;
                break;
            }
            case ASR::ttypeType::CPtr: {
                llvm_mem_type = llvm::Type::getVoidTy(context)->getPointerTo();
                break;
            }
            default:
                throw CodeGenError("Cannot identify the type of member, '" +
                                    std::string(member->m_name) +
                                    "' in derived type, '" + der_type_name + "'.",
                                    member->base.base.loc);
        }
        return llvm_mem_type;
    }

    void LLVMUtils::createStructTypeContext(ASR::Struct_t* der_type) {
        std::string der_type_name = std::string(der_type->m_name);
        if (name2dercontext.find(der_type_name) == name2dercontext.end() ) {
            llvm::StructType* der_type_llvm = llvm::StructType::create(context,
                                {},
                                der_type_name,
                                der_type->m_is_packed);
            name2dercontext[der_type_name] = der_type_llvm;
            if( der_type->m_parent != nullptr ) {
                ASR::Struct_t *par_der_type = ASR::down_cast<ASR::Struct_t>(
                                                ASRUtils::symbol_get_past_external(der_type->m_parent));
                createStructTypeContext(par_der_type);
            }
            for( size_t i = 0; i < der_type->n_members; i++ ) {
                std::string member_name = der_type->m_members[i];
                ASR::symbol_t* sym = der_type->m_symtab->get_symbol(member_name);
                if (ASR::is_a<ASR::Struct_t>(*sym)) {
                    ASR::Struct_t *d_type = ASR::down_cast<ASR::Struct_t>(sym);
                    createStructTypeContext(d_type);
                }
            }
        }
    }

    llvm::Type* LLVMUtils::getStructType(ASR::Struct_t* der_type, llvm::Module* module, bool is_pointer) {
        std::string der_type_name = std::string(der_type->m_name);
        createStructTypeContext(der_type);
        if (std::find(struct_type_stack.begin(), struct_type_stack.end(),
                        der_type_name) != struct_type_stack.end()) {
            LCOMPILERS_ASSERT(name2dercontext.find(der_type_name) != name2dercontext.end());
            return name2dercontext[der_type_name];
        }
        struct_type_stack.push_back(der_type_name);
        llvm::StructType** der_type_llvm;
        if (name2dertype.find(der_type_name) != name2dertype.end() ) {
            der_type_llvm = &name2dertype[der_type_name];
        } else {
            der_type_llvm = &name2dercontext[der_type_name];
            std::vector<llvm::Type*> member_types;
            int member_idx = 0;
            if( der_type->m_parent != nullptr ) {
                ASR::Struct_t *par_der_type = ASR::down_cast<ASR::Struct_t>(
                                                        ASRUtils::symbol_get_past_external(der_type->m_parent));
                llvm::Type* par_llvm = getStructType(par_der_type, module);
                member_types.push_back(par_llvm);
                dertype2parent[der_type_name] = std::string(par_der_type->m_name);
                member_idx += 1;
            }
            Allocator al(1024);
            for( size_t i = 0; i < der_type->n_members; i++ ) {
                std::string member_name = der_type->m_members[i];
                ASR::Variable_t* member = ASR::down_cast<ASR::Variable_t>(der_type->m_symtab->get_symbol(member_name));
                llvm::Type* llvm_mem_type = get_type_from_ttype_t_util(ASRUtils::EXPR(ASR::make_Var_t(
                    al, member->base.base.loc, &member->base)), member->m_type, module, member->m_abi);
                member_types.push_back(llvm_mem_type);
                name2memidx[der_type_name][std::string(member->m_name)] = member_idx;
                member_idx++;
            }
            (*der_type_llvm)->setBody(member_types, true);
            name2dertype[der_type_name] = *der_type_llvm;
        }
        struct_type_stack.pop_back();
        if ( is_pointer ) {
            return (*der_type_llvm)->getPointerTo();
        }
        return (llvm::Type*) *der_type_llvm;
    }

    llvm::Type* LLVMUtils::getUnion(ASR::Union_t* union_type,
        llvm::Module* module, bool is_pointer) {
        std::string union_type_name = std::string(union_type->m_name);
        llvm::StructType* union_type_llvm = nullptr;
        if( name2dertype.find(union_type_name) != name2dertype.end() ) {
            union_type_llvm = name2dertype[union_type_name];
        } else {
            const std::map<std::string, ASR::symbol_t*>& scope = union_type->m_symtab->get_scope();
            llvm::DataLayout data_layout(module->getDataLayout());
            llvm::Type* max_sized_type = nullptr;
            size_t max_type_size = 0;
            for( auto itr = scope.begin(); itr != scope.end(); itr++ ) {
                ASR::Variable_t* member = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(itr->second));
                llvm::Type* llvm_mem_type = getMemberType(member->m_type, member, module);
                size_t type_size = data_layout.getTypeAllocSize(llvm_mem_type);
                if( max_type_size < type_size ) {
                    max_sized_type = llvm_mem_type;
                    type_size = max_type_size;
                }
            }
            union_type_llvm = llvm::StructType::create(context, {max_sized_type}, union_type_name);
            name2dertype[union_type_name] = union_type_llvm;
        }
        if( is_pointer ) {
            return union_type_llvm->getPointerTo();
        }
        return (llvm::Type*) union_type_llvm;
    }

    llvm::Type* LLVMUtils::getUnion(ASR::ttype_t* _type, llvm::Module* module, bool is_pointer) {
        ASR::UnionType_t* union_ = ASR::down_cast<ASR::UnionType_t>(_type);
        ASR::symbol_t* union_sym = ASRUtils::symbol_get_past_external(union_->m_union_type);
        ASR::Union_t* union_type = ASR::down_cast<ASR::Union_t>(union_sym);
        return getUnion(union_type, module, is_pointer);
    }

    llvm::Type* LLVMUtils::getClassType(ASR::Struct_t* der_type, bool is_pointer) {
        std::string der_type_name = std::string(der_type->m_name) + std::string("_polymorphic");
        llvm::StructType* der_type_llvm = nullptr;
        if( name2dertype.find(der_type_name) != name2dertype.end() ) {
            der_type_llvm = name2dertype[der_type_name];
        } else {
            std::vector<llvm::Type*> member_types;
            member_types.push_back(getIntType(8));
            if( der_type_name == "~unlimited_polymorphic_type_polymorphic" ) {
                member_types.push_back(llvm::Type::getVoidTy(context)->getPointerTo());
            } else {
                member_types.push_back(getStructType(der_type, module, true));
            }
            der_type_llvm = llvm::StructType::create(context, member_types, der_type_name);
            name2dertype[der_type_name] = der_type_llvm;
        }
        LCOMPILERS_ASSERT(der_type_llvm != nullptr);
        if( is_pointer ) {
            return der_type_llvm->getPointerTo();
        }
        return (llvm::Type*) der_type_llvm;
    }

    llvm::Type* LLVMUtils::getFPType(int a_kind, bool get_pointer) {
        llvm::Type* type_ptr = nullptr;
        if( get_pointer ) {
            switch(a_kind)
            {
                case 4:
                    type_ptr = llvm::Type::getFloatTy(context)->getPointerTo();
                    break;
                case 8:
                    type_ptr =  llvm::Type::getDoubleTy(context)->getPointerTo();
                    break;
                default:
                    throw CodeGenError("Only 32 and 64 bits real kinds are supported.");
            }
        } else {
            switch(a_kind)
            {
                case 4:
                    type_ptr = llvm::Type::getFloatTy(context);
                    break;
                case 8:
                    type_ptr = llvm::Type::getDoubleTy(context);
                    break;
                default:
                    throw CodeGenError("Only 32 and 64 bits real kinds are supported.");
            }
        }
        return type_ptr;
    }

    llvm::Type* LLVMUtils::getComplexType(int a_kind, bool get_pointer) {
        llvm::Type* type = nullptr;
        switch(a_kind)
        {
            case 4:
                type = complex_type_4;
                break;
            case 8:
                type = complex_type_8;
                break;
            default:
                throw CodeGenError("Only 32 and 64 bits complex kinds are supported.");
        }
        if( type != nullptr ) {
            if( get_pointer ) {
                return type->getPointerTo();
            } else {
                return type;
            }
        }
        return nullptr;
    }

    llvm::Type* LLVMUtils::get_StringType(ASR::ttype_t* type){
        ASR::String_t* str = ASR::down_cast<ASR::String_t>(ASRUtils::extract_type(type));
        switch (str->m_physical_type)
        {
        case ASR::DescriptorString:
            return string_descriptor;
        case ASR::CChar:
            return llvm::Type::getInt8Ty(context);
        default:
            throw LCompilersException("Unhandled string physical type");
            break;
        }
    }

    llvm::Type* LLVMUtils::get_el_type(ASR::expr_t* expr, ASR::ttype_t* m_type_, llvm::Module* module) {
        int a_kind = ASRUtils::extract_kind_from_ttype_t(m_type_);
        llvm::Type* el_type = nullptr;
        bool is_pointer = LLVM::is_llvm_pointer(*m_type_);
        ASR::ttype_t* m_type = ASRUtils::type_get_past_pointer(m_type_);
        switch(m_type->type) {
            case ASR::ttypeType::Integer: {
                el_type = getIntType(a_kind, is_pointer);
                break;
            }
            case ASR::ttypeType::UnsignedInteger: {
                el_type = getIntType(a_kind, is_pointer);
                break;
            }
            case ASR::ttypeType::Real: {
                el_type = getFPType(a_kind, is_pointer);
                break;
            }
            case ASR::ttypeType::Complex: {
                el_type = getComplexType(a_kind, is_pointer);
                break;
            }
            case ASR::ttypeType::Logical: {
                el_type = llvm::Type::getInt1Ty(context);
                break;
            }
            case ASR::ttypeType::CPtr: {
                el_type = llvm::Type::getVoidTy(context)->getPointerTo();
                break;
            }
            case ASR::ttypeType::StructType: {
                if (ASR::down_cast<ASR::StructType_t>(m_type)->m_is_cstruct) {
                    el_type = getStructType(ASR::down_cast<ASR::Struct_t>(
                                                ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(expr))),
                                            module);
                } else {
                    el_type = getClassType(ASR::down_cast<ASR::Struct_t>(
                                                ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(expr))));
                }
                break;
            }
            case ASR::ttypeType::UnionType: {
                el_type = getUnion(m_type_, module);
                break;
            }
            case ASR::ttypeType::String: {
                el_type = get_StringType(m_type_);
                break;
            }
            default:
                LCOMPILERS_ASSERT(false);
                break;
        }
        return el_type;
    }

    int32_t get_type_size(ASR::ttype_t* asr_type, llvm::Type* llvm_type,
                          int32_t a_kind, llvm::Module* module) {
        if( LLVM::is_llvm_struct(asr_type) ||
            ASR::is_a<ASR::String_t>(*asr_type) ||
            ASR::is_a<ASR::Complex_t>(*asr_type) ) {
            llvm::DataLayout data_layout(module->getDataLayout());
            return data_layout.getTypeAllocSize(llvm_type);
        }
        return a_kind;
    }

    llvm::Type* LLVMUtils::get_dict_type(ASR::expr_t* dict_expr, ASR::ttype_t* asr_type, llvm::Module* module) {
        ASR::Dict_t* asr_dict = ASR::down_cast<ASR::Dict_t>(asr_type);
        bool is_local_array_type = false, is_local_malloc_array_type = false;
        bool is_local_list = false;
        ASR::dimension_t* local_m_dims = nullptr;
        int local_n_dims = 0;
        int local_a_kind = -1;
        ASR::storage_typeType local_m_storage = ASR::storage_typeType::Default;
        llvm::Type* key_llvm_type = get_type_from_ttype_t(dict_expr, asr_dict->m_key_type, nullptr, local_m_storage,
                                                            is_local_array_type, is_local_malloc_array_type,
                                                            is_local_list, local_m_dims, local_n_dims,
                                                            local_a_kind, module);
        int32_t key_type_size = get_type_size(asr_dict->m_key_type, key_llvm_type, local_a_kind, module);
        llvm::Type* value_llvm_type = get_type_from_ttype_t(dict_expr, asr_dict->m_value_type, nullptr, local_m_storage,
                                                            is_local_array_type, is_local_malloc_array_type,
                                                            is_local_list, local_m_dims, local_n_dims,
                                                            local_a_kind, module);
        int32_t value_type_size = get_type_size(asr_dict->m_value_type, value_llvm_type, local_a_kind, module);
        std::string key_type_code = ASRUtils::get_type_code(asr_dict->m_key_type);
        std::string value_type_code = ASRUtils::get_type_code(asr_dict->m_value_type);
        set_dict_api(asr_dict);
        return dict_api->get_dict_type(key_type_code, value_type_code, key_type_size,
                                        value_type_size, key_llvm_type, value_llvm_type);
    }

    llvm::Type* LLVMUtils::get_set_type(ASR::expr_t* set_expr, ASR::ttype_t* asr_type, llvm::Module* module) {
        ASR::Set_t* asr_set = ASR::down_cast<ASR::Set_t>(asr_type);
        bool is_local_array_type = false, is_local_malloc_array_type = false;
        bool is_local_list = false;
        ASR::dimension_t* local_m_dims = nullptr;
        int local_n_dims = 0;
        int local_a_kind = -1;
        ASR::storage_typeType local_m_storage = ASR::storage_typeType::Default;
        llvm::Type* el_llvm_type = get_type_from_ttype_t(set_expr, asr_set->m_type, nullptr, local_m_storage,
                                                            is_local_array_type, is_local_malloc_array_type,
                                                            is_local_list, local_m_dims, local_n_dims,
                                                            local_a_kind, module);
        int32_t el_type_size = get_type_size(asr_set->m_type, el_llvm_type, local_a_kind, module);
        std::string el_type_code = ASRUtils::get_type_code(asr_set->m_type);
        set_set_api(asr_set);
        return set_api->get_set_type(el_type_code, el_type_size,
                                                 el_llvm_type);
    }

    llvm::Type* LLVMUtils::get_arg_type_from_ttype_t(ASR::expr_t* arg_expr, ASR::ttype_t* asr_type,
        ASR::symbol_t *type_declaration, ASR::abiType m_abi, ASR::abiType arg_m_abi,
        ASR::storage_typeType m_storage, bool arg_m_value_attr, int& n_dims,
        int& a_kind, bool& is_array_type, ASR::intentType arg_intent, llvm::Module* module,
        bool get_pointer) {
        llvm::Type* type = nullptr;

        #define handle_llvm_pointers2() bool is_pointer_ = ASRUtils::is_class_type(t2) || \
            (ASR::is_a<ASR::String_t>(*t2) && arg_m_abi != ASR::abiType::BindC); \
            type = get_arg_type_from_ttype_t(arg_expr, t2, type_declaration, m_abi, arg_m_abi, \
                        m_storage, arg_m_value_attr, n_dims, a_kind, \
                        is_array_type, arg_intent, module, get_pointer); \
            if( !is_pointer_ ) { \
                type = type->getPointerTo(); \
            } \

        switch (asr_type->type) {
            case ASR::ttypeType::Array: {
                ASR::Array_t* v_type = ASR::down_cast<ASR::Array_t>(asr_type);
                switch( v_type->m_physical_type ) {
                    case ASR::array_physical_typeType::DescriptorArray: {
                        is_array_type = true;
                        llvm::Type* el_type = get_el_type(arg_expr, v_type->m_type, module);
                        type = arr_api->get_array_type(arg_expr, asr_type, el_type, get_pointer);
                        break;
                    }
                    case ASR::array_physical_typeType::PointerToDataArray: {
                        type = nullptr;
                        if( ASR::is_a<ASR::Complex_t>(*v_type->m_type) ) {
                            ASR::Complex_t* complex_t = ASR::down_cast<ASR::Complex_t>(v_type->m_type);
                            type = getComplexType(complex_t->m_kind, true);
                        }


                        if( type == nullptr ) {
                            type = get_type_from_ttype_t_util(arg_expr, v_type->m_type, module, arg_m_abi)->getPointerTo();
                        }
                        break;
                    }
                    case ASR::array_physical_typeType::UnboundedPointerToDataArray: {
                        type = nullptr;
                        if( ASR::is_a<ASR::Complex_t>(*v_type->m_type) ) {
                            ASR::Complex_t* complex_t = ASR::down_cast<ASR::Complex_t>(v_type->m_type);
                            type = getComplexType(complex_t->m_kind, true);
                        }


                        if( type == nullptr ) {
                            type = get_type_from_ttype_t_util(arg_expr, v_type->m_type, module, arg_m_abi)->getPointerTo();
                        }
                        break;
                    }
                    case ASR::array_physical_typeType::FixedSizeArray: {
                        type = llvm::ArrayType::get(get_el_type(arg_expr, v_type->m_type, module),
                                        ASRUtils::get_fixed_size_of_array(
                                            v_type->m_dims, v_type->n_dims))->getPointerTo();
                        break;
                    }
                    case ASR::array_physical_typeType::StringArraySinglePointer: {
                        // type = character_type->getPointerTo();
                        // is_array_type = true;
                        // llvm::Type* el_type = get_el_type(v_type->m_type, module);
                        // type = arr_api->get_array_type(asr_type, el_type, get_pointer);
                        // break;
                        if (ASRUtils::is_fixed_size_array(v_type->m_dims, v_type->n_dims)) {
                            // llvm_type = character_type; -- @character_01.c = internal global i8* null
                            // llvm_type = character_type->getPointerTo(); -- @character_01.c = internal global i8** null
                            // llvm_type = llvm::ArrayType::get(character_type,
                            //     ASRUtils::get_fixed_size_of_array(v_type->m_dims, v_type->n_dims))->getPointerTo();
                            // -- @character_01 = internal global [2 x i8*]* zeroinitializer

                            type = llvm::ArrayType::get(character_type,
                                ASRUtils::get_fixed_size_of_array(v_type->m_dims, v_type->n_dims));
                            break;
                        } else if (ASRUtils::is_dimension_empty(v_type->m_dims, v_type->n_dims)) {
                            // Treat it as a DescriptorArray
                            is_array_type = true;
                            llvm::Type* el_type = character_type;
                            type = arr_api->get_array_type(arg_expr, asr_type, el_type);
                            break;
                        } else {
                            LCOMPILERS_ASSERT(false);
                            break;
                        }
                    }
                    default: {
                        LCOMPILERS_ASSERT(false);
                    }
                }
                break;
            }
            case (ASR::ttypeType::Integer) : {
                ASR::Integer_t* v_type = ASR::down_cast<ASR::Integer_t>(asr_type);
                a_kind = v_type->m_kind;
                if (arg_m_abi == ASR::abiType::BindC
                    && arg_m_value_attr) {
                    type = getIntType(a_kind, false);
                } else {
                    type = getIntType(a_kind, true);
                }
                break;
            }
            case (ASR::ttypeType::UnsignedInteger) : {
                ASR::UnsignedInteger_t* v_type = ASR::down_cast<ASR::UnsignedInteger_t>(asr_type);
                a_kind = v_type->m_kind;
                if (arg_m_abi == ASR::abiType::BindC
                    && arg_m_value_attr) {
                    type = getIntType(a_kind, false);
                } else {
                    type = getIntType(a_kind, true);
                }
                break;
            }
            case (ASR::ttypeType::Pointer) : {
                ASR::ttype_t *t2 = ASRUtils::type_get_past_pointer(asr_type);
                handle_llvm_pointers2()
                break;
            }
            case (ASR::ttypeType::Allocatable) : {
                ASR::ttype_t *t2 = ASRUtils::type_get_past_allocatable(asr_type);
                handle_llvm_pointers2()
                break;
            }
            case (ASR::ttypeType::Real) : {
                ASR::Real_t* v_type = ASR::down_cast<ASR::Real_t>(asr_type);
                a_kind = v_type->m_kind;
                if (arg_m_abi == ASR::abiType::BindC
                    && arg_m_value_attr) {
                    type = getFPType(a_kind, false);
                } else {
                    type = getFPType(a_kind, true);
                }
                break;
            }
            case (ASR::ttypeType::Complex) : {
                ASR::Complex_t* v_type = ASR::down_cast<ASR::Complex_t>(asr_type);
                a_kind = v_type->m_kind;
                if (m_abi != ASR::abiType::BindC
                    || startswith(compiler_options.target, "wasm")) {
                    type = getComplexType(a_kind, true);
                } else {
                    if (arg_m_abi == ASR::abiType::BindC
                            && arg_m_value_attr) {
                        if (a_kind == 4) {
                            if (compiler_options.platform == Platform::Windows) {
                                // type_fx2 is i64
                                llvm::Type* type_fx2 = llvm::Type::getInt64Ty(context);
                                type = type_fx2;
                            } else if (compiler_options.platform == Platform::macOS_ARM) {
                                // type_fx2 is [2 x float]
                                llvm::Type* type_fx2 = llvm::ArrayType::get(llvm::Type::getFloatTy(context), 2);
                                type = type_fx2;
                            } else {
                                // type_fx2 is <2 x float>
                                llvm::Type* type_fx2 = FIXED_VECTOR_TYPE::get(llvm::Type::getFloatTy(context), 2);
                                type = type_fx2;
                            }
                        } else {
                            LCOMPILERS_ASSERT(a_kind == 8)
                            if (compiler_options.platform == Platform::Windows) {
                                // 128 bit aggregate type is passed by reference
                                type = getComplexType(a_kind, true);
                            } else {
                                // Pass by value
                                type = getComplexType(a_kind, false);
                            }
                        }
                    } else {
                        type = getComplexType(a_kind, true);
                    }
                }
                break;
            }
            case (ASR::ttypeType::String) : {
                a_kind = ASR::down_cast<ASR::String_t>(asr_type)->m_kind;
                if(arg_m_value_attr){
                    throw LCompilersException("Unhandled : String parameter with `value` attribute");
                } else {
                    type = get_StringType(asr_type)->getPointerTo();
                }
                break;
            }
            case (ASR::ttypeType::Logical) : {
                ASR::Logical_t* v_type = ASR::down_cast<ASR::Logical_t>(asr_type);
                a_kind = v_type->m_kind;
                if (arg_m_abi == ASR::abiType::BindC
                    && arg_m_value_attr) {
                    type = llvm::Type::getInt1Ty(context);
                } else {
                    type = llvm::Type::getInt1Ty(context)->getPointerTo();
                }
                break;
            }
            case (ASR::ttypeType::StructType) : {
                if (type_declaration) {
                    type_declaration = ASRUtils::symbol_get_past_external(type_declaration);
                    if (ASR::down_cast<ASR::StructType_t>(asr_type)->m_is_cstruct) {
                        type = getStructType(
                            ASR::down_cast<ASR::Struct_t>(type_declaration), module, true);
                    } else {
                        type = getClassType(ASR::down_cast<ASR::Struct_t>(type_declaration), true);
                    }
                } else {
                    if (ASR::down_cast<ASR::StructType_t>(asr_type)->m_is_cstruct) {
                        type = getStructType(
                            ASR::down_cast<ASR::Struct_t>(
                                ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(arg_expr))),
                            module,
                            true);
                    } else {
                        type
                            = getClassType(ASR::down_cast<ASR::Struct_t>(
                                               ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(arg_expr))),
                                           true);
                    }
                }
                break;
            }
            case (ASR::ttypeType::CPtr) : {
                type = llvm::Type::getVoidTy(context)->getPointerTo();
                break;
            }
            case (ASR::ttypeType::Tuple) : {
                type = get_type_from_ttype_t_util(arg_expr, asr_type, module)->getPointerTo();
                break;
            }
            case (ASR::ttypeType::List) : {
                bool is_array_type = false, is_malloc_array_type = false;
                bool is_list = true;
                ASR::dimension_t *m_dims = nullptr;
                ASR::List_t* asr_list = ASR::down_cast<ASR::List_t>(asr_type);
                llvm::Type* el_llvm_type = get_type_from_ttype_t(arg_expr, asr_list->m_type, nullptr, m_storage,
                                                                 is_array_type,
                                                                 is_malloc_array_type,
                                                                 is_list, m_dims, n_dims,
                                                                 a_kind, module, m_abi);
                int32_t type_size = -1;
                if( LLVM::is_llvm_struct(asr_list->m_type) ||
                    ASR::is_a<ASR::String_t>(*asr_list->m_type) ||
                    ASR::is_a<ASR::Complex_t>(*asr_list->m_type) ) {
                    llvm::DataLayout data_layout(module->getDataLayout());
                    type_size = data_layout.getTypeAllocSize(el_llvm_type);
                } else {
                    type_size = a_kind;
                }
                std::string el_type_code = ASRUtils::get_type_code(asr_list->m_type);
                type = list_api->get_list_type(el_llvm_type, el_type_code, type_size)->getPointerTo();
                break;
            }
            case ASR::ttypeType::EnumType: {
                if (arg_m_abi == ASR::abiType::BindC
                    && arg_m_value_attr) {
                    type = llvm::Type::getInt32Ty(context);
                } else {
                    type = llvm::Type::getInt32Ty(context)->getPointerTo();
                }
                break ;
            }
            case (ASR::ttypeType::Dict): {
                ASR::Dict_t* asr_dict = ASR::down_cast<ASR::Dict_t>(asr_type);
                std::string key_type_code = ASRUtils::get_type_code(asr_dict->m_key_type);
                std::string value_type_code = ASRUtils::get_type_code(asr_dict->m_value_type);

                bool is_array_type = false, is_malloc_array_type = false;
                bool is_list = false;
                ASR::dimension_t* m_dims = nullptr;
                llvm::Type* key_llvm_type = get_type_from_ttype_t(arg_expr, asr_dict->m_key_type, type_declaration, m_storage,
                                                                  is_array_type,
                                                                  is_malloc_array_type,
                                                                  is_list, m_dims, n_dims,
                                                                  a_kind, module, m_abi);
                llvm::Type* value_llvm_type = get_type_from_ttype_t(arg_expr, asr_dict->m_value_type, type_declaration, m_storage,
                                                                    is_array_type,
                                                                    is_malloc_array_type,
                                                                    is_list, m_dims, n_dims,
                                                                    a_kind, module, m_abi);
                int32_t key_type_size = get_type_size(asr_dict->m_key_type, key_llvm_type, a_kind, module);
                int32_t value_type_size = get_type_size(asr_dict->m_value_type, value_llvm_type, a_kind, module);
                set_dict_api(asr_dict);
                type = dict_api->get_dict_type(key_type_code, value_type_code,
                                                key_type_size, value_type_size,
                                                key_llvm_type, value_llvm_type)->getPointerTo();
                break;
            }
            case (ASR::ttypeType::Set): {
                ASR::Set_t* asr_set = ASR::down_cast<ASR::Set_t>(asr_type);
                std::string el_type_code = ASRUtils::get_type_code(asr_set->m_type);

                bool is_array_type = false, is_malloc_array_type = false;
                bool is_list = false;
                ASR::dimension_t* m_dims = nullptr;
                llvm::Type* el_llvm_type = get_type_from_ttype_t(arg_expr, asr_set->m_type, type_declaration, m_storage,
                                                                  is_array_type,
                                                                  is_malloc_array_type,
                                                                  is_list, m_dims, n_dims,
                                                                  a_kind, module, m_abi);
                int32_t el_type_size = get_type_size(asr_set->m_type, el_llvm_type, a_kind, module);
                set_set_api(asr_set);
                type = set_api->get_set_type(el_type_code, el_type_size, el_llvm_type)->getPointerTo();
                break;
            }
            case ASR::ttypeType::FunctionType: {
                ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(
                    ASRUtils::symbol_get_past_external(type_declaration));
                type = get_function_type(*fn, module)->getPointerTo();
                break ;
            }
            default :
                LCOMPILERS_ASSERT(false);
        }
        return type;
    }

    void LLVMUtils::set_dict_api(ASR::Dict_t* dict_type) {
        if( ASR::is_a<ASR::String_t>(*dict_type->m_key_type) ) {
            dict_api = dict_api_sc;
        } else {
            dict_api = dict_api_lp;
        }
    }

    void LLVMUtils::set_set_api(ASR::Set_t* /*set_type*/) {
        // As per benchmarks, separate chaining
        // does not provide significant gains over
        // linear probing.
        set_api = set_api_lp;
    }

    std::vector<llvm::Type*> LLVMUtils::convert_args(const ASR::Function_t& x, llvm::Module* module) {
        std::vector<llvm::Type*> args;
        for (size_t i=0; i<x.n_args; i++) {
            if (ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(x.m_args[i])->m_v))) {
                ASR::Variable_t *arg = ASRUtils::EXPR2VAR(x.m_args[i]);
                LCOMPILERS_ASSERT(ASRUtils::is_arg_dummy(arg->m_intent) || arg->m_intent == ASR::intentType::Local);
                // We pass all arguments as pointers for now,
                // except bind(C) value arguments that are passed by value
                llvm::Type *type = nullptr, *type_original = nullptr;
                int n_dims = 0, a_kind = 4;
                bool is_array_type = false;
                type_original = get_arg_type_from_ttype_t(x.m_args[i], arg->m_type,
                    arg->m_type_declaration,
                    ASRUtils::get_FunctionType(x)->m_abi,
                    arg->m_abi, arg->m_storage, arg->m_value_attr,
                    n_dims, a_kind, is_array_type, arg->m_intent,
                    module, false);
                if( is_array_type ) {
                    type = type_original->getPointerTo();
                } else {
                    type = type_original;
                }
                if ( arg->m_type_declaration && ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(arg->m_type_declaration)) &&
                    (arg->m_intent == ASRUtils::intent_out || arg->m_intent == ASRUtils::intent_inout) ) {
                    type = type->getPointerTo();
                }
                if( (arg->m_intent == ASRUtils::intent_out || (arg->m_intent == ASRUtils::intent_unspecified && !arg->m_value_attr)) &&
                    ASR::is_a<ASR::CPtr_t>(*arg->m_type) ) {
                    type = type->getPointerTo();
                }
                std::uint32_t m_h;
                std::string m_name = std::string(x.m_name);
                if( x.class_type == ASR::symbolType::Function ) {
                    ASR::Function_t* _func = (ASR::Function_t*)(&(x.base));
                    m_h = get_hash((ASR::asr_t*)_func);
                }
                if( is_array_type && !LLVM::is_llvm_pointer(*arg->m_type) ) {
                    if( ASRUtils::get_FunctionType(x)->m_abi == ASR::abiType::Source ) {
                        llvm::Type* orig_type = type_original;
                        type = arr_api->get_argument_type(orig_type, m_h, arg->m_name, arr_arg_type_cache);
                        is_array_type = false;
                    } else if( ASRUtils::get_FunctionType(x)->m_abi == ASR::abiType::Intrinsic &&
                        fname2arg_type.find(m_name) != fname2arg_type.end()) {
                        type = fname2arg_type[m_name].second;
                        is_array_type = false;
                    }
                }
                args.push_back(type);
            } else if (ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(x.m_args[i])->m_v))) {
                /* This is likely a procedure passed as an argument. For the
                   type, we need to pass in a function pointer with the
                   correct call signature. */
                ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(
                    ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(
                    x.m_args[i])->m_v));
                llvm::Type* type = get_function_type(*fn, module)->getPointerTo();
                args.push_back(type);
            } else {
                throw CodeGenError("Argument type not implemented");
            }
        }
        return args;
    }

    llvm::FunctionType* LLVMUtils::get_function_type(const ASR::Function_t &x, llvm::Module* module) {
        llvm::Type *return_type;
        if (x.m_return_var) {
            ASR::ttype_t *return_var_type0 = ASRUtils::EXPR2VAR(x.m_return_var)->m_type;
            ASR::ttypeType return_var_type = return_var_type0->type;
            switch (return_var_type) {
                case (ASR::ttypeType::Integer) : {
                    int a_kind = ASR::down_cast<ASR::Integer_t>(return_var_type0)->m_kind;
                    return_type = getIntType(a_kind);
                    break;
                }
                case (ASR::ttypeType::UnsignedInteger) : {
                    int a_kind = ASR::down_cast<ASR::UnsignedInteger_t>(return_var_type0)->m_kind;
                    return_type = getIntType(a_kind);
                    break;
                }
                case (ASR::ttypeType::Real) : {
                    int a_kind = ASR::down_cast<ASR::Real_t>(return_var_type0)->m_kind;
                    return_type = getFPType(a_kind);
                    break;
                }
                case (ASR::ttypeType::Complex) : {
                    int a_kind = ASR::down_cast<ASR::Complex_t>(return_var_type0)->m_kind;
                    if (a_kind == 4) {
                        if (ASRUtils::get_FunctionType(x)->m_abi == ASR::abiType::BindC) {
                            if (compiler_options.platform == Platform::Windows) {
                                // i64
                                return_type = llvm::Type::getInt64Ty(context);
                            } else if (compiler_options.platform == Platform::macOS_ARM) {
                                // {float, float}
                                return_type = getComplexType(a_kind);
                            } else {
                                // <2 x float>
                                return_type = FIXED_VECTOR_TYPE::get(llvm::Type::getFloatTy(context), 2);
                            }
                        } else {
                            return_type = getComplexType(a_kind);
                        }
                    } else {
                        LCOMPILERS_ASSERT(a_kind == 8)
                        if (ASRUtils::get_FunctionType(x)->m_abi == ASR::abiType::BindC) {
                            if (compiler_options.platform == Platform::Windows) {
                                // pass as subroutine
                                return_type = getComplexType(a_kind, true);
                                std::vector<llvm::Type*> args = convert_args(x, module);
                                args.insert(args.begin(), return_type);
                                llvm::FunctionType *function_type = llvm::FunctionType::get(
                                        llvm::Type::getVoidTy(context), args, false);
                                return function_type;
                            } else {
                                return_type = getComplexType(a_kind);
                            }
                        } else {
                            return_type = getComplexType(a_kind);
                        }
                    }
                    break;
                }
                case (ASR::ttypeType::String) :
                    return_type = get_StringType(return_var_type0);
                    break;
                case (ASR::ttypeType::Logical) :
                    return_type = llvm::Type::getInt1Ty(context);
                    break;
                case (ASR::ttypeType::CPtr) :
                    return_type = llvm::Type::getVoidTy(context)->getPointerTo();
                    break;
                case (ASR::ttypeType::Pointer) : {
                    return_type = get_type_from_ttype_t_util(x.m_return_var, ASRUtils::get_contained_type(return_var_type0), module)->getPointerTo();
                    break;
                }
                case (ASR::ttypeType::Allocatable) : {
                    return_type = get_type_from_ttype_t_util(x.m_return_var, ASRUtils::get_contained_type(return_var_type0), module);
                    return_type = ASRUtils::is_string_only(return_var_type0) ? return_type : return_type->getPointerTo();
                    break;
                }
                case (ASR::ttypeType::StructType) :
                    throw CodeGenError("StructType return type not implemented yet");
                    break;
                case (ASR::ttypeType::Tuple) : {
                    ASR::Tuple_t* asr_tuple = ASR::down_cast<ASR::Tuple_t>(return_var_type0);
                    std::string type_code = ASRUtils::get_type_code(asr_tuple->m_type,
                                                                    asr_tuple->n_type);
                    std::vector<llvm::Type*> llvm_el_types;
                    for( size_t i = 0; i < asr_tuple->n_type; i++ ) {
                        bool is_local_array_type = false, is_local_malloc_array_type = false;
                        bool is_local_list = false;
                        ASR::dimension_t* local_m_dims = nullptr;
                        int local_n_dims = 0;
                        int local_a_kind = -1;
                        ASR::storage_typeType local_m_storage = ASR::storage_typeType::Default;
                        llvm_el_types.push_back(get_type_from_ttype_t(x.m_return_var,
                                asr_tuple->m_type[i], nullptr, local_m_storage,
                                is_local_array_type, is_local_malloc_array_type,
                                is_local_list, local_m_dims, local_n_dims, local_a_kind, module));
                    }
                    return_type = tuple_api->get_tuple_type(type_code, llvm_el_types);
                    break;
                }
                case (ASR::ttypeType::List) : {
                    bool is_array_type = false, is_malloc_array_type = false;
                    bool is_list = true;
                    ASR::dimension_t *m_dims = nullptr;
                    ASR::storage_typeType m_storage = ASR::storage_typeType::Default;
                    int n_dims = 0, a_kind = -1;
                    ASR::List_t* asr_list = ASR::down_cast<ASR::List_t>(return_var_type0);
                    llvm::Type* el_llvm_type = get_type_from_ttype_t(x.m_return_var, asr_list->m_type, nullptr, m_storage,
                        is_array_type, is_malloc_array_type, is_list, m_dims, n_dims, a_kind, module);
                    int32_t type_size = -1;
                    if( LLVM::is_llvm_struct(asr_list->m_type) ||
                        ASR::is_a<ASR::String_t>(*asr_list->m_type) ||
                        ASR::is_a<ASR::Complex_t>(*asr_list->m_type) ) {
                        llvm::DataLayout data_layout(module->getDataLayout());
                        type_size = data_layout.getTypeAllocSize(el_llvm_type);
                    } else {
                        type_size = a_kind;
                    }
                    std::string el_type_code = ASRUtils::get_type_code(asr_list->m_type);
                    return_type = list_api->get_list_type(el_llvm_type, el_type_code, type_size);
                    break;
                }
                case (ASR::ttypeType::Dict) : {
                    ASR::Dict_t* asr_dict = ASR::down_cast<ASR::Dict_t>(return_var_type0);
                    std::string key_type_code = ASRUtils::get_type_code(asr_dict->m_key_type);
                    std::string value_type_code = ASRUtils::get_type_code(asr_dict->m_value_type);

                    bool is_local_array_type = false, is_local_malloc_array_type = false;
                    bool is_local_list = false;
                    ASR::dimension_t* local_m_dims = nullptr;
                    ASR::storage_typeType local_m_storage = ASR::storage_typeType::Default;
                    int local_n_dims = 0, local_a_kind = -1;

                    llvm::Type* key_llvm_type = get_type_from_ttype_t(x.m_return_var, asr_dict->m_key_type,
                        nullptr, local_m_storage, is_local_array_type, is_local_malloc_array_type,
                        is_local_list, local_m_dims, local_n_dims, local_a_kind, module);
                    llvm::Type* value_llvm_type = get_type_from_ttype_t(x.m_return_var, asr_dict->m_value_type,
                        nullptr, local_m_storage,is_local_array_type, is_local_malloc_array_type,
                        is_local_list, local_m_dims, local_n_dims, local_a_kind, module);
                    int32_t key_type_size = get_type_size(asr_dict->m_key_type, key_llvm_type, local_a_kind, module);
                    int32_t value_type_size = get_type_size(asr_dict->m_value_type, value_llvm_type, local_a_kind, module);

                    set_dict_api(asr_dict);

                    return_type = dict_api->get_dict_type(key_type_code, value_type_code, key_type_size,value_type_size, key_llvm_type, value_llvm_type);
                    break;
                }
                case (ASR::ttypeType::Set) : {
                    ASR::Set_t* asr_set = ASR::down_cast<ASR::Set_t>(return_var_type0);
                    std::string el_type_code = ASRUtils::get_type_code(asr_set->m_type);

                    bool is_local_array_type = false, is_local_malloc_array_type = false;
                    bool is_local_list = false;
                    ASR::dimension_t* local_m_dims = nullptr;
                    ASR::storage_typeType local_m_storage = ASR::storage_typeType::Default;
                    int local_n_dims = 0, local_a_kind = -1;

                    llvm::Type* el_llvm_type = get_type_from_ttype_t(x.m_return_var, asr_set->m_type,
                        nullptr, local_m_storage, is_local_array_type, is_local_malloc_array_type,
                        is_local_list, local_m_dims, local_n_dims, local_a_kind, module);
                    int32_t el_type_size = get_type_size(asr_set->m_type, el_llvm_type, local_a_kind, module);

                    set_set_api(asr_set);

                    return_type = set_api->get_set_type(el_type_code, el_type_size, el_llvm_type);
                    break;
                }
                default :
                    throw CodeGenError("Type not implemented " + ASRUtils::type_to_str_python(return_var_type0));
            }
        } else {
            return_type = llvm::Type::getVoidTy(context);
        }
        std::vector<llvm::Type*> args = convert_args(x, module);
        llvm::FunctionType *function_type = llvm::FunctionType::get(
                return_type, args, false);
        return function_type;
    }

    std::vector<llvm::Type*> LLVMUtils::convert_args(ASR::Function_t* fn, ASR::FunctionType_t* x) {
        std::vector<llvm::Type*> args;
        for (size_t i=0; i < x->n_arg_types; i++) {
            llvm::Type *type = nullptr, *type_original = nullptr;
            int n_dims = 0, a_kind = 4;
            bool is_array_type = false;
            type_original = get_arg_type_from_ttype_t(fn->m_args[i], x->m_arg_types[i],
                nullptr, x->m_abi, x->m_abi, ASR::storage_typeType::Default,
                false, n_dims, a_kind, is_array_type, ASR::intentType::Unspecified,
                module, false);
            if( is_array_type ) {
                type = type_original->getPointerTo();
            } else {
                type = type_original;
            }
            args.push_back(type);
        }
        return args;
    }

    llvm::Type* LLVMUtils::get_type_from_ttype_t(ASR::expr_t* arg_expr, ASR::ttype_t* asr_type,
        ASR::symbol_t *type_declaration, ASR::storage_typeType m_storage,
        bool& is_array_type, bool& is_malloc_array_type, bool& is_list,
        ASR::dimension_t*& m_dims, int& n_dims, int& a_kind, llvm::Module* module,
        ASR::abiType m_abi) {
        llvm::Type* llvm_type = nullptr;

        #define handle_llvm_pointers1()                                     \
            llvm_type = get_type_from_ttype_t(arg_expr, t2, nullptr, m_storage,       \
                is_array_type, is_malloc_array_type, is_list, m_dims,       \
                n_dims, a_kind, module, m_abi);                             \
            if( !is_pointer_ ) {                                            \
                llvm_type = llvm_type->getPointerTo();                      \
            }                                                               \

        switch (asr_type->type) {
            case ASR::ttypeType::Array: {
                ASR::Array_t* v_type = ASR::down_cast<ASR::Array_t>(asr_type);
                m_dims = v_type->m_dims;
                n_dims = v_type->n_dims;
                a_kind = ASRUtils::extract_kind_from_ttype_t(v_type->m_type);
                                switch( v_type->m_physical_type ) {
                    case ASR::array_physical_typeType::DescriptorArray: {
                        is_array_type = true;
                        llvm::Type* el_type = get_el_type(arg_expr, v_type->m_type, module);
                        llvm_type = arr_api->get_array_type(arg_expr, asr_type, el_type);
                        break;
                    }
                    case ASR::array_physical_typeType::PointerToDataArray:
                    case ASR::array_physical_typeType::UnboundedPointerToDataArray : {
                        llvm_type = get_el_type(arg_expr, v_type->m_type, module);
                        llvm_type = ASRUtils::is_character(*v_type->m_type) ? llvm_type
                            : llvm_type->getPointerTo();
                        break;
                    }
                    case ASR::array_physical_typeType::FixedSizeArray: {
                        LCOMPILERS_ASSERT(ASRUtils::is_fixed_size_array(v_type->m_dims, v_type->n_dims));
                        llvm_type = llvm::ArrayType::get(get_el_type(arg_expr, v_type->m_type, module),
                                        ASRUtils::get_fixed_size_of_array(
                                            v_type->m_dims, v_type->n_dims));
                        break;
                    }
                    case ASR::array_physical_typeType::SIMDArray: {
                        llvm_type = llvm::VectorType::get(get_el_type(arg_expr, v_type->m_type, module),
                            ASRUtils::get_fixed_size_of_array(v_type->m_dims, v_type->n_dims), false);
                        break;
                    }
                    case ASR::array_physical_typeType::StringArraySinglePointer: {
                        if (ASRUtils::is_fixed_size_array(v_type->m_dims, v_type->n_dims)) {
                            llvm_type = llvm::ArrayType::get(character_type,
                                ASRUtils::get_fixed_size_of_array(v_type->m_dims, v_type->n_dims));
                            break;
                        } else if (ASRUtils::is_dimension_empty(v_type->m_dims, v_type->n_dims)) {
                            // Treat it as a DescriptorArray
                            is_array_type = true;
                            llvm::Type* el_type = character_type;
                            llvm_type = arr_api->get_array_type(arg_expr, asr_type, el_type);
                            break;
                        } else {
                            LCOMPILERS_ASSERT(false);
                            break;
                        }
                    }
                    default: {
                        LCOMPILERS_ASSERT(false);
                    }
                }
                break ;
            }
            case (ASR::ttypeType::Integer) : {
                ASR::Integer_t* v_type = ASR::down_cast<ASR::Integer_t>(asr_type);
                a_kind = v_type->m_kind;
                llvm_type = getIntType(a_kind);
                break;
            }
            case (ASR::ttypeType::UnsignedInteger) : {
                ASR::UnsignedInteger_t* v_type = ASR::down_cast<ASR::UnsignedInteger_t>(asr_type);
                a_kind = v_type->m_kind;
                // LLVM does not distinguish signed and unsigned integer types
                // Only integer operations can be signed/unsigned
                llvm_type = getIntType(a_kind);
                break;
            }
            case (ASR::ttypeType::Real) : {
                ASR::Real_t* v_type = ASR::down_cast<ASR::Real_t>(asr_type);
                a_kind = v_type->m_kind;
                llvm_type = getFPType(a_kind);
                break;
            }
            case (ASR::ttypeType::Complex) : {
                ASR::Complex_t* v_type = ASR::down_cast<ASR::Complex_t>(asr_type);
                a_kind = v_type->m_kind;
                llvm_type = getComplexType(a_kind);
                break;
            }
            case (ASR::ttypeType::String) : {
                ASR::String_t* v_type = ASR::down_cast<ASR::String_t>(asr_type);
                a_kind = v_type->m_kind;
                llvm_type = get_StringType(asr_type);
                break;
            }
            case (ASR::ttypeType::Logical) : {
                ASR::Logical_t* v_type = ASR::down_cast<ASR::Logical_t>(asr_type);
                a_kind = v_type->m_kind;
                llvm_type = llvm::Type::getInt1Ty(context);
                break;
            }
            case (ASR::ttypeType::StructType) : {
                if (type_declaration) {
                    type_declaration = ASRUtils::symbol_get_past_external(type_declaration);
                    if (ASR::down_cast<ASR::StructType_t>(asr_type)->m_is_cstruct) {
                        llvm_type = getStructType(ASR::down_cast<ASR::Struct_t>(type_declaration),
                                                  module,
                                                  LLVM::is_llvm_pointer(*asr_type));
                    } else {
                        llvm_type = getClassType(ASR::down_cast<ASR::Struct_t>(type_declaration),
                                                 LLVM::is_llvm_pointer(*asr_type));
                    }
                } else {
                    if (ASR::down_cast<ASR::StructType_t>(asr_type)->m_is_cstruct) {
                        llvm_type = getStructType(
                            ASR::down_cast<ASR::Struct_t>(
                                ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(arg_expr))),
                            module,
                            LLVM::is_llvm_pointer(*asr_type));
                    } else {
                        llvm_type
                            = getClassType(ASR::down_cast<ASR::Struct_t>(
                                               ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(arg_expr))),
                                           LLVM::is_llvm_pointer(*asr_type));
                    }
                }
                break;
            }
            case (ASR::ttypeType::UnionType) : {
                llvm_type = getUnion(asr_type, module, false);
                break;
            }
            case (ASR::ttypeType::Pointer) : {
                ASR::ttype_t *t2 = ASR::down_cast<ASR::Pointer_t>(asr_type)->m_type;
                bool is_pointer_ = (ASRUtils::is_class_type(t2) ||
                    (ASR::is_a<ASR::String_t>(*t2) && m_abi != ASR::abiType::BindC) );
                is_malloc_array_type = ASRUtils::is_array(t2);
                handle_llvm_pointers1()
                break;
            }
            case (ASR::ttypeType::Allocatable) : {
                ASR::ttype_t *t2 = ASR::down_cast<ASR::Allocatable_t>(asr_type)->m_type;
                bool is_pointer_ = ((ASR::is_a<ASR::String_t>(*t2) ||
                                     ASRUtils::is_class_type(t2))
                                    && m_abi != ASR::abiType::BindC);
                is_malloc_array_type = ASRUtils::is_array(t2);
                handle_llvm_pointers1()
                break;
            }
            case (ASR::ttypeType::List) : {
                is_list = true;
                ASR::List_t* asr_list = ASR::down_cast<ASR::List_t>(asr_type);
                llvm::Type* el_llvm_type = get_type_from_ttype_t(arg_expr, asr_list->m_type, nullptr, m_storage,
                                                                 is_array_type, is_malloc_array_type,
                                                                 is_list, m_dims, n_dims,
                                                                 a_kind, module, m_abi);
                std::string el_type_code = ASRUtils::get_type_code(asr_list->m_type);
                int32_t type_size = -1;
                if( LLVM::is_llvm_struct(asr_list->m_type) ||
                    ASR::is_a<ASR::String_t>(*asr_list->m_type) ||
                    ASR::is_a<ASR::Complex_t>(*asr_list->m_type) ) {
                    llvm::DataLayout data_layout(module->getDataLayout());
                    type_size = data_layout.getTypeAllocSize(el_llvm_type);
                } else {
                    type_size = a_kind;
                }
                llvm_type = list_api->get_list_type(el_llvm_type, el_type_code, type_size);
                break;
            }
            case (ASR::ttypeType::Dict): {
                llvm_type = get_dict_type(arg_expr, asr_type, module);
                break;
            }
            case (ASR::ttypeType::Set): {
                llvm_type = get_set_type(arg_expr, asr_type, module);
                break;
            }
            case (ASR::ttypeType::Tuple) : {
                ASR::Tuple_t* asr_tuple = ASR::down_cast<ASR::Tuple_t>(asr_type);
                std::string type_code = ASRUtils::get_type_code(asr_tuple->m_type,
                                                                asr_tuple->n_type);
                std::vector<llvm::Type*> llvm_el_types;
                for( size_t i = 0; i < asr_tuple->n_type; i++ ) {
                    bool is_local_array_type = false, is_local_malloc_array_type = false;
                    bool is_local_list = false;
                    ASR::dimension_t* local_m_dims = nullptr;
                    int local_n_dims = 0;
                    int local_a_kind = -1;
                    ASR::storage_typeType local_m_storage = ASR::storage_typeType::Default;
                    llvm_el_types.push_back(get_type_from_ttype_t(arg_expr, asr_tuple->m_type[i], nullptr, local_m_storage,
                        is_local_array_type, is_local_malloc_array_type,
                        is_local_list, local_m_dims, local_n_dims, local_a_kind, module, m_abi));
                }
                llvm_type = tuple_api->get_tuple_type(type_code, llvm_el_types);
                break;
            }
            case (ASR::ttypeType::CPtr) : {
                a_kind = 8;
                llvm_type = llvm::Type::getVoidTy(context)->getPointerTo();
                break;
            }
            case (ASR::ttypeType::EnumType) : {
                llvm_type = llvm::Type::getInt32Ty(context);
                break ;
            }
            case (ASR::ttypeType::FunctionType) : {
                if( type_declaration ) {
                    ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(
                        ASRUtils::symbol_get_past_external(type_declaration));
                    llvm_type = get_function_type(*fn, module)->getPointerTo();
                } else {
                    llvm_type = get_function_type(*ASRUtils::get_function_from_expr(arg_expr), module)->getPointerTo();
                }
                break;
            }
            default :
                throw CodeGenError("Support for type " + ASRUtils::type_to_str_fortran(asr_type) +
                                   " not yet implemented.");
        }
        return llvm_type;
    }

    llvm::Type* LLVMUtils::get_type_from_ttype_t_util(ASR::expr_t* expr, ASR::ttype_t* asr_type,
        llvm::Module* module, ASR::abiType asr_abi) {
        ASR::storage_typeType m_storage_local = ASR::storage_typeType::Default;
        bool is_array_type_local, is_malloc_array_type_local;
        bool is_list_local;
        ASR::dimension_t* m_dims_local;
        int n_dims_local = 0, a_kind_local = 0;
        return get_type_from_ttype_t(expr, asr_type, nullptr, m_storage_local, is_array_type_local,
                                     is_malloc_array_type_local, is_list_local,
                                     m_dims_local, n_dims_local, a_kind_local, module, asr_abi);
    }

    llvm::Value* LLVMUtils::create_gep(llvm::Value* ds, int idx) {
        std::vector<llvm::Value*> idx_vec = {
        llvm::ConstantInt::get(context, llvm::APInt(32, 0)),
        llvm::ConstantInt::get(context, llvm::APInt(32, idx))};
        return LLVMUtils::CreateGEP(ds, idx_vec);
    }

    llvm::Value* LLVMUtils::create_gep(llvm::Value* ds, llvm::Value* idx) {
        std::vector<llvm::Value*> idx_vec = {
        llvm::ConstantInt::get(context, llvm::APInt(32, 0)),
        idx};
        return LLVMUtils::CreateGEP(ds, idx_vec);
    }

    llvm::Value* LLVMUtils::create_gep2(llvm::Type *t, llvm::Value* ds, int idx) {
        std::vector<llvm::Value*> idx_vec = {
        llvm::ConstantInt::get(context, llvm::APInt(32, 0)),
        llvm::ConstantInt::get(context, llvm::APInt(32, idx))};
        return LLVMUtils::CreateGEP2(t, ds, idx_vec);
    }

    llvm::Value* LLVMUtils::create_gep2(llvm::Type *t, llvm::Value* ds, llvm::Value* idx) {
        std::vector<llvm::Value*> idx_vec = {
        llvm::ConstantInt::get(context, llvm::APInt(32, 0)),
        idx};
        return LLVMUtils::CreateGEP2(t, ds, idx_vec);
    }

    llvm::Value* LLVMUtils::create_ptr_gep(llvm::Value* ptr, int idx) {
        std::vector<llvm::Value*> idx_vec = {
        llvm::ConstantInt::get(context, llvm::APInt(32, idx))};
        return LLVMUtils::CreateInBoundsGEP(ptr, idx_vec);
    }

    llvm::Value* LLVMUtils::create_ptr_gep(llvm::Value* ptr, llvm::Value* idx) {
        std::vector<llvm::Value*> idx_vec = {idx};
        return LLVMUtils::CreateInBoundsGEP(ptr, idx_vec);
    }

    llvm::Value* LLVMUtils::create_ptr_gep2(llvm::Type* type, llvm::Value* ptr, int idx) {
        std::vector<llvm::Value*> idx_vec = {
        llvm::ConstantInt::get(context, llvm::APInt(32, idx))};
        return LLVMUtils::CreateInBoundsGEP2(type, ptr, idx_vec);
    }

    llvm::Value* LLVMUtils::create_ptr_gep2(llvm::Type* type, llvm::Value* ptr, llvm::Value* idx) {
        std::vector<llvm::Value*> idx_vec = {idx};
        return LLVMUtils::CreateInBoundsGEP2(type, ptr, idx_vec);
    }

    llvm::AllocaInst* LLVMUtils::CreateAlloca(llvm::Type* type,
            llvm::Value* size, std::string Name, bool
#if LLVM_VERSION_MAJOR > 16
            is_llvm_ptr
#else
            /*is_llvm_ptr*/
#endif
        ) {
        llvm::BasicBlock &entry_block = builder->GetInsertBlock()->getParent()->getEntryBlock();
        llvm::IRBuilder<> builder0(context);
        builder0.SetInsertPoint(&entry_block, entry_block.getFirstInsertionPt());
        llvm::AllocaInst *alloca;
#if LLVM_VERSION_MAJOR > 16
        llvm::Type *type_ = is_llvm_ptr ? type->getPointerTo() : type;
#else
        llvm::Type *type_ = type;
#endif
        if (Name != "") {
            alloca = builder0.CreateAlloca(type_, size, Name);
        } else {
            alloca = builder0.CreateAlloca(type_, size);
        }
#if LLVM_VERSION_MAJOR > 16
        ptr_type[alloca] = type;
#endif
        return alloca;
    }

    llvm::AllocaInst* LLVMUtils::CreateAlloca(llvm::IRBuilder<> &builder,
            llvm::Type* type, llvm::Value* size, std::string Name, bool
#if LLVM_VERSION_MAJOR > 16
            is_llvm_ptr
#else
            /*is_llvm_ptr*/
#endif
        ) {
        llvm::AllocaInst *alloca;
#if LLVM_VERSION_MAJOR > 16
        llvm::Type *type_ = is_llvm_ptr ? type->getPointerTo() : type;
#else
        llvm::Type *type_ = type;
#endif
        if (Name != "") {
            alloca = builder.CreateAlloca(type_, size, Name);
        } else {
            alloca = builder.CreateAlloca(type_, size);
        }
#if LLVM_VERSION_MAJOR > 16
        ptr_type[alloca] = type;
#endif
        return alloca;
    }

    llvm::Value *LLVMUtils::CreateLoad(llvm::Value *x, bool is_volatile) {
#if LLVM_VERSION_MAJOR <= 16
        llvm::Type *t = x->getType();
        LCOMPILERS_ASSERT(t->isPointerTy());
        LCOMPILERS_ASSERT(t->getNumContainedTypes() > 0);
        llvm::Type *t2 = t->getContainedType(0);
        return builder->CreateLoad(t2, x, is_volatile);
#else
        llvm::Type *type = nullptr, *type_copy = nullptr;
        bool is_type_pointer = false;
        if (ptr_type.find(x) != ptr_type.end()) {
            type_copy = type = ptr_type[x];
        }
        LCOMPILERS_ASSERT(type);
        // getPointerTo() is used for allocatable or pointer
        if (type != character_type && (llvm::isa<llvm::AllocaInst>(x) &&
                llvm::dyn_cast<llvm::AllocaInst>(x)->getAllocatedType()->isPointerTy())) {
            // AllocaInst
            type = type->getPointerTo();
            is_type_pointer = true;
        } else if (llvm::StructType *arr_type = llvm::dyn_cast<llvm::StructType>(type)) {
            // Function arguments
            if (arr_type->getName() == "array" || LCompilers::startswith(
                    std::string(arr_type->getName()), "array.")) {
                type = type->getPointerTo();
                is_type_pointer = true;
            }
        }

        if ( llvm::GetElementPtrInst *
                gep = llvm::dyn_cast<llvm::GetElementPtrInst>(x) ) {
            // GetElementPtrInst
            [[maybe_unused]] llvm::Type *src_type = gep->getSourceElementType();
            LCOMPILERS_ASSERT(llvm::isa<llvm::StructType>(src_type));
            std::string s_name = std::string(llvm::dyn_cast<llvm::StructType>(
                gep->getSourceElementType())->getName());
            if ( name2dertype.find(s_name) != name2dertype.end() ) {
                type = type->getPointerTo();
                is_type_pointer = true;
            }
        }

        llvm::Value *load = builder->CreateLoad(type, x, is_volatile);
        if (is_type_pointer) {
            ptr_type[load] = type_copy;
        }
        return load;
#endif
    }

    llvm::Value *LLVMUtils::CreateLoad2(llvm::Type *t, llvm::Value *x, bool is_volatile) {
        return builder->CreateLoad(t, x, is_volatile);
    }

    llvm::Value* LLVMUtils::CreateGEP(llvm::Value *x,
            std::vector<llvm::Value *> &idx) {
#if LLVM_VERSION_MAJOR <= 16
        llvm::Type *t = x->getType();
        LCOMPILERS_ASSERT(t->isPointerTy());
        LCOMPILERS_ASSERT(t->getNumContainedTypes() > 0);
        llvm::Type *t2 = t->getContainedType(0);
        return builder->CreateGEP(t2, x, idx);
#else
        llvm::Type *type = nullptr;
        auto it = ptr_type.find(x);
        if (it != ptr_type.end()) {
            type = it->second;
        }
        LCOMPILERS_ASSERT(type);
        return builder->CreateGEP(type, x, idx);
#endif
    }

    llvm::Value* LLVMUtils::CreateGEP2(llvm::Type *t, llvm::Value *x,
            std::vector<llvm::Value *> &idx) {
        return builder->CreateGEP(t, x, idx);
    }

    llvm::Value* LLVMUtils::CreateGEP2(llvm::Type *type,
        llvm::Value *x, int idx) {
        std::vector<llvm::Value*> idx_vec = {
        llvm::ConstantInt::get(context, llvm::APInt(32, 0)),
        llvm::ConstantInt::get(context, llvm::APInt(32, idx))};
        return LLVMUtils::CreateGEP2(type, x, idx_vec);
    }

    llvm::Value* LLVMUtils::CreateInBoundsGEP(llvm::Value *x,
            std::vector<llvm::Value *> &idx) {
#if LLVM_VERSION_MAJOR <= 16
        llvm::Type *t = x->getType();
        LCOMPILERS_ASSERT(t->isPointerTy());
        LCOMPILERS_ASSERT(t->getNumContainedTypes() > 0);
        llvm::Type *t2 = t->getContainedType(0);
        return builder->CreateInBoundsGEP(t2, x, idx);
#else
        llvm::Type *type = nullptr;
        if (ptr_type.find(x) != ptr_type.end()) {
            type = ptr_type[x];
        }
        LCOMPILERS_ASSERT(type);
        return builder->CreateInBoundsGEP(type, x, idx);
#endif
    }

    llvm::Value* LLVMUtils::CreateInBoundsGEP2(llvm::Type *t,
            llvm::Value *x, std::vector<llvm::Value *> &idx) {
        return builder->CreateInBoundsGEP(t, x, idx);
    }


    llvm::Function* LLVMUtils::_Deallocate() {
        std::string func_name = "_lfortran_free";
        llvm::Function *free_fn = module->getFunction(func_name);
        if (!free_fn) {
            llvm::FunctionType *function_type = llvm::FunctionType::get(
                    llvm::Type::getVoidTy(context), {
                        character_type
                    }, false);
            free_fn = llvm::Function::Create(function_type,
                    llvm::Function::ExternalLinkage, func_name, *module);
        }
        return free_fn;
    }

    llvm::Type* LLVMUtils::getIntType(int a_kind, bool get_pointer) {
        llvm::Type* type_ptr = nullptr;
        if( get_pointer ) {
            switch(a_kind)
            {
                case 1:
                    type_ptr = llvm::Type::getInt8Ty(context)->getPointerTo();
                    break;
                case 2:
                    type_ptr = llvm::Type::getInt16Ty(context)->getPointerTo();
                    break;
                case 4:
                    type_ptr = llvm::Type::getInt32Ty(context)->getPointerTo();
                    break;
                case 8:
                    type_ptr = llvm::Type::getInt64Ty(context)->getPointerTo();
                    break;
                default:
                    LCOMPILERS_ASSERT(false);
            }
        } else {
            switch(a_kind)
            {
                case 1:
                    type_ptr = llvm::Type::getInt8Ty(context);
                    break;
                case 2:
                    type_ptr = llvm::Type::getInt16Ty(context);
                    break;
                case 4:
                    type_ptr = llvm::Type::getInt32Ty(context);
                    break;
                case 8:
                    type_ptr = llvm::Type::getInt64Ty(context);
                    break;
                default:
                    LCOMPILERS_ASSERT(false);
            }
        }
        return type_ptr;
    }

    void LLVMUtils::start_new_block(llvm::BasicBlock *bb) {
        llvm::BasicBlock *last_bb = builder->GetInsertBlock();
        llvm::Function *fn = last_bb->getParent();
        llvm::Instruction *block_terminator = last_bb->getTerminator();
        if (block_terminator == nullptr) {
            // The previous block is not terminated --- terminate it by jumping
            // to our new block
            builder->CreateBr(bb);
        }
#if LLVM_VERSION_MAJOR >= 16
        fn->insert(fn->end(), bb);
#else
        fn->getBasicBlockList().push_back(bb);
#endif
        builder->SetInsertPoint(bb);
    }

    llvm::Value* LLVMUtils::lfortran_str_cmp(llvm::Value* left_arg, llvm::Value* right_arg,
                                             std::string runtime_func_name, llvm::Module& module)
    {
        llvm::Type* character_type = llvm::Type::getInt8Ty(context)->getPointerTo();
        llvm::Function *fn = module.getFunction(runtime_func_name);
        if(!fn) {
            llvm::FunctionType *function_type = llvm::FunctionType::get(
                    llvm::Type::getInt1Ty(context), {
                        character_type->getPointerTo(),
                        character_type->getPointerTo()
                    }, false);
            fn = llvm::Function::Create(function_type,
                    llvm::Function::ExternalLinkage, runtime_func_name, module);
        }
        llvm::AllocaInst *pleft_arg = LLVMUtils::CreateAlloca(character_type);
        LLVM::CreateStore(*builder, left_arg, pleft_arg);
        llvm::AllocaInst *pright_arg = LLVMUtils::CreateAlloca(character_type);
        LLVM::CreateStore(*builder, right_arg, pright_arg);
        std::vector<llvm::Value*> args = {pleft_arg, pright_arg};
        return builder->CreateCall(fn, args);
    }

    void LLVMUtils::string_init(llvm::Value* arg_size, llvm::Value* arg_string) {
        std::string func_name = "_lfortran_string_init";
        llvm::Function *fn = module->getFunction(func_name);
        if (!fn) {
            llvm::FunctionType *function_type = llvm::FunctionType::get(
                    llvm::Type::getVoidTy(context), {
                        llvm::Type::getInt64Ty(context),
                        llvm::Type::getInt8Ty(context)->getPointerTo()
                    }, false);
            fn = llvm::Function::Create(function_type,
                    llvm::Function::ExternalLinkage, func_name, module);
        }
        std::vector<llvm::Value*> args = {convert_kind(arg_size,
                                                llvm::Type::getInt64Ty(context)),
                                        arg_string};
        builder->CreateCall(fn, args);
    }

    llvm::Constant* LLVMUtils::create_llvm_constant_from_asr_expr(ASR::expr_t* expr,
                                                        llvm::Module* module) {
        switch (expr->type) {
            case ASR::exprType::IntegerConstant: {
                ASR::IntegerConstant_t* ic = ASR::down_cast<ASR::IntegerConstant_t>(expr);
                llvm::Type* llvm_type = get_type_from_ttype_t_util(nullptr, ic->m_type, module);
                return llvm::ConstantInt::get(llvm_type, ic->m_n, true);
            }
            case ASR::exprType::LogicalConstant: {
                ASR::LogicalConstant_t* lc = ASR::down_cast<ASR::LogicalConstant_t>(expr);
                llvm::Type* llvm_type = get_type_from_ttype_t_util(nullptr, lc->m_type, module);
                return llvm::ConstantInt::get(llvm_type, lc->m_value ? 1 : 0, false);
            }
            case ASR::exprType::RealConstant: {
                ASR::RealConstant_t* rc = ASR::down_cast<ASR::RealConstant_t>(expr);
                llvm::Type* llvm_type = get_type_from_ttype_t_util(nullptr, rc->m_type, module);
                if (llvm_type->isFloatTy()) {
                    return llvm::ConstantFP::get(llvm_type, static_cast<float>(rc->m_r));
                } else if (llvm_type->isDoubleTy()) {
                    return llvm::ConstantFP::get(llvm_type, rc->m_r);
                }
                break;
            }
            default:
                throw LCompilersException( "Unsupported constant expression in struct default initializer.");
        }
        return nullptr;
    }

    bool LLVMUtils::is_proper_array_of_strings_llvm_var([[maybe_unused]]ASR::ttype_t* type, [[maybe_unused]] llvm::Value* str){
        LCOMPILERS_ASSERT(ASRUtils::is_array(type))
        LCOMPILERS_ASSERT(ASRUtils::is_character(*type))
        // std::cout<<str->getType()->isPointerTy() << " "
        //                     << !str->getType()->getPointerElementType()->isPointerTy() << " "
        //                     << (str->getType() == get_type_from_ttype_t_util(type, module)) << " "
        //                     << (str->getType()->getPointerElementType()->getContainedType(0) == string_descriptor->getPointerTo()) << " ";
#if LLVM_VERSION_MAJOR < 17
        ASR::String_t* str_type = ASRUtils::get_string_type(type);
        switch(ASRUtils::extract_physical_type(type)){
            case ASR::DescriptorArray:{ 
                switch (str_type->m_physical_type){
                    // A pointer to the Array Descriptor => `{ %string_descriptor*, i32, %dimension_descriptor*, i1, i32 }`
                    case ASR::DescriptorString:{
                        return str->getType()->isPointerTy() && 
                            !str->getType()->getPointerElementType()->isPointerTy() &&
                            (str->getType()->getPointerElementType()->getContainedType(0) == string_descriptor->getPointerTo());
                    }
                    case ASR::CChar:{
                        throw LCompilersException("DescriptorArray can't exist with CChar string physical type");
                    }
                    default:
                        throw LCompilersException("Unhandled String Physical type");
                }
            }
            case ASR::PointerToDataArray:{
                switch (str_type->m_physical_type){
                    // `string_descriptor*` and `char*`
                    case ASR::DescriptorString :
                    case ASR::CChar : {
                        return str->getType()->isPointerTy() && 
                            !str->getType()->getPointerElementType()->isPointerTy() &&
                            (str->getType()->getPointerElementType() == get_StringType(type));
                    }
                    default:
                        throw LCompilersException("Unhandled String Physical type");
                }

            }
            default:
                throw LCompilersException ("Unhandled Array Physical type");
        }
#else
    // Can't check so return true
    return true;
#endif
    }



    bool LLVMUtils::is_proper_string_llvm_variable([[maybe_unused]]ASR::String_t* str_type, [[maybe_unused]]llvm::Value* str){
#if LLVM_VERSION_MAJOR < 17
        switch (str_type->m_physical_type){
            case ASR::DescriptorString:
            case ASR::CChar: { // Check for => `string_descriptor*` and `char*`
                return str->getType()->isPointerTy() && 
                    !str->getType()->getPointerElementType()->isPointerTy() &&
                    (get_StringType((ASR::ttype_t*)str_type)->getPointerTo() == str->getType()) && 
                    (str->getType()->getPointerElementType() == get_StringType((ASR::ttype_t*)str_type));
            }
            default:
            throw LCompilersException("Unhandled string physical type");
        }
#else
        // Can't check so return true
        return true;
#endif
    }

    /*
        Allocates and sets the memory on heap for a string (already exisiting).
    */
    void LLVMUtils::set_string_memory_on_heap(ASR::string_physical_typeType str_physical_type,
        llvm::Value* str , llvm::Value* len /*null-char not included*/){
        llvm::Value *str_data{};
        switch (str_physical_type) {
            case ASR::DescriptorString: {
                str_data = CreateGEP2(string_descriptor, str, 0);
                break;
            }
            default:
                throw LCompilersException("Unhandled string physical type");
        }

        llvm::Value* len_with_null_char = builder->CreateAdd(
            convert_kind(len, llvm::Type::getInt64Ty(context)),
                llvm::ConstantInt::get(context, llvm::APInt(64, 1))); // increment to include null-char. 
        llvm::Value* mem_allocted = LLVM::lfortran_malloc(context, *module, *builder, len_with_null_char);
        string_init(len_with_null_char, mem_allocted);
        builder->CreateStore(mem_allocted, str_data);
    }
    void LLVMUtils::initialize_string_stack(ASR::string_physical_typeType str_physical_type,
        llvm::Value* str /*StringDescritptor*/, llvm::Value* len /*null-char not included*/){
        return set_string_memory_on_heap(str_physical_type, str, len);

// TODO :: Remove code above
        llvm::Value* str_len{}, *str_data{};
        if(str_physical_type == ASR::DescriptorString){
            str_data = CreateGEP2(string_descriptor, str, 0);
            str_len = CreateGEP2(string_descriptor, str, 1);
        } else {
            throw LCompilersException("Unhandled string physical type");
        }
        llvm::Value* len_with_null_char = builder->CreateAdd(
            builder->CreateSExtOrTrunc(len, llvm::Type::getInt32Ty(context)),
                llvm::ConstantInt::get(context, llvm::APInt(32, 1))); // increment to include null-char. 
        llvm::Value *s_alloc = builder->CreateAlloca(character_type, len_with_null_char);
        string_init(len_with_null_char, s_alloc);
        builder->CreateStore(s_alloc, str_data);
        builder->CreateStore(convert_kind(len, llvm::Type::getInt64Ty(context)), str_len);
    }


    llvm::Value* LLVMUtils::create_string(ASR::String_t* str, std::string name){
        switch(str->m_physical_type){
            case (ASR::DescriptorString):{
                return create_string_descriptor(name);
                break;
            }
            default:
                throw LCompilersException("Unhandled string physical type");
        }
    }

    llvm::Value* LLVMUtils::create_empty_string_descriptor(std::string name){
        return create_string_descriptor(
            llvm::ConstantPointerNull::getNullValue(character_type),
            llvm::ConstantInt::get(context, llvm::APInt(64, 0)),
            name);
    }

    llvm::Value* LLVMUtils::create_string_descriptor(llvm::Value* data, llvm::Value* len, std::string name){
        llvm::Value* str_desc = create_string_descriptor(name);
        builder->CreateStore(data, CreateGEP2(string_descriptor, str_desc, 0));
        builder->CreateStore(len, CreateGEP2(string_descriptor, str_desc, 1));
        return str_desc;
    }



    llvm::Value* LLVMUtils::create_string_descriptor(std::string name){

        llvm::DataLayout data_layout_inst(module->getDataLayout());
        llvm::Value* str_desc = builder->CreateBitCast(
            LLVMArrUtils::lfortran_malloc(context,
                *module, *builder,
                llvm::ConstantInt::get(context, llvm::APInt(64, data_layout_inst.getTypeAllocSize(string_descriptor)))),
            string_descriptor->getPointerTo(), name);
        
        return str_desc;

// TODO :: Remove code above.

        llvm::Value* str_descriptor = builder->CreateAlloca(string_descriptor, nullptr, name);
        return str_descriptor;
    }

    llvm::Value* LLVMUtils::get_string_data(ASR::String_t* str_type, llvm::Value* str, bool get_pointer_to_data){
        LCOMPILERS_ASSERT(is_proper_string_llvm_variable(str_type, str))
        llvm::Value* ptr_to_data {};
        switch (str_type->m_physical_type)
        {
            case ASR::DescriptorString:{
                ptr_to_data = create_gep2(string_descriptor, str, 0);
                break;
            }    
            case ASR::CChar:{
                llvm::Value* char_ptr = builder->CreateAlloca(llvm::Type::getInt8Ty(context));
                ptr_to_data = builder->CreateStore(str, char_ptr);
                break;
            }
            default:
                throw LCompilersException("Unsupported string physical type.");
        }
        if(get_pointer_to_data) {
            return ptr_to_data;
        } else {
            return builder->CreateLoad(
                character_type, ptr_to_data); 
        }
    }
    // >>>>>>>>>>>>>> Refactor this

    llvm::Value* LLVMUtils::get_string_length(ASR::String_t* str_type, llvm::Value* str, bool get_pointer_to_len){
        LCOMPILERS_ASSERT(is_proper_string_llvm_variable(str_type, str))
        if(!get_pointer_to_len && str_type->m_len && ASRUtils::is_value_constant(str_type->m_len)){ // CompileTime-Constant Length
            int64_t len; ASRUtils::extract_value(str_type->m_len, len);
            llvm::Value* len_tmp = llvm::ConstantInt::get(context, llvm::APInt(64, len));
                return len_tmp;
        } else {
            switch (str_type->m_physical_type)
            {
                case ASR::DescriptorString:{
                    llvm::Value* ptr_to_len = create_gep2(string_descriptor, str, 1);
                    if(get_pointer_to_len){
                        return ptr_to_len;
                    } else {
                        return builder->CreateLoad(llvm::Type::getInt64Ty(context), ptr_to_len);
                    }
                }
                case ASR::CChar:{
                    // return llvm::ConstantInt::get(context, llvm::APInt(64, 1));
                    throw LCompilersException("Length of string physical CChar, should be a compile-time constant(atleast for now)");
                }
                default:
                    throw LCompilersException("Unsupported string physical type.");
            }
        }
    }

    std::pair<llvm::Value*, llvm::Value*> LLVMUtils::get_string_length_data(ASR::String_t* str_type, llvm::Value* str,
        bool get_pointer_to_data, bool get_pointer_to_len){
        llvm::Value* data = get_string_data(str_type, str, get_pointer_to_data);
        llvm::Value* len = get_string_length(str_type, str, get_pointer_to_len);
        return std::make_pair(data, len);
    }



    // TODO : Refactor names of the following two functions.
    
    llvm::Value* LLVMUtils::get_string_element_in_array_(ASR::String_t* str_type, llvm::Value* data, llvm::Value* arr_idx){

        llvm::Value* string_len = get_string_length(str_type, data);
        llvm::Value* string_len_plus_null_char = builder->CreateAdd(string_len, llvm::ConstantInt::get(context, llvm::APInt(64, 1)));
        llvm::Value* string_data = get_string_data(str_type, data);
        llvm::Value* actual_idx = builder->CreateMul(convert_kind(arr_idx, llvm::Type::getInt64Ty(context)), string_len_plus_null_char);
        llvm::Value* desired_element =  builder->CreateGEP(llvm::Type::getInt8Ty(context), string_data, actual_idx);
        return desired_element;
    }

    llvm::Value* LLVMUtils::get_string_element_in_array(ASR::String_t* str_type, llvm::Value* data, llvm::Value* arr_idx){
        llvm::Value* desired_ptr = get_string_element_in_array_(str_type, data, arr_idx);
        return create_string_descriptor(desired_ptr, get_string_length(str_type, data), "arr_element");
    }


    void LLVMUtils::allocate_allocatable_string(ASR::String_t* str_type, llvm::Value* str, llvm::Value* amount_to_allocate){
        switch (str_type->m_len_kind){
            case ASR::ExpressionLength:
            case ASR::AssumedLength:{ //String length remains the same
                set_string_memory_on_heap(str_type->m_physical_type, 
                    str, get_string_length(str_type, str));
                break;
            }
            case ASR::DeferredLength:{
                LCOMPILERS_ASSERT(amount_to_allocate)
                set_string_memory_on_heap(str_type->m_physical_type, str, amount_to_allocate);
                llvm::Value* str_len = get_string_length(str_type, str, true); // Pointer to it (e.g `i64*`)
                builder->CreateStore(
                    convert_kind(amount_to_allocate, llvm::Type::getInt64Ty(context)),
                    str_len);
                break;
            }
            default:
                throw LCompilersException("Unhandled string length kind");
        } 
    }

    void LLVMUtils::allocate_allocatable_array_of_strings(ASR::String_t* str_type, llvm::Value* str,
        llvm::Value* string_length_to_allocate, llvm::Value* array_size_to_allocte, bool realloc){
        
        switch (str_type->m_len_kind){
            case ASR::ExpressionLength:
            case ASR::AssumedLength:{ // Set memory only. Length already set.
                set_array_of_strings_memory_on_heap(
                    str_type,
                    str,
                    get_string_length(str_type, str),
                    array_size_to_allocte,
                    realloc);
                break;
            }
            case ASR::DeferredLength:{ // Set memory + Length
                LCOMPILERS_ASSERT(string_length_to_allocate)
                string_length_to_allocate = 
                    convert_kind(
                        string_length_to_allocate,
                        llvm::Type::getInt64Ty(context));
                set_array_of_strings_memory_on_heap(
                    str_type,
                    str,
                    string_length_to_allocate,
                    array_size_to_allocte,
                    realloc);
                builder->CreateStore(string_length_to_allocate, get_string_length(str_type, str, true));
                break;
            }
            default:
                throw LCompilersException("Unhandled string length kind");
        }
    }
    llvm::Value* LLVMUtils::get_stringArray_whole_size(ASR::ttype_t* type){
        LCOMPILERS_ASSERT(ASRUtils::is_array_of_strings(type))
        ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(ASRUtils::type_get_past_allocatable_pointer(type));
        ASR::String_t* str = ASRUtils::get_string_type(arr->m_type);

        if(ASRUtils::is_value_constant(str->m_len) && ASRUtils::is_fixed_size_array(type)){
            int64_t arr_size, str_len;
            arr_size = ASRUtils::get_fixed_size_of_array(type);
            str_len = ASR::down_cast<ASR::IntegerConstant_t>(str->m_len)->m_n;
            int64_t str_len_plus_one = str_len + 1;
            return llvm::ConstantInt::get(
                context, llvm::APInt(64, arr_size * str_len_plus_one));
        } else {
            LCOMPILERS_ASSERT_MSG(false, "Not Implemented case : Array size and string length should be compile time constants");
        }
        return nullptr;

    }
    void LLVMUtils::set_array_of_strings_memory_on_heap(
        ASR::String_t* str_type,
        llvm::Value* str, 
        llvm::Value* string_len_to_allocate /*null char not included*/,
        llvm::Value* array_size_to_allocate,
        bool realloc){
        // Calculate needed memory (array_size * string_length)
        string_len_to_allocate = convert_kind(string_len_to_allocate, llvm::Type::getInt64Ty(context));
        llvm::Value* string_len_plus_null_char = builder->CreateAdd(string_len_to_allocate, llvm::ConstantInt::get(context, llvm::APInt(64, 1)));
        llvm::Value* whole_memory_needed = builder->CreateMul(
            convert_kind(array_size_to_allocate, llvm::Type::getInt64Ty(context)),
            string_len_plus_null_char);

        // Allocate memory and store in string's data ptr.
        llvm::Value* allocated_mem = realloc ? 
            LLVMArrUtils::lfortran_realloc(context, *module, *builder, get_string_data(str_type, str), whole_memory_needed)
            : LLVMArrUtils::lfortran_malloc(context, *module, *builder, whole_memory_needed);
        builder->CreateMemSet(allocated_mem, llvm::ConstantInt::get(context, llvm::APInt(8,'\0')), whole_memory_needed, llvm::MaybeAlign());
        builder->CreateStore(allocated_mem, get_string_data(str_type, str, true));
    }

    void LLVMUtils::set_array_of_strings_memory_on_stack(ASR::String_t* str_type,llvm::Value* str,
        llvm::Value* str_len /*null char not inlcluded*/, llvm::Value* array_size){
        return set_array_of_strings_memory_on_heap(str_type, str, str_len, array_size);

//TODO :: Remove Code Above

        str_len = convert_kind(str_len, llvm::Type::getInt64Ty(context));
        llvm::Value* string_len_plus_null_char = builder->CreateAdd(str_len, llvm::ConstantInt::get(context, llvm::APInt(64, 1)));
        llvm::Value* whole_memory_needed = builder->CreateMul(
            convert_kind(array_size, llvm::Type::getInt64Ty(context)),
            string_len_plus_null_char);
        
        llvm::Value* allocated_mem{};
        allocated_mem = builder->CreateAlloca(llvm::Type::getInt8Ty(context), whole_memory_needed);
        builder->CreateMemSet(allocated_mem, llvm::ConstantInt::get(context, llvm::APInt(8,'\0')), whole_memory_needed, llvm::MaybeAlign());
        builder->CreateStore(allocated_mem, get_string_data(str_type, str, true));
    }


    llvm::Value* LLVMUtils::get_stringArray_data(ASR::ttype_t* type, llvm::Value* arr_ptr){
        LCOMPILERS_ASSERT(is_proper_array_of_strings_llvm_var(type, arr_ptr))
        LCOMPILERS_ASSERT(ASRUtils::is_array_of_strings(type))
        ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(ASRUtils::type_get_past_allocatable_pointer(type));
        ASR::String_t* str = ASRUtils::get_string_type(arr->m_type);
        switch(arr->m_physical_type){
            case ASR::DescriptorArray: {
                llvm::Value* str_desc = builder->CreateLoad(
                    get_StringType(ASRUtils::extract_type(type))->getPointerTo(),
                    arr_api->get_pointer_to_data(arr_ptr));
                return get_string_data(str, str_desc);
            }
            case ASR::PointerToDataArray:{
                return get_string_data(str, arr_ptr);
            }
            default:
                throw LCompilersException("Unhandled Array Physical Type");
        }
    }
    llvm::Value* LLVMUtils::get_stringArray_length(ASR::ttype_t* type, llvm::Value* arr_ptr){
        LCOMPILERS_ASSERT(is_proper_array_of_strings_llvm_var(type, arr_ptr))
        LCOMPILERS_ASSERT(ASRUtils::is_array_of_strings(type))
        ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(ASRUtils::type_get_past_allocatable_pointer(type));
        ASR::String_t* str = ASRUtils::get_string_type(arr->m_type);
        switch(arr->m_physical_type){
            case ASR::DescriptorArray: {
                llvm::Value* str_desc = builder->CreateLoad(
                    get_StringType(ASRUtils::extract_type(type))->getPointerTo(),
                    arr_api->get_pointer_to_data(arr_ptr));
                return get_string_length(str, str_desc);
            }
            case ASR::PointerToDataArray:{
                return get_string_length(str, arr_ptr);
            }
            default:
                throw LCompilersException("Unhandled Array Physical Type");
        }
    }

    void LLVMUtils::free_strings(ASR::expr_t* expr, 
        llvm::Value* tmp /*ptr to Array of strings OR standalone string*/){
        ASR::ttype_t* type = ASRUtils::expr_type(expr);
        LCOMPILERS_ASSERT(ASRUtils::is_character(*type))

        // Get string representation in the backend (e.g. `i8*` or `string_descriptor*`)
        llvm::Value* str{};
        if (ASRUtils::is_string_only(type)) {
           str = tmp;
        } else if (ASRUtils::is_array_of_strings(type)){
            llvm::Value* array_of_strings = tmp;
            switch(ASRUtils::extract_physical_type(type)) {
                case ASR::DescriptorArray:
                    str = builder->CreateLoad(
                        get_StringType(ASRUtils::extract_type(type))->getPointerTo(),
                        arr_api->get_pointer_to_data(expr, type, array_of_strings, module));
                    break;
                case ASR::PointerToDataArray:
                    str = array_of_strings;
                    break;
                default:
                    throw LCompilersException("Unhandled array of strings type");
            }
        } else {
            throw LCompilersException("Unvalid state");
        }
        LCOMPILERS_ASSERT(str)
        llvm::Value* str_data {}, *str_len {};
        std::tie(str_data, str_len) = get_string_length_data(ASRUtils::get_string_type(type), str, true, true);
        builder->CreateCall(_Deallocate(),{builder->CreateLoad(character_type, str_data)});
        builder->CreateStore(llvm::ConstantPointerNull::getNullValue(character_type), str_data);
        builder->CreateStore(llvm::ConstantInt::get(llvm::Type::getInt64Ty(context),0), str_len);
    }


    llvm::Value* LLVMUtils::lfortran_str_copy(
        llvm::Value* dest, llvm::Value *src,
        ASR::String_t* dest_str_type, ASR::String_t* src_str_type,
        bool is_dest_allocatable) {
        llvm::Value *lhs_data, *lhs_len;
        llvm::Value *rhs_data, *rhs_len;
        llvm::Value *is_lhs_deferred, *is_lhs_allocatable;

        std::tie(lhs_data, lhs_len) = get_string_length_data(dest_str_type, dest, true, true);
        std::tie(rhs_data, rhs_len) = get_string_length_data(src_str_type, src);
        is_lhs_deferred = llvm::ConstantInt::get(context, llvm::APInt(8, dest_str_type->m_len_kind == ASR::DeferredLength));
        is_lhs_allocatable= llvm::ConstantInt::get(context, llvm::APInt(8, is_dest_allocatable));
        

        std::string runtime_func_name = "_lfortran_strcpy";
        llvm::Function *fn = module->getFunction(runtime_func_name);
        if (!fn) {
            llvm::FunctionType *function_type = llvm::FunctionType::get(
                    llvm::Type::getVoidTy(context),
                    {
                        llvm::Type::getInt8Ty(context)->getPointerTo()->getPointerTo(),
                        llvm::Type::getInt64Ty(context)->getPointerTo(),
                        llvm::Type::getInt8Ty(context), llvm::Type::getInt8Ty(context),
                        llvm::Type::getInt8Ty(context)->getPointerTo(),
                        llvm::Type::getInt64Ty(context)
                    }, false);
            fn = llvm::Function::Create(function_type,
                    llvm::Function::ExternalLinkage, runtime_func_name, *module);
        }
        return builder->CreateCall(fn, {
            lhs_data, lhs_len,
            is_lhs_allocatable, is_lhs_deferred,
            rhs_data, rhs_len});
    }

    llvm::Value* LLVMUtils::declare_string_constant(const ASR::StringConstant_t* str_const){

        /*  Don't depend on null_char.
            Fortran can represent null char is a char not as a terminating flag.
        */
        int64_t str_len = -1;
        ASRUtils::extract_value(ASRUtils::get_string_type(str_const->m_type)->m_len, str_len);
        
        std::string initial_string = std::string(str_const->m_s, str_len); 
        return declare_global_string(
            ASRUtils::get_string_type(str_const->m_type),
            initial_string, true, "string_const");
    }

    llvm::Value* LLVMUtils::declare_constant_stringArray(Allocator &al, const ASR::ArrayConstant_t* arr_const){
        LCOMPILERS_ASSERT(ASRUtils::extract_physical_type(arr_const->m_type) == ASR::PointerToDataArray)
        /*
            Array of string is just consecutive characters in memory. It's of pointerToDataArray physicalType
            We'll create fake duplicate-string type with length = OriginalStringLengthInArray * ArraySize 
        */
       // Insert null char between strings.
        std::string sequence((char*)arr_const->m_data, arr_const->m_n_data);
        { // Remove once we remove dependency on null-char
            int64_t str_len; ASRUtils::extract_value(ASRUtils::get_string_type(arr_const->m_type)->m_len, str_len);
            int64_t arr_size = ASRUtils::get_fixed_size_of_array(arr_const->m_type)*(str_len + 1);
            for(int idx = str_len; idx < arr_size; (idx += (str_len + 1)) ){
                sequence.insert(idx, "\0", 1);
            }
        }
        ASR::ttype_t* fake_string_type = ASRUtils::duplicate_type(al, ASRUtils::extract_type(arr_const->m_type));
        { // Modify length
            ASR::String_t* fake_string = ASR::down_cast<ASR::String_t>(fake_string_type);
            LCOMPILERS_ASSERT(ASR::is_a<ASR::IntegerConstant_t>(*fake_string->m_len))
            int64_t &fake_string_len = ASR::down_cast<ASR::IntegerConstant_t>(fake_string->m_len)->m_n;
            fake_string_len*= ASRUtils::get_fixed_size_of_array(arr_const->m_type); 
            { // Remove once we remove dependceny on null-char
                fake_string_len += (ASRUtils::get_fixed_size_of_array(arr_const->m_type) - 1); // counted null-character inside
            }
        }
        llvm::Value* global_arraystring = declare_global_string(
            ASRUtils::get_string_type(fake_string_type), sequence, true, "stringArray_const");
        //Make sure to set the length properly (Set length with the actual array's element length)
        if(ASRUtils::get_string_type(fake_string_type)->m_physical_type == ASR::DescriptorString){
            int64_t str_len;
             ASRUtils::extract_value(ASRUtils::get_string_type(arr_const->m_type)->m_len, str_len);
            builder->CreateStore(
                llvm::ConstantInt::get(context, llvm::APInt(64, str_len) ),
                get_string_length(ASRUtils::get_string_type(fake_string_type), global_arraystring, true));
        } else {
            throw LCompilersException("Unhandled case");
        }
        return global_arraystring;
    }

    llvm::Value* LLVMUtils::declare_global_string(
        ASR::String_t* str, std::string initial_data, bool is_const, std::string name, 
        llvm::GlobalValue::LinkageTypes linkage /*default is private*/){
        int64_t len = 0; ASRUtils::extract_value(str->m_len, len);
        initial_data.resize(len,' '); // Pad 
        llvm::Constant* len_constant = llvm::ConstantInt::get(context, llvm::APInt(64, len));
        llvm::Constant* string_constant;
        // setup global string constant (llvm array of i8)
        switch(str->m_len_kind){
            case ASR::DeferredLength:{
                string_constant = llvm::ConstantPointerNull::get(character_type);
                LCOMPILERS_ASSERT_MSG(ASRUtils::is_value_constant(str->m_len) || 
                    str->m_len_kind == ASR::DeferredLength,
                    "Handle this case");
                break;
            }
            case ASR::ExpressionLength:{
                LCOMPILERS_ASSERT_MSG(ASRUtils::is_value_constant(str->m_len), "Handle this case");
                // Type -> [len x i8]
                llvm::ArrayType *char_array_type = llvm::ArrayType::get(llvm::Type::getInt8Ty(context), len + 1 /*null-char*/);
                // [len x i8] c "DATA HERE\00"
                llvm::Constant *const_data_as_array = llvm::ConstantDataArray::getString(context, initial_data, true);
                // global [len x i8] c "DATA HERE\00"
                llvm::GlobalVariable *global_string_as_array = new llvm::GlobalVariable(
                    *module,
                    char_array_type,
                    is_const,
                    linkage,
                    const_data_as_array,
                    name+"_data"
                );
                llvm::Value *zero_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
                // i8* getelementptr inbounds ( global [len x i8] c "DATA HERE\00", i32 0, i32 0)
                llvm::Constant *char_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
                    char_array_type,
                    global_string_as_array,
                    {zero_const, zero_const});
                string_constant = char_ptr;
                break;
            }
            default:
                throw LCompilersException("Wrong physicalType for global string declaration");
        }
        // Declare the global string based on its physical type 
        switch(str->m_physical_type) {
            case ASR::DescriptorString:{
                llvm::Constant* string_descriptor_constant = llvm::ConstantStruct::get(
                    llvm::dyn_cast<llvm::StructType>(string_descriptor), 
                    {string_constant, len_constant});
                llvm::GlobalVariable* global_string_desc = new llvm::GlobalVariable(
                        *module,
                        string_descriptor, false,
                        linkage,
                        string_descriptor_constant,
                        name);
                return global_string_desc;
            }
            default:
                throw LCompilersException("Unhandled string physical type");
        }
    }

// Ugly function
// Refactor this (Try to use exisiting functinoalities)
llvm::Value* LLVMUtils::handle_global_nonallocatable_stringArray(Allocator& al, ASR::Array_t* array_t, ASR::ArrayConstant_t* arrayConst_t, std::string name){
    LCOMPILERS_ASSERT(array_t->m_physical_type == ASR::PointerToDataArray)
    /*
        Array of string is just consecutive characters in memory. It's of pointerToDataArray physicalType
        We'll create fake duplicate-string type with length = OriginalStringLengthInArray * ArraySize 
    */

   std::string sequence("");
    if(arrayConst_t){
        sequence = std::string((char*)arrayConst_t->m_data, arrayConst_t->m_n_data);
        // Insert null char between strings.
        { // Remove once we remove dependency on null-char
            int64_t str_len; ASRUtils::extract_value(ASRUtils::get_string_type(array_t->m_type)->m_len, str_len);
            int64_t arr_size = ASRUtils::get_fixed_size_of_array((ASR::ttype_t*)array_t)*(str_len + 1);
            for(int idx = str_len; idx < arr_size; (idx += (str_len + 1)) ){
                sequence.insert(idx, "\0", 1);
            }
        }
    }
    ASR::ttype_t* fake_string_type = ASRUtils::duplicate_type(al, array_t->m_type);
    { // Modify length
        ASR::String_t* fake_string = ASR::down_cast<ASR::String_t>(fake_string_type);
        LCOMPILERS_ASSERT(ASR::is_a<ASR::IntegerConstant_t>(*fake_string->m_len))
        int64_t &fake_string_len = ASR::down_cast<ASR::IntegerConstant_t>(fake_string->m_len)->m_n;
        fake_string_len*= ASRUtils::get_fixed_size_of_array((ASR::ttype_t*)array_t); 
        { // Remove once we remove dependceny on null-char
            fake_string_len += (ASRUtils::get_fixed_size_of_array( (ASR::ttype_t*)array_t ) - 1); // counted null-character inside
        }
    }
        
    ASR::String_t* str = ASRUtils::get_string_type(fake_string_type);
    int64_t len = 0; ASRUtils::extract_value(str->m_len, len);
    int64_t actual_len; ASRUtils::extract_value(ASRUtils::get_string_type(array_t->m_type)->m_len, actual_len);
    sequence.resize(len,' '); // Pad 
    llvm::Constant* len_constant = llvm::ConstantInt::get(context, llvm::APInt(64, actual_len));
    llvm::Constant* string_constant;
    // setup global string constant (llvm array of i8)
    switch(str->m_len_kind){
        case ASR::DeferredLength:{
            string_constant = llvm::ConstantPointerNull::get(character_type);
            LCOMPILERS_ASSERT_MSG(ASRUtils::is_value_constant(str->m_len) || 
                str->m_len_kind == ASR::DeferredLength,
                "Handle this case");
            break;
        }
        case ASR::ExpressionLength:{
            LCOMPILERS_ASSERT_MSG(ASRUtils::is_value_constant(str->m_len), "Handle this case");
            // Type -> [len x i8]
            llvm::ArrayType *char_array_type = llvm::ArrayType::get(llvm::Type::getInt8Ty(context), len + 1 /*null-char*/);
            // [len x i8] c "DATA HERE\00"
            llvm::Constant *const_data_as_array = llvm::ConstantDataArray::getString(context, sequence, true);
            // global [len x i8] c "DATA HERE\00"
            llvm::GlobalVariable *global_string_as_array = new llvm::GlobalVariable(
                *module,
                char_array_type,
                false,
                llvm::GlobalValue::PrivateLinkage,
                const_data_as_array,
                "_data"
            );
            llvm::Value *zero_const = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
            // i8* getelementptr inbounds ( global [len x i8] c "DATA HERE\00", i32 0, i32 0)
            llvm::Constant *char_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
                char_array_type,
                global_string_as_array,
                {zero_const, zero_const});
            string_constant = char_ptr;
            break;
        }
        default:
            throw LCompilersException("Wrong physicalType for global string declaration");
    }
    // Declare the global string based on its physical type 
    switch(str->m_physical_type) {
        case ASR::DescriptorString:{
            llvm::Constant* string_descriptor_constant = llvm::ConstantStruct::get(
                llvm::dyn_cast<llvm::StructType>(string_descriptor), 
                {string_constant, len_constant});
            llvm::GlobalVariable* global_string_desc = new llvm::GlobalVariable(
                    *module,
                    string_descriptor, false,
                    llvm::GlobalValue::PrivateLinkage,
                    string_descriptor_constant,
                    name);
            return global_string_desc;
        }
        default:
            throw LCompilersException("Unhandled string physical type");
    }
    return nullptr;

}
    llvm::Value* LLVMUtils::is_equal_by_value(llvm::Value* left, llvm::Value* right,
                                              llvm::Module* module, ASR::ttype_t* asr_type) {
        switch( asr_type->type ) {
            case ASR::ttypeType::Integer: {
                return builder->CreateICmpEQ(left, right);
            }
            case ASR::ttypeType::Logical: {
                llvm::Value* left_i32 = builder->CreateZExt(left, llvm::Type::getInt32Ty(context));
                llvm::Value* right_i32 = builder->CreateZExt(right, llvm::Type::getInt32Ty(context));
                return builder->CreateICmpEQ(left_i32, right_i32);
            }
            case ASR::ttypeType::Real: {
                return builder->CreateFCmpOEQ(left, right);
            }
            case ASR::ttypeType::String: {
                str_cmp_itr = LLVMUtils::CreateAlloca(llvm::Type::getInt32Ty(context));
                llvm::Value* null_char = llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),
                                                            llvm::APInt(8, '\0'));
                llvm::Value* idx = str_cmp_itr;
                LLVM::CreateStore(*builder,
                    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 0)),
                    idx);
                llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
                llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
                llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

                // head
                start_new_block(loophead);
                {
                    llvm::Value* i = LLVMUtils::CreateLoad2(llvm::Type::getInt32Ty(context), idx);
                    llvm::Value* l = LLVMUtils::CreateLoad2(llvm::Type::getInt8Ty(context), create_ptr_gep2(
                        llvm::Type::getInt8Ty(context), left, i));
                    llvm::Value* r = LLVMUtils::CreateLoad2(llvm::Type::getInt8Ty(context), create_ptr_gep2(
                        llvm::Type::getInt8Ty(context), right, i));
                    llvm::Value *cond = builder->CreateAnd(
                        builder->CreateICmpNE(l, null_char),
                        builder->CreateICmpNE(r, null_char)
                    );
                    cond = builder->CreateAnd(cond, builder->CreateICmpEQ(l, r));
                    builder->CreateCondBr(cond, loopbody, loopend);
                }

                // body
                start_new_block(loopbody);
                {
                    llvm::Value* i = LLVMUtils::CreateLoad2(llvm::Type::getInt32Ty(context), idx);
                    i = builder->CreateAdd(i, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                            llvm::APInt(32, 1)));
                    LLVM::CreateStore(*builder, i, idx);
                }

                builder->CreateBr(loophead);

                // end
                start_new_block(loopend);
                llvm::Value* i = LLVMUtils::CreateLoad2(llvm::Type::getInt32Ty(context), idx);
                llvm::Value* l = LLVMUtils::CreateLoad2(llvm::Type::getInt8Ty(context), create_ptr_gep2(
                    llvm::Type::getInt8Ty(context), left, i));
                llvm::Value* r = LLVMUtils::CreateLoad2(llvm::Type::getInt8Ty(context), create_ptr_gep2(
                    llvm::Type::getInt8Ty(context), right, i));
                return builder->CreateICmpEQ(l, r);
            }
            case ASR::ttypeType::Tuple: {
                ASR::Tuple_t* tuple_type = ASR::down_cast<ASR::Tuple_t>(asr_type);
                return tuple_api->check_tuple_equality(left, right, tuple_type, context,
                                                       builder, module);
            }
            case ASR::ttypeType::List: {
                ASR::List_t* list_type = ASR::down_cast<ASR::List_t>(asr_type);
                return list_api->check_list_equality(left, right, list_type->m_type,
                                                     context, builder, module);
            }
            default: {
                throw LCompilersException("LLVMUtils::is_equal_by_value isn't implemented for " +
                                          ASRUtils::type_to_str_python(asr_type));
            }
        }
    }

    llvm::Value* LLVMUtils::is_ineq_by_value(llvm::Value* left, llvm::Value* right,
                                             llvm::Module* module, ASR::ttype_t* asr_type,
                                             int8_t overload_id, ASR::ttype_t* int32_type) {
        /**
         * overloads:
         *  0    <
         *  1    <=
         *  2    >
         *  3    >=
         */
        llvm::CmpInst::Predicate pred;

        switch( asr_type->type ) {
            case ASR::ttypeType::Integer:
            case ASR::ttypeType::Logical: {
                if( asr_type->type == ASR::ttypeType::Logical ) {
                    left = builder->CreateZExt(left, llvm::Type::getInt32Ty(context));
                    right = builder->CreateZExt(right, llvm::Type::getInt32Ty(context));
                }
                switch( overload_id ) {
                    case 0: {
                        pred = llvm::CmpInst::Predicate::ICMP_SLT;
                        break;
                    }
                    case 1: {
                        pred = llvm::CmpInst::Predicate::ICMP_SLE;
                        break;
                    }
                    case 2: {
                        pred = llvm::CmpInst::Predicate::ICMP_SGT;
                        break;
                    }
                    case 3: {
                        pred = llvm::CmpInst::Predicate::ICMP_SGE;
                        break;
                    }
                    default: {
                        throw CodeGenError("Un-recognized overload-id: " + std::to_string(overload_id));
                    }
                }
                return builder->CreateICmp(pred, left, right);
            }
            case ASR::ttypeType::Real: {
                switch( overload_id ) {
                    case 0: {
                        pred = llvm::CmpInst::Predicate::FCMP_OLT;
                        break;
                    }
                    case 1: {
                        pred = llvm::CmpInst::Predicate::FCMP_OLE;
                        break;
                    }
                    case 2: {
                        pred = llvm::CmpInst::Predicate::FCMP_OGT;
                        break;
                    }
                    case 3: {
                        pred = llvm::CmpInst::Predicate::FCMP_OGE;
                        break;
                    }
                    default: {
                        throw CodeGenError("Un-recognized overload-id: " + std::to_string(overload_id));
                    }
                }
                return builder->CreateFCmp(pred, left, right);
            }
            case ASR::ttypeType::String: {
                str_cmp_itr = LLVMUtils::CreateAlloca(llvm::Type::getInt32Ty(context));
                llvm::Value* null_char = llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),
                                                            llvm::APInt(8, '\0'));
                llvm::Value* idx = str_cmp_itr;
                LLVM::CreateStore(*builder,
                    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 0)),
                    idx);
                llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
                llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
                llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

                // head
                start_new_block(loophead);
                {
                    llvm::Value* i = LLVMUtils::CreateLoad2(llvm::Type::getInt32Ty(context), idx);
                    llvm::Value* l = LLVMUtils::CreateLoad2(llvm::Type::getInt8Ty(context),
                        create_ptr_gep2(llvm::Type::getInt8Ty(context), left, i));
                    llvm::Value* r = LLVMUtils::CreateLoad2(llvm::Type::getInt8Ty(context),
                        create_ptr_gep2(llvm::Type::getInt8Ty(context), right, i));
                    llvm::Value *cond = builder->CreateAnd(
                        builder->CreateICmpNE(l, null_char),
                        builder->CreateICmpNE(r, null_char)
                    );
                    switch( overload_id ) {
                        case 0: {
                            pred = llvm::CmpInst::Predicate::ICMP_ULT;
                            break;
                        }
                        case 1: {
                            pred = llvm::CmpInst::Predicate::ICMP_ULE;
                            break;
                        }
                        case 2: {
                            pred = llvm::CmpInst::Predicate::ICMP_UGT;
                            break;
                        }
                        case 3: {
                            pred = llvm::CmpInst::Predicate::ICMP_UGE;
                            break;
                        }
                        default: {
                            throw CodeGenError("Un-recognized overload-id: " + std::to_string(overload_id));
                        }
                    }
                    cond = builder->CreateAnd(cond, builder->CreateICmp(pred, l, r));
                    builder->CreateCondBr(cond, loopbody, loopend);
                }

                // body
                start_new_block(loopbody);
                {
                    llvm::Value* i = LLVMUtils::CreateLoad2(llvm::Type::getInt32Ty(context), idx);
                    i = builder->CreateAdd(i, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                            llvm::APInt(32, 1)));
                    LLVM::CreateStore(*builder, i, idx);
                }

                builder->CreateBr(loophead);

                // end
                start_new_block(loopend);
                llvm::Value* i = LLVMUtils::CreateLoad2(llvm::Type::getInt32Ty(context), idx);
                llvm::Value* l = LLVMUtils::CreateLoad2(llvm::Type::getInt8Ty(context),
                    create_ptr_gep2(llvm::Type::getInt8Ty(context), left, i));
                llvm::Value* r = LLVMUtils::CreateLoad2(llvm::Type::getInt8Ty(context),
                    create_ptr_gep2(llvm::Type::getInt8Ty(context), right, i));
                return builder->CreateICmpULT(l, r);
            }
            case ASR::ttypeType::Tuple: {
                ASR::Tuple_t* tuple_type = ASR::down_cast<ASR::Tuple_t>(asr_type);
                return tuple_api->check_tuple_inequality(left, right, tuple_type, context,
                                                       builder, module, overload_id);
            }
            case ASR::ttypeType::List: {
                ASR::List_t* list_type = ASR::down_cast<ASR::List_t>(asr_type);
                return list_api->check_list_inequality(left, right, list_type->m_type,
                                                     context, builder, module,
                                                     overload_id, int32_type);
            }
            default: {
                throw LCompilersException("LLVMUtils::is_ineq_by_value isn't implemented for " +
                                          ASRUtils::type_to_str_python(asr_type));
            }
        }
    }

    void LLVMUtils::deepcopy(ASR::expr_t* src_expr, llvm::Value* src, llvm::Value* dest,
                             ASR::ttype_t* asr_dest_type,
                             ASR::ttype_t* asr_src_type, llvm::Module* module,
                             std::map<std::string, std::map<std::string, int>>& name2memidx) {
        switch( ASRUtils::type_get_past_array(asr_src_type)->type ) {
            case ASR::ttypeType::Integer:
            case ASR::ttypeType::UnsignedInteger:
            case ASR::ttypeType::Real:
            case ASR::ttypeType::Logical:
            case ASR::ttypeType::Complex: {
                if( ASRUtils::is_array(asr_src_type) ) {
                    ASR::array_physical_typeType physical_type = ASRUtils::extract_physical_type(asr_src_type);
                    switch( physical_type ) {
                        case ASR::array_physical_typeType::DescriptorArray: {
                            arr_api->copy_array(src, dest, module, asr_src_type, false);
                            break;
                        }
                        case ASR::array_physical_typeType::FixedSizeArray: {
                            llvm::Type* llvm_array_type = get_type_from_ttype_t_util(src_expr,
                                ASRUtils::type_get_past_allocatable(ASRUtils::type_get_past_pointer(asr_src_type)), module);
                            src = create_gep2(llvm_array_type, src, 0);
                            dest = create_gep2(llvm_array_type, dest, 0);
                            ASR::dimension_t* asr_dims = nullptr;
                            size_t asr_n_dims = ASRUtils::extract_dimensions_from_ttype(asr_src_type, asr_dims);
                            int64_t size = ASRUtils::get_fixed_size_of_array(asr_dims, asr_n_dims);
                            llvm::Type* llvm_data_type = get_type_from_ttype_t_util(src_expr, ASRUtils::type_get_past_array(
                                ASRUtils::type_get_past_allocatable(ASRUtils::type_get_past_pointer(asr_src_type))), module);
                            llvm::DataLayout data_layout(module->getDataLayout());
                            uint64_t data_size = data_layout.getTypeAllocSize(llvm_data_type);
                            llvm::Value* llvm_size = llvm::ConstantInt::get(context, llvm::APInt(32, size));
                            llvm_size = builder->CreateMul(llvm_size,
                                llvm::ConstantInt::get(context, llvm::APInt(32, data_size)));
                            builder->CreateMemCpy(dest, llvm::MaybeAlign(), src, llvm::MaybeAlign(), llvm_size);
                            break;
                        }
                        default: {
                            LCOMPILERS_ASSERT(false);
                        }
                    }
                } else {
                    LLVM::CreateStore(*builder, src, dest);
                }
                break ;
            };
            case ASR::ttypeType::String:
                lfortran_str_copy(dest, src, 
                    ASRUtils::get_string_type(asr_dest_type),
                    ASRUtils::get_string_type(asr_src_type),
                    ASRUtils::is_allocatable(asr_dest_type));
                break;
            case ASR::ttypeType::FunctionType:
            case ASR::ttypeType::CPtr: {
                LLVM::CreateStore(*builder, src, dest);
                break ;
            }
            case ASR::ttypeType::Allocatable: {
                ASR::Allocatable_t* alloc_type = ASR::down_cast<ASR::Allocatable_t>(asr_src_type);
                if( ASRUtils::is_array(alloc_type->m_type) ) {// non-primitive type
                    llvm::Type *array_type = get_type_from_ttype_t_util(src_expr, alloc_type->m_type, module);
                    src = CreateLoad2(array_type->getPointerTo(), src);
                    LLVM::CreateStore(*builder, src, dest);
                } else if(ASRUtils::is_string_only(alloc_type->m_type)){ //non-primitive type (vector)
                    lfortran_str_copy(dest, src, 
                        ASRUtils::get_string_type(asr_dest_type),
                        ASRUtils::get_string_type(asr_src_type),
                        ASRUtils::is_allocatable(asr_dest_type));
                } else {
                    LLVM::CreateStore(*builder, src, dest);
                }
                break;
            }
            case ASR::ttypeType::Tuple: {
                ASR::Tuple_t* tuple_type = ASR::down_cast<ASR::Tuple_t>(asr_src_type);
                tuple_api->tuple_deepcopy(src_expr, src, dest, tuple_type, module, name2memidx);
                break ;
            }
            case ASR::ttypeType::List: {
                ASR::List_t* list_type = ASR::down_cast<ASR::List_t>(asr_src_type);
                list_api->list_deepcopy(src_expr, src, dest, list_type, module, name2memidx);
                break ;
            }
            case ASR::ttypeType::Pointer: {
                ASR::Pointer_t* pointer_type = ASR::down_cast<ASR::Pointer_t>(asr_src_type);
                src = CreateLoad2(get_type_from_ttype_t_util(src_expr, pointer_type->m_type, module)->getPointerTo(), src);
                LLVM::CreateStore(*builder, src, dest);
                break ;
            }
            case ASR::ttypeType::Dict: {
                ASR::Dict_t* dict_type = ASR::down_cast<ASR::Dict_t>(asr_src_type);
                set_dict_api(dict_type);
                dict_api->dict_deepcopy(src_expr, src, dest, dict_type, module, name2memidx);
                break ;
            }
            case ASR::ttypeType::StructType: {
                ASR::Struct_t* struct_sym = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(src_expr)));
                std::string der_type_name = std::string(struct_sym->m_name);
                Allocator al(1024);
                while( struct_sym != nullptr ) {
                    for (size_t i = 0; i < struct_sym->n_members; i++) {
                        std::string mem_name = struct_sym->m_members[i];
                        ASR::symbol_t *mem_sym = struct_sym->m_symtab->get_symbol(mem_name);
                        int mem_idx = name2memidx[der_type_name][mem_name];
                        llvm::Value* src_member = nullptr;
                        if (llvm::isa<llvm::ConstantStruct>(src) ||
                            llvm::isa<llvm::ConstantAggregateZero>(src)) {
                            src_member = builder->CreateExtractValue(src, {static_cast<unsigned int>(mem_idx)});
                        } else {
                            src_member = create_gep2(name2dertype[der_type_name], src, mem_idx);
                        }
                        llvm::Type *mem_type = get_type_from_ttype_t_util(ASRUtils::get_expr_from_sym(al, mem_sym),
                            ASRUtils::symbol_type(mem_sym), module);
                        ASR::ttype_t* member_type = ASRUtils::symbol_type(mem_sym);
                        if( !LLVM::is_llvm_struct(member_type) &&
                            !ASRUtils::is_array(member_type) &&
                            !ASRUtils::is_pointer(member_type) &&
                            !ASRUtils::is_descriptorString(member_type) &&
                            !llvm::isa<llvm::ConstantStruct>(src) &&
                            !llvm::isa<llvm::ConstantAggregateZero>(src)) {
                            src_member = LLVMUtils::CreateLoad2(mem_type, src_member);
                        }
                        llvm::Value* dest_member = create_gep2(name2dertype[der_type_name], dest, mem_idx);
                        llvm::Value* is_allocated = llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), 1);

                        // If member is allocatable string, we need to check if it is allocated before copying
                        if (ASRUtils::is_allocatable(ASRUtils::symbol_type(mem_sym)) &&
                            ASR::is_a<ASR::String_t>(*ASRUtils::extract_type(ASRUtils::symbol_type(mem_sym)))) {
                            std::vector<llvm::Value*> idx_vec = {
                                llvm::ConstantInt::get(context, llvm::APInt(32, 0)),
                                llvm::ConstantInt::get(context, llvm::APInt(32, 0))};
                            llvm::Value* src_member_char = builder->CreateGEP(mem_type, src_member, idx_vec);;
                            src_member_char = LLVMUtils::CreateLoad2(
                                llvm::Type::getInt8Ty(context)->getPointerTo(), src_member_char);
                            is_allocated = builder->CreateICmpNE(
                                builder->CreatePtrToInt(src_member_char, llvm::Type::getInt64Ty(context)),
                                llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 0)));
                        }
                        create_if_else(is_allocated, [&]() {
                            deepcopy(ASRUtils::EXPR(ASR::make_Var_t(al, mem_sym->base.loc, mem_sym)), src_member, dest_member,
                            member_type, member_type,
                            module, name2memidx);
                        }, [=]() {});
                    }
                    if( struct_sym->m_parent != nullptr ) {
                        // gep the parent struct, which is the 0th member of the child struct
                        src = create_gep2(name2dertype[struct_sym->m_name], src, 0);
                        dest = create_gep2(name2dertype[struct_sym->m_name], dest, 0);

                        ASR::Struct_t* parent_struct_type_t =
                            ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(struct_sym->m_parent));

                        der_type_name = parent_struct_type_t->m_name;
                        struct_sym = parent_struct_type_t;
                    } else {
                        struct_sym = nullptr;
                    }
                }
                break ;
            }
            default: {
                throw LCompilersException("LLVMUtils::deepcopy isn't implemented for " +
                                          ASRUtils::type_to_str_fortran(asr_src_type));
            }
        }
    }
    llvm::Value* LLVMUtils::convert_kind(llvm::Value* val, llvm::Type* target_type){
        LCOMPILERS_ASSERT(val && target_type)
        LCOMPILERS_ASSERT(
            (val->getType()->isIntegerTy() && target_type->isIntegerTy()) ||
            (val->getType()->isFloatingPointTy() && target_type->isFloatingPointTy()));

        if(val->getType()->getPrimitiveSizeInBits() == target_type->getPrimitiveSizeInBits()){
            return val;
        } else if(val->getType()->getPrimitiveSizeInBits() > target_type->getPrimitiveSizeInBits()){
            return val->getType()->isIntegerTy() ?
                builder->CreateTrunc(val, target_type) : builder->CreateFPTrunc(val, target_type);
        } else {
            return val->getType()->isIntegerTy() ?
                builder->CreateSExt(val, target_type): builder->CreateFPExt(val, target_type);
        }
    }

    LLVMList::LLVMList(llvm::LLVMContext& context_,
        LLVMUtils* llvm_utils_,
        llvm::IRBuilder<>* builder_):
        context(context_),
        llvm_utils(std::move(llvm_utils_)),
        builder(std::move(builder_)) {}

    LLVMDictInterface::LLVMDictInterface(llvm::LLVMContext& context_,
        LLVMUtils* llvm_utils_,
        llvm::IRBuilder<>* builder_):
        context(context_),
        llvm_utils(std::move(llvm_utils_)),
        builder(std::move(builder_)),
        pos_ptr(nullptr), is_key_matching_var(nullptr),
        idx_ptr(nullptr), hash_iter(nullptr),
        hash_value(nullptr), polynomial_powers(nullptr),
        chain_itr(nullptr), chain_itr_prev(nullptr),
        old_capacity(nullptr), old_key_value_pairs(nullptr),
        old_key_mask(nullptr), is_dict_present_(false) {
    }

    LLVMDict::LLVMDict(llvm::LLVMContext& context_,
        LLVMUtils* llvm_utils_,
        llvm::IRBuilder<>* builder_):
        LLVMDictInterface(context_, llvm_utils_, builder_) {
    }

    LLVMDictSeparateChaining::LLVMDictSeparateChaining(
        llvm::LLVMContext& context_,
        LLVMUtils* llvm_utils_,
        llvm::IRBuilder<>* builder_):
        LLVMDictInterface(context_, llvm_utils_, builder_) {
    }

    LLVMDictOptimizedLinearProbing::LLVMDictOptimizedLinearProbing(
        llvm::LLVMContext& context_,
        LLVMUtils* llvm_utils_,
        llvm::IRBuilder<>* builder_):
        LLVMDict(context_, llvm_utils_, builder_) {
        }

    llvm::Type* LLVMList::get_list_type(llvm::Type* el_type, std::string& type_code,
                                        int32_t type_size) {
        if( typecode2listtype.find(type_code) != typecode2listtype.end() ) {
            return std::get<0>(typecode2listtype[type_code]);
        }
        std::vector<llvm::Type*> list_type_vec = {llvm::Type::getInt32Ty(context),
                                                  llvm::Type::getInt32Ty(context),
                                                  el_type->getPointerTo()};
        llvm::StructType* list_desc = llvm::StructType::create(context, list_type_vec, "list");
        typecode2listtype[type_code] = std::make_tuple(list_desc, type_size, el_type);
        return list_desc;
    }

    llvm::Type* LLVMDict::get_dict_type(std::string key_type_code, std::string value_type_code,
        int32_t key_type_size, int32_t value_type_size,
        llvm::Type* key_type, llvm::Type* value_type) {
        is_dict_present_ = true;
        std::pair<std::string, std::string> llvm_key = std::make_pair(key_type_code, value_type_code);
        if( typecode2dicttype.find(llvm_key) != typecode2dicttype.end() ) {
            return std::get<0>(typecode2dicttype[llvm_key]);
        }

        llvm::Type* key_list_type = llvm_utils->list_api->get_list_type(key_type,
                                        key_type_code, key_type_size);
        llvm::Type* value_list_type = llvm_utils->list_api->get_list_type(value_type,
                                        value_type_code, value_type_size);
        std::vector<llvm::Type*> dict_type_vec = {llvm::Type::getInt32Ty(context),
                                                  key_list_type, value_list_type,
                                                  llvm::Type::getInt8Ty(context)->getPointerTo()};
        llvm::Type* dict_desc = llvm::StructType::create(context, dict_type_vec, "dict");
        typecode2dicttype[llvm_key] = std::make_tuple(dict_desc,
                                        std::make_pair(key_type_size, value_type_size),
                                        std::make_pair(key_type, value_type));
        return dict_desc;
    }

    llvm::Type* LLVMDictSeparateChaining::get_key_value_pair_type(
        std::string key_type_code, std::string value_type_code) {
        std::pair<std::string, std::string> llvm_key = std::make_pair(key_type_code, value_type_code);
        return typecode2kvstruct[llvm_key];
    }

    llvm::Type* LLVMDictSeparateChaining::get_key_value_pair_type(
        ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type) {
        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);
        std::string value_type_code = ASRUtils::get_type_code(value_asr_type);
        return get_key_value_pair_type(key_type_code, value_type_code);
    }

    llvm::Type* LLVMDictSeparateChaining::get_dict_type(
        std::string key_type_code, std::string value_type_code,
        int32_t key_type_size, int32_t value_type_size,
        llvm::Type* key_type, llvm::Type* value_type) {
        is_dict_present_ = true;
        std::pair<std::string, std::string> llvm_key = std::make_pair(key_type_code, value_type_code);
        if( typecode2dicttype.find(llvm_key) != typecode2dicttype.end() ) {
            return std::get<0>(typecode2dicttype[llvm_key]);
        }

        std::vector<llvm::Type*> key_value_vec = {key_type, value_type,
                                                  llvm::Type::getInt8Ty(context)->getPointerTo()};
        llvm::Type* key_value_pair = llvm::StructType::create(context, key_value_vec, "key_value");
        std::vector<llvm::Type*> dict_type_vec = {llvm::Type::getInt32Ty(context),
                                                  llvm::Type::getInt32Ty(context),
                                                  llvm::Type::getInt32Ty(context),
                                                  key_value_pair->getPointerTo(),
                                                  llvm::Type::getInt8Ty(context)->getPointerTo(),
                                                  llvm::Type::getInt1Ty(context)};
        llvm::Type* dict_desc = llvm::StructType::create(context, dict_type_vec, "dict");
        typecode2dicttype[llvm_key] = std::make_tuple(dict_desc,
                                        std::make_pair(key_type_size, value_type_size),
                                        std::make_pair(key_type, value_type));
        typecode2kvstruct[llvm_key] = key_value_pair;
        return dict_desc;
    }

    llvm::Value* LLVMList::get_pointer_to_list_data_using_type(llvm::Type* list_type, llvm::Value* list) {
        return llvm_utils->create_gep2(list_type, list, 2);
    }

    llvm::Value* LLVMList::get_pointer_to_current_end_point_using_type(llvm::Type* list_type, llvm::Value* list) {
        return llvm_utils->create_gep2(list_type, list, 0);
    }

    llvm::Value* LLVMList::get_pointer_to_current_capacity_using_type(llvm::Type* list_type, llvm::Value* list) {
        return llvm_utils->create_gep2(list_type, list, 1);
    }

    void LLVMList::list_init(std::string& type_code, llvm::Value* list,
                   llvm::Module* module, int32_t initial_capacity, int32_t n) {
        if( typecode2listtype.find(type_code) == typecode2listtype.end() ) {
            throw LCompilersException("list for " + type_code + " not declared yet.");
        }
        int32_t type_size = std::get<1>(typecode2listtype[type_code]);
        llvm::Value* arg_size = llvm::ConstantInt::get(context,
                                    llvm::APInt(32, type_size * initial_capacity));

        llvm::Value* list_data = LLVM::lfortran_malloc(context, *module, *builder,
                                                       arg_size);
        llvm::Type* el_type = std::get<2>(typecode2listtype[type_code]);
        llvm::Type* list_type= std::get<0>(typecode2listtype[type_code]);

        list_data = builder->CreateBitCast(list_data, el_type->getPointerTo());
        llvm::Value* list_data_ptr = get_pointer_to_list_data_using_type(list_type, list);
        builder->CreateStore(list_data, list_data_ptr);
        llvm::Value* current_end_point = llvm::ConstantInt::get(context, llvm::APInt(32, n));
        llvm::Value* current_capacity = llvm::ConstantInt::get(context, llvm::APInt(32, initial_capacity));
        builder->CreateStore(current_end_point, get_pointer_to_current_end_point_using_type(list_type, list));
        builder->CreateStore(current_capacity, get_pointer_to_current_capacity_using_type(list_type, list));
    }

    void LLVMList::list_init(std::string& type_code, llvm::Value* list,
                             llvm::Module* module, llvm::Value* initial_capacity,
                             llvm::Value* n) {
        if( typecode2listtype.find(type_code) == typecode2listtype.end() ) {
            throw LCompilersException("list for " + type_code + " not declared yet.");
        }
        int32_t type_size = std::get<1>(typecode2listtype[type_code]);
        llvm::Type* list_type = std::get<0>(typecode2listtype[type_code]);
        llvm::Value* llvm_type_size = llvm::ConstantInt::get(context, llvm::APInt(32, type_size));
        llvm::Value* arg_size = builder->CreateMul(llvm_type_size, initial_capacity);
        llvm::Value* list_data = LLVM::lfortran_malloc(context, *module, *builder, arg_size);

        llvm::Type* el_type = std::get<2>(typecode2listtype[type_code]);
        list_data = builder->CreateBitCast(list_data, el_type->getPointerTo());
        llvm::Value* list_data_ptr = get_pointer_to_list_data_using_type(list_type, list);
        builder->CreateStore(list_data, list_data_ptr);
        builder->CreateStore(n, get_pointer_to_current_end_point_using_type(list_type, list));
        builder->CreateStore(initial_capacity, get_pointer_to_current_capacity_using_type(list_type, list));
    }

    llvm::Value* LLVMDict::get_key_list(llvm::Value* dict) {
        return llvm_utils->create_gep(dict, 1);
    }

    llvm::Value* LLVMDictSeparateChaining::get_pointer_to_key_value_pairs(llvm::Value* dict) {
        return llvm_utils->create_gep(dict, 3);
    }

    llvm::Value* LLVMDictSeparateChaining::get_key_list(llvm::Value* /*dict*/) {
        return nullptr;
    }

    llvm::Value* LLVMDict::get_value_list(llvm::Value* dict) {
        return llvm_utils->create_gep(dict, 2);
    }

    llvm::Value* LLVMDictSeparateChaining::get_value_list(llvm::Value* /*dict*/) {
        return nullptr;
    }

    llvm::Value* LLVMDict::get_pointer_to_occupancy(llvm::Value* dict) {
        return llvm_utils->create_gep(dict, 0);
    }

    llvm::Value* LLVMDict::get_pointer_to_occupancy_using_type(llvm::Type* dict_type, llvm::Value* dict) {
        return llvm_utils->create_gep2(dict_type, dict, 0);
    }

    llvm::Value* LLVMDictSeparateChaining::get_pointer_to_occupancy(llvm::Value* dict) {
        return llvm_utils->create_gep(dict, 0);
    }

    llvm::Value* LLVMDictSeparateChaining::get_pointer_to_rehash_flag(llvm::Value* dict) {
        return llvm_utils->create_gep(dict, 5);
    }

    llvm::Value* LLVMDictSeparateChaining::get_pointer_to_number_of_filled_buckets(llvm::Value* dict) {
        return llvm_utils->create_gep(dict, 1);
    }

    llvm::Value* LLVMDict::get_pointer_to_capacity_using_typecode(std::string& /*key_type_code*/, std::string& value_type_code,
                                                    llvm::Value* dict) {
        return llvm_utils->list_api->get_pointer_to_current_capacity_using_type(
                            llvm_utils->list_api->get_list_type(nullptr, value_type_code, 0),
                            get_value_list(dict));
    }

    llvm::Value* LLVMDictSeparateChaining::get_pointer_to_capacity(llvm::Value* dict) {
        return llvm_utils->create_gep(dict, 2);
    }

    llvm::Value* LLVMDictSeparateChaining::get_pointer_to_capacity_using_typecode(std::string& key_type_code, std::string& value_type_code,
                                                                    llvm::Value* dict) {
        return llvm_utils->create_gep2(get_dict_type(key_type_code, value_type_code, 0, 0, nullptr, nullptr), dict, 2);
    }

    void LLVMDict::dict_init(std::string key_type_code, std::string value_type_code,
                             llvm::Value* dict, llvm::Module* module, size_t initial_capacity) {
        llvm::Value* n_ptr = get_pointer_to_occupancy(dict);
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                           llvm::APInt(32, 0)), n_ptr);
        llvm::Value* key_list = get_key_list(dict);
        llvm::Value* value_list = get_value_list(dict);
        llvm_utils->list_api->list_init(key_type_code, key_list, module,
                                        initial_capacity, initial_capacity);
        llvm_utils->list_api->list_init(value_type_code, value_list, module,
                                        initial_capacity, initial_capacity);
        llvm::DataLayout data_layout(module->getDataLayout());
        size_t mask_size = data_layout.getTypeAllocSize(llvm::Type::getInt8Ty(context));
        llvm::Value* llvm_capacity = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                            llvm::APInt(32, initial_capacity));
        llvm::Value* llvm_mask_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                            llvm::APInt(32, mask_size));
        llvm::Value* key_mask = LLVM::lfortran_calloc(context, *module, *builder, llvm_capacity,
                                                      llvm_mask_size);
        LLVM::CreateStore(*builder, key_mask, get_pointer_to_keymask(dict));
    }

    void LLVMDictSeparateChaining::dict_init(std::string key_type_code, std::string value_type_code,
        llvm::Value* dict, llvm::Module* module, size_t initial_capacity) {
        llvm::Value* llvm_capacity = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, initial_capacity));
        llvm::Value* rehash_flag_ptr = get_pointer_to_rehash_flag(dict);
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), llvm::APInt(1, 1)), rehash_flag_ptr);
        dict_init_given_initial_capacity(key_type_code, value_type_code, dict, module, llvm_capacity);
    }

    void LLVMDictSeparateChaining::dict_init_given_initial_capacity(
        std::string key_type_code, std::string value_type_code,
        llvm::Value* dict, llvm::Module* module, llvm::Value* llvm_capacity) {
        llvm::Value* rehash_flag_ptr = get_pointer_to_rehash_flag(dict);
        llvm::Value* rehash_flag = llvm_utils->CreateLoad2(
            llvm::Type::getInt1Ty(context), rehash_flag_ptr);
        llvm::Value* llvm_zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 0));
        llvm::Value* occupancy_ptr = get_pointer_to_occupancy(dict);
        LLVM::CreateStore(*builder, llvm_zero, occupancy_ptr);
        llvm::Value* num_buckets_filled_ptr = get_pointer_to_number_of_filled_buckets(dict);
        LLVM::CreateStore(*builder, llvm_zero, num_buckets_filled_ptr);

        llvm::DataLayout data_layout(module->getDataLayout());
        llvm::Type* key_value_pair_type = get_key_value_pair_type(key_type_code, value_type_code);
        size_t key_value_type_size = data_layout.getTypeAllocSize(key_value_pair_type);
        llvm::Value* llvm_key_value_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, key_value_type_size));
        llvm::Value* malloc_size = builder->CreateMul(llvm_capacity, llvm_key_value_size);
        llvm::Value* key_value_ptr = LLVM::lfortran_malloc(context, *module, *builder, malloc_size);
        rehash_flag = builder->CreateAnd(rehash_flag,
                        builder->CreateICmpNE(key_value_ptr,
                        llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()))
                    );
        key_value_ptr = builder->CreateBitCast(key_value_ptr, key_value_pair_type->getPointerTo());
        LLVM::CreateStore(*builder, key_value_ptr, get_pointer_to_key_value_pairs(dict));

        size_t mask_size = data_layout.getTypeAllocSize(llvm::Type::getInt8Ty(context));
        llvm::Value* llvm_mask_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                            llvm::APInt(32, mask_size));
        llvm::Value* key_mask = LLVM::lfortran_calloc(context, *module, *builder, llvm_capacity,
                                                      llvm_mask_size);
        rehash_flag = builder->CreateAnd(rehash_flag,
                        builder->CreateICmpNE(key_mask,
                        llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()))
                    );
        LLVM::CreateStore(*builder, key_mask, get_pointer_to_keymask(dict));

        llvm::Value* capacity_ptr = get_pointer_to_capacity(dict);
        LLVM::CreateStore(*builder, llvm_capacity, capacity_ptr);
        LLVM::CreateStore(*builder, rehash_flag, rehash_flag_ptr);
    }

    void LLVMList::list_deepcopy(ASR::expr_t* src_expr, llvm::Value* src, llvm::Value* dest,
        ASR::List_t* list_type, llvm::Module* module,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        list_deepcopy(src_expr, src, dest, list_type->m_type, module, name2memidx);
    }

    void LLVMList::list_deepcopy([[maybe_unused]] ASR::expr_t* src_expr, llvm::Value* src, llvm::Value* dest,
                                 ASR::ttype_t* element_type, llvm::Module* module,
                                 std::map<std::string, std::map<std::string, int>>& name2memidx) {
        LCOMPILERS_ASSERT(src->getType() == dest->getType());
        std::string src_type_code = ASRUtils::get_type_code(element_type);
        llvm::Type* list_type = get_list_type(nullptr, src_type_code, 0);

        llvm::Value* src_end_point = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context),
            get_pointer_to_current_end_point_using_type(list_type, src));
        llvm::Value* src_capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context),
            get_pointer_to_current_capacity_using_type(list_type, src));
        llvm::Value* dest_end_point_ptr = get_pointer_to_current_end_point_using_type(list_type, dest);
        llvm::Value* dest_capacity_ptr = get_pointer_to_current_capacity_using_type(list_type, dest);
        builder->CreateStore(src_end_point, dest_end_point_ptr);
        builder->CreateStore(src_capacity, dest_capacity_ptr);
        llvm::Type* el_type = std::get<2>(typecode2listtype[src_type_code]);
        llvm::Value* src_data = llvm_utils->CreateLoad2(el_type->getPointerTo(), get_pointer_to_list_data_using_type(list_type, src));
        int32_t type_size = std::get<1>(typecode2listtype[src_type_code]);
        llvm::Value* arg_size = builder->CreateMul(llvm::ConstantInt::get(context,
                                                   llvm::APInt(32, type_size)), src_capacity);
        llvm::Value* copy_data = LLVM::lfortran_malloc(context, *module, *builder,
                                                       arg_size);
        copy_data = builder->CreateBitCast(copy_data, el_type->getPointerTo());

        // We consider the case when the element type of a list is defined by a struct
        // which may also contain non-trivial structs (such as in case of list[list[f64]],
        // list[tuple[f64]]). We need to make sure that all the data inside those structs
        // is deepcopied and not just the address of the first element of those structs.
        // Hence we dive deeper into the lowest level of nested types and deepcopy everything
        // properly. If we don't consider this case then the data only from first level of nested types
        // will be deep copied and rest will be shallow copied. The importance of this case
        // can be figured out by goind through, integration_tests/test_list_06.py and
        // integration_tests/test_list_07.py.
        if( LLVM::is_llvm_struct(element_type) ) {
            builder->CreateStore(copy_data, get_pointer_to_list_data_using_type(list_type, dest));
            // TODO: Should be created outside the user loop and not here.
            // LLVMList should treat them as data members and create them
            // only if they are NULL
            llvm::AllocaInst *pos_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
            LLVM::CreateStore(*builder, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                               llvm::APInt(32, 0)), pos_ptr);

            llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
            llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
            llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

            // head
            llvm_utils->start_new_block(loophead);
            {
                llvm::Value *cond = builder->CreateICmpSGT(
                                            src_end_point,
                                            llvm_utils->CreateLoad2(
                                                llvm::Type::getInt32Ty(context), pos_ptr));
                builder->CreateCondBr(cond, loopbody, loopend);
            }

            // body
            llvm_utils->start_new_block(loopbody);
            {
                llvm::Value* pos = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), pos_ptr);
                llvm::Value* srci = read_item_using_ttype(element_type, src, pos, false, module, true);
                llvm::Value* desti = read_item_using_ttype(element_type, dest, pos, false, module, true);
                llvm_utils->deepcopy(nullptr, srci, desti, element_type, element_type, module, name2memidx);
                llvm::Value* tmp = builder->CreateAdd(
                            pos,
                            llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
                LLVM::CreateStore(*builder, tmp, pos_ptr);
            }

            builder->CreateBr(loophead);

            // end
            llvm_utils->start_new_block(loopend);
        } else {
            builder->CreateMemCpy(copy_data, llvm::MaybeAlign(), src_data,
                                  llvm::MaybeAlign(), arg_size);
            builder->CreateStore(copy_data, get_pointer_to_list_data_using_type(list_type, dest));
        }
    }

    void LLVMDict::dict_deepcopy(ASR::expr_t* src_expr, llvm::Value* src, llvm::Value* dest,
                                 ASR::Dict_t* dict_type, llvm::Module* module,
                                 std::map<std::string, std::map<std::string, int>>& name2memidx) {
        LCOMPILERS_ASSERT(src->getType() == dest->getType());
        // TODO: Fix when refactoring out type_codes
        std::string key_type_code = ASRUtils::get_type_code(dict_type->m_key_type);
        std::string value_type_code = ASRUtils::get_type_code(dict_type->m_value_type);

        llvm::Value* src_occupancy = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_occupancy(src));
        llvm::Value* dest_occupancy_ptr = get_pointer_to_occupancy(dest);
        LLVM::CreateStore(*builder, src_occupancy, dest_occupancy_ptr);

        llvm::Value* src_key_list = get_key_list(src);
        llvm::Value* dest_key_list = get_key_list(dest);
        llvm_utils->list_api->list_deepcopy(src_expr, src_key_list, dest_key_list,
                                            dict_type->m_key_type, module,
                                            name2memidx);

        llvm::Value* src_value_list = get_value_list(src);
        llvm::Value* dest_value_list = get_value_list(dest);
        llvm_utils->list_api->list_deepcopy(src_expr, src_value_list, dest_value_list,
                                            dict_type->m_value_type, module, name2memidx);

        llvm::Value* src_key_mask = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context)->getPointerTo(),
            get_pointer_to_keymask(src));
        llvm::Value* dest_key_mask_ptr = get_pointer_to_keymask(dest);
        llvm::DataLayout data_layout(module->getDataLayout());
        size_t mask_size = data_layout.getTypeAllocSize(llvm::Type::getInt8Ty(context));
        llvm::Value* llvm_mask_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                            llvm::APInt(32, mask_size));
        llvm::Value* src_capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context),
                                              get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, src));
        llvm::Value* dest_key_mask = LLVM::lfortran_calloc(context, *module, *builder, src_capacity,
                                                      llvm_mask_size);
        builder->CreateMemCpy(dest_key_mask, llvm::MaybeAlign(), src_key_mask,
                              llvm::MaybeAlign(), builder->CreateMul(src_capacity, llvm_mask_size));
        LLVM::CreateStore(*builder, dest_key_mask, dest_key_mask_ptr);
    }

    void LLVMDictSeparateChaining::deepcopy_key_value_pair_linked_list(ASR::expr_t* src_expr,
        llvm::Value* srci, llvm::Value* desti, llvm::Value* dest_key_value_pairs,
        ASR::Dict_t* dict_type, llvm::Module* module,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        src_itr = llvm_utils->CreateAlloca(llvm::Type::getInt8Ty(context)->getPointerTo());
        dest_itr = llvm_utils->CreateAlloca(llvm::Type::getInt8Ty(context)->getPointerTo());
        llvm::Type* key_value_pair_type = get_key_value_pair_type(dict_type->m_key_type, dict_type->m_value_type)->getPointerTo();
        LLVM::CreateStore(*builder,
            builder->CreateBitCast(srci, llvm::Type::getInt8Ty(context)->getPointerTo()),
            src_itr);
        LLVM::CreateStore(*builder,
            builder->CreateBitCast(desti, llvm::Type::getInt8Ty(context)->getPointerTo()),
            dest_itr);
        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");
        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpNE(
                llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context)->getPointerTo(), src_itr),
                llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo())
            );
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* curr_src = builder->CreateBitCast(llvm_utils->CreateLoad2(
                llvm::Type::getInt8Ty(context)->getPointerTo(), src_itr),
                key_value_pair_type);
            llvm::Value* curr_dest = builder->CreateBitCast(llvm_utils->CreateLoad2(
                llvm::Type::getInt8Ty(context)->getPointerTo(), dest_itr),
                key_value_pair_type);
            std::pair<std::string, std::string> llvm_key = std::make_pair(
                ASRUtils::get_type_code(dict_type->m_key_type),
                ASRUtils::get_type_code(dict_type->m_value_type));
            llvm::Type* key_type = std::get<2>(typecode2dicttype[llvm_key]).first;
            llvm::Type* value_type = std::get<2>(typecode2dicttype[llvm_key]).second;
            llvm::Type* key_value_pair = typecode2kvstruct[llvm_key];

            llvm::Value* src_key_ptr = llvm_utils->create_gep2(key_value_pair, curr_src, 0);
            llvm::Value* src_value_ptr = llvm_utils->create_gep2(key_value_pair, curr_src, 1);
            llvm::Value *src_key = src_key_ptr, *src_value = src_value_ptr;
            if( !LLVM::is_llvm_struct(dict_type->m_key_type) ) {
                src_key = llvm_utils->CreateLoad2(key_type, src_key_ptr);
            }
            if( !LLVM::is_llvm_struct(dict_type->m_value_type) ) {
                src_value = llvm_utils->CreateLoad2(value_type, src_value_ptr);
            }
            llvm::Value* dest_key_ptr = llvm_utils->create_gep2(key_value_pair, curr_dest, 0);
            llvm::Value* dest_value_ptr = llvm_utils->create_gep2(key_value_pair, curr_dest, 1);
            llvm_utils->deepcopy(src_expr, src_key, dest_key_ptr, dict_type->m_key_type, dict_type->m_key_type, module, name2memidx);
            llvm_utils->deepcopy(src_expr, src_value, dest_value_ptr, dict_type->m_value_type, dict_type->m_value_type, module, name2memidx);

            llvm::Value* src_next_ptr = llvm_utils->CreateLoad2(
                llvm::Type::getInt8Ty(context)->getPointerTo(), llvm_utils->create_gep2(
                   key_value_pair, curr_src, 2));
            llvm::Value* curr_dest_next_ptr = llvm_utils->create_gep2(
                key_value_pair, curr_dest, 2);
            LLVM::CreateStore(*builder, src_next_ptr, src_itr);
            llvm::Function *fn = builder->GetInsertBlock()->getParent();
            llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", fn);
            llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context, "else");
            llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifcont");
            llvm::Value* src_next_exists = builder->CreateICmpNE(src_next_ptr,
                llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()));
            builder->CreateCondBr(src_next_exists, thenBB, elseBB);
            builder->SetInsertPoint(thenBB);
            {
                llvm::Value* next_idx = llvm_utils->CreateLoad2(
                    llvm::Type::getInt32Ty(context), next_ptr);
                llvm::Value* dest_next_ptr = llvm_utils->create_ptr_gep2(
                    key_value_pair, dest_key_value_pairs, next_idx);
                dest_next_ptr = builder->CreateBitCast(dest_next_ptr, llvm::Type::getInt8Ty(context)->getPointerTo());
                LLVM::CreateStore(*builder, dest_next_ptr, curr_dest_next_ptr);
                LLVM::CreateStore(*builder, dest_next_ptr, dest_itr);
                next_idx = builder->CreateAdd(next_idx, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                              llvm::APInt(32, 1)));
                LLVM::CreateStore(*builder, next_idx, next_ptr);
            }
            builder->CreateBr(mergeBB);
            llvm_utils->start_new_block(elseBB);
            {
                LLVM::CreateStore(*builder,
                    llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()),
                    curr_dest_next_ptr
                );
            }
            llvm_utils->start_new_block(mergeBB);
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);
    }

    void LLVMDictSeparateChaining::write_key_value_pair_linked_list(
        ASR::expr_t* dict_expr, llvm::Value* kv_ll, llvm::Value* dict, llvm::Value* capacity,
        ASR::ttype_t* m_key_type, ASR::ttype_t* m_value_type, llvm::Module* module,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        src_itr = llvm_utils->CreateAlloca(llvm::Type::getInt8Ty(context)->getPointerTo());
        llvm::Type* key_value_pair_type = get_key_value_pair_type(m_key_type, m_value_type)->getPointerTo();
        LLVM::CreateStore(*builder,
            builder->CreateBitCast(kv_ll, llvm::Type::getInt8Ty(context)->getPointerTo()),
            src_itr);
        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");
        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpNE(
                llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context)->getPointerTo(), src_itr),
                llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo())
            );
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* curr_src = builder->CreateBitCast(llvm_utils->CreateLoad2(
                llvm::Type::getInt8Ty(context)->getPointerTo(), src_itr),
                key_value_pair_type);
            std::pair<std::string, std::string> llvm_key = std::make_pair(
                ASRUtils::get_type_code(m_key_type),
                ASRUtils::get_type_code(m_value_type));
            llvm::Type* key_type = std::get<2>(typecode2dicttype[llvm_key]).first;
            llvm::Type* value_type = std::get<2>(typecode2dicttype[llvm_key]).second;
            llvm::Type* key_value_pair = typecode2kvstruct[llvm_key];

            llvm::Value* src_key_ptr = llvm_utils->create_gep2(key_value_pair, curr_src, 0);
            llvm::Value* src_value_ptr = llvm_utils->create_gep2(key_value_pair, curr_src, 1);
            llvm::Value *src_key = src_key_ptr, *src_value = src_value_ptr;
            if( !LLVM::is_llvm_struct(m_key_type) ) {
                src_key = llvm_utils->CreateLoad2(key_type, src_key_ptr);
            }
            if( !LLVM::is_llvm_struct(m_value_type) ) {
                src_value = llvm_utils->CreateLoad2(value_type, src_value_ptr);
            }
            llvm::Value* key_hash = get_key_hash(capacity, src_key, m_key_type, module);
            resolve_collision_for_write(
                dict_expr, dict, key_hash, src_key,
                src_value, module,
                m_key_type, m_value_type,
                name2memidx);

            llvm::Value* src_next_ptr = llvm_utils->CreateLoad2(
                llvm::Type::getInt8Ty(context)->getPointerTo(),
                llvm_utils->create_gep2(
                    key_value_pair,
                    curr_src, 2)
            );
            LLVM::CreateStore(*builder, src_next_ptr, src_itr);
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);
    }

    void LLVMDictSeparateChaining::dict_deepcopy(
        ASR::expr_t* src_expr,
        llvm::Value* src, llvm::Value* dest,
        ASR::Dict_t* dict_type, llvm::Module* module,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        llvm::Value* src_occupancy = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_occupancy(src));
        llvm::Value* src_filled_buckets = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_number_of_filled_buckets(src));
        llvm::Value* src_capacity = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_capacity(src));
        llvm::Value* src_key_mask = llvm_utils->CreateLoad2(
            llvm::Type::getInt8Ty(context)->getPointerTo(), get_pointer_to_keymask(src));
        llvm::Value* src_rehash_flag = llvm_utils->CreateLoad2(
            llvm::Type::getInt1Ty(context), get_pointer_to_rehash_flag(src));
        LLVM::CreateStore(*builder, src_occupancy, get_pointer_to_occupancy(dest));
        LLVM::CreateStore(*builder, src_filled_buckets, get_pointer_to_number_of_filled_buckets(dest));
        LLVM::CreateStore(*builder, src_capacity, get_pointer_to_capacity(dest));
        LLVM::CreateStore(*builder, src_rehash_flag, get_pointer_to_rehash_flag(dest));
        llvm::DataLayout data_layout(module->getDataLayout());
        size_t mask_size = data_layout.getTypeAllocSize(llvm::Type::getInt8Ty(context));
        llvm::Value* llvm_mask_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                            llvm::APInt(32, mask_size));
        llvm::Value* malloc_size = builder->CreateMul(src_capacity, llvm_mask_size);
        llvm::Value* dest_key_mask = LLVM::lfortran_malloc(context, *module, *builder, malloc_size);
        LLVM::CreateStore(*builder, dest_key_mask, get_pointer_to_keymask(dest));

        malloc_size = builder->CreateSub(src_occupancy, src_filled_buckets);
        malloc_size = builder->CreateAdd(src_capacity, malloc_size);
        size_t kv_struct_size = data_layout.getTypeAllocSize(get_key_value_pair_type(dict_type->m_key_type,
                                dict_type->m_value_type));
        llvm::Value* llvm_kv_struct_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, kv_struct_size));
        malloc_size = builder->CreateMul(malloc_size, llvm_kv_struct_size);
        llvm::Value* dest_key_value_pairs = LLVM::lfortran_malloc(context, *module, *builder, malloc_size);
        dest_key_value_pairs = builder->CreateBitCast(
            dest_key_value_pairs,
            get_key_value_pair_type(dict_type->m_key_type, dict_type->m_value_type)->getPointerTo());
        copy_itr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        next_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        llvm::Value* llvm_zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 0));
        LLVM::CreateStore(*builder, llvm_zero, copy_itr);
        LLVM::CreateStore(*builder, src_capacity, next_ptr);

        llvm::Value* src_key_value_pairs = llvm_utils->CreateLoad2(
            get_key_value_pair_type(dict_type->m_key_type, dict_type->m_value_type)->getPointerTo(),
            get_pointer_to_key_value_pairs(src)
        );
        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpSGT(
                                        src_capacity,
                                        llvm_utils->CreateLoad2(
                                            llvm::Type::getInt32Ty(context),
                                            copy_itr));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* itr = llvm_utils->CreateLoad2(
                llvm::Type::getInt32Ty(context), copy_itr);
            llvm::Value* key_mask_value = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context),
                llvm_utils->create_ptr_gep2(
                    llvm::Type::getInt8Ty(context), src_key_mask, itr));
            LLVM::CreateStore(*builder, key_mask_value,
                llvm_utils->create_ptr_gep(dest_key_mask, itr));
            llvm::Value* is_key_set = builder->CreateICmpEQ(key_mask_value,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));

            llvm_utils->create_if_else(is_key_set, [&]() {
                llvm::Value* srci = llvm_utils->create_ptr_gep(src_key_value_pairs, itr);
                llvm::Value* desti = llvm_utils->create_ptr_gep(dest_key_value_pairs, itr);
                deepcopy_key_value_pair_linked_list(src_expr, srci, desti, dest_key_value_pairs,
                    dict_type, module, name2memidx);
            }, [=]() {
            });
            llvm::Value* tmp = builder->CreateAdd(
                        itr,
                        llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, tmp, copy_itr);
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);
        LLVM::CreateStore(*builder, dest_key_value_pairs, get_pointer_to_key_value_pairs(dest));
    }

    void LLVMList::check_index_within_bounds_using_type(llvm::Type* list_type, llvm::Value* list,
                                    llvm::Value* pos, llvm::Module* module) {
        llvm::Value* end_point = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_current_end_point_using_type(list_type, list));
        llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                   llvm::APInt(32, 0));

        llvm::Value* cond = builder->CreateOr(
                                builder->CreateICmpSGE(pos, end_point),
                                builder->CreateICmpSLT(pos, zero));
        llvm_utils->create_if_else(cond, [&]() {
            std::string index_error = "IndexError: %s%d%s%d\n",
            message1 = "List index is out of range. Index range is (0, ",
            message2 = "), but the given index is ";
            llvm::Value *fmt_ptr = builder->CreateGlobalStringPtr(index_error);
            llvm::Value *fmt_ptr1 = builder->CreateGlobalStringPtr(message1);
            llvm::Value *fmt_ptr2 = builder->CreateGlobalStringPtr(message2);
            llvm::Value *end_minus_one = builder->CreateSub(end_point,
                llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            print_error(context, *module, *builder, {fmt_ptr, fmt_ptr1,
                end_minus_one, fmt_ptr2, pos});
            int exit_code_int = 1;
            llvm::Value *exit_code = llvm::ConstantInt::get(context,
                    llvm::APInt(32, exit_code_int));
            exit(context, *module, *builder, exit_code);
        }, [=]() {
        });
    }

    void LLVMList::write_item(ASR::expr_t* expr, llvm::Value* list, llvm::Value* pos,
                              llvm::Value* item, ASR::ttype_t* asr_type,
                              bool enable_bounds_checking, llvm::Module* module,
                              std::map<std::string, std::map<std::string, int>>& name2memidx) {
        std::string el_type_code = ASRUtils::get_type_code(ASRUtils::extract_type(asr_type));
        llvm::Type* list_type = llvm_utils->list_api->get_list_type(nullptr, el_type_code, 0);
        if( enable_bounds_checking ) {
            check_index_within_bounds_using_type(list_type, list, pos, module);
        }
        llvm::Type* t_ = llvm_utils->get_type_from_ttype_t_util(expr,
            ASRUtils::extract_type(asr_type), module);
        llvm::Value* list_data = llvm_utils->CreateLoad2(t_->getPointerTo(), get_pointer_to_list_data_using_type(list_type, list));
        llvm::Value* element_ptr = llvm_utils->create_ptr_gep2(t_, list_data, pos);
        llvm_utils->deepcopy(expr, item, element_ptr, asr_type, asr_type, module, name2memidx);
    }

    void LLVMList::write_item_using_ttype(ASR::ttype_t* el_asr_type, llvm::Value* list, llvm::Value* pos,
                              llvm::Value* item, bool enable_bounds_checking,
                              llvm::Module* module) {
        std::string type_code = ASRUtils::get_type_code(el_asr_type);
        llvm::Type* list_element_type = std::get<2>(typecode2listtype[type_code]);
        llvm::Type* list_type = std::get<0>(typecode2listtype[type_code]);

        if( enable_bounds_checking ) {
            check_index_within_bounds_using_type(list_type, list, pos, module);
        }
        
        llvm::Value* list_data = llvm_utils->CreateLoad2(list_element_type->getPointerTo(),
                                                         get_pointer_to_list_data_using_type(list_type, list));
        llvm::Value* element_ptr = llvm_utils->create_ptr_gep2(list_element_type, list_data, pos);
        LLVM::CreateStore(*builder, item, element_ptr);
    }

    llvm::Value* LLVMDict::get_pointer_to_keymask(llvm::Value* dict) {
        return llvm_utils->create_gep(dict, 3);
    }

    llvm::Value* LLVMDictSeparateChaining::get_pointer_to_keymask(llvm::Value* dict) {
        return llvm_utils->create_gep(dict, 4);
    }

    void LLVMDict::resolve_collision([[maybe_unused]]ASR::expr_t* dict_expr,
        llvm::Value* capacity, llvm::Value* key_hash,
        llvm::Value* key, llvm::Value* key_list,
        llvm::Value* key_mask, llvm::Module* module,
        ASR::ttype_t* key_asr_type, bool for_read) {
        pos_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        is_key_matching_var = llvm_utils->CreateAlloca(llvm::Type::getInt1Ty(context));
        LLVM::CreateStore(*builder, key_hash, pos_ptr);
        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);


        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");


        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value* pos = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), pos_ptr);
            llvm::Value* key_mask_value = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context),
                llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context)->getPointerTo(), key_mask, pos));
            llvm::Value* is_key_skip = builder->CreateICmpEQ(key_mask_value,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 3)));
            llvm::Value* is_key_set = builder->CreateICmpNE(key_mask_value,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0)));
            llvm::Value* is_key_matching = llvm::ConstantInt::get(llvm::Type::getInt1Ty(context),
                                                                  llvm::APInt(1, 0));
            LLVM::CreateStore(*builder, is_key_matching, is_key_matching_var);
            llvm::Value* compare_keys = builder->CreateAnd(is_key_set,
                                            builder->CreateNot(is_key_skip));
            llvm_utils->create_if_else(compare_keys, [&]() {
                llvm::Value* original_key = llvm_utils->list_api->read_item_using_ttype(key_asr_type, key_list, pos,
                                    false, module, LLVM::is_llvm_struct(key_asr_type));
                is_key_matching = llvm_utils->is_equal_by_value(key, original_key, module,
                                                                key_asr_type);
                LLVM::CreateStore(*builder, is_key_matching, is_key_matching_var);
            }, [=]() {
            });

            // TODO: Allow safe exit if pos becomes key_hash again.
            // Ideally should not happen as dict will be resized once
            // load factor touches a threshold (which will always be less than 1)
            // so there will be some key which will not be set. However for safety
            // we can add an exit from the loop with a error message.
            llvm::Value *cond = nullptr;
            if( for_read ) {
                cond = builder->CreateAnd(is_key_set, builder->CreateNot(
                            llvm_utils->CreateLoad2(
                                llvm::Type::getInt1Ty(context), is_key_matching_var))
                );
                cond = builder->CreateOr(is_key_skip, cond);
            } else {
                cond = builder->CreateAnd(is_key_set, builder->CreateNot(is_key_skip));
                cond = builder->CreateAnd(cond, builder->CreateNot(
                            llvm_utils->CreateLoad2(
                                llvm::Type::getInt1Ty(context),
                                is_key_matching_var))
                );
            }
            builder->CreateCondBr(cond, loopbody, loopend);
        }


        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* pos = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), pos_ptr);
            pos = builder->CreateAdd(pos, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                                 llvm::APInt(32, 1)));
            pos = builder->CreateSRem(pos, capacity);
            LLVM::CreateStore(*builder, pos, pos_ptr);
        }


        builder->CreateBr(loophead);


        // end
        llvm_utils->start_new_block(loopend);
    }

    void LLVMDictOptimizedLinearProbing::resolve_collision([[maybe_unused]]ASR::expr_t* dict_expr,
        llvm::Value* capacity, llvm::Value* key_hash,
        llvm::Value* key, llvm::Value* key_list,
        llvm::Value* key_mask, llvm::Module* module,
        ASR::ttype_t* key_asr_type, bool for_read) {

        /**
         * C++ equivalent:
         *
         * pos = key_hash;
         *
         * while( true ) {
         *     is_key_skip = key_mask_value == 3;     // tombstone
         *     is_key_set = key_mask_value != 0;
         *     is_key_matching = 0;
         *
         *     compare_keys = is_key_set && !is_key_skip;
         *     if( compare_keys ) {
         *         original_key = key_list[pos];
         *         is_key_matching = key == original_key;
         *     }
         *
         *     cond;
         *     if( for_read ) {
         *         // for reading, continue to next pos
         *         // even if current pos is tombstone
         *         cond = (is_key_set && !is_key_matching) || is_key_skip;
         *     }
         *     else {
         *         // for writing, do not continue
         *         // if current pos is tombstone
         *         cond = is_key_set && !is_key_matching && !is_key_skip;
         *     }
         *
         *     if( cond ) {
         *         pos += 1;
         *         pos %= capacity;
         *     }
         *     else {
         *         break;
         *     }
         * }
         *
         */
        if( !for_read ) {
            pos_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        }
        is_key_matching_var = llvm_utils->CreateAlloca(llvm::Type::getInt1Ty(context));

        LLVM::CreateStore(*builder, key_hash, pos_ptr);
        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);

        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value* pos = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), pos_ptr);
            llvm::Value* key_mask_value = llvm_utils->CreateLoad2(
                llvm::Type::getInt8Ty(context),
                llvm_utils->create_ptr_gep2(
                    llvm::Type::getInt8Ty(context), key_mask, pos));
            llvm::Value* is_key_skip = builder->CreateICmpEQ(key_mask_value,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 3)));
            llvm::Value* is_key_set = builder->CreateICmpNE(key_mask_value,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0)));
            llvm::Value* is_key_matching = llvm::ConstantInt::get(llvm::Type::getInt1Ty(context),
                                                                llvm::APInt(1, 0));
            LLVM::CreateStore(*builder, is_key_matching, is_key_matching_var);
            llvm::Value* compare_keys = builder->CreateAnd(is_key_set,
                                            builder->CreateNot(is_key_skip));
            llvm_utils->create_if_else(compare_keys, [&]() {
                llvm::Value* original_key = llvm_utils->list_api->read_item_using_ttype(key_asr_type, key_list, pos,
                                false, module, LLVM::is_llvm_struct(key_asr_type));
                is_key_matching = llvm_utils->is_equal_by_value(key, original_key, module,
                                                                key_asr_type);
                LLVM::CreateStore(*builder, is_key_matching, is_key_matching_var);
            }, [=]() {
            });
            // TODO: Allow safe exit if pos becomes key_hash again.
            // Ideally should not happen as dict will be resized once
            // load factor touches a threshold (which will always be less than 1)
            // so there will be some key which will not be set. However for safety
            // we can add an exit from the loop with a error message.
            llvm::Value *cond = nullptr;
            if( for_read ) {
                cond = builder->CreateAnd(is_key_set, builder->CreateNot(
                            llvm_utils->CreateLoad2(
                                llvm::Type::getInt1Ty(context),
                                is_key_matching_var))
                        );
                cond = builder->CreateOr(is_key_skip, cond);
            } else {
                cond = builder->CreateAnd(is_key_set, builder->CreateNot(is_key_skip));
                cond = builder->CreateAnd(cond, builder->CreateNot(
                            llvm_utils->CreateLoad2(
                                llvm::Type::getInt1Ty(context),
                                is_key_matching_var))
                        );
            }
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* pos = llvm_utils->CreateLoad2(
                llvm::Type::getInt32Ty(context), pos_ptr);
            pos = builder->CreateAdd(pos, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                                llvm::APInt(32, 1)));
            pos = builder->CreateSRem(pos, capacity);
            LLVM::CreateStore(*builder, pos, pos_ptr);
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);
    }

    void LLVMDictSeparateChaining::resolve_collision(ASR::expr_t* dict_expr,
        llvm::Value* /*capacity*/, llvm::Value* key_hash,
        llvm::Value* key, llvm::Value* key_value_pair_linked_list,
        llvm::Type* kv_pair_type, llvm::Value* key_mask,
        llvm::Module* module, ASR::ttype_t* key_asr_type) {
        /**
         * C++ equivalent:
         *
         * chain_itr_prev = nullptr;
         *
         * ll_exists = key_mask_value == 1;
         * if( ll_exists ) {
         *     chain_itr = ll_head;
         * }
         * else {
         *     chain_itr = nullptr;
         * }
         * is_key_matching = 0;
         *
         * while( chain_itr != nullptr && !is_key_matching ) {
         *     is_key_matching = (key == kv_struct_key);
         *     if( !is_key_matching ) {
         *         // update for next iteration
         *         chain_itr_prev = chain_itr;
         *         chain_itr = next_kv_struct;  // (*chain_itr)[2]
         *     }
         * }
         *
         * // now, chain_itr either points to kv or is nullptr
         *
         */
        chain_itr = llvm_utils->CreateAlloca(llvm::Type::getInt8Ty(context)->getPointerTo());
        chain_itr_prev = llvm_utils->CreateAlloca(llvm::Type::getInt8Ty(context)->getPointerTo());
        is_key_matching_var = llvm_utils->CreateAlloca(llvm::Type::getInt1Ty(context));

        LLVM::CreateStore(*builder,
                llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()), chain_itr_prev);
        llvm::Value* key_mask_value = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context),
            llvm_utils->create_ptr_gep2(
                llvm::Type::getInt8Ty(context),
                key_mask, key_hash));
        llvm_utils->create_if_else(builder->CreateICmpEQ(key_mask_value,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1))), [&]() {
            llvm::Value* kv_ll_i8 = builder->CreateBitCast(key_value_pair_linked_list,
                                                            llvm::Type::getInt8Ty(context)->getPointerTo());
            LLVM::CreateStore(*builder, kv_ll_i8, chain_itr);
        }, [&]() {
            LLVM::CreateStore(*builder,
                    llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()), chain_itr);
        });
        LLVM::CreateStore(*builder,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(1, 0)),
            is_key_matching_var
        );
        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpNE(
                llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context)->getPointerTo(), chain_itr),
                llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo())
            );
            cond = builder->CreateAnd(cond, builder->CreateNot(
                    llvm_utils->CreateLoad2(llvm::Type::getInt1Ty(context), is_key_matching_var)));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* kv_struct_i8 = llvm_utils->CreateLoad2(
                llvm::Type::getInt8Ty(context)->getPointerTo(), chain_itr);
            llvm::Value* kv_struct = builder->CreateBitCast(kv_struct_i8, kv_pair_type->getPointerTo());
            llvm::Type* llvm_key_type = llvm_utils->get_type_from_ttype_t_util(dict_expr, key_asr_type, module);
            llvm::Value* kv_struct_key = llvm_utils->create_gep2(kv_pair_type, kv_struct, 0);
            if( !LLVM::is_llvm_struct(key_asr_type) ) {
                kv_struct_key = llvm_utils->CreateLoad2(llvm_key_type, kv_struct_key);
            }
            LLVM::CreateStore(*builder, llvm_utils->is_equal_by_value(key, kv_struct_key,
                                module, key_asr_type), is_key_matching_var);
            llvm_utils->create_if_else(builder->CreateNot(llvm_utils->CreateLoad2(
                llvm::Type::getInt1Ty(context), is_key_matching_var)), [&]() {
                LLVM::CreateStore(*builder, kv_struct_i8, chain_itr_prev);
                llvm::Value* next_kv_struct = llvm_utils->CreateLoad2(
                    llvm::Type::getInt8Ty(context)->getPointerTo(),
                    llvm_utils->create_gep2(
                        kv_pair_type, kv_struct, 2));
                LLVM::CreateStore(*builder, next_kv_struct, chain_itr);
            }, []() {});
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);
    }

    void LLVMDict::resolve_collision_for_write(
        ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
        llvm::Value* key, llvm::Value* value,
        llvm::Module* module, ASR::ttype_t* key_asr_type,
        ASR::ttype_t* value_asr_type,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        llvm::Value* key_list = get_key_list(dict);
        llvm::Value* value_list = get_value_list(dict);

        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);
        std::string value_type_code = ASRUtils::get_type_code(value_asr_type);

        llvm::Value* key_mask = llvm_utils->CreateLoad2(
            llvm::Type::getInt8Ty(context)->getPointerTo(), get_pointer_to_keymask(dict));
        llvm::Value* capacity = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, dict));
        this->resolve_collision(nullptr, capacity, key_hash, key, key_list, key_mask, module, key_asr_type);
        llvm::Value* pos = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), pos_ptr);
        llvm_utils->list_api->write_item(dict_expr, key_list, pos, key,
                                         key_asr_type, false, module, name2memidx);
        llvm_utils->list_api->write_item(dict_expr, value_list, pos, value,
                                         value_asr_type, false, module, name2memidx);
        llvm::Value* key_mask_value = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context),
            llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context)->getPointerTo(), key_mask, pos));
        llvm::Value* is_slot_empty = builder->CreateICmpEQ(key_mask_value,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0)));
        is_slot_empty = builder->CreateOr(is_slot_empty, builder->CreateICmpEQ(key_mask_value,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 3))));
        llvm::Value* occupancy_ptr = get_pointer_to_occupancy(dict);
        is_slot_empty = builder->CreateZExt(is_slot_empty, llvm::Type::getInt32Ty(context));
        llvm::Value* occupancy = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), occupancy_ptr);
        LLVM::CreateStore(*builder, builder->CreateAdd(occupancy, is_slot_empty),
                          occupancy_ptr);
        LLVM::CreateStore(*builder,
                          llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)),
                          llvm_utils->create_ptr_gep2(
                            llvm::Type::getInt8Ty(context)->getPointerTo(), key_mask, pos));
    }

    void LLVMDictOptimizedLinearProbing::resolve_collision_for_write(
        ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
        llvm::Value* key, llvm::Value* value,
        llvm::Module* module, ASR::ttype_t* key_asr_type,
        ASR::ttype_t* value_asr_type,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {

        /**
         * C++ equivalent:
         *
         * resolve_collision();     // modifies pos

         * key_list[pos] = key;
         * value_list[pos] = value;

         * key_mask_value = key_mask[pos];
         * is_slot_empty = key_mask_value == 0 || key_mask_value == 3;
         * occupancy += is_slot_empty;

         * linear_prob_happened = (key_hash != pos) || (key_mask[key_hash] == 2);
         * set_max_2 = linear_prob_happened ? 2 : 1;
         * key_mask[key_hash] = set_max_2;
         * key_mask[pos] = set_max_2;
         *
         */

        llvm::Value* key_list = get_key_list(dict);
        llvm::Value* value_list = get_value_list(dict);

        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);
        std::string value_type_code = ASRUtils::get_type_code(value_asr_type);

        llvm::Value* key_mask = llvm_utils->CreateLoad2(
            llvm::Type::getInt8Ty(context)->getPointerTo(), get_pointer_to_keymask(dict));
        llvm::Value* capacity = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, dict));
        this->resolve_collision(dict_expr, capacity, key_hash, key, key_list, key_mask, module, key_asr_type);
        llvm::Value* pos = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), pos_ptr);
        llvm_utils->list_api->write_item(dict_expr, key_list, pos, key,
                                         key_asr_type, false, module, name2memidx);
        llvm_utils->list_api->write_item(dict_expr, value_list, pos, value,
                                         value_asr_type, false, module, name2memidx);

        llvm::Value* key_mask_value = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context),
            llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), key_mask, pos));
        llvm::Value* is_slot_empty = builder->CreateICmpEQ(key_mask_value,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0)));
        is_slot_empty = builder->CreateOr(is_slot_empty, builder->CreateICmpEQ(key_mask_value,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 3))));
        llvm::Value* occupancy_ptr = get_pointer_to_occupancy(dict);
        is_slot_empty = builder->CreateZExt(is_slot_empty, llvm::Type::getInt32Ty(context));
        llvm::Value* occupancy = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), occupancy_ptr);
        LLVM::CreateStore(*builder, builder->CreateAdd(occupancy, is_slot_empty),
                          occupancy_ptr);

        llvm::Value* linear_prob_happened = builder->CreateICmpNE(key_hash, pos);
        linear_prob_happened = builder->CreateOr(linear_prob_happened,
            builder->CreateICmpEQ(
                llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context),
                    llvm_utils->create_ptr_gep2(
                        llvm::Type::getInt8Ty(context), key_mask, key_hash)),
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 2)
            ))
        );
        llvm::Value* set_max_2 = builder->CreateSelect(linear_prob_happened,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 2)),
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));
        LLVM::CreateStore(*builder, set_max_2, llvm_utils->create_ptr_gep2(
            llvm::Type::getInt8Ty(context), key_mask, key_hash));
        LLVM::CreateStore(*builder, set_max_2, llvm_utils->create_ptr_gep2(
            llvm::Type::getInt8Ty(context), key_mask, pos));
    }

    void LLVMDictSeparateChaining::resolve_collision_for_write(
        ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
        llvm::Value* key, llvm::Value* value,
        llvm::Module* module, ASR::ttype_t* key_asr_type,
        ASR::ttype_t* value_asr_type,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {

        /**
         * C++ equivalent:
         *
         * kv_linked_list = key_value_pairs[key_hash];
         * resolve_collision(key);   // modifies chain_itr
         * do_insert = chain_itr == nullptr;
         *
         * if( do_insert ) {
         *     if( chain_itr_prev != nullptr ) {
         *         new_kv_struct = malloc(kv_struct_size);
         *         new_kv_struct[0] = key;
         *         new_kv_struct[1] = value;
         *         new_kv_struct[2] = nullptr;
         *         chain_itr_prev[2] = new_kv_struct;
         *     }
         *     else {
         *         kv_linked_list[0] = key;
         *         kv_linked_list[1] = value;
         *         kv_linked_list[2] = nullptr;
         *     }
         *     occupancy += 1;
         * }
         * else {
         *     kv_struct[0] = key;
         *     kv_struct[1] = value;
         * }
         *
         * buckets_filled_delta = key_mask[key_hash] == 0;
         * buckets_filled += buckets_filled_delta;
         * key_mask[key_hash] = 1;
         *
         */

        llvm::Value* capacity = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_capacity(dict));
        llvm::Type* kv_pair_type = get_key_value_pair_type(key_asr_type, value_asr_type);
        llvm::Value* key_value_pairs = llvm_utils->CreateLoad2(kv_pair_type->getPointerTo(), get_pointer_to_key_value_pairs(dict));
        llvm::Value* key_value_pair_linked_list = llvm_utils->create_ptr_gep2(
            kv_pair_type, key_value_pairs, key_hash);
        llvm::Value* key_mask = llvm_utils->CreateLoad2(
            llvm::Type::getInt8Ty(context)->getPointerTo(), get_pointer_to_keymask(dict));
        llvm::Type* kv_struct_type = get_key_value_pair_type(key_asr_type, value_asr_type);
        this->resolve_collision(dict_expr, capacity, key_hash, key, key_value_pair_linked_list,
                                kv_struct_type, key_mask, module, key_asr_type);
        llvm::Value* kv_struct_i8 = llvm_utils->CreateLoad2(
            llvm::Type::getInt8Ty(context)->getPointerTo(), chain_itr);

        llvm::Function *fn = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", fn);
        llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context, "else");
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifcont");
        llvm::Value* do_insert = builder->CreateICmpEQ(kv_struct_i8,
            llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()));
        builder->CreateCondBr(do_insert, thenBB, elseBB);

        builder->SetInsertPoint(thenBB);
        {
            llvm_utils->create_if_else(builder->CreateICmpNE(
                    llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context)->getPointerTo(), chain_itr_prev),
                    llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo())), [&]() {
                llvm::DataLayout data_layout(module->getDataLayout());
                size_t kv_struct_size = data_layout.getTypeAllocSize(kv_struct_type);
                llvm::Value* malloc_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), kv_struct_size);
                llvm::Value* new_kv_struct_i8 = LLVM::lfortran_malloc(context, *module, *builder, malloc_size);
                llvm::Value* new_kv_struct = builder->CreateBitCast(new_kv_struct_i8, kv_struct_type->getPointerTo());
                llvm_utils->deepcopy(dict_expr, key, llvm_utils->create_gep2(kv_pair_type, new_kv_struct, 0), key_asr_type, key_asr_type, module, name2memidx);
                llvm_utils->deepcopy(dict_expr, value, llvm_utils->create_gep2(kv_pair_type, new_kv_struct, 1), value_asr_type, value_asr_type, module, name2memidx);
                LLVM::CreateStore(*builder,
                    llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()),
                    llvm_utils->create_gep2(kv_pair_type, new_kv_struct, 2));
                llvm::Value* kv_struct_prev_i8 = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context)->getPointerTo(), chain_itr_prev);
                llvm::Value* kv_struct_prev = builder->CreateBitCast(kv_struct_prev_i8, kv_struct_type->getPointerTo());
                LLVM::CreateStore(*builder, new_kv_struct_i8, llvm_utils->create_gep2(
                    kv_pair_type, kv_struct_prev, 2));
            }, [&]() {
                llvm_utils->deepcopy(dict_expr, key, llvm_utils->create_gep2(kv_pair_type, key_value_pair_linked_list, 0), key_asr_type, key_asr_type,module, name2memidx);
                llvm_utils->deepcopy(dict_expr, value, llvm_utils->create_gep2(kv_pair_type, key_value_pair_linked_list, 1), value_asr_type, key_asr_type, module, name2memidx);
                LLVM::CreateStore(*builder,
                    llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()),
                    llvm_utils->create_gep2(
                        kv_pair_type, key_value_pair_linked_list, 2));
            });

            llvm::Value* occupancy_ptr = get_pointer_to_occupancy(dict);
            llvm::Value* occupancy = llvm_utils->CreateLoad2(
                llvm::Type::getInt32Ty(context), occupancy_ptr);
            occupancy = builder->CreateAdd(occupancy,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 1));
            LLVM::CreateStore(*builder, occupancy, occupancy_ptr);
        }
        builder->CreateBr(mergeBB);
        llvm_utils->start_new_block(elseBB);
        {
            llvm::Value* kv_struct = builder->CreateBitCast(kv_struct_i8, kv_struct_type->getPointerTo());
            llvm_utils->deepcopy(dict_expr, key, llvm_utils->create_gep2(kv_pair_type, kv_struct, 0), key_asr_type, key_asr_type, module, name2memidx);
            llvm_utils->deepcopy(dict_expr, value, llvm_utils->create_gep2(kv_pair_type, kv_struct, 1), value_asr_type, value_asr_type, module, name2memidx);
        }
        llvm_utils->start_new_block(mergeBB);
        llvm::Value* buckets_filled_ptr = get_pointer_to_number_of_filled_buckets(dict);
        llvm::Value* key_mask_value_ptr = llvm_utils->create_ptr_gep2(
            llvm::Type::getInt8Ty(context), key_mask, key_hash);
        llvm::Value* key_mask_value = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context), key_mask_value_ptr);
        llvm::Value* buckets_filled_delta = builder->CreateICmpEQ(key_mask_value,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0)));
        llvm::Value* buckets_filled = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), buckets_filled_ptr);
        buckets_filled = builder->CreateAdd(
            buckets_filled,
            builder->CreateZExt(buckets_filled_delta, llvm::Type::getInt32Ty(context))
        );
        LLVM::CreateStore(*builder, buckets_filled, buckets_filled_ptr);
        LLVM::CreateStore(*builder,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)),
            key_mask_value_ptr);
    }

    llvm::Value* LLVMDict::resolve_collision_for_read([[maybe_unused]] ASR::expr_t* dict_expr,
        llvm::Value* dict, llvm::Value* key_hash,
        llvm::Value* key, llvm::Module* module,
        ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type) {
        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);
        std::string value_type_code = ASRUtils::get_type_code(value_asr_type);

        llvm::Value* key_list = get_key_list(dict);
        llvm::Value* value_list = get_value_list(dict);
        llvm::Value* key_mask = llvm_utils->CreateLoad2(
            llvm::Type::getInt8Ty(context)->getPointerTo(), get_pointer_to_keymask(dict));
        llvm::Value* capacity = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, dict));
        this->resolve_collision(nullptr, capacity, key_hash, key, key_list, key_mask, module, key_asr_type, true);
        llvm::Value* pos = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), pos_ptr);
        llvm::Value* item = llvm_utils->list_api->read_item_using_ttype(value_asr_type, value_list, pos, false, module, true);
        return item;
    }

    llvm::Value* LLVMDict::resolve_collision_for_read_with_bound_check([[maybe_unused]] ASR::expr_t* dict_expr,
        llvm::Value* dict, llvm::Value* key_hash,
        llvm::Value* key, llvm::Module* module,
        ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type) {
        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);
        std::string value_type_code = ASRUtils::get_type_code(value_asr_type);
        llvm::Value* key_list = get_key_list(dict);
        llvm::Value* value_list = get_value_list(dict);
        llvm::Value* key_mask = llvm_utils->CreateLoad2(
            llvm::Type::getInt8Ty(context)->getPointerTo(), get_pointer_to_keymask(dict));
        llvm::Value* capacity = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, dict));
        this->resolve_collision(nullptr, capacity, key_hash, key, key_list, key_mask, module, key_asr_type, true);
        llvm::Value* pos = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), pos_ptr);
        llvm::Value* is_key_matching = llvm_utils->is_equal_by_value(key,
            llvm_utils->list_api->read_item_using_ttype(key_asr_type, key_list, pos, false, module,
                LLVM::is_llvm_struct(key_asr_type)), module, key_asr_type);

        llvm_utils->create_if_else(is_key_matching, [&]() {
        }, [&]() {
            std::string message = "The dict does not contain the specified key";
            llvm::Value *fmt_ptr = builder->CreateGlobalStringPtr("KeyError: %s\n");
            llvm::Value *fmt_ptr2 = builder->CreateGlobalStringPtr(message);
            print_error(context, *module, *builder, {fmt_ptr, fmt_ptr2});
            int exit_code_int = 1;
            llvm::Value *exit_code = llvm::ConstantInt::get(context,
                    llvm::APInt(32, exit_code_int));
            exit(context, *module, *builder, exit_code);
        });
        llvm::Value* item = llvm_utils->list_api->read_item_using_ttype(value_asr_type, value_list, pos,
                                                false, module, false);
        return item;
    }

    void LLVMDict::_check_key_present_or_default(ASR::expr_t* dict_expr, llvm::Module* module, llvm::Value *key, llvm::Value *key_list,
        ASR::ttype_t* key_asr_type, llvm::Value *value_list, ASR::ttype_t* value_asr_type, llvm::Value *pos,
        llvm::Value *def_value, llvm::Value* &result) {
        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);
        std::string value_type_code = ASRUtils::get_type_code(value_asr_type);
        llvm::Value* is_key_matching = llvm_utils->is_equal_by_value(key,
            llvm_utils->list_api->read_item_using_ttype(key_asr_type, key_list, pos, false, module,
                LLVM::is_llvm_struct(key_asr_type)), module, key_asr_type);
        llvm_utils->create_if_else(is_key_matching, [&]() {
            llvm::Value* item = llvm_utils->list_api->read_item_using_ttype(value_asr_type, value_list, pos,
                                                false, module, false);
            LLVM::CreateStore(*builder, item, result);
        }, [=]() {
            llvm::Type* llvm_value_type = llvm_utils->get_type_from_ttype_t_util(dict_expr,
                value_asr_type, module);
            LLVM::CreateStore(*builder, llvm_utils->CreateLoad2(llvm_value_type, def_value), result);
        });
    }

    llvm::Value* LLVMDict::resolve_collision_for_read_with_default(ASR::expr_t* dict_expr,
        llvm::Value* dict, llvm::Value* key_hash,
        llvm::Value* key, llvm::Module* module,
        ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type,
        llvm::Value* def_value) {
        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);
        std::string value_type_code = ASRUtils::get_type_code(value_asr_type);
        llvm::Value* key_list = get_key_list(dict);
        llvm::Value* value_list = get_value_list(dict);
        llvm::Value* key_mask = llvm_utils->CreateLoad2(
            llvm::Type::getInt8Ty(context)->getPointerTo(), get_pointer_to_keymask(dict));
        llvm::Value* capacity = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, dict));
        this->resolve_collision(nullptr, capacity, key_hash, key, key_list, key_mask, module, key_asr_type, true);
        llvm::Value* pos = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), pos_ptr);
        std::pair<std::string, std::string> llvm_key = std::make_pair(
            ASRUtils::get_type_code(key_asr_type),
            ASRUtils::get_type_code(value_asr_type)
        );
        llvm::Type* value_type = std::get<2>(typecode2dicttype[llvm_key]).second;
        llvm::Value* result = llvm_utils->CreateAlloca(value_type);
        _check_key_present_or_default(dict_expr, module, key, key_list, key_asr_type, value_list,
                                        value_asr_type, pos, def_value, result);
        return result;
    }

    llvm::Value* LLVMDictOptimizedLinearProbing::resolve_collision_for_read_with_bound_check(ASR::expr_t* dict_expr,
        llvm::Value* dict, llvm::Value* key_hash,
        llvm::Value* key, llvm::Module* module,
        ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type) {

        /**
         * C++ equivalent:
         *
         * key_mask_value = key_mask[key_hash];
         * is_prob_not_needed = key_mask_value == 1;
         * if( is_prob_not_needed ) {
         *     is_key_matching = key == key_list[key_hash];
         *     if( is_key_matching ) {
         *         pos = key_hash;
         *     }
         *     else {
         *         exit(1); // key not present
         *     }
         * }
         * else {
         *     resolve_collision(key, for_read=true);  // modifies pos
         * }
         *
         * is_key_matching = key == key_list[pos];
         * if( !is_key_matching ) {
         *     exit(1); // key not present
         * }
         *
         * return value_list[pos];
         */


        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);
        std::string value_type_code = ASRUtils::get_type_code(value_asr_type);
        llvm::Value* key_list = get_key_list(dict);
        llvm::Value* value_list = get_value_list(dict);
        llvm::Value* key_mask = llvm_utils->CreateLoad2(
            llvm::Type::getInt8Ty(context)->getPointerTo(), get_pointer_to_keymask(dict));
        llvm::Value* capacity = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, dict));
        pos_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        llvm::Function *fn = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", fn);
        llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context, "else");
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifcont");
        llvm::Value* key_mask_value = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context),
            llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), key_mask, key_hash));
        llvm::Value* is_prob_not_neeeded = builder->CreateICmpEQ(key_mask_value,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));
        builder->CreateCondBr(is_prob_not_neeeded, thenBB, elseBB);
        builder->SetInsertPoint(thenBB);
        {
            // A single by value comparison is needed even though
            // we don't need to do linear probing. This is because
            // the user can provide a key which is absent in the dict
            // but is giving the same hash value as one of the keys present in the dict.
            // In the above case we will end up returning value for a key
            // which is not present in the dict. Instead we should return an error
            // which is done in the below code.
            llvm::Value* is_key_matching = llvm_utils->is_equal_by_value(key,
                llvm_utils->list_api->read_item_using_ttype(key_asr_type, key_list, key_hash, false, module,
                    LLVM::is_llvm_struct(key_asr_type)), module, key_asr_type);

            llvm_utils->create_if_else(is_key_matching, [=]() {
                LLVM::CreateStore(*builder, key_hash, pos_ptr);
            }, [&]() {
                std::string message = "The dict does not contain the specified key";
                llvm::Value *fmt_ptr = builder->CreateGlobalStringPtr("KeyError: %s\n");
                llvm::Value *fmt_ptr2 = builder->CreateGlobalStringPtr(message);
                print_error(context, *module, *builder, {fmt_ptr, fmt_ptr2});
                int exit_code_int = 1;
                llvm::Value *exit_code = llvm::ConstantInt::get(context,
                        llvm::APInt(32, exit_code_int));
                exit(context, *module, *builder, exit_code);
            });
        }
        builder->CreateBr(mergeBB);
        llvm_utils->start_new_block(elseBB);
        {
            this->resolve_collision(dict_expr, capacity, key_hash, key, key_list, key_mask,
                           module, key_asr_type, true);
        }
        llvm_utils->start_new_block(mergeBB);
        llvm::Value* pos = llvm_utils->CreateLoad(pos_ptr);
        // Check if the actual key is present or not
        llvm::Value* is_key_matching = llvm_utils->is_equal_by_value(key,
                llvm_utils->list_api->read_item_using_ttype(key_asr_type, key_list, pos, false, module,
                    LLVM::is_llvm_struct(key_asr_type)), module, key_asr_type);

        llvm_utils->create_if_else(is_key_matching, [&]() {
        }, [&]() {
            std::string message = "The dict does not contain the specified key";
            llvm::Value *fmt_ptr = builder->CreateGlobalStringPtr("KeyError: %s\n");
            llvm::Value *fmt_ptr2 = builder->CreateGlobalStringPtr(message);
            print_error(context, *module, *builder, {fmt_ptr, fmt_ptr2});
            int exit_code_int = 1;
            llvm::Value *exit_code = llvm::ConstantInt::get(context,
                    llvm::APInt(32, exit_code_int));
            exit(context, *module, *builder, exit_code);
        });
        llvm::Value* item = llvm_utils->list_api->read_item_using_ttype(value_asr_type, value_list, pos,
                                                        false, module, true);
        return item;
    }

    llvm::Value* LLVMDictOptimizedLinearProbing::resolve_collision_for_read(ASR::expr_t* dict_expr,
        llvm::Value* dict, llvm::Value* key_hash,
        llvm::Value* key, llvm::Module* module,
        ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type) {
        
        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);
        std::string value_type_code = ASRUtils::get_type_code(value_asr_type);
        llvm::Value* key_list = get_key_list(dict);
        llvm::Value* value_list = get_value_list(dict);
        llvm::Value* key_mask = llvm_utils->CreateLoad2(
            llvm::Type::getInt8Ty(context)->getPointerTo(), get_pointer_to_keymask(dict));
        llvm::Value* capacity = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, dict));
        pos_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        llvm::Function *fn = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", fn);
        llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context, "else");
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifcont");
        llvm::Value* key_mask_value = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context),
            llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), key_mask, key_hash));
        llvm::Value* is_prob_not_neeeded = builder->CreateICmpEQ(key_mask_value,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));
        builder->CreateCondBr(is_prob_not_neeeded, thenBB, elseBB);
        builder->SetInsertPoint(thenBB);
        {
            // A single by value comparison is needed even though
            // we don't need to do linear probing. This is because
            // the user can provide a key which is absent in the dict
            // but is giving the same hash value as one of the keys present in the dict.
            // In the above case we will end up returning value for a key
            // which is not present in the dict. Instead we should return an error
            // which is done in the below code.
            llvm::Value* is_key_matching = llvm_utils->is_equal_by_value(key,
                llvm_utils->list_api->read_item_using_ttype(key_asr_type, key_list, key_hash, false, module,
                    LLVM::is_llvm_struct(key_asr_type)), module, key_asr_type);

            llvm_utils->create_if_else(is_key_matching, [=]() {
                LLVM::CreateStore(*builder, key_hash, pos_ptr);
            }, [=]() {
            });
        }
        builder->CreateBr(mergeBB);
        llvm_utils->start_new_block(elseBB);
        {
            this->resolve_collision(dict_expr, capacity, key_hash, key, key_list, key_mask,
                           module, key_asr_type, true);
        }
        llvm_utils->start_new_block(mergeBB);
        llvm::Value* pos = llvm_utils->CreateLoad(pos_ptr);
        llvm::Value* item = llvm_utils->list_api->read_item_using_ttype(value_asr_type, value_list, pos,
                                                        false, module, true);
        return item;
    }

    llvm::Value* LLVMDictOptimizedLinearProbing::resolve_collision_for_read_with_default(ASR::expr_t* dict_expr,
        llvm::Value* dict, llvm::Value* key_hash,
        llvm::Value* key, llvm::Module* module,
        ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type,
        llvm::Value *def_value) {
        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);
        std::string value_type_code = ASRUtils::get_type_code(value_asr_type);
        llvm::Value* key_list = get_key_list(dict);
        llvm::Value* value_list = get_value_list(dict);
        llvm::Value* key_mask = llvm_utils->CreateLoad(get_pointer_to_keymask(dict));
        llvm::Value* capacity = llvm_utils->CreateLoad(get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, dict));
        pos_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        std::pair<std::string, std::string> llvm_key = std::make_pair(
            ASRUtils::get_type_code(key_asr_type),
            ASRUtils::get_type_code(value_asr_type)
        );
        llvm::Type* value_type = std::get<2>(typecode2dicttype[llvm_key]).second;
        llvm::Value* result = llvm_utils->CreateAlloca(value_type);
        llvm::Function *fn = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", fn);
        llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context, "else");
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifcont");
        llvm::Value* key_mask_value = llvm_utils->CreateLoad(
                                        llvm_utils->create_ptr_gep(key_mask, key_hash));
        llvm::Value* is_prob_not_neeeded = builder->CreateICmpEQ(key_mask_value,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));
        builder->CreateCondBr(is_prob_not_neeeded, thenBB, elseBB);
        builder->SetInsertPoint(thenBB);
        {
            llvm::Value* is_key_matching = llvm_utils->is_equal_by_value(key,
                llvm_utils->list_api->read_item_using_ttype(key_asr_type, key_list, key_hash, false, module,
                    LLVM::is_llvm_struct(key_asr_type)), module, key_asr_type);
            llvm_utils->create_if_else(is_key_matching, [=]() {
                LLVM::CreateStore(*builder, key_hash, pos_ptr);
            }, [=]() {
                LLVM::CreateStore(*builder, llvm_utils->CreateLoad(def_value), result);
            });
        }
        builder->CreateBr(mergeBB);
        llvm_utils->start_new_block(elseBB);
        {
            this->resolve_collision(dict_expr, capacity, key_hash, key, key_list, key_mask,
                           module, key_asr_type, true);
        }
        llvm_utils->start_new_block(mergeBB);
        llvm::Value* pos = llvm_utils->CreateLoad(pos_ptr);
        _check_key_present_or_default(dict_expr, module, key, key_list, key_asr_type, value_list,
                                      value_asr_type, pos, def_value, result);
        return result;
    }

    llvm::Value* LLVMDictSeparateChaining::resolve_collision_for_read([[maybe_unused]] ASR::expr_t* dict_expr,
        llvm::Value* dict, llvm::Value* key_hash,
        llvm::Value* key, llvm::Module* module,
        ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type) {
        llvm::Type* kv_struct_type = get_key_value_pair_type(key_asr_type, value_asr_type);
        llvm::Value* capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), get_pointer_to_capacity(dict));
        llvm::Value* key_value_pairs = llvm_utils->CreateLoad2(kv_struct_type->getPointerTo(), get_pointer_to_key_value_pairs(dict));
        llvm::Value* key_value_pair_linked_list = llvm_utils->create_ptr_gep2(
            kv_struct_type, key_value_pairs, key_hash);
        llvm::Value* key_mask = llvm_utils->CreateLoad2(
            llvm::Type::getInt8Ty(context)->getPointerTo(), get_pointer_to_keymask(dict));

        this->resolve_collision(nullptr, capacity, key_hash, key, key_value_pair_linked_list,
                                kv_struct_type, key_mask, module, key_asr_type);
        std::pair<std::string, std::string> llvm_key = std::make_pair(
            ASRUtils::get_type_code(key_asr_type),
            ASRUtils::get_type_code(value_asr_type)
        );
        llvm::Type* value_type = std::get<2>(typecode2dicttype[llvm_key]).second;
        tmp_value_ptr = llvm_utils->CreateAlloca(value_type);
        llvm::Value* kv_struct_i8 = llvm_utils->CreateLoad(chain_itr);
        llvm::Value* kv_struct = builder->CreateBitCast(kv_struct_i8, kv_struct_type->getPointerTo());
        llvm::Value* value = llvm_utils->CreateLoad2(value_type, llvm_utils->create_gep2(kv_struct_type, kv_struct, 1));
        LLVM::CreateStore(*builder, value, tmp_value_ptr);
        return tmp_value_ptr;
    }

    llvm::Value* LLVMDictSeparateChaining::resolve_collision_for_read_with_bound_check([[maybe_unused]] ASR::expr_t* dict_expr,
        llvm::Value* dict, llvm::Value* key_hash,
        llvm::Value* key, llvm::Module* module,
        ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type) {
        /**
         * C++ equivalent:
         *
         * resolve_collision(key);   // modified chain_itr
         * does_kv_exist = key_mask[key_hash] == 1 && chain_itr != nullptr;
         * if( !does_key_exist ) {
         *     exit(1); // KeyError
         * }
         *
         */

        llvm::Type* kv_struct_type = get_key_value_pair_type(key_asr_type, value_asr_type);
        llvm::Value* capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), get_pointer_to_capacity(dict));
        llvm::Value* key_value_pairs = llvm_utils->CreateLoad2(kv_struct_type->getPointerTo(), get_pointer_to_key_value_pairs(dict));

        llvm::Value* key_value_pair_linked_list = llvm_utils->create_ptr_gep2(kv_struct_type, key_value_pairs, key_hash);
        llvm::Value* key_mask = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context)->getPointerTo(), get_pointer_to_keymask(dict));
        this->resolve_collision(nullptr, capacity, key_hash, key, key_value_pair_linked_list,
                                kv_struct_type, key_mask, module, key_asr_type);
        std::pair<std::string, std::string> llvm_key = std::make_pair(
            ASRUtils::get_type_code(key_asr_type),
            ASRUtils::get_type_code(value_asr_type)
        );
        llvm::Type* value_type = std::get<2>(typecode2dicttype[llvm_key]).second;
        tmp_value_ptr = llvm_utils->CreateAlloca(value_type);
        llvm::Value* key_mask_value = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context), 
            llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), key_mask, key_hash));
        llvm::Value* does_kv_exists = builder->CreateICmpEQ(key_mask_value,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));
        does_kv_exists = builder->CreateAnd(does_kv_exists,
            builder->CreateICmpNE(llvm_utils->CreateLoad(chain_itr),
            llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()))
        );

        llvm_utils->create_if_else(does_kv_exists, [&]() {
            llvm::Value* kv_struct_i8 = llvm_utils->CreateLoad(chain_itr);
            llvm::Value* kv_struct = builder->CreateBitCast(kv_struct_i8, kv_struct_type->getPointerTo());
            llvm::Value* value = llvm_utils->CreateLoad2(value_type, llvm_utils->create_gep2(kv_struct_type, kv_struct, 1));
            LLVM::CreateStore(*builder, value, tmp_value_ptr);
        }, [&]() {
            std::string message = "The dict does not contain the specified key";
            llvm::Value *fmt_ptr = builder->CreateGlobalStringPtr("KeyError: %s\n");
            llvm::Value *fmt_ptr2 = builder->CreateGlobalStringPtr(message);
            print_error(context, *module, *builder, {fmt_ptr, fmt_ptr2});
            int exit_code_int = 1;
            llvm::Value *exit_code = llvm::ConstantInt::get(context,
                    llvm::APInt(32, exit_code_int));
            exit(context, *module, *builder, exit_code);
        });
        return tmp_value_ptr;
    }

    llvm::Value* LLVMDictSeparateChaining::resolve_collision_for_read_with_default(ASR::expr_t* dict_expr,
        llvm::Value* dict, llvm::Value* key_hash,
        llvm::Value* key, llvm::Module* module,
        ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type, llvm::Value *def_value) {
        llvm::Value* capacity = llvm_utils->CreateLoad(get_pointer_to_capacity(dict));
        llvm::Value* key_value_pairs = llvm_utils->CreateLoad(get_pointer_to_key_value_pairs(dict));
        llvm::Value* key_value_pair_linked_list = llvm_utils->create_ptr_gep(key_value_pairs, key_hash);
        llvm::Value* key_mask = llvm_utils->CreateLoad(get_pointer_to_keymask(dict));
        llvm::Type* kv_struct_type = get_key_value_pair_type(key_asr_type, value_asr_type);
        this->resolve_collision(dict_expr, capacity, key_hash, key, key_value_pair_linked_list,
                                kv_struct_type, key_mask, module, key_asr_type);
        std::pair<std::string, std::string> llvm_key = std::make_pair(
            ASRUtils::get_type_code(key_asr_type),
            ASRUtils::get_type_code(value_asr_type)
        );
        llvm::Type* value_type = std::get<2>(typecode2dicttype[llvm_key]).second;
        tmp_value_ptr = llvm_utils->CreateAlloca(value_type);
        llvm::Value* key_mask_value = llvm_utils->CreateLoad(
            llvm_utils->create_ptr_gep(key_mask, key_hash));
        llvm::Value* does_kv_exists = builder->CreateICmpEQ(key_mask_value,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));
        does_kv_exists = builder->CreateAnd(does_kv_exists,
            builder->CreateICmpNE(llvm_utils->CreateLoad(chain_itr),
            llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()))
        );

        llvm_utils->create_if_else(does_kv_exists, [&]() {
            llvm::Value* kv_struct_i8 = llvm_utils->CreateLoad(chain_itr);
            llvm::Value* kv_struct = builder->CreateBitCast(kv_struct_i8, kv_struct_type->getPointerTo());
            llvm::Value* value = llvm_utils->CreateLoad(llvm_utils->create_gep(kv_struct, 1));
            LLVM::CreateStore(*builder, value, tmp_value_ptr);
        }, [&]() {
            LLVM::CreateStore(*builder, llvm_utils->CreateLoad(def_value), tmp_value_ptr);
        });
        return tmp_value_ptr;
    }

    llvm::Value* LLVMDictInterface::get_key_hash(llvm::Value* capacity, llvm::Value* key,
        ASR::ttype_t* key_asr_type, llvm::Module* module) {
        // Write specialised hash functions for intrinsic types
        // This is to avoid unnecessary calls to C-runtime and do
        // as much as possible in LLVM directly.
        switch( key_asr_type->type ) {
            case ASR::ttypeType::Integer: {
                // Simple modulo with the capacity of the dict.
                // We can update it later to do a better hash function
                // which produces lesser collisions.

                llvm::Value* int_hash = builder->CreateZExtOrTrunc(
                    builder->CreateURem(key,
                    builder->CreateZExtOrTrunc(capacity, key->getType())),
                    capacity->getType()
                );
                return int_hash;
            }
            case ASR::ttypeType::String: {
                // Polynomial rolling hash function for strings
                llvm::Value* null_char = llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),
                                                                llvm::APInt(8, '\0'));
                llvm::Value* p = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 31));
                llvm::Value* m = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 100000009));
                hash_value = llvm_utils->CreateAlloca(llvm::Type::getInt64Ty(context), nullptr, "hash_value");
                hash_iter = llvm_utils->CreateAlloca(llvm::Type::getInt64Ty(context), nullptr, "hash_iter");
                polynomial_powers = llvm_utils->CreateAlloca(llvm::Type::getInt64Ty(context), nullptr, "p_pow");
                LLVM::CreateStore(*builder,
                    llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 0)),
                    hash_value);
                LLVM::CreateStore(*builder,
                    llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 1)),
                    polynomial_powers);
                LLVM::CreateStore(*builder,
                    llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 0)),
                    hash_iter);
                llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
                llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
                llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

                // head
                llvm_utils->start_new_block(loophead);
                {
                    llvm::Value* i = llvm_utils->CreateLoad(hash_iter);
                    llvm::Value* c = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context), llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), key, i));
                    llvm::Value *cond = builder->CreateICmpNE(c, null_char);
                    builder->CreateCondBr(cond, loopbody, loopend);
                }

                // body
                llvm_utils->start_new_block(loopbody);
                {
                    // for c in key:
                    //     hash_value = (hash_value + (ord(c) + 1) * p_pow) % m
                    //     p_pow = (p_pow * p) % m
                    llvm::Value* i = llvm_utils->CreateLoad(hash_iter);
                    llvm::Value* c = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context), llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), key, i));
                    llvm::Value* p_pow = llvm_utils->CreateLoad(polynomial_powers);
                    llvm::Value* hash = llvm_utils->CreateLoad(hash_value);
                    c = builder->CreateZExt(c, llvm::Type::getInt64Ty(context));
                    c = builder->CreateAdd(c, llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 1)));
                    c = builder->CreateMul(c, p_pow);
                    c = builder->CreateSRem(c, m);
                    hash = builder->CreateAdd(hash, c);
                    hash = builder->CreateSRem(hash, m);
                    LLVM::CreateStore(*builder, hash, hash_value);
                    p_pow = builder->CreateMul(p_pow, p);
                    p_pow = builder->CreateSRem(p_pow, m);
                    LLVM::CreateStore(*builder, p_pow, polynomial_powers);
                    i = builder->CreateAdd(i, llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 1)));
                    LLVM::CreateStore(*builder, i, hash_iter);
                }

                builder->CreateBr(loophead);

                // end
                llvm_utils->start_new_block(loopend);
                llvm::Value* hash = llvm_utils->CreateLoad(hash_value);
                hash = builder->CreateTrunc(hash, llvm::Type::getInt32Ty(context));
                return builder->CreateSRem(hash, capacity);
            }
            case ASR::ttypeType::Tuple: {
                llvm::Value* tuple_hash = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 0));
                ASR::Tuple_t* asr_tuple = ASR::down_cast<ASR::Tuple_t>(key_asr_type);
                for( size_t i = 0; i < asr_tuple->n_type; i++ ) {
                    llvm::Value* llvm_tuple_i = llvm_utils->tuple_api->read_item(key, asr_tuple, i,
                                                    LLVM::is_llvm_struct(asr_tuple->m_type[i]));
                    tuple_hash = builder->CreateAdd(tuple_hash, get_key_hash(capacity, llvm_tuple_i,
                                                                             asr_tuple->m_type[i], module));
                    tuple_hash = builder->CreateSRem(tuple_hash, capacity);
                }
                return tuple_hash;
            }
            case ASR::ttypeType::Logical: {
                // (int32_t)key % capacity
                // modulo is required for the case when dict has a single key, `True`
                llvm::Value* key_i32 = builder->CreateZExt(key, llvm::Type::getInt32Ty(context));
                llvm::Value* logical_hash = builder->CreateZExtOrTrunc(
                    builder->CreateURem(key_i32,
                    builder->CreateZExtOrTrunc(capacity, key_i32->getType())),
                    capacity->getType()
                );
                return logical_hash;
            }
            default: {
                throw LCompilersException("Hashing " + ASRUtils::type_to_str_python(key_asr_type) +
                                          " isn't implemented yet.");
            }
        }
    }

    void LLVMDict::rehash(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Module* module,
        ASR::ttype_t* key_asr_type,
        ASR::ttype_t* value_asr_type,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {

        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);
        std::string value_type_code = ASRUtils::get_type_code(value_asr_type);

        std::pair<std::string, std::string> dict_type_key = std::make_pair(key_type_code, value_type_code);
        llvm::Type* key_llvm_type = std::get<2>(typecode2dicttype[dict_type_key]).first;
        llvm::Type* value_llvm_type = std::get<2>(typecode2dicttype[dict_type_key]).second;
        int32_t key_type_size = std::get<1>(typecode2dicttype[dict_type_key]).first;
        int32_t value_type_size = std::get<1>(typecode2dicttype[dict_type_key]).second;

        llvm::Value* capacity_ptr = get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, dict);
        llvm::Value* old_capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), capacity_ptr);
        llvm::Value* capacity = builder->CreateMul(old_capacity, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                                       llvm::APInt(32, 2)));
        capacity = builder->CreateAdd(capacity, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                                       llvm::APInt(32, 1)));
        LLVM::CreateStore(*builder, capacity, capacity_ptr);


        llvm::Value* key_list = get_key_list(dict);
        llvm::Value* new_key_list = llvm_utils->CreateAlloca(llvm_utils->list_api->get_list_type(key_llvm_type,
                                                          key_type_code, key_type_size));
        llvm_utils->list_api->list_init(key_type_code, new_key_list, module, capacity, capacity);

        llvm::Value* value_list = get_value_list(dict);
        llvm::Value* new_value_list = llvm_utils->CreateAlloca(llvm_utils->list_api->get_list_type(value_llvm_type,
                                                            value_type_code, value_type_size));
        llvm_utils->list_api->list_init(value_type_code, new_value_list, module, capacity, capacity);

        llvm::Value* key_mask = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context)->getPointerTo(),
                                                        get_pointer_to_keymask(dict));
        llvm::DataLayout data_layout(module->getDataLayout());
        size_t mask_size = data_layout.getTypeAllocSize(llvm::Type::getInt8Ty(context));
        llvm::Value* llvm_mask_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                            llvm::APInt(32, mask_size));
        llvm::Value* new_key_mask = LLVM::lfortran_calloc(context, *module, *builder, capacity,
                                                          llvm_mask_size);

        llvm::Value* current_capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), 
                                                get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, dict));
        idx_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
            llvm::APInt(32, 0)), idx_ptr);

        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpSGT(old_capacity, llvm_utils->CreateLoad(idx_ptr));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* idx = llvm_utils->CreateLoad(idx_ptr);
            llvm::Function *fn = builder->GetInsertBlock()->getParent();
            llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", fn);
            llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context, "else");
            llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifcont");
            llvm::Value* is_key_set = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context), 
                                                              llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), 
                                                                                         key_mask, idx));
            is_key_set = builder->CreateICmpNE(is_key_set,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0)));
            builder->CreateCondBr(is_key_set, thenBB, elseBB);
            builder->SetInsertPoint(thenBB);
            {
                llvm::Value* key = llvm_utils->list_api->read_item_using_ttype(key_asr_type, key_list, idx,
                        false, module, LLVM::is_llvm_struct(key_asr_type));
                llvm::Value* value = llvm_utils->list_api->read_item_using_ttype(value_asr_type, value_list,
                    idx, false, module, LLVM::is_llvm_struct(value_asr_type));
                llvm::Value* key_hash = get_key_hash(current_capacity, key, key_asr_type, module);
                this->resolve_collision(dict_expr, current_capacity, key_hash, key, new_key_list,
                               new_key_mask, module, key_asr_type);
                llvm::Value* pos = llvm_utils->CreateLoad(pos_ptr);
                llvm::Value* key_dest = llvm_utils->list_api->read_item_using_ttype(key_asr_type,
                                                new_key_list, pos, false, module, true);
                llvm_utils->deepcopy(nullptr, key, key_dest, key_asr_type, key_asr_type, module, name2memidx);
                llvm::Value* value_dest = llvm_utils->list_api->read_item_using_ttype(value_asr_type,
                                                new_value_list, pos, false, module, true);
                llvm_utils->deepcopy(dict_expr, value, value_dest, value_asr_type, value_asr_type, module, name2memidx);
                
                llvm::Value* linear_prob_happened = builder->CreateICmpNE(key_hash, pos);
                llvm::Value* set_max_2 = builder->CreateSelect(linear_prob_happened,
                    llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 2)),
                    llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));
                LLVM::CreateStore(*builder, set_max_2, llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), 
                                                                                   new_key_mask, key_hash));
                LLVM::CreateStore(*builder, set_max_2, llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), 
                                                                                   new_key_mask, pos));
            }
            builder->CreateBr(mergeBB);

            llvm_utils->start_new_block(elseBB);
            llvm_utils->start_new_block(mergeBB);
            idx = builder->CreateAdd(idx, llvm::ConstantInt::get(
                    llvm::Type::getInt32Ty(context), llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, idx, idx_ptr);
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);

        // TODO: Free key_list, value_list and key_mask
        llvm_utils->list_api->free_data_using_type(key_type_code, key_list, module);
        llvm_utils->list_api->free_data_using_type(value_type_code, value_list, module);
        LLVM::lfortran_free(context, *module, *builder, key_mask);
        LLVM::CreateStore(*builder, llvm_utils->CreateLoad2(llvm_utils->list_api->get_list_type(nullptr, key_type_code, 0), 
                                new_key_list), key_list);
        LLVM::CreateStore(*builder, llvm_utils->CreateLoad2(llvm_utils->list_api->get_list_type(nullptr, value_type_code, 0),
                                new_value_list), value_list);
        LLVM::CreateStore(*builder, new_key_mask, get_pointer_to_keymask(dict));
    }

    void LLVMDictSeparateChaining::rehash(
        [[maybe_unused]] ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Module* module,
        ASR::ttype_t* key_asr_type,
        ASR::ttype_t* value_asr_type,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        old_capacity = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        old_occupancy = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        old_number_of_buckets_filled = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        idx_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        old_key_value_pairs = llvm_utils->CreateAlloca(llvm::Type::getInt8Ty(context)->getPointerTo());
        old_key_mask = llvm_utils->CreateAlloca(llvm::Type::getInt8Ty(context)->getPointerTo());

        llvm::Type* kv_pair_type = get_key_value_pair_type(key_asr_type, value_asr_type);
        llvm::Value* capacity_ptr = get_pointer_to_capacity(dict);
        llvm::Value* occupancy_ptr = get_pointer_to_occupancy(dict);
        llvm::Value* number_of_buckets_filled_ptr = get_pointer_to_number_of_filled_buckets(dict);

        llvm::Value* old_capacity_value = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), capacity_ptr);
        LLVM::CreateStore(*builder, old_capacity_value, old_capacity);
        LLVM::CreateStore(*builder,
            llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), occupancy_ptr),
            old_occupancy
        );
        LLVM::CreateStore(*builder,
            llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), number_of_buckets_filled_ptr),
            old_number_of_buckets_filled
        );
        llvm::Value* old_key_mask_value = llvm_utils->CreateLoad2(
            llvm::Type::getInt8Ty(context)->getPointerTo(), get_pointer_to_keymask(dict));


        llvm::Value* old_key_value_pairs_value = llvm_utils->CreateLoad2(kv_pair_type->getPointerTo(), get_pointer_to_key_value_pairs(dict));
        old_key_value_pairs_value = builder->CreateBitCast(old_key_value_pairs_value, llvm::Type::getInt8Ty(context)->getPointerTo());
        LLVM::CreateStore(*builder, old_key_mask_value, old_key_mask);
        LLVM::CreateStore(*builder, old_key_value_pairs_value, old_key_value_pairs);

        llvm::Value* capacity = builder->CreateMul(old_capacity_value, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                                       llvm::APInt(32, 3)));
        capacity = builder->CreateAdd(capacity, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                                       llvm::APInt(32, 1)));
        dict_init_given_initial_capacity(ASRUtils::get_type_code(key_asr_type),
                                         ASRUtils::get_type_code(value_asr_type),
                                         dict, module, capacity);
        llvm::Function *fn = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *thenBB_rehash = llvm::BasicBlock::Create(context, "then", fn);
        llvm::BasicBlock *elseBB_rehash = llvm::BasicBlock::Create(context, "else");
        llvm::BasicBlock *mergeBB_rehash = llvm::BasicBlock::Create(context, "ifcont");
        llvm::Value* rehash_flag = llvm_utils->CreateLoad2(
            llvm::Type::getInt1Ty(context),get_pointer_to_rehash_flag(dict));
        builder->CreateCondBr(rehash_flag, thenBB_rehash, elseBB_rehash);
        builder->SetInsertPoint(thenBB_rehash);
        old_key_value_pairs_value = llvm_utils->CreateLoad2(kv_pair_type->getPointerTo(), old_key_value_pairs);
        old_key_value_pairs_value = builder->CreateBitCast(old_key_value_pairs_value,
            get_key_value_pair_type(key_asr_type, value_asr_type)->getPointerTo());
        old_key_mask_value = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context)->getPointerTo(), old_key_mask);
        old_capacity_value = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), old_capacity);
        capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), get_pointer_to_capacity(dict));
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 0)), idx_ptr);
        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpSGT(
                                        old_capacity_value,
                                        llvm_utils->CreateLoad(idx_ptr));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* itr = llvm_utils->CreateLoad(idx_ptr);
            llvm::Value* key_mask_value = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context), 
                llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), old_key_mask_value, itr));
            llvm::Value* is_key_set = builder->CreateICmpEQ(key_mask_value,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));

            llvm_utils->create_if_else(is_key_set, [&]() {
                llvm::Value* srci = llvm_utils->create_ptr_gep2(kv_pair_type, old_key_value_pairs_value, itr);
                write_key_value_pair_linked_list(nullptr, srci, dict, capacity, key_asr_type, value_asr_type, module, name2memidx);
            }, [=]() {
            });
            llvm::Value* tmp = builder->CreateAdd(
                        itr,
                        llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, tmp, idx_ptr);
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);
        builder->CreateBr(mergeBB_rehash);
        llvm_utils->start_new_block(elseBB_rehash);
        {
            LLVM::CreateStore(*builder,
                llvm_utils->CreateLoad(old_capacity),
                get_pointer_to_capacity(dict)
            );
            LLVM::CreateStore(*builder,
                llvm_utils->CreateLoad(old_occupancy),
                get_pointer_to_occupancy(dict)
            );
            LLVM::CreateStore(*builder,
                llvm_utils->CreateLoad(old_number_of_buckets_filled),
                get_pointer_to_number_of_filled_buckets(dict)
            );
            LLVM::CreateStore(*builder,
                builder->CreateBitCast(
                    llvm_utils->CreateLoad(old_key_value_pairs),
                    get_key_value_pair_type(key_asr_type, value_asr_type)->getPointerTo()
                ),
                get_pointer_to_key_value_pairs(dict)
            );
            LLVM::CreateStore(*builder,
                llvm_utils->CreateLoad(old_key_mask),
                get_pointer_to_keymask(dict)
            );
        }
        llvm_utils->start_new_block(mergeBB_rehash);
    }

    void LLVMDict::rehash_all_at_once_if_needed(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Module* module,
        ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        /**
         * C++ equivalent:
         *
         * // this condition will be true with 0 capacity too
         * rehash_condition = 5 * occupancy >= 3 * capacity;
         * if( rehash_condition ) {
         *     rehash();
         * }
         *
         */

        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);
        std::string value_type_code = ASRUtils::get_type_code(value_asr_type);

        llvm::Type* dict_type = get_dict_type(key_type_code, value_type_code, 0, 0, nullptr, nullptr);
        llvm::Value* occupancy = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), 
                                                         get_pointer_to_occupancy_using_type(dict_type, dict));
        llvm::Value* capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), 
                                                        get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, dict));
        // Threshold hash is chosen from https://en.wikipedia.org/wiki/Hash_table#Load_factor
        // occupancy / capacity >= 0.6 is same as 5 * occupancy >= 3 * capacity
        llvm::Value* occupancy_times_5 = builder->CreateMul(occupancy, llvm::ConstantInt::get(
                                llvm::Type::getInt32Ty(context), llvm::APInt(32, 5)));
        llvm::Value* capacity_times_3 = builder->CreateMul(capacity, llvm::ConstantInt::get(
                                llvm::Type::getInt32Ty(context), llvm::APInt(32, 3)));
        llvm_utils->create_if_else(builder->CreateICmpSGE(occupancy_times_5,
                                    capacity_times_3), [&]() {
            rehash(dict_expr, dict, module, key_asr_type, value_asr_type, name2memidx);
        }, []() {});
    }

    void LLVMDictSeparateChaining::rehash_all_at_once_if_needed(
        ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Module* module,
        ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {

        /**
         * C++ equivalent:
         *
         * // this condition will be true with 0 buckets_filled too
         * rehash_condition = rehash_flag && (occupancy >= 2 * buckets_filled);
         * if( rehash_condition ) {
         *     rehash();
         * }
         *
         */
        
        llvm::Value* occupancy = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_occupancy(dict));
        llvm::Value* buckets_filled = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_number_of_filled_buckets(dict));
        llvm::Value* rehash_condition = llvm_utils->CreateLoad2(
            llvm::Type::getInt1Ty(context),get_pointer_to_rehash_flag(dict));
        llvm::Value* buckets_filled_times_2 = builder->CreateMul(buckets_filled, llvm::ConstantInt::get(
                                llvm::Type::getInt32Ty(context), llvm::APInt(32, 2)));
        rehash_condition = builder->CreateAnd(rehash_condition,
            builder->CreateICmpSGE(occupancy, buckets_filled_times_2));
        llvm_utils->create_if_else(rehash_condition, [&]() {
            rehash(dict_expr, dict, module, key_asr_type, value_asr_type, name2memidx);
        }, [=]() {
        });
    }

    void LLVMDictInterface::write_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
                              llvm::Value* value, llvm::Module* module,
                              ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type,
                              std::map<std::string, std::map<std::string, int>>& name2memidx) {

        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);
        std::string value_type_code = ASRUtils::get_type_code(value_asr_type);
                 
        rehash_all_at_once_if_needed(dict_expr, dict, module, key_asr_type, value_asr_type, name2memidx);
        llvm::Value* current_capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context),
                 get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, dict));
        llvm::Value* key_hash = get_key_hash(current_capacity, key, key_asr_type, module);
        this->resolve_collision_for_write(dict_expr, dict, key_hash, key, value, module,
                                          key_asr_type, value_asr_type, name2memidx);
        // A second rehash ensures that the threshold is not breached at any point.
        // It can be shown mathematically that rehashing twice would only occur for small dictionaries,
        // for example, for threshold set in linear probing, it occurs only when len(dict) <= 2
        rehash_all_at_once_if_needed(dict_expr, dict, module, key_asr_type, value_asr_type, name2memidx);
    }

    llvm::Value* LLVMDict::read_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
                             llvm::Module* module, ASR::Dict_t* dict_type, bool enable_bounds_checking,
                             bool get_pointer) {

        std::string key_type_code = ASRUtils::get_type_code(dict_type->m_key_type);
        std::string value_type_code = ASRUtils::get_type_code(dict_type->m_value_type);

        std::pair<std::string, std::string> llvm_key = std::make_pair(key_type_code, value_type_code);
        llvm::Type* value_type = std::get<2>(typecode2dicttype[llvm_key]).second;
        llvm::Value* current_capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context),
                                            get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, dict));
        llvm::Value* key_hash = get_key_hash(current_capacity, key, dict_type->m_key_type, module);
        llvm::Value* value_ptr;
        if (enable_bounds_checking) {
            value_ptr = this->resolve_collision_for_read_with_bound_check(dict_expr, dict, key_hash, key, module,
                                                                  dict_type->m_key_type, dict_type->m_value_type);
        } else {
            value_ptr = this->resolve_collision_for_read(dict_expr, dict, key_hash, key, module,
                                                                  dict_type->m_key_type, dict_type->m_value_type);
        }
        if( get_pointer ) {
            return value_ptr;
        }
        return llvm_utils->CreateLoad2(value_type, value_ptr);
    }

    llvm::Value* LLVMDict::get_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
                             llvm::Module* module, ASR::Dict_t* dict_type, llvm::Value* def_value,
                             bool get_pointer) {
        std::string key_type_code = ASRUtils::get_type_code(dict_type->m_key_type);
        std::string value_type_code = ASRUtils::get_type_code(dict_type->m_value_type);
        llvm::Value* current_capacity = llvm_utils->CreateLoad(get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, dict));
        llvm::Value* key_hash = get_key_hash(current_capacity, key, dict_type->m_key_type, module);
        llvm::Value* value_ptr = this->resolve_collision_for_read_with_default(dict_expr, dict, key_hash, key, module,
                                                                  dict_type->m_key_type, dict_type->m_value_type,
                                                                  def_value);
        if( get_pointer ) {
            return value_ptr;
        }
        return llvm_utils->CreateLoad(value_ptr);
    }

    llvm::Value* LLVMDictSeparateChaining::read_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
        llvm::Module* module, ASR::Dict_t* dict_type, bool enable_bounds_checking, bool get_pointer) {
        llvm::Value* current_capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), get_pointer_to_capacity(dict));
        llvm::Value* key_hash = get_key_hash(current_capacity, key, dict_type->m_key_type, module);
        llvm::Value* value_ptr;
        if (enable_bounds_checking) {
            value_ptr = this->resolve_collision_for_read_with_bound_check(dict_expr, dict, key_hash, key, module,
                                                                  dict_type->m_key_type, dict_type->m_value_type);
        } else {
            value_ptr = this->resolve_collision_for_read(dict_expr, dict, key_hash, key, module,
                                                                  dict_type->m_key_type, dict_type->m_value_type);
        }
        std::pair<std::string, std::string> llvm_key = std::make_pair(
            ASRUtils::get_type_code(dict_type->m_key_type),
            ASRUtils::get_type_code(dict_type->m_value_type)
        );
        llvm::Type* value_type = std::get<2>(typecode2dicttype[llvm_key]).second;
        value_ptr = builder->CreateBitCast(value_ptr, value_type->getPointerTo());
        if( get_pointer ) {
            return value_ptr;
        }
        return llvm_utils->CreateLoad(value_ptr);
    }

    llvm::Value* LLVMDictSeparateChaining::get_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
        llvm::Module* module, ASR::Dict_t* dict_type, llvm::Value* def_value, bool get_pointer) {
        llvm::Value* current_capacity = llvm_utils->CreateLoad(get_pointer_to_capacity(dict));
        llvm::Value* key_hash = get_key_hash(current_capacity, key, dict_type->m_key_type, module);
        llvm::Value* value_ptr = this->resolve_collision_for_read_with_default(dict_expr, dict, key_hash, key, module,
                                                                  dict_type->m_key_type, dict_type->m_value_type,
                                                                  def_value);
        std::pair<std::string, std::string> llvm_key = std::make_pair(
            ASRUtils::get_type_code(dict_type->m_key_type),
            ASRUtils::get_type_code(dict_type->m_value_type)
        );
        llvm::Type* value_type = std::get<2>(typecode2dicttype[llvm_key]).second;
        value_ptr = builder->CreateBitCast(value_ptr, value_type->getPointerTo());
        if( get_pointer ) {
            return value_ptr;
        }
        return llvm_utils->CreateLoad(value_ptr);
    }

    llvm::Value* LLVMDict::pop_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
        llvm::Module* module, ASR::Dict_t* dict_type,
        bool get_pointer) {
        /**
         * C++ equivalent:
         *
         * resolve_collision_for_read_with_bound_check(key);  // modifies pos
         * key_mask[pos] = 3;    // tombstone marker
         * occupancy -= 1;
         */

        std::string key_type_code = ASRUtils::get_type_code(dict_type->m_key_type);
        std::string value_type_code = ASRUtils::get_type_code(dict_type->m_value_type);
        llvm::Value* current_capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context),
                                                                get_pointer_to_capacity_using_typecode(
                                                                    key_type_code, value_type_code, dict));
        llvm::Value* key_hash = get_key_hash(current_capacity, key, dict_type->m_key_type, module);
        llvm::Value* value_ptr = this->resolve_collision_for_read_with_bound_check(dict_expr, dict, key_hash, key, module,
                                                                  dict_type->m_key_type, dict_type->m_value_type);
        llvm::Value* pos = llvm_utils->CreateLoad(pos_ptr);
        llvm::Value* key_mask = llvm_utils->CreateLoad2(
            llvm::Type::getInt8Ty(context)->getPointerTo(), get_pointer_to_keymask(dict));
        llvm::Value* key_mask_i = llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), key_mask, pos);
        llvm::Value* tombstone_marker = llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 3));
        LLVM::CreateStore(*builder, tombstone_marker, key_mask_i);

        llvm::Value* occupancy_ptr = get_pointer_to_occupancy(dict);
        llvm::Value* occupancy = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), occupancy_ptr);
        occupancy = builder->CreateSub(occupancy, llvm::ConstantInt::get(
                        llvm::Type::getInt32Ty(context), llvm::APInt(32, 1)));
        LLVM::CreateStore(*builder, occupancy, occupancy_ptr);
        llvm::Type* value_type = llvm_utils->get_type_from_ttype_t_util(nullptr, dict_type->m_value_type, module);
        
        if( get_pointer ) {
            llvm::Value* return_ptr = llvm_utils->CreateAlloca(value_type);
            LLVM::CreateStore(*builder, llvm_utils->CreateLoad2(value_type, value_ptr), return_ptr);
            return return_ptr;
        }

        return llvm_utils->CreateLoad2(value_type, value_ptr);
    }

    llvm::Value* LLVMDictSeparateChaining::pop_item(ASR::expr_t* dict_expr,
        llvm::Value* dict, llvm::Value* key,
        llvm::Module* module, ASR::Dict_t* dict_type,
        bool get_pointer) {
        /**
         * C++ equivalent:
         *
         * // modifies chain_itr and chain_itr_prev
         * resolve_collision_for_read_with_bound_check(key);
         *
         * if(chain_itr_prev != nullptr) {
         *     chain_itr_prev[2] = chain_itr[2]; // next
         * }
         * else {
         *     // head of linked list removed
         *     if( chain_itr[2] == nullptr ) {
         *         // this linked list is now empty
         *         key_mask[key_hash] = 0;
         *         num_buckets_filled--;
         *     }
         *     else {
         *         // not empty yet
         *         key_value_pairs[key_hash] = chain_itr[2];
         *     }
         * }
         *
         * occupancy--;
         *
         */

        llvm::Value* current_capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), get_pointer_to_capacity(dict));
        llvm::Value* key_hash = get_key_hash(current_capacity, key, dict_type->m_key_type, module);
        llvm::Value* value_ptr = this->resolve_collision_for_read_with_bound_check(dict_expr, dict, key_hash, key, module,
                                                                  dict_type->m_key_type, dict_type->m_value_type);
        std::pair<std::string, std::string> llvm_key = std::make_pair(
            ASRUtils::get_type_code(dict_type->m_key_type),
            ASRUtils::get_type_code(dict_type->m_value_type)
        );
        llvm::Type* value_type = std::get<2>(typecode2dicttype[llvm_key]).second;
        value_ptr = builder->CreateBitCast(value_ptr, value_type->getPointerTo());
        llvm::Value* prev = llvm_utils->CreateLoad(chain_itr_prev);
        llvm::Value* found = llvm_utils->CreateLoad(chain_itr);
        llvm::Type* kv_struct_type = get_key_value_pair_type(dict_type->m_key_type, dict_type->m_value_type);
        found = builder->CreateBitCast(found, kv_struct_type->getPointerTo());
        llvm::Value* found_next = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context)->getPointerTo(), llvm_utils->create_gep2(kv_struct_type, found, 2));

        llvm_utils->create_if_else(builder->CreateICmpNE(prev,
                        llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo())), [&]() {
            prev = builder->CreateBitCast(prev, kv_struct_type->getPointerTo());
            LLVM::CreateStore(*builder, found_next, llvm_utils->create_gep2(kv_struct_type, prev, 2));
        }, [&]() {
            llvm_utils->create_if_else(builder->CreateICmpEQ(found_next,
                        llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo())), [&]() {
                llvm::Value* key_mask = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context)->getPointerTo(), get_pointer_to_keymask(dict));
                LLVM::CreateStore(
                    *builder,
                    llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0)),
                    llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), key_mask, key_hash)
                );
                llvm::Value* num_buckets_filled_ptr = get_pointer_to_number_of_filled_buckets(dict);
                llvm::Value* num_buckets_filled = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), num_buckets_filled_ptr);
                num_buckets_filled = builder->CreateSub(num_buckets_filled, llvm::ConstantInt::get(
                            llvm::Type::getInt32Ty(context), llvm::APInt(32, 1)));
                LLVM::CreateStore(*builder, num_buckets_filled, num_buckets_filled_ptr);
            }, [&]() {
                found_next = builder->CreateBitCast(found_next, kv_struct_type->getPointerTo());
                llvm::Value* key_value_pairs = llvm_utils->CreateLoad2(kv_struct_type->getPointerTo(), get_pointer_to_key_value_pairs(dict));
                LLVM::CreateStore(*builder, llvm_utils->CreateLoad2(kv_struct_type, found_next),
                                    llvm_utils->create_ptr_gep2(kv_struct_type, key_value_pairs, key_hash));
            });
        });

        llvm::Value* occupancy_ptr = get_pointer_to_occupancy(dict);
        llvm::Value* occupancy = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), occupancy_ptr);
        occupancy = builder->CreateSub(occupancy, llvm::ConstantInt::get(
                        llvm::Type::getInt32Ty(context), llvm::APInt(32, 1)));
        LLVM::CreateStore(*builder, occupancy, occupancy_ptr);

        if( get_pointer ) {
            std::string key_type_code = ASRUtils::get_type_code(dict_type->m_key_type);
            std::string value_type_code = ASRUtils::get_type_code(dict_type->m_value_type);
            llvm::Type* llvm_value_type = std::get<2>(typecode2dicttype[std::make_pair(
                key_type_code, value_type_code)]).second;
            llvm::Value* return_ptr = llvm_utils->CreateAlloca(llvm_value_type);
            LLVM::CreateStore(*builder, llvm_utils->CreateLoad2(value_type, value_ptr), return_ptr);
            return return_ptr;
        }

        return llvm_utils->CreateLoad(value_ptr);
    }

    void LLVMDict::get_elements_list(ASR::expr_t* expr, llvm::Value* dict,
        llvm::Value* elements_list, ASR::ttype_t* key_asr_type,
        ASR::ttype_t* value_asr_type, llvm::Module* module,
        std::map<std::string, std::map<std::string, int>>& name2memidx,
        bool key_or_value) {

        /**
         * C++ equivalent:
         *
         * // key_or_value = 0 for keys, 1 for values
         *
         * idx = 0;
         *
         * while( capacity > idx ) {
         *     el = key_or_value_list[idx];
         *     key_mask_value = key_mask[idx];
         *
         *     is_key_skip = key_mask_value == 3;     // tombstone
         *     is_key_set = key_mask_value != 0;
         *     add_el = is_key_set && !is_key_skip;
         *     if( add_el ) {
         *         elements_list.append(el);
         *     }
         *
         *     idx++;
         * }
         *
         */

        std::string key_type_code = ASRUtils::get_type_code(key_asr_type);
        std::string value_type_code = ASRUtils::get_type_code(value_asr_type);
        llvm::Value* capacity = llvm_utils->CreateLoad(get_pointer_to_capacity_using_typecode(key_type_code, value_type_code, dict));
        llvm::Value* key_mask = llvm_utils->CreateLoad(get_pointer_to_keymask(dict));
        llvm::Value* el_list = key_or_value == 0 ? get_key_list(dict) : get_value_list(dict);
        ASR::ttype_t* el_asr_type = key_or_value == 0 ? key_asr_type : value_asr_type;
    
        idx_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
            llvm::APInt(32, 0)), idx_ptr);

        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpSGT(capacity, llvm_utils->CreateLoad(idx_ptr));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* idx = llvm_utils->CreateLoad(idx_ptr);
            llvm::Value* key_mask_value = llvm_utils->CreateLoad(
                llvm_utils->create_ptr_gep(key_mask, idx));
            llvm::Value* is_key_skip = builder->CreateICmpEQ(key_mask_value,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 3)));
            llvm::Value* is_key_set = builder->CreateICmpNE(key_mask_value,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0)));

            llvm::Value* add_el = builder->CreateAnd(is_key_set,
                                            builder->CreateNot(is_key_skip));
            llvm_utils->create_if_else(add_el, [&]() {
                llvm::Value* el = llvm_utils->list_api->read_item_using_ttype(el_asr_type, el_list, idx,
                        false, module, LLVM::is_llvm_struct(el_asr_type));
                llvm_utils->list_api->append(expr, elements_list, el,
                                             el_asr_type, module, name2memidx);
            }, [=]() {
            });

            idx = builder->CreateAdd(idx, llvm::ConstantInt::get(
                    llvm::Type::getInt32Ty(context), llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, idx, idx_ptr);
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);
    }

    void LLVMDictSeparateChaining::get_elements_list(ASR::expr_t* expr, llvm::Value* dict,
        llvm::Value* elements_list, ASR::ttype_t* key_asr_type,
        ASR::ttype_t* value_asr_type, llvm::Module* module,
        std::map<std::string, std::map<std::string, int>>& name2memidx,
        bool key_or_value) {
        idx_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        chain_itr = llvm_utils->CreateAlloca(llvm::Type::getInt8Ty(context)->getPointerTo());
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                            llvm::APInt(32, 0)), idx_ptr);

        llvm::Value* capacity = llvm_utils->CreateLoad(get_pointer_to_capacity(dict));
        llvm::Value* key_mask = llvm_utils->CreateLoad(get_pointer_to_keymask(dict));
        llvm::Value* key_value_pairs = llvm_utils->CreateLoad(get_pointer_to_key_value_pairs(dict));
        llvm::Type* kv_pair_type = get_key_value_pair_type(key_asr_type, value_asr_type);
        ASR::ttype_t* el_asr_type = key_or_value == 0 ? key_asr_type : value_asr_type;
        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpSGT(
                                        capacity,
                                        llvm_utils->CreateLoad(idx_ptr));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* idx = llvm_utils->CreateLoad(idx_ptr);
            llvm::Value* key_mask_value = llvm_utils->CreateLoad(
                llvm_utils->create_ptr_gep(key_mask, idx));
            llvm::Value* is_key_set = builder->CreateICmpEQ(key_mask_value,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));

            llvm_utils->create_if_else(is_key_set, [&]() {
                llvm::Value* dict_i = llvm_utils->create_ptr_gep(key_value_pairs, idx);
                llvm::Value* kv_ll_i8 = builder->CreateBitCast(dict_i, llvm::Type::getInt8Ty(context)->getPointerTo());
                LLVM::CreateStore(*builder, kv_ll_i8, chain_itr);

                llvm::BasicBlock *loop2head = llvm::BasicBlock::Create(context, "loop2.head");
                llvm::BasicBlock *loop2body = llvm::BasicBlock::Create(context, "loop2.body");
                llvm::BasicBlock *loop2end = llvm::BasicBlock::Create(context, "loop2.end");

                // head
                llvm_utils->start_new_block(loop2head);
                {
                    llvm::Value *cond = builder->CreateICmpNE(
                        llvm_utils->CreateLoad(chain_itr),
                        llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo())
                    );
                    builder->CreateCondBr(cond, loop2body, loop2end);
                }

                // body
                llvm_utils->start_new_block(loop2body);
                {
                    llvm::Value* kv_struct_i8 = llvm_utils->CreateLoad(chain_itr);
                    llvm::Value* kv_struct = builder->CreateBitCast(kv_struct_i8, kv_pair_type->getPointerTo());
                    llvm::Value* kv_el = llvm_utils->create_gep(kv_struct, key_or_value);
                    if( !LLVM::is_llvm_struct(el_asr_type) ) {
                        kv_el = llvm_utils->CreateLoad(kv_el);
                    }
                    llvm_utils->list_api->append(expr, elements_list, kv_el,
                                                 el_asr_type, module, name2memidx);
                    llvm::Value* next_kv_struct = llvm_utils->CreateLoad(llvm_utils->create_gep(kv_struct, 2));
                    LLVM::CreateStore(*builder, next_kv_struct, chain_itr);
                }

                builder->CreateBr(loop2head);

                // end
                llvm_utils->start_new_block(loop2end);
            }, [=]() {
            });
            llvm::Value* tmp = builder->CreateAdd(idx,
                        llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, tmp, idx_ptr);
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);
    }

    llvm::Value* LLVMList::read_item_using_ttype(ASR::ttype_t* el_asr_type, llvm::Value* list, llvm::Value* pos,
                                     bool enable_bounds_checking,
                                     llvm::Module* module, bool get_pointer) {
        // TODO: Fix when refactoring out type_codes
        std::string type_code = ASRUtils::get_type_code(el_asr_type);

        llvm::Type* list_type = get_list_type(nullptr, type_code, 0);
        llvm::Type* list_element_type = llvm_utils->get_type_from_ttype_t_util(nullptr, el_asr_type, module);

        if( enable_bounds_checking ) {
            check_index_within_bounds_using_type(list_type, list, pos, module);
        }

        llvm::Value* list_data = llvm_utils->CreateLoad2(list_element_type->getPointerTo(), 
                                                         get_pointer_to_list_data_using_type(list_type, list));
        llvm::Value* element_ptr = llvm_utils->create_ptr_gep2(list_element_type, list_data, pos);
        if( get_pointer ) {
            return element_ptr;
        }
        return llvm_utils->CreateLoad2(list_element_type, element_ptr);
    }

    llvm::Value* LLVMList::len_using_type(llvm::Type* list_type, llvm::Value* list) {
        return llvm_utils->CreateLoad2(
                llvm::Type::getInt32Ty(context) ,get_pointer_to_current_end_point_using_type(
                    list_type, list));
    }

    llvm::Value* LLVMDict::len(llvm::Value* dict) {
        return llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), get_pointer_to_occupancy(dict));
    }

    llvm::Value* LLVMDictSeparateChaining::len(llvm::Value* dict) {
        return llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), get_pointer_to_occupancy(dict)) ;
    }

    bool LLVMDictInterface::is_dict_present() {
        return is_dict_present_;
    }

    void LLVMDictInterface::set_is_dict_present(bool value) {
        is_dict_present_ = value;
    }

    LLVMDictInterface::~LLVMDictInterface() {
        typecode2dicttype.clear();
    }

    LLVMDict::~LLVMDict() {
    }

    LLVMDictSeparateChaining::~LLVMDictSeparateChaining() {
    }

    LLVMDictOptimizedLinearProbing::~LLVMDictOptimizedLinearProbing() {}

    void LLVMList::resize_if_needed_using_typecode(std::string& type_code, llvm::Value* list, llvm::Value* n,
                                    llvm::Value* capacity, int32_t type_size,
                                    llvm::Type* el_type, llvm::Module* module) {
        llvm::Type* list_type = get_list_type(el_type, type_code, type_size);
        llvm::Value *cond = builder->CreateICmpEQ(n, capacity);
        llvm::Function *fn = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", fn);
        llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context, "else");
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifcont");
        builder->CreateCondBr(cond, thenBB, elseBB);
        builder->SetInsertPoint(thenBB);
        llvm::Value* new_capacity = builder->CreateMul(llvm::ConstantInt::get(context,
                                                       llvm::APInt(32, 2)), capacity);
        new_capacity = builder->CreateAdd(new_capacity, llvm::ConstantInt::get(context,
                                                        llvm::APInt(32, 1)));
        llvm::Value* arg_size = builder->CreateMul(llvm::ConstantInt::get(context,
                                                   llvm::APInt(32, type_size)),
                                                   new_capacity);
        llvm::Value* copy_data_ptr = get_pointer_to_list_data_using_type(list_type, list);
        llvm::Value* copy_data = llvm_utils->CreateLoad2(el_type->getPointerTo() ,copy_data_ptr);
        copy_data = LLVM::lfortran_realloc(context, *module, *builder,
                                           copy_data, arg_size);
        copy_data = builder->CreateBitCast(copy_data, el_type->getPointerTo());
        builder->CreateStore(copy_data, copy_data_ptr);
        builder->CreateStore(new_capacity, get_pointer_to_current_capacity_using_type(list_type, list));
        builder->CreateBr(mergeBB);
        llvm_utils->start_new_block(elseBB);
        llvm_utils->start_new_block(mergeBB);
    }

    void LLVMList::shift_end_point_by_one_using_typecode(std::string& type_code, llvm::Value* list) {
        llvm::Type* list_type = get_list_type(nullptr, type_code, 0);
        llvm::Value* end_point_ptr = get_pointer_to_current_end_point_using_type(list_type, list);
        llvm::Value* end_point = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context),
                                                            end_point_ptr);
        end_point = builder->CreateAdd(end_point, llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
        builder->CreateStore(end_point, end_point_ptr);
    }

    void LLVMList::append(ASR::expr_t* list_expr, llvm::Value* list, llvm::Value* item,
                          ASR::ttype_t* asr_type, llvm::Module* module,
                          std::map<std::string, std::map<std::string, int>>& name2memidx) {
        std::string type_code = ASRUtils::get_type_code(asr_type);
        int type_size = std::get<1>(typecode2listtype[type_code]);
        llvm::Type* el_type = std::get<2>(typecode2listtype[type_code]);
        llvm::Type* list_type = std::get<0>(typecode2listtype[type_code]);


        llvm::Value* current_end_point = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context),
                                                                 get_pointer_to_current_end_point_using_type(list_type, list));
        llvm::Value* current_capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context),
                                                                get_pointer_to_current_capacity_using_type(list_type, list));
        resize_if_needed_using_typecode(type_code, list, current_end_point, current_capacity,
                         type_size, el_type, module);
        write_item(list_expr, list, current_end_point, item, asr_type, false, module, name2memidx);
        shift_end_point_by_one_using_typecode(type_code, list);
    }

    void LLVMList::insert_item([[maybe_unused]] ASR::expr_t* list_expr, llvm::Value* list, llvm::Value* pos,
                               llvm::Value* item, ASR::ttype_t* asr_type,
                               llvm::Module* module,
                               std::map<std::string, std::map<std::string, int>>& name2memidx) {
        std::string type_code = ASRUtils::get_type_code(asr_type);

        llvm::Type* list_type = std::get<0>(typecode2listtype[type_code]);
        int type_size = std::get<1>(typecode2listtype[type_code]);
        llvm::Type* el_type = std::get<2>(typecode2listtype[type_code]);

        llvm::Value* current_end_point = llvm_utils->CreateLoad2(
                               llvm::Type::getInt32Ty(context), get_pointer_to_current_end_point_using_type(list_type, list));
        llvm::Value* current_capacity = llvm_utils->CreateLoad2(
                               llvm::Type::getInt32Ty(context), get_pointer_to_current_capacity_using_type(list_type, list));
        resize_if_needed_using_typecode(type_code, list, current_end_point, current_capacity,
                         type_size, el_type, module);

        /* While loop equivalent in C++:
         *  end_point         // nth index of list
         *  pos               // ith index to insert the element
         *  pos_ptr = pos;
         *  tmp_ptr = list[pos];
         *  tmp = 0;
         *
         * while(end_point > pos_ptr) {
         *      tmp           = list[pos + 1];
         *      list[pos + 1] = tmp_ptr;
         *      tmp_ptr       = tmp;
         *      pos_ptr++;
         *  }
         *
         * list[pos] = item;
         */

        // TODO: Should be created outside the user loop and not here.
        // LLVMList should treat them as data members and create them
        // only if they are NULL
        llvm::AllocaInst *tmp_ptr = llvm_utils->CreateAlloca(el_type);
        LLVM::CreateStore(*builder, read_item_using_ttype(asr_type, list, pos, false, module, false), tmp_ptr);
        llvm::Value* tmp = nullptr;

        // TODO: Should be created outside the user loop and not here.
        // LLVMList should treat them as data members and create them
        // only if they are NULL
        llvm::AllocaInst *pos_ptr = llvm_utils->CreateAlloca(
                                    llvm::Type::getInt32Ty(context));
        LLVM::CreateStore(*builder, pos, pos_ptr);

        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpSGT(
                                        current_end_point,
                                        llvm_utils->CreateLoad(pos_ptr));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* next_index = builder->CreateAdd(
                            llvm_utils->CreateLoad(pos_ptr),
                            llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            tmp = read_item_using_ttype(asr_type, list, next_index, false, module, false);
            write_item_using_ttype(asr_type, list, next_index, llvm_utils->CreateLoad(tmp_ptr), false, module);
            LLVM::CreateStore(*builder, tmp, tmp_ptr);

            tmp = builder->CreateAdd(
                        llvm_utils->CreateLoad(pos_ptr),
                        llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, tmp, pos_ptr);
        }
        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);

        write_item(nullptr, list, pos, item, asr_type, false, module, name2memidx);
        shift_end_point_by_one_using_typecode(type_code, list);
    }

    void LLVMList::reserve(llvm::Value* list, llvm::Value* n,
                           ASR::ttype_t* asr_type, llvm::Module* module) {
        /**
         * C++ equivalent
         *
         * if( n > current_capacity ) {
         *     list_data = realloc(list_data, sizeof(el_type) * n);
         * }
         *
         */
        std::string type_code = ASRUtils::get_type_code(asr_type);

        llvm::Type* list_type = std::get<0>(typecode2listtype[type_code]);
        int type_size = std::get<1>(typecode2listtype[type_code]);
        llvm::Type* el_type = std::get<2>(typecode2listtype[type_code]);

        llvm::Value* capacity = llvm_utils->CreateLoad(get_pointer_to_current_capacity_using_type(list_type, list));
        llvm_utils->create_if_else(builder->CreateICmpSGT(n, capacity), [&]() {
            llvm::Value* arg_size = builder->CreateMul(llvm::ConstantInt::get(context,
                                                    llvm::APInt(32, type_size)), n);
            llvm::Value* copy_data_ptr = get_pointer_to_list_data_using_type(list_type, list);
            llvm::Value* copy_data = llvm_utils->CreateLoad(copy_data_ptr);
            copy_data = LLVM::lfortran_realloc(context, *module, *builder,
                                            copy_data, arg_size);
            copy_data = builder->CreateBitCast(copy_data, el_type->getPointerTo());
            builder->CreateStore(copy_data, copy_data_ptr);
            builder->CreateStore(n, get_pointer_to_current_capacity_using_type(list_type, list));
        }, []() {});
    }

    void LLVMList::reverse(ASR::ttype_t* el_asr_type, llvm::Value* list, llvm::Module* module) {

        /* Equivalent in C++:
         *
         * int i = 0;
         * int j = end_point - 1;
         *
         * tmp;
         *
         * while(j > i) {
         *      tmp = list[i];
         *      list[i] = list[j];
         *      list[j] = tmp;
         *      i = i + 1;
         *      j = j - 1;
         *  }
         */

        std::string type_code = ASRUtils::get_type_code(el_asr_type);

        llvm::Type* list_type = std::get<0>(typecode2listtype[type_code]);
        llvm::Value* end_point = llvm_utils->CreateLoad2(
                               llvm::Type::getInt32Ty(context) ,get_pointer_to_current_end_point_using_type(list_type, list));

        llvm::Type* pos_type = llvm::Type::getInt32Ty(context);
        llvm::AllocaInst *i = llvm_utils->CreateAlloca(pos_type);
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(
                                    context, llvm::APInt(32, 0)), i);       // i = 0
        llvm::AllocaInst *j = llvm_utils->CreateAlloca(pos_type);
        llvm::Value* tmp = nullptr;
        tmp = builder->CreateSub(end_point, llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
        LLVM::CreateStore(*builder, tmp, j);        // j = end_point - 1

        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpSGT(llvm_utils->CreateLoad(j), llvm_utils->CreateLoad(i));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            tmp = read_item_using_ttype(el_asr_type, list, llvm_utils->CreateLoad(i),
                false, module, false);    // tmp = list[i]
            write_item_using_ttype(el_asr_type, list, llvm_utils->CreateLoad(i),
                        read_item_using_ttype(el_asr_type, list, llvm_utils->CreateLoad(j),
                        false, module, false),
                        false, module);    // list[i] = list[j]
            write_item_using_ttype(el_asr_type, list, llvm_utils->CreateLoad(j),
                        tmp, false, module);    // list[j] = tmp

            tmp = builder->CreateAdd(
                        llvm_utils->CreateLoad(i),
                        llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, tmp, i);
            tmp = builder->CreateSub(
                        llvm_utils->CreateLoad(j),
                        llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, tmp, j);
        }
        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);
    }

    llvm::Value* LLVMList::find_item_position(llvm::Value* list,
        llvm::Value* item, ASR::ttype_t* item_type, llvm::Module* module,
        llvm::Value* start, llvm::Value* end) {
        llvm::Type* pos_type = llvm::Type::getInt32Ty(context);
        std::string type_code = ASRUtils::get_type_code(item_type);
        llvm::Type* list_type = std::get<0>(typecode2listtype[type_code]);

        // TODO: Should be created outside the user loop and not here.
        // LLVMList should treat them as data members and create them
        // only if they are NULL
        llvm::AllocaInst *i = llvm_utils->CreateAlloca(pos_type);
        if(start) {
            LLVM::CreateStore(*builder, start, i);
        }
        else {
            LLVM::CreateStore(*builder, llvm::ConstantInt::get(
                                context, llvm::APInt(32, 0)), i);
        }
        llvm::Value* end_point = nullptr;
        if(end) {
            end_point = end;
        }
        else {
            end_point = llvm_utils->CreateLoad2(
                llvm::Type::getInt32Ty(context), get_pointer_to_current_end_point_using_type(list_type, list));
        }
        llvm::Value* tmp = nullptr;

        /* Equivalent in C++:
         * int i = start;
         * while(list[i] != item && end_point > i) {
         *     i++;
         * }
         *
         * if (i == end_point) {
         *    std::cout << "The list does not contain the element";
         * }
         */

        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value* left_arg = read_item_using_ttype(item_type, list, llvm_utils->CreateLoad(i),
                false, module, LLVM::is_llvm_struct(item_type));
            llvm::Value* is_item_not_equal = builder->CreateNot(
                                                llvm_utils->is_equal_by_value(
                                                    left_arg, item,
                                                    module, item_type)
                                            );
            llvm::Value *cond = builder->CreateAnd(is_item_not_equal,
                                                   builder->CreateICmpSGT(end_point,
                                                    llvm_utils->CreateLoad(i)));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            tmp = builder->CreateAdd(
                        llvm_utils->CreateLoad(i),
                        llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, tmp, i);
        }
        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);

        llvm::Value* cond = builder->CreateICmpEQ(
                              llvm_utils->CreateLoad(i), end_point);
        llvm::Value* start_greater_than_end = builder->CreateICmpSGE(
                                                llvm_utils->CreateLoad(i), end_point);
        llvm::Value* condition = builder->CreateOr(cond, start_greater_than_end);
        llvm_utils->create_if_else(condition, [&]() {
            std::string message = "The list does not contain the element: ";
            llvm::Value *fmt_ptr = builder->CreateGlobalStringPtr("ValueError: %s%d\n");
            llvm::Value *fmt_ptr2 = builder->CreateGlobalStringPtr(message);
            print_error(context, *module, *builder, {fmt_ptr, fmt_ptr2, item});
            int exit_code_int = 1;
            llvm::Value *exit_code = llvm::ConstantInt::get(context,
                    llvm::APInt(32, exit_code_int));
            exit(context, *module, *builder, exit_code);
        }, [=]() {
        });
        return llvm_utils->CreateLoad(i);
    }

    llvm::Value* LLVMList::index(llvm::Value* list, llvm::Value* item,
                                llvm::Value* start, llvm::Value* end,
                                ASR::ttype_t* item_type, llvm::Module* module) {
        return LLVMList::find_item_position(list, item, item_type, module, start, end);
    }

    llvm::Value* LLVMList::count(llvm::Value* list, llvm::Value* item,
                                ASR::ttype_t* item_type, llvm::Module* module) {
        std::string type_code = ASRUtils::get_type_code(item_type);
        llvm::Type* list_type = std::get<0>(typecode2listtype[type_code]);
        llvm::Type* pos_type = llvm::Type::getInt32Ty(context);
        llvm::Value* current_end_point = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_current_end_point_using_type(list_type, list));
        llvm::AllocaInst *i = llvm_utils->CreateAlloca(pos_type);
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(
                                    context, llvm::APInt(32, 0)), i);
        llvm::AllocaInst *cnt = llvm_utils->CreateAlloca(pos_type);
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(
                                    context, llvm::APInt(32, 0)), cnt);
        llvm::Value* tmp = nullptr;

        /* Equivalent in C++:
         * int i = 0;
         * int cnt = 0;
         * while(end_point > i) {
         *     if(list[i] == item) {
         *         tmp = cnt+1;
         *         cnt = tmp;
         *     }
         *     tmp = i+1;
         *     i = tmp;
         * }
         */

        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpSGT(current_end_point,
                                         llvm_utils->CreateLoad(i));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            // if occurrence found, increment cnt
            llvm::Value* left_arg = read_item_using_ttype(item_type, list, llvm_utils->CreateLoad(i),
                false, module, LLVM::is_llvm_struct(item_type));
            llvm::Value* cond = llvm_utils->is_equal_by_value(left_arg, item, module, item_type);
            llvm_utils->create_if_else(cond, [&]() {
                tmp = builder->CreateAdd(
                            llvm_utils->CreateLoad(cnt),
                            llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
                LLVM::CreateStore(*builder, tmp, cnt);
            }, [=]() {
            });
            // increment i
            tmp = builder->CreateAdd(
                        llvm_utils->CreateLoad(i),
                        llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, tmp, i);
        }
        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);

        return llvm_utils->CreateLoad(cnt);
    }

    void LLVMList::remove(llvm::Value* list, llvm::Value* item,
                          ASR::ttype_t* item_type, llvm::Module* module) {
        llvm::Type* pos_type = llvm::Type::getInt32Ty(context);
        std::string type_code = ASRUtils::get_type_code(item_type);
        llvm::Type* list_type = std::get<0>(typecode2listtype[type_code]);
        llvm::Value* current_end_point = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context), get_pointer_to_current_end_point_using_type(list_type, list));
        // TODO: Should be created outside the user loop and not here.
        // LLVMList should treat them as data members and create them
        // only if they are NULL
        llvm::AllocaInst *item_pos = llvm_utils->CreateAlloca(pos_type);
        llvm::Value* tmp = LLVMList::find_item_position(list, item, item_type, module);
        LLVM::CreateStore(*builder, tmp, item_pos);

        /* While loop equivalent in C++:
         * item_pos = find_item_position();
         * while(end_point > item_pos) {
         *     tmp = item_pos + 1;
         *     list[item_pos] = list[tmp];
         *     item_pos = tmp;
         * }
         */

        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpSGT(current_end_point,
                                         llvm_utils->CreateLoad(item_pos));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            tmp = builder->CreateAdd(
                        llvm_utils->CreateLoad(item_pos),
                        llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            write_item_using_ttype(item_type, list, llvm_utils->CreateLoad(item_pos),
                read_item_using_ttype(item_type, list, tmp, false, module, false), false, module);
            LLVM::CreateStore(*builder, tmp, item_pos);
        }
        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);

        // Decrement end point by one
        llvm::Value* end_point_ptr = get_pointer_to_current_end_point_using_type(list_type, list);
        llvm::Value* end_point = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context),end_point_ptr);
        end_point = builder->CreateSub(end_point, llvm::ConstantInt::get(
                                       context, llvm::APInt(32, 1)));
        builder->CreateStore(end_point, end_point_ptr);
    }

    llvm::Value* LLVMList::pop_last(llvm::Value* list, ASR::ttype_t* list_asr_type, llvm::Module* module) {
        // If list is empty, output error
        std::string el_type_code = ASRUtils::get_type_code(ASRUtils::get_contained_type(list_asr_type));
        llvm::Type* list_type = std::get<0>(typecode2listtype[el_type_code]);

        llvm::Value* end_point_ptr = get_pointer_to_current_end_point_using_type(list_type, list);
        llvm::Value* end_point = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context),end_point_ptr);

        llvm::Value* cond = builder->CreateICmpEQ(llvm::ConstantInt::get(
                                    context, llvm::APInt(32, 0)), end_point);
        llvm_utils->create_if_else(cond, [&]() {
            std::string message = "pop from empty list";
            llvm::Value *fmt_ptr = builder->CreateGlobalStringPtr("IndexError: %s\n");
            llvm::Value *fmt_ptr2 = builder->CreateGlobalStringPtr(message);
            print_error(context, *module, *builder, {fmt_ptr, fmt_ptr2});
            int exit_code_int = 1;
            llvm::Value *exit_code = llvm::ConstantInt::get(context,
                    llvm::APInt(32, exit_code_int));
            exit(context, *module, *builder, exit_code);
        }, [=]() {
        });

        // Get last element of list
        llvm::Value* tmp = builder->CreateSub(end_point, llvm::ConstantInt::get(
                                    context, llvm::APInt(32, 1)));
        tmp = read_item_using_ttype(ASRUtils::get_contained_type(list_asr_type), list, tmp, false, module, LLVM::is_llvm_struct(list_asr_type));

        // Decrement end point by one
        end_point = builder->CreateSub(end_point, llvm::ConstantInt::get(
                                    context, llvm::APInt(32, 1)));
        builder->CreateStore(end_point, end_point_ptr);
        return tmp;
    }

    llvm::Value* LLVMList::pop_position(ASR::expr_t* list_expr, llvm::Value* list, llvm::Value* pos,
        ASR::ttype_t* list_element_type, llvm::Module* module,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        /* Equivalent in C++:
         * while(end_point > pos + 1) {
         *     tmp = pos + 1;
         *     list[pos] = list[tmp];
         *     pos = tmp;
         * }
         */
        std::string el_type_code = ASRUtils::get_type_code(list_element_type);
        llvm::Type* list_type = std::get<0>(typecode2listtype[el_type_code]);

        llvm::Value* end_point_ptr = get_pointer_to_current_end_point_using_type(list_type, list);
        llvm::Value* end_point = llvm_utils->CreateLoad2(
            llvm::Type::getInt32Ty(context),end_point_ptr);

        llvm::AllocaInst *pos_ptr = llvm_utils->CreateAlloca(
                                    llvm::Type::getInt32Ty(context));
        LLVM::CreateStore(*builder, pos, pos_ptr);

        // Get element to return
        llvm::Value* item = read_item_using_ttype(list_element_type, list, llvm_utils->CreateLoad(pos_ptr),
                                      true, module, LLVM::is_llvm_struct(list_element_type));
        if( LLVM::is_llvm_struct(list_element_type) ) {
            LCOMPILERS_ASSERT(typecode2listtype.find(el_type_code) != typecode2listtype.end());
            llvm::AllocaInst *target = llvm_utils->CreateAlloca(
                std::get<2>(typecode2listtype[el_type_code]), nullptr,
                "pop_position_item");
            llvm_utils->deepcopy(list_expr, item, target, list_element_type, list_element_type, module, name2memidx);
            item = target;
        }

        llvm::Value* num_elements = builder->CreateSub(end_point, pos);
        llvm::Value* num_elements_shift = builder->CreateSub(num_elements, 
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 1));

        llvm::Value* cond = builder->CreateICmpEQ(num_elements, llvm::ConstantInt::get(
                                                   context, llvm::APInt(32, 1)));

        llvm_utils->create_if_else(cond, [&](){}, [&](){
            llvm::Value *dest_ptr = read_item_using_ttype(list_element_type,
                                                             list, pos, true, module, true);
            llvm::Value *src_ptr = read_item_using_ttype(list_element_type, list, 
                builder->CreateAdd(pos, llvm::ConstantInt::get(context, llvm::APInt(32, 1))),
                true, module, true);
            int32_t type_size = std::get<1>(typecode2listtype[el_type_code]);
            llvm::Value* llvm_type_size = llvm::ConstantInt::get(context, llvm::APInt(32, type_size));

        llvm::Value* bytes_to_move = builder->CreateMul(llvm_type_size, num_elements_shift);
            builder->CreateMemMove(dest_ptr, llvm::MaybeAlign(), src_ptr, llvm::MaybeAlign(), bytes_to_move);
        });

        // Decrement end point by one
        end_point = builder->CreateSub(end_point, llvm::ConstantInt::get(
                                       context, llvm::APInt(32, 1)));
        builder->CreateStore(end_point, end_point_ptr);

        return item;
    }

    void LLVMList::list_clear_using_type(llvm::Type* list_type, llvm::Value* list) {
        llvm::Value* end_point_ptr = get_pointer_to_current_end_point_using_type(list_type, list);
        llvm::Value* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                   llvm::APInt(32, 0));
        LLVM::CreateStore(*builder, zero, end_point_ptr);
    }

    void LLVMList::free_data_using_type(llvm::Type* list_type, llvm::Value* list, llvm::Module* module) {
        llvm::Value* data = llvm_utils->CreateLoad(get_pointer_to_list_data_using_type(list_type, list));
        LLVM::lfortran_free(context, *module, *builder, data);
    }


    void LLVMList::free_data_using_type(std::string& type_code, llvm::Value* list, llvm::Module* module) {
        llvm::Type* list_type = get_list_type(nullptr, type_code, 0);
        llvm::Type* list_el_type = std::get<2>(typecode2listtype[type_code]);
        llvm::Value* data = llvm_utils->CreateLoad2(list_el_type->getPointerTo(), get_pointer_to_list_data_using_type(list_type, list));
        LLVM::lfortran_free(context, *module, *builder, data);
    }

    llvm::Value* LLVMList::check_list_equality(llvm::Value* l1, llvm::Value* l2,
                                                ASR::ttype_t* item_type,
                                                 llvm::LLVMContext& context,
                                                 llvm::IRBuilder<>* builder,
                                                 llvm::Module* module) {
        llvm::AllocaInst *is_equal = llvm_utils->CreateAlloca(llvm::Type::getInt1Ty(context));
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(context, llvm::APInt(1, 1)), is_equal);

        std::string type_code = ASRUtils::get_type_code(item_type);
        llvm::Type* list_type = get_list_type(nullptr, type_code, 0);
        llvm::Value *a_len = llvm_utils->list_api->len_using_type(list_type, l1);
        llvm::Value *b_len = llvm_utils->list_api->len_using_type(list_type, l2);
        llvm::Value *cond = builder->CreateICmpEQ(a_len, b_len);
        llvm::Function *fn = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", fn);
        llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context, "else");
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifcont");
        builder->CreateCondBr(cond, thenBB, elseBB);
        builder->SetInsertPoint(thenBB);
        llvm::AllocaInst *idx = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(
                                    context, llvm::APInt(32, 0)), idx);
        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

            // head
           llvm_utils->start_new_block(loophead);
            {
                llvm::Value* i = llvm_utils->CreateLoad(idx);
                llvm::Value* cnd = builder->CreateICmpSLT(i, a_len);
                builder->CreateCondBr(cnd, loopbody, loopend);
            }

            // body
            llvm_utils->start_new_block(loopbody);
            {
                llvm::Value* i = llvm_utils->CreateLoad(idx);
                llvm::Value* left_arg = llvm_utils->list_api->read_item_using_ttype(item_type, l1, i,
                        false, module, LLVM::is_llvm_struct(item_type));
                llvm::Value* right_arg = llvm_utils->list_api->read_item_using_ttype(item_type, l2, i,
                        false, module, LLVM::is_llvm_struct(item_type));
                llvm::Value* res = llvm_utils->is_equal_by_value(left_arg, right_arg, module,
                                        item_type);
                res = builder->CreateAnd(llvm_utils->CreateLoad(is_equal), res);
                LLVM::CreateStore(*builder, res, is_equal);
                i = builder->CreateAdd(i, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                        llvm::APInt(32, 1)));
                LLVM::CreateStore(*builder, i, idx);
            }

            builder->CreateBr(loophead);

            // end
            llvm_utils->start_new_block(loopend);

        builder->CreateBr(mergeBB);
        llvm_utils->start_new_block(elseBB);
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(context, llvm::APInt(1, 0)), is_equal);
        llvm_utils->start_new_block(mergeBB);
        return llvm_utils->CreateLoad(is_equal);
    }

    llvm::Value* LLVMList::check_list_inequality(llvm::Value* l1, llvm::Value* l2,
                                                 ASR::ttype_t* item_type,
                                                 llvm::LLVMContext& context,
                                                 llvm::IRBuilder<>* builder,
                                                 llvm::Module* module, int8_t overload_id,
                                                 ASR::ttype_t* int32_type) {
        /**
         * Equivalent in C++
         *
         * equality_holds = 1;
         * inequality_holds = 0;
         * i = 0;
         *
         * while( i < a_len && i < b_len && equality_holds ) {
         *     equality_holds &= (a[i] == b[i]);
         *     inequality_holds |= (a[i] op b[i]);
         *     i++;
         * }
         *
         * if( (i == a_len || i == b_len) && equality_holds ) {
         *     inequality_holds = a_len op b_len;
         * }
         *
         */

        std::string el_type_code = ASRUtils::get_type_code(item_type);
        llvm::Type* list_type = get_list_type(nullptr, el_type_code, 0);

        llvm::AllocaInst *equality_holds = llvm_utils->CreateAlloca(
                                                llvm::Type::getInt1Ty(context));
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(context, llvm::APInt(1, 1)),
                          equality_holds);
        llvm::AllocaInst *inequality_holds = llvm_utils->CreateAlloca(
                                                llvm::Type::getInt1Ty(context));
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(context, llvm::APInt(1, 0)),
                          inequality_holds);

        llvm::Value *a_len = llvm_utils->list_api->len_using_type(list_type, l1);
        llvm::Value *b_len = llvm_utils->list_api->len_using_type(list_type, l2);
        llvm::AllocaInst *idx = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(
                                    context, llvm::APInt(32, 0)), idx);
        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value* i = llvm_utils->CreateLoad(idx);
            llvm::Value* cnd = builder->CreateICmpSLT(i, a_len);
            cnd = builder->CreateAnd(cnd, builder->CreateICmpSLT(i, b_len));
            cnd = builder->CreateAnd(cnd, llvm_utils->CreateLoad(equality_holds));
            builder->CreateCondBr(cnd, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* i = llvm_utils->CreateLoad(idx);
            llvm::Value* left_arg = llvm_utils->list_api->read_item_using_ttype(item_type, l1, i,
                    false, module, LLVM::is_llvm_struct(item_type));
            llvm::Value* right_arg = llvm_utils->list_api->read_item_using_ttype(item_type, l2, i,
                    false, module, LLVM::is_llvm_struct(item_type));
            llvm::Value* res = llvm_utils->is_ineq_by_value(left_arg, right_arg, module,
                                    item_type, overload_id);
            res = builder->CreateOr(llvm_utils->CreateLoad(inequality_holds), res);
            LLVM::CreateStore(*builder, res, inequality_holds);
            res = llvm_utils->is_equal_by_value(left_arg, right_arg, module,
                                    item_type);
            res = builder->CreateAnd(llvm_utils->CreateLoad(equality_holds), res);
            LLVM::CreateStore(*builder, res, equality_holds);
            i = builder->CreateAdd(i, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                    llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, i, idx);
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);

        llvm::Value* cond = builder->CreateICmpEQ(llvm_utils->CreateLoad(idx),
                                                  a_len);
        cond = builder->CreateOr(cond, builder->CreateICmpEQ(
                                 llvm_utils->CreateLoad(idx), b_len));
        cond = builder->CreateAnd(cond, llvm_utils->CreateLoad(equality_holds));
        llvm_utils->create_if_else(cond, [&]() {
            LLVM::CreateStore(*builder, llvm_utils->is_ineq_by_value(a_len, b_len,
                module, int32_type, overload_id), inequality_holds);
        }, []() {
            // LLVM::CreateStore(*builder, llvm::ConstantInt::get(
            //     context, llvm::APInt(1, 0)), inequality_holds);
        });

        return llvm_utils->CreateLoad(inequality_holds);
    }

    void LLVMList::list_repeat_copy(ASR::List_t* list_type, llvm::Value* repeat_list, llvm::Value* init_list,
                                    llvm::Value* num_times, llvm::Value* init_list_len,
                                    llvm::Module* module) {
        llvm::Type* pos_type = llvm::Type::getInt32Ty(context);
        std::string el_type_code = ASRUtils::get_type_code(list_type->m_type);
        llvm::AllocaInst *i = llvm_utils->CreateAlloca(pos_type);
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(
                                    context, llvm::APInt(32, 0)), i);       // i = 0
        llvm::AllocaInst *j = llvm_utils->CreateAlloca(pos_type);
        llvm::Value* tmp = nullptr;

        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpSGT(num_times,
                                                       llvm_utils->CreateLoad(i));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            LLVM::CreateStore(*builder, llvm::ConstantInt::get(
                                        context, llvm::APInt(32, 0)), j);       // j = 0

            llvm::BasicBlock *loop2head = llvm::BasicBlock::Create(context, "loop2.head");
            llvm::BasicBlock *loop2body = llvm::BasicBlock::Create(context, "loop2.body");
            llvm::BasicBlock *loop2end = llvm::BasicBlock::Create(context, "loop2.end");

            // head
            llvm_utils->start_new_block(loop2head);
            {
                llvm::Value *cond2 = builder->CreateICmpSGT(init_list_len,
                                                            llvm_utils->CreateLoad(j));
                builder->CreateCondBr(cond2, loop2body, loop2end);
            }

            // body
            llvm_utils->start_new_block(loop2body);
            {
                tmp = builder->CreateMul(init_list_len, llvm_utils->CreateLoad(i));
                tmp = builder->CreateAdd(tmp, llvm_utils->CreateLoad(j));
                write_item_using_ttype(list_type->m_type, repeat_list, tmp,
                           read_item_using_ttype(list_type->m_type, init_list, llvm_utils->CreateLoad(j),
                                     false, module, false),
                           false, module);
                tmp = builder->CreateAdd(
                            llvm_utils->CreateLoad(j),
                            llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
                LLVM::CreateStore(*builder, tmp, j);
            }
            builder->CreateBr(loop2head);

            // end
            llvm_utils->start_new_block(loop2end);

            tmp = builder->CreateAdd(
                        llvm_utils->CreateLoad(i),
                        llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, tmp, i);
        }
        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);
    }

    LLVMTuple::LLVMTuple(llvm::LLVMContext& context_,
                         LLVMUtils* llvm_utils_,
                         llvm::IRBuilder<>* /*builder_*/) :
    context(context_), llvm_utils(llvm_utils_) {}

    llvm::Type* LLVMTuple::get_tuple_type(std::string& type_code,
                                          std::vector<llvm::Type*>& el_types) {
        if( typecode2tupletype.find(type_code) != typecode2tupletype.end() ) {
            return typecode2tupletype[type_code].first;
        }

        llvm::Type* llvm_tuple_type = llvm::StructType::create(context, el_types, "tuple");
        typecode2tupletype[type_code] = std::make_pair(llvm_tuple_type, el_types.size());
        return llvm_tuple_type;
    }

    llvm::Value* LLVMTuple::read_item(llvm::Value* llvm_tuple, ASR::Tuple_t* tuple_type,
                                      size_t pos, bool get_pointer) {
        llvm::Value* llvm_pos = llvm::ConstantInt::get(context, llvm::APInt(32, pos));
        llvm::Type* el_type = llvm_utils->get_type_from_ttype_t_util(nullptr, tuple_type->m_type[pos], llvm_utils->module);
        return read_item_using_pos_value(el_type, llvm_tuple, tuple_type, llvm_pos, get_pointer);
    }

    llvm::Value* LLVMTuple::read_item_using_pos_value(llvm::Type* el_type, llvm::Value* llvm_tuple, ASR::Tuple_t* tuple_type, llvm::Value* pos,
                                      bool get_pointer) {
        llvm::Type* llvm_tuple_type = llvm_utils->get_type_from_ttype_t_util(nullptr, (ASR::ttype_t*)tuple_type, llvm_utils->module);
        llvm::Value* item = llvm_utils->create_gep2(llvm_tuple_type, llvm_tuple, pos);
        if( get_pointer ) {
            return item;
        }
        return llvm_utils->CreateLoad2(el_type, item);
    }

    llvm::Value* LLVMTuple::read_item_using_pos(llvm::Type* el_type, llvm::Value* llvm_tuple, ASR::Tuple_t* tuple_type, size_t pos,
                                      bool get_pointer) {
        llvm::Value* llvm_pos = llvm::ConstantInt::get(context, llvm::APInt(32, pos));
        return read_item_using_pos_value(el_type, llvm_tuple, tuple_type, llvm_pos, get_pointer);
    }

    void LLVMTuple::tuple_init([[maybe_unused]] ASR::expr_t* tuple_expr, llvm::Value* llvm_tuple, std::vector<llvm::Value*>& values,
        ASR::Tuple_t* tuple_type, llvm::Module* module,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        for( size_t i = 0; i < values.size(); i++ ) {
            llvm::Type* el_type = llvm_utils->get_type_from_ttype_t_util(nullptr, tuple_type->m_type[i], module); 
            llvm::Value* item_ptr = read_item_using_pos(el_type, llvm_tuple, tuple_type, i, true);
            llvm_utils->deepcopy(nullptr, values[i], item_ptr,
                                 tuple_type->m_type[i],
                                 tuple_type->m_type[i],
                                 module,
                                 name2memidx);
        }
    }

    void LLVMTuple::tuple_deepcopy([[maybe_unused]] ASR::expr_t* src_expr, llvm::Value* src, llvm::Value* dest,
        ASR::Tuple_t* tuple_type, llvm::Module* module,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        LCOMPILERS_ASSERT(src->getType() == dest->getType());
        for( size_t i = 0; i < tuple_type->n_type; i++ ) {
            llvm::Type* el_type = llvm_utils->get_type_from_ttype_t_util(nullptr, tuple_type->m_type[i], module); 
            llvm::Value* src_item = read_item_using_pos(el_type, src, tuple_type, i, LLVM::is_llvm_struct(
                                              tuple_type->m_type[i]));
            llvm::Value* dest_item_ptr = read_item_using_pos(el_type, dest, tuple_type, i, true);
            llvm_utils->deepcopy(nullptr, src_item, dest_item_ptr,
                                 tuple_type->m_type[i],
                                 tuple_type->m_type[i],
                                 module,
                                 name2memidx);
        }
    }

    llvm::Value* LLVMTuple::check_tuple_equality(llvm::Value* t1, llvm::Value* t2,
                                                 ASR::Tuple_t* tuple_type,
                                                 llvm::LLVMContext& context,
                                                 llvm::IRBuilder<>* builder,
                                                 llvm::Module* module) {
        llvm::Value* is_equal = llvm::ConstantInt::get(context, llvm::APInt(1, 1));
        for( size_t i = 0; i < tuple_type->n_type; i++ ) {
            llvm::Value* t1i = llvm_utils->tuple_api->read_item(t1, tuple_type, i, LLVM::is_llvm_struct(
                                              tuple_type->m_type[i]));
            llvm::Value* t2i = llvm_utils->tuple_api->read_item(t2, tuple_type, i, LLVM::is_llvm_struct(
                                              tuple_type->m_type[i]));
            llvm::Value* is_t1_eq_t2 = llvm_utils->is_equal_by_value(t1i, t2i, module,
                                        tuple_type->m_type[i]);
            is_equal = builder->CreateAnd(is_equal, is_t1_eq_t2);
        }
        return is_equal;
    }

    llvm::Value* LLVMTuple::check_tuple_inequality(llvm::Value* t1, llvm::Value* t2,
                                                 ASR::Tuple_t* tuple_type,
                                                 llvm::LLVMContext& context,
                                                 llvm::IRBuilder<>* builder,
                                                 llvm::Module* module, int8_t overload_id) {
        /**
         * Equivalent in C++
         *
         * equality_holds = 1;
         * inequality_holds = 0;
         * i = 0;
         *
         * // owing to compile-time access of indices,
         * // loop is unrolled into multiple if statements
         * while( i < a_len && equality_holds ) {
         *     inequality_holds |= (a[i] op b[i]);
         *     equality_holds &= (a[i] == b[i]);
         *     i++;
         * }
         *
         * return inequality_holds;
         *
         */

        llvm::AllocaInst *equality_holds = llvm_utils->CreateAlloca(
                                                llvm::Type::getInt1Ty(context));
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(context, llvm::APInt(1, 1)),
                          equality_holds);
        llvm::AllocaInst *inequality_holds = llvm_utils->CreateAlloca(
                                                llvm::Type::getInt1Ty(context));
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(context, llvm::APInt(1, 0)),
                          inequality_holds);

        for( size_t i = 0; i < tuple_type->n_type; i++ ) {
            llvm_utils->create_if_else(llvm_utils->CreateLoad2(llvm::Type::getInt1Ty(context), equality_holds), [&]() {
                llvm::Value* t1i = llvm_utils->tuple_api->read_item(t1, tuple_type, i, LLVM::is_llvm_struct(
                                                tuple_type->m_type[i]));
                llvm::Value* t2i = llvm_utils->tuple_api->read_item(t2, tuple_type, i, LLVM::is_llvm_struct(
                                                tuple_type->m_type[i]));
                llvm::Value* res = llvm_utils->is_ineq_by_value(t1i, t2i, module,
                                                tuple_type->m_type[i], overload_id);
                res = builder->CreateOr(llvm_utils->CreateLoad2(llvm::Type::getInt1Ty(context), inequality_holds), res);
                LLVM::CreateStore(*builder, res, inequality_holds);
                res = llvm_utils->is_equal_by_value(t1i, t2i, module, tuple_type->m_type[i]);
                res = builder->CreateAnd(llvm_utils->CreateLoad2(llvm::Type::getInt1Ty(context), equality_holds), res);
                LLVM::CreateStore(*builder, res, equality_holds);
            }, [](){});
        }

        return llvm_utils->CreateLoad2(llvm::Type::getInt1Ty(context), inequality_holds);
    }

    void LLVMTuple::concat(ASR::expr_t* tuple_1_expr, llvm::Value* t1, llvm::Value* t2, ASR::Tuple_t* tuple_type_1,
                           ASR::Tuple_t* tuple_type_2, llvm::Value* concat_tuple,
                           ASR::Tuple_t* concat_tuple_type, llvm::Module* module,
                           std::map<std::string, std::map<std::string, int>>& name2memidx) {
        std::vector<llvm::Value*> values;
        for( size_t i = 0; i < tuple_type_1->n_type; i++ ) {
            values.push_back(llvm_utils->tuple_api->read_item(t1, tuple_type_1, i,
                LLVM::is_llvm_struct(tuple_type_1->m_type[i])));
        }
        for( size_t i = 0; i < tuple_type_2->n_type; i++ ) {
            values.push_back(llvm_utils->tuple_api->read_item(t2, tuple_type_2, i,
                LLVM::is_llvm_struct(tuple_type_2->m_type[i])));
        }
        tuple_init(tuple_1_expr, concat_tuple, values, concat_tuple_type,
                   module, name2memidx);
    }

    LLVMSetInterface::LLVMSetInterface(llvm::LLVMContext& context_,
        LLVMUtils* llvm_utils_,
        llvm::IRBuilder<>* builder_):
        context(context_),
        llvm_utils(std::move(llvm_utils_)),
        builder(std::move(builder_)),
        pos_ptr(nullptr), is_el_matching_var(nullptr),
        idx_ptr(nullptr), hash_iter(nullptr),
        hash_value(nullptr), polynomial_powers(nullptr),
        chain_itr(nullptr), chain_itr_prev(nullptr),
        old_capacity(nullptr), old_elems(nullptr),
        old_el_mask(nullptr), is_set_present_(false) {
    }

    bool LLVMSetInterface::is_set_present() {
        return is_set_present_;
    }

    void LLVMSetInterface::set_is_set_present(bool value) {
        is_set_present_ = value;
    }

    LLVMSetLinearProbing::LLVMSetLinearProbing(llvm::LLVMContext& context_,
        LLVMUtils* llvm_utils_,
        llvm::IRBuilder<>* builder_):
        LLVMSetInterface(context_, llvm_utils_, builder_) {
    }

    LLVMSetSeparateChaining::LLVMSetSeparateChaining(
        llvm::LLVMContext& context_,
        LLVMUtils* llvm_utils_,
        llvm::IRBuilder<>* builder_):
        LLVMSetInterface(context_, llvm_utils_, builder_) {
    }

    LLVMSetInterface::~LLVMSetInterface() {
        typecode2settype.clear();
    }

    LLVMSetLinearProbing::~LLVMSetLinearProbing() {
    }

    LLVMSetSeparateChaining::~LLVMSetSeparateChaining() {
    }

    llvm::Value* LLVMSetLinearProbing::get_pointer_to_occupancy(llvm::Value* set) {
        return llvm_utils->create_gep(set, 0);
    }

    llvm::Value* LLVMSetLinearProbing::get_pointer_to_capacity_using_type(llvm::Type* el_list_type, llvm::Value* set) {
        return llvm_utils->list_api->get_pointer_to_current_capacity_using_type(
                            el_list_type, get_el_list(set));
    }

    llvm::Value* LLVMSetLinearProbing::get_el_list(llvm::Value* set) {
        return llvm_utils->create_gep(set, 1);
    }

    llvm::Value* LLVMSetLinearProbing::get_pointer_to_occupancy_using_type(llvm::Type* set_type, llvm::Value* set) {
        return llvm_utils->create_gep2(set_type, set, 0);
    }

    llvm::Value* LLVMSetLinearProbing::get_pointer_to_capacity_using_typecode(std::string& type_code, llvm::Value* set) {
        return llvm_utils->list_api->get_pointer_to_current_capacity_using_type(
                            llvm_utils->list_api->get_list_type(nullptr, type_code, 0),
                            get_el_list(set));
    }

    llvm::Value* LLVMSetLinearProbing::get_pointer_to_mask(llvm::Value* set) {
        return llvm_utils->create_gep(set, 2);
    }

    llvm::Value* LLVMSetSeparateChaining::get_el_list(llvm::Value* /*set*/) {
        return nullptr;
    }

    llvm::Value* LLVMSetSeparateChaining::get_pointer_to_occupancy(llvm::Value* set) {
        return llvm_utils->create_gep(set, 0);
    }

    llvm::Value* LLVMSetSeparateChaining::get_pointer_to_occupancy_using_type(llvm::Type* set_type,llvm::Value* set) {
        return llvm_utils->create_gep2(set_type, set, 0);
    }


    llvm::Value* LLVMSetSeparateChaining::get_pointer_to_capacity_using_typecode(std::string& type_code, llvm::Value* set) {
        llvm::Type* set_type = get_set_type(type_code, 0, nullptr); 
        return llvm_utils->create_gep2(set_type, set, 2);
    }

    llvm::Value* LLVMSetSeparateChaining::get_pointer_to_number_of_filled_buckets(llvm::Value* set) {
        return llvm_utils->create_gep(set, 1);
    }

    llvm::Value* LLVMSetSeparateChaining::get_pointer_to_capacity_using_type(llvm::Type* el_list_type, llvm::Value* set) {
        return llvm_utils->create_gep2(el_list_type, set, 2);
    }

    llvm::Value* LLVMSetSeparateChaining::get_pointer_to_elems(llvm::Value* set) {
        return llvm_utils->create_gep(set, 3);
    }

    llvm::Value* LLVMSetSeparateChaining::get_pointer_to_mask(llvm::Value* set) {
        return llvm_utils->create_gep(set, 4);
    }

    llvm::Value* LLVMSetSeparateChaining::get_pointer_to_rehash_flag(llvm::Value* set) {
        return llvm_utils->create_gep(set, 5);
    }

    llvm::Type* LLVMSetLinearProbing::get_set_type(std::string type_code, int32_t type_size,
        llvm::Type* el_type) {
        is_set_present_ = true;
        if( typecode2settype.find(type_code) != typecode2settype.end() ) {
            return std::get<0>(typecode2settype[type_code]);
        }

        llvm::Type* el_list_type = llvm_utils->list_api->get_list_type(el_type,
                                        type_code, type_size);
        std::vector<llvm::Type*> set_type_vec = {llvm::Type::getInt32Ty(context),
                                                el_list_type,
                                                llvm::Type::getInt8Ty(context)->getPointerTo()};
        llvm::Type* set_desc = llvm::StructType::create(context, set_type_vec, "set");
        typecode2settype[type_code] = std::make_tuple(set_desc, type_size, el_type);
        return set_desc;
    }

    llvm::Type* LLVMSetSeparateChaining::get_set_type(
        std::string el_type_code, int32_t el_type_size, llvm::Type* el_type) {
        is_set_present_ = true;
        if( typecode2settype.find(el_type_code) != typecode2settype.end() ) {
            return std::get<0>(typecode2settype[el_type_code]);
        }

        std::vector<llvm::Type*> el_vec = {el_type, llvm::Type::getInt8Ty(context)->getPointerTo()};
        llvm::Type* elstruct = llvm::StructType::create(context, el_vec, "el");
        std::vector<llvm::Type*> set_type_vec = {llvm::Type::getInt32Ty(context),
                                                  llvm::Type::getInt32Ty(context),
                                                  llvm::Type::getInt32Ty(context),
                                                  elstruct->getPointerTo(),
                                                  llvm::Type::getInt8Ty(context)->getPointerTo(),
                                                  llvm::Type::getInt1Ty(context)};
        llvm::Type* set_desc = llvm::StructType::create(context, set_type_vec, "set");
        typecode2settype[el_type_code] = std::make_tuple(set_desc, el_type_size, el_type);
        typecode2elstruct[el_type_code] = elstruct;
        return set_desc;
    }

    void LLVMSetLinearProbing::set_init(std::string type_code, llvm::Value* set,
                           llvm::Module* module, size_t initial_capacity) {
        llvm::Value* n_ptr = get_pointer_to_occupancy(set);
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                           llvm::APInt(32, 0)), n_ptr);
        llvm::Value* el_list = get_el_list(set);
        llvm_utils->list_api->list_init(type_code, el_list, module,
                                        initial_capacity, initial_capacity);
        llvm::DataLayout data_layout(module->getDataLayout());
        size_t mask_size = data_layout.getTypeAllocSize(llvm::Type::getInt8Ty(context));
        llvm::Value* llvm_capacity = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                            llvm::APInt(32, initial_capacity));
        llvm::Value* llvm_mask_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                            llvm::APInt(32, mask_size));
        llvm::Value* el_mask = LLVM::lfortran_calloc(context, *module, *builder, llvm_capacity,
                                                      llvm_mask_size);
        LLVM::CreateStore(*builder, el_mask, get_pointer_to_mask(set));
    }

    void LLVMSetSeparateChaining::set_init(
        std::string el_type_code, llvm::Value* set,
        llvm::Module* module, size_t initial_capacity) {
        llvm::Value* llvm_capacity = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                        llvm::APInt(32, initial_capacity));
        llvm::Value* rehash_flag_ptr = get_pointer_to_rehash_flag(set);
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(llvm::Type::getInt1Ty(context),
                            llvm::APInt(1, 1)), rehash_flag_ptr);
        set_init_given_initial_capacity(el_type_code, set, module, llvm_capacity);
    }

    void LLVMSetSeparateChaining::set_init_given_initial_capacity(
        std::string el_type_code, llvm::Value* set,
        llvm::Module* module, llvm::Value* llvm_capacity) {
        llvm::Value* rehash_flag_ptr = get_pointer_to_rehash_flag(set);
        llvm::Value* rehash_flag = llvm_utils->CreateLoad(rehash_flag_ptr);
        llvm::Value* llvm_zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 0));
        llvm::Value* occupancy_ptr = get_pointer_to_occupancy(set);
        LLVM::CreateStore(*builder, llvm_zero, occupancy_ptr);
        llvm::Value* num_buckets_filled_ptr = get_pointer_to_number_of_filled_buckets(set);
        LLVM::CreateStore(*builder, llvm_zero, num_buckets_filled_ptr);

        llvm::DataLayout data_layout(module->getDataLayout());
        llvm::Type* el_type = typecode2elstruct[el_type_code];
        llvm::Type* el_list_type = llvm_utils->list_api->get_list_type(nullptr, el_type_code, 0);
        size_t el_type_size = data_layout.getTypeAllocSize(el_type);
        llvm::Value* llvm_el_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, el_type_size));
        llvm::Value* malloc_size = builder->CreateMul(llvm_capacity, llvm_el_size);
        llvm::Value* el_ptr = LLVM::lfortran_malloc(context, *module, *builder, malloc_size);
        rehash_flag = builder->CreateAnd(rehash_flag,
                        builder->CreateICmpNE(el_ptr,
                        llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()))
                    );
        el_ptr = builder->CreateBitCast(el_ptr, el_type->getPointerTo());
        LLVM::CreateStore(*builder, el_ptr, get_pointer_to_elems(set));

        size_t mask_size = data_layout.getTypeAllocSize(llvm::Type::getInt8Ty(context));
        llvm::Value* llvm_mask_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                            llvm::APInt(32, mask_size));
        llvm::Value* el_mask = LLVM::lfortran_calloc(context, *module, *builder, llvm_capacity,
                                                      llvm_mask_size);
        rehash_flag = builder->CreateAnd(rehash_flag,
                        builder->CreateICmpNE(el_mask,
                        llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()))
                    );
        LLVM::CreateStore(*builder, el_mask, get_pointer_to_mask(set));

        llvm::Value* capacity_ptr = get_pointer_to_capacity_using_type(el_list_type, set);
        LLVM::CreateStore(*builder, llvm_capacity, capacity_ptr);
        LLVM::CreateStore(*builder, rehash_flag, rehash_flag_ptr);
    }

    llvm::Value* LLVMSetInterface::get_el_hash(
        llvm::Value* capacity, llvm::Value* el,
        ASR::ttype_t* el_asr_type, llvm::Module* module) {
        // Write specialised hash functions for intrinsic types
        // This is to avoid unnecessary calls to C-runtime and do
        // as much as possible in LLVM directly.
        switch( el_asr_type->type ) {
            case ASR::ttypeType::Integer: {
                // Simple modulo with the capacity of the set.
                // We can update it later to do a better hash function
                // which produces lesser collisions.

                llvm::Value* int_hash = builder->CreateZExtOrTrunc(
                    builder->CreateURem(el,
                    builder->CreateZExtOrTrunc(capacity, el->getType())),
                    capacity->getType()
                );
                return int_hash;
            }
            case ASR::ttypeType::String: {
                // Polynomial rolling hash function for strings
                llvm::Value* null_char = llvm::ConstantInt::get(llvm::Type::getInt8Ty(context),
                                                                llvm::APInt(8, '\0'));
                llvm::Value* p = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 31));
                llvm::Value* m = llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 100000009));
                hash_value = llvm_utils->CreateAlloca(llvm::Type::getInt64Ty(context), nullptr, "hash_value");
                hash_iter = llvm_utils->CreateAlloca(llvm::Type::getInt64Ty(context), nullptr, "hash_iter");
                polynomial_powers = llvm_utils->CreateAlloca(llvm::Type::getInt64Ty(context), nullptr, "p_pow");
                LLVM::CreateStore(*builder,
                    llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 0)),
                    hash_value);
                LLVM::CreateStore(*builder,
                    llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 1)),
                    polynomial_powers);
                LLVM::CreateStore(*builder,
                    llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 0)),
                    hash_iter);
                llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
                llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
                llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

                // head
                llvm_utils->start_new_block(loophead);
                {
                    llvm::Value* i = llvm_utils->CreateLoad(hash_iter);
                    llvm::Value* c = llvm_utils->CreateLoad(llvm_utils->create_ptr_gep(el, i));
                    llvm::Value *cond = builder->CreateICmpNE(c, null_char);
                    builder->CreateCondBr(cond, loopbody, loopend);
                }

                // body
                llvm_utils->start_new_block(loopbody);
                {
                    // for c in el:
                    //     hash_value = (hash_value + (ord(c) + 1) * p_pow) % m
                    //     p_pow = (p_pow * p) % m
                    llvm::Value* i = llvm_utils->CreateLoad(hash_iter);
                    llvm::Value* c = llvm_utils->CreateLoad(llvm_utils->create_ptr_gep(el, i));
                    llvm::Value* p_pow = llvm_utils->CreateLoad(polynomial_powers);
                    llvm::Value* hash = llvm_utils->CreateLoad(hash_value);
                    c = builder->CreateZExt(c, llvm::Type::getInt64Ty(context));
                    c = builder->CreateAdd(c, llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 1)));
                    c = builder->CreateMul(c, p_pow);
                    c = builder->CreateSRem(c, m);
                    hash = builder->CreateAdd(hash, c);
                    hash = builder->CreateSRem(hash, m);
                    LLVM::CreateStore(*builder, hash, hash_value);
                    p_pow = builder->CreateMul(p_pow, p);
                    p_pow = builder->CreateSRem(p_pow, m);
                    LLVM::CreateStore(*builder, p_pow, polynomial_powers);
                    i = builder->CreateAdd(i, llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), llvm::APInt(64, 1)));
                    LLVM::CreateStore(*builder, i, hash_iter);
                }

                builder->CreateBr(loophead);

                // end
                llvm_utils->start_new_block(loopend);
                llvm::Value* hash = llvm_utils->CreateLoad(hash_value);
                hash = builder->CreateTrunc(hash, llvm::Type::getInt32Ty(context));
                return builder->CreateSRem(hash, capacity);
            }
            case ASR::ttypeType::Tuple: {
                llvm::Value* tuple_hash = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 0));
                ASR::Tuple_t* asr_tuple = ASR::down_cast<ASR::Tuple_t>(el_asr_type);
                for( size_t i = 0; i < asr_tuple->n_type; i++ ) {
                    llvm::Value* llvm_tuple_i = llvm_utils->tuple_api->read_item(el, asr_tuple, i,
                                                    LLVM::is_llvm_struct(asr_tuple->m_type[i]));
                    tuple_hash = builder->CreateAdd(tuple_hash, get_el_hash(capacity, llvm_tuple_i,
                                                                             asr_tuple->m_type[i], module));
                    tuple_hash = builder->CreateSRem(tuple_hash, capacity);
                }
                return tuple_hash;
            }
            case ASR::ttypeType::Logical: {
                return builder->CreateZExt(el, llvm::Type::getInt32Ty(context));
            }
            default: {
                throw LCompilersException("Hashing " + ASRUtils::type_to_str_python(el_asr_type) +
                                          " isn't implemented yet.");
            }
        }
    }

    void LLVMSetLinearProbing::resolve_collision(
        llvm::Value* capacity, llvm::Value* el_hash,
        llvm::Value* el, llvm::Value* el_list,
        llvm::Value* el_mask, llvm::Module* module,
        ASR::ttype_t* el_asr_type, bool for_read) {

        /**
         * C++ equivalent:
         *
         * pos = el_hash;
         *
         * while( true ) {
         *     is_el_skip = el_mask_value == 3;     // tombstone
         *     is_el_set = el_mask_value != 0;
         *     is_el_matching = 0;
         *
         *     compare_elems = is_el_set && !is_el_skip;
         *     if( compare_elems ) {
         *         original_el = el_list[pos];
         *         is_el_matching = el == original_el;
         *     }
         *
         *     cond;
         *     if( for_read ) {
         *         // for reading, continue to next pos
         *         // even if current pos is tombstone
         *         cond = (is_el_set && !is_el_matching) || is_el_skip;
         *     }
         *     else {
         *         // for writing, do not continue
         *         // if current pos is tombstone
         *         cond = is_el_set && !is_el_matching && !is_el_skip;
         *     }
         *
         *     if( cond ) {
         *         pos += 1;
         *         pos %= capacity;
         *     }
         *     else {
         *         break;
         *     }
         * }
         *
         */

        if( !for_read ) {
            pos_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        }
        is_el_matching_var = llvm_utils->CreateAlloca(llvm::Type::getInt1Ty(context));
        std::string el_type_code = ASRUtils::get_type_code(el_asr_type);

        LLVM::CreateStore(*builder, el_hash, pos_ptr);

        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value* pos = llvm_utils->CreateLoad(pos_ptr);
            llvm::Value* el_mask_value = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context),
                llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), el_mask, pos));
            llvm::Value* is_el_skip = builder->CreateICmpEQ(el_mask_value,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 3)));
            llvm::Value* is_el_set = builder->CreateICmpNE(el_mask_value,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0)));
            llvm::Value* is_el_matching = llvm::ConstantInt::get(llvm::Type::getInt1Ty(context),
                                                                llvm::APInt(1, 0));
            LLVM::CreateStore(*builder, is_el_matching, is_el_matching_var);
            llvm::Value* compare_elems = builder->CreateAnd(is_el_set,
                                            builder->CreateNot(is_el_skip));
            llvm_utils->create_if_else(compare_elems, [&]() {
                llvm::Value* original_el = llvm_utils->list_api->read_item_using_ttype(el_asr_type, el_list, pos,
                                false, module, LLVM::is_llvm_struct(el_asr_type));
                is_el_matching = llvm_utils->is_equal_by_value(el, original_el, module,
                                                                el_asr_type);
                LLVM::CreateStore(*builder, is_el_matching, is_el_matching_var);
            }, [=]() {
            });
            // TODO: Allow safe exit if pos becomes el_hash again.
            // Ideally should not happen as set will be resized once
            // load factor touches a threshold (which will always be less than 1)
            // so there will be some el which will not be set. However for safety
            // we can add an exit from the loop with a error message.
            llvm::Value *cond = nullptr;
            if( for_read ) {
                cond = builder->CreateAnd(is_el_set, builder->CreateNot(
                            llvm_utils->CreateLoad(is_el_matching_var)));
                cond = builder->CreateOr(is_el_skip, cond);
            } else {
                cond = builder->CreateAnd(is_el_set, builder->CreateNot(is_el_skip));
                cond = builder->CreateAnd(cond, builder->CreateNot(
                            llvm_utils->CreateLoad(is_el_matching_var)));
            }
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* pos = llvm_utils->CreateLoad(pos_ptr);
            pos = builder->CreateAdd(pos, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                                llvm::APInt(32, 1)));
            pos = builder->CreateSRem(pos, capacity);
            LLVM::CreateStore(*builder, pos, pos_ptr);
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);
    }

    void LLVMSetSeparateChaining::resolve_collision(
        llvm::Value* el_hash, llvm::Value* el, llvm::Value* el_linked_list,
        llvm::Type* el_struct_type, llvm::Value* el_mask,
        llvm::Module* module, ASR::ttype_t* el_asr_type) {
        /**
         * C++ equivalent:
         *
         * ll_exists = el_mask_value == 1;
         * if( ll_exists ) {
         *     chain_itr = ll_head;
         * }
         * else {
         *     chain_itr = nullptr;
         * }
         * is_el_matching = 0;
         *
         * while( chain_itr != nullptr && !is_el_matching ) {
         *     chain_itr_prev = chain_itr;
         *     is_el_matching = (el == el_struct_el);
         *     if( !is_el_matching ) {
         *         chain_itr = next_el_struct;  // (*chain_itr)[1]
         *     }
         * }
         *
         * // now, chain_itr either points to element or is nullptr
         *
         */

        chain_itr = llvm_utils->CreateAlloca(llvm::Type::getInt8Ty(context)->getPointerTo());
        chain_itr_prev = llvm_utils->CreateAlloca(llvm::Type::getInt8Ty(context)->getPointerTo());
        is_el_matching_var = llvm_utils->CreateAlloca(llvm::Type::getInt1Ty(context));

        LLVM::CreateStore(*builder,
                llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()), chain_itr_prev);
        llvm::Value* el_mask_value = llvm_utils->CreateLoad(
            llvm_utils->create_ptr_gep(el_mask, el_hash));
        llvm_utils->create_if_else(builder->CreateICmpEQ(el_mask_value,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1))), [&]() {
            llvm::Value* el_ll_i8 = builder->CreateBitCast(el_linked_list, llvm::Type::getInt8Ty(context)->getPointerTo());
            LLVM::CreateStore(*builder, el_ll_i8, chain_itr);
        }, [&]() {
            LLVM::CreateStore(*builder,
                    llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()), chain_itr);
        });
        LLVM::CreateStore(*builder,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(1, 0)),
            is_el_matching_var
        );
        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpNE(
                llvm_utils->CreateLoad(chain_itr),
                llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo())
            );
            cond = builder->CreateAnd(cond, builder->CreateNot(
                    llvm_utils->CreateLoad(is_el_matching_var)));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* el_struct_i8 = llvm_utils->CreateLoad(chain_itr);
            LLVM::CreateStore(*builder, el_struct_i8, chain_itr_prev);
            llvm::Value* el_struct = builder->CreateBitCast(el_struct_i8, el_struct_type->getPointerTo());
            llvm::Value* el_struct_el = llvm_utils->create_gep(el_struct, 0);
            if( !LLVM::is_llvm_struct(el_asr_type) ) {
                el_struct_el = llvm_utils->CreateLoad(el_struct_el);
            }
            LLVM::CreateStore(*builder, llvm_utils->is_equal_by_value(el, el_struct_el,
                                module, el_asr_type), is_el_matching_var);
            llvm_utils->create_if_else(builder->CreateNot(llvm_utils->CreateLoad(is_el_matching_var)), [&]() {
                llvm::Value* next_el_struct = llvm_utils->CreateLoad(llvm_utils->create_gep(el_struct, 1));
                LLVM::CreateStore(*builder, next_el_struct, chain_itr);
            }, []() {});
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);

    }

    void LLVMSetLinearProbing::resolve_collision_for_write(
        ASR::expr_t* set_expr, llvm::Value* set, llvm::Value* el_hash, llvm::Value* el,
        llvm::Module* module, ASR::ttype_t* el_asr_type,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {

        /**
         * C++ equivalent:
         *
         * resolve_collision();     // modifies pos
         * el_list[pos] = el;
         * el_mask_value = el_mask[pos];
         * is_slot_empty = el_mask_value == 0 || el_mask_value == 3;
         * occupancy += is_slot_empty;
         * linear_prob_happened = (el_hash != pos) || (el_mask[el_hash] == 2);
         * set_max_2 = linear_prob_happened ? 2 : 1;
         * el_mask[el_hash] = set_max_2;
         * el_mask[pos] = set_max_2;
         *
         */

        llvm::Value* el_list = get_el_list(set);
        std::string el_type_code = ASRUtils::get_type_code(el_asr_type);
        llvm::Value* el_mask = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context)->getPointerTo(),
                                                       get_pointer_to_mask(set));
        llvm::Value* capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context),
                                                        get_pointer_to_capacity_using_typecode(el_type_code, set));
        this->resolve_collision(capacity, el_hash, el, el_list, el_mask, module, el_asr_type);
        llvm::Value* pos = llvm_utils->CreateLoad(pos_ptr);
        llvm_utils->list_api->write_item(set_expr, el_list, pos, el,
                                         el_asr_type, false, module, name2memidx);

        llvm::Value* el_mask_value = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context),
                llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), el_mask, pos));
        llvm::Value* is_slot_empty = builder->CreateICmpEQ(el_mask_value,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0)));
        is_slot_empty = builder->CreateOr(is_slot_empty, builder->CreateICmpEQ(el_mask_value,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 3))));
        llvm::Value* occupancy_ptr = get_pointer_to_occupancy(set);
        is_slot_empty = builder->CreateZExt(is_slot_empty, llvm::Type::getInt32Ty(context));
        llvm::Value* occupancy = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), occupancy_ptr);
        LLVM::CreateStore(*builder, builder->CreateAdd(occupancy, is_slot_empty),
                          occupancy_ptr);

        llvm::Value* linear_prob_happened = builder->CreateICmpNE(el_hash, pos);
        linear_prob_happened = builder->CreateOr(linear_prob_happened,
            builder->CreateICmpEQ(
                llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context), 
                                        llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), el_mask, el_hash)),
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 2)
            ))
        );
        llvm::Value* set_max_2 = builder->CreateSelect(linear_prob_happened,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 2)),
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));
        LLVM::CreateStore(*builder, set_max_2, llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context),
                                                            el_mask, el_hash));
        LLVM::CreateStore(*builder, set_max_2, llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context),
                                                            el_mask, pos));
    }

    void LLVMSetSeparateChaining::resolve_collision_for_write(
        ASR::expr_t* set_expr, llvm::Value* set, llvm::Value* el_hash, llvm::Value* el,
        llvm::Module* module, ASR::ttype_t* el_asr_type,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        /**
         * C++ equivalent:
         *
         * el_linked_list = elems[el_hash];
         * resolve_collision(el);   // modifies chain_itr
         * do_insert = chain_itr == nullptr;
         *
         * if( do_insert ) {
         *     if( chain_itr_prev != nullptr ) {
         *         new_el_struct = malloc(el_struct_size);
         *         new_el_struct[0] = el;
         *         new_el_struct[1] = nullptr;
         *         chain_itr_prev[1] = new_el_struct;
         *     }
         *     else {
         *         el_linked_list[0] = el;
         *         el_linked_list[1] = nullptr;
         *     }
         *     occupancy += 1;
         * }
         * else {
         *     el_struct[0] = el;
         * }
         *
         * buckets_filled_delta = el_mask[el_hash] == 0;
         * buckets_filled += buckets_filled_delta;
         * el_mask[el_hash] = 1;
         *
         */

        llvm::Value* elems = llvm_utils->CreateLoad(get_pointer_to_elems(set));
        llvm::Value* el_linked_list = llvm_utils->create_ptr_gep(elems, el_hash);
        llvm::Value* el_mask = llvm_utils->CreateLoad(get_pointer_to_mask(set));
        llvm::Type* el_struct_type = typecode2elstruct[ASRUtils::get_type_code(el_asr_type)];
        this->resolve_collision(el_hash, el, el_linked_list, el_struct_type,
                                el_mask, module, el_asr_type);
        llvm::Value* el_struct_i8 = llvm_utils->CreateLoad(chain_itr);

        llvm::Function *fn = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", fn);
        llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context, "else");
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifcont");
        llvm::Value* do_insert = builder->CreateICmpEQ(el_struct_i8,
            llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()));
        builder->CreateCondBr(do_insert, thenBB, elseBB);

        builder->SetInsertPoint(thenBB);
        {
            llvm_utils->create_if_else(builder->CreateICmpNE(
                    llvm_utils->CreateLoad(chain_itr_prev),
                    llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo())), [&]() {
                llvm::DataLayout data_layout(module->getDataLayout());
                size_t el_struct_size = data_layout.getTypeAllocSize(el_struct_type);
                llvm::Value* malloc_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), el_struct_size);
                llvm::Value* new_el_struct_i8 = LLVM::lfortran_malloc(context, *module, *builder, malloc_size);
                llvm::Value* new_el_struct = builder->CreateBitCast(new_el_struct_i8, el_struct_type->getPointerTo());
                llvm_utils->deepcopy(set_expr, el, llvm_utils->create_gep(new_el_struct, 0), el_asr_type, el_asr_type, module, name2memidx);
                LLVM::CreateStore(*builder,
                    llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()),
                    llvm_utils->create_gep(new_el_struct, 1));
                llvm::Value* el_struct_prev_i8 = llvm_utils->CreateLoad(chain_itr_prev);
                llvm::Value* el_struct_prev = builder->CreateBitCast(el_struct_prev_i8, el_struct_type->getPointerTo());
                LLVM::CreateStore(*builder, new_el_struct_i8, llvm_utils->create_gep(el_struct_prev, 1));
            }, [&]() {
                llvm_utils->deepcopy(set_expr, el, llvm_utils->create_gep(el_linked_list, 0), el_asr_type, el_asr_type, module, name2memidx);
                LLVM::CreateStore(*builder,
                    llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()),
                    llvm_utils->create_gep(el_linked_list, 1));
            });

            llvm::Value* occupancy_ptr = get_pointer_to_occupancy(set);
            llvm::Value* occupancy = llvm_utils->CreateLoad(occupancy_ptr);
            occupancy = builder->CreateAdd(occupancy,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 1));
            LLVM::CreateStore(*builder, occupancy, occupancy_ptr);
        }
        builder->CreateBr(mergeBB);
        llvm_utils->start_new_block(elseBB);
        {
            llvm::Value* el_struct = builder->CreateBitCast(el_struct_i8, el_struct_type->getPointerTo());
            llvm_utils->deepcopy(set_expr, el, llvm_utils->create_gep(el_struct, 0), el_asr_type, el_asr_type, module, name2memidx);
        }
        llvm_utils->start_new_block(mergeBB);
        llvm::Value* buckets_filled_ptr = get_pointer_to_number_of_filled_buckets(set);
        llvm::Value* el_mask_value_ptr = llvm_utils->create_ptr_gep(el_mask, el_hash);
        llvm::Value* el_mask_value = llvm_utils->CreateLoad(el_mask_value_ptr);
        llvm::Value* buckets_filled_delta = builder->CreateICmpEQ(el_mask_value,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0)));
        llvm::Value* buckets_filled = llvm_utils->CreateLoad(buckets_filled_ptr);
        buckets_filled = builder->CreateAdd(
            buckets_filled,
            builder->CreateZExt(buckets_filled_delta, llvm::Type::getInt32Ty(context))
        );
        LLVM::CreateStore(*builder, buckets_filled, buckets_filled_ptr);
        LLVM::CreateStore(*builder,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)),
            el_mask_value_ptr);
    }

    void LLVMSetLinearProbing::rehash(
        ASR::expr_t* set_expr, llvm::Value* set, llvm::Module* module, ASR::ttype_t* el_asr_type,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {

        /**
         * C++ equivalent:
         *
         * old_capacity = capacity;
         * capacity = 2 * capacity + 1;
         *
         * idx = 0;
         * while( old_capacity > idx ) {
         *     is_el_set = el_mask[idx] != 0;
         *     if( is_el_set ) {
         *         el = el_list[idx];
         *         el_hash = get_el_hash(); // with new capacity
         *         resolve_collision();     // with new_el_list; modifies pos
         *         new_el_list[pos] = el;
         *         linear_prob_happened = el_hash != pos;
         *         set_max_2 = linear_prob_happened ? 2 : 1;
         *         new_el_mask[el_hash] = set_max_2;
         *         new_el_mask[pos] = set_max_2;
         *     }
         *     idx += 1;
         * }
         *
         * free(el_list);
         * free(el_mask);
         * el_list = new_el_list;
         * el_mask = new_el_mask;
         *
         */

        std::string el_type_code = ASRUtils::get_type_code(el_asr_type);
        llvm::Type* el_llvm_type = std::get<2>(typecode2settype[el_type_code]);
        int32_t el_type_size = std::get<1>(typecode2settype[el_type_code]);
        /*llvm::Type* set_type = std::get<0>(typecode2settype[el_type_code]);*/

        llvm::Value* capacity_ptr = get_pointer_to_capacity_using_typecode(el_type_code, set);
        llvm::Value* old_capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), capacity_ptr);
        llvm::Value* capacity = builder->CreateMul(old_capacity, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                                       llvm::APInt(32, 2)));
        capacity = builder->CreateAdd(capacity, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                                       llvm::APInt(32, 1)));
        LLVM::CreateStore(*builder, capacity, capacity_ptr);


        llvm::Value* el_list = get_el_list(set);
        llvm::Value* new_el_list = llvm_utils->CreateAlloca(llvm_utils->list_api->get_list_type(el_llvm_type,
                                                          el_type_code, el_type_size));
        llvm_utils->list_api->list_init(el_type_code, new_el_list, module, capacity, capacity);

        llvm::Value* el_mask = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context)->getPointerTo(), get_pointer_to_mask(set));
        llvm::DataLayout data_layout(module->getDataLayout());
        size_t mask_size = data_layout.getTypeAllocSize(llvm::Type::getInt8Ty(context));
        llvm::Value* llvm_mask_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                            llvm::APInt(32, mask_size));
        llvm::Value* new_el_mask = LLVM::lfortran_calloc(context, *module, *builder, capacity,
                                                          llvm_mask_size);

        llvm::Value* current_capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), 
                                                                get_pointer_to_capacity_using_typecode(el_type_code, set));
        idx_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
            llvm::APInt(32, 0)), idx_ptr);

        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpSGT(old_capacity, llvm_utils->CreateLoad(idx_ptr));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* idx = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), idx_ptr);
            llvm::Function *fn = builder->GetInsertBlock()->getParent();
            llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", fn);
            llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context, "else");
            llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifcont");
            llvm::Value* is_el_set = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context),
                                            llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context), el_mask, idx));
            is_el_set = builder->CreateICmpNE(is_el_set,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0)));
            builder->CreateCondBr(is_el_set, thenBB, elseBB);
            builder->SetInsertPoint(thenBB);
            {
                llvm::Value* el = llvm_utils->list_api->read_item_using_ttype(el_asr_type, el_list, idx,
                        false, module, LLVM::is_llvm_struct(el_asr_type));
                llvm::Value* el_hash = get_el_hash(current_capacity, el, el_asr_type, module);
                this->resolve_collision(current_capacity, el_hash, el, new_el_list,
                               new_el_mask, module, el_asr_type);
                llvm::Value* pos = llvm_utils->CreateLoad(pos_ptr);
                llvm::Value* el_dest = llvm_utils->list_api->read_item_using_ttype(el_asr_type,
                                    new_el_list, pos, false, module, true);
                llvm_utils->deepcopy(set_expr, el, el_dest, el_asr_type, el_asr_type, module, name2memidx);

                llvm::Value* linear_prob_happened = builder->CreateICmpNE(el_hash, pos);
                llvm::Value* set_max_2 = builder->CreateSelect(linear_prob_happened,
                    llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 2)),
                    llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));
                LLVM::CreateStore(*builder, set_max_2, llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context),
                                                                                new_el_mask, el_hash));
                LLVM::CreateStore(*builder, set_max_2, llvm_utils->create_ptr_gep2(llvm::Type::getInt8Ty(context),
                                                                                new_el_mask, pos));
            }
            builder->CreateBr(mergeBB);

            llvm_utils->start_new_block(elseBB);
            llvm_utils->start_new_block(mergeBB);
            idx = builder->CreateAdd(idx, llvm::ConstantInt::get(
                    llvm::Type::getInt32Ty(context), llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, idx, idx_ptr);
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);

        llvm_utils->list_api->free_data_using_type(el_type_code, el_list, module);
        LLVM::lfortran_free(context, *module, *builder, el_mask);
        LLVM::CreateStore(*builder, llvm_utils->CreateLoad(new_el_list), el_list);
        LLVM::CreateStore(*builder, new_el_mask, get_pointer_to_mask(set));
    }

    void LLVMSetSeparateChaining::rehash(
        ASR::expr_t* set_expr, llvm::Value* set, llvm::Module* module, ASR::ttype_t* el_asr_type,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        /**
         * C++ equivalent:
         *
         * capacity = 3 * capacity + 1;
         *
         * if( rehash_flag ) {
         *     while( old_capacity > idx ) {
         *         if( el_mask[el_hash] == 1 ) {
         *             write_el_linked_list(old_elems_value[idx]);
         *         }
         *         idx++;
         *     }
         * }
         * else {
         *     // set to old values
         * }
         *
         */
        old_capacity = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        old_occupancy = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        old_number_of_buckets_filled = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        idx_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        old_elems = llvm_utils->CreateAlloca(llvm::Type::getInt8Ty(context)->getPointerTo());
        old_el_mask = llvm_utils->CreateAlloca(llvm::Type::getInt8Ty(context)->getPointerTo());
        

        std::string el_type_code = ASRUtils::get_type_code(el_asr_type);
        llvm::Type* el_list_type = llvm_utils->list_api->get_list_type(nullptr, el_type_code, 0);

        llvm::Value* capacity_ptr = get_pointer_to_capacity_using_type(el_list_type, set);
        llvm::Value* occupancy_ptr = get_pointer_to_occupancy(set);
        llvm::Value* number_of_buckets_filled_ptr = get_pointer_to_number_of_filled_buckets(set);
        llvm::Value* old_capacity_value = llvm_utils->CreateLoad(capacity_ptr);
        LLVM::CreateStore(*builder, old_capacity_value, old_capacity);
        LLVM::CreateStore(*builder,
            llvm_utils->CreateLoad(occupancy_ptr),
            old_occupancy
        );
        LLVM::CreateStore(*builder,
            llvm_utils->CreateLoad(number_of_buckets_filled_ptr),
            old_number_of_buckets_filled
        );
        llvm::Value* old_el_mask_value = llvm_utils->CreateLoad(get_pointer_to_mask(set));
        llvm::Value* old_elems_value = llvm_utils->CreateLoad(get_pointer_to_elems(set));
        old_elems_value = builder->CreateBitCast(old_elems_value, llvm::Type::getInt8Ty(context)->getPointerTo());
        LLVM::CreateStore(*builder, old_el_mask_value, old_el_mask);
        LLVM::CreateStore(*builder, old_elems_value, old_elems);

        llvm::Value* capacity = builder->CreateMul(old_capacity_value, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                                       llvm::APInt(32, 3)));
        capacity = builder->CreateAdd(capacity, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                                                       llvm::APInt(32, 1)));
        set_init_given_initial_capacity(ASRUtils::get_type_code(el_asr_type),
                                         set, module, capacity);

        llvm::Function *fn = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *thenBB_rehash = llvm::BasicBlock::Create(context, "then", fn);
        llvm::BasicBlock *elseBB_rehash = llvm::BasicBlock::Create(context, "else");
        llvm::BasicBlock *mergeBB_rehash = llvm::BasicBlock::Create(context, "ifcont");
        llvm::Value* rehash_flag = llvm_utils->CreateLoad(get_pointer_to_rehash_flag(set));
        builder->CreateCondBr(rehash_flag, thenBB_rehash, elseBB_rehash);

        builder->SetInsertPoint(thenBB_rehash);
        old_elems_value = llvm_utils->CreateLoad(old_elems);
        old_elems_value = builder->CreateBitCast(old_elems_value,
            typecode2elstruct[ASRUtils::get_type_code(el_asr_type)]->getPointerTo());
        old_el_mask_value = llvm_utils->CreateLoad(old_el_mask);
        old_capacity_value = llvm_utils->CreateLoad(old_capacity);
        capacity = llvm_utils->CreateLoad(get_pointer_to_capacity_using_type(el_list_type, set));
        LLVM::CreateStore(*builder, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 0)), idx_ptr);
        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpSGT(
                                        old_capacity_value,
                                        llvm_utils->CreateLoad(idx_ptr));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* itr = llvm_utils->CreateLoad(idx_ptr);
            llvm::Value* el_mask_value = llvm_utils->CreateLoad(
                llvm_utils->create_ptr_gep(old_el_mask_value, itr));
            llvm::Value* is_el_set = builder->CreateICmpEQ(el_mask_value,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));

            llvm_utils->create_if_else(is_el_set, [&]() {
                llvm::Value* srci = llvm_utils->create_ptr_gep(old_elems_value, itr);
                write_el_linked_list(set_expr, srci, set, capacity, el_asr_type, module, name2memidx);
            }, [=]() {
            });
            llvm::Value* tmp = builder->CreateAdd(
                        itr,
                        llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, tmp, idx_ptr);
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);
        builder->CreateBr(mergeBB_rehash);
        llvm_utils->start_new_block(elseBB_rehash);
        {
            LLVM::CreateStore(*builder,
                llvm_utils->CreateLoad(old_capacity),
                get_pointer_to_capacity_using_type(el_list_type, set)
            );
            LLVM::CreateStore(*builder,
                llvm_utils->CreateLoad(old_occupancy),
                get_pointer_to_occupancy(set)
            );
            LLVM::CreateStore(*builder,
                llvm_utils->CreateLoad(old_number_of_buckets_filled),
                get_pointer_to_number_of_filled_buckets(set)
            );
            LLVM::CreateStore(*builder,
                builder->CreateBitCast(
                    llvm_utils->CreateLoad(old_elems),
                    typecode2elstruct[ASRUtils::get_type_code(el_asr_type)]->getPointerTo()
                ),
                get_pointer_to_elems(set)
            );
            LLVM::CreateStore(*builder,
                llvm_utils->CreateLoad(old_el_mask),
                get_pointer_to_mask(set)
            );
        }
        llvm_utils->start_new_block(mergeBB_rehash);
    }

    void LLVMSetSeparateChaining::write_el_linked_list(
        ASR::expr_t* set_expr, llvm::Value* el_ll, llvm::Value* set, llvm::Value* capacity,
        ASR::ttype_t* m_el_type, llvm::Module* module,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        /**
         * C++ equivalent:
         *
         * while( src_itr != nullptr ) {
         *     resolve_collision_for_write(el_struct[0]);
         *     src_itr = el_struct[1];
         * }
         *
         */

        src_itr = llvm_utils->CreateAlloca(llvm::Type::getInt8Ty(context)->getPointerTo());

        llvm::Type* el_struct_type = typecode2elstruct[ASRUtils::get_type_code(m_el_type)]->getPointerTo();
        LLVM::CreateStore(*builder,
            builder->CreateBitCast(el_ll, llvm::Type::getInt8Ty(context)->getPointerTo()),
            src_itr);
        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");
        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpNE(
                llvm_utils->CreateLoad(src_itr),
                llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo())
            );
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* curr_src = builder->CreateBitCast(llvm_utils->CreateLoad(src_itr),
                el_struct_type);
            llvm::Value* src_el_ptr = llvm_utils->create_gep(curr_src, 0);
            llvm::Value* src_el = src_el_ptr;
            if( !LLVM::is_llvm_struct(m_el_type) ) {
                src_el = llvm_utils->CreateLoad(src_el_ptr);
            }
            llvm::Value* el_hash = get_el_hash(capacity, src_el, m_el_type, module);
            resolve_collision_for_write(set_expr,
                set, el_hash, src_el, module,
                m_el_type, name2memidx);

            llvm::Value* src_next_ptr = llvm_utils->CreateLoad(llvm_utils->create_gep(curr_src, 1));
            LLVM::CreateStore(*builder, src_next_ptr, src_itr);
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);
    }

    void LLVMSetLinearProbing::rehash_all_at_once_if_needed(
        ASR::expr_t* set_expr, llvm::Value* set, llvm::Module* module, ASR::ttype_t* el_asr_type,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {

        /**
         * C++ equivalent:
         *
         * // this condition will be true with 0 capacity too
         * rehash_condition = 5 * occupancy >= 3 * capacity;
         * if( rehash_condition ) {
         *     rehash();
         * }
         *
         */
        std::string el_type_code = ASRUtils::get_type_code(el_asr_type);
        llvm::Type* set_type = get_set_type(ASRUtils::get_type_code(el_asr_type), 0, nullptr);
        llvm::Value* occupancy = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), 
                                                         get_pointer_to_occupancy_using_type(set_type, set));
        llvm::Value* capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), 
                                                        get_pointer_to_capacity_using_typecode(el_type_code, set));
        // Threshold hash is chosen from https://en.wikipedia.org/wiki/Hash_table#Load_factor
        // occupancy / capacity >= 0.6 is same as 5 * occupancy >= 3 * capacity
        llvm::Value* occupancy_times_5 = builder->CreateMul(occupancy, llvm::ConstantInt::get(
                                llvm::Type::getInt32Ty(context), llvm::APInt(32, 5)));
        llvm::Value* capacity_times_3 = builder->CreateMul(capacity, llvm::ConstantInt::get(
                                llvm::Type::getInt32Ty(context), llvm::APInt(32, 3)));
        llvm_utils->create_if_else(builder->CreateICmpSGE(occupancy_times_5,
                                    capacity_times_3), [&]() {
            rehash(set_expr, set, module, el_asr_type, name2memidx);
        }, []() {});
    }

    void LLVMSetSeparateChaining::rehash_all_at_once_if_needed(
        ASR::expr_t* set_expr, llvm::Value* set, llvm::Module* module, ASR::ttype_t* el_asr_type,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        /**
         * C++ equivalent:
         *
         * rehash_condition = rehash_flag && occupancy >= 2 * buckets_filled;
         * if( rehash_condition ) {
         *     rehash();
         * }
         *
         */
        llvm::Value* occupancy = llvm_utils->CreateLoad(get_pointer_to_occupancy(set));
        llvm::Value* buckets_filled = llvm_utils->CreateLoad(get_pointer_to_number_of_filled_buckets(set));
        llvm::Value* rehash_condition = llvm_utils->CreateLoad(get_pointer_to_rehash_flag(set));
        llvm::Value* buckets_filled_times_2 = builder->CreateMul(buckets_filled,
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 2)));
        rehash_condition = builder->CreateAnd(rehash_condition,
            builder->CreateICmpSGE(occupancy, buckets_filled_times_2));
        llvm_utils->create_if_else(rehash_condition, [&]() {
            rehash(set_expr, set, module, el_asr_type, name2memidx);
        }, []() {});
    }

    void LLVMSetInterface::write_item(
        ASR::expr_t* set_expr, llvm::Value* set, llvm::Value* el,
        llvm::Module* module, ASR::ttype_t* el_asr_type,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        rehash_all_at_once_if_needed(set_expr, set, module, el_asr_type, name2memidx);
        std::string el_type_code = ASRUtils::get_type_code(el_asr_type);
        llvm::Value* current_capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), 
                                                                get_pointer_to_capacity_using_typecode(el_type_code, set));
        llvm::Value* el_hash = get_el_hash(current_capacity, el, el_asr_type, module);
        this->resolve_collision_for_write(set_expr, set, el_hash, el, module,
                                          el_asr_type, name2memidx);
    }

    void LLVMSetLinearProbing::resolve_collision_for_read_with_bound_check(
        llvm::Value* set, llvm::Value* el_hash, llvm::Value* el,
        llvm::Module* module, ASR::ttype_t* el_asr_type) {

        /**
         * C++ equivalent:
         *
         * el_mask_value = el_mask[el_hash];
         * is_prob_needed = el_mask_value == 1;
         * if( is_prob_needed ) {
         *     is_el_matching = el == el_list[el_hash];
         *     if( is_el_matching ) {
         *         pos = el_hash;
         *     }
         *     else {
         *         exit(1); // el not present
         *     }
         * }
         * else {
         *     resolve_collision(el, for_read=true);  // modifies pos
         * }
         *
         * is_el_matching = el == el_list[pos];
         * if( !is_el_matching ) {
         *     exit(1); // el not present
         * }
         *
         */
        std::string el_type_code = ASRUtils::get_type_code(el_asr_type);
        llvm::Value* el_list = get_el_list(set);
        llvm::Type* el_list_type = llvm_utils->list_api->get_list_type(nullptr, el_type_code, 0);

        llvm::Value* el_mask = llvm_utils->CreateLoad(get_pointer_to_mask(set));
        llvm::Value* capacity = llvm_utils->CreateLoad(get_pointer_to_capacity_using_type(el_list_type, set));
        pos_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        llvm::Function *fn = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", fn);
        llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context, "else");
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifcont");
        llvm::Value* el_mask_value = llvm_utils->CreateLoad(
                                        llvm_utils->create_ptr_gep(el_mask, el_hash));
        llvm::Value* is_prob_not_needed = builder->CreateICmpEQ(el_mask_value,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));
        builder->CreateCondBr(is_prob_not_needed, thenBB, elseBB);
        builder->SetInsertPoint(thenBB);
        {
            // reasoning for this check explained in
            // LLVMDictOptimizedLinearProbing::resolve_collision_for_read_with_bound_check
            llvm::Value* is_el_matching = llvm_utils->is_equal_by_value(el,
                llvm_utils->list_api->read_item_using_ttype(el_asr_type, el_list, el_hash, false, module,
                    LLVM::is_llvm_struct(el_asr_type)), module, el_asr_type);

            llvm_utils->create_if_else(is_el_matching, [=]() {
                LLVM::CreateStore(*builder, el_hash, pos_ptr);
            }, [&]() {
                std::string message = "The set does not contain the specified element";
                llvm::Value *fmt_ptr = builder->CreateGlobalStringPtr("KeyError: %s\n");
                llvm::Value *fmt_ptr2 = builder->CreateGlobalStringPtr(message);
                print_error(context, *module, *builder, {fmt_ptr, fmt_ptr2});
                int exit_code_int = 1;
                llvm::Value *exit_code = llvm::ConstantInt::get(context,
                        llvm::APInt(32, exit_code_int));
                exit(context, *module, *builder, exit_code);
            });
        }
        builder->CreateBr(mergeBB);
        llvm_utils->start_new_block(elseBB);
        {
            this->resolve_collision(capacity, el_hash, el, el_list, el_mask,
                                    module, el_asr_type, true);
        }
        llvm_utils->start_new_block(mergeBB);
        llvm::Value* pos = llvm_utils->CreateLoad(pos_ptr);
        // Check if the actual element is present or not
        llvm::Value* is_el_matching = llvm_utils->is_equal_by_value(el,
                llvm_utils->list_api->read_item_using_ttype(el_asr_type, el_list, pos, false, module,
                    LLVM::is_llvm_struct(el_asr_type)), module, el_asr_type);

        llvm_utils->create_if_else(is_el_matching, []() {}, [&]() {
            std::string message = "The set does not contain the specified element";
            llvm::Value *fmt_ptr = builder->CreateGlobalStringPtr("KeyError: %s\n");
            llvm::Value *fmt_ptr2 = builder->CreateGlobalStringPtr(message);
            print_error(context, *module, *builder, {fmt_ptr, fmt_ptr2});
            int exit_code_int = 1;
            llvm::Value *exit_code = llvm::ConstantInt::get(context,
                    llvm::APInt(32, exit_code_int));
            exit(context, *module, *builder, exit_code);
        });
    }

    void LLVMSetSeparateChaining::resolve_collision_for_read_with_bound_check(
        llvm::Value* set, llvm::Value* el_hash, llvm::Value* el,
        llvm::Module* module, ASR::ttype_t* el_asr_type) {
        /**
         * C++ equivalent:
         *
         * resolve_collision(el);   // modified chain_itr
         * does_el_exist = el_mask[el_hash] == 1 && chain_itr != nullptr;
         * if( !does_el_exist ) {
         *     exit(1); // KeyError
         * }
         *
         */
        llvm::Value* elems = llvm_utils->CreateLoad(get_pointer_to_elems(set));
        llvm::Value* el_linked_list = llvm_utils->create_ptr_gep(elems, el_hash);
        llvm::Value* el_mask = llvm_utils->CreateLoad(get_pointer_to_mask(set));
        std::string el_type_code = ASRUtils::get_type_code(el_asr_type);
        llvm::Type* el_struct_type = typecode2elstruct[el_type_code];
        this->resolve_collision(el_hash, el, el_linked_list,
                                el_struct_type, el_mask, module, el_asr_type);
        llvm::Value* el_mask_value = llvm_utils->CreateLoad(
            llvm_utils->create_ptr_gep(el_mask, el_hash));
        llvm::Value* does_el_exist = builder->CreateICmpEQ(el_mask_value,
            llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));
        does_el_exist = builder->CreateAnd(does_el_exist,
            builder->CreateICmpNE(llvm_utils->CreateLoad(chain_itr),
            llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()))
        );

        llvm_utils->create_if_else(does_el_exist, []() {}, [&]() {
            std::string message = "The set does not contain the specified element";
            llvm::Value *fmt_ptr = builder->CreateGlobalStringPtr("KeyError: %s\n");
            llvm::Value *fmt_ptr2 = builder->CreateGlobalStringPtr(message);
            print_error(context, *module, *builder, {fmt_ptr, fmt_ptr2});
            int exit_code_int = 1;
            llvm::Value *exit_code = llvm::ConstantInt::get(context,
                    llvm::APInt(32, exit_code_int));
            exit(context, *module, *builder, exit_code);
        });
    }

    void LLVMSetLinearProbing::remove_item(
        llvm::Value* set, llvm::Value* el,
        llvm::Module* module, ASR::ttype_t* el_asr_type) {
        /**
         * C++ equivalent:
         *
         * resolve_collision_for_read(el);  // modifies pos
         * el_mask[pos] = 3;    // tombstone marker
         * occupancy -= 1;
         */

        std::string el_type_code = ASRUtils::get_type_code(el_asr_type);
        llvm::Type* el_list_type = llvm_utils->list_api->get_list_type(nullptr, el_type_code, 0);
        llvm::Value* current_capacity = llvm_utils->CreateLoad(get_pointer_to_capacity_using_type(el_list_type, set));
        llvm::Value* el_hash = get_el_hash(current_capacity, el, el_asr_type, module);
        this->resolve_collision_for_read_with_bound_check(set, el_hash, el, module, el_asr_type);
        llvm::Value* pos = llvm_utils->CreateLoad(pos_ptr);
        llvm::Value* el_mask = llvm_utils->CreateLoad(get_pointer_to_mask(set));
        llvm::Value* el_mask_i = llvm_utils->create_ptr_gep(el_mask, pos);
        llvm::Value* tombstone_marker = llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 3));
        LLVM::CreateStore(*builder, tombstone_marker, el_mask_i);

        llvm::Value* occupancy_ptr = get_pointer_to_occupancy(set);
        llvm::Value* occupancy = llvm_utils->CreateLoad(occupancy_ptr);
        occupancy = builder->CreateSub(occupancy, llvm::ConstantInt::get(
                        llvm::Type::getInt32Ty(context), llvm::APInt(32, 1)));
        LLVM::CreateStore(*builder, occupancy, occupancy_ptr);
    }

    void LLVMSetSeparateChaining::remove_item(
        llvm::Value* set, llvm::Value* el,
        llvm::Module* module, ASR::ttype_t* el_asr_type) {
        /**
         * C++ equivalent:
         *
         * // modifies chain_itr and chain_itr_prev
         * resolve_collision_for_read_with_bound_check(el);
         *
         * if(chain_itr_prev != nullptr) {
         *     chain_itr_prev[1] = chain_itr[1]; // next
         * }
         * else {
         *     // this linked list is now empty
         *     el_mask[el_hash] = 0;
         *     num_buckets_filled--;
         * }
         *
         * occupancy--;
         *
         */

        std::string el_type_code = ASRUtils::get_type_code(el_asr_type);
        llvm::Type* el_list_type = llvm_utils->list_api->get_list_type(nullptr, el_type_code, 0);
        llvm::Value* current_capacity = llvm_utils->CreateLoad(get_pointer_to_capacity_using_type(el_list_type, set));
        llvm::Value* el_hash = get_el_hash(current_capacity, el, el_asr_type, module);
        this->resolve_collision_for_read_with_bound_check(set, el_hash, el, module, el_asr_type);
        llvm::Value* prev = llvm_utils->CreateLoad(chain_itr_prev);
        llvm::Value* found = llvm_utils->CreateLoad(chain_itr);

        llvm::Function *fn = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", fn);
        llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context, "else");
        llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifcont");

        builder->CreateCondBr(
            builder->CreateICmpNE(prev, llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo())),
            thenBB, elseBB
        );
        builder->SetInsertPoint(thenBB);
        {
            llvm::Type* el_struct_type = typecode2elstruct[ASRUtils::get_type_code(el_asr_type)];
            found = builder->CreateBitCast(found, el_struct_type->getPointerTo());
            llvm::Value* found_next = llvm_utils->CreateLoad(llvm_utils->create_gep(found, 1));
            prev = builder->CreateBitCast(prev, el_struct_type->getPointerTo());
            LLVM::CreateStore(*builder, found_next, llvm_utils->create_gep(prev, 1));
        }
        builder->CreateBr(mergeBB);
        llvm_utils->start_new_block(elseBB);
        {
            llvm::Value* el_mask = llvm_utils->CreateLoad(get_pointer_to_mask(set));
            LLVM::CreateStore(
                *builder,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 0)),
                llvm_utils->create_ptr_gep(el_mask, el_hash)
            );
            llvm::Value* num_buckets_filled_ptr = get_pointer_to_number_of_filled_buckets(set);
            llvm::Value* num_buckets_filled = llvm_utils->CreateLoad(num_buckets_filled_ptr);
            num_buckets_filled = builder->CreateSub(num_buckets_filled, llvm::ConstantInt::get(
                        llvm::Type::getInt32Ty(context), llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, num_buckets_filled, num_buckets_filled_ptr);
        }
        llvm_utils->start_new_block(mergeBB);

        llvm::Value* occupancy_ptr = get_pointer_to_occupancy(set);
        llvm::Value* occupancy = llvm_utils->CreateLoad(occupancy_ptr);
        occupancy = builder->CreateSub(occupancy, llvm::ConstantInt::get(
                        llvm::Type::getInt32Ty(context), llvm::APInt(32, 1)));
        LLVM::CreateStore(*builder, occupancy, occupancy_ptr);
    }

    void LLVMSetLinearProbing::set_deepcopy(
        ASR::expr_t* set_expr,
        llvm::Value* src, llvm::Value* dest,
        ASR::Set_t* set_type, llvm::Module* module,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        LCOMPILERS_ASSERT(src->getType() == dest->getType());
        std::string el_type_code = ASRUtils::get_type_code(set_type->m_type);
        llvm::Type* set_type_ = get_set_type(el_type_code, 0, nullptr);
        llvm::Value* src_occupancy = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), 
                                                         get_pointer_to_occupancy_using_type(set_type_, src));
        llvm::Value* dest_occupancy_ptr = get_pointer_to_occupancy(dest);
        LLVM::CreateStore(*builder, src_occupancy, dest_occupancy_ptr);

        llvm::Value* src_el_list = get_el_list(src);
        llvm::Value* dest_el_list = get_el_list(dest);
        llvm_utils->list_api->list_deepcopy(set_expr, src_el_list, dest_el_list,
                                            set_type->m_type, module,
                                            name2memidx);

        llvm::Value* src_el_mask = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context)->getPointerTo(),
                                                           get_pointer_to_mask(src));
        llvm::Value* dest_el_mask_ptr = get_pointer_to_mask(dest);
        llvm::DataLayout data_layout(module->getDataLayout());
        size_t mask_size = data_layout.getTypeAllocSize(llvm::Type::getInt8Ty(context));
        llvm::Value* llvm_mask_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                            llvm::APInt(32, mask_size));
        llvm::Value* src_capacity = llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context),
                                                            get_pointer_to_capacity_using_typecode(el_type_code, src));
        llvm::Value* dest_el_mask = LLVM::lfortran_calloc(context, *module, *builder, src_capacity,
                                                      llvm_mask_size);
        builder->CreateMemCpy(dest_el_mask, llvm::MaybeAlign(), src_el_mask,
                              llvm::MaybeAlign(), builder->CreateMul(src_capacity, llvm_mask_size));
        LLVM::CreateStore(*builder, dest_el_mask, dest_el_mask_ptr);
    }

    void LLVMSetSeparateChaining::set_deepcopy(
        ASR::expr_t* set_expr, llvm::Value* src, llvm::Value* dest,
        ASR::Set_t* set_type, llvm::Module* module,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {

        std::string el_type_code = ASRUtils::get_type_code(set_type->m_type);
        llvm::Type* el_list_type = llvm_utils->list_api->get_list_type(nullptr, el_type_code, 0);

        llvm::Value* src_occupancy = llvm_utils->CreateLoad(get_pointer_to_occupancy(src));
        llvm::Value* src_filled_buckets = llvm_utils->CreateLoad(get_pointer_to_number_of_filled_buckets(src));
        llvm::Value* src_capacity = llvm_utils->CreateLoad(get_pointer_to_capacity_using_type(el_list_type, src));
        llvm::Value* src_el_mask = llvm_utils->CreateLoad(get_pointer_to_mask(src));
        llvm::Value* src_rehash_flag = llvm_utils->CreateLoad(get_pointer_to_rehash_flag(src));
        LLVM::CreateStore(*builder, src_occupancy, get_pointer_to_occupancy(dest));
        LLVM::CreateStore(*builder, src_filled_buckets, get_pointer_to_number_of_filled_buckets(dest));
        LLVM::CreateStore(*builder, src_capacity, get_pointer_to_capacity_using_type(el_list_type, dest));
        LLVM::CreateStore(*builder, src_rehash_flag, get_pointer_to_rehash_flag(dest));
        llvm::DataLayout data_layout(module->getDataLayout());
        size_t mask_size = data_layout.getTypeAllocSize(llvm::Type::getInt8Ty(context));
        llvm::Value* llvm_mask_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                            llvm::APInt(32, mask_size));
        llvm::Value* malloc_size = builder->CreateMul(src_capacity, llvm_mask_size);
        llvm::Value* dest_el_mask = LLVM::lfortran_malloc(context, *module, *builder, malloc_size);
        LLVM::CreateStore(*builder, dest_el_mask, get_pointer_to_mask(dest));

        // number of elements to be copied = capacity + (occupancy - filled_buckets)
        malloc_size = builder->CreateSub(src_occupancy, src_filled_buckets);
        malloc_size = builder->CreateAdd(src_capacity, malloc_size);
        llvm::Type* el_struct_type = typecode2elstruct[ASRUtils::get_type_code(set_type->m_type)];
        size_t el_struct_size = data_layout.getTypeAllocSize(el_struct_type);
        llvm::Value* llvm_el_struct_size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, el_struct_size));
        malloc_size = builder->CreateMul(malloc_size, llvm_el_struct_size);
        llvm::Value* dest_elems = LLVM::lfortran_malloc(context, *module, *builder, malloc_size);
        dest_elems = builder->CreateBitCast(dest_elems, el_struct_type->getPointerTo());
        copy_itr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        next_ptr = llvm_utils->CreateAlloca(llvm::Type::getInt32Ty(context));
        llvm::Value* llvm_zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, 0));
        LLVM::CreateStore(*builder, llvm_zero, copy_itr);
        LLVM::CreateStore(*builder, src_capacity, next_ptr);

        llvm::Value* src_elems = llvm_utils->CreateLoad(get_pointer_to_elems(src));
        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpSGT(
                                        src_capacity,
                                        llvm_utils->CreateLoad(copy_itr));
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* itr = llvm_utils->CreateLoad(copy_itr);
            llvm::Value* el_mask_value = llvm_utils->CreateLoad(
                llvm_utils->create_ptr_gep(src_el_mask, itr));
            LLVM::CreateStore(*builder, el_mask_value,
                llvm_utils->create_ptr_gep(dest_el_mask, itr));
            llvm::Value* is_el_set = builder->CreateICmpEQ(el_mask_value,
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), llvm::APInt(8, 1)));

            llvm_utils->create_if_else(is_el_set, [&]() {
                llvm::Value* srci = llvm_utils->create_ptr_gep(src_elems, itr);
                llvm::Value* desti = llvm_utils->create_ptr_gep(dest_elems, itr);
                deepcopy_el_linked_list(set_expr, srci, desti, dest_elems,
                    set_type, module, name2memidx);
            }, []() {});
            llvm::Value* tmp = builder->CreateAdd(
                        itr,
                        llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            LLVM::CreateStore(*builder, tmp, copy_itr);
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);
        LLVM::CreateStore(*builder, dest_elems, get_pointer_to_elems(dest));
    }

    void LLVMSetSeparateChaining::deepcopy_el_linked_list(
        ASR::expr_t* set_expr, llvm::Value* srci, llvm::Value* desti, llvm::Value* dest_elems,
        ASR::Set_t* set_type, llvm::Module* module,
        std::map<std::string, std::map<std::string, int>>& name2memidx) {
        /**
         * C++ equivalent:
         *
         * // memory allocation done before calling this function
         *
         * while( src_itr != nullptr ) {
         *     deepcopy(src_el, curr_dest_ptr);
         *     src_itr = src_itr_next;
         *     if( src_next_exists ) {
         *         *next_ptr = *next_ptr + 1;
         *         curr_dest[1] = &dest_elems[*next_ptr];
         *         curr_dest = *curr_dest[1];
         *     }
         *     else {
         *         curr_dest[1] = nullptr;
         *     }
         * }
         *
         */
        src_itr = llvm_utils->CreateAlloca(llvm::Type::getInt8Ty(context)->getPointerTo());
        dest_itr = llvm_utils->CreateAlloca(llvm::Type::getInt8Ty(context)->getPointerTo());

        llvm::Type* el_struct_type = typecode2elstruct[ASRUtils::get_type_code(set_type->m_type)]->getPointerTo();
        LLVM::CreateStore(*builder,
            builder->CreateBitCast(srci, llvm::Type::getInt8Ty(context)->getPointerTo()),
            src_itr);
        LLVM::CreateStore(*builder,
            builder->CreateBitCast(desti, llvm::Type::getInt8Ty(context)->getPointerTo()),
            dest_itr);
        llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
        llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
        llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");
        // head
        llvm_utils->start_new_block(loophead);
        {
            llvm::Value *cond = builder->CreateICmpNE(
                llvm_utils->CreateLoad(src_itr),
                llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo())
            );
            builder->CreateCondBr(cond, loopbody, loopend);
        }

        // body
        llvm_utils->start_new_block(loopbody);
        {
            llvm::Value* curr_src = builder->CreateBitCast(llvm_utils->CreateLoad(src_itr),
                el_struct_type);
            llvm::Value* curr_dest = builder->CreateBitCast(llvm_utils->CreateLoad(dest_itr),
                el_struct_type);
            llvm::Value* src_el_ptr = llvm_utils->create_gep(curr_src, 0);
            llvm::Value *src_el = src_el_ptr;
            if( !LLVM::is_llvm_struct(set_type->m_type) ) {
                src_el = llvm_utils->CreateLoad(src_el_ptr);
            }
            llvm::Value* dest_el_ptr = llvm_utils->create_gep(curr_dest, 0);
            llvm_utils->deepcopy(set_expr, src_el, dest_el_ptr, set_type->m_type, set_type->m_type, module, name2memidx);

            llvm::Value* src_next_ptr = llvm_utils->CreateLoad(llvm_utils->create_gep(curr_src, 1));
            llvm::Value* curr_dest_next_ptr = llvm_utils->create_gep(curr_dest, 1);
            LLVM::CreateStore(*builder, src_next_ptr, src_itr);

            llvm::Value* src_next_exists = builder->CreateICmpNE(src_next_ptr,
                llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()));
            llvm_utils->create_if_else(src_next_exists, [&]() {
                llvm::Value* next_idx = llvm_utils->CreateLoad(next_ptr);
                llvm::Value* dest_next_ptr = llvm_utils->create_ptr_gep(dest_elems, next_idx);
                dest_next_ptr = builder->CreateBitCast(dest_next_ptr, llvm::Type::getInt8Ty(context)->getPointerTo());
                LLVM::CreateStore(*builder, dest_next_ptr, curr_dest_next_ptr);
                LLVM::CreateStore(*builder, dest_next_ptr, dest_itr);
                next_idx = builder->CreateAdd(next_idx, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                              llvm::APInt(32, 1)));
                LLVM::CreateStore(*builder, next_idx, next_ptr);
            }, [&]() {
                LLVM::CreateStore(*builder,
                    llvm::ConstantPointerNull::get(llvm::Type::getInt8Ty(context)->getPointerTo()),
                    curr_dest_next_ptr
                );
            });
        }

        builder->CreateBr(loophead);

        // end
        llvm_utils->start_new_block(loopend);
    }

    llvm::Value* LLVMSetInterface::len(llvm::Value* set) {
        return llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), get_pointer_to_occupancy(set));
    }

} // namespace LCompilers

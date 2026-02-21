#include "libasr/asr.h"
#include <libasr/codegen/llvm_array_utils.h>
#include <libasr/codegen/llvm_utils.h>
#include <libasr/asr_utils.h>
#include "llvm_array_utils.h"
#include <libasr/codegen/llvm_compat.h>

namespace LCompilers {

    namespace LLVMArrUtils {

        llvm::Value* lfortran_malloc(llvm::LLVMContext &context, llvm::Module &module,
                llvm::IRBuilder<> &builder, llvm::Value* arg_size) {
            std::string func_name = "_lfortran_malloc";
            llvm::Function *fn = module.getFunction(func_name);
            if (!fn) {
                llvm::FunctionType *function_type = llvm::FunctionType::get(
                        llvm::Type::getInt8Ty(context)->getPointerTo(), {
                            llvm::Type::getInt64Ty(context)
                        }, false);
                fn = llvm::Function::Create(function_type,
                        llvm::Function::ExternalLinkage, func_name, &module);
            }
            arg_size = builder.CreateSExt(arg_size, llvm::Type::getInt64Ty(context));
            std::vector<llvm::Value*> args = {arg_size};
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
                        llvm::Function::ExternalLinkage, func_name, &module);
            }
            std::vector<llvm::Value*> args = {
                builder.CreateBitCast(ptr, llvm::Type::getInt8Ty(context)->getPointerTo()),
                builder.CreateSExt(arg_size, llvm::Type::getInt64Ty(context))};
            return builder.CreateCall(fn, args);
        }

        bool compile_time_dimensions_t(ASR::dimension_t* m_dims, int n_dims) {
            if( n_dims <= 0 ) {
                return false;
            }
            bool is_ok = true;
            for( int r = 0; r < n_dims; r++ ) {
                if( m_dims[r].m_length == nullptr &&
                    m_dims[r].m_start == nullptr ) {
                    is_ok = false;
                    break;
                }
                if( m_dims[r].m_length == nullptr ) {
                    is_ok = false;
                    break;
                }
            }
            return is_ok;
        }

        bool is_explicit_shape(ASR::Variable_t* v) {
            ASR::dimension_t* m_dims = nullptr;
            int n_dims = 0;
            switch( v->m_type->type ) {
                case ASR::ttypeType::Array: {
                    ASR::Array_t* v_type = ASR::down_cast<ASR::Array_t>(v->m_type);
                    m_dims = v_type->m_dims;
                    n_dims = v_type->n_dims;
                    break;
                }
                default: {
                    throw LCompilersException("Explicit shape checking supported only for integer, real, complex, logical and derived types.");
                }
            }
            return compile_time_dimensions_t(m_dims, n_dims);
        }

        std::unique_ptr<Descriptor>
        Descriptor::get_descriptor
        (llvm::LLVMContext& context, llvm::IRBuilder<>* builder,
         LLVMUtils* llvm_utils, DESCR_TYPE descr_type,
         CompilerOptions& co) {
            // Descriptor index type: i64 if --descriptor-index-64 or -fdefault-integer-8, else i32
            llvm::Type* index_type = co.descriptor_index_64
                ? llvm::Type::getInt64Ty(context)
                : llvm::Type::getInt32Ty(context);
            switch( descr_type ) {
                case DESCR_TYPE::_SimpleCMODescriptor: {
                    return std::make_unique<SimpleCMODescriptor>(context, builder, llvm_utils, co, index_type);
                }
            }
            return nullptr;
        }

        SimpleCMODescriptor::SimpleCMODescriptor(llvm::LLVMContext& _context,
            llvm::IRBuilder<>* _builder, LLVMUtils* _llvm_utils, CompilerOptions& co_,
            llvm::Type* _index_type):
        context(_context),
        llvm_utils(std::move(_llvm_utils)),
        builder(std::move(_builder)),
        index_type(_index_type ? _index_type : llvm::Type::getInt32Ty(_context)),
        dim_des(nullptr),
        co(co_) {
            // Create dimension descriptor struct with the configured index type
            dim_des = llvm::StructType::create(
                context,
                std::vector<llvm::Type*>(
                    {index_type,   // stride
                     index_type,   // lower_bound
                     index_type}), // extent
                "dimension_descriptor");
        }

        static inline llvm::Value* load_if_pointer(llvm::Value* val, llvm::Type* target_type,
                                                     llvm::IRBuilder<>* /*builder*/, LLVMUtils* llvm_utils) {
            if (val->getType()->isPointerTy()) {
                return llvm_utils->CreateLoad2(target_type, val);
            }
            return val;
        }

        bool SimpleCMODescriptor::is_array(ASR::ttype_t* asr_type) {
            std::string asr_type_code = ASRUtils::get_type_code(
                ASRUtils::type_get_past_allocatable(asr_type), false, false);
            return (tkr2array.find(asr_type_code) != tkr2array.end() &&
                    ASRUtils::is_array(asr_type));
        }

        llvm::Value* SimpleCMODescriptor::
        convert_to_argument(llvm::Value* tmp, ASR::ttype_t* asr_arg_type,
                            llvm::Type* arg_type, bool data_only) {
            if( data_only ) {
                llvm::Type* t = llvm_utils->get_type_from_ttype_t_util(nullptr, ASRUtils::get_contained_type(asr_arg_type), llvm_utils->module);
                return llvm_utils->CreateLoad2(t, get_pointer_to_data(nullptr, ASRUtils::get_contained_type(asr_arg_type), tmp, llvm_utils->module));
            }
            llvm::Value* arg_struct = llvm_utils->CreateAlloca(*builder, arg_type);
            llvm::Value* first_ele_ptr = nullptr;
            std::string asr_arg_type_code = ASRUtils::get_type_code(ASRUtils::get_contained_type(asr_arg_type), false, false);
            llvm::Type* element_type = llvm_utils->get_type_from_ttype_t_util(
                nullptr, ASRUtils::get_contained_type(asr_arg_type), llvm_utils->module);
            llvm::StructType* tmp_struct_type = tkr2array[asr_arg_type_code].first;
            if( tmp_struct_type->getElementType(0)->isArrayTy() ) {
                first_ele_ptr = llvm_utils->create_gep2(element_type, get_pointer_to_data(arg_type, tmp), 0);
            } else if( tmp_struct_type->getNumElements() < 5 ) {
                first_ele_ptr = llvm_utils->CreateLoad2(element_type, get_pointer_to_data(arg_type, tmp));
            } else if( tmp_struct_type->getNumElements() == 5 ) {
                return tmp;
            }
            llvm::Value* first_arg_ptr = llvm_utils->create_gep2(arg_type, arg_struct, 0);
            builder->CreateStore(first_ele_ptr, first_arg_ptr);
            llvm::Value* sec_ele_ptr = get_offset(arg_type, tmp);
            llvm::Value* sec_arg_ptr = llvm_utils->create_gep2(arg_type, arg_struct, 1);
            builder->CreateStore(sec_ele_ptr, sec_arg_ptr);
            llvm::StructType* arg_struct_type = llvm::dyn_cast<llvm::StructType>(arg_type);
            LCOMPILERS_ASSERT(arg_struct_type != nullptr && "arg_type must be a StructType");
            llvm::Value* third_ele_ptr
                = llvm_utils->CreateLoad2(arg_struct_type->getElementType(2), get_pointer_to_dimension_descriptor_array(arg_type, tmp));
            llvm::Value* third_arg_ptr = llvm_utils->create_gep2(arg_type, arg_struct, 2);
            builder->CreateStore(third_ele_ptr, third_arg_ptr);
            return arg_struct;
        }

        llvm::Type* SimpleCMODescriptor::get_argument_type(llvm::Type* type,
        std::uint32_t m_h, std::string arg_name,
        std::unordered_map<std::uint32_t, std::unordered_map<std::string, llvm::Type*>>& arr_arg_type_cache) {
            llvm::StructType* type_struct = static_cast<llvm::StructType*>(type);
            llvm::Type* first_ele_ptr_type = nullptr;
            if( type_struct->getElementType(0)->isArrayTy() ) {
                llvm::ArrayType* arr_type = static_cast<llvm::ArrayType*>(type_struct->getElementType(0));
                llvm::Type* ele_type = arr_type->getElementType();
                first_ele_ptr_type = ele_type->getPointerTo();
            } else if( type_struct->getElementType(0)->isPointerTy() &&
                       type_struct->getNumElements() < 5 ) {
                first_ele_ptr_type = type_struct->getElementType(0);
            } else if( type_struct->getElementType(0)->isPointerTy() &&
                       type_struct->getNumElements() == 5 ) {
                arr_arg_type_cache[m_h][std::string(arg_name)] = type;
                return type->getPointerTo();
            }
            llvm::Type* new_arr_type = nullptr;

            if( arr_arg_type_cache.find(m_h) == arr_arg_type_cache.end() || (
                arr_arg_type_cache.find(m_h) != arr_arg_type_cache.end() &&
                arr_arg_type_cache[m_h].find(std::string(arg_name)) == arr_arg_type_cache[m_h].end() ) ) {
                std::vector<llvm::Type*> arg_des = {first_ele_ptr_type};
                for( size_t i = 1; i < type_struct->getNumElements(); i++ ) {
                    arg_des.push_back(static_cast<llvm::StructType*>(type)->getElementType(i));
                }
                new_arr_type = llvm::StructType::create(context, arg_des, "array_call");
                arr_arg_type_cache[m_h][std::string(arg_name)] = new_arr_type;
            } else {
                new_arr_type = arr_arg_type_cache[m_h][std::string(arg_name)];
            }
            return new_arr_type->getPointerTo();
        }

        llvm::Type* SimpleCMODescriptor::get_array_type
        (ASR::expr_t* expr, ASR::ttype_t* m_type_, llvm::Type* el_type,
        bool get_pointer) {
            std::string array_key = ASRUtils::get_type_code(m_type_, false, false, true, expr);
            if (ASRUtils::is_character(*m_type_)) {
                ASR::String_t* str_type = ASR::down_cast<ASR::String_t>(ASRUtils::extract_type(m_type_));
                if (str_type->m_physical_type == ASR::string_physical_typeType::DescriptorString) {
                    array_key += "desc";
                }
            }
            if( tkr2array.find(array_key) != tkr2array.end() ) {
                if( get_pointer ) {
                    return tkr2array[array_key].first->getPointerTo();
                }
                return tkr2array[array_key].first;
            }
            llvm::Type* dim_des_array = create_dimension_descriptor_array_type();
            std::vector<llvm::Type*> array_type_vec;
            // Array descriptor layout:
            // 0: data pointer
            // 1: offset (uses index_type for large array support)
            // 2: dimension descriptor array pointer
            // 3: is_allocated flag
            // 4: rank (always i32, small value)
            array_type_vec = {  el_type->getPointerTo(),
                                index_type,
                                dim_des_array,
                                llvm::Type::getInt1Ty(context),
                                llvm::Type::getInt32Ty(context)  };
            llvm::StructType* new_array_type = llvm::StructType::create(context, array_type_vec, "array");
            tkr2array[array_key] = std::make_pair(new_array_type, el_type);
            if( get_pointer ) {
                return tkr2array[array_key].first->getPointerTo();
            }
            return (llvm::Type*) tkr2array[array_key].first;
        }

        llvm::Type* SimpleCMODescriptor::create_dimension_descriptor_array_type() {
            return dim_des->getPointerTo();
        }

        llvm::Type* SimpleCMODescriptor::get_dimension_descriptor_type
        (bool get_pointer) {
            if( !get_pointer ) {
                return dim_des;
            }
            return dim_des->getPointerTo();
        }

        llvm::Value* SimpleCMODescriptor::allocate_descriptor_on_heap(
            llvm::Type* array_desc_type, size_t n_dims) {
            llvm::DataLayout data_layout(llvm_utils->module->getDataLayout());
            int64_t desc_size = data_layout.getTypeAllocSize(array_desc_type);
            llvm::Value* desc_mem = lfortran_malloc(context, *llvm_utils->module, *builder,
                llvm::ConstantInt::get(llvm_utils->getIntType(4), llvm::APInt(32, desc_size)));
            builder->CreateMemSet(desc_mem, llvm::ConstantInt::get(
                context, llvm::APInt(8, 0)),
                llvm::ConstantInt::get(llvm_utils->getIntType(4), llvm::APInt(32, desc_size)),
                llvm::MaybeAlign());
            llvm::Value* desc_ptr = builder->CreateBitCast(desc_mem, array_desc_type->getPointerTo());

            int64_t dim_size = data_layout.getTypeAllocSize(dim_des);
            llvm::Value* dim_mem = lfortran_malloc(context, *llvm_utils->module, *builder,
                llvm::ConstantInt::get(llvm_utils->getIntType(4), llvm::APInt(32, n_dims * dim_size)));
            builder->CreateMemSet(dim_mem, llvm::ConstantInt::get(
                context, llvm::APInt(8, 0)),
                llvm::ConstantInt::get(llvm_utils->getIntType(4), llvm::APInt(32, n_dims * dim_size)),
                llvm::MaybeAlign());

            llvm::Value* dim_ptr_ptr = get_pointer_to_dimension_descriptor_array(array_desc_type, desc_ptr, false);
            builder->CreateStore(builder->CreateBitCast(dim_mem, get_dimension_descriptor_type(true)), dim_ptr_ptr);
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, n_dims)),
                get_rank(array_desc_type, desc_ptr, true));

            return desc_ptr;
        }

        llvm::Value* SimpleCMODescriptor::
        get_pointer_to_dimension_descriptor_array(llvm::Type* type, llvm::Value* arr, bool load) {
            llvm::Value* dim_des_arr_ptr = llvm_utils->create_gep2(type, arr, 2);
            if( !load ) {
                return dim_des_arr_ptr;
            }
            return llvm_utils->CreateLoad2(dim_des->getPointerTo(), dim_des_arr_ptr);
        }

        llvm::Value* SimpleCMODescriptor::
        get_rank(llvm::Type* type, llvm::Value* arr, bool get_pointer) {
            llvm::Value* rank_ptr = llvm_utils->create_gep2(type, arr, 4);
            if( get_pointer ) {
                return rank_ptr;
            }
            llvm::Type *i32 = llvm::Type::getInt32Ty(context);
            return llvm_utils->CreateLoad2(i32, rank_ptr);
        }

        void SimpleCMODescriptor::
        set_rank(llvm::Type* type,llvm::Value* arr, llvm::Value* rank) {
            llvm::Value* rank_ptr = llvm_utils->create_gep2(type, arr, 4);
            LLVM::CreateStore(*builder, rank, rank_ptr);
        }

        llvm::Value* SimpleCMODescriptor::
        get_dimension_size(llvm::Value* dim_des_arr, llvm::Value* dim, bool load) {
            llvm::Value* dim_size = llvm_utils->create_gep2(dim_des, llvm_utils->create_ptr_gep2(dim_des, dim_des_arr, dim), 2);
            if( !load ) {
                return dim_size;
            }
            return llvm_utils->CreateLoad2(index_type, dim_size);
        }

        llvm::Value* SimpleCMODescriptor::
        get_dimension_size(llvm::Value* dim_des_arr, bool load) {
            llvm::Value* dim_size = llvm_utils->create_gep2(dim_des, dim_des_arr, 2);
            if( !load ) {
                return dim_size;
            }
            return llvm_utils->CreateLoad2(index_type, dim_size);
        }

        void SimpleCMODescriptor::fill_array_details(
        llvm::Type* arr_ty, llvm::Value* arr, llvm::Type* llvm_data_type, int n_dims,
        std::vector<std::pair<llvm::Value*, llvm::Value*>>& llvm_dims,
        llvm::Module* module, bool reserve_data_memory) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            llvm::Value* offset_val = llvm_utils->create_gep2(arr_ty, arr, 1);
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)), offset_val);
            llvm::Value* dim_des_val = llvm_utils->create_gep2(arr_ty, arr, 2);
            llvm::Value* arr_rank = llvm::ConstantInt::get(context, llvm::APInt(32, n_dims));
            llvm::Value* dim_des_first = llvm_utils->CreateAlloca(*builder, dim_des, arr_rank);
            builder->CreateStore(dim_des_first, dim_des_val);
            builder->CreateStore(arr_rank, get_rank(arr_ty,arr, true));
            dim_des_val = llvm_utils->CreateLoad2(dim_des->getPointerTo(), dim_des_val);
            llvm::Value* prod = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1));
            for( int r = 0; r < n_dims; r++ ) {
                llvm::Value* dim_val = llvm_utils->create_ptr_gep2(dim_des, dim_des_val, r);
                llvm::Value* s_val = llvm_utils->create_gep2(dim_des, dim_val, 0);
                llvm::Value* l_val = llvm_utils->create_gep2(dim_des, dim_val, 1);
                llvm::Value* dim_size_ptr = llvm_utils->create_gep2(dim_des, dim_val, 2);
                llvm::Value* first = builder->CreateSExtOrTrunc(load_if_pointer(llvm_dims[r].first, index_type, builder, llvm_utils), index_type);
                llvm::Value* dim_size = builder->CreateSExtOrTrunc(load_if_pointer(llvm_dims[r].second, index_type, builder, llvm_utils), index_type);
                builder->CreateStore(prod, s_val);
                builder->CreateStore(first, l_val);
                builder->CreateStore(dim_size, dim_size_ptr);
                prod = builder->CreateMul(prod, dim_size);
            }

            if( !reserve_data_memory ) {
                return ;
            }

            llvm::Value* llvm_size = llvm_utils->CreateAlloca(*builder, index_type);
            builder->CreateStore(prod, llvm_size);
            llvm::Value* first_ptr = get_pointer_to_data(arr_ty, arr);
            llvm::Value* arr_first = nullptr;

            if( !co.stack_arrays ) {
                llvm::DataLayout data_layout(module->getDataLayout());
                uint64_t size = data_layout.getTypeAllocSize(llvm_data_type);
                builder->CreateStore(builder->CreateMul(
                    llvm_utils->CreateLoad2(index_type, llvm_size),
                    llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, size))), llvm_size);
                llvm::Value* arr_first_i8 = lfortran_malloc(
                    context, *module, *builder, llvm_utils->CreateLoad2(index_type, llvm_size));
                arr_first = builder->CreateBitCast(
                    arr_first_i8, llvm_data_type->getPointerTo());
            } else {
                arr_first = llvm_utils->CreateAlloca(*builder,
                    llvm_data_type, llvm_utils->CreateLoad2(index_type, llvm_size));
            }
            builder->CreateStore(arr_first, first_ptr);
        }

        void SimpleCMODescriptor::fill_array_details(ASR::expr_t* src_expr, ASR::expr_t* dest_expr,
            llvm::Value* source, llvm::Value* destination,
            ASR::ttype_t* source_type, ASR::ttype_t* destination_type, llvm::Module* module, bool ignore_data) {
            if( !ignore_data ) {
                // TODO: Implement data filling to destination array
                LCOMPILERS_ASSERT(false);
            }

            llvm::Type *source_array_type = llvm_utils->get_type_from_ttype_t_util(src_expr, source_type, module);
            llvm::Type *dest_array_type = llvm_utils->get_type_from_ttype_t_util(dest_expr, destination_type, module);

            llvm::Value* source_offset_val = llvm_utils->CreateLoad2(
                index_type, llvm_utils->create_gep2(source_array_type, source, 1));
            llvm::Value* dest_offset = llvm_utils->create_gep2(dest_array_type, destination, 1);
            builder->CreateStore(source_offset_val, dest_offset);


            llvm::Value* source_dim_des_val = llvm_utils->CreateLoad2(
                dim_des->getPointerTo(), llvm_utils->create_gep2(source_array_type, source, 2));
            llvm::Value* dest_dim_des_ptr = llvm_utils->create_gep2(dest_array_type, destination, 2);
            builder->CreateStore(source_dim_des_val, dest_dim_des_ptr);

            llvm::Value* source_rank = this->get_rank(source_array_type, source, false);
            this->set_rank(dest_array_type, destination, source_rank);
        };

        void SimpleCMODescriptor::fill_malloc_array_details(
            llvm::Value* arr, llvm::Type* arr_type, llvm::Type* llvm_data_type, ASR::ttype_t* asr_type, int n_dims,
            std::vector<std::pair<llvm::Value*, llvm::Value*>>& llvm_dims, llvm::Value* string_len,
            ASR::symbol_t* const variable_declaration, llvm::Module* module, 
            ASR::symbol_t* allocated_subclass, bool realloc,
            ASR::ttype_t* alloc_type) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            arr = llvm_utils->CreateLoad2(arr_type->getPointerTo(), arr);
            llvm::Value* offset_val = llvm_utils->create_gep2(arr_type, arr, 1);
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)),
                                    offset_val);
            llvm::Value* dim_des_val = llvm_utils->CreateLoad2(dim_des->getPointerTo(), llvm_utils->create_gep2(arr_type, arr, 2));
            llvm::Value* prod = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1));
            for( int r = 0; r < n_dims; r++ ) {
                llvm::Value* dim_val = llvm_utils->create_ptr_gep2(dim_des, dim_des_val, r);
                llvm::Value* s_val = llvm_utils->create_gep2(dim_des, dim_val, 0);
                llvm::Value* l_val = llvm_utils->create_gep2(dim_des, dim_val, 1);
                llvm::Value* dim_size_ptr = llvm_utils->create_gep2(dim_des, dim_val, 2);
                llvm::Value* first = builder->CreateSExtOrTrunc(load_if_pointer(llvm_dims[r].first, index_type, builder, llvm_utils), index_type);
                llvm::Value* dim_size = builder->CreateSExtOrTrunc(load_if_pointer(llvm_dims[r].second, index_type, builder, llvm_utils), index_type);
                builder->CreateStore(prod, s_val);
                builder->CreateStore(first, l_val);
                builder->CreateStore(dim_size, dim_size_ptr);
                prod = builder->CreateMul(prod, dim_size);
            }
            llvm::Value* ptr2firstptr = get_pointer_to_data(arr_type,  arr);

            // Handle special allocation for strings. All Other types are handled the same.
            if(ASRUtils::is_character(*asr_type)){
                llvm_utils->allocate_allocatable_array_of_strings(
                    ASR::down_cast<ASR::String_t>(ASRUtils::extract_type(asr_type)),
                    llvm_utils->CreateLoad2(llvm_data_type->getPointerTo(), ptr2firstptr),
                    string_len,
                    prod,
                    realloc);
            } else if(ASRUtils::is_class_type(ASRUtils::extract_type(asr_type))){
                ASR::StructType_t* struct_type = ASR::down_cast<ASR::StructType_t>(
                    ASRUtils::extract_type(asr_type));
                if (struct_type->m_is_unlimited_polymorphic && alloc_type != nullptr
                        && !ASR::is_a<ASR::StructType_t>(*alloc_type)) {
                    // Unlimited polymorphic array with intrinsic type spec
                    // (e.g., allocate(integer :: arr(5)))
                    llvm_utils->struct_api->allocate_array_of_unlimited_polymorphic_type(
                        ASR::down_cast<ASR::Struct_t>(variable_declaration),
                        struct_type, ptr2firstptr, prod, alloc_type, realloc, module);
                } else {
                    llvm_utils->struct_api->allocate_array_of_classes(
                        ASR::down_cast<ASR::Struct_t>(variable_declaration)
                        , struct_type
                        , ptr2firstptr
                        , prod
                        , allocated_subclass
                        , realloc);
                }
            } else {
                llvm::DataLayout data_layout(module->getDataLayout());
                llvm::Type* ptr_type = llvm_data_type->getPointerTo();
                uint64_t size = data_layout.getTypeAllocSize(llvm_data_type);
                llvm::Value* llvm_size = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, size));
                prod = builder->CreateMul(prod, llvm_size);
                // lfortran_malloc/realloc takes i64 for size, so extend if needed
                llvm::Value* arg_size = builder->CreateSExtOrTrunc(prod, llvm::Type::getInt64Ty(context));
                llvm::Value* ptr_as_char_ptr = nullptr;
                if( realloc ) {
                    ptr_as_char_ptr = lfortran_realloc(context, *module,
                        *builder, llvm_utils->CreateLoad2(llvm_data_type->getPointerTo(), ptr2firstptr),
                        arg_size);
                } else {
                    ptr_as_char_ptr = lfortran_malloc(context, *module,
                        *builder, arg_size);
                }
                llvm::Value* first_ptr = builder->CreateBitCast(ptr_as_char_ptr, ptr_type);
                builder->CreateStore(first_ptr, ptr2firstptr);
            }
        }

        void SimpleCMODescriptor::fill_dimension_descriptor(llvm::Type* type, llvm::Value* arr, int n_dims) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            llvm::Value* dim_des_val = llvm_utils->create_gep2(type, arr, 2);
            llvm::Value* llvm_ndims = llvm_utils->CreateAlloca(*builder, llvm::Type::getInt32Ty(context));
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, n_dims)), llvm_ndims);
            llvm::Value* dim_des_first;
            dim_des_first = llvm_utils->CreateAlloca(*builder, dim_des,
                                    llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), llvm_ndims));

            // If unallocated, set lower bound and size to 1.
            // This is for entering the loop array_op pass generates to check if array is allocated in ArrayItem at runtime.
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)), llvm_utils->create_gep2(dim_des, dim_des_first, 1));
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)), llvm_utils->create_gep2(dim_des, dim_des_first, 2));

            builder->CreateStore(dim_des_first, dim_des_val);
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, n_dims)), get_rank(type, arr, true));
        }

        void SimpleCMODescriptor::reset_array_details(
            llvm::Type* type, llvm::Value* arr, llvm::Value* source_arr,
            llvm::Value** lbs, llvm::Value** lengths,
            int n_dims) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            llvm::Value* offset_val = llvm_utils->create_gep2(type, arr, 1);
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)), offset_val);
            llvm::Value* dim_des_val = this->get_pointer_to_dimension_descriptor_array(type, arr, false);
            llvm::Value* llvm_ndims = llvm_utils->CreateAlloca(*builder, llvm::Type::getInt32Ty(context), nullptr);
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, n_dims)), llvm_ndims);
            llvm::Value* dim_des_first = llvm_utils->CreateAlloca(*builder, dim_des,
                                                            llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), llvm_ndims));
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, n_dims)), get_rank(type, arr, true));
            builder->CreateStore(dim_des_first, dim_des_val);
            dim_des_val = llvm_utils->CreateLoad2(dim_des->getPointerTo(), dim_des_val);
            llvm::Value* source_dim_des_arr = this->get_pointer_to_dimension_descriptor_array(type, source_arr);
            for( int r = 0; r < n_dims; r++ ) {
                llvm::Value* dim_val = llvm_utils->create_ptr_gep2(dim_des, dim_des_val, r);
                llvm::Value* s_val = llvm_utils->create_gep2(dim_des, dim_val, 0);
                llvm::Value* stride = this->get_stride(
                    this->get_pointer_to_dimension_descriptor(source_dim_des_arr,
                    llvm::ConstantInt::get(context, llvm::APInt(32, r))));
                builder->CreateStore(stride, s_val);
                llvm::Value* l_val = llvm_utils->create_gep2(dim_des, dim_val, 1);
                llvm::Value* dim_size_ptr = llvm_utils->create_gep2(dim_des, dim_val, 2);
                if( lbs[r] == nullptr ) {
                    builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)), l_val);
                } else {
                    builder->CreateStore(builder->CreateSExtOrTrunc(load_if_pointer(lbs[r], index_type, builder, llvm_utils), index_type), l_val);
                }
                if( lengths[r] == nullptr ) {
                    llvm::Value* dim_size = this->get_dimension_size(
                    this->get_pointer_to_dimension_descriptor(source_dim_des_arr,
                        llvm::ConstantInt::get(context, llvm::APInt(32, r))));
                    builder->CreateStore(dim_size, dim_size_ptr);
                } else {
                    builder->CreateStore(builder->CreateSExtOrTrunc(load_if_pointer(lengths[r], index_type, builder, llvm_utils), index_type), dim_size_ptr);
                }
            }
        }

        void SimpleCMODescriptor::reset_array_details(llvm::Type* type, llvm::Value* arr, llvm::Type* const source_arr_type, llvm::Value* source_arr, int n_dims) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            llvm::Value* offset_val = llvm_utils->create_gep2(type, arr, 1);
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)), offset_val);
            llvm::Value* dim_des_val = llvm_utils->create_gep2(type, arr, 2);
            llvm::Value* llvm_ndims = llvm_utils->CreateAlloca(*builder, llvm::Type::getInt32Ty(context), nullptr);
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, n_dims)), llvm_ndims);
            llvm::Value* dim_des_first = llvm_utils->CreateAlloca(*builder, dim_des,
                                                               llvm_utils->CreateLoad2(llvm::Type::getInt32Ty(context), llvm_ndims));
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, n_dims)), get_rank(type, arr, true));
            builder->CreateStore(dim_des_first, dim_des_val);
            dim_des_val = llvm_utils->CreateLoad2(dim_des->getPointerTo(), dim_des_val);
            llvm::Value* source_dim_des_arr = this->get_pointer_to_dimension_descriptor_array(source_arr_type, source_arr);
            for( int r = 0; r < n_dims; r++ ) {
                llvm::Value* dim_val = llvm_utils->create_ptr_gep2(dim_des, dim_des_val, r);
                llvm::Value* s_val = llvm_utils->create_gep2(dim_des, dim_val, 0);
                llvm::Value* stride = this->get_stride(
                    this->get_pointer_to_dimension_descriptor(source_dim_des_arr,
                    llvm::ConstantInt::get(context, llvm::APInt(32, r))));
                builder->CreateStore(stride, s_val);
                llvm::Value* l_val = llvm_utils->create_gep2(dim_des, dim_val, 1);
                llvm::Value* dim_size_ptr = llvm_utils->create_gep2(dim_des, dim_val, 2);
                builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)), l_val);
                llvm::Value* dim_size = this->get_dimension_size(
                   this->get_pointer_to_dimension_descriptor(source_dim_des_arr,
                    llvm::ConstantInt::get(context, llvm::APInt(32, r))));
                builder->CreateStore(dim_size, dim_size_ptr);
            }
        }

        void SimpleCMODescriptor::fill_descriptor_for_array_section(
            llvm::Value* value_desc, llvm::Type *value_el_type, ASR::ttype_t* value_type,
            llvm::Value* target, ASR::ttype_t* target_type, ASR::expr_t* target_expr,
            llvm::Value** lbs, llvm::Value** ubs,
            llvm::Value** ds, llvm::Value** non_sliced_indices,
            int value_rank, int target_rank, LocationManager& lm) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            llvm::Value* value_desc_data = llvm_utils->CreateLoad2(value_el_type->getPointerTo(), get_pointer_to_data(target_expr, ASRUtils::type_get_past_allocatable_pointer(target_type), value_desc, llvm_utils->module));
            std::vector<llvm::Value*> section_first_indices;
            for( int i = 0; i < value_rank; i++ ) {
                if( ds[i] != nullptr ) {
                    LCOMPILERS_ASSERT(lbs[i] != nullptr);
                    section_first_indices.push_back(lbs[i]);
                } else {
                    LCOMPILERS_ASSERT(non_sliced_indices[i] != nullptr);
                    section_first_indices.push_back(non_sliced_indices[i]);
                }
            }
            llvm::Type* type_llvm = llvm_utils->get_type_from_ttype_t_util(
                target_expr, ASRUtils::type_get_past_allocatable_pointer(target_type), llvm_utils->module);
            llvm::Value* target_offset = cmo_convertor_single_element(
                type_llvm, value_desc, section_first_indices, value_rank, false, lm);

            if(ASRUtils::is_character(*value_type)){
                LCOMPILERS_ASSERT_MSG(ASRUtils::is_descriptorString(value_type),
                    "Only descriptor strings are supported for now");

                llvm::Value* desired_position = llvm_utils->get_string_element_in_array_(
                    ASRUtils::get_string_type(value_type), value_desc_data, target_offset);

                llvm::Value* target_str_data, *target_str_len;
                std::tie(target_str_data, target_str_len) = llvm_utils->get_string_length_data(
                    ASRUtils::get_string_type(target_type),
                    builder->CreateLoad(llvm_utils->get_StringType(target_type)->getPointerTo(), get_pointer_to_data(target_expr, target_type, target, llvm_utils->module)),
                    true, true);

                llvm::Value* value_str_length = llvm_utils->get_string_length(
                    ASRUtils::get_string_type(value_type), value_desc_data);

                builder->CreateStore(desired_position, target_str_data);
                builder->CreateStore(value_str_length, target_str_len);

            } else {
                value_desc_data = llvm_utils->create_ptr_gep2(value_el_type, value_desc_data, target_offset);
                builder->CreateStore(value_desc_data,  get_pointer_to_data(target_expr, target_type, target, llvm_utils->module));
            }
            llvm::Type* target_type_llvm = llvm_utils->get_type_from_ttype_t_util(target_expr, ASRUtils::type_get_past_allocatable_pointer(target_type), llvm_utils->module);
            builder->CreateStore(
                llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)),
                get_offset(target_type_llvm, target, false));
            llvm::Value* value_dim_des_array = get_pointer_to_dimension_descriptor_array(type_llvm, value_desc);
            llvm::Value* target_dim_des_array = get_pointer_to_dimension_descriptor_array(target_type_llvm, target);
            int j = 0;
            for( int i = 0; i < value_rank; i++ ) {
                if( ds[i] != nullptr ) {
                    llvm::Value* ubsi = builder->CreateSExtOrTrunc(load_if_pointer(ubs[i], index_type, builder, llvm_utils), index_type);
                    llvm::Value* lbsi = builder->CreateSExtOrTrunc(load_if_pointer(lbs[i], index_type, builder, llvm_utils), index_type);
                    llvm::Value* dsi = builder->CreateSExtOrTrunc(load_if_pointer(ds[i], index_type, builder, llvm_utils), index_type);
                    llvm::Value* dim_length = builder->CreateAdd(
                        builder->CreateSDiv(builder->CreateSub(ubsi, lbsi), dsi),
                        llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1))
                        );
                    llvm::Value* zero = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0));
                    // If dim_length is negative then set it to zero
                    dim_length = builder->CreateSelect(builder->CreateICmpSLT(dim_length, zero), zero, dim_length);
                    llvm::Value* value_dim_des = llvm_utils->create_ptr_gep2(dim_des, value_dim_des_array, i);
                    llvm::Value* target_dim_des = llvm_utils->create_ptr_gep2(dim_des, target_dim_des_array, j);
                    llvm::Value* value_stride = get_stride(value_dim_des, true);
                    llvm::Value* target_stride = get_stride(target_dim_des, false);
                    builder->CreateStore(builder->CreateMul(value_stride, builder->CreateSExtOrTrunc(
                        load_if_pointer(ds[i], index_type, builder, llvm_utils), index_type)), target_stride);
                    // Diverges from LPython, 0 should be stored there.
                    builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)),
                                         get_lower_bound(target_dim_des, false));
                    builder->CreateStore(dim_length,
                                         get_dimension_size(target_dim_des, false));
                    j++;
                }
            }
            LCOMPILERS_ASSERT(j == target_rank);
            builder->CreateStore(
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                       llvm::APInt(32, target_rank)),
                get_rank(target_type_llvm, target, true));
        }

        void SimpleCMODescriptor::fill_descriptor_for_array_section_data_only(
            llvm::Value* value_desc, llvm::Type* value_el_type, ASR::ttype_t* value_type,
            llvm::Value* target, ASR::ttype_t* target_type, ASR::expr_t* target_expr,
            llvm::Value** lbs, llvm::Value** ubs,
            llvm::Value** ds, llvm::Value** non_sliced_indices,
            llvm::Value** llvm_diminfo, int value_rank, int target_rank, LocationManager& lm) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            std::vector<llvm::Value*> section_first_indices;
            for( int i = 0; i < value_rank; i++ ) {
                if( ds[i] != nullptr ) {
                    LCOMPILERS_ASSERT(lbs[i] != nullptr);
                    section_first_indices.push_back(lbs[i]);
                } else {
                    LCOMPILERS_ASSERT(non_sliced_indices[i] != nullptr);
                    section_first_indices.push_back(non_sliced_indices[i]);
                }
            }
            llvm::Value* target_offset = cmo_convertor_single_element_data_only(
                llvm_diminfo, section_first_indices, value_rank, false, lm);
            if(ASRUtils::is_character(*value_type)){
                LCOMPILERS_ASSERT_MSG(ASRUtils::is_descriptorString(value_type),
                    "Only descriptor strings are supported for now");

                llvm::Value* desired_position = llvm_utils->get_string_element_in_array_(
                    ASRUtils::get_string_type(value_type), value_desc, target_offset);

                llvm::Value* target_str_data, *target_str_len;
                std::tie(target_str_data, target_str_len) = llvm_utils->get_string_length_data(
                    ASRUtils::get_string_type(target_type),
                    builder->CreateLoad(llvm_utils->get_StringType(target_type)->getPointerTo(), get_pointer_to_data(target_expr, target_type, target, llvm_utils->module)),
                    true, true);


                llvm::Value* value_str_length = llvm_utils->get_string_length(
                    ASRUtils::get_string_type(value_type), value_desc);

                builder->CreateStore(desired_position, target_str_data);
                builder->CreateStore(value_str_length, target_str_len);

            } else {
                value_desc = llvm_utils->create_ptr_gep2(value_el_type, value_desc, target_offset);
                builder->CreateStore(value_desc, get_pointer_to_data(target_expr, target_type, target, llvm_utils->module));
            }
            llvm::Type* target_type_llvm = llvm_utils->get_type_from_ttype_t_util(
                target_expr, ASRUtils::type_get_past_allocatable_pointer(target_type), llvm_utils->module);
            builder->CreateStore(
                llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)),
                get_offset(target_type_llvm,target, false));

            llvm::Value* target_dim_des_array = get_pointer_to_dimension_descriptor_array(target_type_llvm, target);
            int j = 0, r = 1;
            llvm::Value* stride = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1));
            for( int i = 0; i < value_rank; i++ ) {
                if( ds[i] != nullptr ) {
                    llvm::Value* ubsi = builder->CreateSExtOrTrunc(load_if_pointer(ubs[i], index_type, builder, llvm_utils), index_type);
                    llvm::Value* lbsi = builder->CreateSExtOrTrunc(load_if_pointer(lbs[i], index_type, builder, llvm_utils), index_type);
                    llvm::Value* dsi = builder->CreateSExtOrTrunc(load_if_pointer(ds[i], index_type, builder, llvm_utils), index_type);
                    llvm::Value* dim_length = builder->CreateAdd(
                        builder->CreateSDiv(builder->CreateSub(ubsi, lbsi), dsi),
                        llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1))
                        );
                    llvm::Value* zero = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0));
                    // If dim_length is negative then set it to zero
                    dim_length = builder->CreateSelect(builder->CreateICmpSLT(dim_length, zero), zero, dim_length);
                    llvm::Value* target_dim_des = llvm_utils->create_ptr_gep2(dim_des, target_dim_des_array, j);
                    builder->CreateStore(builder->CreateMul(stride, builder->CreateSExtOrTrunc(
                        load_if_pointer(ds[i], index_type, builder, llvm_utils), index_type)), get_stride(target_dim_des, false));
                    builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)),
                                         get_lower_bound(target_dim_des, false));
                    builder->CreateStore(dim_length,
                                         get_dimension_size(target_dim_des, false));
                    j++;
                }
                // Convert dimension info to index_type to match descriptor stride format
                stride = builder->CreateMul(stride,
                    builder->CreateSExtOrTrunc(load_if_pointer(llvm_diminfo[r], index_type, builder, llvm_utils), index_type));
                r += 2;
            }
            LCOMPILERS_ASSERT(j == target_rank);
            builder->CreateStore(
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                       llvm::APInt(32, target_rank)),
                get_rank(target_type_llvm, target, true));
        }

        llvm::Value* SimpleCMODescriptor::get_pointer_to_dimension_descriptor(llvm::Value* dim_des_arr,
            llvm::Value* dim) {
            return llvm_utils->create_ptr_gep2(dim_des, dim_des_arr, dim);
        }

        llvm::Value* SimpleCMODescriptor::get_pointer_to_data(llvm::Type* type, llvm::Value* arr) {
            return llvm_utils->create_gep2(type, arr, 0);
        }

        llvm::Value* SimpleCMODescriptor::get_pointer_to_data(ASR::expr_t* arr_expr, ASR::ttype_t* arr_type, llvm::Value* arr, llvm::Module* module) {
            llvm::Type* const array_desc_type = llvm_utils->arr_api->
                get_array_type(arr_expr, ASRUtils::type_get_past_allocatable_pointer(arr_type),
                    llvm_utils->get_el_type(arr_expr, ASRUtils::extract_type(arr_type), module), false);
            return llvm_utils->create_gep2(array_desc_type, arr, 0);
        }

        llvm::Value* SimpleCMODescriptor::get_offset(llvm::Type* type, llvm::Value* arr, bool load) {
            llvm::Value* offset = llvm_utils->create_gep2(type, arr, 1);
            if( !load ) {
                return offset;
            }
            return llvm_utils->CreateLoad2(index_type, offset);
        }

        llvm::Value* SimpleCMODescriptor::get_lower_bound(llvm::Value* dims, bool load) {
            llvm::Value* lb = llvm_utils->create_gep2(dim_des, dims, 1);
            if( !load ) {
                return lb;
            }
            return llvm_utils->CreateLoad2(index_type, lb);
        }

        llvm::Value* SimpleCMODescriptor::get_upper_bound(llvm::Value* dims) {
            llvm::Value* lb = llvm_utils->CreateLoad2(index_type, llvm_utils->create_gep2(dim_des, dims, 1));
            llvm::Value* dim_size = llvm_utils->CreateLoad2(index_type, llvm_utils->create_gep2(dim_des, dims, 2));
            unsigned bit_width = index_type->getIntegerBitWidth();
            return builder->CreateSub(builder->CreateAdd(dim_size, lb),
                                      llvm::ConstantInt::get(context, llvm::APInt(bit_width, 1)));
        }

        llvm::Value* SimpleCMODescriptor::get_stride(llvm::Value* dims, bool load) {
            llvm::Value* stride = llvm_utils->create_gep2(dim_des, dims, 0);
            if( !load ) {
                return stride;
            }
            return llvm_utils->CreateLoad2(index_type, stride);
        }

        llvm::Value* SimpleCMODescriptor::cmo_convertor_single_element(
            llvm::Type* type, llvm::Value* arr, std::vector<llvm::Value*>& m_args,
            int n_args, bool check_for_bounds, LocationManager& lm, std::string array_name, std::string infile, Location loc) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            llvm::Value* dim_des_arr_ptr = llvm_utils->CreateLoad2(
                dim_des->getPointerTo(), llvm_utils->create_gep2(type, arr, 2));
            llvm::Value* idx = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0));
            for( int r = 0; r < n_args; r++ ) {
                llvm::Value* req_idx = m_args[r];
                llvm::Value* curr_llvm_idx = req_idx;
                llvm::Value* dim_des_ptr = llvm_utils->create_ptr_gep2(dim_des, dim_des_arr_ptr, r);
                llvm::Value* lval = llvm_utils->CreateLoad2(index_type, llvm_utils->create_gep2(dim_des, dim_des_ptr, 1));
                llvm::Value* length = llvm_utils->CreateLoad2(index_type, llvm_utils->create_gep2(dim_des, dim_des_ptr, 2));
                // Cast req_idx to index_type
                req_idx = builder->CreateSExtOrTrunc(req_idx, index_type);
                curr_llvm_idx = builder->CreateSub(req_idx, lval);
                if( check_for_bounds ) {
                    llvm::Value* dimension = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, r + 1));
                    llvm::Value* ubound = builder->CreateSub(builder->CreateAdd(lval, length),
                                                            llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)));
                    llvm::Value* lbound_check = builder->CreateICmpSLT(req_idx, lval);
                    llvm::Value* ubound_check = builder->CreateICmpSGT(req_idx, ubound);

                    llvm_utils->generate_runtime_error(builder->CreateOr(
                                                            lbound_check,
                                                            ubound_check),
                                                "Array '%s' index out of bounds. Tried to access index %d of dimension %d, but valid range is %d to %d.",
                                                {LLVMUtils::RuntimeLabel("", {loc})},
                                                     infile,
                                                     lm,
                                                     LCompilers::create_global_string_ptr(context, *builder->GetInsertBlock()->getParent()->getParent(), *builder, array_name),
                                                     req_idx,
                                                     dimension,
                                                     lval,
                                                     ubound);
                }
                llvm::Value* stride = llvm_utils->CreateLoad2(index_type, llvm_utils->create_gep2(dim_des, dim_des_ptr, 0));
                idx = builder->CreateAdd(idx, builder->CreateMul(stride, curr_llvm_idx));
            }
            llvm::Value* offset_val = llvm_utils->CreateLoad2(index_type, llvm_utils->create_gep2(type, arr, 1));
            return builder->CreateAdd(idx, offset_val);
        }

        llvm::Value* SimpleCMODescriptor::cmo_convertor_single_element_data_only(
            llvm::Value** llvm_diminfo, std::vector<llvm::Value*>& m_args,
            int n_args, bool check_for_bounds,LocationManager& lm, bool is_unbounded_pointer_to_data, std::string array_name, std::string infile, Location loc) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            llvm::Value* prod = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1));
            llvm::Value* idx = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0));
            for( int r = 0, r1 = 0; r < n_args; r++ ) {
                llvm::Value* req_idx = m_args[r];
                llvm::Value* curr_llvm_idx = req_idx;
                llvm::Value* lval = llvm_diminfo[r1];
                if (lval->getType()->isPointerTy()) {
                    lval = llvm_utils->CreateLoad2(index_type, lval);
                }
                // Cast req_idx to index_type
                req_idx = builder->CreateSExtOrTrunc(req_idx, index_type);
                lval = builder->CreateSExtOrTrunc(lval, index_type);
                curr_llvm_idx = builder->CreateSub(req_idx, lval);
                idx = builder->CreateAdd(idx, builder->CreateMul(prod, curr_llvm_idx));
                if (is_unbounded_pointer_to_data) {
                    r1 += 1;
                } else {
                    llvm::Value* dim_size = llvm_diminfo[r1 + 1];
                    if (dim_size->getType()->isPointerTy()) {
                        dim_size = llvm_utils->CreateLoad2(index_type, dim_size);
                    }
                    dim_size = builder->CreateSExtOrTrunc(dim_size, index_type);
                    r1 += 2;
                    if( check_for_bounds ) {
                        llvm::Value* dimension = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), llvm::APInt(32, r + 1));
                        llvm::Value* ubound = builder->CreateSub(builder->CreateAdd(lval, dim_size),
                                llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)));
                        llvm::Value* lbound_check = builder->CreateICmpSLT(req_idx, lval);
                        llvm::Value* ubound_check = builder->CreateICmpSGT(req_idx, ubound);
                        llvm_utils->generate_runtime_error(builder->CreateOr(
                                                                lbound_check,
                                                                ubound_check),
                                                "Runtime error: Array '%s' index out of bounds. Tried to access index %d of dimension %d, but valid range is %d to %d.",
                                                        {LLVMUtils::RuntimeLabel("", {loc})},
                                                        infile,
                                                        lm,
                                                        LCompilers::create_global_string_ptr(context, *builder->GetInsertBlock()->getParent()->getParent(), *builder, array_name),
                                                        req_idx,
                                                        dimension,
                                                        lval,
                                                        ubound);
                    }
                    prod = builder->CreateMul(prod, dim_size);
                }
            }
            return idx;
        }

        llvm::Value* SimpleCMODescriptor::get_single_element(llvm::Type *type, llvm::Value* array,
            std::vector<llvm::Value*>& m_args, int n_args, ASR::ttype_t* asr_type, ASR::expr_t* expr, LocationManager& lm,
            ASR::symbol_t* const variable_type_decl, bool data_only,
            bool is_fixed_size, llvm::Value** llvm_diminfo, bool polymorphic,
            llvm::Type* polymorphic_type, bool is_unbounded_pointer_to_data, bool check_for_bounds, std::string array_name, std::string infile) {
            llvm::Value* tmp = nullptr;
            llvm::Value* idx = nullptr;
            if( data_only || is_fixed_size ) {
                LCOMPILERS_ASSERT(llvm_diminfo);
                idx = cmo_convertor_single_element_data_only(llvm_diminfo, m_args, n_args, check_for_bounds, lm, is_unbounded_pointer_to_data, array_name, infile, expr->base.loc);
                if(ASRUtils::is_character(*asr_type)){// Special handling for array of strings.
                    tmp = llvm_utils->get_string_element_in_array(ASR::down_cast<ASR::String_t>(ASRUtils::extract_type(asr_type)), array, idx);
                } else if(ASRUtils::is_class_type(ASRUtils::extract_type(asr_type))){
                    tmp = llvm_utils->get_class_element_from_array(ASR::down_cast<ASR::Struct_t>(variable_type_decl),
                        ASR::down_cast<ASR::StructType_t>(ASRUtils::extract_type(asr_type)), array, idx);
                } else {
                    if( is_fixed_size ) {
                        tmp = llvm_utils->create_gep2(type, array, idx);
                    } else {
                        tmp = llvm_utils->create_ptr_gep2(type, array, idx);
                    }
                }
            } else {
                llvm::Type* array_type = llvm_utils->get_type_from_ttype_t_util(
                    expr, ASRUtils::type_get_past_allocatable_pointer(asr_type), llvm_utils->module);
                idx = cmo_convertor_single_element(array_type, array, m_args, n_args, check_for_bounds, lm, array_name, infile, expr->base.loc);
                llvm::Value* ptr_to_data_ptr = get_pointer_to_data(
                    expr, ASRUtils::type_get_past_allocatable_pointer(asr_type), array, llvm_utils->module);
                llvm::Value* full_array = nullptr;
                if(ASRUtils::is_character(*asr_type)){
                    full_array = llvm_utils->CreateLoad2(type->getPointerTo(), ptr_to_data_ptr);
                    tmp = llvm_utils->get_string_element_in_array(ASR::down_cast<ASR::String_t>(ASRUtils::extract_type(asr_type)), full_array, idx);
                } else if(ASRUtils::non_unlimited_polymorphic_class(ASRUtils::extract_type(asr_type))){
                    full_array = llvm_utils->CreateLoad2(type->getPointerTo(), ptr_to_data_ptr);
                    tmp = llvm_utils->get_class_element_from_array(
                        ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(variable_type_decl)),
                        ASR::down_cast<ASR::StructType_t>(ASRUtils::extract_type(asr_type)),
                        full_array,
                        idx);
                } else {
                    if( polymorphic ) {
                        if (variable_type_decl == nullptr) {
                            full_array = llvm_utils->CreateLoad2(type->getPointerTo(), ptr_to_data_ptr);
                            tmp = llvm_utils->create_ptr_gep2(type, full_array, idx);
                        } else {
                            ASR::symbol_t* decl_sym = ASRUtils::symbol_get_past_external(variable_type_decl);
                            if (!ASR::is_a<ASR::Struct_t>(*decl_sym)) {
                                full_array = llvm_utils->CreateLoad2(type->getPointerTo(), ptr_to_data_ptr);
                                tmp = llvm_utils->create_ptr_gep2(type, full_array, idx);
                            } else {
                                ASR::Struct_t* class_sym = ASR::down_cast<ASR::Struct_t>(decl_sym);

                                // In a `select type` block, the selector's ASR type can be narrowed
                                // (e.g., `integer(:)`), while the underlying data may still be stored
                                // as boxed values (class wrapper). Reload the data pointer as
                                // `class_type*`, then unwrap `.data`.
                                llvm::Type* class_type = llvm_utils->getClassType(class_sym, false);
                                llvm::Type* class_type_ptr = class_type->getPointerTo();
                                llvm::Value* casted_ptr_to_data_ptr = builder->CreateBitCast(
                                    ptr_to_data_ptr, class_type_ptr->getPointerTo());
                                full_array = llvm_utils->CreateLoad2(class_type_ptr, casted_ptr_to_data_ptr);

                                llvm::Value* data_field_ptr = llvm_utils->create_gep2(class_type, full_array, 1);
                                llvm::Type* data_field_type = class_type->getStructElementType(1);
                                llvm::Value* data_ptr = llvm_utils->CreateLoad2(data_field_type, data_field_ptr);
                                
                                llvm::Value* vptr_ptr = llvm_utils->create_gep2(class_type, full_array, 0);
                                llvm::Value* vptr = llvm_utils->CreateLoad2(llvm_utils->vptr_type, vptr_ptr);
                                llvm::Value* element_ptr_i8 = llvm_utils->get_polymorphic_array_data_ptr(data_ptr, idx, vptr);
                                tmp = builder->CreateBitCast(element_ptr_i8, polymorphic_type->getPointerTo());
                            }
                        }
                    } else {
                        full_array = llvm_utils->CreateLoad2(type->getPointerTo(), ptr_to_data_ptr);
                        tmp = llvm_utils->create_ptr_gep2(type, full_array, idx);
                    }
                }
            }
            return tmp;
        }

        llvm::Value* SimpleCMODescriptor::get_is_allocated_flag(llvm::Value* array, ASR::expr_t* array_exp) {
            llvm::Value* memory_holder{}; // ptr_ptr_to_data
            ASR::ttype_t* array_type = ASRUtils::expr_type(array_exp);
            // Check if array descriptor pointer is NULL
            // Example :: For allocatable members in derived types
            // If the descriptor pointer itself is NULL, the array is not allocated
            llvm::BasicBlock *entryBB = builder->GetInsertBlock();
            llvm::Value* array_ptr_int = builder->CreatePtrToInt(array, llvm::Type::getInt64Ty(context));
            llvm::Value* is_desc_null = builder->CreateICmpEQ(array_ptr_int, 
                llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 0));
            // Create blocks for conditional execution
            llvm::Function *fn = entryBB->getParent();
            llvm::BasicBlock *checkDataBB = llvm::BasicBlock::Create(context, "check_data", fn);
            llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "merge_allocated", fn);
            // If descriptor is NULL, skip to merge with false result
            builder->CreateCondBr(is_desc_null, mergeBB, checkDataBB);
            // Check data pointer block
            builder->SetInsertPoint(checkDataBB);

            llvm::PointerType* const memory_holder_type = [&](){
                if(ASRUtils::is_character(*array_type)) {
                    return llvm_utils->character_type;
                } else if(ASRUtils::non_unlimited_polymorphic_class(array_type)){
                    ASR::Struct_t* const struct_sym = ASR::down_cast<ASR::Struct_t>(
                         ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(array_exp)));
                    return llvm_utils->getStructType(struct_sym, llvm_utils->module)->getPointerTo();
                } else {
                    // Use the array element *storage* type (e.g. logical arrays are i8-backed).
                    return llvm_utils->get_el_type(
                                    array_exp,
                                    ASRUtils::extract_type(array_type),
                                    llvm_utils->module)->getPointerTo();
                } 
            }();

            llvm::BasicBlock *wrapperNullBB = nullptr;  // Only used for polymorphic types
            if(ASRUtils::is_character(*array_type)){
                llvm::Type* load_type = llvm_utils->get_type_from_ttype_t_util(
                    array_exp,
                    ASRUtils::extract_type(array_type), llvm_utils->module)->getPointerTo();
                ASR::String_t* str = ASRUtils::get_string_type(array_type);

                memory_holder = llvm_utils->get_string_data(
                        str,
                        builder->CreateLoad(load_type, get_pointer_to_data(array_exp, array_type, array, llvm_utils->module)), true);
            } else if(ASRUtils::non_unlimited_polymorphic_class(array_type)){
                auto const struct_sym = ASR::down_cast<ASR::Struct_t>( ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(array_exp)));
                llvm::Type* const class_type = llvm_utils->getClassType(struct_sym);
                llvm::Value* const array_data =  builder->CreateLoad(class_type->getPointerTo(), get_pointer_to_data(array_exp, array_type, array, llvm_utils->module));
                // Check if wrapper pointer is null before dereferencing
                llvm::Value* wrapper_ptr_int = builder->CreatePtrToInt(array_data, llvm::Type::getInt64Ty(context));
                llvm::Value* is_wrapper_null = builder->CreateICmpEQ(wrapper_ptr_int,
                    llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 0));
                llvm::BasicBlock *checkWrapperDataBB = llvm::BasicBlock::Create(context, "check_wrapper_data", fn);
                wrapperNullBB = llvm::BasicBlock::Create(context, "wrapper_null", fn);
                builder->CreateCondBr(is_wrapper_null, wrapperNullBB, checkWrapperDataBB);
                // Wrapper is null - go to merge with false
                builder->SetInsertPoint(wrapperNullBB);
                builder->CreateBr(mergeBB);
                // Wrapper is not null - check data pointer
                builder->SetInsertPoint(checkWrapperDataBB);
                llvm::Value* const underlying_struct_ptr = llvm_utils->CreateGEP2(class_type, array_data, 1);
                memory_holder = underlying_struct_ptr;
            } else {
                memory_holder = get_pointer_to_data(array_exp, array_type, array, llvm_utils->module);
            }
            llvm::Value* is_data_allocated = builder->CreateICmpNE(
                builder->CreatePtrToInt(llvm_utils->CreateLoad2(memory_holder_type, memory_holder),
                    llvm::Type::getInt64Ty(context)),
                builder->CreatePtrToInt(llvm::ConstantPointerNull::get(memory_holder_type),
                    llvm::Type::getInt64Ty(context)));
            llvm::BasicBlock *finalDataCheckBB = builder->GetInsertBlock();
            builder->CreateBr(mergeBB);
            // Merge block with PHI node
            builder->SetInsertPoint(mergeBB);
            int numIncoming = wrapperNullBB ? 3 : 2;
            llvm::PHINode *result = builder->CreatePHI(llvm::Type::getInt1Ty(context), numIncoming, "is_allocated");
            result->addIncoming(llvm::ConstantInt::getFalse(context), entryBB); // from entry (NULL descriptor case)
            if (wrapperNullBB) {
                result->addIncoming(llvm::ConstantInt::getFalse(context), wrapperNullBB); // from wrapper_null (NULL wrapper case)
            }
            result->addIncoming(is_data_allocated, finalDataCheckBB); // from final data check block
            return result;
        }

        void SimpleCMODescriptor::reset_is_allocated_flag(llvm::Type* typ_tmp, llvm::Value* array,
            llvm::Type* llvm_data_type) {
            llvm::Value* ptr_to_data = get_pointer_to_data(typ_tmp, array);
            llvm::PointerType* data_ptr_type = llvm::cast<llvm::PointerType>(llvm_data_type->getPointerTo());
            builder->CreateStore(
                llvm::ConstantPointerNull::get(data_ptr_type),
                ptr_to_data
            );
        }

        llvm::Value* SimpleCMODescriptor::get_array_size(llvm::Type* type, llvm::Value* array, llvm::Value* dim, int kind, int dim_kind) {
            llvm::Value* dim_des_val = this->get_pointer_to_dimension_descriptor_array(type, array);
            llvm::Value* tmp = nullptr;
            if( dim ) {
                tmp = builder->CreateSub(dim, llvm::ConstantInt::get(context, llvm::APInt(dim_kind * 8, 1)));
                tmp = this->get_dimension_size(dim_des_val, tmp);
                tmp = builder->CreateSExtOrTrunc(tmp, llvm_utils->getIntType(kind));
                return tmp;
            }
            llvm::Value* rank = this->get_rank(type, array);
            llvm::Value* llvm_size = llvm_utils->CreateAlloca(llvm_utils->getIntType(kind));
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(kind * 8, 1)), llvm_size);

            llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
            llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
            llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

            llvm::Value* r = llvm_utils->CreateAlloca(llvm_utils->getIntType(4));
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, 0)), r);
            // head
            llvm_utils->start_new_block(loophead);
            llvm::Value *cond = builder->CreateICmpSLT(llvm_utils->CreateLoad2(llvm_utils->getIntType(4), r), rank);
            builder->CreateCondBr(cond, loopbody, loopend);

            // body
            llvm_utils->start_new_block(loopbody);
            llvm::Value* r_val = llvm_utils->CreateLoad2(llvm_utils->getIntType(4), r);
            llvm::Value* ret_val = llvm_utils->CreateLoad2(llvm_utils->getIntType(kind), llvm_size);
            llvm::Value* dim_size = this->get_dimension_size(dim_des_val, r_val);
            dim_size = builder->CreateSExtOrTrunc(dim_size, llvm_utils->getIntType(kind));
            ret_val = builder->CreateMul(ret_val, dim_size);
            builder->CreateStore(ret_val, llvm_size);
            r_val = builder->CreateAdd(r_val, llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            builder->CreateStore(r_val, r);
            builder->CreateBr(loophead);

            // end
            llvm_utils->start_new_block(loopend);

            tmp = llvm_utils->CreateLoad2(llvm_utils->getIntType(kind), llvm_size);
            return tmp;
        }

        llvm::Value* SimpleCMODescriptor::reshape(llvm::Type* arr_type, llvm::Value* array, llvm::Type* llvm_data_type,
                                                  llvm::Type* shape_type, llvm::Value* shape, ASR::ttype_t* asr_shape_type,
                                                  llvm::Module* module) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            llvm::Value* reshaped = llvm_utils->CreateAlloca(*builder, arr_type, nullptr, "reshaped");

            // Deep copy data from array to reshaped.
            llvm::Value* num_elements = this->get_array_size(arr_type, array, nullptr, 4);

            llvm::Value* first_ptr = this->get_pointer_to_data(arr_type, reshaped);
            llvm::Value* arr_first = llvm_utils->CreateAlloca(*builder, llvm_data_type, num_elements);
            builder->CreateStore(arr_first, first_ptr);

            llvm::Value* ptr2firstptr = this->get_pointer_to_data(arr_type, array);
            llvm::DataLayout data_layout(module->getDataLayout());
            uint64_t size = data_layout.getTypeAllocSize(llvm_data_type);
            llvm::Value* llvm_size = llvm::ConstantInt::get(context, llvm::APInt(32, size));
            num_elements = builder->CreateMul(num_elements, llvm_size);
            builder->CreateMemCpy(llvm_utils->CreateLoad2(llvm_data_type->getPointerTo(), first_ptr), llvm::MaybeAlign(),
                                  llvm_utils->CreateLoad2(llvm_data_type->getPointerTo(), ptr2firstptr), llvm::MaybeAlign(),
                                  num_elements);

            builder->CreateStore(
                llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)),
                this->get_offset(arr_type, reshaped, false));

            if( ASRUtils::is_array(asr_shape_type) ) {
                llvm::Type *i32 = llvm::Type::getInt32Ty(context);
                builder->CreateStore(llvm_utils->CreateLoad2(index_type, llvm_utils->create_gep2(arr_type, array, 1)),
                            llvm_utils->create_gep2(arr_type, reshaped, 1));

                // Determine n_dims and a pointer to the shape data.
                // When the shape argument is a FixedSizeArray ([N x i32]) we
                // must not access descriptor fields that do not exist.
                llvm::Value* n_dims = nullptr;
                llvm::Value* shape_data = nullptr;
                ASR::array_physical_typeType shape_physical =
                    ASRUtils::extract_physical_type(asr_shape_type);
                if( shape_physical == ASR::array_physical_typeType::FixedSizeArray ) {
                    int64_t compile_time_n_dims =
                        ASRUtils::get_fixed_size_of_array(asr_shape_type);
                    n_dims = llvm::ConstantInt::get(context,
                        llvm::APInt(32, compile_time_n_dims));
                    shape_data = llvm_utils->create_gep2(shape_type, shape, 0);
                } else {
                    n_dims = this->get_array_size(shape_type, shape, nullptr, 4);
                    shape_data = llvm_utils->CreateLoad2(
                        i32->getPointerTo(),
                        this->get_pointer_to_data(shape_type, shape));
                }

                llvm::Value* dim_des_val = llvm_utils->create_gep2(arr_type, reshaped, 2);
                llvm::Value* dim_des_first = llvm_utils->CreateAlloca(*builder, dim_des, n_dims);
                builder->CreateStore(n_dims, this->get_rank(arr_type, reshaped, true));
                builder->CreateStore(dim_des_first, dim_des_val);
                llvm::Value* prod = llvm_utils->CreateAlloca(*builder, index_type);
                builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)), prod);
                dim_des_val = llvm_utils->CreateLoad2(dim_des->getPointerTo(), dim_des_val);
                llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
                llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
                llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

                llvm::Value* r = llvm_utils->CreateAlloca(*builder, llvm_utils->getIntType(4));
                builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, 0)), r);
                // head
                llvm_utils->start_new_block(loophead);
                llvm::Value *cond = builder->CreateICmpSLT(llvm_utils->CreateLoad2(llvm_utils->getIntType(4), r), n_dims);
                builder->CreateCondBr(cond, loopbody, loopend);

                // body
                llvm_utils->start_new_block(loopbody);
                llvm::Value* r_val = llvm_utils->CreateLoad2(llvm_utils->getIntType(4), r);
                llvm::Value* dim_val = llvm_utils->create_ptr_gep2(dim_des, dim_des_val, r_val);
                llvm::Value* s_val = llvm_utils->create_gep2(dim_des, dim_val, 0);
                llvm::Value* l_val = llvm_utils->create_gep2(dim_des, dim_val, 1);
                llvm::Value* dim_size_ptr = llvm_utils->create_gep2(dim_des, dim_val, 2);
                builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)), l_val);
                builder->CreateStore(llvm_utils->CreateLoad2(index_type, prod), s_val);
                llvm::Value* dim_size = builder->CreateSExtOrTrunc(llvm_utils->CreateLoad2(i32, llvm_utils->create_ptr_gep2(i32, shape_data, r_val)), index_type);
                builder->CreateStore(builder->CreateMul(llvm_utils->CreateLoad2(index_type, prod), dim_size), prod);
                builder->CreateStore(dim_size, dim_size_ptr);
                r_val = builder->CreateAdd(r_val, llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
                builder->CreateStore(r_val, r);
                builder->CreateBr(loophead);

                // end
                llvm_utils->start_new_block(loopend);
            }
            return reshaped;
        }

        // Shallow copies source array descriptor to destination descriptor
        void SimpleCMODescriptor::copy_array(llvm::Type* src_ty, llvm::Value* src, llvm::Type* dest_ty, llvm::Value* dest,
            llvm::Module* module, ASR::expr_t* array_expr, ASR::ttype_t* asr_data_type, bool reserve_memory) {
            llvm::Value* num_elements = this->get_array_size(src_ty, src, nullptr, 4);

            llvm::Value* first_ptr = this->get_pointer_to_data(dest_ty, dest);
            llvm::Type* llvm_data_type = llvm_utils->get_el_type(
                array_expr, ASRUtils::extract_type(asr_data_type), module);
            if( reserve_memory ) {
                llvm::Value* arr_first = llvm_utils->CreateAlloca(*builder, llvm_data_type, num_elements);
                builder->CreateStore(arr_first, first_ptr);
            }

            llvm::Value* ptr2firstptr = this->get_pointer_to_data(src_ty, src);
            llvm::DataLayout data_layout(module->getDataLayout());
            uint64_t size = data_layout.getTypeAllocSize(llvm_data_type);
            llvm::Value* llvm_size = llvm::ConstantInt::get(context, llvm::APInt(32, size));
            num_elements = builder->CreateMul(num_elements, llvm_size);
            builder->CreateMemCpy(llvm_utils->CreateLoad2(llvm_data_type->getPointerTo(), first_ptr), llvm::MaybeAlign(),
                                  llvm_utils->CreateLoad2(llvm_data_type->getPointerTo(), ptr2firstptr), llvm::MaybeAlign(),
                                  num_elements);

            llvm::Value* src_dim_des_val = this->get_pointer_to_dimension_descriptor_array(src_ty, src, true);
            llvm::Value* n_dims = this->get_rank(src_ty, src, false);
            llvm::Value* dest_dim_des_val = nullptr;
            dest_dim_des_val = this->get_pointer_to_dimension_descriptor_array(dest_ty, dest, true);
            llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
            llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
            llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");


            // Loop to copy `dimension_descriptor` from src to dest
            llvm::Value* r = llvm_utils->CreateAlloca(*builder, llvm_utils->getIntType(4));
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, 0)), r);
            // head
            llvm_utils->start_new_block(loophead);
            llvm::Value *cond = builder->CreateICmpSLT(llvm_utils->CreateLoad2(llvm_utils->getIntType(4), r), n_dims);
            builder->CreateCondBr(cond, loopbody, loopend);

            // body
            llvm_utils->start_new_block(loopbody);
            llvm::Value* r_val = llvm_utils->CreateLoad2(llvm_utils->getIntType(4), r);
            llvm::Value* src_dim_val = llvm_utils->create_ptr_gep2(dim_des, src_dim_des_val, r_val);
            llvm::Value* dest_dim_val = llvm_utils->create_ptr_gep2(dim_des, dest_dim_des_val, r_val);
            builder->CreateMemCpy(dest_dim_val, llvm::MaybeAlign(),
                                    src_dim_val, llvm::MaybeAlign(),
                                    llvm::ConstantInt::get(
                                    context, llvm::APInt(32, data_layout.getTypeAllocSize(dim_des))));
            r_val = builder->CreateAdd(r_val, llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            builder->CreateStore(r_val, r);
            builder->CreateBr(loophead);

            // end
            llvm_utils->start_new_block(loopend);


            builder->CreateStore(n_dims, this->get_rank(dest_ty , dest, true));
        }

        // Copy destination's descriptor to the source descriptor
        // Move the data pointer from dest to src
        // And set the src's data pointer to null to prevent double deallocation
        void SimpleCMODescriptor::copy_array_move_allocation(llvm::Type* src_ty, llvm::Value* src, llvm::Type* dest_ty, llvm::Value* dest,
            llvm::Module* module, ASR::expr_t* array_exp, ASR::ttype_t* asr_data_type) {

            llvm::Value* first_ptr = this->get_pointer_to_data(dest_ty, dest);
            llvm::Value* src_data_ptr = this->get_pointer_to_data(src_ty, src);
            llvm::Type* llvm_data_type =  llvm_utils->get_el_type(array_exp, ASRUtils::extract_type(asr_data_type), module);
            if(ASRUtils::is_character(*asr_data_type)){ // `{ %string_descriptor*, i32, %dimension_descriptor*, i1, i32 }` -- Copy The whole string descriptor.
                llvm::Value* dest_str_desc_loaded {}; // %string_descriptor*
                llvm::Value* src_str_desc_ptr {};// %string_descriptor
                dest_str_desc_loaded = builder->CreateLoad(llvm_data_type->getPointerTo(), first_ptr);
                src_str_desc_ptr = builder->CreateLoad(llvm_data_type, builder->CreateLoad(llvm_data_type->getPointerTo(), src_data_ptr));
                builder->CreateStore(src_str_desc_ptr, dest_str_desc_loaded);
            } else { // e.g. `{ %f64*, i32, %dimension_descriptor*, i1, i32 }`
                builder->CreateStore(builder->CreateLoad(llvm_data_type->getPointerTo(), src_data_ptr), first_ptr);
            }

            // Data pointer has been moved to dest, so set src's data pointer to null            
            if(ASRUtils::is_character(*asr_data_type)){ // `{ %string_descriptor*, i32, %dimension_descriptor*, i1, i32 }` -- Keep `string_descriptor` but clear its state.
                llvm::Value* src_data_ptr = llvm_utils->get_stringArray_data(asr_data_type /*Type Of Src*/, src, true);
                builder->CreateStore(llvm::ConstantPointerNull::get(llvm_utils->character_type), src_data_ptr);
            } else { // e.g. `{ %f64*, i32, %dimension_descriptor*, i1, i32 }`
                builder->CreateStore(llvm::ConstantPointerNull::get(llvm_data_type->getPointerTo()), src_data_ptr);
            }

            llvm::Value* src_offset = this->get_offset(dest_ty, src);
            llvm::Value* dest_offset = this->get_offset(dest_ty, dest, false);
            builder->CreateStore(src_offset, dest_offset);

            llvm::DataLayout data_layout(module->getDataLayout());
            llvm::Value* src_dim_des_val = this->get_pointer_to_dimension_descriptor_array(src_ty, src, true);
            llvm::Value* n_dims = this->get_rank(src_ty, src, false);
            llvm::Value* dest_dim_des_val = nullptr;
            dest_dim_des_val = this->get_pointer_to_dimension_descriptor_array(dest_ty, dest, true);
            llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
            llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
            llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");


            // Loop to copy `dimension_descriptor` from src to dest
            llvm::Value* r = llvm_utils->CreateAlloca(*builder, llvm_utils->getIntType(4));
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, 0)), r);
            // head
            llvm_utils->start_new_block(loophead);
            llvm::Value *cond = builder->CreateICmpSLT(llvm_utils->CreateLoad2(llvm_utils->getIntType(4), r), n_dims);
            builder->CreateCondBr(cond, loopbody, loopend);

            // body
            llvm_utils->start_new_block(loopbody);
            llvm::Value* r_val = llvm_utils->CreateLoad2(llvm_utils->getIntType(4), r);
            llvm::Value* src_dim_val = llvm_utils->create_ptr_gep2(dim_des, src_dim_des_val, r_val);
            llvm::Value* dest_dim_val = llvm_utils->create_ptr_gep2(dim_des, dest_dim_des_val, r_val);
            builder->CreateMemCpy(dest_dim_val, llvm::MaybeAlign(),
                                    src_dim_val, llvm::MaybeAlign(),
                                    llvm::ConstantInt::get(
                                    context, llvm::APInt(32, data_layout.getTypeAllocSize(dim_des))));
            r_val = builder->CreateAdd(r_val, llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            builder->CreateStore(r_val, r);
            builder->CreateBr(loophead);

            // end
            llvm_utils->start_new_block(loopend);


            builder->CreateStore(n_dims, this->get_rank(dest_ty , dest, true));
        }

        void SimpleCMODescriptor::copy_array_data_only(llvm::Value* src, llvm::Value* dest,
            llvm::Module* module, llvm::Type* llvm_data_type, ASR::ttype_t* arr_type,
            llvm::Value* num_elements) {
            if(ASRUtils::is_array_of_strings(arr_type)) llvm_data_type = llvm::Type::getInt8Ty(context);
            
            llvm::Value* llvm_size {};
            {
                llvm::DataLayout data_layout(module->getDataLayout());
                uint64_t size = data_layout.getTypeAllocSize(llvm_data_type);
                llvm_size = llvm::ConstantInt::get(context, llvm::APInt(32, size));
            }
            num_elements = builder->CreateMul(num_elements, llvm_size);
            builder->CreateMemCpy(src, llvm::MaybeAlign(), dest, llvm::MaybeAlign(), num_elements);
        }

        llvm::Value* SimpleCMODescriptor::create_contiguous_copy_from_descriptor(
            llvm::Type* source_llvm_type, llvm::Value* source_desc,
            llvm::Type* elem_type, int rank, llvm::Module* module) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            // Get dimension bounds from the descriptor first
            llvm::Value* dim_des_array = get_pointer_to_dimension_descriptor_array(
                source_llvm_type, source_desc, true);
            // Collect bounds and compute actual number of elements to copy
            std::vector<llvm::Value*> extents(rank);
            // Calculate number of elements as product of (ub - lb + 1) for each dimension
            llvm::Value* num_elements = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1));
            for (int d = 0; d < rank; d++) {
                llvm::Value* dim_des_elem = get_pointer_to_dimension_descriptor(
                    dim_des_array, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), d));
                llvm::Value* lb = get_lower_bound(dim_des_elem);
                llvm::Value* ub = get_upper_bound(dim_des_elem);
                // extent = ub - lb + 1
                extents[d] = builder->CreateSub(ub, lb);
                extents[d] = builder->CreateAdd(extents[d],
                    llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)));
                num_elements = builder->CreateMul(num_elements, extents[d]);
            }
            // Allocate contiguous data buffer on heap for only the
            // Number of elements to be copied
            llvm::DataLayout data_layout(module->getDataLayout());
            uint64_t elem_size = data_layout.getTypeAllocSize(elem_type);
            llvm::Value* llvm_elem_size = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, elem_size));
            llvm::Value* total_size = builder->CreateMul(num_elements, llvm_elem_size);
            llvm::Value* data_buffer_i8 = lfortran_malloc(context, *module, *builder, total_size);
            llvm::Value* data_buffer = builder->CreateBitCast(
                data_buffer_i8, elem_type->getPointerTo());
            llvm::Value* src_data = get_pointer_to_data(source_llvm_type, source_desc);
            src_data = llvm_utils->CreateLoad2(elem_type->getPointerTo(), src_data);
            // Single flat loop over all elements
            llvm::Value* iter_ptr = builder->CreateAlloca(index_type, nullptr, "copy_iter");
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)), iter_ptr);
            llvm_utils->create_loop("copy_array",
                [&]() {
                    llvm::Value* iter = llvm_utils->CreateLoad2(index_type, iter_ptr);
                    return builder->CreateICmpSLT(iter, num_elements);
                },
                [&]() {
                    llvm::Value* iter = llvm_utils->CreateLoad2(index_type, iter_ptr);
                    llvm::Value* linear_offset = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0));
                    llvm::Value* remaining = iter;
                    for (int d = 0; d < rank; d++) {
                        llvm::Value* dim_idx = builder->CreateSRem(remaining, extents[d]);
                        remaining = builder->CreateSDiv(remaining, extents[d]);
                        llvm::Value* dim_des_elem = get_pointer_to_dimension_descriptor(
                            dim_des_array, llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), d));
                        llvm::Value* stride = get_stride(dim_des_elem);
                        llvm::Value* dim_offset = builder->CreateMul(dim_idx, stride);
                        linear_offset = builder->CreateAdd(linear_offset, dim_offset);
                    }
                    llvm::Value* base_offset = get_offset(source_llvm_type, source_desc);
                    linear_offset = builder->CreateAdd(linear_offset, base_offset);
                    // Copy element
                    llvm::Value* src_elem_ptr = builder->CreateGEP(elem_type, src_data, linear_offset);
                    llvm::Value* elem_val = builder->CreateLoad(elem_type, src_elem_ptr);
                    llvm::Value* dest_ptr = builder->CreateGEP(elem_type, data_buffer, iter);
                    builder->CreateStore(elem_val, dest_ptr);
                    // Increment iterator
                    llvm::Value* new_iter = builder->CreateAdd(iter,
                        llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)));
                    builder->CreateStore(new_iter, iter_ptr);
                }
            );
            return data_buffer;
        }

    } // LLVMArrUtils

} // namespace LCompilers

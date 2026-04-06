#include "libasr/asr.h"
#include <libasr/codegen/llvm_array_utils.h>
#include <libasr/codegen/llvm_utils.h>
#include <libasr/asr_utils.h>
#include "llvm_array_utils.h"
#include <libasr/codegen/llvm_compat.h>

namespace LCompilers {

    namespace LLVMArrUtils {

        static llvm::Value* get_allocator(llvm::LLVMContext &context,
                llvm::Module &module, llvm::IRBuilder<> &builder) {
            llvm::Type* i8_ptr_type = llvm::Type::getInt8Ty(context)->getPointerTo();
            std::string func_name = LLVM::use_memory_debug()
                ? "_lfortran_get_compiler_mem_dbg_allocator"
                : "_lfortran_get_default_allocator";
            llvm::Function *fn = module.getFunction(func_name);
            if (!fn) {
                llvm::FunctionType *ft = llvm::FunctionType::get(i8_ptr_type, {}, false);
                fn = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, func_name, &module);
            }
            return builder.CreateCall(fn, {});
        }

        llvm::Value* lfortran_malloc(llvm::LLVMContext &context, llvm::Module &module,
                llvm::IRBuilder<> &builder, llvm::Value* arg_size) {
            std::string func_name = "_lfortran_malloc_alloc";
            llvm::Type* i8_ptr_type = llvm::Type::getInt8Ty(context)->getPointerTo();
            llvm::Function *fn = module.getFunction(func_name);
            if (!fn) {
                llvm::FunctionType *function_type = llvm::FunctionType::get(
                        i8_ptr_type, {
                            i8_ptr_type,
                            llvm::Type::getInt64Ty(context)
                        }, false);
                fn = llvm::Function::Create(function_type,
                        llvm::Function::ExternalLinkage, func_name, &module);
            }
            arg_size = builder.CreateSExt(arg_size, llvm::Type::getInt64Ty(context));
            llvm::Value* allocator = get_allocator(context, module, builder);
            return builder.CreateCall(fn, {allocator, arg_size});
        }

        llvm::Value* lfortran_realloc(llvm::LLVMContext &context, llvm::Module &module,
                llvm::IRBuilder<> &builder, llvm::Value* ptr, llvm::Value* arg_size) {
            std::string func_name = "_lfortran_realloc_alloc";
            llvm::Type* i8_ptr_type = llvm::Type::getInt8Ty(context)->getPointerTo();
            llvm::Function *fn = module.getFunction(func_name);
            if (!fn) {
                llvm::FunctionType *function_type = llvm::FunctionType::get(
                        i8_ptr_type, {
                            i8_ptr_type,
                            i8_ptr_type,
                            llvm::Type::getInt64Ty(context)
                        }, false);
                fn = llvm::Function::Create(function_type,
                        llvm::Function::ExternalLinkage, func_name, &module);
            }
            llvm::Value* allocator = get_allocator(context, module, builder);
            return builder.CreateCall(fn, {
                allocator,
                builder.CreateBitCast(ptr, i8_ptr_type),
                builder.CreateSExt(arg_size, llvm::Type::getInt64Ty(context))
            });
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
            switch( descr_type ) {
                case DESCR_TYPE::_SimpleCMODescriptor: {
                    return std::make_unique<SimpleCMODescriptor>(context, builder, llvm_utils, co);
                }
            }
            return nullptr;
        }

        SimpleCMODescriptor::SimpleCMODescriptor(llvm::LLVMContext& _context,
            llvm::IRBuilder<>* _builder, LLVMUtils* _llvm_utils, CompilerOptions& co_,
            llvm::Type* /*_index_type*/):
        context(_context),
        llvm_utils(std::move(_llvm_utils)),
        builder(std::move(_builder)),
        index_type(llvm::Type::getInt64Ty(_context)),
        dim_des(nullptr),
        co(co_) {
            // CFI_dim_t: always i64 for lower_bound, extent, stride
            dim_des = llvm::StructType::create(
                context,
                std::vector<llvm::Type*>(
                    {llvm::Type::getInt64Ty(context),   // lower_bound
                     llvm::Type::getInt64Ty(context),   // extent
                     llvm::Type::getInt64Ty(context)}), // stride (sm)
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
            } else {
                return tmp;
            }
            llvm::Value* first_arg_ptr = llvm_utils->create_gep2(arg_type, arg_struct, 0);
            builder->CreateStore(first_ele_ptr, first_arg_ptr);
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
                       type_struct->getNumElements() >= 5 ) {
                // Full CFI descriptor - pass by pointer as-is
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
            int n_dims = ASRUtils::extract_n_dims_from_ttype(m_type_);
            if (n_dims == 0) {
                // Assumed-rank callee parameter: use CFI_MAX_RANK (15) as
                // conservative upper bound for the callee's descriptor type.
                // Call sites allocate correctly-sized descriptors based on
                // the actual argument rank (see asr_utils.h PhysicalCast
                // and asr_to_llvm.cpp polymorphic descriptor handling).
                n_dims = 15;
            }
            std::string array_key = ASRUtils::get_type_code(m_type_, false, false, true, expr);
            if (ASRUtils::is_character(*m_type_)) {
                ASR::String_t* str_type = ASR::down_cast<ASR::String_t>(ASRUtils::extract_type(m_type_));
                if (str_type->m_physical_type == ASR::string_physical_typeType::DescriptorString) {
                    array_key += "desc";
                }
            }
            array_key += "." + std::to_string(n_dims);
            if( tkr2array.find(array_key) != tkr2array.end() ) {
                if( get_pointer ) {
                    return tkr2array[array_key].first->getPointerTo();
                }
                return tkr2array[array_key].first;
            }
            std::vector<llvm::Type*> array_type_vec;
            // CFI_cdesc_t-compatible descriptor layout:
            // 0: base_addr   (element_type*) - data pointer (typed for LLVM 11)
            // 1: elem_len    (i64)           - element size in bytes
            // 2: version     (i32)           - descriptor version
            // 3: rank        (i8)            - CFI_rank_t
            // 4: type        (i8)            - CFI_type_t
            // 5: attribute   (i8)            - CFI_attribute_t
            // 6: extra       (i8)            - reserved byte
            // 7: offset      (i64)           - linear offset
            // 8: dim[n_dims] ([n_dims x {i64,i64,i64}]) - rank-sized trailing array
            array_type_vec = {  el_type->getPointerTo(),                        // 0: base_addr
                                llvm::Type::getInt64Ty(context),                // 1: elem_len
                                llvm::Type::getInt32Ty(context),                // 2: version
                                llvm::Type::getInt8Ty(context),                 // 3: rank
                                llvm::Type::getInt8Ty(context),                 // 4: type
                                llvm::Type::getInt8Ty(context),                 // 5: attribute
                                llvm::Type::getInt8Ty(context),                 // 6: extra
                                llvm::Type::getInt64Ty(context),                // 7: offset
                                llvm::ArrayType::get(dim_des, n_dims)  };       // 8: dim[n_dims]
            llvm::StructType* new_array_type = llvm::StructType::create(
                context, array_type_vec, "array." + std::to_string(n_dims));
            tkr2array[array_key] = std::make_pair(new_array_type, el_type);
            if( get_pointer ) {
                return tkr2array[array_key].first->getPointerTo();
            }
            return (llvm::Type*) tkr2array[array_key].first;
        }

        llvm::Type* SimpleCMODescriptor::get_dimension_descriptor_type
        (bool get_pointer) {
            if( !get_pointer ) {
                return dim_des;
            }
            return dim_des->getPointerTo();
        }

        llvm::Type* SimpleCMODescriptor::get_array_type_for_rank(
            llvm::Type* el_type, int n_dims) {
            auto key = std::make_pair(el_type, n_dims);
            auto it = rank_array_cache.find(key);
            if (it != rank_array_cache.end()) {
                return it->second;
            }
            std::vector<llvm::Type*> array_type_vec = {
                el_type->getPointerTo(),                        // 0: base_addr
                llvm::Type::getInt64Ty(context),                // 1: elem_len
                llvm::Type::getInt32Ty(context),                // 2: version
                llvm::Type::getInt8Ty(context),                 // 3: rank
                llvm::Type::getInt8Ty(context),                 // 4: type
                llvm::Type::getInt8Ty(context),                 // 5: attribute
                llvm::Type::getInt8Ty(context),                 // 6: extra
                llvm::Type::getInt64Ty(context),                // 7: offset
                llvm::ArrayType::get(dim_des, n_dims)           // 8: dim[n_dims]
            };
            llvm::StructType* new_type = llvm::StructType::create(
                context, array_type_vec, "array." + std::to_string(n_dims));
            rank_array_cache[key] = new_type;
            return new_type;
        }

        llvm::Value* SimpleCMODescriptor::allocate_descriptor_on_heap(
            llvm::Type* array_desc_type) {
            llvm::DataLayout data_layout(llvm_utils->module->getDataLayout());
            int64_t desc_size = data_layout.getTypeAllocSize(array_desc_type);
            llvm::Value* desc_mem = lfortran_malloc(context, *llvm_utils->module, *builder,
                llvm::ConstantInt::get(llvm_utils->getIntType(4), llvm::APInt(32, desc_size)));
            llvm::Value* desc_ptr = builder->CreateBitCast(desc_mem, array_desc_type->getPointerTo());

            llvm::StructType* struct_type = llvm::dyn_cast<llvm::StructType>(array_desc_type);
            llvm::ArrayType* dims_type = llvm::dyn_cast<llvm::ArrayType>(
                struct_type->getElementType(FIELD_DIMS));
            uint64_t n_dims = dims_type->getNumElements();
            set_rank(array_desc_type, desc_ptr,
                llvm::ConstantInt::get(context, llvm::APInt(32, n_dims)));

            // Initialize data pointer to null so that a subsequent realloc
            // does not try to free an uninitialized (garbage) pointer.
            llvm::Value* data_ptr = llvm_utils->create_gep2(
                array_desc_type, desc_ptr, FIELD_BASE_ADDR);
            llvm::Type* data_field_type = struct_type->getElementType(FIELD_BASE_ADDR);
            builder->CreateStore(
                llvm::ConstantPointerNull::get(
                    llvm::cast<llvm::PointerType>(data_field_type)),
                data_ptr);

            return desc_ptr;
        }

        llvm::Value* SimpleCMODescriptor::create_descriptor_alloca(
            llvm::Type* array_desc_type, const std::string& name) {
            return llvm_utils->CreateAlloca(array_desc_type, nullptr, name);
        }

        llvm::Value* SimpleCMODescriptor::
        get_pointer_to_dimension_descriptor_array(llvm::Type* type, llvm::Value* arr, bool /*load*/) {
            // Dims are always inlined at FIELD_DIMS. Return pointer to first element.
            llvm::StructType* arr_ty = llvm::dyn_cast<llvm::StructType>(type);
            LCOMPILERS_ASSERT(arr_ty != nullptr);
            llvm::Type* dims_array_ty = arr_ty->getElementType(FIELD_DIMS);
            llvm::Value* dims_array_ptr = llvm_utils->create_gep2(type, arr, FIELD_DIMS);
            return llvm_utils->create_gep2(dims_array_ty, dims_array_ptr, 0);
        }

        llvm::Value* SimpleCMODescriptor::
        get_rank(llvm::Type* type, llvm::Value* arr, bool get_pointer) {
            llvm::Value* rank_ptr = llvm_utils->create_gep2(type, arr, FIELD_RANK);
            if( get_pointer ) {
                return rank_ptr;
            }
            llvm::Value* raw = llvm_utils->CreateLoad2(llvm::Type::getInt8Ty(context), rank_ptr);
            return builder->CreateZExt(raw, llvm::Type::getInt32Ty(context));
        }

        void SimpleCMODescriptor::
        set_rank(llvm::Type* type, llvm::Value* arr, llvm::Value* rank) {
            llvm::Value* rank_ptr = llvm_utils->create_gep2(type, arr, FIELD_RANK);
            llvm::Value* trunc = builder->CreateIntCast(rank, llvm::Type::getInt8Ty(context), false);
            LLVM::CreateStore(*builder, trunc, rank_ptr);
        }

        llvm::Value* SimpleCMODescriptor::
        get_dimension_size(llvm::Value* dim_des_arr, llvm::Value* dim, bool load) {
            llvm::Value* dim_size = llvm_utils->create_gep2(dim_des, llvm_utils->create_ptr_gep2(dim_des, dim_des_arr, dim), DIM_EXTENT);
            if( !load ) {
                return dim_size;
            }
            return llvm_utils->CreateLoad2(index_type, dim_size);
        }

        llvm::Value* SimpleCMODescriptor::
        get_dimension_size(llvm::Value* dim_des_arr, bool load) {
            llvm::Value* dim_size = llvm_utils->create_gep2(dim_des, dim_des_arr, DIM_EXTENT);
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
            llvm::Value* offset_val = llvm_utils->create_gep2(arr_ty, arr, FIELD_OFFSET);
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)), offset_val);
            set_rank(arr_ty, arr, llvm::ConstantInt::get(context, llvm::APInt(32, n_dims)));
            llvm::Value* dim_des_val = get_pointer_to_dimension_descriptor_array(arr_ty, arr);
            llvm::Value* prod = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1));
            for( int r = 0; r < n_dims; r++ ) {
                llvm::Value* dim_val = llvm_utils->create_ptr_gep2(dim_des, dim_des_val, r);
                llvm::Value* s_val = llvm_utils->create_gep2(dim_des, dim_val, DIM_STRIDE);
                llvm::Value* l_val = llvm_utils->create_gep2(dim_des, dim_val, DIM_LOWER_BOUND);
                llvm::Value* dim_size_ptr = llvm_utils->create_gep2(dim_des, dim_val, DIM_EXTENT);
                llvm::Value* first = builder->CreateSExtOrTrunc(load_if_pointer(llvm_dims[r].first, index_type, builder, llvm_utils), index_type);
                llvm::Value* dim_size = builder->CreateSExtOrTrunc(load_if_pointer(llvm_dims[r].second, index_type, builder, llvm_utils), index_type);
                // Fortran standard: negative extent means zero-size array
                llvm::Value* zero = llvm::ConstantInt::get(dim_size->getType(), 0);
                dim_size = builder->CreateSelect(
                    builder->CreateICmpSLT(dim_size, zero), zero, dim_size);
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

            llvm::Value* source_offset_val = this->get_offset(source_array_type, source);
            builder->CreateStore(source_offset_val, this->get_offset(dest_array_type, destination, false));

            llvm::Value* source_rank = this->get_rank(source_array_type, source, false);
            this->set_rank(dest_array_type, destination, source_rank);

            llvm::Value* source_dim_des_val = get_pointer_to_dimension_descriptor_array(source_array_type, source);
            llvm::Value* dest_dim_des_val = get_pointer_to_dimension_descriptor_array(dest_array_type, destination);
            llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
            llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
            llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");
            llvm::Value* r = llvm_utils->CreateAlloca(*builder, llvm_utils->getIntType(4));
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, 0)), r);
            llvm_utils->start_new_block(loophead);
            llvm::Value *cond = builder->CreateICmpSLT(llvm_utils->CreateLoad2(llvm_utils->getIntType(4), r), source_rank);
            builder->CreateCondBr(cond, loopbody, loopend);
            llvm_utils->start_new_block(loopbody);
            llvm::Value* r_val = llvm_utils->CreateLoad2(llvm_utils->getIntType(4), r);
            llvm::Value* src_dim_val = llvm_utils->create_ptr_gep2(dim_des, source_dim_des_val, r_val);
            llvm::Value* dst_dim_val = llvm_utils->create_ptr_gep2(dim_des, dest_dim_des_val, r_val);
            llvm::DataLayout data_layout(module->getDataLayout());
            builder->CreateMemCpy(dst_dim_val, llvm::MaybeAlign(), src_dim_val, llvm::MaybeAlign(),
                llvm::ConstantInt::get(context, llvm::APInt(32, data_layout.getTypeAllocSize(dim_des))));
            r_val = builder->CreateAdd(r_val, llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            builder->CreateStore(r_val, r);
            builder->CreateBr(loophead);
            llvm_utils->start_new_block(loopend);
        };

        void SimpleCMODescriptor::fill_malloc_array_details(
            llvm::Value* arr, llvm::Type* arr_type, llvm::Type* llvm_data_type, ASR::ttype_t* asr_type, int n_dims,
            std::vector<std::pair<llvm::Value*, llvm::Value*>>& llvm_dims, llvm::Value* string_len,
            ASR::symbol_t* const variable_declaration, llvm::Module* module, 
            ASR::symbol_t* allocated_subclass, bool realloc,
            ASR::ttype_t* alloc_type) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            arr = llvm_utils->CreateLoad2(arr_type->getPointerTo(), arr);
            llvm::Value* offset_val = llvm_utils->create_gep2(arr_type, arr, FIELD_OFFSET);
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)),
                                    offset_val);
            llvm::Value* dim_des_val = get_pointer_to_dimension_descriptor_array(arr_type, arr);
            llvm::Value* prod = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1));
            for( int r = 0; r < n_dims; r++ ) {
                llvm::Value* dim_val = llvm_utils->create_ptr_gep2(dim_des, dim_des_val, r);
                llvm::Value* s_val = llvm_utils->create_gep2(dim_des, dim_val, DIM_STRIDE);
                llvm::Value* l_val = llvm_utils->create_gep2(dim_des, dim_val, DIM_LOWER_BOUND);
                llvm::Value* dim_size_ptr = llvm_utils->create_gep2(dim_des, dim_val, DIM_EXTENT);
                llvm::Value* first = builder->CreateSExtOrTrunc(load_if_pointer(llvm_dims[r].first, index_type, builder, llvm_utils), index_type);
                llvm::Value* dim_size = builder->CreateSExtOrTrunc(load_if_pointer(llvm_dims[r].second, index_type, builder, llvm_utils), index_type);
                // Fortran standard: negative extent means zero-size array
                llvm::Value* zero = llvm::ConstantInt::get(dim_size->getType(), 0);
                dim_size = builder->CreateSelect(
                    builder->CreateICmpSLT(dim_size, zero), zero, dim_size);
                builder->CreateStore(prod, s_val);
                builder->CreateStore(first, l_val);
                builder->CreateStore(dim_size, dim_size_ptr);
                prod = builder->CreateMul(prod, dim_size);
            }
            llvm::Value* ptr2firstptr = get_pointer_to_data(arr_type, arr);

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
                    // Uses ONE-wrapper layout: single {vptr, i8* → contiguous data}
                    llvm_utils->struct_api->allocate_array_of_unlimited_polymorphic_type(
                        ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(variable_declaration)),
                        struct_type, ptr2firstptr, prod, alloc_type, realloc, module,
                        string_len);
                } else if (struct_type->m_is_unlimited_polymorphic && alloc_type != nullptr
                        && ASR::is_a<ASR::StructType_t>(*alloc_type)) {
                    // Unlimited polymorphic array with struct type spec
                    // (e.g., allocate(type(point) :: arr(5)))
                    // Route through allocate_array_of_classes which already does ONE-wrapper
                    llvm_utils->struct_api->allocate_array_of_classes(
                        ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(variable_declaration)),
                        struct_type, ptr2firstptr, prod, allocated_subclass, realloc);
                } else if (struct_type->m_is_unlimited_polymorphic) {
                    // Unlimited polymorphic array without explicit type spec
                    // (e.g., from polymorphic array assignment lhs%value = rhs%value).
                    // Allocate ONE wrapper with NULL data; the assignment handler
                    // will copy data from the source wrapper.
                    llvm::Type* const class_type = llvm_utils->getClassType(
                        ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(variable_declaration)));
                    llvm::Value* wrapper_ptr = llvm_utils->alloc_zeroed_type(class_type);
                    builder->CreateStore(wrapper_ptr, ptr2firstptr);
                } else {
                    llvm_utils->struct_api->allocate_array_of_classes(
                        ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(variable_declaration))
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
            llvm::Value* dim_des_first = get_pointer_to_dimension_descriptor_array(type, arr);

            // Initialize all dimensions with default values (stride=0, lower_bound=1, size=1).
            // This ensures no uninitialized memory is read when the array_op pass
            // generates code to check array dimensions before the array is allocated.
            for (int i = 0; i < n_dims; i++) {
                llvm::Value* dim_val = llvm_utils->create_ptr_gep2(dim_des, dim_des_first, i);
                builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)), llvm_utils->create_gep2(dim_des, dim_val, DIM_LOWER_BOUND));
                builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)), llvm_utils->create_gep2(dim_des, dim_val, DIM_EXTENT));
                builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)), llvm_utils->create_gep2(dim_des, dim_val, DIM_STRIDE));
            }

            set_rank(type, arr, llvm::ConstantInt::get(context, llvm::APInt(32, n_dims)));
        }

        void SimpleCMODescriptor::reset_array_details(
            llvm::Type* type, llvm::Value* arr, llvm::Value* source_arr,
            llvm::Value** lbs, llvm::Value** lengths,
            int n_dims) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            llvm::Value* offset_val = llvm_utils->create_gep2(type, arr, FIELD_OFFSET);
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)), offset_val);
            set_rank(type, arr, llvm::ConstantInt::get(context, llvm::APInt(32, n_dims)));
            llvm::Value* dim_des_val = this->get_pointer_to_dimension_descriptor_array(type, arr);
            llvm::Value* source_dim_des_arr = this->get_pointer_to_dimension_descriptor_array(type, source_arr);
            for( int r = 0; r < n_dims; r++ ) {
                llvm::Value* dim_val = llvm_utils->create_ptr_gep2(dim_des, dim_des_val, r);
                llvm::Value* s_val = llvm_utils->create_gep2(dim_des, dim_val, DIM_STRIDE);
                llvm::Value* stride = this->get_stride(
                    this->get_pointer_to_dimension_descriptor(source_dim_des_arr,
                    llvm::ConstantInt::get(context, llvm::APInt(32, r))));
                builder->CreateStore(stride, s_val);
                llvm::Value* l_val = llvm_utils->create_gep2(dim_des, dim_val, DIM_LOWER_BOUND);
                llvm::Value* dim_size_ptr = llvm_utils->create_gep2(dim_des, dim_val, DIM_EXTENT);
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
            llvm::Value* offset_val = llvm_utils->create_gep2(type, arr, FIELD_OFFSET);
            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)), offset_val);
            set_rank(type, arr, llvm::ConstantInt::get(context, llvm::APInt(32, n_dims)));
            llvm::Value* dim_des_val = get_pointer_to_dimension_descriptor_array(type, arr);
            llvm::Value* source_dim_des_arr = this->get_pointer_to_dimension_descriptor_array(source_arr_type, source_arr);
            for( int r = 0; r < n_dims; r++ ) {
                llvm::Value* dim_val = llvm_utils->create_ptr_gep2(dim_des, dim_des_val, r);
                llvm::Value* s_val = llvm_utils->create_gep2(dim_des, dim_val, DIM_STRIDE);
                llvm::Value* stride = this->get_stride(
                    this->get_pointer_to_dimension_descriptor(source_dim_des_arr,
                    llvm::ConstantInt::get(context, llvm::APInt(32, r))));
                builder->CreateStore(stride, s_val);
                llvm::Value* l_val = llvm_utils->create_gep2(dim_des, dim_val, DIM_LOWER_BOUND);
                llvm::Value* dim_size_ptr = llvm_utils->create_gep2(dim_des, dim_val, DIM_EXTENT);
                builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)), l_val);
                llvm::Value* dim_size = this->get_dimension_size(
                   this->get_pointer_to_dimension_descriptor(source_dim_des_arr,
                    llvm::ConstantInt::get(context, llvm::APInt(32, r))));
                builder->CreateStore(dim_size, dim_size_ptr);
            }
        }

        void SimpleCMODescriptor::fill_descriptor_for_array_section(
            llvm::Value* value_desc, llvm::Type *value_el_type, ASR::ttype_t* value_type,
            llvm::Type* value_desc_type,
            llvm::Value* target, ASR::ttype_t* target_type, ASR::expr_t* /*target_expr*/,
            llvm::Type* target_desc_type,
            llvm::Value** lbs, llvm::Value** ubs,
            llvm::Value** ds, llvm::Value** non_sliced_indices,
            int value_rank, int target_rank, LocationManager& lm) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            llvm::Type* value_type_llvm = value_desc_type;
            llvm::Value* value_desc_data = llvm_utils->CreateLoad2(
                value_el_type->getPointerTo(),
                get_pointer_to_data(value_type_llvm, value_desc));
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
            llvm::Value* target_offset = cmo_convertor_single_element(
                value_type_llvm, value_desc, section_first_indices, value_rank, false, lm);

            if(ASRUtils::is_character(*value_type)){
                LCOMPILERS_ASSERT_MSG(ASRUtils::is_descriptorString(value_type),
                    "Only descriptor strings are supported for now");

                llvm::Value* desired_position = llvm_utils->get_string_element_in_array_(
                    ASRUtils::get_string_type(value_type), value_desc_data, target_offset);

                llvm::Type* tgt_llvm_ty = target_desc_type;
                llvm::Value* target_str_data, *target_str_len;
                std::tie(target_str_data, target_str_len) = llvm_utils->get_string_length_data(
                    ASRUtils::get_string_type(target_type),
                    builder->CreateLoad(llvm_utils->get_StringType(target_type)->getPointerTo(), get_pointer_to_data(tgt_llvm_ty, target)),
                    true, true);

                llvm::Value* value_str_length = llvm_utils->get_string_length(
                    ASRUtils::get_string_type(value_type), value_desc_data);

                builder->CreateStore(desired_position, target_str_data);
                builder->CreateStore(value_str_length, target_str_len);

            } else {
                value_desc_data = llvm_utils->create_ptr_gep2(value_el_type, value_desc_data, target_offset);
                llvm::Type* target_type_llvm = target_desc_type;
                builder->CreateStore(value_desc_data, get_pointer_to_data(target_type_llvm, target));
            }
            llvm::Type* target_type_llvm = target_desc_type;
            builder->CreateStore(
                llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)),
                get_offset(target_type_llvm, target, false));
            llvm::Value* value_dim_des_array = get_pointer_to_dimension_descriptor_array(value_type_llvm, value_desc);
            llvm::Value* target_dim_des_array = get_pointer_to_dimension_descriptor_array(target_type_llvm, target);
            int j = 0;
            for( int i = 0; i < value_rank; i++ ) {
                if( ds[i] != nullptr ) {
                    llvm::Value* ubsi = builder->CreateSExtOrTrunc(load_if_pointer(ubs[i], index_type, builder, llvm_utils), index_type);
                    llvm::Value* lbsi = builder->CreateSExtOrTrunc(load_if_pointer(lbs[i], index_type, builder, llvm_utils), index_type);
                    llvm::Value* dsi = builder->CreateSExtOrTrunc(load_if_pointer(ds[i], index_type, builder, llvm_utils), index_type);
                    llvm::Value* raw_dim_length = builder->CreateAdd(
                        builder->CreateSDiv(builder->CreateSub(ubsi, lbsi), dsi),
                        llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1))
                        );
                    llvm::Value* zero = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0));
                    llvm::Value* step_pos = builder->CreateICmpSGT(dsi, zero);
                    llvm::Value* step_neg = builder->CreateICmpSLT(dsi, zero);
                    llvm::Value* ub_lt_lb = builder->CreateICmpSLT(ubsi, lbsi);
                    llvm::Value* ub_gt_lb = builder->CreateICmpSGT(ubsi, lbsi);
                    llvm::Value* pos_step_empty = builder->CreateAnd(step_pos, ub_lt_lb);
                    llvm::Value* neg_step_empty = builder->CreateAnd(step_neg, ub_gt_lb);
                    llvm::Value* use_zero = builder->CreateOr(pos_step_empty, neg_step_empty);
                    llvm::Value* dim_length = builder->CreateSelect(use_zero, zero, raw_dim_length);
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
            set_rank(target_type_llvm, target,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                       llvm::APInt(32, target_rank)));
        }

        void SimpleCMODescriptor::fill_descriptor_for_array_section_data_only(
            llvm::Value* value_desc, llvm::Type* value_el_type, ASR::ttype_t* value_type,
            llvm::Value* target, ASR::ttype_t* target_type, ASR::expr_t* /*target_expr*/,
            llvm::Type* target_desc_type,
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
                llvm::Type* tgt_llvm_ty = target_desc_type;
                std::tie(target_str_data, target_str_len) = llvm_utils->get_string_length_data(
                    ASRUtils::get_string_type(target_type),
                    builder->CreateLoad(llvm_utils->get_StringType(target_type)->getPointerTo(), get_pointer_to_data(tgt_llvm_ty, target)),
                    true, true);


                llvm::Value* value_str_length = llvm_utils->get_string_length(
                    ASRUtils::get_string_type(value_type), value_desc);

                builder->CreateStore(desired_position, target_str_data);
                builder->CreateStore(value_str_length, target_str_len);

            } else {
                value_desc = llvm_utils->create_ptr_gep2(value_el_type, value_desc, target_offset);
                llvm::Type* tgt_llvm_ty2 = target_desc_type;
                builder->CreateStore(value_desc, get_pointer_to_data(tgt_llvm_ty2, target));
            }
            llvm::Type* target_type_llvm = target_desc_type;
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
                    llvm::Value* raw_dim_length = builder->CreateAdd(
                        builder->CreateSDiv(builder->CreateSub(ubsi, lbsi), dsi),
                        llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1))
                        );
                    llvm::Value* zero = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0));
                    llvm::Value* step_pos = builder->CreateICmpSGT(dsi, zero);
                    llvm::Value* step_neg = builder->CreateICmpSLT(dsi, zero);
                    llvm::Value* ub_lt_lb = builder->CreateICmpSLT(ubsi, lbsi);
                    llvm::Value* ub_gt_lb = builder->CreateICmpSGT(ubsi, lbsi);
                    llvm::Value* pos_step_empty = builder->CreateAnd(step_pos, ub_lt_lb);
                    llvm::Value* neg_step_empty = builder->CreateAnd(step_neg, ub_gt_lb);
                    llvm::Value* use_zero = builder->CreateOr(pos_step_empty, neg_step_empty);
                    llvm::Value* dim_length = builder->CreateSelect(use_zero, zero, raw_dim_length);
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
            set_rank(target_type_llvm, target,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(context),
                                       llvm::APInt(32, target_rank)));
        }

        llvm::Value* SimpleCMODescriptor::get_pointer_to_dimension_descriptor(llvm::Value* dim_des_arr,
            llvm::Value* dim) {
            return llvm_utils->create_ptr_gep2(dim_des, dim_des_arr, dim);
        }

        llvm::Value* SimpleCMODescriptor::get_pointer_to_data(llvm::Type* type, llvm::Value* arr) {
            return llvm_utils->create_gep2(type, arr, FIELD_BASE_ADDR);
        }

        llvm::Value* SimpleCMODescriptor::get_pointer_to_data(ASR::expr_t* arr_expr, ASR::ttype_t* arr_type, llvm::Value* arr, llvm::Module* module) {
            llvm::Type* actual_type = llvm_utils->get_type_from_ttype_t_util(arr_expr,
                ASRUtils::type_get_past_allocatable_pointer(arr_type), module);
            return llvm_utils->create_gep2(actual_type, arr, FIELD_BASE_ADDR);
        }


        llvm::Value* SimpleCMODescriptor::get_offset(llvm::Type* type, llvm::Value* arr, bool load) {
            llvm::Value* offset = llvm_utils->create_gep2(type, arr, FIELD_OFFSET);
            if( !load ) {
                return offset;
            }
            return llvm_utils->CreateLoad2(llvm::Type::getInt64Ty(context), offset);
        }

        llvm::Value* SimpleCMODescriptor::get_lower_bound(llvm::Value* dims, bool load) {
            llvm::Value* lb = llvm_utils->create_gep2(dim_des, dims, DIM_LOWER_BOUND);
            if( !load ) {
                return lb;
            }
            return llvm_utils->CreateLoad2(index_type, lb);
        }

        llvm::Value* SimpleCMODescriptor::get_upper_bound(llvm::Value* dims) {
            llvm::Value* lb = llvm_utils->CreateLoad2(index_type, llvm_utils->create_gep2(dim_des, dims, DIM_LOWER_BOUND));
            llvm::Value* dim_size = llvm_utils->CreateLoad2(index_type, llvm_utils->create_gep2(dim_des, dims, DIM_EXTENT));
            unsigned bit_width = index_type->getIntegerBitWidth();
            return builder->CreateSub(builder->CreateAdd(dim_size, lb),
                                      llvm::ConstantInt::get(context, llvm::APInt(bit_width, 1)));
        }

        llvm::Value* SimpleCMODescriptor::get_stride(llvm::Value* dims, bool load) {
            llvm::Value* stride = llvm_utils->create_gep2(dim_des, dims, DIM_STRIDE);
            if( !load ) {
                return stride;
            }
            return llvm_utils->CreateLoad2(index_type, stride);
        }

        llvm::Value* SimpleCMODescriptor::cmo_convertor_single_element(
            llvm::Type* type, llvm::Value* arr, std::vector<llvm::Value*>& m_args,
            int n_args, bool check_for_bounds, LocationManager& lm, std::string array_name, std::string infile, Location loc) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            llvm::Value* dim_des_arr_ptr = get_pointer_to_dimension_descriptor_array(type, arr);
            llvm::Value* idx = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0));
            for( int r = 0; r < n_args; r++ ) {
                llvm::Value* req_idx = m_args[r];
                llvm::Value* curr_llvm_idx = req_idx;
                llvm::Value* dim_des_ptr = llvm_utils->create_ptr_gep2(dim_des, dim_des_arr_ptr, r);
                llvm::Value* lval = llvm_utils->CreateLoad2(index_type, llvm_utils->create_gep2(dim_des, dim_des_ptr, DIM_LOWER_BOUND));
                llvm::Value* length = llvm_utils->CreateLoad2(index_type, llvm_utils->create_gep2(dim_des, dim_des_ptr, DIM_EXTENT));
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
                llvm::Value* stride = llvm_utils->CreateLoad2(index_type, llvm_utils->create_gep2(dim_des, dim_des_ptr, DIM_STRIDE));
                idx = builder->CreateAdd(idx, builder->CreateMul(stride, curr_llvm_idx));
            }
            llvm::Value* offset_val = this->get_offset(type, arr);
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
                            // ONE-wrapper layout: data pointer holds a single
                            // wrapper {vptr, i8* data}. Compute the element
                            // at byte offset idx * elem_size from data.
                            full_array = llvm_utils->CreateLoad2(type->getPointerTo(), ptr_to_data_ptr);
                            // Read vptr and data from the single wrapper
                            llvm::Value* vptr_ptr = builder->CreateBitCast(
                                full_array, llvm_utils->vptr_type->getPointerTo());
                            llvm::Value* vptr = llvm_utils->CreateLoad2(llvm_utils->vptr_type, vptr_ptr);
                            llvm::Value* data_ptr = llvm_utils->CreateLoad2(llvm_utils->i8_ptr,
                                llvm_utils->create_gep2(type, full_array, 1));
                            llvm::Value* element_ptr = llvm_utils->get_polymorphic_array_data_ptr(data_ptr, idx, vptr);
                            // Allocate temp wrapper on stack, fill with vptr + element ptr
                            llvm::Value* temp_wrapper = llvm_utils->CreateAlloca(*builder, type);
                            builder->CreateStore(vptr, builder->CreateBitCast(
                                temp_wrapper, llvm_utils->vptr_type->getPointerTo()));
                            builder->CreateStore(element_ptr,
                                llvm_utils->create_gep2(type, temp_wrapper, 1));
                            tmp = temp_wrapper;
                        } else {
                            ASR::symbol_t* decl_sym = ASRUtils::symbol_get_past_external(variable_type_decl);
                            if (!ASR::is_a<ASR::Struct_t>(*decl_sym)) {
                                full_array = llvm_utils->CreateLoad2(type->getPointerTo(), ptr_to_data_ptr);
                                tmp = llvm_utils->create_ptr_gep2(type, full_array, idx);
                            } else {
                                ASR::Struct_t* class_sym = ASR::down_cast<ASR::Struct_t>(decl_sym);

                                llvm::Type* class_type = llvm_utils->getClassType(class_sym, false);
                                llvm::Type* class_type_ptr = class_type->getPointerTo();
                                llvm::Value* casted_ptr_to_data_ptr = builder->CreateBitCast(
                                    ptr_to_data_ptr, class_type_ptr->getPointerTo());
                                full_array = llvm_utils->CreateLoad2(class_type_ptr, casted_ptr_to_data_ptr);

                                // Check if vptr is NULL (freshly allocated empty wrapper)
                                llvm::Value* vptr_ptr = llvm_utils->create_gep2(class_type, full_array, 0);
                                llvm::Value* vptr = llvm_utils->CreateLoad2(llvm_utils->vptr_type, vptr_ptr);
                                llvm::Value* vptr_is_null = builder->CreateICmpEQ(
                                    builder->CreatePtrToInt(vptr, llvm::Type::getInt64Ty(context)),
                                    llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 0));

                                llvm::Value* data_field_ptr = llvm_utils->create_gep2(class_type, full_array, 1);
                                llvm::Type* data_field_type = class_type->getStructElementType(1);
                                llvm::Value* data_ptr = llvm_utils->CreateLoad2(data_field_type, data_field_ptr);

                                // Allocate temp wrapper on stack
                                llvm::Value* temp_wrapper = llvm_utils->CreateAlloca(*builder, class_type);

                                // Store vptr in field 0 (same for both cases)
                                builder->CreateStore(vptr, llvm_utils->create_gep2(class_type, temp_wrapper, 0));

                                // If vptr is non-NULL, compute element via byte offset
                                // If vptr is NULL (empty wrapper for assignment target),
                                // store NULL data (assignment will initialize it)
                                llvm_utils->create_if_else(vptr_is_null, [&]() {
                                    builder->CreateStore(
                                        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(data_field_type)),
                                        llvm_utils->create_gep2(class_type, temp_wrapper, 1));
                                }, [&]() {
                                    llvm::Value* element_ptr_i8 = llvm_utils->get_polymorphic_array_data_ptr(data_ptr, idx, vptr);
                                    builder->CreateStore(element_ptr_i8, llvm_utils->create_gep2(class_type, temp_wrapper, 1));
                                });

                                if (polymorphic_type != nullptr) {
                                    tmp = builder->CreateBitCast(
                                        llvm_utils->CreateLoad2(data_field_type,
                                            llvm_utils->create_gep2(class_type, temp_wrapper, 1)),
                                        polymorphic_type->getPointerTo());
                                } else {
                                    tmp = temp_wrapper;
                                }
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
                                                  llvm::Module* module, ASR::expr_t* array_expr,
                                                  ASR::ttype_t* asr_data_type,
                                                  llvm::Type* result_desc_type,
                                                  llvm::Value* order,
                                                  ASR::expr_t* order_expr) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();
            llvm::Type* result_type;
            if (result_desc_type) {
                result_type = result_desc_type;
            } else {
                int result_rank = 15;
                ASR::array_physical_typeType shape_phys =
                    ASRUtils::extract_physical_type(asr_shape_type);
                if (shape_phys == ASR::array_physical_typeType::FixedSizeArray) {
                    result_rank = (int)ASRUtils::get_fixed_size_of_array(asr_shape_type);
                }
                result_type = get_array_type_for_rank(llvm_data_type, result_rank);
            }
            llvm::Value* reshaped = create_descriptor_alloca(result_type, "reshaped");

            // Deep copy data from array to reshaped.
            llvm::Value* num_elements = this->get_array_size(arr_type, array, nullptr, 4);

            llvm::Value* first_ptr = this->get_pointer_to_data(result_type, reshaped);
            llvm::Value* arr_first = llvm_utils->CreateAlloca(*builder, llvm_data_type, num_elements);
            builder->CreateStore(arr_first, first_ptr);

            llvm::Value* ptr2firstptr = this->get_pointer_to_data(arr_type, array);

            bool is_struct_type = asr_data_type != nullptr &&
                ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(asr_data_type));
            bool has_order = (order != nullptr && order_expr != nullptr);

            if (!has_order && is_struct_type && array_expr != nullptr) {
                // For derived types with allocatable components, do element-wise
                // deep copy to avoid sharing allocatable component pointers.
                // Zero-initialize dest buffer so allocatable members start as null.
                llvm::DataLayout data_layout(module->getDataLayout());
                uint64_t elem_size = data_layout.getTypeAllocSize(llvm_data_type);
                llvm::Value* total_bytes = builder->CreateMul(num_elements,
                    llvm::ConstantInt::get(context, llvm::APInt(32, elem_size)));
                builder->CreateMemSet(arr_first,
                    llvm::ConstantInt::get(context, llvm::APInt(8, 0)),
                    total_bytes, llvm::MaybeAlign());

                llvm::Value* dest_data = llvm_utils->CreateLoad2(
                    llvm_data_type->getPointerTo(), first_ptr);
                llvm::Value* src_data = llvm_utils->CreateLoad2(
                    llvm_data_type->getPointerTo(), ptr2firstptr);

                llvm::BasicBlock *loopHead = llvm::BasicBlock::Create(
                    context, "reshape_deepcopy.head");
                llvm::BasicBlock *loopBody = llvm::BasicBlock::Create(
                    context, "reshape_deepcopy.body");
                llvm::BasicBlock *loopEnd = llvm::BasicBlock::Create(
                    context, "reshape_deepcopy.end");

                llvm::Value* idx = llvm_utils->CreateAlloca(
                    *builder, index_type);
                builder->CreateStore(
                    llvm::ConstantInt::get(context,
                        llvm::APInt(index_bit_width, 0)), idx);

                llvm_utils->start_new_block(loopHead);
                llvm::Value* idx_val = llvm_utils->CreateLoad2(index_type, idx);
                llvm::Value* cond = builder->CreateICmpSLT(idx_val,
                    builder->CreateSExtOrTrunc(num_elements, index_type));
                builder->CreateCondBr(cond, loopBody, loopEnd);

                llvm_utils->start_new_block(loopBody);
                idx_val = llvm_utils->CreateLoad2(index_type, idx);
                llvm::Value* src_elem = builder->CreateInBoundsGEP(
                    llvm_data_type, src_data, idx_val);
                llvm::Value* dest_elem = builder->CreateInBoundsGEP(
                    llvm_data_type, dest_data, idx_val);

                ASR::ttype_t* elem_type = ASRUtils::extract_type(asr_data_type);
                llvm_utils->deepcopy(array_expr, src_elem, dest_elem,
                    elem_type, elem_type, module);

                llvm::Value* idx_next = builder->CreateAdd(idx_val,
                    llvm::ConstantInt::get(context,
                        llvm::APInt(index_bit_width, 1)));
                builder->CreateStore(idx_next, idx);
                builder->CreateBr(loopHead);

                llvm_utils->start_new_block(loopEnd);
            } else if (!has_order) {
                llvm::DataLayout data_layout(module->getDataLayout());
                uint64_t size = data_layout.getTypeAllocSize(llvm_data_type);
                llvm::Value* llvm_size = llvm::ConstantInt::get(context, llvm::APInt(32, size));
                llvm::Value* total_size = builder->CreateMul(num_elements, llvm_size);
                builder->CreateMemCpy(llvm_utils->CreateLoad2(llvm_data_type->getPointerTo(), first_ptr), llvm::MaybeAlign(),
                                      llvm_utils->CreateLoad2(llvm_data_type->getPointerTo(), ptr2firstptr), llvm::MaybeAlign(),
                                      total_size);
            }

            builder->CreateStore(
                llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)),
                this->get_offset(result_type, reshaped, false));

            if( ASRUtils::is_array(asr_shape_type) ) {
                llvm::Type *i32 = llvm::Type::getInt32Ty(context);
                llvm::Value* src_offset = this->get_offset(arr_type, array);
                builder->CreateStore(src_offset,
                            this->get_offset(result_type, reshaped, false));

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
                this->set_rank(result_type, reshaped, n_dims);
                llvm::Value* prod = llvm_utils->CreateAlloca(*builder, index_type);
                builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)), prod);
                llvm::Value* dim_des_val = get_pointer_to_dimension_descriptor_array(result_type, reshaped);
                llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, "loop.head");
                llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, "loop.body");
                llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, "loop.end");

                llvm::Value* r = llvm_utils->CreateAlloca(*builder, llvm_utils->getIntType(4));
                builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, 0)), r);
                // head
                llvm_utils->start_new_block(loophead);
                llvm::Value *cond = builder->CreateICmpSLT(
                    builder->CreateSExtOrTrunc(llvm_utils->CreateLoad2(llvm_utils->getIntType(4), r), n_dims->getType()),
                    n_dims);
                builder->CreateCondBr(cond, loopbody, loopend);

                // body
                llvm_utils->start_new_block(loopbody);
                llvm::Value* r_val = llvm_utils->CreateLoad2(llvm_utils->getIntType(4), r);
                llvm::Value* dim_val = llvm_utils->create_ptr_gep2(dim_des, dim_des_val, r_val);
                llvm::Value* s_val = llvm_utils->create_gep2(dim_des, dim_val, DIM_STRIDE);
                llvm::Value* l_val = llvm_utils->create_gep2(dim_des, dim_val, DIM_LOWER_BOUND);
                llvm::Value* dim_size_ptr = llvm_utils->create_gep2(dim_des, dim_val, DIM_EXTENT);
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

                if (has_order) {
                    int64_t rank = ASRUtils::get_fixed_size_of_array(asr_shape_type);
                    LCOMPILERS_ASSERT(rank > 0);

                    ASR::ttype_t* asr_order_type = ASRUtils::expr_type(order_expr);
                    ASR::ttype_t* order_elem_type = ASRUtils::type_get_past_array(
                        ASRUtils::type_get_past_allocatable(
                        ASRUtils::type_get_past_pointer(asr_order_type)));
                    llvm::Type* llvm_order_type = llvm_utils->get_el_type(order_expr, order_elem_type, module);
                    llvm::Type* order_array_type = llvm_utils->get_type_from_ttype_t_util(
                        order_expr, ASRUtils::type_get_past_allocatable_pointer(asr_order_type), module);

                    llvm::Value* order_data_base = order;
                    ASR::array_physical_typeType order_physical_type = ASRUtils::extract_physical_type(asr_order_type);
                    if (order_physical_type == ASR::array_physical_typeType::DescriptorArray) {
                        order_data_base = llvm_utils->create_gep2(order_array_type, order_data_base, 0);
                        order_data_base = llvm_utils->CreateLoad2(llvm_order_type->getPointerTo(), order_data_base);
                    } else if (order_physical_type == ASR::array_physical_typeType::FixedSizeArray && 
                                !ASRUtils::expr_value(order_expr)) {
                        order_data_base = llvm_utils->create_gep2(order_array_type, order_data_base, 0);
                    }

                    std::vector<llvm::Value*> shape_values(rank);
                    for (int64_t i = 0; i < rank; i++) {
                        llvm::Value* sv = llvm_utils->CreateLoad2(
                            i32,
                            llvm_utils->create_ptr_gep2(i32, shape_data,
                                llvm::ConstantInt::get(context, llvm::APInt(32, i))));
                        shape_values[i] = builder->CreateSExtOrTrunc(sv, index_type);
                    }

                    llvm::Value* src_data = llvm_utils->CreateLoad2(
                        llvm_data_type->getPointerTo(), ptr2firstptr);
                    llvm::Value* dest_data = llvm_utils->CreateLoad2(
                        llvm_data_type->getPointerTo(), first_ptr);

                    llvm::Value* I_arr = llvm_utils->CreateAlloca(
                        *builder, llvm::Type::getInt64Ty(context),
                        llvm::ConstantInt::get(context, llvm::APInt(32, rank)));

                    llvm::BasicBlock *order_loop_head = llvm::BasicBlock::Create(context, "reshape_order.head");
                    llvm::BasicBlock *order_loop_body = llvm::BasicBlock::Create(context, "reshape_order.body");
                    llvm::BasicBlock *order_loop_end = llvm::BasicBlock::Create(context, "reshape_order.end");

                    llvm::Value* idx = llvm_utils->CreateAlloca(*builder, index_type);
                    builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)), idx);

                    llvm_utils->start_new_block(order_loop_head);
                    llvm::Value* idx_val = llvm_utils->CreateLoad2(index_type, idx);
                    llvm::Value* cond2 = builder->CreateICmpSLT(idx_val,
                        builder->CreateSExtOrTrunc(num_elements, index_type));
                    builder->CreateCondBr(cond2, order_loop_body, order_loop_end);

                    llvm_utils->start_new_block(order_loop_body);
                    idx_val = llvm_utils->CreateLoad2(index_type, idx);
                    llvm::Value* temp_val = idx_val;

                    for (int64_t j = 0; j < rank; j++) {
                        llvm::Value* d_j = builder->CreateSRem(temp_val, shape_values[j]);
                        temp_val = builder->CreateSDiv(temp_val, shape_values[j]);
                        llvm::Value* I_j_ptr = llvm_utils->create_ptr_gep2(
                            llvm::Type::getInt64Ty(context), I_arr,
                            llvm::ConstantInt::get(context, llvm::APInt(32, j)));
                        builder->CreateStore(d_j, I_j_ptr);
                    }

                    llvm::Value* source_index = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0));
                    llvm::Value* stride = llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1));
                    for (int64_t j = 0; j < rank; j++) {
                        llvm::Value* order_j = llvm_utils->CreateLoad2(
                            llvm_order_type,
                            llvm_utils->create_ptr_gep2(
                                llvm_order_type, order_data_base,
                                llvm::ConstantInt::get(context, llvm::APInt(32, j))));
                        order_j = builder->CreateSExtOrTrunc(order_j, index_type);
                        llvm::Value* dim = builder->CreateSub(order_j,
                            llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)));

                        llvm::Value* I_dim_ptr = llvm_utils->create_ptr_gep2(
                            llvm::Type::getInt64Ty(context), I_arr,
                            builder->CreateSExtOrTrunc(dim, llvm::Type::getInt32Ty(context)));
                        llvm::Value* I_dim = llvm_utils->CreateLoad2(
                            llvm::Type::getInt64Ty(context), I_dim_ptr);
                        source_index = builder->CreateAdd(source_index,
                            builder->CreateMul(I_dim, stride));

                        llvm::Value* shape_dim = shape_values[0];
                        for (int64_t k = 1; k < rank; k++) {
                            llvm::Value* dim_eq_k = builder->CreateICmpEQ(
                                dim, llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, k)));
                            shape_dim = builder->CreateSelect(dim_eq_k, shape_values[k], shape_dim);
                        }
                        stride = builder->CreateMul(stride, shape_dim);
                    }

                    llvm::Value* src_ptr = llvm_utils->create_ptr_gep2(llvm_data_type, src_data, source_index);
                    llvm::Value* val = llvm_utils->CreateLoad2(llvm_data_type, src_ptr);
                    llvm::Value* dst_ptr = llvm_utils->create_ptr_gep2(llvm_data_type, dest_data, idx_val);
                    builder->CreateStore(val, dst_ptr);

                    llvm::Value* idx_next = builder->CreateAdd(idx_val,
                        llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)));
                    builder->CreateStore(idx_next, idx);
                    builder->CreateBr(order_loop_head);

                    llvm_utils->start_new_block(order_loop_end);
                }
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

            llvm::Value* src_dim_des_val = this->get_pointer_to_dimension_descriptor_array(src_ty, src);
            llvm::Value* n_dims = this->get_rank(src_ty, src, false);
            llvm::Value* dest_dim_des_val = this->get_pointer_to_dimension_descriptor_array(dest_ty, dest);
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

            this->set_rank(dest_ty, dest, n_dims);
        }

        // Copy destination's descriptor to the source descriptor
        // Move the data pointer from dest to src
        // And set the src's data pointer to null to prevent double deallocation
        void SimpleCMODescriptor::copy_array_move_allocation(llvm::Type* src_ty, llvm::Value* src, llvm::Type* dest_ty, llvm::Value* dest,
            llvm::Module* module, ASR::expr_t* array_exp, ASR::ttype_t* asr_data_type) {

            llvm::Value* first_ptr = this->get_pointer_to_data(dest_ty, dest);
            llvm::Value* src_data_ptr = this->get_pointer_to_data(src_ty, src);
            llvm::Type* llvm_data_type = llvm_utils->get_el_type(array_exp, ASRUtils::extract_type(asr_data_type), module);
            if(ASRUtils::is_character(*asr_data_type)){
                llvm::Value* dest_str_desc_loaded = builder->CreateLoad(llvm_data_type->getPointerTo(), first_ptr);
                llvm::Value* src_str_desc = builder->CreateLoad(llvm_data_type,
                    builder->CreateLoad(llvm_data_type->getPointerTo(), src_data_ptr));
                builder->CreateStore(src_str_desc, dest_str_desc_loaded);
            } else {
                builder->CreateStore(builder->CreateLoad(llvm_data_type->getPointerTo(), src_data_ptr), first_ptr);
            }

            // Data pointer has been moved to dest, so set src's data pointer to null
            if(ASRUtils::is_character(*asr_data_type)){
                llvm::Value* char_data_ptr = llvm_utils->get_stringArray_data(asr_data_type, src, true);
                builder->CreateStore(llvm::ConstantPointerNull::get(llvm_utils->character_type), char_data_ptr);
            } else {
                builder->CreateStore(llvm::ConstantPointerNull::get(llvm_data_type->getPointerTo()), src_data_ptr);
            }

            llvm::Value* src_offset = this->get_offset(dest_ty, src);
            llvm::Value* dest_offset = this->get_offset(dest_ty, dest, false);
            builder->CreateStore(src_offset, dest_offset);

            llvm::DataLayout data_layout(module->getDataLayout());
            llvm::Value* src_dim_des_val = this->get_pointer_to_dimension_descriptor_array(src_ty, src);
            llvm::Value* n_dims = this->get_rank(src_ty, src, false);
            llvm::Value* dest_dim_des_val = this->get_pointer_to_dimension_descriptor_array(dest_ty, dest);
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

            this->set_rank(dest_ty, dest, n_dims);
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
            llvm::Value* src_data = llvm_utils->CreateLoad2(elem_type->getPointerTo(), this->get_pointer_to_data(source_llvm_type, source_desc));
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

        void SimpleCMODescriptor::copy_contiguous_data_to_descriptor(
            llvm::Value* source_data,
            llvm::Type* dest_llvm_type, llvm::Value* dest_desc,
            llvm::Type* elem_type, int rank, llvm::Module* /*module*/) {
            unsigned index_bit_width = index_type->getIntegerBitWidth();

            llvm::Value* dim_des_array = get_pointer_to_dimension_descriptor_array(
                dest_llvm_type, dest_desc, true);

            std::vector<llvm::Value*> extents(rank);
            llvm::Value* num_elements = llvm::ConstantInt::get(
                context, llvm::APInt(index_bit_width, 1));
            for (int d = 0; d < rank; d++) {
                llvm::Value* dim_des_elem = get_pointer_to_dimension_descriptor(
                    dim_des_array,
                    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), d));
                llvm::Value* lb = get_lower_bound(dim_des_elem);
                llvm::Value* ub = get_upper_bound(dim_des_elem);
                extents[d] = builder->CreateAdd(
                    builder->CreateSub(ub, lb),
                    llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)));
                num_elements = builder->CreateMul(num_elements, extents[d]);
            }

            llvm::Value* dest_data = get_pointer_to_data(dest_llvm_type, dest_desc);
            dest_data = llvm_utils->CreateLoad2(elem_type->getPointerTo(), dest_data);

            llvm::Value* iter_ptr = builder->CreateAlloca(
                index_type, nullptr, "strided_copy_iter");
            builder->CreateStore(
                llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 0)),
                iter_ptr);
            llvm_utils->create_loop("copy_to_strided",
                [&]() {
                    llvm::Value* iter = llvm_utils->CreateLoad2(index_type, iter_ptr);
                    return builder->CreateICmpSLT(iter, num_elements);
                },
                [&]() {
                    llvm::Value* iter = llvm_utils->CreateLoad2(index_type, iter_ptr);
                    llvm::Value* linear_offset = llvm::ConstantInt::get(
                        context, llvm::APInt(index_bit_width, 0));
                    llvm::Value* remaining = iter;
                    for (int d = 0; d < rank; d++) {
                        llvm::Value* dim_idx = builder->CreateSRem(
                            remaining, extents[d]);
                        remaining = builder->CreateSDiv(remaining, extents[d]);
                        llvm::Value* dim_des_elem = get_pointer_to_dimension_descriptor(
                            dim_des_array,
                            llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), d));
                        llvm::Value* stride = get_stride(dim_des_elem);
                        llvm::Value* dim_offset = builder->CreateMul(dim_idx, stride);
                        linear_offset = builder->CreateAdd(linear_offset, dim_offset);
                    }
                    llvm::Value* base_offset = get_offset(dest_llvm_type, dest_desc);
                    linear_offset = builder->CreateAdd(linear_offset, base_offset);

                    // Read from contiguous source
                    llvm::Value* src_ptr = llvm_utils->create_ptr_gep2(
                        elem_type, source_data, iter);
                    llvm::Value* elem_val = llvm_utils->CreateLoad2(elem_type, src_ptr);

                    // Write to strided destination
                    llvm::Value* dest_ptr = llvm_utils->create_ptr_gep2(
                        elem_type, dest_data, linear_offset);
                    builder->CreateStore(elem_val, dest_ptr);

                    llvm::Value* new_iter = builder->CreateAdd(iter,
                        llvm::ConstantInt::get(context, llvm::APInt(index_bit_width, 1)));
                    builder->CreateStore(new_iter, iter_ptr);
                }
            );
        }

        llvm::StructType* SimpleCMODescriptor::get_cfi_type(llvm::Type* /*el_type*/, int n_dims) {
            std::vector<llvm::Type*> cfi_vec = {
                llvm::Type::getInt8Ty(context)->getPointerTo(), // 0: base_addr (void*)
                llvm::Type::getInt64Ty(context),                // 1: elem_len
                llvm::Type::getInt32Ty(context),                // 2: version
                llvm::Type::getInt8Ty(context),                 // 3: rank
                llvm::Type::getInt8Ty(context),                 // 4: type
                llvm::Type::getInt8Ty(context),                 // 5: attribute
                llvm::Type::getInt8Ty(context),                 // 6: extra
                llvm::ArrayType::get(dim_des, n_dims)           // 7: dim[n_dims]
            };
            return llvm::StructType::create(context, cfi_vec, "cfi_array");
        }

        llvm::Value* SimpleCMODescriptor::internal_to_cfi(
                llvm::Type* internal_type, llvm::Value* internal_desc,
                llvm::Type* el_type, int n_dims, uint64_t elem_size,
                int8_t type_code, int cfi_attr) {
            llvm::StructType* cfi_type = get_cfi_type(el_type, n_dims);
            llvm::Value* cfi = llvm_utils->CreateAlloca(cfi_type);
            llvm::Value* elem_size_val = llvm::ConstantInt::get(context, llvm::APInt(64, elem_size));

            // Copy base_addr, adjusting by offset.
            // Guard: if base_addr is NULL (unallocated/disassociated),
            // keep it NULL — offset may be uninitialized.
            // Use byte-level GEP (offset * elem_size) so this works for
            // all element types including characters (where internal el_type
            // is string_descriptor but CFI uses raw char*).
            llvm::Type* i8_ptr_type = llvm::Type::getInt8Ty(context)->getPointerTo();
            llvm::Value* base_raw = llvm_utils->CreateLoad2(
                el_type->getPointerTo(),
                llvm_utils->create_gep2(internal_type, internal_desc, FIELD_BASE_ADDR));
            llvm::Value* base_i8 = builder->CreateBitCast(base_raw, i8_ptr_type);
            llvm::Value* offset = get_offset(internal_type, internal_desc, true);
            llvm::Value* offset_bytes = builder->CreateMul(offset, elem_size_val);
            llvm::Value* adjusted = builder->CreateGEP(
                llvm::Type::getInt8Ty(context), base_i8, offset_bytes);
            llvm::Value* is_null = builder->CreateICmpEQ(base_i8,
                llvm::ConstantPointerNull::get(
                    llvm::cast<llvm::PointerType>(base_i8->getType())));
            adjusted = builder->CreateSelect(is_null, base_i8, adjusted);
            builder->CreateStore(adjusted,
                llvm_utils->create_gep2(cfi_type, cfi, CFI_FIELD_BASE_ADDR));

            // elem_len: use elem_size (the CFI element size), not the
            // internal descriptor's elem_len. For characters, the internal
            // elem_len is sizeof(string_descriptor)=16, but CFI needs the
            // actual character length (e.g. 1).
            builder->CreateStore(elem_size_val,
                llvm_utils->create_gep2(cfi_type, cfi, CFI_FIELD_ELEM_LEN));

            // version
            builder->CreateStore(
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 20180515),
                llvm_utils->create_gep2(cfi_type, cfi, CFI_FIELD_VERSION));

            // Copy rank from internal descriptor
            llvm::Value* rank = llvm_utils->CreateLoad2(
                llvm::Type::getInt8Ty(context),
                llvm_utils->create_gep2(internal_type, internal_desc, FIELD_RANK));
            builder->CreateStore(rank,
                llvm_utils->create_gep2(cfi_type, cfi, CFI_FIELD_RANK));

            // type code and attribute from parameters
            builder->CreateStore(
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), type_code),
                llvm_utils->create_gep2(cfi_type, cfi, CFI_FIELD_TYPE));
            builder->CreateStore(
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), cfi_attr),
                llvm_utils->create_gep2(cfi_type, cfi, CFI_FIELD_ATTRIBUTE));

            // extra = 0 (reserved)
            builder->CreateStore(
                llvm::ConstantInt::get(llvm::Type::getInt8Ty(context), 0),
                llvm_utils->create_gep2(cfi_type, cfi, CFI_FIELD_EXTRA));

            // Copy dims, converting element strides to byte strides
            llvm::Value* src_dim_arr = get_pointer_to_dimension_descriptor_array(internal_type, internal_desc);
            llvm::Value* dst_dim_arr = llvm_utils->create_gep2(cfi_type, cfi, CFI_FIELD_DIMS);
            dst_dim_arr = llvm_utils->create_gep2(
                llvm::ArrayType::get(dim_des, n_dims), dst_dim_arr, 0);
            for (int r = 0; r < n_dims; r++) {
                llvm::Value* src_dim = llvm_utils->create_ptr_gep2(dim_des, src_dim_arr, r);
                llvm::Value* dst_dim = llvm_utils->create_ptr_gep2(dim_des, dst_dim_arr, r);
                // lower_bound
                llvm::Value* lb = llvm_utils->CreateLoad2(index_type,
                    llvm_utils->create_gep2(dim_des, src_dim, DIM_LOWER_BOUND));
                builder->CreateStore(lb,
                    llvm_utils->create_gep2(dim_des, dst_dim, DIM_LOWER_BOUND));
                // extent
                llvm::Value* ext = llvm_utils->CreateLoad2(index_type,
                    llvm_utils->create_gep2(dim_des, src_dim, DIM_EXTENT));
                builder->CreateStore(ext,
                    llvm_utils->create_gep2(dim_des, dst_dim, DIM_EXTENT));
                // stride: convert elements → bytes
                llvm::Value* stride = llvm_utils->CreateLoad2(index_type,
                    llvm_utils->create_gep2(dim_des, src_dim, DIM_STRIDE));
                stride = builder->CreateMul(stride, elem_size_val);
                builder->CreateStore(stride,
                    llvm_utils->create_gep2(dim_des, dst_dim, DIM_STRIDE));
            }

            return cfi;
        }

        llvm::Value* SimpleCMODescriptor::cfi_to_internal(
                llvm::Type* internal_type,
                llvm::Type* el_type, llvm::Value* cfi_desc,
                int n_dims, uint64_t elem_size) {
            llvm::StructType* cfi_type = get_cfi_type(el_type, n_dims);
            llvm::Value* internal = create_descriptor_alloca(
                static_cast<llvm::StructType*>(internal_type));

            // Bitcast incoming pointer to CFI type for GEP access
            llvm::Value* cfi_ptr = builder->CreateBitCast(
                cfi_desc, cfi_type->getPointerTo());

            // Copy base_addr (CFI has i8* aka void*, internal has el_type*)
            llvm::Type* i8_ptr_type = llvm::Type::getInt8Ty(context)->getPointerTo();
            llvm::Value* base_i8 = llvm_utils->CreateLoad2(
                i8_ptr_type,
                llvm_utils->create_gep2(cfi_type, cfi_ptr, CFI_FIELD_BASE_ADDR));
            llvm::Value* base = builder->CreateBitCast(base_i8,
                el_type->getPointerTo());
            builder->CreateStore(base,
                llvm_utils->create_gep2(internal_type, internal, FIELD_BASE_ADDR));

            // Copy scalar fields
            auto copy_field = [&](int cfi_idx, int int_idx, llvm::Type* ty) {
                llvm::Value* val = llvm_utils->CreateLoad2(ty,
                    llvm_utils->create_gep2(cfi_type, cfi_ptr, cfi_idx));
                builder->CreateStore(val,
                    llvm_utils->create_gep2(internal_type, internal, int_idx));
            };
            copy_field(CFI_FIELD_ELEM_LEN,  FIELD_ELEM_LEN,  llvm::Type::getInt64Ty(context));
            copy_field(CFI_FIELD_VERSION,    FIELD_VERSION,    llvm::Type::getInt32Ty(context));
            copy_field(CFI_FIELD_RANK,       FIELD_RANK,       llvm::Type::getInt8Ty(context));
            copy_field(CFI_FIELD_TYPE,       FIELD_TYPE,       llvm::Type::getInt8Ty(context));
            copy_field(CFI_FIELD_ATTRIBUTE,  FIELD_ATTRIBUTE,  llvm::Type::getInt8Ty(context));

            // Set offset to 0
            builder->CreateStore(
                llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 0),
                llvm_utils->create_gep2(internal_type, internal, FIELD_OFFSET));

            // Copy dims, converting byte strides to element strides
            llvm::Value* elem_size_val = llvm::ConstantInt::get(
                context, llvm::APInt(64, elem_size));
            llvm::Value* src_dim_arr = llvm_utils->create_gep2(
                cfi_type, cfi_ptr, CFI_FIELD_DIMS);
            src_dim_arr = llvm_utils->create_gep2(
                llvm::ArrayType::get(dim_des, n_dims), src_dim_arr, 0);
            llvm::Value* dst_dim_arr =
                get_pointer_to_dimension_descriptor_array(internal_type, internal);
            for (int r = 0; r < n_dims; r++) {
                llvm::Value* src_dim = llvm_utils->create_ptr_gep2(
                    dim_des, src_dim_arr, r);
                llvm::Value* dst_dim = llvm_utils->create_ptr_gep2(
                    dim_des, dst_dim_arr, r);
                // lower_bound
                llvm::Value* lb = llvm_utils->CreateLoad2(index_type,
                    llvm_utils->create_gep2(dim_des, src_dim, DIM_LOWER_BOUND));
                builder->CreateStore(lb,
                    llvm_utils->create_gep2(dim_des, dst_dim, DIM_LOWER_BOUND));
                // extent
                llvm::Value* ext = llvm_utils->CreateLoad2(index_type,
                    llvm_utils->create_gep2(dim_des, src_dim, DIM_EXTENT));
                builder->CreateStore(ext,
                    llvm_utils->create_gep2(dim_des, dst_dim, DIM_EXTENT));
                // stride: convert bytes → elements
                llvm::Value* stride = llvm_utils->CreateLoad2(index_type,
                    llvm_utils->create_gep2(dim_des, src_dim, DIM_STRIDE));
                stride = builder->CreateSDiv(stride, elem_size_val);
                builder->CreateStore(stride,
                    llvm_utils->create_gep2(dim_des, dst_dim, DIM_STRIDE));
            }

            return internal;
        }

        void SimpleCMODescriptor::push_descriptor_array_args(
                ASR::expr_t* val_expr, ASR::ttype_t* expr_type_full,
                ASR::ttype_t* val_type, llvm::Value* var_ptr,
                llvm::Module* module, std::vector<llvm::Value*>& args) {

            llvm::Type* llvm_desc_type = llvm_utils->get_type_from_ttype_t_util(
                        val_expr, ASRUtils::type_get_past_allocatable_pointer(expr_type_full), module);
            llvm::Type* llvm_elem_type = llvm_utils->get_type_from_ttype_t_util(
                        val_expr, val_type, module);
            llvm::Value* desc_ptr = llvm_utils->CreateLoad2(llvm_desc_type->getPointerTo(), var_ptr);
            llvm::Value* data_field = get_pointer_to_data(llvm_desc_type, desc_ptr);
            llvm::Value* data_ptr_val = llvm_utils->CreateLoad2(llvm_elem_type->getPointerTo(), 
                                        data_field);
            data_ptr_val = builder->CreateBitCast(data_ptr_val, 
                                    llvm::Type::getInt8Ty(context)->getPointerTo());
            llvm::Value* n_elems = get_array_size(llvm_desc_type, desc_ptr, nullptr, 4);
            llvm::Value* dim_des_arr = get_pointer_to_dimension_descriptor_array(
                                    llvm_desc_type, desc_ptr);
            llvm::Value* dim_zero = llvm::ConstantInt::get(context, llvm::APInt(32, 0));
            llvm::Value* dim_desc = get_pointer_to_dimension_descriptor(dim_des_arr, dim_zero);
            llvm::Value* stride_val = get_stride(dim_desc);
            int32_t type_code_val = 2;
            if (ASR::is_a<ASR::Integer_t>(*val_type)){
                type_code_val = (ASR::down_cast<ASR::Integer_t>(val_type)->m_kind <= 4) ? 2 : 3;
            } else if (ASR::is_a<ASR::Real_t>(*val_type)){
                type_code_val = (ASR::down_cast<ASR::Real_t>(val_type)->m_kind == 4) ? 4 : 5;
            } else if (ASR::is_a<ASR::Complex_t>(*val_type)) {
                type_code_val = (ASR::down_cast<ASR::Complex_t>(val_type)->m_kind == 4) ? 6 : 7;
            } else {
                throw CodeGenError("Not implemented: read into allocatable targets for dtype "
                    + ASRUtils::type_to_str_python_expr(val_type, val_expr));
            }
            args.push_back(llvm::ConstantInt::get(context, llvm::APInt(32, 1)));
            args.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), type_code_val));
            args.push_back(data_ptr_val);
            args.push_back(n_elems);
            args.push_back(stride_val);
    }

    } // LLVMArrUtils

} // namespace LCompilers

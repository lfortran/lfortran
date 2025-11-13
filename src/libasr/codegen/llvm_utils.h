#ifndef LFORTRAN_LLVM_UTILS_H
#define LFORTRAN_LLVM_UTILS_H

#include "libasr/asr_utils.h"
#include "libasr/assert.h"
#include <llvm/IR/Value.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <libasr/asr.h>

#include<stack>
#include <map>
#include <string>
#include <unordered_map>
#include <tuple>

#if LLVM_VERSION_MAJOR >= 11
#    define FIXED_VECTOR_TYPE llvm::FixedVectorType
#else
#    define FIXED_VECTOR_TYPE llvm::VectorType
#endif

namespace LCompilers {
class ASRToLLVMVisitor;

    // Platform dependent fast unique hash:
    static inline uint64_t get_hash(ASR::asr_t *node)
    {
        return (uint64_t)node;
    }

    namespace {

    // This exception is used to abort the visitor pattern when an error occurs.
    // This is only used locally in this file, not propagated outside. An error
    // must be already present in ASRToLLVMVisitor::diag before throwing this
    // exception. This is checked with an assert when the CodeGenAbort is
    // caught.
    class CodeGenAbort
    {
    };

    // Local exception that is only used in this file to exit the visitor
    // pattern and caught later (not propagated outside). It accepts an error
    // message that is then appended at the end of ASRToLLVMVisitor::diag.  The
    // `diag` can already contain other errors or warnings.  This is a
    // convenience class. One can also report the error into `diag` directly and
    // call `CodeGenAbort` instead.
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

    }

    namespace LLVMArrUtils {
        class Descriptor;
    }

    static inline void printf(llvm::LLVMContext &context, llvm::Module &module,
        llvm::IRBuilder<> &builder, const std::vector<llvm::Value*> &args)
    {
        llvm::Function *fn_printf = module.getFunction("_lfortran_printf");
        if (!fn_printf) {
            llvm::FunctionType* function_type = llvm::FunctionType::get(
                llvm::Type::getVoidTy(context),
                {
                    llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(context)),  // format
                    llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(context)),  // str
                    llvm::Type::getInt32Ty(context),                               // str_len
                    llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(context)),  // end
                    llvm::Type::getInt32Ty(context)                                // end_len
                },
                false);
            fn_printf = llvm::Function::Create(function_type,
                    llvm::Function::ExternalLinkage, "_lfortran_printf", &module);
        }
        builder.CreateCall(fn_printf, args);
    }


    static inline void print_error(llvm::LLVMContext &context, llvm::Module &module,
        llvm::IRBuilder<> &builder, const std::vector<llvm::Value*> &args)
    {
        llvm::Function *fn_printf = module.getFunction("_lcompilers_print_error");
        if (!fn_printf) {
            llvm::FunctionType *function_type = llvm::FunctionType::get(
                    llvm::Type::getVoidTy(context), {llvm::Type::getInt8Ty(context)->getPointerTo()}, true);
            fn_printf = llvm::Function::Create(function_type,
                    llvm::Function::ExternalLinkage, "_lcompilers_print_error", &module);
        }
        builder.CreateCall(fn_printf, args);
    }

    static inline void exit(llvm::LLVMContext &context, llvm::Module &module,
        llvm::IRBuilder<> &builder, llvm::Value* exit_code)
    {
        llvm::Function *fn_exit = module.getFunction("exit");
        if (!fn_exit) {
            llvm::FunctionType *function_type = llvm::FunctionType::get(
                    llvm::Type::getVoidTy(context), {llvm::Type::getInt32Ty(context)},
                    false);
            fn_exit = llvm::Function::Create(function_type,
                    llvm::Function::ExternalLinkage, "exit", &module);
        }
        builder.CreateCall(fn_exit, {exit_code});
    }

    //! Creates a global string pointer safely across LLVM versions
    static inline llvm::Constant* create_global_string_ptr(
        [[maybe_unused]] llvm::LLVMContext &context,
        [[maybe_unused]] llvm::Module &module,
        [[maybe_unused]] llvm::IRBuilder<> &builder, llvm::StringRef Str,
        const llvm::Twine &Name = "", unsigned AddressSpace = 0)
    {
#if LLVM_VERSION_MAJOR <= 7
        // LLVM 7: CreateGlobalStringPtr does not work with ConstantExpr::getGetElementPtr
        // Use bitcast as a workaround. This creates a text relocation warning but works correctly.
        llvm::Constant *StrConstant = llvm::ConstantDataArray::getString(context, Str);
        auto *GV = new llvm::GlobalVariable(
            module, StrConstant->getType(), true,
            llvm::GlobalValue::PrivateLinkage, StrConstant, Name, nullptr,
            llvm::GlobalVariable::NotThreadLocal, AddressSpace);
        GV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
        // Bitcast to i8* - this works but may produce a linker warning about text relocations
        return llvm::ConstantExpr::getBitCast(GV, llvm::Type::getInt8PtrTy(context, AddressSpace));
#else
        // LLVM 8+: Use the standard IRBuilder method
        return builder.CreateGlobalStringPtr(Str, Name, AddressSpace);
#endif
    }

    // Insert the following anywhere inside the LLVM backend to print
    // addresses at runtime:
    // call_print_stacktrace_addresses(context, *module, *builder, {filename, use_colors});
    static inline void call_print_stacktrace_addresses(llvm::LLVMContext &context,
            llvm::Module &module, llvm::IRBuilder<> &builder,
            const std::vector<llvm::Value*> &args)
    {
        llvm::Function *fn = module.getFunction("print_stacktrace_addresses");
        if (!fn) {
            llvm::FunctionType *function_type = llvm::FunctionType::get(
                llvm::Type::getVoidTy(context), {
                    llvm::Type::getInt8Ty(context)->getPointerTo(),
                    llvm::Type::getInt1Ty(context)
                }, true);
            fn = llvm::Function::Create(function_type,
                llvm::Function::ExternalLinkage, "print_stacktrace_addresses",
                &module);
        }
        builder.CreateCall(fn, args);
    }

    namespace LLVM {

        llvm::Value* CreateStore(llvm::IRBuilder<> &builder, llvm::Value *x, llvm::Value *y);
        llvm::Value* lfortran_malloc(llvm::LLVMContext &context, llvm::Module &module,
                llvm::IRBuilder<> &builder, llvm::Value* arg_size);
        llvm::Value* lfortran_realloc(llvm::LLVMContext &context, llvm::Module &module,
                llvm::IRBuilder<> &builder, llvm::Value* ptr, llvm::Value* arg_size);
        llvm::Value* lfortran_calloc(llvm::LLVMContext &context, llvm::Module &module,
                llvm::IRBuilder<> &builder, llvm::Value* count, llvm::Value* type_size);
        static inline bool is_llvm_struct(ASR::ttype_t* asr_type) {
            return ASR::is_a<ASR::Tuple_t>(*asr_type) ||
                   ASR::is_a<ASR::List_t>(*asr_type) ||
                   ASR::is_a<ASR::StructType_t>(*asr_type) ||
                   ASR::is_a<ASR::Dict_t>(*asr_type);
        }
        // Check if type is represented as a pointer to the backend type.
        // e.g. -> `i64*`
        bool is_llvm_pointer(const ASR::ttype_t& asr_type);

    }

    class LLVMList;
    class LLVMStruct;
    class LLVMTuple;
    class LLVMDictInterface;
    class LLVMSetInterface;

    class LLVMUtils {

        private:

            llvm::LLVMContext& context;
            llvm::IRBuilder<>* builder;
            llvm::AllocaInst *str_cmp_itr;

        public:

            LLVMTuple* tuple_api;
            LLVMList* list_api;
            LLVMStruct* struct_api;
            LLVMDictInterface* dict_api;
            LLVMSetInterface* set_api;
            LLVMArrUtils::Descriptor* arr_api;
            llvm::Module* module;
            std::string& der_type_name;
            std::map<std::string, llvm::StructType*>& name2dertype;
            std::map<std::string, llvm::StructType*>& name2dercontext;
            std::vector<std::string>& struct_type_stack;
            std::map<std::string, std::string>& dertype2parent;
            std::map<std::string, std::map<std::string, int>>& name2memidx;
            std::unordered_map<std::uint32_t, std::unordered_map<std::string, llvm::Type*>>& arr_arg_type_cache;
            std::map<std::string, std::pair<llvm::Type*, llvm::Type*>>& fname2arg_type;

            LLVMDictInterface* dict_api_lp;
            LLVMDictInterface* dict_api_sc;
            LLVMSetInterface* set_api_lp;
            LLVMSetInterface* set_api_sc;

            CompilerOptions &compiler_options;
            std::map<uint64_t, llvm::Value*> &llvm_symtab; // llvm_symtab_value


            llvm::StructType *complex_type_4, *complex_type_8;
            llvm::StructType *complex_type_4_ptr, *complex_type_8_ptr;
            llvm::PointerType *character_type;
            llvm::Type* string_descriptor; /* <{ i8* --DATA-- , i64 --LENGTH-- }> */
            llvm::Type* vptr_type;
            llvm::FunctionType* struct_copy_functype;

#if LLVM_VERSION_MAJOR >= 17
            llvm::PointerType* i8_ptr = llvm::PointerType::getUnqual(llvm::Type::getInt8Ty(context));
#else
            llvm::PointerType* i8_ptr = llvm::Type::getInt8PtrTy(context);
#endif

            LLVMUtils(llvm::LLVMContext& context,
                llvm::IRBuilder<>* _builder, std::string& der_type_name_,
                std::map<std::string, llvm::StructType*>& name2dertype_,
                std::map<std::string, llvm::StructType*>& name2dercontext_,
                std::vector<std::string>& struct_type_stack_,
                std::map<std::string, std::string>& dertype2parent_,
                std::map<std::string, std::map<std::string, int>>& name2memidx_,
                CompilerOptions &compiler_options_,
                std::unordered_map<std::uint32_t, std::unordered_map<std::string, llvm::Type*>>& arr_arg_type_cache_,
                std::map<std::string, std::pair<llvm::Type*, llvm::Type*>>& fname2arg_type_,
                std::map<uint64_t, llvm::Value*> &llvm_symtab_);
                
            llvm::Value* lfortran_free(llvm::Value* ptr);
            llvm::Value* string_format_fortran(const std::vector<llvm::Value*> &args);
            llvm::Value* create_gep2(llvm::Type *t, llvm::Value* ds, llvm::Value* idx);
            llvm::Value* create_gep2(llvm::Type *t, llvm::Value* ds, int idx);

            llvm::Value* create_ptr_gep2(llvm::Type* type, llvm::Value* ptr, int idx);
            llvm::Value* create_ptr_gep2(llvm::Type* type, llvm::Value* ptr, llvm::Value* idx);

            llvm::Value* CreateLoad2(llvm::Type *t, llvm::Value *x, bool is_volatile = false);

            llvm::Value* CreateGEP2(llvm::Type *t, llvm::Value *x,
                std::vector<llvm::Value *> &idx);
            llvm::Value* CreateGEP2(llvm::Type *type, llvm::Value *x, int idx);

            
            llvm::Value* CreateInBoundsGEP2(llvm::Type *t, llvm::Value *x,
                std::vector<llvm::Value *> &idx);
            llvm::Value* CreateInBoundsGEP2(ASR::ttype_t *t, llvm::Value *x, std::vector<llvm::Value *> &idx);

            llvm::AllocaInst* CreateAlloca(llvm::Type* type,
                llvm::Value* size=nullptr, std::string Name="",
                bool is_llvm_ptr=false);
            llvm::AllocaInst* CreateAlloca(llvm::IRBuilder<> &builder,
                llvm::Type* type, llvm::Value* size=nullptr, std::string Name="",
                bool is_llvm_ptr=false);

            llvm::Type* getIntType(int a_kind, bool get_pointer=false);
            llvm::Function* _Deallocate();

            void start_new_block(llvm::BasicBlock *bb);

            llvm::Value* lfortran_str_cmp(llvm::Value* left_arg, llvm::Value* right_arg,
                                          std::string runtime_func_name, llvm::Module& module);

            template<typename... Args>
            void generate_runtime_error(llvm::Value* cond, std::string message, std::string infile, Location loc, LocationManager& lm, Args... args)
            {
                llvm::Function *fn = builder->GetInsertBlock()->getParent();

                llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", fn);
                llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifcont");

                uint32_t line, column;
                if (infile != "" && loc.first != 0 && loc.last != 0) {
                    lm.pos_to_linecol(lm.output_to_input_pos(loc.first, false),
                        line, column, infile);
                    std::stringstream ss;
                    ss << "At " << line << ":" << column << " of file " << infile << "\n" << message;
                    message = ss.str();
                }

                builder->CreateCondBr(cond, thenBB, mergeBB);
                builder->SetInsertPoint(thenBB); {
                        llvm::Value* formatted_msg = create_global_string_ptr(context, *module, *builder, message);
                        llvm::Function* print_error_fn = module->getFunction("_lcompilers_print_error");
                        if (!print_error_fn) {
                            llvm::FunctionType* error_fn_type = llvm::FunctionType::get(
                                llvm::Type::getVoidTy(context),
                                {llvm::Type::getInt8Ty(context)->getPointerTo()},
                                true);
                            print_error_fn = llvm::Function::Create(error_fn_type,
                                llvm::Function::ExternalLinkage, "_lcompilers_print_error", module);
                        }

                        std::vector<llvm::Value*> vec = {formatted_msg, args...};
                        builder->CreateCall(print_error_fn, vec);

                        llvm::Function* exit_fn = module->getFunction("exit");
                        if (!exit_fn) {
                            llvm::FunctionType* exit_fn_type = llvm::FunctionType::get(
                                llvm::Type::getVoidTy(context),
                                {llvm::Type::getInt32Ty(context)},
                                false);
                            exit_fn = llvm::Function::Create(exit_fn_type,
                                llvm::Function::ExternalLinkage, "exit", module);
                        }

                        builder->CreateCall(exit_fn, {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 1)});
                        builder->CreateUnreachable();
                }
                start_new_block(mergeBB);
            }
            std::string get_llvm_type_as_string(llvm::Type* type);
            /*
                * Checker for the desired type while operating on array of strings.
                To make sure of consistency while working, to avoid llvm IR opaque errors.
                * Only functional when llvm_versions < 17
                * Try to only use with debug mode only.
            */
            bool is_proper_array_of_strings_llvm_var(ASR::ttype_t* type, llvm::Value* str);

            /*
                * Checker for the desired type while operating on strings.
                To make sure of consistency while working, to avoid llvm IR opaque errors.
                * Only functional when llvm_versions < 17
                * Try to only use with debug mode only.
            */
            bool is_proper_string_llvm_variable(ASR::String_t* str_type, llvm::Value* str);

            /*
             * Allocate heap memory for string.
             * Notice : It doesn't set the length.
            */
            void set_string_memory_on_heap(ASR::string_physical_typeType str_physical_type, llvm::Value* str, llvm::Value* len);

            /*
             * Allocate stack memory for string.
             * Notice : It doesn't set the length.
            */
            void set_string_memory_on_stack(ASR::string_physical_typeType str_physical_type, llvm::Value* str, llvm::Value* len);

            /*
                Create a string based on the physical type.
                It's not resposbile of setting up the string.
            */
            llvm::Value* create_string(ASR::String_t* str, std::string name);
            /*
             * Create an empty string descriptor with
             * {data = nullptr, length = 0}
            */
            llvm::Value* create_empty_string_descriptor(std::string name = "");

            /*
                Creates a string descriptor and sets data and length
                based on the passed arguments.
            */
            llvm::Value* create_string_descriptor(llvm::Value* data, llvm::Value* len, std::string name = "");

            /**
             * Clones the state of string
             * A STRING STATE : is the length of it, and the amount of allocated memory.
             * @param s_type is assumed to be a string type reflecting the type of both strings.
             */
            void clone_string_state(llvm::Value*  dest, llvm::Value* src, ASR::String_t* str_type);

            /* 
                Creates a string_descriptor.
                Only allocates it, does not set data or length.
            */
            llvm::Value* create_string_descriptor(std::string name);

            /*
                Creates A StringView
                - It doesn't allocate its own memory.
                - Creates String based on physicalType of ASR::String node.
                - Must provide data of the string on which the view is created.
                - Must provide a length.
            */
            llvm::Value* create_stringView(ASR::String_t* string_t, llvm::Value* data, llvm::Value* len, std::string name);
            
            /*
                Checks if the string length is setable.
                - TRUE if the PhysicalType preserves information about the length (at runtime).
            */
            bool is_string_length_setable(ASR::String_t* string_t);

            llvm::Value* get_string_data(ASR::String_t* str_type, llvm::Value* str, bool get_pointer_to_data=false);
            llvm::Value* get_string_length(ASR::String_t* str_type, llvm::Value* str, bool get_pointer_to_len=false);

            // Gets string's length and data
            std::pair<llvm::Value*, llvm::Value*> get_string_length_data(ASR::String_t* str_type, llvm::Value* str,
                bool get_pointer_to_data=false, bool get_pointer_to_len=false);
            

            /*
                Calculates the pointer position, And return pointer to it.
            */
            llvm::Value* get_string_element_in_array_(ASR::String_t* str_type, llvm::Value* data, llvm::Value* arr_idx);
            /*
                Gets the desired string element within array
            */
            llvm::Value* get_string_element_in_array(ASR::String_t* str_type, llvm::Value* array_ptr/*PointerArray*/, llvm::Value* arr_idx);

            /*
                Corresponds to the process of allocating a string.
                e.g. --> `allocate(character(10) :: str)`
                - If deferred length, Use desired amount passed by user.
                - If not deferred length, Use length set by user while declaration.
                - If deferred length, set the desired amount as the current length.
                - If not deferred length, Keep the length set by the user while declaration.
            */
            void allocate_allocatable_string(ASR::String_t* str_type,llvm::Value* str, llvm::Value* amount_to_allocate);

            /*
                Corresponds to the process of allocating an array of strings.
                e.g. --> `allocate(character(10) :: str(7))`
                - If deferred length, Use desired amount passed by user.
                - If not deferred length, Use length set by user while declaration.
                - If deferred length, set the desired amount as the current length.
                - If not deferred length, Keep the length set by the user while declaration.
            */
            void allocate_allocatable_array_of_strings(ASR::String_t* str_type, llvm::Value* str,
                llvm::Value* string_length_to_allocate, llvm::Value* array_size_to_allocte, bool realloc = false);
            
            /*
                Gets the whole memory size needed for array of strings.
                It works only for compile-time arraysize + compile-time string length
            */
            llvm::Value* get_stringArray_whole_size(ASR::ttype_t* type);

            /*
                Allocate the memory needed for an array of strings (on heap), and sets it.
                e.g. --> `character(100) :: arr_of_strings(7)`
                Length is not this function's responsibility.
            */
            void set_array_of_strings_memory_on_heap(ASR::String_t* str_type,llvm::Value* str, 
                llvm::Value* str_len, llvm::Value* array_size, bool realloc = false);

            /*
                Allocate the memory needed for an array of strings (on stack)
                e.g. --> `character(100) :: arr_of_strings(7)`
                Length is not this function's responsibility.
            */
            void set_array_of_strings_memory_on_stack(ASR::String_t* str_type,llvm::Value* str, llvm::Value* str_len, llvm::Value* array_size);


            /*
                * Gets the data pointer of an array of strings
                  based on the physicalType.
                * e.g. --> `character(100) :: arr_of_strings(7)`
                  Returns `i8*`
            */
            llvm::Value* get_stringArray_data(ASR::ttype_t* type, llvm::Value* arr_ptr, bool get_poniter_to_data = false);
            
            /*
                * Gets string length of array of strings
                  based on the physicalType.
                * e.g. --> `character(100) :: arr_of_strings(7)`
                * Returns `int64`
            */
            llvm::Value* get_stringArray_length(ASR::ttype_t* type, llvm::Value* arr_ptr);

            /*
                Frees Strings.
                whether it is a single string or an array of strings.
            */
            void free_strings(ASR::expr_t* expr, llvm::Value* tmp);

            /*
                *String copying src into destination,
                using runtime function.
            */
            llvm::Value* lfortran_str_copy(
                llvm::Value* dest, llvm::Value *src,
                ASR::String_t* dest_str_type, ASR::String_t* src_str_type,
                bool is_dest_allocatable);


            /*
                *String copying src into destination,
                using runtime function with known data.
            */
            llvm::Value* lfortran_str_copy_with_data(
                llvm::Value* lhs_data, llvm::Value *lhs_len,
                llvm::Value* rhs_data, llvm::Value *rhs_len,
                bool is_dest_deferred, bool is_dest_allocatable);;

            // Handles string literals ==> e.g. `print *, "HelloWorld"`
            llvm::Value* declare_string_constant(const ASR::StringConstant_t* str_const);

            llvm::Value* declare_constant_stringArray(Allocator &al, const ASR::ArrayConstant_t* arr_const);
            /*
                Declare + Setup
                string in the global scope of the llvm module.
            */
            llvm::Value* declare_global_string(ASR::String_t* str, std::string initial_data, bool is_const, std::string name = "",
                llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::PrivateLinkage);
            
            /*
                * Sets up the global array of strings that's not allocatable.
                * Depends on the fact that PoitnerToDataArray physicalType for strings
                    is the same representation as the string PhysicalType  
            */
            llvm::Value* handle_global_nonallocatable_stringArray(Allocator& al, ASR::Array_t* array_t,
                ASR::ArrayConstant_t* arrayConst_t, std::string name);

            llvm::Value* is_equal_pointer_string(llvm::Value* left, llvm::Value* right);

            llvm::Value* is_equal_descriptor_string(llvm::Value* left, llvm::Value* right, ASR::String_t* type);

            llvm::Value* is_equal_by_value(llvm::Value* left, llvm::Value* right,
                                           llvm::Module* module, ASR::ttype_t* asr_type);

            llvm::Value* is_ineq_by_value(llvm::Value* left, llvm::Value* right,
                                          llvm::Module* module, ASR::ttype_t* asr_type,
                                          int8_t overload_id, ASR::ttype_t* int32_type=nullptr);

            void set_module(llvm::Module* module_);

            llvm::Type* getMemberType(ASR::ttype_t* mem_type,
                ASR::Variable_t* member, llvm::Module* module);

            void createStructTypeContext(ASR::Struct_t* der_type);

            llvm::Type* getStructType(ASR::Struct_t* der_type, llvm::Module* module, bool is_pointer=false);

            llvm::Type* getUnion(ASR::Union_t* union_type,
                llvm::Module* module, bool is_pointer=false);

            llvm::Type* getClassType(ASR::Struct_t* der_type, bool is_pointer=false);

            llvm::Type* getFPType(int a_kind, bool get_pointer=false);

            llvm::Type* getComplexType(int a_kind, bool get_pointer=false);
            // Returns LLVM Type Based On String's PhysicalType
            llvm::Type* get_StringType(ASR::ttype_t* type);

            llvm::Type* get_el_type(ASR::expr_t* expr, ASR::ttype_t* m_type_, llvm::Module* module);

            llvm::Type* get_dict_type(ASR::expr_t* dict_expr, ASR::ttype_t* asr_type, llvm::Module* module);

            llvm::Type* get_set_type(ASR::expr_t* set_expr, ASR::ttype_t* asr_type, llvm::Module* module);

            llvm::FunctionType* get_function_type(const ASR::Function_t &x, llvm::Module* module);

            std::vector<llvm::Type*> convert_args(const ASR::Function_t &x, llvm::Module* module);

            std::vector<llvm::Type*> convert_args(ASR::Function_t* fn, ASR::FunctionType_t* x);

            llvm::Value* get_array_size(llvm::Value* array_ptr, llvm::Type* array_llvm_type, ASR::ttype_t* array_asr_type, ASRToLLVMVisitor *asr_to_llvm_visitor);


            llvm::Type* get_type_from_ttype_t(ASR::expr_t* arg_expr, ASR::ttype_t* asr_type,
                ASR::symbol_t *type_declaration, ASR::storage_typeType m_storage,
                bool& is_array_type, bool& is_malloc_array_type, bool& is_list,
                ASR::dimension_t*& m_dims, int& n_dims, int& a_kind, llvm::Module* module,
                ASR::abiType m_abi=ASR::abiType::Source);

            llvm::Type* get_type_from_ttype_t_util(ASR::expr_t* expr, ASR::ttype_t* asr_type,
                                                   llvm::Module* module,
                                                   ASR::abiType asr_abi = ASR::abiType::Source);
            llvm::Type* get_type_from_ttype_t_util(ASR::ttype_t* asr_type, ASR::symbol_t* type_decl,
                                                   llvm::Module* module,
                                                   ASR::abiType asr_abi = ASR::abiType::Source);

            llvm::Type* get_arg_type_from_ttype_t(ASR::expr_t* arg_expr, ASR::ttype_t* asr_type,
                ASR::symbol_t *type_declaration, ASR::abiType m_abi, ASR::abiType arg_m_abi,
                ASR::storage_typeType m_storage, bool arg_m_value_attr, int& n_dims,
                int& a_kind, bool& is_array_type, ASR::intentType arg_intent, llvm::Module* module,
                bool get_pointer=true);

            void set_dict_api(ASR::Dict_t* dict_type);

            void set_set_api(ASR::Set_t* set_type);

            void deepcopy(ASR::expr_t* src_expr, llvm::Value* src, llvm::Value* dest,
                ASR::ttype_t* asr_dest_type,
                ASR::ttype_t* asr_src_type, llvm::Module* module);

            llvm::Value* convert_kind(llvm::Value* val, llvm::Type* target_type);


            // Note: `llvm_utils->create_if_else` and `create_loop` are optional APIs
            // that do not have to be used. Many times, for more complicated
            // things, it might be more readable to just use the LLVM API
            // without any extra layer on top. In some other cases, it might
            // be more readable to use this abstraction.
            // The `if_block` and `else_block` must generate one or more blocks. In
            // addition, the `if_block` must not be terminated, we terminate it
            // ourselves. The `else_block` can be either terminated or not.
            template <typename IF, typename ELSE>
            void create_if_else(llvm::Value * cond, IF if_block, ELSE else_block, char *name,
                                std::vector<llvm::BasicBlock*> &loop_or_block_end,
                                std::vector<std::string> &loop_or_block_end_names) {
                llvm::Function *fn = builder->GetInsertBlock()->getParent();

                std::string if_name;
                llvm::BasicBlock *thenBB = nullptr;
                llvm::BasicBlock *elseBB = nullptr;
                llvm::BasicBlock *mergeBB = nullptr;
                if (name) {
                    if_name = std::string(name);
                    std::string if_cont_name = if_name + ".ifcont";
                    thenBB = llvm::BasicBlock::Create(context, if_name + ".then", fn);
                    elseBB = llvm::BasicBlock::Create(context, if_name + ".else");
                    mergeBB = llvm::BasicBlock::Create(context, if_cont_name);
                    loop_or_block_end.push_back(mergeBB);
                    loop_or_block_end_names.push_back(if_cont_name);
                } else {
                    thenBB = llvm::BasicBlock::Create(context, "then", fn);
                    elseBB = llvm::BasicBlock::Create(context, "else");
                    mergeBB = llvm::BasicBlock::Create(context, "ifcont");
                }

                builder->CreateCondBr(cond, thenBB, elseBB);
                builder->SetInsertPoint(thenBB); {
                    if_block();
                }
                builder->CreateBr(mergeBB);

                start_new_block(elseBB); {
                    else_block();
                }
                start_new_block(mergeBB);
            }

            // Overload with defaults
            template <typename IF, typename ELSE>
            void create_if_else(llvm::Value *cond, IF if_block, ELSE else_block, char *name = nullptr) {
                static std::vector<llvm::BasicBlock*> dummy_blocks;
                static std::vector<std::string> dummy_names;
                create_if_else(cond, if_block, else_block, name, dummy_blocks, dummy_names);
            }

            template <typename Cond, typename Body>
            void create_loop(const char *name, Cond condition, Body loop_body){
                std::string loop_name;
                if (name) {
                    loop_name = std::string(name);
                } else {
                    loop_name = "loop";
                }

                std::string loophead_name = loop_name + ".head";
                std::string loopbody_name = loop_name + ".body";
                std::string loopend_name = loop_name + ".end";

                llvm::BasicBlock *loophead = llvm::BasicBlock::Create(context, loophead_name);
                llvm::BasicBlock *loopbody = llvm::BasicBlock::Create(context, loopbody_name);
                llvm::BasicBlock *loopend = llvm::BasicBlock::Create(context, loopend_name);

                // head
                start_new_block(loophead); {
                    llvm::Value* cond = condition();
                    builder->CreateCondBr(cond, loopbody, loopend);
                }

                // body
                start_new_block(loopbody); {
                    loop_body();
                    builder->CreateBr(loophead);
                }

                // end
                start_new_block(loopend);
            }

            /**
             *@class StringFormatReturn
             *
             *@brief Holds return of `_lcompilers_string_format_fortran` 
             *        for the consumer of the return to free it after consumtion (print, write, ..).
             *
             *@details ** `_lcompilers_string_format_fortran` return a `char*` which needs
             *              to be freed by the consumer of that string.
             *         ** It complains if current return wasn't freed.
             */ 
            class StringFormatReturn {
                LLVMUtils   *llvmUtils_instance_;
                llvm::Value *return_val = nullptr; // Holds `_lcompilers_string_format_fortran()` call return
                bool previous_one_freed() { return return_val == nullptr; }

            public:

                StringFormatReturn(LLVMUtils *instance): llvmUtils_instance_(instance) {}

                /// Check that nothing is pending to be freed.
                bool all_clean(){ return return_val == nullptr; }
                
                void set(llvm::Value* val) {
                    LCOMPILERS_ASSERT_MSG(previous_one_freed(),
                                          "Previous StringFormatReturn not freed")
                    return_val = val;
                }

                void free() {
                    if(!return_val) return;
                    llvmUtils_instance_->lfortran_free(return_val);
                    return_val = nullptr;
                }
            };
            StringFormatReturn stringFormat_return {this};
    }; // LLVMUtils
    
    /**
     * @class LLVMFinalize
     * @brief Finalize variables before exiting their scope.
     * @details 
     * Notice Global variables aren't finalized; They live till program ends.
     *
     * 2 Main Finalizer :
     *   - `finalize_type_shell()` --> finalizes the shell of the type not the internals
     *   - `finalize_type()`--> dispatches to finalizers for each type. These ones finalizes the internals.
     * 
     * If container type (e.g. array) needs to finalize the shell of its contained type, It needs to implement its own
     * solution for that. You can see array is just using `free_array_ptr_to_consecutive_data`.
     *   
     *   
     * @see doc/src/llvm_utils.md 
     */
    class LLVMFinalize final {
    private:
        std::stack<ASR::Variable_t*>                                variable_symbol_stack;
        std::unique_ptr<LLVMUtils>                                  &llvm_utils_;
        std::unique_ptr<llvm::IRBuilder<>>                          &builder_;
        Allocator                                                   &al_;
        ASRToLLVMVisitor                                            &asr_to_llvm_visitor_;

    public:
        LLVMFinalize(ASRToLLVMVisitor &asr_to_llvm_visitor,
            std::unique_ptr<LLVMUtils> &llvm_utils, std::unique_ptr<llvm::IRBuilder<>> &builder, Allocator& al)  
        :   llvm_utils_(llvm_utils), builder_(builder), al_(al), asr_to_llvm_visitor_(asr_to_llvm_visitor){}

    private:

        void finalize_variable(ASR::Variable_t* const v){
            if(not_finalizable_variable(v)) return;
            variable_symbol_stack.push(v);

            insert_BB_for_readability((std::string("Finalize_Variable_")+v->m_name).c_str());
            auto const llvm_var = get_llvm_var(v);
            finalize_type(llvm_var, v->m_type);
            finalize_type_shell(llvm_var, v->m_type);

            variable_symbol_stack.pop();
        }

        void finalize_type_shell(llvm::Value* const var_ptr, ASR::ttype_t* const t){
            auto const is_allocatable = ASRUtils::is_allocatable(t);
            auto const t_past = ASRUtils::type_get_past_allocatable_pointer(t);
            switch (t_past->type) {
                case(ASR::StructType) :  
                case(ASR::Integer):
                case(ASR::Real):
                case(ASR::Complex):
                case(ASR::UnsignedInteger):
                case(ASR::Logical):
                case(ASR::List):
                case(ASR::Dict):
                case(ASR::Tuple):
                case(ASR::UnionType):
                case(ASR::Set):
                    if(is_allocatable) llvm_utils_->lfortran_free(var_ptr);
                break;
                case(ASR::FunctionType):
                case(ASR::CPtr):
                case(ASR::String):
                case(ASR::Array) :  
                // Do nothing
                break;
                default: 
                    throw LCompilersException("Unhandled Type.");
            }

        }
        void finalize_type(llvm::Value* const var_ptr, ASR::ttype_t* const t){
            auto const t_past = ASRUtils::type_get_past_allocatable_pointer(t);
            switch (t_past->type) {
                case(ASR::String):
                    finalize_string(var_ptr, t_past);
                break;
                case(ASR::Array) :  
                    finalize_array(var_ptr, t_past);
                break;
                case(ASR::StructType) :  
                    finalize_struct(var_ptr, t_past);
                break;
                case(ASR::List):
                    finalize_list(var_ptr, t_past);
                break;
                case(ASR::Dict):
                    finalize_dict(var_ptr, t_past);
                break;
                case(ASR::Tuple):
                    finalize_tuple(var_ptr, t_past);
                break;
                case(ASR::UnionType):
                    finalize_union(var_ptr, t_past);
                break;
                case(ASR::Set):
                    finalize_set(var_ptr, t_past);
                break;
                case(ASR::Integer):
                case(ASR::Real):
                case(ASR::Complex):
                case(ASR::UnsignedInteger):
                case(ASR::Logical):
                case(ASR::FunctionType):
                case(ASR::CPtr):
                // Pointers -- Do nothing
                break;
                default: 
                    throw LCompilersException("Unhandled Type. Could need finalizer");
            }

        }

        void finalize_string(llvm::Value* const str, ASR::ttype_t* const t){
            ASR::ttype_t* const type_past = ASRUtils::type_get_past_allocatable_pointer(t);
            ASR::String_t* const str_t = ASR::down_cast<ASR::String_t>(type_past);

            verify(str, get_llvm_type(t)->getPointerTo());
            
            /* Free */
            switch(str_t->m_physical_type){
                case ASR::DescriptorString: { // Operates on ` { i8*, i64 }* `
                    llvm::Value* const ptr_to_I8_ptr = llvm_utils_->create_gep2(llvm_utils_->string_descriptor, str, 0);
                    llvm_utils_->lfortran_free(llvm_utils_->CreateLoad2(llvm_utils_->character_type, ptr_to_I8_ptr));
                    // insert_null(llvm_utils_->character_type, ptr_to_I8_ptr);
                break;
                }
                case ASR::CChar:{ // Operates on ` i8** `
                    llvm_utils_->lfortran_free(llvm_utils_->CreateLoad2(llvm_utils_->character_type, str));
                    // insert_null(llvm_utils_->character_type, str_adjusted);
                break;
                }
                default:
                    throw LCompilersException("Unhandled");
                break;
            }
        }

        void finalize_array(llvm::Value* const arr, ASR::ttype_t* const t){
            auto *const arr_t            = ASR::down_cast<ASR::Array_t>(ASRUtils::type_get_past_allocatable_pointer(t));
            auto *const arr_llvm_t       = get_llvm_type(t);
            auto *const arrayType_llvm_t = get_llvm_type(arr_t->m_type);
            auto  const array_size_lazy  = [this, arr, t]() { 
                insert_BB_for_readability("Calculate_arraySize");
                return llvm_utils_->get_array_size(arr, get_llvm_type(t), t, &asr_to_llvm_visitor_);
            };
            switch(arr_t->m_physical_type){
                case ASR::DescriptorArray : { // e.g. `{ {i32, i64*}*, i32, %dimension_descriptor*, i1, i32 }`
                    verify(arr, get_llvm_type(&arr_t->base)->getPointerTo());
                    auto const data = builder_->CreateLoad(arrayType_llvm_t->getPointerTo(), llvm_utils_->create_gep2(arr_llvm_t, arr, 0));
                    free_array_data(data, arr_t->m_type, array_size_lazy);
                    free_array_ptr_to_consecutive_data(data, arr_t->m_type);
                break;
                }
                case ASR::PointerArray :{
                    auto const llvm_type_verify_against = ASRUtils::is_array_of_strings(&arr_t->base) ? 
                                                          get_llvm_type(&arr_t->base)->getPointerTo() :
                                                          get_llvm_type(&arr_t->base);
                    verify(arr, llvm_type_verify_against);
                    auto const data = arr;
                    free_array_data(data, arr_t->m_type, array_size_lazy);
                    free_array_ptr_to_consecutive_data(data, arr_t->m_type);
                    break;
                }
                case ASR::SIMDArray :
                case ASR::FixedSizeArray :
                // Do Nothing -- stack allocated.
                break;
                default :
                    throw LCompilersException("NOT HANDLED : Handle this case!");
                break;
            }
        }

        void finalize_scalar(llvm::Value* const ptr, ASR::ttype_t* const t){
            // Do nothing -- Scalars doesn't have internal allocations to be finalized.
            (void)ptr; (void)t;
        }

        void finalize_struct(llvm::Value* const ptr, ASR::ttype_t* const t){
            // >>>>> TO DO <<<<<
            // Get Struct symbol and push each variable into the `variable_symbol_stack`
            // * Verify
            // * Finailize
            (void)ptr; (void)t;
        }

        void finalize_list(llvm::Value* const ptr, ASR::ttype_t* const t){
            // >>>>> TO DO <<<<<
            // Verify
            // Loop on list -- Create a function to finalize each element.
            // Free the ptr holding the consecutive data.
            (void)ptr; (void)t;
        }

        void finalize_dict(llvm::Value* const ptr, ASR::ttype_t* const t){
            // >>>>> TO DO <<<<<
            // Verify
            // Loop on dictionary -- Create a function to finalize each element.
            // Free the ptr holding the consecutive data.
            (void)ptr; (void)t;
        }
        
        void finalize_set(llvm::Value* const ptr, ASR::ttype_t* const t){
            // >>>>> TO DO <<<<<
            // Verify
            // Loop on set -- Create a function to finalize each element.
            // Free the ptr holding the consecutive data.
            (void)ptr; (void)t;
        }

        void finalize_tuple(llvm::Value* const ptr, ASR::ttype_t* const t){
            // >>>>> TO DO <<<<<
            // Verify
            // Loop on tuple -- Create a function to finalize each element within (more like a struct).
            // Free the ptr holding the consecutive data.
            (void)ptr; (void)t;
        }

        void finalize_union(llvm::Value* const ptr, ASR::ttype_t* const t){
            // >>>>> TO DO <<<<<
            // Verify
            // Loop on union -- Create a function to finalize each element within (more like a struct).
            // Free the ptr holding the consecutive data.
            (void)ptr; (void)t;
        }
        
/*>>>>>>>>>>>>>>>>>>>>> Array Finalization Utilities <<<<<<<<<<<<<<<<<<<<<<< */

        /**
         * @brief Handles the process of freeing each and every struct in an array.

         * @param data_ptr should be a pointer to array's data (e.g. `{i32, i64}*`)
         * @param struct_t should be the underlying ASR struct type of the array.
         * @param array_size is the size of the array.
         *
         * @details Create a loop on `array_size` to fetch each struct, then call finalize_struct on it.
         */ 
        void free_array_structs(llvm::Value* const data_ptr, ASR::StructType_t* const struct_t, llvm::Value* array_size){
            auto const iter_llvm_type =llvm::Type::getInt64Ty(builder_->getContext());
            auto const iter = builder_->CreateAlloca(iter_llvm_type, nullptr, "arrSize_iter");
            builder_->CreateStore(llvm::ConstantInt::get(iter_llvm_type, -1 , true), iter);

            auto const cond_fn = [&](){ // while(++arrSize_iter < array_size)
                auto const loaded_iter = builder_->CreateLoad(iter_llvm_type, iter);
                auto const loaded_iter_incr = builder_->CreateAdd(loaded_iter, llvm::ConstantInt::get(iter_llvm_type, 1)); // arrSize_iter + 1
                builder_->CreateStore(loaded_iter_incr, iter);
                return builder_->CreateICmpSLT(loaded_iter_incr, array_size);
            };

            auto const body_fn = [&]() -> void {
                auto const loaded_iter = builder_->CreateLoad(iter_llvm_type, iter);
                auto const struct_type_llvm = get_llvm_type(&struct_t->base);
                auto const struct_element = llvm_utils_->create_ptr_gep2(struct_type_llvm, data_ptr, loaded_iter);
                finalize_struct(struct_element, &struct_t->base);
            };

            llvm_utils_->create_loop("Finalize_structArray", cond_fn , body_fn);
        }
            
        /**
         * @details It's responsbile of deallocating each element within the array.
         * Example : finalize each {i32, i64*} within the array.
         * Notice: it's a utility for `finalize_array`, not meant to be used by other finalizers.
         *
         * @param data_ptr  should be a pointer to array's data (e.g. `i32*` OR `{i64, f32}*`)
         * @param data_type should be the underlying ASR type of the array.
         * @param array_size is lambda object returning array's size. Lazy evaluate as only array of structs requires looping on each element (for now).
         */
        template<typename LazyEval>
        void free_array_data(llvm::Value* const data_ptr, ASR::ttype_t* const data_type, LazyEval &array_size){
            LCOMPILERS_ASSERT(!ASRUtils::is_allocatable_or_pointer(data_type))
            verify(data_ptr, get_llvm_type(data_type)->getPointerTo());
            switch(data_type->type){
                case ASR::StructType : // Loop and free
                    free_array_structs(data_ptr, ASR::down_cast<ASR::StructType_t>(data_type), array_size());
                break;
                case ASR::String : // Force string finalization on this single string. -- Don't loop, One string holds all.
                    finalize_string(data_ptr, data_type); 
                break; 
                case ASR::Integer :
                case ASR::Real :
                case ASR::Complex :
                case ASR::UnsignedInteger:
                case ASR::Logical :
                case ASR::CPtr:
                // Do Nothing.
                break;
                default:
                throw LCompilersException("Unhandled array type");
            }
        }

        void free_array_ptr_to_consecutive_data(llvm::Value* const ptr, ASR::ttype_t* const t){
            if(ASRUtils::extract_type(t)->type == ASR::String){ 
                // Array of strings are special handled. 
                // it's always stack allocated. (e.g. StringDescriptor -> {i8*, i64})
                return;
            }
            llvm_utils_->lfortran_free(ptr);
        }

/*>>>>>>>>>>>>>>>>>>>>> Utilities <<<<<<<<<<<<<<<<<<<<<<< */

        /// Return LLVM type of the passed ASR type.
        /// It handles the details of what symbol to pass to `get_type_from_ttype_t_util`
        llvm::Type* get_llvm_type(ASR::ttype_t* type){
            static auto const dummy_var_symbol = ASRUtils::EXPR(ASR::make_Var_t(al_, type->base.loc, nullptr));
            LCOMPILERS_ASSERT_MSG(!variable_symbol_stack.empty(),
                                    "You're requesting a type without a variable symbol in stack; "
                                    "If it's of structType we won't be able to deduce its type")
            auto const current_variable_sym = variable_symbol_stack.top();
            ASR::down_cast<ASR::Var_t>(dummy_var_symbol)->m_v = &current_variable_sym->base;
            /* Verify structType and variable are matching */
            if(ASRUtils::extract_type(type)->type == ASR::StructType){
                LCOMPILERS_ASSERT_MSG(ASRUtils::extract_type(current_variable_sym->m_type) == ASRUtils::extract_type(type)
                                     , "Requesting an LLVM type of structType while the variable to extract the struct symbol from "
                                        "probably isn't of the same type -- Also manually check as this isn't 100% guaranteed");
            }
            return llvm_utils_->get_type_from_ttype_t_util(dummy_var_symbol, type, llvm_utils_->module);
        }

        static bool not_finalizable_variable(ASR::Variable_t* const v){ // can't deallocate
            /* TODO :: Handle non local + `Value` attribute. */
            return v->m_intent != ASR::Local
                || ASRUtils::is_pointer(v->m_type)
                || v->m_storage == ASR::Parameter
                || v->m_storage == ASR::Save /*Neglect - Lives till program ends*/;
        }

        static bool non_deallocatable_construct(ASR::asr_t* const s){ // Can't deallocate
            ASR::symbol_t* sym = ASR::is_a<ASR::symbol_t>(*s) ? ASR::down_cast<ASR::symbol_t>(s) : nullptr;
            const bool is_interface = sym && ASR::is_a<ASR::Function_t>(*sym)
                                      && ASRUtils::get_FunctionType(sym)->m_deftype == ASR::Interface;
            const bool is_external_abi = sym && ASR::is_a<ASR::Function_t>(*sym)
                                      && ASRUtils::get_FunctionType(sym)->m_abi == ASR::ExternalUndefined;
            const bool is_module = sym && ASR::is_a<ASR::Module_t>(*sym);
            const bool is_TU = !sym && ASR::is_a<ASR::unit_t>(*s) && ASR::is_a<ASR::TranslationUnit_t>(*(ASR::unit_t*)s);
            return is_TU || is_module || is_interface || is_external_abi ;
        }

        static bool is_variable(ASR::symbol_t* const s){
            return s->type == ASR::Variable;
        }

        llvm::Value* get_llvm_var(ASR::Variable_t* const v){
            llvm::Value* llvm_var {}; {
                const uint32_t v_h = get_hash((ASR::asr_t*)v);
                LCOMPILERS_ASSERT(llvm_utils_->llvm_symtab.find(v_h) != llvm_utils_->llvm_symtab.end());
                llvm_var = llvm_utils_->llvm_symtab[v_h];
            }
            auto const load_needed = LLVM::is_llvm_pointer(*v->m_type);
            if(load_needed){
                auto const variable_llvm_type = get_llvm_type(v->m_type);
                llvm_var = llvm_utils_->CreateLoad2(variable_llvm_type, llvm_var);
            }
            return llvm_var;
        }

        /**
            * Checks that `ptr` is as same as `llvm_type` 
            * @param ptr must be a ptr to the underlying type
            * @param llvm_type the llvm type we want ptr to be as same as.
            *
            * @details Only functional with debug mode.
         */
        void verify(llvm::Value* const ptr, llvm::Type* const llvm_type){
        #if !defined(WITH_LFORTRAN_ASSERT)
            return (void)(ptr && llvm_type);
        #else
            auto const ptr_type = ptr->getType();
            if( !ptr_type->isPointerTy() ) throw LCompilersException("`ptr` is expected to be pointer");
            if( ptr_type != llvm_type ){
                throw LCompilersException(
                    "Unmatching Types :\n"
                        "ptr_type -->" + llvm_utils_->get_llvm_type_as_string(ptr_type) + "\n"
                    + "ptr to actual type -->" + llvm_utils_->get_llvm_type_as_string(llvm_type) + "\n");
            }
        #endif
        }

        /// Inserts a Basic block only for the sake of IR readability.
        void insert_BB_for_readability(const char* bb_name){
            auto const BB = llvm::BasicBlock::Create(builder_->getContext(), bb_name);
            llvm_utils_->start_new_block(BB);
        }

        /// Inserts null into freed ptr holder -- Useful only for debugging
        void insert_null(llvm::Type* null_type, llvm::Value* ptr){ 
            #if LLVM_VERSION_MAJOR <= 15
                if(!(ptr->getType()->isPointerTy() && ptr->getType()->getPointerElementType()->isPointerTy()))
                    throw LCompilersException("ptr parameter must be a PTR to PTR type.");
            #endif
            builder_->CreateStore(
                llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(null_type)), ptr);
        }

    public:

/*>>>>>>>>>>>>>>>>>>>>> Entry <<<<<<<<<<<<<<<<<<<<<<< */

        void finalize_symtab(SymbolTable* symtab){
            LCOMPILERS_ASSERT(!non_deallocatable_construct(symtab->asr_owner))
            auto const finalize_str = std::string("FINALIZE_SYMTABLE_") + 
                                      std::string(ASRUtils::symbol_name(ASR::down_cast<ASR::symbol_t>(symtab->asr_owner)));
            insert_BB_for_readability(finalize_str.c_str());
            auto MAP = symtab->get_scope();
            for(auto &str_sym_pair : MAP){
                ASR::symbol_t* const sym = str_sym_pair.second;
                if (is_variable(sym)){
                    finalize_variable(ASR::down_cast<ASR::Variable_t>(sym));
                }
            }
        }
    };

    class LLVMList {
        private:

            llvm::LLVMContext& context;
            LLVMUtils* llvm_utils;
            llvm::IRBuilder<>* builder;

            std::map<std::string, std::tuple<llvm::Type*, int32_t, llvm::Type*>> typecode2listtype;

            void resize_if_needed_using_typecode(std::string& type_code, llvm::Value* list, llvm::Value* n,
                                                  llvm::Value* capacity, int32_t type_size,
                                                  llvm::Type* el_type, llvm::Module* module);

            void shift_end_point_by_one_using_typecode(std::string& type_code, llvm::Value* list);

        public:

            LLVMList(llvm::LLVMContext& context_, LLVMUtils* llvm_utils,
                     llvm::IRBuilder<>* builder);

            llvm::Type* get_list_type(llvm::Type* el_type, std::string& type_code,
                                        int32_t type_size);

            void list_init(std::string& type_code, llvm::Value* list,
                           llvm::Module* module, llvm::Value* initial_capacity,
                           llvm::Value* n);

            void list_init(std::string& type_code, llvm::Value* list,
                            llvm::Module* module, int32_t initial_capacity=1,
                            int32_t n=0);

            llvm::Value* get_pointer_to_list_data_using_type(llvm::Type* list_type, llvm::Value* list);

            llvm::Value* get_pointer_to_current_end_point_using_type(llvm::Type* list_type, llvm::Value* list);

            llvm::Value* get_pointer_to_current_capacity_using_type(llvm::Type* list_type, llvm::Value* list);

            void list_deepcopy(ASR::expr_t* src_expr, llvm::Value* src, llvm::Value* dest,
                ASR::List_t* list_type, llvm::Module* module);

            void list_deepcopy(ASR::expr_t* src_expr, llvm::Value* src, llvm::Value* dest,
                ASR::ttype_t* element_type, llvm::Module* module);

            llvm::Value* read_item_using_ttype(ASR::ttype_t* el_asr_type, llvm::Value* list, llvm::Value* pos,
                                                   bool enable_bounds_checking,
                                                   llvm::Module* module, bool get_pointer=false);


            llvm::Value* len_using_type(llvm::Type* list_type, llvm::Value* list);

            void check_index_within_bounds_using_type(llvm::Type* list_type, llvm::Value* list, llvm::Value* pos,
                                           llvm::Module* module);

            void write_item(ASR::expr_t* expr, llvm::Value* list, llvm::Value* pos,
                llvm::Value* item, ASR::ttype_t* asr_type,
                bool enable_bounds_checking, llvm::Module* module);

            void write_item_using_ttype(ASR::ttype_t* el_asr_type, llvm::Value* list, llvm::Value* pos,
                            llvm::Value* item, bool enable_bounds_checking,
                            llvm::Module* module);

            void append(ASR::expr_t* list_expr, llvm::Value* list, llvm::Value* item,
                        ASR::ttype_t* asr_type, llvm::Module* module);

            void insert_item(ASR::expr_t* list_expr, llvm::Value* list, llvm::Value* pos,
                            llvm::Value* item, ASR::ttype_t* asr_type,
                            llvm::Module* module);

            void reserve(llvm::Value* list, llvm::Value* n,
                         ASR::ttype_t* asr_type, llvm::Module* module);

            void remove(llvm::Value* list, llvm::Value* item,
                        ASR::ttype_t* item_type, llvm::Module* module);

            llvm::Value* pop_position(ASR::expr_t* list_expr, llvm::Value* list, llvm::Value* pos,
                                      ASR::ttype_t* list_type, llvm::Module* module);

            llvm::Value* pop_last(llvm::Value* list, ASR::ttype_t* list_asr_type, llvm::Module* module);

            void list_clear_using_type(llvm::Type* list_type, llvm::Value* list);

            void reverse(ASR::ttype_t* el_type, llvm::Value* list, llvm::Module* module);

            llvm::Value* find_item_position(llvm::Value* list,
                llvm::Value* item, ASR::ttype_t* item_type,
                llvm::Module* module, llvm::Value* start=nullptr,
                llvm::Value* end=nullptr);

            llvm::Value* index(llvm::Value* list, llvm::Value* item,
                                llvm::Value* start, llvm::Value* end,
                                ASR::ttype_t* item_type, llvm::Module* module);

            llvm::Value* count(llvm::Value* list, llvm::Value* item,
                                ASR::ttype_t* item_type, llvm::Module* module);

            void free_data_using_type(llvm::Type* list_type, llvm::Value* list, llvm::Module* module);

            void free_data_using_type(std::string& type_code, llvm::Value* list, llvm::Module* module);

            llvm::Value* check_list_equality(llvm::Value* l1, llvm::Value* l2, ASR::ttype_t *item_type,
                llvm::LLVMContext& context, llvm::IRBuilder<>* builder, llvm::Module* module);

            llvm::Value* check_list_inequality(llvm::Value* l1, llvm::Value* l2,
                ASR::ttype_t *item_type, llvm::LLVMContext& context,
                llvm::IRBuilder<>* builder, llvm::Module* module,
                int8_t overload_id, ASR::ttype_t* int32_type=nullptr);

            void list_repeat_copy(ASR::List_t* list_type, llvm::Value* repeat_list, llvm::Value* init_list,
                                  llvm::Value* num_times, llvm::Value* init_list_len,
                                  llvm::Module* module);
    };

    class LLVMStruct {
        private:

            llvm::LLVMContext& context;
            LLVMUtils* llvm_utils;
            llvm::IRBuilder<>* builder;
            std::map<ASR::symbol_t*, llvm::Constant*> newclass2vtab;
            std::map<ASR::symbol_t*, llvm::Type*> newclass2vtabtype;
            std::map<uint64_t, llvm::Function*>& llvm_symtab_fn;
            std::function<void(ASR::Struct_t*, llvm::Value*, ASR::ttype_t*, bool)> allocate_struct_array_members;

        public:

            std::map<ASR::symbol_t*, llvm::Constant*> newclass2typeinfo;   // Contains type-info object pointer for each struct
            std::map<std::string, llvm::Constant*> intrinsic_type_info;   // Contains type-info object pointer for each intrincic type and kind
            std::map<std::string, llvm::Constant*> intrinsic_type_vtab;
            std::map<std::string, llvm::Type*> intrinsic_type_vtabtype;
            std::map<ASR::symbol_t*, std::map<std::string, int64_t>> struct_vtab_function_offset;

            LLVMStruct(llvm::LLVMContext& context_, LLVMUtils* llvm_utils,
                     llvm::IRBuilder<>* builder, std::map<uint64_t, llvm::Function*>& llvm_symtab_fn_,
                      std::function<void(ASR::Struct_t*, llvm::Value*, ASR::ttype_t*, bool)> allocate_arr_mem_struct);
    
            llvm::Constant* get_pointer_to_method(ASR::symbol_t* struct_sym, llvm::Module* module);
            void store_class_vptr(ASR::symbol_t* struct_sym, llvm::Value* ptr, llvm::Module* module);
            void store_intrinsic_type_vptr(ASR::ttype_t* ttype, int kind, llvm::Value* ptr, llvm::Module* module);

            void collect_vtable_function_impls(ASR::symbol_t* struct_sym,
                                            std::vector<llvm::Constant*>& impls,
                                            llvm::Module* module);

            void create_type_info_for_intrinsic_type(ASR::ttype_t* ttype,
                                            int kind, llvm::Module* module);
            void create_vtab_for_intrinsic_type(ASR::ttype_t* ttype,
                                            int kind, llvm::Module* module);

            void create_new_vtable_for_struct_type(ASR::symbol_t* struct_sym,
                                                llvm::Module* module);
            void create_type_info_for_struct(ASR::symbol_t* struct_sym,
                                            llvm::Module* module);

            llvm::Function* get_allocate_struct_function(ASR::symbol_t* struct_sym, llvm::Module* module);

            llvm::Function* define_struct_copy_function(ASR::symbol_t* struct_sym,
                                                        llvm::Module* module);
            void fill_struct_copy_body(ASR::symbol_t* struct_sym,
                                    llvm::Function* func,
                                    llvm::Module* module);
            
            llvm::Function* define_intrinsic_type_copy_function(ASR::ttype_t* type, llvm::Module* module); 

            void fill_intrinsic_type_copy_body(ASR::ttype_t* type, llvm::Function* func, llvm::Module* module);

            void struct_deepcopy(ASR::expr_t* src_expr, llvm::Value* src, ASR::ttype_t* src_ty,
                                ASR::ttype_t* dest_ty, llvm::Value* dest, llvm::Module* module);
    };

    class LLVMTuple {
        private:

            llvm::LLVMContext& context;
            LLVMUtils* llvm_utils;
            // llvm::IRBuilder<>* builder;

            std::map<std::string, std::pair<llvm::Type*, size_t>> typecode2tupletype;

        public:

            LLVMTuple(llvm::LLVMContext& context_,
                      LLVMUtils* llvm_utils,
                      llvm::IRBuilder<>* builder);

            llvm::Type* get_tuple_type(std::string& type_code,
                                       std::vector<llvm::Type*>& el_types);

            void tuple_init(ASR::expr_t* tuple_expr, llvm::Value* llvm_tuple, std::vector<llvm::Value*>& values,
                            ASR::Tuple_t* tuple_type, llvm::Module* module);

            llvm::Value* read_item(llvm::Value* llvm_tuple, ASR::Tuple_t* tuple_type,
                                   size_t pos, bool get_pointer=false);

            llvm::Value* read_item_using_pos_value(llvm::Type* el_type, llvm::Value* llvm_tuple, ASR::Tuple_t* tuple_type, llvm::Value* pos,
                                   bool get_pointer=false);

            llvm::Value* read_item_using_pos(llvm::Type* el_type, llvm::Value* llvm_tuple, ASR::Tuple_t* tuple_type, size_t pos,
                                   bool get_pointer=false);

            void tuple_deepcopy(ASR::expr_t* tuple_expr, llvm::Value* src, llvm::Value* dest,
                                ASR::Tuple_t* type_code, llvm::Module* module);

            llvm::Value* check_tuple_equality(llvm::Value* t1, llvm::Value* t2,
                ASR::Tuple_t* tuple_type, llvm::LLVMContext& context,
                llvm::IRBuilder<>* builder, llvm::Module* module);

            llvm::Value* check_tuple_inequality(llvm::Value* t1, llvm::Value* t2,
                ASR::Tuple_t* tuple_type, llvm::LLVMContext& context,
                llvm::IRBuilder<>* builder, llvm::Module* module, int8_t overload_id);

            void concat(ASR::expr_t* tuple_1_expr, llvm::Value* t1, llvm::Value* t2, ASR::Tuple_t* tuple_type_1,
                        ASR::Tuple_t* tuple_type_2, llvm::Value* concat_tuple,
                        ASR::Tuple_t* concat_tuple_type, llvm::Module* module);
    };

    class LLVMDictInterface {

        protected:

            llvm::LLVMContext& context;
            LLVMUtils* llvm_utils;
            llvm::IRBuilder<>* builder;
            llvm::AllocaInst *pos_ptr, *is_key_matching_var;
            llvm::AllocaInst *idx_ptr, *hash_iter, *hash_value;
            llvm::AllocaInst *polynomial_powers;
            llvm::AllocaInst *chain_itr, *chain_itr_prev;
            llvm::AllocaInst *old_capacity, *old_key_value_pairs, *old_key_mask;
            llvm::AllocaInst *old_occupancy, *old_number_of_buckets_filled;
            llvm::AllocaInst *src_itr, *dest_itr, *next_ptr, *copy_itr;
            llvm::Value *tmp_value_ptr;

            std::map<std::pair<std::string, std::string>,
                     std::tuple<llvm::Type*, std::pair<int32_t, int32_t>,
                                std::pair<llvm::Type*, llvm::Type*>>> typecode2dicttype;

        public:

            bool is_dict_present_;

            LLVMDictInterface(
                llvm::LLVMContext& context_,
                LLVMUtils* llvm_utils,
                llvm::IRBuilder<>* builder);

            virtual
            llvm::Type* get_dict_type(std::string key_type_code, std::string value_type_code,
                int32_t key_type_size, int32_t value_type_size,
                llvm::Type* key_type, llvm::Type* value_type) = 0;

            virtual
            void dict_init(ASR::Dict_t* dict_type, llvm::Value* dict, llvm::Module* module, size_t initial_capacity) = 0;

            virtual
            llvm::Value* get_key_list(llvm::Type* type, llvm::Value* dict) = 0;

            virtual
            llvm::Value* get_value_list(llvm::Type* type, llvm::Value* dict) = 0;

            virtual
            llvm::Value* get_pointer_to_occupancy(llvm::Type* type, llvm::Value* dict) = 0;

            virtual
            llvm::Value* get_pointer_to_capacity_using_type(ASR::ttype_t* key_type, ASR::ttype_t* value_type, llvm::Value* dict) = 0;

            virtual
            llvm::Value* get_string_hash(llvm::Value* capacity, llvm::Value* key);

            virtual
            llvm::Value* get_descriptor_string_hash(llvm::Value* capacity, llvm::Value* key, ASR::String_t* type);

            virtual
            llvm::Value* get_key_hash(llvm::Value* capacity, llvm::Value* key,
                ASR::ttype_t* key_asr_type, llvm::Module* module);

            virtual
            void resolve_collision_for_write(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
                llvm::Value* key, llvm::Value* value,
                llvm::Module* module, ASR::ttype_t* key_asr_type,
                ASR::ttype_t* value_asr_type) = 0;

            virtual
            llvm::Value* resolve_collision_for_read(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
                llvm::Value* key, llvm::Module* module,
                ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type) = 0;

            virtual
            llvm::Value* resolve_collision_for_read_with_bound_check(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
                llvm::Value* key, llvm::Module* module,
                ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type) = 0;

            virtual
            llvm::Value* resolve_collision_for_read_with_default(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
                llvm::Value* key, llvm::Module* module,
                ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type, llvm::Value* def_value) = 0;

            virtual
            void rehash(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Module* module,
                ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type) = 0;

            virtual
            void rehash_all_at_once_if_needed(ASR::expr_t* dict_expr, llvm::Value* dict,
                llvm::Module* module,
                ASR::ttype_t* key_asr_type,
                ASR::ttype_t* value_asr_type) = 0;

            virtual
            void write_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
                llvm::Value* value, llvm::Module* module,
                ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type);

            virtual
            llvm::Value* read_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
                llvm::Module* module, ASR::Dict_t* dict_type, bool enable_bounds_checking,
                bool get_pointer=false) = 0;

            virtual
            llvm::Value* get_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
                llvm::Module* module, ASR::Dict_t* dict_type, llvm::Value* def_value,
                bool get_pointer=false) = 0;

            virtual
            llvm::Value* pop_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
                llvm::Module* module, ASR::Dict_t* dict_type,
                bool get_pointer=false) = 0;


            virtual
            void dict_deepcopy(ASR::expr_t* src_expr, llvm::Value* src, llvm::Value* dest,
                ASR::Dict_t* dict_type, llvm::Module* module) = 0;

            virtual
            llvm::Value* len(llvm::Type* type, llvm::Value* dict) = 0;

            virtual
            bool is_dict_present();

            virtual
            void set_is_dict_present(bool value);

            virtual
            void get_elements_list(ASR::expr_t* expr, llvm::Value* dict,
                llvm::Value* elements_list, ASR::ttype_t* key_asr_type,
                ASR::ttype_t* value_asr_type, llvm::Module* module,
                bool key_or_value) = 0;

            virtual ~LLVMDictInterface() = 0;

    };

    class LLVMDict: public LLVMDictInterface {

        public:

            LLVMDict(llvm::LLVMContext& context_,
                     LLVMUtils* llvm_utils,
                     llvm::IRBuilder<>* builder);

            llvm::Type* get_dict_type(std::string key_type_code, std::string value_type_code,
                int32_t key_type_size, int32_t value_type_size,
                llvm::Type* key_type, llvm::Type* value_type);

            void dict_init(ASR::Dict_t* dict_type, llvm::Value* dict, llvm::Module* module, size_t initial_capacity);

            llvm::Value* get_key_list(llvm::Type* type, llvm::Value* dict);

            llvm::Value* get_value_list(llvm::Type* type, llvm::Value* dict);

            llvm::Value* get_pointer_to_occupancy(llvm::Type* type, llvm::Value* dict);

            llvm::Value* get_pointer_to_occupancy_using_type(ASR::ttype_t* key_type, ASR::ttype_t* value_type, llvm::Value* dict);

            llvm::Value* get_pointer_to_capacity_using_type(ASR::ttype_t* key_type, ASR::ttype_t* value_type, llvm::Value* dict);

            virtual
            void resolve_collision(ASR::expr_t* dict_expr, llvm::Value* capacity, llvm::Value* key_hash,
                                llvm::Value* key, llvm::Value* key_list,
                                llvm::Value* key_mask, llvm::Module* module,
                                ASR::ttype_t* key_asr_type, bool for_read=false);

            void resolve_collision_for_write(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
                                          llvm::Value* key, llvm::Value* value,
                                          llvm::Module* module, ASR::ttype_t* key_asr_type,
                                          ASR::ttype_t* value_asr_type);

            void _check_key_present_or_default(ASR::expr_t* dict_expr, llvm::Module* module, llvm::Value *key, llvm::Value *key_list,
                ASR::ttype_t* key_asr_type, llvm::Value *value_list, ASR::ttype_t* value_asr_type, llvm::Value *pos,
                llvm::Value *def_value, llvm::Value* &result);

            llvm::Value* resolve_collision_for_read(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
                                                 llvm::Value* key, llvm::Module* module,
                                                 ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type);

            llvm::Value* resolve_collision_for_read_with_bound_check(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
                                                 llvm::Value* key, llvm::Module* module,
                                                 ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type);

            llvm::Value* resolve_collision_for_read_with_default(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
                                                 llvm::Value* key, llvm::Module* module,
                                                 ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type,
                                                 llvm::Value* def_value);

            void rehash(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Module* module,
                        ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type);

            void rehash_all_at_once_if_needed(ASR::expr_t* dict_expr, llvm::Value* dict,
                                              llvm::Module* module,
                                              ASR::ttype_t* key_asr_type,
                                              ASR::ttype_t* value_asr_type);

            llvm::Value* read_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
                                   llvm::Module* module, ASR::Dict_t* key_asr_type, bool enable_bounds_checking,
                                   bool get_pointer=false);

            llvm::Value* get_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
                                   llvm::Module* module, ASR::Dict_t* key_asr_type, llvm::Value* def_value,
                                   bool get_pointer=false);

            llvm::Value* pop_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
                                   llvm::Module* module, ASR::Dict_t* dict_type,
                                   bool get_pointer=false);

            virtual
            llvm::Value* get_pointer_to_keymask(ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type, llvm::Value* dict);

            void dict_deepcopy(ASR::expr_t* src_expr, llvm::Value* src, llvm::Value* dest,
                ASR::Dict_t* dict_type, llvm::Module* module);

            llvm::Value* len(llvm::Type* type, llvm::Value* dict);

            void get_elements_list(ASR::expr_t* expr, llvm::Value* dict,
                llvm::Value* elements_list, ASR::ttype_t* key_asr_type,
                ASR::ttype_t* value_asr_type, llvm::Module* module,
                bool key_or_value);

            virtual ~LLVMDict();
    };

    class LLVMDictOptimizedLinearProbing: public LLVMDict {

        public:

            LLVMDictOptimizedLinearProbing(llvm::LLVMContext& context_,
                                    LLVMUtils* llvm_utils,
                                    llvm::IRBuilder<>* builder);

            void resolve_collision(ASR::expr_t* dict_expr, llvm::Value* capacity, llvm::Value* key_hash,
                                llvm::Value* key, llvm::Value* key_list,
                                llvm::Value* key_mask, llvm::Module* module,
                                ASR::ttype_t* key_asr_type, bool for_read=false);

            void resolve_collision_for_write(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
                                            llvm::Value* key, llvm::Value* value,
                                            llvm::Module* module, ASR::ttype_t* key_asr_type,
                                            ASR::ttype_t* value_asr_type);

            llvm::Value* resolve_collision_for_read(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
                                                    llvm::Value* key, llvm::Module* module,
                                                    ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type);

            llvm::Value* resolve_collision_for_read_with_bound_check(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
                                                    llvm::Value* key, llvm::Module* module,
                                                    ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type);

            llvm::Value* resolve_collision_for_read_with_default(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
                                                    llvm::Value* key, llvm::Module* module,
                                                    ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type,
                                                    llvm::Value *def_value);

            virtual ~LLVMDictOptimizedLinearProbing();

    };

    class LLVMDictSeparateChaining: public LLVMDictInterface {

        protected:

            std::map<std::pair<std::string, std::string>, llvm::Type*> typecode2kvstruct;
    
            llvm::Value* get_pointer_to_number_of_filled_buckets_using_type(ASR::ttype_t* key_type, 
                ASR::ttype_t* value_type, llvm::Value* dict);

            llvm::Value* get_pointer_to_key_value_pairs_using_type(ASR::ttype_t* key_type, 
                ASR::ttype_t* value_type, llvm::Value* dict);

            llvm::Value* get_pointer_to_rehash_flag_using_type(ASR::ttype_t* key_type, 
                ASR::ttype_t* value_type, llvm::Value* dict);

            void deepcopy_key_value_pair_linked_list(ASR::expr_t* src_expr, llvm::Value* srci, llvm::Value* desti,
                llvm::Value* dest_key_value_pairs, ASR::Dict_t* dict_type, llvm::Module* module);

            void write_key_value_pair_linked_list(ASR::expr_t* dict_expr, llvm::Value* kv_ll, llvm::Value* dict,
                llvm::Value* capacity, ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type, llvm::Module* module);

            void resolve_collision(ASR::expr_t* dict_expr, llvm::Value* capacity, llvm::Value* key_hash,
                llvm::Value* key, llvm::Value* key_value_pair_linked_list,
                llvm::Type* kv_pair_type, llvm::Value* key_mask,
                llvm::Module* module, ASR::ttype_t* key_asr_type);

            llvm::Type* get_key_value_pair_type(std::string key_type_code, std::string value_type_code);

            llvm::Type* get_key_value_pair_type(ASR::ttype_t* key_asr_type, ASR::ttype_t* value_pair_type);

            void dict_init_given_initial_capacity(ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type, llvm::Value* dict, 
                llvm::Module* module, llvm::Value* initial_capacity);

        public:

            LLVMDictSeparateChaining(
                llvm::LLVMContext& context_,
                LLVMUtils* llvm_utils_,
                llvm::IRBuilder<>* builder_);

            llvm::Type* get_dict_type(std::string key_type_code, std::string value_type_code,
                int32_t key_type_size, int32_t value_type_size,
                llvm::Type* key_type, llvm::Type* value_type);

            void dict_init(ASR::Dict_t* dict_type, llvm::Value* dict, llvm::Module* module, size_t initial_capacity);

            llvm::Value* get_key_list(llvm::Type* type, llvm::Value* dict);

            llvm::Value* get_value_list(llvm::Type* type, llvm::Value* dict);

            llvm::Value* get_pointer_to_occupancy(llvm::Type* type, llvm::Value* dict);

            llvm::Value* get_pointer_to_occupancy_using_type(ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type, llvm::Value* dict);

            llvm::Value* get_pointer_to_capacity(llvm::Type* type, llvm::Value* dict);

            llvm::Value* get_pointer_to_capacity_using_type(ASR::ttype_t* key_type, ASR::ttype_t* value_type, llvm::Value* dict);

            void resolve_collision_for_write(
                ASR::expr_t* dict_expr,
                llvm::Value* dict,
                llvm::Value* key_hash,
                llvm::Value* key,
                llvm::Value* value,
                llvm::Module* module,
                ASR::ttype_t* key_asr_type,
                ASR::ttype_t* value_asr_type);

            llvm::Value* resolve_collision_for_read(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
                llvm::Value* key, llvm::Module* module,
                ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type);

            llvm::Value* resolve_collision_for_read_with_bound_check(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
                llvm::Value* key, llvm::Module* module,
                ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type);

            llvm::Value* resolve_collision_for_read_with_default(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key_hash,
                llvm::Value* key, llvm::Module* module,
                ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type,
                llvm::Value* def_value);

            void rehash(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Module* module,
                ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type);

            void rehash_all_at_once_if_needed(ASR::expr_t* dict_expr, llvm::Value* dict,
                llvm::Module* module,
                ASR::ttype_t* key_asr_type,
                ASR::ttype_t* value_asr_type);

            llvm::Value* read_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
                llvm::Module* module, ASR::Dict_t* dict_type, bool enable_bounds_checking,
                bool get_pointer=false);

            llvm::Value* get_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
                llvm::Module* module, ASR::Dict_t* dict_type, llvm::Value* def_value,
                bool get_pointer=false);

            llvm::Value* pop_item(ASR::expr_t* dict_expr, llvm::Value* dict, llvm::Value* key,
                llvm::Module* module, ASR::Dict_t* dict_type,
                bool get_pointer=false);

            llvm::Value* get_pointer_to_keymask(ASR::ttype_t* key_asr_type, ASR::ttype_t* value_asr_type, llvm::Value* dict);

            void dict_deepcopy(ASR::expr_t* src_expr, llvm::Value* src, llvm::Value* dest,
                ASR::Dict_t* dict_type, llvm::Module* module);

            llvm::Value* len(llvm::Type* type, llvm::Value* dict);

            void get_elements_list(ASR::expr_t* expr, llvm::Value* dict,
                llvm::Value* elements_list, ASR::ttype_t* key_asr_type,
                ASR::ttype_t* value_asr_type, llvm::Module* module,
                bool key_or_value);

            virtual ~LLVMDictSeparateChaining();

    };

    class LLVMSetInterface {

        protected:

            llvm::LLVMContext& context;
            LLVMUtils* llvm_utils;
            llvm::IRBuilder<>* builder;
            llvm::AllocaInst *pos_ptr, *is_el_matching_var;
            llvm::AllocaInst *idx_ptr, *hash_iter, *hash_value;
            llvm::AllocaInst *polynomial_powers;
            llvm::AllocaInst *chain_itr, *chain_itr_prev;
            llvm::AllocaInst *old_capacity, *old_elems, *old_el_mask;
            llvm::AllocaInst *old_occupancy, *old_number_of_buckets_filled;
            llvm::AllocaInst *src_itr, *dest_itr, *next_ptr, *copy_itr;

            std::map<std::string, std::tuple<llvm::Type*, int32_t, llvm::Type*>> typecode2settype;

        public:

            bool is_set_present_;

            LLVMSetInterface(
                llvm::LLVMContext& context_,
                LLVMUtils* llvm_utils,
                llvm::IRBuilder<>* builder);

            virtual
            llvm::Type* get_set_type(std::string type_code,
                int32_t type_size, llvm::Type* el_type) = 0;

            virtual
            void set_init(std::string type_code, llvm::Value* set,
                llvm::Module* module, size_t initial_capacity) = 0;

            virtual
            llvm::Value* get_el_list(llvm::Type* type, llvm::Value* set) = 0;

            virtual
            llvm::Value* get_pointer_to_occupancy(llvm::Type* type, llvm::Value* set) = 0;

            virtual
            llvm::Value* get_pointer_to_capacity_using_type(llvm::Type* el_list_type, llvm::Value* set) = 0;

            virtual
            llvm::Value* get_pointer_to_occupancy_using_type(llvm::Type* set_type, llvm::Value* set) = 0;

            virtual
            llvm::Value* get_pointer_to_capacity_using_typecode(std::string& type_code, llvm::Value* set) = 0;

            llvm::Value* get_el_hash(llvm::Value* capacity, llvm::Value* el,
                ASR::ttype_t* el_asr_type, llvm::Module* module);

            virtual
            void resolve_collision_for_write(
                ASR::expr_t* set_expr, llvm::Value* set, llvm::Value* el_hash, llvm::Value* el,
                llvm::Module* module, ASR::ttype_t* el_asr_type) = 0;

            virtual
            void rehash(
                ASR::expr_t* set_expr, llvm::Value* set, llvm::Module* module, ASR::ttype_t* el_asr_type) = 0;

            virtual
            void rehash_all_at_once_if_needed(
                ASR::expr_t* set_expr, llvm::Value* set, llvm::Module* module, ASR::ttype_t* el_asr_type) = 0;

            virtual
            void write_item(
                ASR::expr_t* set_expr, llvm::Value* set, llvm::Value* el,
                llvm::Module* module, ASR::ttype_t* el_asr_type);

            virtual
            void resolve_collision_for_read_with_bound_check(
                llvm::Value* set, llvm::Value* el_hash, llvm::Value* el,
                llvm::Module* module, ASR::ttype_t* el_asr_type) = 0;

            virtual
            void remove_item(
                llvm::Value* set, llvm::Value* el,
                llvm::Module* module, ASR::ttype_t* el_asr_type) = 0;

            virtual
            void set_deepcopy(
                ASR::expr_t* set_expr, llvm::Value* src, llvm::Value* dest,
                ASR::Set_t* set_type, llvm::Module* module) = 0;

            virtual
            llvm::Value* len(llvm::Type* type, llvm::Value* set);

            virtual
            bool is_set_present();

            virtual
            void set_is_set_present(bool value);

            virtual ~LLVMSetInterface() = 0;

    };

    class LLVMSetLinearProbing: public LLVMSetInterface {

        public:

            LLVMSetLinearProbing(
                llvm::LLVMContext& context_,
                LLVMUtils* llvm_utils,
                llvm::IRBuilder<>* builder);

            llvm::Type* get_set_type(
                std::string type_code,
                int32_t type_size, llvm::Type* el_type);

            void set_init(std::string type_code, llvm::Value* set,
                llvm::Module* module, size_t initial_capacity);

            llvm::Value* get_el_list(llvm::Type* type, llvm::Value* set);

            llvm::Value* get_pointer_to_occupancy(llvm::Type* type, llvm::Value* set);

            llvm::Value* get_pointer_to_capacity_using_type(llvm::Type* el_list_type, llvm::Value* set);

            llvm::Value* get_pointer_to_mask(llvm::Type* type, llvm::Value* set);

            llvm::Value* get_pointer_to_occupancy_using_type(llvm::Type* set_type, llvm::Value* set);

            llvm::Value* get_pointer_to_capacity_using_typecode(std::string& type_code, llvm::Value* set);

            void resolve_collision(
                llvm::Value* capacity, llvm::Value* el_hash,
                llvm::Value* el, llvm::Value* el_list,
                llvm::Value* el_mask, llvm::Module* module,
                ASR::ttype_t* el_asr_type, bool for_read=false);

            void resolve_collision_for_write(
                ASR::expr_t* set_expr, llvm::Value* set, llvm::Value* el_hash, llvm::Value* el,
                llvm::Module* module, ASR::ttype_t* el_asr_type);

            void rehash(
                ASR::expr_t* set_expr, llvm::Value* set, llvm::Module* module, ASR::ttype_t* el_asr_type);

            void rehash_all_at_once_if_needed(
                ASR::expr_t* set_expr, llvm::Value* set, llvm::Module* module, ASR::ttype_t* el_asr_type);

            void resolve_collision_for_read_with_bound_check(
                llvm::Value* set, llvm::Value* el_hash, llvm::Value* el,
                llvm::Module* module, ASR::ttype_t* el_asr_type);

            void remove_item(
                llvm::Value* set, llvm::Value* el,
                llvm::Module* module, ASR::ttype_t* el_asr_type);

            void set_deepcopy(
                ASR::expr_t* set_expr, llvm::Value* src, llvm::Value* dest,
                ASR::Set_t* set_type, llvm::Module* module);

            ~LLVMSetLinearProbing();
    };

    class LLVMSetSeparateChaining: public LLVMSetInterface {

        protected:

            std::map<std::string, llvm::Type*> typecode2elstruct;

            llvm::Value* get_pointer_to_number_of_filled_buckets(llvm::Type* type, llvm::Value* set);

            llvm::Value* get_pointer_to_elems(llvm::Type* type, llvm::Value* set);

            llvm::Value* get_pointer_to_rehash_flag(llvm::Type* type, llvm::Value* set);

            void set_init_given_initial_capacity(std::string el_type_code,
                llvm::Value* set, llvm::Module* module, llvm::Value* initial_capacity);

            void resolve_collision(
                llvm::Value* el_hash, llvm::Value* el, llvm::Value* el_linked_list,
                llvm::Type* el_struct_type, llvm::Value* el_mask,
                llvm::Module* module, ASR::ttype_t* el_asr_type);

            void write_el_linked_list(
                ASR::expr_t* set_expr, llvm::Value* el_ll, llvm::Value* set, llvm::Value* capacity,
                ASR::ttype_t* m_el_type, llvm::Module* module);

            void deepcopy_el_linked_list(
                ASR::expr_t* set_expr, llvm::Value* srci, llvm::Value* desti, llvm::Value* dest_elems,
                ASR::Set_t* set_type, llvm::Module* module);

        public:

            LLVMSetSeparateChaining(
                llvm::LLVMContext& context_,
                LLVMUtils* llvm_utils,
                llvm::IRBuilder<>* builder);

            llvm::Type* get_set_type(
                std::string type_code,
                int32_t type_size, llvm::Type* el_type);

            void set_init(std::string type_code, llvm::Value* set,
                llvm::Module* module, size_t initial_capacity);

            llvm::Value* get_el_list(llvm::Type* type, llvm::Value* set);

            llvm::Value* get_pointer_to_occupancy(llvm::Type* type, llvm::Value* set);

            llvm::Value* get_pointer_to_capacity_using_type(llvm::Type* el_list_type, llvm::Value* set);

            llvm::Value* get_pointer_to_mask(llvm::Type* type, llvm::Value* set);


            llvm::Value* get_pointer_to_occupancy_using_type(llvm::Type* set_type, llvm::Value* set);

            llvm::Value* get_pointer_to_capacity_using_typecode(std::string& type_code, llvm::Value* set);

            void resolve_collision_for_write(
                ASR::expr_t* set_expr, llvm::Value* set, llvm::Value* el_hash, llvm::Value* el,
                llvm::Module* module, ASR::ttype_t* el_asr_type);

            void rehash(
                ASR::expr_t* set_expr, llvm::Value* set, llvm::Module* module, ASR::ttype_t* el_asr_type);

            void rehash_all_at_once_if_needed(
                ASR::expr_t* set_expr, llvm::Value* set, llvm::Module* module, ASR::ttype_t* el_asr_type);

            void resolve_collision_for_read_with_bound_check(
                llvm::Value* set, llvm::Value* el_hash, llvm::Value* el,
                llvm::Module* module, ASR::ttype_t* el_asr_type);

            void remove_item(
                llvm::Value* set, llvm::Value* el,
                llvm::Module* module, ASR::ttype_t* el_asr_type);

            void set_deepcopy(
                ASR::expr_t* set_expr, llvm::Value* src, llvm::Value* dest,
                ASR::Set_t* set_type, llvm::Module* module);

            ~LLVMSetSeparateChaining();
    };

} // namespace LCompilers

#endif // LFORTRAN_LLVM_UTILS_H

#ifndef LFORTRAN_LLVM_UTILS_H
#define LFORTRAN_LLVM_UTILS_H

#include "libasr/asr_utils.h"
#include "libasr/assert.h"
#include "libasr/exception.h"
#include <llvm/IR/Value.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <libasr/asr.h>

#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#include <map>
#include <set>
#include <string>
#include <unordered_map>
#include <tuple>

#if LLVM_VERSION_MAJOR >= 11
#    define FIXED_VECTOR_TYPE llvm::FixedVectorType
#else
#    define FIXED_VECTOR_TYPE llvm::VectorType
#endif

static inline void llvm_fn_insert_bb(llvm::Function *fn, llvm::BasicBlock *bb) {
#if LLVM_VERSION_MAJOR >= 16
    fn->insert(fn->end(), bb);
#else
    fn->getBasicBlockList().push_back(bb);
#endif
}

namespace LCompilers {
class ASRToLLVMVisitor;

    // Platform dependent fast unique hash:
    static inline uint64_t get_hash(ASR::asr_t *node)
    {
        return (uint64_t)node;
    }

    // Returns a scope-qualified key for a Struct_t, for use in name2dertype
    // and related maps. E.g., struct "object_t" inside module "mod_a" becomes
    // "mod_a.object_t", preventing collisions between same-named types in
    // different modules. Unlimited polymorphic types (starting with "~") are
    // global sentinels and are never qualified.
    inline std::string get_type_key(ASR::Struct_t* struct_type) {
        std::string name = struct_type->m_name;
        // Skip qualification for internal sentinel types like ~unlimited_polymorphic_type
        if (!name.empty() && name[0] == '~') {
            return name;
        }
        if (struct_type->m_symtab && struct_type->m_symtab->parent &&
                struct_type->m_symtab->parent->asr_owner &&
                ASR::is_a<ASR::symbol_t>(*struct_type->m_symtab->parent->asr_owner)) {
            ASR::symbol_t* parent_sym = ASR::down_cast<ASR::symbol_t>(
                struct_type->m_symtab->parent->asr_owner);
            name = std::string(ASRUtils::symbol_name(parent_sym)) + "." + name;
        }
        return name;
    }

    // Returns a scope-qualified key for a Union_t.
    inline std::string get_type_key(ASR::Union_t* union_type) {
        std::string name = union_type->m_name;
        if (union_type->m_symtab && union_type->m_symtab->parent &&
                union_type->m_symtab->parent->asr_owner &&
                ASR::is_a<ASR::symbol_t>(*union_type->m_symtab->parent->asr_owner)) {
            ASR::symbol_t* parent_sym = ASR::down_cast<ASR::symbol_t>(
                union_type->m_symtab->parent->asr_owner);
            name = std::string(ASRUtils::symbol_name(parent_sym)) + "." + name;
        }
        return name;
    }

    // Returns a scope-qualified key for a symbol_t* that may be Struct or Union.
    // Resolves ExternalSymbol automatically.
    inline std::string get_type_key(ASR::symbol_t* sym) {
        sym = ASRUtils::symbol_get_past_external(sym);
        if (ASR::is_a<ASR::Struct_t>(*sym)) {
            return get_type_key(ASR::down_cast<ASR::Struct_t>(sym));
        } else if (ASR::is_a<ASR::Union_t>(*sym)) {
            return get_type_key(ASR::down_cast<ASR::Union_t>(sym));
        }
        return std::string(ASRUtils::symbol_name(sym));
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
        void set_memory_debug(bool state);
        bool use_memory_debug();
        llvm::Value* lfortran_malloc(llvm::LLVMContext &context, llvm::Module &module,
                llvm::IRBuilder<> &builder, llvm::Value* arg_size);
        llvm::Value* lfortran_malloc_alloc(llvm::LLVMContext &context, llvm::Module &module,
                llvm::IRBuilder<> &builder, llvm::Value* allocator, llvm::Value* arg_size);
        llvm::Value* lfortran_realloc(llvm::LLVMContext &context, llvm::Module &module,
                llvm::IRBuilder<> &builder, llvm::Value* ptr, llvm::Value* arg_size);
        llvm::Value* lfortran_realloc_alloc(llvm::LLVMContext &context, llvm::Module &module,
                llvm::IRBuilder<> &builder, llvm::Value* allocator, llvm::Value* ptr, llvm::Value* arg_size);
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

            llvm::Value* allocate_zeroed_bytes(llvm::Value* size);

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
            llvm::Type* dim_descr_type_; // dimension_descriptor type (used with descriptorArrays)
            llvm::FunctionType* struct_copy_functype;

            // Allocator support: the allocator is an opaque struct pointer
            // passed to runtime functions for compiler-controlled allocation.
            llvm::Value* allocator_instance = nullptr; // cached global allocator ptr

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
            // lfortran_free_nocheck is a variant of lfortran_free that does not check for null pointers before freeing.
            llvm::Value* lfortran_free_nocheck(llvm::Value* ptr);
            llvm::Value* lfortran_free_alloc(llvm::Value* allocator, llvm::Value* ptr);

            // Get or create the cached global allocator pointer
            llvm::Value* get_allocator(llvm::Module* mod);

            llvm::Value* string_format_fortran(const std::vector<llvm::Value*> &args, llvm::Value* decimal_mode=nullptr, llvm::Value* sign_mode=nullptr, llvm::Value* round_mode=nullptr);
            llvm::Value* create_gep2(llvm::Type *t, llvm::Value* ds, llvm::Value* idx);
            llvm::Value* create_gep2(llvm::Type *t, llvm::Value* ds, int idx);

            llvm::Value* create_ptr_gep2(llvm::Type* type, llvm::Value* ptr, int idx);
            llvm::Value* create_ptr_gep2(llvm::Type* type, llvm::Value* ptr, llvm::Value* idx);

            llvm::Value* CreateLoad2(llvm::Type *t, llvm::Value *x, bool is_volatile = false);
            llvm::Value* CreateBitCastForStore(llvm::Value* value, llvm::Value* target_ptr);
            llvm::Value* get_array_descriptor_ptr(llvm::Value* value, llvm::Type* arr_type,
                                                  bool is_character_array);

            llvm::Value* CreateGEP2(llvm::Type *t, llvm::Value *x,
                std::vector<llvm::Value *> &idx);
            llvm::Value* CreateGEP2(llvm::Type *type, llvm::Value *x, int idx);

            
            llvm::Value* CreateInBoundsGEP2(llvm::Type *t  , llvm::Value *x, const std::vector<llvm::Value *> &idx);
            llvm::Value* CreateInBoundsGEP2(ASR::ttype_t *t, llvm::Value *x, const std::vector<llvm::Value *> &idx);

            llvm::AllocaInst* CreateAlloca(llvm::Type* type,
                llvm::Value* size=nullptr, std::string Name="",
                bool is_llvm_ptr=false);
            llvm::AllocaInst* CreateAlloca(llvm::IRBuilder<> &builder,
                llvm::Type* type, llvm::Value* size=nullptr, std::string Name="",
                bool is_llvm_ptr=false);

            llvm::Value* allocate_string_descriptor_on_heap(llvm::Type* string_desc_type);
            void ensure_string_descriptor_on_heap(llvm::Type* array_desc_type, llvm::Value* array_desc,
                llvm::Type* string_desc_type);

            /// Check llvm SSA is matching some type.
            void validate_llvm_SSA([[maybe_unused]] llvm::Type* type_to_check_against, [[maybe_unused]] llvm::Value* llvm_SSA);

            llvm::Type* getIntType(int a_kind, bool get_pointer=false);
            llvm::Function* _Deallocate();

            void start_new_block(llvm::BasicBlock *bb);

            llvm::Value* lfortran_str_cmp(llvm::Value* left_arg, llvm::Value* right_arg,
                                          std::string runtime_func_name, llvm::Module& module);

            /*
            * A Label for runtime error messages
            */
            struct RuntimeLabel {
                bool primary; // primary or secondary label
                std::string message; // format string message
                std::vector<diag::Span> spans; // one or more spans
                std::vector<llvm::Value*> args; // arguments for format string

                RuntimeLabel(const std::string &message, const std::vector<Location> &locations, const std::vector<llvm::Value*> &args = {},
                        bool primary=true) : primary{primary}, message{message}, args{args} {
                    for (auto &loc : locations) {
                        spans.emplace_back(loc);
                    }
                }
            };

            template<typename... Args>
            void generate_runtime_error(llvm::Value* cond, std::string message, std::vector<RuntimeLabel> labels, std::string &infile, LocationManager& lm, Args... args)
            {
                llvm::Function *fn = builder->GetInsertBlock()->getParent();

                llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "then", fn);
                llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "ifcont");

                builder->CreateCondBr(cond, thenBB, mergeBB);
                builder->SetInsertPoint(thenBB); {
                        llvm::Type *span_type = llvm::StructType::get(context, llvm::ArrayRef<llvm::Type *>({
                            llvm::Type::getInt8Ty(context)->getPointerTo(),
                            llvm::Type::getInt32Ty(context),
                            llvm::Type::getInt32Ty(context),
                            llvm::Type::getInt32Ty(context),
                            llvm::Type::getInt32Ty(context),
                        }));
                        llvm::Type *label_type = llvm::StructType::get(context, llvm::ArrayRef<llvm::Type *>({
                            llvm::Type::getInt1Ty(context),
                            llvm::Type::getInt8Ty(context)->getPointerTo(),
                            span_type->getPointerTo(),
                            llvm::Type::getInt32Ty(context),
                        }));
                        llvm::Function* lcompilers_snprintf_fn = module->getFunction("_lcompilers_snprintf_alloc");
                        if (!lcompilers_snprintf_fn) {
                            llvm::FunctionType* snprintf_fn_type = llvm::FunctionType::get(
                                llvm::Type::getInt8Ty(context)->getPointerTo(),
                                {llvm::Type::getInt8Ty(context)->getPointerTo(),
                                 llvm::Type::getInt8Ty(context)->getPointerTo()},
                                true);
                            lcompilers_snprintf_fn = llvm::Function::Create(snprintf_fn_type,
                                llvm::Function::ExternalLinkage, "_lcompilers_snprintf_alloc", module);
                        }

                        // Allocate and populate labels and spans
                        llvm::Type *label_arr_type = llvm::ArrayType::get(label_type, labels.size());
                        llvm::Value *labels_v = builder->CreateAlloca(label_arr_type);
                        for (size_t i = 0; i < labels.size(); i++) {
                            llvm::Value *idx = llvm::ConstantInt::get(context, llvm::APInt(32, i));

                            llvm::Type *span_arr_type = llvm::ArrayType::get(span_type, labels[i].spans.size());
                            llvm::Value *spans_v = builder->CreateAlloca(span_arr_type);
                            for (size_t j = 0; j < labels[i].spans.size(); j++) {
                                llvm::Value *span_idx = llvm::ConstantInt::get(context, llvm::APInt(32, j));
                                llvm::Value *span_j = LLVMUtils::CreateInBoundsGEP2(span_arr_type, spans_v, {llvm::ConstantInt::get(context, llvm::APInt(32, 0)), span_idx});

                                uint32_t start_l, start_c, last_l, last_c;
                                lm.pos_to_linecol(lm.output_to_input_pos(labels[i].spans[j].loc.first, false), start_l, start_c, infile);
                                lm.pos_to_linecol(lm.output_to_input_pos(labels[i].spans[j].loc.last, false), last_l, last_c, infile);

                                builder->CreateStore(LCompilers::create_global_string_ptr(context, *module, *builder, infile),
                                        LLVMUtils::CreateGEP2(span_type, span_j, 0));
                                builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, start_l)),
                                        LLVMUtils::CreateGEP2(span_type, span_j, 1));
                                builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, start_c)),
                                        LLVMUtils::CreateGEP2(span_type, span_j, 2));
                                builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, last_l)),
                                        LLVMUtils::CreateGEP2(span_type, span_j, 3));
                                builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, last_c)),
                                        LLVMUtils::CreateGEP2(span_type, span_j, 4));
                            }

                            std::vector<llvm::Value*> snprintf_args;
                            snprintf_args.push_back(get_allocator(module));
                            snprintf_args.push_back(LCompilers::create_global_string_ptr(context, *module, *builder, labels[i].message));
                            snprintf_args.insert(snprintf_args.end(), labels[i].args.begin(), labels[i].args.end());
                            llvm::Value* formatted_message = builder->CreateCall(lcompilers_snprintf_fn, snprintf_args);

                            llvm::Value *label_i = LLVMUtils::CreateInBoundsGEP2(label_arr_type, labels_v, {llvm::ConstantInt::get(context, llvm::APInt(32, 0)), idx});
                            llvm::Value *span_arr = LLVMUtils::CreateGEP2(span_arr_type, spans_v, 0);
                            llvm::Value *label_spans = LLVMUtils::CreateGEP2(label_type, label_i, 2);
                            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(1, labels[i].primary)),
                                    LLVMUtils::CreateGEP2(label_type, label_i, 0));
                            builder->CreateStore(formatted_message,
                                    LLVMUtils::CreateGEP2(label_type, label_i, 1));
                            builder->CreateStore(span_arr, label_spans);
                            builder->CreateStore(llvm::ConstantInt::get(context, llvm::APInt(32, labels[i].spans.size())),
                                    LLVMUtils::CreateGEP2(label_type, label_i, 3));
                        }


                        llvm::Value* formatted_msg = create_global_string_ptr(context, *module, *builder, message);
                        llvm::Function* print_error_fn = module->getFunction("_lcompilers_runtime_error");
                        if (!print_error_fn) {
                            llvm::FunctionType* error_fn_type = llvm::FunctionType::get(
                                llvm::Type::getVoidTy(context),
                                {llvm::Type::getInt8Ty(context)->getPointerTo(),
                                 label_type->getPointerTo(), llvm::Type::getInt32Ty(context), llvm::Type::getInt8Ty(context)->getPointerTo()},
                                true);
                            print_error_fn = llvm::Function::Create(error_fn_type,
                                llvm::Function::ExternalLinkage, "_lcompilers_runtime_error", module);
                        }

                        std::vector<llvm::Value*> vec = {get_allocator(module), LLVMUtils::CreateGEP2(label_arr_type, labels_v, 0), llvm::ConstantInt::get(context, llvm::APInt(32, labels.size())), formatted_msg, args...};
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

            /**
             * Gets string's consecutive memory data (char*).
             * @param str_type ASR string type node of the string you're operating on.
             * @param str LLVM physical-string. Pass it in one of these forms (`string_descriptor*`, `char*`)
             * @param get_pointer_to_data flag to get a reference to the `char*` (`char**`). 
             * @return an LLVM value of `char*` or `char**` (based on `get_pointer_to_data` flag)
            */

            llvm::Value* get_string_data(ASR::String_t* str_type, llvm::Value* str, bool get_pointer_to_data=false);

            /**
             * Gets string's length
             * @param str_type ASR string type node of the string you're operating on.
             * @param str LLVM physical-string. Pass it in one of these forms (`string_descriptor*`, `char*`)
             * @param get_pointer_to_data flag to get the length `i64` by reference (`i64*`). 
             * @return an LLVM value of `i64` or `i64*` (based on `get_pointer_to_data` flag)
            */
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
                llvm::GlobalValue::LinkageTypes linkage = llvm::GlobalValue::PrivateLinkage,
                std::string data_name = "");
            
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
            /**
             * @brief Gets array element if array of classes
             * @param class_symbol   Symbol of the class. Used to create another class type (llvm) that holds the element + VTable.
             * @param struct_type    Used to do checks, and guarantee proper usage.
             * @param array_data_ptr Plain pointer to array.
            */
            llvm::Value* get_class_element_from_array(ASR::Struct_t* class_symbol, ASR::StructType_t* struct_type, llvm::Value* array_data_ptr, llvm::Value* idx);
            llvm::Value* get_class_type_size_from_vptr(llvm::Value* vptr);
            llvm::Value* get_class_type_tag_from_vptr(llvm::Value* vptr);
            llvm::Value* get_polymorphic_array_data_ptr(llvm::Value* base_ptr, llvm::Value* idx, llvm::Value* vptr);

            // Allocate zero-initialized memory for the given LLVM type.
            // Returns a typed pointer (bitcast of malloc+memset result).
            llvm::Value* alloc_zeroed_type(llvm::Type* type);

            // Extract vptr and data pointer from a ONE-wrapper {vptr, i8*}.
            // Also derives elem_size and copy_fn from the vptr.
            struct UpolyWrapperFields {
                llvm::Value* vptr;
                llvm::Value* data;
                llvm::Value* elem_size;
                llvm::Value* copy_fn;
            };
            UpolyWrapperFields extract_upoly_wrapper(
                llvm::Value* wrapper, llvm::Type* wrapper_type);

            // Initialize an unlimited-polymorphic array wrapper from a
            // mold wrapper: copies vptr, allocates data, and if the mold
            // is a string type, initializes string descriptors.
            void init_mold_upoly_array_data(
                llvm::Value* wrapper, llvm::Value* mold_wrapper,
                llvm::Type* class_type, llvm::Value* num_elements);

            // Initialize string descriptors in a pre-allocated data buffer.
            // Allocates contiguous char data (filled with spaces) and sets
            // each descriptor's pointer and length.
            void init_string_descriptors(llvm::Value* data_mem,
                llvm::Value* num_elements, llvm::Value* str_len);

            // Consolidate per-element string_descriptors into a single
            // string_descriptor with a flat contiguous char buffer.
            llvm::Value* consolidate_char_descriptors(
                llvm::Value* descs_i8, llvm::Value* n_elems_i64);

            // Write back consolidated flat char buffer to original
            // per-element string_descriptors in a polymorphic array.
            void writeback_char_to_polymorphic_descriptors(
                llvm::Value* original_descs_i8, llvm::Value* consolidated_desc,
                llvm::Value* n_elems_i64);

            // Expand a flat char buffer into per-element string_descriptors.
            llvm::Value* expand_flat_to_char_descriptors(
                llvm::Value* flat_data, llvm::Value* char_len,
                llvm::Value* n_elems_i64);
            

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
                ASR::ttype_t* asr_dest_type, ASR::ttype_t* asr_src_type, llvm::Module* module,
                bool use_defined_assignment = false);

            llvm::Value* convert_kind(llvm::Value* val, llvm::Type* target_type);


            // Note: `llvm_utils->create_if_else` and `create_loop` are optional APIs
            // that do not have to be used. Many times, for more complicated
            // things, it might be more readable to just use the LLVM API
            // without any extra layer on top. In some other cases, it might
            // be more readable to use this abstraction.
            // The `if_block` and `else_block` must generate one or more blocks. In
            // addition, the `if_block` must not be terminated, we terminate it
            // ourselves. The `else_block` can be either terminated or not.

            llvm::Value* apply_common_block_alias_cast(llvm::Value* ptr, ASR::expr_t* expr,ASR::ttype_t* expected_type,ASR::ttype_t* actual_type);

            llvm::Value* to_i1(llvm::Value* cond) {
                if (cond->getType()->isIntegerTy(1)) {
                    return cond;
                }
                return builder->CreateICmpNE(
                    cond, llvm::ConstantInt::get(cond->getType(), 0));
            }

            template <typename IF, typename ELSE>
            void create_if_else(llvm::Value * cond, IF if_block, ELSE else_block, const char *name,
                                std::vector<llvm::BasicBlock*> &loop_or_block_end,
                                std::vector<std::string> &loop_or_block_end_names) {
                cond = to_i1(cond);
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
            void create_if_else(llvm::Value *cond, IF if_block, ELSE else_block, const char *name = nullptr) {
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
                    llvm::Value* cond = to_i1(condition());
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

                void free() { // This is runtime lib allocated memory -- Isolate away from compiler's memory debug path
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
     *
     * @see doc/src/llvm_utils.md 
     */
    class LLVMFinalize final {
    private:

        std::unique_ptr<LLVMUtils>                                  &llvm_utils_;
        std::unique_ptr<llvm::IRBuilder<>>                          &builder_;
        Allocator                                                   &al_;
        ASRToLLVMVisitor                                            &asr_to_llvm_visitor_;
        std::map<uint64_t, llvm::Function*>                         &llvm_symtab_fn_;
        std::unordered_map<std::string, llvm::Function*>            type_finalizer_cache_;

    public:
        LLVMFinalize(ASRToLLVMVisitor &asr_to_llvm_visitor,
            std::unique_ptr<LLVMUtils> &llvm_utils, std::unique_ptr<llvm::IRBuilder<>> &builder, Allocator& al,
            std::map<uint64_t, llvm::Function*> &llvm_symtab_fn)  
        :   llvm_utils_(llvm_utils), builder_(builder), al_(al), asr_to_llvm_visitor_(asr_to_llvm_visitor),
            llvm_symtab_fn_(llvm_symtab_fn){}

    private:


        /**
         * Finalizes type by directly dispatching to the corresponding finalizer.
         *
         * @param ptr llvm ptr to the type (instance) we're finalizing.
         * @param t ASR type
         * @param struct_sym Struct symbol that could be related to `t` (if it contains structType), nullptr otherwise.
         * @param in_struct This type (instance) is inside a struct type or not -- Useful for arrays.
         */
        void finalize(llvm::Value* const ptr, ASR::ttype_t* const t, ASR::Struct_t* const struct_sym, const bool in_struct){
            if(!is_finalizable_type(t, struct_sym, in_struct)) { return; }
            
            if(ASRUtils::is_allocatable(t)){
                finalize_allocatable(ptr, t, struct_sym, in_struct);
            } else if(ASRUtils::is_pointer(t)) {
                finalize_pointer(ptr, t, struct_sym);
            } else {
                finalize_type(ptr, t, struct_sym);
            }
        }

        void finalize_variable(ASR::Variable_t* const v){
            if(not_finalizable_variable(v)) return;
            if(!is_finalizable_type(v->m_type, get_struct_sym(v), false)) return;
            LCOMPILERS_ASSERT_MSG(!is_struct_symtab(v->m_parent_symtab), "Struct members don't use this function")

            insert_BB_for_readability((std::string("Finalize_Variable_") + v->m_name).c_str());

            auto const llvm_var = get_llvm_var(v);
            auto* const struct_sym = get_struct_sym(v);
            check_userDefinedFinalizer_then_finalize(llvm_var, v->m_type, struct_sym, false);

            // For non-allocatable DescriptorArray of strings, the cached
            // finalize function only frees the character data but not the
            // heap-allocated string_descriptor array itself.  We must free
            // it here.  (Allocatable arrays use a stack-allocated initial
            // string_descriptor, so this only applies to non-allocatable.)
            if (!ASRUtils::is_allocatable(v->m_type) && !ASRUtils::is_pointer(v->m_type)) {
                ASR::ttype_t* t_past = ASRUtils::type_get_past_allocatable_pointer(v->m_type);
                if (ASRUtils::is_array_t(t_past)) {
                    ASR::Array_t* arr_t = ASR::down_cast<ASR::Array_t>(t_past);
                    if (arr_t->m_physical_type == ASR::array_physical_typeType::DescriptorArray
                        && ASRUtils::extract_type(t_past)->type == ASR::ttypeType::String) {
                        llvm::Type* arr_llvm_t = get_llvm_type(t_past, struct_sym);
                        llvm::Type* elem_llvm_t = get_llvm_type(arr_t->m_type, struct_sym);
                        llvm::Value* data = builder_->CreateLoad(
                            elem_llvm_t->getPointerTo(),
                            llvm_utils_->create_gep2(arr_llvm_t, llvm_var, 0));
                        llvm_utils_->lfortran_free_nocheck(data);
                    }
                }
            }
        }

        void check_userDefinedFinalizer_then_finalize(llvm::Value* ptr, ASR::ttype_t* type, ASR::Struct_t* struct_sym, bool in_struct){
            // Call user-defined FINAL procedures for non-allocatable struct
            // locals at scope exit (Fortran 2018 §7.5.6.3).
            // Allocatable types are handled by the deallocate path.
            if (struct_sym != nullptr
                    && !ASRUtils::is_allocatable(type)
                    && struct_sym->n_member_functions > 0) {
                for (size_t fi = 0; fi < struct_sym->n_member_functions; fi++) {
                    std::string final_proc_name = struct_sym->m_member_functions[fi];
                    ASR::symbol_t* final_sym = struct_sym->m_symtab->parent->get_symbol(final_proc_name);
                    if (final_sym) {
                        final_sym = ASRUtils::symbol_get_past_external(final_sym);
                        uint32_t fh = get_hash((ASR::asr_t*)final_sym);
                        if (llvm_symtab_fn_.find(fh) != llvm_symtab_fn_.end()) {
                            llvm::Function* final_fn = llvm_symtab_fn_[fh];
                            ASR::ttype_t* v_type_past = ASRUtils::type_get_past_allocatable(type);
                            if (ASR::is_a<ASR::Array_t>(*v_type_past)) {
                                // Variable is an array but the final subroutine
                                // takes a scalar — call it element-by-element.
                                ASR::Array_t* arr_t = ASR::down_cast<ASR::Array_t>(v_type_past);
                                llvm::Type* elem_llvm_type = get_llvm_type(arr_t->m_type, struct_sym);
                                llvm::Value* data_ptr = builder_->CreateBitCast(
                                    ptr, elem_llvm_type->getPointerTo());
                                int64_t array_size = ASRUtils::get_fixed_size_of_array(type);
                                auto iter_type = llvm::Type::getInt64Ty(builder_->getContext());
                                auto* iter = builder_->CreateAlloca(iter_type, nullptr, "final_iter");
                                builder_->CreateStore(
                                    llvm::ConstantInt::get(iter_type, -1, true), iter);
                                auto cond_fn = [&]() {
                                    auto* loaded = builder_->CreateLoad(iter_type, iter);
                                    auto* next = builder_->CreateAdd(loaded,
                                        llvm::ConstantInt::get(iter_type, 1));
                                    builder_->CreateStore(next, iter);
                                    return builder_->CreateICmpSLT(next,
                                        llvm::ConstantInt::get(iter_type, array_size));
                                };
                                auto body_fn = [&]() {
                                    auto* idx = builder_->CreateLoad(iter_type, iter);
                                    auto* elem = llvm_utils_->create_ptr_gep2(
                                        elem_llvm_type, data_ptr, idx);
                                    builder_->CreateCall(final_fn, {elem});
                                };
                                llvm_utils_->create_loop("Final_array_elems", cond_fn, body_fn);
                            } else {
                                builder_->CreateCall(final_fn, {ptr});
                            }
                        }
                    }
                }
            }

            finalize(ptr, type, struct_sym, in_struct);
        }
        
        void finalize_allocatable(llvm::Value* ptr, ASR::ttype_t* const t, ASR::Struct_t* const struct_sym,const bool in_struct){
            LCOMPILERS_ASSERT_MSG(ASRUtils::is_allocatable(t), "Must be allocatable.")
            auto const t_past = ASRUtils::type_get_past_allocatable(t);
            switch (t_past->type) {
                case ASR::StructType:
                case ASR::Array:{
                    std::string cache_key = get_type_key(t, struct_sym) + (in_struct ? "__in_struct":"");
                    if(is_cached(cache_key)) {
                        call_cached_finalizer(cache_key, {ptr});
                        return;
                    }
                    auto const checkPoint_BB = 
                    START_CACHE(cache_key, ptr);
                    check_if_allocated_then_finalize(ptr, t, struct_sym, [&]() { 
                        if (struct_sym != nullptr && ASRUtils::is_class_type(t_past)
                                && ASRUtils::is_unlimited_polymorphic_type(struct_sym)) {
                            // Keep unlimited polymorphic cleanup scoped to the
                            // allocatable deallocation flow to avoid finalizing
                            // aliased/non-owning class(*) values.
                            llvm::Type* const derived_llvm_type = get_llvm_type(t_past, struct_sym);
                            auto const vptr = llvm_utils_->CreateLoad2(
                                llvm_utils_->vptr_type,
                                llvm_utils_->create_gep2(derived_llvm_type, ptr, 0));
                            auto const data_ptr = llvm_utils_->CreateLoad2(
                                llvm_utils_->i8_ptr,
                                llvm_utils_->create_gep2(derived_llvm_type, ptr, 1));
                            auto const data_not_null = builder_->CreateICmpNE(
                                data_ptr,
                                llvm::ConstantPointerNull::get(llvm_utils_->i8_ptr));
                            llvm_utils_->create_if_else(data_not_null, [this, vptr, data_ptr]() {
                                auto const type_tag = llvm_utils_->get_class_type_tag_from_vptr(vptr);
                                auto const is_string = builder_->CreateICmpEQ(
                                    type_tag,
                                    llvm::ConstantInt::get(llvm::Type::getInt32Ty(builder_->getContext()), 5));
                                llvm_utils_->create_if_else(is_string, [this, data_ptr]() {
                                    auto const str_desc = builder_->CreateBitCast(
                                        data_ptr, llvm_utils_->string_descriptor->getPointerTo());
                                    auto const char_ptr = llvm_utils_->CreateLoad2(
                                        llvm_utils_->character_type,
                                        llvm_utils_->create_gep2(llvm_utils_->string_descriptor, str_desc, 0));
                                    auto const char_not_null = builder_->CreateICmpNE(
                                        char_ptr,
                                        llvm::ConstantPointerNull::get(llvm_utils_->character_type));
                                    llvm_utils_->create_if_else(char_not_null,
                                        [this, char_ptr]() { llvm_utils_->lfortran_free_nocheck(char_ptr); },
                                        [](){});
                                }, [](){});
                                llvm_utils_->lfortran_free_nocheck(data_ptr);
                            }, [](){});
                        } else {
                            finalize(ptr, t_past, struct_sym, in_struct);
                        }
                        free_allocatable_ptr(ptr, t, struct_sym, in_struct);
                        
                    });
                    END_CACHE(checkPoint_BB);
                }
                break;
                default:
                    finalize(ptr, t_past, struct_sym, in_struct);
                    free_allocatable_ptr(ptr, t, struct_sym, in_struct);
                break;
            }
        }

        void finalize_pointer(llvm::Value* ptr, ASR::ttype_t* const t, [[maybe_unused]] ASR::Struct_t* const struct_sym){
            LCOMPILERS_ASSERT_MSG(ASRUtils::is_pointer(t), "Must be finalizable pointer.")
            auto const t_past = ASRUtils::type_get_past_pointer(t);
            switch (t_past->type) {
                case ASR::Array:
                    llvm_utils_->lfortran_free_nocheck(ptr);
                break;
                case ASR::StructType:
                    if(ASRUtils::is_class_type(t_past)){
                        check_if_allocated_then_finalize(ptr, t, struct_sym, [this, ptr](){
                            llvm_utils_->lfortran_free_nocheck(ptr);
                        });
                    }
                break;
                default:
                    throw LCompilersException("Unhandled Case.");
            }
        }

        /// Frees pointer to allocatable type ( e.g `i32*`, `{i64, i8}*` )
        void free_allocatable_ptr(llvm::Value* const var_ptr, ASR::ttype_t* const t, ASR::Struct_t* const struct_sym, const bool in_struct){
            LCOMPILERS_ASSERT(ASRUtils::is_allocatable(t))
            auto const t_past = ASRUtils::type_get_past_allocatable_pointer(t);
            switch (t_past->type) {
                case(ASR::StructType) :  {
                    if (struct_sym != nullptr && ASRUtils::is_class_type(t_past)
                            && ASRUtils::is_unlimited_polymorphic_type(struct_sym)) {
                        // Unlimited polymorphic class payload is already finalized
                        // (and freed) in finalize_struct(). Keep only wrapper free
                        // below to avoid double-free.
                    } else if (struct_sym != nullptr && ASRUtils::is_class_type(t_past)
                            && !ASRUtils::is_unlimited_polymorphic_type(struct_sym)) {
                        // LLVM 15+ uses opaque pointers, so wrapper-vs-struct must
                        // be decided from the ASR class flag, not the LLVM pointer type.
                        // Use t_past (bare StructType) for GEP since var_ptr already
                        // points to the class wrapper struct {vtable*, struct*}.
                        llvm::Type* const derived_llvm_type = get_llvm_type(t_past, struct_sym);
                        auto const struct_ptr = llvm_utils_->CreateLoad2(
                            llvm_utils_->getStructType(struct_sym, llvm_utils_->module, true),
                            llvm_utils_->create_gep2(derived_llvm_type, var_ptr, 1));
                        check_if_allocated_then_finalize(struct_ptr, struct_sym->m_struct_signature, struct_sym,
                            [this, struct_ptr](){llvm_utils_->lfortran_free_nocheck(struct_ptr);});
                    } else if(ASRUtils::is_class_type(t_past)){ // {VTable*, struct*} -- Free struct
                        llvm::Type* const derived_llvm_type = get_llvm_type(t_past, struct_sym);
                        auto const struct_ptr = llvm_utils_->CreateLoad2(
                            llvm_utils_->getStructType(struct_sym, llvm_utils_->module, true),
                            llvm_utils_->create_gep2(derived_llvm_type, var_ptr, 1));
                        check_if_allocated_then_finalize(struct_ptr, struct_sym->m_struct_signature, struct_sym,
                            [this, struct_ptr](){llvm_utils_->lfortran_free_nocheck(struct_ptr);});
                    }
                    llvm_utils_->lfortran_free_nocheck(var_ptr);
                }
                break;
                case(ASR::Array) : {
                    // Free based on array physical type + in_struct or not.
                    auto const arr_physical_t = ASRUtils::extract_physical_type(t_past);
                    bool const need_free = ( arr_physical_t == ASR::DescriptorArray
                                              || arr_physical_t == ASR::PointerArray) && in_struct;
                    if(need_free) {
                        // Narrow case: allocatable descriptor-array of deferred-
                        // length strings can leave its descriptor backing store
                        // allocated. Free that backing store before wrapper free.
                        if (arr_physical_t == ASR::DescriptorArray
                                && ASRUtils::extract_type(t_past)->type == ASR::String) {
                            auto* const str_t = ASR::down_cast<ASR::String_t>(ASRUtils::extract_type(t_past));
                            if (str_t->m_len == nullptr) {
                                auto* const arr_t = ASR::down_cast<ASR::Array_t>(t_past);
                                llvm::Type* const arr_llvm_t = get_llvm_type(t_past, struct_sym);
                                llvm::Type* const elem_llvm_t = get_llvm_type(arr_t->m_type, struct_sym);
                                auto* const data_ptr = builder_->CreateLoad(
                                    elem_llvm_t->getPointerTo(),
                                    llvm_utils_->create_gep2(arr_llvm_t, var_ptr, 0));
                                auto* const data_not_null = builder_->CreateICmpNE(
                                    data_ptr,
                                    llvm::ConstantPointerNull::get(
                                        llvm::cast<llvm::PointerType>(elem_llvm_t->getPointerTo())));
                                llvm_utils_->create_if_else(data_not_null,
                                    [this, data_ptr]() { llvm_utils_->lfortran_free_nocheck(data_ptr); },
                                    [](){});
                            }
                        }
                        llvm_utils_->lfortran_free_nocheck(var_ptr);
                    }
                }
                break;
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
                    llvm_utils_->lfortran_free_nocheck(var_ptr);
                break;
                case(ASR::FunctionType):
                case(ASR::CPtr):
                case(ASR::String):
                // Do nothing
                break;
                default: 
                    throw LCompilersException("Unhandled Type.");
            }

        }

        /// Dispatches to the correct finalizer based on type.
        void finalize_type(llvm::Value* const var_ptr, ASR::ttype_t* const t, ASR::Struct_t* const struct_sym){
            LCOMPILERS_ASSERT(!ASRUtils::is_allocatable_or_pointer(t))
            switch (t->type) {
                case(ASR::String):
                    finalize_string(var_ptr, t);
                break;
                case(ASR::Array) :  
                    finalize_array(var_ptr, t, struct_sym);
                break;
                case(ASR::StructType) :  
                    finalize_struct(var_ptr, t, struct_sym);
                break;
                case(ASR::List):
                    finalize_list(var_ptr, t, struct_sym);
                break;
                case(ASR::Dict):
                    finalize_dict(var_ptr, t, struct_sym);
                break;
                case(ASR::Tuple):
                    finalize_tuple(var_ptr, t, struct_sym);
                break;
                case(ASR::UnionType):
                    finalize_union(var_ptr, t, struct_sym);
                break;
                case(ASR::Set):
                    finalize_set(var_ptr, t, struct_sym);
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
            ASR::ttype_t* const  type_past = ASRUtils::type_get_past_allocatable_pointer(t);
            ASR::String_t* const str_t = ASR::down_cast<ASR::String_t>(type_past);

            verify(str, get_llvm_type(t, nullptr)->getPointerTo());
            
            /* Free */
            switch(str_t->m_physical_type){
                case ASR::DescriptorString: { // Operates on ` { i8*, i64 }* `
                    llvm::Value* const ptr_to_I8_ptr = llvm_utils_->create_gep2(llvm_utils_->string_descriptor, str, 0);
                    llvm_utils_->lfortran_free_nocheck(llvm_utils_->CreateLoad2(llvm_utils_->character_type, ptr_to_I8_ptr));
                break;
                }
                case ASR::CChar:{ // Operates on ` i8** `
                    llvm_utils_->lfortran_free_nocheck(llvm_utils_->CreateLoad2(llvm_utils_->character_type, str));
                break;
                }
                default:
                    throw LCompilersException("Unhandled");
                break;
            }
        }

        /**
         * @param arr llvm ptr to the array (descriptorArray, PointerArray, etc.)
         * @param t array ASR type
         * @param struct_sym if it's an array of struct. nullptr otherwise.
         */
        void finalize_array(llvm::Value* arr, ASR::ttype_t* const t, ASR::Struct_t* const struct_sym){
            auto *const arr_t            = ASR::down_cast<ASR::Array_t>(ASRUtils::type_get_past_allocatable_pointer(t));
            auto *const arr_llvm_t       = get_llvm_type(t, struct_sym);
            auto *const arrayType_llvm_t = get_llvm_type(arr_t->m_type, struct_sym);
            llvm::Type* array_data_ptr_type = arrayType_llvm_t->getPointerTo();
            auto  const array_size_lazy  = [&]() { 
                insert_BB_for_readability("Calculate_arraySize");
                return llvm_utils_->get_array_size(arr, get_llvm_type(t, struct_sym), t, &asr_to_llvm_visitor_);
            };

            switch(arr_t->m_physical_type){
                case ASR::DescriptorArray : { // e.g. `{ {i32, i64*}*, i32, %dimension_descriptor*, i1, i32 }`
                    std::string const cache_key = "descriptorArray_"+get_type_key(t, struct_sym);
                    if(is_cached(cache_key)){
                        call_cached_finalizer(cache_key, {arr});
                        return;
                    }
                    auto const checkpoint_BB =
                    START_CACHE(cache_key, arr);
                    verify(arr, get_llvm_type(&arr_t->base, struct_sym)->getPointerTo());
                    auto const data = builder_->CreateLoad(array_data_ptr_type, 
                                                            llvm_utils_->create_gep2(arr_llvm_t, arr, 0));
                    if(arr_t->m_type->type == ASR::StructType){
                        check_if_allocated_then_finalize(data, arr_t->m_type, struct_sym,[&](){
                            free_array_data(data, arr_t->m_type, struct_sym, array_size_lazy);});
                    } else {
                        free_array_data(data, arr_t->m_type, struct_sym, array_size_lazy);
                    }

                    free_array_ptr_to_consecutive_data(data, arr_t->m_type);
                    END_CACHE(checkpoint_BB);
                break;
                }
                case ASR::PointerArray :{
                    auto const llvm_type_verify_against = ASRUtils::is_array_of_strings(&arr_t->base) ? 
                                                          get_llvm_type(&arr_t->base, struct_sym)->getPointerTo() :
                                                          get_llvm_type(&arr_t->base, struct_sym);
                    verify(arr, llvm_type_verify_against);
                    auto const data = arr;
                    free_array_data(data, arr_t->m_type, struct_sym, array_size_lazy);
                    free_array_ptr_to_consecutive_data(data, arr_t->m_type);
                    break;
                }
                case ASR::SIMDArray :
                case ASR::FixedSizeArray :{
                    verify(arr, get_llvm_type(&arr_t->base, struct_sym)->getPointerTo());
                    auto const data = builder_->CreateBitCast(arr, array_data_ptr_type);
                    free_array_data(data, arr_t->m_type, struct_sym, array_size_lazy);
                }
                break;
                default :
                    throw LCompilersException("NOT HANDLED : Handle this case!");
                break;
            }
        }

        void finalize_scalar(llvm::Value* const ptr, ASR::ttype_t* const t, ASR::Struct_t* const struct_sym){
            // Do nothing -- Scalars don't have internals to be finalized.
            (void)ptr; (void)t; (void) struct_sym;
        }

        void finalize_struct(llvm::Value* ptr, ASR::ttype_t* const t, ASR::Struct_t* const struct_sym){
            verify(ptr, get_llvm_type(t, struct_sym)->getPointerTo());
            const std::string cache_key = get_type_key(t, struct_sym);
            if(is_cached(cache_key)){
                builder_->CreateCall(type_finalizer_cache_[cache_key], {ptr});
                return;
            }

            const auto checkPoint_BB = 
            START_CACHE(cache_key, ptr);

            if (ASRUtils::is_class_type(t) &&
                    !ASRUtils::is_unlimited_polymorphic_type(struct_sym)) {
                llvm::Type* const derived_llvm_type = get_llvm_type(t, struct_sym);
                ptr = llvm_utils_->CreateLoad2(
                    llvm_utils_->getStructType(struct_sym, llvm_utils_->module, true),
                    llvm_utils_->create_gep2(derived_llvm_type, ptr, 1));
            } else if (ASRUtils::is_class_type(t)) {
                // {VTable*, struct*} -- Fetch struct
                llvm::Type* const derived_llvm_type = get_llvm_type(t, struct_sym);
                ptr = llvm_utils_->CreateLoad2(
                    llvm_utils_->getStructType(struct_sym, llvm_utils_->module, true),
                    llvm_utils_->create_gep2(derived_llvm_type, ptr, 1));
            }

            if (ASRUtils::is_class_type(t)) {
                // Guard: if the inner struct pointer is null (e.g. class wrapper
                // was only default-initialized for an absent optional argument),
                // skip member finalization to avoid dereferencing a null pointer.
                llvm::BasicBlock* finalize_bb = llvm::BasicBlock::Create(
                    builder_->getContext(), "class_ptr_valid",
                    builder_->GetInsertBlock()->getParent());
                llvm::BasicBlock* ret_bb = llvm::BasicBlock::Create(
                    builder_->getContext(), "class_ptr_null",
                    builder_->GetInsertBlock()->getParent());
                llvm::Value* is_null = builder_->CreateICmpEQ(ptr,
                    llvm::ConstantPointerNull::get(
                        llvm::cast<llvm::PointerType>(ptr->getType())));
                builder_->CreateCondBr(is_null, ret_bb, finalize_bb);
                builder_->SetInsertPoint(ret_bb);
                builder_->CreateRetVoid();
                builder_->SetInsertPoint(finalize_bb);
            }

            // Finalize members
            bool is_bindc = (struct_sym->m_abi == ASR::abiType::BindC);
            for (int i = 0; i < (int)struct_sym->n_members; i++){
                auto const member_variable =  ASR::down_cast<ASR::Variable_t>(struct_sym->m_symtab->get_symbol(struct_sym->m_members[i]));
                // bind(C) struct: non-pointer character members are inline i8, nothing to free
                if(is_bindc &&
                   !ASR::is_a<ASR::Allocatable_t>(*member_variable->m_type) &&
                   ASR::is_a<ASR::String_t>(*ASRUtils::type_get_past_array(member_variable->m_type))) { continue; }
                if(!is_finalizable_type(member_variable->m_type, struct_sym, true)){continue;} 


                auto const BB_str_label = std::string("Finalize_struct_") + struct_sym->m_name + "'s_"
                                        + member_variable->m_name +"_member";
                insert_BB_for_readability(BB_str_label.c_str());

                llvm::Value* const member_ptr = get_ptr_to_struct_variable_member(ptr, struct_sym, i);
                auto const member_asr_type = member_variable->m_type;
                auto* member_struct_sym = get_struct_sym(member_variable);
                check_userDefinedFinalizer_then_finalize(member_ptr, member_asr_type, member_struct_sym, true);
            }

            // Finalize Parent
            if(struct_sym->m_parent){
                ASR::Struct_t* const parent_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(struct_sym->m_parent));
                if(is_finalizable_type(parent_struct->m_struct_signature, parent_struct, false)) {
                    insert_BB_for_readability((std::string("Finalize_parent_struct_\"") + parent_struct->m_name + "\"").c_str());
                    llvm::Value* const parent_ptr = llvm_utils_->create_gep2(
                        llvm_utils_->getStructType(struct_sym, llvm_utils_->module), ptr, 0);
                        finalize(parent_ptr, parent_struct->m_struct_signature, parent_struct, true);
                }
                /// Parent is inlined -- Not allocated separately.
            }
            END_CACHE(checkPoint_BB);
        }

        void finalize_list(llvm::Value* const ptr, ASR::ttype_t* const t, ASR::Struct_t* const struct_sym){
            // >>>>> TO DO <<<<<
            // Verify
            // Loop on list -- Create a function to finalize each element.
            // Free the ptr holding the consecutive data.
            (void)ptr; (void)t; (void) struct_sym;
        }

        void finalize_dict(llvm::Value* const ptr, ASR::ttype_t* const t, ASR::Struct_t* const struct_sym){
            // >>>>> TO DO <<<<<
            // Verify
            // Loop on dictionary -- Create a function to finalize each element.
            // Free the ptr holding the consecutive data.
            (void)ptr; (void)t; (void) struct_sym;
        }
        
        void finalize_set(llvm::Value* const ptr, ASR::ttype_t* const t, ASR::Struct_t* const struct_sym){
            // >>>>> TO DO <<<<<
            // Verify
            // Loop on set -- Create a function to finalize each element.
            // Free the ptr holding the consecutive data.
            (void)ptr; (void)t; (void) struct_sym;
        }

        void finalize_tuple(llvm::Value* const ptr, ASR::ttype_t* const t, ASR::Struct_t* const struct_sym){
            // >>>>> TO DO <<<<<
            // Verify
            // Loop on tuple -- Create a function to finalize each element within (more like a struct).
            // Free the ptr holding the consecutive data.
            (void)ptr; (void)t; (void) struct_sym;
        }

        void finalize_union(llvm::Value* const ptr, ASR::ttype_t* const t, ASR::Struct_t* const struct_sym){
            // >>>>> TO DO <<<<<
            // Verify
            // Loop on union -- Create a function to finalize each element within (more like a struct).
            // Free the ptr holding the consecutive data.
            (void)ptr; (void)t; (void) struct_sym;
        }
        
/*>>>>>>>>>>>>>>>>>>>>> Array Finalization Utilities <<<<<<<<<<<<<<<<<<<<<<< */

        /**
         * @brief Handles the process of freeing each and every struct in an array.
         * @details Create a loop on `array_size` to fetch each struct, then call finalize_struct on it.
         *          It also handles the special memory allocation of Class type.
         *
         * @param data_ptr   should be a pointer to array's data (e.g. `{i32, i64}*`)
         * @param struct_t   should be the underlying ASR struct type of the array.
         * @param struct_sym current struct_sym if the array is of struct type.
         * @param array_size is the size of the array.
         *
         */ 
        void free_array_structs(llvm::Value* const data_ptr, ASR::StructType_t* const struct_t, ASR::Struct_t* const struct_sym, llvm::Value* array_size){
            auto const iter_llvm_type =llvm::Type::getInt64Ty(builder_->getContext());
            auto const iter = builder_->CreateAlloca(iter_llvm_type, nullptr, "arrSize_iter");
            builder_->CreateStore(llvm::ConstantInt::get(iter_llvm_type, -1 , true), iter);

            auto const cond_fn = [&](){ // while(++arrSize_iter < array_size)
                auto const loaded_iter = builder_->CreateLoad(iter_llvm_type, iter);
                auto const loaded_iter_incr = builder_->CreateAdd(loaded_iter, llvm::ConstantInt::get(iter_llvm_type, 1)); // arrSize_iter + 1
                builder_->CreateStore(loaded_iter_incr, iter);
                return builder_->CreateICmpSLT(loaded_iter_incr, array_size);
            };

            bool is_class_type = ASRUtils::non_unlimited_polymorphic_class(&struct_t->base);
            
            auto const body_fn = [&]() -> void {
                auto const loaded_iter = builder_->CreateLoad(iter_llvm_type, iter);
                llvm::Value* struct_element = nullptr;
                
                if (is_class_type) {
                    // For class arrays: ONE wrapper with data_ptr pointing to array of underlying structs
                    struct_element = llvm_utils_->get_class_element_from_array(struct_sym, struct_t, data_ptr, loaded_iter);
                } else {
                    auto const struct_type_llvm = get_llvm_type(&struct_t->base, struct_sym);
                    struct_element = llvm_utils_->create_ptr_gep2(struct_type_llvm, data_ptr, loaded_iter);
                }
                finalize(struct_element, &struct_t->base, struct_sym, false);
            };
            
            llvm_utils_->create_loop("Finalize_array_of_structs", cond_fn , body_fn);

            // Free consecutive structs inserted into array's single class structure `{VTable*, underlying_struct*}
            if(is_class_type){
                auto const class_type_llvm = llvm_utils_->getClassType(struct_sym);
                auto const consecutive_field_ptr = llvm_utils_->CreateGEP2(class_type_llvm, data_ptr, 1);
                auto const consecutive_field_type = class_type_llvm->getStructElementType(1);
                auto const allocated_cosecutive_structs = builder_->CreateLoad(consecutive_field_type, consecutive_field_ptr);
                llvm_utils_->lfortran_free_nocheck(allocated_cosecutive_structs);
                // deallocate class wrapper 
                llvm_utils_->lfortran_free_nocheck(data_ptr);
            }
        }
            
        /**
         * @details It's responsbile of deallocating each element within the array;
         * It's main job is to properly free aggregate types (e.g. struct, class, array of strings, etc.).
         *
         * You should expect a loop and a call to type finalizer.
         * Example : finalize each {i32, i64*} within the array.
         * Notice: it's a utility for `finalize_array`, not meant to be used by other finalizers.
         *
         * @param data_ptr  should be a pointer to array's data (e.g. `i32*` OR `{i64, f32}*`)
         * @param data_type should be the underlying ASR type of the array.
         * @param array_size is lambda object returning array's size. Lazy evaluate as only array of structs requires looping on each element (for now).
         */
        template<typename LazyEval>
        void free_array_data(llvm::Value* data_ptr, ASR::ttype_t* const data_type, ASR::Struct_t* struct_sym, LazyEval &array_size){
            LCOMPILERS_ASSERT(!ASRUtils::is_allocatable_or_pointer(data_type))
            llvm::Type* expected_data_ptr_type = get_llvm_type(data_type, struct_sym)->getPointerTo();
            verify(data_ptr, expected_data_ptr_type);
            switch(data_type->type){
                case ASR::StructType :{ // Loop and free
                    const std::string cache_key = "array_data_"+get_type_key(data_type, struct_sym);
                    llvm::Value* arr_size  = array_size();
                    if(is_cached(cache_key)){
                        builder_->CreateCall(type_finalizer_cache_[cache_key], {data_ptr, arr_size});
                        return;
                    }
                    auto const checkPoint_BB =
                    START_CACHE("array_data_"+get_type_key(data_type, struct_sym), data_ptr, arr_size);
                    free_array_structs(data_ptr, ASR::down_cast<ASR::StructType_t>(data_type), struct_sym, arr_size);
                    END_CACHE(checkPoint_BB);
                }
                break;
                case ASR::String : // Force string finalization on this single string. -- Don't loop, One string holds all.
                    finalize(data_ptr, data_type, struct_sym, false); 
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
            } else if (ASRUtils::non_unlimited_polymorphic_class(t)){
                // Class => {VTable* , underlying_struct*}
                // Array of non polymorphic classes is speical handled.
                // Array doesn't allocate consecutive classes, It allocates only one (on stack)
                // and inserts consecutive structs (heap).
                // `free_array_data` handles that clean up.
                return;
            }
            
            llvm_utils_->lfortran_free_nocheck(ptr);
        }

/*>>>>>>>>>>>>>>>>>>>>> Utilities <<<<<<<<<<<<<<<<<<<<<<< */

        // Returns LLVM `i1` constant
        llvm::Value* get_bool_constant(bool const b) {
            return llvm::ConstantInt::get(llvm::Type::getInt1Ty(builder_->getContext()), b);
        }


        // Get a unique string key for finalizable ASR types
        std::string get_type_key(ASR::ttype_t* const t, ASR::Struct_t* const struct_sym) {
            std::string key {};
            if(ASRUtils::is_allocatable(t)) {key += "allocatable__";}
            if(ASRUtils::is_pointer(t)) {key += "pointer__";}
            ASR::ttype_t* const t_past = ASRUtils::type_get_past_allocatable_pointer(t);
            if(ASRUtils::is_array_t(t_past)){
                key += "Array_";
                int n_dims = ASRUtils::extract_n_dims_from_ttype(t_past);
                key += std::to_string(n_dims) + "_";
                key += get_type_key(ASRUtils::extract_type(t_past), struct_sym);
            } else if(struct_sym != nullptr) { // StructType or structType Class
                key += ASRUtils::get_type_code(t_past, false, false, false) +"__" + struct_sym->m_name;
                if(auto module = ASRUtils::get_sym_module(&struct_sym->base)) {
                    key += "_of_"; 
                    key += module->m_name;
                }
            } else {
                key += ASRUtils::get_type_code(t_past, false, false, false);
            }
            return key; 
        }

        bool is_cached(const std::string& cache_key) {
            return type_finalizer_cache_.find(cache_key) != type_finalizer_cache_.end();
        }

        /**
         * Starts CACHE mode for the following set of instructions until `END_CACHE` is called.
         * In CACHE mode, we create a finalizer function for the type we're finalizing,
         * 
         * How to use ?
         * - Pass a unqiue cache for the following set of instructions (mainly tagged by the type we're finalizing)
         * - Pass the signature of the finalizer function. Those are caught by reference, We direct them to the parameters of the finalizer function.
         * - Use returned BasicBlock as the revert point to jump back to after finishing the finalizer function.
         */
        template <typename... SignatureArgs>
        llvm::BasicBlock* START_CACHE(const std::string &cache_key, SignatureArgs&&... signature_args) {
            static_assert((std::is_same_v<llvm::Value*, std::decay_t<SignatureArgs>> && ...));

            LCOMPILERS_ASSERT_MSG(type_finalizer_cache_.find(cache_key) == type_finalizer_cache_.end(),
                                "Cache already exists, Please use it.")

            std::vector<llvm::Value**> args {&signature_args...};

            // Fetch function signature
            std::vector<llvm::Type*> arg_types;
            for (llvm::Value** arg: args) {
                LCOMPILERS_ASSERT(*arg)
                arg_types.push_back((*arg)->getType());
            }

            auto *const finalizer_fn_type = llvm::FunctionType::get(
                llvm::Type::getVoidTy(builder_->getContext()),
                arg_types, false);
            auto *const finalizer_fn = llvm::Function::Create(finalizer_fn_type,
                llvm::Function::InternalLinkage, "finalize_"+cache_key, llvm_utils_->module);

            type_finalizer_cache_[cache_key] = finalizer_fn;
            builder_->CreateCall(finalizer_fn, {signature_args...}); // Insert call to the finalizer in the current block
            
            llvm::BasicBlock *const saved_BB = builder_->GetInsertBlock();
            LCOMPILERS_ASSERT(saved_BB)
            // We don't jump between insert points, No need to save insertpoint.

            llvm::BasicBlock *const entry = llvm::BasicBlock::Create(
                builder_->getContext(), "entry", finalizer_fn);
            builder_->SetInsertPoint(entry);
            
            {// Direct passed arg to function's args
            int i = 0;
            for(auto &fn_arg : finalizer_fn->args()) {
                *(args[i++]) = &fn_arg;
            }
            LCOMPILERS_ASSERT(i == (int)args.size())
            }

            return saved_BB;
        }

        /**
         * Finishes `START_CACHE` job by reverting back to the passed BasicBlock.
         * It also terminates the function body with `return void`
         */
        void END_CACHE(llvm::BasicBlock* revert_bb) {
            LCOMPILERS_ASSERT(revert_bb)
            LCOMPILERS_ASSERT_MSG(!builder_->GetInsertBlock()->getTerminator(),
                "`END CACHE` adds the terminator, not expected to be added by other utility")
            builder_->CreateRetVoid();
            builder_->SetInsertPoint(revert_bb);
        }

        llvm::Value* call_cached_finalizer(const std::string& cache_key,
                const std::vector<llvm::Value*>& call_args) {
            llvm::Function* fn = type_finalizer_cache_[cache_key];
            std::vector<llvm::Value*> fixed_args = call_args;
            llvm::FunctionType* fnty = fn->getFunctionType();
            for (size_t i = 0; i < fixed_args.size() && i < fnty->getNumParams(); i++) {
                if (fixed_args[i]->getType() != fnty->getParamType(i)) {
                    fixed_args[i] = builder_->CreateBitCast(fixed_args[i], fnty->getParamType(i));
                }
            }
            return builder_->CreateCall(fn, fixed_args);
        }

        /// Takes a finalization process and wrap it in allocated or not check to avoid nullptr dereference.
        template <typename finProcess>
        void check_if_allocated_then_finalize(llvm::Value* const ptr, ASR::ttype_t* const t, ASR::Struct_t* const struct_sym, finProcess fin){
            auto const null_ptr_const = llvm::ConstantPointerNull::get(
                                            get_llvm_type(ASRUtils::type_get_past_allocatable_pointer(t), struct_sym)->getPointerTo());
            llvm_utils_->create_if_else(builder_->CreateICmpNE(ptr, null_ptr_const), fin, [](){}, "is_allocated");
        }

        /// Gets Struct (if any) from Variable
        /// Returns nullptr if variable isn't binded to a struct symbol.
        ASR::Struct_t* get_struct_sym(ASR::Variable_t* var){
            if(var->m_type_declaration){
                auto const v_declaration_sym_past = ASRUtils::symbol_get_past_external(var->m_type_declaration);
                if(ASR::is_a<ASR::Struct_t>(*v_declaration_sym_past)) 
                    return ASR::down_cast<ASR::Struct_t>(v_declaration_sym_past);
            }
            return nullptr;
        }

        /// Is this symbolTable owned by a Struct
        bool is_struct_symtab(SymbolTable* const symtab){
            ASR::asr_t* const asr_owner = symtab->asr_owner;
            return ASR::is_a<ASR::symbol_t>(*asr_owner)
                && ASR::is_a<ASR::Struct_t>(*ASR::down_cast<ASR::symbol_t>(asr_owner));
        }

        /**
         * @brief Return LLVM type of the passed ASR type.
         *
         * @param type the ASR type we want its LLVM corresponding type.
         * @param struct_sym current struct if `type` contains StructType type (e.g. array of struct), nullptr if no StructType present. 
         */
        llvm::Type* get_llvm_type(ASR::ttype_t* type, ASR::Struct_t* struct_sym){
            static auto const dummy_var_symbol = ASRUtils::EXPR(ASR::make_Var_t(al_, type->base.loc, nullptr));
            ASR::down_cast<ASR::Var_t>(dummy_var_symbol)->m_v = (ASR::symbol_t*)struct_sym;
            return llvm_utils_->get_type_from_ttype_t_util(dummy_var_symbol, type, llvm_utils_->module);
        }

        /// Check if the nature of the variable can't be finalized
        static bool not_finalizable_variable(ASR::Variable_t* const v){
            /* TODO :: Handle non local + `Value` attribute. */
            if (v->m_intent != ASR::Local) {
                // Most non-local variables are not owned by this scope and must
                // not be finalized here. One exception appears with ENTRY lowering:
                // extra return variables are emitted as intent(return_var) locals.
                // We should finalize those, but still skip the actual function
                // return variable, which is owned by the caller.
                if (v->m_intent == ASR::intentType::ReturnVar) {
                    ASR::symbol_t* owner = ASR::down_cast<ASR::symbol_t>(
                        v->m_parent_symtab->asr_owner);
                    if (!ASR::is_a<ASR::Function_t>(*owner)) {
                        return true;
                    }
                    ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(owner);
                    if (fn->m_return_var && ASR::is_a<ASR::Var_t>(*fn->m_return_var)) {
                        ASR::symbol_t* ret_sym = ASR::down_cast<ASR::Var_t>(
                            fn->m_return_var)->m_v;
                        if (ret_sym == &v->base) {
                            return true;
                        }
                    }
                } else {
                    return true;
                }
            }
            if (v->m_storage == ASR::Parameter) {
                // Keep most PARAMETER symbols non-finalizable. A narrow
                // exception is program-scope parameter structs whose runtime
                // construction can allocate member storage (for example,
                // fixed-length character members initialized from constructors).
                // Those need scope-exit finalization to avoid leaks.
                ASR::symbol_t* owner = ASR::down_cast<ASR::symbol_t>(
                    v->m_parent_symtab->asr_owner);
                if (ASR::is_a<ASR::Program_t>(*owner)) {
                    ASR::ttype_t* t = ASRUtils::type_get_past_array(v->m_type);
                    if (ASR::is_a<ASR::StructType_t>(*t)) {
                        return false;
                    }
                }
                return true;
            }
            if (v->m_storage == ASR::Save) {
                // Save variables in functions persist across calls and must not
                // be finalized at function exit.  In the main Program, however,
                // save struct variables have string members whose data is
                // heap-allocated during struct initialization.  These must be
                // finalized at program exit to avoid leaking that memory.
                // (Plain save strings have static data and must NOT be freed.)
                ASR::symbol_t* owner = ASR::down_cast<ASR::symbol_t>(
                    v->m_parent_symtab->asr_owner);
                if (ASR::is_a<ASR::Program_t>(*owner)) {
                    ASR::ttype_t* t = ASRUtils::type_get_past_array(v->m_type);
                    if (ASR::is_a<ASR::StructType_t>(*t)) {
                        return false;
                    }
                }
                return true;
            }
            // Module-scope non-allocatable, non-pointer string variables
            // (including arrays of strings) have their data in static global
            // buffers.  Attempting to free them crashes the leak detector.
            {
                ASR::symbol_t* owner = ASR::down_cast<ASR::symbol_t>(
                    v->m_parent_symtab->asr_owner);
                if (ASR::is_a<ASR::Module_t>(*owner)
                        && !ASRUtils::is_allocatable(v->m_type)
                        && !ASRUtils::is_pointer(v->m_type)) {
                    ASR::ttype_t* base_t = ASRUtils::type_get_past_array(v->m_type);
                    if (ASR::is_a<ASR::String_t>(*base_t)) {
                        return true;
                    }
                }
            }
            return false;
        }

        static bool non_deallocatable_construct(ASR::asr_t* const s){ // Can't deallocate
            ASR::symbol_t* sym = ASR::is_a<ASR::symbol_t>(*s) ? ASR::down_cast<ASR::symbol_t>(s) : nullptr;
            const bool is_interface = sym && ASR::is_a<ASR::Function_t>(*sym)
                                      && ASRUtils::get_FunctionType(sym)->m_deftype == ASR::Interface;
            const bool is_external_abi = sym && ASR::is_a<ASR::Function_t>(*sym)
                                      && ASRUtils::get_FunctionType(sym)->m_abi == ASR::ExternalUndefined;
            const bool is_TU = !sym && ASR::is_a<ASR::unit_t>(*s) && ASR::is_a<ASR::TranslationUnit_t>(*(ASR::unit_t*)s);
            return is_TU || is_interface || is_external_abi ;
        }

        static bool is_variable(ASR::symbol_t* const s){
            return s->type == ASR::Variable;
        }

        /// Gets LLVM value (LLVM SSA) of the variable.
        /// Type of return is : A pointer to the underlying type.
        llvm::Value* get_llvm_var(ASR::Variable_t* const v){
            LCOMPILERS_ASSERT(!non_deallocatable_construct(v->m_parent_symtab->asr_owner) 
                              && !is_struct_symtab(v->m_parent_symtab))
            llvm::Value* llvm_var {}; {
                const uint32_t v_h = get_hash((ASR::asr_t*)v);
                LCOMPILERS_ASSERT(llvm_utils_->llvm_symtab.find(v_h) != llvm_utils_->llvm_symtab.end());
                llvm_var = llvm_utils_->llvm_symtab[v_h];
            }
            auto const load_needed = LLVM::is_llvm_pointer(*v->m_type);
            if(load_needed){
                auto const variable_llvm_type = get_llvm_type(v->m_type, get_struct_sym(v));
                llvm_var = llvm_utils_->CreateLoad2(variable_llvm_type, llvm_var);
            }
            return llvm_var;
        }

        /**
         * @brief Gets an LLVM pointer to a struct-instance-variable member.
         *
         * @param ptr pointer to the struct.
         * @param struct_ The struct symbol we're working with.
         * @param idx Index (zero based) of the member within the struct we want to get a ptr to.
         */
        llvm::Value* get_ptr_to_struct_variable_member(llvm::Value* const ptr, ASR::Struct_t* const struct_, const int idx) {
            // finalize_struct() unwraps any class wrapper before visiting members.
            verify(ptr, llvm_utils_->getStructType(struct_, llvm_utils_->module, true));
            LCOMPILERS_ASSERT_MSG(!ASRUtils::is_unlimited_polymorphic_type(&struct_->base),
                              "This utility shouldn't be called on unlimited polymorphic struct type")

            bool is_extended = struct_->m_parent != nullptr;
            llvm::Value* const fetched_member = llvm_utils_->create_gep2(
                llvm_utils_->getStructType(struct_, llvm_utils_->module), ptr, idx + is_extended);
            auto const fetched_member_variable = ASR::down_cast<ASR::Variable_t>(struct_->m_symtab->get_symbol(struct_->m_members[idx]));
            auto const fetched_member_asr_type = fetched_member_variable->m_type;
            if(LLVM::is_llvm_pointer(*fetched_member_asr_type)) {
                auto const loaded_fetched_member = llvm_utils_->CreateLoad2(
                    get_llvm_type(fetched_member_asr_type, get_struct_sym(fetched_member_variable)), fetched_member);
                return loaded_fetched_member;
            }
            return fetched_member;
        }
        /**
         * Check if this type is finalizable.
         * It dispatches to other helper functions.
         * @param t ASR type that we're checking
         * @param struct_sym struct symbol of the ASR type (if it's of ASR structType_t)
         * @param in_struct is this type inside a struct symbol. We make decisions based on this info.
         */
        bool is_finalizable_type(ASR::ttype_t* const t, ASR::Struct_t* const struct_sym, const bool in_struct){
            if(ASRUtils::is_allocatable(t)){
                return is_finalizable_type_allocatable(ASR::down_cast<ASR::Allocatable_t>(t), struct_sym, in_struct);
            } else if (ASRUtils::is_pointer(t)){
                return is_finalizable_type_pointer(ASR::down_cast<ASR::Pointer_t>(t), struct_sym, in_struct);
            }
            return is_finalizable_type_atomic(t, struct_sym, in_struct);
        }
        
        /// Does this type (non allocatable, non pointer) require a finalization process
        bool is_finalizable_type_atomic(ASR::ttype_t* const t, ASR::Struct_t* const struct_sym, [[maybe_unused]]const bool in_struct){
            LCOMPILERS_ASSERT_MSG(!ASRUtils::is_allocatable_or_pointer(t), "Doesn't handle allocatable nor pointer")

            switch(t->type){
                case ASR::Integer:
                case ASR::Real:
                case ASR::Complex:
                case ASR::UnsignedInteger:
                case ASR::Logical:
                    return false;
                case ASR::StructType:{
                    if(ASRUtils::is_unlimited_polymorphic_type(struct_sym)) { return false; /*Handled in allocatable cleanup flow*/ }
                    ASR::StructType_t* struc_t = ASR::down_cast<ASR::StructType_t>(t);
                    bool finalizable_struct = false;
                    finalizable_struct |= struc_t->m_is_unlimited_polymorphic;
                    // Check for user-defined FINAL procedures (Fortran 2018 §7.5.6.3)
                    if(struct_sym && struct_sym->n_member_functions > 0) { return true; }
                    if(struct_sym->m_parent){ // Check parent
                        ASR::Struct_t* const parent_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(struct_sym->m_parent));
                        finalizable_struct |= is_finalizable_type(parent_struct->m_struct_signature, parent_struct, false);
                    }
                    for(size_t i = 0; (i < struct_sym->n_members) && !finalizable_struct; i++) {
                        auto* member_var = ASR::down_cast<ASR::Variable_t>(
                            struct_sym->m_symtab->get_symbol(struct_sym->m_members[i]));
                        finalizable_struct |= is_finalizable_type(member_var->m_type, get_struct_sym(member_var), true);
                    }
                    return finalizable_struct;
                }
                case ASR::Array:
                    return is_array_finalizable(ASR::down_cast<ASR::Array_t>(t), struct_sym);
                break;
                case ASR::List:
                case ASR::Dict:
                case ASR::Tuple:
                case ASR::UnionType:
                case ASR::Set:
                return false; // >>>>> TO DO <<<<<
                case ASR::String:
                return true;
                case ASR::FunctionType:
                case ASR::CPtr:
                    return false;
                default:
                    throw LCompilersException("Handle this case");
            }
        }

        // Check if a pointer type is finalizable (TRUE for cases where the compiler allocated some descriptor; User must free data himself)
        bool is_finalizable_type_pointer(ASR::Pointer_t* const t, [[maybe_unused]] ASR::Struct_t* const struct_sym, const bool in_struct){
            ASR::ttype_t* const t_past = ASRUtils::type_get_past_allocatable_pointer(&t->base);
            switch(t_past->type){
                case ASR::Array:
                    return in_struct 
                        && (   ASRUtils::extract_physical_type(t_past) == ASR::DescriptorArray
                            || ASRUtils::extract_physical_type(t_past) == ASR::AssumedRankArray);
                case ASR::StructType:
                    return ASRUtils::is_class_type(t_past);
                case ASR::Integer:
                case ASR::Real:
                case ASR::Complex:
                case ASR::UnsignedInteger:
                case ASR::Logical:
                case ASR::List:
                case ASR::Dict:
                case ASR::Tuple:
                case ASR::UnionType:
                case ASR::Set:
                case ASR::String:
                case ASR::FunctionType:
                case ASR::CPtr:
                    return false;
                default:
                    throw LCompilersException("Unhandled ASR pointer type");
            }
        }

        // Check if an allocatable type is finalizable
        bool is_finalizable_type_allocatable(ASR::Allocatable_t* const t, ASR::Struct_t* const struct_sym, const bool  in_struct){
            ASR::ttype_t* const t_past = ASRUtils::type_get_past_allocatable_pointer(&t->base);
            switch(t_past->type){
                case ASR::Integer:
                case ASR::Real:
                case ASR::Complex:
                case ASR::UnsignedInteger:
                case ASR::Logical:
                case ASR::StructType:
                case ASR::String:
                    return true;
                case ASR::Array:
                    return is_finalizable_type_atomic(t_past, struct_sym, in_struct);
                case ASR::FunctionType:
                case ASR::CPtr:
                case ASR::List:
                case ASR::Dict:
                case ASR::Tuple:
                case ASR::UnionType:
                case ASR::Set:
                    return false;
                default:
                    throw LCompilersException("Unhandled ASR allocatable type");
            }
        }
        
        /// Does this array require finalization
        bool is_array_finalizable(ASR::Array_t* const t, ASR::Struct_t* const struct_sym){
            switch (t->m_physical_type) {
                case ASR::DescriptorArray:
                case ASR::PointerArray:
                    return true;
                case ASR::FixedSizeArray:
                case ASR::SIMDArray:
                    return is_finalizable_type(t->m_type, struct_sym, false);
                default:
                    return false;
            }
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

        /// Inserts null into freed ptr holder -- Useful only for debugging and deallocation statement
        void insert_null(llvm::Type* null_type, llvm::Value* ptr){ 
            #if LLVM_VERSION_MAJOR <= 15
                if(!(ptr->getType()->isPointerTy() && ptr->getType()->getPointerElementType()->isPointerTy()))
                    throw LCompilersException("ptr parameter must be a PTR to PTR type.");
            #endif
            builder_->CreateStore(
                llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(null_type)), ptr);
        }

        void check_all_caches_done_properly(){
            for(auto const& cache_pair : type_finalizer_cache_){
                if(cache_pair.second->back().getTerminator() != nullptr){
                    LCOMPILERS_ASSERT(("Cache function" + 
                            cache_pair.second->getName().str() +
                            "Not properly created").c_str())
                }
                
            }
        }

    public:

/*>>>>>>>>>>>>>>>>>>>>> Entry <<<<<<<<<<<<<<<<<<<<<<< */

        /**
         * Finalize nested allocatable components before explicit deallocate.
         * This ensures nested allocatables are freed before the outer structure.
         */
        void finalize_before_deallocate(llvm::Value* const ptr, ASR::ttype_t* const t,
                ASR::Struct_t* const struct_sym, bool in_struct) {
            ASR::ttype_t* const t_past = ASRUtils::type_get_past_allocatable_pointer(t);
            switch(t_past->type) {
                case ASR::Array: {
                    ASR::Array_t* const arr_t = ASR::down_cast<ASR::Array_t>(t_past);
                    if (arr_t->m_type->type != ASR::StructType) { return; }
                    if (!is_finalizable_type(arr_t->m_type, struct_sym, in_struct)) { return; }
                    // Finalize array elements but don't free the array data itself
                    auto *const arr_llvm_t = get_llvm_type(t_past, struct_sym);
                    auto *const arrayType_llvm_t = get_llvm_type(arr_t->m_type, struct_sym);
                    auto const array_size_lazy = [&]() {
                        return llvm_utils_->get_array_size(ptr, arr_llvm_t, t_past, &asr_to_llvm_visitor_);
                    };
                    auto const data = builder_->CreateLoad(arrayType_llvm_t->getPointerTo(),
                        llvm_utils_->create_gep2(arr_llvm_t, ptr, 0));
                    check_if_allocated_then_finalize(data, arr_t->m_type, struct_sym, [&](){
                        free_array_data(data, arr_t->m_type, struct_sym, array_size_lazy);
                    });
                    return;
                }
                case ASR::StructType: {
                    if (!is_finalizable_type(t_past, struct_sym, in_struct)) { return; }
                    finalize(ptr, t_past, struct_sym, in_struct);
                    return;
                }
                default:
                    return;
            }
            (void)in_struct;  // May be used in future for dimension descriptor handling
        }


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
            check_all_caches_done_properly();
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
            std::map<ASR::symbol_t*, llvm::Type*> newclass2vtabtype;
            std::map<uint64_t, llvm::Function*>& llvm_symtab_fn;
            std::function<void(ASR::Struct_t*, llvm::Value*, ASR::ttype_t*, bool)> allocate_struct_array_members;

        public:
            std::map<ASR::symbol_t*, llvm::Constant*> newclass2vtab;
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
            void store_class_struct(ASR::Struct_t* class_sym, llvm::Value* class_ptr, llvm::Value* struct_ptr);
            void store_intrinsic_type_vptr(ASR::ttype_t* ttype, int kind, llvm::Value* ptr, llvm::Module* module);
            llvm::Constant* get_intrinsic_type_vptr(ASR::ttype_t* ttype, int kind, llvm::Module* module);

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

            llvm::Function* define_allocate_struct_function(ASR::symbol_t* struct_sym, llvm::Module* module);
            void fill_allocate_struct_body(ASR::symbol_t* struct_sym, llvm::Function* func, llvm::Module* module);

            llvm::Function* define_struct_copy_function(ASR::symbol_t* struct_sym,
                                                        llvm::Module* module);
            void fill_struct_copy_body(ASR::symbol_t* struct_sym,
                                    llvm::Function* func,
                                    llvm::Module* module);
            
            llvm::Function* define_intrinsic_type_copy_function(ASR::ttype_t* type, llvm::Module* module); 

            void fill_intrinsic_type_copy_body(ASR::ttype_t* type, llvm::Function* func, llvm::Module* module);

            llvm::Function* define_intrinsic_type_allocate_function(ASR::ttype_t* type, llvm::Module* module);

            void fill_intrinsic_type_allocate_body(ASR::ttype_t* type, llvm::Function* func, llvm::Module* module);

            void struct_deepcopy(ASR::expr_t* src_expr, llvm::Value* src, ASR::ttype_t* src_ty,
                ASR::ttype_t* dest_ty, llvm::Value* dest, llvm::Module* module,
                bool use_defined_assignment = false);

            // Copy dimension descriptors and rank from src to dest array descriptor.
            void copy_dimension_descriptors(
                llvm::Type* llvm_array_type, llvm::Value* src, llvm::Value* dest,
                llvm::Module* module);
            
            /**
             * Class => `{VTable*, struct_t*}`
             *@brief Creates a class structure based on passed ASR node `class_symbol`,
             *       to act as non-owner viewer variable.
             * Note : Corresponding VTable inserted.
             */
            llvm::Value* create_class_view(ASR::Struct_t* class_symbol, llvm::Value* viewed_struct,
                                           llvm::Value* vptr = nullptr);
            
            /**
             * Class Structure => `{VTable*, struct_t*}`
             * @brief Allocates memory for array of classes.
             *        Don't allocate consecutive class structures, instead allocate 1 class structure
             *        and insert consecutive allocated structs into the class structure along with single vtable.
             * @param allocated_subclass If provided, use this type for sizing the underlying data array and setting vptr
             */
            void allocate_array_of_classes(ASR::Struct_t* class_symbol, 
                [[maybe_unused]] ASR::StructType_t* struct_type, llvm::Value* array_data_ptr,
                llvm::Value* size, ASR::symbol_t* allocated_subclass = nullptr, bool realloc = false);

            void allocate_array_of_unlimited_polymorphic_type(
                ASR::Struct_t* class_symbol, ASR::StructType_t* struct_type,
                llvm::Value* array_data_ptr, llvm::Value* size,
                ASR::ttype_t* alloc_type, bool realloc, llvm::Module* module,
                llvm::Value* string_len = nullptr);
    };

    class LLVMTuple {
        private:

            llvm::LLVMContext& context;
            LLVMUtils* llvm_utils;
            llvm::IRBuilder<>* builder;

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

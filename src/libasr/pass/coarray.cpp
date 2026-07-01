#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/replace_coarray.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/intrinsic_subroutine_registry.h>
#include <map>
#include <set>

#ifndef CAF_PRIF_VERSION
#define CAF_PRIF_VERSION 8
#endif

namespace LCompilers {

// PRIFInterface lowers coarray operations into PRIF runtime interface calls.
// It creates the required PRIF function interfaces/wrappers and transforms
// coarray accesses/allocations into runtime calls used by Caffeine.
class PRIFInterface {
    public:
        // Returns a scope-local reference to sym, creating an ExternalSymbol if required.
        ASR::symbol_t* get_symbol_in_scope(SymbolTable *decl_scope, SymbolTable *use_scope,
                                           ASR::symbol_t *sym, const Location &loc) {
            if (decl_scope == use_scope || decl_scope == unit.m_symtab) return sym;
            std::string sym_name = ASRUtils::symbol_name(sym);
            if (ASR::symbol_t *existing = use_scope->get_symbol(sym_name)) {
                return existing;
            }
            std::string mod_name = "";
            if (decl_scope->asr_owner && ASR::is_a<ASR::symbol_t>(*decl_scope->asr_owner) && ASR::is_a<ASR::Module_t>(*ASR::down_cast<ASR::symbol_t>(decl_scope->asr_owner))) {
                mod_name = ASR::down_cast<ASR::Module_t>(ASR::down_cast<ASR::symbol_t>(decl_scope->asr_owner))->m_name;
            } else {
                throw LCompilersException("Coarray companion is in another scope that is not a module");
            }
            ASR::asr_t *ext = ASR::make_ExternalSymbol_t(
                al, loc, use_scope, s2c(al, sym_name), sym,
                s2c(al, mod_name), nullptr, 0, s2c(al, sym_name), ASR::accessType::Private);
            ASR::symbol_t *ext_sym = ASR::down_cast<ASR::symbol_t>(ext);
            use_scope->add_symbol(sym_name, ext_sym);
            return ext_sym;
        }
        ASR::symbol_t* declare_variable(SymbolTable *symtab, const Location &loc,
                                        const std::string &name, ASR::ttype_t *type,
                                        ASR::intentType intent, ASR::symbol_t *type_decl,
                                        ASR::abiType abi, ASR::accessType access,
                                        ASR::presenceType presence, bool value_attr) {
            ASRUtils::ASRBuilder b(al, loc);
            b.VariableDeclaration(symtab, name, type, intent, type_decl, abi, value_attr);
            ASR::symbol_t *sym = symtab->get_symbol(name);
            LCOMPILERS_ASSERT(sym);
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
            var->m_access = access;
            var->m_presence = presence;
            var->m_abi = abi;
            return sym;
        }

        SymbolTable* get_global_scope() {
            return unit.m_symtab;
        }

    private:
        Allocator &al;
        ASR::TranslationUnit_t &unit;

        ASR::symbol_t* get_or_create_dummy_struct(const Location &loc, std::string &struct_name) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string symbol_name = struct_name;
            if (ASR::symbol_t *existing = global_scope->get_symbol(symbol_name)) return existing;
            SymbolTable *struct_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);
            ASR::asr_t *struct_asr = ASR::make_Struct_t(
                al, loc, struct_symtab, s2c(al, symbol_name), nullptr,
                nullptr, 0, nullptr, 0, nullptr, 0,
                ASR::abiType::Source, ASR::accessType::Public,
                false, false, nullptr, 0, nullptr, nullptr, nullptr, 0);
            ASR::symbol_t *struct_sym = ASR::down_cast<ASR::symbol_t>(struct_asr);
            ASR::Struct_t *struct_t = ASR::down_cast<ASR::Struct_t>(struct_sym);
            ASR::ttype_t *struct_type = ASRUtils::make_StructType_t_util(al, loc, struct_sym, true);
            struct_t->m_struct_signature = struct_type;
            global_scope->add_symbol(symbol_name, struct_sym);
            return struct_sym;
        }
        
        ASR::symbol_t* get_or_create_prif_coarray_handle_struct(const Location &loc) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string symbol_name = "prif_coarray_handle";

            if (ASR::symbol_t *existing = global_scope->get_symbol(symbol_name)) {
                return existing;
            }

            SymbolTable *struct_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *cptr_type = b.CPtr();
            declare_variable(
                struct_symtab, loc, "info", cptr_type, ASR::intentType::Local,
                nullptr, ASR::abiType::BindC, ASR::accessType::Private,
                ASR::presenceType::Required, false);

            Vec<char*> members; members.reserve(al, 1);
            members.push_back(al, s2c(al, "info"));

            ASR::asr_t *struct_asr = ASR::make_Struct_t(
                al, loc, struct_symtab, s2c(al, symbol_name), nullptr,
                nullptr, 0, members.p, members.n, nullptr, 0,
                ASR::abiType::BindC, ASR::accessType::Public,
                false, false, nullptr, 0, nullptr, nullptr, nullptr, 0);
            ASR::symbol_t *struct_sym = ASR::down_cast<ASR::symbol_t>(struct_asr);
            ASR::Struct_t *struct_t = ASR::down_cast<ASR::Struct_t>(struct_sym);

            ASR::ttype_t *struct_type = ASRUtils::make_StructType_t_util(
                al, loc, struct_sym, true);
            struct_t->m_struct_signature = struct_type;

            global_scope->add_symbol(symbol_name, struct_sym);
            return struct_sym;
        }

        ASR::expr_t* make_struct_var(SymbolTable *symtab, const Location &loc,
                                     const std::string &name, ASR::symbol_t *struct_sym,
                                     ASR::intentType intent, ASR::presenceType presence,
                                     bool value_attr) {
            ASR::ttype_t *struct_type = ASRUtils::make_StructType_t_util(al, loc, struct_sym, true);
            ASR::symbol_t *sym = declare_variable(
                symtab, loc, name, struct_type, intent, struct_sym,
                ASR::abiType::Source, ASR::accessType::Public, presence, value_attr);
            return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
        }

        ASR::symbol_t* get_or_create_prif_get_subroutine(const Location &loc, std::string symbol_name = "") {
            SymbolTable *global_scope = unit.m_symtab;

            if (symbol_name.empty()) {
                symbol_name = get_mangled_name("prif", "prif_get");
            }
            else {
                symbol_name = get_mangled_name("prif", symbol_name);
            }

            if (global_scope->get_symbol(symbol_name)) {
                return global_scope->get_symbol(symbol_name);
            }

            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);

            ASR::ttype_t *int32_type = int32;
            ASR::ttype_t *int64_type = int64;
            ASR::ttype_t *cptr_type = b.CPtr();
            ASR::expr_t *image_num = b.Variable(fn_symtab, "image_num", int32_type,
                                               ASR::intentType::In, nullptr,
                                               ASR::abiType::Source, true);
            ASR::symbol_t *handle_sym = get_or_create_prif_coarray_handle_struct(loc);
            ASR::expr_t *coarray_handle = make_struct_var(
                fn_symtab, loc, "coarray_handle", handle_sym,
                ASR::intentType::In, ASR::presenceType::Required, false);
            ASR::expr_t *offset = b.Variable(fn_symtab, "offset", int64_type,
                                             ASR::intentType::In, nullptr,
                                             ASR::abiType::Source, true);
            ASR::expr_t *current_image_buffer = b.Variable(fn_symtab, "current_image_buffer", cptr_type,
                                                           ASR::intentType::In, nullptr,
                                                           ASR::abiType::Source, true);
            ASR::expr_t *size_in_bytes = b.Variable(fn_symtab, "size_in_bytes", int64_type,
                                                    ASR::intentType::In, nullptr,
                                                    ASR::abiType::Source, true);
            ASR::ttype_t *errmsg_type = ASRUtils::TYPE(ASR::make_String_t(
                al, loc, 1, nullptr,
                ASR::string_length_kindType::AssumedLength,
                ASR::string_physical_typeType::DescriptorString));
            ASR::ttype_t *errmsg_alloc_type = allocatable_deferred_string();

            ASR::symbol_t *stat_sym = declare_variable(
                fn_symtab, loc, "stat", int32_type, ASR::intentType::Out, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            ASR::expr_t *stat = ASRUtils::EXPR(ASR::make_Var_t(al, loc, stat_sym));

            ASR::symbol_t *errmsg_sym = declare_variable(
                fn_symtab, loc, "errmsg", errmsg_type, ASR::intentType::InOut, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            ASR::expr_t *errmsg = ASRUtils::EXPR(ASR::make_Var_t(al, loc, errmsg_sym));

            ASR::symbol_t *errmsg_alloc_sym = declare_variable(
                fn_symtab, loc, "errmsg_alloc", errmsg_alloc_type, ASR::intentType::InOut, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            ASR::expr_t *errmsg_alloc = ASRUtils::EXPR(ASR::make_Var_t(al, loc, errmsg_alloc_sym));

            Vec<ASR::expr_t*> args;
            args.reserve(al, 8);
            args.push_back(al, image_num);
            args.push_back(al, coarray_handle);
            args.push_back(al, offset);
            args.push_back(al, current_image_buffer);
            args.push_back(al, size_in_bytes);
            args.push_back(al, stat);
            args.push_back(al, errmsg);
            args.push_back(al, errmsg_alloc);

            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, symbol_name), nullptr, 0,
                args.p, args.n, nullptr, 0, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Interface, s2c(al, symbol_name),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);

            global_scope->add_symbol(symbol_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }

        ASR::symbol_t* get_or_create_prif_put_subroutine(const Location &loc) {
            return get_or_create_prif_get_subroutine(loc, "prif_put");
        }

        ASR::symbol_t* get_or_create_prif_initial_team_index_subroutine(const Location &loc) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string sym_name = get_mangled_name("prif", "prif_initial_team_index");
            if (ASR::symbol_t *existing = global_scope->get_symbol(sym_name)) {
                return existing;
            }
            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);
            
            ASR::symbol_t *handle_sym = get_or_create_prif_coarray_handle_struct(loc);
            ASR::expr_t *coarray_handle = make_struct_var(
                fn_symtab, loc, "coarray_handle", handle_sym,
                ASR::intentType::In, ASR::presenceType::Required, false);

            ASR::ttype_t *i64 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8));
            Vec<ASR::dimension_t> dims; dims.reserve(al, 1);
            ASR::dimension_t d; d.loc = loc; d.m_start = nullptr; d.m_length = nullptr;
            dims.push_back(al, d);
            ASR::ttype_t *i64_arr = ASRUtils::make_Array_t_util(al, loc, i64, dims.p, dims.n);
            ASR::expr_t *sub = b.Variable(fn_symtab, "sub", i64_arr,
                ASR::intentType::In, nullptr, ASR::abiType::Source, true);

            ASR::ttype_t *i32 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            ASR::symbol_t *initial_team_index_sym = declare_variable(
                fn_symtab, loc, "initial_team_index", i32, ASR::intentType::Out, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false);
            ASR::expr_t *initial_team_index = ASRUtils::EXPR(ASR::make_Var_t(al, loc, initial_team_index_sym));

            ASR::symbol_t *stat_sym = declare_variable(
                fn_symtab, loc, "stat", i32, ASR::intentType::Out, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            ASR::expr_t *stat = ASRUtils::EXPR(ASR::make_Var_t(al, loc, stat_sym));

            Vec<ASR::expr_t*> args; args.reserve(al, 4);
            args.push_back(al, coarray_handle);
            args.push_back(al, sub);
            args.push_back(al, initial_team_index);
            args.push_back(al, stat);

            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, sym_name), nullptr, 0,
                args.p, args.n, nullptr, 0, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Interface,
                s2c(al, sym_name),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);
            global_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }

        ASR::symbol_t* get_or_create_prif_get_wrapper(const Location &loc,
                              ASR::ttype_t *return_type,
                              ASR::expr_t *base_expr) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string type_suffix = ASRUtils::type_to_str_with_kind(return_type, base_expr);
            std::string symbol_name = "lcompilers_prif_get_" + type_suffix;

            if (ASR::symbol_t *existing = global_scope->get_symbol(symbol_name)) {
                return existing;
            }

            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);

            ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            ASR::ttype_t *int64_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8));
            ASR::ttype_t *cptr_type = ASRUtils::TYPE(ASR::make_CPtr_t(al, loc));
            ASR::symbol_t *handle_sym = get_or_create_prif_coarray_handle_struct(loc);

            ASR::expr_t *coarray_handle = make_struct_var(
                fn_symtab, loc, "coarray_handle", handle_sym,
                ASR::intentType::In, ASR::presenceType::Required, false);
            
            Vec<ASR::dimension_t> dims; dims.reserve(al, 1);
            ASR::dimension_t d; d.loc = loc; d.m_start = nullptr; d.m_length = nullptr;
            dims.push_back(al, d);
            ASR::ttype_t *i64_arr = ASRUtils::make_Array_t_util(al, loc, int64_type, dims.p, dims.n);
            ASR::expr_t *sub = b.Variable(fn_symtab, "sub", i64_arr,
                                               ASR::intentType::In, nullptr,
                                               ASR::abiType::Source, true);
            ASR::expr_t *offset = b.Variable(fn_symtab, "offset", int64_type,
                                             ASR::intentType::In, nullptr,
                                             ASR::abiType::Source, true);
            ASR::expr_t *return_var = b.Variable(fn_symtab, "result", return_type,
                                                 ASR::intentType::ReturnVar, nullptr,
                                                 ASR::abiType::Source, true);

            Vec<ASR::expr_t*> args; args.reserve(al, 3);
            args.push_back(al, coarray_handle);
            args.push_back(al, sub);
            args.push_back(al, offset);

            ASR::expr_t *return_ptr = ASRUtils::EXPR(ASR::make_PointerToCPtr_t(
                al, loc,
                ASRUtils::EXPR(ASR::make_GetPointer_t(al, loc, return_var,
                    ASRUtils::TYPE(ASR::make_Pointer_t(al, loc, ASRUtils::expr_type(return_var))), nullptr)),
                cptr_type, nullptr));

            ASR::expr_t *size_in_bytes = get_size_in_bytes_expr(loc, return_type);

            ASR::symbol_t *prif_get_sym = get_or_create_prif_get_subroutine(loc);

            ASR::symbol_t *image_num_sym = declare_variable(
                fn_symtab, loc, "image_num", int32_type, ASR::intentType::Local, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false);
            ASR::expr_t *image_num = ASRUtils::EXPR(ASR::make_Var_t(al, loc, image_num_sym));

            ASR::symbol_t *prif_img_idx_sym = get_or_create_prif_initial_team_index_subroutine(loc);
            Vec<ASR::call_arg_t> img_idx_args; img_idx_args.reserve(al, 4);
            ASR::call_arg_t h_arg; h_arg.loc = loc; h_arg.m_value = coarray_handle;
            ASR::call_arg_t s_arg; s_arg.loc = loc; s_arg.m_value = sub;
            ASR::call_arg_t i_arg; i_arg.loc = loc; i_arg.m_value = image_num;
            ASR::call_arg_t stat_arg; stat_arg.loc = loc; stat_arg.m_value = nullptr;
            img_idx_args.push_back(al, h_arg);
            img_idx_args.push_back(al, s_arg);
            img_idx_args.push_back(al, i_arg);
            img_idx_args.push_back(al, stat_arg);

            Vec<ASR::call_arg_t> call_args; call_args.reserve(al, 5);

            ASR::call_arg_t image_arg; image_arg.loc = loc; image_arg.m_value = image_num;
            ASR::call_arg_t handle_arg; handle_arg.loc = loc; handle_arg.m_value = coarray_handle;
            ASR::call_arg_t offset_arg; offset_arg.loc = loc; offset_arg.m_value = offset;
            ASR::call_arg_t buffer_arg; buffer_arg.loc = loc; buffer_arg.m_value = return_ptr;
            ASR::call_arg_t size_arg; size_arg.loc = loc; size_arg.m_value = size_in_bytes;

            call_args.push_back(al, image_arg);
            call_args.push_back(al, handle_arg);
            call_args.push_back(al, offset_arg);
            call_args.push_back(al, buffer_arg);
            call_args.push_back(al, size_arg);

            Vec<ASR::stmt_t*> body; body.reserve(al, 2);
            body.push_back(al, ASRUtils::STMT(ASR::make_SubroutineCall_t(
                al, loc, prif_img_idx_sym, nullptr, img_idx_args.p, img_idx_args.n, nullptr, false)));
            body.push_back(al, ASRUtils::STMT(ASR::make_SubroutineCall_t(
                al, loc, prif_get_sym, nullptr, call_args.p, call_args.n, nullptr, false)));

            Vec<char*> dep; dep.reserve(al, 2);
            dep.push_back(al, s2c(al, get_mangled_name("prif", "prif_initial_team_index")));
            dep.push_back(al, s2c(al, get_mangled_name("prif", "prif_get")));

            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, symbol_name), dep.p, dep.n,
                args.p, args.n, body.p, body.n, return_var,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Implementation, nullptr,
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);

            global_scope->add_symbol(symbol_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }

        ASR::symbol_t* get_or_create_prif_put_wrapper(const Location &loc, ASR::ttype_t *value_type, ASR::expr_t *base_expr) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string type_suffix = ASRUtils::type_to_str_with_kind(value_type, base_expr);
            std::string symbol_name = "lcompilers_prif_put_" + type_suffix;

            if (ASR::symbol_t *existing = global_scope->get_symbol(symbol_name)) {
                return existing;
            }

            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);

            ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            ASR::ttype_t *int64_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8));
            ASR::ttype_t *cptr_type = ASRUtils::TYPE(ASR::make_CPtr_t(al, loc));
            ASR::symbol_t *handle_sym = get_or_create_prif_coarray_handle_struct(loc);

            ASR::expr_t *coarray_handle = make_struct_var(
                fn_symtab, loc, "coarray_handle", handle_sym,
                ASR::intentType::In, ASR::presenceType::Required, false);

            Vec<ASR::dimension_t> dims; dims.reserve(al, 1);
            ASR::dimension_t d; d.loc = loc; d.m_start = nullptr; d.m_length = nullptr;
            dims.push_back(al, d);
            ASR::ttype_t *i64_arr = ASRUtils::make_Array_t_util(al, loc, int64_type, dims.p, dims.n);
            ASR::expr_t *sub = b.Variable(fn_symtab, "sub", i64_arr,
                                               ASR::intentType::In, nullptr,
                                               ASR::abiType::Source, true);

            ASR::expr_t *offset = b.Variable(fn_symtab, "offset", int64_type,
                                             ASR::intentType::In, nullptr,
                                             ASR::abiType::Source, true);
            ASR::expr_t *value_var = b.Variable(fn_symtab, "value", value_type,
                                                 ASR::intentType::In, nullptr,
                                                 ASR::abiType::Source, true);

            Vec<ASR::expr_t*> args; args.reserve(al, 4);
            args.push_back(al, coarray_handle);
            args.push_back(al, sub);
            args.push_back(al, offset);
            args.push_back(al, value_var);

            ASR::expr_t *value_ptr = ASRUtils::EXPR(ASR::make_PointerToCPtr_t(
                al, loc,
                ASRUtils::EXPR(ASR::make_GetPointer_t(al, loc, value_var,
                    ASRUtils::TYPE(ASR::make_Pointer_t(al, loc, ASRUtils::expr_type(value_var))), nullptr)),
                cptr_type, nullptr));

            ASR::expr_t *size_in_bytes = get_size_in_bytes_expr(loc, value_type);
            ASR::symbol_t *prif_sym = get_or_create_prif_put_subroutine(loc);

            ASR::symbol_t *image_num_sym = declare_variable(
                fn_symtab, loc, "image_num", int32_type, ASR::intentType::Local, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false);
            ASR::expr_t *image_num = ASRUtils::EXPR(ASR::make_Var_t(al, loc, image_num_sym));

            ASR::symbol_t *prif_img_idx_sym = get_or_create_prif_initial_team_index_subroutine(loc);
            Vec<ASR::call_arg_t> img_idx_args; img_idx_args.reserve(al, 4);
            ASR::call_arg_t h_arg; h_arg.loc = loc; h_arg.m_value = coarray_handle;
            ASR::call_arg_t s_arg; s_arg.loc = loc; s_arg.m_value = sub;
            ASR::call_arg_t i_arg; i_arg.loc = loc; i_arg.m_value = image_num;
            ASR::call_arg_t stat_arg; stat_arg.loc = loc; stat_arg.m_value = nullptr;
            img_idx_args.push_back(al, h_arg);
            img_idx_args.push_back(al, s_arg);
            img_idx_args.push_back(al, i_arg);
            img_idx_args.push_back(al, stat_arg);

            Vec<ASR::call_arg_t> call_args; call_args.reserve(al, 5);

            ASR::call_arg_t image_arg; image_arg.loc = loc; image_arg.m_value = image_num;
            ASR::call_arg_t handle_arg; handle_arg.loc = loc; handle_arg.m_value = coarray_handle;
            ASR::call_arg_t offset_arg; offset_arg.loc = loc; offset_arg.m_value = offset;
            ASR::call_arg_t buffer_arg; buffer_arg.loc = loc; buffer_arg.m_value = value_ptr;
            ASR::call_arg_t size_arg; size_arg.loc = loc; size_arg.m_value = size_in_bytes;

            call_args.push_back(al, image_arg);
            call_args.push_back(al, handle_arg);
            call_args.push_back(al, offset_arg);
            call_args.push_back(al, buffer_arg);
            call_args.push_back(al, size_arg);

            Vec<ASR::stmt_t*> body; body.reserve(al, 2);
            body.push_back(al, ASRUtils::STMT(ASR::make_SubroutineCall_t(
                al, loc, prif_img_idx_sym, nullptr, img_idx_args.p, img_idx_args.n, nullptr, false)));
            body.push_back(al, ASRUtils::STMT(ASR::make_SubroutineCall_t(
                al, loc, prif_sym, nullptr, call_args.p, call_args.n, nullptr, false)));

            Vec<char*> dep; dep.reserve(al, 2);
            dep.push_back(al, s2c(al, get_mangled_name("prif", "prif_initial_team_index")));
            dep.push_back(al, s2c(al, get_mangled_name("prif", "prif_put")));

            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, symbol_name), dep.p, dep.n,
                args.p, args.n, body.p, body.n, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Implementation, nullptr,
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);

            global_scope->add_symbol(symbol_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }

        ASR::expr_t* make_cptr_from_expr(const Location &loc, ASR::expr_t *expr) {
            ASR::ttype_t *cptr_type = ASRUtils::TYPE(ASR::make_CPtr_t(al, loc));
            ASR::ttype_t *ptr_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc, ASRUtils::expr_type(expr)));
            return ASRUtils::EXPR(ASR::make_PointerToCPtr_t(
                al, loc,
                ASRUtils::EXPR(ASR::make_GetPointer_t(al, loc, expr, ptr_type, nullptr)),
                cptr_type, nullptr));
        }

        ASR::expr_t* make_prif_handle_expr(const Location &loc, ASR::expr_t *expr) {
            (void)loc;
            if (ASR::is_a<ASR::Var_t>(*expr)) {
                ASR::symbol_t *var_sym = ASR::down_cast<ASR::Var_t>(expr)->m_v;
                ASR::symbol_t *orig_sym = ASRUtils::symbol_get_past_external(var_sym);
                
                auto companions = get_coarray_companions(orig_sym);
                ASR::symbol_t *hsym_orig = companions.first;
                
                SymbolTable *orig_scope = ASRUtils::symbol_parent_symtab(hsym_orig);
                SymbolTable *current_scope = ASRUtils::symbol_parent_symtab(var_sym);
                ASR::symbol_t *hsym_use = get_symbol_in_scope(orig_scope, current_scope, hsym_orig, loc);
                return ASRUtils::EXPR(ASR::make_Var_t(al, loc, hsym_use));
            }
            LCOMPILERS_ASSERT_MSG(false,
                "Coarray variable used before prif_allocate_coarray initialization");
            return nullptr;
        }

        ASR::expr_t* get_handle_base_expr(ASR::expr_t *expr) {
            if (ASR::is_a<ASR::ArrayItem_t>(*expr)) {
                return ASR::down_cast<ASR::ArrayItem_t>(expr)->m_v;
            }
            if (ASR::is_a<ASR::ArraySection_t>(*expr)) {
                return ASR::down_cast<ASR::ArraySection_t>(expr)->m_v;
            }
            return expr;
        }

        ASR::expr_t* compute_offset_bytes(const Location &loc,
                                          ASR::expr_t *base_expr,
                                          ASR::expr_t *size_in_bytes) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *int64_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8));
            if (ASR::is_a<ASR::Var_t>(*base_expr)) {
                return b.i2i_t(b.i32(0), int64_type);
            }
            else if (!ASR::is_a<ASR::ArrayItem_t>(*base_expr)) {
                throw LCompilersException("compute_offset_bytes: unsupported coarray base expression");
            }

            ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(base_expr);
            ASR::expr_t *arr_expr = item->m_v;
            size_t n_args = item->n_args;

            ASR::expr_t *offset_elements = b.i2i_t(b.i32(0), int64_type);
            ASR::expr_t *stride = b.i2i_t(b.i32(1), int64_type);

            for (size_t i = 0; i < n_args; i++) {
                ASR::array_index_t idx = item->m_args[i];
                if (!idx.m_left) {
                    continue;
                }
                ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
                ASR::expr_t *dim_expr = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                    static_cast<int64_t>(i + 1), int32_type));
                ASR::expr_t *lbound = ASRUtils::EXPR(ASR::make_ArrayBound_t(
                    al, loc, arr_expr, dim_expr, int32_type,
                    ASR::arrayboundType::LBound, nullptr));
                ASR::expr_t *extent = ASRUtils::get_size(arr_expr, static_cast<int>(i + 1), al);

                ASR::expr_t *index_offset = b.Sub(idx.m_left, lbound);
                ASR::expr_t *index_offset64 = b.i2i_t(index_offset, int64_type);
                offset_elements = b.Add(offset_elements, b.Mul(index_offset64, stride));
                stride = b.Mul(stride, b.i2i_t(extent, int64_type));
            }

            return b.Mul(offset_elements, size_in_bytes);
        }

        ASR::expr_t* get_size_in_bytes_expr(const Location &loc, ASR::ttype_t *ttype) {
            ASR::ttype_t *int64_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8));
            ASR::ttype_t *base_type = ASRUtils::type_get_past_array(
                ASRUtils::type_get_past_allocatable(
                    ASRUtils::type_get_past_pointer(ttype)));
            auto [size_bytes, _align] = ASRUtils::compute_type_size_align(base_type);
            (void)_align;
            if (size_bytes <= 0) {
                throw LCompilersException(
                    "get_size_in_bytes_expr: runtime-sized types "
                    "are not supported in coarray lowering");
            }
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, size_bytes, int64_type));
        }

    public:
        struct SavedCoarray {
            ASR::Variable_t *var;
            ASR::symbol_t *handle_sym;
            ASR::symbol_t *data_sym;
            ASR::expr_t *init_value;
            Location loc;
        };
        Vec<SavedCoarray> saved_coarrays;

        std::map<ASR::symbol_t*, std::pair<ASR::symbol_t*, ASR::symbol_t*>> coarray_companions;

        std::pair<ASR::symbol_t*, ASR::symbol_t*> get_coarray_companions(ASR::symbol_t *sym) {
            auto it = coarray_companions.find(sym);
            if (it != coarray_companions.end()) {
                return it->second;
            }
            ASR::symbol_t *orig_sym = ASRUtils::symbol_get_past_external(sym);
            it = coarray_companions.find(orig_sym);
            if (it != coarray_companions.end()) {
                return it->second;
            }
            std::string vname = ASRUtils::symbol_name(orig_sym);
            SymbolTable *orig_scope = ASRUtils::symbol_parent_symtab(orig_sym);
            std::string hname = vname + "__coarray_handle";
            std::string dname = vname + "__coarray_data";
            ASR::symbol_t *hsym = orig_scope->get_symbol(hname);
            ASR::symbol_t *dsym = orig_scope->get_symbol(dname);
            LCOMPILERS_ASSERT_MSG(hsym && dsym, "Coarray companion variables not found");
            coarray_companions[sym] = {hsym, dsym};
            return {hsym, dsym};
        }

        PRIFInterface(Allocator &al_, ASR::TranslationUnit_t &unit_)
            : al(al_), unit(unit_) {
                saved_coarrays.reserve(al, 0);
            }

        std::string get_mangled_name(const std::string& module_name, const std::string& symbol_name) {
            return "__module_" + module_name + "_" + symbol_name;
        }

        // Create the prif_coarray_cleanup_interface Function symbol.
        // Represents: subroutine(prif_coarray_handle) bind(C)
        ASR::symbol_t* get_or_create_cleanup_interface(const Location &loc) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string iface_name = "prif_coarray_cleanup_interface";
            if (ASR::symbol_t *existing = global_scope->get_symbol(iface_name)) {
                return existing;
            }
            SymbolTable *iface_symtab = al.make_new<SymbolTable>(global_scope);
            ASR::symbol_t *handle_struct = get_or_create_prif_coarray_handle_struct(loc);
            ASR::expr_t *handle_arg = make_struct_var(
                iface_symtab, loc, "handle", handle_struct,
                ASR::intentType::In, ASR::presenceType::Required, true);
            Vec<ASR::expr_t*> iface_args; iface_args.reserve(al, 1);
            iface_args.push_back(al, handle_arg);
            ASR::asr_t *iface_fn = ASRUtils::make_Function_t_util(
                al, loc, iface_symtab, s2c(al, iface_name), nullptr, 0,
                iface_args.p, iface_args.n, nullptr, 0, nullptr,
                ASR::abiType::BindC, ASR::accessType::Public,
                ASR::deftypeType::Interface, nullptr,
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);
            global_scope->add_symbol(iface_name, ASR::down_cast<ASR::symbol_t>(iface_fn));
            return ASR::down_cast<ASR::symbol_t>(iface_fn);
        }

        ASR::symbol_t* get_or_create_prif_allocate_coarray_sub(const Location &loc) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string sym_name = get_mangled_name("prif", "prif_allocate_coarray");
            if (ASR::symbol_t *existing = global_scope->get_symbol(sym_name)) {
                return existing;
            }
            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *i64 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8));
            ASR::ttype_t *cptr = ASRUtils::TYPE(ASR::make_CPtr_t(al, loc));
            // lcobounds: integer(8), dimension(:), intent(in)
            Vec<ASR::dimension_t> dims; dims.reserve(al, 1);
            ASR::dimension_t d; d.loc = loc; d.m_start = nullptr; d.m_length = nullptr;
            dims.push_back(al, d);
            ASR::ttype_t *i64_arr = ASRUtils::make_Array_t_util(al, loc, i64, dims.p, dims.n);
            ASR::expr_t *lcobounds = b.Variable(fn_symtab, "lcobounds", i64_arr,
                ASR::intentType::In, nullptr, ASR::abiType::Source, true);
            ASR::expr_t *ucobounds = b.Variable(fn_symtab, "ucobounds", i64_arr,
                ASR::intentType::In, nullptr, ASR::abiType::Source, true);
            ASR::expr_t *size_arg = b.Variable(fn_symtab, "size_in_bytes", i64,
                ASR::intentType::In, nullptr, ASR::abiType::Source, true);
#if CAF_PRIF_VERSION >= 8
            // final_proc: procedure(prif_coarray_cleanup_interface), pointer, intent(in)
            ASR::symbol_t *handle_struct = get_or_create_prif_coarray_handle_struct(loc);
            ASR::symbol_t *cleanup_iface = get_or_create_cleanup_interface(loc);
            ASR::ttype_t *handle_type = ASRUtils::make_StructType_t_util(al, loc, handle_struct, true);
            Vec<ASR::ttype_t*> cleanup_arg_types; cleanup_arg_types.reserve(al, 1);
            cleanup_arg_types.push_back(al, handle_type);
            ASR::ttype_t *cleanup_func_type = ASRUtils::TYPE(ASR::make_FunctionType_t(
                al, loc, cleanup_arg_types.p, cleanup_arg_types.n,
                nullptr, ASR::abiType::BindC, ASR::deftypeType::Interface,
                nullptr, false, false, false, false, false, nullptr, 0, false));
            ASR::ttype_t *cleanup_ptr_type = ASRUtils::TYPE(
                ASR::make_Pointer_t(al, loc, cleanup_func_type));
            ASR::expr_t *final_proc = b.Variable(fn_symtab, "final_proc", cleanup_ptr_type,
                ASR::intentType::In, cleanup_iface, ASR::abiType::BindC, false);
#else
            ASR::expr_t *final_func = b.Variable(fn_symtab, "final_func", cptr,
                ASR::intentType::In, nullptr, ASR::abiType::Source, true);
#endif
            ASR::expr_t *handle_var = make_struct_var(fn_symtab, loc, "coarray_handle",
                handle_struct, ASR::intentType::Out, ASR::presenceType::Required, false);
            ASR::expr_t *alloc_mem = b.Variable(fn_symtab, "allocated_memory", cptr,
                ASR::intentType::Out, nullptr, ASR::abiType::Source, false);

            ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            ASR::symbol_t *stat_sym = declare_variable(
                fn_symtab, loc, "stat", int32_type, ASR::intentType::Out, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            ASR::expr_t *stat = ASRUtils::EXPR(ASR::make_Var_t(al, loc, stat_sym));

            ASR::ttype_t *errmsg_type = ASRUtils::TYPE(ASR::make_String_t(
                al, loc, 1, nullptr,
                ASR::string_length_kindType::AssumedLength,
                ASR::string_physical_typeType::DescriptorString));
            ASR::symbol_t *errmsg_sym = declare_variable(
                fn_symtab, loc, "errmsg", errmsg_type, ASR::intentType::InOut, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            ASR::expr_t *errmsg = ASRUtils::EXPR(ASR::make_Var_t(al, loc, errmsg_sym));

            ASR::ttype_t *errmsg_alloc_type = allocatable_deferred_string();
            ASR::symbol_t *errmsg_alloc_sym = declare_variable(
                fn_symtab, loc, "errmsg_alloc", errmsg_alloc_type, ASR::intentType::InOut, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            ASR::expr_t *errmsg_alloc = ASRUtils::EXPR(ASR::make_Var_t(al, loc, errmsg_alloc_sym));

            Vec<ASR::expr_t*> args; args.reserve(al, 9);
            args.push_back(al, lcobounds);
            args.push_back(al, ucobounds);
            args.push_back(al, size_arg);
#if CAF_PRIF_VERSION >= 8
            args.push_back(al, final_proc);
#else 
            args.push_back(al, final_func);
#endif
            args.push_back(al, handle_var);
            args.push_back(al, alloc_mem);
            args.push_back(al, stat);
            args.push_back(al, errmsg);
            args.push_back(al, errmsg_alloc);
            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, sym_name), nullptr, 0,
                args.p, args.n, nullptr, 0, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Interface,
                s2c(al, sym_name),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);
            global_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }
        
        ASR::symbol_t* get_or_create_prif_team_type_struct(const Location &loc) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string symbol_name = get_mangled_name("prif", "prif_team_type");
            if (ASR::symbol_t *existing = global_scope->get_symbol(symbol_name)) return existing;

            SymbolTable *struct_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);
            ASR::asr_t *struct_asr = ASR::make_Struct_t(
                al, loc, struct_symtab, s2c(al, symbol_name), nullptr,
                nullptr, 0, nullptr, 0, nullptr, 0,
                ASR::abiType::Source, ASR::accessType::Public,
                false, false, nullptr, 0, nullptr, nullptr, nullptr, 0);
            ASR::symbol_t *struct_sym = ASR::down_cast<ASR::symbol_t>(struct_asr);
            ASR::Struct_t *struct_t = ASR::down_cast<ASR::Struct_t>(struct_sym);
            global_scope->add_symbol(symbol_name, struct_sym);
            std::string type_info_symbol_name = get_mangled_name("prif", "prif_dummy_team_descriptor");
            ASR::symbol_t* type_info_sym = get_or_create_dummy_struct(loc, type_info_symbol_name);
            ASR::ttype_t* type_info_type = ASRUtils::make_StructType_t_util(al, loc, type_info_sym, true);
            ASR::ttype_t *info_ptr_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc, type_info_type ));
            declare_variable(struct_symtab, loc, "info", info_ptr_type, ASR::intentType::Local, type_info_sym, ASR::abiType::Source, ASR::accessType::Private, ASR::presenceType::Required, false);

            Vec<char*> members; members.reserve(al, 1);
            members.push_back(al, s2c(al, "info"));

            struct_t->m_members = members.p;
            struct_t->n_members = members.n;

            ASR::ttype_t *struct_type = ASRUtils::make_StructType_t_util(al, loc, struct_sym, true);
            struct_t->m_struct_signature = struct_type;

            return struct_sym;
        }


        ASR::symbol_t* get_or_create_prif_init_sub(const Location &loc) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string sym_name = get_mangled_name("prif", "prif_init");
            if (ASR::symbol_t *existing = global_scope->get_symbol(sym_name)) {
                return existing;
            }
            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *int32_type = int32;
            // exit_code: integer(c_int), intent(out), optional
            ASR::symbol_t *ec_sym = declare_variable(
                fn_symtab, loc, "exit_code", int32_type, ASR::intentType::Out, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false);
            ASR::expr_t *ec_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, ec_sym));
            Vec<ASR::expr_t*> args; args.reserve(al, 1);
            args.push_back(al, ec_expr);
            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, sym_name), nullptr, 0,
                args.p, args.n, nullptr, 0, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Interface,
                s2c(al, sym_name),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);
            global_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }

        ASR::symbol_t* get_or_create_prif_num_images_sub(const Location &loc) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string sym_name = get_mangled_name("prif", "prif_num_images");
            if (ASR::symbol_t *existing = global_scope->get_symbol(sym_name)) {
                return existing;
            }
            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *int32_type = int32;
            
            ASR::symbol_t *ni_sym = declare_variable(
                fn_symtab, loc, "num_images", int32_type, ASR::intentType::Out, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false);
            ASR::expr_t *ni_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, ni_sym));
            Vec<ASR::expr_t*> args; args.reserve(al, 1);
            args.push_back(al, ni_expr);
            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, sym_name), nullptr, 0,
                args.p, args.n, nullptr, 0, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Interface,
                s2c(al, sym_name),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);
            global_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }

        ASR::symbol_t* get_or_create_prif_this_image_no_coarray_sub(const Location &loc) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string sym_name = get_mangled_name("prif", "prif_this_image_no_coarray");
            if (ASR::symbol_t *existing = global_scope->get_symbol(sym_name)) {
                return existing;
            }
            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *int32_type = int32;

            ASR::symbol_t *team_type_sym = get_or_create_prif_team_type_struct(loc);
            ASR::ttype_t *team_type = ASRUtils::make_StructType_t_util(al, loc, team_type_sym, true);

            ASR::symbol_t *team_sym = declare_variable(
                fn_symtab, loc, "team", team_type, ASR::intentType::In, team_type_sym,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            ASR::expr_t *team_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, team_sym));

            ASR::symbol_t *ti_sym = declare_variable(
                fn_symtab, loc, "this_image", int32_type, ASR::intentType::Out, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false);
            ASR::expr_t *ti_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, ti_sym));

            Vec<ASR::expr_t*> args; args.reserve(al, 2);
            args.push_back(al, team_expr);
            args.push_back(al, ti_expr);

            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, sym_name), nullptr, 0,
                args.p, args.n, nullptr, 0, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Interface,
                s2c(al, sym_name),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);
            global_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }

        ASR::symbol_t* get_or_create_prif_stop_sub(const Location &loc) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string sym_name = get_mangled_name("prif", "prif_stop");
            if (ASR::symbol_t *existing = global_scope->get_symbol(sym_name)) {
                return existing;
            }
            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 1));
            ASR::ttype_t *int32_type = int32;
            ASR::ttype_t *str_type = ASRUtils::TYPE(ASR::make_String_t(
                al, loc, 1, nullptr,
                ASR::string_length_kindType::AssumedLength,
                ASR::string_physical_typeType::DescriptorString));
            ASR::expr_t *quiet = b.Variable(fn_symtab, "quiet", logical_type,
                ASR::intentType::In, nullptr, ASR::abiType::Source, true);
            // stop_code_int: integer(c_int), intent(in), optional
            ASR::symbol_t *sci_sym = declare_variable(
                fn_symtab, loc, "stop_code_int", int32_type, ASR::intentType::In, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, true);
            ASR::expr_t *sci = ASRUtils::EXPR(ASR::make_Var_t(al, loc, sci_sym));
            // stop_code_char: character(*), intent(in), optional
            ASR::symbol_t *scc_sym = declare_variable(
                fn_symtab, loc, "stop_code_char", str_type, ASR::intentType::In, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, true);
            ASR::expr_t *scc = ASRUtils::EXPR(ASR::make_Var_t(al, loc, scc_sym));
            Vec<ASR::expr_t*> args; args.reserve(al, 3);
            args.push_back(al, quiet);
            args.push_back(al, sci);
            args.push_back(al, scc);
            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, sym_name), nullptr, 0,
                args.p, args.n, nullptr, 0, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Interface,
                s2c(al, sym_name),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);
            global_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }

        ASR::symbol_t* get_or_create_prif_sync_all_sub(const Location &loc) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string sym_name = get_mangled_name("prif", "prif_sync_all");
            if (ASR::symbol_t *existing = global_scope->get_symbol(sym_name)) {
                return existing;
            }
            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *int32_type = int32;
            ASR::ttype_t *str_type = ASRUtils::TYPE(ASR::make_String_t(
                al, loc, 1, nullptr,
                ASR::string_length_kindType::AssumedLength,
                ASR::string_physical_typeType::DescriptorString));
            ASR::ttype_t *alloc_str_type = allocatable_deferred_string();

            ASR::symbol_t *stat_sym = declare_variable(
                fn_symtab, loc, "stat", int32_type, ASR::intentType::Out, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            ASR::expr_t *stat = ASRUtils::EXPR(ASR::make_Var_t(al, loc, stat_sym));

            ASR::symbol_t *errmsg_sym = declare_variable(
                fn_symtab, loc, "errmsg", str_type, ASR::intentType::InOut, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            ASR::expr_t *errmsg = ASRUtils::EXPR(ASR::make_Var_t(al, loc, errmsg_sym));

            ASR::symbol_t *errmsg_alloc_sym = declare_variable(
                fn_symtab, loc, "errmsg_alloc", alloc_str_type, ASR::intentType::InOut, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            ASR::expr_t *errmsg_alloc = ASRUtils::EXPR(ASR::make_Var_t(al, loc, errmsg_alloc_sym));

            Vec<ASR::expr_t*> args; args.reserve(al, 3);
            args.push_back(al, stat);
            args.push_back(al, errmsg);
            args.push_back(al, errmsg_alloc);

            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, sym_name), nullptr, 0,
                args.p, args.n, nullptr, 0, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Interface,
                s2c(al, sym_name),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);
            global_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }
        void declare_prif_status_args(SymbolTable *fn_symtab, const Location &loc, Vec<ASR::expr_t*> &args) {
            ASR::ttype_t *str_type = ASRUtils::TYPE(ASR::make_String_t(
                al, loc, 1, nullptr,
                ASR::string_length_kindType::AssumedLength,
                ASR::string_physical_typeType::DescriptorString));

            ASR::symbol_t *stat_sym = declare_variable(
                fn_symtab, loc, "stat", int32, ASR::intentType::Out, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, stat_sym)));

            ASR::symbol_t *errmsg_sym = declare_variable(
                fn_symtab, loc, "errmsg", str_type, ASR::intentType::InOut, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, errmsg_sym)));

            ASR::symbol_t *errmsg_alloc_sym = declare_variable(
                fn_symtab, loc, "errmsg_alloc", allocatable_deferred_string(), ASR::intentType::InOut, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, errmsg_alloc_sym)));
        }

        ASR::symbol_t* get_or_create_prif_co_minmaxsum_sub(const Location &loc, const std::string &prif_name) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string sym_name = get_mangled_name("prif", prif_name);
            if (ASR::symbol_t *existing = global_scope->get_symbol(sym_name)) {
                return existing;
            }
            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *int32_type = int32;

            std::string derived_type_name = "~assumed_type";
            ASR::symbol_t *type_declaration = global_scope->resolve_symbol(derived_type_name);
            if (!type_declaration) {
                SymbolTable *struct_symtab = al.make_new<SymbolTable>(global_scope);
                ASR::asr_t* dtype = ASR::make_Struct_t(al, loc, struct_symtab,
                                                s2c(al, derived_type_name), nullptr, nullptr, 0, nullptr, 0,
                                                nullptr, 0, ASR::abiType::Source, ASR::accessType::Public, false, true,
                                                nullptr, 0, nullptr, nullptr, nullptr, 0);
                ASR::symbol_t* struct_symbol = ASR::down_cast<ASR::symbol_t>(dtype);
                ASR::ttype_t* struct_type = ASRUtils::make_StructType_t_util(al, loc, struct_symbol, false);
                ASR::Struct_t* struct_ = ASR::down_cast<ASR::Struct_t>(struct_symbol);
                struct_->m_struct_signature = struct_type;
                type_declaration = struct_symbol;
                global_scope->add_symbol(derived_type_name, type_declaration);
            }
            ASR::ttype_t * assumed_type = ASRUtils::make_StructType_t_util(al, loc, type_declaration, false);
            ASR::ttype_t * a_type_assumed = ASRUtils::TYPE(ASR::make_Array_t(al, loc, assumed_type, nullptr, 0, ASR::array_physical_typeType::AssumedRankArray));

            ASR::symbol_t *a_sym = declare_variable(
                fn_symtab, loc, "a", a_type_assumed, ASR::intentType::InOut, type_declaration,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false);
            ASR::down_cast<ASR::Variable_t>(a_sym)->m_target_attr = true;
            ASR::expr_t *a = ASRUtils::EXPR(ASR::make_Var_t(al, loc, a_sym));

            ASR::symbol_t *res_img_sym = declare_variable(
                fn_symtab, loc, "result_image", int32_type, ASR::intentType::In, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            ASR::expr_t *res_img = ASRUtils::EXPR(ASR::make_Var_t(al, loc, res_img_sym));

            Vec<ASR::expr_t*> args; args.reserve(al, 5);
            args.push_back(al, a);
            args.push_back(al, res_img);
            declare_prif_status_args(fn_symtab, loc, args);

            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, sym_name), nullptr, 0,
                args.p, args.n, nullptr, 0, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Interface,
                s2c(al, sym_name),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);
            global_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }

        ASR::symbol_t* get_or_create_prif_co_minmax_character_sub(const Location &loc, const std::string &prif_name) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string sym_name = get_mangled_name("prif", prif_name);
            if (ASR::symbol_t *existing = global_scope->get_symbol(sym_name)) {
                return existing;
            }
            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *int32_type = int32;

            ASR::ttype_t *a_char_type = ASRUtils::TYPE(ASR::make_String_t(
                al, loc, 1, nullptr,
                ASR::string_length_kindType::AssumedLength,
                ASR::string_physical_typeType::DescriptorString));
            ASR::ttype_t *a_type_assumed = ASRUtils::TYPE(ASR::make_Array_t(
                al, loc, a_char_type, nullptr, 0,
                ASR::array_physical_typeType::AssumedRankArray));

            ASR::symbol_t *a_sym = declare_variable(
                fn_symtab, loc, "a", a_type_assumed, ASR::intentType::InOut, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false);
            ASR::down_cast<ASR::Variable_t>(a_sym)->m_target_attr = true;
            ASR::expr_t *a = ASRUtils::EXPR(ASR::make_Var_t(al, loc, a_sym));

            ASR::symbol_t *res_img_sym = declare_variable(
                fn_symtab, loc, "result_image", int32_type, ASR::intentType::In, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Optional, false);
            ASR::expr_t *res_img = ASRUtils::EXPR(ASR::make_Var_t(al, loc, res_img_sym));

            Vec<ASR::expr_t*> args; args.reserve(al, 5);
            args.push_back(al, a);
            args.push_back(al, res_img);
            declare_prif_status_args(fn_symtab, loc, args);

            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, sym_name), nullptr, 0,
                args.p, args.n, nullptr, 0, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Interface,
                s2c(al, sym_name),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);
            global_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }

        ASR::stmt_t* make_prif_co_minmaxsum_call(const Location &loc, const std::string &prif_name,
                                           ASR::expr_t *a,
                                           ASR::expr_t *result_image = nullptr,
                                           ASR::expr_t *stat = nullptr,
                                           ASR::expr_t *errmsg = nullptr,
                                           ASR::expr_t *errmsg_alloc = nullptr) {
            ASR::ttype_t *a_type = ASRUtils::expr_type(a);
            ASR::symbol_t *sub = ASRUtils::is_character(*a_type)
                                        ? get_or_create_prif_co_minmax_character_sub(loc, prif_name + "_character")
                                        : get_or_create_prif_co_minmaxsum_sub(loc, prif_name);
            Vec<ASR::call_arg_t> call_args; call_args.reserve(al, 5);

            ASR::call_arg_t arg1; arg1.loc = loc; arg1.m_value = a;
            ASR::call_arg_t arg2; arg2.loc = loc; arg2.m_value = result_image;
            ASR::call_arg_t arg3; arg3.loc = loc; arg3.m_value = stat;
            ASR::call_arg_t arg4; arg4.loc = loc; arg4.m_value = errmsg;
            ASR::call_arg_t arg5; arg5.loc = loc; arg5.m_value = errmsg_alloc;

            call_args.push_back(al, arg1);
            call_args.push_back(al, arg2);
            call_args.push_back(al, arg3);
            call_args.push_back(al, arg4);
            call_args.push_back(al, arg5);

            return ASRUtils::STMT(ASR::make_SubroutineCall_t(
                al, loc, sub, nullptr, call_args.p, call_args.n, nullptr, false));
        }



        ASR::stmt_t* make_prif_sync_all_call(const Location &loc,
                                             ASR::expr_t *stat = nullptr,
                                             ASR::expr_t *errmsg = nullptr,
                                             ASR::expr_t *errmsg_alloc = nullptr) {
            ASR::symbol_t *sub = get_or_create_prif_sync_all_sub(loc);
            Vec<ASR::call_arg_t> call_args; call_args.reserve(al, 3);

            ASR::call_arg_t arg1; arg1.loc = loc; arg1.m_value = stat;
            ASR::call_arg_t arg2; arg2.loc = loc; arg2.m_value = errmsg;
            ASR::call_arg_t arg3; arg3.loc = loc; arg3.m_value = errmsg_alloc;

            call_args.push_back(al, arg1);
            call_args.push_back(al, arg2);
            call_args.push_back(al, arg3);

            return ASRUtils::STMT(ASR::make_SubroutineCall_t(
                al, loc, sub, nullptr, call_args.p, call_args.n, nullptr, false));
        }
        
        void declare_coarray_companions(SymbolTable *scope, const Location &loc) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *cptr = b.CPtr();
            ASR::symbol_t *handle_struct = get_or_create_prif_coarray_handle_struct(loc);
            
            for (auto &item : scope->get_scope()) {
                ASR::symbol_t *sym = item.second;
                if (!ASR::is_a<ASR::Variable_t>(*sym)) continue;
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
                if (var->n_codims == 0) continue;
                
                // If it's already a Pointer, it might have been processed by a previous pass
                if (ASRUtils::is_pointer(var->m_type)) continue;

                LCOMPILERS_ASSERT_MSG(!(ASRUtils::is_allocatable(var->m_type)), "Allocatable coarrays are not yet supported");

                std::string vname = var->m_name;
                bool is_save = (var->m_storage == ASR::storage_typeType::Save);
                SymbolTable *companion_scope = scope;
                std::string hname = vname + "__coarray_handle";
                std::string dname = vname + "__coarray_data";

                bool is_module = (scope->asr_owner && ASR::is_a<ASR::symbol_t>(*scope->asr_owner) && ASR::is_a<ASR::Module_t>(*ASR::down_cast<ASR::symbol_t>(scope->asr_owner)));

                if (is_save && scope != unit.m_symtab && !is_module) {
                    companion_scope = unit.m_symtab;
                    hname = companion_scope->get_unique_name(vname + "__coarray_handle");
                    dname = companion_scope->get_unique_name(vname + "__coarray_data");
                }

                ASR::ttype_t *ht = ASRUtils::make_StructType_t_util(al, loc, handle_struct, true);
                ASR::symbol_t *handle_sym = declare_variable(
                    companion_scope, loc, hname, ht, ASR::intentType::Local, handle_struct,
                    ASR::abiType::Source, ASR::accessType::Public,
                    ASR::presenceType::Required, false);

                ASR::symbol_t *data_sym = declare_variable(
                    companion_scope, loc, dname, cptr, ASR::intentType::Local, nullptr,
                    ASR::abiType::Source, ASR::accessType::Public,
                    ASR::presenceType::Required, false);

                coarray_companions[sym] = {handle_sym, data_sym};

                if (is_save) {
                    SavedCoarray sc;
                    sc.var = var;
                    sc.handle_sym = handle_sym;
                    sc.data_sym = data_sym;
                    sc.init_value = var->m_value;
                    sc.loc = loc;
                    saved_coarrays.push_back(al, sc);
                    var->m_value = nullptr;
                    var->m_symbolic_value = nullptr;
                }

                ASR::ttype_t *orig_type = var->m_type;
                ASR::ttype_t *ptr_type = ASRUtils::TYPE(
                    ASR::make_Pointer_t(al, loc, orig_type));
                var->m_type = ptr_type;
            }
        }

        void emit_allocate_call(ASR::Variable_t *var, ASR::expr_t *hexpr, ASR::expr_t *dexpr,
                                ASR::symbol_t *alloc_sub, ASR::symbol_t *handle_struct,
                                ASR::ttype_t *i64, const Location &loc,
                                Vec<ASR::stmt_t*> &new_body) {
            ASRUtils::ASRBuilder b(al, loc);
            if (var->n_codims <= 0) {
                throw LCompilersException("Coarray variable must have codimensions");
            }
            int64_t corank = var->n_codims;
            Vec<ASR::expr_t*> lco_elems; lco_elems.reserve(al, corank);
            Vec<ASR::expr_t*> uco_elems; uco_elems.reserve(al, corank > 1 ? corank - 1 : 0);
            for (int64_t ci = 0; ci < corank; ci++) {
                int64_t lb = 1;
                if (ci < (int64_t)var->n_codims && var->m_codims[ci].m_start) {
                    ASRUtils::extract_value(var->m_codims[ci].m_start, lb);
                }
                lco_elems.push_back(al, b.i64(lb));
                if (ci == corank - 1) {
                    LCOMPILERS_ASSERT(var->m_codims[ci].m_end == nullptr);
                }
                if (ci < corank - 1 && ci < (int64_t)var->n_codims
                    && var->m_codims[ci].m_end) {
                    int64_t ub = 0;
                    ASRUtils::extract_value(var->m_codims[ci].m_end, ub);
                    uco_elems.push_back(al, b.i64(ub));
                }
            }
            
            Vec<ASR::dimension_t> arr_d; arr_d.reserve(al, 1);
            ASR::dimension_t ad; ad.loc = loc;
            ad.m_start = b.i32(1);
            ad.m_length = b.i32(static_cast<int64_t>(lco_elems.n));
            arr_d.push_back(al, ad);
            std::vector<ASR::expr_t*> lco_vec;
            for (size_t i = 0; i < lco_elems.n; i++) {
                lco_vec.push_back(lco_elems.p[i]);
            }
            ASR::expr_t *lcobounds_val = b.ArrayConstant(lco_vec, i64, false);
            
            Vec<ASR::dimension_t> arr_d0; arr_d0.reserve(al, 1);
            ASR::dimension_t ad0; ad0.loc = loc;
            ad0.m_start = b.i32(1);
            ad0.m_length = b.i32(static_cast<int64_t>(uco_elems.n));
            arr_d0.push_back(al, ad0);
            std::vector<ASR::expr_t*> uco_vec;
            for (size_t i = 0; i < uco_elems.n; i++) {
                uco_vec.push_back(uco_elems.p[i]);
            }
            ASR::expr_t *ucobounds_val = b.ArrayConstant(uco_vec, i64, false);
            
            ASR::ttype_t *base_type = ASRUtils::type_get_past_allocatable_pointer(var->m_type);
            ASR::expr_t *sz = get_size_in_bytes_expr(loc, base_type);
            
            ASR::ttype_t *handle_type_fp = ASRUtils::make_StructType_t_util(
                al, loc, handle_struct, true);
            Vec<ASR::ttype_t*> fp_arg_types; fp_arg_types.reserve(al, 1);
            fp_arg_types.push_back(al, handle_type_fp);
            ASR::ttype_t *cleanup_ft = ASRUtils::TYPE(ASR::make_FunctionType_t(
                al, loc, fp_arg_types.p, fp_arg_types.n,
                nullptr, ASR::abiType::BindC, ASR::deftypeType::Interface,
                nullptr, false, false, false, false, false, nullptr, 0, false));
            ASR::expr_t *null_fptr = ASRUtils::EXPR(
                ASR::make_PointerNullConstant_t(al, loc, cleanup_ft, nullptr));
            
            Vec<ASR::call_arg_t> call_args; call_args.reserve(al, 9);
            ASR::call_arg_t a1; a1.loc=loc; a1.m_value=lcobounds_val;
            ASR::call_arg_t a2; a2.loc=loc; a2.m_value=ucobounds_val;
            ASR::call_arg_t a3; a3.loc=loc; a3.m_value=sz;
            ASR::call_arg_t a4; a4.loc=loc; a4.m_value=null_fptr;
            ASR::call_arg_t a5; a5.loc=loc; a5.m_value=hexpr;
            ASR::call_arg_t a6; a6.loc=loc; a6.m_value=dexpr;
            call_args.push_back(al, a1);
            call_args.push_back(al, a2);
            call_args.push_back(al, a3);
            call_args.push_back(al, a4);
            call_args.push_back(al, a5);
            call_args.push_back(al, a6);
            ASR::stmt_t *call_stmt = ASRUtils::STMT(ASR::make_SubroutineCall_t(
                al, loc, alloc_sub, nullptr,
                call_args.p, call_args.n, nullptr, false));
            new_body.push_back(al, call_stmt);
        }

        void allocate_coarrays(SymbolTable *scope, SymbolTable *body_scope, const Location &loc,
                                    Vec<ASR::stmt_t*> &new_body) {
            ASRUtils::ASRBuilder b(al, loc);
            bool initialized = false;
            ASR::ttype_t *i64 = nullptr;
            ASR::symbol_t *handle_struct = nullptr;
            ASR::symbol_t *alloc_sub = nullptr;
            for (auto &item : scope->get_scope()) {
                ASR::symbol_t *sym = item.second;
                if (!ASR::is_a<ASR::Variable_t>(*sym)) continue;
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
                if (var->n_codims == 0) continue;

                auto companions = get_coarray_companions(sym);
                ASR::symbol_t *hsym_orig = companions.first;
                ASR::symbol_t *dsym_orig = companions.second;

                if (var->m_storage == ASR::storage_typeType::Save) {
                    ASR::symbol_t *dsym_use = get_symbol_in_scope(ASRUtils::symbol_parent_symtab(dsym_orig), body_scope, dsym_orig, loc);
                    ASR::symbol_t *sym_use = get_symbol_in_scope(scope, body_scope, sym, loc);
                    ASR::expr_t *dexpr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, dsym_use));
                    ASR::expr_t *var_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym_use));
                    ASR::stmt_t *cfp_stmt = ASRUtils::STMT(
                        ASR::make_CPtrToPointer_t(al, loc, dexpr, var_expr, nullptr, nullptr));
                    new_body.push_back(al, cfp_stmt);
                    continue;
                }

                if (!initialized) {
                    i64 = int64;
                    handle_struct = get_or_create_prif_coarray_handle_struct(loc);
                    alloc_sub = get_or_create_prif_allocate_coarray_sub(loc);
                    initialized = true;
                }

                ASR::symbol_t *hsym_use = get_symbol_in_scope(ASRUtils::symbol_parent_symtab(hsym_orig), body_scope, hsym_orig, loc);
                ASR::symbol_t *dsym_use = get_symbol_in_scope(ASRUtils::symbol_parent_symtab(dsym_orig), body_scope, dsym_orig, loc);
                ASR::symbol_t *sym_use = get_symbol_in_scope(scope, body_scope, sym, loc);

                ASR::expr_t *hexpr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, hsym_use));
                ASR::expr_t *dexpr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, dsym_use));

                emit_allocate_call(var, hexpr, dexpr, alloc_sub, handle_struct, i64, loc, new_body);

                ASR::expr_t *var_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym_use));
                ASR::stmt_t *cfp_stmt = ASRUtils::STMT(
                    ASR::make_CPtrToPointer_t(al, loc, dexpr, var_expr, nullptr, nullptr));
                new_body.push_back(al, cfp_stmt);

                if (var->m_value) {
                    ASR::stmt_t *assign = ASRUtils::STMT(ASR::make_Assignment_t(
                        al, loc, var_expr, var->m_value, nullptr, false, false));
                    new_body.push_back(al, assign);
                    var->m_value = nullptr;
                }
                if (var->m_symbolic_value) {
                    var->m_symbolic_value = nullptr;
                }
            }
        }

        // names of saved coarrays to avoid linker collisions.
        std::string get_tu_init_function_name() {
            std::string fn_name = "__lfortran_coarray_init";
            std::set<std::string> parent_names;
            for (size_t i = 0; i < saved_coarrays.n; i++) {
                SymbolTable *var_scope = ASRUtils::symbol_parent_symtab(
                    &saved_coarrays.p[i].var->base);
                if (var_scope->asr_owner &&
                    ASR::is_a<ASR::symbol_t>(*var_scope->asr_owner)) {
                    parent_names.insert(ASRUtils::symbol_name(
                        ASR::down_cast<ASR::symbol_t>(var_scope->asr_owner)));
                }
            }
            for (const auto &pn : parent_names) {
                fn_name += "_" + pn;
            }
            return fn_name;
        }

        // Emit a prif_init(exit_code) call into body, declaring exit_code
        // in the given scope. Reusable by both generate_tu_init_function
        // and visit_Program.
        void emit_prif_init_call(SymbolTable *scope, const Location &loc,
                                 Vec<ASR::stmt_t*> &body) {
            ASR::ttype_t *int32_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));
            ASR::symbol_t *ec_sym = declare_variable(
                scope, loc, "stat", int32_type,
                ASR::intentType::Local, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false);
            ASR::expr_t *ec_expr = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, ec_sym));
            ASR::symbol_t *init_sub = get_or_create_prif_init_sub(loc);
            Vec<ASR::call_arg_t> init_args; init_args.reserve(al, 1);
            ASR::call_arg_t ec_arg; ec_arg.loc = loc;
            ec_arg.m_value = ec_expr;
            init_args.push_back(al, ec_arg);
            body.push_back(al, ASRUtils::STMT(ASR::make_SubroutineCall_t(
                al, loc, init_sub, nullptr,
                init_args.p, init_args.n, nullptr, false)));
        }

        // Generate a per-TU init function that allocates all saved coarrays.
        // Registered via @llvm.global_ctors by the LLVM backend so it runs
        // automatically before main(), making saved coarray allocation work
        // across separate compilation units.
        void generate_tu_init_function(const Location &loc) {
            if (saved_coarrays.n == 0) return;

            SymbolTable *global_scope = unit.m_symtab;
            std::string fn_name = get_tu_init_function_name();

            // Avoid creating duplicate if already present
            if (global_scope->get_symbol(fn_name)) return;

            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);

            ASR::ttype_t *i64 = int64;
            ASR::symbol_t *handle_struct = get_or_create_prif_coarray_handle_struct(loc);
            ASR::symbol_t *alloc_sub = get_or_create_prif_allocate_coarray_sub(loc);

            Vec<ASR::stmt_t*> body;
            body.reserve(al, saved_coarrays.n * 3 + 1);

            // prif_init() must run first since @llvm.global_ctors executes
            // before main(), before the program body's own prif_init call.
            emit_prif_init_call(fn_symtab, loc, body);

            for (size_t i = 0; i < saved_coarrays.n; i++) {
                ASR::Variable_t *var = saved_coarrays.p[i].var;
                ASR::symbol_t *hsym_orig = saved_coarrays.p[i].handle_sym;
                ASR::symbol_t *dsym_orig = saved_coarrays.p[i].data_sym;
                ASR::expr_t *init_value = saved_coarrays.p[i].init_value;

                ASR::symbol_t *hsym_use = get_symbol_in_scope(
                    global_scope, fn_symtab, hsym_orig, loc);
                ASR::symbol_t *dsym_use = get_symbol_in_scope(
                    global_scope, fn_symtab, dsym_orig, loc);

                ASR::expr_t *hexpr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, hsym_use));
                ASR::expr_t *dexpr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, dsym_use));

                emit_allocate_call(var, hexpr, dexpr, alloc_sub, handle_struct, i64, loc, body);

                if (init_value != nullptr) {
                    int64_t val = 0;
                    if (ASRUtils::extract_value(init_value, val)) {
                        LCOMPILERS_ASSERT_MSG(val == 0, "Initialization of coarrays is not yet supported");
                    }
                }
                // If the saved coarray had an initial value (e.g., x[*] = 0),
                // bind the data pointer to a local variable and assign the value.
                // if (init_value) {
                //     std::string local_name = std::string(var->m_name) + "__init_ptr";
                //     ASR::ttype_t *var_ptr_type = var->m_type; // already Pointer_t
                //     ASR::symbol_t *local_sym = declare_variable(
                //         fn_symtab, loc, local_name, var_ptr_type,
                //         ASR::intentType::Local, nullptr,
                //         ASR::abiType::Source, ASR::accessType::Public,
                //         ASR::presenceType::Required, false);
                //     ASR::expr_t *local_expr = ASRUtils::EXPR(
                //         ASR::make_Var_t(al, loc, local_sym));

                //     body.push_back(al, ASRUtils::STMT(
                //         ASR::make_CPtrToPointer_t(al, loc, dexpr, local_expr,
                //                                   nullptr, nullptr)));
                //     body.push_back(al, ASRUtils::STMT(
                //         ASR::make_Assignment_t(al, loc, local_expr, init_value,
                //                               nullptr, false, false)));
                // }
            }

            Vec<char*> deps; deps.reserve(al, 2);
            deps.push_back(al, s2c(al, get_mangled_name("prif", "prif_init")));
            deps.push_back(al, s2c(al, get_mangled_name("prif", "prif_allocate_coarray")));

            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, fn_name), deps.p, deps.n,
                nullptr, 0, body.p, body.n, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Implementation, nullptr,
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);

            global_scope->add_symbol(fn_name, ASR::down_cast<ASR::symbol_t>(fn));
        }

        ASR::expr_t* make_prif_get_call(const Location &loc,
                                        ASR::expr_t *base_expr,
                                        ASR::array_index_t *coindices,
                                        size_t n_coindices,
                                        ASR::ttype_t *return_type) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *int64_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8));

            Vec<ASR::expr_t*> sub_elems; sub_elems.reserve(al, n_coindices == 0 ? 1 : n_coindices);
            if (n_coindices == 0) {
                LCOMPILERS_ASSERT(false);
            } else {
                for (size_t i = 0; i < n_coindices; i++) {
                    if (coindices[i].m_left) {
                        sub_elems.push_back(al, b.i2i_t(coindices[i].m_left, int64_type));
                    } else {
                        LCOMPILERS_ASSERT(false);
                    }
                }
            }

            Vec<ASR::dimension_t> arr_d; arr_d.reserve(al, 1);
            ASR::dimension_t ad; ad.loc = loc;
            ad.m_start = b.i32(1);
            ad.m_length = b.i32(static_cast<int64_t>(sub_elems.n));
            arr_d.push_back(al, ad);
            ASR::ttype_t *sub_arr_t = ASRUtils::make_Array_t_util(al, loc, int64_type, arr_d.p, arr_d.n);
            std::vector<ASR::expr_t*> sub_vec;
            for (size_t i = 0; i < sub_elems.n; i++) {
                sub_vec.push_back(sub_elems.p[i]);
            }
            ASR::expr_t *sub = b.ArrayConstant(sub_vec, int64_type, false, sub_arr_t);

            ASR::expr_t *handle_base = get_handle_base_expr(base_expr);
            ASR::expr_t *coarray_handle = make_prif_handle_expr(loc, handle_base);
            ASR::symbol_t *wrapper = get_or_create_prif_get_wrapper(loc, return_type, base_expr);

            ASR::expr_t *size_in_bytes = get_size_in_bytes_expr(loc, return_type);
            ASR::expr_t *offset = compute_offset_bytes(loc, base_expr, size_in_bytes);

            Vec<ASR::call_arg_t> args;
            args.reserve(al, 3);
            ASR::call_arg_t handle_arg; handle_arg.loc = loc; handle_arg.m_value = coarray_handle;
            ASR::call_arg_t sub_arg; sub_arg.loc = loc; sub_arg.m_value = sub;
            ASR::call_arg_t offset_arg; offset_arg.loc = loc; offset_arg.m_value = offset;
            args.push_back(al, handle_arg);
            args.push_back(al, sub_arg);
            args.push_back(al, offset_arg);

            return ASRUtils::EXPR(ASR::make_FunctionCall_t(
                al, loc, wrapper, wrapper,
                args.p, args.n, return_type, nullptr, nullptr));
        }

        ASR::stmt_t* make_prif_put_call(const Location &loc,
                                        ASR::expr_t *base_expr,
                                        ASR::array_index_t *coindices,
                                        size_t n_coindices,
                                        ASR::expr_t *value_expr) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *int64_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8));

            Vec<ASR::expr_t*> sub_elems; sub_elems.reserve(al, n_coindices == 0 ? 1 : n_coindices);
            if (n_coindices == 0) {
                LCOMPILERS_ASSERT(false);
            } else {
                for (size_t i = 0; i < n_coindices; i++) {
                    if (coindices[i].m_left) {
                        sub_elems.push_back(al, b.i2i_t(coindices[i].m_left, int64_type));
                    } else {
                        LCOMPILERS_ASSERT(false);
                    }
                }
            }

            Vec<ASR::dimension_t> arr_d; arr_d.reserve(al, 1);
            ASR::dimension_t ad; ad.loc = loc;
            ad.m_start = b.i32(1);
            ad.m_length = b.i32(static_cast<int64_t>(sub_elems.n));
            arr_d.push_back(al, ad);
            ASR::ttype_t *sub_arr_t = ASRUtils::make_Array_t_util(al, loc, int64_type, arr_d.p, arr_d.n);
            std::vector<ASR::expr_t*> sub_vec;
            for (size_t i = 0; i < sub_elems.n; i++) {
                sub_vec.push_back(sub_elems.p[i]);
            }
            ASR::expr_t *sub = b.ArrayConstant(sub_vec, int64_type, false, sub_arr_t);

            ASR::expr_t *handle_base = get_handle_base_expr(base_expr);
            ASR::expr_t *coarray_handle = make_prif_handle_expr(loc, handle_base);

            ASR::ttype_t *value_type = ASRUtils::expr_type(value_expr);
            ASR::symbol_t *wrapper = get_or_create_prif_put_wrapper(loc, value_type, base_expr);

            ASR::expr_t *size_in_bytes = get_size_in_bytes_expr(loc, value_type);
            ASR::expr_t *offset = compute_offset_bytes(loc, base_expr, size_in_bytes);

            Vec<ASR::call_arg_t> args;
            args.reserve(al, 4);
            ASR::call_arg_t handle_arg; handle_arg.loc = loc; handle_arg.m_value = coarray_handle;
            ASR::call_arg_t sub_arg; sub_arg.loc = loc; sub_arg.m_value = sub;
            ASR::call_arg_t offset_arg; offset_arg.loc = loc; offset_arg.m_value = offset;
            ASR::call_arg_t value_arg; value_arg.loc = loc; value_arg.m_value = value_expr;

            args.push_back(al, handle_arg);
            args.push_back(al, sub_arg);
            args.push_back(al, offset_arg);
            args.push_back(al, value_arg);

            return ASRUtils::STMT(ASR::make_SubroutineCall_t(
                al, loc, wrapper, nullptr,
                args.p, args.n, nullptr, false));
        }

        ASR::expr_t* make_prif_num_images_call(const Location &loc, ASR::ttype_t *type) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string sym_name = "lcompilers_prif_num_images";
            std::string dep_name = get_mangled_name("prif", "prif_num_images");
            ASR::symbol_t *wrapper_fn = global_scope->get_symbol(sym_name);
            if (!wrapper_fn) {
                SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
                ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));

                ASR::symbol_t *ret_sym = declare_variable(
                    fn_symtab, loc, sym_name, int32_type, ASR::intentType::ReturnVar, nullptr,
                    ASR::abiType::Source, ASR::accessType::Public, ASR::presenceType::Required, false);

                ASR::symbol_t *sub = get_or_create_prif_num_images_sub(loc);
                Vec<ASR::call_arg_t> call_args; call_args.reserve(al, 1);
                ASR::call_arg_t arg; arg.loc = loc;
                arg.m_value = ASRUtils::EXPR(ASR::make_Var_t(al, loc, ret_sym));
                call_args.push_back(al, arg);

                Vec<ASR::stmt_t*> body; body.reserve(al, 1);
                body.push_back(al, ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(
                    al, loc, sub, sub, call_args.p, call_args.n, nullptr, nullptr, false)));

                Vec<char*> deps;
                deps.reserve(al, 1);
                deps.push_back(al, s2c(al, dep_name));

                ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                    al, loc, fn_symtab, s2c(al, sym_name), deps.p, deps.n,
                    nullptr, 0, body.p, body.n, ASRUtils::EXPR(ASR::make_Var_t(al, loc, ret_sym)),
                    ASR::abiType::Source, ASR::accessType::Public,
                    ASR::deftypeType::Implementation, nullptr,
                    false, false, false, false, false, nullptr, 0,
                    false, false, false, nullptr);
                wrapper_fn = ASR::down_cast<ASR::symbol_t>(fn);
                global_scope->add_symbol(sym_name, wrapper_fn);
            }
            return ASRUtils::EXPR(ASR::make_FunctionCall_t(
                al, loc, wrapper_fn, wrapper_fn, nullptr, 0,
                type, nullptr, nullptr));
        }

        ASR::expr_t* make_prif_this_image_call(const Location &loc, ASR::ttype_t *type) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string sym_name = "lcompilers_prif_this_image";
            std::string dep_name = get_mangled_name("prif", "prif_this_image_no_coarray");
            ASR::symbol_t *wrapper_fn = global_scope->get_symbol(sym_name);
            if (!wrapper_fn) {
                SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
                ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));

                ASR::symbol_t *ret_sym = declare_variable(
                    fn_symtab, loc, sym_name, int32_type, ASR::intentType::ReturnVar, nullptr,
                    ASR::abiType::Source, ASR::accessType::Public, ASR::presenceType::Required, false);

                ASR::symbol_t *sub = get_or_create_prif_this_image_no_coarray_sub(loc);
                Vec<ASR::call_arg_t> call_args; call_args.reserve(al, 2);

                ASR::call_arg_t team_arg; team_arg.loc = loc;
                team_arg.m_value = nullptr;
                call_args.push_back(al, team_arg);

                ASR::call_arg_t arg; arg.loc = loc;
                arg.m_value = ASRUtils::EXPR(ASR::make_Var_t(al, loc, ret_sym));
                call_args.push_back(al, arg);

                Vec<ASR::stmt_t*> body; body.reserve(al, 1);
                body.push_back(al, ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(
                    al, loc, sub, sub, call_args.p, call_args.n, nullptr, nullptr, false)));

                Vec<char*> deps;
                deps.reserve(al, 1);
                deps.push_back(al, s2c(al, dep_name));

                ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                    al, loc, fn_symtab, s2c(al, sym_name), deps.p, deps.n,
                    nullptr, 0, body.p, body.n, ASRUtils::EXPR(ASR::make_Var_t(al, loc, ret_sym)),
                    ASR::abiType::Source, ASR::accessType::Public,
                    ASR::deftypeType::Implementation, nullptr,
                    false, false, false, false, false, nullptr, 0,
                    false, false, false, nullptr);
                wrapper_fn = ASR::down_cast<ASR::symbol_t>(fn);
                global_scope->add_symbol(sym_name, wrapper_fn);
            }
            return ASRUtils::EXPR(ASR::make_FunctionCall_t(
                al, loc, wrapper_fn, wrapper_fn, nullptr, 0,
                type, nullptr, nullptr));
        }
};

class CoarrayPrifReplacer : public ASR::BaseExprReplacer<CoarrayPrifReplacer> {
    public:
        Allocator &al;
        PRIFInterface &prif;
        SymbolTable *current_scope = nullptr;

        CoarrayPrifReplacer(Allocator &al_, PRIFInterface &prif_)
            : al(al_), prif(prif_) {}

        void replace_CoarrayRef(ASR::CoarrayRef_t *x) {
            ASR::expr_t *call = prif.make_prif_get_call(
                x->base.base.loc, x->m_var, x->m_coindices,
                x->n_coindices, x->m_type);
            *current_expr = call;
        }

        void replace_IntrinsicElementalFunction(ASR::IntrinsicElementalFunction_t *x) {
            std::string intrinsic_name = ASRUtils::get_intrinsic_name(x->m_intrinsic_id);
            if (intrinsic_name == "NumImages") {
                ASR::expr_t *call = prif.make_prif_num_images_call(x->base.base.loc, x->m_type);
                *current_expr = call;
            } else if (intrinsic_name == "ThisImage") {
                ASR::expr_t *call = prif.make_prif_this_image_call(x->base.base.loc, x->m_type);
                *current_expr = call;
            } else {
                ASR::BaseExprReplacer<CoarrayPrifReplacer>::replace_IntrinsicElementalFunction(x);
            }
        }
};

class CoarrayPrifVisitor : public ASR::CallReplacerOnExpressionsVisitor<CoarrayPrifVisitor> {
    private:
        CoarrayPrifReplacer replacer;
    public:
        CoarrayPrifVisitor(Allocator &al_, PRIFInterface &prif)
            : replacer(al_, prif) {
        }

        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.current_scope = current_scope;
            replacer.replace_expr(*current_expr);
        }

        void visit_Assignment(const ASR::Assignment_t &x) {
            ASR::Assignment_t &xx = const_cast<ASR::Assignment_t &>(x);
            if (xx.m_target) {
                visit_expr(*xx.m_target);
            }

            ASR::expr_t **current_expr_copy_1 = current_expr;
            current_expr = &(xx.m_value);
            call_replacer();
            current_expr = current_expr_copy_1;
            if (xx.m_value && visit_expr_after_replacement) {
                visit_expr(*xx.m_value);
            }
            if (x.m_overloaded) {
                visit_stmt(*x.m_overloaded);
            }
        }

        void transform_stmts(ASR::stmt_t **&m_body, size_t &n_body) {
            Vec<ASR::stmt_t*> body;
            body.reserve(replacer.al, n_body);
            for (size_t i=0; i<n_body; i++) {
                if (m_body[i]->type == ASR::stmtType::SyncAll) {
                    ASR::SyncAll_t *x = ASR::down_cast<ASR::SyncAll_t>(m_body[i]);
                    body.push_back(replacer.al, replacer.prif.make_prif_sync_all_call(
                        x->base.base.loc, x->m_stat, x->m_errmsg));
                } else if (m_body[i]->type == ASR::stmtType::IntrinsicImpureSubroutine) {
                    ASR::IntrinsicImpureSubroutine_t *x = ASR::down_cast<ASR::IntrinsicImpureSubroutine_t>(m_body[i]);
                    std::string intrinsic_name = ASRUtils::get_intrinsic_subroutine_name(x->m_sub_intrinsic_id);
                    if (intrinsic_name == "CoSum") {
                        ASR::expr_t *a = nullptr;
                        ASR::expr_t *result_image = nullptr;
                        ASR::expr_t *stat = nullptr;
                        ASR::expr_t *errmsg = nullptr;
                        if (x->n_args >= 1) a = x->m_args[0];
                        if (x->n_args >= 2) result_image = x->m_args[1];
                        if (x->n_args >= 3) stat = x->m_args[2];
                        if (x->n_args >= 4) errmsg = x->m_args[3];
                        
                        body.push_back(replacer.al, replacer.prif.make_prif_co_minmaxsum_call(
                            x->base.base.loc, "prif_co_sum", a, result_image, stat, errmsg));
                    } 
                    else if (intrinsic_name == "CoMax") {
                        ASR::expr_t *a = nullptr;
                        ASR::expr_t *result_image = nullptr;
                        ASR::expr_t *stat = nullptr;
                        ASR::expr_t *errmsg = nullptr;
                        if (x->n_args >= 1) a = x->m_args[0];
                        if (x->n_args >= 2) result_image = x->m_args[1];
                        if (x->n_args >= 3) stat = x->m_args[2];
                        if (x->n_args >= 4) errmsg = x->m_args[3];
                        
                        body.push_back(replacer.al, replacer.prif.make_prif_co_minmaxsum_call(
                            x->base.base.loc, "prif_co_max", a, result_image, stat, errmsg));
                    }
                    else if (intrinsic_name == "CoMin") {
                        ASR::expr_t *a = nullptr;
                        ASR::expr_t *result_image = nullptr;
                        ASR::expr_t *stat = nullptr;
                        ASR::expr_t *errmsg = nullptr;
                        if (x->n_args >= 1) a = x->m_args[0];
                        if (x->n_args >= 2) result_image = x->m_args[1];
                        if (x->n_args >= 3) stat = x->m_args[2];
                        if (x->n_args >= 4) errmsg = x->m_args[3];
                             
                        body.push_back(replacer.al, replacer.prif.make_prif_co_minmaxsum_call(
                            x->base.base.loc, "prif_co_min", a, result_image, stat, errmsg));
                    }
                    else {
                        body.push_back(replacer.al, m_body[i]);
                    }
                } else if (m_body[i]->type == ASR::stmtType::Assignment) {
                    ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(m_body[i]);
                    if (a->m_target && ASR::is_a<ASR::CoarrayRef_t>(*a->m_target)) {
                        ASR::expr_t **current_expr_copy_1 = current_expr;
                        current_expr = &(a->m_value);
                        call_replacer();
                        current_expr = current_expr_copy_1;
                        if (a->m_value && visit_expr_after_replacement) {
                            visit_expr(*a->m_value);
                        }

                        ASR::CoarrayRef_t *coarray_ref = ASR::down_cast<ASR::CoarrayRef_t>(a->m_target);
                        ASR::stmt_t *put_call = replacer.prif.make_prif_put_call(
                            a->base.base.loc, coarray_ref->m_var, coarray_ref->m_coindices,
                            coarray_ref->n_coindices, a->m_value);
                        body.push_back(replacer.al, put_call);
                    }
                    else {
                        body.push_back(replacer.al, m_body[i]);
                    }
                }
                else {
                    body.push_back(replacer.al, m_body[i]);
                }
            }
            m_body = body.p;
            n_body = body.n;
            for (size_t i=0; i<n_body; i++) {
                visit_stmt(*m_body[i]);
            }
        }
};

// CoarrayCompanionVisitor traverses the ASR and declares companion
// variables for coarrays in each scope (Program, Function, Module)
// before any allocation or lookup is performed.
class CoarrayCompanionVisitor : public ASR::BaseWalkVisitor<CoarrayCompanionVisitor> {
    private:
        void process_scope(SymbolTable *symtab, const Location &loc) {
            prif.declare_coarray_companions(symtab, loc);
            for (auto &item : symtab->get_scope()) {
                visit_symbol(*item.second);
            }
        }
    public:
        PRIFInterface &prif;

        CoarrayCompanionVisitor(PRIFInterface &prif_)
            : prif(prif_) {}

        void visit_Module(const ASR::Module_t &x) {
            process_scope(x.m_symtab, x.base.base.loc);
        }

        void visit_Program(const ASR::Program_t &x) {
            process_scope(x.m_symtab, x.base.base.loc);
        }

        void visit_Function(const ASR::Function_t &x) {
            process_scope(x.m_symtab, x.base.base.loc);
        }
};

// CoarrayInitVisitor traverses the AST and inserts initialization
// code for coarrays in each scope (Program, Function, Module).
// It emits a call to prif_allocate_coarray. It also ensures the
// global PRIF runtime initialization and cleanup routines are called.
class CoarrayInitVisitor : public ASR::BaseWalkVisitor<CoarrayInitVisitor> {
    private:
    public:
        Allocator &al;
        PRIFInterface &prif;
        CoarrayInitVisitor(Allocator &al_, PRIFInterface &prif_)
            : al(al_), prif(prif_) {}

        void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
            // Process the rest of the symbol table normally
            for (auto &item : x.m_symtab->get_scope()) {
                visit_symbol(*item.second);
            }
            // Generate per-TU init function for saved coarrays
            Location loc; loc.first = 1; loc.last = 1;
            prif.generate_tu_init_function(loc);
        }

        void visit_Module(const ASR::Module_t &x) {
            for (auto &item : x.m_symtab->get_scope()) {
                visit_symbol(*item.second);
            }
        }

        void visit_Program(const ASR::Program_t &x) {
            ASR::Program_t &xx = const_cast<ASR::Program_t &>(x);

            Vec<ASR::stmt_t*> new_body;
            new_body.reserve(al, xx.n_body + 16);
            Location loc = xx.base.base.loc;

            ASRUtils::ASRBuilder b(al, loc);

            // Insert prif_init() call first
            prif.emit_prif_init_call(xx.m_symtab, loc, new_body);

            // Allocate coarrays for all used modules first
            for (auto &item : prif.get_global_scope()->get_scope()) {
                if (ASR::is_a<ASR::Module_t>(*item.second)) {
                    ASR::Module_t *mod = ASR::down_cast<ASR::Module_t>(item.second);
                    prif.allocate_coarrays(mod->m_symtab, xx.m_symtab, loc, new_body);
                }
            }

            // Allocate coarrays in Program scope
            prif.allocate_coarrays(xx.m_symtab, xx.m_symtab, loc, new_body);

            // Need to synchronize all the images after completing 
            // initialization of any save coarrays allocated above,
            // to prevent potential races with coindexed access below.
            new_body.push_back(al, prif.make_prif_sync_all_call(loc));

            // Append original body
            for (size_t i = 0; i < xx.n_body; i++) {
                new_body.push_back(al, xx.m_body[i]);
            }

            // Append prif_stop() at the end
            ASR::symbol_t *stop_sub = prif.get_or_create_prif_stop_sub(loc);
            Vec<ASR::call_arg_t> stop_args; stop_args.reserve(al, 3);
            ASR::call_arg_t quiet_arg; quiet_arg.loc = loc;
            quiet_arg.m_value = ASRUtils::EXPR(ASR::make_LogicalConstant_t(
                al, loc, false, ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 1))));
            ASR::call_arg_t sci_arg; sci_arg.loc = loc; sci_arg.m_value = nullptr;
            ASR::call_arg_t scc_arg; scc_arg.loc = loc; scc_arg.m_value = nullptr;
            stop_args.push_back(al, quiet_arg);
            stop_args.push_back(al, sci_arg);
            stop_args.push_back(al, scc_arg);
            new_body.push_back(al, ASRUtils::STMT(ASR::make_SubroutineCall_t(
                al, loc, stop_sub, nullptr, stop_args.p, stop_args.n, nullptr, false)));

            xx.m_body = new_body.p;
            xx.n_body = new_body.n;
            for (auto &item : xx.m_symtab->get_scope()) {
                visit_symbol(*item.second);
            }
        }

        void visit_Function(const ASR::Function_t &x) {
            ASR::Function_t &xx = const_cast<ASR::Function_t &>(x);

            Vec<ASR::stmt_t*> new_body;
            new_body.reserve(al, xx.n_body + 16);
            
            prif.allocate_coarrays(xx.m_symtab, xx.m_symtab, xx.base.base.loc, new_body);

            // Append original body
            for (size_t i = 0; i < xx.n_body; i++) {
                new_body.push_back(al, xx.m_body[i]);
            }

            xx.m_body = new_body.p;
            xx.n_body = new_body.n;
            for (auto &item : xx.m_symtab->get_scope()) {
                visit_symbol(*item.second);
            }
        }
};

void pass_replace_coarray(Allocator &al, ASR::TranslationUnit_t &unit,
                               const LCompilers::PassOptions &po) {
    if (po.coarray != true) {
        return;
    }
    PRIFInterface prif(al, unit);
    // Phase 1: Declare coarray companion variables
    CoarrayCompanionVisitor comp_v(prif);
    comp_v.visit_TranslationUnit(unit);

    // Phase 2: Create allocation calls for coarrays
    CoarrayInitVisitor init_v(al, prif);
    init_v.visit_TranslationUnit(unit);

    // Phase 3: Replace coarray expressions
    CoarrayPrifVisitor v(al, prif);
    v.visit_TranslationUnit(unit);

    // Phase 4: Update dependencies
    PassUtils::UpdateDependenciesVisitor(al).visit_TranslationUnit(unit);
}

} // namespace LCompilers
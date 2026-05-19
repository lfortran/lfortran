#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/replace_coarray.h>
#include <map>

namespace LCompilers {

class PRIFInterface {
    public:
        std::map<std::string, std::pair<ASR::expr_t*, ASR::expr_t*>> coarray_handle_map;
    private:
        Allocator &al;
        ASR::TranslationUnit_t &unit;

        ASR::symbol_t* get_or_create_prif_coarray_handle_struct(const Location &loc) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string symbol_name = "prif_coarray_handle";

            if (ASR::symbol_t *existing = global_scope->get_symbol(symbol_name)) {
                return existing;
            }

            SymbolTable *struct_symtab = al.make_new<SymbolTable>(global_scope);
            ASR::ttype_t *cptr_type = ASRUtils::TYPE(ASR::make_CPtr_t(al, loc));

            ASR::symbol_t *info_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(
                    al, loc, struct_symtab, s2c(al, "info"), nullptr, 0,
                    ASR::intentType::Local, nullptr, nullptr,
                    ASR::storage_typeType::Default, cptr_type, nullptr,
                    ASR::abiType::BindC, ASR::accessType::Private,
                    ASR::presenceType::Required, false, false, false,
                    nullptr, false, false, ASR::pass_attrType::NotMethod,
                    nullptr, 0, nullptr, 0, false));
            struct_symtab->add_symbol(s2c(al, "info"), info_sym);

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
            ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(
                    al, loc, symtab, s2c(al, name), nullptr, 0,
                    intent, nullptr, nullptr, ASR::storage_typeType::Default,
                    struct_type, struct_sym, ASR::abiType::Source,
                    ASR::accessType::Public, presence, value_attr, false, false,
                    nullptr, false, false, ASR::pass_attrType::NotMethod,
                    nullptr, 0, nullptr, 0, false));
            symtab->add_symbol(s2c(al, name), sym);
            return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
        }

        ASR::symbol_t* get_or_create_prif_get_subroutine(const Location &loc) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string symbol_name = "__module_prif_prif_get";

            if (global_scope->get_symbol(symbol_name)) {
                return global_scope->get_symbol(symbol_name);
            }

            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);

            ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            ASR::ttype_t *int64_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8));
            ASR::ttype_t *cptr_type = ASRUtils::TYPE(ASR::make_CPtr_t(al, loc));
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
            ASR::ttype_t *errmsg_alloc_type = ASRUtils::TYPE(ASR::make_Allocatable_t(
                al, loc,
                ASRUtils::TYPE(ASR::make_String_t(
                    al, loc, 1, nullptr,
                    ASR::string_length_kindType::DeferredLength,
                    ASR::string_physical_typeType::DescriptorString))));

            ASR::symbol_t *stat_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(
                    al, loc, fn_symtab, s2c(al, "stat"), nullptr, 0,
                    ASR::intentType::Out, nullptr, nullptr,
                    ASR::storage_typeType::Default, int32_type, nullptr,
                    ASR::abiType::Source, ASR::accessType::Public,
                    ASR::presenceType::Optional, false, false, false,
                    nullptr, false, false, ASR::pass_attrType::NotMethod,
                    nullptr, 0, nullptr, 0, false));
            fn_symtab->add_symbol(s2c(al, "stat"), stat_sym);
            ASR::expr_t *stat = ASRUtils::EXPR(ASR::make_Var_t(al, loc, stat_sym));

            ASR::symbol_t *errmsg_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(
                    al, loc, fn_symtab, s2c(al, "errmsg"), nullptr, 0,
                    ASR::intentType::InOut, nullptr, nullptr,
                    ASR::storage_typeType::Default, errmsg_type, nullptr,
                    ASR::abiType::Source, ASR::accessType::Public,
                    ASR::presenceType::Optional, false, false, false,
                    nullptr, false, false, ASR::pass_attrType::NotMethod,
                    nullptr, 0, nullptr, 0, false));
            fn_symtab->add_symbol(s2c(al, "errmsg"), errmsg_sym);
            ASR::expr_t *errmsg = ASRUtils::EXPR(ASR::make_Var_t(al, loc, errmsg_sym));

            ASR::symbol_t *errmsg_alloc_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(
                    al, loc, fn_symtab, s2c(al, "errmsg_alloc"), nullptr, 0,
                    ASR::intentType::InOut, nullptr, nullptr,
                    ASR::storage_typeType::Default, errmsg_alloc_type, nullptr,
                    ASR::abiType::Source, ASR::accessType::Public,
                    ASR::presenceType::Optional, false, false, false,
                    nullptr, false, false, ASR::pass_attrType::NotMethod,
                    nullptr, 0, nullptr, 0, false));
            fn_symtab->add_symbol(s2c(al, "errmsg_alloc"), errmsg_alloc_sym);
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
                ASR::deftypeType::Interface, s2c(al, "__module_prif_prif_get"),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);

            global_scope->add_symbol(symbol_name, ASR::down_cast<ASR::symbol_t>(fn));
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
            ASR::expr_t *image_num = b.Variable(fn_symtab, "image_num", int32_type,
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
            args.push_back(al, image_num);
            args.push_back(al, offset);

            ASR::expr_t *return_ptr = ASRUtils::EXPR(ASR::make_PointerToCPtr_t(
                al, loc,
                ASRUtils::EXPR(ASR::make_GetPointer_t(al, loc, return_var,
                    ASRUtils::TYPE(ASR::make_Pointer_t(al, loc, ASRUtils::expr_type(return_var))), nullptr)),
                cptr_type, nullptr));

            ASR::expr_t *size_in_bytes = get_size_in_bytes_expr(loc, return_type);

            ASR::symbol_t *prif_get_sym = get_or_create_prif_get_subroutine(loc);
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

            Vec<ASR::stmt_t*> body; body.reserve(al, 1);
            body.push_back(al, ASRUtils::STMT(ASR::make_SubroutineCall_t(
                al, loc, prif_get_sym, nullptr, call_args.p, call_args.n, nullptr, false)));

            SetChar dep; dep.reserve(al, 1);
            dep.push_back(al, s2c(al, "__module_prif_prif_get"));

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
                std::string name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(expr)->m_v);
                auto it = coarray_handle_map.find(name);
                if (it != coarray_handle_map.end()) {
                    return it->second.first;
                }
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
            if (!ASR::is_a<ASR::ArrayItem_t>(*base_expr)) {
                return b.i2i_t(b.i32(0), int64_type);
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
                size_bytes = 0;
            }
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, size_bytes, int64_type));
        }

    public:
        PRIFInterface(Allocator &al_, ASR::TranslationUnit_t &unit_)
            : al(al_), unit(unit_) {}

        ASR::symbol_t* get_or_create_prif_allocate_coarray_sub(const Location &loc) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string sym_name = "__module_prif_prif_allocate_coarray";
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
            ASR::expr_t *final_func = b.Variable(fn_symtab, "final_func", cptr,
                ASR::intentType::In, nullptr, ASR::abiType::Source, true);
            ASR::symbol_t *handle_struct = get_or_create_prif_coarray_handle_struct(loc);
            ASR::expr_t *handle_var = make_struct_var(fn_symtab, loc, "coarray_handle",
                handle_struct, ASR::intentType::Out, ASR::presenceType::Required, false);
            ASR::expr_t *alloc_mem = b.Variable(fn_symtab, "allocated_memory", cptr,
                ASR::intentType::Out, nullptr, ASR::abiType::Source, false);
            Vec<ASR::expr_t*> args; args.reserve(al, 6);
            args.push_back(al, lcobounds);
            args.push_back(al, ucobounds);
            args.push_back(al, size_arg);
            args.push_back(al, final_func);
            args.push_back(al, handle_var);
            args.push_back(al, alloc_mem);
            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, sym_name), nullptr, 0,
                args.p, args.n, nullptr, 0, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Interface,
                s2c(al, "__module_prif_prif_allocate_coarray"),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);
            global_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }

        ASR::symbol_t* get_or_create_prif_init_sub(const Location &loc) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string sym_name = "__module_prif_prif_init";
            if (ASR::symbol_t *existing = global_scope->get_symbol(sym_name)) {
                return existing;
            }
            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            // exit_code: integer(c_int), intent(out), optional
            ASR::symbol_t *ec_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(
                    al, loc, fn_symtab, s2c(al, "exit_code"), nullptr, 0,
                    ASR::intentType::Out, nullptr, nullptr,
                    ASR::storage_typeType::Default, int32_type, nullptr,
                    ASR::abiType::Source, ASR::accessType::Public,
                    ASR::presenceType::Optional, false, false, false,
                    nullptr, false, false, ASR::pass_attrType::NotMethod,
                    nullptr, 0, nullptr, 0, false));
            fn_symtab->add_symbol(s2c(al, "exit_code"), ec_sym);
            ASR::expr_t *ec_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, ec_sym));
            Vec<ASR::expr_t*> args; args.reserve(al, 1);
            args.push_back(al, ec_expr);
            ASR::asr_t *fn = ASRUtils::make_Function_t_util(
                al, loc, fn_symtab, s2c(al, sym_name), nullptr, 0,
                args.p, args.n, nullptr, 0, nullptr,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Interface,
                s2c(al, "__module_prif_prif_init"),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);
            global_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }

        ASR::symbol_t* get_or_create_prif_stop_sub(const Location &loc) {
            SymbolTable *global_scope = unit.m_symtab;
            std::string sym_name = "__module_prif_prif_stop";
            if (ASR::symbol_t *existing = global_scope->get_symbol(sym_name)) {
                return existing;
            }
            SymbolTable *fn_symtab = al.make_new<SymbolTable>(global_scope);
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *logical_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc, 1));
            ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            ASR::ttype_t *str_type = ASRUtils::TYPE(ASR::make_String_t(
                al, loc, 1, nullptr,
                ASR::string_length_kindType::AssumedLength,
                ASR::string_physical_typeType::DescriptorString));
            ASR::expr_t *quiet = b.Variable(fn_symtab, "quiet", logical_type,
                ASR::intentType::In, nullptr, ASR::abiType::Source, true);
            // stop_code_int: integer(c_int), intent(in), optional
            ASR::symbol_t *sci_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(
                    al, loc, fn_symtab, s2c(al, "stop_code_int"), nullptr, 0,
                    ASR::intentType::In, nullptr, nullptr,
                    ASR::storage_typeType::Default, int32_type, nullptr,
                    ASR::abiType::Source, ASR::accessType::Public,
                    ASR::presenceType::Optional, true, false, false,
                    nullptr, false, false, ASR::pass_attrType::NotMethod,
                    nullptr, 0, nullptr, 0, false));
            fn_symtab->add_symbol(s2c(al, "stop_code_int"), sci_sym);
            ASR::expr_t *sci = ASRUtils::EXPR(ASR::make_Var_t(al, loc, sci_sym));
            // stop_code_char: character(*), intent(in), optional
            ASR::symbol_t *scc_sym = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(
                    al, loc, fn_symtab, s2c(al, "stop_code_char"), nullptr, 0,
                    ASR::intentType::In, nullptr, nullptr,
                    ASR::storage_typeType::Default, str_type, nullptr,
                    ASR::abiType::Source, ASR::accessType::Public,
                    ASR::presenceType::Optional, true, false, false,
                    nullptr, false, false, ASR::pass_attrType::NotMethod,
                    nullptr, 0, nullptr, 0, false));
            fn_symtab->add_symbol(s2c(al, "stop_code_char"), scc_sym);
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
                s2c(al, "__module_prif_prif_stop"),
                false, false, false, false, false, nullptr, 0,
                false, false, false, nullptr);
            global_scope->add_symbol(sym_name, ASR::down_cast<ASR::symbol_t>(fn));
            return ASR::down_cast<ASR::symbol_t>(fn);
        }

        void init_coarrays_in_scope(SymbolTable *scope, const Location &loc,
                                    Vec<ASR::stmt_t*> &new_body,
                                    ASR::stmt_t **old_body, size_t n_old_body) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *i64 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8));
            ASR::ttype_t *cptr = ASRUtils::TYPE(ASR::make_CPtr_t(al, loc));
            ASR::symbol_t *handle_struct = get_or_create_prif_coarray_handle_struct(loc);
            ASR::symbol_t *alloc_sub = get_or_create_prif_allocate_coarray_sub(loc);
            for (auto &item : scope->get_scope()) {
                ASR::symbol_t *sym = item.second;
                if (!ASR::is_a<ASR::Variable_t>(*sym)) continue;
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
                if (!var->m_is_coarray && var->n_codims == 0) continue;
                std::string vname = var->m_name;
                // Create companion handle variable
                std::string hname = vname + "__coarray_handle";
                ASR::ttype_t *ht = ASRUtils::make_StructType_t_util(al, loc, handle_struct, true);
                ASR::symbol_t *hsym = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(
                        al, loc, scope, s2c(al, hname), nullptr, 0,
                        ASR::intentType::Local, nullptr, nullptr,
                        ASR::storage_typeType::Default, ht, handle_struct,
                        ASR::abiType::Source, ASR::accessType::Public,
                        ASR::presenceType::Required, false, false, false,
                        nullptr, false, false, ASR::pass_attrType::NotMethod,
                        nullptr, 0, nullptr, 0, false));
                scope->add_symbol(s2c(al, hname), hsym);
                ASR::expr_t *hexpr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, hsym));
                // Create companion data variable
                std::string dname = vname + "__coarray_data";
                ASR::symbol_t *dsym = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(
                        al, loc, scope, s2c(al, dname), nullptr, 0,
                        ASR::intentType::Local, nullptr, nullptr,
                        ASR::storage_typeType::Default, cptr, nullptr,
                        ASR::abiType::Source, ASR::accessType::Public,
                        ASR::presenceType::Required, false, false, false,
                        nullptr, false, false, ASR::pass_attrType::NotMethod,
                        nullptr, 0, nullptr, 0, false));
                scope->add_symbol(s2c(al, dname), dsym);
                ASR::expr_t *dexpr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, dsym));
                coarray_handle_map[vname] = {hexpr, dexpr};
                // Build lcobounds and ucobounds from var->m_codims
                int64_t corank = var->m_corank > 0 ? var->m_corank : 1;
                Vec<ASR::expr_t*> lco_elems; lco_elems.reserve(al, corank);
                Vec<ASR::expr_t*> uco_elems; uco_elems.reserve(al, corank > 1 ? corank - 1 : 0);
                for (int64_t ci = 0; ci < corank; ci++) {
                    // Lower cobound: from codims or default to 1
                    int64_t lb = 1;
                    if (ci < (int64_t)var->n_codims && var->m_codims[ci].m_start) {
                        ASRUtils::extract_value(var->m_codims[ci].m_start, lb);
                    }
                    lco_elems.push_back(al, b.i64(lb));
                    // Upper cobound: skip last dim (it's *)
                    if (ci < corank - 1 && ci < (int64_t)var->n_codims
                        && var->m_codims[ci].m_end) {
                        int64_t ub = 0;
                        ASRUtils::extract_value(var->m_codims[ci].m_end, ub);
                        uco_elems.push_back(al, b.i64(ub));
                    }
                }
                // lcobounds array
                Vec<ASR::dimension_t> arr_d; arr_d.reserve(al, 1);
                ASR::dimension_t ad; ad.loc = loc;
                ad.m_start = b.i32(1);
                ad.m_length = b.i32(static_cast<int64_t>(lco_elems.n));
                arr_d.push_back(al, ad);
                ASR::ttype_t *lco_arr_t = ASRUtils::make_Array_t_util(al, loc, i64, arr_d.p, arr_d.n);
                ASR::expr_t *lcobounds_val = ASRUtils::EXPR(ASR::make_ArrayConstructor_t(
                    al, loc, lco_elems.p, lco_elems.n, lco_arr_t, nullptr,
                    ASR::arraystorageType::ColMajor, nullptr));
                // ucobounds array
                Vec<ASR::dimension_t> arr_d0; arr_d0.reserve(al, 1);
                ASR::dimension_t ad0; ad0.loc = loc;
                ad0.m_start = b.i32(1);
                ad0.m_length = b.i32(static_cast<int64_t>(uco_elems.n));
                arr_d0.push_back(al, ad0);
                ASR::ttype_t *uco_arr_t = ASRUtils::make_Array_t_util(al, loc, i64, arr_d0.p, arr_d0.n);
                ASR::expr_t *ucobounds_val = ASRUtils::EXPR(ASR::make_ArrayConstructor_t(
                    al, loc, uco_elems.p, uco_elems.n, uco_arr_t, nullptr,
                    ASR::arraystorageType::ColMajor, nullptr));
                // size_in_bytes
                ASR::expr_t *sz = get_size_in_bytes_expr(loc, var->m_type);
                // final_func = c_null_funptr (use CPtr null - same ABI as c_funptr)
                ASR::expr_t *null_fptr = ASRUtils::EXPR(
                    ASR::make_PointerNullConstant_t(al, loc, cptr, nullptr));
                // Build call args
                Vec<ASR::call_arg_t> call_args; call_args.reserve(al, 6);
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
                // Transform coarray variable to Pointer type and link to PRIF memory
                ASR::ttype_t *orig_type = var->m_type;
                ASR::ttype_t *ptr_type = ASRUtils::TYPE(
                    ASR::make_Pointer_t(al, loc, orig_type));
                var->m_type = ptr_type;
                // c_f_pointer(x__coarray_data, x) — make x point to PRIF-allocated memory
                ASR::expr_t *var_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
                ASR::stmt_t *cfp_stmt = ASRUtils::STMT(
                    ASR::make_CPtrToPointer_t(al, loc, dexpr, var_expr, nullptr, nullptr));
                new_body.push_back(al, cfp_stmt);
            }
            // Append original body
            for (size_t i = 0; i < n_old_body; i++) {
                new_body.push_back(al, old_body[i]);
            }
        }

        ASR::expr_t* make_prif_get_call(const Location &loc,
                                        ASR::expr_t *base_expr,
                                        ASR::array_index_t *coindices,
                                        size_t n_coindices,
                                        ASR::ttype_t *return_type) {
            ASRUtils::ASRBuilder b(al, loc);
            ASR::ttype_t *int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
            ASR::expr_t *image_num = b.i32(1);
            if (n_coindices > 0 && coindices[0].m_left) {
                image_num = b.i2i_t(coindices[0].m_left, int32_type);
            }

            ASR::expr_t *handle_base = get_handle_base_expr(base_expr);
            ASR::expr_t *coarray_handle = make_prif_handle_expr(loc, handle_base);
            ASR::symbol_t *wrapper = get_or_create_prif_get_wrapper(loc, return_type, base_expr);

            ASR::expr_t *size_in_bytes = get_size_in_bytes_expr(loc, return_type);
            ASR::expr_t *offset = compute_offset_bytes(loc, base_expr, size_in_bytes);

            Vec<ASR::call_arg_t> args;
            args.reserve(al, 3);
            ASR::call_arg_t handle_arg; handle_arg.loc = loc; handle_arg.m_value = coarray_handle;
            ASR::call_arg_t image_arg; image_arg.loc = loc; image_arg.m_value = image_num;
            ASR::call_arg_t offset_arg; offset_arg.loc = loc; offset_arg.m_value = offset;
            args.push_back(al, handle_arg);
            args.push_back(al, image_arg);
            args.push_back(al, offset_arg);

            return ASRUtils::EXPR(ASR::make_FunctionCall_t(
                al, loc, wrapper, wrapper,
                args.p, args.n, return_type, nullptr, nullptr));
        }
};

class CoarrayPrifReplacer : public ASR::BaseExprReplacer<CoarrayPrifReplacer> {
    public:
        Allocator &al;
        PRIFInterface &prif;
        SymbolTable *current_scope = nullptr;
        bool allow_coarray_ref = true;

        CoarrayPrifReplacer(Allocator &al_, PRIFInterface &prif_)
            : al(al_), prif(prif_) {}

        void replace_CoarrayRef(ASR::CoarrayRef_t *x) {
            if (!allow_coarray_ref) {
                return;
            }
            ASR::expr_t *call = prif.make_prif_get_call(
                x->base.base.loc, x->m_var, x->m_coindices,
                x->n_coindices, x->m_type);
            *current_expr = call;
        }
};

class CoarrayPrifVisitor : public ASR::CallReplacerOnExpressionsVisitor<CoarrayPrifVisitor> {
    private:
        CoarrayPrifReplacer replacer;
    public:
        CoarrayPrifVisitor(Allocator &al, PRIFInterface &prif)
            : replacer(al, prif) {}

        void call_replacer() {
            replacer.current_expr = current_expr;
            replacer.current_scope = current_scope;
            replacer.replace_expr(*current_expr);
        }

        void visit_Assignment(const ASR::Assignment_t &x) {
            ASR::Assignment_t &xx = const_cast<ASR::Assignment_t &>(x);
            bool allow_copy = replacer.allow_coarray_ref;
            replacer.allow_coarray_ref = false;
            ASR::expr_t **current_expr_copy_0 = current_expr;
            current_expr = &(xx.m_target);
            call_replacer();
            current_expr = current_expr_copy_0;
            if (xx.m_target && visit_expr_after_replacement) {
                visit_expr(*xx.m_target);
            }
            replacer.allow_coarray_ref = allow_copy;

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
};

class CoarrayInitVisitor : public ASR::BaseWalkVisitor<CoarrayInitVisitor> {
    private:
        // Check whether a SymbolTable contains any coarray variables.
        static bool scope_has_coarrays(SymbolTable *scope) {
            for (auto &item : scope->get_scope()) {
                ASR::symbol_t *sym = item.second;
                if (!ASR::is_a<ASR::Variable_t>(*sym)) continue;
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
                if (var->m_is_coarray || var->n_codims > 0) return true;
            }
            return false;
        }

    public:
        Allocator &al;
        PRIFInterface &prif;
        CoarrayInitVisitor(Allocator &al_, PRIFInterface &prif_)
            : al(al_), prif(prif_) {}

        void visit_Program(const ASR::Program_t &x) {
            ASR::Program_t &xx = const_cast<ASR::Program_t &>(x);

            bool has_coarrays = scope_has_coarrays(xx.m_symtab);

            Vec<ASR::stmt_t*> new_body;
            new_body.reserve(al, xx.n_body + 16);
            Location loc = xx.base.base.loc;

            if (has_coarrays) {
                // Insert prif_init() call first — exit_code is optional (absent)
                ASR::symbol_t *init_sub = prif.get_or_create_prif_init_sub(loc);
                Vec<ASR::call_arg_t> init_args; init_args.reserve(al, 1);
                ASR::call_arg_t ec_arg; ec_arg.loc = loc; ec_arg.m_value = nullptr;
                init_args.push_back(al, ec_arg);
                new_body.push_back(al, ASRUtils::STMT(ASR::make_SubroutineCall_t(
                    al, loc, init_sub, nullptr, init_args.p, init_args.n, nullptr, false)));
            }

            // Allocate coarrays (init_coarrays_in_scope skips non-coarray vars internally)
            prif.init_coarrays_in_scope(xx.m_symtab, loc,
                new_body, xx.m_body, xx.n_body);

            if (has_coarrays) {
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
            }

            xx.m_body = new_body.p;
            xx.n_body = new_body.n;
            for (auto &item : xx.m_symtab->get_scope()) {
                visit_symbol(*item.second);
            }
        }

        void visit_Function(const ASR::Function_t &x) {
            ASR::Function_t &xx = const_cast<ASR::Function_t &>(x);
            if (!scope_has_coarrays(xx.m_symtab)) {
                // No coarray variables in this function; skip lowering
                for (auto &item : xx.m_symtab->get_scope()) {
                    visit_symbol(*item.second);
                }
                return;
            }
            Vec<ASR::stmt_t*> new_body;
            new_body.reserve(al, xx.n_body + 16);
            prif.init_coarrays_in_scope(xx.m_symtab, xx.base.base.loc,
                new_body, xx.m_body, xx.n_body);
            xx.m_body = new_body.p;
            xx.n_body = new_body.n;
            for (auto &item : xx.m_symtab->get_scope()) {
                visit_symbol(*item.second);
            }
        }
};

void pass_replace_coarray(Allocator &al, ASR::TranslationUnit_t &unit,
                               const LCompilers::PassOptions &) {
    // Quick check: skip the entire pass if no coarrays exist anywhere
    bool has_coarrays = false;
    for (auto &item : unit.m_symtab->get_scope()) {
        ASR::symbol_t *sym = item.second;
        if (ASR::is_a<ASR::Variable_t>(*sym)) {
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
            if (var->m_is_coarray || var->n_codims > 0) {
                has_coarrays = true;
                break;
            }
        } else if (ASR::is_a<ASR::Program_t>(*sym)) {
            SymbolTable *scope = ASR::down_cast<ASR::Program_t>(sym)->m_symtab;
            for (auto &s : scope->get_scope()) {
                if (ASR::is_a<ASR::Variable_t>(*s.second)) {
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s.second);
                    if (v->m_is_coarray || v->n_codims > 0) {
                        has_coarrays = true;
                        break;
                    }
                }
            }
        }
        if (has_coarrays) break;
    }
    if (!has_coarrays) return;

    PRIFInterface prif(al, unit);
    // Phase 1: Create companion vars and allocation calls for coarrays
    CoarrayInitVisitor init_v(al, prif);
    init_v.visit_TranslationUnit(unit);
    // Phase 2: Replace CoarrayRef with prif_get calls using real handles
    CoarrayPrifVisitor v(al, prif);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers
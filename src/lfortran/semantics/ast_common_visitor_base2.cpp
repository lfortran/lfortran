// Member functions of CommonVisitorBase, in the order of their declarations
// (split over ast_common_visitor_base*.cpp to compile in parallel).

#include <lfortran/semantics/ast_common_visitor.h>

namespace LCompilers::LFortran {

void CommonVisitorBase::create_intrinsic_function_wrapper(const std::string& sym, Location loc) {
    if (intrinsic_mapping.find(sym) == intrinsic_mapping.end()) {
        return;
    }
    const auto& mapping = intrinsic_mapping.at(sym);
    const std::string& generic_name = mapping.first;
    const std::string& type_name = mapping.second[0];

    ASR::ttype_t* arg_type = nullptr;
    if (type_name == "int4") arg_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    else if (type_name == "int8") arg_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 8));
    else if (type_name == "real4") arg_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 4));
    else if (type_name == "real8") arg_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 8));
    if (!arg_type) return;

    ASR::ttype_t* ret_type = arg_type;
    int64_t intrinsic_id = -1;
    if (generic_name == "abs") {
        intrinsic_id = static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::Abs);
    }
    if (intrinsic_id == -1) return;

    SymbolTable *parent_scope = current_scope;
    current_scope = al.make_new<SymbolTable>(parent_scope);

    std::string arg_name = sym + "_arg";
    SetChar dep;
    dep.reserve(al, 1);
    ASR::asr_t* arg_asr = ASRUtils::make_Variable_t_util(al, loc,
        current_scope, s2c(al, arg_name), dep.p, 0,
        ASRUtils::intent_unspecified, nullptr, nullptr,
        ASR::storage_typeType::Default, arg_type, nullptr,
        ASR::abiType::Source, ASR::Public, ASR::presenceType::Required, false);
    current_scope->add_symbol(arg_name, ASR::down_cast<ASR::symbol_t>(arg_asr));

    std::string ret_name = sym + "_return_var_name";
    ASR::asr_t* ret_asr = ASRUtils::make_Variable_t_util(al, loc,
        current_scope, s2c(al, ret_name), dep.p, 0,
        ASRUtils::intent_return_var, nullptr, nullptr,
        ASR::storage_typeType::Default, ret_type, nullptr,
        ASR::abiType::Source, ASR::Public, ASR::presenceType::Required, false);
    current_scope->add_symbol(ret_name, ASR::down_cast<ASR::symbol_t>(ret_asr));

    ASR::symbol_t* arg_sym = current_scope->get_symbol(arg_name);
    ASR::symbol_t* ret_sym = current_scope->get_symbol(ret_name);
    ASR::expr_t* arg_var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, arg_sym));
    ASR::expr_t* ret_var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, ret_sym));

    Vec<ASR::expr_t*> fn_args;
    fn_args.reserve(al, 1);
    fn_args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, arg_sym)));

    Vec<ASR::expr_t*> intrinsic_args;
    intrinsic_args.reserve(al, 1);
    intrinsic_args.push_back(al, arg_var);
    ASR::expr_t* intrinsic_call = ASRUtils::EXPR(
        ASR::make_IntrinsicElementalFunction_t(al, loc, intrinsic_id,
            intrinsic_args.p, intrinsic_args.n, 0, ret_type, nullptr));

    Vec<ASR::stmt_t*> body;
    body.reserve(al, 1);
    body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
        al, loc, ret_var, intrinsic_call, nullptr, false, false)));

    tmp = ASRUtils::make_Function_t_util(
        al, loc, current_scope, s2c(al, sym),
        nullptr, 0, fn_args.p, fn_args.n, body.p, body.n,
        ASRUtils::EXPR(ASR::make_Var_t(al, loc, ret_sym)),
        ASR::abiType::Source, ASR::accessType::Public,
        ASR::deftypeType::Implementation,
        nullptr, false, false, false, false, false, nullptr, 0,
        false, false, false);
    parent_scope->add_or_overwrite_symbol(sym, ASR::down_cast<ASR::symbol_t>(tmp));
    current_scope = parent_scope;
}

bool CommonVisitorBase::is_inaccessible_external_procedure(ASR::symbol_t* sym,
        const std::string &name) {
    if (!ASR::is_a<ASR::Function_t>(*sym)) {
        return false;
    }
    SymbolTable* sym_scope = ASRUtils::symbol_parent_symtab(sym);
    if (!ASRUtils::is_tu_scope(sym_scope) || sym_scope == current_scope) {
        return false;
    }
    for (SymbolTable* s = current_scope; s != sym_scope; s = s->parent) {
        // A scope that the procedure owns is in the procedure itself.
        // Interactive evaluation chains translation units by scope, see
        // `ASRUtils::cell_prefix()`: leave a procedure of an earlier
        // cell accessible.
        if (s->asr_owner == &sym->base || ASRUtils::is_tu_scope(s)) {
            return false;
        }
        auto it = procedure_references.find(s->counter);
        if (it != procedure_references.end() && it->second.count(name) > 0) {
            return false;
        }
    }
    return true;
}

ASR::Function_t* CommonVisitorBase::cell_procedure_for_declaration(const std::string &name) {
    if (!compiler_options.interactive || current_scope->parent == nullptr) {
        return nullptr;
    }
    // The symbol of a procedure or a main program is created after its
    // specification part.
    ASR::asr_t *owner = current_scope->asr_owner;
    if (owner != nullptr && (!ASR::is_a<ASR::symbol_t>(*owner)
            || !(ASR::is_a<ASR::Function_t>(*ASR::down_cast<ASR::symbol_t>(owner))
                || ASR::is_a<ASR::Program_t>(*ASR::down_cast<ASR::symbol_t>(owner))
                || ASR::is_a<ASR::Block_t>(*ASR::down_cast<ASR::symbol_t>(owner))))) {
        return nullptr;
    }
    if (std::find(current_procedure_args.begin(),
            current_procedure_args.end(), name) != current_procedure_args.end()
            || std::find(current_entry_args.begin(),
                current_entry_args.end(), name) != current_entry_args.end()) {
        return nullptr;
    }
    ASR::symbol_t *proc = current_scope->parent->resolve_symbol(name);
    if (proc == nullptr || !ASR::is_a<ASR::Function_t>(*proc)) {
        return nullptr;
    }
    ASR::Function_t *impl = ASR::down_cast<ASR::Function_t>(proc);
    SymbolTable *impl_scope = impl->m_symtab->parent;
    if (ASRUtils::get_FunctionType(impl)->m_deftype
                != ASR::deftypeType::Implementation
            || !ASRUtils::is_tu_scope(impl_scope)
            || ASRUtils::cell_prefix(impl_scope).empty()) {
        return nullptr;
    }
    return impl;
}

void CommonVisitorBase::bind_interface_body_to_cell_procedure(const std::string &name) {
    ASR::symbol_t *sym = current_scope->get_symbol(name);
    if (sym == nullptr || !ASR::is_a<ASR::Function_t>(*sym)) return;
    ASR::Function_t *iface = ASR::down_cast<ASR::Function_t>(sym);
    ASR::FunctionType_t *ftype = ASRUtils::get_FunctionType(iface);
    if (ftype->m_deftype != ASR::deftypeType::Interface
            || ftype->m_abi != ASR::abiType::Source
            || !ASRUtils::is_external_procedure_interface(ftype)
            || ASRUtils::is_abstract_interface(*iface)) {
        return;
    }
    ASR::Function_t *impl = cell_procedure_for_declaration(name);
    if (impl == nullptr || !ASRUtils::types_equal(impl->m_function_signature,
            iface->m_function_signature,
            ASRUtils::get_expr_from_sym(al, &impl->base),
            ASRUtils::get_expr_from_sym(al, sym))) {
        return;
    }
    current_scope->erase_symbol(name);
    access_cell_procedure(name);
}

bool CommonVisitorBase::bind_external_to_cell_procedure(const std::string &name,
        ASR::ttype_t *declared_type) {
    ASR::Function_t *impl = cell_procedure_for_declaration(name);
    if (impl == nullptr) return false;
    ASR::ttype_t *impl_type = ASRUtils::get_FunctionType(impl)->m_return_var_type;
    if (declared_type == nullptr) {
        if (impl_type != nullptr) {
            declared_type = compiler_options.implicit_typing
                ? implicit_dictionary[std::string(1, name[0])] : nullptr;
        }
    } else if (impl_type == nullptr) {
        return false;
    }
    if (impl_type != nullptr && (declared_type == nullptr
            || !ASRUtils::types_equal(declared_type, impl_type,
                nullptr, nullptr))) {
        return false;
    }
    access_cell_procedure(name);
    return true;
}

bool CommonVisitorBase::is_pending_proc_placeholder(ASR::symbol_t *s) {
    s = ASRUtils::symbol_get_past_external(s);
    for (auto &placeholder : pending_proc_placeholders) {
        if (ASRUtils::symbol_get_past_external(placeholder.placeholder) == s) {
            return true;
        }
    }
    return false;
}

bool CommonVisitorBase::check_initializer_type(ASR::ttype_t* type, ASR::ttype_t* init_type,
        ASR::expr_t* init_expr) {
    if (ASRUtils::check_equal_type(type, ASRUtils::expr_type(init_expr),
            nullptr, nullptr, false)) {
        return true;
    }
    std::string ltype = ASRUtils::type_to_str_fortran_expr(type, nullptr);
    std::string rtype = ASRUtils::type_to_str_fortran_expr(init_type, init_expr);
    diag.add(Diagnostic(
        "type mismatch in initialization: `" +
        rtype + "` cannot be assigned to `" + ltype + "`",
        Level::Error, Stage::Semantic, {
            Label("", {init_expr->base.loc})
        }));
    return false;
}

void CommonVisitorBase::report_not_variable_or_procedure(const std::string &name,
        const Location &loc, ASR::symbol_t *sym) {
    std::string sym_type = ASRUtils::symbol_type_name(*sym);
    diag.add(Diagnostic(
        "Symbol '" + name + "' must be a variable or a procedure",
        Level::Error, Stage::Semantic, {
            Label("cannot use a '" + sym_type + "' as a variable", {loc}),
            Label("'" + name + "' declared as a '" + sym_type + "' here",
                {sym->base.loc}, false),
        }));
}

bool CommonVisitorBase::declare_external_procedure(const std::string &sym,
        const Location &loc, ASR::Function_t *iface,
        ASR::accessType access, ASR::abiType abi, char *bindc_name) {
    ASRUtils::SymbolDuplicator duplicator(al);
    ASR::symbol_t *proc = duplicator.duplicate_Function(iface,
        current_scope);
    if (!proc) {
        return false;
    }
    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(proc);
    f->base.base.loc = loc;
    f->m_name = s2c(al, sym);
    f->m_body = nullptr;
    f->n_body = 0;
    f->m_access = access;
    ASR::FunctionType_t *ftype = ASRUtils::get_FunctionType(f);
    ftype->m_deftype = ASR::deftypeType::Interface;
    ftype->m_bindc_name = nullptr;
    if (abi == ASR::abiType::BindC) {
        ftype->m_abi = ASR::abiType::BindC;
        ftype->m_bindc_name = bindc_name;
    }
    current_scope->add_symbol(sym, proc);
    return true;
}

ASR::ttype_t* CommonVisitorBase::specific_intrinsic_arg_type(const std::string &type_name,
        const Location &loc) {
    if (type_name == "int4") {
        return ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    } else if (type_name == "real4") {
        return ASRUtils::TYPE(ASR::make_Real_t(al, loc, 4));
    } else if (type_name == "real8") {
        return ASRUtils::TYPE(ASR::make_Real_t(al, loc, 8));
    } else if (type_name == "complex4") {
        return ASRUtils::TYPE(ASR::make_Complex_t(al, loc, 4));
    } else if (type_name == "complex8") {
        return ASRUtils::TYPE(ASR::make_Complex_t(al, loc, 8));
    } else if (type_name == "character") {
        return ASRUtils::TYPE(ASR::make_String_t(al, loc, 1, nullptr,
            ASR::string_length_kindType::AssumedLength,
            ASR::string_physical_typeType::DescriptorString));
    }
    return nullptr;
}

ASR::symbol_t* CommonVisitorBase::create_specific_intrinsic_interface(const Location &loc,
        const std::string &name, const std::string &iface_name,
        SymbolTable *parent_scope) {
    auto specific = specific_intrinsic_interfaces.find(name);
    if (specific == specific_intrinsic_interfaces.end()) {
        specific = intrinsic_mapping.find(name);
        if (specific == intrinsic_mapping.end()) {
            return nullptr;
        }
    }
    std::string generic_name = specific->second.first;
    const std::vector<std::string> &arg_type_names = specific->second.second;
    bool is_len = (generic_name == "len");
    std::vector<std::string> dummy_names = {"string"};
    size_t max_args = 1;
    if (!is_len) {
        // The specific names of `min` and `max` (`min0`, `amax1`, ...)
        // take any number of arguments.
        if (generic_name == "min" || generic_name == "max"
                || name2signature.find(generic_name) == name2signature.end()
                || !ASRUtils::IntrinsicElementalFunctionRegistry::
                    is_intrinsic_function(generic_name)) {
            return nullptr;
        }
        const IntrinsicSignature &signature = name2signature.at(generic_name);
        dummy_names = signature.kwarg_names;
        max_args = signature.max_args;
    }
    if (arg_type_names.size() > dummy_names.size()) {
        return nullptr;
    }
    SymbolTable *fn_scope = al.make_new<SymbolTable>(parent_scope);
    Vec<ASR::expr_t*> args;
    args.reserve(al, arg_type_names.size());
    Vec<ASR::expr_t*> call_args;
    call_args.reserve(al, max_args);
    for (size_t i = 0; i < arg_type_names.size(); i++) {
        ASR::ttype_t *arg_type = specific_intrinsic_arg_type(
            arg_type_names[i], loc);
        if (!arg_type) {
            return nullptr;
        }
        std::string arg_name = to_lower(dummy_names[i]);
        ASR::symbol_t *arg = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, fn_scope,
                s2c(al, arg_name), nullptr, 0, ASRUtils::intent_in,
                nullptr, nullptr, ASR::storage_typeType::Default, arg_type,
                nullptr, ASR::abiType::Source, ASR::Public,
                ASR::presenceType::Required, false));
        fn_scope->add_symbol(arg_name, arg);
        args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, arg)));
        call_args.push_back(al, args[i]);
    }
    ASR::ttype_t *return_type = nullptr;
    if (is_len) {
        return_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    } else {
        // The result has the type of the generic intrinsic applied to
        // the specific arguments, with its optional arguments absent, as
        // a reference to the generic intrinsic would have.
        while (call_args.size() < max_args) {
            call_args.push_back(al, nullptr);
        }
        fill_optional_args(generic_name, call_args, loc);
        fill_optional_kind_arg(generic_name, call_args);
        diag::Diagnostics call_diag;
        ASR::asr_t *call = ASRUtils::IntrinsicElementalFunctionRegistry::
            get_create_function(generic_name)(al, loc, call_args, call_diag);
        if (!call || call_diag.has_error()) {
            return nullptr;
        }
        return_type = ASRUtils::expr_type(ASRUtils::EXPR(call));
    }
    ASR::symbol_t *return_var = ASR::down_cast<ASR::symbol_t>(
        ASRUtils::make_Variable_t_util(al, loc, fn_scope,
            s2c(al, name), nullptr, 0, ASRUtils::intent_return_var,
            nullptr, nullptr, ASR::storage_typeType::Default, return_type,
            nullptr, ASR::abiType::Source, ASR::Public,
            ASR::presenceType::Required, false));
    fn_scope->add_symbol(name, return_var);
    ASR::symbol_t *iface = ASR::down_cast<ASR::symbol_t>(
        ASRUtils::make_Function_t_util(al, loc, fn_scope,
            s2c(al, iface_name), nullptr, 0, args.p, args.size(),
            nullptr, 0, ASRUtils::EXPR(ASR::make_Var_t(al, loc, return_var)),
            ASR::abiType::Source, ASR::accessType::Public,
            ASR::deftypeType::Interface, nullptr,
            /* m_elemental */ !is_len, /* m_pure */ true, false, false, false,
            nullptr, 0, false, false, false));
    parent_scope->add_symbol(iface_name, iface);
    return iface;
}

bool CommonVisitorBase::check_is_external(std::string sym, SymbolTable* scope) {
    // The `external_procedures` member accumulates the external procedures
    // of the scope currently being built (filled by
    // `create_external_function`). It is only valid for that in-progress
    // scope, so consult it only when the caller did not pass a specific
    // `scope`. We must not overwrite it: the previous implementation
    // assigned the (still empty) map entry to this member, which dropped
    // every external procedure declared earlier in the same scope.
    if (scope == nullptr &&
        std::find(external_procedures.begin(), external_procedures.end(),
            sym) != external_procedures.end()) {
        return true;
    }
    // Then consult the persisted mapping for the requested (or current)
    // scope and all of its parents. Skip tables whose asr_owner is not
    // set yet (the ASR node is created after the spec part for Program /
    // Function / Subroutine) rather than aborting the walk.
    for (SymbolTable *s = (scope ? scope : current_scope); s; s = s->parent) {
        if (!s->asr_owner) continue;
        auto it = external_procedures_mapping.find(get_hash(s->asr_owner));
        if (it != external_procedures_mapping.end()) {
            const std::vector<std::string>& procs = it->second;
            if (std::find(procs.begin(), procs.end(), sym) != procs.end()) {
                return true;
            }
        }
    }
    return false;
}

bool CommonVisitorBase::check_is_explicit_intrinsic(std::string sym, SymbolTable* scope) {
    if (scope && scope->asr_owner) {
        explicit_intrinsic_procedures = explicit_intrinsic_procedures_mapping[get_hash(scope->asr_owner)];
    } else if (current_scope && current_scope->asr_owner) {
        explicit_intrinsic_procedures = explicit_intrinsic_procedures_mapping[get_hash(current_scope->asr_owner)];
    }
    if (std::find(explicit_intrinsic_procedures.begin(), explicit_intrinsic_procedures.end(), sym) != explicit_intrinsic_procedures.end()) {
        return true;
    }
    SymbolTable* s = (scope ? scope : current_scope);
    s = s->parent;
    while (s && s->asr_owner) {
        auto it = explicit_intrinsic_procedures_mapping.find(get_hash(s->asr_owner));
        if (it != explicit_intrinsic_procedures_mapping.end()) {
            const std::vector<std::string>& parent_procs = it->second;
            if (std::find(parent_procs.begin(), parent_procs.end(), sym) != parent_procs.end()) {
                return true;
            }
        }
        s = s->parent;
    }
    return false;
}

void CommonVisitorBase::erase_from_explicit_intrinsic_mapping(std::string sym) {
    uint64_t hash = get_hash(current_scope->asr_owner);
    explicit_intrinsic_procedures_mapping[hash].erase(
        std::remove(
            explicit_intrinsic_procedures_mapping[hash].begin(),
            explicit_intrinsic_procedures_mapping[hash].end(),
            sym
        )
    );
}

ASR::expr_t* CommonVisitorBase::evaluate_parameter_array_section(
        ASR::ArraySection_t* section, Allocator& al) {
    ASR::expr_t* val = ASRUtils::expr_value(section->m_v);
    if (val) val = ASRUtils::expr_value(val);
    if (!val || !ASR::is_a<ASR::ArrayConstant_t>(*val)) return nullptr;
    if (section->n_args != 1) return nullptr;

    ASR::ArrayConstant_t* src = ASR::down_cast<ASR::ArrayConstant_t>(val);
    ASR::array_index_t arg = section->m_args[0];
    int64_t left_val = -1, right_val = -1, step_val = 1;
    if (arg.m_left) {
        ASR::expr_t* lv = ASRUtils::expr_value(arg.m_left);
        if (!lv || !ASR::is_a<ASR::IntegerConstant_t>(*lv)) return nullptr;
        left_val = ASR::down_cast<ASR::IntegerConstant_t>(lv)->m_n;
    } else {
        return nullptr;
    }
    if (arg.m_right) {
        ASR::expr_t* rv = ASRUtils::expr_value(arg.m_right);
        if (!rv || !ASR::is_a<ASR::IntegerConstant_t>(*rv)) return nullptr;
        right_val = ASR::down_cast<ASR::IntegerConstant_t>(rv)->m_n;
    } else {
        return nullptr;
    }
    if (arg.m_step) {
        ASR::expr_t* sv = ASRUtils::expr_value(arg.m_step);
        if (sv && ASR::is_a<ASR::IntegerConstant_t>(*sv)) {
            step_val = ASR::down_cast<ASR::IntegerConstant_t>(sv)->m_n;
        }
    }
    if (step_val == 0) return nullptr;

    int64_t based_index = 1;
    if (ASR::is_a<ASR::Var_t>(*section->m_v)) {
        ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(section->m_v)->m_v;
        sym = ASRUtils::symbol_get_past_external(sym);
        if (ASR::is_a<ASR::Variable_t>(*sym)) {
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
            if (ASRUtils::is_array(var->m_type)) {
                ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(var->m_type);
                if (arr->n_dims > 0 && arr->m_dims[0].m_start) {
                    ASR::expr_t* start = ASRUtils::expr_value(arr->m_dims[0].m_start);
                    if (start && ASR::is_a<ASR::IntegerConstant_t>(*start)) {
                        based_index = ASR::down_cast<ASR::IntegerConstant_t>(start)->m_n;
                    }
                }
            }
        }
    }

    int64_t start_0 = left_val - based_index;
    int64_t end_0 = right_val - based_index;
    int64_t array_size = ASRUtils::get_fixed_size_of_array(src->m_type);
    if (start_0 < 0 || end_0 < 0 || start_0 >= array_size || end_0 >= array_size) {
        return nullptr;
    }

    Vec<ASR::expr_t*> section_values;
    int64_t n_elems = std::abs(end_0 - start_0) / std::abs(step_val) + 1;
    section_values.reserve(al, n_elems);
    for (int64_t i = start_0; step_val > 0 ? i <= end_0 : i >= end_0; i += step_val) {
        section_values.push_back(al,
            ASRUtils::fetch_ArrayConstant_value(al, src, i));
    }

    Location loc = section->base.base.loc;
    ASR::ttype_t* element_type = ASRUtils::type_get_past_array(src->m_type);
    void* new_data = ASRUtils::set_ArrayConstant_data(
        section_values.p, section_values.size(), element_type);
    ASR::ttype_t* int_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, loc, 4));
    Vec<ASR::dimension_t> dims;
    dims.reserve(al, 1);
    ASR::dimension_t dim;
    dim.loc = loc;
    dim.m_start = ASRUtils::EXPR(
        ASR::make_IntegerConstant_t(al, loc, 1, int_type));
    dim.m_length = ASRUtils::EXPR(
        ASR::make_IntegerConstant_t(al, loc,
            section_values.size(), int_type));
    dims.push_back(al, dim);
    ASR::array_physical_typeType phys_type =
        ASRUtils::is_character(*element_type)
            ? ASR::array_physical_typeType::PointerArray
            : ASR::array_physical_typeType::FixedSizeArray;
    ASR::ttype_t* new_arr_type = ASRUtils::TYPE(
        ASR::make_Array_t(al, loc, element_type,
            dims.p, dims.n, phys_type, ASR::memory_spaceType::Global));
    return ASRUtils::EXPR(
        ASRUtils::make_ArrayConstant_t_util(al, loc, new_data,
            new_arr_type, ASR::arraystorageType::ColMajor));
}

void CommonVisitorBase::validate_and_adjust_character_parameter_length(
    ASR::expr_t* init_expr, ASR::expr_t*& value, ASR::ttype_t* type,
    const Location& loc, Allocator& al, diag::Diagnostics& diag) {

    bool is_array_reshape = false;
    if (ASR::is_a<ASR::ArrayReshape_t>(*init_expr)) {
        is_array_reshape = true;
    }
    ASR::String_t *lhs_type = ASR::down_cast<ASR::String_t>(
        ASRUtils::type_get_past_array(type));
    ASR::String_t *rhs_type = nullptr;
    if (is_array_reshape) {
        ASR::ArrayReshape_t* array_reshape = ASR::down_cast<ASR::ArrayReshape_t>(init_expr);
        rhs_type = ASR::down_cast<ASR::String_t>(ASRUtils::type_get_past_array(array_reshape->m_type));
    } else {
        ASR::expr_t* rhs_source = value ? value : init_expr;
        LCOMPILERS_ASSERT(
            !ASRUtils::is_allocatable_or_pointer(type) &&
            !ASRUtils::is_allocatable_or_pointer(ASRUtils::expr_type(rhs_source)))
        rhs_type = ASR::down_cast<ASR::String_t>(
            ASRUtils::type_get_past_array(ASRUtils::expr_type(rhs_source)));
    }
    int64_t lhs_len = 0, rhs_len = 0;
    bool is_lhs_length_constant = ASRUtils::extract_value(lhs_type->m_len, lhs_len);
    bool is_rhs_length_constant = ASRUtils::extract_value(rhs_type->m_len, rhs_len);
    if( is_lhs_length_constant && is_rhs_length_constant ){
        if((lhs_len < rhs_len)){
            diag.semantic_warning_label(
                "The LHS character len="
                    + std::to_string(lhs_len)
                    + " and the RHS character len="
                    + std::to_string(rhs_len)
                    + " are not equal.",
                {loc},
                "help: consider changing the RHS character len to match the LHS character len"
            );
        }
        if((lhs_len != rhs_len)) {
            // Adjust character string by padding or trimming
            // Notice that we only trim when variable is parameter, to have compile-time-correct string.
            if(is_array_reshape){
                ASR::ArrayReshape_t* array_reshape_t = ASR::down_cast<ASR::ArrayReshape_t>(init_expr);
                (void)ASRUtils::adjust_array_character_length(
                            array_reshape_t->m_array, lhs_len, rhs_len, al);
                if (array_reshape_t->m_value) {
                    (void)ASRUtils::adjust_array_character_length(
                                array_reshape_t->m_value, lhs_len, rhs_len, al);
                }
                {
                    ASR::String_t* reshape_str_t = ASRUtils::get_string_type(array_reshape_t->m_type);
                    LCOMPILERS_ASSERT(ASRUtils::is_value_constant(reshape_str_t->m_len))
                    {
                        ASR::String_t* arr_const_str_t = ASRUtils::get_string_type(array_reshape_t->m_array);
                        LCOMPILERS_ASSERT(ASRUtils::is_value_constant(arr_const_str_t->m_len))
                        ASR::down_cast<ASR::IntegerConstant_t>(reshape_str_t->m_len)->m_n =
                            ASR::down_cast<ASR::IntegerConstant_t>(arr_const_str_t->m_len)->m_n;
                    }
                }
            } else if (value && ASR::is_a<ASR::ArrayConstant_t>(*value)) {
                value = ASRUtils::adjust_array_character_length(value, lhs_len,
                    rhs_len, al);
            } else if (value) {
                value = ASRUtils::adjust_character_length(value, lhs_len,
                    rhs_len, init_expr->base.loc, al);
            }
        }

    }
    if (!is_lhs_length_constant && is_rhs_length_constant
            && lhs_type->m_len_kind == ASR::ExpressionLength) {
        // The length depends on a deferred constant of a template, so
        // the value is padded or truncated when it is instantiated.
        // Until then the named constant has no value, which keeps its
        // uses from being folded with the unadjusted initializer.
        value = nullptr;
    }
    if(!is_rhs_length_constant){
            diag.add(Diagnostic(
            "The RHS character len must be known at compile time",
            Level::Error, Stage::Semantic, {
                Label("",{rhs_type->base.base.loc})
            }));
        throw SemanticAbort();
    }

    if (lhs_type->m_len_kind == ASR::DeferredLength) {
        diag.add(Diagnostic(
            "The LHS character length must not be deferred (allocatable) in a parameter declaration",
            Level::Error, Stage::Semantic, {
                Label("",{loc})
            }));
        throw SemanticAbort();
    }
    // Change length kind from AssumedLength to ExpressionLength + Set Length
    if(lhs_type->m_len_kind == ASR::AssumedLength){
        lhs_type->m_len = rhs_type->m_len;
        lhs_type->m_len_kind = ASR::ExpressionLength;
    }
}

void CommonVisitorBase::check_array_initializer_shape(ASR::ttype_t *decl_type,
        ASR::expr_t *init_expr, const Location &loc) {
    ASR::ttype_t *init_type = ASRUtils::expr_type(init_expr);
    if (!ASR::is_a<ASR::Array_t>(*decl_type)
            || !ASR::is_a<ASR::Array_t>(*init_type)) {
        return;
    }
    ASR::Array_t *arr_lhs = ASR::down_cast<ASR::Array_t>(decl_type);
    ASR::Array_t *arr_rhs = ASR::down_cast<ASR::Array_t>(init_type);
    if (arr_lhs->n_dims != arr_rhs->n_dims) {
        return;
    }
    for (size_t i = 0; i < arr_lhs->n_dims; i++) {
        int64_t lhs_len = -1, rhs_len = -1;
        if (arr_lhs->m_dims[i].m_length && arr_rhs->m_dims[i].m_length
                && ASRUtils::extract_value(ASRUtils::expr_value(
                    arr_lhs->m_dims[i].m_length), lhs_len)
                && ASRUtils::extract_value(ASRUtils::expr_value(
                    arr_rhs->m_dims[i].m_length), rhs_len)
                && lhs_len >= 0 && rhs_len >= 0
                && lhs_len != rhs_len) {
            diag.add(Diagnostic(
                "different shape for array initialization on dimension "
                + std::to_string(i + 1) + " (" + std::to_string(lhs_len)
                + " and " + std::to_string(rhs_len) + ")",
                Level::Error, Stage::Semantic, {
                    Label("", {loc})
                }));
            throw SemanticAbort();
        }
    }
}

void CommonVisitorBase::check_type_initializer_type(ASR::ttype_t *decl_type,
        ASR::symbol_t *decl_type_declaration, ASR::expr_t *init_expr,
        const Location &loc) {
    ASR::ttype_t *init_type = ASRUtils::expr_type(init_expr);
    bool decl_is_struct = ASR::is_a<ASR::StructType_t>(
        *ASRUtils::extract_type(decl_type));
    bool init_is_struct = ASR::is_a<ASR::StructType_t>(
        *ASRUtils::extract_type(init_type));
    if (!decl_is_struct && !init_is_struct) {
        return;
    }
    ASR::symbol_t *decl_struct_sym = decl_is_struct
        ? decl_type_declaration : nullptr;
    ASR::symbol_t *init_struct_sym = init_is_struct
        ? ASRUtils::get_struct_sym_from_struct_expr(init_expr) : nullptr;
    if ((decl_is_struct && decl_struct_sym == nullptr) ||
            (init_is_struct && init_struct_sym == nullptr)) {
        return;
    }
    if (decl_struct_sym) {
        decl_struct_sym = ASRUtils::symbol_get_past_external(decl_struct_sym);
    }
    if (init_struct_sym) {
        init_struct_sym = ASRUtils::symbol_get_past_external(init_struct_sym);
    }
    if (decl_is_struct && init_is_struct) {
        if (!ASR::is_a<ASR::Struct_t>(*decl_struct_sym) ||
                !ASR::is_a<ASR::Struct_t>(*init_struct_sym)) {
            return;
        }
        ASR::Struct_t *decl_struct = ASR::down_cast<ASR::Struct_t>(decl_struct_sym);
        ASR::Struct_t *init_struct = ASR::down_cast<ASR::Struct_t>(init_struct_sym);
        if (decl_struct == init_struct ||
                (!ASRUtils::is_parent(decl_struct, init_struct) &&
                 !ASRUtils::is_parent(init_struct, decl_struct) &&
                 ASRUtils::is_derived_type_similar(decl_struct, init_struct))) {
            return;
        }
    }
    auto type_name = [](ASR::ttype_t *t, ASR::symbol_t *struct_sym) {
        if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(t))) {
            return "type(" + std::string(ASRUtils::symbol_name(struct_sym)) + ")";
        }
        return ASRUtils::type_to_str_with_kind(ASRUtils::extract_type(t), nullptr);
    };
    diag.add(Diagnostic(
        "type mismatch in initialization: `" + type_name(init_type, init_struct_sym) +
        "` cannot be assigned to `" + type_name(decl_type, decl_struct_sym) + "`",
        Level::Error, Stage::Semantic, {
            Label("", {loc})
        }));
    throw SemanticAbort();
}

void CommonVisitorBase::emit_fortran_slash_init_warning(const AST::var_sym_t &s) {
    LCOMPILERS_ASSERT(s.m_initializer != nullptr);
    std::string init_str = "<expr>";
    uint32_t fl, fc, ll, lc;
    std::string f1, f2;

    lm.pos_to_linecol(
        lm.output_to_input_pos(s.m_initializer->base.loc.first, false),
        fl, fc, f1
    );
    lm.pos_to_linecol(
        lm.output_to_input_pos(s.m_initializer->base.loc.last, true),
        ll, lc, f2
    );

    if (f1 == f2) {
        std::string src;
        if (read_file(f1, src)) {
            std::stringstream ss(src);
            std::string line;
            for (uint32_t i = 1; i <= fl && std::getline(ss, line); i++) {}
            if (fl == ll && fc >= 1 && lc >= fc && lc <= line.size()) {
                init_str = line.substr(fc - 1, lc - fc + 1);
            }
        }
    }

    std::string hint = "Use modern syntax instead: integer :: "
        + std::string(s.m_name) + " = " + init_str;
    diag.semantic_warning_label(
        "non-standard Fortran 77-style initialization (extension)",
        {s.loc},
        hint
    );
}

void CommonVisitorBase::check_no_storage_association_in_template(const std::string &stmt,
        const Location &loc) {
    if (!in_template_definition) return;
    diag.add(Diagnostic(
        stmt + " statement is not allowed in a template or templated "
        "procedure",
        Level::Error, Stage::Semantic, {
            Label("", {loc})}));
    throw SemanticAbort();
}

bool CommonVisitorBase::is_deferred_const_decl(const AST::Declaration_t &x) {
    for (size_t i = 0; i < x.n_attributes; i++) {
        if (AST::is_a<AST::SimpleAttribute_t>(*x.m_attributes[i])
                && AST::down_cast<AST::SimpleAttribute_t>(x.m_attributes[i])
                    ->m_attr == AST::simple_attributeType::AttrDeferred) {
            return true;
        }
    }
    return false;
}

bool CommonVisitorBase::is_assumed_character_length(const AST::AttrType_t &t) {
    if (t.n_kind == 0) {
        return t.m_sym == AST::symbolType::DoubleAsterisk;
    }
    for (size_t i = 0; i < t.n_kind; i++) {
        if (t.m_kind[i].m_id == nullptr
                || to_lower(t.m_kind[i].m_id) == "len") {
            return t.m_kind[i].m_type == AST::kind_item_typeType::Star;
        }
    }
    return false;
}

void CommonVisitorBase::check_deferred_const_decl(const AST::Declaration_t &x) {
    // A deferred constant is a deferred argument (16.4.1.3 p1), so the
    // statement is only meaningful where deferred arguments are declared:
    // the specification part of a REQUIREMENT or TEMPLATE construct, or of
    // a templated subprogram (R1615).
    if (!(is_template || is_requirement)) {
        diag.add(Diagnostic(
            "a `deferred` declaration is only allowed in a requirement, a"
            " template or a templated subprogram",
            Level::Error, Stage::Semantic, {
                Label("", {x.base.base.loc})}));
        throw SemanticAbort();
    }

    // C1618: A deferred-const-attr-spec-list shall specify the PARAMETER
    // attribute. R1619 allows only DIMENSION, PARAMETER and a rank-clause,
    // which is all the parser accepts, so only PARAMETER can be missing.
    bool has_parameter = false;
    for (size_t i = 0; i < x.n_attributes; i++) {
        if (AST::is_a<AST::SimpleAttribute_t>(*x.m_attributes[i])
                && AST::down_cast<AST::SimpleAttribute_t>(x.m_attributes[i])
                    ->m_attr == AST::simple_attributeType::AttrParameter) {
            has_parameter = true;
        }
    }
    if (!has_parameter) {
        diag.add(Diagnostic(
            "a `deferred` constant declaration must specify the `parameter`"
            " attribute",
            Level::Error, Stage::Semantic, {
                Label("", {x.base.base.loc})}));
        throw SemanticAbort();
    }

    // C1619: the declaration-type-spec shall specify type integer, logical
    // or character, and a character type shall have assumed length. A
    // deferred constant is substituted by the instantiation argument, so
    // only the types that a constant expression can be built from at
    // instantiation time are allowed, and the length of a character
    // constant comes from that argument rather than from the template.
    if (x.m_vartype == nullptr
            || !AST::is_a<AST::AttrType_t>(*x.m_vartype)) {
        diag.add(Diagnostic(
            "the type of a `deferred` constant must be integer, logical or"
            " character",
            Level::Error, Stage::Semantic, {
                Label("", {x.base.base.loc})}));
        throw SemanticAbort();
    }
    AST::AttrType_t *t = AST::down_cast<AST::AttrType_t>(x.m_vartype);
    if (t->m_type != AST::decl_typeType::TypeInteger
            && t->m_type != AST::decl_typeType::TypeLogical
            && t->m_type != AST::decl_typeType::TypeCharacter) {
        diag.add(Diagnostic(
            "the type of a `deferred` constant must be integer, logical or"
            " character",
            Level::Error, Stage::Semantic, {
                Label("", {t->base.base.loc})}));
        throw SemanticAbort();
    }
    if (t->m_type == AST::decl_typeType::TypeCharacter
            && !is_assumed_character_length(*t)) {
        diag.add(Diagnostic(
            "a `deferred` character constant must have assumed length,"
            " declared as `character(*)`",
            Level::Error, Stage::Semantic, {
                Label("", {t->base.base.loc})}));
        throw SemanticAbort();
    }

    // The array-spec of a deferred constant can come from the entity
    // declaration (R1620), from a DIMENSION attribute or from a rank-clause
    // (R1619); C1621 applies to all three the same way.
    AST::dimension_t *attr_dim = nullptr;
    size_t attr_n_dim = 0;
    Location attr_dim_loc = x.base.base.loc;
    bool has_rank_clause = false;
    for (size_t i = 0; i < x.n_attributes; i++) {
        if (AST::is_a<AST::AttrDimension_t>(*x.m_attributes[i])) {
            AST::AttrDimension_t *ad =
                AST::down_cast<AST::AttrDimension_t>(x.m_attributes[i]);
            attr_dim = ad->m_dim;
            attr_n_dim = ad->n_dim;
            attr_dim_loc = ad->base.base.loc;
        } else if (AST::is_a<AST::AttrRank_t>(*x.m_attributes[i])) {
            has_rank_clause = true;
            attr_dim_loc = x.m_attributes[i]->base.loc;
        }
    }

    for (size_t i = 0; i < x.n_syms; i++) {
        AST::var_sym_t &s = x.m_syms[i];
        // R1620 is `deferred-const-name [ ( array-spec ) ]`: there is no
        // place for an initializer, the value comes from instantiation.
        if (s.m_initializer != nullptr) {
            diag.add(Diagnostic(
                "a `deferred` constant must not be given a value; its value"
                " comes from the instantiation argument",
                Level::Error, Stage::Semantic, {
                    Label("", {s.loc})}));
            throw SemanticAbort();
        }
        // C1620: A deferred-const-name shall be the name of a deferred
        // constant, i.e. of a deferred argument of the scoping unit
        // containing the statement (16.4.1.3 p1, R1615).
        if (std::find(current_procedure_args.begin(),
                current_procedure_args.end(), to_lower(s.m_name))
                == current_procedure_args.end()) {
            diag.add(Diagnostic(
                "'" + to_lower(s.m_name) + "' is not a deferred argument of"
                " this template or requirement",
                Level::Error, Stage::Semantic, {
                    Label("", {s.loc})}));
            throw SemanticAbort();
        }
        if (s.n_dim > 0 && (attr_n_dim > 0 || has_rank_clause)) {
            diag.add(Diagnostic(
                "the rank of '" + to_lower(s.m_name) + "' is specified"
                " twice",
                Level::Error, Stage::Semantic, {
                    Label("", {s.loc}),
                    Label("", {attr_dim_loc}, false)}));
            throw SemanticAbort();
        }
        AST::dimension_t *dim = s.n_dim > 0 ? s.m_dim : attr_dim;
        size_t n_dim = s.n_dim > 0 ? s.n_dim : attr_n_dim;
        Location dim_loc = s.n_dim > 0 ? s.loc : attr_dim_loc;
        check_deferred_const_array_spec(dim, n_dim, dim_loc);
        if (n_dim > 0 || has_rank_clause) {
            diag.add(Diagnostic(
                "a `deferred` constant that is an array is not supported"
                " yet",
                Level::Error, Stage::Semantic, {
                    Label("", {dim_loc})}));
            throw SemanticAbort();
        }
    }
}

void CommonVisitorBase::check_deferred_const_array_spec(AST::dimension_t *dim, size_t n_dim,
        const Location &loc) {
    bool all_star = true, all_explicit = true;
    for (size_t i = 0; i < n_dim; i++) {
        if (dim[i].m_end_star == AST::dimension_typeType::AssumedRank) {
            if (n_dim != 1) {
                diag.add(Diagnostic(
                    "`..` must be the only dimension of a `deferred`"
                    " constant",
                    Level::Error, Stage::Semantic, {
                        Label("", {dim[i].loc})}));
                throw SemanticAbort();
            }
            return;
        }
        bool is_star = dim[i].m_end_star
            == AST::dimension_typeType::DimensionStar;
        if (!is_star && dim[i].m_end == nullptr) {
            diag.add(Diagnostic(
                "a `deferred` constant must not have an assumed or deferred"
                " shape `:`",
                Level::Error, Stage::Semantic, {
                    Label("", {dim[i].loc})}));
            throw SemanticAbort();
        }
        all_star = all_star && is_star;
        all_explicit = all_explicit && !is_star;
    }
    if (n_dim > 0 && !all_star && !all_explicit) {
        diag.add(Diagnostic(
            "the dimensions of a `deferred` constant must be either all"
            " upper bounds, as in `(3,4)`, or all `*`, as in `(*,*)`",
            Level::Error, Stage::Semantic, {
                Label("", {loc})}));
        throw SemanticAbort();
    }
}

ASR::ttype_t* CommonVisitorBase::make_array_of_declared_element_type(const Location& loc,
        ASR::Array_t* array_type, ASR::ttype_t* element_type,
        ASR::abiType abi, bool is_argument) {
    if (array_type->m_physical_type ==
            ASR::array_physical_typeType::AssumedRankArray) {
        return ASRUtils::TYPE(ASR::make_Array_t(al, loc, element_type,
            nullptr, 0, ASR::array_physical_typeType::AssumedRankArray,
            array_type->m_memory_space));
    }
    // An assumed-size array is the only array without its last extent
    // that is not a descriptor.
    bool is_dimension_star = array_type->m_physical_type !=
            ASR::array_physical_typeType::DescriptorArray &&
        array_type->m_dims[array_type->n_dims - 1].m_length == nullptr;
    return ASRUtils::make_Array_t_util(al, loc, element_type,
        array_type->m_dims, array_type->n_dims, abi, is_argument,
        ASR::array_physical_typeType::DescriptorArray, false,
        is_dimension_star, true, array_type->m_memory_space);
}

void CommonVisitorBase::set_scoped_variable_attribute(const std::string &name,
        bool asynchronous, const Location &loc) {
    ASR::symbol_t *symbol = current_scope->resolve_symbol(name);
    if (symbol == nullptr) return;
    ASR::symbol_t *canonical = ASRUtils::symbol_get_past_external(symbol);
    if (!ASR::is_a<ASR::Variable_t>(*canonical)) return;
    auto *variable = ASR::down_cast<ASR::Variable_t>(canonical);
    if (variable->m_parent_symtab == current_scope) {
        if (asynchronous) {
            variable->m_is_asynchronous = true;
        } else {
            variable->m_is_volatile = true;
        }
        return;
    }
    std::string attribute_name = current_scope->get_unique_name(
        "__scoped_attribute_" + name);
    current_scope->add_symbol(attribute_name,
        ASR::down_cast<ASR::symbol_t>(ASR::make_ScopedVariableAttribute_t(
            al, loc, current_scope, s2c(al, attribute_name),
            symbol,
            !asynchronous, asynchronous)));
}

void CommonVisitorBase::ensure_deferred_shape_for_allocatable_or_pointer_array(ASR::ttype_t* type, const std::string& sym) {
    LCOMPILERS_ASSERT(type)
    if (ASRUtils::is_array_t(type) && ASRUtils::is_allocatable_or_pointer(type)) {
        ASR::Array_t* const array_type = ASR::down_cast<ASR::Array_t>(
            ASRUtils::type_get_past_allocatable_pointer(type));
        if (array_type->m_physical_type == ASR::AssumedRankArray) return;
        for (size_t i = 0; i < array_type->n_dims; i++) {
            std::string const attr_name = ASRUtils::is_allocatable(type) ? 
                                            "Allocatable" : "Pointer";
            if (array_type->m_dims[i].m_start != nullptr ||
                array_type->m_dims[i].m_length != nullptr ||
                has_postponed_bound(array_type->m_dims[i])) {
                diag.add(Diagnostic(
                    std::string(attr_name) + " array '" + sym +
                    "' must have a deferred shape or assumed rank",
                    Level::Error, Stage::Semantic, {
                        Label("", {type->base.loc})
                    }));
                throw SemanticAbort();
            }
        }
    }

}

void CommonVisitorBase::replace_sentinel_kinds(ASR::ttype_t* type,
    const std::map<int64_t, int64_t>& sentinel_to_actual)
{
    if (!type) return;

    ASR::ttype_t* base_type = type;
    if (ASR::is_a<ASR::Pointer_t>(*base_type)) {
        base_type = ASR::down_cast<ASR::Pointer_t>(base_type)->m_type;
    } else if (ASR::is_a<ASR::Allocatable_t>(*base_type)) {
        base_type = ASR::down_cast<ASR::Allocatable_t>(base_type)->m_type;
    }
    base_type = ASRUtils::type_get_past_array(base_type);

    switch (base_type->type) {
        case ASR::ttypeType::Integer: {
            ASR::Integer_t* t = ASR::down_cast<ASR::Integer_t>(base_type);
            auto it = sentinel_to_actual.find(t->m_kind);
            if (it != sentinel_to_actual.end()) t->m_kind = it->second;
            break;
        }
        case ASR::ttypeType::Real: {
            ASR::Real_t* t = ASR::down_cast<ASR::Real_t>(base_type);
            auto it = sentinel_to_actual.find(t->m_kind);
            if (it != sentinel_to_actual.end()) t->m_kind = it->second;
            break;
        }
        case ASR::ttypeType::Complex: {
            ASR::Complex_t* t = ASR::down_cast<ASR::Complex_t>(base_type);
            auto it = sentinel_to_actual.find(t->m_kind);
            if (it != sentinel_to_actual.end()) t->m_kind = it->second;
            break;
        }
        case ASR::ttypeType::Logical: {
            ASR::Logical_t* t = ASR::down_cast<ASR::Logical_t>(base_type);
            auto it = sentinel_to_actual.find(t->m_kind);
            if (it != sentinel_to_actual.end()) t->m_kind = it->second;
            break;
        }
        default:
            break;
    }
}

ASR::ttype_t* CommonVisitorBase::rewrap_pdt_member_type(ASR::ttype_t* original_type,
    ASR::ttype_t* new_base_type, const Location& loc)
{
    switch (original_type->type) {
        case ASR::ttypeType::Pointer: {
            ASR::Pointer_t* pointer_type = ASR::down_cast<ASR::Pointer_t>(original_type);
            return ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                rewrap_pdt_member_type(pointer_type->m_type, new_base_type, loc)));
        }
        case ASR::ttypeType::Allocatable: {
            ASR::Allocatable_t* alloc_type = ASR::down_cast<ASR::Allocatable_t>(original_type);
            return ASRUtils::TYPE(ASRUtils::make_Allocatable_t_util(al, loc,
                rewrap_pdt_member_type(alloc_type->m_type, new_base_type, loc)));
        }
        case ASR::ttypeType::Array: {
            ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(original_type);
            ASR::dimension_t* copied_dims = ASRUtils::duplicate_dimensions(
                al, array_type->m_dims, array_type->n_dims);
            return ASRUtils::make_Array_t_util(al, loc,
                rewrap_pdt_member_type(array_type->m_type, new_base_type, loc),
                copied_dims, array_type->n_dims, ASR::abiType::Source, false,
                array_type->m_physical_type, true);
        }
        default: {
            return new_base_type;
        }
    }
}

CommonVisitorBase::StructConstructorInfo CommonVisitorBase::get_struct_constructor_info(ASR::symbol_t* v) {
    std::vector<ASR::Struct_t*> chain;
    while (v) {
        ASR::Struct_t* type = ASR::down_cast<ASR::Struct_t>(
            ASRUtils::symbol_get_past_external(v));
        chain.push_back(type);
        v = type->m_parent;
    }
    StructConstructorInfo info;
    for (auto it = chain.rbegin(); it != chain.rend(); ++it) {
        ASR::Struct_t* type = *it;
        size_t offset = info.members.size();
        for (size_t i = 0; i < type->n_members; i++) {
            info.members.push_back(type->m_symtab->get_symbol(type->m_members[i]));
        }
        for (size_t i = 0; i < type->n_kind_params; i++) {
            ASR::symbol_t* param = type->m_symtab->get_symbol(type->m_kind_params[i]);
            auto member = std::find(info.members.begin() + offset, info.members.end(), param);
            LCOMPILERS_ASSERT(member != info.members.end());
            info.kind_indices.push_back(std::distance(info.members.begin(), member));
        }
    }
    return info;
}

ASR::expr_t* CommonVisitorBase::get_folded_struct_constant(const Location& loc,
        ASR::symbol_t* dt_sym, ASR::call_arg_t* args, size_t n_args,
        ASR::ttype_t* type) {
    StructConstructorInfo info = get_struct_constructor_info(dt_sym);
    if (!info.kind_indices.empty() || info.members.size() != n_args) {
        return nullptr;
    }
    Vec<ASR::call_arg_t> folded_args;
    folded_args.reserve(al, n_args);
    for (size_t i = 0; i < n_args; i++) {
        ASR::call_arg_t folded_arg = args[i];
        ASR::expr_t* arg = args[i].m_value;
        if (arg == nullptr) {
            folded_args.push_back(al, folded_arg);
            continue;
        }
        if (!info.members[i] || !ASR::is_a<ASR::Variable_t>(*info.members[i])) {
            return nullptr;
        }
        ASR::ttype_t* member_type = ASRUtils::symbol_type(info.members[i]);
        if (ASRUtils::is_allocatable_or_pointer(member_type)) {
            if (!ASR::is_a<ASR::PointerNullConstant_t>(*arg)) {
                return nullptr;
            }
            folded_args.push_back(al, folded_arg);
            continue;
        }
        ASR::ttype_t* element_type = ASRUtils::type_get_past_array(member_type);
        if (ASR::is_a<ASR::CPtr_t>(*member_type)) {
            if (!ASR::is_a<ASR::PointerNullConstant_t>(*arg)) {
                return nullptr;
            }
            folded_args.push_back(al, folded_arg);
            continue;
        }
        if (ASR::is_a<ASR::StructType_t>(*element_type)) {
            if (ASRUtils::is_array(member_type)) {
                return nullptr;
            }
            ASR::expr_t* nested_arg_value = ASRUtils::expr_value(arg);
            ASR::expr_t* nested_arg =
                (nested_arg_value && ASR::is_a<ASR::StructConstant_t>(*nested_arg_value))
                ? nested_arg_value : arg;
            if (ASR::is_a<ASR::StructConstructor_t>(*nested_arg)) {
                ASR::StructConstructor_t* nested = ASR::down_cast<ASR::StructConstructor_t>(nested_arg);
                folded_arg.m_value = get_folded_struct_constant(arg->base.loc,
                    nested->m_dt_sym, nested->m_args, nested->n_args, nested->m_type);
            } else if (ASR::is_a<ASR::StructConstant_t>(*nested_arg)) {
                ASR::StructConstant_t* nested = ASR::down_cast<ASR::StructConstant_t>(nested_arg);
                folded_arg.m_value = get_folded_struct_constant(arg->base.loc,
                    nested->m_dt_sym, nested->m_args, nested->n_args, nested->m_type);
            } else {
                return nullptr;
            }
            if (folded_arg.m_value == nullptr) {
                return nullptr;
            }
            folded_args.push_back(al, folded_arg);
            continue;
        }
        ASR::expr_t* value = ASRUtils::expr_value(arg);
        if (value == nullptr) {
            return nullptr;
        }
        switch (value->type) {
            case ASR::exprType::IntegerConstant:
            case ASR::exprType::UnsignedIntegerConstant:
            case ASR::exprType::RealConstant:
            case ASR::exprType::ComplexConstant:
            case ASR::exprType::LogicalConstant:
            case ASR::exprType::StringConstant:
            case ASR::exprType::ArrayConstant: {
                break;
            }
            default: {
                return nullptr;
            }
        }
        ASR::ttype_t* value_type = ASRUtils::expr_type(value);
        if (ASRUtils::is_array(member_type) != ASRUtils::is_array(value_type)
                || !ASRUtils::types_equal(element_type,
                    ASRUtils::type_get_past_array(value_type), nullptr, nullptr)) {
            return nullptr;
        }
        if (ASRUtils::is_character(*element_type)) {
            ASR::String_t* member_str = ASR::down_cast<ASR::String_t>(element_type);
            ASR::String_t* value_str = ASRUtils::get_string_type(value_type);
            int64_t member_len = 0, value_len = 0;
            if (member_str->m_len == nullptr || value_str == nullptr || value_str->m_len == nullptr
                    || !ASRUtils::extract_value(ASRUtils::expr_value(member_str->m_len), member_len)
                    || !ASRUtils::extract_value(ASRUtils::expr_value(value_str->m_len), value_len)
                    || member_len != value_len) {
                return nullptr;
            }
        }
        folded_arg.m_value = value;
        folded_args.push_back(al, folded_arg);
    }
    return ASRUtils::EXPR(ASR::make_StructConstant_t(al, loc, dt_sym,
        folded_args.p, folded_args.size(), type));
}

ASR::expr_t* CommonVisitorBase::get_static_struct_initializer(ASR::expr_t* init) {
    ASR::expr_t* constant = init;
    if (ASR::is_a<ASR::StructConstructor_t>(*init)) {
        ASR::StructConstructor_t* constructor = ASR::down_cast<ASR::StructConstructor_t>(init);
        if (constructor->m_value) {
            constant = constructor->m_value;
        }
    }
    if (ASR::is_a<ASR::StructConstant_t>(*constant)) {
        ASR::StructConstant_t* c = ASR::down_cast<ASR::StructConstant_t>(constant);
        return get_folded_struct_constant(init->base.loc, c->m_dt_sym,
            c->m_args, c->n_args, c->m_type);
    } else if (ASR::is_a<ASR::StructConstructor_t>(*constant)) {
        ASR::StructConstructor_t* c = ASR::down_cast<ASR::StructConstructor_t>(constant);
        return get_folded_struct_constant(init->base.loc, c->m_dt_sym,
            c->m_args, c->n_args, c->m_type);
    }
    return nullptr;
}

ASR::expr_t* CommonVisitorBase::broadcast_struct_scalar_to_array_initializer(
        const Location& loc, ASR::expr_t* init, ASR::ttype_t* type) {
    if (init == nullptr || !ASRUtils::is_array(type)
            || !ASR::is_a<ASR::StructType_t>(*ASRUtils::type_get_past_array(type))
            || ASRUtils::is_array(ASRUtils::expr_type(init))) {
        return nullptr;
    }
    ASR::expr_t* value = ASRUtils::expr_value(init);
    ASR::expr_t* element = (value && ASR::is_a<ASR::StructConstant_t>(*value)) ? value : init;
    if (!ASR::is_a<ASR::StructConstant_t>(*element)) {
        ASR::expr_t* static_init = get_static_struct_initializer(init);
        if (static_init) {
            element = static_init;
        }
    }
    if (!ASR::is_a<ASR::StructConstant_t>(*element)) {
        return nullptr;
    }
    ASR::dimension_t* mdims = nullptr;
    size_t ndims = ASRUtils::extract_dimensions_from_ttype(type, mdims);
    Vec<ASR::expr_t*> lengths;
    lengths.reserve(al, ndims);
    for (size_t i = 0; i < ndims; i++) {
        ASR::expr_t* length = mdims[i].m_length;
        if (length == nullptr) {
            diag.add(Diagnostic(
                "array of derived type initialized with a scalar structure constructor must have constant explicit shape",
                Level::Error, Stage::Semantic, {
                    Label("", {loc})
                }));
            throw SemanticAbort();
        }
        ASR::expr_t* length_value = ASRUtils::expr_value(length);
        if (length_value == nullptr ||
                !ASR::is_a<ASR::IntegerConstant_t>(*length_value)) {
            diag.add(Diagnostic(
                "array of derived type initialized with a scalar structure constructor must have constant explicit shape",
                Level::Error, Stage::Semantic, {
                    Label("", {loc})
                }));
            throw SemanticAbort();
        }
        lengths.push_back(al, length_value);
    }
    Vec<ASR::dimension_t> shape_dims;
    shape_dims.reserve(al, 1);
    ASR::dimension_t shape_dim;
    shape_dim.loc = loc;
    shape_dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
        1, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
    shape_dim.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
        ndims, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4))));
    shape_dims.push_back(al, shape_dim);
    ASR::ttype_t* shape_type = ASRUtils::TYPE(ASR::make_Array_t(al, loc,
        ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4)),
        shape_dims.p, shape_dims.size(),
        ASR::array_physical_typeType::FixedSizeArray,
        ASR::memory_spaceType::Global));
    ASR::expr_t* shape = ASRUtils::EXPR(
        ASRUtils::make_ArrayConstructor_t_util(al, loc, lengths.p,
            lengths.size(), shape_type, ASR::arraystorageType::ColMajor));
    return ASRUtils::EXPR(ASR::make_ArrayBroadcast_t(al, loc,
        element, shape, type, nullptr));
}

std::vector<CommonVisitorBase::ParentComponent> CommonVisitorBase::get_parent_components(ASR::symbol_t* struct_sym) {
    std::vector<ParentComponent> parents;
    ASR::Struct_t* struct_type = ASR::down_cast<ASR::Struct_t>(
        ASRUtils::symbol_get_past_external(struct_sym));
    while (struct_type->m_parent != nullptr) {
        ASR::symbol_t* ancestor = ASRUtils::symbol_get_past_external(
            struct_type->m_parent);
        parents.push_back({to_lower(ASRUtils::symbol_name(
            struct_type->m_parent)), ancestor});
        struct_type = ASR::down_cast<ASR::Struct_t>(ancestor);
    }
    return parents;
}

ASR::symbol_t* CommonVisitorBase::get_positional_ancestor(ASR::symbol_t* struct_sym,
        ASR::expr_t* value, ASR::symbol_t* first_member) {
    ASR::ttype_t* value_type = ASRUtils::expr_type(value);
    if (ASRUtils::is_array(value_type)
            || !ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(value_type))) {
        return nullptr;
    }
    ASR::symbol_t* value_sym = ASRUtils::get_struct_sym_from_struct_expr(value);
    if (value_sym == nullptr) {
        return nullptr;
    }
    value_sym = ASRUtils::symbol_get_past_external(value_sym);
    if (!ASR::is_a<ASR::Struct_t>(*value_sym)) {
        return nullptr;
    }
    ASR::Struct_t* value_struct = ASR::down_cast<ASR::Struct_t>(value_sym);
    if (!ASRUtils::is_parent(value_struct, ASR::down_cast<ASR::Struct_t>(
            ASRUtils::symbol_get_past_external(struct_sym)))) {
        return nullptr;
    }
    if (first_member == nullptr || !ASR::is_a<ASR::Variable_t>(*first_member)) {
        return value_sym;
    }
    ASR::Variable_t* member_var = ASR::down_cast<ASR::Variable_t>(first_member);
    ASR::ttype_t* member_type = ASRUtils::extract_type(member_var->m_type);
    if (!ASR::is_a<ASR::StructType_t>(*member_type)
            || member_var->m_type_declaration == nullptr) {
        return value_sym;
    }
    ASR::symbol_t* member_type_sym = ASRUtils::symbol_get_past_external(
        member_var->m_type_declaration);
    if (!ASR::is_a<ASR::Struct_t>(*member_type_sym)) {
        return value_sym;
    }
    ASR::Struct_t* member_struct = ASR::down_cast<ASR::Struct_t>(member_type_sym);
    bool member_holds_value = ASRUtils::is_class_type(member_type)
        ? ASRUtils::can_pass_derviedtype_arg_to_parameter(value_struct, member_struct)
        : value_struct == member_struct;
    return member_holds_value ? nullptr : value_sym;
}

CommonVisitorBase::NullReference CommonVisitorBase::get_null_reference(AST::expr_t* arg, ASR::expr_t* value) {
    if (value == nullptr || !AST::is_a<AST::FuncCallOrArray_t>(*arg)
            || !ASR::is_a<ASR::PointerNullConstant_t>(*value)) {
        return NullReference::none;
    }
    AST::FuncCallOrArray_t* call = AST::down_cast<AST::FuncCallOrArray_t>(arg);
    return call->n_args + call->n_keywords > 0
        ? NullReference::with_mold : NullReference::without_mold;
}

bool CommonVisitorBase::null_constant_fits_component(ASR::expr_t* value,
        ASR::Variable_t* member_var) {
    ASR::ttype_t* member_type = member_var->m_type;
    ASR::ttype_t* value_scalar = ASRUtils::extract_type(ASRUtils::expr_type(value));
    ASR::ttype_t* member_scalar = ASRUtils::extract_type(member_type);
    bool value_is_c_pointer = ASR::is_a<ASR::CPtr_t>(*value_scalar);
    bool member_is_c_pointer = ASR::is_a<ASR::CPtr_t>(*member_scalar);
    if (value_is_c_pointer || member_is_c_pointer) {
        return value_is_c_pointer && member_is_c_pointer
            && ASRUtils::check_equal_type(member_scalar, value_scalar,
                nullptr, value);
    }
    if (ASR::is_a<ASR::StructType_t>(*value_scalar)
            || ASR::is_a<ASR::StructType_t>(*member_scalar)
            || ASR::is_a<ASR::FunctionType_t>(*value_scalar)
            || ASR::is_a<ASR::FunctionType_t>(*member_scalar)) {
        return true;
    }
    return ASRUtils::check_equal_type(member_scalar, value_scalar, nullptr, nullptr);
}

std::string CommonVisitorBase::fortran_type_to_str(ASR::ttype_t* type,
        ASR::symbol_t* type_declaration) {
    ASR::ttype_t* scalar = ASRUtils::extract_type(type);
    std::string type_str = ASRUtils::type_to_str_fortran_symbol(
        scalar, type_declaration, true);
    if (ASR::is_a<ASR::StructType_t>(*scalar)) {
        type_str = "type(" + type_str + ")";
    }
    ASR::dimension_t* dims = nullptr;
    size_t rank = ASRUtils::extract_dimensions_from_ttype(type, dims);
    for (size_t d = 0; d < rank; d++) {
        type_str += d == 0 ? ", dimension(" : ", ";
        int64_t extent = 0;
        if (dims[d].m_length != nullptr && ASRUtils::extract_value(
                ASRUtils::expr_value(dims[d].m_length), extent)) {
            type_str += std::to_string(extent);
        } else {
            type_str += ":";
        }
        if (d + 1 == rank) {
            type_str += ")";
        }
    }
    return type_str;
}

void CommonVisitorBase::declare_forward_procedure_target(ASR::symbol_t* member,
        AST::expr_t* value) {
    if (is_body_visitor || !in_module || !AST::is_a<AST::Name_t>(*value)
            || member == nullptr || !ASR::is_a<ASR::Variable_t>(*member)
            || current_scope->parent == nullptr
            || current_scope->parent->parent != nullptr) {
        return;
    }
    ASR::Variable_t* member_var = ASR::down_cast<ASR::Variable_t>(member);
    ASR::FunctionType_t* iface = ASRUtils::get_procedure_pointer_component_type(
        member_var);
    if (iface == nullptr) {
        return;
    }
    std::string name = to_lower(AST::down_cast<AST::Name_t>(value)->m_id);
    ASR::symbol_t* declared = current_scope->resolve_symbol(name);
    if (declared != nullptr) {
        if (forward_procedure_targets.count(declared) > 0) {
            deferred_procedure_component_values.push_back(
                {declared, member_var, value->base.loc});
        }
        return;
    }
    ASR::FunctionType_t* placeholder_type = ASR::down_cast<ASR::FunctionType_t>(
        ASRUtils::duplicate_type(al, &iface->base));
    placeholder_type->m_deftype = ASR::deftypeType::Interface;
    SymbolTable* fn_scope = al.make_new<SymbolTable>(current_scope);
    ASR::symbol_t* placeholder = ASR::down_cast<ASR::symbol_t>(
        ASR::make_Function_t(al, value->base.loc, fn_scope, s2c(al, name),
            &placeholder_type->base, nullptr, 0, nullptr, 0, nullptr, 0,
            nullptr, ASR::accessType::Public, false, false, nullptr,
            nullptr, nullptr, nullptr));
    current_scope->add_symbol(name, placeholder);
    pending_proc_placeholders.push_back({name, placeholder, current_scope});
    forward_procedure_targets.insert(placeholder);
    deferred_procedure_component_values.push_back(
        {placeholder, member_var, value->base.loc});
}

void CommonVisitorBase::report_internal_initial_procedure_target(const std::string& name,
        const std::string& pointer, const Location& loc) {
    diag.add(Diagnostic("internal procedure '" + name
            + "' cannot be the initial target of " + pointer,
        Level::Error, Stage::Semantic, {
            Label("an initial procedure target shall be a nonelemental "
                "external or module procedure or a specific intrinsic "
                "function (Fortran 2023 C1519)", {loc})}));
    if (!compiler_options.continue_compilation) {
        throw SemanticAbort();
    }
}

ASR::Function_t* CommonVisitorBase::procedure_interface(ASR::symbol_t* sym) {
    if (sym != nullptr) {
        sym = ASRUtils::symbol_get_past_external(sym);
        if (ASR::is_a<ASR::Variable_t>(*sym)) {
            sym = ASR::down_cast<ASR::Variable_t>(sym)->m_type_declaration;
        }
    }
    if (sym != nullptr) {
        sym = ASRUtils::symbol_get_past_external(sym);
    }
    if (sym == nullptr || !ASR::is_a<ASR::Function_t>(*sym)) {
        return nullptr;
    }
    return ASR::down_cast<ASR::Function_t>(sym);
}

ASR::Function_t* CommonVisitorBase::procedure_interface(ASR::expr_t* e) {
    if (ASR::is_a<ASR::Var_t>(*e)) {
        return procedure_interface(ASR::down_cast<ASR::Var_t>(e)->m_v);
    } else if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
        return procedure_interface(
            ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_m);
    }
    return nullptr;
}

std::string CommonVisitorBase::procedure_interface_to_str(ASR::Function_t* iface) {
    if (iface != nullptr) {
        std::string name = iface->m_name;
        // Skip compiler-generated interface names.
        if (name.rfind("__", 0) != 0) {
            return "procedure(" + name + ")";
        }
    }
    return "procedure";
}

bool CommonVisitorBase::procedure_component_value_matches(ASR::Variable_t* member_var,
        ASR::expr_t* arg) {
    ASR::FunctionType_t* member_func_type =
        ASRUtils::get_procedure_pointer_component_type(member_var);
    ASR::FunctionType_t* arg_func_type =
        ASRUtils::get_procedure_type_from_expr(arg);
    if (member_func_type == nullptr || arg_func_type == nullptr
            || ASRUtils::is_opaque_procedure_type(&member_func_type->base)
            || ASRUtils::is_opaque_procedure_type(&arg_func_type->base)) {
        return true;
    }
    ASR::Function_t* member_iface = procedure_interface(&member_var->base);
    ASR::Function_t* arg_iface = procedure_interface(arg);
    // Derived-type dummy arguments are told apart through the procedures
    // declaring the interfaces, when both are known.
    ASR::expr_t* member_expr = nullptr;
    ASR::expr_t* arg_expr = nullptr;
    if (member_iface != nullptr && arg_iface != nullptr
            && member_iface->n_args == member_func_type->n_arg_types
            && arg_iface->n_args == arg_func_type->n_arg_types) {
        member_expr = ASRUtils::EXPR(ASR::make_Var_t(al, arg->base.loc,
            &member_iface->base));
        arg_expr = ASRUtils::EXPR(ASR::make_Var_t(al, arg->base.loc,
            &arg_iface->base));
    }
    if (ASRUtils::types_equal(&member_func_type->base,
            &arg_func_type->base, member_expr, arg_expr)) {
        return true;
    }
    diag.add(Diagnostic("interface mismatch for procedure pointer component '"
        + std::string(member_var->m_name) + "' in structure constructor",
        Level::Error, Stage::Semantic, {
            Label("incompatible interfaces ("
                + procedure_interface_to_str(member_iface) + " and "
                + procedure_interface_to_str(arg_iface) + ")",
                {arg->base.loc})}));
    return false;
}

void CommonVisitorBase::set_null_context_to_component(ASR::symbol_t* member, const Location& loc) {
    current_variable_type_ = nullptr;
    current_struct_type_var_expr = nullptr;
    if (member == nullptr || !ASR::is_a<ASR::Variable_t>(*member)) {
        return;
    }
    ASR::Variable_t* member_var = ASR::down_cast<ASR::Variable_t>(member);
    current_variable_type_ = member_var->m_type;
    ASR::ttype_t* member_type = ASRUtils::extract_type(member_var->m_type);
    if (ASR::is_a<ASR::StructType_t>(*member_type) ||
            ASR::is_a<ASR::FunctionType_t>(*member_type)) {
        // The mold of a derived type or procedure `null()` is the
        // component, which lives in the type's scope, so refer to its
        // type as seen from the current scope, as a default initializer
        // filled into a constructor does.
        ASR::expr_t* member_null = ASRUtils::EXPR(ASR::make_PointerNullConstant_t(
            al, loc, member_var->m_type, ASRUtils::EXPR(ASR::make_Var_t(al, loc, member))));
        member_null = ASRUtils::externalize_struct_refs_in_init(al, member_null, current_scope);
        ASR::PointerNullConstant_t* null_context =
            ASR::down_cast<ASR::PointerNullConstant_t>(member_null);
        current_variable_type_ = null_context->m_type;
        current_struct_type_var_expr = null_context->m_var_expr;
    }
}

bool CommonVisitorBase::check_struct_constructor_array_arg_shape(ASR::expr_t* arg,
        ASR::symbol_t* member) {
    ASR::ttype_t* member_type = ASRUtils::symbol_type(member);
    std::string member_name = ASRUtils::symbol_name(member);
    ASR::dimension_t* member_dims = nullptr;
    ASR::dimension_t* arg_dims = nullptr;
    size_t member_rank = ASRUtils::extract_dimensions_from_ttype(
        member_type, member_dims);
    size_t arg_rank = ASRUtils::extract_dimensions_from_ttype(
        ASRUtils::expr_type(arg), arg_dims);
    if (member_rank != arg_rank) {
        diag.add(Diagnostic("component '" + member_name + "' has rank "
            + std::to_string(member_rank) + ", but the structure constructor "
            "argument has rank " + std::to_string(arg_rank),
            Level::Error, Stage::Semantic, {
                Label("rank " + std::to_string(arg_rank) + " argument",
                    {arg->base.loc})}));
        if (!compiler_options.continue_compilation) {
            throw SemanticAbort();
        }
        return false;
    }
    if (ASRUtils::is_allocatable_or_pointer(member_type)) {
        return true;
    }
    for (size_t d = 0; d < member_rank; d++) {
        int64_t member_extent = 0, arg_extent = 0;
        if (member_dims[d].m_length == nullptr || arg_dims[d].m_length == nullptr
                || !ASRUtils::extract_value(ASRUtils::expr_value(
                    member_dims[d].m_length), member_extent)
                || !ASRUtils::extract_value(ASRUtils::expr_value(
                    arg_dims[d].m_length), arg_extent)
                || member_extent == arg_extent) {
            continue;
        }
        diag.add(Diagnostic("component '" + member_name + "' has extent "
            + std::to_string(member_extent) + " in dimension "
            + std::to_string(d + 1) + ", but the structure constructor "
            "argument has extent " + std::to_string(arg_extent),
            Level::Error, Stage::Semantic, {
                Label("extent " + std::to_string(arg_extent) + " in dimension "
                    + std::to_string(d + 1), {arg->base.loc})}));
        if (!compiler_options.continue_compilation) {
            throw SemanticAbort();
        }
        return false;
    }
    return true;
}

void CommonVisitorBase::conform_struct_constructor_args(Vec<ASR::call_arg_t>& vals,
        const std::vector<ASR::symbol_t*>& members) {
    for (size_t i = 0; i < vals.size() && i < members.size(); i++) {
        ASR::expr_t* arg = vals[i].m_value;
        if (arg == nullptr || members[i] == nullptr
                || !ASR::is_a<ASR::Variable_t>(*members[i])) {
            continue;
        }
        ASR::ttype_t* member_type = ASRUtils::symbol_type(members[i]);
        if (ASRUtils::is_array(ASRUtils::expr_type(arg))
                && !check_struct_constructor_array_arg_shape(arg, members[i])) {
            // The mismatch is reported; drop the argument so the
            // constructor stays well formed.
            vals.p[i].m_value = nullptr;
            continue;
        }
        if (ASRUtils::is_allocatable_or_pointer(member_type)) {
            continue;
        }
        ASR::ttype_t* element_type = ASRUtils::type_get_past_array(member_type);
        int64_t element_len = 0;
        if (ASRUtils::is_character(*element_type)) {
            ASR::expr_t* element_len_expr =
                ASR::down_cast<ASR::String_t>(element_type)->m_len;
            if (element_len_expr == nullptr || !ASRUtils::extract_value(
                    ASRUtils::expr_value(element_len_expr), element_len)) {
                continue;
            }
        }
        ASR::expr_t* value = ASRUtils::expr_value(arg);
        if (value != nullptr && ASRUtils::is_character(*element_type)) {
            // Case: `t("hi")` for `character(len=3) :: c`.
            ASR::expr_t* adjusted = ASRUtils::conform_character_constant_length(
                value, element_len, arg->base.loc, al);
            if (adjusted != nullptr) {
                value = adjusted;
                arg = adjusted;
                vals.p[i].m_value = adjusted;
            }
        }
        if (!ASRUtils::is_array(member_type)
                || ASRUtils::is_array(ASRUtils::expr_type(arg))) {
            continue;
        }
        // An empty array constant of a derived type takes the whole
        // lowered assignment with it (#13382), so a derived type
        // component with no elements, which has nothing to spread
        // anyway, is left as it is.
        if (ASRUtils::get_fixed_size_of_array(member_type) == 0) {
            continue;
        }
        if (value == nullptr && ASR::is_a<ASR::StructConstructor_t>(*arg)
                && ASRUtils::is_value_constant(arg)
                && ASRUtils::is_byte_representable_struct(
                    ASR::down_cast<ASR::StructConstructor_t>(arg)->m_dt_sym)) {
            // A derived type constructor is folded into a StructConstant
            // only once the whole constructor is known to be constant, so
            // a nested one has no value of its own yet. Fold it here, as
            // the shape it is given below is the component's.
            value = fold_struct_constant_arg(arg);
        }
        if (value == nullptr) {
            continue;
        }
        // A null constant is an ordinary value only for a C pointer
        // component; for a pointer or an allocatable component it is an
        // association status, and those components are skipped above.
        bool is_c_pointer_null = ASR::is_a<ASR::PointerNullConstant_t>(*value)
            && ASR::is_a<ASR::CPtr_t>(*element_type);
        if (!(ASR::is_a<ASR::IntegerConstant_t>(*value)
                || ASR::is_a<ASR::UnsignedIntegerConstant_t>(*value)
                || ASR::is_a<ASR::RealConstant_t>(*value)
                || ASR::is_a<ASR::ComplexConstant_t>(*value)
                || ASR::is_a<ASR::LogicalConstant_t>(*value)
                || ASR::is_a<ASR::StringConstant_t>(*value)
                || ASR::is_a<ASR::StructConstant_t>(*value)
                || is_c_pointer_null)) {
            continue;
        }
        // The broadcast stores its elements as raw bytes, so a derived
        // type value is spread only when every component of its type is
        // stored inline.
        if (ASR::is_a<ASR::StructConstant_t>(*value)
                && !ASRUtils::is_byte_representable_struct(
                    ASR::down_cast<ASR::StructConstant_t>(value)->m_dt_sym)) {
            continue;
        }
        // Case: `t(5.0)` for `real :: x(3)`, like `real :: x(3) = 5.0`,
        // `t(c_null_ptr)` for `type(c_ptr) :: p(2)`, and `t(u(5))` for
        // `type(u) :: c(2)`.
        ASR::expr_t* broadcast = ASRUtils::broadcast_scalar_constant_to_array(
            al, arg->base.loc, value, member_type);
        if (broadcast != nullptr) {
            vals.p[i].m_value = broadcast;
        }
    }
}

const Location& CommonVisitorBase::extra_argument_loc(AST::fnarg_t* args, size_t n_args,
        size_t first_extra, const Location& constructor_loc) {
    if (first_extra >= n_args) {
        return constructor_loc;
    }
    if (args[first_extra].m_end == nullptr) {
        return args[first_extra].loc;
    }
    return args[first_extra].m_end->base.loc;
}

void CommonVisitorBase::check_allocatable_initializer_components(ASR::symbol_t* dt_sym,
        ASR::call_arg_t* args, size_t n_args) {
    StructConstructorInfo info = get_struct_constructor_info(dt_sym);
    if (!info.kind_indices.empty() || info.members.size() != n_args) {
        return;
    }
    for (size_t i = 0; i < n_args; i++) {
        if (args[i].m_value == nullptr || info.members[i] == nullptr
                || !ASR::is_a<ASR::Variable_t>(*info.members[i])) {
            continue;
        }
        ASR::Variable_t* member_var =
            ASR::down_cast<ASR::Variable_t>(info.members[i]);
        if (ASRUtils::is_allocatable(member_var->m_type)) {
            diag.add(Diagnostic(
                "an initialization expression cannot specify a value for allocatable component '"
                + std::string(member_var->m_name) + "' of a structure constructor",
                Level::Error, Stage::Semantic, {
                    Label("", {args[i].m_value->base.loc})}));
            if (!compiler_options.continue_compilation) {
                throw SemanticAbort();
            }
            args[i].m_value = nullptr;
            continue;
        }
        ASR::expr_t* value = ASRUtils::expr_value(args[i].m_value);
        ASR::expr_t* element = (value
            && ASR::is_a<ASR::StructConstant_t>(*value)) ? value
            : args[i].m_value;
        if (ASR::is_a<ASR::StructConstructor_t>(*element)) {
            ASR::StructConstructor_t* nested =
                ASR::down_cast<ASR::StructConstructor_t>(element);
            check_allocatable_initializer_components(nested->m_dt_sym,
                nested->m_args, nested->n_args);
        } else if (ASR::is_a<ASR::StructConstant_t>(*element)) {
            ASR::StructConstant_t* nested =
                ASR::down_cast<ASR::StructConstant_t>(element);
            check_allocatable_initializer_components(nested->m_dt_sym,
                nested->m_args, nested->n_args);
        }
    }
}

ASR::asr_t* CommonVisitorBase::make_struct_constructor(const Location& loc, ASR::symbol_t* v,
        Vec<ASR::call_arg_t>& vals, const std::vector<ASR::symbol_t*>& members,
        bool is_const) {
    conform_struct_constructor_args(vals, members);

    // The restriction is on an initialization expression only. A
    // structure constructor in an assignment may supply a value for an
    // allocatable component, which allocates it, and is valid even
    // though its arguments are constants.
    if (is_const
            && restricted_expr_context == RestrictedExprContext::InitExpr) {
        check_allocatable_initializer_components(v, vals.p, vals.size());
    }

    // Ensure all values are constant before creating StructConstant
    for (size_t i = 0; i < vals.size(); i++) {
        ASR::symbol_t* member = i < members.size() ? members[i] : nullptr;
        if (!ASRUtils::is_constant_struct_component_value(member,
                vals.p[i].m_value)) {
            is_const = false;
            break;
        }
    }
    ASR::ttype_t* der = ASRUtils::make_StructType_t_util(al, loc, v, true);
    if (is_const) {
        // A StructConstant holds constants only: store the folded value of
        // each argument, not the parameter reference or nested constructor
        for (size_t i = 0; i < vals.size(); i++) {
            vals.p[i].m_value = fold_struct_constant_arg(vals.p[i].m_value);
        }
        return ASR::make_StructConstant_t(al, loc,
                v, vals.p, vals.size(), der);
    }
    return ASR::make_StructConstructor_t(al, loc,
            v, vals.p, vals.size(), der, nullptr);
}

bool CommonVisitorBase::is_struct_type_parameter(ASR::expr_t* expr) {
    if (!ASR::is_a<ASR::Var_t>(*expr)) {
        return false;
    }
    ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(expr)->m_v);
    if (!ASR::is_a<ASR::Variable_t>(*sym)) {
        return false;
    }
    ASR::asr_t* owner = ASR::down_cast<ASR::Variable_t>(sym)->m_parent_symtab->asr_owner;
    return owner && ASR::is_a<ASR::symbol_t>(*owner) &&
        ASR::is_a<ASR::Struct_t>(*ASR::down_cast<ASR::symbol_t>(owner));
}

ASR::expr_t* CommonVisitorBase::fold_struct_constant_arg(ASR::expr_t* arg) {
    if (arg == nullptr) {
        return arg;
    }
    // A type parameter keeps its reference: its value is a placeholder
    // until the parameterized type is instantiated
    if (is_struct_type_parameter(arg)) {
        return arg;
    }
    if (ASR::is_a<ASR::StructConstructor_t>(*arg)) {
        ASR::StructConstructor_t* sc = ASR::down_cast<ASR::StructConstructor_t>(arg);
        if (sc->m_value) {
            return fold_struct_constant_arg(sc->m_value);
        }
        Vec<ASR::call_arg_t> args;
        args.reserve(al, sc->n_args);
        for (size_t i = 0; i < sc->n_args; i++) {
            ASR::call_arg_t folded = sc->m_args[i];
            folded.m_value = fold_struct_constant_arg(folded.m_value);
            if (folded.m_value && is_struct_type_parameter(folded.m_value)) {
                return arg;
            }
            args.push_back(al, folded);
        }
        return ASRUtils::EXPR(ASR::make_StructConstant_t(al, arg->base.loc,
            sc->m_dt_sym, args.p, args.size(), sc->m_type));
    }
    ASR::expr_t* value = ASRUtils::expr_value(arg);
    if (value && value != arg) {
        return fold_struct_constant_arg(value);
    }
    return arg;
}

ASR::expr_t* CommonVisitorBase::get_struct_member_value_from_constant_array(
        ASR::expr_t* value, ASR::symbol_t* member,
        ASR::ttype_t* result_type) {
    ASR::expr_t* scalar_value =
        ASRUtils::get_struct_member_value_from_constant(value, member);
    if (scalar_value) {
        if (ASRUtils::is_array(result_type)
                && !ASRUtils::is_array(ASRUtils::expr_type(scalar_value))) {
            ASR::expr_t* broadcast = ASRUtils::broadcast_scalar_constant_to_array(
                al, scalar_value->base.loc, scalar_value, result_type);
            if (broadcast) {
                return broadcast;
            }
        }
        return scalar_value;
    }

    ASR::expr_t* expr_value = ASRUtils::expr_value(value);
    if (expr_value && expr_value != value) {
        value = expr_value;
    }
    Vec<ASR::expr_t*> elements;
    Location loc = value->base.loc;
    if (ASR::is_a<ASR::ArrayConstant_t>(*value)) {
        ASR::ArrayConstant_t* array_constant =
            ASR::down_cast<ASR::ArrayConstant_t>(value);
        if (!ASR::is_a<ASR::StructType_t>(
                *ASRUtils::type_get_past_array(array_constant->m_type))) {
            return nullptr;
        }
        int64_t size = ASRUtils::get_fixed_size_of_array(array_constant->m_type);
        if (size < 0) {
            return nullptr;
        }
        elements.reserve(al, size);
        for (int64_t i = 0; i < size; i++) {
            ASR::expr_t* element =
                ASRUtils::fetch_ArrayConstant_value(al, array_constant, i);
            ASR::expr_t* member_value =
                ASRUtils::get_struct_member_value_from_constant(element, member);
            if (member_value == nullptr) {
                return nullptr;
            }
            elements.push_back(al, member_value);
        }
    } else if (ASR::is_a<ASR::ArrayConstructor_t>(*value)) {
        ASR::ArrayConstructor_t* array_constructor =
            ASR::down_cast<ASR::ArrayConstructor_t>(value);
        elements.reserve(al, array_constructor->n_args);
        for (size_t i = 0; i < array_constructor->n_args; i++) {
            ASR::expr_t* element = ASRUtils::expr_value(array_constructor->m_args[i]);
            if (element == nullptr) {
                element = array_constructor->m_args[i];
            }
            ASR::expr_t* member_value =
                ASRUtils::get_struct_member_value_from_constant(element, member);
            if (member_value == nullptr) {
                return nullptr;
            }
            elements.push_back(al, member_value);
        }
    } else {
        return nullptr;
    }

    if (!ASRUtils::is_array(result_type)) {
        return nullptr;
    }
    return ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(al,
        loc, elements.p, elements.size(), result_type,
        ASR::arraystorageType::ColMajor));
}

int64_t CommonVisitorBase::get_based_indexing(ASR::symbol_t* v) {
    if (v != nullptr && ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(v))) {
        ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(v));
        if (ASRUtils::is_array(var->m_type) && var->m_value && var->m_storage == ASR::storage_typeType::Parameter) {
            ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(var->m_type);
            for (size_t i = 0; i < arr->n_dims; i++) {
                ASR::dimension_t dim = arr->m_dims[i];
                if (dim.m_start != nullptr) {
                    ASR::expr_t *start = ASRUtils::expr_value(dim.m_start);
                    if (start) {
                        ASR::IntegerConstant_t *start2 = ASR::down_cast<ASR::IntegerConstant_t>(start);
                        return start2->m_n;
                    }
                }
            }
        }
    }
    return 1; // default
}

void CommonVisitorBase::give_struct_member_item_shape_of_base(ASR::expr_t* expr) {
    if( !ASR::is_a<ASR::ArrayItem_t>(*expr) ) {
        return;
    }
    ASR::ArrayItem_t* array_item = ASR::down_cast<ASR::ArrayItem_t>(expr);
    ASR::expr_t* base = ASRUtils::struct_base_lending_shape(array_item);
    if( base == nullptr || ASRUtils::is_array(array_item->m_type) ) {
        return;
    }
    ASR::ttype_t* base_type = ASRUtils::type_get_past_allocatable_pointer(
        ASRUtils::expr_type(base));
    ASR::Array_t* base_array = ASR::down_cast<ASR::Array_t>(base_type);
    // The reference has as many elements as the base in every dimension,
    // but it is not the base: like any array-valued reference that is not
    // a whole array, its bounds start at one. Taking the base's lower
    // bounds as well would report `w(2:5)%u(1)` as running from 2 to 5.
    Vec<ASR::dimension_t> dims;
    dims.reserve(al, base_array->n_dims);
    ASR::ttype_t* index_type = ASRUtils::TYPE(ASR::make_Integer_t(
        al, expr->base.loc, compiler_options.po.default_integer_kind));
    for( size_t i = 0; i < base_array->n_dims; i++ ) {
        ASR::dimension_t dim;
        dim.loc = base_array->m_dims[i].loc;
        dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
            al, expr->base.loc, 1, index_type));
        dim.m_length = base_array->m_dims[i].m_length;
        dims.push_back(al, dim);
    }
    array_item->m_type = ASRUtils::duplicate_type(al, array_item->m_type,
        &dims, base_array->m_physical_type, true);
    // A compile time value folded out of a single element of the base
    // describes one element, not the array this reference denotes.
    array_item->m_value = nullptr;
    array_item->m_in_bounds = false;
}

void CommonVisitorBase::check_subscript_types(const Vec<ASR::array_index_t>& args,
        bool is_substring) {
    std::string index_kind = is_substring ? "a substring index"
        : "an array subscript";
    for (const ASR::array_index_t& a : args) {
        bool is_vector_subscript_allowed = !is_substring &&
            a.m_left == nullptr && a.m_step == nullptr;
        ASR::expr_t* indices[] = {a.m_left, a.m_right, a.m_step};
        for (ASR::expr_t* index : indices) {
            if (index == nullptr) {
                continue;
            }
            ASR::ttype_t* index_type = ASRUtils::expr_type(index);
            if (!ASRUtils::is_integer(*index_type) &&
                    !ASRUtils::is_unsigned_integer(*index_type)) {
                diag.add(Diagnostic(
                    index_kind + " must be of type integer, found " +
                    ASRUtils::type_to_str_with_kind(index_type, index),
                    Level::Error, Stage::Semantic, {
                        Label("", {index->base.loc})}));
                throw SemanticAbort();
            }
            if (ASRUtils::is_array(index_type) &&
                    !is_vector_subscript_allowed) {
                diag.add(Diagnostic(
                    is_substring ? "a substring index must be scalar"
                        : "a bound or stride of a subscript triplet must "
                          "be scalar",
                    Level::Error, Stage::Semantic, {
                        Label("", {index->base.loc})}));
                throw SemanticAbort();
            }
        }
    }
}

int CommonVisitorBase::omitted_section_bound_kind(ASR::expr_t* other, ASR::expr_t* step) {
    int kind = compiler_options.descriptor_index_64 ? 8 : 4;
    for (ASR::expr_t* e : {other, step}) {
        if (e != nullptr && ASRUtils::is_integer(*ASRUtils::expr_type(e)) &&
                !ASRUtils::is_array(ASRUtils::expr_type(e))) {
            kind = std::max(kind,
                ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(e)));
        }
    }
    return kind;
}

ASR::expr_t* CommonVisitorBase::fold_substring_value(const Location& loc,
        ASR::expr_t* string_value, ASR::expr_t* start, ASR::expr_t* end,
        ASR::expr_t* step) {
    if (string_value == nullptr ||
            !ASR::is_a<ASR::StringConstant_t>(*string_value)) {
        return nullptr;
    }
    ASR::StringConstant_t* string_constant =
        ASR::down_cast<ASR::StringConstant_t>(string_value);
    ASR::String_t* string_type =
        ASRUtils::get_string_type(string_constant->m_type);
    int64_t start_value = 0, end_value = 0, step_value = 0, string_len = 0;
    if (!ASRUtils::extract_value(ASRUtils::expr_value(start), start_value) ||
            !ASRUtils::extract_value(ASRUtils::expr_value(end), end_value) ||
            !ASRUtils::extract_value(ASRUtils::expr_value(step), step_value) ||
            !ASRUtils::extract_value(string_type->m_len, string_len) ||
            step_value != 1) {
        return nullptr;
    }
    // A substring whose end is before its start has zero length, whatever
    // its bounds are. Any other out of bounds substring is not folded.
    int64_t substring_len = 0;
    if (start_value <= end_value) {
        if (start_value < 1 || end_value > string_len) {
            return nullptr;
        }
        substring_len = end_value - start_value + 1;
    }
    std::vector<std::string> characters = ASRUtils::string_value_characters(
        string_constant->m_s, string_type->m_kind);
    // A value shorter than the length of its type is blank padded
    characters.resize(string_len, " ");
    std::string substring;
    for (int64_t i = start_value; i <= end_value; i++) {
        substring += characters[i - 1];
    }
    ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc,
        compiler_options.po.default_integer_kind));
    ASR::ttype_t* substring_type = ASRUtils::TYPE(ASR::make_String_t(al, loc,
        string_type->m_kind,
        ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, substring_len,
            int_type)),
        ASR::ExpressionLength, ASR::DescriptorString));
    return ASRUtils::EXPR(ASR::make_StringConstant_t(al, loc,
        s2c(al, substring), substring_type));
}

void CommonVisitorBase::check_if_type_spec_has_asterisk(const ASR::ttype_t* type) {
    if (type && 
        ASR::is_a<ASR::String_t>(*type) &&
        ASR::down_cast<ASR::String_t>(type)->m_len_kind == ASR::string_length_kindType::AssumedLength) {
        // e.g. [character(len=*) :: "a", "apple"], this isn't allowed
            diag.add(Diagnostic("Type-spec cannot contain an asterisk for a type "
                "parameter", Level::Error, Stage::Semantic,
                {Label("",{type->base.loc})}));
            throw SemanticAbort();
    }
}

int64_t CommonVisitorBase::compute_implied_do_element_count(ASR::ImpliedDoLoop_t* idl) {
    int64_t start_val = 0, end_val = 0, step_val = 1;
    bool bounds_constant =
        ASRUtils::extract_value(idl->m_start, start_val) &&
        ASRUtils::extract_value(idl->m_end, end_val) &&
        (idl->m_increment == nullptr ||
         ASRUtils::extract_value(idl->m_increment, step_val));
    if (!bounds_constant || step_val == 0) {
        return -1;
    }
    int64_t n_iters;
    if (step_val > 0) {
        n_iters = std::max((int64_t)0,
            (end_val - start_val + step_val) / step_val);
    } else {
        n_iters = std::max((int64_t)0,
            (start_val - end_val - step_val) / (-step_val));
    }
    int64_t elements_per_iter = 0;
    for (size_t i = 0; i < idl->n_values; i++) {
        if (ASR::is_a<ASR::ImpliedDoLoop_t>(*idl->m_values[i])) {
            int64_t inner = compute_implied_do_element_count(
                ASR::down_cast<ASR::ImpliedDoLoop_t>(idl->m_values[i]));
            if (inner < 0) return -1;
            elements_per_iter += inner;
        } else if (ASRUtils::is_fixed_size_array(
                       ASRUtils::expr_type(idl->m_values[i]))) {
            elements_per_iter += ASRUtils::get_fixed_size_of_array(
                ASRUtils::expr_type(idl->m_values[i]));
        } else {
            elements_per_iter += 1;
        }
    }
    return n_iters * elements_per_iter;
}

int64_t CommonVisitorBase::compute_array_constructor_element_count(
        ASR::ArrayConstructor_t* ac) {
    int64_t total = 0;
    for (size_t i = 0; i < ac->n_args; i++) {
        ASR::expr_t* elem = ac->m_args[i];
        if (ASR::is_a<ASR::ImpliedDoLoop_t>(*elem)) {
            int64_t idl_count = compute_implied_do_element_count(
                ASR::down_cast<ASR::ImpliedDoLoop_t>(elem));
            if (idl_count < 0) return -1;
            total += idl_count;
        } else if (ASR::is_a<ASR::ArrayConstructor_t>(*elem)) {
            int64_t inner = compute_array_constructor_element_count(
                ASR::down_cast<ASR::ArrayConstructor_t>(elem));
            if (inner < 0) return -1;
            total += inner;
        } else if (ASRUtils::is_fixed_size_array(
                       ASRUtils::expr_type(elem))) {
            total += ASRUtils::get_fixed_size_of_array(
                ASRUtils::expr_type(elem));
        } else {
            total += 1;
        }
    }
    return total;
}

void CommonVisitorBase::fix_exprs_ttype_t(std::vector<ASR::expr_t*>& exprs,
                       Vec<ASR::call_arg_t>& orig_args,
                       ASR::Function_t* orig_func) {
    ASRUtils::ExprStmtDuplicator expr_duplicator(al);
    expr_duplicator.allow_procedure_calls = true;
    ASRUtils::ReplaceArgVisitor arg_replacer(al, current_scope, orig_func,
                    orig_args, current_function_dependencies, current_module_dependencies);
    for( size_t i = 0; i < exprs.size(); i++ ) {
        ASR::expr_t* expri = exprs[i];
        if (expri) {
            expr_duplicator.success = true;
            ASR::expr_t* expri_copy = expr_duplicator.duplicate_expr(expri);
            LCOMPILERS_ASSERT(expr_duplicator.success);
            arg_replacer.current_expr = &expri_copy;
            arg_replacer.replace_expr(expri_copy);
            exprs[i] = expri_copy;
        }
    }
}

void CommonVisitorBase::fill_new_dims(ASR::Array_t* t, const std::vector<ASR::expr_t*>& func_calls,
    Vec<ASR::dimension_t>& new_dims) {
    new_dims.reserve(al, t->n_dims);
    for ( size_t i = 0, j = 0; i < func_calls.size(); i += 2, j++ ) {
        if (func_calls[i] != nullptr) {
            ASR::dimension_t new_dim;
            new_dim.loc = func_calls[i]->base.loc;
            new_dim.m_start = func_calls[i];
            new_dim.m_length = func_calls[i + 1];
            new_dims.push_back(al, new_dim);
        } else {
            new_dims.push_back(al, t->m_dims[j]);
        }
    }
}

ASR::ttype_t* CommonVisitorBase::handle_return_type(ASR::ttype_t *return_type, const Location &loc,
                                 Vec<ASR::call_arg_t>& args,
                                 ASR::Function_t* f) {
    // Rebuild the return type if needed and make FunctionCalls use ExternalSymbol
    std::vector<ASR::expr_t*> func_calls;
    switch( return_type->type ) {
        case ASR::ttypeType::Allocatable: {
            ASR::Allocatable_t* allocatable_t = ASR::down_cast<ASR::Allocatable_t>(return_type);
            return ASRUtils::TYPE(ASRUtils::make_Allocatable_t_util(al, loc,
                ASRUtils::type_get_past_allocatable(
                    ASRUtils::type_get_past_pointer(
                        handle_return_type(allocatable_t->m_type, loc, args, f)))));
        }
        case ASR::ttypeType::Pointer: {
            ASR::Pointer_t* pointer_t = ASR::down_cast<ASR::Pointer_t>(return_type);
            return ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                ASRUtils::type_get_past_allocatable(
                    ASRUtils::type_get_past_pointer(
                        handle_return_type(pointer_t->m_type, loc, args, f)))));
        }
        case ASR::ttypeType::Array: {
            ASR::Array_t* t = ASR::down_cast<ASR::Array_t>(return_type);
            ASR::ttype_t* t_m_type = handle_return_type(t->m_type, loc, args, f);
            fill_expr_in_ttype_t(func_calls, t->m_dims, t->n_dims);
            fix_exprs_ttype_t(func_calls, args, f);
            Vec<ASR::dimension_t> new_dims;
            fill_new_dims(t, func_calls, new_dims);
            ASRUtils::set_function_result_lbounds_to_one(al, new_dims.p, new_dims.size());
            bool for_type = true;
            for( size_t k = 0; k < new_dims.size(); k++ ) {
                if( new_dims[k].m_length &&
                    ASR::is_a<ASR::ArraySize_t>(*new_dims[k].m_length) ) {
                    ASR::ArraySize_t* array_size_t =
                        ASR::down_cast<ASR::ArraySize_t>(new_dims[k].m_length);
                    if( (ASR::is_a<ASR::FunctionCall_t>(*array_size_t->m_v) &&
                        ASRUtils::is_allocatable(array_size_t->m_v)) ||
                        ASR::is_a<ASR::IntrinsicArrayFunction_t>(*array_size_t->m_v) ) {
                        for_type = false;
                        break;
                    }
                }
            }
            return ASRUtils::make_Array_t_util(
                al, loc, t_m_type, new_dims.p, new_dims.size(),
                current_procedure_abi_type, false,
                ASR::DescriptorArray, false, false, for_type);
        }
        case ASR::ttypeType::String: {
            ASR::String_t *t = ASR::down_cast<ASR::String_t>(return_type);
            func_calls.push_back(t->m_len);
            fix_exprs_ttype_t(func_calls, args, f);
            ASR::expr_t* a_len = func_calls[0] ?  func_calls[0] : t->m_len;
            return ASRUtils::TYPE(ASR::make_String_t(
                al, loc, t->m_kind, a_len,
                t->m_len_kind,
                t->m_physical_type));
        }
        case ASR::ttypeType::StructType: {
            return ASRUtils::duplicate_type(al, return_type);
        }
        default: {
            return return_type;
        }
    }
    return nullptr;
}

ASR::asr_t* CommonVisitorBase::create_GenericProcedure(const Location &loc,
            Vec<ASR::call_arg_t>& args, ASR::symbol_t *v) {
    ASR::GenericProcedure_t *p = ASR::down_cast<ASR::GenericProcedure_t>(v);
    int idx = ASRUtils::select_generic_procedure(args, *p, loc,
            [&](const std::string &msg, const Location &loc) {
                    diag.add(Diagnostic(msg, Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                },
            false, false, &pending_implicit_dummy_procedure);
    if( idx == -1 ) {
        std::string v_name = ASRUtils::symbol_name(v);
        v = resolve_intrinsic_function(loc, v_name);
        if( !v ) {
            diag.add(Diagnostic("Couldn't find any function " + v_name + ".",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }
        return create_FunctionCall(loc, v, args);
    }
    ASR::symbol_t *final_sym = p->m_procs[idx];

    ASR::ttype_t *type = nullptr;
    ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(
        ASRUtils::symbol_get_past_external(final_sym));
    ASR::expr_t* first_array_arg = ASRUtils::find_first_array_arg_if_elemental(func, args);
    if (first_array_arg) {
        ASR::dimension_t* array_dims;
        size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
            ASRUtils::expr_type(first_array_arg), array_dims
        );
        Vec<ASR::dimension_t> new_dims;
        new_dims.from_pointer_n_copy(al, array_dims, array_n_dims);
        type = ASRUtils::duplicate_type(al,
                        ASRUtils::get_FunctionType(func)->m_return_var_type,
                        &new_dims);
    } else {
        type = ASRUtils::EXPR2VAR(func->m_return_var)->m_type;
        type = handle_return_type(type, loc, args, func);
    }
    if (ASRUtils::symbol_parent_symtab(final_sym)->get_counter() != current_scope->get_counter()) {
        ADD_ASR_DEPENDENCIES(current_scope, final_sym, current_function_dependencies);
    }
    ASRUtils::insert_module_dependency(final_sym, al, current_module_dependencies);
    validate_missing_required_arguments(loc, args, func);
    ASRUtils::set_absent_optional_arguments_to_null(args, func, al);
    cast_procedure_actuals(args, func);
    return ASRUtils::make_FunctionCall_t_util(al, loc,
        final_sym, v, args.p, args.size(), type,
        nullptr, nullptr, current_scope, current_function_dependencies,
        compiler_options.implicit_argument_casting);
}

bool CommonVisitorBase::is_ieee_support_scalar_inquiry_specific(ASR::symbol_t *spec) {
    if (spec == nullptr) {
        return false;
    }
    std::string mod_name;
    std::string orig_name;
    if (ASR::is_a<ASR::ExternalSymbol_t>(*spec)) {
        ASR::ExternalSymbol_t *ext = ASR::down_cast<ASR::ExternalSymbol_t>(spec);
        mod_name = ext->m_module_name;
        orig_name = ext->m_original_name;
    } else {
        ASR::symbol_t *resolved = ASRUtils::symbol_get_past_external(spec);
        if (resolved == nullptr) {
            return false;
        }
        ASR::Module_t *mod = ASRUtils::get_sym_module0(resolved);
        if (mod == nullptr) {
            return false;
        }
        mod_name = mod->m_name;
        orig_name = ASRUtils::symbol_name(resolved);
    }
    if (mod_name != "lfortran_intrinsic_ieee_arithmetic") {
        return false;
    }
    const char *inquiries[] = {
        "support_sqrt", "support_divide", "support_denormal",
        "support_standard", "support_io", "support_nan",
        "support_inf", "support_datatype"
    };
    for (const char *inquiry : inquiries) {
        std::string suffix = inquiry;
        if (orig_name == "spieee_" + suffix || orig_name == "dpieee_" + suffix) {
            return true;
        }
    }
    return false;
}

bool CommonVisitorBase::has_real_array_actual(const Vec<ASR::call_arg_t> &args) {
    for (size_t i = 0; i < args.size(); i++) {
        ASR::expr_t *value = args.p[i].m_value;
        if (value == nullptr) {
            continue;
        }
        ASR::ttype_t *type = ASRUtils::expr_type(value);
        if (!ASRUtils::is_array(type)) {
            continue;
        }
        if (ASRUtils::is_real(*ASRUtils::extract_type(type))) {
            return true;
        }
    }
    return false;
}

void CommonVisitorBase::scalarize_ieee_support_inquiry_args(ASR::symbol_t *sym,
        Vec<ASR::call_arg_t> &args, bool is_dt_present) {
    sym = ASRUtils::symbol_get_past_external(sym);
    if (sym == nullptr || !ASR::is_a<ASR::GenericProcedure_t>(*sym)) {
        return;
    }
    if (!has_real_array_actual(args)) {
        return;
    }
    ASR::GenericProcedure_t *gp = ASR::down_cast<ASR::GenericProcedure_t>(sym);
    Location loc = args.size() > 0 ? args.p[0].loc : sym->base.loc;
    auto ignore_error = [](const std::string &, const Location &) {};
    int idx = ASRUtils::select_generic_procedure(args, *gp, loc,
        ignore_error, false, is_dt_present);
    if (idx != -1) {
        ASR::symbol_t *spec = gp->m_procs[idx];
        if (!ASRUtils::is_elemental(spec)
                || !is_ieee_support_scalar_inquiry_specific(spec)) {
            return;
        }
    } else {
        bool inquiry = false;
        for (size_t i = 0; i < gp->n_procs; i++) {
            if (is_ieee_support_scalar_inquiry_specific(gp->m_procs[i])) {
                inquiry = true;
                break;
            }
        }
        if (!inquiry) {
            return;
        }
    }
    for (size_t i = 0; i < args.size(); i++) {
        ASR::expr_t *value = args.p[i].m_value;
        if (value == nullptr) {
            continue;
        }
        ASR::ttype_t *type = ASRUtils::expr_type(value);
        if (!ASRUtils::is_array(type)) {
            continue;
        }
        ASR::ttype_t *elem = ASRUtils::extract_type(type);
        if (!ASRUtils::is_real(*elem)) {
            continue;
        }
        int64_t kind = ASRUtils::extract_kind_from_ttype_t(elem);
        ASR::ttype_t *scalar_type = ASRUtils::TYPE(ASR::make_Real_t(
            al, args.p[i].loc, kind));
        // Kind 10/16 constants are not a host double. make_RealConstant_t
        // would store one; make_RealConstant_util encodes the payload.
        args.p[i].m_value = ASRUtils::make_RealConstant_util(
            al, args.p[i].loc, 0.0, scalar_type);
    }
}

const char* CommonVisitorBase::element_of_nonsequence_array(ASR::expr_t *actual) {
    if (!ASR::is_a<ASR::ArrayItem_t>(*actual)) {
        return nullptr;
    }
    ASR::expr_t *array = ASR::down_cast<ASR::ArrayItem_t>(actual)->m_v;
    ASR::ttype_t *array_type = ASRUtils::expr_type(array);
    if (ASR::is_a<ASR::String_t>(*ASRUtils::extract_type(array_type))) {
        return nullptr;
    }
    bool associate_name = false;
    if (ASR::is_a<ASR::Var_t>(*array)) {
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(array)->m_v);
        if (ASR::is_a<ASR::Variable_t>(*sym)) {
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
            associate_name = is_variable_associate_name(v);
            // Contiguous only if its selector is. An element of a
            // non-contiguous one is rejected: its address is not
            // followed by the next elements in memory.
            if (associate_name && !v->m_contiguous_attr) {
                return "element of a non-contiguous array";
            }
        }
    }
    if (!associate_name && ASRUtils::is_pointer(array_type)) {
        return "element of a pointer array";
    }
    if (ASR::is_a<ASR::Var_t>(*array)) {
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(array)->m_v);
        if (ASR::is_a<ASR::Variable_t>(*sym) &&
                ASRUtils::is_arg_dummy(ASR::down_cast<ASR::Variable_t>(sym)->m_intent) &&
                !ASRUtils::is_allocatable(array_type) &&
                ASRUtils::extract_physical_type(array_type) ==
                    ASR::array_physical_typeType::DescriptorArray) {
            return "element of an assumed-shape array";
        }
    }
    if (ASRUtils::is_class_type(ASRUtils::extract_type(array_type))) {
        return "element of a polymorphic array";
    }
    return nullptr;
}

void CommonVisitorBase::report_array_element_for_array_dummy(diag::Diagnostics &diag,
        ASR::expr_t *actual, ASR::expr_t *dummy) {
    ASR::ttype_t *dummy_type = ASRUtils::expr_type(dummy);
    std::string message = "Passing a scalar argument to an array dummy argument is not allowed";
    std::string actual_label = "scalar argument";
    std::string dummy_label = "array dummy argument";
    const char *nonsequence_element = element_of_nonsequence_array(actual);
    if (is_sequence_associable_dummy(dummy_type) && nonsequence_element != nullptr) {
        actual_label = nonsequence_element;
        message = std::string("Passing an ") + nonsequence_element +
            " to an array dummy argument is not allowed";
    } else if (is_sequence_associable_dummy(dummy_type)) {
        message += ". Use --legacy-array-sections to enable sequence association";
    } else if (ASRUtils::is_allocatable(dummy_type)) {
        dummy_label = "allocatable array dummy argument";
    } else if (ASRUtils::is_pointer(dummy_type)) {
        dummy_label = "pointer array dummy argument";
    } else {
        dummy_label = "assumed-shape array dummy argument";
    }
    diag.add(diag::Diagnostic(message, diag::Level::Error, diag::Stage::Semantic, {
            diag::Label(actual_label, {actual->base.loc}),
            diag::Label(dummy_label, {dummy->base.loc})
        }));
    throw SemanticAbort();
}

void CommonVisitorBase::validate_create_function_arguments(Vec<ASR::call_arg_t>& args, ASR::symbol_t *v){
    ASR::Function_t* func = ASRUtils::get_function(v);
    if (func == nullptr) {
        // A procedure variable without an explicit interface.
        return;
    }
    ASR::FunctionType_t* func_type = ASRUtils::get_FunctionType(v);
    // Callers diagnose more actual than formal arguments first.
    LCOMPILERS_ASSERT(args.size() <= func_type->n_arg_types);

    // Currently present function is supporting only integer arguments
    // After implementing present below if condition should be removed
    if( to_lower(func->m_name) != "present" ){
        for( size_t i = 0; i < args.size(); i++ ) {
            if( args.p[i].m_value == nullptr ) {
                continue;
            }
            ASR::expr_t* arg = args.p[i].m_value;
            ASR::ttype_t* arg_type = ASRUtils::type_get_past_allocatable(
                    ASRUtils::type_get_past_pointer(ASRUtils::expr_type(arg)));
            ASR::ttype_t* orig_arg_type = ASRUtils::type_get_past_allocatable(
                    ASRUtils::type_get_past_pointer(func_type->m_arg_types[i]));

            if( ASR::is_a<ASR::FunctionType_t>(*arg_type) ) continue;

            bool is_compile_time = true;
            size_t rhs_rank = ASRUtils::extract_n_dims_from_ttype(orig_arg_type);
            size_t lhs_rank = ASRUtils::extract_n_dims_from_ttype(arg_type);

            if( ASRUtils::is_array(arg_type) ){
                ASR::Array_t* arr_lhs = ASR::down_cast<ASR::Array_t>(arg_type);
                for(size_t i = 0; i < lhs_rank; i++){
                    if( !arr_lhs->m_dims[i].m_length || !(ASRUtils::expr_value(arr_lhs->m_dims[i].m_length)) ){
                        is_compile_time = false;
                        break;
                    }
                }
            }

            if( ASRUtils::is_array(orig_arg_type) ){
                ASR::Array_t* arr_rhs = ASR::down_cast<ASR::Array_t>(orig_arg_type);
                for(size_t i = 0; i < rhs_rank; i++){
                    if( !arr_rhs->m_dims[i].m_length || !(ASRUtils::expr_value(arr_rhs->m_dims[i].m_length)) ){
                        is_compile_time = false;
                        break;
                    }
                }
            }

            if( is_compile_time ){
                if ( ASR::is_a<ASR::Array_t>(*arg_type) && ASR::is_a<ASR::Array_t>(*orig_arg_type) ){
                    ASR::Array_t* arr_rhs = ASR::down_cast<ASR::Array_t>(orig_arg_type);
                    ASR::Array_t* arr_lhs = ASR::down_cast<ASR::Array_t>(arg_type);
                    int64_t lhs_ele = 1;
                    int64_t rhs_ele = 1;
                    bool rhs_size_known = true;
                    for (size_t i = 0; i < arr_rhs->n_dims; i++) {
                        std::int64_t rhs_dim = ASRUtils::extract_dim_value_int(arr_rhs->m_dims[i].m_length);
                        if( rhs_dim != -1 ){
                            rhs_ele *= rhs_dim;
                        } else {
                            rhs_size_known = false;
                            break;
                        }
                    }
                    if ( arr_rhs->n_dims == 0 ) {
                        rhs_size_known = false;
                    }
                    for (size_t i = 0; i < arr_lhs->n_dims; i++) {
                        std::int64_t lhs_dim = ASRUtils::extract_dim_value_int(arr_lhs->m_dims[i].m_length);
                        if( lhs_dim != -1 ){
                            lhs_ele *= lhs_dim;
                        } else {
                            lhs_ele = rhs_ele;
                            break;
                        }
                    }
                    ASR::ttype_t* rhs_element_type = ASRUtils::type_get_past_array(orig_arg_type);
                    bool dummy_is_assumed_len_character_explicit_shape =
                        ASR::is_a<ASR::String_t>(*rhs_element_type) &&
                        ASR::down_cast<ASR::String_t>(rhs_element_type)->m_len_kind ==
                            ASR::string_length_kindType::AssumedLength &&
                        ASRUtils::is_fixed_size_array(arr_rhs->m_dims, arr_rhs->n_dims);
                    if( rhs_size_known && lhs_ele < rhs_ele &&
                        !dummy_is_assumed_len_character_explicit_shape ){
                        diag.add(Diagnostic("Array passed into function has `" + std::to_string(lhs_ele) +
                            "` elements but function expects `" + std::to_string(rhs_ele) + "`.",
                            Level::Error, Stage::Semantic, {Label("", {args.p[i].loc})}));
                        throw SemanticAbort();
                    }
                }
            }

            if(ASRUtils::is_unlimited_polymorphic_to_specific_type(orig_arg_type, arg_type) ||
                (!ASRUtils::check_equal_type(arg_type,orig_arg_type, arg, func->m_args[i]) &&
                !ASRUtils::check_class_assignment_compatibility(func->m_args[i], arg))){
                // Allow scalar integer kind mismatch when implicit_argument_casting is enabled
                bool allow_mismatch = false;
                if (compiler_options.implicit_argument_casting &&
                    ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_array(arg_type)) &&
                    ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_array(orig_arg_type)) &&
                    !ASRUtils::is_array(arg_type) && !ASRUtils::is_array(orig_arg_type)) {
                    allow_mismatch = true;
                }
                if (!allow_mismatch) {
                    std::string arg_str = ASRUtils::type_to_str_with_kind(arg_type, arg);
                    std::string orig_arg_str = ASRUtils::type_to_str_with_kind(orig_arg_type, func->m_args[i]);
                    std::string arg_name = ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(func->m_args[i])->m_v);
                    diag.add(Diagnostic("Type mismatch in argument `" + arg_name +
                                        "`: expected `" + orig_arg_str + "` but got `" + arg_str + "`.",
                                        Level::Error, Stage::Semantic, {Label("", {args.p[i].loc})}));
                    throw SemanticAbort();
                }
            }
        }
    }

}

void CommonVisitorBase::legacy_array_sections_helper(ASR::symbol_t *v, Vec<ASR::call_arg_t>& args, const Location &loc) {
    bool callee_is_external_symbol = ASR::is_a<ASR::ExternalSymbol_t>(*v);
    ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(v));
    if (compiler_options.legacy_array_sections) {
        // call b(w(icon)) -> call b(w(icon:)) if b is expecting an array
        std::map<size_t, ASR::ttype_t*> array_arg_idx;
        for (size_t i = 0; i < f->n_args; i++) {
            if (ASRUtils::is_array(ASRUtils::expr_type(f->m_args[i])) &&
                !ASRUtils::is_assumed_rank_array(ASRUtils::expr_type(f->m_args[i]))) {
                array_arg_idx[i] = ASRUtils::expr_type(f->m_args[i]);
            }
        }
        Vec<ASR::call_arg_t> args_with_array_section;
        args_with_array_section.reserve(al, args.size());
        for (size_t i = 0; i < args.size(); i++) {
            // check if i is in array_arg_idx
            if (array_arg_idx.find(i) != array_arg_idx.end()) {
                ASR::call_arg_t arg = args[i];
                ASR::array_physical_typeType expected_phys = ASRUtils::extract_physical_type(array_arg_idx[i]);
                ASR::ttype_t* expected_arg_type = ASRUtils::duplicate_type(al, array_arg_idx[i], nullptr, expected_phys, true);
                ASR::expr_t* arg_expr = arg.m_value;
                // A vector-subscripted ArrayItem is an array expression, not an
                // array element designator, so it is passed unchanged.
                if (arg_expr && ASR::is_a<ASR::ArrayItem_t>(*arg_expr) &&
                        !ASRUtils::is_array(ASRUtils::expr_type(arg_expr))) {
                    ASR::ArrayItem_t* array_item = ASR::down_cast<ASR::ArrayItem_t>(arg_expr);
                    ASR::expr_t* array_expr = array_item->m_v;

                    // Only explicit-shape and assumed-size dummies are sequence
                    // associated with the elements that start at the actual element,
                    // and only when the elements that follow it in the array follow
                    // it in memory.
                    if (!is_sequence_associable_element(arg_expr, f->m_args[i])) {
                        report_array_element_for_array_dummy(diag, arg_expr, f->m_args[i]);
                    }

                    // When element types differ (e.g., complex actual vs real dummy),
                    // skip the ArrayItem→ArraySection transformation and let
                    // implicit_argument_casting in Call_t_body handle both the
                    // scalar-to-array and type conversion.
                    ASR::ttype_t* actual_elem = ASRUtils::extract_type(
                        ASRUtils::expr_type(array_expr));
                    ASR::ttype_t* expected_elem = ASRUtils::extract_type(
                        array_arg_idx[i]);
                    if (!ASRUtils::types_equal(actual_elem, expected_elem,
                            arg_expr, f->m_args[i], true)) {
                        args_with_array_section.push_back(al, args[i]);
                        continue;
                    }

                    // Legacy sequence association passes the address of the first element.
                    // For externals (implicit interface), force PointerArray to match the ABI.
                    // For known/internal procedures (including recursive self-calls), do not
                    // mutate the callee signature here; only rewrite the actual argument.
                    if (callee_is_external_symbol) {
                        expected_phys = ASR::array_physical_typeType::PointerArray;
                    }
                    // The actual argument is sequence associated with the dummy, so the
                    // dummy's bounds are never part of the argument's type. Drop them:
                    // they belong to the callee and may reference symbols (FunctionParam,
                    // getters of module variables) that the caller must not depend on.
                    expected_arg_type = ASRUtils::duplicate_type_with_empty_dims(
                        al, array_arg_idx[i], expected_phys, true);
                    ASR::ttype_t* expected_arg_type_past_ptr = ASRUtils::type_get_past_allocatable(
                        ASRUtils::type_get_past_pointer(expected_arg_type));

                    LCOMPILERS_ASSERT(array_item->n_args > 0);

                    ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(expected_arg_type_past_ptr);

                    ASR::asr_t* expected_array = ASR::make_Array_t(al, loc,
                                                    ASRUtils::type_get_past_array(expected_arg_type_past_ptr),
                                                    array_t->m_dims, array_t->n_dims,
                                                    ASRUtils::extract_physical_type(expected_arg_type_past_ptr), ASR::memory_spaceType::Global);
                    ASR::array_physical_typeType expected_phys_type = ASRUtils::extract_physical_type(expected_arg_type);
                    // Sequence association: use empty dims to skip per-dimension
                    // runtime checks. The elements that start at the actual element
                    // need not have the dummy's shape; the callee uses its own bounds.
                    ASR::ttype_t* cast_target_type = ASRUtils::duplicate_type_with_empty_dims(
                        al, ASRUtils::TYPE(expected_array), expected_phys_type, true);

                    // Pass the address of the element. This is exactly the element
                    // sequence that starts at the element, whereas an ArraySection
                    // a(i:, j:) does not describe it (and may be copied in as a
                    // non-contiguous section). It also avoids materializing a temporary
                    // ArraySection descriptor, preventing stack growth from
                    // dimension-descriptor allocas.
                    arg.m_value = ASRUtils::EXPR(ASR::make_GetPointer_t(
                        al, array_item->base.base.loc, arg_expr, cast_target_type, nullptr));
                    args_with_array_section.push_back(al, arg);
                } else if (arg_expr && ASR::is_a<ASR::ArraySection_t>(*arg_expr)) {
                    ASR::call_arg_t arg = args[i];
                    ASR::ttype_t* original_dummy_type = ASRUtils::type_get_past_allocatable(
                        ASRUtils::type_get_past_pointer(array_arg_idx[i]));
                    int expected_n_dims_cast = ASRUtils::extract_n_dims_from_ttype(original_dummy_type);
                    int actual_n_dims = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(arg_expr));

                    if (expected_n_dims_cast != actual_n_dims) {
                        ASR::array_physical_typeType actual_phys_type =
                            ASRUtils::extract_physical_type(ASRUtils::expr_type(arg_expr));
                        ASR::ttype_t* cast_target_type = ASRUtils::duplicate_type_with_empty_dims(
                            al, expected_arg_type, expected_phys, true);
                        ASR::asr_t* array_cast = ASRUtils::make_ArrayPhysicalCast_t_util(
                            al, arg_expr->base.loc, arg_expr, actual_phys_type,
                            expected_phys, cast_target_type, nullptr);
                        arg.m_value = ASRUtils::EXPR(array_cast);
                    }

                    args_with_array_section.push_back(al, arg);
                } else {
                    args_with_array_section.push_back(al, args[i]);
                }
            } else {
                args_with_array_section.push_back(al, args[i]);
            }
        }
        args = args_with_array_section;
    } else {
        // Check if a scalar (ArrayItem) is passed to an array argument
        for (size_t i = 0; i < f->n_args && i < args.size(); i++) {
            if (ASRUtils::is_array(ASRUtils::expr_type(f->m_args[i])) &&
                    !ASRUtils::is_assumed_rank_array(ASRUtils::expr_type(f->m_args[i])) &&
                    args[i].m_value &&
                    ASR::is_a<ASR::ArrayItem_t>(*args[i].m_value) &&
                    !ASRUtils::is_array(ASRUtils::expr_type(args[i].m_value))) {
                report_array_element_for_array_dummy(diag, args[i].m_value, f->m_args[i]);
            }
            // `w%u(2)` with `w` an array is one element of the component
            // taken from every element of the base, so what it denotes is
            // strided by the size of an element of the base. Passing it
            // would hand the callee the elements that follow the first one
            // in memory instead, and write them back through an
            // `intent(out)` or `intent(inout)` dummy. Reject it until the
            // argument is built by gathering the elements it names.
            if (args[i].m_value &&
                    ASR::is_a<ASR::ArrayItem_t>(*args[i].m_value) &&
                    ASRUtils::struct_base_lending_shape(
                        ASR::down_cast<ASR::ArrayItem_t>(args[i].m_value))
                        != nullptr) {
                diag.add(diag::Diagnostic(
                    "Passing an element of an array component of an array "
                    "as an argument is not supported yet",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {args[i].m_value->base.loc})
                    }));
                throw SemanticAbort();
            }
        }
    }
    check_array_actual_for_scalar_dummy(v, f, args);
}

} // namespace LCompilers::LFortran

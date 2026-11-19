// Member functions of CommonVisitorBase, in the order of their declarations
// (split over ast_common_visitor_base*.cpp to compile in parallel).

#include <lfortran/semantics/ast_common_visitor.h>

namespace LCompilers::LFortran {

void CommonVisitorBase::check_array_actual_for_scalar_dummy(ASR::symbol_t *v, ASR::Function_t *f,
        Vec<ASR::call_arg_t> &args) {
    if (ASRUtils::is_elemental(v)) {
        return;
    }
    // A procedure defined outside any module or procedure of this file.
    bool has_implicit_interface = !ASR::is_a<ASR::ExternalSymbol_t>(*v) &&
        ASRUtils::is_tu_scope(f->m_symtab->parent);
    for (size_t i = 0; i < f->n_args && i < args.size(); i++) {
        ASR::expr_t *actual = args[i].m_value;
        if (actual == nullptr || !ASRUtils::is_array(ASRUtils::expr_type(actual))) {
            continue;
        }
        ASR::symbol_t *dummy = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v);
        if (!ASR::is_a<ASR::Variable_t>(*dummy)) {
            continue;
        }
        ASR::Variable_t *dummy_var = ASR::down_cast<ASR::Variable_t>(dummy);
        if (ASRUtils::is_array(dummy_var->m_type)) {
            continue;
        }
        std::string message = "rank mismatch in argument `" +
            std::string(dummy_var->m_name) + "`: the dummy argument is a scalar "
            "but the actual argument is an array of rank " + std::to_string(
            ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(actual)));
        Label label("pass one element of the array instead", {actual->base.loc});
        if (compiler_options.legacy_array_sections && has_implicit_interface &&
                ASR::is_a<ASR::Var_t>(*actual)) {
            diag.add(Diagnostic(message + "; as a legacy extension, only its "
                "first element is passed", Level::Warning, Stage::Semantic, {label}));
            args.p[i].m_value = first_array_element(actual);
            continue;
        }
        diag.add(Diagnostic(message, Level::Error, Stage::Semantic, {label}));
        throw SemanticAbort();
    }
}

ASR::expr_t* CommonVisitorBase::first_array_element(ASR::expr_t *array) {
    const Location &loc = array->base.loc;
    ASRUtils::ASRBuilder b(al, loc);
    size_t n_dims = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(array));
    Vec<ASR::array_index_t> indices;
    indices.reserve(al, n_dims);
    for (size_t d = 1; d <= n_dims; d++) {
        ASR::array_index_t index;
        index.loc = loc;
        index.m_left = nullptr;
        index.m_right = b.ArrayLBound(array, d, compiler_options.po.default_integer_kind);
        index.m_step = nullptr;
        indices.push_back(al, index);
    }
    return ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al, loc, array,
        indices.p, indices.n, ASRUtils::expr_type(array),
        ASR::arraystorageType::ColMajor, nullptr));
}

void CommonVisitorBase::validate_missing_required_arguments(const Location &loc,
            Vec<ASR::call_arg_t>& args, ASR::Function_t* func,
            ASR::expr_t* dt, bool nopass) {
    const int offset = (dt != nullptr && !nopass) ? 1 : 0;
    for (size_t i = 0; i + offset < func->n_args; i++) {
        ASR::expr_t* dummy = func->m_args[i + offset];
        ASR::symbol_t* dummy_sym = nullptr;
        if (ASR::is_a<ASR::Var_t>(*dummy)) {
            dummy_sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(dummy)->m_v);
        }
        if (dummy_sym == nullptr) {
            continue;
        }
        std::string dummy_name;
        bool is_required = false;
        if (ASR::is_a<ASR::Variable_t>(*dummy_sym)) {
            ASR::Variable_t* dummy_var = ASR::down_cast<ASR::Variable_t>(dummy_sym);
            dummy_name = dummy_var->m_name;
            is_required = dummy_var->m_presence != ASR::presenceType::Optional;
            if (i < args.size() && args[i].m_value != nullptr) {
                ASR::ttype_t* dummy_type = dummy_var->m_type;
                ASR::ttype_t* actual_type = ASRUtils::expr_type(args[i].m_value);
                // An assumed-size actual has no extent for its last
                // dimension, so it cannot be associated with a dummy
                // that needs the full shape: a pointer/allocatable dummy
                // (covers assumed-rank pointer/allocatable, which has no
                // dimensions) or an assumed-shape dummy (empty dimensions
                // lowered to a descriptor). Empty dimensions alone are not
                // enough: assumed-size dummies and implicit-interface
                // dummies also carry empty dimensions but legally accept
                // assumed-size actuals, as do plain assumed-rank dummies.
                if (ASRUtils::is_array(actual_type) && ASRUtils::is_array(dummy_type) &&
                        ASRUtils::extract_physical_type(actual_type) ==
                            ASR::array_physical_typeType::UnboundedPointerArray &&
                        (ASRUtils::is_pointer(dummy_type) ||
                         ASRUtils::is_allocatable(dummy_type) ||
                         (ASRUtils::is_dimension_empty(dummy_type) &&
                          ASRUtils::extract_physical_type(dummy_type) ==
                              ASR::array_physical_typeType::DescriptorArray))) {
                    diag.add(diag::Diagnostic(
                        "actual argument for '" + dummy_name +
                        "' cannot be an assumed-size array",
                        diag::Level::Error, diag::Stage::Semantic, {
                            diag::Label("", {args[i].m_value->base.loc})}));
                    throw SemanticAbort();
                }
            }
        } else if (ASR::is_a<ASR::Function_t>(*dummy_sym)) {
            dummy_name = ASR::down_cast<ASR::Function_t>(dummy_sym)->m_name;
            is_required = true;
        }
        if (is_required && (i >= args.size() || args[i].m_value == nullptr)) {
            diag.add(diag::Diagnostic(
                "Missing actual argument for argument '" +
                dummy_name + "'",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {loc})}));
            throw SemanticAbort();
        }
    }
}

void CommonVisitorBase::validate_function_call_argument_count(const Location &loc,
            Vec<ASR::call_arg_t>& args, ASR::Function_t* func) {
    if (args.size() > func->n_args) {
        const Location args_loc { ASRUtils::get_vec_loc(args) };
        diag.add(diag::Diagnostic(
                "More actual than formal arguments in procedure call",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {args_loc})}));
        throw SemanticAbort();
    }
    validate_missing_required_arguments(loc, args, func);
}

ASR::asr_t* CommonVisitorBase::create_Function(const Location &loc,
            Vec<ASR::call_arg_t>& args, ASR::symbol_t *v) {
    ASR::symbol_t *f2 = ASRUtils::symbol_get_past_external(v);
    
    // Special handling for compiler_options(): replace with CompilerOptions ASR node
    if (std::string(ASRUtils::symbol_name(f2)) == "compiler_options") {
        Vec<ASR::expr_t*> expr_args;
        expr_args.reserve(al, args.size());
        for (size_t i = 0; i < args.size(); i++) {
            expr_args.push_back(al, args[i].m_value);
        }
        return ASRUtils::CompilerOptions::create_CompilerOptions(al, loc, expr_args, diag);
    }
    
    ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(f2);
    if (ASRUtils::get_FunctionType(func)->m_abi == ASR::abiType::Intrinsic) {
        std::string func_name = std::string(func->m_name);
        
        std::string intrinsic_name = func_name;
        if (intrinsic_mapping.count(func_name) > 0) {
            intrinsic_name = intrinsic_mapping[func_name].first;
        }
        
        if (ASRUtils::IntrinsicElementalFunctionRegistry::is_intrinsic_function(intrinsic_name)) {
            Vec<ASR::expr_t*> expr_args;
            expr_args.reserve(al, args.size());
            for (size_t i = 0; i < args.size(); i++) {
                expr_args.push_back(al, args[i].m_value);
            }
            ASRUtils::create_intrinsic_function create_func =
                ASRUtils::IntrinsicElementalFunctionRegistry::get_create_function(intrinsic_name);
            return create_func(al, loc, expr_args, diag);
        } else if (ASRUtils::IntrinsicArrayFunctionRegistry::is_intrinsic_function(intrinsic_name)) {
            Vec<ASR::expr_t*> expr_args;
            expr_args.reserve(al, args.size());
            for (size_t i = 0; i < args.size(); i++) {
                expr_args.push_back(al, args[i].m_value);
            }
            CommonVisitorMethods::set_intrinsic_return_kind(al, loc, intrinsic_name,
                expr_args, compiler_options.po.default_integer_kind);
            ASRUtils::create_intrinsic_function create_func =
                ASRUtils::IntrinsicArrayFunctionRegistry::get_create_function(intrinsic_name);
            return create_func(al, loc, expr_args, diag);
        }
    }
    
    validate_function_call_argument_count(loc, args, func);

    ASR::ttype_t *return_type = nullptr;
    ASR::expr_t* first_array_arg = ASRUtils::find_first_array_arg_if_elemental(func, args);
    if (first_array_arg) {
        ASR::dimension_t* array_dims;
        size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
            ASRUtils::expr_type(first_array_arg), array_dims
        );
        Vec<ASR::dimension_t> new_dims;
        new_dims.from_pointer_n_copy(al, array_dims, array_n_dims);
        return_type = ASRUtils::duplicate_type(al,
                        ASRUtils::get_FunctionType(func)->m_return_var_type,
                        &new_dims);
    } else {
        return_type = ASRUtils::EXPR2VAR(func->m_return_var)->m_type;
        return_type = handle_return_type(return_type, loc, args, func);
    }
    ASR::expr_t* value = nullptr;
    if (ASR::is_a<ASR::ExternalSymbol_t>(*v)) {
        // Populate value
        ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(f2);
        if (ASRUtils::is_intrinsic_procedure(f)) {
            value = intrinsic_procedures.comptime_eval(f->m_name, al, loc, args, compiler_options);
            char *mod = ASR::down_cast<ASR::ExternalSymbol_t>(v)->m_module_name;
            current_module_dependencies.push_back(al, mod);
        }
    }
    if (ASRUtils::symbol_parent_symtab(v)->get_counter() != current_scope->get_counter()) {
        ADD_ASR_DEPENDENCIES(current_scope, v, current_function_dependencies);
    }
    if ((_processing_dimensions || _processing_char_len) && _declaring_variable &&
        ASRUtils::symbol_parent_symtab(v)->get_counter() != current_scope->get_counter() &&
        !ASR::is_a<ASR::ExternalSymbol_t>(*v)) {
        current_function_dependencies.push_back(al, ASRUtils::symbol_name(v));
    }
    ASR::Module_t* v_module = ASRUtils::get_sym_module0(f2);
    if( v_module ) {
        current_module_dependencies.push_back(al, v_module->m_name);
    }
    ASRUtils::insert_module_dependency(v, al, current_module_dependencies);
    ASRUtils::set_absent_optional_arguments_to_null(args, func, al);
    cast_procedure_actuals(args, func);
    legacy_array_sections_helper(v, args, loc);
    validate_create_function_arguments(args, v);
    if (!func->m_deterministic) {
        current_function_deterministic = false;
    }
    if (!func->m_side_effect_free) {
        current_function_side_effect_free = false;
    }
    return ASRUtils::make_FunctionCall_t_util(al, loc, v, nullptr,
        args.p, args.size(), return_type, value, nullptr, current_scope, current_function_dependencies,
        compiler_options.implicit_argument_casting);
}

ASR::asr_t* CommonVisitorBase::create_FunctionFromFunctionTypeVariable(const Location &loc,
            Vec<ASR::call_arg_t>& args, ASR::symbol_t *v,
            const std::string &source_name, bool is_dt_present) {
    // `v` is a procedure variable, or the call-site interface of a call
    // through a procedure with an implicit interface.
    ASR::symbol_t* v_past_external = ASRUtils::symbol_get_past_external(v);
    ASR::ttype_t* proc_type = ASR::is_a<ASR::Function_t>(*v_past_external)
        ? ASR::down_cast<ASR::Function_t>(v_past_external)->m_function_signature
        : ASRUtils::import_procedure_type(al, ASRUtils::symbol_type(v),
            current_scope);
    ASR::FunctionType_t* func = ASR::down_cast<ASR::FunctionType_t>(ASRUtils::type_get_past_pointer(proc_type));
    ASR::ttype_t *return_type = func->m_return_var_type;
    auto declared_result = implicit_call_result_types.find(v);
    if (declared_result != implicit_call_result_types.end()) {
        return_type = declared_result->second;
    }
    if (return_type == nullptr) {
        diag.add(Diagnostic("procedure '" + source_name +
            "' is referenced as a function, but its interface has no result",
            Level::Error, Stage::Semantic,
            {Label("'" + source_name + "' cannot be called as a function", {loc})}));
        throw SemanticAbort();
    }
    if (ASRUtils::symbol_parent_symtab(v)->get_counter() != current_scope->get_counter()) {
        ADD_ASR_DEPENDENCIES(current_scope, v, current_function_dependencies);
    }
    // Resolve FunctionParam references in return type dimensions
    if (return_type && ASRUtils::is_array(return_type)) {
        ASR::call_arg_t* call_args;
        size_t n_call_args;
        if (is_dt_present) {
            call_args = args.p + 1;
            n_call_args = args.size() - 1;
        } else {
            call_args = args.p;
            n_call_args = args.size();
        }
        ASR::dimension_t* m_dims = nullptr;
        size_t n_dims = ASRUtils::extract_dimensions_from_ttype(return_type, m_dims);
        ASRUtils::ReplaceFunctionParamWithArg r(al, call_args, n_call_args);
        Vec<ASR::dimension_t> new_dims;
        new_dims.reserve(al, n_dims);
        for (size_t i = 0; i < n_dims; i++) {
            ASR::dimension_t dim;
            dim.loc = m_dims[i].loc;
            dim.m_start = m_dims[i].m_start ? r.replace_FunctionParam_with_arg(m_dims[i].m_start) : nullptr;
            dim.m_length = m_dims[i].m_length ? r.replace_FunctionParam_with_arg(m_dims[i].m_length) : nullptr;
            new_dims.push_back(al, dim);
        }
        ASRUtils::set_function_result_lbounds_to_one(al, new_dims.p, new_dims.size());
        return_type = ASRUtils::duplicate_type(al, return_type, &new_dims);
    }
    // TODO: Uncomment later
    // ASRUtils::set_absent_optional_arguments_to_null(args, ASR::down_cast<ASR::Function_t>(v), al);
    if( is_dt_present ) {
        ASR::expr_t* dt = ASRUtils::EXPR(ASR::make_StructInstanceMember_t(
            al, loc, args.p[0].m_value, v, proc_type, nullptr));
        check_procedure_component_of_constant(dt, "called");
        ASR::call_arg_t* call_args = args.p + 1;
        size_t n_call_args = args.size() - 1;
        ASRUtils::insert_self_arg(al, v, call_args, n_call_args, dt);
        return ASRUtils::make_FunctionCall_t_util(al, loc, v, nullptr,
            call_args, n_call_args, return_type, nullptr, dt, current_scope, current_function_dependencies,
            compiler_options.implicit_argument_casting);
    } else {
        ASR::Function_t* interface_func = ASRUtils::get_function(v);
        if (interface_func != nullptr) {
            validate_function_call_argument_count(loc, args, interface_func);
        }
        validate_create_function_arguments(args, v);
        // The call names the pointer temporary that holds the cast.
        // Record the procedure that was cast; a subroutine call already does.
        ASR::symbol_t *cast_source = nullptr;
        auto found = implicit_call_procedures.find(v);
        if (found == implicit_call_procedures.end()) {
            found = implicit_call_procedures.find(
                ASRUtils::symbol_get_past_external(v));
        }
        if (found != implicit_call_procedures.end()) {
            cast_source = found->second;
        }
        return ASRUtils::make_FunctionCall_t_util(al, loc, v, cast_source,
            args.p, args.size(), return_type, nullptr, nullptr, current_scope, current_function_dependencies,
            compiler_options.implicit_argument_casting);
    }
}

ASR::asr_t* CommonVisitorBase::create_FunctionCall(const Location &loc,
            ASR::symbol_t *fn, Vec<ASR::call_arg_t>& args) {
    ASR::symbol_t *f2 = ASRUtils::symbol_get_past_external(fn);
    if (ASR::is_a<ASR::Function_t>(*f2)) {
        return create_Function(loc, args, fn);
    } else {
        LCOMPILERS_ASSERT(ASR::is_a<ASR::GenericProcedure_t>(*f2))
        return create_GenericProcedure(loc, args, fn);
    }
}

ASR::ttype_t* CommonVisitorBase::vector_subscript_result_type(ASR::ttype_t* array_type,
        const Vec<ASR::array_index_t> &indices) {
    Vec<ASR::dimension_t> dims;
    dims.reserve(al, indices.size());
    for (size_t i = 0; i < indices.size(); i++) {
        ASR::expr_t* index = indices[i].m_right;
        if (index == nullptr || !ASRUtils::is_array(ASRUtils::expr_type(index))) {
            continue;
        }
        ASR::dimension_t* index_dims = nullptr;
        ASRUtils::extract_dimensions_from_ttype(ASRUtils::expr_type(index), index_dims);
        ASR::dimension_t dim;
        dim.loc = index->base.loc;
        dim.m_length = index_dims[0].m_length;
        dim.m_start = dim.m_length ? ASRUtils::EXPR(ASR::make_IntegerConstant_t(
            al, index->base.loc, 1, ASRUtils::TYPE(ASR::make_Integer_t(
            al, index->base.loc, compiler_options.po.default_integer_kind)))) : nullptr;
        dims.push_back(al, dim);
    }
    if (ASRUtils::is_class_type(ASRUtils::extract_type(array_type))) {
        // Constant dimensions would make it a FixedSizeArray, which an
        // array of polymorphic elements cannot be.
        return ASRUtils::duplicate_type(al, array_type, &dims,
            ASR::array_physical_typeType::DescriptorArray, true);
    }
    return ASRUtils::duplicate_type(al, array_type, &dims);
}

ASR::asr_t* CommonVisitorBase::resolve_intrinsic_type_member(const Location &loc,
        const std::string &member_name, ASR::expr_t* base,
        const std::string &base_name) {
    ASR::ttype_t* base_type = ASRUtils::extract_type(ASRUtils::expr_type(base));
    if (ASR::is_a<ASR::StructType_t>(*base_type) ||
            ASR::is_a<ASR::UnionType_t>(*base_type)) {
        return nullptr;
    }
    if (member_name == "kind") {
        ASRUtils::create_intrinsic_function create_func =
            ASRUtils::IntrinsicElementalFunctionRegistry::get_create_function(member_name);
        Vec<ASR::expr_t*> args;
        args.reserve(al, 1);
        args.push_back(al, base);
        return create_func(al, loc, args, diag);
    }
    if (ASR::is_a<ASR::Complex_t>(*base_type)) {
        if (member_name != "re" && member_name != "im") {
            diag.add(Diagnostic("Complex variable '" + base_name + "' only has %re, %im, and %kind members, not '" + member_name + "'",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }
        ASR::ttype_t* real_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc,
            ASRUtils::extract_kind_from_ttype_t(base_type)));
        if (ASRUtils::is_array(ASRUtils::expr_type(base))) {
            ASR::expr_t* desc_arr = ASRUtils::cast_to_descriptor(al, base);
            ASR::dimension_t* m_dims = nullptr;
            int n_dims = static_cast<int>(ASRUtils::extract_dimensions_from_ttype(
                ASRUtils::type_get_past_allocatable_pointer(
                    ASRUtils::expr_type(base)), m_dims));
            Vec<ASR::dimension_t> dim_vec = ASRUtils::make_complex_dimensions_bounds(al, loc, m_dims, n_dims);
            ASR::ttype_t* real_arr_type = ASRUtils::duplicate_type(al, real_type,
                &dim_vec, ASR::array_physical_typeType::DescriptorArray, true);
            if (member_name == "re") {
                return ASR::make_ComplexRe_t(al, loc, desc_arr, real_arr_type, nullptr);
            }
            return ASR::make_ComplexIm_t(al, loc, desc_arr, real_arr_type, nullptr);
        }
        if (member_name == "re") {
            return ASR::make_ComplexRe_t(al, loc, base, real_type, nullptr);
        }
        return ASR::make_ComplexIm_t(al, loc, base, real_type, nullptr);
    }
    if (ASR::is_a<ASR::String_t>(*base_type)) {
        if (member_name != "len") {
            diag.add(Diagnostic("Character variable '" + base_name + "' only has %len and %kind members, not '" + member_name + "'",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }
        return create_StringLen_from_expr(base, int32, loc);
    }
    diag.add(Diagnostic("Variable '" + base_name + "' doesn't have any member named, '" + member_name + "'.",
        Level::Error, Stage::Semantic, {Label("", {loc})}));
    throw SemanticAbort();
}

bool CommonVisitorBase::is_scope_in_module(SymbolTable *scope, const std::string &mod_name) {
    ASR::Module_t *mod = nullptr;
    if (scope->asr_owner != nullptr && ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) {
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
        mod = ASR::is_a<ASR::Module_t>(*owner) ? ASR::down_cast<ASR::Module_t>(owner)
            : ASRUtils::get_sym_module0(owner);
    }
    while (mod != nullptr) {
        if (mod_name == mod->m_name) {
            return true;
        }
        if (mod->m_parent_module == nullptr) {
            break;
        }
        if (mod_name == mod->m_parent_module) {
            return true;
        }
        ASR::symbol_t *parent = ASRUtils::symbol_get_past_external(
            scope->resolve_symbol(mod->m_parent_module));
        mod = (parent != nullptr && ASR::is_a<ASR::Module_t>(*parent))
            ? ASR::down_cast<ASR::Module_t>(parent) : nullptr;
    }
    return false;
}

ASR::symbol_t *CommonVisitorBase::declared_binding(ASR::symbol_t *b) {
    while (true) {
        ASR::symbol_t *type_sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::symbol_parent_symtab(b)->asr_owner);
        ASR::symbol_t *parent_sym = ASR::down_cast<ASR::Struct_t>(type_sym)->m_parent;
        ASR::symbol_t *inherited = nullptr;
        while (parent_sym != nullptr && inherited == nullptr) {
            ASR::Struct_t *parent = ASR::down_cast<ASR::Struct_t>(
                ASRUtils::symbol_get_past_external(parent_sym));
            inherited = parent->m_symtab->get_symbol(ASRUtils::symbol_name(b));
            parent_sym = parent->m_parent;
        }
        if (inherited == nullptr || inherited->type != b->type) {
            return b;
        }
        if (ASR::is_a<ASR::StructMethodDeclaration_t>(*b)) {
            ASR::StructMethodDeclaration_t *d
                = ASR::down_cast<ASR::StructMethodDeclaration_t>(b);
            ASR::StructMethodDeclaration_t *pd
                = ASR::down_cast<ASR::StructMethodDeclaration_t>(inherited);
            if (d->m_access != pd->m_access
                    || ASRUtils::symbol_get_past_external(d->m_proc)
                        != ASRUtils::symbol_get_past_external(pd->m_proc)) {
                return b;
            }
        } else {
            ASR::GenericProcedure_t *g = ASR::down_cast<ASR::GenericProcedure_t>(b);
            ASR::GenericProcedure_t *pg = ASR::down_cast<ASR::GenericProcedure_t>(inherited);
            if (g->m_access != pg->m_access) {
                return b;
            }
            for (size_t i = 0; i < g->n_procs; i++) {
                std::string name = ASRUtils::symbol_name(
                    ASRUtils::symbol_get_past_external(g->m_procs[i]));
                bool found = false;
                for (size_t j = 0; j < pg->n_procs && !found; j++) {
                    found = name == ASRUtils::symbol_name(
                        ASRUtils::symbol_get_past_external(pg->m_procs[j]));
                }
                if (!found) {
                    return b;
                }
            }
        }
        b = inherited;
    }
}

void CommonVisitorBase::check_binding_accessible(ASR::symbol_t *binding, const Location &loc) {
    ASR::symbol_t *b = ASRUtils::symbol_get_past_external(binding);
    if (b == nullptr) {
        return;
    }
    ASR::accessType access = ASR::accessType::Public;
    std::string what = "type-bound procedure";
    if (ASR::is_a<ASR::StructMethodDeclaration_t>(*b)) {
        access = ASR::down_cast<ASR::StructMethodDeclaration_t>(b)->m_access;
    } else if (ASR::is_a<ASR::GenericProcedure_t>(*b)
            && ASRUtils::is_type_bound_generic(b)) {
        access = ASR::down_cast<ASR::GenericProcedure_t>(b)->m_access;
        what = "generic type-bound procedure";
    }
    if (access != ASR::accessType::Private) {
        return;
    }
    b = declared_binding(b);
    ASR::Module_t *mod = ASRUtils::get_sym_module(b);
    if (mod == nullptr || is_scope_in_module(current_scope, mod->m_name)) {
        return;
    }
    ASR::symbol_t *type_sym = ASR::down_cast<ASR::symbol_t>(
        ASRUtils::symbol_parent_symtab(b)->asr_owner);
    diag.add(Diagnostic(what + " '"
        + std::string(ASRUtils::symbol_name(b)) + "' of derived type '"
        + std::string(ASRUtils::symbol_name(type_sym))
        + "' is private and cannot be accessed outside module '"
        + std::string(mod->m_name) + "'",
        Level::Error, Stage::Semantic, {Label("", {loc})}));
    if (!compiler_options.continue_compilation) throw SemanticAbort();
}

ASR::symbol_t* CommonVisitorBase::generic_specific_binding(ASR::symbol_t *specific,
        ASR::symbol_t *resolved) {
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(specific);
    ASR::symbol_t *r = resolved ? ASRUtils::symbol_get_past_external(resolved) : nullptr;
    if (r != nullptr && s != nullptr &&
            ASR::is_a<ASR::StructMethodDeclaration_t>(*r) &&
            ASR::is_a<ASR::StructMethodDeclaration_t>(*s) &&
            !ASRUtils::binding_overrides(
                ASR::down_cast<ASR::StructMethodDeclaration_t>(r),
                ASR::down_cast<ASR::StructMethodDeclaration_t>(s))) {
        return specific;
    }
    return resolved;
}

ASR::symbol_t* CommonVisitorBase::resolve_deriv_type_proc(const Location &loc, const std::string &var_name,
        const std::string dt_name, ASR::expr_t* dt_expr, ASR::ttype_t* dt_type, SymbolTable*& scope,
        ASR::symbol_t* parent) {
    ASR::symbol_t* v = nullptr;
    ASR::Struct_t* der_type = nullptr;
    if( dt_type ) {
        dt_type = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable(dt_type));
    }
    if( parent == nullptr ) {
        if ( ASR::is_a<ASR::StructType_t>(*dt_type) ) {
            der_type = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(
                ASRUtils::symbol_get_past_external(ASRUtils::get_struct_sym_from_struct_expr(dt_expr))));
        } else {
            diag.add(Diagnostic("Variable '" + dt_name + "' is not a derived type",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }
    } else {
        v = ASRUtils::symbol_get_past_external(parent);
        der_type = ASR::down_cast<ASR::Struct_t>(v);
    }
    ASR::symbol_t* member = der_type->m_symtab->get_symbol(var_name);
    if( member != nullptr ) {
        scope = der_type->m_symtab;
    } else if( der_type->m_parent != nullptr ) {
        member = resolve_deriv_type_proc(loc, var_name, "", nullptr, nullptr, scope, der_type->m_parent);
    } else {
        diag.add(Diagnostic("Variable '" + dt_name + "' doesn't have any member named, '" + var_name + "'.",
            Level::Error, Stage::Semantic, {Label("", {loc})}));
        throw SemanticAbort();
    }
    return member;
}

void CommonVisitorBase::check_intrinsic_arg_count(const Location& loc, size_t total_args,
    size_t min_args, size_t max_args, const std::string& intrinsic_name) {
    if( !(total_args <= max_args && total_args >= min_args) ) {
        if (min_args == max_args) {
            diag.add(diag::Diagnostic(
                "Incorrect number of arguments "
                "passed to the '" + intrinsic_name + "' intrinsic. "
                "It accepts exactly " + std::to_string(min_args) +
                " arguments.",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {loc})}));
                throw SemanticAbort();
        } else {
            diag.add(diag::Diagnostic(
                "Incorrect number of arguments "
                "passed to the '" + intrinsic_name + "' intrinsic. "
                "It accepts at least " + std::to_string(min_args) +
                " and at most " + std::to_string(max_args) + " arguments.",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {loc})}));
                throw SemanticAbort();
        }
    }
}

int64_t CommonVisitorBase::handle_kind(ASR::expr_t* kind) {
    if( kind == nullptr ) {
        return compiler_options.po.default_integer_kind;
    }

    ASR::expr_t* kind_value = ASRUtils::expr_value(kind);
    if( kind_value == nullptr ) {
        diag.add(diag::Diagnostic(
            "Only Integer literals or expressions "
            "which reduce to constant Integer are "
            "accepted as kind parameters.",
            diag::Level::Error, diag::Stage::Semantic, {
                diag::Label("", {kind->base.loc})}));
            throw SemanticAbort();
    }
    return ASR::down_cast<ASR::IntegerConstant_t>(kind_value)->m_n;
}

ASR::asr_t* CommonVisitorBase::make_assumed_rank_bounds(const Location& loc, ASR::expr_t* v,
        ASR::ttype_t* type, ASR::arrayboundType bound) {
    // `rank` and a dimension of an array descriptor are integer(4).
    ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    ASR::expr_t* dim_var = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
        declare_implicit_variable2(loc,
            current_scope->get_unique_name("lfortran_implied_do_dim"),
            ASRUtils::intent_local, int_type)));
    ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
        1, int_type));
    Vec<ASR::expr_t*> values;
    values.reserve(al, 1);
    values.push_back(al, ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc, v,
        dim_var, type, bound, nullptr)));
    Vec<ASR::expr_t*> args;
    args.reserve(al, 1);
    args.push_back(al, ASRUtils::EXPR(ASR::make_ImpliedDoLoop_t(al, loc,
        values.p, values.size(), dim_var, one,
        ASRUtils::EXPR(ASR::make_ArrayRank_t(al, loc, v, int_type, nullptr)),
        nullptr, type, nullptr)));
    Vec<ASR::dimension_t> dims;
    dims.reserve(al, 1);
    ASR::dimension_t dim;
    dim.loc = loc;
    dim.m_start = one;
    dim.m_length = nullptr;
    dims.push_back(al, dim);
    ASR::ttype_t* result_type = ASRUtils::duplicate_type(al, type, &dims,
        ASR::array_physical_typeType::DescriptorArray, true);
    return ASRUtils::make_ArrayConstructor_t_util(al, loc, args.p,
        args.size(), result_type, ASR::arraystorageType::ColMajor);
}

bool CommonVisitorBase::is_operation_on_variable(ASR::expr_t* e) {
    if (ASRUtils::expr_value(e) != nullptr) {
        return false;
    }
    if (!ASRUtils::is_binop_expr(e) && !ASRUtils::is_unaryop_expr(e)
            && !ASR::is_a<ASR::IntrinsicElementalFunction_t>(*e)) {
        return false;
    }
    return ASRUtils::reads_variable_value(e);
}

ASR::asr_t* CommonVisitorBase::create_StringLen_from_expr(ASR::expr_t* v, ASR::ttype_t* type, const Location& loc) {
    // Optimize len(trim(s)) -> len_trim(s) to avoid return-slot ordering
    // issues when the expression appears as an array dimension
    if (ASR::is_a<ASR::IntrinsicElementalFunction_t>(*v)) {
        auto* ief = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(v);
        if (ief->m_intrinsic_id == static_cast<int64_t>(
                ASRUtils::IntrinsicElementalFunctions::StringTrim)) {
            ASR::expr_t* m_value = nullptr;
            if (ief->m_value) {
                ASR::ttype_t* val_type = ASRUtils::type_get_past_allocatable_pointer(
                    ASRUtils::expr_type(ief->m_value));
                if (ASR::is_a<ASR::String_t>(*val_type)) {
                    int64_t len_val;
                    if (ASRUtils::extract_value(
                            ASR::down_cast<ASR::String_t>(val_type)->m_len, len_val)) {
                        m_value = make_ConstantWithType(
                            make_IntegerConstant_t, len_val, type, loc);
                    }
                }
            }
            Vec<ASR::expr_t*> args;
            args.reserve(al, 1);
            args.push_back(al, ief->m_args[0]);
            return ASR::make_IntrinsicElementalFunction_t(al, loc,
                static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::StringLenTrim),
                args.p, args.size(), 0, type, m_value);
        }
    }

    ASR::expr_t* len_compiletime = nullptr;
    ASR::ttype_t* v_type = ASRUtils::expr_type(v);
    if( !ASRUtils::is_character(*v_type) ) {
        if (ASRUtils::is_unlimited_polymorphic_type(v_type)) {
            diag.add(Diagnostic(
                "Character intrinsic 'len' with class(*) argument must be inside " 
                "'type is (character(len=*))' guard", Level::Error, Stage::Semantic, {
                    Label("invalid use of character intrinsic with polymorphic argument",
                    {v->base.loc})
                }));
            throw SemanticAbort();
        } else {
            diag.add(Diagnostic(
                "Argument of 'len' intrinsic must be CHARACTER, found " + 
                ASRUtils::type_to_str_fortran_expr(v_type, v) + " instead.", 
                Level::Error, Stage::Semantic, {Label("", {v->base.loc})}));
            throw SemanticAbort();
        }
    }
    if(ASR::is_a<ASR::ArrayItem_t>(*v)) {
        ASR::ArrayItem_t* arr_item = ASR::down_cast<ASR::ArrayItem_t>(v);
            if (ASR::is_a<ASR::Var_t>(*arr_item->m_v)) {
            // Simple case: plain array variable e.g. arr(i)
            ASR::Var_t* arr_var = ASR::down_cast<ASR::Var_t>(arr_item->m_v);
            ASR::symbol_t* arr_sym = arr_var->m_v;
            ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(ASRUtils::type_get_past_allocatable_pointer(ASRUtils::symbol_type(arr_sym)));
            ASR::String_t* str = ASR::down_cast<ASR::String_t>(arr->m_type);
            int length;
            len_compiletime = ASRUtils::extract_value(str->m_len, length) ? make_ConstantWithType(make_IntegerConstant_t, length, type, loc) : nullptr;
        } else {
            // Complex case: e.g. this%geometries(j) via CLASS/TYPE —
            // expr_type of m_v carries the declared character length directly
            ASR::ttype_t* mv_type = ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(arr_item->m_v));
            mv_type = ASRUtils::type_get_past_array(mv_type);
            if (ASR::is_a<ASR::String_t>(*mv_type)) {
                ASR::String_t* str = ASR::down_cast<ASR::String_t>(mv_type);
                int64_t length;
                len_compiletime = ASRUtils::extract_value(str->m_len, length)
                    ? make_ConstantWithType(make_IntegerConstant_t, length, type, loc)
                    : nullptr;
            }
        }
    }

    // LEN of a character array is the length of its element type, which
    // is defined even when the array has zero size. Keep the array as the
    // StringLen argument rather than indexing its first element, which
    // would be an out-of-bounds access for a zero-size array.
    { // Try to get expression's string length (if exist)
        ASR::ttype_t* base_t = ASRUtils::type_get_past_allocatable_pointer( v_type );
        base_t = ASRUtils::type_get_past_array(base_t);
        if (ASR::is_a<ASR::String_t>(*base_t)) {
            ASR::String_t* string_t = ASR::down_cast<ASR::String_t>(base_t);
            int64_t len = -1;
            if( ASRUtils::extract_value(string_t->m_len, len) ) {
                len_compiletime = make_ConstantWithType(make_IntegerConstant_t, len, type, loc);
            } else {
                len_compiletime = nullptr;
            }
        }
    }
    return ASR::make_StringLen_t(al, loc, v, type, len_compiletime);
}

bool CommonVisitorBase::reject_non_fortran_intrinsic_attribute(const std::string &name,
        const Location &loc) {
    if (!compiler_options.implicit_interface ||
            !is_non_fortran_registry_intrinsic(name)) {
        return false;
    }
    diag.add(Diagnostic("'" + name + "' is not an intrinsic procedure",
        Level::Error, Stage::Semantic, {
            Label("", {loc})
        }));
    if (!compiler_options.continue_compilation) {
        throw SemanticAbort();
    }
    return true;
}

bool CommonVisitorBase::is_intrinsic_registry_function(std::string var_name) {
    if (compiler_options.implicit_interface &&
            is_non_fortran_registry_intrinsic(var_name)) {
        // A reference to such a name is a reference to an external
        // procedure with an implicit interface, as for any other name
        // that is not an intrinsic procedure.
        return false;
    }
    bool is_specific_type_intrinsic = intrinsic_mapping.count(var_name);
    if (intrinsic_procedures_as_asr_nodes.is_intrinsic_present_in_ASR(var_name) ||
        intrinsic_procedures_as_asr_nodes.is_kind_based_selection_required(var_name) ||
        ASRUtils::IntrinsicElementalFunctionRegistry::is_intrinsic_function(var_name) ||
        ASRUtils::IntrinsicArrayFunctionRegistry::is_intrinsic_function(var_name) ||
        ASRUtils::IntrinsicImpureFunctionRegistry::is_intrinsic_function(var_name) ||
        is_specific_type_intrinsic) {
        return true;
    }
    return false;
}

void CommonVisitorBase::fill_optional_kind_arg(std::string &name, Vec<ASR::expr_t*> &args) {
    if (name == "aimag") {
        if (args.size() == 1) {
            Location &loc = args[0]->base.loc;
            int kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(args[0]));
            ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, kind));
            ASRUtils::ASRBuilder b(al, loc);
            args.push_back(al, b.i_t(kind, type));
        }
    }
}

void CommonVisitorBase::scalar_kind_arg(const std::string &name,
        const std::string &referenced_name, Vec<ASR::expr_t*> &args) {
    std::unordered_map<std::string, int> kind_arg_index_map = {
        {"logical", 1}, {"storage_size", 1}, {"anint", 1}, {"nint", 1}, {"aint", 1},
        {"floor", 1}, {"ceiling", 1}, {"aimag", 1}, {"maskl", 1}, {"maskr", 1},
        {"ichar", 1}, {"char", 1}, {"achar", 1}, {"iachar", 1}, {"real", 1},
        {"int", 1}, {"len_trim", 1}, {"len", 1}, {"shape", 1}, {"coshape", 1},
        {"ieee_real", 1}, {"ieee_int", 2}, {"lbound", 2}, {"ubound", 2}, {"size", 2},
        {"verify", 3}, {"index", 3}, {"scan", 3}, {"cmplx", 2}
    };

    auto it = kind_arg_index_map.find(name);
    if (it != kind_arg_index_map.end()) {
        int kind_arg_index = it->second;

        LCOMPILERS_ASSERT(kind_arg_index < (int)args.size());
        if (args[kind_arg_index]) {
            if (ASRUtils::is_array(ASRUtils::expr_type(args[kind_arg_index]))) {
                diag.add(Diagnostic(
                    "`kind` argument of `" + referenced_name + "` intrinsic must be a scalar",
                    Level::Error, Stage::Semantic, {
                        Label("", {args[kind_arg_index]->base.loc})
                    }));
                throw SemanticAbort();
            }
        }
    }
}

void CommonVisitorBase::fill_optional_args(std::string intrinsic_name, Vec<ASR::expr_t*> &args, const Location &loc) {
    ASR::ttype_t *int_type = ASRUtils::TYPE(
                ASR::make_Integer_t(al, loc, 4));
    ASR::ttype_t *bool_type = ASRUtils::TYPE(
                ASR::make_Logical_t(al, loc, 4));
    if (intrinsic_name == "selected_real_kind") {
        if (args[0] == nullptr && args[1] == nullptr && args[2] == nullptr) {
            diag.semantic_error_label(
                "selected_real_kind requires at least one of p, r or radix",
                {loc}, "");
            throw SemanticAbort();
        }
        ASR::expr_t* zero = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 0,
                                            int_type));
        ASR::expr_t* two = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 2,
                                            int_type));
        if (args[0] == nullptr) {
            args.p[0] = zero;
        }
        if (args[1] == nullptr) {
            args.p[1] = zero;
        }
        if (args[2] == nullptr) {
            args.p[2] = two;
        }
    } else if (intrinsic_name == "verify" || intrinsic_name == "index" || intrinsic_name == "scan") {
        ASR::ttype_t *bool_type = ASRUtils::TYPE(
                ASR::make_Logical_t(al, loc, 4));
        ASR::expr_t* f = ASRUtils::EXPR(
            ASR::make_LogicalConstant_t(al, loc, false, bool_type));
        ASR::expr_t* four = ASRUtils::EXPR(
            ASR::make_IntegerConstant_t(al, loc, 4, int_type));
        if (args[2] == nullptr) {
            args.p[2] = f;
        }
        if (args[3] == nullptr) {
            args.p[3] = four;
        }
    } else if (intrinsic_name == "cmplx") {
        if (args[1] == nullptr && !is_complex(*ASRUtils::expr_type(args[0]))) {
            ASR::ttype_t *real_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 4));
            ASR::expr_t* zero = ASRUtils::EXPR(
                ASR::make_RealConstant_t(al, loc, 0.0, real_type));
            args.p[1] = zero;
        }
        if (args[2] == nullptr) {
            int kind = 4;
            ASR::expr_t* val = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, kind, int_type));
            args.p[2] = val;
        }
    } else if (intrinsic_name == "real") {
        if (args[1] == nullptr) {
            if (is_complex(*ASRUtils::expr_type(args[0]))) {
                int kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(args[0]));
                ASR::expr_t* val = ASRUtils::EXPR(
                    ASR::make_IntegerConstant_t(al, loc, kind, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, kind))));
                args.p[1] = val;
            }
        }
    } else if (intrinsic_name == "ishftc"){
        if(args[2] == nullptr){
            int value;
            int kind = ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(args[0]));
            value = kind*8;
            ASR::expr_t* val = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, value, int_type));
            args.p[2] = val;
        }
    } else if (intrinsic_name == "out_of_range"){
        if(args[2] == nullptr){
            bool value = false;
            ASR::expr_t* val = ASRUtils::EXPR(
                ASR::make_LogicalConstant_t(al, loc, value, bool_type));
            args.p[2] = val;
        }
    }
}

void CommonVisitorBase::check_specific_type_intrinsics(std::string intrinsic_name, Vec<ASR::expr_t*> &args, const Location &loc) {
    std::set<std::string>array_intrinsic_mapping_names = {"min0", "amin0", "min1", "amin1", "dmin1", "max0", "amax0", "max1", "amax1", "dmax1"};
    if (intrinsic_mapping.find(intrinsic_name) == intrinsic_mapping.end()) {
        return;
    }

    size_t arg_size = args.size();
    if(intrinsic_name == "dint" || intrinsic_name == "dnint") {
        arg_size = 1;
        if (args[1]) {
            diag.add(Diagnostic("Too many arguments to call `" + intrinsic_name + "`",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }
    }

    for (size_t i = 0; i < arg_size; i++) {
        std::string argument_type = "";
        if (array_intrinsic_mapping_names.find(intrinsic_name) != array_intrinsic_mapping_names.end()) {
            argument_type = intrinsic_mapping[intrinsic_name].second[0];
        } else {
            if(i < intrinsic_mapping[intrinsic_name].second.size()){
                argument_type = intrinsic_mapping[intrinsic_name].second[i];
            } else {
                diag.add(Diagnostic("Too many arguments to call `" + intrinsic_name + "`",
                    Level::Error, Stage::Semantic, {Label("", {loc})}));
                throw SemanticAbort();
            }
        }
        if (argument_type == "int4") {
            if (args[i] != nullptr) {
                ASR::ttype_t *arg_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(args[i]));
                if (!is_integer(*arg_type)) {
                    diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                        " must be of integer type",
                                        Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                }
            }
        } else if (argument_type == "real") {
            if (args[i] != nullptr) {
                ASR::ttype_t *arg_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(args[i]));
                if (!is_real(*arg_type)) {
                    diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                        " must be of real type",
                                        Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                }
            }
        } else if (argument_type == "real4") {
            if (args[i] != nullptr) {
                ASR::ttype_t *arg_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(args[i]));
                int kind = ASRUtils::extract_kind_from_ttype_t(arg_type);
                if (!is_real(*arg_type) || kind != 4) {
                    diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                        " must be of single precision real type",
                                        Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                }
            }
        } else if (argument_type == "real8") {
            if (args[i] != nullptr) {
                ASR::ttype_t *arg_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(args[i]));
                int kind = ASRUtils::extract_kind_from_ttype_t(arg_type);
                if (!is_real(*arg_type) || kind != 8) {
                    diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                        " must be of double precision real type",
                                        Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                }
            }
        } else if (argument_type == "complex") {
            if (args[i] != nullptr) {
                ASR::ttype_t *arg_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(args[i]));
                if (!is_complex(*arg_type)) {
                    diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                        " must be of complex type",
                                        Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                }
            }
        } else if (argument_type == "complex4") {
            if (args[i] != nullptr) {
                ASR::ttype_t *arg_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(args[i]));
                int kind = ASRUtils::extract_kind_from_ttype_t(arg_type);
                if (!is_complex(*arg_type) || kind != 4) {
                    diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                        " must be of single precision complex type",
                                        Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                }
            }
        } else if (argument_type == "complex8") {
            if (args[i] != nullptr) {
                ASR::ttype_t *arg_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(args[i]));
                int kind = ASRUtils::extract_kind_from_ttype_t(arg_type);
                if (!is_complex(*arg_type) || kind != 8) {
                    diag.add(Diagnostic("Argument " + std::to_string(i + 1) + " of " + intrinsic_name +
                                        " must be of double precision complex type",
                                        Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                }
            }
        }
    }
}

void CommonVisitorBase::is_coarray_or_atomic(std::string intrinsic_name, const Location& loc){
    std::vector<std::string> coarray_intrinsics, atomic_intrinsics;
    coarray_intrinsics = {"co_reduce", "lcobound", "ucobound", "failed_images",
        "image_status", "get_team", "image_index", "stopped_images", "team_number", "coshape", "corank",
        "event_query"};
    atomic_intrinsics = {"atomic_add", "atomic_and", "atomic_cas", "atomic_define", "atomic_fetch_add", "atomic_fetch_and",
        "atomic_fetch_or", "atomic_fetch_xor", "atomic_or", "atomic_ref", "atomic_xor"};
    if (std::find(coarray_intrinsics.begin(), coarray_intrinsics.end(), intrinsic_name) != coarray_intrinsics.end()) {
        diag.add(diag::Diagnostic(
                "Coarrays are not supported yet",
                diag::Level::Error, diag::Stage::Semantic, {
                diag::Label("", {loc})}));
        throw SemanticAbort();
    } else if (std::find(atomic_intrinsics.begin(), atomic_intrinsics.end(), intrinsic_name) != atomic_intrinsics.end()) {
        diag.add(diag::Diagnostic(
            "Atomic operations are not supported yet",
            diag::Level::Error, diag::Stage::Semantic, {
                diag::Label("", {loc})}));
            throw SemanticAbort();
    }
}

ASR::expr_t* CommonVisitorBase::compiletime_broadcast_elemental_intrinsic(
    Vec<ASR::expr_t*> args,
    ASR::ArrayConstant_t** result_array,
    std::vector<size_t> array_indices_in_args,
    ASRUtils::create_intrinsic_function create_func,
    const Location& loc,
    Allocator& al)
{
    ASR::expr_t* first_arg_ = ASRUtils::expr_value(args[array_indices_in_args[0]]);
    size_t max_array_size = ASRUtils::get_fixed_size_of_array(ASRUtils::expr_type(first_arg_));
    if (max_array_size == 0) {
        Vec<ASR::expr_t*> dummy_args;
        dummy_args.reserve(al, args.size());
        bool can_create_dummy = true;
        for (size_t j = 0; j < args.size(); j++) {
            if (std::find(array_indices_in_args.begin(), array_indices_in_args.end(), j) != array_indices_in_args.end()) {
                ASR::expr_t* arg_ = ASRUtils::expr_value(args[j]);
                ASR::ttype_t* s_type = ASRUtils::type_get_past_array(ASRUtils::expr_type(arg_));
                ASR::expr_t* dummy_val = ASRUtils::get_constant_zero_with_given_type(al, s_type);
                if (!dummy_val) {
                    can_create_dummy = false;
                    break;
                }
                dummy_args.push_back(al, dummy_val);
            } else {
                dummy_args.push_back(al, args[j]);
            }
        }
        if (can_create_dummy) {
            ASR::asr_t* dummy_res = create_func(al, loc, dummy_args, diag);      
            if (dummy_res) {
                ASR::expr_t* res_expr = ASRUtils::expr_value(ASRUtils::EXPR(dummy_res));
                ASR::ttype_t* res_type = ASRUtils::expr_type(res_expr);
                ASR::Array_t* orig_arr_type = ASR::down_cast<ASR::Array_t>((*result_array)->m_type);
                ASR::ttype_t* correct_type = ASRUtils::make_Array_t_util(al, orig_arr_type->base.base.loc,
                                            res_type, orig_arr_type->m_dims,     
                                            orig_arr_type->n_dims, ASR::abiType::Source, false,
                                            orig_arr_type->m_physical_type);     
                (*result_array)->m_type = correct_type;
            }
        }
        return (ASR::expr_t*)(*result_array);
    }
    ASR::ttype_t* array_type = ASRUtils::expr_type(first_arg_);
    Vec<ASR::expr_t*> new_expr; new_expr.reserve(al, max_array_size);

    for (size_t i = 0; i < max_array_size; i++) {
        Vec<ASR::expr_t*> intrinsic_args;
        intrinsic_args.reserve(al, args.size());

        for (size_t j = 0; j < args.size(); j++) {
            if (std::find(array_indices_in_args.begin(), array_indices_in_args.end(), j) != array_indices_in_args.end()) {
                ASR::expr_t* arg_ = ASRUtils::expr_value(args[j]);
                ASR::ArrayConstant_t* array_arg = ASR::down_cast<ASR::ArrayConstant_t>(arg_);
                if (max_array_size != (size_t) ASRUtils::get_fixed_size_of_array(array_arg->m_type)) {
                    diag.add(diag::Diagnostic("Different shape of arguments for broadcasting " +
                        std::to_string(max_array_size) + " and " + std::to_string(ASRUtils::get_fixed_size_of_array(array_arg->m_type)),
                        diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {loc})}));
                    throw SemanticAbort();
                }
                ASR::expr_t* elem = ASRUtils::fetch_ArrayConstant_value(al, array_arg, i);
                intrinsic_args.push_back(al, elem);
            } else {
                intrinsic_args.push_back(al, args[j]);
            }
        }
        // Call the intrinsic function for the current combination of arguments
        ASR::asr_t* element_asr = create_func(al, loc, intrinsic_args, diag);
        if (element_asr == nullptr) {
            throw SemanticAbort();
        }
        ASR::expr_t* result = ASRUtils::expr_value(ASRUtils::EXPR(element_asr));
        if (result == nullptr) {
            // The intrinsic cannot be folded for this element, e.g. for
            // a real(16) or complex(16) value: leave it to run time
            return nullptr;
        }
        array_type = ASRUtils::expr_type(result);
        new_expr.push_back(al, result);
        // ASRUtils::set_ArrayConstant_value(result_array, result, i);
    }
    ASR::Array_t* result_arr_type = ASR::down_cast<ASR::Array_t>((*result_array)->m_type);
    ASR::ttype_t* correct_type = ASRUtils::make_Array_t_util(al, result_arr_type->base.base.loc,
                                array_type, result_arr_type->m_dims,
                                result_arr_type->n_dims, ASR::abiType::Source, false,
                                result_arr_type->m_physical_type);
    (*result_array)->m_type = correct_type;
    ASR::expr_t* new_expr_ = ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(al, (*result_array)->base.base.loc,
                                new_expr.p, new_expr.n, (*result_array)->m_type, (*result_array)->m_storage_format));
    // ArrayConstant when the elements fold to one; otherwise the
    // constructor itself.
    ASR::expr_t* folded = new_expr_;
    if (!ASR::is_a<ASR::ArrayConstant_t>(*folded)) {
        ASR::expr_t* value = ASRUtils::expr_value(folded);
        if (value && ASR::is_a<ASR::ArrayConstant_t>(*value)) {
            folded = value;
        }
    }
    if (ASR::is_a<ASR::ArrayConstant_t>(*folded)) {
        ASR::down_cast<ASR::ArrayConstant_t>(folded)->m_type = correct_type;
    } else if (ASR::is_a<ASR::ArrayConstructor_t>(*folded)) {
        ASR::down_cast<ASR::ArrayConstructor_t>(folded)->m_type = correct_type;
    }
    return folded;
}

std::vector<size_t> CommonVisitorBase::find_array_indices_in_args(const Vec<ASR::expr_t*>& args) {
    std::vector<size_t> array_indices_in_args;

    for (size_t i = 0; i < args.size(); i++) {
        if (!args[i]) {
            continue;
        }
        ASR::expr_t* arg = ASRUtils::expr_value(args[i]);
        if (arg && ASR::is_a<ASR::ArrayConstant_t>(*arg)) {
            array_indices_in_args.push_back(i);
        }
    }
    return array_indices_in_args;
}

bool CommonVisitorBase::struct_has_descriptor_string_member(ASR::expr_t* expr) {
    ASR::ttype_t* type = ASRUtils::expr_type(expr);
    type = ASRUtils::type_get_past_array(
               ASRUtils::type_get_past_allocatable(
                   ASRUtils::type_get_past_pointer(type)));
    if (!ASR::is_a<ASR::StructType_t>(*type)) return false;
    ASR::symbol_t* struct_sym = ASRUtils::symbol_get_past_external(
        ASRUtils::get_struct_sym_from_struct_expr(expr));
    if (struct_sym && ASR::is_a<ASR::Struct_t>(*struct_sym)) {
        ASR::Struct_t* st_sym = ASR::down_cast<ASR::Struct_t>(struct_sym);
        // bind(C) and SEQUENCE types have a defined storage layout,
        // so c_loc is allowed without warning.
        if (st_sym->m_abi == ASR::abiType::BindC) return false;
        if (st_sym->m_is_sequence) return false;
    }
    ASR::StructType_t* st = ASR::down_cast<ASR::StructType_t>(type);
    for (size_t i = 0; i < st->n_data_member_types; i++) {
        ASR::ttype_t* mt = ASRUtils::type_get_past_array(
                               ASRUtils::type_get_past_allocatable(
                                   ASRUtils::type_get_past_pointer(
                                       st->m_data_member_types[i])));
        if (ASR::is_a<ASR::String_t>(*mt)) return true;
    }
    return false;
}

ASR::asr_t* CommonVisitorBase::handle_intrinsic_float_dfloat(Allocator &al, Vec<ASR::call_arg_t> args,
                                    const Location &loc, int kind) {
    ASR::expr_t *arg = nullptr, *value = nullptr;
    ASR::ttype_t *type = nullptr;
    if (args.size() > 0) {
        arg = args[0].m_value;
        type = ASRUtils::expr_type(arg);
    }
    ASR::ttype_t *to_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, kind));
    if (!arg) {
        return ASR::make_RealConstant_t(al, loc, 0.0, to_type);
    }
    if (ASR::is_a<ASR::Array_t>(*type)) {
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
        to_type = ASRUtils::make_Array_t_util(al, loc, to_type,
            arr->m_dims, arr->n_dims);
    }
    if (ASRUtils::is_integer(*type)) {
        if (ASRUtils::expr_value(arg) != nullptr &&
                ASR::is_a<ASR::IntegerConstant_t>(
                    *ASRUtils::expr_value(arg))) {
            double dval = static_cast<double>(ASR::down_cast<ASR::IntegerConstant_t>(
                                    ASRUtils::expr_value(arg))->m_n);
            value =  ASR::down_cast<ASR::expr_t>(make_RealConstant_t(al,
                            loc, dval, to_type));
        }
        return (ASR::asr_t *)ASR::down_cast<ASR::expr_t>(ASR::make_Cast_t(
            al, loc, arg, ASR::cast_kindType::IntegerToReal,
            to_type, value, nullptr));
    } else {
        diag.add(Diagnostic("Argument of intrinsic must be an integer",
            Level::Error, Stage::Semantic, {Label("", {loc})}));
        throw SemanticAbort();
    }
    // TODO: Make this work if the argument is, let's say, a class.
    return nullptr;
}

ASR::asr_t* CommonVisitorBase::handle_intrinsic_dble(Allocator &al, Vec<ASR::call_arg_t> args,
                                    const Location &loc) {
    ASR::expr_t *arg = nullptr, *value = nullptr;
    ASR::ttype_t *type = nullptr;
    if (args.size() > 0) {
        arg = args[0].m_value;
        type = ASRUtils::expr_type(arg);
    }
    ASR::ttype_t *to_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 8));
    if (!arg) {
        return ASR::make_RealConstant_t(al, loc, 0.0, to_type);
    }
    // `dble` accepts a BOZ literal constant: its bits are the internal
    // representation of the real(8) result, no numeric conversion happens
    if (is_boz_constant(arg)) {
        return (ASR::asr_t *)boz_literal_to_real(arg, 8);
    }
    if (ASR::is_a<ASR::Array_t>(*type)) {
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
        to_type = ASRUtils::make_Array_t_util(al, loc, to_type,
            arr->m_dims, arr->n_dims);
    }
    if (ASRUtils::is_integer(*type)) {
        if (ASRUtils::expr_value(arg) != nullptr &&
                ASR::is_a<ASR::IntegerConstant_t>(
                    *ASRUtils::expr_value(arg))) {
            double dval = static_cast<double>(ASR::down_cast<ASR::IntegerConstant_t>(
                                    ASRUtils::expr_value(arg))->m_n);
            value =  ASR::down_cast<ASR::expr_t>(make_RealConstant_t(al,
                            loc, dval, to_type));
        }
        return (ASR::asr_t *)ASR::down_cast<ASR::expr_t>(ASR::make_Cast_t(
            al, loc, arg, ASR::cast_kindType::IntegerToReal,
            to_type, value, nullptr));
    } else if (ASRUtils::is_logical(*type)) {
        if (ASRUtils::expr_value(arg) != nullptr &&
                ASR::is_a<ASR::LogicalConstant_t>(
                    *ASRUtils::expr_value(arg))) {
            double dval = ASR::down_cast<ASR::LogicalConstant_t>(
                                    ASRUtils::expr_value(arg))->m_value;
            value =  ASR::down_cast<ASR::expr_t>(make_RealConstant_t(al,
                            loc, dval, to_type));
        }
        return (ASR::asr_t *)ASR::down_cast<ASR::expr_t>(ASR::make_Cast_t(
            al, loc, arg, ASR::cast_kindType::LogicalToReal,
            to_type, value, nullptr));
    } else if (ASRUtils::is_real(*type)) {
        // float() always returns 64-bit floating point numbers.
        if (ASRUtils::extract_kind_from_ttype_t(type) != 8) {
            return (ASR::asr_t *)ASR::down_cast<ASR::expr_t>(ASR::make_Cast_t(
                al, loc, arg, ASR::cast_kindType::RealToReal,
                to_type, value, nullptr));
        }
        return (ASR::asr_t *)arg;
    } else if (ASRUtils::is_complex(*type)) {
        return (ASR::asr_t *)ASR::down_cast<ASR::expr_t>(ASR::make_Cast_t(
                al, loc, arg, ASR::cast_kindType::ComplexToReal,
                to_type, value, nullptr));
    } else {
        std::string stype = ASRUtils::type_to_str_fortran_expr(type, arg);
        diag.add(Diagnostic("Conversion of '" + stype + "' to float is not Implemented",
                            Level::Error, Stage::Semantic, {Label("", {loc})}));
        throw SemanticAbort();
    }
    // TODO: Make this work if the argument is, let's say, a class.
    return nullptr;
}

ASR::symbol_t* CommonVisitorBase::make_fpcast_call_target(
    const Location& loc,
    ASR::symbol_t* source,
    ASR::symbol_t* to_iface)
{
    LCOMPILERS_ASSERT(current_body != nullptr);
    LCOMPILERS_ASSERT(current_body_scope != nullptr);
    ASR::symbol_t* to_sym = ASRUtils::symbol_get_past_external(to_iface);
    LCOMPILERS_ASSERT(ASR::is_a<ASR::Function_t>(*to_sym));
    ASR::Function_t* to_fn = ASR::down_cast<ASR::Function_t>(to_sym);
    // The cast's type is FunctionType (same as Var of a Function), not
    // Pointer(FunctionType): Associate of a pointer-typed value would load
    // through it. The temp procedure-pointer variable is Pointer(...).
    ASR::ttype_t* fn_type = ASRUtils::duplicate_type(al, to_fn->m_function_signature);

    SymbolTable* tmp_scope = current_body_scope;
    Vec<ASR::stmt_t*>* association_body = current_body;
    if (statement_function_host_body != nullptr &&
            is_statement_function_host_dummy(source)) {
        // A dummy procedure of the host is a value of the host, which a
        // statement function can only reach through a variable of the
        // host. The host associates that temporary where the statement
        // function is defined; a dummy does not change while the host
        // runs.
        tmp_scope = statement_function_host_body_scope;
        association_body = statement_function_host_body;
    }
    ASR::symbol_t*& tmp_var = fpcast_call_targets[tmp_scope][{source, to_sym}];
    if (tmp_var == nullptr) {
        ASR::ttype_t* ptr_type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
            ASRUtils::duplicate_type(al, to_fn->m_function_signature)));
        std::string tmp_name = tmp_scope->get_unique_name(generated_symbol_name(
            ASRUtils::symbol_name(source), "fpcast_ptr"), false);
        tmp_var = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(
                al, loc, tmp_scope, s2c(al, tmp_name), nullptr, 0,
                ASR::intentType::Local, nullptr, nullptr,
                ASR::storage_typeType::Default, ptr_type, to_sym,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::presenceType::Required, false));
        tmp_scope->add_symbol(tmp_name, tmp_var);
        implicit_call_procedures[tmp_var] = source;
    }

    ASR::expr_t* src = ASRUtils::EXPR(ASR::make_Var_t(al, loc, source));
    ASR::expr_t* cast = ASRUtils::EXPR(ASR::make_FunctionPointerCast_t(
        al, loc, src, to_sym, fn_type, nullptr));
    ASR::expr_t* lhs = ASRUtils::EXPR(ASR::make_Var_t(al, loc, tmp_var));
    // Associate, not Assignment: procedure pointers are associated the
    // same way as `sp => add_ten`, and Assignment's store lowering does
    // not match the Pointer(FunctionType) physical representation.
    association_body->push_back(al, ASRUtils::STMT(
        ASR::make_Associate_t(al, loc, lhs, cast, ASR::pointer_bounds_formType::NoPointerBounds)));
    return tmp_var;
}

ASR::symbol_t* CommonVisitorBase::make_opaque_procedure_interface(const Location& loc,
        SymbolTable* scope, const std::string& name, ASR::ttype_t* type,
        ASR::accessType access, SymbolTable* fn_scope) {
    if (fn_scope == nullptr) {
        fn_scope = al.make_new<SymbolTable>(scope);
    }
    ASR::expr_t* return_var = nullptr;
    ASR::ttype_t* result_type = ASR::down_cast<ASR::FunctionType_t>(
        type)->m_return_var_type;
    if (result_type != nullptr) {
        std::string result_name = name + "_return_var_name";
        SetChar dependencies;
        dependencies.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, dependencies, result_type);
        ASR::symbol_t* result = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, fn_scope,
                s2c(al, result_name), dependencies.p, dependencies.size(),
                ASRUtils::intent_return_var, nullptr, nullptr,
                ASR::storage_typeType::Default, result_type, nullptr,
                ASR::abiType::Source, ASR::Public, ASR::presenceType::Required,
                false));
        fn_scope->add_symbol(result_name, result);
        return_var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, result));
    }
    ASR::symbol_t* iface = ASR::down_cast<ASR::symbol_t>(ASR::make_Function_t(
        al, loc, fn_scope, s2c(al, name), type, nullptr, 0, nullptr, 0,
        nullptr, 0, return_var, access, false, false, nullptr, nullptr,
        nullptr, nullptr));
    scope->add_symbol(name, iface);
    return iface;
}

const std::map<std::string, ASR::ttype_t*>* CommonVisitorBase::declaring_implicit_types(
        ASR::symbol_t* sym) {
    for (SymbolTable* s = ASRUtils::symbol_parent_symtab(
                ASRUtils::symbol_get_past_external(sym));
            s != nullptr; s = s->parent) {
        if (s->asr_owner == nullptr) continue;
        auto it = implicit_mapping.find(get_hash(s->asr_owner));
        if (it != implicit_mapping.end() && !it->second.empty()) {
            return &it->second;
        }
    }
    return nullptr;
}

bool CommonVisitorBase::is_statement_function_host_dummy(ASR::symbol_t* source) {
    ASR::symbol_t* proc = ASRUtils::symbol_get_past_external(source);
    bool is_procedure_value = ASR::is_a<ASR::Function_t>(*proc) ||
        (ASR::is_a<ASR::Variable_t>(*proc) && ASR::is_a<ASR::FunctionType_t>(
            *ASR::down_cast<ASR::Variable_t>(proc)->m_type));
    if (!is_procedure_value || statement_function_parent_scope == nullptr) {
        return false;
    }
    SymbolTable* host_scope = statement_function_parent_scope;
    while (host_scope->asr_owner && ASR::is_a<ASR::symbol_t>(*host_scope->asr_owner)
            && (ASR::is_a<ASR::Block_t>(*ASR::down_cast<ASR::symbol_t>(host_scope->asr_owner))
                || ASR::is_a<ASR::AssociateBlock_t>(*ASR::down_cast<ASR::symbol_t>(
                    host_scope->asr_owner)))) {
        host_scope = host_scope->parent;
    }
    if (host_scope->asr_owner == nullptr || !ASR::is_a<ASR::symbol_t>(*host_scope->asr_owner)
            || !ASR::is_a<ASR::Function_t>(*ASR::down_cast<ASR::symbol_t>(
                host_scope->asr_owner))) {
        return false;
    }
    return ASRUtils::is_dummy_argument(*ASR::down_cast<ASR::Function_t>(
        ASR::down_cast<ASR::symbol_t>(host_scope->asr_owner)), proc);
}

SymbolTable* CommonVisitorBase::implicit_interface_scope() {
    SymbolTable *sym_scope = statement_function_parent_scope
        ? statement_function_parent_scope : current_scope;
    while (sym_scope->asr_owner && ASR::is_a<ASR::symbol_t>(*sym_scope->asr_owner)) {
        ASR::symbol_t* owner = ASR::down_cast<ASR::symbol_t>(sym_scope->asr_owner);
        if (!ASR::is_a<ASR::AssociateBlock_t>(*owner) && !ASR::is_a<ASR::Block_t>(*owner)) {
            break;
        }
        sym_scope = sym_scope->parent;
    }
    return sym_scope;
}

ASR::symbol_t* CommonVisitorBase::get_or_create_opaque_procedure(const std::string &name,
        const Location &loc) {
    SymbolTable* sym_scope = implicit_interface_scope();
    ASR::symbol_t* existing = sym_scope->get_symbol(name);
    if (existing && ASRUtils::is_bare_implicit_interface(existing)) {
        return existing;
    }
    SymbolTable* fn_scope = al.make_new<SymbolTable>(sym_scope);
    ASR::symbol_t* fn = ASR::down_cast<ASR::symbol_t>(
        ASRUtils::make_Function_t_util(al, loc, fn_scope, s2c(al, name),
            nullptr, 0, nullptr, 0, nullptr, 0, nullptr,
            ASR::abiType::Source, ASR::accessType::Public,
            ASR::deftypeType::ImplicitInterface, nullptr, false, false,
            false, false, false, nullptr, 0, false, false, false));
    sym_scope->add_or_overwrite_symbol(name, fn);
    return fn;
}

ASR::symbol_t* CommonVisitorBase::replace_variable_with_opaque_procedure(const std::string &name,
        ASR::symbol_t* var, ASR::ttype_t* return_type, const Location &loc) {
    SymbolTable* owner_scope = ASRUtils::symbol_parent_symtab(var);
    SymbolTable* fn_scope = al.make_new<SymbolTable>(owner_scope);
    ASR::expr_t* return_var = nullptr;
    if (return_type) {
        std::string return_var_name = name + "_return_var_name";
        ASR::symbol_t* rv = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, fn_scope,
                s2c(al, return_var_name), nullptr, 0,
                ASRUtils::intent_return_var, nullptr, nullptr,
                ASR::storage_typeType::Default,
                ASRUtils::type_get_past_allocatable_pointer(return_type),
                nullptr, ASR::abiType::Source, ASR::Public,
                ASR::presenceType::Required, false));
        fn_scope->add_symbol(return_var_name, rv);
        return_var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, rv));
    }
    ASR::symbol_t* fn = ASR::down_cast<ASR::symbol_t>(
        ASRUtils::make_Function_t_util(al, loc, fn_scope, s2c(al, name),
            nullptr, 0, nullptr, 0, nullptr, 0, return_var,
            ASR::abiType::Source, ASR::accessType::Public,
            ASR::deftypeType::ImplicitInterface, nullptr, false, false,
            false, false, false, nullptr, 0, false, false, false));
    owner_scope->add_or_overwrite_symbol(name, fn);
    if (owner_scope->asr_owner && ASR::is_a<ASR::symbol_t>(*owner_scope->asr_owner)
            && ASR::is_a<ASR::Function_t>(*ASR::down_cast<ASR::symbol_t>(
                owner_scope->asr_owner))) {
        ASR::Function_t* owner = ASR::down_cast<ASR::Function_t>(
            ASR::down_cast<ASR::symbol_t>(owner_scope->asr_owner));
        ASR::FunctionType_t* owner_type = ASRUtils::get_FunctionType(owner);
        for (size_t i = 0; i < owner->n_args; i++) {
            if (ASR::is_a<ASR::Var_t>(*owner->m_args[i]) &&
                    ASR::down_cast<ASR::Var_t>(owner->m_args[i])->m_v == var) {
                owner->m_args[i] = ASRUtils::EXPR(ASR::make_Var_t(al, loc, fn));
                procedures_with_late_procedure_dummies.insert(&owner->base);
                owner_type->m_arg_types[i] = ASR::down_cast<ASR::Function_t>(
                    fn)->m_function_signature;
            }
        }
    }
    // Earlier references to the variable in this scope now name the
    // procedure.
    ASRUtils::update_call_args(al, current_scope, compiler_options.implicit_interface);
    return fn;
}

void CommonVisitorBase::make_dummy_opaque_procedure(ASR::Variable_t* v, ASR::ttype_t* return_type,
        const Location &loc) {
    v->m_type = ASRUtils::make_opaque_procedure_type(al, loc,
        ASRUtils::type_get_past_allocatable_pointer(return_type));
    SymbolTable* owner_scope = v->m_parent_symtab;
    // Like `procedure()`, the variable declares its type with an opaque
    // procedure of the same type. The `~implicit_interface_` name lets
    // the siblings of a procedure with ENTRY points, which hold copies of
    // the same dummy, get their own copy of it.
    SymbolTable* iface_scope = al.make_new<SymbolTable>(owner_scope);
    std::string iface_name = "~implicit_interface_" + std::string(v->m_name)
        + "_" + iface_scope->get_counter();
    ASR::symbol_t* iface = make_opaque_procedure_interface(loc, owner_scope,
        iface_name, v->m_type, ASR::accessType::Private, iface_scope);
    v->m_type_declaration = iface;
    if (owner_scope->asr_owner && ASR::is_a<ASR::symbol_t>(*owner_scope->asr_owner)
            && ASR::is_a<ASR::Function_t>(*ASR::down_cast<ASR::symbol_t>(
                owner_scope->asr_owner))) {
        ASR::Function_t* owner = ASR::down_cast<ASR::Function_t>(
            ASR::down_cast<ASR::symbol_t>(owner_scope->asr_owner));
        ASR::FunctionType_t* owner_type = ASRUtils::get_FunctionType(owner);
        for (size_t i = 0; i < owner->n_args; i++) {
            if (ASR::is_a<ASR::Var_t>(*owner->m_args[i]) &&
                    ASR::down_cast<ASR::Var_t>(owner->m_args[i])->m_v == &v->base) {
                owner_type->m_arg_types[i] = v->m_type;
                procedures_with_late_procedure_dummies.insert(&owner->base);
            }
        }
    }
}

ASR::ttype_t* CommonVisitorBase::callsite_dummy_type(ASR::expr_t* actual, ASR::Function_t* impl_func,
        size_t i) {
    ASR::ttype_t *var_type = ASRUtils::expr_type(actual);
    // A dummy of a procedure without an explicit interface is neither
    // allocatable nor a pointer, so an allocatable or pointer scalar
    // actual associates its target with the dummy. A procedure pointer
    // actual associates the procedure it points to.
    if (!ASRUtils::is_array(var_type)) {
        var_type = ASRUtils::type_get_past_allocatable_pointer(var_type);
    }
    // Use Implementation's parameter type if available and implicit_argument_casting enabled
    if (impl_func && i < impl_func->n_args && ASR::is_a<ASR::Var_t>(*impl_func->m_args[i])
            && ASR::is_a<ASR::Variable_t>(*ASR::down_cast<ASR::Var_t>(impl_func->m_args[i])->m_v)) {
        ASR::Variable_t* impl_arg = ASRUtils::EXPR2VAR(impl_func->m_args[i]);
        ASR::ttype_t* impl_type = impl_arg->m_type;
        // Only override for scalar integers with different kinds
        if (ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_array(var_type)) &&
            ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_array(impl_type)) &&
            !ASRUtils::is_array(var_type) && !ASRUtils::is_array(impl_type)) {
            var_type = impl_type;
        }
    }
    if (ASRUtils::is_array(var_type)) {
        // For arrays like A(n, m) we use A(*) in implicit interface.
        ASR::ttype_t* array_var_type = ASRUtils::type_get_past_allocatable(
            ASRUtils::type_get_past_pointer(var_type));
        ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(array_var_type);
        // Dropping the actual's allocatable or pointer wrapper leaves a
        // deferred length on the element type, which only an allocatable
        // or pointer entity may carry. The dummy of an external procedure
        // takes the length from the string descriptor at run time, so it
        // is an assumed length, exactly like `character(len=*)`.
        if (ASR::is_a<ASR::String_t>(*array_type->m_type)) {
            ASR::String_t* elem_str = ASR::down_cast<ASR::String_t>(array_type->m_type);
            if (elem_str->m_len_kind == ASR::string_length_kindType::DeferredLength) {
                ASR::ttype_t* assumed_len_type = ASRUtils::TYPE(ASR::make_String_t(
                    al, array_type->m_type->base.loc, elem_str->m_kind, nullptr,
                    ASR::string_length_kindType::AssumedLength,
                    elem_str->m_physical_type));
                array_var_type = ASRUtils::make_Array_t_util(al, array_var_type->base.loc,
                    assumed_len_type, array_type->m_dims, array_type->n_dims,
                    ASR::abiType::Source, true, array_type->m_physical_type, true);
                array_type = ASR::down_cast<ASR::Array_t>(array_var_type);
            }
        }
        ASR::array_physical_typeType phys_type;
        if (array_type->m_physical_type == ASR::array_physical_typeType::AssumedRankArray) {
            phys_type = array_type->m_physical_type;
        } else {
            phys_type = ASR::array_physical_typeType::PointerArray;
        }
        var_type = ASRUtils::duplicate_type_with_empty_dims(al, array_var_type, phys_type, true);
    }
    // The length of a character dummy of a procedure without an
    // explicit interface is not known here, so the dummy synthesized for
    // a character actual takes the assumed-length form:
    // `character(len=*)`. The callee receives the length of the actual,
    // which is neither truncated to the length of an earlier actual that
    // shares the interface nor tied to the caller's scope (for
    // `character(len=n)`). A character *expression* actual (a
    // concatenation with a runtime-length operand, a substring with
    // computed bounds, ...) has a `DeferredLength` string type, which
    // only an allocatable or pointer may carry, so it is passed the same
    // way.
    if (!ASRUtils::is_allocatable(var_type) && !ASRUtils::is_pointer(var_type)) {
        ASR::ttype_t* elem_type = ASRUtils::type_get_past_array(var_type);
        if (ASR::is_a<ASR::String_t>(*elem_type)) {
            ASR::String_t* str_type = ASR::down_cast<ASR::String_t>(elem_type);
            if (str_type->m_len_kind == ASR::DeferredLength ||
                    str_type->m_len_kind == ASR::ExpressionLength) {
                ASR::ttype_t* assumed_len_type = ASRUtils::TYPE(ASR::make_String_t(
                    al, elem_type->base.loc, str_type->m_kind, nullptr,
                    ASR::AssumedLength, str_type->m_physical_type));
                if (ASRUtils::is_array(var_type)) {
                    ASR::Array_t* arr_type = ASR::down_cast<ASR::Array_t>(var_type);
                    var_type = ASRUtils::TYPE(ASR::make_Array_t(al,
                        var_type->base.loc, assumed_len_type, arr_type->m_dims,
                        arr_type->n_dims, arr_type->m_physical_type, arr_type->m_memory_space));
                } else {
                    var_type = assumed_len_type;
                }
            }
        }
    }
    return var_type;
}

ASR::symbol_t* CommonVisitorBase::callsite_dummy_type_declaration(ASR::expr_t* actual,
        ASR::ttype_t* dummy_type) {
    if (!ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_array(dummy_type))) {
        return ASRUtils::get_struct_sym_from_struct_expr(actual);
    }
    if (ASR::is_a<ASR::Var_t>(*actual)) {
        ASR::symbol_t* passed_sym = ASR::down_cast<ASR::Var_t>(actual)->m_v;
        if (ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(passed_sym))) {
            return passed_sym;
        } else if (ASR::is_a<ASR::Variable_t>(*passed_sym)) {
            return ASR::down_cast<ASR::Variable_t>(passed_sym)->m_type_declaration;
        }
    }
    return nullptr;
}

ASR::symbol_t* CommonVisitorBase::get_callsite_interface(const Location &loc, const std::string &name,
        Vec<ASR::call_arg_t> &args, ASR::ttype_t* return_type) {
    SymbolTable* sym_scope = implicit_interface_scope();
    std::string sym_name = to_lower(name);

    // A character result whose length is an expression of the caller
    // (`character(len=n), external :: f`) is returned into a string of
    // that length, which the called procedure receives at run time: the
    // interface declares an assumed-length result, not tied to the
    // caller's scope.
    if (return_type && ASR::is_a<ASR::String_t>(*return_type)) {
        ASR::String_t* result_str = ASR::down_cast<ASR::String_t>(return_type);
        if (result_str->m_len_kind == ASR::ExpressionLength && result_str->m_len &&
                !ASR::is_a<ASR::IntegerConstant_t>(*result_str->m_len)) {
            return_type = ASRUtils::TYPE(ASR::make_String_t(al, return_type->base.loc,
                result_str->m_kind, nullptr, ASR::AssumedLength,
                result_str->m_physical_type));
        }
    }

    // For implicit argument casting, look up the Implementation to get correct param types
    ASR::Function_t* impl_func = nullptr;
    if (compiler_options.implicit_argument_casting) {
        SymbolTable* global_scope = current_scope;
        while (global_scope->parent != nullptr) {
            global_scope = global_scope->parent;
        }
        ASR::symbol_t* impl_sym = global_scope->get_symbol(sym_name);
        if (impl_sym && ASR::is_a<ASR::Function_t>(*impl_sym)) {
            ASR::Function_t* candidate = ASR::down_cast<ASR::Function_t>(impl_sym);
            if (ASRUtils::get_FunctionType(candidate)->m_deftype
                    == ASR::deftypeType::Implementation) {
                impl_func = candidate;
            }
        }
    }

    Vec<ASR::ttype_t*> arg_types;
    arg_types.reserve(al, args.size());
    Vec<ASR::symbol_t*> arg_decls;
    arg_decls.reserve(al, args.size());
    for (size_t i = 0; i < args.size(); i++) {
        ASR::expr_t* actual = args[i].m_value;
        ASR::ttype_t* t = callsite_dummy_type(actual, impl_func, i);
        arg_types.push_back(al, t);
        arg_decls.push_back(al, callsite_dummy_type_declaration(actual, t));
    }

    // Reuse an interface of an earlier reference with the same signature.
    std::string prefix = generated_symbol_name(sym_name, "fpcast");
    for (auto &item : sym_scope->get_scope()) {
        if (item.first.rfind(prefix, 0) != 0 ||
                !ASR::is_a<ASR::Function_t>(*item.second)) {
            continue;
        }
        ASR::Function_t* candidate = ASR::down_cast<ASR::Function_t>(item.second);
        ASR::FunctionType_t* ct = ASRUtils::get_FunctionType(candidate);
        if (candidate->n_args != args.size() || ct->n_arg_types != args.size()
                || (ct->m_return_var_type == nullptr) != (return_type == nullptr)
                || (return_type && !ASRUtils::types_equal(ct->m_return_var_type,
                    return_type, nullptr, nullptr, true))) {
            continue;
        }
        bool same = true;
        for (size_t i = 0; same && i < args.size(); i++) {
            ASR::expr_t* dummy = candidate->m_args[i];
            ASR::symbol_t* dummy_sym = ASR::down_cast<ASR::Var_t>(dummy)->m_v;
            ASR::symbol_t* dummy_decl = ASR::is_a<ASR::Variable_t>(*dummy_sym)
                ? ASR::down_cast<ASR::Variable_t>(dummy_sym)->m_type_declaration
                : dummy_sym;
            same = ASRUtils::types_equal(ct->m_arg_types[i], arg_types[i],
                    nullptr, nullptr, true)
                && ASRUtils::symbol_get_past_external(dummy_decl)
                    == ASRUtils::symbol_get_past_external(arg_decls[i]);
        }
        if (same) {
            return item.second;
        }
    }

    SymbolTable* fn_scope = al.make_new<SymbolTable>(sym_scope);
    Vec<ASR::expr_t*> dummies;
    dummies.reserve(al, args.size());
    for (size_t i = 0; i < args.size(); i++) {
        std::string arg_name = generated_symbol_name(sym_name,
            "arg_" + std::to_string(i));
        ASR::expr_t* actual = args[i].m_value;
        if (ASR::is_a<ASR::Var_t>(*actual) &&
                ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(actual)->m_v))) {
            dummies.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                ASR::down_cast<ASR::Var_t>(actual)->m_v)));
            continue;
        }
        ASR::ttype_t* var_type = arg_types[i];
        ASR::symbol_t* type_decl = arg_decls[i];
        if (type_decl == nullptr && ASR::is_a<ASR::FunctionType_t>(
                *ASRUtils::type_get_past_array(var_type))) {
            ASR::FunctionType_t* ft = ASR::down_cast<ASR::FunctionType_t>(
                ASRUtils::type_get_past_array(var_type));
            SymbolTable* iface_scope = al.make_new<SymbolTable>(sym_scope);
            std::string iface_name = "~implicit_interface_" + arg_name + "_" +
                iface_scope->get_counter();
            ASR::expr_t* iface_return = nullptr;
            if (ft->m_return_var_type) {
                std::string ret_name = iface_name + "_return_var";
                ASR::asr_t* ret_var = ASRUtils::make_Variable_t_util(al, loc,
                    iface_scope, s2c(al, ret_name), nullptr, 0,
                    ASRUtils::intent_return_var, nullptr, nullptr,
                    ASR::storage_typeType::Default, ft->m_return_var_type, nullptr,
                    ASR::abiType::Source, ASR::Public, ASR::presenceType::Required, false);
                iface_scope->add_symbol(ret_name, ASR::down_cast<ASR::symbol_t>(ret_var));
                iface_return = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                    ASR::down_cast<ASR::symbol_t>(ret_var)));
            }
            ASR::asr_t* iface_func = ASRUtils::make_Function_t_util(al, loc,
                iface_scope, s2c(al, iface_name), nullptr, 0,
                nullptr, 0, nullptr, 0, iface_return,
                ASR::abiType::Source, ASR::accessType::Public,
                ASR::deftypeType::Interface, nullptr, false, false,
                false, false, false, nullptr, 0, false, false, false);
            sym_scope->add_symbol(iface_name, ASR::down_cast<ASR::symbol_t>(iface_func));
            type_decl = ASR::down_cast<ASR::symbol_t>(iface_func);
        }
        SetChar variable_dependencies_vec;
        variable_dependencies_vec.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, var_type);
        ASR::symbol_t* v = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc,
            fn_scope, s2c(al, arg_name), variable_dependencies_vec.p,
            variable_dependencies_vec.size(), ASRUtils::intent_unspecified,
            nullptr, nullptr, ASR::storage_typeType::Default, var_type, type_decl,
            ASR::abiType::Source, ASR::Public, ASR::presenceType::Required,
            false));
        fn_scope->add_or_overwrite_symbol(arg_name, v);
        dummies.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, v)));
    }
    ASR::expr_t *to_return = nullptr;
    if (return_type) {
        std::string return_var_name = generated_symbol_name(sym_name, "result");
        SetChar variable_dependencies_vec;
        variable_dependencies_vec.reserve(al, 1);
        ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, return_type);
        ASR::asr_t *return_var = ASRUtils::make_Variable_t_util(al, loc,
            fn_scope, s2c(al, return_var_name), variable_dependencies_vec.p,
            variable_dependencies_vec.size(), ASRUtils::intent_return_var,
            nullptr, nullptr, ASR::storage_typeType::Default, return_type, nullptr,
            ASR::abiType::Source, ASR::Public, ASR::presenceType::Required,
            false);
        fn_scope->add_symbol(return_var_name, ASR::down_cast<ASR::symbol_t>(return_var));
        to_return = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
            ASR::down_cast<ASR::symbol_t>(return_var)));
    }
    std::string iface_name = sym_scope->get_unique_name(prefix, false);
    ASR::symbol_t* iface = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Function_t_util(
        al, loc, fn_scope, s2c(al, iface_name), nullptr, 0,
        dummies.p, dummies.size(), nullptr, 0, to_return,
        ASR::abiType::Source, ASR::accessType::Public, ASR::deftypeType::Interface,
        nullptr, false, false, false, false, false, nullptr, 0,
        false, false, false));
    sym_scope->add_symbol(iface_name, iface);
    return iface;
}

void CommonVisitorBase::check_implicit_procedure_kind(const Location &loc, const std::string &name,
        ASR::symbol_t* source, bool is_function) {
    ASR::symbol_t* proc = ASRUtils::symbol_get_past_external(source);
    bool is_external = false;
    if (ASR::is_a<ASR::Function_t>(*proc)) {
        ASR::symbol_t* owner = ASRUtils::get_asr_owner(proc);
        is_external = !(owner && ASR::is_a<ASR::Function_t>(*owner) &&
            ASRUtils::is_dummy_argument(*ASR::down_cast<ASR::Function_t>(owner), proc));
    }
    bool first_use_is_function = is_external
        ? external_procedure_is_function.emplace(
            ASRUtils::symbol_name(proc), is_function).first->second
        : implicit_procedure_is_function.emplace(proc, is_function).first->second;
    bool contradicts = first_use_is_function != is_function;
    if (!contradicts && is_function && is_external) {
        ASR::symbol_t* definition = ASRUtils::get_tu_symtab(current_scope)->get_symbol(
            ASRUtils::symbol_name(proc));
        contradicts = definition && definition != proc &&
            ASR::is_a<ASR::Function_t>(*definition) &&
            ASRUtils::get_FunctionType(definition)->m_deftype ==
                ASR::deftypeType::Implementation &&
            ASR::down_cast<ASR::Function_t>(definition)->m_return_var == nullptr;
    }
    if (contradicts) {
        diag.add(Diagnostic(std::string(is_function ? "Subroutine" : "Function") +
            " `" + name + "` called as a " + (is_function ? "function" : "subroutine"),
            Level::Error, Stage::Semantic, {Label("", {loc})}));
        throw SemanticAbort();
    }
}

ASR::symbol_t* CommonVisitorBase::implicit_call_interface(const Location &loc, const std::string &name,
        ASR::symbol_t* source, Vec<ASR::call_arg_t> &args, ASR::ttype_t* return_type) {
    check_implicit_procedure_kind(loc, name, source, return_type != nullptr);
    check_actuals_against_external_definition(loc, source, args,
        return_type != nullptr);
    ASR::symbol_t* iface = get_callsite_interface(loc, name, args, return_type);
    // Nothing is known about the called procedure.
    current_function_deterministic = false;
    current_function_side_effect_free = false;
    return iface;
}

ASR::Module_t* CommonVisitorBase::enclosing_module(SymbolTable* scope) {
    for (SymbolTable* s = scope; s != nullptr; s = s->parent) {
        if (s->asr_owner && ASR::is_a<ASR::symbol_t>(*s->asr_owner)) {
            ASR::symbol_t* owner = ASR::down_cast<ASR::symbol_t>(s->asr_owner);
            if (ASR::is_a<ASR::Module_t>(*owner)) {
                return ASR::down_cast<ASR::Module_t>(owner);
            }
        }
    }
    return nullptr;
}

bool CommonVisitorBase::module_depends_on(SymbolTable* tu_scope, const std::string& name,
        const std::string& used, std::set<std::string>& visited) {
    if (!visited.insert(name).second) {
        return false;
    }
    ASR::symbol_t* sym = tu_scope->get_symbol(name);
    if (sym == nullptr || !ASR::is_a<ASR::Module_t>(*sym)) {
        return false;
    }
    ASR::Module_t* m = ASR::down_cast<ASR::Module_t>(sym);
    for (size_t i = 0; i < m->n_dependencies; i++) {
        if (used == m->m_dependencies[i]
                || module_depends_on(tu_scope, m->m_dependencies[i], used, visited)) {
            return true;
        }
    }
    return false;
}

void CommonVisitorBase::collect_used_modules(SymbolTable* scope, std::set<std::string>& modules) {
    for (auto &item : scope->get_scope()) {
        ASR::symbol_t* s = item.second;
        if (ASR::is_a<ASR::ExternalSymbol_t>(*s)) {
            ASR::symbol_t* owner = ASRUtils::get_asr_owner(
                ASR::down_cast<ASR::ExternalSymbol_t>(s)->m_external);
            while (owner && !ASR::is_a<ASR::Module_t>(*owner)) {
                owner = ASRUtils::get_asr_owner(owner);
            }
            if (owner) {
                modules.insert(ASRUtils::symbol_name(owner));
            }
        } else if (SymbolTable* nested = ASRUtils::symbol_symtab(s)) {
            collect_used_modules(nested, modules);
        }
    }
}

bool CommonVisitorBase::call_module_getters(SymbolTable* module_scope,
        const std::set<ASR::symbol_t*>& getters, SymbolTable* iface_scope) {
    std::map<ASR::symbol_t*, ASR::symbol_t*>& copies = getter_copies[module_scope];
    std::map<std::string, std::string> names;
    for (ASR::symbol_t* getter : getters) {
        ASR::symbol_t*& copy = copies[getter];
        if (copy == nullptr) {
            ASRUtils::SymbolDuplicator duplicator(al);
            copy = duplicator.duplicate_Function(
                ASR::down_cast<ASR::Function_t>(getter), module_scope);
            if (copy == nullptr) {
                return false;
            }
            ASR::Function_t* copy_fn = ASR::down_cast<ASR::Function_t>(copy);
            std::string copy_name = module_scope->get_unique_name(
                ASRUtils::symbol_name(getter), false);
            copy_fn->m_name = s2c(al, copy_name);
            // Public, as the getters of module procedures are, so that
            // a USE of the module imports it.
            copy_fn->m_access = ASR::accessType::Public;
            module_scope->add_symbol(copy_name, copy);
        }
        names[ASRUtils::symbol_name(getter)] = ASRUtils::symbol_name(copy);
    }
    class RetargetGetters : public ASR::BaseWalkVisitor<RetargetGetters> {
    public:
        Allocator& al;
        std::map<ASR::symbol_t*, ASR::symbol_t*>& copies;
        std::map<std::string, std::string>& names;
        RetargetGetters(Allocator& al, std::map<ASR::symbol_t*, ASR::symbol_t*>& copies,
            std::map<std::string, std::string>& names)
            : al(al), copies(copies), names(names) {}
        void visit_FunctionCall(const ASR::FunctionCall_t& x) {
            ASR::FunctionCall_t& xx = const_cast<ASR::FunctionCall_t&>(x);
            auto copy = copies.find(x.m_name);
            if (copy != copies.end()) {
                xx.m_name = copy->second;
            }
            if (x.m_original_name != nullptr) {
                copy = copies.find(x.m_original_name);
                if (copy != copies.end()) {
                    xx.m_original_name = copy->second;
                }
            }
            ASR::BaseWalkVisitor<RetargetGetters>::visit_FunctionCall(x);
        }
        void visit_Variable(const ASR::Variable_t& x) {
            // A copied variable shares its dependencies with the original.
            SetChar dependencies;
            dependencies.reserve(al, x.n_dependencies);
            for (size_t i = 0; i < x.n_dependencies; i++) {
                auto name = names.find(x.m_dependencies[i]);
                dependencies.push_back(al, name != names.end()
                    ? s2c(al, name->second) : x.m_dependencies[i]);
            }
            ASR::Variable_t& xx = const_cast<ASR::Variable_t&>(x);
            xx.m_dependencies = dependencies.p;
            xx.n_dependencies = dependencies.size();
            ASR::BaseWalkVisitor<RetargetGetters>::visit_Variable(x);
        }
    };
    RetargetGetters retarget(al, copies, names);
    retarget.visit_compile_time_value = true;
    for (auto &item : iface_scope->get_scope()) {
        retarget.visit_symbol(*item.second);
    }
    return true;
}

ASR::symbol_t* CommonVisitorBase::make_interface_copy(ASR::Function_t* fn, SymbolTable* scope,
        const std::string& name, std::string* cyclic_module,
        std::string* file_procedure) {
    // The entities of `fn` that the declarations of its dummy arguments
    // and of its result name, directly or through each other.
    class ReferencedEntities : public ASR::BaseWalkVisitor<ReferencedEntities> {
    public:
        SymbolTable* symtab;
        const std::set<ASR::symbol_t*>& getter_functions;
        std::set<ASR::symbol_t*> found;
        std::vector<ASR::symbol_t*> pending;
        ReferencedEntities(SymbolTable* symtab,
                const std::set<ASR::symbol_t*>& getter_functions)
            : symtab(symtab), getter_functions(getter_functions) {
            this->visit_compile_time_value = true;
        }
        // The getters of the translation unit that the declarations call.
        std::set<ASR::symbol_t*> getters;
        // Another procedure of the translation unit that they call.
        ASR::symbol_t* file_procedure = nullptr;
        void add(ASR::symbol_t* s) {
            if (s != nullptr && ASRUtils::symbol_parent_symtab(s) == symtab
                    && found.insert(s).second) {
                pending.push_back(s);
            }
        }
        void visit_Var(const ASR::Var_t& x) {
            add(x.m_v);
        }
        // A component of a use-associated variable, as in `p(d%n)`,
        // is named by an ExternalSymbol of `fn` that the copy needs too.
        void visit_StructInstanceMember(const ASR::StructInstanceMember_t& x) {
            add(x.m_m);
            ASR::BaseWalkVisitor<ReferencedEntities>::visit_StructInstanceMember(x);
        }
        void visit_FunctionCall(const ASR::FunctionCall_t& x) {
            add(x.m_name);
            add(x.m_original_name);
            if (ASR::is_a<ASR::Function_t>(*x.m_name)
                    && ASRUtils::is_tu_scope(ASRUtils::symbol_parent_symtab(x.m_name))) {
                if (getter_functions.find(x.m_name) != getter_functions.end()) {
                    getters.insert(x.m_name);
                } else if (file_procedure == nullptr) {
                    file_procedure = x.m_name;
                }
            }
            ASR::BaseWalkVisitor<ReferencedEntities>::visit_FunctionCall(x);
        }
        void visit_GenericProcedure(const ASR::GenericProcedure_t& x) {
            for (size_t i = 0; i < x.n_procs; i++) {
                add(x.m_procs[i]);
            }
        }
        void visit_CustomOperator(const ASR::CustomOperator_t& x) {
            for (size_t i = 0; i < x.n_procs; i++) {
                add(x.m_procs[i]);
            }
        }
        void visit_StructConstructor(const ASR::StructConstructor_t& x) {
            add(x.m_dt_sym);
            ASR::BaseWalkVisitor<ReferencedEntities>::visit_StructConstructor(x);
        }
        void visit_Variable(const ASR::Variable_t& x) {
            add(x.m_type_declaration);
            ASR::BaseWalkVisitor<ReferencedEntities>::visit_Variable(x);
        }
        void visit_Struct(const ASR::Struct_t& x) {
            add(x.m_parent);
            ASR::BaseWalkVisitor<ReferencedEntities>::visit_Struct(x);
        }
    };
    ReferencedEntities referenced(fn->m_symtab, getter_functions);
    for (size_t i = 0; i < fn->n_args; i++) {
        referenced.visit_expr(*fn->m_args[i]);
    }
    if (fn->m_return_var) {
        referenced.visit_expr(*fn->m_return_var);
    }
    while (!referenced.pending.empty()) {
        ASR::symbol_t* s = referenced.pending.back();
        referenced.pending.pop_back();
        referenced.visit_symbol(*s);
    }
    if (referenced.file_procedure != nullptr) {
        if (file_procedure != nullptr) {
            *file_procedure = ASRUtils::symbol_name(referenced.file_procedure);
        }
        return nullptr;
    }
    SymbolTable* iface_scope = al.make_new<SymbolTable>(scope);
    ASRUtils::SymbolDuplicator duplicator(al);
    std::vector<std::pair<ASR::Variable_t*, ASR::Variable_t*>> variables;
    for (auto &item : fn->m_symtab->get_scope()) {
        ASR::symbol_t* s = item.second;
        if (referenced.found.find(s) == referenced.found.end()) {
            continue;
        }
        duplicator.duplicate_symbol(s, iface_scope);
        ASR::symbol_t* copy = iface_scope->get_symbol(item.first);
        if (copy == nullptr) {
            return nullptr;
        }
        if (ASR::is_a<ASR::Variable_t>(*s) && ASR::is_a<ASR::Variable_t>(*copy)) {
            variables.push_back({ASR::down_cast<ASR::Variable_t>(s),
                ASR::down_cast<ASR::Variable_t>(copy)});
        }
    }
    duplicator.remap_local_references(variables, iface_scope);
    duplicator.fixup_local_type_declarations(iface_scope, fn->m_symtab);
    // A copied generic name or defined operator names the copies of its
    // specific procedures of `fn`, not the procedures themselves.
    auto copied_procs = [&](ASR::symbol_t** procs, size_t n_procs) {
        Vec<ASR::symbol_t*> copies;
        copies.reserve(al, n_procs);
        for (size_t i = 0; i < n_procs; i++) {
            ASR::symbol_t* proc = procs[i];
            if (ASRUtils::symbol_parent_symtab(proc) == fn->m_symtab) {
                proc = iface_scope->get_symbol(ASRUtils::symbol_name(proc));
                LCOMPILERS_ASSERT(proc != nullptr);
            }
            copies.push_back(al, proc);
        }
        return copies.p;
    };
    for (auto &item : iface_scope->get_scope()) {
        if (ASR::is_a<ASR::GenericProcedure_t>(*item.second)) {
            ASR::GenericProcedure_t* g = ASR::down_cast<ASR::GenericProcedure_t>(item.second);
            g->m_procs = copied_procs(g->m_procs, g->n_procs);
        } else if (ASR::is_a<ASR::CustomOperator_t>(*item.second)) {
            ASR::CustomOperator_t* op = ASR::down_cast<ASR::CustomOperator_t>(item.second);
            op->m_procs = copied_procs(op->m_procs, op->n_procs);
        }
    }
    for (auto &item : iface_scope->get_scope()) {
        if (!ASR::is_a<ASR::Variable_t>(*item.second)) {
            continue;
        }
        ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(item.second);
        if (v->m_type_declaration == nullptr) {
            continue;
        }
        v->m_type_declaration = ASRUtils::import_type_declaration(al,
            v->m_type_declaration, iface_scope);
        if (!ASRUtils::is_visible_from(v->m_type_declaration, iface_scope)) {
            return nullptr;
        }
    }
    ASRUtils::ExprStmtWithScopeDuplicator node_duplicator(al, iface_scope);
    node_duplicator.success = true;
    Vec<ASR::expr_t*> iface_args;
    iface_args.reserve(al, fn->n_args);
    for (size_t i = 0; i < fn->n_args; i++) {
        iface_args.push_back(al, node_duplicator.duplicate_expr(fn->m_args[i]));
    }
    ASR::expr_t* iface_return_var = node_duplicator.duplicate_expr(fn->m_return_var);
    if (!node_duplicator.success) {
        return nullptr;
    }
    ASR::Module_t* module = enclosing_module(scope);
    std::set<std::string> used_modules;
    if (module != nullptr) {
        collect_used_modules(iface_scope, used_modules);
        for (ASR::symbol_t* getter : referenced.getters) {
            collect_used_modules(ASRUtils::symbol_symtab(getter), used_modules);
        }
        used_modules.erase(module->m_name);
        for (auto &used : used_modules) {
            std::set<std::string> visited;
            if (module_depends_on(scope->get_tu_scope(), used, module->m_name, visited)) {
                if (cyclic_module != nullptr) {
                    *cyclic_module = used;
                }
                return nullptr;
            }
        }
    }
    if (module != nullptr && !referenced.getters.empty()
            && !call_module_getters(module->m_symtab, referenced.getters, iface_scope)) {
        return nullptr;
    }
    ASR::FunctionType_t* fn_type = ASRUtils::get_FunctionType(fn);
    ASR::symbol_t* iface = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Function_t_util(
        al, fn->base.base.loc, iface_scope, s2c(al, name), nullptr, 0,
        iface_args.p, iface_args.size(), nullptr, 0, iface_return_var,
        fn_type->m_abi, fn->m_access, ASR::deftypeType::Interface,
        fn_type->m_bindc_name, fn_type->m_elemental, fn_type->m_pure,
        false, false, false, nullptr, 0, false, fn->m_deterministic,
        fn->m_side_effect_free));
    PassUtils::UpdateDependenciesVisitor dependencies(al);
    dependencies.visit_Function(*ASR::down_cast<ASR::Function_t>(iface));
    for (auto &used : used_modules) {
        current_module_dependencies.push_back(al, s2c(al, used));
    }
    scope->add_symbol(name, iface);
    return iface;
}

ASR::symbol_t* CommonVisitorBase::external_procedure_reference(ASR::symbol_t* proc,
        const Location& loc) {
    if (proc == nullptr || !ASR::is_a<ASR::Function_t>(*proc)
            || ASRUtils::symbol_parent_symtab(proc)->parent != nullptr
            || ASRUtils::get_FunctionType(proc)->m_deftype
                != ASR::deftypeType::Implementation
            || enclosing_module(current_scope) == nullptr) {
        return proc;
    }
    ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(proc);
    SymbolTable* sym_scope = implicit_interface_scope();
    if (sym_scope->get_symbol(fn->m_name) != nullptr) {
        return proc;
    }
    std::string cyclic_module, file_procedure;
    ASR::symbol_t* iface = make_interface_copy(fn, sym_scope, fn->m_name,
        &cyclic_module, &file_procedure);
    if (iface == nullptr) {
        if (compiler_options.implicit_interface) {
            // As if the procedure were defined in another file, but
            // the references are still checked against the definition.
            ASR::symbol_t* opaque = get_or_create_opaque_procedure(fn->m_name, loc);
            external_procedure_definitions[opaque] = proc;
            return opaque;
        }
        std::string module_name = enclosing_module(current_scope)->m_name;
        std::string reason = "cannot be copied into the module";
        std::string help = "help: use --implicit-interface to reference it "
            "through an implicit interface";
        if (!cyclic_module.empty()) {
            reason = "names an entity of the module '" + cyclic_module
                + "', which uses '" + module_name + "'";
        } else if (!file_procedure.empty()) {
            // The declarations of the procedure are not conforming:
            // a specification function needs an explicit interface.
            reason = "calls the procedure '" + file_procedure
                + "', which has an implicit interface";
            help = "help: give '" + file_procedure + "' an explicit interface in '"
                + std::string(fn->m_name) + "', or use --implicit-interface";
        }
        diag.add(Diagnostic("referencing the external procedure '"
            + std::string(fn->m_name) + "' in the module '" + module_name
            + "' without an explicit interface is not supported yet when its interface "
            + reason, Level::Error, Stage::Semantic, {Label(help, {loc})}));
        throw SemanticAbort();
    }
    external_procedure_definitions[iface] = proc;
    return iface;
}

void CommonVisitorBase::check_call_actual_type(ASR::expr_t* passed_arg, ASR::expr_t* dummy,
        bool self_passing_call) {
    ASR::Variable_t* v = ASRUtils::EXPR2VAR(dummy);
    ASR::ttype_t* passed_type = ASRUtils::expr_type(passed_arg);
    ASR::ttype_t* param_type = v->m_type;
    // Skip type checking for function types (procedure dummy
    // arguments are validated separately).
    bool is_function_type_arg = ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_array(passed_type)) ||
                        ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_array(param_type));
    // For polymorphic (class) types, don't unconditionally skip —
    // verify actual compatibility instead. A genuinely incompatible
    // actual argument (e.g. an integer passed to a class(T) dummy)
    // must be reported here, otherwise it silently reaches codegen
    // and crashes there.
    bool has_class_type_side = ASRUtils::is_class_type(ASRUtils::type_get_past_array(passed_type)) ||
                        ASRUtils::is_class_type(ASRUtils::type_get_past_array(param_type));
    bool is_checkable_class_arg = !is_function_type_arg && has_class_type_side && !self_passing_call;
    bool skip_check = is_function_type_arg || (has_class_type_side && self_passing_call) ||
                    (is_checkable_class_arg ? ASRUtils::can_pass_class_argument(dummy, passed_arg) : false);
    // For implicit_argument_casting, skip type checking for
    // compatible type families (e.g., numeric↔numeric, string↔string)
    // but reject fundamentally incompatible types (e.g., string→integer)
    // that would cause an ICE in Call_t_body.
    // Also reject arrays of integers with different kinds, which
    // Call_t_body cannot handle.
    if (compiler_options.implicit_argument_casting && !skip_check) {
        ASR::ttype_t* passed_elem = ASRUtils::type_get_past_array(passed_type);
        ASR::ttype_t* param_elem = ASRUtils::type_get_past_array(param_type);
        auto is_numeric = [](ASR::ttype_t* t) {
            return ASR::is_a<ASR::Integer_t>(*t) ||
                   ASR::is_a<ASR::Real_t>(*t) ||
                   ASR::is_a<ASR::Complex_t>(*t);
        };
        bool both_numeric = is_numeric(passed_elem) && is_numeric(param_elem);
        // `same_type_class` is only meaningful for scalar
        // built-in types where the AST-level Cast node can
        // paper over differences in kind/length. Structs,
        // classes, pointers, and function types must still
        // be checked strictly — otherwise e.g. passing
        // type(foo) where type(bar) is expected would be
        // silently accepted.
        bool same_type_class = (passed_elem->type == param_elem->type)
            && !ASR::is_a<ASR::StructType_t>(*passed_elem)
            && !ASR::is_a<ASR::Pointer_t>(*passed_elem)
            && !ASR::is_a<ASR::FunctionType_t>(*passed_elem)
            && !ASRUtils::is_class_type(passed_elem);
        bool compatible_types = both_numeric || same_type_class;
        bool both_are_arrays = ASRUtils::is_array(passed_type) && ASRUtils::is_array(param_type);
        bool both_are_integers = ASR::is_a<ASR::Integer_t>(*passed_elem) &&
                                 ASR::is_a<ASR::Integer_t>(*param_elem);
        bool different_kinds = ASRUtils::extract_kind_from_ttype_t(passed_elem) !=
                               ASRUtils::extract_kind_from_ttype_t(param_elem);
        if (compatible_types && !(both_are_arrays && both_are_integers && different_kinds)) {
            skip_check = true;
        }
    }
    // Check if types are equal
    if (!skip_check && (ASRUtils::is_unlimited_polymorphic_to_specific_type(param_type, passed_type) ||
            !ASRUtils::check_equal_type(passed_type, param_type, passed_arg, dummy))) {
        std::string passed_type_str = ASRUtils::type_to_str_with_kind(passed_type, passed_arg);
        std::string param_type_str = ASRUtils::type_to_str_with_kind(param_type, dummy);
        diag.add(diag::Diagnostic(
            "Type mismatch in argument `" + std::string(v->m_name) +
            "`: expected `" + param_type_str + "` but got `" +passed_type_str + "`",
            diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {passed_arg->base.loc})
            }));
        throw SemanticAbort();
    }
}

void CommonVisitorBase::check_actuals_against_external_definition(const Location &loc,
        ASR::symbol_t* source, Vec<ASR::call_arg_t> &args, bool is_function) {
    if (!ASRUtils::is_bare_implicit_interface(source)) {
        return;
    }
    auto definition = external_procedure_definitions.find(source);
    if (definition == external_procedure_definitions.end()) {
        return;
    }
    ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(definition->second);
    validate_function_call_argument_count(loc, args, fn);
    // The interface built from the actuals has no dummy for an absent
    // optional argument, so the definition would not receive it.
    for (size_t i = args.size(); i < fn->n_args; i++) {
        ASR::symbol_t* dummy = ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v;
        if (ASR::is_a<ASR::Variable_t>(*dummy)) {
            std::string dummy_name = ASRUtils::symbol_name(dummy);
            diag.add(Diagnostic("the external procedure '" + std::string(fn->m_name)
                + "' is referenced through an implicit interface, so its optional "
                "dummy argument '" + dummy_name + "' cannot be absent",
                Level::Error, Stage::Semantic, {Label("help: pass an actual "
                    "argument for '" + dummy_name + "'", {loc})}));
            throw SemanticAbort();
        }
    }
    if (is_function) {
        validate_create_function_arguments(args, definition->second);
        return;
    }
    for (size_t i = 0; i < args.size(); i++) {
        if (args[i].m_value != nullptr && ASR::is_a<ASR::Variable_t>(
                *ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v)) {
            check_call_actual_type(args[i].m_value, fn->m_args[i], false);
        }
    }
}

bool CommonVisitorBase::is_mismatched_external_procedure_call(ASR::symbol_t* proc,
        const Vec<ASR::call_arg_t> &args) {
    auto definition = external_procedure_definitions.find(proc);
    if (definition != external_procedure_definitions.end()) {
        proc = definition->second;
    }
    if (!compiler_options.implicit_argument_casting
            || !ASR::is_a<ASR::Function_t>(*proc)
            || ASRUtils::symbol_parent_symtab(proc)->parent != nullptr) {
        return false;
    }
    ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(proc);
    ASR::FunctionType_t* fn_type = ASRUtils::get_FunctionType(fn);
    if (fn_type->m_abi != ASR::abiType::Source
            || fn_type->m_deftype != ASR::deftypeType::Implementation) {
        return false;
    }
    auto is_numeric = [](ASR::ttype_t* t) {
        return ASR::is_a<ASR::Integer_t>(*t) || ASR::is_a<ASR::Real_t>(*t) ||
            ASR::is_a<ASR::Complex_t>(*t);
    };
    for (size_t i = 0; i < args.size() && i < fn->n_args; i++) {
        if (args[i].m_value == nullptr ||
                !ASR::is_a<ASR::Variable_t>(*ASR::down_cast<ASR::Var_t>(
                    fn->m_args[i])->m_v)) {
            continue;
        }
        ASR::ttype_t* actual_elem = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(args[i].m_value)));
        ASR::ttype_t* dummy_elem = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(fn->m_args[i])));
        if (is_numeric(actual_elem) && is_numeric(dummy_elem)
                && !(ASR::is_a<ASR::Integer_t>(*actual_elem)
                    && ASR::is_a<ASR::Integer_t>(*dummy_elem))
                && (actual_elem->type != dummy_elem->type
                    || ASRUtils::extract_kind_from_ttype_t(actual_elem)
                        != ASRUtils::extract_kind_from_ttype_t(dummy_elem))) {
            return true;
        }
    }
    return false;
}

ASR::symbol_t* CommonVisitorBase::get_cast_interface(ASR::symbol_t* decl, ASR::FunctionType_t* formal) {
    if (decl == nullptr) {
        return nullptr;
    }
    ASR::symbol_t* decl_fn = ASRUtils::symbol_get_past_external(decl);
    if (!ASR::is_a<ASR::Function_t>(*decl_fn)) {
        return nullptr;
    }
    ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(decl_fn);
    if (ASRUtils::is_bare_implicit_interface(*fn) || fn->n_args != formal->n_arg_types) {
        return nullptr;
    }
    if (ASRUtils::is_visible_from(decl, current_scope)) {
        return decl;
    }
    SymbolTable* sym_scope = implicit_interface_scope();
    ASR::symbol_t*& copy = cast_interface_copies[sym_scope][decl_fn];
    if (copy != nullptr) {
        return copy;
    }
    std::string name = sym_scope->get_unique_name(
        generated_symbol_name(fn->m_name, "cast_interface"), false);
    ASR::symbol_t* dup = make_interface_copy(fn, sym_scope, name);
    if (dup == nullptr) {
        return nullptr;
    }
    copy = dup;
    return copy;
}

ASR::expr_t* CommonVisitorBase::cast_procedure(ASR::expr_t* actual, ASR::FunctionType_t* formal,
        ASR::symbol_t* formal_decl) {
    ASR::ttype_t* actual_type = ASRUtils::type_get_past_allocatable_pointer(
        ASRUtils::expr_type(actual));
    if (!ASR::is_a<ASR::FunctionType_t>(*actual_type)) {
        return actual;
    }
    ASR::FunctionType_t* actual_ft = ASR::down_cast<ASR::FunctionType_t>(actual_type);
    if (ASRUtils::procedure_types_identical(actual_ft, formal)) {
        return actual;
    }
    const Location &loc = actual->base.loc;
    if (ASRUtils::is_bare_implicit_interface(*formal)) {
        return ASRUtils::EXPR(ASR::make_FunctionPointerCast_t(al, loc, actual,
            nullptr, ASRUtils::duplicate_type(al, &formal->base), nullptr));
    }
    if (!ASRUtils::is_bare_implicit_interface(*actual_ft)) {
        return actual;
    }
    ASR::symbol_t* to = get_cast_interface(formal_decl, formal);
    if (to == nullptr) {
        ASR::ttype_t* opaque = ASRUtils::make_opaque_procedure_type(al, loc,
            formal->m_return_var_type);
        if (ASRUtils::procedure_types_identical(actual_ft,
                ASR::down_cast<ASR::FunctionType_t>(opaque))) {
            return actual;
        }
        return ASRUtils::EXPR(ASR::make_FunctionPointerCast_t(al, loc, actual,
            nullptr, opaque, nullptr));
    }
    return ASRUtils::EXPR(ASR::make_FunctionPointerCast_t(al, loc, actual,
        to, ASRUtils::duplicate_type(al, &formal->base), nullptr));
}

ASR::expr_t* CommonVisitorBase::named_constant_of_data_ref(ASR::expr_t* data_ref) {
    while (true) {
        if (ASR::is_a<ASR::StructInstanceMember_t>(*data_ref)) {
            data_ref = ASR::down_cast<ASR::StructInstanceMember_t>(data_ref)->m_v;
        } else if (ASR::is_a<ASR::ArrayItem_t>(*data_ref)) {
            data_ref = ASR::down_cast<ASR::ArrayItem_t>(data_ref)->m_v;
        } else {
            break;
        }
    }
    return ASRUtils::is_const(data_ref) ? data_ref : nullptr;
}

bool CommonVisitorBase::check_procedure_component_of_constant(ASR::expr_t* data_ref,
        ASR::symbol_t* component, const Location& loc,
        const std::string& what) {
    if (data_ref == nullptr || component == nullptr) {
        return false;
    }
    component = ASRUtils::symbol_get_past_external(component);
    if (!ASR::is_a<ASR::Variable_t>(*component) ||
            !ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_pointer(
                ASR::down_cast<ASR::Variable_t>(component)->m_type))) {
        return false;
    }
    ASR::expr_t* constant = named_constant_of_data_ref(data_ref);
    if (constant == nullptr) {
        return false;
    }
    diag.add(Diagnostic(
        "a procedure pointer component of a named constant cannot be "
        + what,
        Level::Error, Stage::Semantic, {
            Label("`" + std::string(ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(constant)->m_v))
                + "` is a named constant, not a variable",
                {loc})}));
    if (!compiler_options.continue_compilation) throw SemanticAbort();
    return true;
}

bool CommonVisitorBase::check_procedure_component_of_constant(ASR::expr_t* expr,
        const std::string& what) {
    if (expr == nullptr || !ASR::is_a<ASR::StructInstanceMember_t>(*expr)) {
        return false;
    }
    ASR::StructInstanceMember_t* member =
        ASR::down_cast<ASR::StructInstanceMember_t>(expr);
    return check_procedure_component_of_constant(member->m_v, member->m_m,
        expr->base.loc, what);
}

ASR::expr_t* CommonVisitorBase::cast_procedure_actual(ASR::expr_t* actual, ASR::expr_t* dummy) {
    if (actual == nullptr || dummy == nullptr || !ASR::is_a<ASR::Var_t>(*dummy)) {
        return actual;
    }
    ASR::symbol_t* dummy_sym = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(dummy)->m_v);
    if (ASR::is_a<ASR::Function_t>(*dummy_sym)) {
        check_procedure_component_of_constant(actual, "an actual argument");
        return cast_procedure(actual, ASRUtils::get_FunctionType(
            ASR::down_cast<ASR::Function_t>(dummy_sym)), dummy_sym);
    }
    if (!ASR::is_a<ASR::Variable_t>(*dummy_sym)) {
        return actual;
    }
    ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(dummy_sym);
    if (ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_pointer(v->m_type))) {
        check_procedure_component_of_constant(actual, "an actual argument");
    }
    // A pointer dummy is associated with the pointer itself.
    if (ASRUtils::is_pointer(v->m_type) ||
            !ASR::is_a<ASR::FunctionType_t>(*v->m_type)) {
        return actual;
    }
    return cast_procedure(actual, ASR::down_cast<ASR::FunctionType_t>(v->m_type),
        v->m_type_declaration);
}

ASR::symbol_t* CommonVisitorBase::declare_implied_do_variable(const std::string &name,
        const Location &loc) {
    ASR::symbol_t* sym = current_scope->resolve_symbol(name);
    if (sym == nullptr
            || !ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(sym))) {
        return nullptr;
    }
    ASR::expr_t* var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
    check_implied_do_loop_variable(var, name, loc);
    ASR::ttype_t* type = ASRUtils::type_get_past_allocatable_pointer(
        ASRUtils::expr_type(var));
    return declare_implicit_variable2(loc,
        current_scope->get_unique_name("lfortran_implied_do_" + name),
        ASRUtils::intent_local, type);
}

void CommonVisitorBase::check_implied_do_loop_variable(ASR::expr_t* a_var, const std::string &name,
    const Location &loc)
{
    ASR::ttype_t* var_type = ASRUtils::expr_type(a_var);
    if (ASRUtils::is_integer(*ASRUtils::type_get_past_allocatable_pointer(var_type))
            && !ASRUtils::is_array(var_type)) {
        return;
    }
    diag.add(Diagnostic("The implied do loop variable '" + name
        + "' must be a scalar integer, not "
        + ASRUtils::type_to_str_with_kind(var_type, a_var),
        Level::Error, Stage::Semantic, {Label("", {loc})}));
    throw SemanticAbort();
}

bool CommonVisitorBase::implied_do_value_types_equal(ASR::ttype_t* type, ASR::expr_t* expr,
        ASR::ttype_t* first_type, ASR::expr_t* first_expr) {
    if (!ASRUtils::types_equal(type, first_type, expr, first_expr)) {
        return false;
    }
    if (!ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(type))) {
        return true;
    }
    ASR::symbol_t* struct_sym = ASRUtils::get_struct_sym_from_struct_expr(expr);
    ASR::symbol_t* first_struct_sym = ASRUtils::get_struct_sym_from_struct_expr(first_expr);
    if (struct_sym == nullptr || first_struct_sym == nullptr) {
        return true;
    }
    return ASRUtils::symbol_get_past_external(struct_sym)
        == ASRUtils::symbol_get_past_external(first_struct_sym);
}

bool CommonVisitorBase::implied_do_loop_has_scalar_values(ASR::ImpliedDoLoop_t* idl) {
    for (size_t i = 0; i < idl->n_values; i++) {
        ASR::expr_t* value = idl->m_values[i];
        if (ASR::is_a<ASR::ImpliedDoLoop_t>(*value)) {
            if (!implied_do_loop_has_scalar_values(
                    ASR::down_cast<ASR::ImpliedDoLoop_t>(value))) {
                return false;
            }
        } else if (ASRUtils::is_array(ASRUtils::expr_type(value))) {
            return false;
        }
    }
    return true;
}

void CommonVisitorBase::fetch_implied_do_loop_variables(ASR::ImpliedDoLoop_t* idl, std::vector<ASR::symbol_t*>& loop_vars) {
    ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(idl->m_var);
    loop_vars.push_back(var->m_v);
    for (size_t i = 0; i < idl->n_values; i++) {
        ASR::expr_t* expr = idl->m_values[i];
        if (ASR::is_a<ASR::ImpliedDoLoop_t>(*expr)) {
            fetch_implied_do_loop_variables(ASR::down_cast<ASR::ImpliedDoLoop_t>(expr), loop_vars);
        }
    }
    return;
}

ASR::expr_t* CommonVisitorBase::get_implied_do_struct_constant(ASR::expr_t* expr,
        ImpliedDoLoopValuesVisitor& visitor) {
    ASR::expr_t* value = ASRUtils::expr_value(expr);
    if (value && ASR::is_a<ASR::StructConstant_t>(*value)) {
        return value;
    }
    if (!ASR::is_a<ASR::StructConstructor_t>(*expr)) {
        return nullptr;
    }
    ASR::StructConstructor_t* sc = ASR::down_cast<ASR::StructConstructor_t>(expr);
    auto evaluate = [&](ASR::expr_t* x) -> ASR::expr_t* {
        if (ASRUtils::is_value_constant(ASRUtils::expr_value(x))) {
            return ASRUtils::expr_value(x);
        }
        return visitor.evaluate_constant(*x);
    };
    Vec<ASR::call_arg_t> args;
    args.reserve(al, sc->n_args);
    for (size_t i = 0; i < sc->n_args; i++) {
        ASR::call_arg_t arg = sc->m_args[i];
        ASR::expr_t* arg_value = arg.m_value;
        if (arg_value == nullptr
                || ASR::is_a<ASR::PointerNullConstant_t>(*arg_value)) {
            args.push_back(al, arg);
            continue;
        }
        if (ASR::is_a<ASR::StructType_t>(*ASRUtils::expr_type(arg_value))) {
            arg.m_value = get_implied_do_struct_constant(arg_value, visitor);
        } else if (ASR::is_a<ASR::ArrayConstructor_t>(*arg_value)
                && !ASRUtils::is_value_constant(ASRUtils::expr_value(arg_value))) {
            // `t([i, -i])` for an array component
            ASR::ArrayConstructor_t* ac =
                ASR::down_cast<ASR::ArrayConstructor_t>(arg_value);
            Vec<ASR::expr_t*> elements;
            elements.reserve(al, ac->n_args);
            for (size_t j = 0; j < ac->n_args; j++) {
                ASR::expr_t* element = ASRUtils::is_array(
                    ASRUtils::expr_type(ac->m_args[j]))
                    ? nullptr : evaluate(ac->m_args[j]);
                if (element == nullptr
                        || !ASRUtils::is_value_constant(element)) {
                    return nullptr;
                }
                elements.push_back(al, element);
            }
            arg.m_value = ASRUtils::expr_value(ASRUtils::EXPR(
                ASRUtils::make_ArrayConstructor_t_util(al, arg_value->base.loc,
                    elements.p, elements.size(), ac->m_type,
                    ac->m_storage_format)));
        } else {
            arg.m_value = evaluate(arg_value);
        }
        if (arg.m_value == nullptr
                || !ASRUtils::is_value_constant(arg.m_value)) {
            return nullptr;
        }
        args.push_back(al, arg);
    }
    // The values are checked against the components' types
    return get_folded_struct_constant(expr->base.loc, sc->m_dt_sym,
        args.p, args.size(), sc->m_type);
}

void CommonVisitorBase::check_shifta_args(Vec<ASR::expr_t*> &args, const Location &loc) {
    ASR::ttype_t* base_i = ASRUtils::type_get_past_array(
        ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(args[0])));
    ASR::ttype_t* base_shift = ASRUtils::type_get_past_array(
        ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(args[1])));
    if (!ASRUtils::is_integer(*base_i) || !ASRUtils::is_integer(*base_shift)) {
        diag.semantic_error_label(
            "Arguments to the 'shifta' intrinsic must be of type INTEGER",
            { loc },
            "help: check the variable types passed to shifta"
        );
        throw SemanticAbort();
    }
    int64_t bit_size = 8 * ASRUtils::extract_kind_from_ttype_t(base_i);
    auto check_shift = [&](ASR::expr_t* shift_value) {
        if (!shift_value || !ASR::is_a<ASR::IntegerConstant_t>(*shift_value)) {
            return;
        }
        int64_t shift = ASR::down_cast<ASR::IntegerConstant_t>(shift_value)->m_n;
        if (shift < 0 || shift > bit_size) {
            diag.semantic_error_label(
                "the shift argument of 'shifta' intrinsic must be non-negative "
                "and less than or equal to the bit size of the integer",
                { args[1]->base.loc },
                "shift value is " + std::to_string(shift)
                    + ", but bit size of integer is " + std::to_string(bit_size));
            throw SemanticAbort();
        }
    };
    ASR::expr_t* shift_value = ASRUtils::expr_value(args[1]);
    if (shift_value && ASR::is_a<ASR::ArrayConstant_t>(*shift_value)) {
        ASR::ArrayConstant_t* shifts = ASR::down_cast<ASR::ArrayConstant_t>(shift_value);
        size_t n = ASRUtils::get_constant_ArrayConstant_size(shifts);
        for (size_t i = 0; i < n; i++) {
            check_shift(ASRUtils::fetch_ArrayConstant_value(al, shifts, i));
        }
    } else {
        check_shift(shift_value);
    }
}

void CommonVisitorBase::validate_coarray_bounds(ASR::Variable_t* var, const std::string& name,
                              const Vec<ASR::coarray_index_t>& coindices) {
    if (!var || var->n_codims == 0) {
        return;
    }
    size_t n = std::min(var->n_codims, coindices.n);
    for (size_t i = 0; i < n; i++) {
        ASR::codimension_t &codim = var->m_codims[i];
        if (codim.m_end_star == ASR::codimension_typeType::CodimensionStar) {
            continue;
        }
        ASR::expr_t* idx = coindices[i].m_index;
        if (!idx) {
            continue;
        }
        ASR::expr_t* idx_val = ASRUtils::expr_value(idx);
        if (!idx_val || !ASR::is_a<ASR::IntegerConstant_t>(*idx_val)) {
            continue;
        }
        int64_t idx_n = ASR::down_cast<ASR::IntegerConstant_t>(idx_val)->m_n;
        int64_t lb = 1;
        int64_t ub = 0;
        if (codim.m_start) {
            ASR::expr_t* lb_val = ASRUtils::expr_value(codim.m_start);
            if (!lb_val || !ASR::is_a<ASR::IntegerConstant_t>(*lb_val)) {
                continue;
            }
            lb = ASR::down_cast<ASR::IntegerConstant_t>(lb_val)->m_n;
        }
        if (codim.m_end) {
            ASR::expr_t* ub_val = ASRUtils::expr_value(codim.m_end);
            if (!ub_val || !ASR::is_a<ASR::IntegerConstant_t>(*ub_val)) {
                continue;
            }
            ub = ASR::down_cast<ASR::IntegerConstant_t>(ub_val)->m_n;
        } else {
            continue;
        }
        if (idx_n < lb || idx_n > ub) {
            diag.add(Diagnostic(
                "Coarray '" + name + "' coindex " + std::to_string(i + 1) +
                " is out of bounds: " + std::to_string(idx_n) +
                " not in [" + std::to_string(lb) + ", " + std::to_string(ub) + "]",
                Level::Error, Stage::Semantic, {Label("", {coindices[i].loc})}));
            throw SemanticAbort();
        }
    }
}

} // namespace LCompilers::LFortran

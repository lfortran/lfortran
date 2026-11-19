// Member functions of CommonVisitorBase, in the order of their declarations
// (split over ast_common_visitor_base*.cpp to compile in parallel).

#include <lfortran/semantics/ast_common_visitor.h>

namespace LCompilers::LFortran {

void CommonVisitorBase::check_two_argument_intrinsic_args(const std::string& intrinsic_name,
        const Vec<ASR::expr_t*>& args, const Location& loc) {
    LCOMPILERS_ASSERT(args.size() == 2 && args[0] && args[1]);
    ASR::ttype_t* y_type = ASRUtils::expr_type(args[0]);
    ASR::ttype_t* x_type = ASRUtils::expr_type(args[1]);
    if (!ASRUtils::is_real(*y_type) || !ASRUtils::is_real(*x_type)) {
        diag.add(Diagnostic("arguments 'y' and 'x' of the '" +
            intrinsic_name + "' intrinsic must be real, found " +
            ASRUtils::type_to_str_with_kind(y_type, args[0]) + " and " +
            ASRUtils::type_to_str_with_kind(x_type, args[1]),
            Level::Error, Stage::Semantic, {Label("", {loc})}));
        throw SemanticAbort();
    }
    if (ASRUtils::extract_kind_from_ttype_t(y_type) !=
            ASRUtils::extract_kind_from_ttype_t(x_type)) {
        diag.add(Diagnostic("arguments 'y' and 'x' of the '" +
            intrinsic_name + "' intrinsic must have the same kind, found " +
            ASRUtils::type_to_str_with_kind(
                ASRUtils::extract_type(y_type), args[0]) + " and " +
            ASRUtils::type_to_str_with_kind(
                ASRUtils::extract_type(x_type), args[1]),
            Level::Error, Stage::Semantic, {Label("", {loc})}));
        throw SemanticAbort();
    }
}

bool CommonVisitorBase::has_implicit_none_external(AST::decl_stmt_t **items,
        size_t n_items) {
    for (size_t i = 0; i < n_items; i++) {
        if (!AST::is_a<AST::ImplicitNone_t>(*items[i])) continue;
        AST::ImplicitNone_t *none = AST::down_cast<AST::ImplicitNone_t>(items[i]);
        for (size_t j = 0; j < none->n_specs; j++) {
            if (AST::is_a<AST::ImplicitNoneExternal_t>(*none->m_specs[j])) {
                return true;
            }
        }
    }
    return false;
}

void CommonVisitorBase::check_implicit_none_external(const std::string &name,
        ASR::symbol_t *sym, const Location &loc) {
    if (implicit_none_external_stack.empty()
            || !implicit_none_external_stack.back()) {
        return;
    }
    if (sym && ASRUtils::symbol_parent_symtab(sym)->parent != nullptr) {
        return;
    }
    for (SymbolTable *s = current_scope; s; s = s->parent) {
        if (sym && s->asr_owner == &sym->base) {
            return;
        }
        if (s->asr_owner && ASR::is_a<ASR::symbol_t>(*s->asr_owner)) {
            // A procedure with ENTRY statements is implemented by a
            // master function named after it.
            std::string owner = to_lower(ASRUtils::symbol_name(
                ASR::down_cast<ASR::symbol_t>(s->asr_owner)));
            owner = owner.substr(0, owner.find("_main__lcompilers"));
            auto entries = entry_functions.find(owner);
            if (entries != entry_functions.end() && (name == owner
                    || entries->second.count(name) > 0)) {
                return;
            }
        }
    }
    diag.add(diag::Diagnostic(
        "procedure '" + name + "' is not explicitly declared",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("help: declare it with the EXTERNAL attribute "
                "or give it an explicit interface, as required by "
                "'implicit none (external)'", {loc})}));
    throw SemanticAbort();
}

bool CommonVisitorBase::is_pending_generic_procedure(const std::string &name,
        SymbolTable *scope, bool enclosing) {
    auto it = generic_procedures.find(name);
    if (it == generic_procedures.end()) {
        return false;
    }
    for (auto &specific : it->second) {
        for (SymbolTable *s = scope; s != nullptr; s = s->parent) {
            if (s == specific.scope) {
                return true;
            }
            if (!enclosing) {
                break;
            }
        }
    }
    return false;
}

const ASR::DoLoop_t* CommonVisitorBase::forget_ended_do_loop(ASR::symbol_t *v) {
    for (auto it = ended_do_loops.begin(); it != ended_do_loops.end(); ++it) {
        const ASR::DoLoop_t *loop = it->first;
        if (ASR::down_cast<ASR::Var_t>(loop->m_head.m_v)->m_v == v) {
            ended_do_loops.erase(it);
            return loop;
        }
    }
    return nullptr;
}

CommonVisitorBase::CommonVisitorBase(Allocator &al, SymbolTable *symbol_table,
    diag::Diagnostics &diagnostics, CompilerOptions &compiler_options,
    std::map<uint64_t, std::map<std::string, ASR::ttype_t*>> &implicit_mapping,
    std::map<uint64_t, std::vector<std::string>>& external_procedures_mapping,
    std::map<uint32_t, std::set<std::string>> &procedure_references,
    std::map<uint64_t, std::vector<std::string>>& explicit_intrinsic_procedures_mapping,
    std::map<uint32_t, std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>>> &instantiate_types,
    std::map<uint32_t, std::map<std::string, ASR::symbol_t*>> &instantiate_symbols,
    std::map<std::string, std::map<std::string, std::vector<AST::decl_stmt_t*>>> &entry_functions,
    std::map<std::string, std::vector<int>> &entry_function_arguments_mapping,
    std::map<uint32_t, std::vector<ASR::stmt_t*>> &data_structure,
    std::set<ASR::symbol_t*> &getter_functions,
        LCompilers::LocationManager &lm
): diag{diagnostics}, al{al}, compiler_options{compiler_options},
      current_scope{symbol_table}, implicit_mapping{implicit_mapping},
      external_procedures_mapping{external_procedures_mapping},
      explicit_intrinsic_procedures_mapping{explicit_intrinsic_procedures_mapping},
      procedure_references{procedure_references},
      entry_functions{entry_functions},entry_function_arguments_mapping{entry_function_arguments_mapping},
      current_variable_type_{nullptr}, instantiate_types{instantiate_types},
      instantiate_symbols{instantiate_symbols}, data_structure{data_structure},
      getter_functions{getter_functions}, lm{lm}
{
    current_module_dependencies.reserve(al, 4);
    enum_init_val = 0;
}

ASR::symbol_t* CommonVisitorBase::resolve_symbol(const Location &loc, const std::string &sub_name) {
    SymbolTable *scope = current_scope;
    ASR::symbol_t *sub = scope->resolve_symbol(sub_name);
    if (!sub) {
        diag.add(Diagnostic(
            "Symbol '" + sub_name + "' not declared",
            Level::Error, Stage::Semantic, {
                Label("",{loc})
            }));
        throw SemanticAbort();
    }
    return sub;
}

bool CommonVisitorBase::is_omp_threadprivate_directive(const std::string &text) {
    const std::string directive = "threadprivate";
    std::string trimmed = omp_directive_body(text);
    std::string lower_text = LCompilers::to_lower(trimmed);
    if (!LCompilers::startswith(lower_text, directive)) {
        return false;
    }
    if (lower_text.size() == directive.size()) {
        return true;
    }
    char next = lower_text[directive.size()];
    return next == '(' || next == ' ' || next == '\t';
}

void CommonVisitorBase::parse_omp_threadprivate_vars(const std::string &text,
        const Location &loc, std::vector<std::string> &names) {
    const std::string directive = "threadprivate";
    std::string trimmed = omp_directive_body(text);
    size_t pos = directive.size();
    while (pos < trimmed.size() &&
            (trimmed[pos] == ' ' || trimmed[pos] == '\t')) {
        pos++;
    }
    if (pos >= trimmed.size() || trimmed[pos] != '(') {
        diag.add(Diagnostic(
            "malformed `!$omp threadprivate(...)` directive: "
            "variable list missing or empty",
            Level::Error, Stage::Semantic, { Label("", {loc}) }));
        throw SemanticAbort();
    }
    size_t lp = pos;
    size_t rp = trimmed.rfind(')');
    if (rp == std::string::npos || rp <= lp + 1) {
        diag.add(Diagnostic(
            "malformed `!$omp threadprivate(...)` directive: "
            "variable list missing or empty",
            Level::Error, Stage::Semantic, { Label("", {loc}) }));
        throw SemanticAbort();
    }
    if (trim_omp_directive_part(trimmed.substr(rp + 1)) != "") {
        diag.add(Diagnostic(
            "malformed `!$omp threadprivate(...)` directive: "
            "unexpected text after variable list",
            Level::Error, Stage::Semantic, { Label("", {loc}) }));
        throw SemanticAbort();
    }
    std::string inner = trimmed.substr(lp + 1, rp - lp - 1);
    size_t start = 0;
    while (start <= inner.size()) {
        size_t comma = inner.find(',', start);
        size_t len = (comma == std::string::npos)
            ? std::string::npos : comma - start;
        std::string name = inner.substr(start, len);
        name = trim_omp_directive_part(name);
        if (name == "") {
            diag.add(Diagnostic(
                "malformed `!$omp threadprivate(...)` directive: "
                "empty variable name in list",
                Level::Error, Stage::Semantic, { Label("", {loc}) }));
            throw SemanticAbort();
        }
        if (name.find('/') != std::string::npos) {
            diag.add(Diagnostic(
                "`!$omp threadprivate(...)` common blocks are not "
                "supported yet",
                Level::Error, Stage::Semantic, { Label("", {loc}) }));
            throw SemanticAbort();
        }
        names.push_back(LCompilers::to_lower(name));
        if (comma == std::string::npos) break;
        start = comma + 1;
    }
}

void CommonVisitorBase::mark_omp_threadprivate_vars(const std::string &text, const Location &loc) {
    std::vector<std::string> names;
    parse_omp_threadprivate_vars(text, loc, names);
    for (const std::string &name: names) {
        ASR::symbol_t *sym = current_scope->resolve_symbol(name);
        if (!sym) {
            diag.add(Diagnostic(
                "symbol `" + name + "` named in `!$omp threadprivate(...)` "
                "is not declared in this scope",
                Level::Error, Stage::Semantic, { Label("", {loc}) }));
            throw SemanticAbort();
        }
        ASR::symbol_t *resolved = ASRUtils::symbol_get_past_external(sym);
        if (!ASR::is_a<ASR::Variable_t>(*resolved)) {
            diag.add(Diagnostic(
                "symbol `" + name + "` named in `!$omp threadprivate(...)` "
                "is not a variable",
                Level::Error, Stage::Semantic, { Label("", {loc}) }));
            throw SemanticAbort();
        }
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(resolved);
        if (var->m_storage == ASR::storage_typeType::Parameter) {
            diag.add(Diagnostic(
                "symbol `" + name + "` named in `!$omp threadprivate(...)` "
                "cannot have the parameter attribute",
                Level::Error, Stage::Semantic, { Label("", {loc}) }));
            throw SemanticAbort();
        }
        ASR::symbol_t *owner = ASRUtils::get_asr_owner(resolved);
        if (owner == nullptr || !ASR::is_a<ASR::Module_t>(*owner)) {
            diag.add(Diagnostic(
                "symbol `" + name + "` named in `!$omp threadprivate(...)` "
                "must be a module variable",
                Level::Error, Stage::Semantic, { Label("", {loc}) }));
            throw SemanticAbort();
        }
        if (var->m_common_block != nullptr) {
            diag.add(Diagnostic(
                "`!$omp threadprivate(...)` common block variables are "
                "not supported yet",
                Level::Error, Stage::Semantic, { Label("", {loc}) }));
            throw SemanticAbort();
        }
        var->m_storage = ASR::storage_typeType::Threadprivate;
    }
}

ASR::symbol_t* CommonVisitorBase::declare_implicit_variable(const Location &loc,
        const std::string &var_name, ASR::intentType intent, ASR::expr_t* value) {
    ASR::ttype_t *type = nullptr;
    char first_letter = var_name[0];
    // The default implicit typing is:
    // implicit real (a-h,o-z)
    if (first_letter >= 'i' && first_letter <= 'n') {
        // it is an integer
        type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, compiler_options.po.default_integer_kind));
    } else {
        // it is a real
        type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 4));
    }
    ASR::symbol_t* type_decl = nullptr;
    if ( value != nullptr ) {
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *value_type = ASRUtils::expr_type(value);
        value = b.t2t(value, value_type, type);
        type_decl = ASRUtils::get_struct_sym_from_struct_expr(value);
    }
    SetChar variable_dependencies_vec;
    variable_dependencies_vec.reserve(al, 1);
    ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type);
    ASR::symbol_t *v = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(al, loc,
        current_scope, s2c(al, var_name), variable_dependencies_vec.p,
        variable_dependencies_vec.size(), intent, value, value != nullptr ? ASRUtils::expr_value(value) : value,
        ASR::storage_typeType::Default, type, type_decl,
        current_procedure_abi_type, ASR::Public,
        ASR::presenceType::Required, false));
    current_scope->add_symbol(var_name, v);
    return v;
}

ASR::symbol_t* CommonVisitorBase::declare_implicit_variable2(const Location &loc,
        const std::string &var_name, ASR::intentType intent,
        ASR::ttype_t *type, ASR::expr_t* value) {
    ASR::symbol_t* type_decl = nullptr;
    if ( value != nullptr ) {
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *value_type = ASRUtils::expr_type(value);
        value = b.t2t(value, value_type, type);
        type_decl = ASRUtils::get_struct_sym_from_struct_expr(value);
    }
    SetChar variable_dependencies_vec;
    variable_dependencies_vec.reserve(al, 1);
    ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type, nullptr, value);
    ASR::symbol_t *v = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(al, loc,
        current_scope, s2c(al, var_name), variable_dependencies_vec.p,
        variable_dependencies_vec.size(), intent, value, value != nullptr ? ASRUtils::expr_value(value) : value,
        ASR::storage_typeType::Default, type, type_decl,
        current_procedure_abi_type, ASR::Public,
        ASR::presenceType::Required, false));
    current_scope->add_symbol(var_name, v);
    return v;
}

void CommonVisitorBase::check_namelist_object_type_confirmed(ASR::Variable_t *object,
        ASR::ttype_t *declared_type, ASR::symbol_t *declared_type_decl,
        const Location &loc) {
    ASR::ttype_t *implied = ASRUtils::extract_type(object->m_type);
    ASR::ttype_t *declared = ASRUtils::extract_type(declared_type);
    if (implied->type == declared->type
            && ASRUtils::types_equal(implied, declared, nullptr, nullptr)) {
        return;
    }
    auto type_name = [](ASR::ttype_t *t, ASR::symbol_t *type_decl) {
        if (ASR::is_a<ASR::StructType_t>(*t) && type_decl != nullptr) {
            return "type(" + std::string(ASRUtils::symbol_name(
                ASRUtils::symbol_get_past_external(type_decl))) + ")";
        }
        return ASRUtils::type_to_str_fortran_symbol(t, type_decl, true);
    };
    diag.add(Diagnostic("namelist object '" + std::string(object->m_name)
        + "' was implicitly typed " + type_name(implied,
            object->m_type_declaration)
        + " at the namelist statement; its declaration as "
        + type_name(declared, declared_type_decl)
        + " does not confirm that type",
        Level::Error, Stage::Semantic, {Label("", {loc})}));
    throw SemanticAbort();
}

void CommonVisitorBase::reject_null_initializer_if_not_pointer_or_allocatable(
        const std::string& name, ASR::ttype_t* type,
        ASR::symbol_t* type_declaration, bool is_pointer,
        bool is_allocatable, const Location& loc) {
    if (is_pointer || is_allocatable) {
        return;
    }
    std::string init_type_name;
    if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(type))
            && type_declaration != nullptr) {
        init_type_name = "type(" + std::string(
            ASRUtils::symbol_name(type_declaration)) + ")";
    } else {
        init_type_name = ASRUtils::type_to_str_fortran_symbol(
            ASRUtils::extract_type(type), type_declaration, true);
    }
    diag.add(Diagnostic("null() cannot initialize '"
        + name + "' of type " + init_type_name
        + ", which is neither a pointer nor allocatable",
        Level::Error, Stage::Semantic, {Label("", {loc})}));
    throw SemanticAbort();
}

void CommonVisitorBase::check_hidden_derived_type_component_reference(const Location &loc,
        const std::string &var_name) {
    if (hidden_derived_type_components &&
            hidden_derived_type_components->find(var_name)
                != hidden_derived_type_components->end()) {
        diag.semantic_error_label("component `" + var_name
            + "` cannot be referenced in a component declaration", {loc},
            "`" + var_name + "` is a component of the derived type being defined");
        throw SemanticAbort();
    }
}

ASR::asr_t* CommonVisitorBase::resolve_variable(const Location &loc, const std::string &var_name) {
    SymbolTable *scope = current_scope;
    ASR::symbol_t *v = scope->resolve_symbol(var_name);
    if (v && is_inaccessible_external_procedure(v, var_name)) {
        v = nullptr;
    }
    if (compiler_options.implicit_typing) {
        if (!in_Subroutine) {
            if (implicit_mapping.size() != 0 && current_scope->asr_owner) {
                auto it = implicit_mapping.find(get_hash(current_scope->asr_owner));
                if (it != implicit_mapping.end()) {
                    implicit_dictionary = it->second;
                }
                if (implicit_dictionary.size() == 0 && current_scope->parent
                        && current_scope->parent->asr_owner) {
                    implicit_dictionary = implicit_mapping[get_hash(
                        current_scope->parent->asr_owner)];
                }
                }
        }
    }

    if (ASRUtils::is_iso_c_null_symbol(current_scope, v)) {
        tmp = (ASR::asr_t*)ASRUtils::make_iso_c_null_constant(
            al, loc, current_scope, v);
        return tmp;
    }
    if (!v) {
        check_hidden_derived_type_component_reference(loc, var_name);
        if (check_is_explicit_intrinsic(var_name)) {
            std::string intrinsic_name = var_name;
            std::vector<std::string> arg_types;
            if (intrinsic_mapping.count(var_name) > 0) {
                intrinsic_name = intrinsic_mapping[var_name].first;
                arg_types = intrinsic_mapping[var_name].second;
            }
            if (intrinsic_procedures.is_intrinsic(intrinsic_name)) {
                v = resolve_intrinsic_function(loc, intrinsic_name);
            }
            if (!v) {
                if (ASRUtils::IntrinsicElementalFunctionRegistry::is_intrinsic_function(intrinsic_name) ||
                    ASRUtils::IntrinsicArrayFunctionRegistry::is_intrinsic_function(intrinsic_name)) {
                    SymbolTable *parent_scope = current_scope;
                    current_scope = al.make_new<SymbolTable>(parent_scope);
                    
                    ASR::ttype_t *arg_type = nullptr;
                    ASR::ttype_t *return_type = nullptr;
                    if (!arg_types.empty()) {
                        std::string type_str = arg_types[0];
                        if (type_str == "int4" || type_str == "int") {
                            arg_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
                            return_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
                        } else if (type_str == "real4" || type_str == "real") {
                            arg_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 4));
                            return_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 4));
                        } else if (type_str == "real8") {
                            arg_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 8));
                            return_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 8));
                        } else if (type_str == "complex4" || type_str == "complex") {
                            arg_type = ASRUtils::TYPE(ASR::make_Complex_t(al, loc, 4));
                            return_type = arg_type;
                        } else if (type_str == "complex8") {
                            arg_type = ASRUtils::TYPE(ASR::make_Complex_t(al, loc, 8));
                            return_type = arg_type;
                        }
                    }
                    if (!arg_type) {
                        arg_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, 4));
                        return_type = arg_type;
                    }
                    
                    std::string arg_name = "x";
                    SetChar arg_dependencies_vec;
                    arg_dependencies_vec.reserve(al, 1);
                    ASRUtils::collect_variable_dependencies(al, arg_dependencies_vec, arg_type);
                    ASR::asr_t *arg_var = ASRUtils::make_Variable_t_util(al, loc,
                        current_scope, s2c(al, arg_name), arg_dependencies_vec.p,
                        arg_dependencies_vec.size(), ASRUtils::intent_in,
                        nullptr, nullptr, ASR::storage_typeType::Default, arg_type, nullptr,
                        ASR::abiType::Source, ASR::Public, ASR::presenceType::Required,
                        false);
                    current_scope->add_symbol(arg_name, ASR::down_cast<ASR::symbol_t>(arg_var));
                    
                    std::string return_var_name = var_name + "_return_var_name";
                    SetChar return_dependencies_vec;
                    return_dependencies_vec.reserve(al, 1);
                    ASRUtils::collect_variable_dependencies(al, return_dependencies_vec, return_type);
                    ASR::asr_t *return_var = ASRUtils::make_Variable_t_util(al, loc,
                        current_scope, s2c(al, return_var_name), return_dependencies_vec.p,
                        return_dependencies_vec.size(), ASRUtils::intent_return_var,
                        nullptr, nullptr, ASR::storage_typeType::Default, return_type, nullptr,
                        ASR::abiType::Source, ASR::Public, ASR::presenceType::Required,
                        false);
                    current_scope->add_symbol(return_var_name, ASR::down_cast<ASR::symbol_t>(return_var));
                    ASR::expr_t *to_return = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        ASR::down_cast<ASR::symbol_t>(return_var)));
                    
                    Vec<ASR::expr_t*> func_args;
                    func_args.reserve(al, 1);
                    func_args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        ASR::down_cast<ASR::symbol_t>(arg_var))));
                    
                    Vec<ASR::expr_t*> intrinsic_call_args;
                    intrinsic_call_args.reserve(al, 1);
                    intrinsic_call_args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                        ASR::down_cast<ASR::symbol_t>(arg_var))));
                    
                    ASR::expr_t* intrinsic_call = nullptr;
                    if (ASRUtils::IntrinsicElementalFunctionRegistry::is_intrinsic_function(intrinsic_name)) {
                        ASRUtils::create_intrinsic_function create_func =
                            ASRUtils::IntrinsicElementalFunctionRegistry::get_create_function(intrinsic_name);
                        intrinsic_call = ASRUtils::EXPR(create_func(al, loc, intrinsic_call_args, diag));
                    } else if (ASRUtils::IntrinsicArrayFunctionRegistry::is_intrinsic_function(intrinsic_name)) {
                        ASRUtils::create_intrinsic_function create_func =
                            ASRUtils::IntrinsicArrayFunctionRegistry::get_create_function(intrinsic_name);
                        intrinsic_call = ASRUtils::EXPR(create_func(al, loc, intrinsic_call_args, diag));
                    }
                    
                    Vec<ASR::stmt_t*> body;
                    body.reserve(al, 1);
                    if (intrinsic_call) {
                        body.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
                            to_return, intrinsic_call, nullptr, false, false, false, false)));
                    }
                    
                    tmp = ASRUtils::make_Function_t_util(
                        al, loc,
                        current_scope,
                        s2c(al, var_name),
                        nullptr, 0,
                        func_args.p, func_args.size(),
                        body.p, body.size(),
                        to_return,
                        ASR::abiType::Source, ASR::accessType::Public, ASR::deftypeType::Implementation,
                        nullptr, false, false, false, false, false, nullptr, 0,
                        false, false, false);
                    parent_scope->add_symbol(var_name, ASR::down_cast<ASR::symbol_t>(tmp));
                    current_scope = parent_scope;
                    v = ASR::down_cast<ASR::symbol_t>(tmp);
                } else {
                    diag.semantic_error_label(
                        "Intrinsic '" + var_name + "' is not recognized",
                        {loc},
                        "'" + var_name + "' is not a known intrinsic function"
                    );
                    throw SemanticAbort();
                }
            }
            if (v) {
                return ASR::make_Var_t(al, loc, v);
            }
        }
        if (compiler_options.implicit_typing) {
            // If inside a statement function body, declare implicit
            // variables in the parent (enclosing) scope so they are
            // shared with the host scope, matching Fortran semantics.
            SymbolTable* saved_scope = nullptr;
            if (statement_function_parent_scope) {
                saved_scope = current_scope;
                current_scope = statement_function_parent_scope;
            }
            std::string first_letter = std::string(1,var_name[0]);
            if (implicit_dictionary.find(first_letter) != implicit_dictionary.end()) {
                ASR::ttype_t *t = implicit_dictionary[first_letter];
                if (t == nullptr) {
                    diag.semantic_error_label("Variable '" + var_name
                        + "' is not declared", {loc},
                        "'" + var_name + "' is undeclared");
                    throw SemanticAbort();
                }
                ASR::intentType intent;
                if (std::find(current_procedure_args.begin(),
                        current_procedure_args.end(), var_name) !=
                        current_procedure_args.end()) {
                    intent = ASRUtils::intent_unspecified;
                } else {
                    intent = ASRUtils::intent_local;
                }
                v = declare_implicit_variable2(loc, var_name, intent, t);
            } else {
                ASR::intentType intent;
                if (std::find(current_procedure_args.begin(),
                        current_procedure_args.end(), var_name) !=
                        current_procedure_args.end()) {
                    intent = ASRUtils::intent_unspecified;
                } else {
                    intent = ASRUtils::intent_local;
                }
                v = declare_implicit_variable(loc, var_name, intent);
            }
            if (saved_scope) {
                current_scope = saved_scope;
            }
        } else if (_processing_dimensions && !v) {
            // Declare an implicit variable with integer type
            ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, compiler_options.po.default_integer_kind));
            SetChar variable_dependencies_vec;
            variable_dependencies_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type);
            v = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(al, loc,
                current_scope, s2c(al, var_name), variable_dependencies_vec.p,
                variable_dependencies_vec.size(), ASRUtils::intent_unspecified, nullptr, nullptr,
                ASR::storage_typeType::Default, type, nullptr,
                current_procedure_abi_type, ASR::Public,
                ASR::presenceType::Required, false));
            pre_declared_array_dims[var_name] = 1;
            current_scope->add_symbol(var_name, v);
        } else {
            diag.semantic_error_label("Variable '" + var_name
                + "' is not declared", {loc},
                "'" + var_name + "' is undeclared");
            throw SemanticAbort();
        }
    }

    // The symbol `v` must be a Variable
    ASR::symbol_t *vpast = ASRUtils::symbol_get_past_external(v);
    if (ASR::is_a<ASR::Variable_t>(*vpast) || ASR::is_a<ASR::Function_t>(*vpast)) {
        ASR::asr_t* v_var = ASR::make_Var_t(al, loc, v);
        // Check if this variable needs casting due to select type block
        auto it = select_type_casts_map.find(v);
        if (it != select_type_casts_map.end()) {
            ASR::symbol_t* select_type_override_sym = it->second.struct_sym;
            ASR::ttype_t* select_type_override_type = it->second.target_type;
            ASR::cast_kindType cast_kind = it->second.cast_kind;
            ASR::expr_t* dest_expr = select_type_override_sym ?
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, select_type_override_sym)) : nullptr;
            v_var = ASR::make_Cast_t(al, loc, ASRUtils::EXPR(v_var), cast_kind,
                select_type_override_type, nullptr, dest_expr);
        }
        return v_var;
    } else {
        report_not_variable_or_procedure(var_name, loc, vpast);
        throw SemanticAbort();
    }
}

std::string CommonVisitorBase::create_getter_function(const Location& loc, ASR::symbol_t* end_sym,
        ASR::ttype_t* type, SymbolTable* parent_scope) {
    SymbolTable* current_scope_copy = current_scope;
    ASRUtils::ASRBuilder b(al, loc);
    current_scope = al.make_new<SymbolTable>(parent_scope);

    // In lower case, like any Fortran name, so that a USE of the module
    // imports the getter under its own name.
    std::string base_name = "__lcompilers_get_" + std::string(ASRUtils::symbol_name(end_sym));
    std::string func_name = to_lower(parent_scope->get_unique_name(base_name));
    if (parent_scope->get_symbol(func_name) != nullptr) {
        func_name = parent_scope->get_unique_name(base_name, false);
    }
    if (ASR::is_a<ASR::ExternalSymbol_t>(*end_sym)) {
        ASRUtils::SymbolDuplicator sd(al);
        sd.duplicate_symbol(end_sym, current_scope);
        end_sym = current_scope->resolve_symbol(ASRUtils::symbol_name(end_sym));
    }
    ASR::expr_t* return_var_expr = b.Variable(current_scope, func_name, type,
                            ASR::intentType::ReturnVar);
    // populate body
    Vec<ASR::stmt_t*> body; body.reserve(al, 1);
    body.push_back(al, b.Assignment( return_var_expr, b.Var(end_sym) ));

    // A getter of an external procedure lives in the translation unit's
    // scope, and every translation unit that needs one creates its own
    // copy, so it must not be exported. A getter of a module procedure
    // stays public, so that a USE of the module can import it.
    ASR::accessType access = ASRUtils::is_tu_scope(parent_scope)
        ? ASR::accessType::Private : ASR::accessType::Public;
    ASR::symbol_t* func_sym = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Function_t_util(al, loc,
                            current_scope, s2c(al, func_name), nullptr, 0, nullptr, 0, body.p, body.n,
                            return_var_expr, ASR::abiType::Source,
                            access, ASR::deftypeType::Implementation,
                            nullptr, false, true, false, false, false, nullptr, 0, false, false, true, nullptr));
    current_scope = current_scope_copy;
    parent_scope->add_symbol(func_name,func_sym);
    getter_functions.insert(func_sym);

    return func_name;
}

ASR::expr_t* CommonVisitorBase::get_transformed_function_call(ASR::symbol_t* end_sym) {
    /*
        module y
            contains
            subroutine a(cs)
                use xx
                real, dimension(nx), intent(in) :: cs
            end subroutine
        end module y

        transform to:
        module y
            contains
            pure integer function __lcompilers_get_nx()
            use xx
            get_nx = nx
            end function

            subroutine a(cs)
                use xx
                real, dimension(__lcompilers_get_nx()), intent(in) :: cs
            end subroutine
        end module y
    */
    // The getter is created in the host of the procedure (not in the
    // procedure itself, which the nested_vars pass would wrongly
    // manipulate). The procedures of an interface block in a BLOCK
    // construct have the BLOCK as their host, but the backends define no
    // procedures there, so the getter is created in the procedure or
    // program that contains the BLOCK. A variable of the BLOCK is not
    // visible there, so the getter reads it through a pointer of that
    // procedure or program instead, see construct_variable_pointers.
    SymbolTable* parent_scope = ASRUtils::scope_past_constructs(current_scope->parent);
    ASR::ttype_t* type = ASRUtils::type_get_past_allocatable_pointer(
        ASRUtils::symbol_type(end_sym));
    ASR::symbol_t* read_sym = end_sym;
    if (parent_scope != current_scope->parent &&
            !ASR::is_a<ASR::ExternalSymbol_t>(*end_sym)) {
        SymbolTable* symbol_scope = ASRUtils::symbol_parent_symtab(end_sym);
        if (symbol_scope != parent_scope &&
                !ASRUtils::is_parent(symbol_scope, parent_scope)) {
            read_sym = create_construct_variable_pointer(end_sym, parent_scope);
        }
    }
    std::string func_name = create_getter_function(end_sym->base.loc, read_sym,
        type, parent_scope);
    ASR::symbol_t* getter_func_sym = parent_scope->resolve_symbol(LCompilers::s2c(al,func_name));
    current_function_dependencies.push_back(al,s2c(al, func_name));

    ASR::expr_t* func_call = ASRUtils::EXPR(ASRUtils::make_FunctionCall_t_util(al, getter_func_sym->base.loc,
                            getter_func_sym, getter_func_sym, nullptr, 0, type, nullptr, nullptr, current_scope, current_function_dependencies,
                            compiler_options.implicit_argument_casting));
    return func_call;
}

ASR::symbol_t* CommonVisitorBase::create_construct_variable_pointer(ASR::symbol_t* variable,
        SymbolTable* scope) {
    LCOMPILERS_ASSERT(ASR::is_a<ASR::Variable_t>(*variable));
    ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(variable);
    ASRUtils::ASRBuilder b(al, variable->base.loc);
    std::string name = scope->get_unique_name(
        "__lcompilers_block_" + std::string(v->m_name), false);
    ASR::ttype_t* type = ASRUtils::TYPE(ASR::make_Pointer_t(al,
        variable->base.loc, ASRUtils::type_get_past_allocatable_pointer(v->m_type)));
    ASR::symbol_t* pointer = ASR::down_cast<ASR::Var_t>(
        b.Variable(scope, name, type, ASR::intentType::Local))->m_v;
    v->m_target_attr = true;
    construct_variable_pointers.push_back({pointer, variable});
    return pointer;
}

ASR::expr_t* CommonVisitorBase::convert_integer_binop_to_function_call(ASR::expr_t* end, bool is_argument) {
    ASR::IntegerBinOp_t* end_bin_op = ASR::down_cast<ASR::IntegerBinOp_t>(end);
    ASR::expr_t* left = end_bin_op->m_left;
    ASR::expr_t* right = end_bin_op->m_right;
    if (ASR::is_a<ASR::Var_t>(*left) && !ASR::is_a<ASR::Var_t>(*right)) {
        /*
        Handle expressions like `nx + a` where `a` can either be
        an integer or an `IntegerBinOp_t`.

        Examples - nx + 1 and nx + ny * nz
        */
        ASR::Var_t* end_var = ASR::down_cast<ASR::Var_t>(left);
        ASR::symbol_t* end_sym = end_var->m_v;
        SymbolTable* symbol_scope = ASRUtils::symbol_parent_symtab(end_sym);
        if ((ASRUtils::expr_value(end) == nullptr) && (ASR::is_a<ASR::ExternalSymbol_t>(*end_sym) ||
            (symbol_scope->counter != current_scope->counter && is_argument)) ) {
            left = get_transformed_function_call(end_sym);
        }
        if (ASR::is_a<ASR::IntegerBinOp_t>(*right)) {
            right = convert_integer_binop_to_function_call(right, is_argument);
        }
    } else if (!ASR::is_a<ASR::Var_t>(*left) && ASR::is_a<ASR::Var_t>(*right)) {
        /*
        Handle expressions like `a + nx` where `a` can either be
        an integer or an `IntegerBinOp_t`.

        Examples - 1 + nx and ny * nz + nx
        */
        ASR::Var_t* end_var = ASR::down_cast<ASR::Var_t>(right);
        ASR::symbol_t* end_sym = end_var->m_v;
        SymbolTable* symbol_scope = ASRUtils::symbol_parent_symtab(end_sym);
        if ((ASRUtils::expr_value(end) == nullptr) && (ASR::is_a<ASR::ExternalSymbol_t>(*end_sym) ||
            (symbol_scope->counter != current_scope->counter && is_argument)) ) {
            right = get_transformed_function_call(end_sym);
        }
        if (ASR::is_a<ASR::IntegerBinOp_t>(*left)) {
            left = convert_integer_binop_to_function_call(left, is_argument);
        }
    } else if (ASR::is_a<ASR::Var_t>(*left) && ASR::is_a<ASR::Var_t>(*right)) {
        // Handle expressions like `nx + ny` where both `nx` and `ny` are
        // external variables.
        ASR::symbol_t* first_end_sym = ASR::down_cast<ASR::Var_t>(left)->m_v;
        ASR::symbol_t* second_end_sym = ASR::down_cast<ASR::Var_t>(right)->m_v;
        SymbolTable* first_symbol_scope = ASRUtils::symbol_parent_symtab(first_end_sym);
        SymbolTable* second_symbol_scope = ASRUtils::symbol_parent_symtab(second_end_sym);
        if ((ASRUtils::expr_value(end) == nullptr) && (ASR::is_a<ASR::ExternalSymbol_t>(*first_end_sym) ||
            (first_symbol_scope->counter != current_scope->counter && is_argument)) ) {
            left = get_transformed_function_call(first_end_sym);
        }
        if ((ASRUtils::expr_value(end) == nullptr) && (ASR::is_a<ASR::ExternalSymbol_t>(*second_end_sym) ||
            (second_symbol_scope->counter != current_scope->counter && is_argument)) ) {
            right = get_transformed_function_call(second_end_sym);
        }
    } else {
        /*
        Handle expressions like `a + b` where both `a` and `b` can either be
        an integer or an `IntegerBinOp_t`.

        Examples - 1 + 2 and 1 + nx + ny
        */
        if (ASR::is_a<ASR::IntegerBinOp_t>(*left)) {
            left = convert_integer_binop_to_function_call(left, is_argument);
        }
        if (ASR::is_a<ASR::IntegerBinOp_t>(*right)) {
            right = convert_integer_binop_to_function_call(right, is_argument);
        }
    }
    return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, end->base.loc,
        left, end_bin_op->m_op, right, end_bin_op->m_type, end_bin_op->m_value));
}

bool CommonVisitorBase::is_fortran_intrinsic_type(AST::decl_typeType t) {
    return t == AST::decl_typeType::TypeInteger
        || t == AST::decl_typeType::TypeReal
        || t == AST::decl_typeType::TypeComplex
        || t == AST::decl_typeType::TypeLogical
        || t == AST::decl_typeType::TypeCharacter
        || t == AST::decl_typeType::TypeDoublePrecision
        || t == AST::decl_typeType::TypeDoubleComplex;
}

bool CommonVisitorBase::procedure_names_intrinsic_type(const AST::AttrType_t &t) {
    if (!t.m_name) {
        return false;
    }
    std::string name = to_lower(t.m_name);
    return name == "real" || name == "integer" || name == "logical"
        || name == "complex" || name == "character"
        || name == "doubleprecision";
}

ASR::expr_t* CommonVisitorBase::read_variables_through_getters(ASR::expr_t* end,
        bool in_signature) {
    if (in_signature) {
        check_block_variable_reads(end, end, true);
    }
    ASR::expr_t* read = end;
    if (ASR::is_a<ASR::Var_t>(*end)) {
        ASR::symbol_t* end_sym = ASR::down_cast<ASR::Var_t>(end)->m_v;
        SymbolTable* symbol_scope = ASRUtils::symbol_parent_symtab(end_sym);
        if (ASRUtils::expr_value(end) == nullptr &&
            (ASR::is_a<ASR::ExternalSymbol_t>(*end_sym) ||
            (symbol_scope->counter != current_scope->counter && in_signature)) ) {
            read = get_transformed_function_call(end_sym);
        }
    } else if (ASR::is_a<ASR::IntegerBinOp_t>(*end)) {
        read = convert_integer_binop_to_function_call(end, in_signature);
    }
    if (in_signature) {
        check_block_variable_reads(end, read, false);
    }
    return read;
}

void CommonVisitorBase::check_block_variable_reads(ASR::expr_t* bound, ASR::expr_t* read,
        bool only_allocatable_or_pointer) {
    BlockVariableFinder finder(current_scope, only_allocatable_or_pointer);
    finder.visit_expr(*read);
    if (finder.variable == nullptr) {
        return;
    }
    diag.add(Diagnostic(
        "a specification expression in an interface body that "
        "references a variable of the enclosing BLOCK construct is "
        "not supported yet in this form",
        Level::Error, Stage::Semantic, {
            Label("'" + std::string(ASRUtils::symbol_name(finder.variable))
                + "' is a variable of the enclosing BLOCK construct",
                {bound->base.loc})
        }));
    throw SemanticAbort();
}

void CommonVisitorBase::check_intent_out_in_spec_expr(ASR::expr_t* expr) {
    if (!expr) return;
    if (ASR::is_a<ASR::Var_t>(*expr)) {
        ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(expr)->m_v);
        if (ASR::is_a<ASR::Variable_t>(*sym)) {
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
            if (var->m_intent == ASRUtils::intent_out) {
                diag.add(Diagnostic(
                    "Dummy argument '" + std::string(var->m_name) +
                    "' has `intent(out)` and cannot be used for "
                    "array bounds declaration",
                    Level::Error, Stage::Semantic, {
                        Label("hint: use `intent(in/inout)` or "
                            "`allocatable` for '" +
                            std::string(var->m_name) +
                            "', or pass the size as a separate "
                            "`intent(in)` argument",
                            {expr->base.loc})
                    }));
                throw SemanticAbort();
            }
            if (var->m_presence == ASR::presenceType::Optional) {
                diag.add(Diagnostic(
                    "OPTIONAL dummy argument '" +
                    std::string(var->m_name) +
                    "' cannot be used for array bounds "
                    "declaration",
                    Level::Error, Stage::Semantic, {
                        Label("hint: use a non-optional "
                            "argument, or declare the array "
                            "as `allocatable`",
                            {expr->base.loc})
                    }));
                throw SemanticAbort();
            }
        }
    } else if (ASR::is_a<ASR::StructInstanceMember_t>(*expr)) {
        check_intent_out_in_spec_expr(
            ASR::down_cast<ASR::StructInstanceMember_t>(expr)->m_v);
    } else if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
        auto* binop = ASR::down_cast<ASR::IntegerBinOp_t>(expr);
        check_intent_out_in_spec_expr(binop->m_left);
        check_intent_out_in_spec_expr(binop->m_right);
    } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*expr)) {
        check_intent_out_in_spec_expr(
            ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr)->m_arg);
    } else if (ASR::is_a<ASR::Cast_t>(*expr)) {
        check_intent_out_in_spec_expr(
            ASR::down_cast<ASR::Cast_t>(expr)->m_arg);
    }
}

bool CommonVisitorBase::is_named_constant_expr(ASR::expr_t* expr) {
    if (ASRUtils::is_value_constant(expr)) {
        return true;
    }
    switch (expr->type) {
        case ASR::exprType::ArraySize:
        case ASR::exprType::ArrayBound:
        case ASR::exprType::ArrayRank: {
            // Inquiries about constant properties need not read a
            // constant array, just as LEN need not read a constant string.
            return ASRUtils::is_value_constant(ASRUtils::expr_value(expr));
        }
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t* binop = ASR::down_cast<ASR::IntegerBinOp_t>(expr);
            return is_named_constant_expr(binop->m_left) &&
                is_named_constant_expr(binop->m_right);
        }
        case ASR::exprType::IntegerUnaryMinus: {
            return is_named_constant_expr(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr)->m_arg);
        }
        case ASR::exprType::StringLen: {
            return is_named_constant_expr(
                ASR::down_cast<ASR::StringLen_t>(expr)->m_arg);
        }
        case ASR::exprType::Cast: {
            return is_named_constant_expr(
                ASR::down_cast<ASR::Cast_t>(expr)->m_arg);
        }
        case ASR::exprType::ArrayItem: {
            // An element of a named constant array
            ASR::ArrayItem_t* item = ASR::down_cast<ASR::ArrayItem_t>(expr);
            if (!is_named_constant_expr(item->m_v)) {
                return false;
            }
            for (size_t i = 0; i < item->n_args; i++) {
                ASR::expr_t* parts[3] = {item->m_args[i].m_left,
                    item->m_args[i].m_right, item->m_args[i].m_step};
                for (ASR::expr_t* part: parts) {
                    if (part != nullptr && !is_named_constant_expr(part)) {
                        return false;
                    }
                }
            }
            return true;
        }
        case ASR::exprType::IntrinsicElementalFunction: {
            ASR::IntrinsicElementalFunction_t* func =
                ASR::down_cast<ASR::IntrinsicElementalFunction_t>(expr);
            for (size_t i = 0; i < func->n_args; i++) {
                if (func->m_args[i] != nullptr &&
                        !is_named_constant_expr(func->m_args[i])) {
                    return false;
                }
            }
            return true;
        }
        default: {
            return false;
        }
    }
}

bool CommonVisitorBase::character_length_requires_constant() {
    if (!_declaring_variable || is_derived_type || in_Subroutine) {
        return false;
    }
    for (SymbolTable* scope = current_scope; scope; scope = scope->parent) {
        if (scope->asr_owner == nullptr ||
                !ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) {
            continue;
        }
        ASR::symbol_t* owner = ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
        if (ASR::is_a<ASR::Function_t>(*owner) ||
                ASR::is_a<ASR::Block_t>(*owner) ||
                ASR::is_a<ASR::AssociateBlock_t>(*owner) ||
                ASR::is_a<ASR::Struct_t>(*owner)) {
            return false;
        }
        if (ASR::is_a<ASR::Program_t>(*owner) ||
                ASR::is_a<ASR::Module_t>(*owner)) {
            break;
        }
    }
    return true;
}

void CommonVisitorBase::check_character_length_expr(ASR::expr_t* expr, bool require_constant) {
    if (require_constant && !is_named_constant_expr(expr)) {
        diag.add(Diagnostic(
            "character length must be a constant expression in this context",
            Level::Error, Stage::Semantic, {
                Label("", {expr->base.loc})
            }));
        throw SemanticAbort();
    }
}

void CommonVisitorBase::dimension_attribute_error_check(ASR::expr_t* dim_expr) {
    check_intent_out_in_spec_expr(dim_expr);
    bool error = false;
    if (ASR::is_a<ASR::Var_t>(*dim_expr)) {
        ASR::Var_t* dim_expr_var = ASR::down_cast<ASR::Var_t>(dim_expr);
        ASR::symbol_t* dim_expr_sym = dim_expr_var->m_v;
        SymbolTable* symbol_scope = ASRUtils::symbol_parent_symtab(dim_expr_sym);
        if (ASR::is_a<ASR::Variable_t>(*dim_expr_sym)) {
            ASR::Variable_t* dim_expr_variable = ASR::down_cast<ASR::Variable_t>(dim_expr_sym);

            if (ASRUtils::type_get_past_allocatable_pointer(dim_expr_variable->m_type)->type != ASR::ttypeType::Integer) {
                error = true;
            } else {

                if ((dim_expr_variable->m_storage != ASR::storage_typeType::Parameter) && !(in_Subroutine) && (symbol_scope->counter == current_scope->counter)) {
                    error = true;
                }
            }
        }
    } else if (ASR::is_a<ASR::ArraySize_t>(*dim_expr)){
        ASR::ArraySize_t* dim_expr_arrsize = ASR::down_cast<ASR::ArraySize_t>(dim_expr);
        if  (ASR::is_a<ASR::Var_t>(*dim_expr_arrsize->m_v)){
            ASR::Var_t* arr_var = ASR::down_cast<ASR::Var_t>(dim_expr_arrsize->m_v);
            ASR::symbol_t* arr_sym = arr_var->m_v;
            ASR::Variable_t* arr_variable = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(arr_sym));
            SymbolTable* symbol_scope = ASRUtils::symbol_parent_symtab(arr_sym);
            if ((arr_variable->m_type->type == ASR::ttypeType::Allocatable) && !(in_Subroutine) && (symbol_scope->counter == current_scope->counter)) {
                error = true;
            }
        }
    }  else {
        ASR::ttype_t* dim_expr_type = ASRUtils::expr_type(dim_expr);
        if (dim_expr_type->type != ASR::ttypeType::Integer) {
            error = true;
        } else if (!is_named_constant_expr(dim_expr)) {
            bool in_function_scope = in_Subroutine;
            if (!in_function_scope) {
                SymbolTable* scope = current_scope;
                while (scope != nullptr && !in_function_scope) {
                    if (scope->asr_owner != nullptr &&
                            ASR::is_a<ASR::symbol_t>(*scope->asr_owner) &&
                            ASR::is_a<ASR::Function_t>(
                                *ASR::down_cast<ASR::symbol_t>(scope->asr_owner))) {
                        in_function_scope = true;
                    }
                    scope = scope->parent;
                }
            }
            if (!in_function_scope) {
                diag.add(Diagnostic(
                    "Explicit shaped array with nonconstant bounds",
                    Level::Error, Stage::Semantic, {
                        Label("",{dim_expr->base.loc})
                    }));
                throw SemanticAbort();
            }
        }
    }

    if(error){
        diag.add(Diagnostic(
            "Expecting a scalar integer or parameter annotated integer variable ",
            Level::Error, Stage::Semantic, {
                Label("",{dim_expr->base.loc})
            }));
        throw SemanticAbort();
    }
}

void CommonVisitorBase::set_postponed_explicit_shape_physical_type(ASR::Variable_t* v) {
    if (!ASR::is_a<ASR::Array_t>(*v->m_type)) {
        return;
    }
    ASR::Array_t* array_type = ASR::down_cast<ASR::Array_t>(v->m_type);
    if (array_type->m_physical_type == ASR::array_physical_typeType::DescriptorArray
            && !ASRUtils::is_dimension_empty(array_type->m_dims, array_type->n_dims)) {
        array_type->m_physical_type = ASR::array_physical_typeType::PointerArray;
    }
}

bool CommonVisitorBase::is_local_of_current_scoping_unit(ASR::symbol_t *sym) {
    if (sym == nullptr) {
        return false;
    }
    // Use association reaches the variable through an ExternalSymbol.
    if (ASR::is_a<ASR::ExternalSymbol_t>(*sym)) {
        return false;
    }
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(sym);
    if (!ASR::is_a<ASR::Variable_t>(*s)) {
        return false;
    }
    ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(s);
    // Host association: declared by an enclosing scoping unit.
    if (var->m_parent_symtab != current_scope) {
        return false;
    }
    // A named constant is a permitted primary.
    if (var->m_storage == ASR::storage_typeType::Parameter) {
        return false;
    }
    // A dummy argument is a permitted primary. One that is only named in
    // the argument list so far, whose declaration comes later in the
    // specification part, is still a dummy argument.
    if (var->m_intent != ASRUtils::intent_local) {
        return false;
    }
    if (std::find(current_procedure_args.begin(), current_procedure_args.end(),
            to_lower(std::string(var->m_name))) != current_procedure_args.end()) {
        return false;
    }
    // A variable in a common block is a permitted primary.
    if (current_common_block_objects.find(to_lower(std::string(var->m_name)))
            != current_common_block_objects.end()) {
        return false;
    }
    return true;
}

void CommonVisitorBase::erase_failed_instantiation(SymbolTable *scope,
        const std::map<std::string, ASR::symbol_t*> &scope_before) {
    std::vector<std::string> added;
    for (auto &item: scope->get_scope()) {
        if (scope_before.find(item.first) == scope_before.end()) {
            added.push_back(item.first);
        }
    }
    for (auto &name: added) {
        scope->erase_symbol(name);
    }
}

void CommonVisitorBase::check_specification_expr(ASR::expr_t *e, RestrictedExprContext context) {
    // A character length is also built for the type specification of an
    // array constructor, which is an ordinary expression rather than a
    // specification expression, so only a declaration is checked.
    if (e == nullptr || is_derived_type || !_declaring_variable) {
        return;
    }
    if (context != RestrictedExprContext::ArrayBound &&
            context != RestrictedExprContext::CharacterLength) {
        return;
    }
    SpecificationExprLocalFinder finder(*this);
    finder.visit_expr(*e);
    std::string what = (context == RestrictedExprContext::CharacterLength)
        ? "a character length" : "an array bound";
    if (finder.local_sym == nullptr) {
        // LBOUND and UBOUND without DIM of an assumed-rank array are
        // also built as one, `[(ubound(c, i), i = 1, rank(c))]`.
        if (finder.implied_do_loop != nullptr) {
            diag.add(Diagnostic(
                "an array constructor with an implied-do loop in a "
                "specification expression is not supported yet",
                Level::Error, Stage::Semantic, {
                    Label("an implied-do loop is evaluated here to give "
                        + what, {finder.implied_do_loop->base.base.loc})}));
            throw SemanticAbort();
        }
        return;
    }
    std::string name = ASRUtils::symbol_name(finder.local_sym);
    diag.add(Diagnostic(
        "the variable '" + name + "' is local to this scoping unit, so it "
        "cannot appear in a specification expression",
        Level::Error, Stage::Semantic, {
            Label("the value of '" + name + "' is read to give " + what,
                {finder.local_loc}),
            Label("'" + name + "' is declared here", {finder.local_sym->base.loc},
                false),
            Label("help: Fortran 2023 10.1.11 allows a dummy argument, a "
                "variable in a common block, and one made accessible by "
                "use or host association; a named constant, and an inquiry "
                "such as `size` or `len` about a local, are also allowed",
                {finder.local_loc}, false)}));
    throw SemanticAbort();
}

bool CommonVisitorBase::depends_on_pdt_kind_param(ASR::expr_t* e) {
    if (!is_derived_type || e == nullptr || current_scope->asr_owner == nullptr
            || !ASR::is_a<ASR::symbol_t>(*current_scope->asr_owner)) {
        return false;
    }
    ASR::symbol_t* owner = ASR::down_cast<ASR::symbol_t>(current_scope->asr_owner);
    if (!ASR::is_a<ASR::Struct_t>(*owner)) {
        return false;
    }
    return depends_on_kind_param(ASR::down_cast<ASR::Struct_t>(owner), e);
}

bool CommonVisitorBase::depends_on_kind_param(ASR::Struct_t* pdt, ASR::expr_t* e) {
    if (e == nullptr || pdt->n_kind_params == 0) {
        return false;
    }
    SetChar identifiers;
    identifiers.reserve(al, 1);
    ASRUtils::CollectIdentifiersFromASRExpression collector(al, identifiers);
    collector.visit_expr(*e);
    for (size_t i = 0; i < identifiers.size(); i++) {
        for (size_t j = 0; j < pdt->n_kind_params; j++) {
            if (std::string(identifiers[i]) == pdt->m_kind_params[j]) {
                return true;
            }
        }
    }
    return false;
}

ASR::expr_t* CommonVisitorBase::pdt_component_extent(ASR::expr_t* start, ASR::expr_t* end) {
    int64_t start_int = 0;
    if (start == nullptr || (!depends_on_pdt_kind_param(start)
            && ASRUtils::extract_value(ASRUtils::expr_value(start), start_int)
            && start_int == 1)) {
        return end;
    }
    ASRUtils::cast_to_wider_kind(al, start, end);
    const Location &loc = end->base.loc;
    ASR::ttype_t* type = ASRUtils::expr_type(end);
    auto make_binop = [&](ASR::expr_t* left, ASR::binopType op,
            ASR::expr_t* right) {
        ASR::expr_t* left_value = ASRUtils::expr_value(left);
        ASR::expr_t* right_value = ASRUtils::expr_value(right);
        ASR::expr_t* value = left_value && right_value
            ? visit_BinOp_helper(left_value, right_value, op, loc, type)
            : nullptr;
        return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, left, op,
            right, type, value));
    };
    ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
        1, type));
    return make_binop(make_binop(end, ASR::binopType::Sub, start),
        ASR::binopType::Add, one);
}

void CommonVisitorBase::make_pdt_signature_scope_free(ASR::Struct_t* pdt) {
    if (pdt->n_kind_params == 0 || pdt->m_struct_signature == nullptr
            || !ASR::is_a<ASR::StructType_t>(*pdt->m_struct_signature)) {
        return;
    }
    ASR::StructType_t* signature = ASR::down_cast<ASR::StructType_t>(
        pdt->m_struct_signature);
    Vec<ASR::ttype_t*> member_types;
    member_types.reserve(al, signature->n_data_member_types);
    for (size_t i = 0; i < signature->n_data_member_types; i++) {
        member_types.push_back(al, signature->m_data_member_types[i]);
    }
    for (size_t i = 0; i < member_types.size(); i++) {
        ASR::ttype_t* member_type = member_types[i];
        if (!ASR::is_a<ASR::Array_t>(*member_type)) {
            continue;
        }
        ASR::Array_t* array = ASR::down_cast<ASR::Array_t>(member_type);
        bool kind_extent = false;
        for (size_t j = 0; j < array->n_dims; j++) {
            kind_extent = kind_extent
                || depends_on_kind_param(pdt, array->m_dims[j].m_start)
                || depends_on_kind_param(pdt, array->m_dims[j].m_length);
        }
        if (!kind_extent) {
            continue;
        }
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, array->n_dims);
        for (size_t j = 0; j < array->n_dims; j++) {
            ASR::dimension_t dim = array->m_dims[j];
            if (dim.m_start) {
                dim.m_start = ASRUtils::expr_value(dim.m_start);
            }
            if (dim.m_length) {
                dim.m_length = ASRUtils::expr_value(dim.m_length);
            }
            LCOMPILERS_ASSERT(!depends_on_kind_param(pdt, dim.m_start)
                && !depends_on_kind_param(pdt, dim.m_length));
            dims.push_back(al, dim);
        }
        member_types.p[i] = ASRUtils::TYPE(ASR::make_Array_t(
            al, member_type->base.loc, array->m_type, dims.p, dims.size(),
            array->m_physical_type, array->m_memory_space));
    }
    signature->m_data_member_types = member_types.p;
}

bool CommonVisitorBase::has_pdt_kind_extent(ASR::ttype_t* type) {
    ASR::dimension_t* dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(type, dims);
    for (size_t i = 0; i < n_dims; i++) {
        if (depends_on_pdt_kind_param(dims[i].m_start)
                || depends_on_pdt_kind_param(dims[i].m_length)) {
            return true;
        }
    }
    return false;
}

ASR::expr_t* CommonVisitorBase::pdt_component_broadcast(ASR::expr_t* scalar, ASR::ttype_t* type) {
    const Location &loc = scalar->base.loc;
    ASR::dimension_t* dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(type, dims);
    Vec<ASR::expr_t*> lengths;
    lengths.reserve(al, n_dims);
    for (size_t i = 0; i < n_dims; i++) {
        lengths.push_back(al, dims[i].m_length);
    }
    ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    Vec<ASR::dimension_t> shape_dims;
    shape_dims.reserve(al, 1);
    ASR::dimension_t shape_dim;
    shape_dim.loc = loc;
    shape_dim.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
        1, int_type));
    shape_dim.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
        n_dims, int_type));
    shape_dims.push_back(al, shape_dim);
    ASR::ttype_t* shape_type = ASRUtils::TYPE(ASR::make_Array_t(al, loc,
        int_type, shape_dims.p, shape_dims.size(),
        ASR::array_physical_typeType::FixedSizeArray,
        ASR::memory_spaceType::Global));
    ASR::expr_t* shape = ASRUtils::EXPR(ASR::make_ArrayConstructor_t(al, loc,
        lengths.p, lengths.size(), shape_type, nullptr,
        ASR::arraystorageType::ColMajor, nullptr));
    return ASRUtils::EXPR(ASR::make_ArrayBroadcast_t(al, loc, scalar, shape,
        ASRUtils::duplicate_type(al, type), nullptr));
}

ASR::accessType CommonVisitorBase::get_asr_simple_attr(AST::simple_attributeType simple_attr) {
    ASR::accessType access_type = ASR::accessType::Public;
    switch( simple_attr ) {
        case AST::simple_attributeType::AttrPublic: {
            access_type = ASR::accessType::Public;
            break;
        }
        case AST::simple_attributeType::AttrPrivate: {
            access_type = ASR::accessType::Private;
            break;
        }
        default:
            LCOMPILERS_ASSERT(false);
    }
    return access_type;
}

void CommonVisitorBase::append_module_dependency(char *m_name) {
    if (!m_name || m_name[0] == '\0') return;
    // We are in body visitor
    // Add the module `m` to current module dependencies
    if (current_module) {
        SetChar vec;
        vec.from_pointer_n_copy(al, current_module->m_dependencies,
                    current_module->n_dependencies);
        vec.push_back(al, m_name);
        current_module->m_dependencies = vec.p;
        current_module->n_dependencies = vec.size();
    } else {
        // We are in the symtab visitor or body visitor (the
        // current_module_dependencies is not used in body visitor)
        current_module_dependencies.push_back(al, m_name);
    }
}

bool CommonVisitorBase::is_legacy_assumed_size_dummy(ASR::expr_t* array) {
    if (!compiler_options.legacy_array_sections ||
            !ASR::is_a<ASR::Var_t>(*array)) {
        return false;
    }
    ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(array)->m_v);
    if (!ASR::is_a<ASR::Variable_t>(*sym)) {
        return false;
    }
    ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
    if (!ASRUtils::is_arg_dummy(var->m_intent) ||
            ASRUtils::is_allocatable_or_pointer(var->m_type)) {
        return false;
    }
    ASR::dimension_t* m_dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(var->m_type, m_dims);
    if (n_dims == 0 || ASRUtils::is_dimension_empty(m_dims, n_dims)) {
        return false;
    }
    int64_t lb = 0, len = 0;
    ASR::dimension_t& last = m_dims[n_dims - 1];
    return last.m_start && last.m_length &&
        ASRUtils::extract_value(ASRUtils::expr_value(last.m_start), lb) &&
        ASRUtils::extract_value(ASRUtils::expr_value(last.m_length), len) &&
        lb == 1 && len == 1;
}

bool CommonVisitorBase::is_legacy_assumed_size_element(ASR::expr_t* array,
        ASR::array_index_t* args, size_t n_args) {
    if (ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(array))
            != (int64_t) n_args || !is_legacy_assumed_size_dummy(array)) {
        return false;
    }
    for (size_t i = 0; i < n_args; i++) {
        if (args[i].m_left != nullptr || args[i].m_step != nullptr ||
                args[i].m_right == nullptr ||
                ASRUtils::is_array(ASRUtils::expr_type(args[i].m_right))) {
            return false;
        }
    }
    return true;
}

bool CommonVisitorBase::validate_fixed_size_array_index_bounds(ASR::expr_t* array,
        ASR::array_index_t* args, size_t n_args, const Location& loc) {
    if (_processing_common_block_object) {
        return false;
    }
    ASR::ttype_t* array_type = ASRUtils::expr_type(array);
    ASR::ttype_t* arr_type = ASRUtils::type_get_past_pointer(
        ASRUtils::type_get_past_allocatable(array_type));
    ASR::dimension_t* m_dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(arr_type, m_dims);
    bool in_bounds = n_dims > 0 && n_dims == n_args &&
        !ASRUtils::is_allocatable(array_type) &&
        !ASRUtils::is_pointer(array_type);
    bool legacy_assumed_size = n_dims == n_args &&
        is_legacy_assumed_size_dummy(array);
    if (legacy_assumed_size) {
        n_dims--;
    }
    for (size_t i = 0; i < std::min(n_dims, n_args); i++) {
        if (args[i].m_left != nullptr || args[i].m_step != nullptr) {
            in_bounds = false;
            continue;
        }
        ASR::expr_t* idx_expr = args[i].m_right;
        if (idx_expr == nullptr) {
            in_bounds = false;
            continue;
        }
        ASR::expr_t* idx_value = ASRUtils::expr_value(idx_expr);
        int64_t idx = 0;
        if (idx_value && ASRUtils::extract_value(idx_value, idx)) {
            int64_t lb = 1;
            bool lb_known = false;
            if (m_dims[i].m_start) {
                ASR::expr_t* start_val = ASRUtils::expr_value(m_dims[i].m_start);
                if (start_val) {
                    lb_known = ASRUtils::extract_value(start_val, lb);
                }
            }
            bool ub_known = false;
            if (m_dims[i].m_length) {
                ASR::expr_t* len_val = ASRUtils::expr_value(m_dims[i].m_length);
                int64_t len = 0;
                if (len_val && ASRUtils::extract_value(len_val, len)) {
                    ub_known = true;
                    int64_t ub = lb + len - 1;
                    if (idx < lb || idx > ub) {
                        diag.add(Diagnostic(
                            "Array index " + std::to_string(idx) +
                            " is out of bounds (" + std::to_string(lb) +
                            " to " + std::to_string(ub) +
                            ") in dimension " + std::to_string(i + 1),
                            Level::Error, Stage::Semantic, {Label("", {loc})}));
                        throw SemanticAbort();
                    }
                }
            }
            in_bounds = in_bounds && lb_known && ub_known;
        } else {
            in_bounds = false;
        }
    }
    return legacy_assumed_size
        ? is_legacy_assumed_size_element(array, args, n_args) : in_bounds;
}

bool CommonVisitorBase::data_object_has_saved_initializer(ASR::expr_t* object) {
    if (ASR::is_a<ASR::ArrayItem_t>(*object)) {
        return data_object_has_saved_initializer(
            ASR::down_cast<ASR::ArrayItem_t>(object)->m_v);
    }
    if (ASR::is_a<ASR::StringSection_t>(*object)) {
        return data_object_has_saved_initializer(
            ASR::down_cast<ASR::StringSection_t>(object)->m_arg);
    }
    ASR::Variable_t* v = ASRUtils::expr_to_variable_or_null(object);
    if (!v) return false;
    ASR::symbol_t* owner = ASRUtils::get_asr_owner(&v->base);
    return is_data_saved_local_owner(owner)
        && v->m_storage == ASR::storage_typeType::Save
        && v->m_symbolic_value != nullptr && v->m_common_block == nullptr;
}

bool CommonVisitorBase::extract_data_vector_subscript(ASR::expr_t* expr,
        std::vector<int64_t>& subscripts) {
    ASR::expr_t* expr_value = ASRUtils::expr_value(expr);
    if (!expr_value) expr_value = expr;
    if (ASR::is_a<ASR::ArrayConstant_t>(*expr_value)) {
        ASR::ArrayConstant_t* ac = ASR::down_cast<ASR::ArrayConstant_t>(expr_value);
        int64_t n = ASRUtils::get_fixed_size_of_array(ac->m_type);
        for (int64_t k = 0; k < n; k++) {
            int64_t idx = 0;
            if (!extract_data_bound(
                    ASRUtils::fetch_ArrayConstant_value(al, ac, k), idx)) {
                return false;
            }
            subscripts.push_back(idx);
        }
        return true;
    }
    if (ASR::is_a<ASR::ArrayConstructor_t>(*expr_value)) {
        ASR::ArrayConstructor_t* ac = ASR::down_cast<ASR::ArrayConstructor_t>(expr_value);
        for (size_t k = 0; k < ac->n_args; k++) {
            int64_t idx = 0;
            if (!extract_data_bound(ac->m_args[k], idx)) return false;
            subscripts.push_back(idx);
        }
        return true;
    }
    return false;
}

bool CommonVisitorBase::resolve_data_section(ASR::expr_t* array, ASR::array_index_t* args,
        size_t n_args, DataSectionBounds& b, const Location& loc) {
    ASR::ttype_t* var_type = ASRUtils::type_get_past_allocatable(
        ASRUtils::type_get_past_pointer(ASRUtils::expr_type(array)));
    if (!ASR::is_a<ASR::Array_t>(*var_type)) return false;
    ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(var_type);
    size_t n = arr->n_dims;
    if (n == 0 || n_args != n) return false;
    b.lb.assign(n, 1);
    b.extent.assign(n, 0);
    b.subscripts.assign(n, {});
    b.size = 1;
    for (size_t d = 0; d < n; d++) {
        if ((arr->m_dims[d].m_start &&
                !extract_data_bound(arr->m_dims[d].m_start, b.lb[d])) ||
                !extract_data_bound(arr->m_dims[d].m_length, b.extent[d])) {
            return false;
        }
        int64_t ub = b.lb[d] + b.extent[d] - 1;
        ASR::array_index_t &arg = args[d];
        std::vector<int64_t> &subs = b.subscripts[d];
        if (!arg.m_left && !arg.m_step) {
            if (!arg.m_right) return false;
            if (ASRUtils::is_array(ASRUtils::expr_type(arg.m_right))) {
                if (!extract_data_vector_subscript(arg.m_right, subs)) {
                    return false;
                }
            } else {
                int64_t idx = 0;
                if (!extract_data_bound(arg.m_right, idx)) return false;
                subs.push_back(idx);
            }
        } else {
            int64_t start = b.lb[d], end = ub, step = 1;
            if ((arg.m_left && !extract_data_bound(arg.m_left, start)) ||
                    (arg.m_right && !extract_data_bound(arg.m_right, end)) ||
                    (arg.m_step && !extract_data_bound(arg.m_step, step)) ||
                    step == 0) {
                return false;
            }
            for (int64_t idx = start; step > 0 ? idx <= end : idx >= end;
                    idx += step) {
                subs.push_back(idx);
            }
        }
        for (int64_t idx : subs) {
            if (idx < b.lb[d] || idx > ub) {
                diag.add(Diagnostic(
                    "Array index " + std::to_string(idx) +
                    " is out of bounds (" + std::to_string(b.lb[d]) +
                    " to " + std::to_string(ub) +
                    ") in dimension " + std::to_string(d + 1),
                    Level::Error, Stage::Semantic, {Label("", {loc})}));
                throw SemanticAbort();
            }
        }
        b.size *= (int64_t) subs.size();
    }
    return true;
}

bool CommonVisitorBase::resolve_data_section(ASR::expr_t* object, DataSectionBounds& b) {
    if (ASR::is_a<ASR::ArraySection_t>(*object)) {
        ASR::ArraySection_t* section = ASR::down_cast<ASR::ArraySection_t>(object);
        return resolve_data_section(section->m_v, section->m_args,
            section->n_args, b, object->base.loc);
    }
    LCOMPILERS_ASSERT(ASR::is_a<ASR::ArrayItem_t>(*object));
    ASR::ArrayItem_t* item = ASR::down_cast<ASR::ArrayItem_t>(object);
    return resolve_data_section(item->m_v, item->m_args, item->n_args, b,
        object->base.loc);
}

void CommonVisitorBase::set_data_initial_value(ASR::Variable_t* v, ASR::expr_t* value) {
    v->m_value = value;
    v->m_symbolic_value = value;
    // DATA gives a local variable static lifetime, just like declaration
    // initialization. COMMON storage and derived-type members are owned
    // by their enclosing objects instead.
    ASR::symbol_t* owner = ASRUtils::get_asr_owner(&v->base);
    if (is_data_saved_local_owner(owner)
            && v->m_intent == ASR::intentType::Local
            && v->m_common_block == nullptr
            && v->m_storage == ASR::storage_typeType::Default) {
        v->m_storage = ASR::storage_typeType::Save;
    }
    SetChar var_deps_vec;
    var_deps_vec.reserve(al, 1);
    ASRUtils::collect_variable_dependencies(al, var_deps_vec, v->m_type,
        v->m_symbolic_value, v->m_value);
    v->m_dependencies = var_deps_vec.p;
    v->n_dependencies = var_deps_vec.size();
    if (ASR::CommonBlock_t* block = ASRUtils::get_common_block(v)) {
        ASRUtils::update_common_block_role(block);
    }
}

ASR::expr_t* CommonVisitorBase::replace_loop_var_in_expr(ASR::expr_t* expr,
                                      ASR::symbol_t* var_sym,
                                      int64_t value,
                                      ASR::ttype_t* int_type) {
    if (!expr) return expr;
    if (ASR::is_a<ASR::Var_t>(*expr)) {
        ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(expr)->m_v);
        if (sym == var_sym) {
            return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, expr->base.loc, value, int_type));
        }
        return expr;
    } else if (ASR::is_a<ASR::IntegerBinOp_t>(*expr)) {
        ASR::IntegerBinOp_t* binop = ASR::down_cast<ASR::IntegerBinOp_t>(expr);
        ASR::expr_t* left = replace_loop_var_in_expr(binop->m_left, var_sym, value, int_type);
        ASR::expr_t* right = replace_loop_var_in_expr(binop->m_right, var_sym, value, int_type);
        binop->m_left = left;
        binop->m_right = right;
        ASR::expr_t* left_val = ASRUtils::expr_value(left);
        ASR::expr_t* right_val = ASRUtils::expr_value(right);
        if (!left_val) left_val = left;
        if (!right_val) right_val = right;
        if (ASR::is_a<ASR::IntegerConstant_t>(*left_val) &&
            ASR::is_a<ASR::IntegerConstant_t>(*right_val)) {
            int64_t lv = ASR::down_cast<ASR::IntegerConstant_t>(left_val)->m_n;
            int64_t rv = ASR::down_cast<ASR::IntegerConstant_t>(right_val)->m_n;
            int64_t result = 0;
            switch (binop->m_op) {
                case ASR::binopType::Add: result = lv + rv; break;
                case ASR::binopType::Sub: result = lv - rv; break;
                case ASR::binopType::Mul: result = lv * rv; break;
                case ASR::binopType::Div: result = lv / rv; break;
                case ASR::binopType::Pow: {
                    result = 1;
                    for (int64_t p = 0; p < rv; p++) result *= lv;
                    break;
                }
                default: return expr;
            }
            return ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, expr->base.loc, result, binop->m_type));
        }
        return expr;
    } else if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*expr)) {
        ASR::IntegerUnaryMinus_t* umin = ASR::down_cast<ASR::IntegerUnaryMinus_t>(expr);
        ASR::expr_t* arg = replace_loop_var_in_expr(umin->m_arg, var_sym, value, int_type);
        umin->m_arg = arg;
        ASR::expr_t* arg_val = ASRUtils::expr_value(arg);
        if (!arg_val) arg_val = arg;
        if (ASR::is_a<ASR::IntegerConstant_t>(*arg_val)) {
            int64_t av = ASR::down_cast<ASR::IntegerConstant_t>(arg_val)->m_n;
            return ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, expr->base.loc, -av, umin->m_type));
        }
        return expr;
    } else if (ASR::is_a<ASR::IntegerConstant_t>(*expr)) {
        return expr;
    }
    return expr;
}

void CommonVisitorBase::substitute_var_in_implied_do_loop(ASR::ImpliedDoLoop_t* idl,
                                       ASR::symbol_t* var_sym,
                                       int64_t value,
                                       ASR::ttype_t* int_type) {
    idl->m_start = replace_loop_var_in_expr(idl->m_start, var_sym, value, int_type);
    idl->m_end = replace_loop_var_in_expr(idl->m_end, var_sym, value, int_type);
    idl->m_increment = replace_loop_var_in_expr(idl->m_increment, var_sym, value, int_type);
    for (size_t i = 0; i < idl->n_values; i++) {
        ASR::expr_t* val = idl->m_values[i];
        if (ASR::is_a<ASR::ImpliedDoLoop_t>(*val)) {
            substitute_var_in_implied_do_loop(
                ASR::down_cast<ASR::ImpliedDoLoop_t>(val), var_sym, value, int_type);
        } else {
            substitute_var_in_data_object(val, var_sym, value, int_type);
        }
    }
}

bool CommonVisitorBase::substitute_var_in_data_object(ASR::expr_t* object,
                                   ASR::symbol_t* var_sym,
                                   int64_t value,
                                   ASR::ttype_t* int_type) {
    if (ASR::is_a<ASR::ArrayItem_t>(*object)) {
        ASR::ArrayItem_t* arr = ASR::down_cast<ASR::ArrayItem_t>(object);
        for (size_t j = 0; j < arr->n_args; j++) {
            ASR::expr_t* idx = arr->m_args[j].m_right;
            if (idx) {
                arr->m_args[j].m_right = replace_loop_var_in_expr(
                    idx, var_sym, value, int_type);
            }
        }
        substitute_var_in_data_object(arr->m_v, var_sym, value, int_type);
        return true;
    }
    if (ASR::is_a<ASR::StructInstanceMember_t>(*object)) {
        return substitute_var_in_data_object(
            ASR::down_cast<ASR::StructInstanceMember_t>(object)->m_v,
            var_sym, value, int_type);
    }
    return false;
}

void CommonVisitorBase::set_data_local_elements_value(const AST::DataStmt_t &data_stmt,
        const std::vector<DataLocalElement> &elements) {
    std::vector<ASR::Variable_t*> arrays;
    std::map<ASR::Variable_t*, std::vector<std::pair<int64_t, ASR::expr_t*>>> values;
    for (const DataLocalElement &e : elements) {
        auto &array_values = values[e.array];
        if (array_values.empty()) arrays.push_back(e.array);
        array_values.push_back({e.flat, e.element_value});
    }
    for (ASR::Variable_t* array : arrays) {
        set_data_array_elements_value(array, values[array],
            data_stmt.base.base.loc);
    }
    for (const DataLocalElement &e : elements) {
        if (!data_object_has_saved_initializer(&e.target->base)) {
            emit_data_element_assignment(data_stmt, &e.target->base, e.value);
        }
    }
}

bool CommonVisitorBase::data_array_element_position(ASR::ArrayItem_t* arr_item,
        ASR::Variable_t*& arr_var, int64_t& flat) {
    arr_var = nullptr;
    if (ASR::is_a<ASR::Var_t>(*arr_item->m_v)) {
        arr_var = ASR::down_cast<ASR::Variable_t>(
            ASR::down_cast<ASR::Var_t>(arr_item->m_v)->m_v);
    } else if (ASR::is_a<ASR::StructInstanceMember_t>(*arr_item->m_v)) {
        ASR::StructInstanceMember_t* mem = ASR::down_cast<ASR::StructInstanceMember_t>(arr_item->m_v);
        arr_var = ASR::down_cast<ASR::Variable_t>(
            ASRUtils::symbol_get_past_external(mem->m_m));
    }
    if (!arr_var || !ASRUtils::is_array(arr_var->m_type)
            || ASR::is_a<ASR::Pointer_t>(*arr_var->m_type)) {
        return false;
    }
    ASR::ttype_t* arr_ttype = ASRUtils::type_get_past_allocatable(
        ASRUtils::type_get_past_pointer(arr_var->m_type));
    int64_t total_size = ASRUtils::get_fixed_size_of_array(arr_ttype);
    if (total_size <= 0) {
        return false;
    }
    ASR::Array_t* arr_t = ASR::down_cast<ASR::Array_t>(arr_ttype);
    // Compute flat (ColMajor) index
    flat = 0;
    int64_t stride = 1;
    bool idx_ok = (arr_t->n_dims == arr_item->n_args);
    for (size_t d = 0; d < arr_t->n_dims && idx_ok; d++) {
        int64_t idx_val = 0, start_val = 1, dim_size = 1;
        ASR::expr_t* idx_expr = arr_item->m_args[d].m_right;
        if (idx_expr) {
            ASR::expr_t* iv = ASRUtils::expr_value(idx_expr);
            if (!iv) iv = idx_expr;
            if (!ASRUtils::extract_value(iv, idx_val)) idx_ok = false;
        } else {
            idx_ok = false;
        }
        if (arr_t->m_dims[d].m_start) {
            ASR::expr_t* sv = ASRUtils::expr_value(arr_t->m_dims[d].m_start);
            if (sv) ASRUtils::extract_value(sv, start_val);
        }
        if (arr_t->m_dims[d].m_length) {
            ASR::expr_t* lv = ASRUtils::expr_value(arr_t->m_dims[d].m_length);
            if (lv) ASRUtils::extract_value(lv, dim_size);
        }
        flat += (idx_val - start_val) * stride;
        stride *= dim_size;
    }
    return idx_ok && flat >= 0 && flat < total_size;
}

ASR::expr_t* CommonVisitorBase::data_array_initial_element(ASR::Variable_t* arr_var,
        int64_t flat) {
    if (arr_var->m_value && ASR::is_a<ASR::ArrayConstant_t>(*arr_var->m_value)) {
        ASR::ArrayConstant_t* prev = ASR::down_cast<ASR::ArrayConstant_t>(arr_var->m_value);
        if (flat < ASRUtils::get_fixed_size_of_array(prev->m_type)) {
            return ASRUtils::fetch_ArrayConstant_value(al, prev, (int)flat);
        }
    }
    return nullptr;
}

bool CommonVisitorBase::update_data_array_constant(ASR::Variable_t* arr_var,
        int64_t total_size, ASR::ttype_t* elem_type,
        const std::vector<std::pair<int64_t, ASR::expr_t*>> &elements,
        const Location &loc) {
    if (!arr_var->m_value || !ASR::is_a<ASR::ArrayConstant_t>(*arr_var->m_value)) {
        return false;
    }
    ASR::ArrayConstant_t* prev = ASR::down_cast<ASR::ArrayConstant_t>(arr_var->m_value);
    if (ASRUtils::get_fixed_size_of_array(prev->m_type) != total_size
            || !ASRUtils::types_equal(ASRUtils::type_get_past_array(prev->m_type),
                elem_type, nullptr, nullptr)) {
        return false;
    }
    // The layout of the data of an ArrayConstant (set_ArrayConstant_data)
    int kind = ASRUtils::extract_kind_from_ttype_t(elem_type);
    size_t elem_bytes = 0;
    if (ASR::is_a<ASR::Integer_t>(*elem_type) && (kind == 1 || kind == 2
            || kind == 4 || kind == 8)) {
        elem_bytes = kind;
    } else if (ASR::is_a<ASR::Real_t>(*elem_type) && (kind == 4 || kind == 8)) {
        elem_bytes = kind;
    } else if (ASR::is_a<ASR::Complex_t>(*elem_type) && (kind == 4 || kind == 8)) {
        elem_bytes = 2 * kind;
    } else if (ASR::is_a<ASR::Logical_t>(*elem_type)) {
        elem_bytes = sizeof(bool);
    } else {
        return false;
    }
    for (const auto &element : elements) {
        ASR::expr_t* value = element.second;
        if (!ASRUtils::types_equal(ASRUtils::expr_type(value), elem_type,
                    nullptr, nullptr)
                || !(ASR::is_a<ASR::IntegerConstant_t>(*value)
                    || ASR::is_a<ASR::RealConstant_t>(*value)
                    || ASR::is_a<ASR::ComplexConstant_t>(*value)
                    || ASR::is_a<ASR::LogicalConstant_t>(*value))) {
            return false;
        }
    }
    char* data = al.allocate<char>(total_size * elem_bytes);
    std::memcpy(data, prev->m_data, total_size * elem_bytes);
    for (const auto &element : elements) {
        LCOMPILERS_ASSERT(element.first >= 0 && element.first < total_size);
        char* dst = data + element.first * elem_bytes;
        auto put = [&](auto x, size_t offset) {
            std::memcpy(dst + offset, &x, sizeof(x));
        };
        ASR::expr_t* value = element.second;
        if (ASR::is_a<ASR::IntegerConstant_t>(*value)) {
            int64_t n = ASR::down_cast<ASR::IntegerConstant_t>(value)->m_n;
            switch (kind) {
                case 1: put((int8_t) n, 0); break;
                case 2: put((int16_t) n, 0); break;
                case 4: put((int32_t) n, 0); break;
                default: put((int64_t) n, 0); break;
            }
        } else if (ASR::is_a<ASR::RealConstant_t>(*value)) {
            double r = ASR::down_cast<ASR::RealConstant_t>(value)->m_r;
            if (kind == 4) put((float) r, 0); else put(r, 0);
        } else if (ASR::is_a<ASR::ComplexConstant_t>(*value)) {
            ASR::ComplexConstant_t* c = ASR::down_cast<ASR::ComplexConstant_t>(value);
            if (kind == 4) {
                put((float) ASRUtils::complex_constant_re(c), 0);
                put((float) ASRUtils::complex_constant_im(c), sizeof(float));
            } else {
                put(ASRUtils::complex_constant_re(c), 0);
                put(ASRUtils::complex_constant_im(c), sizeof(double));
            }
        } else {
            put((bool) ASR::down_cast<ASR::LogicalConstant_t>(value)->m_value, 0);
        }
    }
    set_data_initial_value(arr_var, ASRUtils::EXPR(
        ASRUtils::make_ArrayConstant_t_util(al, loc, data, prev->m_type,
            prev->m_storage_format)));
    return true;
}

void CommonVisitorBase::set_data_array_elements_value(ASR::Variable_t* arr_var,
        const std::vector<std::pair<int64_t, ASR::expr_t*>> &elements,
        const Location &loc) {
    ASR::ttype_t* arr_ttype = ASRUtils::type_get_past_allocatable(
        ASRUtils::type_get_past_pointer(arr_var->m_type));
    int64_t total_size = ASRUtils::get_fixed_size_of_array(arr_ttype);
    ASR::ttype_t* elem_type = ASR::down_cast<ASR::Array_t>(arr_ttype)->m_type;
    if (update_data_array_constant(arr_var, total_size, elem_type,
            elements, loc)) {
        return;
    }
    std::vector<ASR::expr_t*> slots(total_size, nullptr);
    // Preserve values from a prior DATA statement
    if (arr_var->m_value && ASR::is_a<ASR::ArrayConstant_t>(*arr_var->m_value)) {
        ASR::ArrayConstant_t* prev = ASR::down_cast<ASR::ArrayConstant_t>(arr_var->m_value);
        int64_t prev_size = ASRUtils::get_fixed_size_of_array(prev->m_type);
        for (int64_t k = 0; k < prev_size && k < total_size; k++) {
            slots[k] = ASRUtils::fetch_ArrayConstant_value(al, prev, (int)k);
        }
    }
    for (const auto &element : elements) {
        LCOMPILERS_ASSERT(element.first >= 0 && element.first < total_size);
        slots[element.first] = element.second;
    }
    Vec<ASR::expr_t*> full_values;
    full_values.reserve(al, (size_t)total_size);
    for (int64_t k = 0; k < total_size; k++) {
        if (slots[k] == nullptr) {
            slots[k] = ASRUtils::get_constant_zero_with_given_type(al, elem_type);
        }
        full_values.push_back(al, slots[k]);
    }
    ASR::asr_t* arr_constructor = ASRUtils::make_ArrayConstructor_t_util(
        al, loc, full_values.p,
        full_values.size(), arr_var->m_type,
        ASR::arraystorageType::ColMajor);
    set_data_initial_value(arr_var, ASRUtils::EXPR(arr_constructor));
}

bool CommonVisitorBase::set_data_array_element_value(ASR::ArrayItem_t* arr_item,
        const std::function<ASR::expr_t*(ASR::expr_t*)> &element_value,
        const Location &loc) {
    ASR::Variable_t* arr_var = nullptr;
    int64_t flat = 0;
    if (!data_array_element_position(arr_item, arr_var, flat)) {
        return false;
    }
    ASR::expr_t* value = element_value(
        data_array_initial_element(arr_var, flat));
    if (value == nullptr) {
        return false;
    }
    set_data_array_elements_value(arr_var, {{flat, value}}, loc);
    return true;
}

ASR::ArrayItem_t* CommonVisitorBase::data_component_array_element(ASR::expr_t* object,
        std::vector<ASR::expr_t*>& parts) {
    parts.clear();
    ASR::expr_t* e = object;
    while (true) {
        if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
            parts.push_back(e);
            e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
        } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
            ASR::ArrayItem_t* item = ASR::down_cast<ASR::ArrayItem_t>(e);
            if (!parts.empty() && ASR::is_a<ASR::Var_t>(*item->m_v)) {
                std::reverse(parts.begin(), parts.end());
                return item;
            }
            parts.push_back(e);
            e = item->m_v;
        } else {
            return nullptr;
        }
    }
}

ASR::expr_t* CommonVisitorBase::data_default_struct_value(ASR::symbol_t* dt_sym,
        ASR::ttype_t* type, const Location& loc) {
    StructConstructorInfo info = get_struct_constructor_info(dt_sym);
    if (!info.kind_indices.empty()) {
        return nullptr;
    }
    Vec<ASR::call_arg_t> args;
    args.reserve(al, info.members.size());
    for (ASR::symbol_t* member : info.members) {
        ASR::call_arg_t arg;
        arg.loc = loc;
        arg.m_value = nullptr;
        if (member && ASR::is_a<ASR::Variable_t>(*member)) {
            ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(member);
            if (v->m_symbolic_value) {
                if (!ASRUtils::is_value_constant(v->m_value)) {
                    return nullptr;
                }
                arg.m_value = v->m_value;
            }
        }
        args.push_back(al, arg);
    }
    return ASRUtils::EXPR(ASR::make_StructConstant_t(al, loc, dt_sym,
        args.p, args.size(), type));
}

ASR::expr_t* CommonVisitorBase::data_component_updated_value(ASR::expr_t* previous,
        const std::vector<const DataComponentValue*>& values, size_t k,
        const Location& loc) {
    size_t first = 0;
    for (size_t v = 0; v < values.size(); v++) {
        if (values[v]->parts.size() == k) {
            previous = values[v]->value;
            first = v + 1;
        }
    }
    if (first == values.size()) {
        return previous;
    }
    ASR::expr_t* part = values[first]->parts[k];
    if (ASR::is_a<ASR::StructInstanceMember_t>(*part)) {
        ASR::StructInstanceMember_t* member =
            ASR::down_cast<ASR::StructInstanceMember_t>(part);
        ASR::ttype_t* struct_type = ASRUtils::expr_type(member->m_v);
        ASR::symbol_t* dt_sym = ASRUtils::get_struct_sym_from_struct_expr(member->m_v);
        if (previous == nullptr) {
            previous = data_default_struct_value(dt_sym, struct_type, loc);
        }
        if (previous == nullptr || !ASR::is_a<ASR::StructConstant_t>(*previous)) {
            return nullptr;
        }
        ASR::StructConstant_t* sc = ASR::down_cast<ASR::StructConstant_t>(previous);
        std::vector<ASR::symbol_t*> members = get_struct_constructor_info(dt_sym).members;
        if (sc->n_args != members.size()) {
            return nullptr;
        }
        // The values of each component, by its position in `members`
        std::map<size_t, std::vector<const DataComponentValue*>> components;
        for (size_t v = first; v < values.size(); v++) {
            ASR::expr_t* p = values[v]->parts[k];
            if (!ASR::is_a<ASR::StructInstanceMember_t>(*p)) {
                return nullptr;
            }
            auto it = std::find(members.begin(), members.end(),
                ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::StructInstanceMember_t>(p)->m_m));
            if (it == members.end()) {
                return nullptr;
            }
            components[std::distance(members.begin(), it)].push_back(values[v]);
        }
        Vec<ASR::call_arg_t> args;
        args.reserve(al, sc->n_args);
        for (size_t i = 0; i < sc->n_args; i++) {
            args.push_back(al, sc->m_args[i]);
        }
        for (const auto &component : components) {
            ASR::expr_t* component_value = data_component_updated_value(
                sc->m_args[component.first].m_value, component.second,
                k + 1, loc);
            if (component_value == nullptr) {
                return nullptr;
            }
            args.p[component.first].m_value = component_value;
        }
        return ASRUtils::EXPR(ASR::make_StructConstant_t(al, loc, sc->m_dt_sym,
            args.p, args.size(), sc->m_type));
    }
    // Elements of an array component, such as `x(j)` in `a(i)%x(j)`
    if (!ASR::is_a<ASR::ArrayItem_t>(*part)) {
        return nullptr;
    }
    ASR::ArrayItem_t* item = ASR::down_cast<ASR::ArrayItem_t>(part);
    ASR::ttype_t* array_type = ASRUtils::expr_type(item->m_v);
    if (ASRUtils::is_allocatable_or_pointer(array_type)) {
        return nullptr;
    }
    int64_t total_size = ASRUtils::get_fixed_size_of_array(array_type);
    // The values of each element, by its column-major position
    std::map<int64_t, std::vector<const DataComponentValue*>> array_elements;
    for (size_t v = first; v < values.size(); v++) {
        ASR::expr_t* p = values[v]->parts[k];
        if (!ASR::is_a<ASR::ArrayItem_t>(*p)) {
            return nullptr;
        }
        ASR::ArrayItem_t* element_item = ASR::down_cast<ASR::ArrayItem_t>(p);
        DataSectionBounds b;
        if (!resolve_data_section(element_item->m_v, element_item->m_args,
                element_item->n_args, b, p->base.loc) || b.size != 1) {
            return nullptr;
        }
        int64_t flat = 0, stride = 1;
        for (size_t d = 0; d < b.lb.size(); d++) {
            flat += (b.subscripts[d][0] - b.lb[d]) * stride;
            stride *= b.extent[d];
        }
        if (stride != total_size || flat < 0 || flat >= total_size) {
            return nullptr;
        }
        array_elements[flat].push_back(values[v]);
    }
    ASR::ttype_t* elem_type = ASRUtils::type_get_past_array(array_type);
    Vec<ASR::expr_t*> elements;
    elements.reserve(al, total_size);
    if (previous) {
        if (!ASR::is_a<ASR::ArrayConstant_t>(*previous)) {
            return nullptr;
        }
        ASR::ArrayConstant_t* ac = ASR::down_cast<ASR::ArrayConstant_t>(previous);
        if (ASRUtils::get_fixed_size_of_array(ac->m_type) != total_size) {
            return nullptr;
        }
        for (int64_t i = 0; i < total_size; i++) {
            elements.push_back(al, ASRUtils::fetch_ArrayConstant_value(al, ac, i));
        }
    } else {
        ASR::expr_t* zero = nullptr;
        if (ASR::is_a<ASR::StructType_t>(*elem_type)) {
            zero = data_default_struct_value(
                ASRUtils::get_struct_sym_from_struct_expr(item->m_v),
                elem_type, loc);
        } else if (ASR::is_a<ASR::Integer_t>(*elem_type)
                || ASR::is_a<ASR::Real_t>(*elem_type)
                || ASR::is_a<ASR::Complex_t>(*elem_type)
                || ASR::is_a<ASR::Logical_t>(*elem_type)) {
            zero = ASRUtils::get_constant_zero_with_given_type(al, elem_type);
        }
        if (zero == nullptr) {
            return nullptr;
        }
        for (int64_t i = 0; i < total_size; i++) {
            elements.push_back(al, zero);
        }
    }
    for (const auto &array_element : array_elements) {
        ASR::expr_t* element = data_component_updated_value(
            elements[array_element.first], array_element.second, k + 1, loc);
        if (element == nullptr) {
            return nullptr;
        }
        elements.p[array_element.first] = element;
    }
    ASR::expr_t* result = ASRUtils::EXPR(ASRUtils::make_ArrayConstructor_t_util(
        al, loc, elements.p, elements.size(), array_type,
        ASR::arraystorageType::ColMajor));
    return ASR::is_a<ASR::ArrayConstant_t>(*result) ? result : nullptr;
}

void CommonVisitorBase::set_data_component_value(ASR::ArrayItem_t* element,
        const std::vector<ASR::expr_t*>& parts, ASR::expr_t* object,
        ASR::expr_t* value, DataComponentElements& component_elements) {
    const Location& loc = object->base.loc;
    ASR::ttype_t* object_type = ASRUtils::expr_type(object);
    ImplicitCastRules::set_converted_value(al, loc, &value,
        ASRUtils::expr_type(value), object_type, diag);
    if (!ASRUtils::types_equal(ASRUtils::expr_type(value), object_type,
                value, object)) {
        diag.add(Diagnostic(
            "Type mismatch during data initialization",
            Level::Error, Stage::Semantic, {
                Label("", {loc})
            }));
        throw SemanticAbort();
    }
    ASR::expr_t* constant = ASRUtils::expr_value(value);
    // A character constant is padded with blanks or truncated to the
    // length of the component
    if (constant && ASR::is_a<ASR::StringConstant_t>(*constant)
            && ASR::is_a<ASR::String_t>(*object_type)) {
        ASR::String_t* object_str = ASR::down_cast<ASR::String_t>(object_type);
        ASR::String_t* value_str = ASR::down_cast<ASR::String_t>(
            ASRUtils::expr_type(constant));
        int64_t object_len = 0, value_len = 0;
        if (object_str->m_len && value_str->m_len
                && ASRUtils::extract_value(object_str->m_len, object_len)
                && ASRUtils::extract_value(value_str->m_len, value_len)
                && object_len != value_len) {
            constant = ASRUtils::adjust_character_length(constant,
                object_len, value_len, loc, al);
        }
    }
    ASR::Variable_t* arr_var = nullptr;
    int64_t flat = 0;
    if (constant == nullptr
            || !data_array_element_position(element, arr_var, flat)) {
        data_component_value_error(loc);
    }
    auto &elements = component_elements.elements[arr_var];
    if (elements.empty()) {
        component_elements.arrays.push_back(arr_var);
    }
    elements[flat].push_back({object, parts, constant});
}

[[noreturn]] void CommonVisitorBase::data_component_value_error(const Location& loc) {
    diag.add(Diagnostic(
        "this structure component of an array element cannot be "
        "initialized by a DATA implied do loop in a module yet",
        Level::Error, Stage::Semantic, {
            Label("", {loc})
        }));
    throw SemanticAbort();
}

void CommonVisitorBase::set_data_component_elements_value(
        DataComponentElements& component_elements, const Location& loc) {
    for (ASR::Variable_t* arr_var : component_elements.arrays) {
        const auto &elements = component_elements.elements[arr_var];
        std::vector<std::pair<int64_t, ASR::expr_t*>> values;
        if (data_array_initial_element(arr_var, 0) == nullptr) {
            ASR::ttype_t* elem_type = ASRUtils::type_get_past_array(
                ASRUtils::type_get_past_allocatable_pointer(arr_var->m_type));
            ASR::expr_t* default_value = data_default_struct_value(
                arr_var->m_type_declaration, elem_type, loc);
            int64_t total_size = ASRUtils::get_fixed_size_of_array(
                ASRUtils::type_get_past_allocatable_pointer(arr_var->m_type));
            for (int64_t k = 0; k < total_size; k++) {
                if (elements.find(k) == elements.end()) {
                    if (default_value == nullptr) {
                        data_component_value_error(loc);
                    }
                    values.push_back({k, default_value});
                }
            }
        }
        for (const auto &element : elements) {
            std::vector<const DataComponentValue*> element_values;
            for (const DataComponentValue &value : element.second) {
                element_values.push_back(&value);
            }
            ASR::expr_t* element_value = data_component_updated_value(
                data_array_initial_element(arr_var, element.first),
                element_values, 0, loc);
            if (element_value == nullptr) {
                data_component_value_error(element.second[0].object->base.loc);
            }
            values.push_back({element.first, element_value});
        }
        set_data_array_elements_value(arr_var, values, loc);
    }
}

bool CommonVisitorBase::set_data_substring_value(ASR::StringSection_t* section,
        ASR::expr_t* value, const Location &loc) {
    if (value == nullptr || !ASR::is_a<ASR::StringConstant_t>(*value)) {
        return false;
    }
    ASR::ttype_t* base_type = ASRUtils::expr_type(section->m_arg);
    if (ASRUtils::is_pointer(base_type) || ASRUtils::is_allocatable(base_type)
            || ASRUtils::is_array(base_type)
            || !ASR::is_a<ASR::String_t>(*ASRUtils::extract_type(base_type))) {
        return false;
    }
    ASR::String_t* str = ASR::down_cast<ASR::String_t>(
        ASRUtils::extract_type(base_type));
    int64_t length = 0, start = 1, end = 0, step = 1;
    if (str->m_kind != 1 || str->m_len == nullptr
            || !ASRUtils::extract_value(ASRUtils::expr_value(str->m_len), length)) {
        return false;
    }
    end = length;
    if ((section->m_start && !ASRUtils::extract_value(
                ASRUtils::expr_value(section->m_start), start))
            || (section->m_end && !ASRUtils::extract_value(
                ASRUtils::expr_value(section->m_end), end))
            || (section->m_step && !ASRUtils::extract_value(
                ASRUtils::expr_value(section->m_step), step))
            || step != 1 || start < 1 || end > length || start > end) {
        return false;
    }
    // The characters of a constant of default kind, which may include
    // null characters.
    auto characters_of = [](ASR::expr_t* e, std::string &text) {
        e = ASRUtils::expr_value(e);
        int64_t n = 0;
        if (e == nullptr || !ASR::is_a<ASR::StringConstant_t>(*e)
                || !ASRUtils::extract_value(ASRUtils::expr_value(
                    ASRUtils::get_string_type(ASRUtils::expr_type(e))->m_len), n)) {
            return false;
        }
        text = std::string(ASR::down_cast<ASR::StringConstant_t>(e)->m_s, n);
        return true;
    };
    std::string text;
    if (!characters_of(value, text)) {
        return false;
    }
    text.resize(end - start + 1, ' ');
    auto merged = [&](ASR::expr_t* previous) -> ASR::expr_t* {
        std::string whole;
        if (previous != nullptr && !characters_of(previous, whole)) {
            return nullptr;
        }
        whole.resize(length, ' ');
        whole.replace(start - 1, end - start + 1, text);
        return ASRUtils::EXPR(ASR::make_StringConstant_t(al, loc,
            s2c(al, whole), base_type));
    };
    if (ASR::is_a<ASR::ArrayItem_t>(*section->m_arg)) {
        return set_data_array_element_value(
            ASR::down_cast<ASR::ArrayItem_t>(section->m_arg), merged, loc);
    }
    if (!ASR::is_a<ASR::Var_t>(*section->m_arg)) {
        return false;
    }
    ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(section->m_arg)->m_v);
    if (!ASR::is_a<ASR::Variable_t>(*sym)) {
        return false;
    }
    ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sym);
    ASR::expr_t* whole = merged(v->m_value);
    if (whole == nullptr) {
        return false;
    }
    set_data_initial_value(v, whole);
    return true;
}

int64_t CommonVisitorBase::count_implied_do_loop_elements(ASR::ImpliedDoLoop_t* implied_do_loop, bool& can_validate) {
    ASR::expr_t* start_expr_value = ASRUtils::expr_value(implied_do_loop->m_start);
    ASR::expr_t* end_expr_value = ASRUtils::expr_value(implied_do_loop->m_end);
    ASR::expr_t* increment_expr = implied_do_loop->m_increment;

    if (!start_expr_value || !end_expr_value) {
        can_validate = false;
        return 0;
    }
    if (!ASR::is_a<ASR::IntegerConstant_t>(*start_expr_value) ||
        !ASR::is_a<ASR::IntegerConstant_t>(*end_expr_value)) {
        can_validate = false;
        return 0;
    }
    int64_t increment = 1;
    if (increment_expr) {
        ASR::expr_t* increment_value = ASRUtils::expr_value(increment_expr);
        if (!increment_value || !ASR::is_a<ASR::IntegerConstant_t>(*increment_value)) {
            can_validate = false;
            return 0;
        }
        increment = ASR::down_cast<ASR::IntegerConstant_t>(increment_value)->m_n;
    }

    int64_t start = ASR::down_cast<ASR::IntegerConstant_t>(start_expr_value)->m_n;
    int64_t end = ASR::down_cast<ASR::IntegerConstant_t>(end_expr_value)->m_n;
    int64_t iterations = 0;
    if (increment > 0) {
        iterations = (end - start) / increment + 1;
        if (iterations < 0) iterations = 0;
    } else {
        can_validate = false;
        return 0;
    }

    // Count elements per iteration (may contain nested implied do loops)
    int64_t elements_per_iteration = 0;
    for (size_t i = 0; i < implied_do_loop->n_values; i++) {
        ASR::expr_t* value = implied_do_loop->m_values[i];
        if (ASR::is_a<ASR::ImpliedDoLoop_t>(*value)) {
            elements_per_iteration += count_implied_do_loop_elements(
                ASR::down_cast<ASR::ImpliedDoLoop_t>(value), can_validate);
            if (!can_validate) return 0;
        } else {
            elements_per_iteration += 1;
        }
    }
    return iterations * elements_per_iteration;
}

ASR::Variable_t* CommonVisitorBase::data_stmt_object_parameter(ASR::expr_t* expr) {
    if (!expr) return nullptr;
    if (ASR::is_a<ASR::Var_t>(*expr)) {
        ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(expr)->m_v);
        if (ASR::is_a<ASR::Variable_t>(*sym)) {
            ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(sym);
            return v->m_storage == ASR::storage_typeType::Parameter ? v
                                                                   : nullptr;
        }
        return nullptr;
    }
    if (ASR::is_a<ASR::StructInstanceMember_t>(*expr)) {
        return data_stmt_object_parameter(
            ASR::down_cast<ASR::StructInstanceMember_t>(expr)->m_v);
    }
    if (ASR::is_a<ASR::ArrayItem_t>(*expr)) {
        return data_stmt_object_parameter(
            ASR::down_cast<ASR::ArrayItem_t>(expr)->m_v);
    }
    if (ASR::is_a<ASR::ImpliedDoLoop_t>(*expr)) {
        ASR::ImpliedDoLoop_t* idl = ASR::down_cast<ASR::ImpliedDoLoop_t>(expr);
        for (size_t i = 0; i < idl->n_values; i++) {
            ASR::Variable_t* v = data_stmt_object_parameter(idl->m_values[i]);
            if (v) return v;
        }
    }
    return nullptr;
}

ASR::CommonBlock_t* CommonVisitorBase::get_or_create_common_block(const std::string &name,
        const Location &loc) {
    std::string key = common_block_key(name);
    if (ASR::symbol_t *sym = current_scope->get_symbol(key)) {
        return ASR::down_cast<ASR::CommonBlock_t>(sym);
    }
    // The storage keeps the global name it had when a common block was
    // a variable of a module generated for it, so that object files
    // compiled before still link with new ones.
    std::string spelled = name.empty() ? "blank#block" : name;
    std::string link_name = "__module_file_common_block_" + spelled
        + "_struct_instance_" + spelled;
    ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(
        ASR::make_CommonBlock_t(al, loc, current_scope, s2c(al, key),
            name.empty() ? nullptr : s2c(al, name), s2c(al, link_name),
            nullptr, 0, nullptr, 0, ASR::abiType::Source,
            is_global_save_enabled ? ASR::storage_typeType::Save
                : ASR::storage_typeType::Default,
            ASR::common_block_roleType::Tentative));
    current_scope->add_symbol(key, sym);
    return ASR::down_cast<ASR::CommonBlock_t>(sym);
}

void CommonVisitorBase::check_common_block_object(const ASR::Variable_t &v,
        const Location &loc) {
    std::string name = v.m_name;
    auto reject = [&](const std::string &message) {
        diag.add(Diagnostic(message, Level::Error, Stage::Semantic, {
            Label("", {loc})}));
        throw SemanticAbort();
    };
    if (v.m_common_block != nullptr) {
        reject("variable '" + name + "' is already in a common block");
    }
    if (v.m_intent != ASR::intentType::Local) {
        reject("dummy argument '" + name + "' cannot be in a common block");
    }
    if (v.m_storage == ASR::storage_typeType::Parameter) {
        reject("named constant '" + name + "' cannot be in a common block");
    }
    if (ASRUtils::is_allocatable(v.m_type)) {
        reject("allocatable variable '" + name + "' cannot be in a "
            "common block");
    }
    if (v.n_codims > 0) {
        reject("coarray '" + name + "' cannot be in a common block");
    }
    if (v.m_abi == ASR::abiType::BindC || v.m_bindc_name != nullptr) {
        reject("variable '" + name + "' with the BIND attribute cannot "
            "be in a common block");
    }
    if (v.m_storage == ASR::storage_typeType::Threadprivate) {
        reject("threadprivate variable '" + name + "' in a common block "
            "is not supported yet");
    }
    if (ASRUtils::is_pointer(v.m_type)) {
        return;
    }
    if (ASRUtils::is_array(v.m_type)
            && !ASRUtils::is_fixed_size_array(v.m_type)) {
        reject("COMMON block array bounds must be constant expressions");
    }
    if (ASRUtils::is_character(*v.m_type)) {
        int64_t length;
        ASR::expr_t *len = ASRUtils::get_string_type(v.m_type)->m_len;
        if (len == nullptr || !ASRUtils::extract_value(
                ASRUtils::expr_value(len), length)) {
            reject("character length of '" + name + "' in a common "
                "block must be a constant expression");
        }
    }
    if (ASR::is_a<ASR::StructType_t>(*ASRUtils::type_get_past_array(
            v.m_type))) {
        ASR::symbol_t *decl = v.m_type_declaration
            ? ASRUtils::symbol_get_past_external(v.m_type_declaration)
            : nullptr;
        if (decl == nullptr || !ASR::is_a<ASR::Struct_t>(*decl) ||
                !(ASR::down_cast<ASR::Struct_t>(decl)->m_is_sequence ||
                  ASR::down_cast<ASR::Struct_t>(decl)->m_abi ==
                    ASR::abiType::BindC)) {
            reject("derived type variable '" + name + "' in a common "
                "block must have the SEQUENCE or the BIND(C) attribute");
        }
    }
}

ASR::Variable_t * CommonVisitorBase::get_symtab_var_for_common(AST::var_sym_t const &s) {
	std::string const sym {to_lower(s.m_name)};
	ASR::symbol_t *get_sym = current_scope->get_symbol(sym);
	if (get_sym == nullptr) {
	    if (compiler_options.implicit_typing) {
		ASR::intentType intent = ASRUtils::intent_local;
		get_sym = declare_implicit_variable2(s.loc, sym, intent, implicit_dictionary[std::string(1,sym[0])]);
	    } else {
		diag.add(Diagnostic(
			     "Cannot implicitly declare variable in COMMON block",
			     Level::Error, Stage::Semantic, {
				 Label("",{s.loc})
			     }));
		throw SemanticAbort();
	    }
	}

	ASR::Variable_t* var_{nullptr};
	if (ASR::is_a<ASR::Variable_t>(*get_sym)) {
	    var_ = ASR::down_cast<ASR::Variable_t>(get_sym);
	} else {
	    diag.add(Diagnostic(
			 "Can only add variables to COMMON blocks",
			 Level::Error, Stage::Semantic, {
			     Label("",{s.loc})
			 }));
	    throw SemanticAbort();
	}
	return var_;
}

void CommonVisitorBase::reject_common_block_function_result(ASR::Variable_t &result) {
    ASR::CommonBlock_t *block = ASRUtils::get_common_block(&result);
    diag.add(Diagnostic("function result '" + std::string(result.m_name)
        + "' cannot be in a common block", Level::Error, Stage::Semantic,
        {Label("", {result.base.base.loc})}));
    for (size_t i = 0; i < block->n_members; i++) {
        ASR::down_cast<ASR::Variable_t>(block->m_members[i])->m_common_block = nullptr;
    }
    for (size_t i = 0; i < block->n_associations; i++) {
        ASR::down_cast<ASR::Variable_t>(
            block->m_associations[i].m_variable)->m_common_block = nullptr;
    }
    block->m_parent_symtab->erase_symbol(block->m_name);
    if (!compiler_options.continue_compilation) {
        throw SemanticAbort();
    }
}

bool CommonVisitorBase::common_statement_names(AST::decl_stmt_t **items,
        size_t n_items, const std::string &name) {
    for (size_t i = 0; i < n_items; i++) {
        if (!AST::is_a<AST::Declaration_t>(*items[i])) continue;
        AST::Declaration_t *d = AST::down_cast<AST::Declaration_t>(items[i]);
        for (size_t j = 0; j < d->n_attributes; j++) {
            if (!AST::is_a<AST::AttrCommon_t>(*d->m_attributes[j])) continue;
            AST::AttrCommon_t *c = AST::down_cast<AST::AttrCommon_t>(
                d->m_attributes[j]);
            for (size_t b = 0; b < c->n_blks; b++) {
                if ((c->m_blks[b].m_name ? to_lower(c->m_blks[b].m_name)
                        : std::string()) == name) {
                    return true;
                }
            }
        }
    }
    return false;
}

bool CommonVisitorBase::is_common_equivalence_set(const AST::equi_t &set) {
    for (size_t m = 0; m < set.n_set_list; m++) {
        ASR::symbol_t *sym = current_scope->get_symbol(
            equivalence_object_name(*set.m_set_list[m]));
        if (sym != nullptr && ASR::is_a<ASR::Variable_t>(*sym) &&
                ASR::down_cast<ASR::Variable_t>(sym)->m_common_block) {
            return true;
        }
    }
    return false;
}

std::string CommonVisitorBase::equivalence_object_name(const AST::expr_t &object) {
    if (AST::is_a<AST::Name_t>(object)) {
        return to_lower(AST::down_cast<AST::Name_t>(&object)->m_id);
    }
    if (AST::is_a<AST::FuncCallOrArray_t>(object)) {
        return to_lower(AST::down_cast<AST::FuncCallOrArray_t>(&object)->m_func);
    }
    return "";
}

bool CommonVisitorBase::is_use_associated(const std::string &name) {
    ASR::symbol_t *sym = current_scope->get_symbol(name);
    if (sym == nullptr || !ASR::is_a<ASR::ExternalSymbol_t>(*sym)) {
        return false;
    }
    ASR::symbol_t *entity = ASRUtils::symbol_get_past_external(sym);
    ASR::symbol_t *owner = ASRUtils::get_asr_owner(entity);
    if (owner != nullptr && ASR::is_a<ASR::Enum_t>(*owner) &&
            ASRUtils::symbol_parent_symtab(owner) == current_scope) {
        return false;
    }
    ASR::asr_t *unit = current_scope->asr_owner;
    if (unit != nullptr && ASR::is_a<ASR::symbol_t>(*unit) &&
            ASR::is_a<ASR::Module_t>(*ASR::down_cast<ASR::symbol_t>(unit))) {
        char *parent = ASR::down_cast2<ASR::Module_t>(unit)->m_parent_module;
        if (parent != nullptr) {
            ASR::symbol_t *inherited = ASR::down_cast<ASR::Module_t>(
                current_scope->get_tu_scope()->get_symbol(parent))
                ->m_symtab->get_symbol(name);
            if (inherited != nullptr &&
                    ASRUtils::symbol_get_past_external(inherited) == entity) {
                return false;
            }
        }
    }
    return true;
}

void CommonVisitorBase::check_equivalence_objects(const AST::AttrEquivalence_t &eq) {
    for (size_t i = 0; i < eq.n_args; i++) {
        const AST::expr_t *object = parenthesized_equivalence_object(
            eq.m_args[i]);
        if (object != nullptr) {
            diag.semantic_error_label("an equivalence object must be a "
                "variable, an array element or a substring",
                {object->base.loc}, "not allowed in EQUIVALENCE");
            throw SemanticAbort();
        }
        object = use_associated_equivalence_object(eq.m_args[i]);
        if (object != nullptr) {
            diag.semantic_error_label("'" + equivalence_object_name(*object)
                + "' is use-associated and cannot appear in an "
                "EQUIVALENCE statement", {object->base.loc},
                "not allowed in EQUIVALENCE");
            throw SemanticAbort();
        }
    }
}

int64_t CommonVisitorBase::target_independent_element_size(ASR::ttype_t *type) {
    type = ASRUtils::type_get_past_array(type);
    int64_t kind = ASRUtils::extract_kind_from_ttype_t(type);
    switch (type->type) {
        case ASR::ttypeType::Integer:
        case ASR::ttypeType::Logical:
        case ASR::ttypeType::Real:
            return kind == 1 || kind == 2 || kind == 4 || kind == 8
                ? kind : -1;
        case ASR::ttypeType::Complex:
            return kind == 4 || kind == 8 ? 2 * kind : -1;
        case ASR::ttypeType::String: {
            int64_t len;
            ASR::String_t *s = ASR::down_cast<ASR::String_t>(type);
            return s->m_kind == 1 && s->m_len && ASRUtils::extract_value(
                ASRUtils::expr_value(s->m_len), len) ? len : -1;
        }
        default:
            return -1;
    }
}

void CommonVisitorBase::check_common_block_start(ASR::CommonBlock_t *block,
        ASR::Variable_t *anchor, int64_t anchor_element,
        int64_t anchor_character, ASR::Variable_t *variable,
        int64_t element, int64_t character, const Location &loc) {
    if (block->n_members == 0 || block->m_members[0] != &anchor->base) {
        return;
    }
    auto position = [](ASR::Variable_t *v, int64_t e, int64_t c) {
        int64_t size = target_independent_element_size(v->m_type);
        return size < 0 ? int64_t(-1) : e * size + c;
    };
    int64_t in_anchor = position(anchor, anchor_element, anchor_character);
    int64_t in_variable = position(variable, element, character);
    if (in_anchor < 0 || in_variable < 0) return;
    if (in_anchor < in_variable) {
        std::string name = block->m_block_name
            ? "common block /" + std::string(block->m_block_name) + "/"
            : std::string("blank common");
        diag.add(Diagnostic("EQUIVALENCE cannot extend " + name +
            " before its first storage unit", Level::Error,
            Stage::Semantic, {Label("", {loc})}));
        throw SemanticAbort();
    }
}

ASR::Variable_t* CommonVisitorBase::equivalence_common_object(ASR::expr_t *designator,
        const Location &loc) {
    ASR::expr_t *base = designator;
    if (ASR::is_a<ASR::StringSection_t>(*base)) {
        base = ASR::down_cast<ASR::StringSection_t>(base)->m_arg;
    } else if (ASR::is_a<ASR::StringItem_t>(*base)) {
        base = ASR::down_cast<ASR::StringItem_t>(base)->m_arg;
    }
    if (ASR::is_a<ASR::ArrayItem_t>(*base)) {
        base = ASR::down_cast<ASR::ArrayItem_t>(base)->m_v;
    }
    if (ASR::is_a<ASR::Var_t>(*base) && ASR::is_a<ASR::Variable_t>(
            *ASR::down_cast<ASR::Var_t>(base)->m_v)) {
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(
            ASR::down_cast<ASR::Var_t>(base)->m_v);
        if (ASRUtils::is_pointer(v->m_type) ||
                ASRUtils::is_allocatable(v->m_type)) {
            diag.semantic_error_label("a pointer or an allocatable "
                "variable cannot be an equivalence object", {loc},
                "not allowed in EQUIVALENCE");
            throw SemanticAbort();
        }
        if (v->m_parent_symtab != current_scope) {
            diag.semantic_error_label("an equivalence object must be a "
                "local variable", {loc}, "not allowed in EQUIVALENCE");
            throw SemanticAbort();
        }
        int64_t element, character;
        if (ASRUtils::get_storage_designator_offset(designator, element,
                character) == v) {
            return v;
        }
    } else {
        diag.semantic_error_label("an equivalence object must be a "
            "variable, an array element or a substring", {loc},
            "not allowed in EQUIVALENCE");
        throw SemanticAbort();
    }
    diag.semantic_error_label(
        "equivalence array bounds and subscripts must be constant",
        {loc}, "unsupported equivalence");
    throw SemanticAbort();
}

bool CommonVisitorBase::take_saved_before_declaration(const std::string &name) {
    auto end = std::remove_if(saved_before_declaration.begin(),
        saved_before_declaration.end(),
        [&](const SavedBeforeDeclaration &s) {
            return s.scope == current_scope && s.name == name;
        });
    bool found = end != saved_before_declaration.end();
    saved_before_declaration.erase(end, saved_before_declaration.end());
    return found;
}

void CommonVisitorBase::report_saved_without_declaration() {
    for (const SavedBeforeDeclaration &s : saved_before_declaration) {
        std::string msg = "Save `" + s.name + "` has no IMPLICIT Type";
        if (!compiler_options.implicit_typing) {
            msg += ", use `--implicit-typing`";
        }
        diag.add(Diagnostic(msg, Level::Error, Stage::Semantic, {
            Label("", {s.loc})}));
    }
    saved_before_declaration.clear();
}

void CommonVisitorBase::make_optional_procedure_dummy(const std::string &sym,
        ASR::Function_t* func, const Location &attr_loc) {
    ASR::ttype_t* proc_type = func->m_function_signature;
    SetChar variable_dependencies_vec;
    variable_dependencies_vec.reserve(al, 1);
    ASRUtils::collect_variable_dependencies(
        al, variable_dependencies_vec, proc_type);
    SymbolTable* parent_scope = current_scope->parent;
    LCOMPILERS_ASSERT(parent_scope != nullptr);
    std::string iface_name = "~proc_" + sym + "_" +
        current_scope->get_counter();
    func->m_name = s2c(al, iface_name);
    func->m_symtab->parent = parent_scope;
    ASR::symbol_t* iface = &func->base;
    parent_scope->add_symbol(iface_name, iface);
    ASR::asr_t* proc_var = ASRUtils::make_Variable_t_util(
        al, attr_loc, current_scope,
        s2c(al, sym),
        variable_dependencies_vec.p,
        variable_dependencies_vec.size(),
        ASRUtils::intent_unspecified, nullptr,
        nullptr, ASR::storage_typeType::Default,
        proc_type, iface,
        ASR::abiType::Source,
        ASR::accessType::Public,
        ASR::presenceType::Optional, false);
    ASR::symbol_t* proc_var_sym =
        ASR::down_cast<ASR::symbol_t>(proc_var);
    current_scope->add_or_overwrite_symbol(sym, proc_var_sym);
    if (current_scope->asr_owner &&
        ASR::is_a<ASR::symbol_t>(*current_scope->asr_owner)) {
        ASR::symbol_t* asr_owner_sym =
            ASR::down_cast<ASR::symbol_t>(current_scope->asr_owner);
        if (ASR::is_a<ASR::Function_t>(*asr_owner_sym)) {
            ASR::Function_t* current_function =
                ASR::down_cast<ASR::Function_t>(asr_owner_sym);
            for (size_t j = 0; j < current_function->n_args; j++) {
                if (ASR::is_a<ASR::Var_t>(*current_function->m_args[j])) {
                    ASR::Var_t* var =
                        ASR::down_cast<ASR::Var_t>(current_function->m_args[j]);
                    if (std::string(ASRUtils::symbol_name(var->m_v)) == sym) {
                        var->m_v = proc_var_sym;
                    }
                }
            }
        }
    }
}

void CommonVisitorBase::create_external_function(std::string sym, Location loc, ASR::ttype_t* determined_type) {
    if (compiler_options.implicit_interface) {
        bool is_subroutine = false;
        external_procedures.push_back(sym);
        ASR::symbol_t *sym_ = current_scope->resolve_symbol(sym);
        ASR::symbol_t *local_sym = current_scope->get_symbol(sym);
        // The EXTERNAL attribute makes `sym` a local name of this scope,
        // which hides a generic interface accessible from the host (F2023
        // 19.5.1.4). Such a generic does not define the characteristics of
        // the external procedure.
        if (sym_ && !local_sym && ASR::is_a<ASR::GenericProcedure_t>(
                *ASRUtils::symbol_get_past_external(sym_))) {
            sym_ = nullptr;
        }
        bool is_optional_dummy = (local_sym &&
            ASR::is_a<ASR::Variable_t>(*local_sym) &&
            ASR::down_cast<ASR::Variable_t>(local_sym)->m_presence ==
                ASR::presenceType::Optional) ||
            (!local_sym && assgnd_presence.count(sym) &&
            assgnd_presence[sym] == ASR::presenceType::Optional &&
            std::find(current_procedure_args.begin(),
                current_procedure_args.end(), sym)
                != current_procedure_args.end());
        // Respect the default accessibility of the enclosing scope (e.g. a
        // module `private` statement) as well as any explicit access already
        // assigned to this name. An external procedure declared in a
        // `private` module must be private, otherwise `use` would wrongly
        // import it and later re-declarations (e.g. via an F77 `include`)
        // would be reported as bogus redeclarations.
        ASR::accessType ext_access = dflt_access;
        if (assgnd_access.find(sym) != assgnd_access.end()) {
            ext_access = assgnd_access[sym];
        }
        // Do not record `ext_access` back into `assgnd_access`: that map is
        // never cleared between program units, so a `private` module would
        // make the same-named external private in every later program unit
        // of the same file.
        if (assgnd_pointer.count(sym) > 0) {
            // Without a declared type the pointer can be associated with
            // a subroutine or with a function of any type, and a function
            // referenced through it has its implicit type.
            ASR::ttype_t *func_type = ASRUtils::make_opaque_procedure_type(
                al, loc, determined_type);
            std::string iface_name = "__" + sym + "_iface_implicit";
            SymbolTable *parent_scope = current_scope->parent;
            if (!parent_scope) parent_scope = current_scope;
            ASR::symbol_t *iface_sym = parent_scope->get_symbol(iface_name);
            if (!iface_sym) {
                iface_sym = make_opaque_procedure_interface(loc, parent_scope,
                    iface_name, func_type, ASR::accessType::Private);
            }
            ASR::ttype_t *ptr_type = ASRUtils::TYPE(
                ASR::make_Pointer_t(al, loc, func_type));
            SetChar variable_dependencies_vec;
            variable_dependencies_vec.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(
                al, variable_dependencies_vec, ptr_type);
            ASR::asr_t *var = ASRUtils::make_Variable_t_util(al, loc,
                current_scope, s2c(al, sym),
                variable_dependencies_vec.p,
                variable_dependencies_vec.size(),
                ASR::intentType::Local, nullptr, nullptr,
                ASR::storage_typeType::Default, ptr_type, iface_sym,
                ASR::abiType::Source, ext_access,
                ASR::presenceType::Required, false);
            current_scope->add_or_overwrite_symbol(
                sym, ASR::down_cast<ASR::symbol_t>(var));
            assgnd_pointer.erase(sym);
            return;
        }
        SymbolTable *parent_scope = current_scope;
        current_scope = al.make_new<SymbolTable>(parent_scope);
        ASR::ttype_t *type = nullptr;
        if (determined_type) {
            // if explicit type provided, give preference to it.
            type = determined_type;
            if (sym_ && ASR::is_a<ASR::Function_t>(*sym_)) {
                ASR::Function_t* func_sym = ASR::down_cast<ASR::Function_t>(sym_);
                if (!func_sym->m_return_var) {
                    is_subroutine = true;
                }
            }
        } else if (sym_) {
            if (ASR::is_a<ASR::Function_t>(*sym_)) {
                ASR::Function_t* func_sym = ASR::down_cast<ASR::Function_t>(sym_);
                if (!func_sym->m_return_var) {
                    is_subroutine = true;
                }
            }
            if (!is_subroutine) {
                type = ASRUtils::symbol_type(sym_);
            }
        } else if (compiler_options.implicit_typing) {
            type = implicit_dictionary[std::string(1,sym[0])];
            if (!type) {
                is_subroutine = true;
            }
        } else {
            is_subroutine = true;
        }
        // add return var
        std::string return_var_name = sym + "_return_var_name";
        SetChar variable_dependencies_vec;
        variable_dependencies_vec.reserve(al, 1);
        ASR::asr_t *return_var = nullptr;
        ASR::expr_t *to_return = nullptr;
        if (!is_subroutine) {
            ASRUtils::collect_variable_dependencies(al, variable_dependencies_vec, type);
            return_var = ASRUtils::make_Variable_t_util(al, loc,
                current_scope, s2c(al, return_var_name), variable_dependencies_vec.p,
                variable_dependencies_vec.size(), ASRUtils::intent_return_var,
                nullptr, nullptr, ASR::storage_typeType::Default, type, nullptr,
                ASR::abiType::Source, ASR::Public, ASR::presenceType::Required,
                false);
            current_scope->add_symbol(return_var_name, ASR::down_cast<ASR::symbol_t>(return_var));
            to_return = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                ASR::down_cast<ASR::symbol_t>(return_var)));
        }
        tmp = ASRUtils::make_Function_t_util(
            al, loc,
            /* a_symtab */ current_scope,
            /* a_name */ s2c(al, sym),
            nullptr, 0,
            /* a_args */ nullptr,
            /* n_args */ 0,
            /* a_body */ nullptr,
            /* n_body */ 0,
            /* a_return_var */ to_return,
            ASR::abiType::Source, ext_access,
            ASR::deftypeType::ImplicitInterface,
            nullptr, false, false, false, false, false, nullptr, 0,
            false, false, false);
        current_scope = parent_scope;
        if (is_optional_dummy) {
            make_optional_procedure_dummy(sym,
                ASR::down_cast2<ASR::Function_t>(tmp), loc);
            return;
        }
        // The type that EXTERNAL or an earlier type declaration statement
        // declares, not the type of a procedure accessible from the host.
        if (local_sym == nullptr || ASR::is_a<ASR::Variable_t>(*local_sym)) {
            ASR::ttype_t *declared_type = determined_type;
            if (declared_type == nullptr && local_sym != nullptr) {
                declared_type = ASRUtils::symbol_type(local_sym);
            }
            if (bind_external_to_cell_procedure(sym, declared_type)) {
                if (local_sym != nullptr) parent_scope->erase_symbol(sym);
                return;
            }
        }
        parent_scope->add_or_overwrite_symbol(sym, ASR::down_cast<ASR::symbol_t>(tmp));
    } else {
        diag.add(Diagnostic(
            "function interface must be specified explicitly; you can enable implicit interfaces with `--implicit-interface`",
            Level::Error, Stage::Semantic, {
                Label("",{loc})
            }));
        throw SemanticAbort();
    }
}

} // namespace LCompilers::LFortran

// Member functions of CommonVisitorBase, in the order of their declarations
// (split over ast_common_visitor_base*.cpp to compile in parallel).

#include <lfortran/semantics/ast_common_visitor.h>

namespace LCompilers::LFortran {

ASR::symbol_t* CommonVisitorBase::resolve_intrinsic_function(const Location &loc, const std::string &remote_sym,
        bool unmatched_generic) {
    if (!intrinsic_procedures.is_intrinsic(remote_sym)) {
        check_hidden_derived_type_component_reference(loc, remote_sym);
        // An undeclared name becomes a procedure with an implicit
        // interface, which is filed in the enclosing program unit. A
        // reference made directly in the top-level scope of an
        // interactive cell, whose owner is the cell's translation unit,
        // has no program unit to file it in (filing it in the
        // translation unit crashes), so it is reported as not found. A
        // reference in any other scope, such as a top-level BLOCK or
        // ASSOCIATE, keeps the fallback, and the JIT linker resolves
        // the procedure.
        SymbolTable* reference_scope = statement_function_parent_scope
            ? statement_function_parent_scope : current_scope;
        bool in_cell_top_level = compiler_options.interactive
            && ASRUtils::is_tu_scope(reference_scope);
        if (compiler_options.implicit_interface && !unmatched_generic
                && !in_cell_top_level) {
            return nullptr;
        } else {
            is_coarray_or_atomic(remote_sym, loc);
            diag.add(Diagnostic(unmatched_generic
                ? "no specific procedure of the generic '" + remote_sym
                    + "' matches the arguments"
                : "Function '" + remote_sym + "' not found"
                    " (not user defined nor intrinsic)",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }
    }
    std::string module_name = intrinsic_procedures.get_module(remote_sym, loc, diag);

    if (module_name == "lfortran_intrinsic_ieee_arithmetic") {
        diag.add(Diagnostic(
            "'" + remote_sym + "' is not accessible in the current scope; "
            "add 'use, intrinsic :: ieee_arithmetic' to use it",
            Level::Error, Stage::Semantic, {Label("not in scope", {loc})}));
        throw SemanticAbort();
    }

    SymbolTable *tu_symtab = ASRUtils::get_tu_symtab(current_scope);
    std::set<std::string> empty_set;
    ASR::Module_t *m = ASRUtils::load_module(al, tu_symtab, module_name,
            loc, true, empty_set, compiler_options.po, !diag.has_error(),
            [&](const std::string &msg, const Location &loc) {
                    diag.add(Diagnostic(msg, Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                }, lm, compiler_options.separate_compilation
            );

    ASR::symbol_t *t = m->m_symtab->resolve_symbol(remote_sym);
    if (!t) {
        diag.add(Diagnostic("The symbol '" + remote_sym
            + "' not found in the module '" + module_name + "'",
            Level::Error, Stage::Semantic, {Label("", {loc})}));
        throw SemanticAbort();
    } else if (! (ASR::is_a<ASR::GenericProcedure_t>(*t)
                || ASR::is_a<ASR::Function_t>(*t)
                )) {
        diag.add(Diagnostic("The symbol '" + remote_sym
            + "' found in the module '" + module_name + "', "
            + "but it is not a function, subroutine or a generic procedure.",
            Level::Error, Stage::Semantic, {Label("", {loc})}));
        throw SemanticAbort();
    }
    char *fn_name = ASRUtils::symbol_name(t);
    append_module_dependency(m->m_name);
    if (ASR::is_a<ASR::GenericProcedure_t>(*t)) {
        import_generic(t, fn_name);
        return current_scope->get_symbol(fn_name);
    }
    ASR::asr_t *fn = ASR::make_ExternalSymbol_t(
        al, t->base.loc,
        /* a_symtab */ current_scope,
        /* a_name */ fn_name,
        t,
        m->m_name, nullptr, 0, fn_name,
        ASR::accessType::Private
        );
    std::string sym = fn_name;

    current_scope->add_or_overwrite_symbol(sym, ASR::down_cast<ASR::symbol_t>(fn));
    return ASR::down_cast<ASR::symbol_t>(fn);
}

bool CommonVisitorBase::assignment_types_agree(ASR::ttype_t *target, ASR::ttype_t *value) {
    // For now we will just check basic type mismatch
    if (target->type == value->type) {
        return true;
    }
    if (is_integer(*target) && is_integer(*value)) {
        return true;
    }
    if (is_real(*target) && is_real(*value)) {
        return true;
    }
    return false;
}

ASR::expr_t* CommonVisitorBase::visit_BinOp_helper(ASR::expr_t* left, ASR::expr_t* right, ASR::binopType op, const Location& loc, ASR::ttype_t* dest_type) {
    bool division_by_zero = false;
    ASR::expr_t* value = ASRUtils::fold_binop_constants(al, left, right,
        op, loc, dest_type, division_by_zero);
    if (division_by_zero) {
        diag.add(Diagnostic(
            "Division by zero",
            Level::Error, Stage::Semantic, {
                Label("", {loc})
            })
        );
        throw SemanticAbort();
    }
    return value;
}

ASR::expr_t* CommonVisitorBase::extract_value(ASR::expr_t* left_value, ASR::expr_t* right_value, ASR::binopType op, ASR::ttype_t* dest_type, const Location& loc) {
    if (left_value && right_value &&
        ASR::is_a<ASR::ArrayConstant_t>(*left_value) &&
        ASR::is_a<ASR::ArrayConstant_t>(*right_value)) {
        ASR::ArrayConstant_t* left_array = ASR::down_cast<ASR::ArrayConstant_t>(left_value);
        ASR::ArrayConstant_t* right_array = ASR::down_cast<ASR::ArrayConstant_t>(right_value);

        Vec<ASR::expr_t*> values; values.reserve(al, ASRUtils::get_fixed_size_of_array(left_array->m_type));

        for (size_t i = 0; i < (size_t) ASRUtils::get_fixed_size_of_array(left_array->m_type); i++) {
            ASR::expr_t* element = visit_BinOp_helper(ASRUtils::fetch_ArrayConstant_value(al, left_array, i),
                            ASRUtils::fetch_ArrayConstant_value(al, right_array, i), op, loc, ASRUtils::expr_type(ASRUtils::fetch_ArrayConstant_value(al, left_array, i)));
            if (element == nullptr) {
                // An element that cannot be folded leaves the array
                // to be evaluated at run time
                return nullptr;
            }
            values.push_back(al, element);
        }

        ASR::ttype_t* value_type = ASRUtils::type_get_past_allocatable_pointer(dest_type);
        value_type = ASRUtils::type_get_past_pointer(value_type);
        LCOMPILERS_ASSERT(ASRUtils::is_array(value_type));

        ASR::Array_t* value_array_type = ASR::down_cast<ASR::Array_t>(value_type);
        void* data = ASRUtils::set_ArrayConstant_data(values.p, values.size(), value_array_type->m_type);

        return ASRUtils::EXPR(ASRUtils::make_ArrayConstant_t_util(al, loc,
                                data, value_type,
                                ASR::arraystorageType::ColMajor));
    }
    return nullptr;
}

void CommonVisitorBase::visit_BinOp2(Allocator &al, const AST::BinOp_t &x,
                ASR::expr_t *&left, ASR::expr_t *&right,
                ASR::asr_t *&asr, std::string& intrinsic_op_name,
                SymbolTable* curr_scope) {
    ASR::binopType op;
    switch (x.m_op) {
        case (AST::Add):
            op = ASR::Add;
            break;
        case (AST::Sub):
            op = ASR::Sub;
            break;
        case (AST::Mul):
            op = ASR::Mul;
            break;
        case (AST::Div):
            op = ASR::Div;
            break;
        case (AST::Pow):
            op = ASR::Pow;
            break;
        // Fix compiler warning:
        default: {
            LCOMPILERS_ASSERT(false);
            op = ASR::binopType::Pow;
        }
    }

    // Cast LHS or RHS if necessary
    ASR::ttype_t *left_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(left));
    ASR::ttype_t *right_type = ASRUtils::type_get_past_allocatable(ASRUtils::expr_type(right));
    ASR::expr_t *overloaded = nullptr;
    if ( ASRUtils::use_overloaded(left, right, op,
                intrinsic_op_name, curr_scope, asr, al,
                x.base.base.loc, current_function_dependencies,
                current_module_dependencies,
                [&](const std::string &msg, const Location &loc)
                {
                    diag.add(Diagnostic(msg, Level::Error, Stage::Semantic, {Label("", {loc})}));
                    throw SemanticAbort();
                }) ) {
        overloaded = ASRUtils::EXPR(asr);
    }

    ASR::expr_t **conversion_cand = &left;
    ASR::ttype_t *source_type = left_type;
    ASR::ttype_t *dest_type = right_type;

    if( overloaded == nullptr ) {
        // Only an intrinsic type has intrinsic arithmetic. An operand of
        // a derived type, `type(c_ptr)` or a deferred type of a template
        // needs a defined operator (resolved as `overloaded` above).
        auto has_intrinsic_type = [](ASR::ttype_t *t) {
            return ASRUtils::is_integer(*t) || ASRUtils::is_unsigned_integer(*t) ||
                ASRUtils::is_real(*t) || ASRUtils::is_complex(*t) ||
                ASRUtils::is_character(*t) || ASRUtils::is_logical(*t);
        };
        // A logical operand is accepted (as an extension) only when the
        // other operand is numeric; two logical operands have no numeric
        // type to be converted to.
        bool both_logical = ASRUtils::is_logical(*left_type) &&
            ASRUtils::is_logical(*right_type);
        if (!has_intrinsic_type(left_type) || !has_intrinsic_type(right_type) ||
                both_logical) {
            std::string op_str = "+";
            switch (op) {
                case (ASR::Add):
                    break;
                case (ASR::Sub):
                    op_str = "-";
                    break;
                case (ASR::Mul):
                    op_str = "*";
                    break;
                case (ASR::Div):
                    op_str = "/";
                    break;
                case (ASR::Pow):
                    op_str = "**";
                    break;
                default:
                    LCOMPILERS_ASSERT(false);
            }
            diag.add(Diagnostic("Operator `" + op_str + "` undefined for the types in the expression `" + ASRUtils::type_to_str_with_kind(left_type, left)
                                + " " +  op_str + " " + ASRUtils::type_to_str_with_kind(right_type, right) + "`", Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
            throw SemanticAbort();
        }
        ImplicitCastRules::find_conversion_candidate(&left, &right, left_type,
                                                right_type, conversion_cand,
                                                &source_type, &dest_type);
        // Fortran standard: real(dp)*complex(sp) produces complex(dp).
        // When mixing real and complex, use max(real_kind, complex_kind).
        {
            ASR::ttype_t *src2 = ASRUtils::type_get_past_array(
                ASRUtils::type_get_past_pointer(
                    ASRUtils::type_get_past_allocatable(source_type)));
            ASR::ttype_t *dst2 = ASRUtils::type_get_past_array(
                ASRUtils::type_get_past_pointer(
                    ASRUtils::type_get_past_allocatable(dest_type)));
            if (ASRUtils::is_real(*src2) && ASRUtils::is_complex(*dst2)) {
                int src_kind = ASRUtils::extract_kind_from_ttype_t(src2);
                int dst_kind = ASRUtils::extract_kind_from_ttype_t(dst2);
                if (src_kind > dst_kind) {
                    dest_type = ASRUtils::TYPE(ASR::make_Complex_t(
                        al, dest_type->base.loc, src_kind));
                    ASR::expr_t **other = (conversion_cand == &left) ? &right : &left;
                    ImplicitCastRules::set_converted_value(al, x.base.base.loc,
                        other, ASRUtils::expr_type(*other), dest_type, diag);
                }
            }
        }
        if((op == ASR::binopType::Pow) &&
            (ASRUtils::is_real(*dest_type) || ASRUtils::is_complex(*dest_type)) &&
            ASRUtils::is_integer(*right_type)){ // Don't cast exponent to preserve precision.
            // Do nothing.
        } else {
            ImplicitCastRules::set_converted_value(al, x.base.base.loc, conversion_cand,
                                                source_type, dest_type, diag);
        }
    }

    if( (ASRUtils::is_array(right_type) || ASRUtils::is_array(left_type)) &&
         !ASRUtils::is_array(dest_type) ) {
        ASR::dimension_t* m_dims = nullptr;
        size_t n_dims = 0;
        if( ASRUtils::is_array(left_type) ) {
            n_dims = ASRUtils::extract_dimensions_from_ttype(left_type, m_dims);
        } else if( ASRUtils::is_array(right_type) ) {
            n_dims = ASRUtils::extract_dimensions_from_ttype(right_type, m_dims);
        }
        dest_type = ASRUtils::make_Array_t_util(al, dest_type->base.loc,
            ASRUtils::type_get_past_pointer(dest_type), m_dims, n_dims);
        if( ASR::is_a<ASR::Allocatable_t>(*left_type) || ASR::is_a<ASR::Allocatable_t>(*right_type) ) {
            dest_type = ASRUtils::TYPE(ASRUtils::make_Allocatable_t_util(al, dest_type->base.loc, dest_type));
        }
    }
    if((op == ASR::binopType::Pow) &&
        (ASRUtils::is_real(*dest_type) || ASRUtils::is_complex(*dest_type)) &&
        ASRUtils::is_integer(*right_type)) {
        // Don't Check.
    } else if (!ASRUtils::check_equal_type(ASRUtils::expr_type(left),
                                ASRUtils::expr_type(right), left, right) && overloaded == nullptr) {
        std::string ltype = ASRUtils::type_to_str_fortran_expr(ASRUtils::expr_type(left), left);
        std::string rtype = ASRUtils::type_to_str_fortran_expr(ASRUtils::expr_type(right), right);
        diag.add(Diagnostic(
            "Type mismatch in binary operator, the types must be compatible",
            Level::Error, Stage::Semantic, {
                Label("type mismatch (" + ltype + " and " + rtype + ")",
                        {left->base.loc, right->base.loc})
            })
        );
        throw SemanticAbort();
    }
    ASR::expr_t *value = nullptr;

    if (ASRUtils::is_integer(*dest_type)) {

        if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr ) {
            value = visit_BinOp_helper(ASRUtils::expr_value(left), ASRUtils::expr_value(right), op, x.base.base.loc, dest_type);
        }

        ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, left, right);
        value = value ? value : extract_value(ASRUtils::expr_value(left), ASRUtils::expr_value(right), op, dest_type, x.base.base.loc);
        asr = ASR::make_IntegerBinOp_t(al, x.base.base.loc, left, op, right, ASRUtils::type_get_past_allocatable_pointer(dest_type), value);

    } else if (ASRUtils::is_unsigned_integer(*dest_type)) {

        if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr ) {
            value = visit_BinOp_helper(ASRUtils::expr_value(left), ASRUtils::expr_value(right), op, x.base.base.loc, dest_type);
        }

        ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, left, right);
        value = value ? value : extract_value(ASRUtils::expr_value(left), ASRUtils::expr_value(right), op, dest_type, x.base.base.loc);
        asr = ASR::make_UnsignedIntegerBinOp_t(al, x.base.base.loc, left, op, right,
            ASRUtils::type_get_past_pointer(dest_type), value);

    } else if (ASRUtils::is_real(*dest_type)) {

        if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr) {
            value = visit_BinOp_helper(ASRUtils::expr_value(left), ASRUtils::expr_value(right), op, x.base.base.loc, dest_type);
        }

        ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, left, right);
        value = value ? value : extract_value(ASRUtils::expr_value(left), ASRUtils::expr_value(right), op, dest_type, x.base.base.loc);
        asr = ASR::make_RealBinOp_t(al, x.base.base.loc, left, op, right,
            ASRUtils::type_get_past_pointer(dest_type), value);

    } else if (ASRUtils::is_complex(*dest_type)) {

        if (ASRUtils::expr_value(left) != nullptr && ASRUtils::expr_value(right) != nullptr) {
            value = visit_BinOp_helper(ASRUtils::expr_value(left), ASRUtils::expr_value(right), op, x.base.base.loc, dest_type);
        }

        ASRUtils::make_ArrayBroadcast_t_util(al, x.base.base.loc, left, right);
        value = value ? value : extract_value(ASRUtils::expr_value(left), ASRUtils::expr_value(right), op, dest_type, x.base.base.loc);
        asr = ASR::make_ComplexBinOp_t(al, x.base.base.loc, left, op, right,
            ASRUtils::type_get_past_pointer(dest_type), value);

    } else if (ASRUtils::is_character(*dest_type)) {
        if (overloaded == nullptr) {
            diag.semantic_error_label(
                            "Binary numeric operators cannot be used on strings",
                            {x.base.base.loc},
                            "help: use '//' for string concatenation"
                        );
            throw SemanticAbort();
        }
    } else if( overloaded == nullptr ) {
        LCOMPILERS_ASSERT(false);
    }

    if (overloaded != nullptr) {
        asr = ASR::make_OverloadedBinOp_t(al, x.base.base.loc, left, op, right, dest_type, value, overloaded);
    }

}

Vec<AST::decl_attribute_t*> CommonVisitorBase::match_instantiation_args(
        AST::decl_attribute_t** args, size_t n_args,
        char** deferred, size_t n_deferred,
        const std::string &count_mismatch_msg, const Location &loc) {
    bool has_keyword = false;
    for (size_t i = 0; i < n_args; i++) {
        if (AST::is_a<AST::AttrKeyword_t>(*args[i])) {
            has_keyword = true;
            break;
        }
    }

    if (!has_keyword) {
        if (n_args != n_deferred) {
            diag.add(Diagnostic(count_mismatch_msg, Level::Error,
                Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }
        Vec<AST::decl_attribute_t*> ordered;
        ordered.reserve(al, n_args);
        for (size_t i = 0; i < n_args; i++) {
            ordered.push_back(al, args[i]);
        }
        return ordered;
    }

    Vec<AST::decl_attribute_t*> ordered;
    ordered.reserve(al, n_deferred);
    for (size_t j = 0; j < n_deferred; j++) {
        ordered.push_back(al, nullptr);
    }

    bool seen_keyword = false;
    for (size_t i = 0; i < n_args; i++) {
        AST::decl_attribute_t *arg = args[i];
        const Location &arg_loc = arg->base.loc;
        size_t pos = i;
        if (AST::is_a<AST::AttrKeyword_t>(*arg)) {
            AST::AttrKeyword_t *kw = AST::down_cast<AST::AttrKeyword_t>(arg);
            std::string keyword = to_lower(kw->m_name);
            bool found = false;
            for (size_t j = 0; j < n_deferred; j++) {
                if (to_lower(deferred[j]) == keyword) {
                    pos = j;
                    found = true;
                    break;
                }
            }
            if (!found) {
                diag.add(Diagnostic("'" + keyword + "' is not a deferred"
                    " argument of the referenced template or requirement",
                    Level::Error, Stage::Semantic,
                    {Label("", {arg_loc})}));
                throw SemanticAbort();
            }
            arg = kw->m_value;
            seen_keyword = true;
        } else {
            if (seen_keyword) {
                diag.add(Diagnostic("a positional instantiation argument"
                    " cannot follow a keyword one", Level::Error,
                    Stage::Semantic, {Label("", {arg_loc})}));
                throw SemanticAbort();
            }
            if (pos >= n_deferred) {
                diag.add(Diagnostic(count_mismatch_msg, Level::Error,
                    Stage::Semantic, {Label("", {loc})}));
                throw SemanticAbort();
            }
        }
        if (ordered[pos] != nullptr) {
            diag.add(Diagnostic("more than one instantiation argument"
                " corresponds to the deferred argument '"
                + to_lower(deferred[pos]) + "'", Level::Error,
                Stage::Semantic, {Label("", {arg_loc})}));
            throw SemanticAbort();
        }
        ordered.p[pos] = arg;
    }

    for (size_t j = 0; j < n_deferred; j++) {
        if (ordered[j] == nullptr) {
            diag.add(Diagnostic("no instantiation argument corresponds to"
                " the deferred argument '" + to_lower(deferred[j]) + "'",
                Level::Error, Stage::Semantic, {Label("", {loc})}));
            throw SemanticAbort();
        }
    }

    return ordered;
}

std::vector<size_t> CommonVisitorBase::instantiation_arg_order(ASR::Template_t *temp) {
    std::vector<size_t> order;
    order.reserve(temp->n_args);
    std::vector<size_t> rest;
    for (size_t i = 0; i < temp->n_args; i++) {
        ASR::symbol_t *param_sym = temp->m_symtab->get_symbol(temp->m_args[i]);
        if (param_sym && ASR::is_a<ASR::Variable_t>(*param_sym)
                && ASRUtils::is_type_parameter(
                    *ASRUtils::symbol_type(param_sym))) {
            order.push_back(i);
        } else {
            rest.push_back(i);
        }
    }
    order.insert(order.end(), rest.begin(), rest.end());
    return order;
}

void CommonVisitorBase::queue_body_instantiation(
        const std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>> &type_subs,
        const std::map<std::string, ASR::symbol_t*> &symbol_subs,
        const std::vector<std::pair<ASR::symbol_t*, ASR::symbol_t*>> &symbols) {
    if (symbols.empty()) return;
    PendingBodyInstantiation p{type_subs, symbol_subs, symbols};
    if (is_body_visitor) {
        pending_body_instantiations.push_back(p);
    } else {
        instantiate_pending_body(p);
    }
}

SymbolTable *CommonVisitorBase::template_symbol_symtab(ASR::symbol_t *s) {
    switch (s->type) {
        case ASR::symbolType::Function:
            return ASR::down_cast<ASR::Function_t>(s)->m_symtab;
        case ASR::symbolType::Template:
            return ASR::down_cast<ASR::Template_t>(s)->m_symtab;
        case ASR::symbolType::Struct:
            return ASR::down_cast<ASR::Struct_t>(s)->m_symtab;
        default:
            return nullptr;
    }
}

bool CommonVisitorBase::is_within(ASR::symbol_t *target, ASR::symbol_t *source) {
    if (target == source) return true;
    SymbolTable *source_symtab = template_symbol_symtab(source);
    SymbolTable *parent = ASRUtils::symbol_parent_symtab(source);
    if (parent != nullptr && parent->asr_owner != nullptr
            && ASR::is_a<ASR::symbol_t>(*parent->asr_owner)
            && ASR::is_a<ASR::Template_t>(
                *ASR::down_cast<ASR::symbol_t>(parent->asr_owner))) {
        source_symtab = parent;
    }
    if (source_symtab == nullptr) return false;
    for (SymbolTable *t = ASRUtils::symbol_parent_symtab(target);
            t != nullptr; t = t->parent) {
        if (t == source_symtab) return true;
    }
    return false;
}

void CommonVisitorBase::instantiate_pending_bodies_from(size_t i, std::vector<bool> &started) {
    if (started[i]) return;
    started[i] = true;
    for (size_t j = 0; j < pending_body_instantiations.size(); j++) {
        if (started[j]) continue;
        bool needed = false;
        for (auto &src : pending_body_instantiations[i].symbols) {
            for (auto &dst : pending_body_instantiations[j].symbols) {
                if (is_within(dst.first, src.second)) needed = true;
            }
        }
        if (needed) instantiate_pending_bodies_from(j, started);
    }
    instantiate_pending_body(pending_body_instantiations[i]);
}

void CommonVisitorBase::visit_DefTOp(ASR::expr_t* first_operand, ASR::expr_t* second_operand, const std::string op, const Location loc) {
    bool is_binary = (second_operand != nullptr);

    ASR::Struct_t *first_struct = nullptr;
    ASR::ttype_t* first_operand_type = ASRUtils::type_get_past_allocatable_pointer(
        ASRUtils::expr_type(first_operand));
    if (ASR::is_a<ASR::StructType_t>(*first_operand_type)) {
        ASR::symbol_t* first_struct_sym = ASRUtils::symbol_get_past_external(
            ASRUtils::get_struct_sym_from_struct_expr(first_operand));
        if (first_struct_sym != nullptr &&
            ASR::is_a<ASR::Struct_t>(*ASRUtils::symbol_get_past_external(first_struct_sym))) {
            first_struct = ASR::down_cast<ASR::Struct_t>(
                ASRUtils::symbol_get_past_external(first_struct_sym));
        }
    }

    ASR::Struct_t *second_struct = nullptr;
    if (is_binary) {
        ASR::ttype_t* second_operand_type = ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(second_operand));
        if (ASR::is_a<ASR::StructType_t>(*second_operand_type)) {
            ASR::symbol_t* second_struct_sym = ASRUtils::symbol_get_past_external(
                ASRUtils::get_struct_sym_from_struct_expr(second_operand));
            if (second_struct_sym != nullptr &&
                ASR::is_a<ASR::Struct_t>(*ASRUtils::symbol_get_past_external(second_struct_sym))) {
                second_struct = ASR::down_cast<ASR::Struct_t>(
                    ASRUtils::symbol_get_past_external(second_struct_sym));
            }
        }
    }

    std::string new_op = op;
    ASR::symbol_t* op_sym = nullptr;
    ASR::symbol_t* operator_sym = nullptr;

    // Interface operators are stored as `~~op`. Do not look the operator
    // up under the bare name when an operand is a derived type: that name
    // is a specific procedure when the operator and one of its procedures
    // are both called `op`, and the procedure would be called for every
    // operand type. Type-bound generics are `~def_op~op` on the type.
    if (first_struct == nullptr) {
        new_op = update_custom_op_name(new_op);
        op_sym = current_scope->resolve_symbol(to_lower(new_op));
        operator_sym = ASRUtils::symbol_get_past_external(op_sym);
    }

    if (first_struct != nullptr && operator_sym == nullptr) {
        // Walk up the inheritance chain to find the operator
        ASR::Struct_t* cur_struct = first_struct;
        while (cur_struct != nullptr) {
            operator_sym = cur_struct->m_symtab->resolve_symbol("~def_op~" + op);
            if (operator_sym != nullptr) break;
            if (cur_struct->m_parent != nullptr) {
                cur_struct = ASR::down_cast<ASR::Struct_t>(
                    ASRUtils::symbol_get_past_external(cur_struct->m_parent));
            } else {
                break;
            }
        }
        if (operator_sym == nullptr) {
            // The operator is not type-bound; try resolving as a regular
            // custom operator (with the "~~" prefix) in the current scope.
            std::string prefixed_op = update_custom_op_name(op);
            op_sym = current_scope->resolve_symbol(to_lower(prefixed_op));
            operator_sym = ASRUtils::symbol_get_past_external(op_sym);
        }
    }

    if (operator_sym == nullptr && second_struct != nullptr) {
        // Try the RHS struct's inheritance chain
        ASR::Struct_t* cur_struct = second_struct;
        while (cur_struct != nullptr) {
            operator_sym = cur_struct->m_symtab->resolve_symbol("~def_op~" + op);
            if (operator_sym != nullptr) break;
            if (cur_struct->m_parent != nullptr) {
                cur_struct = ASR::down_cast<ASR::Struct_t>(
                    ASRUtils::symbol_get_past_external(cur_struct->m_parent));
            } else {
                break;
            }
        }
    }

    if (operator_sym == nullptr) {
        diag.add(Diagnostic(
            "`" + op + "` is not defined or imported",
            Level::Error, Stage::Semantic, {Label("", {loc})}
        ));
        throw SemanticAbort();
    }
    if (!ASR::is_a<ASR::CustomOperator_t>(*operator_sym)
        && !ASR::is_a<ASR::Function_t>(*operator_sym)) {
        diag.add(Diagnostic(
            "User operator procedure must be a FUNCTION",
            Level::Error,
            Stage::Semantic,
            { Label("", { loc }) }
        ));
        throw SemanticAbort();
    }

    if (!is_binary && ASR::is_a<ASR::Function_t>(*operator_sym)) {
        ASR::Function_t* single_func = ASR::down_cast<ASR::Function_t>(operator_sym);
        if (is_binary || single_func->n_args != 1) {
            diag.add(Diagnostic(
                "Defined unary operator `" + op + "` must have exactly one argument",
                Level::Error, Stage::Semantic, {Label("", {loc})}
            ));
            throw SemanticAbort();
        }

        Vec<ASR::call_arg_t> a_args;
        a_args.reserve(al, 1);
        a_args.push_back(al, { first_operand->base.loc, first_operand });

        ASR::expr_t* first_array_arg = ASRUtils::find_first_array_arg_if_elemental(single_func, a_args);
        ASR::ttype_t* return_type = nullptr;
        if (first_array_arg) {
            ASR::dimension_t* dims;
            size_t n_dims = ASRUtils::extract_dimensions_from_ttype(
                ASRUtils::expr_type(first_array_arg), dims);
            Vec<ASR::dimension_t> new_dims;
            new_dims.from_pointer_n_copy(al, dims, n_dims);
            return_type = ASRUtils::duplicate_type(
                al, ASRUtils::get_FunctionType(single_func)->m_return_var_type, &new_dims);
        } else {
            ASR::ttype_t* type = ASRUtils::expr_type(single_func->m_return_var);
            return_type = ASRUtils::duplicate_type_without_dims(al, type, loc);
        }

        std::string func_name = to_lower(single_func->m_name);
        ASR::symbol_t* call_sym = ASRUtils::import_type_bound_specific(al,
            current_scope, operator_sym, to_lower(new_op));
        if (ASRUtils::symbol_parent_symtab(call_sym)->get_counter()
            != current_scope->get_counter()) {
            ADD_ASR_DEPENDENCIES_WITH_NAME(current_scope,
                call_sym, current_function_dependencies, s2c(al, func_name));
        }
        tmp = ASRUtils::make_FunctionCall_t_util(al, loc, call_sym, nullptr,
            a_args.p, a_args.size(), return_type, nullptr, nullptr, current_scope,
            current_function_dependencies, compiler_options.implicit_argument_casting);
        return;
    }

    if (is_binary && ASR::is_a<ASR::Function_t>(*operator_sym)) {
        ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(operator_sym);

        if (func->n_args != 2) {
            diag.add(Diagnostic(
                "Defined binary operator `" + op + "` must have exactly two arguments",
                Level::Error, Stage::Semantic, {Label("", {loc})}
            ));
            throw SemanticAbort();
        }

        Vec<ASR::call_arg_t> a_args;
        a_args.reserve(al, 2);
        a_args.push_back(al, { first_operand->base.loc, first_operand });
        a_args.push_back(al, { second_operand->base.loc, second_operand });

        ASR::expr_t* first_array_arg = ASRUtils::find_first_array_arg_if_elemental(func, a_args);
        ASR::ttype_t* return_type = nullptr;
        if (first_array_arg) {
            ASR::dimension_t* dims;
            size_t n_dims =
                ASRUtils::extract_dimensions_from_ttype(
                    ASRUtils::expr_type(first_array_arg), dims);
            Vec<ASR::dimension_t> new_dims;
            new_dims.from_pointer_n_copy(al, dims, n_dims);
            return_type = ASRUtils::duplicate_type(
                al, ASRUtils::get_FunctionType(func)->m_return_var_type, &new_dims);
        } else {
            return_type = ASRUtils::duplicate_type(
                al, ASRUtils::expr_type(func->m_return_var));
        }

        ASR::symbol_t* a_name = ASRUtils::import_type_bound_specific(al,
            current_scope, operator_sym, to_lower(new_op));

        tmp = ASRUtils::make_FunctionCall_t_util(
            al, loc, a_name, nullptr,
            a_args.p, a_args.size(), return_type,
            nullptr, nullptr, current_scope,
            current_function_dependencies,
            compiler_options.implicit_argument_casting);
        return;
    }
    if (!ASR::is_a<ASR::CustomOperator_t>(*operator_sym)) {
        diag.add(Diagnostic(
            "" + op + " is not a valid defined operator",
            Level::Error, Stage::Semantic, {Label("", {loc})}
        ));
        throw SemanticAbort();
    }
    ASR::CustomOperator_t* gen_proc = ASR::down_cast<ASR::CustomOperator_t>(operator_sym);
    ASR::ttype_t* left_type = ASRUtils::expr_type(first_operand);
    ASR::ttype_t* right_type = nullptr;
    if (is_binary)
        right_type = ASRUtils::expr_type(second_operand);
    bool matched = false;
    // Resolve the specific procedure with the same priority as
    // ASRUtils::select_generic_procedure: try non-elemental procedures
    // first (matching rank exactly), and only fall back to elemental
    // procedures (which broadcast over array actuals) if none matched.
    // Otherwise an elemental scalar overload declared before a rank-1
    // overload would be wrongly selected for an array actual argument.
    std::vector<size_t> proc_order;
    for (size_t i = 0; i < gen_proc->n_procs; ++i) {
        if (!ASRUtils::is_elemental(gen_proc->m_procs[i])) proc_order.push_back(i);
    }
    for (size_t i = 0; i < gen_proc->n_procs; ++i) {
        if (ASRUtils::is_elemental(gen_proc->m_procs[i])) proc_order.push_back(i);
    }
    for (size_t oi = 0; oi < proc_order.size(); ++oi) {
        size_t i = proc_order[oi];
        ASR::symbol_t* proc;
        ASR::StructMethodDeclaration_t* binding =
            ASRUtils::type_bound_specific_binding(gen_proc->m_procs[i], nullptr);
        if (binding != nullptr) {
            proc = binding->m_proc;
        } else {
            proc = gen_proc->m_procs[i];
        }

        if (!ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(proc))) {
            diag.add(Diagnostic("User operator procedure must be a FUNCTION",
                                Level::Error,
                                Stage::Semantic,
                                { Label("", { loc }) }));
            throw SemanticAbort();
        }
        ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(proc));
        if ((is_binary && func->n_args != 2) || (!is_binary && func->n_args != 1))
            continue;
        // For non-elemental procedures the actual argument rank must match
        // the dummy argument rank, so compare dimensions; elemental
        // procedures broadcast, so dimensions are ignored.
        bool check_dims = !ASRUtils::get_FunctionType(func)->m_elemental;
        bool args_match = ASRUtils::check_equal_type(ASRUtils::expr_type(func->m_args[0]), left_type, func->m_args[0], first_operand, check_dims);
        if (is_binary) {
            args_match = args_match
                         && ASRUtils::check_equal_type(ASRUtils::expr_type(func->m_args[1]),
                                                       right_type, func->m_args[1], second_operand, check_dims);
        }
        if (!args_match)
            continue;

        Vec<ASR::call_arg_t> a_args;
        a_args.reserve(al, is_binary ? 2 : 1);

        a_args.push_back(al, { first_operand->base.loc, first_operand });
        if (is_binary)
            a_args.push_back(al, { second_operand->base.loc, second_operand });

        ASR::ttype_t* return_type = nullptr;
        ASR::expr_t* first_array_arg
            = ASRUtils::find_first_array_arg_if_elemental(func, a_args);
        if (first_array_arg) {
            ASR::dimension_t* array_dims;
            size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                ASRUtils::expr_type(first_array_arg), array_dims);
            Vec<ASR::dimension_t> new_dims;
            new_dims.from_pointer_n_copy(al, array_dims, array_n_dims);
            return_type = ASRUtils::duplicate_type(
                al, ASRUtils::get_FunctionType(func)->m_return_var_type, &new_dims);
        } else {
            return_type = ASRUtils::EXPR2VAR(func->m_return_var)->m_type;
            return_type = handle_return_type(return_type, loc, a_args, func);
        }
        std::string func_name = to_lower(func->m_name);
        ASR::symbol_t* call_sym = nullptr;
        if (op_sym != nullptr && !ASRUtils::is_type_bound_generic(operator_sym)
                && binding == nullptr) {
            call_sym = gen_proc->m_procs[i];
        } else {
            call_sym = ASRUtils::import_type_bound_specific(al, current_scope,
                proc, to_lower(new_op));
        }

        if (ASRUtils::symbol_parent_symtab(call_sym)->get_counter()
                != current_scope->get_counter()) {
            ADD_ASR_DEPENDENCIES_WITH_NAME(
                current_scope, call_sym, current_function_dependencies, s2c(al, func_name)
            );
        }
        tmp = ASRUtils::make_FunctionCall_t_util(al, loc, call_sym, op_sym, a_args.p, a_args.size(), return_type, nullptr, nullptr, current_scope, current_function_dependencies,
            compiler_options.implicit_argument_casting);
        matched = true;
        break;
    }

    if (!matched && is_binary && second_struct != nullptr) {
        // Try the RHS struct's inheritance chain for the operator
        ASR::symbol_t* rhs_operator_sym = nullptr;
        ASR::Struct_t* cur_struct = second_struct;
        while (cur_struct != nullptr) {
            rhs_operator_sym = cur_struct->m_symtab->resolve_symbol("~def_op~" + op);
            if (rhs_operator_sym != nullptr) break;
            if (cur_struct->m_parent != nullptr) {
                cur_struct = ASR::down_cast<ASR::Struct_t>(
                    ASRUtils::symbol_get_past_external(cur_struct->m_parent));
            } else {
                break;
            }
        }
        if (rhs_operator_sym != nullptr && ASR::is_a<ASR::CustomOperator_t>(*rhs_operator_sym)) {
            ASR::CustomOperator_t* rhs_gen_proc = ASR::down_cast<ASR::CustomOperator_t>(rhs_operator_sym);
            for (size_t i = 0; i < rhs_gen_proc->n_procs; ++i) {
                ASR::symbol_t* proc;
                ASR::StructMethodDeclaration_t* binding =
                    ASRUtils::type_bound_specific_binding(rhs_gen_proc->m_procs[i], nullptr);
                if (binding != nullptr) {
                    proc = binding->m_proc;
                } else {
                    proc = rhs_gen_proc->m_procs[i];
                }
                if (!ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(proc)))
                    continue;
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(proc));
                if (func->n_args != 2) continue;
                bool args_match = ASRUtils::check_equal_type(
                    ASRUtils::expr_type(func->m_args[0]), left_type, func->m_args[0], first_operand);
                args_match = args_match && ASRUtils::check_equal_type(
                    ASRUtils::expr_type(func->m_args[1]), right_type, func->m_args[1], second_operand);
                if (!args_match) continue;

                Vec<ASR::call_arg_t> a_args;
                a_args.reserve(al, 2);
                a_args.push_back(al, { first_operand->base.loc, first_operand });
                a_args.push_back(al, { second_operand->base.loc, second_operand });

                ASR::ttype_t* return_type = nullptr;
                ASR::expr_t* first_array_arg = ASRUtils::find_first_array_arg_if_elemental(func, a_args);
                if (first_array_arg) {
                    ASR::dimension_t* array_dims;
                    size_t array_n_dims = ASRUtils::extract_dimensions_from_ttype(
                        ASRUtils::expr_type(first_array_arg), array_dims);
                    Vec<ASR::dimension_t> new_dims;
                    new_dims.from_pointer_n_copy(al, array_dims, array_n_dims);
                    return_type = ASRUtils::duplicate_type(
                        al, ASRUtils::get_FunctionType(func)->m_return_var_type, &new_dims);
                } else {
                    return_type = ASRUtils::duplicate_type(
                        al, ASRUtils::expr_type(func->m_return_var));
                }
                std::string func_name = to_lower(func->m_name);
                ASR::symbol_t* call_sym = ASRUtils::import_type_bound_specific(al,
                    current_scope, proc, to_lower(new_op));
                if (ASRUtils::symbol_parent_symtab(call_sym)->get_counter()
                        != current_scope->get_counter()) {
                    ADD_ASR_DEPENDENCIES_WITH_NAME(
                        current_scope, call_sym, current_function_dependencies, s2c(al, func_name)
                    );
                }
                tmp = ASRUtils::make_FunctionCall_t_util(al, loc, call_sym, op_sym, a_args.p, a_args.size(), return_type, nullptr, nullptr, current_scope, current_function_dependencies,
                    compiler_options.implicit_argument_casting);
                matched = true;
                break;
            }
        }
    }

    if (!matched) {
        std::string operand_types = ASRUtils::type_to_str_with_kind(
            ASRUtils::expr_type(first_operand), first_operand);
        if (is_binary) {
            operand_types = "types " + operand_types + " and "
                + ASRUtils::type_to_str_with_kind(
                    ASRUtils::expr_type(second_operand), second_operand);
        } else {
            operand_types = "type " + operand_types;
        }
        diag.add(Diagnostic(
            "No matching procedure found for defined operator `." + op
                + ".` with operand " + operand_types,
            Level::Error,
            Stage::Semantic,
            { Label("", { loc }) }));
        throw SemanticAbort();
    }
}

ASR::expr_t* CommonVisitorBase::cast_assumed_rank_selector(ASR::expr_t* expr) {
    if (!ASRUtils::is_assumed_rank_array(ASRUtils::expr_type(expr)) ||
            !ASR::is_a<ASR::Var_t>(*expr)) {
        return expr;
    }
    std::string array_name = ASRUtils::symbol_name(
        ASR::down_cast<ASR::Var_t>(expr)->m_v);
    auto it = assumed_rank_arrays.find(array_name);
    if (it == assumed_rank_arrays.end()) {
        return expr;
    }
    // `create_array_type_with_empty_dims` returns the element type itself
    // for `rank(0)`, i.e. a scalar selector.
    ASR::ttype_t* desc_type = ASRUtils::create_array_type_with_empty_dims(al,
        it->second, ASRUtils::extract_type(ASRUtils::expr_type(expr)));
    return ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(al,
        expr->base.loc, expr, ASR::array_physical_typeType::AssumedRankArray,
        ASR::array_physical_typeType::DescriptorArray, desc_type, nullptr));
}

ASR::expr_t* CommonVisitorBase::resolve_selector_object(ASR::expr_t* base, const Location& loc) {
    if (!ASRUtils::is_assumed_rank_array(ASRUtils::expr_type(base)) ||
            !ASR::is_a<ASR::Var_t>(*base)) {
        return base;
    }
    ASR::expr_t* selector = cast_assumed_rank_selector(base);
    if (ASRUtils::is_assumed_rank_array(ASRUtils::expr_type(selector))) {
        diag.add(Diagnostic("assumed-rank array '" + std::string(
            ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(base)->m_v)) +
            "' cannot be used as the base of a component or binding reference",
            Level::Error, Stage::Semantic, {Label("", {loc})}));
        throw SemanticAbort();
    }
    if (ASRUtils::is_array(ASRUtils::expr_type(selector))) {
        return base;
    }
    return selector;
}

void CommonVisitorBase::ensure_not_assumed_rank(ASR::expr_t* expr, const std::string& op_kind,
        const Location& loc) {
    if (!ASRUtils::is_assumed_rank_array(ASRUtils::expr_type(expr))) {
        return;
    }
    std::string array_name = "";
    if (ASR::is_a<ASR::Var_t>(*expr)) {
        array_name = " ('" + std::string(ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(expr)->m_v)) + "')";
    }
    diag.add(Diagnostic(op_kind + " operations are not allowed on "
        "assumed-rank arrays" + array_name,
        Level::Error, Stage::Semantic, {Label("", {loc})}));
    throw SemanticAbort();
}

void CommonVisitorBase::reject_conditional_expr_in_restricted_context(const Location &loc) {
    if (restricted_expr_context == RestrictedExprContext::None) {
        return;
    }
    std::string clause, requirement;
    switch (restricted_expr_context) {
        case RestrictedExprContext::KindParameter: {
            clause = "constant";
            requirement = "a kind type parameter shall be a constant "
                "expression (Fortran 2023 C701)";
            break;
        }
        case RestrictedExprContext::InitExpr: {
            clause = "constant";
            requirement = "an initialization expression shall be a "
                "constant expression (Fortran 2023 C1012)";
            break;
        }
        case RestrictedExprContext::CharacterLength: {
            clause = "specification";
            requirement = "a type parameter value shall be a "
                "specification expression (Fortran 2023 C704)";
            break;
        }
        case RestrictedExprContext::ArrayBound: {
            clause = "specification";
            requirement = "an array bound shall be a specification "
                "expression";
            break;
        }
        default: {
            return;
        }
    }
    std::string listing = (clause == "constant") ? "10.1.12" : "10.1.11";
    diag.add(Diagnostic(
        "a conditional expression is not allowed in a " + clause
            + " expression",
        Level::Error, Stage::Semantic, {
            Label(requirement, {loc}),
            Label("help: Fortran 2023 " + listing + " does not list a "
                "conditional expression among the permitted primaries; "
                "use `merge`, which is permitted here", {loc}, false)}));
    throw SemanticAbort();
}

void CommonVisitorBase::conditional_arm_type_strs(ASR::expr_t *body, ASR::expr_t *orelse,
        std::string &body_str, std::string &orelse_str) {
    ASR::ttype_t *body_type = ASRUtils::expr_type(body);
    ASR::ttype_t *orelse_type = ASRUtils::expr_type(orelse);
    body_str = ASRUtils::type_to_str_with_kind(body_type, body);
    orelse_str = ASRUtils::type_to_str_with_kind(orelse_type, orelse);
    if (body_str == orelse_str) {
        body_str += "(kind=" + std::to_string(
            ASRUtils::extract_kind_from_ttype_t(body_type)) + ")";
        orelse_str += "(kind=" + std::to_string(
            ASRUtils::extract_kind_from_ttype_t(orelse_type)) + ")";
    }
}

void CommonVisitorBase::check_conditional_arg(AST::ConditionalExpr_t *x) {
    std::vector<AST::expr_t*> consequents;
    collect_consequents((AST::expr_t*)(&x->base), consequents);
    for (AST::expr_t *c: consequents) {
        if (!AST::is_a<AST::Nil_t>(*c)) {
            return;
        }
    }
    diag.add(Diagnostic(
        "every consequent of this conditional argument is `.nil.`",
        Level::Error, Stage::Semantic, {
            Label("at least one consequent shall be an argument "
                "(Fortran 2023 C1540)", {x->base.base.loc})}));
    throw SemanticAbort();
}

ASR::Variable_t* CommonVisitorBase::consequent_variable(AST::expr_t *c) {
    if (!AST::is_a<AST::Name_t>(*c)) {
        return nullptr;
    }
    AST::Name_t *n = AST::down_cast<AST::Name_t>(c);
    if (n->n_member > 0) {
        return nullptr;
    }
    ASR::symbol_t *s = current_scope->resolve_symbol(to_lower(n->m_id));
    if (s == nullptr) {
        return nullptr;
    }
    s = ASRUtils::symbol_get_past_external(s);
    if (!ASR::is_a<ASR::Variable_t>(*s)) {
        return nullptr;
    }
    return ASR::down_cast<ASR::Variable_t>(s);
}

void CommonVisitorBase::check_conditional_arg_consequents(AST::ConditionalExpr_t *x) {
    std::vector<AST::expr_t*> consequents;
    collect_consequents((AST::expr_t*)(&x->base), consequents);
    AST::expr_t *first = nullptr;
    ASR::ttype_t *first_type = nullptr;
    for (AST::expr_t *c: consequents) {
        ASR::Variable_t *v = consequent_variable(c);
        if (v == nullptr) {
            continue;
        }
        if (first == nullptr) {
            first = c;
            first_type = v->m_type;
            continue;
        }
        size_t first_rank = ASRUtils::extract_n_dims_from_ttype(first_type);
        size_t rank = ASRUtils::extract_n_dims_from_ttype(v->m_type);
        if (first_rank != rank) {
            diag.add(Diagnostic(
                "the consequents of a conditional argument must have the "
                "same rank",
                Level::Error, Stage::Semantic, {
                    Label("rank mismatch (" + std::to_string(first_rank)
                        + " and " + std::to_string(rank) +
                        ") (Fortran 2023 C1539)",
                        {first->base.loc, c->base.loc})}));
            throw SemanticAbort();
        }
        if (!ASRUtils::types_equal(first_type, v->m_type, nullptr, nullptr,
                false)) {
            diag.add(Diagnostic(
                "the consequents of a conditional argument must have the "
                "same type and kind",
                Level::Error, Stage::Semantic, {
                    Label("type mismatch ("
                        + ASRUtils::type_to_str_fortran_expr(first_type, nullptr)
                        + " and "
                        + ASRUtils::type_to_str_fortran_expr(
                            v->m_type, nullptr) +
                        ") (Fortran 2023 C1538)",
                        {first->base.loc, c->base.loc})}));
            throw SemanticAbort();
        }
    }
}

void CommonVisitorBase::check_conditional_arg_generic(AST::ConditionalExpr_t *x) {
    std::vector<AST::expr_t*> consequents;
    collect_consequents((AST::expr_t*)(&x->base), consequents);
    AST::expr_t *first = nullptr;
    bool first_alloc = false, first_ptr = false;
    for (AST::expr_t *c: consequents) {
        ASR::Variable_t *v = consequent_variable(c);
        if (v == nullptr) {
            continue;
        }
        ASR::ttype_t *t = v->m_type;
        bool is_alloc = ASRUtils::is_allocatable(t);
        bool is_ptr = ASRUtils::is_pointer(t);
        if (first == nullptr) {
            first = c;
            first_alloc = is_alloc;
            first_ptr = is_ptr;
            continue;
        }
        if (is_alloc == first_alloc && is_ptr == first_ptr) {
            continue;
        }
        auto attr = [](bool a, bool p) -> std::string {
            if (a) return "allocatable";
            if (p) return "a pointer";
            return "neither allocatable nor a pointer";
        };
        diag.add(Diagnostic(
            "the consequents of a conditional argument to a generic "
            "procedure must have the same `allocatable` and `pointer` "
            "attributes",
            Level::Error, Stage::Semantic, {
                Label("this consequent is " + attr(first_alloc, first_ptr)
                    + ", this one is " + attr(is_alloc, is_ptr) +
                    " (Fortran 2023 C1545)",
                    {first->base.loc, c->base.loc})}));
        throw SemanticAbort();
    }
}

int64_t CommonVisitorBase::first_conditional_arg(AST::fnarg_t *args, size_t n_args,
        AST::keyword_t *kwargs, size_t n_kwargs) {
    for (size_t i = 0; i < n_args; i++) {
        if (args[i].m_end != nullptr && args[i].m_start == nullptr
                && args[i].m_step == nullptr
                && AST::is_a<AST::ConditionalExpr_t>(*args[i].m_end)) {
            return (int64_t)i;
        }
    }
    for (size_t i = 0; i < n_kwargs; i++) {
        if (kwargs[i].m_value != nullptr
                && AST::is_a<AST::ConditionalExpr_t>(*kwargs[i].m_value)) {
            return (int64_t)(n_args + i);
        }
    }
    return -1;
}

void CommonVisitorBase::copy_args_with_consequent(AST::fnarg_t *args, size_t n_args,
        AST::keyword_t *kwargs, size_t n_kwargs, int64_t idx,
        AST::expr_t *consequent, AST::fnarg_t *&new_args,
        AST::keyword_t *&new_kwargs) {
    new_args = n_args > 0 ? al.allocate<AST::fnarg_t>(n_args) : nullptr;
    for (size_t i = 0; i < n_args; i++) {
        new_args[i] = args[i];
        if ((int64_t)i == idx) {
            new_args[i].m_end = consequent;
        }
    }
    new_kwargs = n_kwargs > 0 ? al.allocate<AST::keyword_t>(n_kwargs)
        : nullptr;
    for (size_t i = 0; i < n_kwargs; i++) {
        new_kwargs[i] = kwargs[i];
        if ((int64_t)(n_args + i) == idx) {
            new_kwargs[i].m_value = consequent;
        }
    }
}

ASR::symbol_t* CommonVisitorBase::referenced_procedure(const AST::FuncCallOrArray_t &x) {
    // A reference through a component (`o%f(...)`) is not expanded: the
    // final member is a binding or an array component, and only
    // resolving it against the declared type tells which.
    if (x.n_member > 0 || x.n_temp_args > 0) {
        return nullptr;
    }
    std::string name = to_lower(x.m_func);
    ASR::symbol_t *s = current_scope->resolve_symbol("~" + name);
    if (s == nullptr) {
        s = current_scope->resolve_symbol(name);
    }
    if (s == nullptr) {
        return nullptr;
    }
    s = ASRUtils::symbol_get_past_external(s);
    if (ASR::is_a<ASR::Function_t>(*s)
            || ASR::is_a<ASR::GenericProcedure_t>(*s)
            || ASR::is_a<ASR::StructMethodDeclaration_t>(*s)
            // A procedure pointer and a dummy procedure are references
            // too, and their dummy arguments are as definable as any
            || ASRUtils::is_symbol_procedure_variable(s)) {
        return s;
    }
    return nullptr;
}

double CommonVisitorBase::boz_bits_as_real(uint64_t bits, int kind) {
    if( kind == 4 ) {
        uint32_t bits_32 = static_cast<uint32_t>(bits);
        float value_32 = 0.0f;
        std::memcpy(&value_32, &bits_32, sizeof(value_32));
        return static_cast<double>(value_32);
    }
    double value_64 = 0.0;
    std::memcpy(&value_64, &bits, sizeof(value_64));
    return value_64;
}

ASR::expr_t* CommonVisitorBase::boz_literal_to_real(ASR::expr_t* e, int kind) {
    if( !is_boz_constant(e) || (kind != 4 && kind != 8) ) {
        return e;
    }
    const Location& loc = e->base.loc;
    uint64_t bits = static_cast<uint64_t>(
        ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n);
    ASR::ttype_t* real_type = ASRUtils::TYPE(ASR::make_Real_t(al, loc, kind));
    return ASRUtils::EXPR(ASR::make_RealConstant_t(al, loc,
        boz_bits_as_real(bits, kind), real_type));
}

void CommonVisitorBase::convert_boz_args_to_real(const std::string& intrinsic_name,
        Vec<ASR::expr_t*>& args) {
    size_t n_boz_args = 0, kind_arg_index = 0;
    if( intrinsic_name == "real" ) {
        n_boz_args = 1;
        kind_arg_index = 1;
    } else if( intrinsic_name == "cmplx" ) {
        n_boz_args = 2;
        kind_arg_index = 2;
    } else {
        return;
    }
    if( args.size() <= kind_arg_index ) {
        return;
    }
    bool has_boz_arg = false;
    for( size_t i = 0; i < n_boz_args; i++ ) {
        has_boz_arg |= is_boz_constant(args[i]);
    }
    if( !has_boz_arg ) {
        return;
    }
    int kind = 4;
    if( args[kind_arg_index] != nullptr ) {
        kind = static_cast<int>(handle_kind(args[kind_arg_index]));
    }
    for( size_t i = 0; i < n_boz_args; i++ ) {
        args.p[i] = boz_literal_to_real(args[i], kind);
    }
}

bool CommonVisitorBase::is_valid_complex_literal_part(const AST::expr_t &expr) {
    if (AST::is_a<AST::Num_t>(expr)) return true;
    if (AST::is_a<AST::Real_t>(expr)) return true;
    if (AST::is_a<AST::Name_t>(expr)) return true;
    if (AST::is_a<AST::UnaryOp_t>(expr)) {
        const AST::UnaryOp_t &u = *AST::down_cast<AST::UnaryOp_t>(&expr);
        if (u.m_op == AST::unaryopType::USub || u.m_op == AST::unaryopType::UAdd) {
            return AST::is_a<AST::Num_t>(*u.m_operand) ||
                   AST::is_a<AST::Real_t>(*u.m_operand);
        }
    }
    return false;
}

bool CommonVisitorBase::is_bare_null_intrinsic(AST::expr_t* e) {
    if( !AST::is_a<AST::FuncCallOrArray_t>(*e) ) {
        return false;
    }
    AST::FuncCallOrArray_t* f = AST::down_cast<AST::FuncCallOrArray_t>(e);
    if( to_lower(f->m_func) != "null" || f->n_args != 0 ||
        f->n_keywords != 0 || f->n_member != 0 ) {
        return false;
    }
    // `null` may be shadowed by a user defined symbol
    return current_scope->resolve_symbol("null") == nullptr;
}

bool CommonVisitorBase::is_null_intrinsic_reference(AST::expr_t* e) {
    e = skip_parentheses(e);
    if( !AST::is_a<AST::FuncCallOrArray_t>(*e) ) {
        return false;
    }
    AST::FuncCallOrArray_t* f = AST::down_cast<AST::FuncCallOrArray_t>(e);
    return to_lower(f->m_func) == "null" && f->n_member == 0
        && current_scope->resolve_symbol("null") == nullptr;
}

ASR::Variable_t* CommonVisitorBase::get_dummy_argument(ASR::symbol_t* proc, size_t i) {
    if( proc == nullptr ) {
        return nullptr;
    }
    proc = ASRUtils::symbol_get_past_external(proc);
    if( !ASR::is_a<ASR::Function_t>(*proc) ) {
        return nullptr;
    }
    ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(proc);
    if( i >= f->n_args || !ASR::is_a<ASR::Var_t>(*f->m_args[i]) ) {
        return nullptr;
    }
    ASR::symbol_t* dummy = ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v);
    if( !ASR::is_a<ASR::Variable_t>(*dummy) ) {
        return nullptr;
    }
    return ASR::down_cast<ASR::Variable_t>(dummy);
}

std::vector<std::string> CommonVisitorBase::convert_fn_args_to_string(
        ASR::expr_t **expr_list, size_t n) {
    std::vector<std::string> result;
    for (size_t i=0; i < n; i++) {
        ASR::symbol_t* sym_i = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(expr_list[i])->m_v);
        if( ASR::is_a<ASR::Variable_t>(*sym_i) ) {
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym_i);
            result.push_back(v->m_name);
        } else if( ASR::is_a<ASR::Function_t>(*sym_i) ) {
            ASR::Function_t* f = ASR::down_cast<ASR::Function_t>(sym_i);
            result.push_back(f->m_name);
        } else {
            LCOMPILERS_ASSERT(false);
        }
    }
    return result;
}

bool CommonVisitorBase::is_designator(ASR::expr_t* e) {
    if( !ASR::is_a<ASR::Var_t>(*e) &&
        !ASR::is_a<ASR::StructInstanceMember_t>(*e) &&
        !ASR::is_a<ASR::ArrayItem_t>(*e) ) {
        return false;
    }
    CallFinder call_finder;
    call_finder.visit_expr(*e);
    return !call_finder.found;
}

ASR::expr_t* CommonVisitorBase::evaluate_into_temporary(ASR::expr_t* value) {
    // A `where` body runs only for the elements the mask selects, while
    // the value assigned there is evaluated once whatever the mask is, so
    // the assignment belongs to the body that holds the `where`.
    Vec<ASR::stmt_t*>* body = body_enclosing_where != nullptr
        ? body_enclosing_where : current_body;
    if( body == nullptr || idl_nesting_level > 0 ) {
        return nullptr;
    }
    const Location& loc = value->base.loc;
    ASR::ttype_t* type = ASRUtils::expr_type(value);
    ASR::symbol_t* type_declaration = ASRUtils::import_struct_type(al,
        ASRUtils::get_struct_sym_from_struct_expr(value), current_scope);
    std::string tmp_name = current_scope->get_unique_name("lfortran_tmp");
    SetChar tmp_deps;
    tmp_deps.reserve(al, 1);
    ASRUtils::collect_variable_dependencies(al, tmp_deps, type, nullptr,
        nullptr, tmp_name);
    ASR::symbol_t* tmp_sym = ASR::down_cast<ASR::symbol_t>(
        ASRUtils::make_Variable_t_util(al, loc, current_scope,
            s2c(al, tmp_name), tmp_deps.p, tmp_deps.n,
            ASR::intentType::Local, nullptr, nullptr,
            ASR::storage_typeType::Default, type, type_declaration,
            ASR::abiType::Source, ASR::accessType::Private,
            ASR::presenceType::Required, false));
    current_scope->add_symbol(tmp_name, tmp_sym);
    ASR::expr_t* tmp_var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, tmp_sym));
    body->push_back(al, ASRUtils::STMT(
        ASRUtils::make_Assignment_t_util(al, loc, tmp_var, value, nullptr,
            compiler_options.po.realloc_lhs_arrays, false)));
    return tmp_var;
}

bool CommonVisitorBase::spread_parent_component_value(Vec<ASR::call_arg_t>& args,
        const std::vector<ASR::symbol_t*>& constructor_arg_syms,
        const std::string& name, ASR::expr_t* parent_value,
        size_t n_parent_args, diag::Diagnostics& diag) {
    LCOMPILERS_ASSERT(n_parent_args <= args.size());
    ASR::call_arg_t* parent_args = nullptr;
    [[maybe_unused]] size_t n_args = 0;
    if( ASR::is_a<ASR::StructConstructor_t>(*parent_value) ) {
        ASR::StructConstructor_t* constructor =
            ASR::down_cast<ASR::StructConstructor_t>(parent_value);
        parent_args = constructor->m_args;
        n_args = constructor->n_args;
    } else if( ASR::is_a<ASR::StructConstant_t>(*parent_value) ) {
        ASR::StructConstant_t* constant =
            ASR::down_cast<ASR::StructConstant_t>(parent_value);
        parent_args = constant->m_args;
        n_args = constant->n_args;
    }
    if( parent_args != nullptr ) {
        // A parent constructor already carries one argument per component
        // of the parent, in the same order as the leading arguments here.
        LCOMPILERS_ASSERT(n_args == n_parent_args);
        for( size_t i = 0; i < n_parent_args; i++ ) {
            args.p[i] = parent_args[i];
        }
        return true;
    }
    // Any other expression is read component by component. It must be
    // evaluated exactly once, so anything that is not a designator is
    // assigned to a temporary first, and each component reference gets
    // its own copy of the base so that no node is shared between slots.
    // Where the temporary cannot be assigned the value has to be rejected:
    // reading the expression once per component would evaluate it several
    // times, and every component could then come from a different
    // evaluation.
    ASR::expr_t* base = parent_value;
    if( !is_designator(base) ) {
        ASR::expr_t* base_tmp = evaluate_into_temporary(base);
        if( base_tmp == nullptr ) {
            bool in_implied_do = idl_nesting_level > 0;
            diag.add(Diagnostic("the value given for the parent component "
                "'" + name + "' must be a constant or a variable " +
                (in_implied_do ? "inside an implied do loop" : "here") +
                ", it would otherwise be evaluated once for every "
                "component of '" + name + "'",
                Level::Error, Stage::Semantic, {
                    Label(in_implied_do
                        ? "help: assign it to a variable before the array "
                          "constructor and give that variable here"
                        : "help: give a named constant here",
                        {parent_value->base.loc})}));
            if( !compiler_options.continue_compilation ) {
                throw SemanticAbort();
            }
            return false;
        }
        base = base_tmp;
    }
    ASR::symbol_t* base_sym = ASR::is_a<ASR::Var_t>(*base)
        ? ASR::down_cast<ASR::Var_t>(base)->m_v : nullptr;
    ASRUtils::ExprStmtDuplicator expr_duplicator(al);
    for( size_t i = 0; i < n_parent_args; i++ ) {
        ASR::expr_t* base_i = expr_duplicator.duplicate_expr(base);
        args.p[i].loc = parent_value->base.loc;
        args.p[i].m_value = ASRUtils::EXPR(
            ASRUtils::getStructInstanceMember_t(al,
                parent_value->base.loc, &base_i->base, base_sym,
                constructor_arg_syms[i], current_scope));
    }
    return true;
}

ASR::expr_t* CommonVisitorBase::get_default_initialized_struct_value(ASR::symbol_t* type_sym,
        const Location& loc) {
    ASR::symbol_t* struct_sym = ASRUtils::symbol_get_past_external(type_sym);
    if (struct_sym == nullptr || !ASR::is_a<ASR::Struct_t>(*struct_sym)) {
        return nullptr;
    }
    StructConstructorInfo info = get_struct_constructor_info(struct_sym);
    if (!info.kind_indices.empty()) {
        return nullptr;
    }
    bool has_default_initialization = false;
    Vec<ASR::call_arg_t> args;
    args.reserve(al, info.members.size());
    for (ASR::symbol_t* member : info.members) {
        ASR::call_arg_t arg;
        arg.loc = loc;
        arg.m_value = nullptr;
        if (member != nullptr && ASR::is_a<ASR::Variable_t>(*member)) {
            ASR::Variable_t* member_var = ASR::down_cast<ASR::Variable_t>(member);
            if (member_var->m_value == nullptr
                    && member_var->m_symbolic_value != nullptr) {
                // The component's default initializer, such as a scalar
                // structure constructor for an array component, has no
                // constant value; it is not used here, and leaving the
                // component undefined would lose it.
                return nullptr;
            }
            arg.m_value = member_var->m_value != nullptr ? member_var->m_value
                : get_default_initialized_component_value(member_var, loc);
        }
        if (arg.m_value != nullptr) {
            has_default_initialization = true;
        }
        args.push_back(al, arg);
    }
    if (!has_default_initialization) {
        return nullptr;
    }
    return ASRUtils::EXPR(make_struct_constructor(loc, type_sym, args,
        info.members, false));
}

ASR::expr_t* CommonVisitorBase::get_default_initialized_component_value(
        ASR::Variable_t* member_var, const Location& loc) {
    if (member_var->m_value != nullptr || member_var->m_symbolic_value != nullptr
            || member_var->m_type_declaration == nullptr
            || ASRUtils::is_allocatable(member_var->m_type)
            || ASRUtils::is_pointer(member_var->m_type)
            || !ASR::is_a<ASR::StructType_t>(
                *ASRUtils::type_get_past_array(member_var->m_type))) {
        return nullptr;
    }
    return get_default_initialized_struct_value(
        member_var->m_type_declaration, loc);
}

void CommonVisitorBase::set_array_type_of_intermediate_struct_members(ASR::expr_t* expr,
    ASR::ttype_t* array_type) {
    ASR::ttype_t* shape = ASRUtils::type_get_past_allocatable_pointer(array_type);
    if( !ASR::is_a<ASR::Array_t>(*shape) ) {
        return;
    }
    ASR::Array_t* shape_array = ASR::down_cast<ASR::Array_t>(shape);
    Vec<ASR::StructInstanceMember_t*> members;
    members.reserve(al, 1);
    if( ASR::is_a<ASR::ArrayItem_t>(*expr) ) {
        expr = ASR::down_cast<ASR::ArrayItem_t>(expr)->m_v;
    }
    while( ASR::is_a<ASR::StructInstanceMember_t>(*expr) ) {
        ASR::StructInstanceMember_t* member =
            ASR::down_cast<ASR::StructInstanceMember_t>(expr);
        members.push_back(al, member);
        expr = member->m_v;
    }
    for( size_t j = members.size(); j > 0; j-- ) {
        ASR::StructInstanceMember_t* member = members[j - 1];
        if( !ASRUtils::is_array(ASRUtils::expr_type(member->m_v)) ||
            ASRUtils::is_array(member->m_type) ) {
            continue;
        }
        // An `allocatable` or `pointer` intermediate cannot be given the
        // base's shape: what it denotes is an array of indirections,
        // which this type representation cannot express, and reshaping
        // it would silently drop the indirection and make the backend
        // read the component's storage as if it held the value inline.
        // Fortran forbids such a reference anyway (C919), so leave the
        // declared type alone instead of replacing it with a wrong one.
        if( ASR::is_a<ASR::Allocatable_t>(*member->m_type) ||
            ASR::is_a<ASR::Pointer_t>(*member->m_type) ) {
            continue;
        }
        // Rebuild through `duplicate_type` rather than `make_Array_t`, so
        // the member gets its own copy of the dimensions instead of
        // aliasing the base array's.
        Vec<ASR::dimension_t> dims;
        dims.from_pointer_n_copy(al, shape_array->m_dims, shape_array->n_dims);
        member->m_type = ASRUtils::duplicate_type(al, member->m_type, &dims,
            shape_array->m_physical_type, true);
    }
}

bool CommonVisitorBase::is_iso_c_binding_c_char(AST::expr_t* kind_value) {
    if (!AST::is_a<AST::Name_t>(*kind_value)) {
        return false;
    }
    std::string name = to_lower(AST::down_cast<AST::Name_t>(kind_value)->m_id);
    ASR::symbol_t* kind_sym = current_scope->resolve_symbol(name);
    if (!kind_sym) {
        return false;
    }
    ASR::symbol_t* orig_sym = ASRUtils::symbol_get_past_external(kind_sym);
    if (!orig_sym || to_lower(ASRUtils::symbol_name(orig_sym)) != "c_char") {
        return false;
    }
    ASR::symbol_t* owner = ASRUtils::get_asr_owner(orig_sym);
    return owner && ASR::is_a<ASR::Module_t>(*owner) &&
        startswith(to_lower(ASRUtils::symbol_name(owner)),
            "lfortran_intrinsic_iso_c_binding");
}

ASR::symbol_t *CommonVisitorBase::import_generic_specific(ASR::symbol_t *specific,
        const std::string &generic_name) {
    ASR::symbol_t *target = ASRUtils::symbol_get_past_external(specific);
    if (!ASR::is_a<ASR::Function_t>(*target)) {
        return specific;
    }
    ASR::Module_t *owner = ASRUtils::get_sym_module(target);
    if (owner == nullptr) {
        return specific;
    }
    std::string name = std::string(ASRUtils::symbol_name(target)) + "@" + generic_name;
    std::string local_name = name;
    for (int i = 1; current_scope->get_symbol(local_name) != nullptr; i++) {
        ASR::symbol_t *s = current_scope->get_symbol(local_name);
        if (ASRUtils::symbol_get_past_external(s) == target) {
            return s;
        }
        local_name = name + std::to_string(i);
    }
    ASR::symbol_t *s = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
        al, target->base.loc, current_scope, s2c(al, local_name), target,
        owner->m_name, nullptr, 0, ASRUtils::symbol_name(target),
        ASR::accessType::Private));
    current_scope->add_symbol(local_name, s);
    append_module_dependency(owner->m_name);
    return s;
}

void CommonVisitorBase::import_generic(ASR::symbol_t *generic, const std::string &local_name) {
    bool is_gp = ASR::is_a<ASR::GenericProcedure_t>(*generic);
    Vec<ASR::symbol_t*> procs;
    procs.reserve(al, 1);
    // `g` is this scope's node when `in_scope`, else a generic to import.
    auto add_specifics = [&](ASR::symbol_t *g, bool in_scope) {
        ASR::symbol_t **g_procs;
        size_t n_g_procs;
        if (is_gp) {
            ASR::GenericProcedure_t *gp = ASR::down_cast<ASR::GenericProcedure_t>(g);
            g_procs = gp->m_procs;
            n_g_procs = gp->n_procs;
        } else {
            ASR::CustomOperator_t *op = ASR::down_cast<ASR::CustomOperator_t>(g);
            g_procs = op->m_procs;
            n_g_procs = op->n_procs;
        }
        for (size_t i = 0; i < n_g_procs; i++) {
            ASR::symbol_t *target = ASRUtils::symbol_get_past_external(g_procs[i]);
            bool present = false;
            for (size_t j = 0; j < procs.size() && !present; j++) {
                present = ASRUtils::symbol_get_past_external(procs[j]) == target;
            }
            if (!present) {
                procs.push_back(al, in_scope
                    ? g_procs[i] : import_generic_specific(g_procs[i], local_name));
            }
        }
    };
    ASR::symbol_t *existing = current_scope->get_symbol(local_name);
    if (existing != nullptr && existing->type != generic->type) {
        existing = nullptr;
    }
    if (existing != nullptr) {
        add_specifics(existing, true);
    }
    add_specifics(generic, false);
    if (existing != nullptr) {
        if (is_gp) {
            ASR::GenericProcedure_t *gp = ASR::down_cast<ASR::GenericProcedure_t>(existing);
            gp->m_procs = procs.p;
            gp->n_procs = procs.size();
        } else {
            ASR::CustomOperator_t *op = ASR::down_cast<ASR::CustomOperator_t>(existing);
            op->m_procs = procs.p;
            op->n_procs = procs.size();
        }
        return;
    }
    ASR::asr_t *node = is_gp
        ? ASR::make_GenericProcedure_t(al, generic->base.loc, current_scope,
            s2c(al, local_name), procs.p, procs.size(), dflt_access)
        : ASR::make_CustomOperator_t(al, generic->base.loc, current_scope,
            s2c(al, local_name), procs.p, procs.size(), dflt_access);
    current_scope->add_or_overwrite_symbol(local_name, ASR::down_cast<ASR::symbol_t>(node));
}

std::string CommonVisitorBase::import_all(const ASR::Module_t* m, bool to_submodule,
                       std::vector<std::string> symbols_already_imported_with_renaming,
                       std::set<std::string> submodule_proc_names) {
    // Import all symbols from the module, e.g.:
    //     use a
    for (auto &item : m->m_symtab->get_scope()) {
        if ( symbols_already_imported_with_renaming.size() > 0 &&
             std::find(symbols_already_imported_with_renaming.begin(),
                       symbols_already_imported_with_renaming.end(),
                       item.first) != symbols_already_imported_with_renaming.end() ) {
            continue;
        }
        if( current_scope->get_symbol(item.first) != nullptr) {
            // If the existing symbol and the incoming symbol are both
            // generic interfaces (GenericProcedure) or user-defined
            // operators (CustomOperator), merge their specific procedures
            // instead of skipping. This is required by Fortran semantics:
            // generics with the same name imported from multiple modules
            // are merged at the use-point (F2018 C1515).
            ASR::symbol_t* existing = current_scope->get_symbol(item.first);
            ASR::symbol_t* existing_past = ASRUtils::symbol_get_past_external(existing);
            ASR::symbol_t* incoming_past = ASRUtils::symbol_get_past_external(item.second);
            // Skip if the incoming symbol already resolves to the same
            // underlying symbol as the existing one (re-import of the same
            // generic via a different re-export chain).
            if (existing_past == incoming_past) {
                continue;
            }
            bool is_gp_merge = existing_past != nullptr && incoming_past != nullptr &&
                ASR::is_a<ASR::GenericProcedure_t>(*incoming_past) &&
                ASR::is_a<ASR::GenericProcedure_t>(*existing_past);
            bool is_co_merge = existing_past != nullptr && incoming_past != nullptr &&
                ASR::is_a<ASR::CustomOperator_t>(*incoming_past) &&
                ASR::is_a<ASR::CustomOperator_t>(*existing_past);
            if (!is_gp_merge && !is_co_merge) {
                continue;
            }
        }
        // TODO: only import "public" symbols from the module
        if (ASR::is_a<ASR::Function_t>(*item.second)) {
            ASR::Function_t *mfn = ASR::down_cast<ASR::Function_t>(item.second);
            // A private specific of a generic comes with the generic,
            // see import_generic.
            if (!to_submodule && mfn->m_access == ASR::accessType::Private) {
                continue;
            }
            if (to_submodule &&
                ASRUtils::get_FunctionType(mfn)->m_deftype == ASR::deftypeType::Interface &&
                ASRUtils::get_FunctionType(mfn)->m_module &&
                submodule_proc_names.count(item.first) > 0) {
                continue;
            }
            ASR::asr_t *fn = ASR::make_ExternalSymbol_t(
                al, mfn->base.base.loc,
                /* a_symtab */ current_scope,
                /* a_name */ mfn->m_name,
                (ASR::symbol_t*)mfn,
                m->m_name, nullptr, 0, mfn->m_name,
                dflt_access
                );
            std::string sym = to_lower(mfn->m_name);
            current_scope->add_symbol(sym, ASR::down_cast<ASR::symbol_t>(fn));
        } else if (ASR::is_a<ASR::GenericProcedure_t>(*item.second)) {
            ASR::GenericProcedure_t *gp = ASR::down_cast<
                ASR::GenericProcedure_t>(item.second);
            if (!to_submodule && gp->m_access == ASR::accessType::Private) {
                continue;
            }
            import_generic(item.second, to_lower(gp->m_name));
        }  else if (ASR::is_a<ASR::CustomOperator_t>(*item.second)) {
            ASR::CustomOperator_t *op = ASR::down_cast<
                ASR::CustomOperator_t>(item.second);
            if (!to_submodule && op->m_access == ASR::accessType::Private) {
                continue;
            }
            import_generic(item.second, op->m_name);
        } else if (ASR::is_a<ASR::Variable_t>(*item.second)) {
            ASR::Variable_t *mvar = ASR::down_cast<ASR::Variable_t>(item.second);
            // check if m_access of mvar is public
            if ( mvar->m_access == ASR::accessType::Public || to_submodule ) {
                ASR::asr_t *var = ASR::make_ExternalSymbol_t(
                    al, mvar->base.base.loc,
                    /* a_symtab */ current_scope,
                    /* a_name */ mvar->m_name,
                    (ASR::symbol_t*)mvar,
                    m->m_name, nullptr, 0, mvar->m_name,
                    dflt_access
                    );
                std::string sym = to_lower(mvar->m_name);
                current_scope->add_symbol(sym, ASR::down_cast<ASR::symbol_t>(var));
            }
        } else if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) {
            // We have to "repack" the ExternalSymbol so that it lives in the
            // local symbol table
            ASR::ExternalSymbol_t *es0 = ASR::down_cast<ASR::ExternalSymbol_t>(item.second);
            if ((!to_submodule) && es0->m_access == ASR::accessType::Private) {
                continue;
            }
            ASR::asr_t *es = ASR::make_ExternalSymbol_t(
                al, es0->base.base.loc,
                /* a_symtab */ current_scope,
                /* a_name */ s2c(al, item.first),
                es0->m_external,
                es0->m_module_name, es0->m_scope_names, es0->n_scope_names,
                es0->m_original_name,
                dflt_access
                );
            current_scope->add_or_overwrite_symbol(item.first, ASR::down_cast<ASR::symbol_t>(es));
            // If the symbol originates from a different module than the one
            // we are directly importing from, add the origin module as a
            // transitive dependency so that the verify pass does not fail.
            // Only do this for top-level modules (whose parent is the
            // TranslationUnit), not for nested modules like enums.
            if (std::string(es0->m_module_name) != std::string(m->m_name)) {
                ASR::symbol_t* origin_mod_sym = current_scope->get_tu_scope()->get_symbol(es0->m_module_name);
                if (origin_mod_sym && ASR::is_a<ASR::Module_t>(*origin_mod_sym)) {
                    current_module_dependencies.push_back(al, es0->m_module_name);
                }
            }
        } else if( ASR::is_a<ASR::Struct_t>(*item.second) ) {
            ASR::Struct_t *mv = ASR::down_cast<ASR::Struct_t>(item.second);
            if ((!to_submodule) && mv->m_access == ASR::accessType::Private) {
                continue;
            }
            // `mv` is the Variable in a module. Now we construct
            // an ExternalSymbol that points to it.
            Str name;
            name.from_str(al, item.first);
            char *cname = name.c_str(al);
            ASR::asr_t *v = ASR::make_ExternalSymbol_t(
                al, mv->base.base.loc,
                /* a_symtab */ current_scope,
                /* a_name */ cname,
                (ASR::symbol_t*)mv,
                m->m_name, nullptr, 0, mv->m_name,
                dflt_access
                );
            current_scope->add_symbol(item.first, ASR::down_cast<ASR::symbol_t>(v));
        } else if (ASR::is_a<ASR::Requirement_t>(*item.second)) {
            ASR::Requirement_t *req = ASR::down_cast<ASR::Requirement_t>(item.second);
            Str name;
            name.from_str(al, item.first);
            char *cname = name.c_str(al);
            ASR::asr_t *v = ASR::make_ExternalSymbol_t(
                al, req->base.base.loc,
                current_scope,
                cname,
                (ASR::symbol_t*)req,
                m->m_name, nullptr, 0, req->m_name,
                dflt_access
            );
            current_scope->add_symbol(item.first, ASR::down_cast<ASR::symbol_t>(v));
        } else if (ASR::is_a<ASR::Template_t>(*item.second)) {
            ASR::Template_t *temp = ASR::down_cast<ASR::Template_t>(item.second);
            Str name;
            name.from_str(al, item.first);
            char *cname = name.c_str(al);
            ASR::asr_t *v = ASR::make_ExternalSymbol_t(
                al, temp->base.base.loc,
                current_scope,
                cname,
                (ASR::symbol_t*)temp,
                m->m_name, nullptr, 0, temp->m_name,
                dflt_access
            );
            current_scope->add_symbol(item.first, ASR::down_cast<ASR::symbol_t>(v));
        }  else if( ASR::is_a<ASR::Union_t>(*item.second) ) {
            ASR::Union_t *mv = ASR::down_cast<ASR::Union_t>(item.second);
            // `mv` is the Variable in a module. Now we construct
            // an ExternalSymbol that points to it.
            Str name;
            name.from_str(al, item.first);
            char *cname = name.c_str(al);
            ASR::asr_t *v = ASR::make_ExternalSymbol_t(
                al, mv->base.base.loc,
                /* a_symtab */ current_scope,
                /* a_name */ cname,
                (ASR::symbol_t*)mv,
                m->m_name, nullptr, 0, mv->m_name,
                dflt_access
                );
            current_scope->add_symbol(item.first, ASR::down_cast<ASR::symbol_t>(v));
        } else if( ASR::is_a<ASR::Namelist_t>(*item.second) ) {
            ASR::Namelist_t *nml = ASR::down_cast<ASR::Namelist_t>(item.second);
            // `nml` is the Namelist in a module. Now we construct
            // an ExternalSymbol that points to it.
            Str name;
            name.from_str(al, item.first);
            char *cname = name.c_str(al);
            ASR::asr_t *v = ASR::make_ExternalSymbol_t(
                al, nml->base.base.loc,
                /* a_symtab */ current_scope,
                /* a_name */ cname,
                (ASR::symbol_t*)nml,
                m->m_name, nullptr, 0, nml->m_group_name,
                dflt_access
                );
            current_scope->add_symbol(item.first, ASR::down_cast<ASR::symbol_t>(v));
        } else if( ASR::is_a<ASR::Enum_t>(*item.second) ) {
            // Do nothing as enum variables will already be present as
            // External symbol in module from which we are importing
        } else if( ASR::is_a<ASR::CommonBlock_t>(*item.second) ) {
            // A common block is not made accessible by use association:
            // the module's view of it lays out the storage of the
            // module's own variables, which are imported themselves.
        } else if (ASR::is_a<ASR::ScopedVariableAttribute_t>(*item.second)) {
            // These declarations apply only in the declaring scope,
            // not in scopes that use its variables.
        } else {
            return item.first;
        }
    }
    return "";
}

void CommonVisitorBase::import_symbols_util(ASR::Module_t *m, std::string& msym,
                         std::string& remote_sym, std::string& local_sym,
                         std::queue<std::pair<std::string, std::string>>& to_be_imported_later,
                         const Location& loc) {
    remote_sym = to_lower(remote_sym);
    ASR::symbol_t *t = m->m_symtab->resolve_symbol(remote_sym);
    if (!t) {
        diag.add(diag::Diagnostic(
            "The symbol '" + remote_sym + "' not found in the module '" + msym + "'",
            diag::Level::Error, diag::Stage::Semantic, {
                diag::Label("", {loc})}));
        throw SemanticAbort();
    }
    if (ASR::is_a<ASR::Function_t>(*t) &&
        ASR::down_cast<ASR::Function_t>(t)->m_return_var == nullptr) {
        if (current_scope->get_symbol(local_sym) != nullptr) {
            diag.add(Diagnostic(
                "Symbol '" + local_sym + "' from module '" + m->m_name + "' shadows '" + local_sym + "' in the current scope",
                Level::Warning, Stage::Semantic, {
                    Label("", {loc})
                }
            ));
            // if the symbol exists in the current scope, we erase it
            // and write the new symbol which points to the new module
            current_scope->erase_symbol(local_sym);
        }
        ASR::Function_t *msub = ASR::down_cast<ASR::Function_t>(t);
        // `msub` is the Subroutine in a module. Now we construct
        // an ExternalSymbol that points to
        // `msub` via the `external` field.
        Str name;
        name.from_str(al, local_sym);
        ASR::asr_t *sub = ASR::make_ExternalSymbol_t(
            al, loc,
            /* a_symtab */ current_scope,
            /* a_name */ name.c_str(al),
            (ASR::symbol_t*)msub,
            m->m_name, nullptr, 0, msub->m_name,
            dflt_access
            );
        current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(sub));
    } else if (ASR::is_a<ASR::GenericProcedure_t>(*t) ||
            ASR::is_a<ASR::CustomOperator_t>(*t)) {
        import_generic(t, local_sym);
    } else if (ASR::is_a<ASR::Function_t>(*t)) {
        bool is_already_defined = false;
        ASR::symbol_t* imported_func_sym = current_scope->get_symbol(local_sym);
        if (imported_func_sym != nullptr) {
            ASR::ExternalSymbol_t* ext_sym = ASR::down_cast<ASR::ExternalSymbol_t>(imported_func_sym);
            if( ext_sym->m_external != t ) {
                is_already_defined = true;
            }
        }
        if( is_already_defined ) {
            diag.add(Diagnostic(
                "Symbol '" + local_sym + "' from module '" + m->m_name + "' shadows '" + local_sym + "' in the current scope",
                Level::Warning, Stage::Semantic, {
                    Label("", {loc})
                }
            ));
            // if the symbol exists in the current scope, we erase it
            // and write the new symbol which points to the new module
            current_scope->erase_symbol(local_sym);
        }
        ASR::Function_t *mfn = ASR::down_cast<ASR::Function_t>(t);
        // `mfn` is the Function in a module. Now we construct
        // an ExternalSymbol that points to it.
        Str name;
        name.from_str(al, local_sym);
        char *cname = name.c_str(al);
        ASR::asr_t *fn = ASR::make_ExternalSymbol_t(
            al, loc,
            /* a_symtab */ current_scope,
            /* a_name */ cname,
            (ASR::symbol_t*)mfn,
            m->m_name, nullptr, 0, mfn->m_name,
            dflt_access
            );
        current_scope->add_or_overwrite_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(fn));
    } else if (ASR::is_a<ASR::Variable_t>(*t)) {
        ASR::symbol_t* existing = current_scope->get_symbol(local_sym);
        if (existing != nullptr) {
            existing = ASRUtils::symbol_get_past_external(existing);
            ASR::symbol_t* incoming = ASRUtils::symbol_get_past_external(t);
            if (existing == incoming) {
                return;
            }
            diag.add(Diagnostic(
                "Symbol '" + local_sym + "' from module '" + m->m_name + "' shadows '" + local_sym + "' in the current scope",
                Level::Warning, Stage::Semantic, {
                    Label("", {loc})
                }
            ));
            // if the symbol exists in the current scope, we erase it
            // and write the new symbol which points to the new module
            current_scope->erase_symbol(local_sym);
        }
        ASR::Variable_t *mv = ASR::down_cast<ASR::Variable_t>(t);
        // `mv` is the Variable in a module. Now we construct
        // an ExternalSymbol that points to it.
        Str name;
        name.from_str(al, local_sym);
        char *cname = name.c_str(al);
        if (mv->m_access == ASR::accessType::Private) {
            diag.add(diag::Diagnostic(
                "Private variable `" + local_sym + "` cannot be imported",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {loc})}));
            throw SemanticAbort();
        }
        ASR::asr_t *v = ASR::make_ExternalSymbol_t(
            al, loc,
            /* a_symtab */ current_scope,
            /* a_name */ cname,
            (ASR::symbol_t*)mv,
            m->m_name, nullptr, 0, mv->m_name,
            dflt_access
            );
        current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(v));
    } else if( ASR::is_a<ASR::Struct_t>(*t) ) {
        // Check for any interface overriding a constructor for the struct
        ASR::symbol_t *interface_override_s = m->m_symtab->resolve_symbol("~" + remote_sym);
        if (interface_override_s) {
            to_be_imported_later.push(std::make_pair("~" + remote_sym, "~" + local_sym));
        }
        ASR::symbol_t* imported_struct_type = current_scope->get_symbol(local_sym);
        ASR::Struct_t *mv = ASR::down_cast<ASR::Struct_t>(t);
        if (imported_struct_type != nullptr) {
            imported_struct_type = ASRUtils::symbol_get_past_external(imported_struct_type);
            if( imported_struct_type == t ) {
                return ;
            }
            diag.add(Diagnostic(
                "Symbol '" + local_sym + "' from module '" + m->m_name + "' shadows '" + local_sym + "' in the current scope",
                Level::Warning, Stage::Semantic, {
                    Label("", {loc})
                }
            ));
            // if the symbol exists in the current scope, we erase it
            // and write the new symbol which points to the new module
            current_scope->erase_symbol(local_sym);
        }
        // `mv` is the Variable in a module. Now we construct
        // an ExternalSymbol that points to it.
        Str name;
        name.from_str(al, local_sym);
        char *cname = name.c_str(al);
        ASR::asr_t *v = ASR::make_ExternalSymbol_t(
            al, loc,
            /* a_symtab */ current_scope,
            /* a_name */ cname,
            (ASR::symbol_t*)mv,
            m->m_name, nullptr, 0, mv->m_name,
            dflt_access
            );
        current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(v));
    } else if (ASR::is_a<ASR::Requirement_t>(*t)) {
        ASR::Requirement_t *mreq = ASR::down_cast<ASR::Requirement_t>(t);
        ASR::asr_t *req = ASR::make_ExternalSymbol_t(
            al, loc,
            current_scope,
            s2c(al, local_sym),
            (ASR::symbol_t*) mreq,
            m->m_name, nullptr, 0, mreq->m_name,
            dflt_access);
        current_scope->add_or_overwrite_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(req));
    } else if (ASR::is_a<ASR::Template_t>(*t)) {
        ASR::Template_t *mtemp = ASR::down_cast<ASR::Template_t>(t);
        ASR::asr_t *temp = ASR::make_ExternalSymbol_t(
            al, loc,
            current_scope,
            s2c(al, local_sym),
            (ASR::symbol_t*) mtemp,
            m->m_name, nullptr, 0, mtemp->m_name,
            dflt_access);
        current_scope->add_or_overwrite_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(temp));
    } else if (ASR::is_a<ASR::Namelist_t>(*t)) {
        ASR::Namelist_t *nml = ASR::down_cast<ASR::Namelist_t>(t);
        ASR::asr_t *v = ASR::make_ExternalSymbol_t(
            al, loc,
            current_scope,
            s2c(al, local_sym),
            (ASR::symbol_t*) nml,
            m->m_name, nullptr, 0, nml->m_group_name,
            dflt_access);
        current_scope->add_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(v));
    } else if (ASR::is_a<ASR::ExternalSymbol_t>(*t)) {
        ASR::ExternalSymbol_t* ext_sym = ASR::down_cast<ASR::ExternalSymbol_t>(t);
        if (ext_sym->m_access == ASR::accessType::Private &&
                ASR::is_a<ASR::Variable_t>(
                    *ASRUtils::symbol_get_past_external(t))) {
            // A use-associated variable that `m` does not re-export.
            diag.add(diag::Diagnostic(
                "Private variable `" + local_sym + "` cannot be imported",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {loc})}));
            throw SemanticAbort();
        }
        ASR::asr_t* temp = ASR::make_ExternalSymbol_t(
            al, loc,
            current_scope,
            s2c(al, local_sym),
            ext_sym->m_external,
            ext_sym->m_module_name,
            ext_sym->m_scope_names, ext_sym->n_scope_names,
            ext_sym->m_original_name,
            dflt_access);
        current_scope->add_or_overwrite_symbol(local_sym, ASR::down_cast<ASR::symbol_t>(temp));
        // If the symbol originates from a different module than the one
        // we are directly importing from, add the origin module as a
        // transitive dependency so that the verify pass does not fail.
        // Only do this for top-level modules (whose parent is the
        // TranslationUnit), not for nested modules like enums.
        if (std::string(ext_sym->m_module_name) != std::string(m->m_name)) {
            ASR::symbol_t* origin_mod_sym = current_scope->get_tu_scope()->get_symbol(ext_sym->m_module_name);
            if (origin_mod_sym && ASR::is_a<ASR::Module_t>(*origin_mod_sym)) {
                current_module_dependencies.push_back(al, ext_sym->m_module_name);
            }
        }
        ASR::symbol_t *ext = ASRUtils::symbol_get_past_external(ext_sym->m_external);
        if (remote_sym.size() > 0 && remote_sym[0] != '~' &&
                ASR::is_a<ASR::Struct_t>(*ext)) {
            ASR::symbol_t *interface_override_s =
                m->m_symtab->resolve_symbol("~" + remote_sym);
            if (interface_override_s) {
                to_be_imported_later.push(
                    std::make_pair("~" + remote_sym, "~" + local_sym));
            }
        }
    } else {
        throw LCompilersException("Only Subroutines, Functions, Variables and Derived supported in 'use', found: " +
            std::to_string(t->type) + ", name is: " + std::string(ASRUtils::symbol_name(t)));
    }
}

void CommonVisitorBase::import_use_symbols(ASR::Module_t *m, const AST::Use_t &x) {
    std::string msym = to_lower(x.m_module);
    if (msym == "ieee_arithmetic" || msym == "ieee_exceptions") {
        msym = "lfortran_intrinsic_" + msym;
    }
    if (x.n_symbols == 0) {
        modules_imported_all.insert({current_scope, msym});
        std::string unsupported_sym_name = import_all(m);
        if( !unsupported_sym_name.empty() ) {
            throw LCompilersException("'" + unsupported_sym_name + "' is not supported yet for declaring with use.");
        }
    } else if ( !x.m_only_present ) {
        // Import all symbols, but there exists some
        // symbols which need to be imported with renaming e.g.:
        // use a, x => y
        modules_imported_all.insert({current_scope, msym});
        std::vector<std::string> symbols_already_imported_with_renaming;
        std::queue<std::pair<std::string, std::string>> to_be_imported_with_renaming;
        for (size_t i = 0; i < x.n_symbols; i++) {
            std::string remote_sym;
            switch (x.m_symbols[i]->type)
            {
                case AST::use_symbolType::UseSymbol: {
                    remote_sym = to_lower(AST::down_cast<AST::UseSymbol_t>(x.m_symbols[i])->m_remote_sym);
                    break;
                }
                case AST::use_symbolType::UseAssignment: {
                    remote_sym = "~assign";
                    break;
                }
                case AST::use_symbolType::IntrinsicOperator: {
                    AST::intrinsicopType op_type = AST::down_cast<AST::IntrinsicOperator_t>(x.m_symbols[i])->m_op;
                    remote_sym = intrinsic2str[op_type];
                    break;
                }
                case AST::use_symbolType::DefinedOperator: {
                    remote_sym = AST::down_cast<AST::DefinedOperator_t>(
                        x.m_symbols[i])->m_opName;

                    // Append "~~" to the begining of any custom defined operator
                    remote_sym = update_custom_op_name(remote_sym);
                    break;
                }
                case AST::use_symbolType::UseWrite: {
                    remote_sym = AST::down_cast<AST::UseWrite_t>(
                        x.m_symbols[i])->m_id;
                    if (remote_sym != "formatted" && remote_sym != "unformatted") {
                        diag.add(diag::Diagnostic(
                            "Can only be `formatted` or `unformatted`",
                            diag::Level::Error, diag::Stage::Semantic, {
                                diag::Label("", {x.m_symbols[i]->base.loc})}));
                        throw SemanticAbort();
                    }
                    remote_sym = "~write_" + remote_sym;
                    break;
                }
                case AST::use_symbolType::UseRead: {
                    remote_sym = AST::down_cast<AST::UseRead_t>(
                        x.m_symbols[i])->m_id;
                    if (remote_sym != "formatted" && remote_sym != "unformatted") {
                        diag.add(diag::Diagnostic(
                            "Can only be `formatted` or `unformatted`",
                            diag::Level::Error, diag::Stage::Semantic, {
                                diag::Label("", {x.m_symbols[i]->base.loc})}));
                        throw SemanticAbort();
                    }
                    remote_sym = "~read_" + remote_sym;
                    break;
                }
                default:
                    diag.add(diag::Diagnostic(
                        "Symbol with use not supported yet " + std::to_string(x.m_symbols[i]->type),
                        diag::Level::Error, diag::Stage::Semantic, {
                            diag::Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
            }
            std::string local_sym;
            if (AST::is_a<AST::UseSymbol_t>(*x.m_symbols[i]) &&
                AST::down_cast<AST::UseSymbol_t>(x.m_symbols[i])->m_local_rename) {
                local_sym = to_lower(AST::down_cast<AST::UseSymbol_t>(x.m_symbols[i])->m_local_rename);
            } else {
                local_sym = remote_sym;
            }
            import_symbols_util(m, msym, remote_sym, local_sym,
                                to_be_imported_with_renaming, x.m_symbols[i]->base.loc);
            symbols_already_imported_with_renaming.push_back(remote_sym);
        }
        // Importing procedures defined for overloaded operators like assignment
        // after all the user imports are complete. This avoids
        // importing the same function twice i.e., if the user has already imported
        // the required procedures manually then importing later avoids polluting the
        // symbol table.
        while( !to_be_imported_with_renaming.empty() ) {
            std::string remote_sym = to_be_imported_with_renaming.front().first;
            std::string local_sym = to_be_imported_with_renaming.front().second;
            to_be_imported_with_renaming.pop();
            if( current_scope->resolve_symbol(local_sym) == nullptr ) {
                import_symbols_util(m, msym, remote_sym, local_sym,
                                    to_be_imported_with_renaming, x.base.base.loc);
                symbols_already_imported_with_renaming.push_back(remote_sym);
            }
        }
        std::string unsupported_sym_name = import_all(m, false, symbols_already_imported_with_renaming);
        if( !unsupported_sym_name.empty() ) {
            throw LCompilersException("'" + unsupported_sym_name + "' is not supported yet for declaring with use.");
        }
    } else {
        // Only import individual symbols from the module, e.g.:
        //     use a, only: x, y, z
        std::queue<std::pair<std::string, std::string>> to_be_imported_later;
        for (size_t i = 0; i < x.n_symbols; i++) {
            std::string remote_sym;
            switch (x.m_symbols[i]->type)
            {
                case AST::use_symbolType::UseSymbol: {
                    remote_sym = to_lower(AST::down_cast<AST::UseSymbol_t>(x.m_symbols[i])->m_remote_sym);
                    break;
                }
                case AST::use_symbolType::UseAssignment: {
                    remote_sym = "~assign";
                    break;
                }
                case AST::use_symbolType::IntrinsicOperator: {
                    AST::intrinsicopType op_type = AST::down_cast<AST::IntrinsicOperator_t>(x.m_symbols[i])->m_op;
                    remote_sym = intrinsic2str[op_type];
                    break;
                }
                case AST::use_symbolType::DefinedOperator: {
                    remote_sym = AST::down_cast<AST::DefinedOperator_t>(
                        x.m_symbols[i])->m_opName;

                    // Append "~~" to the begining of any custom defined operator
                    remote_sym = update_custom_op_name(remote_sym);
                    break;
                }
                case AST::use_symbolType::UseWrite: {
                    remote_sym = AST::down_cast<AST::UseWrite_t>(
                        x.m_symbols[i])->m_id;
                    if (remote_sym != "formatted" && remote_sym != "unformatted") {
                        diag.add(diag::Diagnostic(
                            "Can only be `formatted` or `unformatted`",
                            diag::Level::Error, diag::Stage::Semantic, {
                                diag::Label("", {x.m_symbols[i]->base.loc})}));
                        throw SemanticAbort();
                    }
                    remote_sym = "~write_" + remote_sym;
                    break;
                }
                case AST::use_symbolType::UseRead: {
                    remote_sym = AST::down_cast<AST::UseRead_t>(
                        x.m_symbols[i])->m_id;
                    if (remote_sym != "formatted" && remote_sym != "unformatted") {
                        diag.add(diag::Diagnostic(
                            "Can only be `formatted` or `unformatted`",
                            diag::Level::Error, diag::Stage::Semantic, {
                                diag::Label("", {x.m_symbols[i]->base.loc})}));
                        throw SemanticAbort();
                    }
                    remote_sym = "~read_" + remote_sym;
                    break;
                }
                default:
                    diag.add(diag::Diagnostic(
                        "Symbol with use not supported yet " + std::to_string(x.m_symbols[i]->type),
                        diag::Level::Error, diag::Stage::Semantic, {
                            diag::Label("", {x.base.base.loc})}));
                    throw SemanticAbort();
            }
            std::string local_sym;
            if (AST::is_a<AST::UseSymbol_t>(*x.m_symbols[i]) &&
                AST::down_cast<AST::UseSymbol_t>(x.m_symbols[i])->m_local_rename) {
                local_sym = to_lower(AST::down_cast<AST::UseSymbol_t>(x.m_symbols[i])->m_local_rename);
                remote_sym = to_lower(remote_sym);
                if (remote_sym != local_sym) {
                    ASR::symbol_t* existing = current_scope->resolve_symbol(remote_sym);
                    if (existing && ASR::is_a<ASR::ExternalSymbol_t>(*existing)) {
                        ASR::ExternalSymbol_t* ext_sym = ASR::down_cast<ASR::ExternalSymbol_t>(existing);
                        // Only erase the bare name if it was brought into
                        // scope by an earlier *wildcard* `use` of this
                        // module (e.g. a plain `use mod`). If it was
                        // instead explicitly imported by name via a
                        // previous `use mod, only: <name>`, it was
                        // requested on purpose and must stay accessible.
                        if (std::string(ext_sym->m_module_name) == msym &&
                            modules_imported_all.find({current_scope, msym}) !=
                                modules_imported_all.end()) {
                            current_scope->erase_symbol(remote_sym);
                        }
                    }
                }
            } else {
                remote_sym = to_lower(remote_sym);
                local_sym = remote_sym;
            }
            import_symbols_util(m, msym, remote_sym, local_sym,
                                to_be_imported_later, x.m_symbols[i]->base.loc);
        }

        // Importing procedures defined for overloaded operators like assignment
        // after all the user imports are complete. This avoids
        // importing the same function twice i.e., if the user has already imported
        // the required procedures manually then importing later avoids polluting the
        // symbol table.
        while( !to_be_imported_later.empty() ) {
            std::string remote_sym = to_be_imported_later.front().first;
            std::string local_sym = to_be_imported_later.front().second;
            to_be_imported_later.pop();
            if( current_scope->resolve_symbol(local_sym) == nullptr ) {
                import_symbols_util(m, msym, remote_sym, local_sym,
                                    to_be_imported_later, x.base.base.loc);
            }
        }
    }
}

std::string CommonVisitorBase::update_custom_op_name(const std::string &op) {
    // if operator name start with (contain) "~", it is already distinct so return it as it is
    if (op.find_first_of("~") != std::string::npos) {
        return op;
    }

    // make custom operators names distinct by appending "~~" to the begining of their names
    return "~~" + op;
}

std::string CommonVisitorBase::instantiation_intrinsic_op_str(AST::intrinsicopType op) {
    switch (op) {
        case AST::intrinsicopType::AND: return ".and.";
        case AST::intrinsicopType::OR: return ".or.";
        case AST::intrinsicopType::XOR: return ".xor.";
        case AST::intrinsicopType::EQV: return ".eqv.";
        case AST::intrinsicopType::NEQV: return ".neqv.";
        case AST::intrinsicopType::PLUS: return "+";
        case AST::intrinsicopType::MINUS: return "-";
        case AST::intrinsicopType::STAR: return "*";
        case AST::intrinsicopType::DIV: return "/";
        case AST::intrinsicopType::POW: return "**";
        case AST::intrinsicopType::NOT: return ".not.";
        case AST::intrinsicopType::EQ: return "==";
        case AST::intrinsicopType::GT: return ">";
        case AST::intrinsicopType::GTE: return ">=";
        case AST::intrinsicopType::LT: return "<";
        case AST::intrinsicopType::LTE: return "<=";
        case AST::intrinsicopType::NOTEQ: return "/=";
        case AST::intrinsicopType::CONCAT: return "//";
    }
    return "";
}

bool CommonVisitorBase::instantiation_generic_spec(AST::use_symbol_t *item,
        std::string &remote_sym, std::string &local_sym, std::string &spec) {
    switch (item->type) {
        case AST::use_symbolType::UseSymbol: {
            return false;
        }
        case AST::use_symbolType::UseAssignment: {
            remote_sym = "~assign";
            spec = "assignment(=)";
            break;
        }
        case AST::use_symbolType::IntrinsicOperator: {
            AST::intrinsicopType op =
                AST::down_cast<AST::IntrinsicOperator_t>(item)->m_op;
            remote_sym = intrinsic2str[op];
            spec = "operator(" + instantiation_intrinsic_op_str(op) + ")";
            break;
        }
        case AST::use_symbolType::DefinedOperator: {
            std::string op_name = to_lower(
                AST::down_cast<AST::DefinedOperator_t>(item)->m_opName);
            remote_sym = update_custom_op_name(op_name);
            spec = "operator(." + op_name + ".)";
            break;
        }
        case AST::use_symbolType::RenameOperator: {
            AST::RenameOperator_t *rename =
                AST::down_cast<AST::RenameOperator_t>(item);
            std::string op_name = to_lower(rename->m_use_defop);
            remote_sym = update_custom_op_name(op_name);
            local_sym = update_custom_op_name(
                to_lower(rename->m_local_defop));
            spec = "operator(." + op_name + ".)";
            return true;
        }
        case AST::use_symbolType::UseWrite:
        case AST::use_symbolType::UseRead: {
            bool is_write = AST::is_a<AST::UseWrite_t>(*item);
            std::string id = to_lower(is_write
                ? AST::down_cast<AST::UseWrite_t>(item)->m_id
                : AST::down_cast<AST::UseRead_t>(item)->m_id);
            std::string kind = is_write ? "write" : "read";
            remote_sym = "~" + kind + "_" + id;
            spec = kind + "(" + id + ")";
            break;
        }
    }
    local_sym = remote_sym;
    return true;
}

ASR::symbol_t* CommonVisitorBase::make_intrinsic_procedure_wrapper(const std::string &arg,
        ASR::Function_t *f,
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>> &type_subs,
        SymbolTable *scope, const Location &loc) {
    SymbolTable *saved_scope = current_scope;
    SymbolTable *parent_scope = scope;
    current_scope = al.make_new<SymbolTable>(parent_scope);
    Vec<ASR::expr_t*> wargs;
    wargs.reserve(al, f->n_args);
    Vec<ASR::expr_t*> call_args;
    call_args.reserve(al, f->n_args);
    for (size_t j=0; j<f->n_args; j++) {
        ASR::ttype_t *var_type = ASRUtils::duplicate_type(al,
            ASRUtils::subs_expr_type(type_subs, f->m_args[j]));
        ASR::symbol_t *var_type_decl =
            ASRUtils::get_struct_sym_from_struct_expr(f->m_args[j]);
        std::string var_name = "arg" + std::to_string(j);
        ASR::asr_t *v = ASRUtils::make_Variable_t_util(al,
            loc, current_scope,
            s2c(al, var_name), nullptr, 0, ASR::intentType::In,
            nullptr, nullptr, ASR::storage_typeType::Default,
            var_type, var_type_decl, ASR::abiType::Source,
            ASR::accessType::Private, ASR::presenceType::Required,
            false);
        current_scope->add_symbol(var_name,
            ASR::down_cast<ASR::symbol_t>(v));
        ASR::expr_t *var_expr = ASRUtils::EXPR(ASR::make_Var_t(al,
            loc, current_scope->get_symbol(var_name)));
        wargs.push_back(al, var_expr);
        call_args.push_back(al, var_expr);
    }

    ASRUtils::create_intrinsic_function create_func =
        ASRUtils::IntrinsicElementalFunctionRegistry::get_create_function(arg);
    ASR::asr_t *call_value_asr = create_func(al,
        loc, call_args, diag);
    if (call_value_asr == nullptr) {
        current_scope = saved_scope;
        throw SemanticAbort();
    }
    ASR::expr_t *call_value = ASRUtils::EXPR(call_value_asr);

    ASR::ttype_t *return_type = ASRUtils::duplicate_type(al,
        ASRUtils::subs_expr_type(type_subs, f->m_return_var));
    ASR::asr_t *return_v = ASRUtils::make_Variable_t_util(al,
        loc, current_scope, s2c(al, "ret"),
        nullptr, 0, ASR::intentType::ReturnVar, nullptr, nullptr,
        ASR::storage_typeType::Default, return_type,
        ASRUtils::get_struct_sym_from_struct_expr(call_value),
        ASR::abiType::Source, ASR::accessType::Private,
        ASR::presenceType::Required, false);
    current_scope->add_symbol("ret",
        ASR::down_cast<ASR::symbol_t>(return_v));
    ASR::expr_t *return_expr = ASRUtils::EXPR(ASR::make_Var_t(al,
        loc, current_scope->get_symbol("ret")));

    if (!ASRUtils::check_equal_type(
            ASRUtils::expr_type(call_value), return_type,
            call_value, f->m_return_var)) {
        diag.add(Diagnostic(
            "Unapplicable types for intrinsic function " + arg,
            Level::Error, Stage::Semantic, {
                Label("", {loc})}));
        current_scope = saved_scope;
        throw SemanticAbort();
    }

    Vec<ASR::stmt_t*> body;
    body.reserve(al, 1);
    ASRUtils::make_ArrayBroadcast_t_util(al,
        loc, return_expr, call_value);
    ASR::stmt_t *assignment = ASRUtils::STMT(
        ASRUtils::make_Assignment_t_util(al,
            loc, return_expr, call_value,
            nullptr, false, false));
    body.push_back(al, assignment);

    std::string func_name = parent_scope->get_unique_name(
        arg + "_intrinsic");
    ASR::FunctionType_t *req_type =
        ASR::down_cast<ASR::FunctionType_t>(f->m_function_signature);
    ASR::asr_t *op_function = ASRUtils::make_Function_t_util(
        al, loc, current_scope,
        s2c(al, func_name), nullptr, 0, wargs.p, wargs.size(),
        body.p, body.size(), return_expr,
        ASR::abiType::Source, ASR::accessType::Public,
        ASR::deftypeType::Implementation, nullptr,
        req_type->m_elemental, req_type->m_pure,
        req_type->m_module, req_type->m_inline,
        req_type->m_static, nullptr, 0, /* is_restriction */ false,
        /* deterministic */ true, /* side_effect_free */ true);
    ASR::symbol_t *op_sym =
        ASR::down_cast<ASR::symbol_t>(op_function);
    parent_scope->add_symbol(func_name, op_sym);

    current_scope = saved_scope;
    return op_sym;
}

ASR::symbol_t* CommonVisitorBase::make_operator_proc_visible(ASR::symbol_t *proc,
        const std::string &op_name, SymbolTable *scope) {
    ASR::symbol_t *proc_def = ASRUtils::symbol_get_past_external(proc);
    std::string name = ASRUtils::symbol_name(proc_def);
    ASR::symbol_t *existing = scope->resolve_symbol(name);
    if (existing && ASRUtils::symbol_get_past_external(existing) == proc_def) {
        return existing;
    }
    std::string local_name = name + "@" + op_name;
    existing = scope->resolve_symbol(local_name);
    if (existing && ASRUtils::symbol_get_past_external(existing) == proc_def) {
        return existing;
    }
    ASR::symbol_t *owner = ASRUtils::get_asr_owner(proc_def);
    if (owner == nullptr || !ASR::is_a<ASR::Module_t>(*owner)) {
        return proc;
    }
    if (scope->get_symbol(local_name) != nullptr) {
        local_name = scope->get_unique_name(local_name, false);
    }
    ASR::symbol_t *imported = ASR::down_cast<ASR::symbol_t>(
        ASR::make_ExternalSymbol_t(al, proc_def->base.loc, scope,
            s2c(al, local_name), proc_def, ASRUtils::symbol_name(owner),
            nullptr, 0, s2c(al, name), ASR::accessType::Private));
    scope->add_symbol(local_name, imported);
    return imported;
}

void CommonVisitorBase::bind_defined_operator_arg(const AST::AttrDefinedOperator_t &x,
        const std::string &param, ASR::symbol_t *param_sym,
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>> &type_subs,
        std::map<std::string, ASR::symbol_t*> &symbol_subs,
        SymbolTable *op_scope) {
    std::string op = to_lower(x.m_op_name);
    if (!ASR::is_a<ASR::Function_t>(*param_sym)) {
        diag.add(diag::Diagnostic(
            "the instantiation argument 'operator(." + op + ".)' for '"
            + param + "' requires a deferred procedure",
            diag::Level::Error, diag::Stage::Semantic, {
                diag::Label("", {x.base.base.loc})}));
        throw SemanticAbort();
    }
    std::string op_name = update_custom_op_name(op);
    ASR::symbol_t *op_sym = current_scope->resolve_symbol(op_name);
    ASR::symbol_t *orig_sym = op_sym
        ? ASRUtils::symbol_get_past_external(op_sym) : nullptr;
    if (!orig_sym || !ASR::is_a<ASR::CustomOperator_t>(*orig_sym)) {
        diag.add(diag::Diagnostic(
            "the defined operator '." + op + ".' is not declared",
            diag::Level::Error, diag::Stage::Semantic, {
                diag::Label("", {x.base.base.loc})}));
        throw SemanticAbort();
    }
    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(param_sym);
    ASR::CustomOperator_t *gen_proc = ASR::down_cast<ASR::CustomOperator_t>(orig_sym);
    for (size_t i = 0; i < gen_proc->n_procs; i++) {
        ASR::symbol_t *proc = gen_proc->m_procs[i];
        if (check_restriction(type_subs, symbol_subs, f, proc,
                x.base.base.loc, diag, []() { throw SemanticAbort(); }, false)) {
            symbol_subs[f->m_name] = make_operator_proc_visible(
                proc, op_name, op_scope);
            return;
        }
    }
    diag.add(diag::Diagnostic(
        "no specific procedure of the defined operator '." + op
        + ".' matches the interface of '" + param + "'",
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label("", {x.base.base.loc})}));
    throw SemanticAbort();
}

void CommonVisitorBase::bind_intrinsic_op_arg(AST::intrinsicopType op,
        const std::string &param, ASR::symbol_t *param_sym,
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>> &type_subs,
        std::map<std::string, ASR::symbol_t*> &symbol_subs,
        SymbolTable *op_scope, const Location &loc, const Location &arg_loc) {
    static const std::map<AST::intrinsicopType, std::string> op_spelling = {
        {AST::PLUS, "+"}, {AST::MINUS, "-"}, {AST::STAR, "*"},
        {AST::DIV, "/"}, {AST::POW, "**"}, {AST::EQ, "=="},
        {AST::NOTEQ, "/="}, {AST::LT, "<"}, {AST::LTE, "<="},
        {AST::GT, ">"}, {AST::GTE, ">="}, {AST::NOT, ".not."},
        {AST::AND, ".and."}, {AST::OR, ".or."}, {AST::EQV, ".eqv."},
        {AST::NEQV, ".neqv."}, {AST::XOR, ".xor."}, {AST::CONCAT, "//"}};
    if (!ASR::is_a<ASR::Function_t>(*param_sym)
            || ASR::down_cast<ASR::Function_t>(param_sym)->m_return_var == nullptr) {
        diag.add(Diagnostic("the instantiation argument 'operator("
            + op_spelling.at(op) + ")' for '" + param
            + "' requires a deferred function",
            Level::Error, Stage::Semantic, {Label("", {arg_loc})}));
        throw SemanticAbort();
    }
    ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(param_sym);
    bool is_unary = f->n_args == 1
        && (op == AST::PLUS || op == AST::MINUS || op == AST::NOT);
    ASR::binopType binop = ASR::Add;
    ASR::cmpopType cmpop = ASR::Eq;
    bool is_binop = false;
    std::string op_name;
    switch (op) {
        case (AST::PLUS):
            is_binop = true; binop = ASR::Add; op_name = "~add"; break;
        case (AST::MINUS):
            is_binop = true; binop = ASR::Sub; op_name = "~sub"; break;
        case (AST::STAR):
            is_binop = true; binop = ASR::Mul; op_name = "~mul"; break;
        case (AST::DIV):
            is_binop = true; binop = ASR::Div; op_name = "~div"; break;
        case (AST::POW):
            is_binop = true; binop = ASR::Pow; op_name = "~pow"; break;
        case (AST::EQ):
            cmpop = ASR::Eq; op_name = "~eq"; break;
        case (AST::NOTEQ):
            cmpop = ASR::NotEq; op_name = "~neq"; break;
        case (AST::LT):
            cmpop = ASR::Lt; op_name = "~lt"; break;
        case (AST::LTE):
            cmpop = ASR::LtE; op_name = "~lte"; break;
        case (AST::GT):
            cmpop = ASR::Gt; op_name = "~gt"; break;
        case (AST::GTE):
            cmpop = ASR::GtE; op_name = "~gte"; break;
        case (AST::NOT):
            if (is_unary) {
                op_name = "~not";
                break;
            }
            diag.add(Diagnostic("Unsupported binary operator",
                Level::Error, Stage::Semantic, {Label("", {arg_loc})}));
            throw SemanticAbort();
        default:
            diag.add(Diagnostic("Unsupported binary operator",
                Level::Error, Stage::Semantic, {Label("", {arg_loc})}));
            throw SemanticAbort();
    }

    ASR::symbol_t *op_sym = current_scope->resolve_symbol(op_name);
    ASR::symbol_t *orig_sym = op_sym
        ? ASRUtils::symbol_get_past_external(op_sym) : nullptr;
    if (orig_sym && ASR::is_a<ASR::CustomOperator_t>(*orig_sym)) {
        ASR::CustomOperator_t *gen_proc = ASR::down_cast<ASR::CustomOperator_t>(orig_sym);
        for (size_t i = 0; i < gen_proc->n_procs; i++) {
            ASR::symbol_t *proc = gen_proc->m_procs[i];
            if (check_restriction(type_subs, symbol_subs, f, proc, loc,
                    diag, []() { throw SemanticAbort(); }, false)) {
                symbol_subs[f->m_name] = make_operator_proc_visible(
                    proc, op_name, op_scope);
                return;
            }
        }
    }

    if (!is_unary && f->n_args != 2) {
        diag.add(Diagnostic("The restriction " + std::string(f->m_name)
            + " does not have 2 parameters", Level::Error, Stage::Semantic,
            {Label("", {loc})}));
        throw SemanticAbort();
    }

    SymbolTable *fn_scope = al.make_new<SymbolTable>(op_scope);
    Vec<ASR::expr_t*> args;
    args.reserve(al, f->n_args);
    for (size_t i = 0; i < f->n_args; i++) {
        std::string var_name = "arg" + std::to_string(i);
        ASR::symbol_t *var = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, fn_scope,
                s2c(al, var_name), nullptr, 0, ASR::intentType::In, nullptr,
                nullptr, ASR::storage_typeType::Default,
                ASRUtils::duplicate_type(al,
                    ASRUtils::subs_expr_type(type_subs, f->m_args[i])),
                ASRUtils::get_struct_sym_from_struct_expr(f->m_args[i]),
                ASR::abiType::Source, ASR::accessType::Private,
                ASR::presenceType::Required, false));
        fn_scope->add_symbol(var_name, var);
        args.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, var)));
    }

    ASR::ttype_t *return_type = nullptr;
    ASR::expr_t *value = nullptr;
    if (is_unary) {
        ASR::expr_t *operand = args[0];
        ASR::ttype_t *arg_type = ASRUtils::expr_type(operand);
        bool applies = op == AST::NOT ? ASRUtils::is_logical(*arg_type)
            : (ASRUtils::is_integer(*arg_type) || ASRUtils::is_real(*arg_type)
                || ASRUtils::is_complex(*arg_type));
        if (!applies) {
            diag.add(Diagnostic("the intrinsic operator `" + op_spelling.at(op)
                + "` cannot be applied to the argument of '"
                + std::string(f->m_name) + "', of type "
                + ASRUtils::type_to_str_fortran_symbol(arg_type, nullptr, true),
                Level::Error, Stage::Semantic, {Label("", {arg_loc})}));
            throw SemanticAbort();
        }
        value = operand;
        if (op == AST::NOT) {
            value = ASRUtils::EXPR(ASR::make_LogicalNot_t(al, loc, operand,
                arg_type, nullptr));
        } else if (op == AST::MINUS) {
            if (ASRUtils::is_integer(*arg_type)) {
                value = ASRUtils::EXPR(ASR::make_IntegerUnaryMinus_t(al, loc,
                    operand, arg_type, nullptr));
            } else if (ASRUtils::is_real(*arg_type)) {
                value = ASRUtils::EXPR(ASR::make_RealUnaryMinus_t(al, loc,
                    operand, arg_type, nullptr));
            } else {
                value = ASRUtils::EXPR(ASR::make_ComplexUnaryMinus_t(al, loc,
                    operand, arg_type, nullptr));
            }
        }
        return_type = ASRUtils::duplicate_type(al,
            ASRUtils::subs_expr_type(type_subs, f->m_return_var));
        if (!ASRUtils::check_equal_type(arg_type, return_type,
                f->m_args[0], f->m_return_var)) {
            ImplicitCastRules::set_converted_value(al, loc, &value,
                arg_type, return_type, diag);
        }
    } else if (is_binop) {
        ASR::expr_t *left = args[0];
        ASR::expr_t *right = args[1];
        ASR::ttype_t *left_type = ASRUtils::expr_type(left);
        ASR::ttype_t *right_type = ASRUtils::expr_type(right);
        ASR::expr_t **conversion_cand = &left;
        ASR::ttype_t *source_type = left_type;
        ASR::ttype_t *dest_type = right_type;
        ImplicitCastRules::find_conversion_candidate(&left, &right, left_type,
            right_type, conversion_cand, &source_type, &dest_type);
        ImplicitCastRules::set_converted_value(al, loc, conversion_cand,
            source_type, dest_type, diag);
        return_type = ASRUtils::duplicate_type(al,
            ASRUtils::subs_expr_type(type_subs, f->m_return_var));
        value = ASRUtils::EXPR(ASRUtils::make_Binop_util(al, loc, binop,
            left, right, dest_type));
        if (!ASRUtils::check_equal_type(dest_type, return_type,
                f->m_args[1], f->m_return_var)) {
            ImplicitCastRules::set_converted_value(al, loc, &value,
                dest_type, return_type, diag);
        }
    } else {
        return_type = ASRUtils::TYPE(ASR::make_Logical_t(al, loc,
            compiler_options.po.default_integer_kind));
        value = ASRUtils::EXPR(ASRUtils::make_Cmpop_util(al, loc, cmpop,
            args[0], args[1], ASRUtils::expr_type(args[0])));
    }

    ASR::symbol_t *ret = ASR::down_cast<ASR::symbol_t>(
        ASRUtils::make_Variable_t_util(al, loc, fn_scope, s2c(al, "ret"),
            nullptr, 0, ASR::intentType::ReturnVar, nullptr, nullptr,
            ASR::storage_typeType::Default, return_type,
            ASRUtils::get_struct_sym_from_struct_expr(value),
            ASR::abiType::Source, ASR::accessType::Private,
            ASR::presenceType::Required, false));
    fn_scope->add_symbol("ret", ret);
    ASR::expr_t *target = ASRUtils::EXPR(ASR::make_Var_t(al, loc, ret));
    ASRUtils::make_ArrayBroadcast_t_util(al, loc, target, value);
    Vec<ASR::stmt_t*> body;
    body.reserve(al, 1);
    body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al,
        loc, target, value, nullptr,
        compiler_options.po.realloc_lhs_arrays, false)));

    std::string func_name = op_scope->get_unique_name(op_name + "_intrinsic");
    ASR::FunctionType_t *req_type = ASR::down_cast<ASR::FunctionType_t>(
        f->m_function_signature);
    ASR::symbol_t *op_function = ASR::down_cast<ASR::symbol_t>(
        ASRUtils::make_Function_t_util(al, loc, fn_scope, s2c(al, func_name),
            nullptr, 0, args.p, args.size(), body.p, 1,
            ASRUtils::EXPR(ASR::make_Var_t(al, loc, ret)),
            ASR::abiType::Source, ASR::accessType::Public,
            ASR::deftypeType::Implementation, nullptr, req_type->m_elemental,
            req_type->m_pure, req_type->m_module, req_type->m_inline,
            req_type->m_static, nullptr, 0, /* is_restriction */ false,
            /* deterministic */ true, /* side_effect_free */ true));
    op_scope->add_symbol(func_name, op_function);
    // Bind the wrapper only to the deferred procedure, not to the
    // instantiating scope's operator interface.
    symbol_subs[f->m_name] = op_function;
}

ASR::symbol_t* CommonVisitorBase::resolve_custom_operator_proc(const std::string& intrinsic_op_name, ASR::expr_t *left, ASR::expr_t *right,
    const AST::StrOp_t &x, Vec<ASR::call_arg_t> &args) {
    ASR::symbol_t* sym = nullptr;
    ASR::symbol_t* left_symbol = nullptr;
    ASR::symbol_t* right_symbol = nullptr;
    ASR::call_arg_t arg1;
    ASR::call_arg_t arg2;

    arg1.loc = x.base.base.loc;
    arg1.m_value = left;

    arg2.loc = x.base.base.loc;
    arg2.m_value = right;

    args.reserve(al, 2);
    args.push_back(al, arg1);
    args.push_back(al, arg2);

    sym = current_scope->resolve_symbol(intrinsic_op_name);
    if (sym != nullptr) {
        // operator is defined outside both left and right symbols but inside current scope
        ASR::symbol_t *op_proc = get_custom_op_proc(sym, args, x, false);
        if (op_proc) {
            return op_proc;
        }
    }

    left_symbol = resolve_struct_symbol(left);
    if (left_symbol == nullptr)
        right_symbol = resolve_struct_symbol(right);

    ASR::symbol_t* struct_sym = left_symbol != nullptr ? left_symbol : right_symbol;

    if (struct_sym != nullptr) {
        ASR::Struct_t* op_struct = ASR::down_cast<ASR::Struct_t>(ASRUtils::symbol_get_past_external(struct_sym));
        sym = op_struct->m_symtab->resolve_symbol(intrinsic_op_name);
    }

    if (sym == nullptr) {
        diag.add(Diagnostic(
            "No custom operator exists for this expression",
            Level::Error, Stage::Semantic, {Label("", {x.base.base.loc})}));
        throw SemanticAbort();
    }

    return get_custom_op_proc(sym, args, x, true);
}

ASR::symbol_t* CommonVisitorBase::get_custom_op_proc(ASR::symbol_t *op_sym, Vec<ASR::call_arg_t>& args, const AST::StrOp_t &x, bool raise_error) {
    LCOMPILERS_ASSERT(ASR::is_a<ASR::CustomOperator_t>(*ASRUtils::symbol_get_past_external(op_sym)));

    ASR::CustomOperator_t* custom_op = ASR::down_cast<ASR::CustomOperator_t>(
    ASRUtils::symbol_get_past_external(op_sym));

    int i = ASRUtils::select_generic_procedure(args, *custom_op, x.base.base.loc,
        [&](const std::string &msg, const Location &loc) {
                diag.add(Diagnostic(msg, Level::Error, Stage::Semantic, {Label("", {loc})}));
                throw SemanticAbort();
            }, raise_error, false, &pending_implicit_dummy_procedure);

    if (i != -1) {
        return custom_op->m_procs[i];
    }

    return nullptr;
}

} // namespace LCompilers::LFortran

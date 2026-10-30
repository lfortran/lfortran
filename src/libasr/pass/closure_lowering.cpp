#include <libasr/pass/closure_lowering.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/asr_utils.h>

#include <map>
#include <set>

namespace LCompilers {
namespace {

ASR::ttype_t *represented_type(Allocator &al, ASR::ttype_t *type,
        ASR::procedure_representationType representation) {
    const Location &loc = type->base.loc;
    if (ASR::is_a<ASR::Pointer_t>(*type)) {
        return ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
            represented_type(al, ASR::down_cast<ASR::Pointer_t>(type)->m_type,
                representation)));
    }
    if (ASR::is_a<ASR::Reference_t>(*type)) {
        return ASRUtils::TYPE(ASR::make_Reference_t(al, loc,
            represented_type(al, ASR::down_cast<ASR::Reference_t>(type)->m_type,
                representation)));
    }
    if (ASR::is_a<ASR::FunctionType_t>(*type)) {
        ASRUtils::ExprStmtDuplicator duplicate(al);
        type = duplicate.duplicate_ttype(type);
        class CallableBounds: public ASR::BaseExprReplacer<CallableBounds> {
        public:
            void replace_Array(ASR::Array_t *array) {
                if (array->m_physical_type !=
                        ASR::array_physical_typeType::FixedSizeArray) {
                    for (size_t i = 0; i < array->n_dims; i++) {
                        array->m_dims[i].m_start = nullptr;
                        array->m_dims[i].m_length = nullptr;
                    }
                }
                ASR::BaseExprReplacer<CallableBounds>::replace_Array(array);
            }
        } bounds;
        // Shape/length evaluation has already been made explicit by the ABI
        // passes. Callable values carry rank and representation, not a
        // callee's private snapshot temporaries.
        bounds.replace_ttype(type);
        auto *signature = ASR::down_cast<ASR::FunctionType_t>(type);
        if (signature->m_deftype == ASR::deftypeType::ImplicitInterface &&
                signature->m_return_var_type &&
                ASR::is_a<ASR::String_t>(*signature->m_return_var_type) &&
                ASR::down_cast<ASR::String_t>(
                    signature->m_return_var_type)->m_len_kind ==
                        ASR::string_length_kindType::ExpressionLength) {
            // An external character result's declared length belongs to the
            // caller, not to the scope-free callable value or a dummy index.
            auto *result = ASR::down_cast<ASR::String_t>(
                ASR::down_cast<ASR::FunctionType_t>(type)->m_return_var_type);
            result->m_len = nullptr;
            result->m_len_kind = ASR::string_length_kindType::AssumedLength;
        }
        return ASRUtils::TYPE(ASR::make_ProcedureType_t(al, loc, type,
            representation));
    }
    return type;
}

class ProcedureStorage: public ASR::BaseWalkVisitor<ProcedureStorage> {
    Allocator &al;
public:
    std::map<ASR::symbol_t*, ASR::symbol_t*> dummy_variables;
    std::set<ASR::symbol_t*> legacy_pointers;
    std::set<const ASR::symbol_t*> visited;
    ProcedureStorage(Allocator &al_): al(al_) {}

    void convert_pointer(ASR::Function_t *function) {
        auto *scope = function->m_symtab->parent;
        std::string name = function->m_name;
        scope->erase_symbol(name);
        function->m_name = s2c(al, scope->get_unique_name(name + "_interface"));
        scope->add_symbol(function->m_name, &function->base);
        auto *signature = ASRUtils::get_FunctionType(function);
        if (signature->m_return_var_type) {
            signature->m_return_var_type = ASRUtils::type_get_past_pointer(
                signature->m_return_var_type);
            if (function->m_return_var) {
                ASRUtils::EXPR2VAR(function->m_return_var)->m_type =
                    signature->m_return_var_type;
            }
        }
        auto *type = ASRUtils::TYPE(ASR::make_Pointer_t(al,
            function->base.base.loc, represented_type(al,
                function->m_function_signature,
                ASR::procedure_representationType::Closure)));
        auto *variable = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, function->base.base.loc, scope,
                s2c(al, name), nullptr, 0, ASR::intentType::Local, nullptr,
                nullptr, ASR::storage_typeType::Default, type, &function->base,
                ASR::abiType::Source, function->m_access,
                ASR::presenceType::Required, false));
        scope->add_symbol(name, variable);
        dummy_variables[&function->base] = variable;
        legacy_pointers.insert(variable);
    }

    void visit_Function(const ASR::Function_t &x) {
        if (!visited.insert(&x.base).second) return;
        auto &function = const_cast<ASR::Function_t&>(x);
        bool raw = ASRUtils::get_FunctionType(x)->m_abi == ASR::abiType::BindC;
        for (size_t i = 0; i < x.n_args; i++) {
            auto *arg = ASR::down_cast<ASR::Var_t>(x.m_args[i]);
            auto *symbol = ASRUtils::symbol_get_past_external(arg->m_v);
            auto converted = dummy_variables.find(symbol);
            if (converted != dummy_variables.end()) {
                arg->m_v = converted->second;
                ASR::down_cast<ASR::Variable_t>(arg->m_v)->m_intent =
                    ASR::intentType::Unspecified;
                continue;
            }
            if (!ASR::is_a<ASR::Function_t>(*symbol)) continue;
            auto *interface = ASR::down_cast<ASR::Function_t>(symbol);
            std::string name = ASRUtils::symbol_name(arg->m_v);
            function.m_symtab->erase_symbol(name);
            interface->m_name = s2c(al, function.m_symtab->get_unique_name(
                name + "_interface"));
            function.m_symtab->add_symbol(interface->m_name, symbol);
            auto *type = represented_type(al, interface->m_function_signature,
                raw ? ASR::procedure_representationType::RawPointer :
                    ASR::procedure_representationType::Closure);
            auto *variable = ASR::down_cast<ASR::symbol_t>(
                ASRUtils::make_Variable_t_util(al, arg->base.base.loc,
                    function.m_symtab, s2c(al, name), nullptr, 0,
                    ASR::intentType::In, nullptr, nullptr,
                    ASR::storage_typeType::Default, type, symbol,
                    ASR::abiType::Source, ASR::accessType::Private,
                    ASR::presenceType::Required, false));
            function.m_symtab->add_symbol(name, variable);
            dummy_variables[symbol] = variable;
            arg->m_v = variable;
        }
        ASR::BaseWalkVisitor<ProcedureStorage>::visit_Function(x);
        auto *signature = ASRUtils::get_FunctionType(function);
        for (size_t i = 0; i < function.n_args; i++) {
            if (legacy_pointers.count(ASR::down_cast<ASR::Var_t>(
                    function.m_args[i])->m_v)) {
                signature->m_arg_types[i] = ASRUtils::expr_type(function.m_args[i]);
                continue;
            }
            auto *type = ASRUtils::procedure_value_type(
                ASRUtils::expr_type(function.m_args[i]));
            if (type) signature->m_arg_types[i] = represented_type(al,
                signature->m_arg_types[i], type->m_representation);
        }
        if (function.m_return_var) {
            if (auto *type = ASRUtils::procedure_value_type(
                    ASRUtils::expr_type(function.m_return_var))) {
                signature->m_return_var_type = represented_type(al,
                    signature->m_return_var_type, type->m_representation);
            }
        }
    }

    void visit_ExternalSymbol(const ASR::ExternalSymbol_t &x) {
        auto *target = ASRUtils::symbol_get_past_external(x.m_external);
        if (ASR::is_a<ASR::Variable_t>(*target) &&
                ASRUtils::is_reference(
                    ASR::down_cast<ASR::Variable_t>(target)->m_type)) {
            visit_Variable(*ASR::down_cast<ASR::Variable_t>(target));
        }
    }

    void visit_StructType(const ASR::StructType_t &x) {
        auto &type = const_cast<ASR::StructType_t&>(x);
        for (size_t i = 0; i < type.n_data_member_types; i++) {
            type.m_data_member_types[i] = represented_type(al,
                type.m_data_member_types[i], ASR::procedure_representationType::Closure);
        }
        ASR::BaseWalkVisitor<ProcedureStorage>::visit_StructType(x);
    }

    void visit_Variable(const ASR::Variable_t &x) {
        if (!visited.insert(&x.base).second) return;
        bool raw = false;
        auto *owner = x.m_parent_symtab->asr_owner;
        if (owner && ASR::is_a<ASR::symbol_t>(*owner) &&
                ASR::is_a<ASR::Function_t>(*ASR::down_cast<ASR::symbol_t>(owner)) &&
                ASRUtils::is_arg_dummy(x.m_intent)) {
            raw = ASRUtils::get_FunctionType(ASR::down_cast<ASR::Function_t>(
                ASR::down_cast<ASR::symbol_t>(owner)))->m_abi == ASR::abiType::BindC;
        }
        const_cast<ASR::Variable_t&>(x).m_type = represented_type(al, x.m_type,
            raw ? ASR::procedure_representationType::RawPointer :
                ASR::procedure_representationType::Closure);
        ASR::BaseWalkVisitor<ProcedureStorage>::visit_Variable(x);
    }
};

class ClosureValues:
    public ASR::CallReplacerOnExpressionsVisitor<ClosureValues> {
    Allocator &al;
    const std::map<ASR::symbol_t*, ASR::symbol_t*> &dummies;
    std::map<ASR::symbol_t*, ASR::symbol_t*> entries;
    diag::Diagnostics *diagnostics;

    ASR::ttype_t *forwarding_type(ASR::ttype_t *type, bool environment) {
        ASRUtils::ExprStmtDuplicator duplicate(al);
        auto *copy = duplicate.duplicate_ttype(type);
        if (environment) {
            class FrameMetadata: public ASR::BaseExprReplacer<FrameMetadata> {
            public:
                void replace_Array(ASR::Array_t *array) {
                    if (array->m_physical_type !=
                            ASR::array_physical_typeType::FixedSizeArray) {
                        for (size_t i = 0; i < array->n_dims; i++) {
                            array->m_dims[i].m_start = nullptr;
                            array->m_dims[i].m_length = nullptr;
                        }
                    }
                    ASR::BaseExprReplacer<FrameMetadata>::replace_Array(array);
                }
                void replace_String(ASR::String_t *string) {
                    if (string->m_len_kind ==
                            ASR::string_length_kindType::ExpressionLength &&
                            string->m_len &&
                            !ASR::is_a<ASR::IntegerConstant_t>(*string->m_len)) {
                        string->m_len = nullptr;
                        string->m_len_kind =
                            ASR::string_length_kindType::AssumedLength;
                    }
                }
            } metadata;
            // The entry forwards frame addresses, never source specification
            // expressions embedded in the referenced variable types.
            metadata.replace_ttype(copy);
        }
        auto *element = ASRUtils::type_get_past_allocatable_pointer(copy);
        if (ASR::is_a<ASR::Array_t>(*element)) {
            auto *array = ASR::down_cast<ASR::Array_t>(element);
            if (array->m_physical_type !=
                    ASR::array_physical_typeType::FixedSizeArray) {
                for (size_t i = 0; i < array->n_dims; i++) {
                    array->m_dims[i].m_start = nullptr;
                    array->m_dims[i].m_length = nullptr;
                }
            }
        }
        return copy;
    }

    ASR::symbol_t *entry(ASR::ProcedureClosure_t &closure) {
        auto *target = ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(closure.m_procedure)->m_v));
        auto found = entries.find(&target->base);
        if (found != entries.end()) return found->second;
        const Location &loc = closure.base.base.loc;
        SymbolTable *parent = target->m_symtab->parent;
        SymbolTable *scope = al.make_new<SymbolTable>(parent);
        ASRUtils::SymbolDuplicator duplicate(al);
        std::vector<std::pair<ASR::Variable_t*, ASR::Variable_t*>> variables;
        Vec<ASR::expr_t*> args;
        Vec<ASR::call_arg_t> actuals;
        args.reserve(al, target->n_args);
        actuals.reserve(al, target->n_args);
        ASR::expr_t *environment = nullptr;
        for (size_t i = 0; i < target->n_args; i++) {
            auto *original = ASRUtils::EXPR2VAR(target->m_args[i]);
            duplicate.duplicate_symbol(&original->base, scope);
            auto *copy = ASR::down_cast<ASR::Variable_t>(
                scope->get_symbol(original->m_name));
            copy->m_symbolic_value = nullptr;
            copy->m_value = nullptr;
            variables.push_back({original, copy});
            auto *value = ASRUtils::EXPR(ASR::make_Var_t(al, loc, &copy->base));
            actuals.push_back(al, {loc, value});
            if (&original->base == closure.m_environment_parameter) {
                copy->m_storage = ASR::storage_typeType::ClosureEnvironment;
                environment = value;
            }
        }
        LCOMPILERS_ASSERT(environment != nullptr);
        duplicate.remap_local_references(variables, scope);
        duplicate.fixup_local_type_declarations(scope, target->m_symtab);
        for (auto &variable: variables) {
            variable.second->m_symbolic_value = nullptr;
            variable.second->m_value = nullptr;
            variable.second->m_type = forwarding_type(variable.second->m_type,
                variable.second->m_storage ==
                    ASR::storage_typeType::ClosureEnvironment);
        }
        std::vector<size_t> strings;
        for (size_t i = 0; i < target->n_args; i++) {
            if (ASRUtils::is_string_dummy_with_hidden_length(*target,
                    target->m_args[i])) strings.push_back(i);
        }
        size_t first_length = target->n_args - strings.size();
        for (size_t i = 0; i < strings.size(); i++) {
            auto *type = ASRUtils::extract_type(variables[strings[i]].second->m_type);
            auto *string = ASR::down_cast<ASR::String_t>(type);
            string->m_len_kind = ASR::string_length_kindType::ExpressionLength;
            string->m_len = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                &variables[first_length + i].second->base));
        }
        args.push_back(al, environment);
        for (size_t i = 0; i < actuals.size(); i++) {
            if (actuals[i].m_value != environment) args.push_back(al, actuals[i].m_value);
        }
        Vec<ASR::stmt_t*> body;
        body.reserve(al, 1);
        ASR::expr_t *result = nullptr;
        std::string name = parent->get_unique_name(
            std::string(target->m_name) + "_closure_entry");
        if (target->m_return_var) {
            auto *original = ASRUtils::EXPR2VAR(target->m_return_var);
            duplicate.duplicate_symbol(&original->base, scope);
            auto *copy = ASR::down_cast<ASR::Variable_t>(
                scope->get_symbol(original->m_name));
            copy->m_symbolic_value = nullptr;
            copy->m_value = nullptr;
            result = ASRUtils::EXPR(ASR::make_Var_t(al, loc, &copy->base));
            auto *call = ASRUtils::EXPR(ASRUtils::make_FunctionCall_t_util(al,
                loc, &target->base, nullptr, actuals.p, actuals.size(),
                copy->m_type, nullptr, nullptr));
            body.push_back(al, ASRUtils::STMT(ASRUtils::make_Assignment_t_util(
                al, loc, result, call, nullptr, false, false)));
        } else {
            body.push_back(al, ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(
                al, loc, &target->base, nullptr, actuals.p, actuals.size(),
                nullptr, nullptr, false)));
        }
        auto *ft = ASRUtils::get_FunctionType(target);
        auto *function = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Function_t_util(al, loc, scope, s2c(al, name),
                nullptr, 0, args.p, args.size(), body.p, body.size(), result,
                ft->m_abi, ASR::accessType::Private,
                ASR::deftypeType::Implementation,
                ft->m_abi == ASR::abiType::BindC ? s2c(al, name) : nullptr,
                false, false, false, false, false, nullptr, 0, false, false, false));
        parent->add_symbol(name, function);
        entries[&target->base] = function;
        return function;
    }

public:
    ClosureValues(Allocator &al_,
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &dummies_,
            diag::Diagnostics *diagnostics_):
        al(al_), dummies(dummies_), diagnostics(diagnostics_) {}

    ASR::expr_t *raw_pointer(ASR::expr_t *value) {
        auto *type = ASRUtils::procedure_value_type(ASRUtils::expr_type(value));
        if (!type || type->m_representation ==
                ASR::procedure_representationType::RawPointer) return value;
        auto *source = value;
        while (ASR::is_a<ASR::FunctionPointerCast_t>(*source)) {
            source = ASR::down_cast<ASR::FunctionPointerCast_t>(source)->m_arg;
        }
        if (ASR::is_a<ASR::ProcedureClosure_t>(*source) &&
                ASR::down_cast<ASR::ProcedureClosure_t>(source)->m_environment &&
                diagnostics) {
            diagnostics->message_label(
                "a capturing procedure cannot be passed as a C function pointer",
                {value->base.loc}, "this procedure requires its host environment",
                diag::Level::Error, diag::Stage::ASRPass);
        }
        return ASRUtils::EXPR(ASR::make_ProcedureRawPointer_t(al,
            value->base.loc, value, ASRUtils::TYPE(ASR::make_ProcedureType_t(
                al, value->base.loc, type->m_signature,
                ASR::procedure_representationType::RawPointer))));
    }

    void visit_CLoc(const ASR::CLoc_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<ClosureValues>::visit_CLoc(x);
        const_cast<ASR::CLoc_t&>(x).m_arg = raw_pointer(x.m_arg);
    }

    void visit_PointerToCPtr(const ASR::PointerToCPtr_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<ClosureValues>::visit_PointerToCPtr(x);
        auto *value = x.m_arg;
        if (ASR::is_a<ASR::GetPointer_t>(*value)) {
            auto *argument = ASR::down_cast<ASR::GetPointer_t>(value)->m_arg;
            if (ASRUtils::procedure_value_type(ASRUtils::expr_type(argument))) {
                value = argument;
            }
        }
        const_cast<ASR::PointerToCPtr_t&>(x).m_arg = raw_pointer(value);
    }

    ASR::expr_t *fortran_value(ASR::expr_t *value, ASR::ttype_t *type) {
        return ASRUtils::EXPR(ASR::make_ProcedureClosure_t(al,
            value->base.loc, value, nullptr, nullptr, nullptr,
            ASRUtils::TYPE(ASR::make_ProcedureType_t(al, value->base.loc,
                ASRUtils::procedure_value_type(type)->m_signature,
                ASR::procedure_representationType::Closure))));
    }

    void visit_Associate(const ASR::Associate_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<ClosureValues>::visit_Associate(x);
        auto *target = ASRUtils::procedure_value_type(ASRUtils::expr_type(x.m_target));
        auto *source = ASRUtils::procedure_value_type(ASRUtils::expr_type(x.m_value));
        if (!target || !source || target->m_representation == source->m_representation) return;
        const_cast<ASR::Associate_t&>(x).m_value =
            target->m_representation == ASR::procedure_representationType::Closure
                ? fortran_value(x.m_value, ASRUtils::expr_type(x.m_target))
                : raw_pointer(x.m_value);
    }

    void visit_PointerAssociated(const ASR::PointerAssociated_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<ClosureValues>::visit_PointerAssociated(x);
        if (!x.m_tgt || !ASRUtils::is_closure_type(
                ASRUtils::expr_type(x.m_ptr))) return;
        auto *target = ASRUtils::procedure_value_type(ASRUtils::expr_type(x.m_tgt));
        if (target && target->m_representation ==
                ASR::procedure_representationType::RawPointer) {
            const_cast<ASR::PointerAssociated_t&>(x).m_tgt =
                fortran_value(x.m_tgt, ASRUtils::expr_type(x.m_ptr));
        }
    }

    void visit_ProcedureClosure(const ASR::ProcedureClosure_t &x) {
        auto &closure = const_cast<ASR::ProcedureClosure_t&>(x);
        if (closure.m_environment) {
            auto *function = entry(closure);
            closure.m_procedure = ASRUtils::EXPR(ASR::make_Var_t(al,
                x.base.base.loc, function));
            closure.m_environment_parameter = ASR::down_cast<ASR::Var_t>(
                ASR::down_cast<ASR::Function_t>(function)->m_args[0])->m_v;
            visit_expr(*closure.m_environment);
        }
        if (closure.m_interface) {
            closure.m_type = ASRUtils::import_procedure_type(al,
                ASR::down_cast<ASR::Function_t>(
                    ASRUtils::symbol_get_past_external(
                        closure.m_interface))->m_function_signature,
                current_scope);
        }
        if (!ASRUtils::procedure_value_type(closure.m_type)) {
            closure.m_type = represented_type(al, closure.m_type,
                ASR::procedure_representationType::Closure);
        }
    }

    void visit_FunctionPointerCast(const ASR::FunctionPointerCast_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<ClosureValues>::visit_FunctionPointerCast(x);
        auto *source = ASRUtils::procedure_value_type(ASRUtils::expr_type(x.m_arg));
        if (source) const_cast<ASR::FunctionPointerCast_t&>(x).m_type =
            represented_type(al, x.m_type, source->m_representation);
    }

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<ClosureValues>::visit_StructInstanceMember(x);
        auto *member = ASRUtils::symbol_get_past_external(x.m_m);
        if (!ASR::is_a<ASR::Variable_t>(*member)) return;
        auto *type = ASR::down_cast<ASR::Variable_t>(member)->m_type;
        if (ASRUtils::is_reference(type)) type =
            ASR::down_cast<ASR::Reference_t>(type)->m_type;
        if (ASRUtils::procedure_value_type(type)) {
            const_cast<ASR::StructInstanceMember_t&>(x).m_type =
                ASRUtils::import_procedure_type(al, type, current_scope);
        }
    }

    void visit_PointerNullConstant(const ASR::PointerNullConstant_t &x) {
        ASR::CallReplacerOnExpressionsVisitor<ClosureValues>::visit_PointerNullConstant(x);
        const_cast<ASR::PointerNullConstant_t&>(x).m_type =
            represented_type(al, x.m_type, ASR::procedure_representationType::Closure);
    }

    template <typename T> void call(T &x) {
        auto found = dummies.find(ASRUtils::symbol_get_past_external(x.m_name));
        if (found != dummies.end()) {
            if (ASR::is_a<ASR::ExternalSymbol_t>(*x.m_name)) {
                ASR::down_cast<ASR::ExternalSymbol_t>(x.m_name)->m_external =
                    found->second;
            } else {
                x.m_name = found->second;
            }
        }
    }
    template <typename T> void actuals(T &x) {
        auto *callee = ASRUtils::get_function(x.m_name);
        if (!callee) return;
        for (size_t i = 0; i < x.n_args && i < callee->n_args; i++) {
            auto *value = x.m_args[i].m_value;
            if (!value) continue;
            auto *formal = ASRUtils::procedure_value_type(
                ASRUtils::expr_type(callee->m_args[i]));
            if (!formal) continue;
            auto *actual = ASRUtils::procedure_value_type(ASRUtils::expr_type(value));
            if (!actual) continue;
            if (formal->m_representation == ASR::procedure_representationType::RawPointer) {
                x.m_args[i].m_value = raw_pointer(value);
            } else if (actual->m_representation ==
                    ASR::procedure_representationType::RawPointer) {
                x.m_args[i].m_value = fortran_value(value,
                    ASRUtils::expr_type(callee->m_args[i]));
            }
        }
    }
    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        call(const_cast<ASR::FunctionCall_t&>(x));
        ASR::CallReplacerOnExpressionsVisitor<ClosureValues>::visit_FunctionCall(x);
        actuals(const_cast<ASR::FunctionCall_t&>(x));
        auto *type = ASRUtils::get_FunctionType(x.m_name)->m_return_var_type;
        if (ASRUtils::procedure_value_type(type)) {
            const_cast<ASR::FunctionCall_t&>(x).m_type = type;
        }
    }
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        call(const_cast<ASR::SubroutineCall_t&>(x));
        ASR::CallReplacerOnExpressionsVisitor<ClosureValues>::visit_SubroutineCall(x);
        actuals(const_cast<ASR::SubroutineCall_t&>(x));
    }
    void call_replacer() {
        if (*current_expr == nullptr ||
                !ASR::is_a<ASR::Var_t>(**current_expr)) return;
        auto *var = ASR::down_cast<ASR::Var_t>(*current_expr);
        auto *symbol = ASRUtils::symbol_get_past_external(var->m_v);
        auto found = dummies.find(symbol);
        if (found != dummies.end()) {
            if (ASR::is_a<ASR::ExternalSymbol_t>(*var->m_v)) {
                ASR::down_cast<ASR::ExternalSymbol_t>(var->m_v)->m_external =
                    found->second;
            } else {
                var->m_v = found->second;
            }
            return;
        }
        if (!ASR::is_a<ASR::Function_t>(*symbol)) return;
        auto *signature = ASRUtils::import_procedure_type(al,
            ASRUtils::expr_type(*current_expr), current_scope);
        auto *type = represented_type(al, signature,
            ASR::procedure_representationType::Closure);
        *current_expr = ASRUtils::EXPR(ASR::make_ProcedureClosure_t(al,
            var->base.base.loc, *current_expr, var->m_v, nullptr, nullptr, type));
    }
};
}

void pass_closure_lowering(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &options) {
    if (!options.procedure_closures) return;
    ProcedureStorage storage(al);
    class PointerTargets: public ASR::BaseWalkVisitor<PointerTargets> {
    public:
        std::set<ASR::Function_t*> procedures;
        void visit_Associate(const ASR::Associate_t &x) {
            if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
                auto *symbol = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(x.m_target)->m_v);
                if (ASR::is_a<ASR::Function_t>(*symbol) &&
                        ASRUtils::is_bare_implicit_interface(
                            *ASR::down_cast<ASR::Function_t>(symbol))) {
                    procedures.insert(ASR::down_cast<ASR::Function_t>(symbol));
                }
            }
            ASR::BaseWalkVisitor<PointerTargets>::visit_Associate(x);
        }
    } pointers;
    pointers.visit_TranslationUnit(unit);
    for (auto *pointer: pointers.procedures) storage.convert_pointer(pointer);
    storage.visit_TranslationUnit(unit);
    ClosureValues values(al, storage.dummy_variables, options.diagnostics);
    values.visit_TranslationUnit(unit);
    PassUtils::UpdateDependenciesVisitor dependencies(al);
    dependencies.visit_TranslationUnit(unit);
}
}

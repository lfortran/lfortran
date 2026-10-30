#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/dtio_callbacks.h>
#include <libasr/pass/pass_utils.h>

#include <map>
#include <tuple>
#include <vector>

/*
 * A formatted output StringFormat lists in `dtio_bindings` the defined output
 * procedure (`write(formatted)`) of each derived type among its items. The
 * runtime formatter calls that procedure when such an item meets a DT edit
 * descriptor. It cannot call the procedure itself: the procedure takes an
 * assumed-shape v_list, may be type-bound (and is then called through the
 * dynamic type of a polymorphic item), and its dtv dummy may be polymorphic.
 * So this pass gives each binding a `callback` with a fixed interface,
 *
 *     subroutine __lfortran_dtio_write_wf_t(dtv, unit, iotype, n_v, v_list, iostat, iomsg)
 *         type(t), intent(in) :: dtv            ! class(t) for a polymorphic item
 *         integer(4), intent(in) :: unit, n_v
 *         character(len=*), intent(in) :: iotype
 *         integer(4), intent(in) :: v_list(n_v)
 *         integer(4), intent(out) :: iostat
 *         character(len=*), intent(inout) :: iomsg
 *         call dtv%wf(unit, iotype, v_list, iostat, iomsg)  ! or call wf(dtv, ...)
 *     end subroutine
 *
 * which after `string_length_arguments` is `lfortran_dtio_write_fn` of the
 * runtime (the lengths of iotype and iomsg follow the other arguments).
 *
 * A callback is added to the module or program that contains the statement
 * (the translation unit for an external procedure), once per procedure, type
 * and polymorphism. It runs before the passes that lower procedures and calls,
 * so the callback is lowered like any other procedure.
 *
 * Only the statements this translation unit generates code for get callbacks.
 * Under separate compilation a module read from a `.mod` file is compiled
 * into an object file of its own, which defines the callbacks of its
 * statements, so its procedures are skipped here. A callback is private, since
 * only the callback table of its statement refers to it (one in the
 * translation unit is then not exported from the object file).
 */

namespace LCompilers {

namespace {

class CollectDtioStringFormats :
    public ASR::CallReplacerOnExpressionsVisitor<CollectDtioStringFormats> {
    bool separate_compilation;

public:
    std::vector<std::pair<ASR::StringFormat_t*, SymbolTable*>> formats;

    CollectDtioStringFormats(bool separate_compilation_)
        : separate_compilation(separate_compilation_) {}

    void visit_Module(const ASR::Module_t &x) {
        if (separate_compilation && x.m_loaded_from_mod) {
            return;
        }
        ASR::CallReplacerOnExpressionsVisitor<CollectDtioStringFormats>::visit_Module(x);
    }

    void visit_StringFormat(const ASR::StringFormat_t &x) {
        if (x.n_dtio_bindings > 0) {
            formats.push_back({const_cast<ASR::StringFormat_t*>(&x), current_scope});
        }
        ASR::CallReplacerOnExpressionsVisitor<CollectDtioStringFormats>::visit_StringFormat(x);
    }
};

bool is_ancestor_or_self(SymbolTable* ancestor, SymbolTable* scope) {
    for (SymbolTable* s = scope; s != nullptr; s = s->parent) {
        if (s == ancestor) {
            return true;
        }
    }
    return false;
}

ASR::symbol_t* scope_owner(SymbolTable* scope) {
    if (scope->asr_owner && ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) {
        return ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
    }
    return nullptr;
}

// The name of `sym` preceded by the name of the module, program or procedure
// that defines it, so that same-named types or procedures of different
// program units give different names.
std::string qualified_name(ASR::symbol_t* sym) {
    std::string name = ASRUtils::symbol_name(sym);
    ASR::symbol_t* owner = ASRUtils::get_asr_owner(sym);
    if (owner != nullptr) {
        name = std::string(ASRUtils::symbol_name(owner)) + "_" + name;
    }
    return name;
}

class DtioCallbackBuilder {
    Allocator &al;
    std::map<std::tuple<SymbolTable*, ASR::symbol_t*, ASR::symbol_t*, bool>,
        ASR::symbol_t*> callbacks;

public:
    DtioCallbackBuilder(Allocator &al_) : al(al_) {}

    // The scope a callback for a statement in `scope` goes into: the module
    // or program that contains the statement, or the translation unit. A
    // derived type or procedure that is local to a procedure is not visible
    // from there, and then the callback goes into that procedure.
    // `unit_name` receives the name of the program unit of the statement.
    SymbolTable* callback_scope(SymbolTable* scope, ASR::symbol_t* struct_sym,
            ASR::symbol_t* procedure, std::string &unit_name) {
        SymbolTable* s = scope;
        unit_name = "";
        while (s->parent != nullptr) {
            ASR::symbol_t* owner = scope_owner(s);
            if (owner && (ASR::is_a<ASR::Module_t>(*owner) ||
                    ASR::is_a<ASR::Program_t>(*owner))) {
                break;
            }
            if (owner && ASR::is_a<ASR::Function_t>(*owner)) {
                unit_name = ASRUtils::symbol_name(owner);
            }
            s = s->parent;
        }
        for (ASR::symbol_t* sym : {struct_sym, procedure}) {
            SymbolTable* defined_in = ASRUtils::symbol_parent_symtab(sym);
            ASR::symbol_t* owner = scope_owner(defined_in);
            if (owner && ASR::is_a<ASR::Function_t>(*owner) &&
                    !is_ancestor_or_self(defined_in, s) &&
                    is_ancestor_or_self(defined_in, scope)) {
                s = defined_in;
            }
        }
        return s;
    }

    // `target`, as a symbol visible from `scope`.
    ASR::symbol_t* import_symbol(SymbolTable* scope, ASR::symbol_t* target) {
        if (ASRUtils::is_visible_from(target, scope)) {
            return target;
        }
        ASR::symbol_t* imported = ASRUtils::import_symbol(al, target, scope,
            ASR::accessType::Private, "1_");
        if (!ASRUtils::is_visible_from(imported, scope)) {
            throw LCompilersException("dtio_callbacks: `" +
                std::string(ASRUtils::symbol_name(target)) +
                "` is not visible from the callback");
        }
        return imported;
    }

    ASR::symbol_t* make_callback(const Location &loc, SymbolTable* parent,
            const std::string &unit_name, ASR::symbol_t* procedure,
            ASR::symbol_t* struct_sym, bool polymorphic) {
        ASRUtils::ASRBuilder b(al, loc);
        ASR::symbol_t* target = ASRUtils::symbol_get_past_external(procedure);
        bool type_bound = ASR::is_a<ASR::StructMethodDeclaration_t>(*target);
        // A binding is named by the type it belongs to, a procedure by the
        // program unit that defines it.
        std::string proc_name = type_bound
            ? std::string(ASR::down_cast<ASR::StructMethodDeclaration_t>(target)->m_name)
            : qualified_name(target);
        // The name includes the type, qualified by the program unit that
        // defines it, and the polymorphism, so that the callbacks of a scope
        // have distinct names (an overriding binding has the name of the
        // binding it overrides, and types of different modules may have the
        // same name). A callback in the translation unit, for a statement in
        // an external procedure, also includes the name of that procedure.
        std::string base_name = "__lfortran_dtio_write_" + proc_name + "_"
            + qualified_name(struct_sym) + (polymorphic ? "_class" : "");
        if (ASRUtils::is_tu_scope(parent) && !unit_name.empty()) {
            base_name += "_" + unit_name;
        }
        std::string fn_name = parent->get_unique_name(base_name);
        SymbolTable* fn_symtab = al.make_new<SymbolTable>(parent);

        ASR::ttype_t* i32 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
        auto assumed_length_string = [&]() {
            return ASRUtils::TYPE(ASR::make_String_t(al, loc, 1, nullptr,
                ASR::string_length_kindType::AssumedLength,
                ASR::string_physical_typeType::DescriptorString));
        };
        Vec<ASR::expr_t*> args;
        args.reserve(al, 7);
        // dtv is type(t), or class(t) for a polymorphic item.
        ASR::symbol_t* struct_decl = import_symbol(fn_symtab, struct_sym);
        ASR::expr_t* dtv = b.Variable(fn_symtab, "dtv",
            ASRUtils::make_StructType_t_util(al, loc, struct_decl, !polymorphic),
            ASR::intentType::In, struct_decl);
        ASR::expr_t* unit = b.Variable(fn_symtab, "unit", i32, ASR::intentType::In);
        ASR::expr_t* iotype = b.Variable(fn_symtab, "iotype",
            assumed_length_string(), ASR::intentType::In);
        ASR::expr_t* n_v = b.Variable(fn_symtab, "n_v", i32, ASR::intentType::In);
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = b.i32(1);
        dim.m_length = n_v;
        dims.push_back(al, dim);
        SetChar v_list_dependencies;
        v_list_dependencies.reserve(al, 1);
        v_list_dependencies.push_back(al, s2c(al, "n_v"));
        ASR::symbol_t* v_list_sym = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Variable_t_util(al, loc, fn_symtab, s2c(al, "v_list"),
                v_list_dependencies.p, v_list_dependencies.n, ASR::intentType::In,
                nullptr, nullptr, ASR::storage_typeType::Default,
                ASRUtils::TYPE(ASR::make_Array_t(al, loc, i32, dims.p, dims.n,
                    ASR::array_physical_typeType::PointerArray,
                    ASR::memory_spaceType::Global)),
                nullptr, ASR::abiType::Source, ASR::Public,
                ASR::presenceType::Required, false, false, false, nullptr, false, false,
                false, ASR::pass_attrType::NotMethod, nullptr));
        fn_symtab->add_symbol("v_list", v_list_sym);
        ASR::expr_t* v_list = ASRUtils::EXPR(ASR::make_Var_t(al, loc, v_list_sym));
        ASR::expr_t* iostat = b.Variable(fn_symtab, "iostat", i32, ASR::intentType::Out);
        ASR::expr_t* iomsg = b.Variable(fn_symtab, "iomsg",
            assumed_length_string(), ASR::intentType::InOut);

        for (ASR::expr_t* arg : {dtv, unit, iotype, n_v, v_list, iostat, iomsg}) {
            args.push_back(al, arg);
        }

        // The call of the defined output procedure.
        ASR::symbol_t* a_name = nullptr;
        ASR::symbol_t* a_original_name = nullptr;
        ASR::expr_t* a_dt = nullptr;
        if (type_bound) {
            a_name = ASRUtils::import_class_procedure(al, loc, target, fn_symtab);
            a_original_name = ASR::is_a<ASR::ExternalSymbol_t>(*a_name) ? a_name
                : ASR::down_cast<ASR::StructMethodDeclaration_t>(target)->m_proc;
            a_dt = dtv;
        } else {
            a_name = import_symbol(fn_symtab, target);
            a_original_name = a_name;
        }
        ASR::Function_t* proc_fn = ASRUtils::get_function(a_name);
        LCOMPILERS_ASSERT(proc_fn->n_args == 6);
        ASR::expr_t* v_list_actual = v_list;
        ASR::ttype_t* v_list_dummy_type = ASRUtils::expr_type(proc_fn->m_args[3]);
        if (ASRUtils::is_array(v_list_dummy_type) &&
                ASRUtils::extract_physical_type(v_list_dummy_type) ==
                    ASR::array_physical_typeType::DescriptorArray) {
            v_list_actual = ASRUtils::cast_to_descriptor(al, v_list);
        }
        Vec<ASR::call_arg_t> call_args;
        call_args.reserve(al, 6);
        for (ASR::expr_t* value : {dtv, unit, iotype, v_list_actual, iostat, iomsg}) {
            if (a_dt != nullptr && value == dtv) {
                continue;
            }
            ASR::call_arg_t call_arg;
            call_arg.loc = loc;
            call_arg.m_value = value;
            call_args.push_back(al, call_arg);
        }
        ASR::call_arg_t* m_args = call_args.p;
        size_t n_args = call_args.n;
        ASRUtils::insert_self_arg(al, a_name, m_args, n_args, a_dt);
        Vec<ASR::stmt_t*> body;
        body.reserve(al, 1);
        body.push_back(al, ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(al, loc,
            a_name, a_original_name, m_args, n_args, a_dt, nullptr, false)));

        SetChar dependencies;
        dependencies.reserve(al, 1);
        if (!ASR::is_a<ASR::ExternalSymbol_t>(*a_name) &&
                ASRUtils::symbol_parent_symtab(a_name) != fn_symtab) {
            dependencies.push_back(al, ASRUtils::symbol_name(a_name));
        }

        ASR::symbol_t* callback = ASR::down_cast<ASR::symbol_t>(
            ASRUtils::make_Function_t_util(al, loc, fn_symtab, s2c(al, fn_name),
                dependencies.p, dependencies.n, args.p, args.n, body.p, body.n,
                nullptr, ASR::abiType::Source, ASR::accessType::Private,
                ASR::deftypeType::Implementation, nullptr, false, false, false,
                false, false, nullptr, 0, false, false, false));
        parent->add_symbol(fn_name, callback);
        return callback;
    }

    void build(ASR::StringFormat_t* x, SymbolTable* scope) {
        for (size_t i = 0; i < x->n_dtio_bindings; i++) {
            ASR::dtio_binding_t &binding = x->m_dtio_bindings[i];
            if (binding.m_callback != nullptr) {
                continue;
            }
            ASR::symbol_t* struct_sym = ASRUtils::symbol_get_past_external(
                binding.m_type_declaration);
            ASR::symbol_t* target = ASRUtils::symbol_get_past_external(
                binding.m_procedure);
            ASR::symbol_t* proc_def = target;
            if (ASR::is_a<ASR::StructMethodDeclaration_t>(*target)) {
                proc_def = struct_sym;
            }
            std::string unit_name;
            SymbolTable* parent = callback_scope(scope, struct_sym, proc_def,
                unit_name);
            auto key = std::make_tuple(parent, target, struct_sym,
                binding.m_polymorphic);
            auto found = callbacks.find(key);
            if (found == callbacks.end()) {
                ASR::symbol_t* callback = make_callback(binding.loc, parent,
                    unit_name, binding.m_procedure, struct_sym,
                    binding.m_polymorphic);
                found = callbacks.insert({key, callback}).first;
            }
            binding.m_callback = ASRUtils::EXPR(ASR::make_Var_t(al,
                binding.loc, found->second));
        }
    }
};

} // namespace

void pass_dtio_callbacks(Allocator &al, ASR::TranslationUnit_t &unit,
                         const PassOptions &pass_options) {
    CollectDtioStringFormats collect(pass_options.separate_compilation);
    collect.visit_TranslationUnit(unit);
    if (collect.formats.empty()) {
        return;
    }
    // The bindings are shared by the copies of a StringFormat that passes
    // and the duplicator make, so a binding gets its callback once.
    DtioCallbackBuilder builder(al);
    for (auto &format : collect.formats) {
        builder.build(format.first, format.second);
    }
}

} // namespace LCompilers

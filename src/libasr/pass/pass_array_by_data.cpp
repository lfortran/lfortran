#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/asr_serialization_visitor.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/array_by_data.h>
#include <libasr/asr_builder.h>
#include <libasr/pass/intrinsic_array_function_registry.h>

#include <algorithm>
#include <vector>
#include <utility>
#include <deque>
#include <set>
#include <map>
#include <tuple>

/*
This ASR to ASR pass can be called whenever you want to avoid
using descriptors for passing arrays to functions/subroutines
in the backend. By default, it is currently being used by all
the backends (via pass manager). Backends like WASM which
do not support array descriptors always need this pass
to be called.

The possibility to avoid descriptors and pass array by data
to functions/subroutines is determined by ASRUtils::is_pass_array_by_data_possible
defined in asr_utils.h.

Advantages and dis-advantages of this pass are as follows,

Advantages:

* Avoiding array descriptors and just using simple data points leads to
easier handling in the backend.

* A lot of indirection to access dimensional information is also avoided
because it is already provided via extra variable arguments.

Dis-advantages:

* Requires access to all the code, as function interfaces have to be modified. Hence, not always possible.

* The arrays become contiguous, which most of the time is fine, but sometimes you might lose performance.

*/


namespace LCompilers {

// After duplicating a function body and its symbol table, BlockCall statements
// still reference the original Block symbols. This visitor remaps them to the
// corresponding duplicated Blocks in the new symbol table.
class BlockCallRemapper : public ASR::BaseWalkVisitor<BlockCallRemapper> {
    SymbolTable* new_symtab;
public:
    BlockCallRemapper(SymbolTable* new_symtab_) : new_symtab(new_symtab_) {}

    void visit_BlockCall(const ASR::BlockCall_t& x) {
        ASR::BlockCall_t& xx = const_cast<ASR::BlockCall_t&>(x);
        if (ASR::is_a<ASR::Block_t>(*xx.m_m)) {
            std::string block_name = ASR::down_cast<ASR::Block_t>(xx.m_m)->m_name;
            ASR::symbol_t* new_block = new_symtab->get_symbol(block_name);
            if (new_block) {
                xx.m_m = new_block;
            }
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t& x) {
        ASR::AssociateBlockCall_t& xx = const_cast<ASR::AssociateBlockCall_t&>(x);
        if (ASR::is_a<ASR::AssociateBlock_t>(*xx.m_m)) {
            std::string block_name = ASR::down_cast<ASR::AssociateBlock_t>(xx.m_m)->m_name;
            ASR::symbol_t* new_block = new_symtab->get_symbol(block_name);
            if (new_block) {
                xx.m_m = new_block;
            }
        }
    }
};

static void remap_block_calls(Vec<ASR::stmt_t*>& body,
        SymbolTable* /*old_symtab*/, SymbolTable* new_symtab) {
    BlockCallRemapper remapper(new_symtab);
    for (size_t i = 0; i < body.size(); i++) {
        remapper.visit_stmt(*body[i]);
    }
}

// Whether `x`, a BLOCK in it or a procedure it contains has a variable
// that keeps its value between calls: one with the SAVE attribute, explicit
// or implied by an initializer, or a threadprivate one (as in the check of
// wrap_descriptor_entries).
static bool has_saved_state(ASR::Function_t* x) {
    std::vector<SymbolTable*> scopes{x->m_symtab};
    for (size_t i = 0; i < scopes.size(); i++) {
        for (auto& item : scopes[i]->get_scope()) {
            ASR::symbol_t* sym = item.second;
            if (ASR::is_a<ASR::Variable_t>(*sym)) {
                ASR::storage_typeType storage =
                    ASR::down_cast<ASR::Variable_t>(sym)->m_storage;
                if (storage == ASR::storage_typeType::Save ||
                        storage == ASR::storage_typeType::Threadprivate) {
                    return true;
                }
            } else if (ASR::is_a<ASR::Block_t>(*sym) ||
                    ASR::is_a<ASR::AssociateBlock_t>(*sym) ||
                    ASR::is_a<ASR::Function_t>(*sym)) {
                scopes.push_back(ASRUtils::symbol_symtab(sym));
            }
        }
    }
    return false;
}

// The field that holds the object of `x`, if `x` is an inquiry about the
// bounds, shape, rank, contiguity, length or type parameters of a data
// object (F2018 10.1.11): ArraySize, ArrayBound, ArrayRank, SHAPE,
// ArrayIsContiguous, StringLen or TypeInquiry (KIND, BIT_SIZE,
// STORAGE_SIZE, ...). nullptr otherwise, or if the inquiry has no object.
static ASR::expr_t** inquiry_object(ASR::expr_t& x) {
    switch (x.type) {
        case ASR::exprType::ArraySize:
            return &ASR::down_cast<ASR::ArraySize_t>(&x)->m_v;
        case ASR::exprType::ArrayBound:
            return &ASR::down_cast<ASR::ArrayBound_t>(&x)->m_v;
        case ASR::exprType::ArrayRank:
            return &ASR::down_cast<ASR::ArrayRank_t>(&x)->m_v;
        case ASR::exprType::ArrayIsContiguous:
            return &ASR::down_cast<ASR::ArrayIsContiguous_t>(&x)->m_array;
        case ASR::exprType::StringLen:
            return &ASR::down_cast<ASR::StringLen_t>(&x)->m_arg;
        case ASR::exprType::TypeInquiry: {
            ASR::TypeInquiry_t* inquiry = ASR::down_cast<ASR::TypeInquiry_t>(&x);
            return inquiry->m_arg ? &inquiry->m_arg : nullptr;
        }
        case ASR::exprType::IntrinsicArrayFunction: {
            ASR::IntrinsicArrayFunction_t* f = ASR::down_cast<ASR::IntrinsicArrayFunction_t>(&x);
            return f->m_arr_intrinsic_id == static_cast<int64_t>(
                ASRUtils::IntrinsicArrayFunctions::Shape) && f->n_args > 0 ?
                &f->m_args[0] : nullptr;
        }
        default:
            return nullptr;
    }
}

// The designator of which `x` is a section, element, substring, component
// or complex part (F2018 9.4.4), or which `x` casts to another physical
// array or string type, or nullptr if `x` is none of these. These are all
// the nodes a designator is made of: the frontend folds a type parameter
// inquiry (`w%s%len`, `w%c%kind`) to a constant or to an inquiry.
static ASR::expr_t* designator_base(const ASR::expr_t& x) {
    switch (x.type) {
        case ASR::exprType::ComplexRe:
            return ASR::down_cast<ASR::ComplexRe_t>(&x)->m_arg;
        case ASR::exprType::ComplexIm:
            return ASR::down_cast<ASR::ComplexIm_t>(&x)->m_arg;
        case ASR::exprType::StringPhysicalCast:
            return ASR::down_cast<ASR::StringPhysicalCast_t>(&x)->m_arg;
        case ASR::exprType::ArraySection:
            return ASR::down_cast<ASR::ArraySection_t>(&x)->m_v;
        case ASR::exprType::ArrayItem:
            return ASR::down_cast<ASR::ArrayItem_t>(&x)->m_v;
        case ASR::exprType::StringSection:
            return ASR::down_cast<ASR::StringSection_t>(&x)->m_arg;
        case ASR::exprType::StringItem:
            return ASR::down_cast<ASR::StringItem_t>(&x)->m_arg;
        case ASR::exprType::StructInstanceMember:
            return ASR::down_cast<ASR::StructInstanceMember_t>(&x)->m_v;
        case ASR::exprType::ArrayPhysicalCast:
            return ASR::down_cast<ASR::ArrayPhysicalCast_t>(&x)->m_arg;
        default:
            return nullptr;
    }
}

// The variable of which the designator `x` is a part, or nullptr if `x` is
// not a designator.
static ASR::symbol_t* designator_variable(ASR::expr_t* x) {
    while (ASR::expr_t* base = designator_base(*x)) x = base;
    return ASR::is_a<ASR::Var_t>(*x) ? ASR::down_cast<ASR::Var_t>(x)->m_v : nullptr;
}

// Every symbol that an ASR node, or a symbol it owns, refers to. A module
// file has to resolve each such reference, so the serialization visitor
// writes all of them; this one records them and writes nothing else.
//
// Each reference comes with whether it only designates the object of an
// inquiry (inquiry_object): a variable, or a section, element, substring,
// component, complex part or physical cast of one, through any number of
// these (designator_base). The
// subscripts and the types met on the way, and any other object, such as a
// function reference, are references like any other.
struct SymbolReference {
    ASR::symbol_t* symbol;
    bool inquiry;
};

class SymbolReferences : public ASR::SerializationBaseVisitor<SymbolReferences> {
    typedef ASR::SerializationBaseVisitor<SymbolReferences> Base;
    // Whether the expression being visited designates an inquired object.
    bool inquired = false;
    // The object of the inquiry being visited.
    const ASR::expr_t* object = nullptr;
    // The base of the designator being visited, which was visited already.
    const ASR::expr_t* visited_base = nullptr;

public:
    std::vector<SymbolReference> symbols;

    void write_symbol(const ASR::symbol_t& x) {
        symbols.push_back({const_cast<ASR::symbol_t*>(&x), inquired});
    }
    template <typename... T> void write_int8(T...) {}
    template <typename... T> void write_int64(T...) {}
    template <typename... T> void write_bool(T...) {}
    template <typename... T> void write_string(T...) {}
    template <typename... T> void write_real(T...) {}
    template <typename... T> void write_complex_part(T...) {}
    template <typename... T> void write_void(T...) {}

    void visit_expr(const ASR::expr_t& x) {
        // Each designator node visits its base first.
        if (&x == visited_base) {
            visited_base = nullptr;
            return;
        }
        bool outer_inquired = inquired;
        const ASR::expr_t* outer_object = object;
        const ASR::expr_t* base = designator_base(x);
        inquired = &x == object && (ASR::is_a<ASR::Var_t>(x) || base);
        if (inquired && base) {
            object = base;
            visit_expr(*base);
            visited_base = base;
            inquired = false;
        }
        ASR::expr_t** inquiry = inquiry_object(const_cast<ASR::expr_t&>(x));
        object = inquiry ? *inquiry : nullptr;
        Base::visit_expr(x);
        inquired = outer_inquired;
        object = outer_object;
    }
};

// The symbol of `scope` that `sym` is, or is declared in, or nullptr if
// `sym` is not in `scope`.
static ASR::symbol_t* symbol_of_scope(SymbolTable* scope, ASR::symbol_t* sym) {
    for (SymbolTable* s = ASRUtils::symbol_parent_symtab(sym); s; s = s->parent) {
        if (s == scope) return sym;
        if (!s->asr_owner || !ASR::is_a<ASR::symbol_t>(*s->asr_owner)) return nullptr;
        sym = ASR::down_cast<ASR::symbol_t>(s->asr_owner);
    }
    return nullptr;
}

// Whether a procedure whose body was replaced can still hold `sym`, one of
// its own symbols other than its dummies and result: a declaration that has
// neither code nor state of the procedure. A local variable would be
// initialized or finalized again on entry, and an internal procedure or a
// BLOCK refers to what the old body used.
static bool is_declaration_only(ASR::symbol_t* sym) {
    switch (sym->type) {
        case ASR::symbolType::Variable: {
            // A COMMON variable lives in the storage of its block. An entry
            // initialized local holds a bound of a dummy, which every entry
            // evaluates from the dummies and host entities.
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
            return var->m_storage == ASR::storage_typeType::Parameter ||
                var->m_common_block || ASRUtils::is_entry_initialized_local(*var);
        }
        case ASR::symbolType::Function:
            return ASRUtils::get_FunctionType(ASR::down_cast<ASR::Function_t>(sym))
                ->m_deftype == ASR::deftypeType::Interface;
        case ASR::symbolType::Struct:
        case ASR::symbolType::Union:
        case ASR::symbolType::Enum:
        case ASR::symbolType::CommonBlock:
        case ASR::symbolType::ExternalSymbol:
        case ASR::symbolType::ScopedVariableAttribute:
        case ASR::symbolType::GenericProcedure:
        case ASR::symbolType::CustomOperator:
            return true;
        default:
            return false;
    }
}

// Whether `sym` is an internal procedure of the scope `scope` that has a
// body.
static bool is_internal_procedure(SymbolTable* scope, ASR::symbol_t* sym) {
    return ASR::is_a<ASR::Function_t>(*sym) &&
        ASR::down_cast<ASR::Function_t>(sym)->m_symtab->parent == scope &&
        ASRUtils::get_FunctionType(ASR::down_cast<ASR::Function_t>(sym))
            ->m_deftype == ASR::deftypeType::Implementation;
}

// Whether a local variable that a forwarder keeps only for inquiries can
// be declared with the type shape_only_type() makes of its own: one that is
// neither a pointer nor allocatable (whose shape is deferred), nor a
// procedure variable, and that is a scalar or an array whose physical type
// does not depend on its element type.
static bool has_shape_only_type(ASR::Variable_t* var) {
    ASR::ttype_t* type = var->m_type;
    if (ASRUtils::is_pointer(type) || ASRUtils::is_allocatable(type) ||
            ASR::is_a<ASR::FunctionType_t>(*ASRUtils::extract_type(type))) return false;
    if (!ASR::is_a<ASR::Array_t>(*type)) return true;
    ASR::array_physical_typeType physical = ASR::down_cast<ASR::Array_t>(type)->m_physical_type;
    return physical == ASR::array_physical_typeType::DescriptorArray ||
        physical == ASR::array_physical_typeType::PointerArray ||
        physical == ASR::array_physical_typeType::FixedSizeArray;
}

// An integer(1) type with the rank, dimensions and physical type of `type`.
static ASR::ttype_t* shape_only_type(Allocator& al, ASR::ttype_t* type) {
    const Location& loc = type->base.loc;
    ASR::ttype_t* byte = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 1));
    if (!ASR::is_a<ASR::Array_t>(*type)) return byte;
    ASR::Array_t* array = ASR::down_cast<ASR::Array_t>(type);
    return ASRUtils::TYPE(ASR::make_Array_t(al, loc, byte, array->m_dims, array->n_dims,
        array->m_physical_type, array->m_memory_space));
}

// The symbols of `f` that its signature needs: its dummies, its result and,
// transitively, every symbol of `f` that their declarations refer to (types,
// bounds, lengths, type declarations, derived type components, interfaces).
// Returns false if one of them is neither is_declaration_only(), nor an
// internal procedure whose own signature closure succeeds, nor a variable.
// No specification expression refers to such a symbol (a BLOCK, say).
//
// A variable that is not a declaration has storage, which the forwarder
// keeps although it never uses it: for an automatic object, the dimensions
// and lengths that its declaration evaluates are needed. The specification
// expressions of a signature refer to a local variable only as the object
// of inquiries (F2018 10.1.11), such as `x(ubound(w, 1))` with an automatic
// `w(n + 1)` or `x(size(w(1)%c, d))`. Such a local, if it has_shape_only_type(),
// is added to `inquired_only`, and StandIns::apply() replaces its
// inquiries by inquiries about stand-ins. Other variables are kept too:
// one that only declarations which are never evaluated (the signatures of
// interfaces and the types of procedure variables) refer to, whose value
// then does not matter, an ac-implied-do variable, which the signature
// defines before using it, and one an inquiry about which has no stand-in.
// The forwarder must neither initialize nor finalize a local that it keeps:
// StandIns::covers() checks that every kept local whose declaration would
// do something on entry or exit gets stand-ins, and the copy is not
// forwarded otherwise.
//
// An internal procedure that the signature needs, as the interface of a
// dummy procedure, contributes only its own signature, which `interfaces`
// maps it to: a procedure whose body was replaced keeps it as an interface
// (make_interface). That interface is never evaluated, so a variable that
// only its declarations refer to is kept without its value mattering. For
// example, a bound that is a host dummy reads it through the frame pointer
// of the host (nested_vars), which is kept, and through getters, which
// become interfaces too.
static bool signature_closure(ASR::Function_t* f, std::set<ASR::symbol_t*>& closure,
        std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>>& interfaces,
        std::set<ASR::symbol_t*>& inquired_only, bool evaluated_signature = true) {
    std::vector<ASR::symbol_t*> pending;
    for (size_t i = 0; i < f->n_args; i++) {
        pending.push_back(ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v);
    }
    if (f->m_return_var) {
        pending.push_back(ASR::down_cast<ASR::Var_t>(f->m_return_var)->m_v);
    }
    closure.insert(pending.begin(), pending.end());
    // The symbols that only the signatures of interfaces need.
    std::set<ASR::symbol_t*> unevaluated;
    if (!evaluated_signature) unevaluated.insert(pending.begin(), pending.end());
    // The variables that the signature keeps although they have storage,
    // and the symbols that it refers to other than as the object of an
    // inquiry.
    std::set<ASR::symbol_t*> kept, not_inquired;
    while (!pending.empty()) {
        ASR::symbol_t* sym = pending.back();
        pending.pop_back();
        std::vector<SymbolReference> references;
        // Neither the signature of an interface nor the type of a
        // procedure variable is evaluated.
        bool evaluated = !unevaluated.count(sym) && !(ASR::is_a<ASR::Variable_t>(*sym) &&
            ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_pointer(
                ASR::down_cast<ASR::Variable_t>(sym)->m_type)));
        if (interfaces.count(sym)) {
            // The internal procedure's signature can refer to symbols of
            // `f` by host association.
            evaluated = false;
            for (ASR::symbol_t* item : interfaces[sym]) {
                SymbolReferences item_references;
                item_references.visit_symbol(*item);
                references.insert(references.end(),
                    item_references.symbols.begin(), item_references.symbols.end());
            }
        } else {
            SymbolReferences sym_references;
            sym_references.visit_symbol(*sym);
            references = sym_references.symbols;
        }
        for (auto& [reference, inquiry] : references) {
            ASR::symbol_t* needed = symbol_of_scope(f->m_symtab, reference);
            if (!needed) continue;
            if (!inquiry || needed != reference) not_inquired.insert(needed);
            if (!closure.insert(needed).second) {
                // Reached again by a declaration that is evaluated.
                if (!evaluated || !unevaluated.erase(needed)) continue;
            } else if (is_internal_procedure(f->m_symtab, needed)) {
                if (!signature_closure(ASR::down_cast<ASR::Function_t>(needed),
                        interfaces[needed], interfaces, inquired_only, false)) return false;
                pending.push_back(needed);
                continue;
            }
            if (!is_declaration_only(needed)) {
                if (!ASR::is_a<ASR::Variable_t>(*needed)) return false;
                kept.insert(needed);
            }
            if (!evaluated) unevaluated.insert(needed);
            pending.push_back(needed);
        }
    }
    for (ASR::symbol_t* sym : kept) {
        ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
        if (var->m_intent == ASR::intentType::Local && !not_inquired.count(sym) &&
                has_shape_only_type(var)) {
            inquired_only.insert(sym);
        }
    }
    return true;
}

// StandIns::apply() declares each variable of `inquired_only` (see
// signature_closure), a variable of `copy` or of an interface in it that
// the signature refers to only as the object of inquiries, as an integer(1)
// object of the same shape, with neither an initializer nor a type
// declaration, and makes each inquiry about it, through any designator,
// ask a stand-in that gives the same answer:
//
// - an inquiry about the shape (SIZE, LBOUND, UBOUND, SHAPE, RANK,
//   IS_CONTIGUOUS) asks about the designator rebuilt on integer(1) objects:
//   the variable itself, and for an array component of a scalar, an object
//   with the bounds that the component declares, which are constant. A
//   component of an array, or an element of one, has the shape of the
//   array but, not being a whole array, has lower bounds 1, so for LBOUND
//   and UBOUND it becomes a section of all of the array (F2018 9.5.2,
//   16.9.109);
// - LEN asks about a scalar character of the declared length, or about
//   the same substring of it;
// - an inquiry about the type (KIND, BIT_SIZE, DIGITS, STORAGE_SIZE, ...)
//   asks about a scalar of the same intrinsic or character type, or about
//   a disassociated pointer of the same derived type.
//
// The stand-ins are never used otherwise, so they are neither initialized
// nor finalized and take little storage. A variable some inquiry about
// which has no such stand-in keeps its type and its inquiries: one about a
// component of a parameterized type, an allocatable or pointer component,
// the contiguity of a component, or the STORAGE_SIZE of a component of a
// derived type that the scope does not know. covers() refuses a forwarder
// that would keep such a variable, or any other local, whose declaration
// does something on entry or exit.
class StandIns {
    Allocator& al;
    // The variables whose inquiries ask stand-ins.
    std::set<ASR::symbol_t*> replaced;
    // The stand-ins made, by their scope, the variable or component whose
    // shape, length or type they have, and that kind of stand-in.
    enum Kind { Shape, Length, Type };
    std::map<std::tuple<SymbolTable*, ASR::symbol_t*, int, std::string>, ASR::symbol_t*> made;
    // The variables whose declarations now refer to stand-ins.
    std::set<ASR::symbol_t*> changed;

    // Every inquiry about a variable of `variables`, with the variable
    // whose declaration it is in, if any.
    struct Inquiries : public ASR::BaseWalkVisitor<Inquiries> {
        const std::set<ASR::symbol_t*>& variables;
        std::vector<std::pair<ASR::expr_t*, ASR::symbol_t*>> found;
        std::set<ASR::expr_t*> seen;
        ASR::symbol_t* declaration = nullptr;
        Inquiries(const std::set<ASR::symbol_t*>& variables) : variables(variables) {}
        void visit_Variable(const ASR::Variable_t& x) {
            declaration = const_cast<ASR::symbol_t*>(&x.base);
            ASR::BaseWalkVisitor<Inquiries>::visit_Variable(x);
            declaration = nullptr;
        }
        void visit_expr(const ASR::expr_t& x) {
            ASR::expr_t* e = const_cast<ASR::expr_t*>(&x);
            ASR::expr_t** object = inquiry_object(*e);
            if (object && variables.count(designator_variable(*object)) &&
                    seen.insert(e).second) {
                found.push_back({e, declaration});
            }
            ASR::BaseWalkVisitor<Inquiries>::visit_expr(x);
        }
    };

    static bool constant(ASR::expr_t* e, int64_t& value) {
        return e && ASRUtils::extract_value(ASRUtils::expr_value(e), value);
    }

    // The member variable that the component `x` selects.
    static ASR::Variable_t* member(const ASR::StructInstanceMember_t* x) {
        ASR::symbol_t* m = ASRUtils::symbol_get_past_external(x->m_m);
        return ASR::is_a<ASR::Variable_t>(*m) ? ASR::down_cast<ASR::Variable_t>(m) : nullptr;
    }

    // Whether the array component `var` has constant bounds.
    static bool constant_bounds(ASR::Variable_t* var) {
        if (ASRUtils::is_pointer(var->m_type) || ASRUtils::is_allocatable(var->m_type)) {
            return false;
        }
        ASR::dimension_t* dims;
        size_t rank = ASRUtils::extract_dimensions_from_ttype(var->m_type, dims);
        for (size_t i = 0; i < rank; i++) {
            int64_t value = 0;
            if (!constant(dims[i].m_start, value) || !constant(dims[i].m_length, value)) {
                return false;
            }
        }
        return true;
    }

    // The designator of which `x` is a substring, or a cast to another
    // physical string type, or `x`.
    static ASR::expr_t* without_substrings(ASR::expr_t* x) {
        while (ASR::is_a<ASR::StringSection_t>(*x) || ASR::is_a<ASR::StringItem_t>(*x) ||
                ASR::is_a<ASR::StringPhysicalCast_t>(*x)) {
            x = designator_base(*x);
        }
        return x;
    }

    // The last component that the designator `x` selects, or nullptr.
    static ASR::StructInstanceMember_t* last_component(ASR::expr_t* x) {
        for (; x; x = designator_base(*x)) {
            if (ASR::is_a<ASR::StructInstanceMember_t>(*x)) {
                return ASR::down_cast<ASR::StructInstanceMember_t>(x);
            }
        }
        return nullptr;
    }

    // The declared length of the character designator `x`, or of the one
    // of which it is a substring, if a stand-in can have it: that of the
    // variable, or a constant one of a component.
    static ASR::expr_t* length(ASR::expr_t* x) {
        x = without_substrings(x);
        ASR::StructInstanceMember_t* component = last_component(x);
        ASR::ttype_t* type = component ? (member(component) ? member(component)->m_type : nullptr) :
            ASRUtils::symbol_type(designator_variable(x));
        if (!type) return nullptr;
        ASR::ttype_t* element = ASRUtils::type_get_past_array(type);
        if (!ASR::is_a<ASR::String_t>(*element)) return nullptr;
        ASR::String_t* string = ASR::down_cast<ASR::String_t>(element);
        int64_t value = 0;
        if (string->m_len_kind != ASR::string_length_kindType::ExpressionLength ||
                !string->m_len || (component && !constant(string->m_len, value))) {
            return nullptr;
        }
        return string->m_len;
    }

    // Whether the shape of the designator `x` has a stand-in (shape()).
    static bool has_shape(ASR::expr_t* x, bool contiguity) {
        switch (x->type) {
            case ASR::exprType::Var:
                return true;
            case ASR::exprType::ArrayPhysicalCast:
            case ASR::exprType::StringPhysicalCast:
            case ASR::exprType::ArraySection:
            case ASR::exprType::ArrayItem:
            case ASR::exprType::ComplexRe:
            case ASR::exprType::ComplexIm:
                return has_shape(designator_base(*x), contiguity);
            case ASR::exprType::StructInstanceMember: {
                ASR::StructInstanceMember_t* c = ASR::down_cast<ASR::StructInstanceMember_t>(x);
                ASR::Variable_t* var = member(c);
                if (contiguity || !var) return false;
                if (ASRUtils::is_array(var->m_type) &&
                        !ASRUtils::is_array(ASRUtils::expr_type(c->m_v))) {
                    return constant_bounds(var);
                }
                return has_shape(c->m_v, contiguity);
            }
            default:
                return false;
        }
    }

    // The symbol that declares the derived type of the designator `x` in
    // the scope of its variable, or nullptr if there is none.
    static ASR::symbol_t* type_declaration(ASR::expr_t* x) {
        ASR::symbol_t* var = designator_variable(x);
        ASR::StructInstanceMember_t* component = last_component(x);
        if (!component) return ASR::down_cast<ASR::Variable_t>(var)->m_type_declaration;
        if (!member(component) || !member(component)->m_type_declaration) return nullptr;
        ASR::symbol_t* type = ASRUtils::symbol_get_past_external(
            member(component)->m_type_declaration);
        for (SymbolTable* s = ASRUtils::symbol_parent_symtab(var); s; s = s->parent) {
            for (auto& item : s->get_scope()) {
                if (ASRUtils::symbol_get_past_external(item.second) == type) {
                    return item.second;
                }
            }
        }
        return nullptr;
    }

    // Whether the inquiry `x` has a stand-in.
    static bool has_stand_in(ASR::expr_t* x) {
        ASR::expr_t* object = *inquiry_object(*x);
        if (ASR::is_a<ASR::StringLen_t>(*x)) return length(object) != nullptr;
        if (ASR::is_a<ASR::TypeInquiry_t>(*x)) {
            ASR::ttype_t* element = ASRUtils::type_get_past_array(
                ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(object)));
            return ASRUtils::is_integer(*element) || ASRUtils::is_real(*element) ||
                ASRUtils::is_complex(*element) || ASRUtils::is_logical(*element) ||
                (ASRUtils::is_character(*element) && length(object)) ||
                (ASR::is_a<ASR::StructType_t>(*element) && type_declaration(object));
        }
        return has_shape(object, ASR::is_a<ASR::ArrayIsContiguous_t>(*x));
    }

    // The variable of `scope` that stands in for `key`, of `type`. A type
    // stand-in has the type of its designator, which for a complex part
    // differs from that of the component, so its type is part of its key.
    ASR::expr_t* stand_in(SymbolTable* scope, ASR::symbol_t* key, Kind kind,
            ASR::ttype_t* type, const Location& loc,
            ASR::symbol_t* type_declaration = nullptr) {
        ASR::symbol_t*& sym = made[std::make_tuple(scope, key, (int) kind,
            kind == Type ? ASRUtils::get_type_code(type) : std::string())];
        if (!sym) {
            static const char* suffixes[] = {"_shape", "_len", "_type"};
            std::string name = scope->get_unique_name(std::string("__libasr_") +
                ASRUtils::symbol_name(ASRUtils::symbol_get_past_external(key)) +
                suffixes[kind], false);
            SetChar dependencies;
            dependencies.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, dependencies, type, nullptr,
                nullptr, name);
            sym = ASR::down_cast<ASR::symbol_t>(ASRUtils::make_Variable_t_util(al, loc,
                scope, s2c(al, name), dependencies.p, dependencies.size(),
                ASR::intentType::Local, nullptr, nullptr, ASR::storage_typeType::Default,
                type, type_declaration, ASR::abiType::Source, ASR::accessType::Private,
                ASR::presenceType::Required, false));
            scope->add_symbol(name, sym);
        }
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
    }

    // The character designator `x` rebuilt on a scalar character stand-in
    // with the declared length of the designator of which `x` is a
    // substring, or of `x` itself: the substrings, with the same bounds,
    // have the same lengths.
    ASR::expr_t* length_stand_in(SymbolTable* scope, ASR::expr_t* x) {
        const Location& loc = x->base.loc;
        if (ASR::is_a<ASR::StringPhysicalCast_t>(*x)) {
            return length_stand_in(scope, ASR::down_cast<ASR::StringPhysicalCast_t>(x)->m_arg);
        }
        if (ASR::is_a<ASR::StringSection_t>(*x)) {
            ASR::StringSection_t* s = ASR::down_cast<ASR::StringSection_t>(x);
            return ASRUtils::EXPR(ASR::make_StringSection_t(al, loc,
                length_stand_in(scope, s->m_arg), s->m_start, s->m_end, s->m_step,
                ASRUtils::type_get_past_array(s->m_type), nullptr));
        }
        if (ASR::is_a<ASR::StringItem_t>(*x)) {
            ASR::StringItem_t* s = ASR::down_cast<ASR::StringItem_t>(x);
            return ASRUtils::EXPR(ASR::make_StringItem_t(al, loc,
                length_stand_in(scope, s->m_arg), s->m_idx,
                ASRUtils::type_get_past_array(s->m_type), nullptr));
        }
        ASR::StructInstanceMember_t* component = last_component(x);
        ASR::symbol_t* key = component ? component->m_m : designator_variable(x);
        ASR::String_t* string = ASR::down_cast<ASR::String_t>(
            ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(x))));
        ASR::expr_t* len = length(x);
        ASR::ttype_t* len_type = ASRUtils::expr_type(len);
        int64_t value = 0;
        if (constant(len, value)) {
            len = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                std::max<int64_t>(value, 0), len_type, ASR::integerbozType::Decimal));
        } else {
            // A negative declared length is zero (F2018 7.4.4.2), which a
            // scalar has to be declared with.
            Vec<ASR::expr_t*> args;
            args.reserve(al, 2);
            args.push_back(al, len);
            args.push_back(al, ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 0,
                len_type, ASR::integerbozType::Decimal)));
            len = ASRUtils::EXPR(ASRUtils::make_IntrinsicElementalFunction_t_util(al, loc,
                static_cast<int64_t>(ASRUtils::IntrinsicElementalFunctions::Max),
                args.p, args.size(), 0, len_type, nullptr));
        }
        return stand_in(scope, key, Length, ASRUtils::TYPE(ASR::make_String_t(al, loc,
            string->m_kind, len, ASR::string_length_kindType::ExpressionLength,
            string->m_physical_type)), loc);
    }

    // The section `v(:, ..., :)` of all of the integer(1) array `v`.
    ASR::expr_t* whole_section(ASR::expr_t* v) {
        const Location& loc = v->base.loc;
        ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(v)->m_v;
        ASR::ttype_t* index = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
        size_t rank = ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(v));
        Vec<ASR::array_index_t> indices;
        indices.reserve(al, rank);
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, rank);
        for (size_t i = 0; i < rank; i++) {
            ASR::expr_t* dim = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                i + 1, index, ASR::integerbozType::Decimal));
            ASR::array_index_t ai;
            ai.loc = loc;
            ai.m_left = ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym)), dim, index,
                ASR::arrayboundType::LBound, nullptr));
            ai.m_right = ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc,
                ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym)), dim, index,
                ASR::arrayboundType::UBound, nullptr));
            ai.m_step = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, 1, index,
                ASR::integerbozType::Decimal));
            indices.push_back(al, ai);
            ASR::dimension_t d;
            d.loc = loc;
            d.m_start = nullptr;
            d.m_length = nullptr;
            dims.push_back(al, d);
        }
        ASR::ttype_t* type = ASRUtils::TYPE(ASR::make_Array_t(al, loc,
            ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 1)), dims.p, dims.size(),
            ASR::array_physical_typeType::DescriptorArray, ASR::memory_spaceType::Global));
        return ASRUtils::EXPR(ASR::make_ArraySection_t(al, loc, v, indices.p,
            indices.size(), type, nullptr));
    }

    // The designator `x` (has_shape()) rebuilt on integer(1) stand-ins of
    // the same shape, and of the same bounds if `bounds`.
    ASR::expr_t* shape(SymbolTable* scope, ASR::expr_t* x, bool bounds) {
        const Location& loc = x->base.loc;
        switch (x->type) {
            case ASR::exprType::ArrayPhysicalCast: {
                ASR::ArrayPhysicalCast_t* c = ASR::down_cast<ASR::ArrayPhysicalCast_t>(x);
                ASR::expr_t* base = shape(scope, c->m_arg, bounds);
                ASR::array_physical_typeType physical =
                    ASRUtils::extract_physical_type(ASRUtils::expr_type(base));
                if (physical == c->m_new) return base;
                return ASRUtils::EXPR(ASR::make_ArrayPhysicalCast_t(al, loc, base,
                    physical, c->m_new, shape_only_type(al, c->m_type), nullptr));
            }
            case ASR::exprType::StringPhysicalCast:
                return shape(scope, designator_base(*x), bounds);
            case ASR::exprType::ComplexRe:
            case ASR::exprType::ComplexIm: {
                // A complex part of an array is not a whole array, so its
                // lower bounds are 1 (F2018 16.9.109).
                ASR::expr_t* base = shape(scope, designator_base(*x), bounds);
                ASR::expr_t* whole = base;
                while (ASR::is_a<ASR::ArrayPhysicalCast_t>(*whole)) {
                    whole = designator_base(*whole);
                }
                return bounds && ASRUtils::is_array(ASRUtils::expr_type(whole)) &&
                    ASR::is_a<ASR::Var_t>(*whole) ? whole_section(whole) : base;
            }
            case ASR::exprType::ArraySection: {
                ASR::ArraySection_t* s = ASR::down_cast<ASR::ArraySection_t>(x);
                return ASRUtils::EXPR(ASR::make_ArraySection_t(al, loc, shape(scope, s->m_v, bounds),
                    s->m_args, s->n_args, shape_only_type(al, s->m_type), nullptr));
            }
            case ASR::exprType::ArrayItem: {
                ASR::ArrayItem_t* item = ASR::down_cast<ASR::ArrayItem_t>(x);
                // An element of an array component of an array has the shape
                // of that array.
                if (ASRUtils::is_array(item->m_type)) return shape(scope, item->m_v, bounds);
                return ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc, shape(scope, item->m_v, bounds),
                    item->m_args, item->n_args, shape_only_type(al, item->m_type),
                    item->m_storage_format, nullptr, item->m_in_bounds));
            }
            case ASR::exprType::StructInstanceMember: {
                ASR::StructInstanceMember_t* c = ASR::down_cast<ASR::StructInstanceMember_t>(x);
                ASR::Variable_t* var = member(c);
                if (ASRUtils::is_array(var->m_type) &&
                        !ASRUtils::is_array(ASRUtils::expr_type(c->m_v))) {
                    ASR::dimension_t* declared;
                    size_t rank = ASRUtils::extract_dimensions_from_ttype(var->m_type, declared);
                    Vec<ASR::dimension_t> dims;
                    dims.reserve(al, rank);
                    for (size_t i = 0; i < rank; i++) {
                        int64_t start = 0, length = 0;
                        constant(declared[i].m_start, start);
                        constant(declared[i].m_length, length);
                        ASR::ttype_t* index = ASRUtils::expr_type(declared[i].m_length);
                        ASR::dimension_t d;
                        d.loc = loc;
                        d.m_start = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                            start, index, ASR::integerbozType::Decimal));
                        d.m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc,
                            length, index, ASR::integerbozType::Decimal));
                        dims.push_back(al, d);
                    }
                    return stand_in(scope, c->m_m, Shape, ASRUtils::TYPE(ASR::make_Array_t(al,
                        loc, ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 1)), dims.p,
                        dims.size(), ASR::array_physical_typeType::FixedSizeArray,
                        ASR::memory_spaceType::Global)), loc);
                }
                ASR::expr_t* base = shape(scope, c->m_v, bounds);
                return bounds && ASRUtils::is_array(ASRUtils::expr_type(base)) &&
                    ASR::is_a<ASR::Var_t>(*base) ? whole_section(base) : base;
            }
            default:
                return x;
        }
    }

    // The object that the inquiry `x` (has_stand_in()) asks instead.
    ASR::expr_t* replacement(ASR::expr_t* x) {
        ASR::expr_t* object = *inquiry_object(*x);
        SymbolTable* scope = ASRUtils::symbol_parent_symtab(designator_variable(object));
        if (ASR::is_a<ASR::StringLen_t>(*x)) return length_stand_in(scope, object);
        if (ASR::is_a<ASR::TypeInquiry_t>(*x)) {
            ASR::ttype_t* element = ASRUtils::type_get_past_array(
                ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(object)));
            if (ASRUtils::is_character(*element)) return length_stand_in(scope, object);
            if (ASR::is_a<ASR::StructType_t>(*element)) {
                // STORAGE_SIZE: a pointer, which is neither initialized nor
                // finalized, of the same type.
                ASR::symbol_t* declaration = type_declaration(object);
                return stand_in(scope, declaration, Type, ASRUtils::TYPE(
                    ASR::make_Pointer_t(al, x->base.loc, element)), x->base.loc,
                    declaration);
            }
            ASR::StructInstanceMember_t* component = last_component(object);
            return stand_in(scope, component ? component->m_m : designator_variable(object),
                Type, element, x->base.loc);
        }
        return shape(scope, object, ASR::is_a<ASR::ArrayBound_t>(*x));
    }

    // Whether `var` is a variable that a procedure creates on every call:
    // a local that is neither saved, nor in COMMON, nor a named constant,
    // nor an entry initialized local.
    static bool is_unsaved_local(ASR::Variable_t* var) {
        return var->m_intent == ASR::intentType::Local &&
            var->m_storage == ASR::storage_typeType::Default && !var->m_common_block &&
            !ASRUtils::is_entry_initialized_local(*var);
    }

    // Whether an unsaved local of `type` has a size known only at run time
    // (an automatic object, F2018 8.3): a bound or length that is not a
    // constant. Allocatable and pointer objects have no such storage.
    static bool is_automatic(ASR::ttype_t* type) {
        if (ASRUtils::is_allocatable(type) || ASRUtils::is_pointer(type)) return false;
        ASR::dimension_t* dims = nullptr;
        size_t rank = ASRUtils::extract_dimensions_from_ttype(type, dims);
        int64_t value = 0;
        for (size_t i = 0; i < rank; i++) {
            if (!constant(dims[i].m_start, value) || !constant(dims[i].m_length, value)) {
                return true;
            }
        }
        ASR::ttype_t* element = ASRUtils::type_get_past_array(type);
        if (!ASR::is_a<ASR::String_t>(*element)) return false;
        ASR::String_t* string = ASR::down_cast<ASR::String_t>(element);
        return string->m_len_kind != ASR::string_length_kindType::ExpressionLength ||
            !constant(string->m_len, value);
    }

    // Whether declaring an unsaved local of `type` can do anything on
    // entry or exit other than reserving storage of a size known at
    // compile time. Every type other than a numeric or logical one counts:
    // a derived type can have default initialization, finalization,
    // allocatable components or character components, and the storage of
    // a character object can be allocated on entry and freed on exit.
    static bool has_effects(ASR::ttype_t* type) {
        if (ASRUtils::is_allocatable(type) || ASRUtils::is_pointer(type)) return false;
        ASR::ttype_t* element = ASRUtils::type_get_past_array(type);
        return !(ASRUtils::is_integer(*element) || ASRUtils::is_real(*element) ||
            ASRUtils::is_complex(*element) || ASRUtils::is_logical(*element)) ||
            is_automatic(type);
    }

    // The variables of `inquired_only` whose inquiries, `found`, all have
    // stand-ins.
    static std::set<ASR::symbol_t*> with_stand_ins(
            const std::vector<std::pair<ASR::expr_t*, ASR::symbol_t*>>& found,
            const std::set<ASR::symbol_t*>& inquired_only) {
        std::set<ASR::symbol_t*> result = inquired_only;
        for (auto& [inquiry, declaration] : found) {
            if (!has_stand_in(inquiry)) {
                result.erase(designator_variable(*inquiry_object(*inquiry)));
            }
        }
        return result;
    }

public:
    StandIns(Allocator& al) : al(al) {}

    // Whether apply() leaves no local that the forwarder of `copy` keeps,
    // its signature closure `signature` with `interfaces` and
    // `inquired_only` (see signature_closure), doing anything on entry or
    // exit other than reserving storage of a size known at compile time:
    // every such local gets stand-ins. Called before the body of `copy` is
    // replaced, so it walks only the declarations that the forwarder keeps.
    static bool covers(const std::set<ASR::symbol_t*>& signature,
            const std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>>& interfaces,
            const std::set<ASR::symbol_t*>& inquired_only) {
        Inquiries inquiries(inquired_only);
        std::vector<ASR::symbol_t*> pending(signature.begin(), signature.end());
        while (!pending.empty()) {
            ASR::symbol_t* sym = pending.back();
            pending.pop_back();
            auto interface = interfaces.find(sym);
            if (interface == interfaces.end()) {
                inquiries.visit_symbol(*sym);
            } else {
                pending.insert(pending.end(), interface->second.begin(),
                    interface->second.end());
            }
        }
        std::set<ASR::symbol_t*> replaced = with_stand_ins(inquiries.found, inquired_only);
        for (ASR::symbol_t* sym : signature) {
            if (!ASR::is_a<ASR::Variable_t>(*sym) || replaced.count(sym)) continue;
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
            if (is_unsaved_local(var) && has_effects(var->m_type)) return false;
        }
        return true;
    }

    void apply(ASR::Function_t* copy, const std::set<ASR::symbol_t*>& inquired_only) {
        Inquiries inquiries(inquired_only);
        inquiries.visit_Function(*copy);
        replaced = with_stand_ins(inquiries.found, inquired_only);
        // Every replacement is made before any variable is retyped, from
        // the types that the inquiries were made with.
        std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> replacements;
        for (auto& [inquiry, declaration] : inquiries.found) {
            ASR::expr_t** object = inquiry_object(*inquiry);
            if (!replaced.count(designator_variable(*object))) continue;
            replacements.push_back({object, replacement(inquiry)});
            if (declaration) changed.insert(declaration);
        }
        for (auto& [object, stand_in] : replacements) *object = stand_in;
        // A saved or initialized variable becomes an unsaved local, which
        // the forwarder creates and frees on every call like the other
        // stand-ins: a saved stand-in for a character array would be
        // allocated on every call and never freed.
        for (ASR::symbol_t* sym : replaced) {
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
            var->m_type = shape_only_type(al, var->m_type);
            var->m_type_declaration = nullptr;
            var->m_symbolic_value = nullptr;
            var->m_value = nullptr;
            var->m_storage = ASR::storage_typeType::Default;
            changed.insert(sym);
        }
        for (ASR::symbol_t* sym : changed) {
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
            SetChar dependencies;
            dependencies.reserve(al, 1);
            ASRUtils::collect_variable_dependencies(al, dependencies, var->m_type,
                var->m_symbolic_value, var->m_value, var->m_name);
            var->m_dependencies = dependencies.p;
            var->n_dependencies = dependencies.size();
        }
    }
};

// Makes the internal procedure `x` an interface that keeps only the symbols
// of its signature, `signature`. Its name is local to its host, so it is an
// abstract interface, which names no procedure: one that names an existing
// procedure would declare a global procedure of that name.
static void make_interface(Allocator& al, ASR::Function_t* x,
        const std::set<ASR::symbol_t*>& signature) {
    ASRUtils::ExprStmtDuplicator duplicator(al);
    ASR::ttype_t* type = duplicator.duplicate_ttype(x->m_function_signature);
    ASR::down_cast<ASR::FunctionType_t>(type)->m_deftype = ASR::deftypeType::Interface;
    x->m_function_signature = type;
    ASR::interface_body_t* body = al.make_new<ASR::interface_body_t>();
    body->loc = x->base.base.loc;
    body->m_abstract = true;
    x->m_interface_body = body;
    x->m_body = nullptr;
    x->n_body = 0;
    x->m_dependencies = nullptr;
    x->n_dependencies = 0;
    std::vector<std::string> unused;
    for (auto& item : x->m_symtab->get_scope()) {
        if (!signature.count(item.second)) unused.push_back(item.first);
    }
    for (auto& name : unused) x->m_symtab->erase_symbol(name);
}

/*
A device procedure is transformed in place (see proc2inplace below), so a
dummy it takes by data is taken that way from every caller, and addressed
as contiguous. A caller that cannot redirect a call to a copy has no
other way to hand such a dummy an array whose elements need not be
adjacent: an array pointer, such as the name an earlier pass binds to a
section it hands to a routine (`matmul(w(1:n, 1:m, l), x)`), or a dummy
of a device procedure that keeps its descriptor. Such a dummy keeps its
descriptor instead, and the device code generators hand it the strides
of its actual argument. On the host a call with a pointer actual keeps
calling the procedure that takes descriptors, as can_edit_call decides.
*/
class DeviceDescriptorDummies : public ASR::BaseWalkVisitor<DeviceDescriptorDummies>
{
    public:

        // A device procedure and the index of an array dummy that keeps
        // its descriptor.
        std::set<std::pair<ASR::symbol_t*, size_t>> kept;

        void collect(ASR::TranslationUnit_t &unit) {
            do {
                changed = false;
                visit_TranslationUnit(unit);
            } while( changed );
        }

        void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
            visit_call(x.m_name, x.m_args, x.n_args);
            ASR::BaseWalkVisitor<DeviceDescriptorDummies>::visit_SubroutineCall(x);
        }

        void visit_FunctionCall(const ASR::FunctionCall_t &x) {
            visit_call(x.m_name, x.m_args, x.n_args);
            ASR::BaseWalkVisitor<DeviceDescriptorDummies>::visit_FunctionCall(x);
        }

    private:

        bool changed = false;

        void visit_call(ASR::symbol_t *name, ASR::call_arg_t *args,
                size_t n_args) {
            ASR::symbol_t *fn = ASRUtils::symbol_get_past_external(name);
            if( !ASRUtils::runs_on_device(fn) ) {
                return;
            }
            for( size_t i = 0; i < n_args; i++ ) {
                if( args[i].m_value && may_be_strided(args[i].m_value) &&
                    kept.insert({fn, i}).second ) {
                    changed = true;
                }
            }
        }

        bool may_be_strided(ASR::expr_t *e) {
            e = ASRUtils::get_past_array_physical_cast(e);
            if( !ASR::is_a<ASR::Var_t>(*e) ||
                !ASRUtils::is_array(ASRUtils::expr_type(e)) ) {
                return false;
            }
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v);
            if( !ASR::is_a<ASR::Variable_t>(*sym) ) {
                return false;
            }
            if( ASRUtils::is_pointer(ASR::down_cast<ASR::Variable_t>(sym)->m_type) ) {
                return true;
            }
            ASR::asr_t *owner = ASRUtils::symbol_parent_symtab(sym)->asr_owner;
            if( !owner || !ASR::is_a<ASR::symbol_t>(*owner) ) {
                return false;
            }
            ASR::symbol_t *owner_sym = ASR::down_cast<ASR::symbol_t>(owner);
            if( !ASRUtils::runs_on_device(owner_sym) ) {
                return false;
            }
            ASR::Function_t *owner_fn = ASR::down_cast<ASR::Function_t>(owner_sym);
            for( size_t i = 0; i < owner_fn->n_args; i++ ) {
                if( ASR::is_a<ASR::Var_t>(*owner_fn->m_args[i]) &&
                    ASR::down_cast<ASR::Var_t>(owner_fn->m_args[i])->m_v == sym ) {
                    return kept.count({owner_sym, i}) > 0;
                }
            }
            return false;
        }
};

/*
The following visitor converts function/subroutines (a.k.a procedures)
with array arguments having empty dimensions to arrays having dimensional
information available from function arguments. See example below,

    subroutine f(array1, array2)
        integer, intent(in) :: array1(:)
        integer, intent(out) :: array2(:)
    end subroutine

gets converted to,

    subroutine f_array1_array2(array1, m1, n1, array2, m2, n2)
        integer, intent(in) :: m1, m2, n1, n2
        integer, intent(in) :: array1(m1:n1)
        integer, intent(out) :: array2(m2:n2)
    end subroutine
 */
class PassArrayByDataProcedureVisitor : public PassUtils::PassVisitor<PassArrayByDataProcedureVisitor>
{
    private:

        ASRUtils::ExprStmtDuplicator node_duplicator;
        const LCompilers::PassOptions& pass_options;

    public:

        std::map< ASR::symbol_t*, std::pair<ASR::symbol_t*, std::vector<size_t>> > proc2newproc;
        std::set<ASR::symbol_t*> newprocs;
        // A procedure transformed in place rather than specialised into a
        // copy, and the argument list it had before the extents were added.
        // A call site is still written against that list, so it is what the
        // call site has to be matched against.
        struct InPlaceProcedure {
            std::vector<size_t> indices;
            ASR::expr_t** args;
            size_t n_args;
        };
        // A device procedure is transformed in place because there is no
        // caller to redirect to a copy: a kernel is only ever named by a
        // launch statement, and a routine it calls is reachable from nowhere
        // but the device code.
        std::map< ASR::symbol_t*, InPlaceProcedure > proc2inplace;
        // The array dummies of device procedures that keep their
        // descriptor (see DeviceDescriptorDummies).
        std::set<std::pair<ASR::symbol_t*, size_t>> device_descriptor_dummies;

        PassArrayByDataProcedureVisitor(Allocator& al_, const LCompilers::PassOptions& pass_options_) :
        PassVisitor(al_, nullptr), node_duplicator(al_), pass_options(pass_options_)
        {}

        int get_index_kind() const {
            return pass_options.descriptor_index_64 ? 8 : 4;
        }

        ASR::symbol_t* insert_new_procedure(ASR::Function_t* x, std::vector<size_t>& indices) {
            Vec<ASR::stmt_t*> new_body;
            new_body.reserve(al, x->n_body);
            node_duplicator.allow_procedure_calls = true;
            node_duplicator.allow_reshape = false;
            for( size_t i = 0; i < x->n_body; i++ ) {
                node_duplicator.success = true;
                ASR::stmt_t* new_stmt = node_duplicator.duplicate_stmt(x->m_body[i]);
                if( !node_duplicator.success ) {
                    return nullptr;
                }
                new_body.push_back(al, new_stmt);
            }

            node_duplicator.allow_procedure_calls = true;
            // The copy is declared beside the original, which is not in the
            // scope being visited if it is declared in a BLOCK construct.
            SymbolTable* parent_scope = x->m_symtab->parent;
            SymbolTable* new_symtab = al.make_new<SymbolTable>(parent_scope);
            ASRUtils::SymbolDuplicator symbol_duplicator(al);
            // first duplicate the external symbols
            // so they can be referenced by derived_type
            for( auto& item: x->m_symtab->get_scope() ) {
                if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) {
                    symbol_duplicator.duplicate_symbol(item.second, new_symtab);
                }
            }
            for( auto& item: x->m_symtab->get_scope() ) {
                if (!ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) {
                    symbol_duplicator.duplicate_symbol(item.second, new_symtab);
                }
            }
            symbol_duplicator.fixup_local_type_declarations(new_symtab, x->m_symtab);
            // The duplicated body's BlockCall statements still reference
            // Block symbols in the original symtab. Remap them to the
            // corresponding duplicated Blocks in new_symtab.
            remap_block_calls(new_body, x->m_symtab, new_symtab);

            Vec<ASR::expr_t*> new_args;
            std::string suffix = "";
            new_args.reserve(al, x->n_args);
            ASR::expr_t* return_var = nullptr;
            for( size_t i = 0; i < x->n_args + 1; i++ ) {
                ASR::Variable_t* arg = nullptr;
                ASR::Function_t* arg_func = nullptr;
                if( i < x->n_args ) {
                    if (ASR::is_a<ASR::Var_t>(*(x->m_args[i]))) {
                        ASR::Var_t* x_arg = ASR::down_cast<ASR::Var_t>(x->m_args[i]);
                        if (ASR::is_a<ASR::Function_t>(*(x_arg->m_v))) {
                            arg_func = ASR::down_cast<ASR::Function_t>(x_arg->m_v);
                        } else {
                            arg = ASRUtils::EXPR2VAR(x->m_args[i]);
                        }
                    }
                } else if( x->m_return_var ) {
                    arg = ASRUtils::EXPR2VAR(x->m_return_var);
                } else {
                    break ;
                }
                if( std::find(indices.begin(), indices.end(), i) != indices.end() ) {
                    if( arg_func ) {
                        suffix += "_" + ASRUtils::type_to_str_fortran_expr(arg_func->m_function_signature, 
                            ASRUtils::get_expr_from_sym(al, &arg_func->base));
                    } else {
                        suffix += "_" + ASRUtils::type_to_str_fortran_symbol(arg->m_type, 
                            arg->m_type_declaration);
                    }
                    suffix += "_" + std::to_string(i);
                }
                ASR::expr_t* new_arg;
                if (arg_func) {
                    new_arg = ASRUtils::EXPR(ASR::make_Var_t(al,
                                        arg_func->base.base.loc, new_symtab->get_symbol(
                                                        std::string(arg_func->m_name))));
                } else {
                    new_arg = ASRUtils::EXPR(ASR::make_Var_t(al,
                                        arg->base.base.loc, new_symtab->get_symbol(
                                                        std::string(arg->m_name))));
                }
                if( i < x->n_args ) {
                    new_args.push_back(al, new_arg);
                } else {
                    return_var = new_arg;
                }
            }
            ASR::symbol_t* new_symbol = nullptr;
            suffix = to_lower(suffix);
            for( size_t suffixi = 0; suffixi < suffix.size(); suffixi++ ) {
                if( !((suffix[suffixi] >= 'a' && suffix[suffixi] <= 'z') ||
                    (suffix[suffixi] >= '0' && suffix[suffixi] <= '9')) ) {
                    suffix[suffixi] = '_';
                }
            }
            std::string new_name = std::string(x->m_name) + suffix;
            // In interactive mode the global symbol table persists across
            // evaluations and the pass may run multiple times. If the
            // generated procedure already exists in the current scope from
            // a previous run, skip re-processing this function (which would
            // trip add_symbol's uniqueness assertion). Call sites in the
            // freshly translated AST will be rewritten by the call-site
            // replacer using existing proc2newproc state populated below
            // for newly inserted procedures only.
            if (ASR::symbol_t* existing = parent_scope->get_symbol(new_name)) {
                // The specialisation is already there: either an earlier run
                // of this pass in the same interactive session made it, or an
                // earlier cell did (and the JIT already holds its definition).
                // Map to it, so that this cell's call sites are rewritten to
                // the procedure that actually exists.
                proc2newproc[(ASR::symbol_t*) x] = std::make_pair(existing, indices);
                return nullptr;
            }
            if( ASR::is_a<ASR::Function_t>( *((ASR::symbol_t*) x) ) ) {
                ASR::FunctionType_t* x_func_type = ASRUtils::get_FunctionType(x);
                std::string new_bindc_name = "";
                if( x_func_type->m_bindc_name ) {
                    new_bindc_name = std::string(x_func_type->m_bindc_name) + suffix;
                }
                ASR::asr_t* new_subrout = ASRUtils::make_Function_t_util(al, x->base.base.loc,
                    new_symtab, s2c(al, new_name), x->m_dependencies, x->n_dependencies,
                    new_args.p, new_args.size(),  new_body.p, new_body.size(),
                    return_var, x_func_type->m_abi, x->m_access, x_func_type->m_deftype,
                    s2c(al, new_bindc_name), x_func_type->m_elemental,
                    x_func_type->m_pure, x_func_type->m_module, x_func_type->m_inline,
                    x_func_type->m_static,
                    x_func_type->m_restrictions, x_func_type->n_restrictions, false, false, false);
                new_symbol = ASR::down_cast<ASR::symbol_t>(new_subrout);
            }
            parent_scope->add_symbol(new_name, new_symbol);
            proc2newproc[(ASR::symbol_t*) x] = std::make_pair(new_symbol, indices);
            newprocs.insert(new_symbol);
            return new_symbol;
        }

        void edit_procedure_args(ASR::Function_t* x, std::vector<size_t>& indices) {
            SymbolTable* enclosing_scope = x->m_symtab->parent;
            Vec<ASR::expr_t*> new_args;
            new_args.reserve(al, x->n_args);
            for( size_t i = 0; i < x->n_args; i++ ) {
                new_args.push_back(al, x->m_args[i]);
                if( std::find(indices.begin(), indices.end(), i) !=
                    indices.end() ) {
                    ASR::Variable_t* arg = ASRUtils::EXPR2VAR(x->m_args[i]);
                    ASR::dimension_t* dims = nullptr;
                    int n_dims = checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(arg->m_type, dims));

                    // Check if this is an assumed-size array (UnboundedPointerArray)
                    // If so, don't add dimension variables - keep dimensions as-is
                    ASR::array_physical_typeType phys_type = ASRUtils::extract_physical_type(arg->m_type);
                    if (phys_type == ASR::array_physical_typeType::UnboundedPointerArray) {
                        // Don't modify assumed-size arrays - they don't pass dimension info
                        continue;
                    }

                    Vec<ASR::expr_t*> dim_variables;
                    std::string arg_name = std::string(arg->m_name);
                    int index_kind = get_index_kind();
                    PassUtils::create_vars(dim_variables, n_dims, arg->base.base.loc, al,
                                            x->m_symtab, arg_name, ASR::intentType::In, arg->m_presence, index_kind);
                    Vec<ASR::dimension_t> new_dims;
                    new_dims.reserve(al, n_dims);
                    ASRUtils::ASRBuilder builder(al, arg->base.base.loc);
                    for( int j = 0, k = 0; j < n_dims; j++ ) {
                        ASR::dimension_t new_dim;
                        new_dim.loc = dims[j].loc;
                        if (dims[j].m_start) {
                            new_dim.m_start = dims[j].m_start;
                        } else {
                            new_dim.m_start = builder.i_idx(1, index_kind);
                        }
                        new_dim.m_length = dim_variables[k];
                        new_dims.push_back(al, new_dim);
                        k += 1;
                    }
                    ASR::ttype_t* new_type = ASRUtils::duplicate_type(al, arg->m_type, &new_dims);
                    arg->m_type = new_type;
                    for( int k = 0; k < n_dims; k++ ) {
                        new_args.push_back(al, dim_variables[k]);
                    }
                }
            }

            ASR::FunctionType_t* func_type = ASRUtils::get_FunctionType(*x);
            x->m_function_signature = ASRUtils::TYPE(ASRUtils::make_FunctionType_t_util(
                al, func_type->base.base.loc, new_args.p, new_args.size(), x->m_return_var, func_type, enclosing_scope));
            x->m_args = new_args.p;
            x->n_args = new_args.size();
        }


        template <typename T>
        bool visit_SymbolContainingFunctions(const T& x,
            std::deque<ASR::Function_t*>& pass_array_by_data_functions) {
            T& xx = const_cast<T&>(x);
            current_scope = xx.m_symtab;
            // Including the procedures declared in the BLOCK constructs
            ASRUtils::for_each_symbol_in_scope_and_constructs(xx.m_symtab, [&](ASR::symbol_t* sym) {
                if( ASR::is_a<ASR::Function_t>(*sym) ) {
                    ASR::Function_t* subrout = ASR::down_cast<ASR::Function_t>(sym);
                    std::vector<size_t> arg_indices;
                    if( ASRUtils::runs_on_device(*subrout) ) {
                        // Device code is emitted as Metal/CUDA source, where
                        // an array argument is a bare pointer that carries no
                        // extents, so it needs them as explicit arguments.
                        // Transform it in place; a specialised copy would be
                        // pointless because the only reference to it is
                        // inside the device code.
                        pass_array_by_data_functions.push_back(subrout);
                        if( ASRUtils::is_pass_array_by_data_possible(subrout, arg_indices) ) {
                            arg_indices.erase(std::remove_if(arg_indices.begin(),
                                arg_indices.end(), [&](size_t i) {
                                    return device_descriptor_dummies.count(
                                        {sym, i}) > 0;
                                }), arg_indices.end());
                        }
                        if( !arg_indices.empty() ) {
                            proc2inplace[sym] = {arg_indices,
                                subrout->m_args, subrout->n_args};
                            edit_procedure_args(subrout, arg_indices);
                        }
                        return;
                    }
                    pass_array_by_data_functions.push_back(subrout);
                    if( ASRUtils::is_pass_array_by_data_possible(subrout, arg_indices) ) {
                        ASR::symbol_t* new_sym = insert_new_procedure(subrout, arg_indices);
                        if( new_sym != nullptr ) {
                            ASR::Function_t* new_subrout = ASR::down_cast<ASR::Function_t>(new_sym);
                            edit_procedure_args(new_subrout, arg_indices);
                        }
                    }
                }
            });
            return pass_array_by_data_functions.size() > 0;
        }

        #define bfs_visit_SymbolContainingFunctions() std::deque<ASR::Function_t*> pass_array_by_data_functions;    \
            visit_SymbolContainingFunctions(x, pass_array_by_data_functions);    \
            while( pass_array_by_data_functions.size() > 0 ) {    \
                ASR::Function_t* function = pass_array_by_data_functions.front();    \
                pass_array_by_data_functions.pop_front();    \
                visit_SymbolContainingFunctions(*function, pass_array_by_data_functions);    \
            }    \

        void visit_TranslationUnit(const ASR::TranslationUnit_t& x) {
            // In interactive mode each cell is a TranslationUnit chained to
            // the previous one, so earlier cells' symbols live in ancestor
            // scopes. Their procedures were specialised when their own cell
            // was compiled; specialise them here too so that calls from this
            // cell resolve to the procedures the JIT already holds. The
            // generated names are derived from the signature, so they come
            // out the same as they did then.
            for (SymbolTable* s = x.m_symtab->parent; s != nullptr; s = s->parent) {
                if( !ASRUtils::is_tu_scope(s) ) continue;
                // The earlier cell's own procedures and the ones nested
                // in them, as that cell specialised them. A dummy
                // procedure whose interface was specialised takes its
                // actual argument by data, so a procedure this cell
                // passes to it has to be the specialisation as well.
                const ASR::TranslationUnit_t& tu =
                    *ASR::down_cast2<ASR::TranslationUnit_t>(s->asr_owner);
                std::deque<ASR::Function_t*> functions;
                visit_SymbolContainingFunctions(tu, functions);
                while( functions.size() > 0 ) {
                    ASR::Function_t* function = functions.front();
                    functions.pop_front();
                    visit_SymbolContainingFunctions(*function, functions);
                }
                for (auto &a : s->get_scope()) {
                    if( ASR::is_a<ASR::Module_t>(*a.second) ) {
                        this->visit_symbol(*a.second);
                    }
                }
            }

            // Visit functions in global scope first
            bfs_visit_SymbolContainingFunctions();

            // Visit Module so that all functions in it are updated
            for (auto &a : x.m_symtab->get_scope()) {
                if( ASR::is_a<ASR::Module_t>(*a.second) ) {
                    this->visit_symbol(*a.second);
                }
            }

            // When a module has submodules, the Interface declaration
            // (empty body) can be transformed but the Implementation
            // (in the submodule) may fail (e.g. body contains reshape).
            // Remove such Interface entries so callers are not redirected
            // to a function that has no definition.
            std::vector<ASR::symbol_t*> iface_to_remove;
            for (auto& kv : proc2newproc) {
                ASR::Function_t* func = ASR::down_cast<ASR::Function_t>(kv.first);
                ASR::FunctionType_t* ftype = ASRUtils::get_FunctionType(func);
                if (ftype->m_deftype != ASR::deftypeType::Interface) continue;
                // Only consider module procedure interfaces (m_module=true).
                // Abstract interfaces should not be removed.
                if (!ftype->m_module) continue;
                SymbolTable* parent_symtab = func->m_symtab->parent;
                if (!parent_symtab || !parent_symtab->asr_owner) continue;
                ASR::symbol_t* parent_sym = ASR::down_cast<ASR::symbol_t>(
                    parent_symtab->asr_owner);
                if (!ASR::is_a<ASR::Module_t>(*parent_sym)) continue;
                ASR::Module_t* mod = ASR::down_cast<ASR::Module_t>(parent_sym);
                if (!mod->m_has_submodules) continue;
                std::string func_name = std::string(func->m_name);
                bool impl_processed = false;
                bool impl_declared = false;
                for (auto& top_item : x.m_symtab->get_scope()) {
                    if (!ASR::is_a<ASR::Module_t>(*top_item.second)) continue;
                    ASR::Module_t* sub_mod = ASR::down_cast<ASR::Module_t>(
                        top_item.second);
                    if (!sub_mod->m_parent_module) continue;
                    if (std::string(sub_mod->m_parent_module) !=
                            std::string(mod->m_name)) continue;
                    ASR::symbol_t* impl_sym = sub_mod->m_symtab->resolve_symbol(
                        func_name);
                    if (impl_sym && ASR::is_a<ASR::Function_t>(*impl_sym)) {
                        impl_declared = true;
                        if (proc2newproc.find(impl_sym) != proc2newproc.end()) {
                            impl_processed = true;
                            break;
                        }
                    }
                }
                if (impl_declared && !impl_processed) {
                    iface_to_remove.push_back(kv.first);
                }
            }
            for (ASR::symbol_t* sym : iface_to_remove) {
                ASR::Function_t* new_func = ASR::down_cast<ASR::Function_t>(
                    proc2newproc[sym].first);
                SymbolTable* scope = new_func->m_symtab->parent;
                scope->erase_symbol(std::string(new_func->m_name));
                newprocs.erase(proc2newproc[sym].first);
                proc2newproc.erase(sym);
            }

            // Visit the program
            for (auto &a : x.m_symtab->get_scope()) {
                if( ASR::is_a<ASR::Program_t>(*a.second) ) {
                    this->visit_symbol(*a.second);
                }
            }
        }

        void visit_Program(const ASR::Program_t& x) {
            bfs_visit_SymbolContainingFunctions()
        }

        void visit_Module(const ASR::Module_t& x) {
            // Do not visit intrinsic modules
            if( x.m_intrinsic ) {
                return ;
            }

            bfs_visit_SymbolContainingFunctions()
        }
};

#define edit_symbol_reference(attr) ASR::symbol_t* x_sym = xx.m_##attr;    \
    SymbolTable* x_sym_symtab = ASRUtils::symbol_parent_symtab(x_sym);    \
    if( x_sym_symtab->get_counter() != current_scope->get_counter() &&    \
        !ASRUtils::is_parent(x_sym_symtab, current_scope) ) {    \
        std::string x_sym_name = std::string(ASRUtils::symbol_name(x_sym));    \
        xx.m_##attr = current_scope->resolve_symbol(x_sym_name);    \
        LCOMPILERS_ASSERT(xx.m_##attr != nullptr);    \
    }    \

// Whether `symtab` belongs to a translation unit compiled in the scope of
// the one `scope` belongs to. In interactive mode the procedures of earlier
// cells are edited too (see EditProcedureVisitor::visit_TranslationUnit),
// and their types can call a helper function that the cell being compiled
// created for them (see function_call_in_declaration). That reference is
// already right, and no name resolved from the earlier cell reaches it.
static inline bool is_in_enclosed_unit(SymbolTable* symtab, SymbolTable* scope) {
    SymbolTable* unit = symtab->get_tu_scope();
    SymbolTable* scope_unit = scope->get_tu_scope();
    return unit != scope_unit && ASRUtils::is_parent(scope_unit, unit);
}

#define edit_symbol_pointer(attr) ASR::symbol_t* x_sym = x->m_##attr;    \
    SymbolTable* x_sym_symtab = ASRUtils::symbol_parent_symtab(x_sym);    \
    if( x_sym_symtab->get_counter() != current_scope->get_counter() &&    \
        !ASRUtils::is_parent(x_sym_symtab, current_scope) &&    \
        !is_in_enclosed_unit(x_sym_symtab, current_scope) ) {    \
        std::string x_sym_name = std::string(ASRUtils::symbol_name(x_sym));    \
        x->m_##attr = current_scope->resolve_symbol(x_sym_name);    \
        LCOMPILERS_ASSERT(x->m_##attr != nullptr);    \
    }    \

class EditProcedureReplacer: public ASR::BaseExprReplacer<EditProcedureReplacer> {

    public:

    PassArrayByDataProcedureVisitor& v;
    SymbolTable* current_scope;

    EditProcedureReplacer(PassArrayByDataProcedureVisitor& v_):
     v(v_), current_scope(nullptr) {}

    /*
        We need this for the cases where pass array by data is possible for parent function and also
        for the child function. Let's take the following example:
        ```fortran
        subroutine cobyla(x)
            implicit none

            real, intent(inout) :: x(:)
            call evaluate(calcfc_internal)
            contains

            subroutine calcfc_internal(x_internal)
                implicit none
                real, intent(in) :: x_internal(:)
            end subroutine calcfc_internal

            subroutine evaluate(calcfc)
                use, non_intrinsic :: pintrf_mod, only : OBJCON
                implicit none
                procedure(OBJCON) :: calcfc
            end subroutine evaluate

        end subroutine
        ```

        Here, cobyla is parent and calcfc_internal is child. What happens is by `bfs` we first
        create `cobyla_real____0` and add it as a symbol in global scope. Then we visit `cobyla`,
        create `calcfc_internal_real____0` as replacement of `calcfc_internal` and add it as a symbol
        in scope of `cobyla`. Now, we visit `cobyla_real____0` and replace `calcfc_internal` with
        `calcfc_internal_real____0`

        This means both symbols `cobyla` and `cobyla_real____0` have `calcfc_internal_real____0` as a symbol
        but eventually `cobyla` is removed from scope.

        In `proc2newproc` we map `cobyla(1) -> cobyla_real____0(2)` and `calcfc_internal(1.1) -> calcfc_internal_real____0(1.2)`
        and also have `calcfc_internal(2.1) -> calcfc_internal_real____0(2.2)`. Where `(x)` represents symtab counter.

        When it comes to replace `call evaluate(calcfc_internal(1.1))`, it gets replaced with `call evaluate(calcfc_internal_real____0(1.2))`
        which eventually gets deleted. We need to actually replace it with `call evaluate(calcfc_internal_real____0(2.2))`, so what we do is
        we resolve the symbol to the latest symbol in the scope.

        From `calcfc_internal(1.1)`, we get `calcfc_internal_real____0(1.2)` and then we get the parent of `calcfc_internal_real____0(1.2)`,
        i.e. `cobyla(1)`, resolve it to `cobyla_real____0(2)` and then get the symbol `calcfc_internal_real____0(2.2)` from it.
    */
    ASR::symbol_t* resolve_new_proc(ASR::symbol_t* old_sym) {
        ASR::symbol_t* ext_sym = ASRUtils::symbol_get_past_external(old_sym);
        if( v.proc2newproc.find(ext_sym) != v.proc2newproc.end() ) {
            ASR::symbol_t* new_sym = v.proc2newproc[ext_sym].first;
            SymbolTable* new_sym_scope = ASRUtils::symbol_parent_symtab(new_sym);
            ASR::asr_t* new_sym_parent = new_sym_scope->asr_owner;
            if ( ASR::is_a<ASR::symbol_t>(*new_sym_parent) &&
                 current_scope->get_counter() != new_sym_scope->get_counter() &&
                 !ASRUtils::is_parent(new_sym_scope, current_scope) ) {
                ASR::symbol_t* resolved_parent_sym = resolve_new_proc(ASR::down_cast<ASR::symbol_t>(new_sym_parent));
                if ( resolved_parent_sym != nullptr ) {
                    ASR::symbol_t* sym_to_return = ASRUtils::symbol_symtab(resolved_parent_sym)->get_symbol(ASRUtils::symbol_name(new_sym));
                    return sym_to_return ? sym_to_return : new_sym;
                }
            }
            return new_sym;
        }
        return nullptr;
    }

    void replace_Var(ASR::Var_t* x) {
        ASR::symbol_t* x_sym_ = x->m_v;
        bool is_external = ASR::is_a<ASR::ExternalSymbol_t>(*x_sym_);
        if ( v.proc2newproc.find(ASRUtils::symbol_get_past_external(x_sym_)) != v.proc2newproc.end() ) {
            x->m_v = resolve_new_proc(x_sym_);
            if( is_external ) {
                ASR::ExternalSymbol_t* x_sym_ext = ASR::down_cast<ASR::ExternalSymbol_t>(x_sym_);
                std::string new_func_sym_name = current_scope->get_unique_name(ASRUtils::symbol_name(
                    ASRUtils::symbol_get_past_external(x_sym_)));
                 ASR::symbol_t* new_func_sym_ = ASR::down_cast<ASR::symbol_t>(
                    ASR::make_ExternalSymbol_t(v.al, x_sym_->base.loc, current_scope,
                        s2c(v.al, new_func_sym_name), x->m_v, x_sym_ext->m_module_name,
                        x_sym_ext->m_scope_names, x_sym_ext->n_scope_names, ASRUtils::symbol_name(x->m_v),
                        x_sym_ext->m_access));
                v.proc2newproc[x_sym_] = {new_func_sym_, {}};
                current_scope->add_symbol(new_func_sym_name, new_func_sym_);
                x->m_v = new_func_sym_;
            }
            return ;
        }

        edit_symbol_pointer(v)
    }

    void replace_ArrayPhysicalCast(ASR::ArrayPhysicalCast_t* x) {
        ASR::BaseExprReplacer<EditProcedureReplacer>::replace_ArrayPhysicalCast(x);
        // TODO: Allow for DescriptorArray to DescriptorArray physical cast for allocatables
        // later on
        x->m_old = ASRUtils::extract_physical_type(ASRUtils::expr_type(x->m_arg));
        if( (x->m_old == x->m_new &&
             x->m_old != ASR::array_physical_typeType::DescriptorArray) ||
            (x->m_old == x->m_new && x->m_old == ASR::array_physical_typeType::DescriptorArray &&
            (ASR::is_a<ASR::Allocatable_t>(*ASRUtils::expr_type(x->m_arg)) ||
             ASR::is_a<ASR::Pointer_t>(*ASRUtils::expr_type(x->m_arg))) &&
            ASRUtils::extract_n_dims_from_ttype(ASRUtils::expr_type(x->m_arg)) ==
            ASRUtils::extract_n_dims_from_ttype(x->m_type))) {
            *current_expr = x->m_arg;
        } else if (x->m_old == ASR::array_physical_typeType::AssumedRankArray &&
                   !ASRUtils::is_array(x->m_type)) {
            // rank(0): AssumedRankArray → scalar, nothing to update
        } else {
            ASR::Array_t* arr = ASR::down_cast<ASR::Array_t>(ASRUtils::type_get_past_pointer(
                  ASRUtils::type_get_past_allocatable(x->m_type)));
            // A cast to a data pointer carries no descriptor, so empty target
            // extents stay empty: they describe a dummy that declares no
            // extent (`a(*)`, or an assumed-shape dummy that this pass replaces
            // by a data pointer) and must not become the actual argument's
            // extents, which the call-site size check would then mistake for
            // the dummy's shape. A descriptor or fixed-size target does need
            // them to be built.
            bool target_needs_extents =
                x->m_new == ASR::array_physical_typeType::DescriptorArray ||
                x->m_new == ASR::array_physical_typeType::FixedSizeArray;
            if (target_needs_extents &&
                    ASRUtils::is_dimension_empty(arr->m_dims, arr->n_dims) && !(x->m_old == ASR::array_physical_typeType::AssumedRankArray)) {
                ASR::Array_t* src_arr = ASR::down_cast<ASR::Array_t>(
                    ASRUtils::type_get_past_allocatable(ASRUtils::type_get_past_pointer(
                        ASRUtils::expr_type(x->m_arg))));
                // Don't replace dims when the target has more dimensions than
                // the source (e.g., assumed-rank target with 15 empty dims).
                if (arr->n_dims <= src_arr->n_dims) {
                    arr->m_dims = ASRUtils::duplicate_dimensions(v.al, src_arr->m_dims, src_arr->n_dims);
                    arr->n_dims = src_arr->n_dims;
                }
            }
        }
    }

    void replace_FunctionCall(ASR::FunctionCall_t* x) {
        edit_symbol_pointer(name)
        edit_original_name(x->m_original_name);
        ASR::BaseExprReplacer<EditProcedureReplacer>::replace_FunctionCall(x);
    }

    // A call through a call-site interface names the procedure it calls
    // as its original name; in the copy that is the copied procedure.
    void edit_original_name(ASR::symbol_t*& original_name) {
        if (original_name == nullptr) {
            return;
        }
        SymbolTable* symtab = ASRUtils::symbol_parent_symtab(original_name);
        if (symtab->get_counter() != current_scope->get_counter() &&
                !ASRUtils::is_parent(symtab, current_scope)) {
            ASR::symbol_t* resolved = current_scope->resolve_symbol(
                ASRUtils::symbol_name(original_name));
            if (resolved != nullptr) {
                original_name = resolved;
            }
        }
    }

    // The duplicated body of a copied procedure still names the symbols
    // declared in the original: the ExternalSymbols of its scope and the
    // components and types declared in it. Return the symbol of the copy
    // that has the same path of names from the procedure down to `sym`. A
    // symbol not declared in a copied procedure is returned unchanged.
    ASR::symbol_t* symbol_in_copy(ASR::symbol_t* sym) {
        std::vector<std::string> path;
        SymbolTable* scope = nullptr;
        for (ASR::symbol_t* s = sym; scope == nullptr; ) {
            path.push_back(ASRUtils::symbol_name(s));
            SymbolTable* parent = ASRUtils::symbol_parent_symtab(s);
            if (parent == nullptr || parent->asr_owner == nullptr ||
                    !ASR::is_a<ASR::symbol_t>(*parent->asr_owner)) {
                return sym;
            }
            s = ASR::down_cast<ASR::symbol_t>(parent->asr_owner);
            auto it = v.proc2newproc.find(s);
            if (it != v.proc2newproc.end()) {
                scope = ASRUtils::symbol_symtab(it->second.first);
                if (scope == nullptr || (scope != current_scope &&
                        !ASRUtils::is_parent(scope, current_scope))) {
                    return sym;
                }
            }
        }
        for (size_t i = path.size(); i > 0; i--) {
            ASR::symbol_t* resolved = scope->get_symbol(path[i - 1]);
            if (resolved == nullptr) {
                return sym;
            }
            if (i == 1) {
                return resolved;
            }
            scope = ASRUtils::symbol_symtab(resolved);
            if (scope == nullptr) {
                return sym;
            }
        }
        return sym;
    }

    void replace_StructInstanceMember(ASR::StructInstanceMember_t* x) {
        x->m_m = symbol_in_copy(x->m_m);
        ASR::BaseExprReplacer<EditProcedureReplacer>::replace_StructInstanceMember(x);
    }

    void replace_StructConstructor(ASR::StructConstructor_t* x) {
        x->m_dt_sym = symbol_in_copy(x->m_dt_sym);
        ASR::BaseExprReplacer<EditProcedureReplacer>::replace_StructConstructor(x);
    }

    // The interface of a cast made at a call site of the copied procedure
    // is a symbol of that procedure, so the copy casts to its own copy.
    void replace_FunctionPointerCast(ASR::FunctionPointerCast_t* x) {
        if (x->m_to) {
            edit_symbol_pointer(to)
        }
        ASR::BaseExprReplacer<EditProcedureReplacer>::replace_FunctionPointerCast(x);
    }

};

class EditProcedureVisitor: public ASR::CallReplacerOnExpressionsVisitor<EditProcedureVisitor> {

    public:

    PassArrayByDataProcedureVisitor& v;
    EditProcedureReplacer replacer;

    EditProcedureVisitor(PassArrayByDataProcedureVisitor& v_):
    v(v_), replacer(v) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.current_scope = current_scope;
        replacer.replace_expr(*current_expr);
    }

    void visit_BlockCall(const ASR::BlockCall_t& x) {
        ASR::BlockCall_t& xx = const_cast<ASR::BlockCall_t&>(x);
        edit_symbol_reference(m)
        ASR::CallReplacerOnExpressionsVisitor<EditProcedureVisitor>::visit_BlockCall(x);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t& x) {
        ASR::AssociateBlockCall_t& xx = const_cast<ASR::AssociateBlockCall_t&>(x);
        edit_symbol_reference(m)
        ASR::CallReplacerOnExpressionsVisitor<EditProcedureVisitor>::visit_AssociateBlockCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
        ASR::SubroutineCall_t& xx = const_cast<ASR::SubroutineCall_t&>(x);
        edit_symbol_reference(name)
        replacer.current_scope = current_scope;
        replacer.edit_original_name(xx.m_original_name);
        ASR::CallReplacerOnExpressionsVisitor<EditProcedureVisitor>::visit_SubroutineCall(x);
    }

    // The copy of a procedure shares the defined output bindings of its
    // StringFormats with the original, so a binding that names a symbol of
    // the original's scope is copied and made to name the symbol of the
    // same name in the copy's scope.
    void visit_StringFormat(const ASR::StringFormat_t& x) {
        ASR::StringFormat_t& xx = const_cast<ASR::StringFormat_t&>(x);
        auto outside = [&](ASR::symbol_t* sym) {
            if (sym == nullptr) {
                return false;
            }
            SymbolTable* sym_symtab = ASRUtils::symbol_parent_symtab(sym);
            return sym_symtab->get_counter() != current_scope->get_counter() &&
                !ASRUtils::is_parent(sym_symtab, current_scope);
        };
        auto in_scope = [&](ASR::symbol_t* sym) {
            if (!outside(sym)) {
                return sym;
            }
            ASR::symbol_t* resolved = current_scope->resolve_symbol(
                ASRUtils::symbol_name(sym));
            LCOMPILERS_ASSERT(resolved != nullptr);
            return resolved;
        };
        bool needs_copy = false;
        for (size_t i = 0; i < xx.n_dtio_bindings; i++) {
            ASR::dtio_binding_t &b = xx.m_dtio_bindings[i];
            needs_copy = needs_copy || outside(b.m_type_declaration) ||
                outside(b.m_procedure) || outside(b.m_callback);
        }
        if (needs_copy) {
            ASR::dtio_binding_t* bindings = v.al.allocate<ASR::dtio_binding_t>(
                xx.n_dtio_bindings);
            for (size_t i = 0; i < xx.n_dtio_bindings; i++) {
                bindings[i] = xx.m_dtio_bindings[i];
                bindings[i].m_type_declaration = in_scope(bindings[i].m_type_declaration);
                bindings[i].m_procedure = in_scope(bindings[i].m_procedure);
                bindings[i].m_callback = in_scope(bindings[i].m_callback);
            }
            xx.m_dtio_bindings = bindings;
        }
        ASR::CallReplacerOnExpressionsVisitor<EditProcedureVisitor>::visit_StringFormat(x);
    }

    void update_procedure_variable_type_declarations(SymbolTable* symtab) {
        for (auto it: symtab->get_scope()) {
            if ( ASR::is_a<ASR::Variable_t>(*it.second) &&
                ASR::down_cast<ASR::Variable_t>(it.second)->m_type_declaration ) {
                ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(it.second);
                ASR::symbol_t* resolved_type_dec = nullptr;
                if ( v.proc2newproc.find(var->m_type_declaration) != v.proc2newproc.end() ) {
                    resolved_type_dec = v.proc2newproc[var->m_type_declaration].first;
                } else if ( v.proc2newproc.find(ASRUtils::symbol_get_past_external(var->m_type_declaration)) != v.proc2newproc.end() ) {
                    resolved_type_dec = v.proc2newproc[ASRUtils::symbol_get_past_external(var->m_type_declaration)].first;
                }
                if ( resolved_type_dec ) {
                    ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(resolved_type_dec);
                    // The rewritten procedure lives in whichever scope this
                    // pass rebuilt it into, so import it the same way the
                    // shared Variable constructor would.
                    ASR::symbol_t* new_type_dec = ASRUtils::import_type_declaration(
                        v.al, resolved_type_dec, var->m_parent_symtab);
                    if ( !ASRUtils::is_visible_from(new_type_dec, var->m_parent_symtab) &&
                            ASR::is_a<ASR::ExternalSymbol_t>(*var->m_type_declaration) ) {
                        // The old declaration reached the procedure through an
                        // ExternalSymbol; the specialisation lives beside the
                        // original, so it is reachable the very same way.
                        ASR::ExternalSymbol_t* old_ext = ASR::down_cast<ASR::ExternalSymbol_t>(
                            var->m_type_declaration);
                        std::string ext_name = var->m_parent_symtab->get_unique_name(
                            ASRUtils::symbol_name(resolved_type_dec));
                        new_type_dec = ASR::down_cast<ASR::symbol_t>(
                            ASR::make_ExternalSymbol_t(v.al, old_ext->base.base.loc,
                                var->m_parent_symtab, s2c(v.al, ext_name), resolved_type_dec,
                                old_ext->m_module_name, old_ext->m_scope_names,
                                old_ext->n_scope_names,
                                ASRUtils::symbol_name(resolved_type_dec), old_ext->m_access));
                        var->m_parent_symtab->add_symbol(ext_name, new_type_dec);
                    }
                    var->m_type_declaration = new_type_dec;
                    ASR::ttype_t* new_type = fn->m_function_signature;
                    if (ASR::is_a<ASR::Pointer_t>(*var->m_type)) {
                        new_type = ASRUtils::TYPE(ASR::make_Pointer_t(v.al, var->base.base.loc, new_type));
                    }
                    var->m_type = new_type;
                }
            }
        }
    }

    void visit_TranslationUnit(const ASR::TranslationUnit_t& x) {
        // In interactive mode the earlier cells' procedures were specialised
        // again in this cell (see PassArrayByDataProcedureVisitor). Edit them
        // the same way their own cell did, so that this cell declares them
        // with the signature that cell compiled: a dummy procedure whose
        // interface was specialised takes its actual argument by data.
        for (SymbolTable* s = x.m_symtab->parent; s != nullptr; s = s->parent) {
            if( !ASRUtils::is_tu_scope(s) ) continue;
            SymbolTable* current_scope_copy = current_scope;
            current_scope = s;
            for (auto &a : s->get_scope()) {
                if( ASR::is_a<ASR::Module_t>(*a.second) ||
                    ASR::is_a<ASR::Function_t>(*a.second) ) {
                    this->visit_symbol(*a.second);
                }
            }
            current_scope = current_scope_copy;
        }
        ASR::CallReplacerOnExpressionsVisitor<EditProcedureVisitor>::visit_TranslationUnit(x);
    }

    void visit_Module(const ASR::Module_t& x) {
        update_procedure_variable_type_declarations(x.m_symtab);
        ASR::CallReplacerOnExpressionsVisitor<EditProcedureVisitor>::visit_Module(x);
    }

    void visit_Program(const ASR::Program_t& x) {
        update_procedure_variable_type_declarations(x.m_symtab);
        ASR::CallReplacerOnExpressionsVisitor<EditProcedureVisitor>::visit_Program(x);
    }

    void visit_Function(const ASR::Function_t& x) {
        update_procedure_variable_type_declarations(x.m_symtab);
        ASR::CallReplacerOnExpressionsVisitor<EditProcedureVisitor>::visit_Function(x);
    }

};

// Copying an input to contiguous storage must not make its descriptor or
// address observable. In particular, do not forward such an array to another
// procedure whose descriptor requirements are not known here.
class DescriptorWrapperSafety : public ASR::BaseWalkVisitor<DescriptorWrapperSafety> {
    std::set<ASR::symbol_t*> arrays;
public:
    bool safe = true;
    explicit DescriptorWrapperSafety(const std::set<ASR::symbol_t*>& arrays_)
        : arrays(arrays_) {}

    void visit_ArrayIsContiguous(const ASR::ArrayIsContiguous_t&) { safe = false; }
    void visit_CLoc(const ASR::CLoc_t&) { safe = false; }
    void visit_GetPointer(const ASR::GetPointer_t&) { safe = false; }
    void visit_PointerToCPtr(const ASR::PointerToCPtr_t&) { safe = false; }

    void visit_BlockCall(const ASR::BlockCall_t& x) {
        visit_Block(*ASR::down_cast<ASR::Block_t>(x.m_m));
    }

    bool uses_input(ASR::expr_t* expr) {
        struct ArrayUse : public ASR::BaseWalkVisitor<ArrayUse> {
            const std::set<ASR::symbol_t*>& arrays;
            bool found = false;
            explicit ArrayUse(const std::set<ASR::symbol_t*>& arrays_)
                : arrays(arrays_) {}
            void visit_Var(const ASR::Var_t& x) {
                found |= arrays.count(x.m_v) != 0;
            }
        };
        ArrayUse use(arrays);
        use.visit_expr(*expr);
        return use.found;
    }

    void visit_Associate(const ASR::Associate_t& x) {
        if (uses_input(x.m_value)) {
            if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
                arrays.insert(ASR::down_cast<ASR::Var_t>(x.m_target)->m_v);
            } else {
                safe = false;
            }
        }
        ASR::BaseWalkVisitor<DescriptorWrapperSafety>::visit_Associate(x);
    }

    void check_args(ASR::call_arg_t* args, size_t n_args) {
        for (size_t i = 0; i < n_args; i++) {
            if (args[i].m_value && ASRUtils::is_array(
                    ASRUtils::expr_type(args[i].m_value))) {
                safe &= !uses_input(args[i].m_value);
            }
        }
    }

    void check_function(const ASR::Function_t& function) {
        size_t previous;
        do {
            previous = arrays.size();
            for (auto& item : function.m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                    visit_Variable(*ASR::down_cast<ASR::Variable_t>(item.second));
                }
            }
            for (size_t i = 0; i < function.n_body; i++) visit_stmt(*function.m_body[i]);
        } while (safe && previous != arrays.size());
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& x) {
        check_args(x.m_args, x.n_args);
        ASR::BaseWalkVisitor<DescriptorWrapperSafety>::visit_FunctionCall(x);
    }
    void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
        check_args(x.m_args, x.n_args);
        ASR::BaseWalkVisitor<DescriptorWrapperSafety>::visit_SubroutineCall(x);
    }
};

// A procedure signature can name a bound of one of its arguments. That
// symbol lives in the procedure. A null() in another scope cannot take
// that signature: the reference would point outside its symbol table.
class ProcedureTypeScopeCheck : public ASR::BaseWalkVisitor<ProcedureTypeScopeCheck> {
public:
    SymbolTable* scope = nullptr;
    bool in_scope = true;

    void note(ASR::symbol_t* sym) {
        if (!in_scope || sym == nullptr) {
            return;
        }
        if (scope == nullptr) {
            in_scope = false;
            return;
        }
        SymbolTable* parent = ASRUtils::symbol_parent_symtab(sym);
        for (SymbolTable* s = scope; s != nullptr; s = s->parent) {
            if (s == parent) {
                return;
            }
        }
        in_scope = false;
    }

    void visit_Var(const ASR::Var_t& x) {
        note(x.m_v);
        if (in_scope) {
            ASR::BaseWalkVisitor<ProcedureTypeScopeCheck>::visit_Var(x);
        }
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& x) {
        note(x.m_name);
        if (in_scope) {
            ASR::BaseWalkVisitor<ProcedureTypeScopeCheck>::visit_FunctionCall(x);
        }
    }
};

static bool procedure_type_symbols_in_scope(ASR::ttype_t* type, SymbolTable* scope) {
    ProcedureTypeScopeCheck check;
    check.scope = scope;
    check.visit_ttype(*type);
    return check.in_scope;
}

/*
    The following visitor replaces procedure calls with arrays as arguments
    to procedure calls having dimensional information passed as arguments. See example below,

        call f1(array1, array2)
        sum = f(array) + g(array)

    gets converted to,

        call f1_array1_array2(array1, m1, n1, array2, m2, n2)
        sum = f_array(array, m, n) + g_array(array, m, n)

    As can be seen dimensional information, m1, n1 is passed along
    with array1 and similarly m2, n2 is passed along with array2.
 */
class EditProcedureCallsVisitor : public ASR::ASRPassBaseWalkVisitor<EditProcedureCallsVisitor>
{
    private:

        Allocator& al;
        PassArrayByDataProcedureVisitor& v;
        std::set<ASR::symbol_t*>& not_to_be_erased;
        const LCompilers::PassOptions& pass_options;
        // Originals whose copy calls them (forward_copies_with_saved_state).
        std::set<ASR::symbol_t*> forwarded;

    public:

        int get_index_kind() const {
            return pass_options.descriptor_index_64 ? 8 : 4;
        }

        EditProcedureCallsVisitor(Allocator& al_,
            PassArrayByDataProcedureVisitor& v_,
            std::set<ASR::symbol_t*>& not_to_be_erased_,
            const LCompilers::PassOptions& pass_options_):
        al(al_), v(v_), not_to_be_erased(not_to_be_erased_), pass_options(pass_options_) {}

        // this is exactly the same as the one in EditProcedureReplacer
        ASR::symbol_t* resolve_new_proc(ASR::symbol_t* old_sym) {
            ASR::symbol_t* ext_sym = ASRUtils::symbol_get_past_external(old_sym);
            if( v.proc2newproc.find(ext_sym) != v.proc2newproc.end() ) {
                ASR::symbol_t* new_sym = v.proc2newproc[ext_sym].first;
                SymbolTable* new_sym_scope = ASRUtils::symbol_parent_symtab(new_sym);
                ASR::asr_t* new_sym_parent = new_sym_scope->asr_owner;
                if ( ASR::is_a<ASR::symbol_t>(*new_sym_parent) &&
                        current_scope->get_counter() != new_sym_scope->get_counter() &&
                        !ASRUtils::is_parent(new_sym_scope, current_scope) ) {
                    ASR::symbol_t* resolved_parent_sym = resolve_new_proc(ASR::down_cast<ASR::symbol_t>(new_sym_parent));
                    if ( resolved_parent_sym != nullptr ) {
                        ASR::symbol_t* sym_to_return = ASRUtils::symbol_symtab(resolved_parent_sym)->get_symbol(ASRUtils::symbol_name(new_sym));
                        return sym_to_return ? sym_to_return : new_sym;
                    }
                }
                return new_sym;
            }
            return nullptr;
        }

        template <typename T>
        void update_args_for_pass_arr_by_data_funcs_passed_as_callback(const T& x) {
            bool args_updated = false;
            Vec<ASR::call_arg_t> new_args;
            new_args.reserve(al, x.n_args);
            for ( size_t i = 0; i < x.n_args; i++ ) {
                ASR::call_arg_t arg = x.m_args[i];
                ASR::expr_t* expr = arg.m_value;
                if (expr) {
                    if (ASR::is_a<ASR::Var_t>(*expr)) {
                        ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(expr);
                        ASR::symbol_t* sym = var->m_v;
                        if ( v.proc2newproc.find(sym) != v.proc2newproc.end() ) {
                            ASR::symbol_t* new_var_sym = resolve_new_proc(sym);
                            ASR::expr_t* new_var = ASRUtils::EXPR(ASR::make_Var_t(al, var->base.base.loc, new_var_sym));
                            {
                                // update exisiting arg
                                arg.m_value = new_var;
                                arg.loc = arg.loc;
                            }
                            args_updated = true;
                        }
                    }
                }
                new_args.push_back(al, arg);
            }
            if (args_updated) {
                T&xx = const_cast<T&>(x);
                xx.m_args = new_args.p;
                xx.n_args = new_args.size();
            }
        }

        inline void get_dimensions(ASR::expr_t* array, Vec<ASR::expr_t*>& dims,
                                        Allocator& al_) {
            int index_kind = get_index_kind();
            ASR::ttype_t* array_type = ASRUtils::expr_type(array);
            // A physical cast's type is a copy of its argument's declared
            // type, which earlier passes do not update; for example, its
            // extent can still be the specification expression `count(x > 0)`.
            // Take the extents from the argument itself.
            ASR::ttype_t* storage_type = ASRUtils::expr_type(
                ASRUtils::get_past_array_physical_cast(array));
            if( ASRUtils::extract_n_dims_from_ttype(storage_type) ==
                    ASRUtils::extract_n_dims_from_ttype(array_type) ) {
                array_type = storage_type;
            }
            ASR::dimension_t* compile_time_dims = nullptr;
            int n_dims = checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(array_type, compile_time_dims));
            if( n_dims == 0 ) {
                ASR::ttype_t* scalar_type = ASRUtils::type_get_past_allocatable_pointer(array_type);
                if( scalar_type && ASR::is_a<ASR::String_t>(*scalar_type) ) {
                    ASRUtils::ASRBuilder builder(al_, array->base.loc);
                    ASR::expr_t* length = builder.StringLen(array);
                    dims.push_back(al_, length);
                    return;
                }
            }
            ASRUtils::ExprStmtDuplicator length_duplicator(al_);
            for( int i = 0; i < n_dims; i++ ) {
                ASR::expr_t* length = compile_time_dims[i].m_length;
                if( length != nullptr ) {
                    // The argument gets its own copy of the extent: later
                    // passes rewrite it in place (inlining a call in it, for
                    // example), and the extent must not change with it.
                    length = length_duplicator.duplicate_expr(length);
                }
                if( length == nullptr ) {
                    if (ASRUtils::is_allocatable(array_type)) {
                        length = ASRUtils::get_size(array, i + 1, al_);
                    } else {
                        // Pass the extent (ubound - lbound + 1), not the
                        // upper bound. The callee stores this value as the
                        // dimension's extent in its descriptor; using ubound
                        // would corrupt the upper bound for arrays with
                        // non-default lower bounds.
                        ASR::expr_t* ub = PassUtils::get_bound(array, i + 1, "ubound", al_, index_kind);
                        ASR::expr_t* lb = PassUtils::get_bound(array, i + 1, "lbound", al_, index_kind);
                        ASR::ttype_t* int_t = ASRUtils::TYPE(ASR::make_Integer_t(al_, array->base.loc, index_kind));
                        ASR::expr_t* one = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al_, array->base.loc, 1, int_t));
                        ASR::expr_t* ub_minus_lb = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al_, array->base.loc,
                            ub, ASR::binopType::Sub, lb, int_t, nullptr));
                        length = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al_, array->base.loc,
                            ub_minus_lb, ASR::binopType::Add, one, int_t, nullptr));
                    }
                }
                if ( ASRUtils::is_integer(*ASRUtils::expr_type(length)) ) {
                    ASR::Integer_t* int_type = ASR::down_cast<ASR::Integer_t>(ASRUtils::type_get_past_array(
                                                ASRUtils::type_get_past_allocatable_pointer(ASRUtils::expr_type(length))));
                    if (int_type->m_kind != index_kind) {
                        // Cast to target index kind
                        length = ASRUtils::EXPR(ASRUtils::make_Cast_t_value(al_, length->base.loc, length, ASR::cast_kindType::IntegerToInteger,
                            ASRUtils::TYPE(ASR::make_Integer_t(al_, length->base.loc, index_kind))));
                    }

                }
                dims.push_back(al_, length);
            }
        }

        int64_t get_expected_n_dims(ASR::symbol_t* subrout_sym, size_t arg_idx) {
            if( !ASR::is_a<ASR::Function_t>(*subrout_sym) ) {
                return -1;
            }
            ASR::Function_t* subrout = ASR::down_cast<ASR::Function_t>(subrout_sym);
            ASR::expr_t** args = subrout->m_args;
            size_t n_args = subrout->n_args;
            auto inplace = v.proc2inplace.find(subrout_sym);
            if( inplace != v.proc2inplace.end() ) {
                // The procedure was transformed in place, so its own argument
                // list already carries the extents. The call site does not
                // yet, so match it against the list it was written for.
                args = inplace->second.args;
                n_args = inplace->second.n_args;
            }
            if( arg_idx >= n_args ) {
                return -1;
            }
            if( !ASR::is_a<ASR::Var_t>(*args[arg_idx]) ) {
                return -1;
            }
            ASR::Variable_t* arg = ASRUtils::EXPR2VAR(args[arg_idx]);
            ASR::dimension_t* dims = nullptr;
            return ASRUtils::extract_dimensions_from_ttype(arg->m_type, dims);
        }

        ASR::expr_t* maybe_cast_class_arg_to_struct(ASR::symbol_t* subrout_sym,
            size_t arg_idx, ASR::expr_t* arg_expr) {
            if( arg_expr == nullptr || !ASR::is_a<ASR::Function_t>(*subrout_sym) ) {
                return arg_expr;
            }
            ASR::Function_t* subrout = ASR::down_cast<ASR::Function_t>(subrout_sym);
            if( arg_idx >= subrout->n_args || !ASR::is_a<ASR::Var_t>(*subrout->m_args[arg_idx]) ) {
                return arg_expr;
            }

            ASR::expr_t* formal_arg = subrout->m_args[arg_idx];
            ASR::ttype_t* formal_arg_full_type = ASRUtils::expr_type(formal_arg);
            if (ASRUtils::is_allocatable(formal_arg_full_type) ||
                ASRUtils::is_pointer(formal_arg_full_type)) {
                return arg_expr;
            }
            if (!ASR::is_a<ASR::ArrayItem_t>(*arg_expr)) {
                return arg_expr;
            }
            ASR::ttype_t* actual_type = ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(arg_expr));
            ASR::ttype_t* formal_type = ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(formal_arg));

            if( !actual_type || !formal_type ||
                !ASR::is_a<ASR::StructType_t>(*actual_type) ||
                !ASR::is_a<ASR::StructType_t>(*formal_type) ) {
                return arg_expr;
            }

            if( !ASRUtils::is_class_type(actual_type) || ASRUtils::is_class_type(formal_type) ) {
                return arg_expr;
            }

            ASR::symbol_t* actual_struct_sym = ASRUtils::symbol_get_past_external(
                ASRUtils::get_struct_sym_from_struct_expr(arg_expr));
            ASR::symbol_t* formal_struct_sym = ASRUtils::symbol_get_past_external(
                ASRUtils::get_struct_sym_from_struct_expr(formal_arg));
            if( !actual_struct_sym || !formal_struct_sym ||
                !ASR::is_a<ASR::Struct_t>(*actual_struct_sym) ||
                !ASR::is_a<ASR::Struct_t>(*formal_struct_sym) ) {
                return arg_expr;
            }

            if( actual_struct_sym != formal_struct_sym ) {
                return arg_expr;
            }

            return ASRUtils::EXPR(ASR::make_Cast_t(al, arg_expr->base.loc, arg_expr,
                ASR::cast_kindType::ClassToStruct,
                ASRUtils::duplicate_type(al, ASRUtils::expr_type(formal_arg)), nullptr, nullptr));
        }

        Vec<ASR::call_arg_t> construct_new_args(ASR::symbol_t* subrout_sym,
            size_t n_args, ASR::call_arg_t* orig_args, std::vector<size_t>& indices) {
            // A device backend hands every array over as a bare pointer, so
            // a physical cast has nothing to convert there; inserting one only
            // puts a temporary pointer between the caller and the callee.
            bool add_physical_cast =
                v.proc2inplace.find(subrout_sym) == v.proc2inplace.end();
            Vec<ASR::call_arg_t> new_args;
            new_args.reserve(al, n_args);
            for( size_t i = 0; i < n_args; i++ ) {
                if (orig_args[i].m_value == nullptr) {
                    new_args.push_back(al, orig_args[i]);
                    continue;
                } else if (std::find(indices.begin(), indices.end(), i) == indices.end()) {
                    ASR::expr_t* expr = orig_args[i].m_value;
                    if (ASR::is_a<ASR::Var_t>(*expr)) {
                        ASR::Var_t* var = ASR::down_cast<ASR::Var_t>(expr);
                        ASR::symbol_t* sym = var->m_v;
                        if ( v.proc2newproc.find(sym) != v.proc2newproc.end() ) {
                            ASR::symbol_t* new_var_sym = v.proc2newproc[sym].first;
                            ASR::expr_t* new_var = ASRUtils::EXPR(ASR::make_Var_t(al, var->base.base.loc, new_var_sym));
                            orig_args[i].m_value = new_var;
                        }
                    }
                    orig_args[i].m_value = maybe_cast_class_arg_to_struct(
                        subrout_sym, i, orig_args[i].m_value);
                    // The function parameter expects PointerArray but the actual
                    // call arg may be DescriptorArray (e.g. a temp created by
                    // subroutine_from_function). Add ArrayPhysicalCast.
                    ASR::ttype_t* arg_type = ASRUtils::expr_type(orig_args[i].m_value);
                    if (add_physical_cast && ASRUtils::is_array(arg_type)) {
                        ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(
                            ASRUtils::type_get_past_allocatable(
                                ASRUtils::type_get_past_pointer(arg_type)));
                        if (array_t->m_physical_type == ASR::array_physical_typeType::DescriptorArray) {
                            ASR::FunctionType_t* ft = ASRUtils::get_FunctionType(subrout_sym);
                            size_t param_idx = i;
                            if (param_idx < ft->n_arg_types &&
                                    ASRUtils::is_array(ft->m_arg_types[param_idx])) {
                                ASR::Array_t* param_array = ASR::down_cast<ASR::Array_t>(
                                    ASRUtils::type_get_past_allocatable(
                                        ASRUtils::type_get_past_pointer(ft->m_arg_types[param_idx])));
                                if (param_array->m_physical_type == ASR::array_physical_typeType::PointerArray) {
                                    ASR::expr_t* physical_cast = ASRUtils::EXPR(
                                        ASRUtils::make_ArrayPhysicalCast_t_util(
                                            al, orig_args[i].m_value->base.loc,
                                            orig_args[i].m_value, array_t->m_physical_type,
                                            ASR::array_physical_typeType::PointerArray,
                                            ASRUtils::duplicate_type(al, arg_type, nullptr,
                                                ASR::array_physical_typeType::PointerArray, true),
                                            nullptr));
                                    orig_args[i].m_value = physical_cast;
                                }
                            }
                        }
                    }
                    new_args.push_back(al, orig_args[i]);
                    continue;
                }

                ASR::expr_t* orig_arg_i = orig_args[i].m_value;
                ASR::ttype_t* orig_arg_type = ASRUtils::expr_type(orig_arg_i);
                if( add_physical_cast && ASRUtils::is_array(orig_arg_type) ) {
                    ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(
                        ASRUtils::type_get_past_allocatable(ASRUtils::type_get_past_pointer(orig_arg_type)));
                    if( array_t->m_physical_type != ASR::array_physical_typeType::PointerArray ) {
                        // Every argument in `indices` is an assumed-shape
                        // dummy, which this pass turns into a data pointer
                        // whose extents travel as separate arguments. The cast
                        // target therefore describes the dummy's declared
                        // shape, which declares no extent: a call-site size
                        // check must not measure it against the actual
                        // argument's extents.
                        ASR::FunctionType_t* ft = ASRUtils::get_FunctionType(subrout_sym);
                        ASR::dimension_t* formal_dims = nullptr;
                        int n_formal_dims = (i < ft->n_arg_types) ?
                            checked_integer_cast<int>(
                                ASRUtils::extract_dimensions_from_ttype(
                                    ft->m_arg_types[i], formal_dims)) : 0;
                        Vec<ASR::dimension_t> cast_dims;
                        ASR::ttype_t* cast_type = nullptr;
                        if ((size_t) n_formal_dims == array_t->n_dims) {
                            cast_dims.from_pointer_n_copy(al, formal_dims, n_formal_dims);
                            cast_type = ASRUtils::duplicate_type(al, orig_arg_type,
                                &cast_dims, ASR::array_physical_typeType::PointerArray, true);
                        } else {
                            cast_type = ASRUtils::duplicate_type(al, orig_arg_type,
                                nullptr, ASR::array_physical_typeType::PointerArray, true);
                        }
                        ASR::expr_t* physical_cast = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(
                            al, orig_arg_i->base.loc, orig_arg_i, array_t->m_physical_type,
                            ASR::array_physical_typeType::PointerArray, cast_type, nullptr));
                        ASR::call_arg_t physical_cast_arg;
                        physical_cast_arg.loc = orig_arg_i->base.loc;
                        physical_cast_arg.m_value = physical_cast;
                        new_args.push_back(al, physical_cast_arg);
                    } else {
                        new_args.push_back(al, orig_args[i]);
                    }
                } else {
                    new_args.push_back(al, orig_args[i]);
                }

                Vec<ASR::expr_t*> actual_dim_vars;
                actual_dim_vars.reserve(al, 2);
                get_dimensions(orig_arg_i, actual_dim_vars, al);

                int64_t expected_n_dims = get_expected_n_dims(subrout_sym, i);
                Vec<ASR::expr_t*> dim_vars;
                dim_vars.reserve(al, actual_dim_vars.size());

                if( expected_n_dims <= 0 ||
                    (size_t) expected_n_dims == actual_dim_vars.size() ) {
                    for( size_t j = 0; j < actual_dim_vars.size(); j++ ) {
                        dim_vars.push_back(al, actual_dim_vars[j]);
                    }
                } else {
                    ASRUtils::ASRBuilder builder(al, orig_arg_i->base.loc);
                    if( (size_t) expected_n_dims < actual_dim_vars.size() ) {
                        for( int64_t j = 0; j < expected_n_dims - 1; j++ ) {
                            dim_vars.push_back(al, actual_dim_vars[j]);
                        }
                        ASR::expr_t* tail_prod = actual_dim_vars[expected_n_dims - 1];
                        for( size_t j = (size_t) expected_n_dims; j < actual_dim_vars.size(); j++ ) {
                            tail_prod = builder.Mul(tail_prod, actual_dim_vars[j]);
                        }
                        dim_vars.push_back(al, tail_prod);
                    } else {
                        for( size_t j = 0; j < actual_dim_vars.size(); j++ ) {
                            dim_vars.push_back(al, actual_dim_vars[j]);
                        }
                        int index_kind = get_index_kind();
                        for( int64_t j = (int64_t) actual_dim_vars.size(); j < expected_n_dims; j++ ) {
                            dim_vars.push_back(al, make_ConstantWithKind(make_IntegerConstant_t, make_Integer_t, 1, index_kind, orig_arg_i->base.loc));
                        }
                    }
                }

                for( size_t j = 0; j < dim_vars.size(); j++ ) {
                    ASR::call_arg_t dim_var;
                    dim_var.loc = dim_vars[j]->base.loc;
                    dim_var.m_value = dim_vars[j];
                    new_args.push_back(al, dim_var);
                }
            }
            return new_args;
        }

        // Whether the copy of `f` can forward its result from a call of `f`.
        // subroutine_from_function has made the other results arguments,
        // except in the WASM pipeline, which runs this pass without it:
        // there such a copy keeps its own saved state, as it did before
        // forwarding.
        static bool forwardable_result(ASR::Function_t* f, bool& pointer_result) {
            pointer_result = false;
            if (!f->m_return_var) return true;
            ASR::ttype_t* type = ASRUtils::expr_type(f->m_return_var);
            pointer_result = ASRUtils::is_pointer(type);
            return pointer_result || (!ASRUtils::is_array(type) &&
                !ASRUtils::is_allocatable(type) &&
                (ASRUtils::is_integer(*type) || ASRUtils::is_real(*type) ||
                 ASRUtils::is_complex(*type) || ASRUtils::is_logical(*type)));
        }

        // The body of `copy` that calls `original` with its own dummies,
        // passing as descriptors the arrays at `data_args` that it takes by
        // data. The copy has a dummy of the same name for each one of the
        // original.
        Vec<ASR::stmt_t*> forwarding_body(ASR::Function_t* original,
                ASR::Function_t* copy, const std::vector<size_t>& data_args,
                bool pointer_result) {
            ASR::symbol_t* callee = &original->base;
            const Location& loc = copy->base.base.loc;
            Vec<ASR::call_arg_t> args;
            args.reserve(al, original->n_args);
            for (size_t i = 0; i < original->n_args; i++) {
                ASR::symbol_t* sym = copy->m_symtab->get_symbol(ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(original->m_args[i])->m_v));
                LCOMPILERS_ASSERT(sym);
                ASR::call_arg_t arg;
                arg.loc = original->m_args[i]->base.loc;
                arg.m_value = ASRUtils::EXPR(ASR::make_Var_t(al, arg.loc, sym));
                if (std::find(data_args.begin(), data_args.end(), i) != data_args.end()) {
                    arg.m_value = ASRUtils::cast_to_descriptor(al, arg.m_value);
                }
                args.push_back(al, arg);
            }

            Vec<ASR::stmt_t*> body;
            body.reserve(al, 1);
            if (copy->m_return_var) {
                ASR::expr_t* value = ASRUtils::EXPR(ASR::make_FunctionCall_t(al, loc,
                    callee, callee, args.p, args.size(),
                    ASRUtils::expr_type(copy->m_return_var), nullptr, nullptr));
                if (pointer_result) {
                    body.push_back(al, ASRUtils::STMT(ASR::make_Associate_t(al, loc,
                        copy->m_return_var, value,
                        ASR::pointer_bounds_formType::NoPointerBounds)));
                } else {
                    body.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
                        copy->m_return_var, value, nullptr, false, false, false, false)));
                }
            } else {
                body.push_back(al, ASRUtils::STMT(ASR::make_SubroutineCall_t(al, loc,
                    callee, callee, args.p, args.size(), nullptr, false)));
            }
            return body;
        }

        // The original finalizes and initializes its intent(out) dummies on
        // entry; a copy that forwards to it must not do it a second time.
        // Both intents pass arguments alike. A separate module procedure
        // keeps the interface its ancestor module declares, which asr_verify
        // compares it with, so the copy of that interface is relaxed too.
        static void relax_intent_out(ASR::Function_t* copy) {
            std::vector<ASR::Function_t*> procedures{copy};
            SymbolTable* parent = copy->m_symtab->parent;
            ASR::asr_t* owner = parent->asr_owner;
            if (owner && ASR::is_a<ASR::symbol_t>(*owner) &&
                    ASR::is_a<ASR::Module_t>(*ASR::down_cast<ASR::symbol_t>(owner))) {
                ASR::Function_t* declared = declared_module_interface(parent->parent,
                    ASR::down_cast2<ASR::Module_t>(owner)->m_parent_module, copy->m_name);
                if (declared) procedures.push_back(declared);
            }
            for (ASR::Function_t* f : procedures) {
                for (size_t i = 0; i < f->n_args; i++) {
                    ASR::Variable_t* arg = ASRUtils::expr_to_variable_or_null(f->m_args[i]);
                    if (arg && arg->m_intent == ASR::intentType::Out) {
                        arg->m_intent = ASR::intentType::InOut;
                    }
                }
            }
        }

        // A saved variable of a procedure is shared by all its calls (F2023
        // 8.5.16), but the copy that takes arrays by data duplicated it with
        // the rest of the procedure. Where the original that takes
        // descriptors is kept beside the copy, the copy calls the original
        // instead, so that both entries reach the original's variables. The
        // contiguous array the copy takes is a valid actual argument for the
        // assumed-shape dummy of the original.
        //
        // The forwarder keeps exactly the symbols its signature needs
        // (signature_closure) and drops the others, which only the old body
        // used. An internal procedure the signature needs, such as the
        // interface of a dummy procedure, becomes an interface. The
        // specification expressions of the dummies and result can use
        // dummies, named constants, COMMON and module variables and
        // non-internal pure functions, which are declarations, and
        // inquiries about local objects (F2018 10.1.11). The forwarder keeps
        // a local that is only inquired about, unused, and
        // StandIns::apply() makes its inquiries ask stand-ins
        // with the same answers, so that the forwarder neither initializes
        // nor finalizes it, and allocates at most a byte per element or
        // character of it: only the original does more, once per call.
        // Evaluating a specification function of the signature in both
        // entries is harmless: it is pure.
        //
        // A copy that is not forwarded keeps its own saved state, which
        // calls with a section (on the original) and the other calls (on
        // the copy) then split. signature_closure keeps every variable that
        // the signature refers to, so it fails only for a symbol that is
        // neither a variable nor a declaration nor an internal procedure,
        // which no specification expression refers to, and every local
        // that a signature can refer to with an effect on entry or exit is
        // only inquired about, which StandIns::covers() accepts. A debug
        // build checks that an external procedure is always forwarded. A
        // result that forwardable_result refuses is an argument by then
        // (subroutine_from_function), unless a pipeline omits that pass.
        void forward_copies_with_saved_state() {
            for (auto& entry : v.proc2newproc) {
                if (!ASR::is_a<ASR::Function_t>(*entry.first) ||
                        v.newprocs.find(entry.second.first) == v.newprocs.end()) continue;
                ASR::Function_t* original = ASR::down_cast<ASR::Function_t>(entry.first);
                ASR::Function_t* copy = ASR::down_cast<ASR::Function_t>(entry.second.first);
                SymbolTable* parent = original->m_symtab->parent;
                bool pointer_result;
                std::set<ASR::symbol_t*> signature;
                std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>> interfaces;
                std::set<ASR::symbol_t*> inquired_only;
                if (parent->get_symbol(original->m_name) != entry.first ||
                        parent->get_symbol(copy->m_name) != entry.second.first ||
                        original->n_body == 0 || !has_saved_state(original) ||
                        !forwardable_result(original, pointer_result)) continue;
                if (!signature_closure(copy, signature, interfaces, inquired_only) ||
                        !StandIns::covers(signature, interfaces, inquired_only)) {
                    LCOMPILERS_ASSERT(!ASRUtils::is_tu_scope(parent));
                    continue;
                }
                forwarded.insert(entry.first);

                Vec<ASR::stmt_t*> body = forwarding_body(original, copy,
                    entry.second.second, pointer_result);
                copy->m_body = body.p;
                copy->n_body = body.size();
                relax_intent_out(copy);
                std::vector<std::string> unused;
                for (auto& item : copy->m_symtab->get_scope()) {
                    if (!signature.count(item.second)) unused.push_back(item.first);
                }
                for (auto& name : unused) copy->m_symtab->erase_symbol(name);
                for (auto& item : interfaces) {
                    make_interface(al, ASR::down_cast<ASR::Function_t>(item.first),
                        item.second);
                }
                StandIns(al).apply(copy, inquired_only);
            }
        }

        void wrap_descriptor_entries() {
            for (auto& entry : v.proc2newproc) {
                // The copy of a forwarded original calls it already.
                if (!ASR::is_a<ASR::Function_t>(*entry.first) ||
                        forwarded.count(entry.first)) continue;
                ASR::Function_t* original = ASR::down_cast<ASR::Function_t>(entry.first);
                ASR::FunctionType_t* ft = ASRUtils::get_FunctionType(original);
                if (original->m_symtab->parent->get_symbol(original->m_name) != entry.first ||
                        original->m_return_var || original->n_body == 0 ||
                        ft->m_deftype != ASR::deftypeType::Implementation ||
                        ft->m_abi != ASR::abiType::Source || original->m_gpu ||
                        ASRUtils::runs_on_device(*original)) continue;

                std::set<ASR::symbol_t*> arguments, arrays;
                bool eligible = true;
                for (size_t i = 0; i < original->n_args; i++) {
                    ASR::Variable_t* arg = ASRUtils::expr_to_variable_or_null(original->m_args[i]);
                    if (!arg) { eligible = false; break; }
                    arguments.insert(&arg->base);
                    if (arg->m_presence != ASR::presenceType::Required ||
                            ASRUtils::is_allocatable(arg->m_type) ||
                            ASRUtils::is_pointer(arg->m_type) || arg->m_target_attr ||
                            arg->m_is_volatile || arg->m_is_asynchronous || arg->n_codims) {
                        eligible = false;
                        break;
                    }
                    if (!ASRUtils::is_array(arg->m_type)) {
                        // Optional lowering replaces OPTIONAL with required
                        // dummies and logical presence flags. Keep those
                        // entries unchanged by accepting only numeric scalars
                        // and derived-type receivers.
                        bool receiver = ASR::is_a<ASR::StructType_t>(*arg->m_type);
                        if (!(ASRUtils::is_integer(*arg->m_type) ||
                              ASRUtils::is_real(*arg->m_type) ||
                              ASRUtils::is_complex(*arg->m_type) ||
                              receiver) ||
                                (receiver && arg->m_intent == ASRUtils::intent_out)) eligible = false;
                        continue;
                    }
                    ASR::ttype_t* element = ASRUtils::type_get_past_array(arg->m_type);
                    ASR::dimension_t* dims;
                    size_t rank = ASRUtils::extract_dimensions_from_ttype(arg->m_type, dims);
                    bool assumed_shape = true;
                    for (size_t j = 0; j < rank; j++) assumed_shape &= !dims[j].m_length;
                    if (arg->m_intent != ASRUtils::intent_in || !assumed_shape ||
                            !(ASRUtils::is_integer(*element) || ASRUtils::is_real(*element) ||
                              ASRUtils::is_complex(*element)) ||
                            std::find(entry.second.second.begin(), entry.second.second.end(), i)
                                == entry.second.second.end()) {
                        eligible = false;
                        break;
                    }
                    arrays.insert(&arg->base);
                }
                if (!eligible || arrays.empty()) continue;

                // Saved state and contained procedures belong to this entry;
                // do not move their ownership to a specialised copy.
                std::vector<SymbolTable*> scopes{original->m_symtab};
                for (size_t i = 0; i < scopes.size() && eligible; i++) {
                    for (auto& item : scopes[i]->get_scope()) {
                        if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) continue;
                        if (ASR::is_a<ASR::Block_t>(*item.second)) {
                            scopes.push_back(ASR::down_cast<ASR::Block_t>(
                                item.second)->m_symtab);
                            continue;
                        }
                        if (!ASR::is_a<ASR::Variable_t>(*item.second)) {
                            eligible = false;
                            break;
                        }
                        ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(item.second);
                        if (arguments.count(item.second)) continue;
                        ASR::ttype_t* element = ASRUtils::type_get_past_array(
                            ASRUtils::type_get_past_allocatable_pointer(var->m_type));
                        if (var->m_storage == ASR::storage_typeType::Save ||
                                var->m_storage == ASR::storage_typeType::Threadprivate ||
                                var->m_common_block ||
                                !(ASRUtils::is_integer(*element) || ASRUtils::is_real(*element) ||
                                  ASRUtils::is_complex(*element) || ASRUtils::is_logical(*element))) {
                            eligible = false;
                            break;
                        }
                    }
                }
                if (!eligible) continue;
                DescriptorWrapperSafety safety(arrays);
                safety.check_function(*original);
                if (!safety.safe) continue;

                struct SignatureSymbols : public ASR::BaseWalkVisitor<SignatureSymbols> {
                    std::set<ASR::symbol_t*> needed;
                    bool safe = true;
                    void visit_Var(const ASR::Var_t& x) {
                        if (needed.insert(x.m_v).second &&
                                ASR::is_a<ASR::Variable_t>(*x.m_v)) {
                            visit_Variable(*ASR::down_cast<ASR::Variable_t>(x.m_v));
                        }
                    }
                    // A specification function must not be evaluated twice.
                    void visit_FunctionCall(const ASR::FunctionCall_t&) { safe = false; }
                } signature;
                for (size_t i = 0; i < original->n_args; i++) {
                    signature.visit_expr(*original->m_args[i]);
                }
                if (!signature.safe) continue;

                current_scope = original->m_symtab;
                Vec<ASR::call_arg_t> args;
                args.reserve(al, original->n_args);
                for (size_t i = 0; i < original->n_args; i++) {
                    ASR::call_arg_t arg;
                    arg.loc = original->m_args[i]->base.loc;
                    arg.m_value = original->m_args[i];
                    args.push_back(al, arg);
                }
                Vec<ASR::call_arg_t> converted = construct_new_args(entry.first,
                    args.size(), args.p, entry.second.second);
                Vec<ASR::stmt_t*> body;
                body.reserve(al, 1);
                body.push_back(al, ASRUtils::STMT(ASR::make_SubroutineCall_t(al,
                    original->base.base.loc, entry.second.first, entry.second.first,
                    converted.p, converted.size(), nullptr, false)));
                original->m_body = body.p;
                original->n_body = body.size();

                // The old automatic arrays are owned by the data-ABI body.
                // Retain only the declarations needed by the original signature,
                // including the transitive dependencies of its bounds.
                std::vector<std::string> unused;
                for (auto& item : current_scope->get_scope()) {
                    if ((ASR::is_a<ASR::Variable_t>(*item.second) &&
                            !signature.needed.count(item.second)) ||
                            ASR::is_a<ASR::Block_t>(*item.second)) unused.push_back(item.first);
                }
                for (auto& name : unused) current_scope->erase_symbol(name);
            }
            current_scope = nullptr;
        }

        // A host-associated explicit-shape array is a descriptor view. The
        // dummy it is passed to is still a data pointer. Extract that pointer
        // here: the callee was not rewritten, so the call would otherwise
        // hand over the descriptor.
        template <typename T>
        void cast_descriptor_actual_to_data_pointer(T& x) {
            if( x.m_name == nullptr ) {
                return;
            }
            ASR::symbol_t* resolved = ASRUtils::symbol_get_past_external(x.m_name);
            if( !(ASR::is_a<ASR::Function_t>(*resolved) ||
                    ASR::is_a<ASR::StructMethodDeclaration_t>(*resolved) ||
                    ASR::is_a<ASR::Variable_t>(*resolved)) ) {
                return;
            }
            if( ASR::is_a<ASR::Variable_t>(*resolved) ) {
                ASR::ttype_t* var_type = ASR::down_cast<ASR::Variable_t>(resolved)->m_type;
                if( ASR::is_a<ASR::Pointer_t>(*var_type) ) {
                    var_type = ASR::down_cast<ASR::Pointer_t>(var_type)->m_type;
                }
                if( !ASR::is_a<ASR::FunctionType_t>(*var_type) ) {
                    return;
                }
            }
            ASR::FunctionType_t* ft = ASRUtils::get_FunctionType(x.m_name);
            for( size_t i = 0; i < x.n_args && i < ft->n_arg_types; i++ ) {
                if( x.m_args[i].m_value == nullptr ) {
                    continue;
                }
                ASR::ttype_t* formal = ft->m_arg_types[i];
                if( formal == nullptr || ASRUtils::is_pointer(formal) ||
                        ASRUtils::is_allocatable(formal) ||
                        !ASRUtils::is_array(formal) ||
                        ASRUtils::extract_physical_type(formal) !=
                            ASR::array_physical_typeType::PointerArray ) {
                    continue;
                }
                ASR::ttype_t* actual = ASRUtils::expr_type(x.m_args[i].m_value);
                if( actual == nullptr || !ASRUtils::is_array(actual) ) {
                    continue;
                }
                ASR::ttype_t* actual_array = ASRUtils::type_get_past_allocatable(
                    ASRUtils::type_get_past_pointer(actual));
                if( !ASR::is_a<ASR::Array_t>(*actual_array) ||
                        ASR::down_cast<ASR::Array_t>(actual_array)->m_physical_type !=
                            ASR::array_physical_typeType::DescriptorArray ) {
                    continue;
                }
                ASR::ttype_t* cast_type = ASRUtils::duplicate_type(al, actual_array,
                    nullptr, ASR::array_physical_typeType::PointerArray, true);
                x.m_args[i].m_value = ASRUtils::EXPR(ASRUtils::make_ArrayPhysicalCast_t_util(
                    al, x.m_args[i].m_value->base.loc, x.m_args[i].m_value,
                    ASR::array_physical_typeType::DescriptorArray,
                    ASR::array_physical_typeType::PointerArray, cast_type, nullptr));
            }
        }

        bool can_edit_call(ASR::call_arg_t* args, size_t n_args) {
            for ( size_t i = 0; i < n_args; i++ ) {
                if( args[i].m_value &&
                    ASRUtils::expr_type(args[i].m_value) &&
                    ASR::is_a<ASR::Pointer_t>(*
                        ASRUtils::expr_type(args[i].m_value)) ) {
                    return false;
                }
            }
            return true;
        }
        
        // A dummy procedure that takes arrays by data replaces the one that
        // took descriptors, so no procedure taking descriptors is left to
        // call: every call has to go to the replacement.
        bool replaces_dummy_procedure(ASR::symbol_t* sym) {
            auto it = v.proc2newproc.find(sym);
            if( it == v.proc2newproc.end() ) {
                return false;
            }
            ASR::symbol_t* new_sym = ASRUtils::symbol_get_past_external(it->second.first);
            return ASR::is_a<ASR::Function_t>(*new_sym) &&
                ASRUtils::is_dummy_procedure(*ASR::down_cast<ASR::Function_t>(new_sym));
        }

        static bool is_struct_method_declaration(ASR::symbol_t* const sym){
            return ASR::is_a<ASR::StructMethodDeclaration_t>(*ASRUtils::symbol_get_past_external(sym));
        }

        template <typename T>
        void visit_Call(const T& x) {
            T& xx = const_cast<T&>(x);
            ASR::symbol_t* subrout_sym = x.m_name;
            bool is_external = ASR::is_a<ASR::ExternalSymbol_t>(*subrout_sym);
            subrout_sym = ASRUtils::symbol_get_past_StructMethodDeclaration(
                            ASRUtils::symbol_get_past_external(subrout_sym));
            if(ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(x.m_name))){
                // Case: procedure(cb) :: call_back (Here call_back is variable of type cb which is a function)
                ASR::Variable_t* variable = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(x.m_name));
                ASR::symbol_t* type_dec = variable->m_type_declaration;
                subrout_sym = ASRUtils::symbol_get_past_external(type_dec);

                // check if subrout_sym is present as value in proc2newproc
                bool is_present = false;
                std::vector<size_t> present_indices;
                ASR::symbol_t* original_subrout_sym = nullptr;
                for ( auto it: v.proc2newproc ) {
                    if (it.second.first == subrout_sym) {
                        is_present = true;
                        present_indices = it.second.second;
                        original_subrout_sym = it.first;
                        break;
                    }
                }

                if ( v.proc2newproc.find(subrout_sym) != v.proc2newproc.end()) {
                    ASR::symbol_t* new_x_name = nullptr;
                    if (is_external && (v.proc2newproc.find(x.m_name) != v.proc2newproc.end())) {
                        new_x_name = v.proc2newproc[x.m_name].first;
                    } else {
                        new_x_name = resolve_new_proc(x.m_name);
                        // If the call target was an ExternalSymbol and
                        // resolve_new_proc returned a symbol outside the
                        // current scope (e.g. a new Variable inside a
                        // struct's symtab), wrap it in an ExternalSymbol
                        // so that m_name stays within the current scope.
                        if (is_external && new_x_name != nullptr &&
                            ASRUtils::symbol_parent_symtab(new_x_name)->get_counter() !=
                                current_scope->get_counter()) {
                            ASR::ExternalSymbol_t* orig_ext =
                                ASR::down_cast<ASR::ExternalSymbol_t>(x.m_name);
                            std::string new_ext_name = current_scope->get_unique_name(
                                ASRUtils::symbol_name(new_x_name));
                            ASR::symbol_t* new_ext = ASR::down_cast<ASR::symbol_t>(
                                ASR::make_ExternalSymbol_t(al, x.m_name->base.loc,
                                    current_scope, s2c(al, new_ext_name), new_x_name,
                                    orig_ext->m_module_name,
                                    orig_ext->m_scope_names, orig_ext->n_scope_names,
                                    ASRUtils::symbol_name(new_x_name),
                                    orig_ext->m_access));
                            current_scope->add_symbol(new_ext_name, new_ext);
                            new_x_name = new_ext;
                        }
                    }
                    // If the variable hasn't been registered yet (e.g. due to
                    // alphabetical visit order where an AssociateBlock or Block
                    // containing the call is visited before this variable),
                    // process it now so the call can be properly updated.
                    if ( new_x_name == nullptr && !is_external &&
                         v.proc2newproc.find((ASR::symbol_t*)variable) == v.proc2newproc.end() ) {
                        visit_Variable(*variable);
                        new_x_name = resolve_new_proc(x.m_name);
                    }
                    if ( new_x_name != nullptr ) {
                        ASR::Function_t* new_func = ASR::down_cast<ASR::Function_t>(resolve_new_proc(subrout_sym));
                        ASR::Variable_t* call_var = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(x.m_name));
                        ASR::ttype_t* new_type = new_func->m_function_signature;
                        if (ASR::is_a<ASR::Pointer_t>(*call_var->m_type)) {
                            new_type = ASRUtils::TYPE(ASR::make_Pointer_t(v.al, call_var->base.base.loc, new_type));
                        }
                        call_var->m_type = new_type;
                        xx.m_name = new_x_name;
                        xx.m_original_name = new_x_name;
                        std::vector<size_t>& indices = v.proc2newproc[subrout_sym].second;
                        // Self is now explicitly in call args, so no offset needed.
                        Vec<ASR::call_arg_t> new_args = construct_new_args(subrout_sym, x.n_args, x.m_args, indices);
                        xx.m_args = new_args.p;
                        xx.n_args = new_args.size();
                        return;
                    } else if ( new_x_name == nullptr ) {
                        std::vector<size_t>& indices = v.proc2newproc[subrout_sym].second;
                        Vec<ASR::call_arg_t> new_args = construct_new_args(subrout_sym, x.n_args, x.m_args, indices);
                        xx.m_args = new_args.p;
                        xx.n_args = new_args.size();
                        return;
                    }
                } else if ( is_present ) {
                    Vec<ASR::call_arg_t> new_args = construct_new_args(original_subrout_sym, x.n_args, x.m_args, present_indices);
                    xx.m_args = new_args.p;
                    xx.n_args = new_args.size();
                    return;
                }
            }

            auto inplace = v.proc2inplace.find(subrout_sym);
            if( inplace != v.proc2inplace.end() ) {
                // The callee kept its name and symbol, so only the arguments
                // have to grow.
                Vec<ASR::call_arg_t> new_args = construct_new_args(subrout_sym,
                    x.n_args, x.m_args, inplace->second.indices);
                xx.m_args = new_args.p;
                xx.n_args = new_args.size();
                return;
            }
            if( !can_edit_call(x.m_args, x.n_args)
                    && !is_struct_method_declaration(x.m_name)
                    && !replaces_dummy_procedure(subrout_sym) ) {
                cast_descriptor_actual_to_data_pointer(xx);
                not_to_be_erased.insert(subrout_sym);
                return ;
            }
            if( v.proc2newproc.find(subrout_sym) == v.proc2newproc.end() ) {
                update_args_for_pass_arr_by_data_funcs_passed_as_callback(x);
                cast_descriptor_actual_to_data_pointer(xx);
                return;
            }

            // Don't transform calls dispatched through StructMethodDeclaration
            // (type-bound procedures). These may go through the vtable, which
            // is linkonce_odr and must be identical across all compilation
            // units. Since ____N versions are only created when function bodies
            // are available, the vtable would be inconsistent between units
            // that define vs. merely use the type.
            if (is_struct_method_declaration(x.m_name)) {
                cast_descriptor_actual_to_data_pointer(xx);
                not_to_be_erased.insert(subrout_sym);
                return;
            }

            ASR::symbol_t* new_func_sym = resolve_new_proc(subrout_sym);
            std::vector<size_t>& indices = v.proc2newproc[subrout_sym].second;
            Vec<ASR::call_arg_t> new_args = construct_new_args(subrout_sym, x.n_args, x.m_args, indices);

            {
                ASR::Function_t* new_func_ = ASR::down_cast<ASR::Function_t>(new_func_sym);
                size_t min_args = 0, max_args = 0;
                for( size_t i = 0; i < new_func_->n_args; i++ ) {
                    ASR::Var_t* arg = ASR::down_cast<ASR::Var_t>(new_func_->m_args[i]);
                    if( ASR::is_a<ASR::Variable_t>(*arg->m_v) &&
                        ASR::down_cast<ASR::Variable_t>(arg->m_v)->m_presence
                            == ASR::presenceType::Optional ) {
                        max_args += 1;
                    } else {
                        min_args += 1;
                        max_args += 1;
                    }
                }
                if( !(min_args <= new_args.size() &&
                    new_args.size() <= max_args) ) {
                    throw LCompilersException("Number of arguments in the new function call " +
                        std::string(ASRUtils::symbol_name(subrout_sym)) + " doesn't satisfy "
                        "min_args <= new_args.size() <= max_args, " + std::to_string(min_args) + " <= " +
                        std::to_string(new_args.size()) + " <= " + std::to_string(max_args));
                }
            }
            ASR::symbol_t* new_func_sym_ = new_func_sym;
            // Note: smd.m_proc is intentionally NOT updated (see
            // visit_StructMethodDeclaration). Calls through SMDs are skipped
            // earlier in this function so this code path is only reached for
            // non-SMD calls.
            if( is_external && !is_struct_method_declaration(x.m_name) ) { // Redirect ExternalSymbol to new fn symbol.
                ASR::ExternalSymbol_t* func_ext_sym = ASR::down_cast<ASR::ExternalSymbol_t>(x.m_name);
                std::string new_func_sym_name = std::string(ASRUtils::symbol_name(new_func_sym));
                ASR::symbol_t* existing_sym = current_scope->resolve_symbol(new_func_sym_name);
                if( existing_sym != nullptr ) {
                    ASR::symbol_t* existing_target = ASRUtils::symbol_get_past_external(existing_sym);
                    if( existing_target == new_func_sym ) {
                        new_func_sym_ = existing_sym;
                    } else {
                        // Name is occupied by an unrelated symbol (possibly in a
                        // parent scope). Create a unique alias for this external
                        // symbol in the scope it is added to.
                        new_func_sym_name = func_ext_sym->m_parent_symtab->get_unique_name(
                            new_func_sym_name, false);
                        existing_sym = nullptr;
                    }
                }
                if( existing_sym == nullptr ) {
                    char* new_func_sym_name_c = s2c(al, new_func_sym_name);
                    if ( ASR::is_a<ASR::Variable_t>(*ASRUtils::symbol_get_past_external(func_ext_sym->m_external)) &&
                         ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(func_ext_sym->m_external))->m_type_declaration ) {
                        ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(ASRUtils::symbol_get_past_external(func_ext_sym->m_external));
                        ASR::symbol_t* type_dec_sym = current_scope->resolve_symbol(ASRUtils::symbol_name(var->m_type_declaration));
                        std::string module_name = func_ext_sym->m_module_name;

                        if ( type_dec_sym && ASR::is_a<ASR::ExternalSymbol_t>(*type_dec_sym) ) {
                            module_name = ASR::down_cast<ASR::ExternalSymbol_t>(type_dec_sym)->m_module_name;
                        }
                        new_func_sym_ = ASR::down_cast<ASR::symbol_t>(
                            ASR::make_ExternalSymbol_t(al, x.m_name->base.loc, func_ext_sym->m_parent_symtab,
                                new_func_sym_name_c, new_func_sym, s2c(al, module_name),
                                func_ext_sym->m_scope_names, func_ext_sym->n_scope_names,
                                ASRUtils::symbol_name(new_func_sym),
                                func_ext_sym->m_access));
                    } else {
                        new_func_sym_ = ASR::down_cast<ASR::symbol_t>(
                            ASR::make_ExternalSymbol_t(al, x.m_name->base.loc, func_ext_sym->m_parent_symtab,
                                new_func_sym_name_c, new_func_sym, func_ext_sym->m_module_name,
                                func_ext_sym->m_scope_names, func_ext_sym->n_scope_names,
                                ASRUtils::symbol_name(new_func_sym),
                                func_ext_sym->m_access));
                    }

                    func_ext_sym->m_parent_symtab->add_symbol(new_func_sym_name, new_func_sym_);
                }
            }
            if(!ASR::is_a<ASR::Variable_t>(*x.m_name) && !is_struct_method_declaration(x.m_name)){
                xx.m_name = new_func_sym_;
                xx.m_original_name = new_func_sym_;
            } else if(v.proc2newproc.find(x.m_name) != v.proc2newproc.end()){
                xx.m_name = resolve_new_proc(x.m_name);
                xx.m_original_name = resolve_new_proc(x.m_name);
            }
            xx.m_args = new_args.p;
            xx.n_args = new_args.size();
        }

        // A GPU kernel is transformed in place, so the launch keeps
        // pointing at the same symbol and only has to hand over the extents
        // the kernel now takes as arguments.
        void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t& x) {
            ASR::ASRPassBaseWalkVisitor<EditProcedureCallsVisitor>::visit_GpuKernelLaunch(x);
            auto it = v.proc2inplace.find(x.m_kernel);
            if( it == v.proc2inplace.end() ) {
                return;
            }
            std::vector<size_t>& indices = it->second.indices;
            Vec<ASR::call_arg_t> new_args;
            new_args.reserve(al, x.n_args);
            for( size_t i = 0; i < x.n_args; i++ ) {
                new_args.push_back(al, x.m_args[i]);
                if( std::find(indices.begin(), indices.end(), i) == indices.end() ) {
                    continue;
                }
                Vec<ASR::expr_t*> dim_vars;
                dim_vars.reserve(al, 2);
                get_dimensions(x.m_args[i].m_value, dim_vars, al);
                for( size_t j = 0; j < dim_vars.size(); j++ ) {
                    ASR::call_arg_t dim_arg;
                    dim_arg.loc = dim_vars[j]->base.loc;
                    dim_arg.m_value = dim_vars[j];
                    new_args.push_back(al, dim_arg);
                }
            }
            ASR::GpuKernelLaunch_t& xx = const_cast<ASR::GpuKernelLaunch_t&>(x);
            xx.m_args = new_args.p;
            xx.n_args = new_args.size();
        }

        void visit_TranslationUnit(const ASR::TranslationUnit_t &x) {
            SymbolTable* current_scope_copy = current_scope;
            current_scope = x.m_symtab;
            // Visit modules in dependency order so that struct member
            // transformations in a dependency are visible when processing
            // modules that use those structs (e.g. StructInstanceMember
            // references).
            std::vector<std::string> mod_order
                = ASRUtils::determine_module_dependencies(x);
            for (auto &name : mod_order) {
                ASR::symbol_t *sym = x.m_symtab->get_symbol(name);
                if (sym && ASR::is_a<ASR::Module_t>(*sym)) {
                    this->visit_symbol(*sym);
                }
            }
            for (auto &a : x.m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::Module_t>(*a.second)) {
                    this->visit_symbol(*a.second);
                }
            }
            current_scope = current_scope_copy;
        }

        void visit_Program(const ASR::Program_t &x) {
            ASR::Program_t& xx = const_cast<ASR::Program_t&>(x);
            SymbolTable* current_scope_copy = current_scope;
            current_scope = x.m_symtab;
            for (auto &a : x.m_symtab->get_scope()) {
                if(!ASR::is_a<ASR::Function_t>(*a.second)){
                    this->visit_symbol(*a.second);
                }
            }
            for (auto &a : x.m_symtab->get_scope()) {
                if(ASR::is_a<ASR::Function_t>(*a.second)){
                    this->visit_symbol(*a.second);
                }
            }
            this->transform_stmts(xx.m_body, xx.n_body);
            current_scope = current_scope_copy;
        }

        void visit_Module(const ASR::Module_t &x) {
            SymbolTable* current_scope_copy = current_scope;
            current_scope = x.m_symtab;
            for (auto &a : x.m_symtab->get_scope()) {
                if(!ASR::is_a<ASR::Function_t>(*a.second)){
                    this->visit_symbol(*a.second);
                }
            }
            for (auto &a : x.m_symtab->get_scope()) {
                if(ASR::is_a<ASR::Function_t>(*a.second)){
                    this->visit_symbol(*a.second);
                }
            }
            current_scope = current_scope_copy;
        }

        void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
            ASR::ASRPassBaseWalkVisitor<EditProcedureCallsVisitor>::visit_SubroutineCall(x);
            visit_Call(x);
        }

        // A defined output procedure is called by its callback, whose call is
        // edited like any other. The binding keeps naming the procedure, so
        // the procedure is kept as well.
        void visit_dtio_binding(const ASR::dtio_binding_t& x) {
            not_to_be_erased.insert(ASRUtils::symbol_get_past_external(x.m_procedure));
        }

        void visit_FunctionCall(const ASR::FunctionCall_t& x) {
            ASR::ASRPassBaseWalkVisitor<EditProcedureCallsVisitor>::visit_FunctionCall(x);
            visit_Call(x);
        }

        void visit_Variable(const ASR::Variable_t &x) {
            if (v.proc2newproc.find((ASR::symbol_t*) &x) != v.proc2newproc.end()){
                return;
            }
            // Case: procedure(cb) :: call_back (Here call_back is variable of type cb which is a function)
            ASR::symbol_t* type_dec = x.m_type_declaration;
            if (v.proc2newproc.find(ASRUtils::symbol_get_past_external(type_dec)) != v.proc2newproc.end() &&
                    v.proc2newproc.find(type_dec) == v.proc2newproc.end()){
                ASR::symbol_t* new_func = resolve_new_proc(ASRUtils::symbol_get_past_external(type_dec));
                ASR::ExternalSymbol_t* x_sym_ext = ASR::down_cast<ASR::ExternalSymbol_t>(type_dec);
                std::string new_func_sym_name = x_sym_ext->m_parent_symtab->get_unique_name(ASRUtils::symbol_name(
                    ASRUtils::symbol_get_past_external(type_dec)));
                ASR::symbol_t* new_func_sym_ = ASR::down_cast<ASR::symbol_t>(
                    ASR::make_ExternalSymbol_t(v.al, type_dec->base.loc, x_sym_ext->m_parent_symtab,
                        s2c(v.al, new_func_sym_name), new_func, x_sym_ext->m_module_name,
                        x_sym_ext->m_scope_names, x_sym_ext->n_scope_names, ASRUtils::symbol_name(new_func),
                        x_sym_ext->m_access));
                v.proc2newproc[type_dec] = {new_func_sym_, {}};
                x_sym_ext->m_parent_symtab->add_symbol(new_func_sym_name, new_func_sym_);
            }
            if(type_dec && v.proc2newproc.find(type_dec) != v.proc2newproc.end() &&
                ASR::is_a<ASR::Function_t>(*ASRUtils::symbol_get_past_external(type_dec))){
                ASR::symbol_t* new_sym = resolve_new_proc(type_dec);
                if ( new_sym == nullptr ) {
                    return;
                }
                ASR::expr_t* sym_val = x.m_symbolic_value;
                ASR::expr_t* m_val = x.m_value;
                ASR::Function_t * subrout = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(new_sym));
                if (x.m_symbolic_value && ASR::is_a<ASR::PointerNullConstant_t>(*x.m_symbolic_value)) {
                    ASR::PointerNullConstant_t* pnc = ASR::down_cast<ASR::PointerNullConstant_t>(x.m_symbolic_value);
                    pnc->m_type = subrout->m_function_signature;
                    sym_val = (ASR::expr_t*) pnc;
                }
                if (x.m_value && ASR::is_a<ASR::PointerNullConstant_t>(*x.m_value)) {
                    ASR::PointerNullConstant_t* pnc = ASR::down_cast<ASR::PointerNullConstant_t>(x.m_value);
                    pnc->m_type = subrout->m_function_signature;
                    m_val = (ASR::expr_t*) pnc;
                }
                std::string new_sym_name = x.m_parent_symtab->get_unique_name(x.m_name);
                ASR::ttype_t* new_var_type = subrout->m_function_signature;
                // A member of a host frame (see nested_vars) refers to a
                // procedure variable of the host
                bool reference = ASRUtils::is_reference(x.m_type);
                ASR::ttype_t* var_type = reference
                    ? ASR::down_cast<ASR::Reference_t>(x.m_type)->m_type : x.m_type;
                if (ASR::is_a<ASR::Pointer_t>(*var_type)) {
                    new_var_type = ASRUtils::TYPE(ASR::make_Pointer_t(v.al, x.base.base.loc, new_var_type));
                }
                if (reference) {
                    new_var_type = ASRUtils::TYPE(ASR::make_Reference_t(v.al,
                        x.base.base.loc, new_var_type));
                }
                ASR::symbol_t* new_func_sym_ = ASR::down_cast<ASR::symbol_t>(
                    ASRUtils::make_Variable_t_util(v.al, x.base.base.loc, x.m_parent_symtab, s2c(v.al, new_sym_name),
                        x.m_dependencies, x.n_dependencies, x.m_intent,
                        sym_val, m_val, x.m_storage, new_var_type,
                        new_sym, x.m_abi, x.m_access, x.m_presence, x.m_value_attr,
                        x.m_target_attr, x.m_contiguous_attr, x.m_bindc_name,
                        x.m_is_volatile, x.m_is_asynchronous, x.m_is_protected, x.m_pass_attr, x.m_self_argument));
                v.proc2newproc[(ASR::symbol_t *) &x] = {new_func_sym_, {}};
                x.m_parent_symtab->add_symbol(new_sym_name, new_func_sym_);
                not_to_be_erased.insert(new_func_sym_);
            }
            ASR::ASRPassBaseWalkVisitor<EditProcedureCallsVisitor>::visit_Variable(x);
        }

        // null() keeps its own copy of the procedure type. The pointer it
        // names is specialised above, so the constant has to follow. The
        // procedure's signature also names the symbols of its bounds. Those
        // symbols belong to the procedure, so a constant in another scope
        // keeps the copy it already has.
        void visit_PointerNullConstant(const ASR::PointerNullConstant_t& x) {
            ASR::ASRPassBaseWalkVisitor<EditProcedureCallsVisitor>::visit_PointerNullConstant(x);
            if( x.m_var_expr == nullptr ) {
                return;
            }
            ASR::ttype_t* var_type = ASRUtils::expr_type(x.m_var_expr);
            if( var_type == nullptr || !ASR::is_a<ASR::FunctionType_t>(
                    *ASRUtils::type_get_past_pointer(var_type)) ) {
                return;
            }
            if( !procedure_type_symbols_in_scope(var_type, current_scope) ) {
                return;
            }
            const_cast<ASR::PointerNullConstant_t&>(x).m_type = var_type;
        }

        void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
            //Case: prob % calfun => temp_calfun (where calfun is procedure variable)
            ASR::ASRPassBaseWalkVisitor<EditProcedureCallsVisitor>::visit_StructInstanceMember(x);
            if (v.proc2newproc.find(ASRUtils::symbol_get_past_external(x.m_m)) != v.proc2newproc.end()) {
                ASR::symbol_t* new_func_sym_ = x.m_m;
                if (ASR::is_a<ASR::ExternalSymbol_t>(*x.m_m)) {
                    ASR::symbol_t* new_func = v.proc2newproc[ASRUtils::symbol_get_past_external(x.m_m)].first;
                    ASR::ExternalSymbol_t* x_sym_ext = ASR::down_cast<ASR::ExternalSymbol_t>(x.m_m);
                    // Use current_scope when the ExternalSymbol's parent
                    // symtab differs (e.g. in duplicated functions where
                    // m_m was not remapped by EditProcedureVisitor).
                    SymbolTable* target_scope = x_sym_ext->m_parent_symtab;
                    if (target_scope->get_counter() != current_scope->get_counter()) {
                        target_scope = current_scope;
                    }
                    std::string new_func_sym_name = target_scope->get_unique_name(ASRUtils::symbol_name(x.m_m));
                    new_func_sym_ = ASR::down_cast<ASR::symbol_t>(
                        ASR::make_ExternalSymbol_t(v.al, x.m_m->base.loc, target_scope,
                            s2c(v.al, new_func_sym_name), new_func, x_sym_ext->m_module_name,
                            x_sym_ext->m_scope_names, x_sym_ext->n_scope_names, ASRUtils::symbol_name(new_func),
                            x_sym_ext->m_access));
                    v.proc2newproc[x.m_m] = {new_func_sym_, {}};
                    target_scope->add_symbol(new_func_sym_name, new_func_sym_);
                } else if (ASR::is_a<ASR::Variable_t>(*x.m_m)) {
                    new_func_sym_ = v.proc2newproc[x.m_m].first;
                }
                ASR::StructInstanceMember_t& sim = const_cast<ASR::StructInstanceMember_t&>(x);
                sim.m_m = new_func_sym_;
                sim.m_type = ASRUtils::symbol_type(new_func_sym_);
                // A member of Reference type designates the variable it
                // refers to
                if (ASRUtils::is_reference(sim.m_type)) {
                    sim.m_type = ASR::down_cast<ASR::Reference_t>(sim.m_type)->m_type;
                }
            }
        }

        void visit_Struct(const ASR::Struct_t &x) {
            //just to update names of changed symbols of Struct
            ASR::ASRPassBaseWalkVisitor<EditProcedureCallsVisitor>::visit_Struct(x);
            ASR::Struct_t& ss = const_cast<ASR::Struct_t&>(x);
            // FINAL procedures are referenced by the type, rather than by
            // an explicit call. Keep their original descriptor interface for
            // finalization; a data-only specialization cannot replace it.
            for (size_t i = 0; i < x.n_member_functions; i++) {
                ASR::symbol_t* final_sym = x.m_symtab->parent->get_symbol(
                    x.m_member_functions[i]);
                if (final_sym) {
                    not_to_be_erased.insert(
                        ASRUtils::symbol_get_past_external(final_sym));
                }
            }
            for (size_t i = 0; i < x.n_members; i++) {
                ASR::symbol_t* old_sym = x.m_symtab->get_symbol(x.m_members[i]);
                if (v.proc2newproc.find(old_sym) != v.proc2newproc.end()) {
                    ss.m_members[i] = ASRUtils::symbol_name(v.proc2newproc[old_sym].first);
                }
            }
        }

        void visit_Var(const ASR::Var_t &x) {
            if (v.proc2newproc.find(x.m_v) != v.proc2newproc.end()){
                ASR::symbol_t* new_sym = v.proc2newproc[x.m_v].first;
                ASR::Var_t& vv = const_cast<ASR::Var_t&>(x);
                vv.m_v = new_sym;
                return;
            }
            if (ASR::is_a<ASR::ExternalSymbol_t>(*x.m_v) &&
                v.proc2newproc.find(ASRUtils::symbol_get_past_external(x.m_v)) != v.proc2newproc.end()) {
                ASR::symbol_t* new_func_sym_ = x.m_v;
                ASR::symbol_t* new_func = v.proc2newproc[ASRUtils::symbol_get_past_external(x.m_v)].first;
                ASR::ExternalSymbol_t* x_sym_ext = ASR::down_cast<ASR::ExternalSymbol_t>(x.m_v);
                std::string new_func_sym_name = x_sym_ext->m_parent_symtab->get_unique_name(ASRUtils::symbol_name(x.m_v));
                new_func_sym_ = ASR::down_cast<ASR::symbol_t>(
                    ASR::make_ExternalSymbol_t(v.al, x.m_v->base.loc, x_sym_ext->m_parent_symtab,
                        s2c(v.al, new_func_sym_name), new_func, x_sym_ext->m_module_name,
                        x_sym_ext->m_scope_names, x_sym_ext->n_scope_names, ASRUtils::symbol_name(new_func),
                        x_sym_ext->m_access));
                v.proc2newproc[x.m_v] = {new_func_sym_, {}};
                x_sym_ext->m_parent_symtab->add_symbol(new_func_sym_name, new_func_sym_);
                ASR::Var_t& vv = const_cast<ASR::Var_t&>(x);
                vv.m_v = new_func_sym_;
            }
        }
};

/*
Since the above visitors have replaced procedure and calls to those procedures
with arrays as arguments, we don't need the original ones anymore. So we remove
them from the ASR. The reason to do this is that some backends like WASM don't
have support for accepting and returning arrays via array descriptors. Therefore,
they cannot generate code for such functions. To avoid backends like WASM from failing
we remove those functions by implementing and calling the following visitor.
*/
class RemoveArrayByDescriptorProceduresVisitor : public PassUtils::PassVisitor<RemoveArrayByDescriptorProceduresVisitor>
{
    private:

        PassArrayByDataProcedureVisitor& v;
        std::set<ASR::symbol_t*>& not_to_be_erased;
        bool skip_removal;

        // With skip_removal set, only the descriptor originals that another
        // compilation unit or a later interactive cell could link to are
        // kept: module procedures and top-level external procedures. A
        // nested/inner procedure and a scope-local procedure cannot be
        // reached from outside. Compiler-generated helpers (intrinsic
        // lowerings such as `_lcompilers_Sum_...`) are top-level, but a later
        // unit regenerates its own copy instead of resolving an earlier one
        // by name, so keeping their descriptor original only inflates the
        // defining cell's IR.
        //
        // Provenance is structural first: a procedure written in the source
        // program carries the location of its defining name in
        // `m_start_name`, while a helper synthesized by an ASR pass or an
        // intrinsic lowering has no source name. A user is free to name a
        // procedure `_lcompilers_...`; it still has a `m_start_name` and its
        // descriptor original is kept, so a later cell can link to it.
        //
        // Callers can switch this mode on without supplying name metadata
        // (the unit tests exercise that path), so a procedure without a name
        // location is only dropped when its name is one the compiler itself
        // generates.
        static bool keep_for_a_later_unit(SymbolTable* scope,
                ASR::symbol_t* sym) {
            if (scope == nullptr || sym == nullptr) {
                return false;
            }
            if (!ASR::is_a<ASR::Function_t>(*sym)) {
                return false;
            }
            ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(sym);
            if (fn->m_start_name == nullptr
                    && ASRUtils::is_compiler_generated_name(fn->m_name)) {
                return false;
            }
            if (ASRUtils::is_tu_scope(scope)) {
                return true;
            }
            ASR::asr_t* owner = scope->asr_owner;
            if (owner != nullptr && ASR::is_a<ASR::symbol_t>(*owner)) {
                ASR::symbol_t* owner_sym = ASR::down_cast<ASR::symbol_t>(owner);
                return ASR::is_a<ASR::Module_t>(*owner_sym);
            }
            return false;
        }

        // Other object files can link only to a public external procedure with a body.
        static bool is_linkable_external_procedure(SymbolTable* scope,
                ASR::symbol_t* sym) {
            if (!ASRUtils::is_tu_scope(scope) || !ASR::is_a<ASR::Function_t>(*sym)) {
                return false;
            }
            ASR::Function_t* fn = ASR::down_cast<ASR::Function_t>(sym);
            if (fn->m_access == ASR::accessType::Private) {
                return false;
            }
            if (ASRUtils::get_FunctionType(fn)->m_deftype
                    != ASR::deftypeType::Implementation) {
                return false;
            }
            // Compiler-made helpers are not part of the link interface.
            return fn->m_start_name != nullptr
                || !ASRUtils::is_compiler_generated_name(fn->m_name);
        }

        // `sym` as reached by walking from a component up to `old_proc`,
        // looked up along the same names in `new_proc`.
        static ASR::symbol_t* corresponding_symbol(ASR::symbol_t* sym,
                ASR::symbol_t* old_proc, ASR::symbol_t* new_proc) {
            std::vector<std::string> path;
            ASR::symbol_t* s = sym;
            while (s != nullptr && s != old_proc) {
                path.push_back(ASRUtils::symbol_name(s));
                s = ASRUtils::get_asr_owner(s);
            }
            if (s != old_proc) {
                return nullptr;
            }
            SymbolTable* scope = ASRUtils::symbol_symtab(new_proc);
            for (size_t i = path.size(); i > 0; i--) {
                if (scope == nullptr) {
                    return nullptr;
                }
                ASR::symbol_t* resolved = scope->get_symbol(path[i - 1]);
                if (resolved == nullptr) {
                    return nullptr;
                }
                if (i == 1) {
                    return resolved;
                }
                scope = ASRUtils::symbol_symtab(resolved);
            }
            return nullptr;
        }

        // A component of a procedure-local derived type is named from the
        // enclosing module, with a scope path through the procedure. The
        // specialised procedure has its own copy of the type. Once the
        // original procedure is removed, that module symbol has to name
        // the copy.
        void retarget_local_type_members(SymbolTable* scope,
                ASR::symbol_t* old_proc, ASR::symbol_t* new_proc) {
            for (SymbolTable* s = scope; s != nullptr; s = s->parent) {
                for (auto& item : s->get_scope()) {
                    if (!ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) {
                        continue;
                    }
                    ASR::ExternalSymbol_t* e = ASR::down_cast<ASR::ExternalSymbol_t>(
                        item.second);
                    if (e->m_external == nullptr) {
                        continue;
                    }
                    ASR::symbol_t* target = ASRUtils::symbol_get_past_external(
                        e->m_external);
                    ASR::symbol_t* owner = ASRUtils::get_asr_owner(target);
                    if (owner == nullptr || !ASR::is_a<ASR::Struct_t>(*owner)) {
                        continue;
                    }
                    ASR::symbol_t* repl = corresponding_symbol(target, old_proc, new_proc);
                    if (repl == nullptr) {
                        continue;
                    }
                    e->m_external = repl;
                    ASRUtils::set_struct_member_owner_path(al, e);
                }
            }
        }

    public:

        RemoveArrayByDescriptorProceduresVisitor(Allocator& al_, PassArrayByDataProcedureVisitor& v_,
            std::set<ASR::symbol_t*>& not_to_be_erased_, bool skip_removal_=false):
            PassVisitor(al_, nullptr), v(v_), not_to_be_erased(not_to_be_erased_),
            skip_removal(skip_removal_) {}

        // Shouldn't be done because allocatable arrays when
        // assigned to array constants work fine in gfortran
        // and hence we will need calling the original function
        // and not the new one. WASM backend should be supporting
        // such cases for the following to be removed.
        template <typename T>
        void visit_Unit(const T& x) {
            T& xx = const_cast<T&>(x);
            current_scope = xx.m_symtab;

            std::vector<std::pair<std::string, ASR::symbol_t*>> to_be_erased;

            for( auto& item: current_scope->get_scope() ) {
                if (ASR::is_a<ASR::Function_t>(*item.second)) {
                    SymbolTable* current_scope_copy = current_scope;
                    visit_symbol(*item.second);
                    current_scope = current_scope_copy;
                }
                ASR::symbol_t* sym = item.second;
                if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) {
                    sym = ASR::down_cast<ASR::ExternalSymbol_t>(item.second)->m_external;
                }
                if( v.proc2newproc.find(sym) != v.proc2newproc.end() ) {
                    LCOMPILERS_ASSERT(item.first == ASRUtils::symbol_name(item.second))
                    to_be_erased.push_back({item.first, sym});
                }
                if ( ASR::is_a<ASR::Module_t>(*item.second) ||
                    ASR::is_a<ASR::Program_t>(*item.second) ||
                    ASR::is_a<ASR::Function_t>(*item.second) ||
                    ASR::is_a<ASR::Struct_t>(*item.second) ||
                    ASR::is_a<ASR::Block_t>(*item.second) ||
                    ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
                    SymbolTable* current_scope_copy = current_scope;
                    visit_symbol(*item.second);
                    current_scope = current_scope_copy;
                }
            }

            {
                for (auto &item: to_be_erased) {
                    if (not_to_be_erased.find(item.second) != not_to_be_erased.end()) {
                        continue;
                    }
                    if ((skip_removal && keep_for_a_later_unit(
                            current_scope, item.second))
                            || is_linkable_external_procedure(
                            current_scope, item.second)) {
                        continue;
                    }
                    // If this function is in a submodule, check if the parent
                    // module's counterpart is protected (referenced by an SMD).
                    // The submodule provides the body for the parent module's
                    // interface function, so it must not be erased either.
                    bool protect = false;
                    ASR::asr_t* owner = current_scope->asr_owner;
                    if (ASR::is_a<ASR::symbol_t>(*owner)) {
                        ASR::symbol_t* owner_sym = ASR::down_cast<ASR::symbol_t>(owner);
                        if (ASR::is_a<ASR::Module_t>(*owner_sym)) {
                            ASR::Module_t* mod = ASR::down_cast<ASR::Module_t>(owner_sym);
                            if (mod->m_parent_module && std::strlen(mod->m_parent_module) > 0) {
                                ASR::symbol_t* parent_mod_sym =
                                    current_scope->parent->resolve_symbol(mod->m_parent_module);
                                if (parent_mod_sym && ASR::is_a<ASR::Module_t>(*parent_mod_sym)) {
                                    ASR::Module_t* parent_mod = ASR::down_cast<ASR::Module_t>(parent_mod_sym);
                                    ASR::symbol_t* parent_func =
                                        parent_mod->m_symtab->resolve_symbol(item.first);
                                    if (parent_func &&
                                            not_to_be_erased.find(parent_func) != not_to_be_erased.end()) {
                                        protect = true;
                                    }
                                    // Also check if any Struct in the parent
                                    // module has an SMD referencing this
                                    // function. When compiling a submodule,
                                    // the Struct is only reachable through
                                    // ExternalSymbols so
                                    // visit_StructMethodDeclaration may not
                                    // have been called.
                                    if (!protect && parent_func) {
                                        for (auto &sym_item : parent_mod->m_symtab->get_scope()) {
                                            if (!ASR::is_a<ASR::Struct_t>(*sym_item.second)) continue;
                                            ASR::Struct_t* st = ASR::down_cast<ASR::Struct_t>(sym_item.second);
                                            for (auto &st_item : st->m_symtab->get_scope()) {
                                                if (!ASR::is_a<ASR::StructMethodDeclaration_t>(*st_item.second)) continue;
                                                ASR::StructMethodDeclaration_t* smd =
                                                    ASR::down_cast<ASR::StructMethodDeclaration_t>(st_item.second);
                                                if (ASRUtils::symbol_get_past_external(smd->m_proc) == parent_func) {
                                                    protect = true;
                                                    break;
                                                }
                                            }
                                            if (protect) break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    if (!protect) {
                        auto it = v.proc2newproc.find(item.second);
                        if (it != v.proc2newproc.end()) {
                            retarget_local_type_members(current_scope,
                                item.second, it->second.first);
                        }
                        current_scope->erase_symbol(item.first);
                    }
                }
            }
        }

        void visit_TranslationUnit(const ASR::TranslationUnit_t& x) {
            // The earlier interactive cells' procedures were specialised
            // again in this cell; drop their descriptor originals the way
            // their own cell did.
            for (SymbolTable* s = x.m_symtab->parent; s != nullptr; s = s->parent) {
                if( ASRUtils::is_tu_scope(s) ) {
                    visit_Unit(*ASR::down_cast2<ASR::TranslationUnit_t>(s->asr_owner));
                }
            }
            visit_Unit(x);
        }

        void visit_Program(const ASR::Program_t& x) {
            visit_Unit(x);
        }

        void visit_Function(const ASR::Function_t& x) {
            visit_Unit(x);
        }

        void visit_Module(const ASR::Module_t& x) {
            if (x.m_intrinsic) {
                return ;
            }

            visit_Unit(x);
        }

        void visit_Struct(const ASR::Struct_t& x) {
            ASR::ASRPassBaseWalkVisitor<RemoveArrayByDescriptorProceduresVisitor>::visit_Struct(x);
            visit_Unit(x);
        }

        void visit_Block(const ASR::Block_t& x) {
            visit_Unit(x);
        }

        void visit_AssociateBlock(const ASR::AssociateBlock_t& x) {
            visit_Unit(x);
        }

        // Note: We intentionally do NOT update m_proc to the ____N version.
        // The vtable is linkonce_odr and must reference the original
        // (descriptor-based) functions so it stays identical across all
        // compilation units. We also mark the original function as
        // not-to-be-erased so it remains available for the vtable.
        void visit_StructMethodDeclaration(const ASR::StructMethodDeclaration_t &x){
            ASR::ASRPassBaseWalkVisitor<RemoveArrayByDescriptorProceduresVisitor>::visit_StructMethodDeclaration(x);
            ASR::symbol_t* proc = ASRUtils::symbol_get_past_external(x.m_proc);
            // Always protect SMD-referenced functions. For submodule
            // procedures, proc points to the parent module's interface
            // (not in proc2newproc), but the submodule's implementation
            // must still be kept so the vtable can reference it.
            not_to_be_erased.insert(proc);
        }
};

/*
    When pass_array_by_data transforms a callback interface (e.g. `cb`) but the
    actual function passed through it (e.g. `f`) cannot be transformed (its body
    contains reshape), the procedure variable's type would be updated to the
    transformed interface while the actual function keeps its original signature.
    This causes an LLVM IR type mismatch at the call site.

    This visitor detects such mismatches after PassArrayByDataProcedureVisitor
    has populated proc2newproc, and removes the offending interface entries
    before EditProcedureVisitor updates type declarations.
 */
class CallbackMismatchCleanup : public ASR::BaseWalkVisitor<CallbackMismatchCleanup> {
public:
    PassArrayByDataProcedureVisitor& v;
    std::set<ASR::symbol_t*> to_remove;
    std::map<ASR::symbol_t*, std::set<ASR::symbol_t*>> iface_to_assigned;

    CallbackMismatchCleanup(PassArrayByDataProcedureVisitor& v_) : v(v_) {}

    void check_callback_args(ASR::symbol_t* callee_name,
                             ASR::call_arg_t* args, size_t n_args) {
        ASR::symbol_t* callee_sym = ASRUtils::symbol_get_past_external(callee_name);
        if (!ASR::is_a<ASR::Function_t>(*callee_sym)) return;
        ASR::Function_t* callee = ASR::down_cast<ASR::Function_t>(callee_sym);

        for (size_t i = 0; i < n_args && i < callee->n_args; i++) {
            if (!args[i].m_value) continue;
            if (!ASR::is_a<ASR::Var_t>(*args[i].m_value)) continue;

            ASR::symbol_t* arg_sym = ASR::down_cast<ASR::Var_t>(
                args[i].m_value)->m_v;
            ASR::symbol_t* arg_resolved = ASRUtils::symbol_get_past_external(
                arg_sym);

            // Is the argument a function being passed as a callback?
            if (!ASR::is_a<ASR::Function_t>(*arg_resolved)) continue;
            // Was this function NOT transformed?
            if (v.proc2newproc.find(arg_resolved) != v.proc2newproc.end()) continue;

            // Get the corresponding parameter's type_declaration (the interface)
            if (!ASR::is_a<ASR::Var_t>(*callee->m_args[i])) continue;
            ASR::symbol_t* param_sym = ASR::down_cast<ASR::Var_t>(
                callee->m_args[i])->m_v;
            if (!ASR::is_a<ASR::Variable_t>(*param_sym)) continue;
            ASR::Variable_t* param = ASR::down_cast<ASR::Variable_t>(param_sym);
            if (!param->m_type_declaration) continue;

            ASR::symbol_t* type_decl = ASRUtils::symbol_get_past_external(
                param->m_type_declaration);

            // If the interface WAS transformed → mismatch
            if (v.proc2newproc.find(type_decl) != v.proc2newproc.end()) {
                to_remove.insert(type_decl);
            }
        }
    }
    static ASR::symbol_t* get_slot_iface(ASR::expr_t* target) {
        ASR::symbol_t* slot_var = nullptr;
        if (ASR::is_a<ASR::StructInstanceMember_t>(*target)) {
            slot_var = ASR::down_cast<ASR::StructInstanceMember_t>(target)->m_m;
        } else if (ASR::is_a<ASR::Var_t>(*target)) {
            slot_var = ASR::down_cast<ASR::Var_t>(target)->m_v;
        } else {
            return nullptr;
        }
        ASR::symbol_t* resolved = ASRUtils::symbol_get_past_external(slot_var);
        if (!resolved || !ASR::is_a<ASR::Variable_t>(*resolved)) return nullptr;
        ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(resolved);
        if (!var->m_type_declaration) return nullptr;
        ASR::symbol_t* iface = ASRUtils::symbol_get_past_external(
            var->m_type_declaration);
        if (!iface || !ASR::is_a<ASR::Function_t>(*iface)) return nullptr;
        return iface;
    }

    static ASR::symbol_t* get_rhs_function(ASR::expr_t* value) {
        if (!value || !ASR::is_a<ASR::Var_t>(*value)) return nullptr;
        ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(value)->m_v;
        ASR::symbol_t* resolved = ASRUtils::symbol_get_past_external(sym);
        if (!resolved || !ASR::is_a<ASR::Function_t>(*resolved)) return nullptr;
        return resolved;
    }

    void record_assignment(ASR::expr_t* target, ASR::expr_t* value) {
        ASR::symbol_t* iface = get_slot_iface(target);
        ASR::symbol_t* fn = get_rhs_function(value);
        if (iface && fn) {
            iface_to_assigned[iface].insert(fn);
        }
    }

    void visit_Associate(const ASR::Associate_t& x) {
        record_assignment(x.m_target, x.m_value);
        BaseWalkVisitor::visit_Associate(x);
    }

    void visit_Variable(const ASR::Variable_t& x) {
        // Capture default initialisation `procedure(iface), pointer :: p => f`.
        if (x.m_type_declaration && x.m_symbolic_value) {
            ASR::symbol_t* iface = ASRUtils::symbol_get_past_external(
                x.m_type_declaration);
            if (iface && ASR::is_a<ASR::Function_t>(*iface)) {
                ASR::symbol_t* fn = get_rhs_function(x.m_symbolic_value);
                if (fn) {
                    iface_to_assigned[iface].insert(fn);
                }
            }
        }
        BaseWalkVisitor::visit_Variable(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t& x) {
        check_callback_args(x.m_name, x.m_args, x.n_args);
        BaseWalkVisitor::visit_SubroutineCall(x);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t& x) {
        check_callback_args(x.m_name, x.m_args, x.n_args);
        BaseWalkVisitor::visit_FunctionCall(x);
    }

    void resolve_pointer_assignment_groups() {
        for (auto& kv : iface_to_assigned) {
            ASR::symbol_t* iface = kv.first;
            bool any_untransformable = false;
            for (ASR::symbol_t* fn : kv.second) {
                if (v.proc2newproc.find(fn) == v.proc2newproc.end()) {
                    any_untransformable = true;
                    break;
                }
            }
            if (!any_untransformable) continue;
            if (v.proc2newproc.find(iface) != v.proc2newproc.end()) {
                to_remove.insert(iface);
            }
            for (ASR::symbol_t* fn : kv.second) {
                if (v.proc2newproc.find(fn) != v.proc2newproc.end()) {
                    to_remove.insert(fn);
                }
            }
        }
    }

    void apply() {
        resolve_pointer_assignment_groups();
        for (ASR::symbol_t* sym : to_remove) {
            ASR::Function_t* new_func = ASR::down_cast<ASR::Function_t>(
                v.proc2newproc[sym].first);
            SymbolTable* scope = new_func->m_symtab->parent;
            scope->erase_symbol(std::string(new_func->m_name));
            v.newprocs.erase(v.proc2newproc[sym].first);
            v.proc2newproc.erase(sym);
        }
    }
};

void pass_array_by_data(Allocator &al, ASR::TranslationUnit_t &unit,
                        const LCompilers::PassOptions& pass_options) {
    PassArrayByDataProcedureVisitor v(al, pass_options);
    {
        DeviceDescriptorDummies d;
        d.collect(unit);
        v.device_descriptor_dummies = std::move(d.kept);
    }
    v.visit_TranslationUnit(unit);
    CallbackMismatchCleanup cleanup(v);
    cleanup.visit_TranslationUnit(unit);
    cleanup.apply();
    EditProcedureVisitor e(v);
    e.visit_TranslationUnit(unit);
    std::set<ASR::symbol_t*> not_to_be_erased;
    EditProcedureCallsVisitor u(al, v, not_to_be_erased, pass_options);
    u.visit_TranslationUnit(unit);
    /*
        RemoveArrayByDescriptorProceduresVisitor removes original (descriptor-
        based) procedures that have been replaced by ____N versions. It keeps
        public external procedures with a body, which other object files link
        to by name, and with skip_removal set (separate-compilation mode) also
        module procedures, which other compilation units may still reference.

        Note: StructMethodDeclaration.m_proc is intentionally NOT updated to
        point to the ____N version.  The vtable uses linkonce_odr linkage and
        must be identical across all compilation units.  Since ____N versions
        are only created when function bodies are available, updating the SMD
        would make the vtable inconsistent between submodules (which have the
        bodies) and other units that merely `use` the module.

        If separate compilation is enabled using `--separate-compilation`, then we don't
        drop the original ( unused ) procedures. This is for the module procedures where when
        loaded from other file transformation may or maynot take place and then while linking
        it shows missing symbol. There can be multiple reasons:

        1. Module procedure is transformed in current file but not in the other file. -- this approach fixes it.
            a. Function `get_midpoints(A, B)` where A(:) and B(:) are arrays, get transformed in current file, but
               in other file, it got called as `print *, get_midpoints(A(1:3), b(1:3))`, LFortran treats these as
               pointers and hence doesn't transform it.
        2. Module procedure is not transformed in current file but is done in other file -- very less prone to happen.
            a. For the functions where reshape is used over Function arguments, it is not transformed in current file but
               in other file it is transformed as these routines are marked as external ( meaning n_body = 0 ).
    */
    RemoveArrayByDescriptorProceduresVisitor x(al, v, not_to_be_erased,
        pass_options.skip_removal_of_unused_procedures_in_pass_array_by_data);
    x.visit_TranslationUnit(unit);
    u.forward_copies_with_saved_state();
    // Keep exported descriptor signatures and vtable identities unchanged.
    // The usual call conversion and later temporary pass provide the same
    // runtime contiguity checks/copy-in as at an ordinary data-ABI call site.
    if (pass_options.supports_descriptor_entry_wrappers) {
        u.wrap_descriptor_entries();
    }
    PassUtils::UpdateDependenciesVisitor y(al);
    y.visit_TranslationUnit(unit);
}

} // namespace LCompilers

#ifndef LFORTRAN_ASR_UTILS_H
#define LFORTRAN_ASR_UTILS_H

#include <functional>
#include <map>
#include <set>
#include <limits>
#include <optional>
#include <vector>
#include <utility>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <new>

#include <libasr/assert.h>
#include <libasr/asr.h>
#include <libasr/string_utils.h>
#include <libasr/utils.h>
#include <libasr/casting_utils.h>
#include <libasr/checked_cast.h>
#include <libasr/asr_expr_stmt_duplicator_visitor.h>
#include <libasr/asr_expr_base_replacer_visitor.h>
#include <libasr/asr_stmt_base_replacer_visitor.h>
#include <libasr/asr_expr_call_replacer_visitor.h>
#include <libasr/asr_expr_type_visitor.h>
#include <libasr/asr_expr_value_visitor.h>
#include <libasr/asr_walk_visitor.h>
#include <libasr/runtime/lfortran_float128_quadmath.h>

#include <complex>
#include <string>

#define ADD_ASR_DEPENDENCIES(current_scope, final_sym, current_function_dependencies) ASR::symbol_t* asr_owner_sym = nullptr; \
    if(current_scope->asr_owner && ASR::is_a<ASR::symbol_t>(*current_scope->asr_owner) ) { \
        asr_owner_sym = ASR::down_cast<ASR::symbol_t>(current_scope->asr_owner); \
    } \
    SymbolTable* temp_scope = current_scope; \
    if (asr_owner_sym && !ASR::is_a<ASR::ExternalSymbol_t>(*final_sym) && !ASR::is_a<ASR::Variable_t>(*final_sym)) { \
        while (temp_scope->parent && temp_scope->asr_owner && ASR::is_a<ASR::symbol_t>(*temp_scope->asr_owner)) { \
            ASR::symbol_t* temp_owner_sym = ASR::down_cast<ASR::symbol_t>(temp_scope->asr_owner); \
            if (!ASR::is_a<ASR::AssociateBlock_t>(*temp_owner_sym) && !ASR::is_a<ASR::Block_t>(*temp_owner_sym)) { \
                break; \
            } \
            temp_scope = temp_scope->parent; \
        } \
        if (!ASRUtils::is_scope_between(ASRUtils::symbol_parent_symtab(final_sym), current_scope, temp_scope)) { \
            current_function_dependencies.push_back(al, ASRUtils::symbol_name(final_sym)); \
        } \
    } \

#define ADD_ASR_DEPENDENCIES_WITH_NAME(current_scope, final_sym, current_function_dependencies, dep_name) ASR::symbol_t* asr_owner_sym = nullptr; \
    if(current_scope->asr_owner && ASR::is_a<ASR::symbol_t>(*current_scope->asr_owner) ) { \
        asr_owner_sym = ASR::down_cast<ASR::symbol_t>(current_scope->asr_owner); \
    } \
    SymbolTable* temp_scope = current_scope; \
    if (asr_owner_sym && !ASR::is_a<ASR::ExternalSymbol_t>(*final_sym) && !ASR::is_a<ASR::Variable_t>(*final_sym)) { \
        while (temp_scope->parent && temp_scope->asr_owner && ASR::is_a<ASR::symbol_t>(*temp_scope->asr_owner)) { \
            ASR::symbol_t* temp_owner_sym = ASR::down_cast<ASR::symbol_t>(temp_scope->asr_owner); \
            if (!ASR::is_a<ASR::AssociateBlock_t>(*temp_owner_sym) && !ASR::is_a<ASR::Block_t>(*temp_owner_sym)) { \
                break; \
            } \
            temp_scope = temp_scope->parent; \
        } \
        if (!ASRUtils::is_scope_between(ASRUtils::symbol_parent_symtab(final_sym), current_scope, temp_scope)) { \
            current_function_dependencies.push_back(al, dep_name); \
        } \
    } \

namespace LCompilers  {

    namespace ASRUtils  {

// A specific procedure that shares its generic interface's name is stored in
// the symbol table under "<name>~genericprocedure" to avoid clashing with the
// GenericProcedure symbol. This is the disambiguating suffix; the real
// external (link) name is "<name>". Semantics appends it; codegen strips it.
inline const std::string genericprocedure_suffix = "~genericprocedure";

// Recover the original name from a possibly-disambiguated symbol name by
// removing the genericprocedure_suffix if present.
inline std::string strip_genericprocedure_suffix(const std::string &sym_name) {
    if (sym_name.size() > genericprocedure_suffix.size() &&
        sym_name.compare(sym_name.size() - genericprocedure_suffix.size(),
                         genericprocedure_suffix.size(),
                         genericprocedure_suffix) == 0) {
        return sym_name.substr(0, sym_name.size() - genericprocedure_suffix.size());
    }
    return sym_name;
}

ASR::symbol_t* import_class_procedure(Allocator &al, const Location& loc,
        ASR::symbol_t* original_sym, SymbolTable *current_scope);

ASR::asr_t* make_Binop_util(Allocator &al, const Location& loc, ASR::binopType binop,
                        ASR::expr_t* lexpr, ASR::expr_t* rexpr, ASR::ttype_t* ttype);

ASR::asr_t* make_Cmpop_util(Allocator &al, const Location& loc, ASR::cmpopType cmpop,
                        ASR::expr_t* lexpr, ASR::expr_t* rexpr, ASR::ttype_t* ttype);

bool check_equal_type(ASR::ttype_t* x, ASR::ttype_t* y, ASR::expr_t* x_expr, ASR::expr_t* y_expr, bool check_for_dimensions=false);
bool types_equal(ASR::ttype_t *a, ASR::ttype_t *b, ASR::expr_t* a_expr, ASR::expr_t* b_expr, bool check_for_dimensions=false);

static inline std::string type_to_str_python_expr(const ASR::ttype_t *t, ASR::expr_t* expr);

std::string type_to_str_python_symbol(const ASR::ttype_t *t, ASR::symbol_t* sym);

const ASR::Function_t* get_function_from_expr(ASR::expr_t* expr);

ASR::symbol_t* get_struct_sym_from_struct_expr(ASR::expr_t* expression);
void set_struct_sym_to_struct_expr(ASR::expr_t* expression, ASR::symbol_t* struct_sym);
ASR::cptr_kindType get_cptr_kind_from_type(ASR::ttype_t* type);
ASR::symbol_t* get_cptr_type_declaration_from_expr(ASR::expr_t* expression);
ASR::symbol_t* get_iso_c_binding_symbol(SymbolTable* scope,
        const std::string &original_name);
bool is_iso_c_binding_symbol(SymbolTable* scope, ASR::symbol_t* sym,
        const std::string &original_name);
bool is_iso_c_null_symbol(SymbolTable* scope, ASR::symbol_t* sym);
bool is_iso_c_ptr_type_symbol(SymbolTable* scope, ASR::symbol_t* sym);
bool is_iso_c_funptr_type_symbol(SymbolTable* scope, ASR::symbol_t* sym);
bool is_iso_fortran_env_team_type_symbol(SymbolTable* scope, ASR::symbol_t* sym);
ASR::cptr_kindType cptr_kind_from_type_declaration(SymbolTable* scope,
        ASR::symbol_t* type_decl);
ASR::ttype_t* make_cptr_type(Allocator& al, const Location& loc,
        SymbolTable* scope, ASR::symbol_t* type_decl);
ASR::expr_t* make_iso_c_null_constant(Allocator& al, const Location& loc,
        SymbolTable* scope, ASR::symbol_t* null_sym);
void anchor_cptr_null_constant_to_type_declaration(Allocator& al,
        ASR::expr_t* expr, ASR::Variable_t* var);
ASR::Variable_t* get_variable_from_symbol(ASR::symbol_t* sym);
ASR::FunctionType_t* get_procedure_type_from_expr(ASR::expr_t* expr);
ASR::FunctionType_t* get_procedure_pointer_component_type(
        ASR::Variable_t* member_var);
bool is_procedure_pointer_value(ASR::Variable_t* member_var,
        ASR::expr_t* value);
bool is_valid_pointer_assignment_target(ASR::expr_t* expr);
void set_null_context_from_variable(Allocator& al, const Location& loc,
        SymbolTable* scope, ASR::Variable_t* var,
        ASR::ttype_t*& current_variable_type,
        ASR::expr_t*& current_struct_type_var_expr);

// Resolve type-bound assignment(=) (~assign) on a struct or its ancestors.
// Returns the CustomOperator symbol, or nullptr if none is found.
ASR::symbol_t* resolve_struct_assign_symbol(ASR::Struct_t* s);
ASR::symbol_t* resolve_struct_assign_symbol(ASR::expr_t* expression);

// The procedure that an assignment of `s` to `s` is defined by, or nullptr
// when there is none. `resolve_struct_assign_symbol` alone is not enough: it
// answers with any `~assign` visible from the type, whose procedures may all
// take other types, so the procedures are matched against `s` here.
ASR::symbol_t* resolve_struct_defined_assignment_proc(ASR::Struct_t* s);

// An intrinsic assignment whose variable is of derived type does more than
// copy the components across. F2018 7.5.6.3 p1: the variable is finalized
// after the expression is evaluated and before the variable is defined.
// F2018 10.2.1.3 p13: every nonpointer component of derived type that has a
// type-bound defined assignment is assigned through that defined assignment.
// Both belong to the assignment and are carried out when the value is copied
// into the variable, so this reports whether assigning to `struct_sym` does
// either, in which case the value has to exist before the variable is
// written: built straight into the variable it would skip them.
// Only a component declared by the type itself is looked at. A component the
// type inherits is written through the parent component, which is a component
// of derived type in its own right (F2018 7.5.7.2) and is copied as one.
bool struct_assignment_is_more_than_a_copy(ASR::symbol_t* struct_sym);

// Whether finalizing an entity of the derived type `struct_sym` does anything
// (F2018 7.5.6.2): the type or a type it extends has a final subroutine, or
// a nonpointer, nonallocatable, nonpolymorphic scalar component (inherited
// ones included) is of a type for which this holds.
bool struct_needs_finalization(ASR::symbol_t* struct_sym);

// Whether a function result of type `type`, declared by `struct_sym`, is one
// that F2018 7.5.6.3 p5 finalizes after the statement that references the
// function: a nonpointer, nonallocatable, nonpolymorphic scalar of a derived
// type whose finalization does anything. Such a result is not finalized when
// the function is invoked, as an intent(out) dummy argument would be.
bool is_finalizable_function_result(ASR::ttype_t* type,
        ASR::symbol_t* struct_sym);

// Whether `expr` is a function reference whose result is one of those.
bool is_finalizable_function_reference(ASR::expr_t* expr);

// Whether `expr` or one of its subexpressions is such a reference.
bool contains_finalizable_function_reference(ASR::expr_t* expr);

// Whether `expr` or one of its subexpressions is a function reference.
bool contains_function_reference(ASR::expr_t* expr);

// Whether `x` contains no other statement and does not transfer control
// elsewhere, so that it can be made the body of a BLOCK that is left only
// by completing it (or by terminating the program).
bool is_single_statement(const ASR::stmt_t &x);

ASR::symbol_t* get_union_sym_from_union_expr(ASR::expr_t* expression);
static inline bool is_unlimited_polymorphic_type(ASR::Struct_t* st);
static inline bool is_unlimited_polymorphic_type(ASR::ttype_t* const t);

static inline std::string extract_real(const char *s) {
    // TODO: this is inefficient. We should
    // convert this in the tokenizer where we know most information
    std::string x = s;
    x = replace(x, "d", "e");
    x = replace(x, "D", "E");
    // Fortran quad-precision exponent letter (e.g. 1.5q10 == 1.5e10 in real(16))
    x = replace(x, "q", "e");
    x = replace(x, "Q", "E");
    return x;
}

static inline std::string extract_real_16_str(const char *s) {
    std::string r = extract_real(s);         // normalise d/D/q/Q → e/E
    auto pos = r.rfind('_');
    if (pos != std::string::npos) {
        r = r.substr(0, pos);
    }
    return r;
}

// --- real(16) value access ---------------------------------------------------
//
// The `RealConstant_t.m_r` field is a 64-bit payload whose meaning depends on
// the constant's kind:
//   kind=4,8:  m_r is the IEEE-754 double approximation / exact value.
//   kind=16:   m_r is bit-cast from a `uint8_t*` pointing at 16 arena-allocated
//              bytes that hold the binary128 representation. Reading m_r as a
//              `double` for kind=16 is undefined behaviour.
//
// All access to m_r MUST go through these kind-aware helpers (and through
// `extract_value<double>`, which refuses for kind=16). Constructing a kind=16
// constant from a decimal string is `make_RealConstant_r16_from_str`.

// Recover the pointer to the 16 binary128 bytes from an m_r payload. Inverse of
// `real_constant_pack_r16`. Only valid for a kind=16 payload.
static inline const uint8_t* real_constant_unpack_r16(double m_r) {
    uintptr_t addr;
    std::memcpy(&addr, &m_r, sizeof(addr));
    return reinterpret_cast<const uint8_t*>(addr);
}

// Read the 16 raw bytes of a kind=16 RealConstant (binary128, little-endian).
// Asserts that the constant is kind=16. Lifetime: tied to the ASR allocator
// that built the node.
static inline const uint8_t* real_constant_get_r16_bytes(
        const ASR::RealConstant_t* c) {
    return real_constant_unpack_r16(c->m_r);
}

// Pack 16 bytes into the m_r payload (compose the pointer-encoded double).
// Used by the literal handler and by f_t when constructing kind=16 constants.
static inline double real_constant_pack_r16(const uint8_t* bytes) {
    uintptr_t addr = reinterpret_cast<uintptr_t>(bytes);
    double m_r;
    std::memcpy(&m_r, &addr, sizeof(addr));
    return m_r;
}

// Read the binary128 value of a kind=16 RealConstant.
static inline lf_float128 real_constant_get_r16(const ASR::RealConstant_t* c) {
    lf_float128 v;
    std::memcpy(v.bytes, real_constant_get_r16_bytes(c), 16);
    return v;
}

// Build a kind=16 RealConstant from a binary128 value: allocate 16 bytes on the
// ASR arena, copy the bytes in, and pack the pointer into m_r. This is the only
// supported way to materialise a computed (non-literal) kind=16 constant.
static inline ASR::expr_t* make_RealConstant_r16(Allocator& al,
        const Location& loc, lf_float128 v, ASR::ttype_t* type) {
    uint8_t* bytes = static_cast<uint8_t*>(al.alloc(16));
    std::memcpy(bytes, v.bytes, 16);
    return ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(
        al, loc, real_constant_pack_r16(bytes), type));
}

// --- real(10) value access ---------------------------------------------------
static inline const uint8_t* real_constant_unpack_r10(double m_r) {
    uintptr_t addr;
    std::memcpy(&addr, &m_r, sizeof(addr));
    return reinterpret_cast<const uint8_t*>(addr);
}

static inline const uint8_t* real_constant_get_r10_bytes(
        const ASR::RealConstant_t* c) {
    return real_constant_unpack_r10(c->m_r);
}

// Read the value of a kind=10 RealConstant as a host long double.
static inline long double real_constant_get_r10(const ASR::RealConstant_t* c) {
    long double v;
    std::memcpy(&v, real_constant_get_r10_bytes(c), sizeof(long double));
    return v;
}

static inline double real_constant_pack_r10(const uint8_t* bytes) {
    uintptr_t addr = reinterpret_cast<uintptr_t>(bytes);
    double m_r;
    std::memcpy(&m_r, &addr, sizeof(addr));
    return m_r;
}

static inline ASR::expr_t* make_RealConstant_r10(Allocator& al,
        const Location& loc, long double v, ASR::ttype_t* type) {
    uint8_t* bytes = static_cast<uint8_t*>(al.alloc(sizeof(long double)));
    std::memcpy(bytes, &v, sizeof(long double));
    return ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(
        al, loc, real_constant_pack_r10(bytes), type));
}

// ----------------------------------------------------------------------------

static inline double extract_real_4(const char *s) {
    std::string r_str = ASRUtils::extract_real(s);
    float r = std::strtof(r_str.c_str(), nullptr);
    return r;
}

static inline double extract_real_8(const char *s) {
    std::string r_str = ASRUtils::extract_real(s);
    return std::strtod(r_str.c_str(), nullptr);
}

static inline ASR::expr_t* EXPR(const ASR::asr_t *f)
{
    return ASR::down_cast<ASR::expr_t>(f);
}

static inline ASR::stmt_t* STMT(const ASR::asr_t *f)
{
    return ASR::down_cast<ASR::stmt_t>(f);
}

static inline ASR::case_stmt_t* CASE_STMT(const ASR::asr_t *f)
{
    return ASR::down_cast<ASR::case_stmt_t>(f);
}

static inline ASR::ttype_t* TYPE(const ASR::asr_t *f)
{
    return ASR::down_cast<ASR::ttype_t>(f);
}

static inline ASR::FunctionType_t* get_FunctionType(const ASR::Function_t* x) {
    return ASR::down_cast<ASR::FunctionType_t>(x->m_function_signature);
}

static inline ASR::FunctionType_t* get_FunctionType(const ASR::Function_t& x) {
    return ASR::down_cast<ASR::FunctionType_t>(x.m_function_signature);
}

// The execution space a function runs in. Host runs on the CPU only, Device
// on the GPU only, HostDevice is compiled for both, and Kernel is the entry
// point of a GPU kernel: it runs on the device and the host launches it.
static inline ASR::exec_spaceType get_exec_space(const ASR::Function_t& x) {
    return ASRUtils::get_FunctionType(x)->m_exec_space;
}

// True when the function is compiled for the device, whether as a kernel, as
// a routine the device call graph reaches, or as one compiled for both. The
// device-preparation passes and the device code generators ask this.
bool runs_on_device(const ASR::Function_t& x);

static inline bool runs_on_device(const ASR::symbol_t* s) {
    if (s == nullptr || !ASR::is_a<ASR::Function_t>(*s)) {
        return false;
    }
    return ASRUtils::runs_on_device(*ASR::down_cast<ASR::Function_t>(s));
}

// True when the function exists on the device only, so a host code generator
// has nothing to emit for it.
static inline bool is_device_only_function(const ASR::Function_t& x) {
    ASR::exec_spaceType space = ASRUtils::get_exec_space(x);
    return space == ASR::exec_spaceType::Device ||
           space == ASR::exec_spaceType::Kernel;
}

// True when `s` is the entry point of a GPU kernel: the host launches it with
// a GpuKernelLaunch, and the device code generators give it the kernel
// qualifier, its buffer bindings and its registration.
static inline bool is_device_kernel(const ASR::symbol_t* s) {
    if (s == nullptr || !ASR::is_a<ASR::Function_t>(*s)) {
        return false;
    }
    return ASRUtils::get_exec_space(*ASR::down_cast<ASR::Function_t>(s))
        == ASR::exec_spaceType::Kernel;
}
class ExprStmtDuplicator: public ASR::BaseExprStmtDuplicator<ExprStmtDuplicator>
{
    public:

    ExprStmtDuplicator(Allocator &al): BaseExprStmtDuplicator(al) {}

};

static inline ASR::symbol_t *symbol_get_past_external(ASR::symbol_t *f)
{
    if (f && f->type == ASR::symbolType::ExternalSymbol) {
        ASR::ExternalSymbol_t *e = ASR::down_cast<ASR::ExternalSymbol_t>(f);
        if( e->m_external == nullptr ) {
            return nullptr;
        }
        LCOMPILERS_ASSERT(!ASR::is_a<ASR::ExternalSymbol_t>(*e->m_external));
        return e->m_external;
    } else {
        return f;
    }
}

// A walk visitor that also walks the body of a BLOCK or ASSOCIATE construct.
//
// `BlockCall` and `AssociateBlockCall` name their construct through a symbol,
// and the generated `BaseWalkVisitor` does not follow a symbol out of a
// statement, so a plain walk stops at the construct and never sees the
// statements inside it. A visitor that has to see the whole executable part
// of a scope -- most collectors and most checkers do -- says so by deriving
// from this instead, rather than by carrying its own copy of the descent.
template <class StructType>
class BlockBodyWalkVisitor : public ASR::BaseWalkVisitor<StructType> {
public:

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (s == nullptr || !ASR::is_a<ASR::Block_t>(*s)) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(s);
        for (size_t i = 0; i < block->n_body; i++) {
            this->visit_stmt(*block->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (s == nullptr || !ASR::is_a<ASR::AssociateBlock_t>(*s)) return;
        ASR::AssociateBlock_t *block =
            ASR::down_cast<ASR::AssociateBlock_t>(s);
        for (size_t i = 0; i < block->n_body; i++) {
            this->visit_stmt(*block->m_body[i]);
        }
    }

};

static inline ASR::symbol_t* symbol_get_past_StructMethodDeclaration(ASR::symbol_t* f){
    LCOMPILERS_ASSERT(f != nullptr);
    if(ASR::is_a<ASR::StructMethodDeclaration_t>(*f)){
        ASR::symbol_t* func = ASR::down_cast<ASR::StructMethodDeclaration_t>(f)->m_proc;
        LCOMPILERS_ASSERT(func != nullptr);
        return symbol_get_past_external(func);
    }
    return f;
}

/*
Returns true, when the symbol 'f' is a procedure variable
*/
static inline bool is_symbol_procedure_variable(ASR::symbol_t* f) {
    if (ASR::is_a<ASR::Variable_t>(*f)) {
        ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(f);
        ASR::ttype_t* t = v->m_type;
        if (ASR::is_a<ASR::Pointer_t>(*t)) {
            t = ASR::down_cast<ASR::Pointer_t>(t)->m_type;
        }
        return ASR::is_a<ASR::FunctionType_t>(*t);
    }
    return false;
}

static inline bool is_symbol_procedure_variable(const ASR::symbol_t* f) {
    if (ASR::is_a<ASR::Variable_t>(*f)) {
        ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(f);
        ASR::ttype_t* t = v->m_type;
        if (ASR::is_a<ASR::Pointer_t>(*t)) {
            t = ASR::down_cast<ASR::Pointer_t>(t)->m_type;
        }
        return ASR::is_a<ASR::FunctionType_t>(*t);
    }
    return false;
}

template <typename T>
Location get_vec_loc(const Vec<T>& args) {
    LCOMPILERS_ASSERT(args.size() > 0);
    Location args_loc;
    args_loc.first = args[0].loc.first;
    args_loc.last = args[args.size() - 1].loc.last;
    return args_loc;
}

ASR::FunctionType_t* get_FunctionType(ASR::symbol_t* x);

ASR::Function_t* get_function(ASR::symbol_t* x);

static inline const ASR::symbol_t *symbol_get_past_external(const ASR::symbol_t *f)
{
    if (f->type == ASR::symbolType::ExternalSymbol) {
        ASR::ExternalSymbol_t *e = ASR::down_cast<ASR::ExternalSymbol_t>(f);
        LCOMPILERS_ASSERT(!ASR::is_a<ASR::ExternalSymbol_t>(*e->m_external));
        return e->m_external;
    } else {
        return f;
    }
}

static inline ASR::ttype_t *type_get_past_pointer(ASR::ttype_t *f)
{
    LCOMPILERS_ASSERT(f != nullptr);
    if (ASR::is_a<ASR::Pointer_t>(*f)) {
        ASR::Pointer_t *e = ASR::down_cast<ASR::Pointer_t>(f);
        LCOMPILERS_ASSERT(!ASR::is_a<ASR::Pointer_t>(*e->m_type));
        return e->m_type;
    } else {
        return f;
    }
}

static inline ASR::ttype_t *type_get_past_allocatable(ASR::ttype_t *f)
{
    if (ASR::is_a<ASR::Allocatable_t>(*f)) {
        ASR::Allocatable_t *e = ASR::down_cast<ASR::Allocatable_t>(f);
        return type_get_past_allocatable(e->m_type);
    } else {
        return f;
    }
}

static inline ASR::expr_t* get_past_array_physical_cast(ASR::expr_t* x) {
    if( !ASR::is_a<ASR::ArrayPhysicalCast_t>(*x) ) {
        return x;
    }
    return get_past_array_physical_cast(ASR::down_cast<ASR::ArrayPhysicalCast_t>(x)->m_arg);
}

static inline ASR::expr_t* get_past_array_broadcast(ASR::expr_t* x) {
    if( !ASR::is_a<ASR::ArrayBroadcast_t>(*x) ) {
        return x;
    }
    return ASR::down_cast<ASR::ArrayBroadcast_t>(x)->m_array;
}

static inline ASR::ttype_t *type_get_past_array(ASR::ttype_t *f)
{
    if (ASR::is_a<ASR::Array_t>(*f)) {
        ASR::Array_t *e = ASR::down_cast<ASR::Array_t>(f);
        LCOMPILERS_ASSERT(!ASR::is_a<ASR::Array_t>(*e->m_type));
        return e->m_type;
    } else {
        return f;
    }
}

static inline ASR::ttype_t* extract_type(ASR::ttype_t *f) {
    return type_get_past_array(
            type_get_past_allocatable(
                type_get_past_pointer(f)));
}

static inline ASR::ttype_t* type_get_past_allocatable_pointer(ASR::ttype_t* f) {
    return type_get_past_allocatable(
        type_get_past_pointer(f)
    );
}

int extract_kind_from_ttype_t(const ASR::ttype_t* type);

// Kind-aware RealConstant constructor from a `double`. For kind 4/8 the value
// is stored directly; for kind=16 the double is widened to binary128 and the
// pointer-encoded payload is produced via make_RealConstant_r16. This is the
// single supported way to build a real constant from a host double when the
// kind may be 16 (e.g. intrinsic-generated literals such as 0.0, 1.0, pi).
static inline ASR::expr_t* make_RealConstant_util(Allocator& al,
        const Location& loc, double value, ASR::ttype_t* type) {
    int k = extract_kind_from_ttype_t(type);
    if (k == 16) {
        return make_RealConstant_r16(al, loc, lf_f128_from_double(value), type);
    } else if (k == 10) {
        return make_RealConstant_r10(al, loc, (long double)value, type);
    }
    return ASR::down_cast<ASR::expr_t>(ASR::make_RealConstant_t(al, loc, value, type));
}

void set_kind_to_ttype_t(ASR::ttype_t* type, int kind);

ASR::Variable_t* expr_to_variable_or_null(ASR::expr_t* expr);

static inline ASR::Variable_t* EXPR2VAR(const ASR::expr_t *f)
{
    return ASR::down_cast<ASR::Variable_t>(symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(f)->m_v));
}

static inline ASR::Function_t* EXPR2FUN(const ASR::expr_t *f)
{
    return ASR::down_cast<ASR::Function_t>(symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(f)->m_v));
}

static inline ASR::ttype_t* expr_type(const ASR::expr_t *f)
{
    return ASR::expr_type0(f);
}

static inline ASR::ttype_t* subs_expr_type(std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>> subs,
        const ASR::expr_t *expr) {
    ASR::ttype_t *ttype = ASRUtils::expr_type(expr);
    if (ASR::is_a<ASR::TypeParameter_t>(*ttype)) {
        ASR::TypeParameter_t *tparam = ASR::down_cast<ASR::TypeParameter_t>(ttype);
        ttype = subs[tparam->m_param].first;
    }
    return ttype;
}

ASR::ttype_t* symbol_type(const ASR::symbol_t *f);

ASR::expr_t* get_expr_from_sym(Allocator& al, ASR::symbol_t* sym);

static inline bool is_class_type(ASR::ttype_t* type) {
    if (ASR::is_a<ASR::StructType_t>(*type)) {
        ASR::StructType_t* struct_type = ASR::down_cast<ASR::StructType_t>(type);
        return !struct_type->m_is_cstruct;
    }
    return false;
}

std::string symbol_type_name(const ASR::symbol_t &s);

ASR::abiType symbol_abi(const ASR::symbol_t *f);

std::string type_to_str_fortran_expr(const ASR::ttype_t *t, ASR::expr_t* expr);

std::string type_to_str_fortran_symbol(const ASR::ttype_t *t, ASR::symbol_t* sym, bool show_kind = false);

char *symbol_name(const ASR::symbol_t *f);
// Includes contained scopes: whole-routine restrictions must also respect
// regions granting asynchronous/volatile access to the canonical entity.
bool has_scoped_variable_attribute(const ASR::Variable_t &variable,
    const SymbolTable *scope);

std::string symbol_to_str_fortran(const ASR::symbol_t &s, bool add_symbol_access_specifiers);

ASR::ttype_t* get_contained_type(ASR::ttype_t* asr_type, int overload=0);

ASR::array_physical_typeType extract_physical_type(ASR::ttype_t* e);

ASR::abiType expr_abi(ASR::expr_t* e);

char *symbol_name(const ASR::symbol_t *f);

bool get_class_proc_nopass_val(ASR::symbol_t* func_sym);

// Returns the index of the pass argument in the function's formal parameter
// list. Returns 0 when pass_arg is unspecified (default pass = first arg).
size_t get_pass_arg_index(ASR::symbol_t* a_name);

void encode_dimensions(size_t n_dims, std::string& res,
    bool use_underscore_sep=false);

std::string type_to_str_fortran_symbol(const ASR::ttype_t* t,
                                                     ASR::symbol_t* struct_sym,
                                                     bool show_kind);

static inline std::string intrinsic_type_to_str_with_kind(const ASR::ttype_t* t, int kind) {
    return type_to_str_fortran_symbol(t, nullptr) + "_" + std::to_string(kind);
}

std::string type_to_str_fortran_expr(const ASR::ttype_t* t, ASR::expr_t* expr);

// Returns type string with kind specifier for error messages (e.g., "integer(4)", "real(8)")
// Use this ONLY in error message contexts where kind differences need to be shown.
// For symbol names, variable names, and ASR dumps, use type_to_str_fortran_symbol instead.
std::string type_to_str_with_kind(const ASR::ttype_t* t, ASR::expr_t* expr);

std::string type_to_str_with_substitution(ASR::expr_t* expr, const ASR::ttype_t *t,
    std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>> subs);

static inline std::string binop_to_str(const ASR::binopType t) {
    switch (t) {
        case (ASR::binopType::Add): { return " + "; }
        case (ASR::binopType::Sub): { return " - "; }
        case (ASR::binopType::Mul): { return "*"; }
        case (ASR::binopType::Div): { return "/"; }
        default : throw LCompilersException("Cannot represent the binary operator as a string");
    }
}

std::string cmpop_to_str(const ASR::cmpopType t);

static inline std::string logicalbinop_to_str_python(const ASR::logicalbinopType t) {
    switch (t) {
        case (ASR::logicalbinopType::And): { return " && "; }
        case (ASR::logicalbinopType::Or): { return " || "; }
        case (ASR::logicalbinopType::Eqv): { return " == "; }
        case (ASR::logicalbinopType::NEqv): { return " != "; }
        default : throw LCompilersException("Cannot represent the boolean operator as a string");
    }
}

static inline ASR::expr_t* expr_value(ASR::expr_t *f)
{
    return ASR::expr_value0(f);
}

std::pair<char**, size_t> symbol_dependencies(const ASR::symbol_t *f);

static inline bool is_present_in_current_scope(ASR::ExternalSymbol_t* external_symbol, SymbolTable* current_scope) {
        SymbolTable* scope = external_symbol->m_parent_symtab;
        while (scope != nullptr) {
            if (scope->get_counter() == current_scope->get_counter()) {
                return true;
            }
            scope = scope->parent;
        }
        return false;
    }

// Whether `scope` is `inner` or one of the scopes that enclose it, up to
// `outer`. Between a procedure (`outer`) and a scope of its body (`inner`) are
// its BLOCK and ASSOCIATE scopes: a procedure declared in one of them is not a
// dependency of the procedure.
static inline bool is_scope_between(SymbolTable *scope, SymbolTable *inner,
        SymbolTable *outer) {
    for (SymbolTable *s = inner; s != nullptr; s = s->parent) {
        if (s->get_counter() == scope->get_counter()) {
            return true;
        }
        if (s == outer) {
            break;
        }
    }
    return false;
}

SymbolTable *symbol_parent_symtab(const ASR::symbol_t *f);

// The scope of the procedure, program, module, ... that `scope` belongs to:
// `scope` itself, unless it is the scope of a BLOCK or ASSOCIATE construct,
// which belongs to the procedure, program, ... it is nested in.
SymbolTable *scope_past_constructs(SymbolTable *scope);

// Returns the `symbol`'s symtab, or nullptr if the symbol has no symtab
SymbolTable *symbol_symtab(const ASR::symbol_t *f);

// Calls `f` with each symbol of `scope` and, recursively, of the scopes of
// the BLOCK and ASSOCIATE constructs in it. The procedures declared by an
// interface block in a BLOCK construct are in the scope of the BLOCK, so a
// pass that transforms the procedures declared in a procedure or program
// has to visit these scopes too.
template <typename F>
static inline void for_each_symbol_in_scope_and_constructs(SymbolTable *scope, const F &f)
{
    for (auto &item : scope->get_scope()) {
        f(item.second);
        if (ASR::is_a<ASR::Block_t>(*item.second) ||
                ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
            for_each_symbol_in_scope_and_constructs(symbol_symtab(item.second), f);
        }
    }
}

static inline ASR::symbol_t *get_asr_owner(const ASR::symbol_t *sym) {
    const SymbolTable *s = symbol_parent_symtab(sym);
    if( s->asr_owner == nullptr ||
        !ASR::is_a<ASR::symbol_t>(*s->asr_owner) ) {
        return nullptr;
    }
    return ASR::down_cast<ASR::symbol_t>(s->asr_owner);
}

// True if this scope belongs to a procedure of a Template, i.e. a templated
// reference written here is being compiled as part of the template itself
// rather than instantiating it.
//
// Every link in the chain is optional. A scope owned by a Program has the
// TranslationUnit as its parent, which is not a symbol, so the walk stops
// there and the answer is false -- a templated reference in a main program
// instantiates, it is not inside a template.
static inline bool is_owned_by_template(const SymbolTable *scope) {
    if( scope == nullptr || scope->asr_owner == nullptr ||
        !ASR::is_a<ASR::symbol_t>(*scope->asr_owner) ) {
        return false;
    }
    ASR::symbol_t *owner = get_asr_owner(
        ASR::down_cast<ASR::symbol_t>(scope->asr_owner));
    return owner != nullptr && ASR::is_a<ASR::Template_t>(*owner);
}

// True if this scope belongs to a TranslationUnit.
//
// Interactive evaluation chains one TranslationUnit per cell, each scope
// parented to the previous cell's, so a scope chain no longer ends at the only
// TranslationUnit -- it passes through one per cell. Walks that look for "the"
// translation unit therefore have to stop at the nearest one, which is the cell
// currently being compiled. With a single TranslationUnit (ordinary
// compilation) the nearest one is also the root, so nothing changes.
static inline bool is_tu_scope(const SymbolTable *s) {
    return s->asr_owner != nullptr && ASR::is_a<ASR::unit_t>(*s->asr_owner);
}

// Interactive evaluation compiles one TranslationUnit per cell, chained by
// scope, and a cell may redeclare a name an earlier cell already used. Both
// declarations stay live -- code compiled earlier keeps using the old one -- so
// they need distinct symbols. Qualify by the cell a symbol belongs to, the way
// symbols in a module are qualified by the module. The first cell is
// unqualified, so that it and ordinary compilation produce identical symbols.
std::string cell_prefix(const SymbolTable *s);

// Returns the Module_t the symbol is in, or nullptr if not in a module
static inline ASR::Module_t *get_sym_module(const ASR::symbol_t *sym) {
    const SymbolTable *s = symbol_parent_symtab(sym);
    while (s->parent != nullptr && !is_tu_scope(s)) {
        ASR::symbol_t *asr_owner = ASR::down_cast<ASR::symbol_t>(s->asr_owner);
        if (ASR::is_a<ASR::Module_t>(*asr_owner)) {
            return ASR::down_cast<ASR::Module_t>(asr_owner);
        }
        s = s->parent;
    }
    return nullptr;
}

// Whether `sym` is a generic procedure or operator bound to a derived type.
static inline bool is_type_bound_generic(ASR::symbol_t *sym) {
    ASR::asr_t *owner = symbol_parent_symtab(symbol_get_past_external(sym))->asr_owner;
    return owner != nullptr && ASR::is_a<ASR::symbol_t>(*owner)
        && ASR::is_a<ASR::Struct_t>(*ASR::down_cast<ASR::symbol_t>(owner));
}

// Whether `sym` is a symbol of `scope` or of a scope that contains it.
static inline bool is_symbol_of_scope_chain(SymbolTable *scope, ASR::symbol_t *sym) {
    SymbolTable *sym_scope = symbol_parent_symtab(sym);
    for (SymbolTable *s = scope; s != nullptr; s = s->parent) {
        if (s == sym_scope) {
            return true;
        }
    }
    return false;
}

// The symbol through which `scope` calls `proc`, a procedure bound to the
// type-bound operator `generic`. A type and its bindings can be used
// without `use`, so the procedure is imported here, at the call, as
// `<proc>@<generic>`, unless `scope` already has it.
ASR::symbol_t *import_type_bound_specific(Allocator &al, SymbolTable *scope,
    ASR::symbol_t *proc, const std::string &generic);

ASR::symbol_t *get_asr_owner(const ASR::expr_t *expr);

// Returns the Module_t the symbol is in, or nullptr if not in a module
// or no asr_owner yet
ASR::Module_t *get_sym_module0(const ASR::symbol_t *sym);

// Returns true if `name` was produced by the compiler itself (an ASR pass or
// an intrinsic lowering) rather than written by the user. Such names must not
// appear in diagnostics, and symbols carrying them are exempt from checks that
// only make sense for user-written code.
// The spellings in use today are `__libasr_created_*`, `__libasr_created__*`,
// `__libasr__created__var__*`, `__libasr_index_*`, `__libasr_omp_arg_*`,
// `_lcompilers_*` and `__lcompilers*`, so the two families are matched by
// their common prefix. Matching is anchored at the start on purpose: a
// user-written name that merely contains one of these is not generated.
static inline bool is_compiler_generated_name(const std::string &name) {
    return startswith(name, "__libasr")
        || startswith(name, "_lcompilers_")
        || startswith(name, "__lcompilers");
}

// Returns true if the Function is intrinsic, otherwise false
template <typename T>
static inline bool is_intrinsic_procedure(const T *fn) {
    ASR::symbol_t *sym = (ASR::symbol_t*)fn;
    ASR::Module_t *m = get_sym_module0(sym);
    if (m != nullptr) {
        if (startswith(m->m_name, "lfortran_intrinsic")) return true;
    }
    return false;
}

bool is_intrinsic_symbol(const ASR::symbol_t *fn);

// Returns true if the Function is intrinsic, otherwise false
// This version uses the `intrinsic` member of `Module`, so it
// should be used instead of is_intrinsic_procedure
bool is_intrinsic_function2(const ASR::Function_t *fn);

// Returns true if the Function is intrinsic, otherwise false
template <typename T>
static inline bool is_intrinsic_optimization(const T *routine) {
    ASR::symbol_t *sym = (ASR::symbol_t*)routine;
    if( ASR::is_a<ASR::ExternalSymbol_t>(*sym) ) {
        ASR::ExternalSymbol_t* ext_sym = ASR::down_cast<ASR::ExternalSymbol_t>(sym);
        return (std::string(ext_sym->m_module_name).find("lfortran_intrinsic_optimization") != std::string::npos);
    }
    ASR::Module_t *m = get_sym_module0(sym);
    if (m != nullptr) {
        return (std::string(m->m_name).find("lfortran_intrinsic_optimization") != std::string::npos);
    }
    return false;
}

// Returns true if all arguments have a `value`
static inline bool all_args_have_value(const Vec<ASR::expr_t*> &args) {
    for (auto &a : args) {
        ASR::expr_t *v = expr_value(a);
        if (v == nullptr) return false;
    }
    return true;
}

/*

This function determines if a given expression represents a variable according
to the rules of Fortran variable definitions. Here's an illustration of declaration's:

```fortran
    CHARACTER(len=5) x, y(3)
    INTEGER, PARAMETER :: i
    TYPE (EMPLOYEE) e
        INTEGER age
    END TYPE EMPLOYEE
```

then 'x', 'y', 'y(1)', 'y(1:2)', 'e % age' are all variables, while 'i' isn't

TODO: this definitely needs some extensions to include others as "variable"'s
*/
bool is_variable(ASR::expr_t* a_value);

// `p => tgt` in a declaration associates the pointer with a designator — a
// whole variable, an array element or section, or a component — instead of
// giving it a value. `is_static_pointer_association` says which of these a
// backend lays out as static data; the `global_init` pass turns the rest into
// the pointer assignment that runs before any user code observes the pointer.
//
// A named constant is not a valid target, so a `parameter` at the base of the
// designator says no. `=> null()` is a value, not a designator, and is laid
// out statically as before.
bool is_pointer_association_initializer(ASR::expr_t* init);

// Whether `expr` designates a named constant or a subobject of one, such as
// `c(1)%k` or `c(2:3)` for a PARAMETER `c`. Such a designator is a constant,
// not a variable, so it cannot be defined.
bool is_named_constant_designator(ASR::expr_t* expr);

// Returns true if expression can be safely used as an actual argument for
// INTENT(OUT/INOUT) dummy arguments.
bool is_modifiable_actual_argument_expr(ASR::expr_t* a_value);

bool is_value_constant(ASR::expr_t *a_value);

// A local that is neither saved nor a parameter is initialized on every entry
// to its procedure or BLOCK. A non-constant initializer on such a local is
// how a pass evaluates a specification expression, such as the bound of an
// automatic array, exactly once on entry.
static inline bool is_entry_initialized_local(const ASR::Variable_t &v) {
    return v.m_intent == ASR::intentType::Local &&
        v.m_storage == ASR::storage_typeType::Default &&
        v.m_symbolic_value != nullptr && v.m_value == nullptr &&
        ASR::is_a<ASR::Integer_t>(*v.m_type) &&
        !is_value_constant(v.m_symbolic_value);
}

// The first local of `scope` whose value or descriptor metadata is needed by
// `expr` but is not defined on entry. Inquiries do not need element values.
const ASR::Variable_t* undefined_local_on_entry(ASR::expr_t* expr, SymbolTable* scope);

// Whether `f` is a reference to the intrinsic MAX.
bool is_intrinsic_max(const ASR::IntrinsicElementalFunction_t &f);

// A Variable of HostAssociated storage denotes a variable of the host of its
// procedure, which its symbolic_value designates through the frame dummy
// argument of the procedure.
static inline bool is_host_associated(const ASR::Variable_t &v) {
    return v.m_storage == ASR::storage_typeType::HostAssociated;
}

// Whether `v` has storage of its own, which its scope allocates, initializes,
// finalizes and deallocates. A variable in a common block lives in the
// storage of the block, and a HostAssociated variable denotes the storage of
// a host variable.
static inline bool has_own_storage(const ASR::Variable_t &v) {
    return v.m_common_block == nullptr && !is_host_associated(v);
}

// Whether an internal procedure reaches the host variable `v` through the
// frame of its host (see nested_vars). A variable in a common block lives in
// the static storage of the block, and each thread has its own threadprivate
// variable: internal procedures reference them directly.
static inline bool is_reached_through_host_frame(const ASR::Variable_t &v) {
    return v.m_common_block == nullptr &&
        v.m_storage != ASR::storage_typeType::Threadprivate;
}

static inline bool is_reference(const ASR::ttype_t *t) {
    return t != nullptr && ASR::is_a<ASR::Reference_t>(*t);
}

// The member of `x` when it is a member of Reference type, which `x` then
// designates the referenced variable of; nullptr otherwise.
static inline ASR::Variable_t* get_reference_member(
        const ASR::StructInstanceMember_t &x) {
    ASR::symbol_t *member = symbol_get_past_external(x.m_m);
    if (member == nullptr || !ASR::is_a<ASR::Variable_t>(*member)) {
        return nullptr;
    }
    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(member);
    return is_reference(v->m_type) ? v : nullptr;
}

// Whether `decl` declares the type of a frame (see nested_vars): a derived
// type with a member of Reference type.
static inline bool is_frame_type(ASR::symbol_t *decl) {
    decl = decl != nullptr ? symbol_get_past_external(decl) : nullptr;
    if (decl == nullptr || !ASR::is_a<ASR::Struct_t>(*decl)) {
        return false;
    }
    for (auto &item: ASR::down_cast<ASR::Struct_t>(decl)->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Variable_t>(*item.second) && is_reference(
                ASR::down_cast<ASR::Variable_t>(item.second)->m_type)) {
            return true;
        }
    }
    return false;
}

// Whether `decl` is the type of the frame slots of a thunk pool (see
// nested_vars): a derived type whose one member is a pointer to a frame.
static inline bool is_frame_slot_type(ASR::symbol_t *decl) {
    decl = decl != nullptr ? symbol_get_past_external(decl) : nullptr;
    if (decl == nullptr || !ASR::is_a<ASR::Struct_t>(*decl)) {
        return false;
    }
    ASR::Struct_t *s = ASR::down_cast<ASR::Struct_t>(decl);
    if (s->n_members != 1) {
        return false;
    }
    ASR::symbol_t *member = s->m_symtab->get_symbol(s->m_members[0]);
    if (member == nullptr || !ASR::is_a<ASR::Variable_t>(*member)) {
        return false;
    }
    ASR::Variable_t *m = ASR::down_cast<ASR::Variable_t>(member);
    return m->m_type != nullptr && ASR::is_a<ASR::Pointer_t>(*m->m_type) &&
        is_frame_type(m->m_type_declaration);
}

// Whether `v` holds frame slots of a host (see nested_vars), which the
// procedures nested in the host read directly: a saved pointer to a frame,
// or the saved array of the frame slots of the thunk pools.
static inline bool is_frame_slot(const ASR::Variable_t &v) {
    if (v.m_storage != ASR::storage_typeType::Save || v.m_type == nullptr) {
        return false;
    }
    if (ASR::is_a<ASR::Pointer_t>(*v.m_type)) {
        return is_frame_type(v.m_type_declaration);
    }
    return ASR::is_a<ASR::Array_t>(*v.m_type) &&
        is_frame_slot_type(v.m_type_declaration);
}

// The common block in whose storage `v` lives, or nullptr when `v` has
// storage of its own.
static inline ASR::CommonBlock_t* get_common_block(const ASR::Variable_t *v) {
    if (v->m_common_block == nullptr) {
        return nullptr;
    }
    return ASR::down_cast<ASR::CommonBlock_t>(v->m_common_block);
}

// A view of a common block that a translation unit defines gives its
// storage initial values exactly when one of its variables has one.
void update_common_block_role(ASR::CommonBlock_t *block);

static inline bool is_value_constant(ASR::expr_t *a_value, int64_t& const_value) {
    if( a_value == nullptr ) {
        return false;
    }
    if (ASR::is_a<ASR::IntegerConstant_t>(*a_value)) {
        ASR::IntegerConstant_t* const_int = ASR::down_cast<ASR::IntegerConstant_t>(a_value);
        const_value = const_int->m_n;
    } else {
        return false;
    }
    return true;
}

static inline bool is_value_constant(ASR::expr_t *a_value, bool& const_value) {
    if( a_value == nullptr ) {
        return false;
    }
    if (ASR::is_a<ASR::LogicalConstant_t>(*a_value)) {
        ASR::LogicalConstant_t* const_logical = ASR::down_cast<ASR::LogicalConstant_t>(a_value);
        const_value = const_logical->m_value;
    } else {
        return false;
    }
    return true;
}

bool is_value_constant(ASR::expr_t *a_value, double& const_value);

static inline bool is_value_constant(ASR::expr_t *a_value, std::string& const_value) {
    if( a_value == nullptr ) {
        return false;
    }
    if (ASR::is_a<ASR::StringConstant_t>(*a_value)) {
        ASR::StringConstant_t* const_string = ASR::down_cast<ASR::StringConstant_t>(a_value);
        const_value = std::string(const_string->m_s);
    } else {
        return false;
    }
    return true;
}

bool is_value_equal(ASR::expr_t* test_expr, ASR::expr_t* desired_expr);

bool is_value_in_range(ASR::expr_t* start, ASR::expr_t* end, ASR::expr_t* value);

// Returns true if all arguments are evaluated
static inline bool all_args_evaluated(const Vec<ASR::expr_t*> &args, bool ignore_null=false) {
    for (auto &a : args) {
        if (ignore_null && !a) continue;
        ASR::expr_t* a_value = ASRUtils::expr_value(a);
        if( !is_value_constant(a_value) ) {
            return false;
        }
    }
    return true;
}

static inline std::string get_mangled_name(ASR::Module_t* module, std::string symbol_name) {
    std::string module_name = module->m_name;
    if( module_name == symbol_name ) {
        return "__" + std::string(module->m_name) + "_" + symbol_name;
    } else {
        return symbol_name;
    }
}

// Returns true if all arguments are evaluated
// Overload for array
bool all_args_evaluated(const Vec<ASR::array_index_t> &args);

ASR::Variable_t* extract_ExternalSymbol_Variable(ASR::expr_t* a_expr);

bool extract_value(ASR::expr_t* value_expr,
    std::complex<double>& value);

bool extract_string_value(ASR::expr_t* value_expr,
    std::string& value);

template <typename T,
    typename = typename std::enable_if<
        std::is_same<T, std::complex<double>>::value == false &&
        std::is_same<T, std::complex<float>>::value == false>::type>
static inline bool extract_value(ASR::expr_t* value_expr, T& value) { // Returns extraction state.
    if( !is_value_constant(value_expr) ) {
        return false;
    }

    switch( value_expr->type ) {
        case ASR::exprType::IntegerConstant: {
            ASR::IntegerConstant_t* const_int = ASR::down_cast<ASR::IntegerConstant_t>(value_expr);
            value = (T) const_int->m_n;
            break;
        }
        case ASR::exprType::UnsignedIntegerConstant: {
            ASR::UnsignedIntegerConstant_t* const_int = ASR::down_cast<ASR::UnsignedIntegerConstant_t>(value_expr);
            value = (T) const_int->m_n;
            break;
        }
        case ASR::exprType::RealConstant: {
            ASR::RealConstant_t* const_real = ASR::down_cast<ASR::RealConstant_t>(value_expr);
            // kind=16 (real128) and kind=10 (x86_fp80) cannot be represented in
            // double m_r — m_r is a pointer-encoded payload.
            int r_k = ASRUtils::extract_kind_from_ttype_t(const_real->m_type);
            if (r_k == 16 || r_k == 10) {
                return false;
            }
            if constexpr (std::is_same<T, double>::value){
                value = (T) const_real->m_r;
            }
            break;
        }
        case ASR::exprType::LogicalConstant: {
            ASR::LogicalConstant_t* const_logical = ASR::down_cast<ASR::LogicalConstant_t>(value_expr);
            if constexpr (std::is_same<T, bool>::value){
                value = (T) const_logical->m_value;
            }
            break;
        }
        case ASR::exprType::StringConstant:{
            ASR::StringConstant_t* const_string = ASR::down_cast<ASR::StringConstant_t>(value_expr);
            if constexpr (std::is_same<T, char*>::value){
                value = (T) const_string->m_s;
            }
            break;
        }
        case ASR::exprType::Var: {
            ASR::Variable_t* var = EXPR2VAR(value_expr);
            if (var->m_storage == ASR::storage_typeType::Parameter
                    && !extract_value(var->m_value, value)) {
                return false;
            }
            break;
        }
        case ASR::exprType::Cast : {
            ASR::Cast_t* cast = ASR::down_cast<ASR::Cast_t>(value_expr);
            return extract_value(cast->m_arg, value);
        }
        case ASR::exprType::IntegerUnaryMinus:
        case ASR::exprType::RealUnaryMinus:
        case ASR::exprType::FunctionCall:
        case ASR::exprType::IntegerBinOp:
        case ASR::exprType::StringLen:
        case ASR::exprType::ArrayItem: {
            if (!extract_value(expr_value(value_expr), value)) {
                return false;
            }
            break;
        }
        default:
            return false;
    }
    return true;
}

template <typename T,
    typename = typename std::enable_if<
        std::is_same<T, std::complex<double>>::value == false &&
        std::is_same<T, std::complex<float>>::value == false>::type>
inline void extract_value_(ASR::expr_t* value_expr, T& value) { // Raises error if expr doesn't have value.
    bool extracted = extract_value(value_expr, value);
    if (!extracted) LCompilersException("Expr value can't be extracted");
}

static inline std::string extract_dim_value(ASR::expr_t* dim) {
    int64_t length_dim = 0;
    if( dim == nullptr ||
        !ASRUtils::extract_value(ASRUtils::expr_value(dim), length_dim)) {
        return ":";
    }

    return std::to_string(length_dim);
}

static inline std::int64_t extract_dim_value_int(ASR::expr_t* dim) {
    int64_t length_dim = 0;
    if( dim == nullptr ||
        !ASRUtils::extract_value(ASRUtils::expr_value(dim), length_dim)) {
        return -1;
    }

    return length_dim;
}

std::string type_encode_dims(size_t n_dims, ASR::dimension_t* m_dims );

// A struct or union is named by its declaration: the one of `expr`, or
// `type_declaration` when there is no expression.
std::string get_type_code(const ASR::ttype_t *t, bool use_underscore_sep=false,
    bool encode_dimensions_=true, bool set_dimensional_hint=true, ASR::expr_t* expr = nullptr,
    ASR::symbol_t* type_declaration = nullptr);

static inline std::string get_type_code(ASR::ttype_t** types, size_t n_types,
    bool use_underscore_sep=false, bool encode_dimensions=true) {
    std::string code = "";
    for( size_t i = 0; i < n_types; i++ ) {
        code += get_type_code(types[i], use_underscore_sep, encode_dimensions) + "_";
    }
    return code;
}

std::string type_to_str_python_symbol(const ASR::ttype_t *t, ASR::symbol_t* struct_sym);

static inline std::string type_to_str_python_expr(const ASR::ttype_t *t, ASR::expr_t* expr)
{
    ASR::symbol_t* struct_sym = nullptr;

    if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(const_cast<ASR::ttype_t*>(t)))) {
        LCOMPILERS_ASSERT_MSG(expr != nullptr, "`expr` should be non-null for `StructType`");
        struct_sym = ASRUtils::get_struct_sym_from_struct_expr(expr);
    }

    return type_to_str_python_symbol(t, struct_sym);
}

std::string binop_to_str_python(const ASR::binopType t);

static inline bool is_immutable(const ASR::ttype_t *type) {
    return ((ASR::is_a<ASR::String_t>(*type) || ASR::is_a<ASR::Tuple_t>(*type)
        || ASR::is_a<ASR::Complex_t>(*type)));
}

// Returns a list of values
Vec<ASR::call_arg_t> get_arg_values(Allocator &al, const Vec<ASR::call_arg_t>& args);

// Converts a vector of call_arg to a vector of expr
// It skips missing call_args
static inline Vec<ASR::expr_t*> call_arg2expr(Allocator &al, const Vec<ASR::call_arg_t>& call_args) {
    Vec<ASR::expr_t*> args;
    args.reserve(al, call_args.size());
    for (auto &a : call_args) {
        if (a.m_value != nullptr) {
            args.push_back(al, a.m_value);
        }
    }
    return args;
}

// Returns the TranslationUnit_t's symbol table by going via parents
static inline SymbolTable *get_tu_symtab(SymbolTable *symtab) {
    SymbolTable *s = symtab;
    while (s->parent != nullptr && !is_tu_scope(s)) {
        s = s->parent;
    }
    LCOMPILERS_ASSERT(ASR::is_a<ASR::unit_t>(*s->asr_owner))
    return s;
}

// Returns the name of scopes in reverse order (local scope first, function second, module last)
Vec<char*> get_scope_names(Allocator &al, const SymbolTable *symtab);

ASR::expr_t* get_constant_zero_with_given_type(Allocator& al, ASR::ttype_t* asr_type);


ASR::expr_t* get_constant_one_with_given_type(Allocator& al, ASR::ttype_t* asr_type);

void mark_modules_as_external(const LCompilers::ASR::TranslationUnit_t &u);

// Returns a string as long as `string_type` with every character equal to
// `fill`, or nullptr when that length is not known at compile time.
ASR::expr_t* get_string_filled_with_char(Allocator& al,
        ASR::ttype_t* string_type, unsigned char fill);

ASR::expr_t* get_minimum_value_with_given_type(Allocator& al, ASR::ttype_t* asr_type);

ASR::expr_t* get_maximum_value_with_given_type(Allocator& al, ASR::ttype_t* asr_type);

const ASR::intentType intent_local=ASR::intentType::Local; // local variable (not a dummy argument)
const ASR::intentType intent_in   =ASR::intentType::In; // dummy argument, intent(in)
const ASR::intentType intent_out  =ASR::intentType::Out; // dummy argument, intent(out)
const ASR::intentType intent_inout=ASR::intentType::InOut; // dummy argument, intent(inout)
const ASR::intentType intent_return_var=ASR::intentType::ReturnVar; // return variable of a function
const ASR::intentType intent_unspecified=ASR::intentType::Unspecified; // dummy argument, ambiguous intent

// True if `sym` is a dummy argument of the procedure `fn`.
static inline bool is_dummy_argument(const ASR::Function_t &fn, const ASR::symbol_t *sym) {
    for (size_t i = 0; i < fn.n_args; i++) {
        if (ASR::is_a<ASR::Var_t>(*fn.m_args[i]) &&
                ASR::down_cast<ASR::Var_t>(fn.m_args[i])->m_v == sym) {
            return true;
        }
    }
    return false;
}

// True if `fn` is a dummy procedure: a dummy argument of the procedure
// whose scope declares it.
static inline bool is_dummy_procedure(const ASR::Function_t &fn) {
    SymbolTable *scope = fn.m_symtab->parent;
    if (scope == nullptr || scope->asr_owner == nullptr ||
            !ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) {
        return false;
    }
    ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
    return ASR::is_a<ASR::Function_t>(*owner) &&
        is_dummy_argument(*ASR::down_cast<ASR::Function_t>(owner), &fn.base);
}

static inline bool is_arg_dummy(int intent) {
    return intent == intent_in || intent == intent_out
        || intent == intent_inout || intent == intent_unspecified;
}

static inline bool main_program_present(const ASR::TranslationUnit_t &unit)
{
    for (auto &a : unit.m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Program_t>(*a.second)) return true;
    }
    return false;
}

static inline bool global_function_present(const ASR::TranslationUnit_t &unit)
{
    bool contains_global_function = false;

    for (auto &a : unit.m_symtab->get_scope()) {
        if (ASR::is_a<ASR::Function_t>(*a.second)) {
            contains_global_function = true;
        } else if (ASR::is_a<ASR::Module_t>(*a.second)) {
            // If the ASR contains a module, then the global
            // function is not the only symbol present.
            return false;
        }
    }

    return contains_global_function;
}

// A BLOCK DATA program unit is compiled into an object file of its own: it
// gives the initial values of the common blocks it declares.
static inline bool block_data_present(const ASR::TranslationUnit_t &unit)
{
    for (auto &a : unit.m_symtab->get_scope()) {
        if (ASR::is_a<ASR::BlockData_t>(*a.second)) return true;
    }
    return false;
}

// Accepts dependencies in the form A -> [B, D, ...], B -> [C, D]
// Returns a list of dependencies in the order that they should be built:
// [D, C, B, A]
std::vector<std::string> order_deps(std::map<std::string,
        std::vector<std::string>> const &deps);

std::vector<std::string> determine_module_dependencies(
        const ASR::TranslationUnit_t &unit);

std::vector<std::string> determine_function_definition_order(
         SymbolTable* symtab);

std::vector<std::string> determine_class_procedure_declaration_order(
         SymbolTable* symtab);

std::vector<std::string> determine_variable_declaration_order(
         SymbolTable* symtab);

void extract_module_python(const ASR::TranslationUnit_t &m,
        std::vector<std::pair<std::string, ASR::Module_t*>>& children_modules,
        std::string module_name);

// True if the procedure is declared rather than defined: it has no body, so
// the backend only emits a declaration for it. Includes both a real explicit
// interface and an interface-less external (ImplicitInterface).
static inline bool is_declaration_deftype(ASR::deftypeType deftype) {
    return deftype == ASR::deftypeType::Interface
        || deftype == ASR::deftypeType::ImplicitInterface;
}

// True if `f_type` is the interface of a procedure that is not a module
// procedure: an interface body, an `external` statement or an implicit
// interface of an external or dummy procedure, or an abstract interface.
static inline bool is_external_procedure_interface(
        const ASR::FunctionType_t *f_type) {
    return is_declaration_deftype(f_type->m_deftype)
        && f_type->m_abi != ASR::abiType::Intrinsic
        && !f_type->m_module;
}

// True if `x` has deftype ImplicitInterface: the opaque procedure type of a
// procedure with an implicit interface (e.g. `integer, external :: f`, an
// `external f` dummy or a `procedure()` pointer).
//   * `arg_types` is always empty and means "unknown", not "no arguments".
//   * `return_var_type` is set only when the procedure is explicitly typed.
//   * The type is never changed as uses of the procedure are seen.
// An opaque procedure is never a call target. Every call goes through a
// FunctionPointerCast to an interface built from that call's actuals, and a
// procedure passed to a dummy of a different type is cast to the dummy's type.
static inline bool is_bare_implicit_interface(const ASR::FunctionType_t &x) {
    return x.m_deftype == ASR::deftypeType::ImplicitInterface;
}

static inline bool is_bare_implicit_interface(const ASR::Function_t &x) {
    return is_bare_implicit_interface(
        *ASR::down_cast<ASR::FunctionType_t>(x.m_function_signature));
}

static inline bool is_bare_implicit_interface(ASR::symbol_t *v) {
    if (!v) {
        return false;
    }
    ASR::symbol_t *f2 = symbol_get_past_external(v);
    if (!f2 || !ASR::is_a<ASR::Function_t>(*f2)) {
        return false;
    }
    return is_bare_implicit_interface(*ASR::down_cast<ASR::Function_t>(f2));
}

// True if `t`, past pointer and allocatable, is the opaque procedure type.
static inline bool is_opaque_procedure_type(ASR::ttype_t *t) {
    if (t == nullptr) {
        return false;
    }
    t = type_get_past_allocatable(type_get_past_pointer(t));
    return ASR::is_a<ASR::FunctionType_t>(*t) && is_bare_implicit_interface(
        *ASR::down_cast<ASR::FunctionType_t>(t));
}

// The opaque procedure type, returning `return_type` (null for a procedure
// that is not explicitly typed).
static inline ASR::ttype_t* make_opaque_procedure_type(Allocator &al,
        const Location &loc, ASR::ttype_t *return_type) {
    return ASRUtils::TYPE(ASR::make_FunctionType_t(al, loc, nullptr, 0,
        return_type, ASR::abiType::Source, ASR::deftypeType::ImplicitInterface,
        nullptr, false, false, false, false, false, nullptr, 0, false,
        ASR::exec_spaceType::Host));
}

// Character dummy arguments passed as a data pointer plus a hidden length.
//
// A procedure of type `ft` passes each of its nonallocatable, nonpointer
// character dummies (scalars, and explicit-shape or assumed-size arrays) as a
// pointer to the character data, and the length of the actual (the element
// length for an array) by value in a hidden `integer(8), value, intent(in)`
// dummy. The hidden dummies follow all the other dummies, in the order of the
// character dummies they belong to. This is the convention of gfortran and the
// other Unix Fortran compilers. The `string_length_arguments` pass adds the
// hidden dummies to every such procedure and the lengths to every call, and a
// code generator passes such a dummy as its data pointer alone.
//
// It applies to every procedure without BIND(C), whatever kind (external,
// module, internal, procedure dummy or pointer, interface, implicit
// interface), except for intrinsic procedures, whose arguments are lowered by
// the compiler, and device code.
bool has_hidden_string_lengths(const ASR::FunctionType_t &ft);

// True if dummy `v` of a procedure of type `ft` is passed as a data pointer
// with a hidden length (see has_hidden_string_lengths). Deferred-length
// (allocatable, pointer) dummies, assumed-shape arrays and VALUE dummies are
// passed differently and are not.
bool is_string_dummy_with_hidden_length(
        const ASR::FunctionType_t &ft, const ASR::Variable_t &v);

// True if `x` is a dummy of procedure `fn` passed as a data pointer with a
// hidden length.
bool is_string_dummy_with_hidden_length(
        const ASR::Function_t &fn, ASR::expr_t *x);

// True if a procedure of type `a` can be used where type `b` is expected
// without a FunctionPointerCast: both are opaque or both are explicit, with
// the same arguments, result and abi. Unlike `types_equal`, an opaque type is
// never identical to an explicit zero-argument one.
bool procedure_types_identical(ASR::FunctionType_t *a,
        ASR::FunctionType_t *b);

void update_call_args(Allocator &al, SymbolTable *current_scope, bool implicit_interface);


ASR::Module_t* extract_module(const ASR::TranslationUnit_t &m);

void fix_translation_unit(Allocator &al, ASR::TranslationUnit_t *tu, SymbolTable *symtab, bool run_verify);

ASR::Module_t* load_module(Allocator &al, SymbolTable *symtab,
                            const std::string &module_name,
                            const Location &loc, bool intrinsic,
                            std::set<std::string> &loaded_submodules,
                            LCompilers::PassOptions& pass_options,
                            bool run_verify,
                            const std::function<void (const std::string &, const Location &)> err,
                            LCompilers::LocationManager &lm,
                            bool generate_object_code = false,
                            bool load_submodules = false);

void load_dependent_submodules(Allocator &al, SymbolTable *symtab,
                               ASR::Module_t* mod, const Location &loc,
                               std::set<std::string> &loaded_submodules,
                               LCompilers::PassOptions& pass_options,
                               bool run_verify,
                               const std::function<void (const std::string &, const Location &)> err,
                               LCompilers::LocationManager &lm);

Result<ASR::TranslationUnit_t*, ErrorMessage> find_and_load_module(Allocator &al, const std::string &msym,
                                                SymbolTable &symtab, bool intrinsic,
                                                LCompilers::PassOptions& pass_options,
                                                LCompilers::LocationManager &lm);

Result<std::vector<ASR::TranslationUnit_t*>, ErrorMessage> find_and_load_submodules(Allocator &al, const std::string &msym,
                                                            SymbolTable &symtab,
                                                            LCompilers::PassOptions &pass_options,
                                                            LCompilers::LocationManager &lm);

void set_intrinsic(ASR::TranslationUnit_t* trans_unit);

// The variable a designator of a common_association is based on, and where
// the first storage unit it designates lies within that variable,
// independently of the target: `element_index` counts array elements from
// zero in array element order and `character_index` counts characters from
// zero within that element. A designator is the variable itself, an element
// of it with constant subscripts, or a nonempty substring of either with
// constant bounds and stride 1 (an absent start is 1, an absent end the
// length). Returns nullptr for anything else, and for a position outside the
// constant bounds of the variable.
ASR::Variable_t* get_storage_designator_offset(ASR::expr_t *designator,
    int64_t &element_index, int64_t &character_index);

static inline bool is_const(ASR::expr_t *x) {
    if (ASR::is_a<ASR::Var_t>(*x)) {
        ASR::Var_t* v = ASR::down_cast<ASR::Var_t>(x);
        ASR::symbol_t* sym = ASRUtils::symbol_get_past_external(v->m_v);
        if (sym && ASR::is_a<ASR::Variable_t>(*sym)) {
            ASR::Variable_t* var = ASR::down_cast<ASR::Variable_t>(sym);
            return var->m_storage == ASR::storage_typeType::Parameter;
        }
    }
    return false;
}

ASR::expr_t* get_struct_member_value_from_constant(ASR::expr_t* value,
                            ASR::symbol_t* member);

// The compile-time value of `expr`, a named constant or a part of one
// selected by components and constant subscripts. Unlike `expr_value`, this
// also finds the value of a part whose folded value is not attached to it
// because it names a procedure inaccessible where `expr` appears, so that the
// data components of that part can still be folded.
ASR::expr_t* designator_constant_value(Allocator& al, ASR::expr_t* expr);

// Whether the constant `value` of derived type names a procedure, as the
// value of an associated procedure pointer component does. The procedure is
// named by a symbol of the scope that wrote the constant, so the constant
// cannot stand in for its named constant in another scope.
bool struct_constant_names_procedure(ASR::expr_t* value);

// Whether the constant `value` names a procedure through a symbol that
// cannot be named from `scope`, as the value of a procedure pointer
// component of a named constant of another module does. Such a value cannot
// stand in for the constant there; it is read at run time instead.
bool constant_names_procedure_outside(ASR::expr_t* value, SymbolTable* scope);

// The compile-time value `value` of a component of a named constant, for a
// reference to that component from `scope`, with its derived types imported
// there, or null if `constant_names_procedure_outside(value, scope)`.
ASR::expr_t* import_folded_constant_value(Allocator& al,
    ASR::expr_t* value, SymbolTable* scope);

ASR::asr_t* getStructInstanceMember_t(Allocator& al, const Location& loc,
                            ASR::asr_t* v_var, ASR::symbol_t *v,
                            ASR::symbol_t* member, SymbolTable* current_scope);

bool use_overloaded(ASR::expr_t* left, ASR::expr_t* right,
                    ASR::binopType op, std::string& intrinsic_op_name,
                    SymbolTable* curr_scope, ASR::asr_t*& asr,
                    Allocator &al, const Location& loc,
                    SetChar& current_function_dependencies,
                    SetChar& current_module_dependencies,
                    const std::function<void (const std::string &, const Location &)> err);

bool use_overloaded_unary_op(ASR::expr_t* operand, const std::string& op_name,
    SymbolTable* curr_scope, ASR::asr_t*& asr, Allocator &al,
    const Location& loc, SetChar& current_function_dependencies,
    SetChar& current_module_dependencies,
    const std::function<void (const std::string &, const Location &)> err);

bool is_op_overloaded(ASR::binopType op, std::string& intrinsic_op_name,
                      SymbolTable* curr_scope, ASR::Struct_t* left_struct=nullptr);

bool use_overloaded(ASR::expr_t* left, ASR::expr_t* right,
                    ASR::cmpopType op, std::string& intrinsic_op_name,
                    SymbolTable* curr_scope, ASR::asr_t*& asr,
                    Allocator &al, const Location& loc,
                    SetChar& current_function_dependencies,
                    SetChar& current_module_dependencies,
                    const std::function<void (const std::string &, const Location &)> err);

bool is_op_overloaded(ASR::cmpopType op, std::string& intrinsic_op_name,
                      SymbolTable* curr_scope, ASR::Struct_t *left_struct);

bool use_overloaded(ASR::expr_t* left, ASR::expr_t* right,
                    ASR::logicalbinopType op, std::string& intrinsic_op_name,
                    SymbolTable* curr_scope, ASR::asr_t*& asr,
                    Allocator &al, const Location& loc,
                    SetChar& current_function_dependencies,
                    SetChar& current_module_dependencies,
                    const std::function<void (const std::string &, const Location &)> err);

bool is_op_overloaded(ASR::logicalbinopType op, std::string& intrinsic_op_name,
                      SymbolTable* curr_scope, ASR::Struct_t* left_struct=nullptr);

bool use_overloaded_assignment(ASR::expr_t* target, ASR::expr_t* value,
                               SymbolTable* curr_scope, ASR::asr_t*& asr,
                               Allocator &al, const Location& loc,
                               SetChar& current_function_dependencies,
                               SetChar& /*current_module_dependencies*/,
                               const std::function<void (const std::string &, const Location &)> err);

bool use_overloaded_file_read_write(std::string &read_write, Vec<ASR::expr_t*> args,
                               SymbolTable* curr_scope, ASR::asr_t*& asr,
                               Allocator &al, const Location& loc,
                               SetChar& current_function_dependencies,
                               SetChar& /*current_module_dependencies*/,
                               const std::function<void (const std::string &, const Location &)> err);

// The specific procedure of the defined input/output generic `read_write`
// (such as "~write_formatted") that `item` is transferred with, imported into
// `curr_scope` with the dependencies a call needs: the symbol a
// SubroutineCall names, or nullptr if there is none. `a_original_name`
// receives the call's original name, and `expr_dt` the item for a type-bound
// procedure (the call's `m_dt`), or nullptr. With `elementwise`, `item` may be
// an array whose elements are transferred one by one.
ASR::symbol_t* resolve_defined_io_procedure(std::string &read_write, ASR::expr_t* item,
                               SymbolTable* curr_scope, Allocator &al, const Location& loc,
                               SetChar& current_function_dependencies,
                               SetChar& current_module_dependencies,
                               const std::function<void (const std::string &, const Location &)> err,
                               ASR::symbol_t*& a_original_name, ASR::expr_t*& expr_dt,
                               bool elementwise=false);

void set_intrinsic(ASR::symbol_t* sym);

static inline bool is_pointer(ASR::ttype_t *x) {
    return ASR::is_a<ASR::Pointer_t>(*x);
}

static inline bool is_integer(ASR::ttype_t &x) {
    // return ASR::is_a<ASR::Integer_t>(
    //     *type_get_past_const(
    //         type_get_past_array(
    //         type_get_past_allocatable(
    //             type_get_past_pointer(
    //                 type_get_past_const(&x))))));
    return ASR::is_a<ASR::Integer_t>(
        *type_get_past_array(
            type_get_past_allocatable(
                type_get_past_pointer(&x))));
}

static inline bool is_unsigned_integer(ASR::ttype_t &x) {
    return ASR::is_a<ASR::UnsignedInteger_t>(
        *type_get_past_array(
            type_get_past_allocatable(
                type_get_past_pointer(&x))));
}

static inline bool is_real(ASR::ttype_t &x) {
    // return ASR::is_a<ASR::Real_t>(
    //     *type_get_past_const(
    //         type_get_past_array(
    //             type_get_past_allocatable(
    //                 type_get_past_pointer(&x)))));
    return ASR::is_a<ASR::Real_t>(
        *type_get_past_array(
            type_get_past_allocatable(
                type_get_past_pointer(&x))));
}

static inline bool is_character(ASR::ttype_t &x) {
    return ASR::is_a<ASR::String_t>(
        *type_get_past_array(
            type_get_past_allocatable(
                type_get_past_pointer(&x))));
}

// The Fortran characters of a compile time string value: one entry per
// character. A kind 1 value stores one byte per character, while a kind > 1
// value stores UTF-8 in `StringConstant::m_s`, so one entry can span several
// bytes.
std::vector<std::string> string_value_characters(
        const char* s, int64_t kind);

// The Unicode code point of the first character of a compile time string
// value, i.e. the result of ICHAR/IACHAR applied to it. A kind 1 value yields
// the byte, a kind > 1 value the code point its UTF-8 encodes.
static inline int64_t string_first_code_point(ASR::StringConstant_t* s) {
    int64_t kind = extract_kind_from_ttype_t(s->m_type);
    if (s->m_s[0] == '\0') return 0;
    if (kind == 1) return (int64_t)(unsigned char)s->m_s[0];
    std::vector<uint32_t> code_points = utf8_decode(std::string(s->m_s));
    LCOMPILERS_ASSERT(code_points.size() >= 1);
    return (int64_t)code_points[0];
}

// The character kinds the compiler supports: 1 (one byte per character,
// ASCII / the default kind) and 4 (four bytes per character, ISO 10646 /
// UCS-4). This matches GFortran; LLVM Flang additionally supports 2 (UCS-2).
static inline bool is_supported_character_kind(int64_t kind) {
    return kind == 1 || kind == 4;
}

static inline bool is_complex(ASR::ttype_t &x) {
    return ASR::is_a<ASR::Complex_t>(
        *type_get_past_array(
            type_get_past_allocatable(
                type_get_past_pointer(&x))));
}

template <int32_t kind>
static inline bool is_complex(ASR::ttype_t &x) {
    return is_complex(x) && ASRUtils::extract_kind_from_ttype_t(&x) == kind;
}


static inline bool is_logical(ASR::ttype_t &x) {
    return ASR::is_a<ASR::Logical_t>(
        *type_get_past_array(
            type_get_past_allocatable(
                type_get_past_pointer(&x))));
}

static inline bool is_struct(ASR::ttype_t& x) {
    return ASR::is_a<ASR::StructType_t>(*extract_type(&x)) && !ASRUtils::is_class_type(extract_type(&x));
}

// Checking if the ttype 't' is a type parameter
bool is_type_parameter(ASR::ttype_t &x);

// Checking if the symbol 'x' is a virtual function defined inside a requirement
static inline bool is_requirement_function(ASR::symbol_t *x) {
    ASR::symbol_t* x2 = symbol_get_past_external(x);
    switch (x2->type) {
        case ASR::symbolType::Function: {
            ASR::Function_t *func_sym = ASR::down_cast<ASR::Function_t>(x2);
            return ASRUtils::get_FunctionType(func_sym)->m_is_restriction;
        }
        default: return false;
    }
}

// Checking if the symbol 'x' is a generic function defined inside a template
bool is_generic_function(ASR::symbol_t *x);

// Checking if the string `arg_name` corresponds to one of the arguments of the template `x`
bool is_template_arg(ASR::symbol_t *x, std::string arg_name);

int get_body_size(ASR::symbol_t* s);

size_t extract_dimensions_from_ttype(ASR::ttype_t *x,
                                         ASR::dimension_t*& m_dims);

bool is_fixed_size_array(ASR::dimension_t* m_dims, size_t n_dims);

inline bool is_stringToArray_cast(ASR::expr_t* const expr){
    LCOMPILERS_ASSERT(expr)
    return  ASR::is_a<ASR::Cast_t>(*expr)
            && ASR::down_cast<ASR::Cast_t>(expr)->m_kind == ASR::StringToArray;
}

static inline bool is_fixed_size_array(ASR::ttype_t* type) {
    ASR::dimension_t* m_dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(type, m_dims);
    return ASRUtils::is_fixed_size_array(m_dims, n_dims);
}

int64_t get_fixed_size_of_array(ASR::dimension_t* m_dims, size_t n_dims);

int64_t get_fixed_size_of_ArraySection(ASR::ArraySection_t* x);

static inline int64_t get_fixed_size_of_array(ASR::ttype_t* type) {
    ASR::dimension_t* m_dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(type, m_dims);
    return ASRUtils::get_fixed_size_of_array(m_dims, n_dims);
}

std::pair<int64_t, int64_t> compute_type_size_align(ASR::ttype_t* type);

// Size and alignment a component contributes to its enclosing derived type.
// A pointer or allocatable component is stored as a pointer, whatever it
// points at; every other component contributes its own layout.
static inline std::pair<int64_t, int64_t> compute_struct_member_size_align(
        ASR::ttype_t* member_type) {
    if ((ASR::is_a<ASR::Pointer_t>(*member_type) ||
         ASR::is_a<ASR::Allocatable_t>(*member_type)) &&
        !ASR::is_a<ASR::Array_t>(*member_type)) {
        return {8, 8};
    }
    return compute_type_size_align(member_type);
}

// Compute the byte size and alignment of an ASR type, matching the
// LLVM struct layout rules used by LFortran's codegen on LP64 targets.
// Returns {size_bytes, align_bytes}. Returns {-1, -1} on failure.
std::pair<int64_t, int64_t> compute_type_size_align(ASR::ttype_t* type);

    // Compute the LLVM layout for a declared derived type. StructType_t carries
    // only its component types, while Struct_t also records an inherited parent
    // that LLVM lowers as the first, inlined field.
    std::pair<int64_t, int64_t>
    compute_struct_type_size_align(ASR::Struct_t* struct_type);

    // Byte size of the declared derived type of a struct expression, or -1
    // when it cannot be resolved. An ASR::StructType_t lists only the
    // components a type declares itself, so for an extended type it omits
    // the inherited ones; those are reachable only through the
    // ASR::Struct_t symbol, whose m_parent the backends inline as the first
    // field of the layout. Resolving the symbol therefore gives the size the
    // generated code actually allocates.
    int64_t get_struct_expr_byte_size(ASR::expr_t* expr);

int64_t get_type_byte_size(ASR::ttype_t* type);

inline int extract_n_dims_from_ttype(ASR::ttype_t *x) {
    ASR::dimension_t* m_dims_temp = nullptr;
    return checked_integer_cast<int>(extract_dimensions_from_ttype(x, m_dims_temp));
}

static inline bool is_dimension_empty(ASR::dimension_t& dim) {
    return ((dim.m_length == nullptr) ||
            (dim.m_start == nullptr));
}

static inline bool is_dimension_empty(ASR::dimension_t* dims, size_t n) {
    for( size_t i = 0; i < n; i++ ) {
        if( is_dimension_empty(dims[i]) || (dims[i].m_length != nullptr && dims[i].m_start == nullptr) ||
            (dims[i].m_length == nullptr && dims[i].m_start != nullptr) ) {
            return true;
        }
    }
    return false;
}

static inline bool is_dimension_empty(ASR::ttype_t* type) {
    ASR::dimension_t* dims = nullptr;
    size_t n = ASRUtils::extract_dimensions_from_ttype(type, dims);
    return is_dimension_empty(dims, n);
}

static inline bool is_only_upper_bound_empty(ASR::dimension_t& dim) {
    return (dim.m_start != nullptr && dim.m_length == nullptr);
}

inline bool is_assumed_rank_array(ASR::ttype_t* x) {
    x = type_get_past_allocatable(type_get_past_pointer(x));
    if (!ASR::is_a<ASR::Array_t>(*x)) {
        return false;
    }
    ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(x);
    return array_t->m_physical_type == ASR::array_physical_typeType::AssumedRankArray;
}

// Whether `x` is the type of an assumed-size array `a(..., *)`, whose last
// extent is not known. Such an array is an UnboundedPointerArray, except for
// a dummy of a bind(c) procedure: that is a PointerArray whose last
// dimension alone has no length.
inline bool is_assumed_size_array(ASR::ttype_t* x) {
    x = type_get_past_allocatable(type_get_past_pointer(x));
    if (!ASR::is_a<ASR::Array_t>(*x)) {
        return false;
    }
    ASR::Array_t* array_t = ASR::down_cast<ASR::Array_t>(x);
    if (array_t->m_physical_type ==
            ASR::array_physical_typeType::UnboundedPointerArray) {
        return true;
    }
    if (array_t->m_physical_type != ASR::array_physical_typeType::PointerArray ||
            array_t->n_dims == 0 ||
            array_t->m_dims[array_t->n_dims - 1].m_length != nullptr) {
        return false;
    }
    for (size_t i = 0; i + 1 < array_t->n_dims; i++) {
        if (array_t->m_dims[i].m_length == nullptr) {
            return false;
        }
    }
    return true;
}

inline bool is_array_t(ASR::ttype_t* const x){
    return ASR::is_a<ASR::Array_t>(*type_get_past_allocatable_pointer(x));
}

inline bool is_array_t(ASR::expr_t* const x){
    return ASR::is_a<ASR::Array_t>(*type_get_past_allocatable_pointer(expr_type(x)));
}

// Check through number of dims.
inline bool is_array(ASR::ttype_t *x) {
    ASR::dimension_t* dims = nullptr;
    return extract_dimensions_from_ttype(x, dims) > 0 || is_assumed_rank_array(x);
}

class ExprDependentOnlyOnArguments: public ASR::BaseWalkVisitor<ExprDependentOnlyOnArguments> {

    public:

        bool is_dependent_only_on_argument;
        bool only_intent_in_args;
        // A deferred-shape component of an argument has an extent only a
        // runtime descriptor knows. Set this where that extent is readable
        // where the expression is used, such as GPU device code, whose
        // arrays carry their extents as arguments.
        bool allow_deferred_shape_members;

        ExprDependentOnlyOnArguments():
            is_dependent_only_on_argument(false),
            only_intent_in_args(false),
            allow_deferred_shape_members(false)
        {}

        void visit_Var(const ASR::Var_t& x) {
            if( ASR::is_a<ASR::Variable_t>(*x.m_v) ) {
                ASR::Variable_t* x_m_v = ASR::down_cast<ASR::Variable_t>(x.m_v);
                if ( ASRUtils::is_array(x_m_v->m_type) && !only_intent_in_args ) {
                    is_dependent_only_on_argument = is_dependent_only_on_argument && ASRUtils::is_arg_dummy(x_m_v->m_intent);
                } else {
                    is_dependent_only_on_argument = is_dependent_only_on_argument && (x_m_v->m_intent == ASR::intentType::In);
                }
            } else {
                is_dependent_only_on_argument = false;
            }
        }

        void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
            ASR::BaseWalkVisitor<ExprDependentOnlyOnArguments>::visit_StructInstanceMember(x);
            if (!allow_deferred_shape_members &&
                ASRUtils::is_array(ASRUtils::symbol_type(x.m_m)) &&
                !ASRUtils::is_fixed_size_array(ASRUtils::symbol_type(x.m_m)) ) {
                is_dependent_only_on_argument = false;
            }
        }

        // Function calls returning non-primitive types (allocatable strings,
        // arrays, structs, etc.) will be converted to subroutine calls by
        // subroutine_from_function, replacing the call expression with a
        // local return-slot variable. After that transformation the dimension
        // no longer depends only on function arguments, so treat it as such.
        bool returns_non_primitive_type(ASR::ttype_t* t);

        void visit_IntrinsicElementalFunction(const ASR::IntrinsicElementalFunction_t& x) {
            if (returns_non_primitive_type(x.m_type)) {
                is_dependent_only_on_argument = false;
            } else {
                ASR::BaseWalkVisitor<ExprDependentOnlyOnArguments>::visit_IntrinsicElementalFunction(x);
            }
        }

        void visit_FunctionCall(const ASR::FunctionCall_t& x) {
            if (returns_non_primitive_type(x.m_type)) {
                is_dependent_only_on_argument = false;
            } else {
                ASR::BaseWalkVisitor<ExprDependentOnlyOnArguments>::visit_FunctionCall(x);
            }
        }
};

class ExprReferencesSymbolVisitor:
    public ASR::BaseWalkVisitor<ExprReferencesSymbolVisitor> {
public:
    ASR::symbol_t* target_sym;
    bool found;

    ExprReferencesSymbolVisitor(ASR::symbol_t* sym) :
        target_sym(sym), found(false) {}

    void visit_Var(const ASR::Var_t& x) {
        if (x.m_v == target_sym) {
            found = true;
        }
    }
};

static inline bool expr_references_symbol(ASR::expr_t* expr, ASR::symbol_t* sym) {
    if (!expr) return false;
    ExprReferencesSymbolVisitor visitor(sym);
    visitor.visit_expr(*expr);
    return visitor.found;
}

// Output implied-do loops whose bounds are all integer literals are expanded
// at compile time in full. Bounds that are only folded constants (a named
// constant, `size()` of a fixed size array) are often large, and every
// iteration costs compile time and code size, so such loops are expanded
// only up to this many iterations in total, nested loops included; the
// `implied_do_loops` pass keeps the run-time loop for larger ones.
constexpr uint64_t implied_do_loop_folded_bounds_expansion_limit = 256;

// The largest number of values into which an output implied-do loop with
// values of two or more different derived types is expanded at compile
// time. All expanded values are passed to one runtime call, each struct
// value through its own temporary, so compile time grows faster than
// linearly with their number: 1000 values compile in about half a second,
// 4000 in about 10 seconds and 8000 in over a minute. Real output lists are
// far shorter.
constexpr uint64_t implied_do_loop_expansion_limit = 1000;

// What keeps an output implied-do loop from being expanded at compile time
// into the list of its values.
enum class ImpliedDoExpansionBlocker {
    None,
    // The start, end or step of the loop or of a nested loop has no constant
    // value once the variables of the enclosing loops are replaced by theirs.
    RunTimeBounds,
    // A nested loop has the variable of an enclosing loop as its variable
    // (not conforming, but accepted).
    ReusedVariable,
    // The bounds are not all integer literals and the loop has more than
    // `implied_do_loop_folded_bounds_expansion_limit` iterations, those of
    // its nested loops included.
    TooManyIterations,
    // The loop has values of two or more different derived types and more
    // than `implied_do_loop_expansion_limit` values, those of its nested
    // loops included.
    TooManyValues,
};

struct ImpliedDoExpansion {
    ImpliedDoExpansionBlocker blocker;
    // The bound, step or nested loop that blocks the expansion, if any.
    ASR::expr_t* culprit;
};

// Expands the output implied-do loop `idl` at compile time: appends its
// values for each iteration to `result`, with the variables of the loops
// replaced by their values, and returns `ImpliedDoExpansionBlocker::None`.
// Otherwise returns what keeps it from being expanded, and `result` is
// incomplete. The `implied_do_loops` pass expands loops with values of
// different types this way; the semantic checks use it to reject the loops
// over derived types that cannot be expanded.
ImpliedDoExpansion expand_implied_do_loop(Allocator &al,
    ASR::ImpliedDoLoop_t* idl, Vec<ASR::expr_t*> &result);

// Whether an implied-do loop of the input/output list `values` contains
// another one.
static inline bool has_nested_implied_do_loop(ASR::expr_t** values, size_t n_values) {
    for (size_t i = 0; i < n_values; i++) {
        if (!ASR::is_a<ASR::ImpliedDoLoop_t>(*values[i])) continue;
        ASR::ImpliedDoLoop_t* idl = ASR::down_cast<ASR::ImpliedDoLoop_t>(values[i]);
        for (size_t j = 0; j < idl->n_values; j++) {
            if (ASR::is_a<ASR::ImpliedDoLoop_t>(*idl->m_values[j])) return true;
        }
    }
    return false;
}

// How many loops of the perfect loop nest `loops`, from the outermost one,
// form a rectangular iteration space: the bounds of each of them read none
// of the indices of the loops around it in the nest. A collapsed iteration
// space is described by bounds evaluated before any iteration runs, so a
// loop whose bounds depend on an enclosing index (a non-rectangular nest,
// e.g. `do j = 1, n; do i = j, n`) cannot be part of it.
size_t rectangular_loop_nest_depth(
        const std::vector<ASR::DoLoop_t*> &loops);

// This replacer is used for replacing FunctionParam in expressions by the arguments which are passed in.
// To be used when creating FunctionCall or SubroutineCall.
class ReplaceFunctionParamWithArg: public ASR::BaseExprReplacer<ReplaceFunctionParamWithArg> {
    private:
    Allocator& al;
    ASR::call_arg_t* m_args;
    // Only read by an assert, which is compiled out of a release build.
    [[maybe_unused]] size_t n_args;
    std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> replacements;

    public:
    ReplaceFunctionParamWithArg(Allocator& al_, ASR::call_arg_t* m_args_, size_t n_args_) :
        al(al_), m_args(m_args_), n_args(n_args_) {}

    void replace_FunctionParam(ASR::FunctionParam_t *x) {
        if (current_expr) {
            size_t n = x->m_param_number;
            LCOMPILERS_ASSERT_MSG(n < n_args,"FunctionParam param number not in range.");
            
            replacements.push_back({current_expr, *current_expr});
            *current_expr = m_args[n].m_value;
        }
    }

    ASR::expr_t* replace_FunctionParam_with_arg(ASR::expr_t* t);

    void replace_type(ASR::ttype_t* t);
};

bool is_dimension_dependent_only_on_arguments(ASR::dimension_t* m_dims, size_t n_dims, bool only_intent_in_args=false,
    bool allow_deferred_shape_members=false);

static inline bool is_dimension_dependent_only_on_arguments(ASR::ttype_t* type, bool only_intent_in_args=false) {
    ASR::dimension_t* m_dims;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(type, m_dims);
    return is_dimension_dependent_only_on_arguments(m_dims, n_dims, only_intent_in_args);
}

bool is_binop_expr(ASR::expr_t* x);

bool is_unaryop_expr(ASR::expr_t* x);

ASR::expr_t* extract_member_from_unaryop(ASR::expr_t* x);

ASR::expr_t* extract_member_from_binop(ASR::expr_t* x, int8_t member);

size_t get_constant_ArrayConstant_size(ASR::ArrayConstant_t* x);

ASR::expr_t* get_compile_time_array_size(Allocator& al, ASR::ttype_t* array_type);
ASR::expr_t* get_expr_size_expr(ASR::expr_t* x, bool inside_binop = false);
ASR::expr_t* get_ArrayConstant_size(Allocator& al, ASR::ArrayConstant_t* x);

ASR::expr_t* get_ImpliedDoLoop_size(Allocator& al, ASR::ImpliedDoLoop_t* implied_doloop);

ASR::expr_t* get_ArrayConstructor_size(Allocator& al, ASR::ArrayConstructor_t* x);

ASR::asr_t* make_Assignment_t_util(Allocator &al, const Location &a_loc,
    ASR::expr_t* a_target, ASR::expr_t* a_value,
    ASR::stmt_t* a_overloaded, bool a_realloc_lhs, bool a_move,
    bool a_new_target = false, bool a_new_allocation = false);

// Return `head` in the normalized form required of every counted DO-loop
// head (`DoLoop`, each head of `DoConcurrentLoop` and `ForAllSingle`):
// `m_start`, `m_end` and `m_increment` have exactly the type of the DO
// variable `m_v` (explicit `Cast` nodes are inserted where needed) and a
// missing `m_increment` becomes 1 (located at `loc`). A DO-forever head (all
// four fields absent) is returned unchanged. A counted head must have a DO
// variable.
ASR::do_loop_head_t normalize_do_loop_head(Allocator &al,
    const Location &loc, const ASR::do_loop_head_t &head);

// Whether `head` is in the normalized form (see `normalize_do_loop_head`).
// On failure, `reason` describes the first violation.
bool is_normalized_do_loop_head(const ASR::do_loop_head_t &head,
    std::string &reason);

// `make_DoLoop_t` with a normalized head. Use it (or `ASRBuilder::DoLoop`)
// instead of `ASR::make_DoLoop_t` whenever a loop is created.
ASR::asr_t* make_DoLoop_t_util(Allocator &al, const Location &a_loc,
    char* a_name, const ASR::do_loop_head_t &a_head,
    ASR::stmt_t** a_body, size_t n_body,
    ASR::stmt_t** a_orelse, size_t n_orelse);

// `make_DoConcurrentLoop_t` with every head normalized in place.
ASR::asr_t* make_DoConcurrentLoop_t_util(Allocator &al, const Location &a_loc,
    ASR::do_loop_head_t* a_head, size_t n_head,
    ASR::expr_t** a_shared, size_t n_shared,
    ASR::expr_t** a_local, size_t n_local,
    ASR::reduction_expr_t* a_reduction, size_t n_reduction,
    ASR::stmt_t** a_body, size_t n_body);

// `make_ForAllSingle_t` with a normalized head.
ASR::asr_t* make_ForAllSingle_t_util(Allocator &al, const Location &a_loc,
    const ASR::do_loop_head_t &a_head, ASR::stmt_t* a_assign_stmt);

ASR::asr_t* make_ArraySize_t_util(
    Allocator &al, const Location &a_loc, ASR::expr_t* a_v,
    ASR::expr_t* a_dim, ASR::ttype_t* a_type, ASR::expr_t* a_value,
    bool for_type=true);

ASR::symbol_t* import_struct_type(Allocator& al,
    ASR::symbol_t* struct_sym, SymbolTable* scope);

// Whether `sym` can be named from `scope` without importing it first.
static inline bool is_visible_from(ASR::symbol_t* sym, SymbolTable* scope) {
    if (sym == nullptr) return true;
    SymbolTable* owner = symbol_parent_symtab(sym);
    for (SymbolTable* s = scope; s != nullptr; s = s->parent) {
        if (s == owner) return true;
    }
    return false;
}

// Whether `sym` is what its name stands for in `scope`, so that a lookup by
// name, as passes do, finds it.
static inline bool is_named_from(ASR::symbol_t* sym, SymbolTable* scope) {
    return scope->resolve_symbol(symbol_name(sym)) == sym;
}

// Returns a symbol that names the definition of `sym` from `scope`: `sym` or
// its definition when named from there, an existing import of the definition,
// or a new ExternalSymbol added to `scope` under a name that is free at this
// point and starts with `name_prefix` and ends with `name_suffix`. Returns
// `sym` unchanged when it cannot be imported because no module or derived
// type owns its definition.
ASR::symbol_t* import_symbol(Allocator &al, ASR::symbol_t* sym,
        SymbolTable* scope, ASR::accessType access=ASR::accessType::Private,
        const std::string &name_prefix="", const std::string &name_suffix="");

// The argument and result types of a procedure type can reference module
// symbols named only where its interface is declared, for example the getter
// of a use-associated explicit-shape bound. Imports them into `scope` (with
// `do_import`) or only records whether there are any.
class ProcedureTypeImporter: public ASR::BaseExprReplacer<ProcedureTypeImporter> {
    Allocator &al;
    SymbolTable* scope;
    bool do_import;

    // The import is made under a `1_`-prefixed name no user code can spell:
    // a name the user can spell could be taken by a later declaration or
    // capture a later implicit reference in the same scope.
    ASR::symbol_t* named(ASR::symbol_t* sym) {
        if (is_named_from(sym, scope)) return sym;
        needed = true;
        return do_import ? import_symbol(al, sym, scope,
            ASR::accessType::Private, "1_") : sym;
    }

public:
    bool needed = false;

    ProcedureTypeImporter(Allocator &al_, SymbolTable* scope_, bool do_import_)
        : al(al_), scope(scope_), do_import(do_import_) {}

    void replace_FunctionCall(ASR::FunctionCall_t* x) {
        ASR::BaseExprReplacer<ProcedureTypeImporter>::replace_FunctionCall(x);
        x->m_name = named(x->m_name);
        if (x->m_original_name) x->m_original_name = named(x->m_original_name);
    }

    void replace_Var(ASR::Var_t* x) {
        x->m_v = named(x->m_v);
    }

    // The member types of a derived type belong to its Struct.
    void replace_StructType(ASR::StructType_t* /*x*/) {}
};

// Returns `type` if it is not a procedure type or references only symbols
// named from `scope`, otherwise a copy that references them through imports.
ASR::ttype_t* import_procedure_type(Allocator &al,
        ASR::ttype_t* type, SymbolTable* scope);

// A variable's type declaration has to be visible from the variable's own
// scope, the same way its name is. A producer that carries a type over from
// somewhere else -- the frontend declaring an entity of a type it imported,
// or a pass building a temporary from an existing expression -- would
// otherwise leave the variable naming a type nothing in its scope can reach.
// Import it here, once, rather than at every one of those producers.
ASR::symbol_t* import_type_declaration(Allocator &al,
        ASR::symbol_t* type_declaration, SymbolTable* scope);

void set_cptr_type_declaration(ASR::ttype_t* type,
        ASR::symbol_t* type_declaration);

ASR::asr_t* make_Variable_t_util(Allocator &al, const Location &a_loc,
    SymbolTable* a_parent_symtab, char* a_name, char** a_dependencies, size_t n_dependencies,
    ASR::intentType a_intent, ASR::expr_t* a_symbolic_value, ASR::expr_t* a_value, ASR::storage_typeType a_storage,
    ASR::ttype_t* a_type, ASR::symbol_t* a_type_declaration, ASR::abiType a_abi, ASR::accessType a_access, ASR::presenceType a_presence,
    bool a_value_attr, bool a_target_attr = false, bool a_contiguous_attr = false,
    char* a_bindc_name=nullptr, bool a_is_volatile = false, bool a_is_asynchronous = false,
    bool a_is_protected = false,
    ASR::pass_attrType a_pass_attr = ASR::pass_attrType::NotMethod, char* a_self_argument = nullptr,
    ASR::codimension_t* a_codims = nullptr, size_t n_codims = 0,
    ASR::symbol_t* a_common_block = nullptr
);

ASR::ttype_t* make_Array_t_util(Allocator& al, const Location& loc,
    ASR::ttype_t* type, ASR::dimension_t* m_dims, size_t n_dims,
    ASR::abiType abi=ASR::abiType::Source, bool is_argument=false,
    ASR::array_physical_typeType physical_type=ASR::array_physical_typeType::DescriptorArray,
    bool override_physical_type=false, bool is_dimension_star=false, bool for_type=true,
    ASR::memory_spaceType memory_space=ASR::memory_spaceType::Global);

ASR::ttype_t* make_StructType_t_util(Allocator& al,
                                                 Location loc,
                                                 ASR::symbol_t* derived_type_sym,
                                                 bool is_cstruct);

// The type of an instance of the derived type `struct_sym`, which is not
// polymorphic (`asr.verify.struct.signature_is_instance_type`).
static inline ASR::ttype_t* get_struct_signature(ASR::symbol_t* struct_sym) {
    return ASR::down_cast<ASR::Struct_t>(
        symbol_get_past_external(struct_sym))->m_struct_signature;
}

// A `class(t)` declaration of a deferred type argument (C706 of the Fortran 2028
// working draft J3/26-007r1) is recorded as an ASR::TypeParameter_t whose
// `is_class` is set. Substituting such a type parameter with a derived type has
// to yield the polymorphic ASR::StructType_t that `class(x)` yields for a
// concrete type `x`; otherwise the instantiated entity would be a
// non-polymorphic variable of a possibly abstract type. `subs` is the
// substituted type and `subs_sym` the derived type symbol it names; both are
// returned unchanged when the type parameter was not used in a CLASS
// declaration or when the substituted type is not of derived type.
ASR::ttype_t* substitute_class_type_parameter(Allocator& al,
                                                 ASR::TypeParameter_t* param,
                                                 ASR::ttype_t* subs,
                                                 ASR::symbol_t* subs_sym);

// Sets the dimension member of `ttype_t`. Returns `true` if dimensions set.
// Returns `false` if the `ttype_t` does not have a dimension member.
bool ttype_set_dimensions(ASR::ttype_t** x,
            ASR::dimension_t *m_dims, int64_t n_dims,
            Allocator& al, ASR::abiType abi=ASR::abiType::Source,
            bool is_argument=false, bool is_dimension_star=false);

bool is_aggregate_type(ASR::ttype_t* asr_type);

ASR::dimension_t* duplicate_dimensions(Allocator& al, ASR::dimension_t* m_dims, size_t n_dims);

// Fortran array-valued complex part designators (%re, %im) have default
// lower bound 1; preserve each dimension's length from the base array.
Vec<ASR::dimension_t> make_complex_dimensions_bounds(Allocator& al,
        const Location &loc, ASR::dimension_t* m_dims, int n_dims);

ASR::ttype_t* duplicate_type(Allocator& al, const ASR::ttype_t* t,
    Vec<ASR::dimension_t>* dims=nullptr,
    ASR::array_physical_typeType physical_type=ASR::array_physical_typeType::DescriptorArray,
    bool override_physical_type=false);


/*
    -- This class is responsible of replacing FuncParam in Function's type
        into the actual argument passed in the FunctionCall. --
    -------------------------------------------------------------------------
    * - It duplicates the FunctionReturn type; No need to duplicate it yourself.
    * - Only used through static `replace` to prevent further usages.
    * - If used frequently, You can create a static instance in `replace` and reuse it instaed.
    --------------------------------------------------------------------------
    * EXAMPLE - Length Of String Type:
    
    (String ( FunctionParam 0 (Integer 4) ) ExpressionLength DescriptorString )
        |
        INTO
        |
        V
    (String ( var 1 arg )                   ExpressionLength DescriptorString )
*/
class FuncParamToArgReplacer : public ASR::BaseExprReplacer<FuncParamToArgReplacer> {
    private :
    ASR::FunctionCall_t* f_call_;
    Allocator &al_;
    FuncParamToArgReplacer(Allocator& al, ASR::FunctionCall_t* f_call): f_call_(f_call), al_(al) {}
    void replace_(){
        /* 
            Duplicate, As we assume passed type is Function's node;
            If not duplicated, Changes will reflect on original node.
        */
        ASR::ttype_t* const f_call_t_dup = ASRUtils::duplicate_type(al_, f_call_->m_type);
        f_call_->m_type = f_call_t_dup;
        replace_ttype(f_call_->m_type);

    }
    public : 
    void replace_FunctionParam(ASR::FunctionParam_t* x){
        /* Replace FuncParam With Correspondent Arg */
        LCOMPILERS_ASSERT(x->m_param_number < (int64_t)f_call_->n_args)
        *current_expr = f_call_->m_args[x->m_param_number].m_value;
    }

    inline static void replace(Allocator& al, ASR::FunctionCall_t* f_call){
        auto instance = FuncParamToArgReplacer(al, f_call);
        instance.replace_();
    }
};
void set_absent_optional_arguments_to_null(
    Vec<ASR::call_arg_t>& args, ASR::Function_t* func, Allocator& al,
    ASR::expr_t* dt=nullptr, bool nopass = false);

// Check if the passed ttype node is character type node of
// physical type `DescriptorString`.
static inline bool is_descriptorString(ASR::ttype_t* t){
    return is_character(*t) &&
        ASR::down_cast<ASR::String_t>(
        extract_type(t))->m_physical_type == ASR::string_physical_typeType::DescriptorString;
}

static inline bool is_deferredLength_string(ASR::ttype_t* t){
    ASR::ttype_t* type = ASRUtils::extract_type(t);
    if(ASR::is_a<ASR::String_t>(*type)){
        return ASR::down_cast<ASR::String_t>(type)->m_len_kind == ASR::DeferredLength;
    }
    return false;
}

bool is_allocatable_descriptor_string(ASR::ttype_t* t);

static inline ASR::String_t* get_string_type(ASR::ttype_t* s){
    LCOMPILERS_ASSERT(is_character(*s))
    return ASR::down_cast<ASR::String_t>(ASRUtils::extract_type(s));
}

static inline ASR::String_t* get_string_type(ASR::expr_t* s){
    LCOMPILERS_ASSERT(is_character(*expr_type(s)))
    return ASR::down_cast<ASR::String_t>(extract_type(expr_type(s)));
}

/*
    A character member of a bind(C) or SEQUENCE struct is laid out inline in
    the struct (a flat byte blob at a fixed offset) instead of behind a string
    descriptor, so that the struct matches the C/Fortran storage layout.
    Pointer and allocatable members keep their descriptor.
*/
static inline bool is_inline_character_struct_member(
        ASR::Struct_t* struct_type, ASR::ttype_t* member_type) {
    return (struct_type->m_abi == ASR::abiType::BindC
            || struct_type->m_is_sequence)
        && !ASR::is_a<ASR::Pointer_t>(*member_type)
        && !ASR::is_a<ASR::Allocatable_t>(*member_type)
        && ASR::is_a<ASR::String_t>(*type_get_past_array(member_type));
}

/*
    Total number of bytes a character member stored inline occupies in the
    struct: the element length times the kind times the number of elements
    (1 for a scalar member).
*/
int64_t inline_character_storage_size(ASR::ttype_t* member_type);

// Same as above, for an expression that may be a struct member access.
bool is_inline_character_struct_member(ASR::expr_t* expr);

/*
    Checks if type is a string.
    If array of strings, Returns false.
*/
static inline bool is_string_only(ASR::ttype_t* type){
    return ASR::is_a<ASR::String_t>(*type_get_past_allocatable_pointer(type));
}

/*
    Check if type is an array of strings
*/
static inline bool is_array_of_strings(ASR::ttype_t* type){
    return is_array(type) &&
           ASR::is_a<ASR::String_t>(*extract_type(type));
}

// Whether `t` is an assumed-size array of strings, represented by the
// descriptor of its data and its length (which has an address like a
// variable, see nested_vars)
static inline bool is_assumed_size_string_array(ASR::ttype_t *t) {
    return is_array(t) && extract_physical_type(t) ==
            ASR::array_physical_typeType::UnboundedPointerArray &&
        is_character(*extract_type(t)) &&
        get_string_type(t)->m_physical_type == ASR::DescriptorString;
}

/*
 * Makes sure to cast the string to the required physical type.
 * If the string is already of the required physical type, it returns the same string
 * PointerString -> Needs to have information about the length of the string
*/
ASR::expr_t* create_string_physical_cast(Allocator& al, ASR::expr_t* string,
        ASR::string_physical_typeType to, bool make_allocatable = false);
static inline bool is_character_phsyical_types_matched(ASR::ttype_t* x, ASR::ttype_t* y){
    LCOMPILERS_ASSERT(ASR::is_a<ASR::String_t>(*x) && ASR::is_a<ASR::String_t>(*y));
    ASR::String_t* x_str = ASR::down_cast<ASR::String_t>(x);
    ASR::String_t* y_str = ASR::down_cast<ASR::String_t>(y);
    return x_str->m_physical_type == y_str->m_physical_type;
}


ASR::ttype_t* duplicate_type_with_empty_dims(Allocator& al, ASR::ttype_t* t,
    ASR::array_physical_typeType physical_type=ASR::array_physical_typeType::DescriptorArray,
    bool override_physical_type=false);

ASR::ttype_t* duplicate_type_without_dims(Allocator& al, const ASR::ttype_t* t, const Location& loc);

inline bool is_array_physically_descriptor(ASR::ttype_t* t){
    LCOMPILERS_ASSERT(t)
    ASR::ttype_t* const t_past = ASRUtils::type_get_past_allocatable_pointer(t);
    LCOMPILERS_ASSERT(ASRUtils::is_array_t(t_past))
    return ASRUtils::extract_physical_type(t_past) == ASR::DescriptorArray
        || ASRUtils::extract_physical_type(t_past) == ASR::AssumedRankArray;
}

static inline ASR::asr_t* make_Allocatable_t_util(Allocator& al, const Location& loc, ASR::ttype_t* type) {
    if (is_assumed_rank_array(type)) {
        return ASR::make_Allocatable_t(al, loc, type);
    }
    return ASR::make_Allocatable_t(
        al, loc, duplicate_type_with_empty_dims(al, type));
}

inline std::string remove_trailing_white_spaces(std::string str) {
    size_t end = str.size();
    while (end > 0 && str[end - 1] == ' ') {
        end--;
    }
    return str.substr(0, end);
}

bool is_same_type_pointer(ASR::ttype_t* source, ASR::ttype_t* dest);

int extract_kind_str(char* m_n, char *&kind_str);

// True if `v` is a deferred constant, i.e. a named constant that is a
// deferred argument of a template, templated procedure or requirement. The
// symbol owning the scope that declares it is only created once its
// specification part has been visited, so it cannot be looked up while the
// template is being defined. A deferred argument is declared like a dummy
// argument of that scoping unit, though, which a named constant otherwise
// cannot be.
static inline bool is_deferred_constant(const ASR::Variable_t* v) {
    return v->m_storage == ASR::storage_typeType::Parameter
        && v->m_intent == ASR::intentType::Unspecified
        && v->m_value == nullptr && v->m_symbolic_value == nullptr;
}

// True if the named constant `v` has no value because its initializer uses a
// deferred constant, directly or through other such named constants.
bool depends_on_deferred_constant(const ASR::Variable_t* v);

// A deferred constant, or a named constant initialized with one, has no value
// until its template is instantiated, and a symbolic kind is not supported
// yet. With --continue-compilation, a named constant whose initializer is
// invalid has no value either.
template <typename SemanticAbort>
[[noreturn]] inline void report_valueless_kind_parameter(
        const ASR::Variable_t* kind_variable, const Location& loc,
        diag::Diagnostics &diag) {
    std::string name = kind_variable->m_name;
    std::string message;
    std::string label = "`" + name + "` has no value until the template is instantiated";
    if (is_deferred_constant(kind_variable)) {
        message = "using a deferred constant as a kind parameter is not supported yet";
    } else if (depends_on_deferred_constant(kind_variable)) {
        message = "using a named constant that depends on a deferred constant "
            "as a kind parameter is not supported yet";
    } else {
        message = "the kind parameter `" + name + "` is not a constant expression";
        label = "";
    }
    diag.add(diag::Diagnostic(message,
        diag::Level::Error, diag::Stage::Semantic, {
            diag::Label(label, {loc})}));
    throw SemanticAbort();
}

// Kind and length values are stored as int64_t in ASR, but are used as int.
template <typename SemanticAbort>
inline int constant_value_to_int(int64_t value, const std::string &what,
        const Location& loc, diag::Diagnostics &diag) {
    if (!integer_fits<int>(value)) {
        diag.add(diag::Diagnostic(
            what + " " + std::to_string(value) + " is out of range for integer(4)",
            diag::Level::Error, diag::Stage::Semantic, {
                diag::Label("", {loc})}));
        throw SemanticAbort();
    }
    return static_cast<int>(value);
}

// this function only extract's the 'kind' and raises an error when it's of
// inappropriate type (e.g. float), but doesn't ensure 'kind' is appropriate
// for whose kind it is
template <typename SemanticAbort>
inline int extract_kind(ASR::expr_t* kind_expr, const Location& loc, diag::Diagnostics &diag) {
    switch( kind_expr->type ) {
        case ASR::exprType::Var: {
            ASR::Var_t* kind_var = ASR::down_cast<ASR::Var_t>(kind_expr);
            ASR::Variable_t* kind_variable = ASR::down_cast<ASR::Variable_t>(
                    symbol_get_past_external(kind_var->m_v));
            bool is_parent_enum = false;
            if (kind_variable->m_parent_symtab->asr_owner != nullptr &&
                    ASR::is_a<ASR::symbol_t>(*kind_variable->m_parent_symtab->asr_owner)) {
                ASR::symbol_t *s = ASR::down_cast<ASR::symbol_t>(
                    kind_variable->m_parent_symtab->asr_owner);
                is_parent_enum = ASR::is_a<ASR::Enum_t>(*s);
            }
            if( is_parent_enum ) {
                return ASRUtils::extract_kind_from_ttype_t(kind_variable->m_type);
            } else if( kind_variable->m_storage == ASR::storage_typeType::Parameter ) {
                if( kind_variable->m_type->type == ASR::ttypeType::Integer ) {
                    if( kind_variable->m_value == nullptr ) {
                        report_valueless_kind_parameter<SemanticAbort>(
                            kind_variable, loc, diag);
                    }
                    return constant_value_to_int<SemanticAbort>(
                        ASR::down_cast<ASR::IntegerConstant_t>(kind_variable->m_value)->m_n,
                        "kind value", loc, diag);
                } else {
                    diag.add(diag::Diagnostic(
                        "Integer variable required. " + std::string(kind_variable->m_name) +
                                    " is not an Integer variable.",
                        diag::Level::Error, diag::Stage::Semantic, {
                            diag::Label("", {loc})}));
                    throw SemanticAbort();
                }
            } else {
                diag.add(diag::Diagnostic(
                    "Parameter '" + std::string(kind_variable->m_name) +
                    "' is a variable, which does not reduce to a constant expression",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {loc})}));
                throw SemanticAbort();
            }
        }
        case ASR::exprType::IntrinsicElementalFunction: {
            ASR::IntrinsicElementalFunction_t* kind_isf =
                ASR::down_cast<ASR::IntrinsicElementalFunction_t>(kind_expr);
            if ( kind_isf->m_value &&
                    ASR::is_a<ASR::IntegerConstant_t>(*kind_isf->m_value) ) {
                return constant_value_to_int<SemanticAbort>(
                    ASR::down_cast<ASR::IntegerConstant_t>(kind_isf->m_value)->m_n,
                    "kind value", loc, diag);
            } else {
                diag.add(diag::Diagnostic(
                    "Only Integer literals or expressions which "
                    "reduce to constant Integer are accepted as kind parameters",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {loc})}));
                throw SemanticAbort();
            }
            break;
        }
        case ASR::exprType::TypeInquiry: {
            ASR::TypeInquiry_t* kind_ti =
                ASR::down_cast<ASR::TypeInquiry_t>(kind_expr);
            if (kind_ti->m_value) {
                return constant_value_to_int<SemanticAbort>(
                    ASR::down_cast<ASR::IntegerConstant_t>(kind_ti->m_value)->m_n,
                    "kind value", loc, diag);
            } else {
                diag.add(diag::Diagnostic(
                    "Only Integer literals or expressions which "
                    "reduce to constant Integer are accepted as kind parameters",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {loc})}));
                throw SemanticAbort();
            }
            break;
        }
        case ASR::exprType::BitCast: {
            ASR::BitCast_t* kind_bc = ASR::down_cast<ASR::BitCast_t>(kind_expr);
            if (kind_bc->m_value) {
                return constant_value_to_int<SemanticAbort>(
                    ASR::down_cast<ASR::IntegerConstant_t>(kind_bc->m_value)->m_n,
                    "kind value", loc, diag);
            } else {
                diag.add(diag::Diagnostic(
                    "Only Integer literals or expressions which "
                    "reduce to constant Integer are accepted as kind parameters",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {loc})}));
                throw SemanticAbort();
            }
            break;
        }
        // allow integer binary operator kinds (e.g. '1 + 7')
        case ASR::exprType::IntegerBinOp:
        // allow integer kinds (e.g. 4, 8 etc.)
        case ASR::exprType::IntegerConstant: {
            int a_kind = -1;
            if (!ASRUtils::extract_value(kind_expr, a_kind)) {
                // we still need to ensure that values are constant
                // e.g. "integer :: a = 4; real(1*a) :: x" is an invalid kind,
                // as 'a' isn't a constant.
                // ToDo: we should raise a better error, by "locating" just
                // 'a' as well, instead of the whole '1*a'
                diag.add(diag::Diagnostic(
                    "Only Integer literals or expressions which "
                    "reduce to constant Integer are accepted as kind parameters",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("", {loc})}));
                throw SemanticAbort();
            }
            return a_kind;
        }
        // make sure not to allow kind having "RealConstant" (e.g. 4.0),
        // and everything else
        default: {
            diag.add(diag::Diagnostic(
                "Only Integer literals or expressions which "
                "reduce to constant Integer are accepted as kind parameters",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {loc})}));
            throw SemanticAbort();
        }
    }
}

template <typename SemanticAbort>
inline int extract_len(ASR::expr_t* len_expr, const Location& loc, diag::Diagnostics &diag) {
    int a_len = -1;
    switch( len_expr->type ) {
        case ASR::exprType::IntegerConstant: {
            a_len = constant_value_to_int<SemanticAbort>(
                ASR::down_cast<ASR::IntegerConstant_t>(len_expr)->m_n,
                "character length", loc, diag);
            break;
        }
        case ASR::exprType::Var: {
            ASR::Var_t* len_var =
                ASR::down_cast<ASR::Var_t>(len_expr);
            ASR::Variable_t* len_variable =
                ASR::down_cast<ASR::Variable_t>(
                    symbol_get_past_external(len_var->m_v));
            if( len_variable->m_storage == ASR::storage_typeType::Parameter ) {
                if( len_variable->m_type->type == ASR::ttypeType::Integer ) {
                    LCOMPILERS_ASSERT( len_variable->m_value != nullptr );
                    a_len = constant_value_to_int<SemanticAbort>(
                        ASR::down_cast<ASR::IntegerConstant_t>(len_variable->m_value)->m_n,
                        "character length", loc, diag);
                } else {
                    diag.add(diag::Diagnostic(
                        "Integer variable required. " + std::string(len_variable->m_name) +
                        " is not an Integer variable",
                        diag::Level::Error, diag::Stage::Semantic, {
                            diag::Label("", {loc})}));
                    throw SemanticAbort();
                }
            }
            break;
        }
        default: {}
    }
    return a_len;
}

inline bool is_parent(SymbolTable* a, SymbolTable* b) {
    SymbolTable* current_parent = b->parent;
    while( current_parent ) {
        if( current_parent == a ) {
            return true;
        }
        current_parent = current_parent->parent;
    }
    return false;
}

// Enumerate the allocatable array components of `st`, including the ones it
// inherits from the types it extends.  Components declared by an ancestor are
// listed before the type's own, matching the order in which the type layers
// are laid out.  `visited` guards against a malformed cyclic parent chain.
void collect_allocatable_array_members(ASR::Struct_t* st,
        std::vector<std::pair<std::string, ASR::Variable_t*>>& members,
        std::set<ASR::Struct_t*>& visited);

inline std::vector<std::pair<std::string, ASR::Variable_t*>>
collect_allocatable_array_members(ASR::Struct_t* st) {
    std::vector<std::pair<std::string, ASR::Variable_t*>> members;
    std::set<ASR::Struct_t*> visited;
    collect_allocatable_array_members(st, members, visited);
    return members;
}

bool is_parent(ASR::Struct_t* a, ASR::Struct_t* b);
/**
 * Check if two derived type definitions specify the same type: they are the
 * same definition, or (F2018 7.5.2.4) different definitions with the same
 * name that both have the SEQUENCE attribute or both have the BIND attribute
 * and whose components agree.
 */
inline bool is_same_derived_type(ASR::Struct_t* a, ASR::Struct_t* b) {
    if (a == b) {
        return true;
    }

    if (std::string(a->m_name) == std::string(b->m_name)) {
        if ((a->m_abi == ASR::abiType::BindC && b->m_abi == ASR::abiType::BindC) ||
            (a->m_is_sequence && b->m_is_sequence)) {
            return types_equal(a->m_struct_signature, b->m_struct_signature, nullptr, nullptr, false);
        }
    }

    return false;
}

/**
 * Check if two derived types are :
 * 1) the same, or
 * 2) one is parent of another, or
 * 3) both are unlimited polymorphic types
 */
inline bool is_derived_type_similar(ASR::Struct_t* a, ASR::Struct_t* b) {
    return is_same_derived_type(a, b) || is_parent(a, b) || is_parent(b, a) ||
        (is_unlimited_polymorphic_type(a) && is_unlimited_polymorphic_type(b));
}

// Can we pass this ARGUMENT of this derivedtype --> to this PARAMETER of this derivedtype?
inline bool can_pass_derviedtype_arg_to_parameter(ASR::Struct_t* arg, ASR::Struct_t* param) {
    return arg == param || is_parent(param, arg) || is_unlimited_polymorphic_type(param);
}

// Helper: check if IntegerBinOp is identity (x+0, 0+x, x-0)
// Returns the non-identity operand if so, nullptr otherwise
// Note: deliberately excludes x*1 to match GFortran behavior
ASR::expr_t* get_identity_operand(ASR::IntegerBinOp_t* binop);

// Compares two expressions for structural equality
// Handles: Var (by symbol), IntegerBinOp (recursive), constants
// Returns true for unknown types to avoid false negatives
bool expr_equal(ASR::expr_t* x, ASR::expr_t* y);

// Compares two dimension expressions for equality.
// Optionally allows skipping determination in certain cases.
bool dimension_expr_equal(
    ASR::expr_t* dim_a,
    ASR::expr_t* dim_b
);

inline bool dimensions_compatible(ASR::dimension_t* dims_a, size_t n_dims_a,
    ASR::dimension_t* dims_b, size_t n_dims_b,
    bool check_n_dims= true){

    if (check_n_dims && (n_dims_a != n_dims_b)) {
        return false;
    }
    int64_t total_a = get_fixed_size_of_array(dims_a,n_dims_a);
    int64_t total_b = get_fixed_size_of_array(dims_b,n_dims_b);
    // -1 means found dimension with no value at compile time, then return true anyway.
    return (total_a == -1) || (total_b == -1) || (total_a >= total_b);
}

bool cptr_type_declarations_match(ASR::ttype_t* left_type,
        ASR::ttype_t* right_type, ASR::expr_t* left_expr,
        ASR::expr_t* right_expr);

// Compares two ASR types for structural equality.
//
// Parameters:
//   a, b: The types to compare (required, but nullptr is allowed for both).
//   a_expr, b_expr: Optional expressions providing context for resolving symbol
//       references in complex types (StructType, UnionType, FunctionType).
//       - Non-null: Uses the expression to look up actual struct/union/function
//         symbols via get_struct_sym_from_struct_expr(), etc. Required when the
//         type nodes alone do not uniquely identify the declaration (e.g., two
//         StructType_t nodes referencing different struct declarations).
//       - Null: Falls back to structural comparison of the types fields directly
//         (compares m_is_unlimited_polymorphic, member types, etc.). Use null
//         when expression context is unavailable, such as in utility functions
//         like make_ArrayPhysicalCast_t_util() that operate on types alone.
//   check_for_dimensions: If true, also compare array dimensions.
//
// Returns true if the types are structurally equal.
bool types_equal(ASR::ttype_t *a, ASR::ttype_t *b, ASR::expr_t* a_expr, ASR::expr_t* b_expr,
    bool check_for_dimensions);

bool types_equal_with_substitution(ASR::ttype_t *a, ASR::ttype_t *b,
    std::map<std::string, std::pair<ASR::ttype_t*, ASR::symbol_t*>> subs, ASR::expr_t* a_expr, ASR::expr_t* b_expr,
    bool check_for_dimensions=false);

bool check_equal_type(ASR::ttype_t* x, ASR::ttype_t* y, ASR::expr_t* x_expr, ASR::expr_t* y_expr, bool check_for_dimensions);

bool check_class_assignment_compatibility(ASR::expr_t* target, ASR::expr_t* value);

bool check_class_assignment_compatibility(ASR::symbol_t* target, ASR::symbol_t* value);

// Check if the passed argument can be assigned to the parameter of a class type
inline bool can_pass_class_argument(ASR::expr_t* param_expr, ASR::expr_t* passed_arg) {
    if (is_unlimited_polymorphic_type(ASRUtils::expr_type(param_expr))) {
        return true;
    }
    return check_class_assignment_compatibility(param_expr, passed_arg);
}

// An unlimited polymorphic value is type compatible only with an unlimited
// polymorphic variable or dummy argument, although `types_equal` accepts it
// on either side.
inline bool is_unlimited_polymorphic_to_specific_type(ASR::ttype_t* variable_type,
        ASR::ttype_t* value_type) {
    return is_unlimited_polymorphic_type(value_type) &&
        !is_unlimited_polymorphic_type(variable_type) &&
        !is_type_parameter(*type_get_past_allocatable_pointer(variable_type));
}

bool is_elemental(ASR::symbol_t* x);


bool select_func_subrout(const ASR::symbol_t* proc, const Vec<ASR::call_arg_t>& args,
    Location& loc, const std::function<void (const std::string &, const Location &)> err);

template <typename T>
int select_generic_procedure(const Vec<ASR::call_arg_t> &args,
    const T &p, Location loc,
    const std::function<void (const std::string &, const Location &)> err,
    bool raise_error=true, bool is_dt_present=false) {
    // When `is_dt_present` is true, `args[0]` is the passed-object (the `dt`
    // of a type-bound procedure call). A `nopass` specific procedure does not
    // receive the passed-object, so it must be matched against the arguments
    // excluding `args[0]`.
    auto matches = [&](ASR::symbol_t* proc_sym) -> bool {
        if( ASR::is_a<ASR::StructMethodDeclaration_t>(*proc_sym) ) {
            ASR::StructMethodDeclaration_t *clss_fn
                = ASR::down_cast<ASR::StructMethodDeclaration_t>(proc_sym);
            const ASR::symbol_t *proc = ASRUtils::symbol_get_past_external(clss_fn->m_proc);
            if( is_dt_present && clss_fn->m_is_nopass && args.n >= 1 ) {
                Vec<ASR::call_arg_t> args_no_dt;
                args_no_dt.from_pointer_n(args.p + 1, args.n - 1);
                return select_func_subrout(proc, args_no_dt, loc, err);
            }
            return select_func_subrout(proc, args, loc, err);
        } else {
            return select_func_subrout(proc_sym, args, loc, err);
        }
    };
    for (size_t i=0; i < p.n_procs; i++) {
        if (is_elemental(p.m_procs[i])) {     // Prioritize direct arg matching, then look for elemental
            continue;
        }
        if( matches(p.m_procs[i]) ) {
            return checked_integer_cast<int>(i);
        }
    }
    for (size_t i=0; i < p.n_procs; i++) {
        if (!is_elemental(p.m_procs[i])) {
            continue;
        }
        if( matches(p.m_procs[i]) ) {
            return checked_integer_cast<int>(i);
        }
    }
    if( raise_error ) {
        err("Arguments do not match for any generic procedure, " + std::string(p.m_name), loc);
    }
    return -1;
}

ASR::asr_t* symbol_resolve_external_generic_procedure_without_eval(
            const Location &loc,
            ASR::symbol_t *v, Vec<ASR::call_arg_t>& args,
            SymbolTable* current_scope, Allocator& al,
            const std::function<void (const std::string &, const Location &)> err);

static inline ASR::storage_typeType symbol_StorageType(const ASR::symbol_t* s){
    switch( s->type ) {
        case ASR::symbolType::Variable: {
            return ASR::down_cast<ASR::Variable_t>(s)->m_storage;
        }
        default: {
            throw LCompilersException("Cannot return storage type of, " +
                                    std::to_string(s->type) + " symbol.");
        }
    }
}

static inline ASR::intentType symbol_intent(const ASR::symbol_t *f)
{
    switch( f->type ) {
        case ASR::symbolType::Variable: {
            return ASR::down_cast<ASR::Variable_t>(f)->m_intent;
        }
        default: {
            throw LCompilersException("Cannot return intent of, " +
                                    std::to_string(f->type) + " symbol.");
        }
    }
    return ASR::intentType::Unspecified;
}

static inline ASR::intentType expr_intent(ASR::expr_t* expr) {
    switch( expr->type ) {
        case ASR::exprType::Var: {
            return ASRUtils::symbol_intent(ASR::down_cast<ASR::Var_t>(expr)->m_v);
        }
        default: {
            throw LCompilersException("Cannot extract intent of ASR::exprType::" +
                ASRUtils::type_to_str_python_expr(ASRUtils::expr_type(expr), expr));
        }
    }
    return ASR::intentType::Unspecified;
}

static inline bool is_data_only_array(ASR::ttype_t* type, ASR::abiType abi) {
    ASR::dimension_t* m_dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(type, m_dims);
    if( n_dims == 0 ) {
        return false;
    }
    return (abi == ASR::abiType::BindC || !ASRUtils::is_dimension_empty(m_dims, n_dims));
}

void insert_module_dependency(ASR::symbol_t* a,
    Allocator& al, SetChar& module_dependencies);

ASR::ttype_t* get_type_parameter(ASR::ttype_t* t);

// If the derived type of the component that the ExternalSymbol `e` refers to
// is defined in a module, directly or in a procedure or block of it, names
// that module in `e` and the path from it to the type (e.g. `[run, t]`) in its
// scope names, so that deserialization does not resolve the type by a name
// that something else may stand for in `e`'s scope, e.g. a module of the same
// name or a dummy argument. Returns false, leaving `e` unchanged, if there is
// no such module, or if an enclosing procedure is still being declared and so
// has no symbol yet.
bool set_struct_member_owner_path(Allocator& al,
        ASR::ExternalSymbol_t* e);

// An ExternalSymbol `name` in `scope` for the component `struct_member`,
// naming its type by a path from a module (set_struct_member_owner_path), or
// otherwise by `struct_local_name`, the type's name in `scope`.
ASR::symbol_t* make_struct_member_external_symbol(Allocator& al,
        const Location& loc, SymbolTable* scope, const std::string& name,
        ASR::symbol_t* struct_member, const std::string& struct_local_name,
        ASR::accessType access);

ASR::symbol_t* import_struct_instance_member(Allocator& al,
                                                           ASR::symbol_t* struct_member,
                                                           SymbolTable* scope);

ASR::symbol_t* import_enum_member(Allocator& al, ASR::symbol_t* v,
    SymbolTable* scope);

// Import a Struct type from an external module into the target scope.
// Returns the ExternalSymbol for the Struct, or the original symbol if local.
ASR::symbol_t* import_struct_type(Allocator& al, ASR::symbol_t* struct_sym,
    SymbolTable* scope);

// Recursively process a default initialization expression and externalize
// any Struct references that are from external modules.
ASR::expr_t* externalize_struct_refs_in_init(Allocator& al,
    ASR::expr_t* init_expr, SymbolTable* scope);

class ReplaceArgVisitor: public ASR::BaseExprReplacer<ReplaceArgVisitor> {

    private:

    Allocator& al;

    SymbolTable* current_scope;

    ASR::Function_t* orig_func;

    Vec<ASR::call_arg_t>& orig_args;

    SetChar& current_function_dependencies;
    SetChar& current_module_dependencies;

    public:

    ReplaceArgVisitor(Allocator& al_, SymbolTable* current_scope_,
                      ASR::Function_t* orig_func_, Vec<ASR::call_arg_t>& orig_args_,
                      SetChar& current_function_dependencies_, SetChar& current_module_dependencies_) :
        al(al_), current_scope(current_scope_), orig_func(orig_func_),
        orig_args(orig_args_), current_function_dependencies(current_function_dependencies_),
        current_module_dependencies(current_module_dependencies_)
    {}

    void replace_FunctionCall(ASR::FunctionCall_t* x);

    void replace_Var(ASR::Var_t* x);

    void replace_StructInstanceMember(ASR::StructInstanceMember_t* x) {
        ASR::BaseExprReplacer<ReplaceArgVisitor>::replace_StructInstanceMember(x);

        ASR::symbol_t* m_m = x->m_m;
        SymbolTable* m_m_scope = ASRUtils::symbol_parent_symtab(m_m);
        if (m_m_scope->get_counter() != current_scope->get_counter()) {
            ASR::symbol_t* imported = import_struct_instance_member(al, m_m, current_scope);
            if (imported) {
                x->m_m = imported;
            }
        }
    }

    void replace_ArraySize(ASR::ArraySize_t* x) {
        ASR::BaseExprReplacer<ReplaceArgVisitor>::replace_ArraySize(x);
        if( ASR::is_a<ASR::ArraySection_t>(*x->m_v) ) {
            *current_expr = ASRUtils::EXPR(ASRUtils::make_ArraySize_t_util(
                al, x->base.base.loc, x->m_v, x->m_dim, x->m_type, x->m_value, true));
        }
    }

};

// Finds the argument index that is equal to `v`, otherwise -1.
inline int64_t lookup_var_index(ASR::expr_t **args, size_t n_args, ASR::Var_t *v) {
    ASR::symbol_t *s = v->m_v;
    for (size_t i = 0; i < n_args; i++) {
        if (ASR::down_cast<ASR::Var_t>(args[i])->m_v == s) {
            return i;
        }
    }
    return -1;
}


class ExprStmtWithScopeDuplicator: public ASR::BaseExprStmtDuplicator<ExprStmtWithScopeDuplicator>
{
    public:
    SymbolTable* current_scope;
    bool use_resolve_symbol = false;
    ExprStmtWithScopeDuplicator(Allocator &al, SymbolTable* current_scope): BaseExprStmtDuplicator(al), current_scope(current_scope) {}

    ASR::asr_t* duplicate_Var(ASR::Var_t* x) {
        std::string name = ASRUtils::symbol_name(x->m_v);
        ASR::symbol_t* m_v = use_resolve_symbol
            ? current_scope->resolve_symbol(name)
            : current_scope->get_symbol(name);
        if (m_v == nullptr) {
            return ASR::make_Var_t(al, x->base.base.loc, x->m_v);
        }
        return ASR::make_Var_t(al, x->base.base.loc, m_v);
    }

    ASR::asr_t* duplicate_FunctionPointerCast(ASR::FunctionPointerCast_t* x);

    ASR::asr_t* duplicate_AssociateBlockCall(ASR::AssociateBlockCall_t* x) {
        std::string name = ASRUtils::symbol_name(x->m_m);
        ASR::symbol_t* m_m = current_scope->get_symbol(name);
        if (m_m == nullptr) m_m = x->m_m;
        return ASR::make_AssociateBlockCall_t(al, x->base.base.loc, m_m);
    }

    ASR::asr_t* duplicate_BlockCall(ASR::BlockCall_t* x) {
        std::string name = ASRUtils::symbol_name(x->m_m);
        ASR::symbol_t* m_m = current_scope->get_symbol(name);
        if (m_m == nullptr) m_m = x->m_m;
        return ASR::make_BlockCall_t(al, x->base.base.loc, x->m_label, m_m);
    }

    ASR::asr_t* duplicate_StructInstanceMember(ASR::StructInstanceMember_t* x);

    ASR::asr_t* duplicate_FunctionCall(ASR::FunctionCall_t* x);

    ASR::asr_t* duplicate_SubroutineCall(ASR::SubroutineCall_t* x);

};


class ReplaceWithFunctionParamVisitor: public ASR::BaseExprReplacer<ReplaceWithFunctionParamVisitor> {

    private:

    Allocator& al;

    ASR::expr_t** m_args;

    size_t n_args;

    SymbolTable* current_scope;

    public:

    ReplaceWithFunctionParamVisitor(Allocator& al_, ASR::expr_t** m_args_, size_t n_args_) :
        al(al_), m_args(m_args_), n_args(n_args_), current_scope(nullptr) {}

    void replace_Var(ASR::Var_t* x);

    ASR::ttype_t* replace_args_with_FunctionParam(ASR::ttype_t* t, SymbolTable* current_scope);

};

ASR::asr_t* make_FunctionType_t_util(Allocator &al,
    const Location &a_loc, ASR::expr_t** a_args, size_t n_args,
    ASR::expr_t* a_return_var, ASR::abiType a_abi, ASR::deftypeType a_deftype,
    char* a_bindc_name, bool a_elemental, bool a_pure, bool a_module, bool a_inline,
    bool a_static,
    ASR::symbol_t** a_restrictions, size_t n_restrictions, bool a_is_restriction, SymbolTable* current_scope,
    ASR::exec_spaceType a_exec_space=ASR::exec_spaceType::Host);

inline ASR::asr_t* make_FunctionType_t_util(Allocator &al, const Location &a_loc,
    ASR::expr_t** a_args, size_t n_args, ASR::expr_t* a_return_var, ASR::FunctionType_t* ft, SymbolTable* current_scope) {
    return ASRUtils::make_FunctionType_t_util(al, a_loc, a_args, n_args, a_return_var,
        ft->m_abi, ft->m_deftype, ft->m_bindc_name, ft->m_elemental,
        ft->m_pure, ft->m_module, ft->m_inline, ft->m_static,
        ft->m_restrictions,
        ft->n_restrictions, ft->m_is_restriction, current_scope, ft->m_exec_space);
}

ASR::asr_t* make_Function_t_util(Allocator& al, const Location& loc,
    SymbolTable* m_symtab, char* m_name, char** m_dependencies, size_t n_dependencies,
    ASR::expr_t** a_args, size_t n_args, ASR::stmt_t** m_body, size_t n_body,
    ASR::expr_t* m_return_var, ASR::abiType m_abi, ASR::accessType m_access,
    ASR::deftypeType m_deftype, char* m_bindc_name, bool m_elemental, bool m_pure,
    bool m_module, bool m_inline, bool m_static,
    ASR::symbol_t** m_restrictions, size_t n_restrictions, bool m_is_restriction,
    bool m_deterministic, bool m_side_effect_free, char *m_c_header=nullptr, Location* m_start_name = nullptr,
    Location* m_end_name = nullptr,
    ASR::exec_spaceType m_exec_space = ASR::exec_spaceType::Host,
    bool m_recursive = true);


class SymbolDuplicator {

    private:

    Allocator& al;

    public:

    SymbolDuplicator(Allocator& al_):
    al(al_) {

    }

    void duplicate_SymbolTable(SymbolTable* symbol_table,
        SymbolTable* destination_symtab);

    // The type and the values of a copied variable may refer to other
    // variables of the same scope, e.g. `a(n)` or `character(len=len(s))`.
    // When such a variable is copied before the variables it refers to,
    // those references still point into the original scope, so copy them
    // again once every variable of the scope exists.
    void remap_local_references(
            const std::vector<std::pair<ASR::Variable_t*, ASR::Variable_t*>> &variables,
            SymbolTable* destination_symtab);

    void duplicate_symbol(ASR::symbol_t* symbol,
        SymbolTable* destination_symtab);

    // A copied Variable has storage of its own: when it was in a common
    // block, the copy of that block puts it back in, see
    // duplicate_CommonBlock.
    ASR::symbol_t* duplicate_Variable(ASR::Variable_t* variable,
        SymbolTable* destination_symtab);

    ASR::symbol_t* duplicate_ExternalSymbol(ASR::ExternalSymbol_t* external_symbol,
        SymbolTable* destination_symtab) {
            return ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                al, external_symbol->base.base.loc, destination_symtab,
                external_symbol->m_name, external_symbol->m_external,
                external_symbol->m_module_name, external_symbol->m_scope_names,
                external_symbol->n_scope_names, external_symbol->m_original_name,
                external_symbol->m_access));
    }

    ASR::symbol_t* duplicate_AssociateBlock(ASR::AssociateBlock_t* associate_block,
        SymbolTable* destination_symtab);

    // Re-duplicate the statements of a Block/AssociateBlock body inside
    // the new scope `new_symtab`, resolving every symbol reference
    // through the new scope chain. Returns false (leaving `new_body`
    // unusable) if any statement could not be duplicated.
    bool reduplicate_body_in_scope(SymbolTable *new_symtab,
            ASR::stmt_t **orig_body, size_t orig_n_body,
            Vec<ASR::stmt_t*> &new_body);

    // Recursively re-duplicate AssociateBlock and Block bodies with
    // scope awareness. duplicate_AssociateBlock and duplicate_Block use
    // a non-scoped duplicator, so Var references in the body still
    // point to original symbols. This walks through all AssociateBlocks
    // and Blocks (including nested ones) and re-duplicates from the
    // original bodies using a scoped duplicator that resolves symbols
    // through the new scope chain.
    // A variable copied from `orig_scope` into `new_scope` whose type is
    // declared by a symbol of `orig_scope` (e.g. a procedure variable
    // declared with an interface of the same procedure) is declared by the
    // copy of that symbol, so the copy does not refer into the original.
    // The same holds for a variable of a scope nested in the copy (a BLOCK,
    // ASSOCIATE or contained procedure) declared by a symbol of any
    // enclosing copied scope. Such a variable also reads the copies of the
    // variables of the enclosing copied scopes in its type and values, such
    // as the extent `n` of `real :: w(n)` in a BLOCK of a routine with the
    // dummy argument `n`: a nested scope is copied before the variables of
    // its enclosing scope may exist, so remap_local_references, which
    // resolves names in the variable's own scope, cannot do it.
    void fixup_local_type_declarations(SymbolTable *new_scope,
            SymbolTable *orig_scope) {
        std::vector<std::pair<SymbolTable*, SymbolTable*>> copied_scopes;
        fixup_type_declarations_in_scope(new_scope, orig_scope, copied_scopes);
    }

    void fixup_type_declarations_in_scope(SymbolTable *new_scope,
            SymbolTable *orig_scope,
            std::vector<std::pair<SymbolTable*, SymbolTable*>> &copied_scopes);

    void fixup_nested_block_bodies(SymbolTable *new_scope,
            SymbolTable *orig_scope);

    ASR::symbol_t* duplicate_Function(ASR::Function_t* function,
        SymbolTable* destination_symtab);

    ASR::symbol_t* duplicate_Module(ASR::Module_t* module_t,
        SymbolTable* destination_symtab);

    // A Requirement and a Template are a scope plus the deferred argument names
    // and the requirements they pull in. The names and the require_instantiation
    // nodes hold identifiers only, no symbols, so they are carried over as they
    // are -- the same way duplicate_Module carries m_dependencies.
    ASR::symbol_t* duplicate_Requirement(ASR::Requirement_t* requirement_t,
        SymbolTable* destination_symtab);

    ASR::symbol_t* duplicate_Template(ASR::Template_t* template_t,
        SymbolTable* destination_symtab);

    ASR::symbol_t* duplicate_Block(ASR::Block_t* block_t,
        SymbolTable* destination_symtab);

    ASR::symbol_t* duplicate_Struct(ASR::Struct_t* struct_type_t,
        SymbolTable* destination_symtab);
    ASR::symbol_t* duplicate_GenericProcedure(ASR::GenericProcedure_t* genericProcedure, SymbolTable* destination_symtab){
        return ASR::down_cast<ASR::symbol_t>(ASR::make_GenericProcedure_t(
            al, genericProcedure->base.base.loc, destination_symtab,
            genericProcedure->m_name, genericProcedure->m_procs,
            genericProcedure->n_procs, genericProcedure->m_access));
    }

    ASR::symbol_t* duplicate_CustomOperator(ASR::CustomOperator_t* customOperator,
        SymbolTable* destination_symtab) {
        return ASR::down_cast<ASR::symbol_t>(ASR::make_CustomOperator_t(
            al, customOperator->base.base.loc, destination_symtab,
            customOperator->m_name, customOperator->m_procs,
            customOperator->n_procs, customOperator->m_access));
    }

    ASR::symbol_t* duplicate_StructMethodDeclaration(
        ASR::StructMethodDeclaration_t* structMethod,
        SymbolTable* destination_symtab);

    ASR::symbol_t* duplicate_Namelist(ASR::Namelist_t* namelist,
        SymbolTable* destination_symtab);

    // The copy of a common block view is the view of the copies of its
    // variables, which are copied first where they are not yet, and which
    // the copy puts into the block.
    ASR::symbol_t* duplicate_CommonBlock(ASR::CommonBlock_t* common_block,
        SymbolTable* destination_symtab);

};

// This replacer is for checking if symbols in the input expression are accessible from current_scope.
// If they are accessible as external symbols then replace them by the corresponding ExternalSymbol.
// If atleast one symbol is not accessible then result is false.
class CheckSymbolReplacer: public ASR::BaseExprReplacer<CheckSymbolReplacer> {
    private:

    Allocator &al;
    bool result;
    SymbolTable* current_scope;
    SetChar& current_function_dependencies;

    public:
    CheckSymbolReplacer(Allocator &al_, SymbolTable* current_scope_, SetChar& current_function_dependencies_) :
        al(al_), result(true), current_scope(current_scope_), current_function_dependencies(current_function_dependencies_) {}

    ASR::symbol_t* handle_symbol(ASR::symbol_t* x);

    void replace_Var(ASR::Var_t *x) {
        x->m_v = handle_symbol(x->m_v);
        ADD_ASR_DEPENDENCIES(current_scope, x->m_v, current_function_dependencies);
    }

    void replace_FunctionCall(ASR::FunctionCall_t* x) {
        x->m_name = handle_symbol(x->m_name);
        ADD_ASR_DEPENDENCIES(current_scope, x->m_name, current_function_dependencies);
        if (x->m_original_name) {
            x->m_original_name = handle_symbol(x->m_original_name);
            ADD_ASR_DEPENDENCIES(current_scope, x->m_original_name, current_function_dependencies);
        }
    }

    void replace_StructInstanceMember(ASR::StructInstanceMember_t* x);

    bool check_and_update_symbols(ASR::expr_t* t) {
        if (t) {
            ASR::expr_t** current_copy = current_expr;
            current_expr = &t;
            replace_expr(t);
            current_expr = current_copy;
            return result;
        }
        return false;
    }
};

class ReplaceReturnWithGotoVisitor: public ASR::BaseStmtReplacer<ReplaceReturnWithGotoVisitor> {

    private:

    Allocator& al;

    uint64_t goto_label;

    public:

    ReplaceReturnWithGotoVisitor(Allocator& al_, uint64_t goto_label_) :
        al(al_), goto_label(goto_label_)
    {}

    void set_goto_label(uint64_t label) {
        goto_label = label;
    }

    void replace_Return(ASR::Return_t* x) {
        *current_stmt = ASRUtils::STMT(ASR::make_GoTo_t(al, x->base.base.loc, goto_label,
                            s2c(al, "__" + std::to_string(goto_label))));
        has_replacement_happened = true;
    }

};

static inline bool present(Vec<ASR::symbol_t*> &v, const ASR::symbol_t* name) {
    for (auto &a : v) {
        if (a == name) {
            return true;
        }
    }
    return false;
}

// Singleton LabelGenerator so that it generates
// unique labels for different statements, from
// wherever it is called (be it ASR passes, be it
// AST to ASR transition, etc).
class LabelGenerator {
    private:

        static LabelGenerator *label_generator;
        uint64_t unique_label;
        std::map<ASR::asr_t*, uint64_t> node2label;

        // Private constructor so that more than
        // one object cannot be created by calling the
        // constructor.
        LabelGenerator() {
            unique_label = 0;
        }

    public:

        static LabelGenerator *get_instance() {
            if (!label_generator) {
                label_generator = new LabelGenerator;
            }
            return label_generator;
        }

        int get_unique_label() {
            unique_label += 1;
            return checked_integer_cast<int>(unique_label);
        }

        void add_node_with_unique_label(ASR::asr_t* node, uint64_t label) {
            LCOMPILERS_ASSERT( node2label.find(node) == node2label.end() );
            node2label[node] = label;
        }

        bool verify(ASR::asr_t* node) {
            return node2label.find(node) != node2label.end();
        }
};

ASR::asr_t* make_Cast_t_value(Allocator &al, const Location &a_loc,
        ASR::expr_t* a_arg, ASR::cast_kindType a_kind, ASR::ttype_t* a_type);

// Compile-time evaluation of operations on scalar constants, shared by the
// frontends and by the passes that create new constant expressions, such as
// the instantiation of a template. Each returns nullptr when the operands are
// not constants of a kind it can evaluate.

// Evaluates `left op right` for IntegerConstant, RealConstant (of every kind)
// and ComplexConstant operands, giving a constant of `dest_type`. An integer
// division by zero sets `division_by_zero` and returns nullptr: the caller
// reports it.
ASR::expr_t* fold_binop_constants(Allocator &al, ASR::expr_t* left,
        ASR::expr_t* right, ASR::binopType op, const Location& loc,
        ASR::ttype_t* dest_type, bool &division_by_zero);

// Evaluates the comparison `left op right` of two IntegerConstant, two
// RealConstant or two LogicalConstant operands, giving a LogicalConstant of
// `logical_type`.
ASR::expr_t* fold_compare_constants(Allocator &al, ASR::expr_t* left,
        ASR::expr_t* right, ASR::cmpopType op, const Location& loc,
        ASR::ttype_t* logical_type);

// Evaluates the logical operation `left op right`; returns false for an
// operation it does not evaluate (`Xor`).
bool fold_logical_binop(ASR::logicalbinopType op, bool left, bool right,
        bool &result);

// True if `e` reads a named constant that has no compile-time value, such as
// a deferred constant of a template, or a named constant of a template
// initialized with an expression of one.
bool reads_valueless_parameter(ASR::expr_t* e);

// True if `e` reads the value of a variable that is not a named constant, so
// that it is not a constant expression even once every named constant has a
// value. Inquiries about a variable, e.g. `size(a)`, do not read its value.
bool reads_variable_value(ASR::expr_t* e);

// Casts the integer `a` or `b`, whichever has the smaller kind, to the kind of the other.
void cast_to_wider_kind(Allocator& al, ASR::expr_t*& a, ASR::expr_t*& b);

ASR::expr_t* compute_length_from_start_end(Allocator& al, ASR::expr_t* start, ASR::expr_t* end);

// Number of elements of the section `start:end:step`, that is
// `(end - start + step)/step`, which is 0 for an empty section. Writing it as
// `(end - start)/step + 1` instead gives 1 for a section like `2:1:2`, because
// integer division truncates towards zero. The result is folded when all three
// are known at compile time. A unit step is handled by
// `compute_length_from_start_end`, which simplifies more aggressively.
ASR::expr_t* compute_length_from_start_end_step(Allocator& al,
        ASR::expr_t* start, ASR::expr_t* end, ASR::expr_t* step);

bool is_pass_array_by_data_possible(ASR::Function_t* x, std::vector<size_t>& v);

template <typename SemanticAbort>
static inline ASR::expr_t* get_bound(ASR::expr_t* arr_expr, int dim,
                                     std::string bound, Allocator& al, diag::Diagnostics &diag, int index_kind = 4) {
    ASR::ttype_t* int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, arr_expr->base.loc, index_kind));
    ASR::expr_t* dim_expr = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, arr_expr->base.loc,
                                                                       dim, int_type));
    ASR::arrayboundType bound_type = ASR::arrayboundType::LBound;
    if( bound == "ubound" ) {
        bound_type = ASR::arrayboundType::UBound;
    }
    ASR::expr_t* bound_value = nullptr;
    ASR::dimension_t* arr_dims = nullptr;
    int arr_n_dims = checked_integer_cast<int>(ASRUtils::extract_dimensions_from_ttype(
        ASRUtils::expr_type(arr_expr), arr_dims));
    if( dim > arr_n_dims || dim < 1) {
        if ( ASR::is_a<ASR::Var_t>(*arr_expr )) {
            ASR::Var_t* non_array_var = ASR::down_cast<ASR::Var_t>(arr_expr);
            ASR::Variable_t* non_array_variable = ASR::down_cast<ASR::Variable_t>(
                symbol_get_past_external(non_array_var->m_v));
            std::string msg;
            if (arr_n_dims == 0) {
                msg = "Variable " + std::string(non_array_variable->m_name) +
                            " is not an array so it cannot be indexed.";
            } else {
                msg = "Variable " + std::string(non_array_variable->m_name) +
                            " does not have enough dimensions.";
            }
            diag.add(diag::Diagnostic(
                msg, diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {arr_expr->base.loc})}));
            throw SemanticAbort();
        } else if ( ASR::is_a<ASR::StructInstanceMember_t>(*arr_expr )) {
            ASR::StructInstanceMember_t* non_array_struct_inst_mem = ASR::down_cast<ASR::StructInstanceMember_t>(arr_expr);
            ASR::Variable_t* non_array_variable = ASR::down_cast<ASR::Variable_t>(
                symbol_get_past_external(non_array_struct_inst_mem->m_m));
            std::string msg;
            if (arr_n_dims == 0) {
                msg = "Type member " + std::string(non_array_variable->m_name) +
                            " is not an array so it cannot be indexed.";
            } else {
                msg = "Type member " + std::string(non_array_variable->m_name) +
                            " does not have enough dimensions.";
            }
            diag.add(diag::Diagnostic(
                msg, diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {arr_expr->base.loc})}));
            throw SemanticAbort();
        } else {
            diag.add(diag::Diagnostic(
                "Expression cannot be indexed",
                diag::Level::Error, diag::Stage::Semantic, {
                    diag::Label("", {arr_expr->base.loc})}));
            throw SemanticAbort();
        }
    }
    dim = dim - 1;
    if( arr_dims[dim].m_start && arr_dims[dim].m_length ) {
        ASR::expr_t* arr_start = ASRUtils::expr_value(arr_dims[dim].m_start);
        ASR::expr_t* arr_length = ASRUtils::expr_value(arr_dims[dim].m_length);
        if( bound_type == ASR::arrayboundType::LBound &&
            ASRUtils::is_value_constant(arr_start) ) {
            int64_t const_lbound = -1;
            if( !ASRUtils::extract_value(arr_start, const_lbound) ) {
                LCOMPILERS_ASSERT(false);
            }
            bound_value = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                            al, arr_expr->base.loc, const_lbound, int_type));
        } else if( bound_type == ASR::arrayboundType::UBound &&
            ASRUtils::is_value_constant(arr_start) &&
            ASRUtils::is_value_constant(arr_length) ) {
            int64_t const_lbound = -1;
            if( !ASRUtils::extract_value(arr_start, const_lbound) ) {
                LCOMPILERS_ASSERT(false);
            }
            int64_t const_length = -1;
            if( !ASRUtils::extract_value(arr_length, const_length) ) {
                LCOMPILERS_ASSERT(false);
            }
            bound_value = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
                            al, arr_expr->base.loc,
                            const_lbound + const_length - 1, int_type));
        }
    }
    return ASRUtils::EXPR(ASR::make_ArrayBound_t(al, arr_expr->base.loc, arr_expr, dim_expr,
                int_type, bound_type, bound_value));
}

static inline ASR::expr_t* get_size(ASR::expr_t* arr_expr, int dim,
                                    Allocator& al) {
    ASR::ttype_t* int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, arr_expr->base.loc, 4));
    ASR::expr_t* dim_expr = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, arr_expr->base.loc, dim, int32_type));
    return ASRUtils::EXPR(ASRUtils::make_ArraySize_t_util(al, arr_expr->base.loc, arr_expr, dim_expr,
                                                int32_type, nullptr));
}

static inline ASR::expr_t* get_size(ASR::expr_t* arr_expr, Allocator& al, bool for_type=true) {
    ASR::ttype_t* int32_type = ASRUtils::TYPE(ASR::make_Integer_t(al, arr_expr->base.loc, 4));
    return ASRUtils::EXPR(ASRUtils::make_ArraySize_t_util(al, arr_expr->base.loc, arr_expr, nullptr, int32_type, nullptr, for_type));
}

ASR::ttype_t* get_union_type(Allocator &al, const Location &loc, ASR::symbol_t* union_sym);

static inline ASR::Enum_t* get_Enum_from_symbol(ASR::symbol_t* s) {
    ASR::Variable_t* s_var = ASR::down_cast<ASR::Variable_t>(s);
    if( ASR::is_a<ASR::EnumType_t>(*s_var->m_type) ) {
        ASR::EnumType_t* enum_ = ASR::down_cast<ASR::EnumType_t>(s_var->m_type);
        return ASR::down_cast<ASR::Enum_t>(enum_->m_enum_type);
    }
    ASR::symbol_t* enum_type_cand = ASR::down_cast<ASR::symbol_t>(s_var->m_parent_symtab->asr_owner);
    LCOMPILERS_ASSERT(ASR::is_a<ASR::Enum_t>(*enum_type_cand));
    return ASR::down_cast<ASR::Enum_t>(enum_type_cand);
}

static inline bool is_unlimited_polymorphic_type(ASR::Struct_t* st) {
    if (st->m_struct_signature == nullptr) return false;
    return ASR::down_cast<ASR::StructType_t>(
        st->m_struct_signature)->m_is_unlimited_polymorphic;
}

static inline bool is_unlimited_polymorphic_type(ASR::symbol_t* sym)
{
    sym = ASRUtils::symbol_get_past_external(sym);
    if (!ASR::is_a<ASR::Struct_t>(*sym)) return false;
    return is_unlimited_polymorphic_type(
        ASR::down_cast<ASR::Struct_t>(sym));
}

inline bool is_unlimited_polymorphic_type(ASR::ttype_t* const t){
    if(!ASR::is_a<ASR::StructType_t>(*extract_type(t))) return false;

    return ASR::down_cast<ASR::StructType_t>(extract_type(t))->m_is_unlimited_polymorphic;
}

static inline bool is_unlimited_polymorphic_type(ASR::expr_t* expr)
{
    return is_unlimited_polymorphic_type(ASRUtils::expr_type(expr));
}
/// Check if type is a class + not unlimited-polymorphic one
inline bool non_unlimited_polymorphic_class(ASR::ttype_t* const t){
    return is_class_type(extract_type(t)) && !is_unlimited_polymorphic_type(t);
}

void set_enum_value_type(ASR::enumtypeType &enum_value_type,
        SymbolTable *scope);

class CollectIdentifiersFromASRExpression: public ASR::BaseWalkVisitor<CollectIdentifiersFromASRExpression> {
private:

        Allocator& al;
        SetChar& identifiers;
        std::string current_name;

public:

// Constructors
        CollectIdentifiersFromASRExpression(Allocator& al_, SetChar& identifiers_, std::string current_name_ = "") :
        al(al_), identifiers(identifiers_), current_name(current_name_)
        {}

// Visitors
        void visit_Var(const ASR::Var_t& x) {
            if (ASRUtils::symbol_name(x.m_v) != this->current_name) {
                identifiers.push_back(al, ASRUtils::symbol_name(x.m_v));
            }
        }
        void visit_FunctionCall(const ASR::FunctionCall_t &x){
            identifiers.push_back(al, ASRUtils::symbol_name(x.m_name));
            ASR::BaseWalkVisitor<CollectIdentifiersFromASRExpression>::visit_FunctionCall(x);
        }
};

void collect_variable_dependencies(Allocator& al, SetChar& deps_vec,
    ASR::ttype_t* type=nullptr, ASR::expr_t* init_expr=nullptr,
    ASR::expr_t* value=nullptr, std::string current_name="");

int KMP_string_match(std::string &s_var, std::string &sub);

int KMP_string_match_count(std::string &s_var, std::string &sub);

static inline void visit_expr_list(Allocator &al, Vec<ASR::call_arg_t>& exprs,
        Vec<ASR::expr_t*>& exprs_vec) {
    LCOMPILERS_ASSERT(exprs_vec.reserve_called);
    for( size_t i = 0; i < exprs.n; i++ ) {
        exprs_vec.push_back(al, exprs[i].m_value);
    }
}

static inline void visit_expr_list(Allocator &al, Vec<ASR::expr_t *> exprs,
        Vec<ASR::call_arg_t>& exprs_vec) {
    LCOMPILERS_ASSERT(exprs_vec.reserve_called);
    for( size_t i = 0; i < exprs.n; i++ ) {
        ASR::call_arg_t arg;
        arg.loc = exprs[i]->base.loc;
        arg.m_value = exprs[i];
        exprs_vec.push_back(al, arg);
    }
}

class VerifyAbort {};

static inline void require_impl(bool cond, const std::string &error_code,
    const std::string &error_msg, const Location &loc,
    diag::Diagnostics &diagnostics) {
    if (!cond) {
        diagnostics.message_label(error_msg,
            {loc}, "failed here",
            diag::Level::Error, diag::Stage::ASRVerify, error_code);
        throw VerifyAbort();
    }
}

static inline void require_impl(bool cond, const std::string &error_msg,
    const Location &loc, diag::Diagnostics &diagnostics) {
    require_impl(cond, "", error_msg, loc, diagnostics);
}

ASR::dimension_t* duplicate_dimensions(Allocator& al, ASR::dimension_t* m_dims, size_t n_dims);

static inline bool is_allocatable(ASR::expr_t* expr) {
    return ASR::is_a<ASR::Allocatable_t>(*ASRUtils::expr_type(expr));
}

static inline bool is_allocatable(ASR::ttype_t* type) {
    return ASR::is_a<ASR::Allocatable_t>(*type);
}

static inline bool is_allocatable_or_pointer(ASR::ttype_t* type) {
    return is_allocatable(type) || is_pointer(type);
}

// Whether `to` is type compatible with `from` (F2018 7.3.2.3) and has the
// same kind type parameters, as MOVE_ALLOC requires (F2018 16.9.137). The
// requirement that `to` be polymorphic if `from` is is not checked here.
bool is_move_alloc_type_compatible(ASR::expr_t* from,
        ASR::expr_t* to);

// Whether an Assignment with move_allocation can transfer the allocation of
// `value` to `target`: both are allocatable descriptor arrays, or both are
// allocatable noncharacter scalars and `target` is type compatible with
// `value` (F2018 16.9.137), so that the object keeps its representation.
bool is_move_allocation_pair(ASR::expr_t* target,
        ASR::expr_t* value);

// Reports whether a value of the derived type `st` has a constant byte
// representation, i.e. whether every one of its components is stored inline
// in the type. An allocatable or a pointer component holds an array
// descriptor or a pointer to memory owned elsewhere, and a polymorphic
// component carries its dynamic type at run time, so a value of a type with
// such a component has no byte representation to store. The inherited parent
// and the type of every derived type component are checked as well.
// `visited` guards against a cyclic parent or component chain.
bool is_byte_representable_struct(ASR::Struct_t* st,
        std::set<ASR::Struct_t*>& visited);

inline bool is_byte_representable_struct(ASR::symbol_t* struct_sym) {
    ASR::symbol_t* sym = struct_sym ? symbol_get_past_external(struct_sym) : nullptr;
    if( !sym || !ASR::is_a<ASR::Struct_t>(*sym) ) {
        return false;
    }
    std::set<ASR::Struct_t*> visited;
    return is_byte_representable_struct(
        ASR::down_cast<ASR::Struct_t>(sym), visited);
}

static inline bool is_coarray(ASR::symbol_t* s) {
    s = symbol_get_past_external(s);
    if (ASR::is_a<ASR::Variable_t>(*s)) {
        ASR::Variable_t* v = ASR::down_cast<ASR::Variable_t>(s);
        return v->n_codims > 0;
    }
    return false;
}

static inline int64_t symbol_corank(ASR::symbol_t* s) {
    s = symbol_get_past_external(s);
    if (ASR::is_a<ASR::Variable_t>(*s)) {
        return ASR::down_cast<ASR::Variable_t>(s)->n_codims;
    }
    return 0;
}

int64_t expr_corank(const ASR::expr_t* expr);

void import_struct_t(Allocator& al,
    const Location& loc, ASR::ttype_t*& var_type,
    ASR::intentType intent, SymbolTable* current_scope, ASR::expr_t* var);

ASR::symbol_t* import_struct_sym_as_external(Allocator& al,
    const Location& loc, ASR::expr_t* v_expr, SymbolTable* current_scope);

ASR::asr_t* make_ArrayPhysicalCast_t_util(Allocator &al, const Location &a_loc,
    ASR::expr_t* a_arg, ASR::array_physical_typeType a_old, ASR::array_physical_typeType a_new,
    ASR::ttype_t* a_type, ASR::expr_t* a_value);

// inline void flatten_ArrayConstant(Allocator& al, void* data, size_t n_args, Vec<ASR::expr_t*> &new_args) {
//     for (size_t i = 0; i < n_args; i++) {
//         if (ASR::is_a<ASR::ArrayConstant_t>(*a_args[i])) {
//             ASR::ArrayConstant_t* a_arg = ASR::down_cast<ASR::ArrayConstant_t>(a_args[i]);
//             flatten_ArrayConstant(al, a_arg->m_args, a_arg->n_args, new_args);
//         } else if (ASR::is_a<ASR::ArrayConstant_t>(*ASRUtils::expr_value(a_args[i]))) {
//             ASR::ArrayConstant_t* a_arg = ASR::down_cast<ASR::ArrayConstant_t>(ASRUtils::expr_value(a_args[i]));
//             flatten_ArrayConstant(al, a_arg->m_args, a_arg->n_args, new_args);
//         } else {
//             new_args.push_back(al, ASRUtils::expr_value(a_args[i]));
//         }
//     }
// }

// Assigns "x->m_data[i] = value", casting the internal data pointer correctly using the type of "value"
void set_ArrayConstant_value(ASR::ArrayConstant_t* x, ASR::expr_t* value, int64_t i);

template <typename T>
inline std::string to_string_with_precision(const T a_value, const int n) {
    std::ostringstream out;
    out.precision(n);
    out << std::scientific << a_value;
    return std::move(out).str();
}

std::string fetch_ArrayConstant_value(void *data, ASR::ttype_t* type, int64_t i);

inline std::string fetch_ArrayConstant_value(ASR::ArrayConstant_t* x, int64_t i) {
    ASR::ttype_t* type = ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(x->m_type));
    return fetch_ArrayConstant_value(x->m_data, type, i);
}

inline std::string fetch_ArrayConstant_value(ASR::ArrayConstant_t &x, int64_t i) {
    ASR::ttype_t* type = ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(x.m_type));
    return fetch_ArrayConstant_value(x.m_data, type, i);
}

inline std::string fetch_ArrayConstant_value(const ASR::ArrayConstant_t &x, int64_t i) {
    ASR::ttype_t* type = ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(x.m_type));
    return fetch_ArrayConstant_value(x.m_data, type, i);
}

ASR::expr_t* fetch_ArrayConstant_value_helper(Allocator &al, const Location& loc, void *data, ASR::ttype_t* type, int64_t i);

inline ASR::expr_t* fetch_ArrayConstant_value(Allocator &al, ASR::ArrayConstant_t* x, int64_t i) {
    ASR::ttype_t* type = ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(x->m_type));
    return fetch_ArrayConstant_value_helper(al, x->base.base.loc, x->m_data, type, i);
}

inline ASR::expr_t* fetch_ArrayConstant_value(Allocator &al, ASR::ArrayConstant_t &x, int64_t i) {
    ASR::ttype_t* type = ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(x.m_type));
    return fetch_ArrayConstant_value_helper(al, x.base.base.loc, x.m_data, type, i);
}

inline ASR::expr_t* fetch_ArrayConstant_value(Allocator &al, const ASR::ArrayConstant_t &x, int64_t i) {
    ASR::ttype_t* type = ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(x.m_type));
    return fetch_ArrayConstant_value_helper(al, x.base.base.loc, x.m_data, type, i);
}

// The compile-time value of the element `array(indices)`, or nullptr unless
// `array` has a constant value and every index is a constant subscript
// within its bounds.
ASR::expr_t* fold_ArrayItem_value(Allocator &al, ASR::expr_t* array,
        ASR::array_index_t* indices, size_t n_indices);

template<typename T>
T* set_data_int(T* data, ASR::expr_t** a_args, size_t n_args) {
    for (size_t i = 0; i < n_args; i++) {
        data[i] = checked_bit_pattern_cast<T>(
            ASR::down_cast<ASR::IntegerConstant_t>(ASRUtils::expr_value(a_args[i]))->m_n);
    }
    return data;
}

template<typename T>
T* set_data_uint(T* data, ASR::expr_t** a_args, size_t n_args) {
    for (size_t i = 0; i < n_args; i++) {
        data[i] = checked_bit_pattern_cast<T>(
            ASR::down_cast<ASR::UnsignedIntegerConstant_t>(ASRUtils::expr_value(a_args[i]))->m_n);
    }
    return data;
}

template<typename T>
T* set_data_real(T* data, ASR::expr_t** a_args, size_t n_args) {
    for (size_t i = 0; i < n_args; i++) {
        data[i] = static_cast<T>(
            ASR::down_cast<ASR::RealConstant_t>(ASRUtils::expr_value(a_args[i]))->m_r);
    }
    return data;
}

template<typename T>
T* set_data_complex(T* data, ASR::expr_t** a_args, size_t n_args) {
    for (size_t i = 0; i < n_args; i++) {
        data[2*i] = static_cast<T>(
            ASR::down_cast<ASR::ComplexConstant_t>(ASRUtils::expr_value(a_args[i]))->m_re);
        data[2*i + 1] = static_cast<T>(
            ASR::down_cast<ASR::ComplexConstant_t>(ASRUtils::expr_value(a_args[i]))->m_im);
    }
    return data;
}

void* set_ArrayConstant_data(ASR::expr_t** a_args, size_t n_args, ASR::ttype_t* a_type);

int64_t get_ArrayConstant_data_size(size_t n_args, ASR::ttype_t* a_type);

inline int64_t get_ArrayConstant_data_size(ASR::ttype_t* array_type) {
    int64_t array_size = ASRUtils::get_fixed_size_of_array(array_type);
    LCOMPILERS_ASSERT(array_size >= 0);
    return get_ArrayConstant_data_size(
        array_size, ASRUtils::type_get_past_array(array_type));
}

inline ASR::asr_t* make_ArrayConstant_t_util(Allocator &al,
        const Location &loc, void *data, ASR::ttype_t* array_type,
        ASR::arraystorageType storage_format) {
    int64_t n_data = get_ArrayConstant_data_size(array_type);
    LCOMPILERS_ASSERT(n_data == 0 || data != nullptr);
    return ASR::make_ArrayConstant_t(
        al, loc, n_data, data, array_type, storage_format);
}

// pad (with ' ') or trim character string 'value'
ASR::expr_t* adjust_character_length(ASR::expr_t* value, int64_t lhs_len, int64_t rhs_len, const Location& loc, Allocator& al);

ASR::expr_t* adjust_array_character_length(ASR::expr_t* value, int64_t lhs_len, int64_t rhs_len, Allocator& al);

// Returns the character constant `value` (a scalar or an array) with its
// length set to `len`, blank padded or truncated, as for
// `character(len=3) :: c = "hi"`. Returns nullptr if the length is
// already `len` or is not constant.
ASR::expr_t* conform_character_constant_length(ASR::expr_t* value,
        int64_t len, const Location& loc, Allocator& al);

void flatten_ArrayConstant_data(Allocator &al, Vec<ASR::expr_t*> &data, ASR::expr_t** a_args, size_t n_args, ASR::ttype_t* a_type, int &curr_idx, ASR::ArrayConstant_t* x = nullptr);

ASR::asr_t* make_ArrayConstructor_t_util(Allocator &al, const Location &a_loc,
    ASR::expr_t** a_args, size_t n_args, ASR::ttype_t* a_type, ASR::arraystorageType a_storage_format,
    ASR::expr_t* a_struct_var=nullptr);

// Returns the array constant of the fixed size type `array_type` whose every
// element is the scalar constant `value`, as for `real :: x(3) = 5.0`, or
// nullptr if `array_type` does not have a fixed size or the elements do not
// fold.
ASR::expr_t* broadcast_scalar_constant_to_array(Allocator& al,
        const Location& loc, ASR::expr_t* value, ASR::ttype_t* array_type);

void make_ArrayBroadcast_t_util(Allocator& al, const Location& loc,
    ASR::expr_t*& expr1, ASR::expr_t*& expr2);

// Wraps argument in stringformat if it's not a single argument of type Character.
ASR::asr_t* make_print_t_util(Allocator& al, const Location& loc,
    ASR::expr_t** a_args, size_t n_args);

template <typename SemanticAbort>
inline void check_simple_intent_mismatch(diag::Diagnostics &diag, ASR::Function_t* f, const Vec<ASR::call_arg_t>& args) {
    for (size_t i = 0; i < args.size(); i++) {
        ASR::expr_t* passed_arg_expr = args[i].m_value;

        // An argument with no value is a dummy argument that is not present.
        // `.nil.`, the consequent of a conditional argument that leaves it
        // absent, is the only way to write one (15.5.2.3), and C1540 allows
        // it only when the dummy argument is optional.
        if (!passed_arg_expr && i < f->n_args
                && ASR::is_a<ASR::Var_t>(*f->m_args[i])) {
            ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v;
            if (ASR::is_a<ASR::Variable_t>(*sym)
                    && ASR::down_cast<ASR::Variable_t>(sym)->m_presence
                        != ASR::presenceType::Optional) {
                diag.add(diag::Diagnostic(
                    "`.nil.` is not allowed for the dummy argument `"
                    + std::string(ASRUtils::symbol_name(sym))
                    + "`, which is not optional",
                    diag::Level::Error, diag::Stage::Semantic, {
                        diag::Label("a consequent may be `.nil.` only when "
                            "the dummy argument is optional "
                            "(Fortran 2023 C1540)", {args[i].loc})}));
                throw SemanticAbort();
            }
        }

        if (passed_arg_expr && i < f->n_args) {
            if (ASR::is_a<ASR::Var_t>(*f->m_args[i])) {
                ASR::symbol_t* sym = ASR::down_cast<ASR::Var_t>(f->m_args[i])->m_v;
                if (ASR::is_a<ASR::Variable_t>(*sym)) {
                    ASR::Variable_t* callee_param = ASR::down_cast<ASR::Variable_t>(sym);
                    if (!ASR::is_a<ASR::FunctionType_t>(*callee_param->m_type)) {
                        if ((callee_param->m_intent == ASR::intentType::Out ||
                             callee_param->m_intent == ASR::intentType::InOut)) {
                            if (!ASRUtils::is_modifiable_actual_argument_expr(passed_arg_expr)) {
                                diag.add(diag::Diagnostic(
                                    "Non-variable expression in variable definition context "
                                    "(actual argument to INTENT = OUT/INOUT)",
                                    diag::Level::Error, diag::Stage::Semantic, {
                                        diag::Label("", {passed_arg_expr->base.loc})
                                    }));
                                throw SemanticAbort();
                            }
                        }
                    }
                }
            }
        }

        if (passed_arg_expr && ASR::is_a<ASR::Var_t>(*passed_arg_expr)) {
            ASR::symbol_t* passed_sym = ASR::down_cast<ASR::Var_t>(passed_arg_expr)->m_v;
            if (ASR::is_a<ASR::Variable_t>(*passed_sym)) {
                ASR::Variable_t* passed_var = ASR::down_cast<ASR::Variable_t>(passed_sym);
                if (passed_var->m_intent == ASR::intentType::In) {
                    if (i < f->n_args) {
                        ASR::Variable_t* callee_param = ASRUtils::EXPR2VAR(f->m_args[i]);
                        if (callee_param->m_intent == ASR::intentType::Out ||
                            callee_param->m_intent == ASR::intentType::InOut) {
                            diag.add(diag::Diagnostic(
                                "Argument `" + std::string(passed_var->m_name) +
                                "` with intent(in) passed to a dummy argument with modifying intent",
                                diag::Level::Error, diag::Stage::Semantic, {
                                    diag::Label("", {passed_arg_expr->base.loc})
                                }));
                            throw SemanticAbort();
                        }
                    }
                }
            }
        }
    }
}

inline bool is_stringToArray_cast_needed(ASR::ttype_t* const argument_ty, ASR::ttype_t* const parameter_ty){
    ASR::ttype_t* const argument_ty_past  = ASRUtils::type_get_past_allocatable_pointer(argument_ty);
    ASR::ttype_t* const parameter_ty_past = ASRUtils::type_get_past_allocatable_pointer(parameter_ty);
    return ASRUtils::is_array_of_strings(parameter_ty_past)
        && argument_ty_past->type  == ASR::String;
}

ASR::Cast_t* cast_string_to_array(Allocator &al, ASR::expr_t* const string_expr, ASR::ttype_t* const array_type);

void Call_t_body(Allocator& al, ASR::symbol_t* a_name,
    ASR::call_arg_t* a_args, size_t n_args, ASR::expr_t* a_dt, ASR::stmt_t** cast_stmt,
    bool implicit_argument_casting, SymbolTable* current_scope = nullptr, std::optional<std::reference_wrapper<SetChar>> current_function_dependencies = std::nullopt);

// Matches the arguments of the call `x` with the dummy arguments of its
// procedure again, without cast statements or implicit casting, after a pass
// replaced some of them with expressions of another physical type.
template <typename T>
static inline void rematch_call_args(Allocator &al, T &x) {
    Call_t_body(al, x.m_name, x.m_args, x.n_args, x.m_dt, nullptr, false);
}

// Insert self (a_dt) into args at the PASS position for method calls.
// Callers must invoke this BEFORE make_*Call_t_util so that m_args
// is in 1:1 correspondence with func_type->m_arg_types.
// For procedure pointer Variables whose a_dt is a StructInstanceMember,
// the base object (m_v) is used as self, not the member expression.
// No-op when a_dt is null or the callee has NOPASS.
void insert_self_arg(
    Allocator& al, ASR::symbol_t* a_name,
    ASR::call_arg_t*& a_args, size_t& n_args,
    ASR::expr_t* a_dt);

ASR::asr_t* make_FunctionCall_t_util(
    Allocator &al, const Location &a_loc, ASR::symbol_t* a_name,
    ASR::symbol_t* a_original_name, ASR::call_arg_t* a_args, size_t n_args,
    ASR::ttype_t* a_type, ASR::expr_t* a_value, ASR::expr_t* a_dt, SymbolTable* current_scope = nullptr, std::optional<std::reference_wrapper<SetChar>> current_function_dependencies = std::nullopt,
    bool implicit_argument_casting = false);

ASR::asr_t* make_SubroutineCall_t_util(
    Allocator &al, const Location &a_loc, ASR::symbol_t* a_name,
    ASR::symbol_t* a_original_name, ASR::call_arg_t* a_args, size_t n_args,
    ASR::expr_t* a_dt, ASR::stmt_t** cast_stmt, bool implicit_argument_casting, SymbolTable* current_scope = nullptr, std::optional<std::reference_wrapper<SetChar>> current_function_dependencies = std::nullopt, bool a_strict_bounds_checking = false);

/*
    Checks if the function `f` is elemental and any of argument in
    `args` is an array type, if yes, it returns the first array
    argument, otherwise returns nullptr
*/
ASR::expr_t* find_first_array_arg_if_elemental(
    const ASR::Function_t* f,
    const Vec<ASR::call_arg_t>& args
);

static inline void promote_ints_to_kind_8(ASR::expr_t** m_args, size_t n_args,
    Allocator& al, const Location& loc) {
    for (size_t i = 0; i < n_args; i++) {
        if (ASRUtils::is_integer(*ASRUtils::expr_type(m_args[i]))) {
            ASR::ttype_t* arg_type = ASRUtils::expr_type(m_args[i]);
            ASR::ttype_t* dest_type = ASRUtils::duplicate_type(al, arg_type);
            ASRUtils::set_kind_to_ttype_t(dest_type, 8);
            m_args[i] = CastingUtil::perform_casting(m_args[i], dest_type, al, loc);
        }
    }
}

ASR::asr_t* make_StringFormat_t_util(Allocator &al, const Location &a_loc,
        ASR::expr_t* a_fmt, ASR::expr_t** a_args, size_t n_args, ASR::string_format_kindType a_kind,
        ASR::dtio_binding_t* a_dtio_bindings, size_t n_dtio_bindings, ASR::expr_t* a_dtio_unit,
        ASR::ttype_t* a_type, ASR::expr_t* a_value);

ASR::expr_t* cast_to_descriptor(Allocator& al, ASR::expr_t* arg);

ASR::asr_t* make_IntrinsicElementalFunction_t_util(
    Allocator &al, const Location &a_loc, int64_t a_intrinsic_id,
    ASR::expr_t** a_args, size_t n_args, int64_t a_overload_id,
    ASR::ttype_t* a_type, ASR::expr_t* a_value);

ASR::asr_t* make_IntrinsicArrayFunction_t_util(
    Allocator &al, const Location &a_loc, int64_t arr_intrinsic_id,
    ASR::expr_t** a_args, size_t n_args, int64_t a_overload_id,
    ASR::ttype_t* a_type, ASR::expr_t* a_value);

// Whether `x`, an array variable whose rightmost part-name is `v`, can be
// discontiguous by itself: a pointer, an assumed-shape or an assumed-rank
// array without the CONTIGUOUS attribute.
static inline bool is_possibly_discontiguous_part(ASR::Variable_t* v) {
    if (v->m_contiguous_attr) {
        return false;
    }
    ASR::ttype_t* type = v->m_type;
    if (is_pointer(type) || is_assumed_rank_array(type)) {
        return true;
    }
    return is_arg_dummy(v->m_intent) && !is_allocatable(type) && is_array(type) &&
        extract_physical_type(type) == ASR::array_physical_typeType::DescriptorArray;
}

// The variable of the rightmost part-name of the designator `x`, or nullptr.
// An ArrayPhysicalCast of the designator, such as the selector of SELECT
// RANK, refers to the same variable.
ASR::Variable_t* rightmost_part_variable(ASR::expr_t* x);

// Whether `x` is a whole array variable or array component that is
// contiguous by its declaration: one with the CONTIGUOUS attribute, or one
// that is not a pointer, assumed-shape or assumed-rank.
static inline bool is_contiguous_array_designator(ASR::expr_t* x) {
    ASR::Variable_t* v = rightmost_part_variable(x);
    return v != nullptr && is_array(v->m_type) && !is_possibly_discontiguous_part(v);
}

// Whether the array variable `x` is simply contiguous (F2018 9.5.4): an
// array designator that is contiguous by its form, or a reference to a
// function that returns a pointer with the CONTIGUOUS attribute.
//
// This inspects the subscripts of a section as the frontend builds them. On
// ASR that passes have rewritten, a true result still holds, but a false one
// may be needlessly conservative: a `:` that a pass spells other than below
// counts as a partial triplet, which can only make the section not simply
// contiguous, and a pointer that a pass puts in place of a variable is not
// simply contiguous, even when the variable is. A true result relies on a
// scalar subscript having only an upper bound and a triplet having a stride,
// as both the frontend and the passes build them.
//
// A subscript `:` is represented by the LBOUND and UBOUND of the array, an
// absent stride by 1. As an extension to 9.5.4, a triplet whose bounds are
// the constant declared bounds of an explicit-shape array, such as
// `b(1:4, 2:3)` for `b(4, 5)`, counts as `:`, and an explicit stride 1 as
// none. Such a section is contiguous as well.
bool is_simply_contiguous(ASR::expr_t* x);

ASR::asr_t* make_Associate_t_util(
    Allocator &al, const Location &a_loc,
    ASR::expr_t* a_target, ASR::expr_t* a_value,
    ASR::pointer_bounds_formType a_form=ASR::pointer_bounds_formType::NoPointerBounds);

static inline ASR::expr_t* extract_array_variable(ASR::expr_t* x) {
    LCOMPILERS_ASSERT(ASRUtils::is_array(ASRUtils::expr_type(x)));
    if( x->type == ASR::exprType::ArrayItem ) {
        return ASR::down_cast<ASR::ArrayItem_t>(x)->m_v;
    } else if( x->type == ASR::exprType::ArraySection ) {
        return ASR::down_cast<ASR::ArraySection_t>(x)->m_v;
    }

    return x;
}

void extract_array_indices(ASR::expr_t* x, Allocator &al,
    Vec<ASR::array_index_t>& m_args, int& n_args);

void extract_indices(ASR::expr_t* x,
    ASR::array_index_t*& m_args, size_t& n_args);

bool is_array_indexed_with_array_indices(ASR::array_index_t* m_args, size_t n_args);

template <typename T>
static inline bool is_array_indexed_with_array_indices(T* x) {
    return is_array_indexed_with_array_indices(x->m_args, x->n_args);
}

// The part of nonzero rank of a chain of components, as in `w%nest` with `w`
// an array, or `s%w` with `s` a scalar and `w` an array component: walk down
// the chain for as long as the parent of a component is itself an array. What
// is left is the array whose elements the chain takes its components from.
static inline ASR::expr_t* get_struct_member_chain_array_part(ASR::expr_t* expr) {
    while( ASR::is_a<ASR::StructInstanceMember_t>(*expr) ) {
        ASR::expr_t* parent = ASR::down_cast<ASR::StructInstanceMember_t>(expr)->m_v;
        if( !ASRUtils::is_array(ASRUtils::expr_type(parent)) ) {
            break;
        }
        expr = parent;
    }
    return expr;
}

// Selecting an element of an array component of an array, as in `w%u(2)` or
// `w%nest%v(2)` with `w` an array, reads one element of the component out of
// every element of the base. The subscripts consume the rank of the
// component, not the rank of the base, so the reference denotes an array
// shaped like the base although every subscript is scalar. Returns the base
// whose shape the reference carries, or nullptr when it carries none.
//
// What such a reference denotes is a view of the base strided by the size of
// an element of the base, and no `array_physical_type` says that. It is never
// handed to a backend as it is: the passes index the base element by element
// and select the component out of each element. The base has to be reached
// through components that hold their value inline, and has to bottom out in
// an array variable (of any kind: fixed size, `allocatable`, `pointer`,
// assumed-shape), in an array component of a scalar, as in `s%w%u(1)`, or in
// a section of either, as in `w(2:4)%u(1)`. The array passes bind such a
// section to a pointer, or copy it, before they index it.
ASR::expr_t* struct_base_lending_shape(ASR::ArrayItem_t* x);

ASR::ttype_t* create_array_type_with_empty_dims(Allocator& al,
    size_t value_n_dims, ASR::ttype_t* value_type);


ASR::asr_t* make_ArrayItem_t_util(Allocator &al, const Location &a_loc,
    ASR::expr_t* a_v, ASR::array_index_t* a_args, size_t n_args, ASR::ttype_t* a_type,
    ASR::arraystorageType a_storage_format, ASR::expr_t* a_value,
    bool a_in_bounds = false);

ASR::ttype_t* make_Pointer_t_util(Allocator& al, const Location& loc, ASR::ttype_t* type);

int64_t compute_trailing_zeros(int64_t number, int64_t kind);
int64_t compute_leading_zeros(int64_t number, int64_t kind);
void append_error(diag::Diagnostics& diag, const std::string& msg,
                const Location& loc);

static inline bool is_simd_array(ASR::ttype_t* t) {
    return (ASR::is_a<ASR::Array_t>(*t) &&
        ASR::down_cast<ASR::Array_t>(t)->m_physical_type
            == ASR::array_physical_typeType::SIMDArray);
}

static inline bool is_simd_array(ASR::expr_t *v) {
    return is_simd_array(expr_type(v));
}

bool is_argument_of_type_CPtr(ASR::expr_t *var);

// Converts integer or real arguments of different kinds to a common kind
// (used for MIN and MAX, where mixed kinds are a non-standard extension, and
// for FINDLOC, whose VALUE may have a different kind than ARRAY). By default
// that is the largest kind among the arguments; with `use_first_array_kind`
// it is the kind of the first array argument. Array arguments are converted
// element-wise, keeping their shape.
void promote_arguments_kinds(Allocator &al, const Location &loc,
        Vec<ASR::expr_t*> &args, diag::Diagnostics &diag,
        bool use_first_array_kind=false);

class RemoveArrayProcessingNodeReplacer: public ASR::BaseExprReplacer<RemoveArrayProcessingNodeReplacer> {

    public:

    Allocator& al;
    bool preserve_struct_array_broadcast;

    RemoveArrayProcessingNodeReplacer(Allocator& al_): al(al_) {
        preserve_struct_array_broadcast = false;
    }

    void replace_ArrayBroadcast(ASR::ArrayBroadcast_t* x);

    void replace_ArrayPhysicalCast(ASR::ArrayPhysicalCast_t* x) {
        if( x->m_new == ASR::array_physical_typeType::SIMDArray ) {
            return ;
        }
        ASR::BaseExprReplacer<RemoveArrayProcessingNodeReplacer>::replace_ArrayPhysicalCast(x);
        if( !ASRUtils::is_array(ASRUtils::expr_type(x->m_arg)) ) {
            *current_expr = x->m_arg;
        }
    }

};

class RemoveArrayProcessingNodeVisitor: public ASR::CallReplacerOnExpressionsVisitor<RemoveArrayProcessingNodeVisitor> {

    private:

    RemoveArrayProcessingNodeReplacer replacer;

    public:

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    RemoveArrayProcessingNodeVisitor(Allocator& al_): replacer(al_) {}

    void visit_Variable(const ASR::Variable_t& x);

    void visit_ArrayPhysicalCast(const ASR::ArrayPhysicalCast_t& x) {
        if( x.m_new == ASR::array_physical_typeType::SIMDArray ) {
            return ;
        }

        ASR::CallReplacerOnExpressionsVisitor<RemoveArrayProcessingNodeVisitor>::visit_ArrayPhysicalCast(x);
    }
};

static inline int64_t get_fixed_string_len(const ASR::ttype_t* type_t) {
    if (ASR::is_a<ASR::String_t>(*type_t)) {
        ASR::expr_t* len_expr = ASR::down_cast<ASR::String_t>(type_t)->m_len;
        if (len_expr && ASR::is_a<ASR::IntegerConstant_t>(*len_expr)) {
            return ASR::down_cast<ASR::IntegerConstant_t>(len_expr)->m_n;
        }
    }
    return -1;
}

// Returns the type of `e`, or nullptr if the expression is malformed in a way
// that makes its type unobtainable (a Var referencing a symbol that carries no
// type, such as a Program). Such expressions have their own dedicated checks,
// so type comparisons must simply be skipped for them instead of asking for a
// type that does not exist.
ASR::ttype_t* typed_expr_type(const ASR::expr_t *e);

// Procedure types are compared by their own dedicated checks, and they have no
// type code, so they cannot take part in the generic type comparisons.
bool is_procedure_type(ASR::ttype_t *t);

// A StructType spells out its member types inline, so two StructType nodes for
// the same derived type can differ structurally, for example when a pass
// rewrites the signature of a procedure pointer component in one of them.
// Derived type identity is established from the struct symbol, which a bare
// signature type does not carry, so such types are left to the dedicated
// struct checks.
bool is_struct_like_type(ASR::ttype_t *t);

// The one way in which two procedures that must present the same interface
// fail to do so. `code` names the check that failed, so a caller can label the
// diagnostic it raises; both fields are empty when the interfaces conform.
struct InterfaceMismatch {
    bool mismatch = false;
    std::string code;
    std::string message;
};

// Compares two procedures that must present the same interface. `skip` is the
// position of the one dummy argument they may declare differently (the
// passed-object dummy argument), or `impl->n_args` when there is none. `what`
// names the procedure at the start of the message. Type equality uses the
// argument expressions to resolve the struct symbols they refer to, which
// requires dereferencing ExternalSymbol; pass `use_expr_context = false`
// before externals are resolved to fall back to a structural comparison.
InterfaceMismatch interface_mismatch(const std::string &what,
    ASR::Function_t *impl, ASR::Function_t *decl, size_t skip,
    bool use_expr_context);

// The position of the passed-object dummy argument of a binding, or `n_args`
// when the binding has none.
size_t passed_object_index(const ASR::StructMethodDeclaration_t &x,
    ASR::Function_t *proc);

// The binding of the same name that `x` overrides, searched up the parent
// chain of the derived type `x` belongs to, or nullptr.
ASR::StructMethodDeclaration_t* overridden_binding(
    const ASR::StructMethodDeclaration_t &x);

// Fortran 2018 7.5.7.3: an overriding type-bound procedure and the one it
// overrides must have the same interface apart from the passed-object dummy
// argument. Nothing downstream re-derives this, so a mismatch means a dispatch
// through the parent type calls a procedure whose signature does not match the
// call site the parent's interface promised. Returns no mismatch when `x`
// overrides nothing.
InterfaceMismatch binding_override_mismatch(
    const ASR::StructMethodDeclaration_t &x, ASR::Function_t *proc,
    const std::string &what, bool use_expr_context);

std::set<ASR::Function_t*> get_called_functions(ASR::stmt_t **body,
        size_t n_body, bool procedure_values = false);

// Whether calling the function `fn` has no effect but its result: it is
// pure, or it reads its arguments only (all of them `intent(in)`), defines
// nothing but its own locals and result, and calls only such functions --
// as the routines shared lowering gives an intrinsic, such as `maxval`,
// are written. Such a call can be evaluated anywhere its arguments can,
// any number of times.
bool is_side_effect_free_function(ASR::Function_t &fn,
        std::set<ASR::Function_t*> &visiting);

inline bool is_side_effect_free_function(ASR::Function_t &fn) {
    std::set<ASR::Function_t*> visiting;
    return is_side_effect_free_function(fn, visiting);
}

// Whether a member of `s`, of one of its parents, or of one of its derived
// type members, cannot be described by static data and so has to be set up by
// executable code: an array member needs a descriptor of its own or its
// dimensions filled in, a string member its data, and a class member its type
// pointer. A scalar of an intrinsic type, and a pointer or allocatable scalar,
// are fully described by a constant.
//
// It decides two things that have to agree: whether the `global_init` pass
// turns a declaration initializer of such an array into a statement, and
// whether a backend lays that initializer out as static data. Laying an
// element out statically when this is true would give every element the one
// descriptor the constant holds, so a write through one element would be seen
// through all of them.
bool struct_needs_member_init(ASR::Struct_t* s,
        std::set<ASR::Struct_t*>& visited);

// `struct_needs_member_init` for the element type of the array `type`, a
// derived type that `type_declaration` declares. False for anything that is
// not an array of a derived type.
bool needs_struct_array_member_init(ASR::symbol_t* type_declaration,
        ASR::ttype_t* type);

// The zero based offset, in elements, of the array element `x` names in the
// storage of its array, or -1 when it is not known at compile time. Fortran
// lays an array out with its first dimension varying fastest, so the offset
// of `a(i, j)` is `(i - lbound(a, 1)) + (j - lbound(a, 2)) * size(a, 1)`.
int64_t constant_array_item_offset(ASR::ArrayItem_t* x);

// A variable a module declares. Every backend lays those out as static data
// of the module and leaves their declaration initializers on the declaration
// for that; a variable of a procedure, of a block or of a program is instead
// initialized by statements of the scope that declares it, put there by the
// `global_init` pass or by `array_struct_temporary`.
static inline bool is_module_variable(const ASR::Variable_t &v) {
    return v.m_parent_symtab != nullptr
        && v.m_parent_symtab->asr_owner != nullptr
        && ASR::is_a<ASR::symbol_t>(*v.m_parent_symtab->asr_owner)
        && ASR::is_a<ASR::Module_t>(*ASR::down_cast<ASR::symbol_t>(
            v.m_parent_symtab->asr_owner));
}

// The designator `d` names storage whose address a linker can compute: a
// variable of a module, an element of one at constant indices, or a component
// of one.
bool has_link_time_address(ASR::expr_t* d);

// `p => tgt` in the declaration of the pointer `v`, a variable or a
// component, whose initial value is the address of the target, a link-time
// constant.
//
// An array pointer is deliberately not one of these. It is a descriptor --
// data pointer, bounds and strides -- rather than a bare address, and the
// descriptor is filled in from the target by the association itself. Neither
// is a character pointer, whose descriptor carries the length as well, nor a
// polymorphic pointer, whose type pointer is not the target's, nor one whose
// target is a coarray, whose storage the run time allocates.
static inline bool has_static_pointer_target(const ASR::Variable_t &v) {
    if (!is_pointer(v.m_type)
            || !is_pointer_association_initializer(v.m_symbolic_value)) {
        return false;
    }
    if (v.n_codims > 0 || is_array(v.m_type) || is_character(*v.m_type)
            || is_class_type(extract_type(v.m_type))) {
        return false;
    }
    return has_link_time_address(v.m_symbolic_value);
}

// `p => tgt` in a declaration that a backend lays out as the variable's own
// static initializer instead of assigning it with a statement (see
// `has_static_pointer_target`), so the association needs no executable code
// at all and the unit it is declared in needs no startup initializer because
// of it. A pointer of a procedure, of a block or of a program is not one of
// these: its declaration initializer becomes a statement of that scope
// whatever shape it has.
static inline bool is_static_pointer_association(const ASR::Variable_t &v) {
    return is_module_variable(v) && has_static_pointer_target(v);
}

// Every default initial target of a data pointer component of the derived
// type `struct_sym`, of its parents and of its derived type components is
// one that `has_static_pointer_target` lays out as static data.
bool has_only_static_component_targets(ASR::symbol_t* struct_sym);

// The code that describes a scalar of `type` to the runtime's formatted output
// (`_lcompilers_string_format_fortran` in `libasr/runtime/lfortran_intrinsics.c`),
// which every backend that calls it must produce the same way:
//
//     I<kind>, U<kind>      integer, unsigned integer
//     R<kind>               real
//     {R<kind>,R<kind>}     complex, as a pair of reals
//     L<bits>               logical, read as an integer of that many bits
//     S-DESC-K<kind>        character, as a string descriptor
//     S-CCHAR-K<kind>       character, as a pointer to its data
//     CPtr                  type(c_ptr)
//
// A character code leaves out the optional `-<len>` suffix, which a backend
// appends when the runtime should take the length from the code rather than
// from the value. An empty string means the type has no scalar code: an array
// or a derived type is described through its elements or components.
std::string get_format_type_code(ASR::ttype_t* type);

} // namespace ASRUtils

} // namespace LCompilers

#endif // LFORTRAN_ASR_UTILS_H

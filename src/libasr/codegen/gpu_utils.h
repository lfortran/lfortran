#ifndef LFORTRAN_GPU_UTILS_H
#define LFORTRAN_GPU_UTILS_H

#include <libasr/asr.h>
#include <libasr/asr_utils.h>

#include <functional>
#include <set>
#include <string>
#include <vector>

namespace LCompilers {

// One derived extent of an array or of a per-thread workspace, in the small
// grammar the offload machinery can both size a host buffer by and stride a
// device thread's slice by.
//
// The extent is derived exactly once, by gpu_derive_extent(), and every
// consumer renders that one derivation: the pre-flight asks whether it
// exists at all, the launch builds from it the host expression that sizes
// the buffer, and the device emitter writes from it the shader expression
// that strides the buffer. Deriving it once is what keeps the size the host
// allocates and the stride the device walks from drifting apart -- when
// they disagree the kernel reads past the end of its slice, silently.
enum class GpuExtentKind {
    None,       // nothing could be derived: the extent is not offloadable
    Constant,   // the integer `int_value`
    Opaque,     // the value of `expr`, spelled by whoever renders it
    Cast,       // an explicit integer conversion of children[0]
    BinOp,      // children[0] `binop` children[1]
    Neg,        // -children[0]
    Compare,    // children[0] `cmpop` children[1]
    Select,     // children[0] ? children[1] : children[2]
    Product,    // children[0] * children[1] * ...
    ArgScalar,  // the value of the kernel parameter `arg_index`
    ArgMember,  // the component `member_path` of parameter `arg_index`
    ArgElement, // the element at subscripts children[0..] of `arg_index`
    ArrayDim,   // extent `int_value` (0-based) of array parameter `arg_index`
    Size,       // size(`array`, `dim`), with children[0] the derived `dim`
    Bound,      // `bound`(`array`, `dim`), with children[0] the derived `dim`
};

struct GpuExtent {
    GpuExtentKind kind = GpuExtentKind::None;
    // The ASR node the extent was derived from. A leaf is rendered on the
    // device through this node, in the names the kernel binds; the host
    // rebuilds the same leaf over the actual arguments instead.
    ASR::expr_t *expr = nullptr;
    int64_t int_value = 0;
    ASR::binopType binop = ASR::binopType::Add;
    ASR::cmpopType cmpop = ASR::cmpopType::Eq;
    ASR::arrayboundType bound = ASR::arrayboundType::LBound;
    size_t arg_index = 0;
    std::string name;                     // ArrayDim: the array parameter
    std::vector<std::string> member_path; // ArgMember: the component chain
    // Size and Bound: the designator whose shape is read -- after the walk
    // down to the operand that actually carries it -- and the dimension
    // asked for, or nullptr for the whole element count.
    ASR::expr_t *array = nullptr;
    ASR::expr_t *dim = nullptr;
    std::vector<GpuExtent> children;

    bool ok() const { return kind != GpuExtentKind::None; }
};

// What an extent expression is read against: the kernel whose parameters it
// may name, the scope its other names are declared in, and the body those
// names are given their values in.
struct GpuExtentScope {
    const ASR::Function_t *kernel = nullptr;
    std::vector<std::string> arg_names;
    SymbolTable *symtab = nullptr;
    ASR::stmt_t **body = nullptr;
    size_t n_body = 0;

    bool is_arg(const std::string &name, size_t &index) const {
        for (size_t i = 0; i < arg_names.size(); i++) {
            if (arg_names[i] == name) {
                index = i;
                return true;
            }
        }
        return false;
    }
};

// Describes one dimension of a VLA workspace buffer. Every extent is one
// of exactly three things: a compile-time constant, the extents of a
// struct member the device reads out of a sizes buffer, or `derived` --
// which covers everything the host can work out for itself.
// An allocatable component of a struct variable: which variable, and which
// component of it.
//
// This is a key, not a designator: the same component of the same variable
// has to be recognised again in another routine and in another file, so it
// is carried by name rather than by symbol. The variable is a callee's dummy
// in one context and the kernel's own array in another, and the component is
// re-resolved against whichever struct type the reader is holding -- which
// may be a different Struct_t, or the parent one the component is inherited
// from. Neither name is a symbol the two sides could compare.
//
// What they can share is the shape of the key. Glued into one "a.b" string
// it was built at five places and taken back apart at three, each with its
// own find('.') and substr; kept as two fields it cannot be taken apart
// wrongly, and a component whose name has a dot in it is no longer a key
// that reads as a different component.
struct GpuStructMemberKey {
    std::string base;    // the struct variable
    std::string member;  // its allocatable array component

    bool empty() const { return base.empty() || member.empty(); }
    bool operator==(const GpuStructMemberKey &o) const {
        return base == o.base && member == o.member;
    }
    bool operator<(const GpuStructMemberKey &o) const {
        if (base != o.base) return base < o.base;
        return member < o.member;
    }
};

struct GpuVlaDim {
    ASR::expr_t *source_extent = nullptr;
    ASR::symbol_t *extent_parameter = nullptr;
    bool is_constant = true;
    int64_t constant_value = 1;
    // When true, size is read from a struct member's allocatable
    // array size, resolved at dispatch time from the struct array's
    // per-element sizes. Such an extent names the loop index, so it has
    // no form the host could evaluate and `derived` is empty for it.
    bool is_struct_member_size = false;
    GpuStructMemberKey struct_member_key;
    // Rank of that component. The sizes buffer holds this many extents
    // per element, so the element count is their product, not [0].
    size_t struct_member_rank = 1;
    // 0-based index of the struct-array element the extent names, or -1
    // when that index is not a compile-time constant. A per-thread
    // workspace cannot be strided by one element's product if another
    // thread may need a larger one.
    int64_t struct_member_elem_index = -1;
    // The one derivation of this extent. The launch builds the buffer size
    // from it and the emitter the per-thread stride, so neither works the
    // extent out for itself and neither can disagree with the other.
    GpuExtent derived;
};

// Describes a VLA workspace buffer required by a GPU kernel.
struct GpuVlaWorkspace {
    // The array the workspace stands in for. Null only for a workspace
    // built to ask whether some array *would* be offloadable -- there is
    // no variable behind that question yet.
    ASR::symbol_t *var = nullptr;
    std::string var_name;
    int buffer_index;
    int elem_size;
    std::vector<GpuVlaDim> dims;
};

// Every name of `names` that an expression reads. Used to order a scope's
// declarations by what they depend on rather than by what they are called.
class GpuScopeNameReader : public ASR::BaseWalkVisitor<GpuScopeNameReader> {
public:
    const std::set<std::string> &names;
    std::set<std::string> &found;

    GpuScopeNameReader(const std::set<std::string> &names,
        std::set<std::string> &found) : names(names), found(found) {}

    void visit_Var(const ASR::Var_t &x) {
        std::string name = ASRUtils::symbol_name(x.m_v);
        if (names.count(name)) found.insert(name);
    }
};

inline void gpu_read_scope_names(ASR::expr_t *e,
        const std::set<std::string> &names, std::set<std::string> &found) {
    if (e == nullptr || names.empty()) return;
    GpuScopeNameReader reader(names, found);
    reader.visit_expr(*e);
}

// The names a derived extent reads once it is written out. Only a leaf is
// written through the ASR node it came from; everything above a leaf is
// written from the derivation itself, so the names an operand mentioned
// before it was reduced away are not read.
inline void gpu_read_extent_names(const GpuExtent &e,
        const std::set<std::string> &names, std::set<std::string> &found) {
    switch (e.kind) {
        case GpuExtentKind::Constant:
            return;
        case GpuExtentKind::BinOp:
        case GpuExtentKind::Neg:
        case GpuExtentKind::Compare:
        case GpuExtentKind::Select:
        case GpuExtentKind::Product: {
            for (const GpuExtent &c : e.children) {
                gpu_read_extent_names(c, names, found);
            }
            return;
        }
        default: break;
    }
    gpu_read_scope_names(e.expr, names, found);
    gpu_read_scope_names(e.array, names, found);
    gpu_read_scope_names(e.dim, names, found);
    for (const GpuExtent &c : e.children) {
        gpu_read_extent_names(c, names, found);
    }
}

// The names one declaration of `symtab` reads from that same scope.
//
// A declaration is not always self-contained: the extent of a per-thread
// workspace slice, the length of a run-time sized local array and the value
// of a named constant are all expressions the scope's own names can appear
// in.
inline std::set<std::string> gpu_declaration_reads(ASR::Variable_t *var,
        const std::set<std::string> &names,
        const std::vector<GpuVlaWorkspace> &workspaces) {
    std::set<std::string> found;
    if (var->m_storage == ASR::storage_typeType::Parameter && var->m_value) {
        gpu_read_scope_names(var->m_value, names, found);
        return found;
    }
    std::string var_name(var->m_name);
    for (const GpuVlaWorkspace &ws : workspaces) {
        if (ws.var != &var->base) continue;
        for (const GpuVlaDim &dim : ws.dims) {
            if (dim.is_constant) continue;
            if (dim.is_struct_member_size) {
                if (names.count(dim.struct_member_key.base)) {
                    found.insert(dim.struct_member_key.base);
                }
                continue;
            }
            gpu_read_extent_names(dim.derived, names, found);
        }
        return found;
    }
    ASR::ttype_t *type = ASRUtils::type_get_past_allocatable_pointer(
        var->m_type);
    if (ASR::is_a<ASR::Array_t>(*type)) {
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
        for (size_t d = 0; d < arr->n_dims; d++) {
            gpu_read_scope_names(arr->m_dims[d].m_length, names, found);
            gpu_read_scope_names(arr->m_dims[d].m_start, names, found);
        }
    }
    return found;
}

// The order a scope's declarations are emitted in.
//
// A declaration written over another name of the same scope has to follow
// it. Emitted in name order, whether it does is decided by the spelling of
// the two names: sorted one way the shader does not compile, sorted the
// other it reads a local that holds nothing yet -- and a per-thread
// workspace slice strided by that is a wrong answer with no diagnostic.
//
// `defined_ahead` names the scope's own variables that already hold their
// values when its declarations begin -- a kernel's parameters, unpacked at
// the head of the body. They are not declared here and nothing waits for
// them.
//
// Names that read nothing of each other keep the order they had, so a
// scope whose declarations are all self-contained is emitted unchanged.
// A cycle cannot be ordered at all; the names in it keep their order too,
// so that the scope is still emitted in full and in a fixed order.
inline std::vector<ASR::Variable_t*> gpu_scope_declaration_order(
        SymbolTable *symtab,
        const std::vector<GpuVlaWorkspace> &workspaces,
        const std::set<std::string> &defined_ahead = {}) {
    std::vector<ASR::Variable_t*> vars;
    std::set<std::string> names;
    for (auto &item : symtab->get_scope()) {
        if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(item.second);
        if (defined_ahead.count(std::string(var->m_name))) continue;
        vars.push_back(var);
        names.insert(std::string(var->m_name));
    }
    std::vector<std::set<std::string>> reads;
    reads.reserve(vars.size());
    for (ASR::Variable_t *var : vars) {
        reads.push_back(gpu_declaration_reads(var, names, workspaces));
    }
    std::vector<ASR::Variable_t*> ordered;
    std::vector<bool> done(vars.size(), false);
    std::set<std::string> emitted;
    for (size_t n = 0; n < vars.size(); n++) {
        size_t pick = vars.size();
        for (size_t i = 0; i < vars.size(); i++) {
            if (done[i]) continue;
            bool ready = true;
            for (const std::string &r : reads[i]) {
                if (r != std::string(vars[i]->m_name) && !emitted.count(r)) {
                    ready = false;
                    break;
                }
            }
            if (ready) { pick = i; break; }
        }
        if (pick == vars.size()) {
            for (size_t i = 0; i < vars.size(); i++) {
                if (!done[i]) { pick = i; break; }
            }
        }
        done[pick] = true;
        emitted.insert(std::string(vars[pick]->m_name));
        ordered.push_back(vars[pick]);
    }
    return ordered;
}

// How the host runtime's argument slot holds one kernel parameter.
enum class GpuKernelParamKind {
    Buffer,           // slot points at the buffer pointer
    StructReference,  // slot points at the pointer to the struct
    ScalarStruct,     // slot points at the packed scalar struct itself
};

// One parameter of a generated device kernel, in the order the host runtime
// binds it. A dialect that has to unpack the runtime's argument array again --
// the CPU emulation of CUDA does -- reads the list back.
struct GpuKernelParam {
    std::string type;   // element type, or the struct name for a reference
    std::string name;
    GpuKernelParamKind kind;
};

// An allocatable array component of a kernel argument that is an array of a
// derived type reaches the device as three buffers: the elements' data laid
// out end to end, the per-element offsets into it, and the per-element sizes.
// The sizes buffer holds the *extents* of the component, one entry per
// dimension per element, in dimension order: element `k` of a rank `R`
// component occupies entries `k*R .. k*R + R - 1`. A rank-one component
// therefore keeps the single entry per element it has always had, that
// element's number of elements, and the buffer accounting below is unchanged;
// for a higher rank the element count is the product of the R entries.
// Carrying the extents rather than only the total is what lets the shader
// linearize `a(i)%m(p,q)`, which needs the extent of every dimension but the
// last. The host fills the buffer in device_launch_expand and the device
// reads it in asr_to_gpu_c.h; this is the one place the two agree.
// The column-major position of the element `subscripts` selects, as device
// source: the sum over every dimension of its subscript counted from its
// lower bound, times the extents of the dimensions before it.
//
// This is the one position both sides count elements by: the host lays a
// struct component's data out in array element order and the shader reads
// it back in that order, and an ordinary array is indexed the same way.
// Written once, so the two orders cannot come apart.
//
// `lower(d)` and `extent(d)` say how those are spelled where the caller
// reads them -- off the array's own type, or out of the sizes buffer a
// component's extents travel in. The last dimension is never counted past,
// so it needs no extent; an empty `extent(d)` says the caller has none for
// that dimension either, and an empty subscript that it has none for it.
inline std::string gpu_linearized_index_str(
        const std::vector<std::string> &subscripts,
        const std::function<std::string(size_t)> &lower,
        const std::function<std::string(size_t)> &extent) {
    std::string out, stride;
    for (size_t d = 0; d < subscripts.size(); d++) {
        if (subscripts[d].empty()) continue;
        std::string term = "((int)(" + subscripts[d] + ") - ("
            + lower(d) + "))";
        if (!stride.empty()) term = "(" + stride + " * " + term + ")";
        out += out.empty() ? term : (" + " + term);
        if (d + 1 < subscripts.size()) {
            std::string ext = extent(d);
            if (ext.empty()) continue;
            stride = stride.empty() ? ext
                : ("(" + stride + " * " + ext + ")");
        }
    }
    return out;
}

// The symbol `name` on `struct_sym` or a type it extends. Inherited
// components live in the parent Struct, so a lookup that only reads
// the child's table misses them.
inline ASR::symbol_t* gpu_struct_lookup_member(ASR::symbol_t *struct_sym,
        const std::string &name) {
    if (struct_sym == nullptr) return nullptr;
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(struct_sym);
    if (!s || !ASR::is_a<ASR::Struct_t>(*s)) return nullptr;
    ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(s);
    std::set<ASR::Struct_t*> seen;
    while (st != nullptr) {
        if (!seen.insert(st).second) break;
        ASR::symbol_t *member = st->m_symtab->get_symbol(name);
        if (member) return member;
        if (!st->m_parent) break;
        ASR::symbol_t *parent = ASRUtils::symbol_get_past_external(
            st->m_parent);
        if (!ASR::is_a<ASR::Struct_t>(*parent)) break;
        st = ASR::down_cast<ASR::Struct_t>(parent);
    }
    return nullptr;
}

inline size_t gpu_struct_member_rank(const ASR::Variable_t *var) {
    ASR::ttype_t *inner = ASRUtils::type_get_past_allocatable(var->m_type);
    if (!ASR::is_a<ASR::Array_t>(*inner)) return 0;
    return ASR::down_cast<ASR::Array_t>(inner)->n_dims;
}

// The integer, real and logical kinds the device languages have a type of the
// same width for. A buffer reaches the device as a block of bytes sized from
// the host element type, so a device type of a different width makes the
// kernel stride through it at the wrong size: it reads and writes the wrong
// elements, and nothing says so. `logical(8)` is the case that bites -- the
// emitter has no 64-bit boolean and used to fall back to a 4-byte `int`.
//
// The launch (device_launch_expand.cpp) and the emitter (asr_to_gpu_c.h) both
// go through this, so a scalar type either has one width on both sides or the
// loop is not offloaded at all.  What a dialect can represent on top of this
// is narrower still -- Metal has no 64-bit floating point type -- and is
// checked where the dialect is known.
inline bool gpu_scalar_width_supported(ASR::ttype_t *t) {
    switch (t->type) {
        case ASR::ttypeType::Integer:
            switch (ASR::down_cast<ASR::Integer_t>(t)->m_kind) {
                case 4: case 8: return true;
                default: return false;
            }
        case ASR::ttypeType::Real:
            switch (ASR::down_cast<ASR::Real_t>(t)->m_kind) {
                case 4: case 8: return true;
                default: return false;
            }
        case ASR::ttypeType::Logical:
            switch (ASR::down_cast<ASR::Logical_t>(t)->m_kind) {
                case 1: case 2: case 4: return true;
                default: return false;
            }
        default:
            return false;
    }
}

// The type as a user would write it, for a diagnostic that has to name the
// kind it is turning down: `logical(8)`, `real(16)`.
inline std::string gpu_scalar_type_name(ASR::ttype_t *t) {
    std::string base;
    switch (t->type) {
        case ASR::ttypeType::Integer: base = "integer"; break;
        case ASR::ttypeType::Real: base = "real"; break;
        case ASR::ttypeType::Logical: base = "logical"; break;
        case ASR::ttypeType::Complex: base = "complex"; break;
        default: return "that type";
    }
    return base + "(" +
        std::to_string(ASRUtils::extract_kind_from_ttype_t(t)) + ")";
}

// Classify kernel arguments into buffer (array/struct) and scalar categories.
// Returns the count of buffer args and scalar args respectively.
// For struct array args with allocatable array members, counts 3 extra
// buffers per member (data, offsets, sizes) as emitted by Metal codegen.
inline std::pair<int, int> classify_gpu_kernel_args(
        const ASR::Function_t &kernel) {
    int n_buffer = 0, n_scalar = 0;
    for (size_t i = 0; i < kernel.n_args; i++) {
        ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(kernel.m_args[i]);
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
            ASRUtils::symbol_get_past_external(v->m_v));
        ASR::ttype_t *type = var->m_type;
        if (ASRUtils::is_array(type) ||
                ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(type))) {
            n_buffer++;
            if (ASRUtils::is_array(type) && var->m_type_declaration) {
                ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                    var->m_type_declaration);
                if (ASR::is_a<ASR::Struct_t>(*s)) {
                    // Members inherited from the types this one extends are
                    // decomposed like its own, so they count here too.
                    n_buffer += 3 *
                        (int)ASRUtils::collect_allocatable_array_members(
                            ASR::down_cast<ASR::Struct_t>(s)).size();
                }
            }
        } else {
            n_scalar++;
        }
    }
    return {n_buffer, n_scalar};
}

// Helper to recursively find the first Allocate statement for a given
// variable name within a statement list.
inline ASR::Allocate_t* find_allocate_for_var(
        ASR::stmt_t **stmts, size_t n, const std::string &var_name) {
    for (size_t i = 0; i < n; i++) {
        if (ASR::is_a<ASR::Allocate_t>(*stmts[i])) {
            ASR::Allocate_t *alloc =
                ASR::down_cast<ASR::Allocate_t>(stmts[i]);
            for (size_t ai = 0; ai < alloc->n_args; ai++) {
                if (!alloc->m_args[ai].m_a) continue;
                if (!ASR::is_a<ASR::Var_t>(*alloc->m_args[ai].m_a))
                    continue;
                std::string aname = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(
                        alloc->m_args[ai].m_a)->m_v);
                if (aname == var_name) return alloc;
            }
        }
        if (ASR::is_a<ASR::WhileLoop_t>(*stmts[i])) {
            ASR::WhileLoop_t *wl =
                ASR::down_cast<ASR::WhileLoop_t>(stmts[i]);
            auto *r = find_allocate_for_var(wl->m_body, wl->n_body,
                var_name);
            if (r) return r;
        } else if (ASR::is_a<ASR::DoLoop_t>(*stmts[i])) {
            ASR::DoLoop_t *dl =
                ASR::down_cast<ASR::DoLoop_t>(stmts[i]);
            auto *r = find_allocate_for_var(dl->m_body, dl->n_body,
                var_name);
            if (r) return r;
        } else if (ASR::is_a<ASR::If_t>(*stmts[i])) {
            ASR::If_t *if_s =
                ASR::down_cast<ASR::If_t>(stmts[i]);
            auto *r = find_allocate_for_var(if_s->m_body, if_s->n_body,
                var_name);
            if (r) return r;
            r = find_allocate_for_var(if_s->m_orelse, if_s->n_orelse,
                var_name);
            if (r) return r;
        } else if (ASR::is_a<ASR::BlockCall_t>(*stmts[i])) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmts[i])->m_m);
            if (b == nullptr || !ASR::is_a<ASR::Block_t>(*b)) continue;
            ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
            auto *r = find_allocate_for_var(blk->m_body, blk->n_body,
                var_name);
            if (r) return r;
        }
    }
    return nullptr;
}

// A Fortran named constant carries its value on the symbol, not in the
// expression node: `integer, parameter :: end_point = 1` reaches the
// backends as a `Var` whose `Variable` holds an `IntegerConstant` in
// `m_value`. Such a name is as knowable ahead of the launch as the literal
// `1` is, so an extent that mentions one is host-evaluable. Returns the
// constant the expression folds to, or nullptr when it does not fold.
//
// The pre-flight, the host-side rebuild of the extent and the device-side
// rendering of it all ask this one question, so that the size the host
// allocates and the stride the device walks cannot disagree.
inline ASR::expr_t* gpu_folded_int_constant(ASR::expr_t *e) {
    if (e == nullptr) return nullptr;
    if (ASR::is_a<ASR::IntegerConstant_t>(*e)) return e;
    ASR::expr_t *val = ASRUtils::expr_value(e);
    if (val == nullptr || val == e) return nullptr;
    if (!ASR::is_a<ASR::IntegerConstant_t>(*val)) return nullptr;
    return val;
}

// Try to evaluate an ASR integer expression as a compile-time constant.
inline bool try_eval_int_constant(ASR::expr_t *e, int64_t &val) {
    if (!e) return false;
    if (ASR::is_a<ASR::IntegerConstant_t>(*e)) {
        val = ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n;
        return true;
    }
    ASR::expr_t *v = ASRUtils::expr_value(e);
    if (v && v != e) return try_eval_int_constant(v, val);
    if (ASR::is_a<ASR::Cast_t>(*e)) {
        return try_eval_int_constant(
            ASR::down_cast<ASR::Cast_t>(e)->m_arg, val);
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*e)) {
        int64_t l, r;
        auto *op = ASR::down_cast<ASR::IntegerBinOp_t>(e);
        if (!try_eval_int_constant(op->m_left, l)) return false;
        if (!try_eval_int_constant(op->m_right, r)) return false;
        switch (op->m_op) {
            case ASR::binopType::Add: val = l + r; return true;
            case ASR::binopType::Sub: val = l - r; return true;
            case ASR::binopType::Mul: val = l * r; return true;
            case ASR::binopType::Div:
                if (r != 0) { val = l / r; return true; }
                return false;
            default: return false;
        }
    }
    return false;
}

// Forward declaration for mutual recursion with
// try_resolve_array_size_via_associate.
inline bool try_resolve_alloc_dim_constant(
        ASR::expr_t *dim,
        ASR::stmt_t **body, size_t n_body,
        int64_t &result);

// Try to resolve ArraySize(ptr_var, dim) to a constant by tracing an
// Associate statement back to an ArraySection with constant bounds.
// If no Associate is found, traces through Allocate statements to
// resolve transitively (e.g. ArraySize(temp_merge) → temp_merge is
// allocated with ArraySize(temp_compare) → temp_compare is associated
// with a constant-bounds ArraySection).
inline bool try_resolve_array_size_via_associate(
        ASR::ArraySize_t *as,
        ASR::stmt_t **body, size_t n_body,
        int64_t &result) {
    if (!as->m_v || !ASR::is_a<ASR::Var_t>(*as->m_v)) return false;
    std::string var_name = ASRUtils::symbol_name(
        ASR::down_cast<ASR::Var_t>(as->m_v)->m_v);
    int64_t target_dim = 1;
    if (as->m_dim) {
        if (!try_eval_int_constant(as->m_dim, target_dim)) return false;
    }
    for (size_t i = 0; i < n_body; i++) {
        if (!ASR::is_a<ASR::Associate_t>(*body[i])) continue;
        ASR::Associate_t *assoc =
            ASR::down_cast<ASR::Associate_t>(body[i]);
        if (!ASR::is_a<ASR::Var_t>(*assoc->m_target)) continue;
        std::string tname = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(assoc->m_target)->m_v);
        if (tname != var_name) continue;
        if (!ASR::is_a<ASR::ArraySection_t>(*assoc->m_value)) return false;
        ASR::ArraySection_t *sec =
            ASR::down_cast<ASR::ArraySection_t>(assoc->m_value);
        int range_dim = 0;
        for (size_t d = 0; d < sec->n_args; d++) {
            ASR::array_index_t &idx = sec->m_args[d];
            if (idx.m_left == nullptr) continue;
            range_dim++;
            if (range_dim == target_dim) {
                int64_t start_val, end_val, stride_val = 1;
                if (!try_eval_int_constant(idx.m_left, start_val))
                    return false;
                if (!try_eval_int_constant(idx.m_right, end_val))
                    return false;
                if (idx.m_step &&
                        !try_eval_int_constant(idx.m_step, stride_val))
                    return false;
                if (stride_val == 0) return false;
                result = (end_val - start_val) / stride_val + 1;
                if (result < 0) result = 0;
                return true;
            }
        }
        return false;
    }
    // No Associate found — try tracing through an Allocate statement
    // for the same variable.  If its allocation dimension for
    // `target_dim` is itself resolvable to a constant, use that.
    ASR::Allocate_t *alloc = find_allocate_for_var(
        body, n_body, var_name);
    if (alloc) {
        for (size_t ai = 0; ai < alloc->n_args; ai++) {
            if (!alloc->m_args[ai].m_a) continue;
            if (!ASR::is_a<ASR::Var_t>(*alloc->m_args[ai].m_a)) continue;
            std::string aname = ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(
                    alloc->m_args[ai].m_a)->m_v);
            if (aname != var_name) continue;
            ASR::alloc_arg_t &targ = alloc->m_args[ai];
            if (target_dim < 1 ||
                    (size_t)target_dim > targ.n_dims)
                return false;
            ASR::expr_t *dim_expr =
                targ.m_dims[target_dim - 1].m_length;
            if (!dim_expr) return false;
            return try_resolve_alloc_dim_constant(
                dim_expr, body, n_body, result);
        }
    }
    return false;
}

// The extent an ArraySize reads straight off the type of what it measures.
// `size(a(i)%m)`, where the component is declared `real :: m(2)`, is 2
// whatever `i` is: the index is never evaluated, so the extent is the same
// for every thread and the workspace it sizes is a compile-time constant.
// A deferred shape carries no lengths in its type, so this says nothing
// about an allocatable or a pointer.
inline bool try_resolve_array_size_from_type(ASR::ArraySize_t *as,
        int64_t &result) {
    if (as->m_v == nullptr) return false;
    ASR::ttype_t *t = ASRUtils::expr_type(as->m_v);
    if (t == nullptr) return false;
    if (ASRUtils::is_allocatable(t) || ASRUtils::is_pointer(t)) return false;
    ASR::dimension_t *dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(t, dims);
    if (n_dims == 0 || dims == nullptr) return false;
    if (as->m_dim != nullptr) {
        int64_t d = 0;
        if (!try_eval_int_constant(as->m_dim, d)) return false;
        if (d < 1 || (size_t)d > n_dims) return false;
        if (dims[d - 1].m_length == nullptr) return false;
        return ASRUtils::extract_value(
            ASRUtils::expr_value(dims[d - 1].m_length), result);
    }
    int64_t total = ASRUtils::get_fixed_size_of_array(dims, n_dims);
    if (total < 0) return false;
    result = total;
    return true;
}

// Try to resolve an Allocate dimension to a compile-time constant,
// including tracing ArraySize through Associate statements.
inline bool try_resolve_alloc_dim_constant(
        ASR::expr_t *dim,
        ASR::stmt_t **body, size_t n_body,
        int64_t &result) {
    if (try_eval_int_constant(dim, result)) return true;
    ASR::expr_t *e = ASRUtils::get_past_array_physical_cast(dim);
    while (ASR::is_a<ASR::Cast_t>(*e)) {
        e = ASR::down_cast<ASR::Cast_t>(e)->m_arg;
    }
    if (ASR::is_a<ASR::ArraySize_t>(*e)) {
        ASR::ArraySize_t *as = ASR::down_cast<ASR::ArraySize_t>(e);
        if (try_resolve_array_size_from_type(as, result)) return true;
        return try_resolve_array_size_via_associate(as, body, n_body, result);
    }
    return false;
}

// The names the offload machinery gives to the symbols it synthesises.
//
// A generated name is a contract between two halves of the machinery: the
// offload pass creates a symbol under it, and the device emitter -- another
// file away -- writes that symbol out or looks it up again by the same name.
// Spelled afresh at each site, the two halves can drift apart, and they
// drift silently: a prefix typed one way here and another way there is a
// parameter that is simply never found. So every spelling lives here, one
// function per kind of synthesised symbol, and a name can only be built the
// one way it is matched.
//
// Nothing in here decides *what* to generate -- only how the thing it is
// given is spelled. Names with a single construction site and no matcher
// (the kernel function itself, a loop temporary) do not need a contract and
// stay where they are made.
namespace GpuNames {

// The kernel's scalar parameter carrying extent `d` (0-based) of the array
// parameter `array`, as the offload pass creates it and the extent
// resolvers look it up again.
inline std::string dim_arg(const std::string &array, size_t d) {
    return "__dim_" + array + "_" + std::to_string(d);
}

// The scalar parameter carrying extent `d` (0-based) of the array dummy
// `array` of a kernel, or of a routine spliced into one: pass_array_by_data
// names an assumed-shape dummy's extents after the dummy itself.
inline std::string dim_size(const std::string &array, size_t d) {
    return "__size_" + array + "_dim" + std::to_string(d + 1);
}

// The scalar parameter carrying the whole element count of the array dummy
// `array`, for a dummy whose extents are not passed one by one.
inline std::string array_size(const std::string &array) {
    return "__size_" + array;
}

// The scalar parameter carrying the whole element count of the allocatable
// array component `member` of the struct argument `var`.
inline std::string member_size(const std::string &var,
        const std::string &member) {
    return "__size_" + var + "_" + member;
}

// The scalar parameter carrying extent `d` (0-based) of that component, so
// that size(var%member, d) inside a spliced routine reads the extent rather
// than the element count.
inline std::string member_dim_size(const std::string &var,
        const std::string &member, size_t d) {
    return member_size(var, member) + "_dim" + std::to_string(d + 1);
}

// The device buffer holding the elements of that component.
inline std::string member_data(const std::string &var,
        const std::string &member) {
    return "__data_" + var + "_" + member;
}

// Where each struct element's component starts in `member_data`, when the
// argument is an array of structs and the components are flattened into one
// buffer behind it.
inline std::string member_offsets(const std::string &var,
        const std::string &member) {
    return "__offsets_" + var + "_" + member;
}

// How far each struct element's component runs there, one extent per
// dimension per element.
inline std::string member_sizes(const std::string &var,
        const std::string &member) {
    return "__sizes_" + var + "_" + member;
}

// The scalar parameter carrying the lower bound of dimension `d` (0-based)
// of the array argument `array`, for an array whose subscripts are counted
// from something other than one.
inline std::string lower_bound(const std::string &array, size_t d) {
    return "__lb_" + array + "_" + std::to_string(d);
}

// The device buffer backing the per-thread workspace of the kernel-local
// allocatable array `var`, whose extent is only known once the kernel runs.
inline std::string vla_workspace(const std::string &var) {
    return "__vla_" + var;
}

} // namespace GpuNames

// The array and the 0-based dimension whose extent `e` is, when `e` is one
// dimension of an array designator however it is spelled: `size(a, d)`,
// `size(a)` on a rank-1 array, or the `ubound(a,d) - lbound(a,d) + 1` that
// a whole dimension of an assumed-shape array is lowered to.
inline bool gpu_extent_of_array_dim(ASR::expr_t *e, std::string &name,
        size_t &dim) {
    if (e == nullptr) return false;
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
    while (ASR::is_a<ASR::Cast_t>(*v)) {
        v = ASR::down_cast<ASR::Cast_t>(v)->m_arg;
    }
    auto array_name = [](ASR::expr_t *a, std::string &out) {
        ASR::expr_t *b = ASRUtils::get_past_array_physical_cast(a);
        if (!b || !ASR::is_a<ASR::Var_t>(*b)) return false;
        out = ASRUtils::symbol_name(ASR::down_cast<ASR::Var_t>(b)->m_v);
        return true;
    };
    if (ASR::is_a<ASR::ArraySize_t>(*v)) {
        ASR::ArraySize_t *sz = ASR::down_cast<ASR::ArraySize_t>(v);
        if (!array_name(sz->m_v, name)) return false;
        if (sz->m_dim == nullptr) {
            // Only unambiguous on a rank-1 array, where the total size is
            // the one dimension's extent.
            ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(sz->m_v));
            if (!t || !ASR::is_a<ASR::Array_t>(*t)) return false;
            if (ASR::down_cast<ASR::Array_t>(t)->n_dims != 1) return false;
            dim = 0;
            return true;
        }
        int64_t d;
        if (!try_eval_int_constant(sz->m_dim, d) || d < 1) return false;
        dim = (size_t)d - 1;
        return true;
    }
    // (ubound(a,d) - lbound(a,d)) + 1
    if (!ASR::is_a<ASR::IntegerBinOp_t>(*v)) return false;
    ASR::IntegerBinOp_t *add = ASR::down_cast<ASR::IntegerBinOp_t>(v);
    int64_t one;
    if (add->m_op != ASR::binopType::Add) return false;
    if (!try_eval_int_constant(add->m_right, one) || one != 1) return false;
    ASR::expr_t *l = ASRUtils::get_past_array_physical_cast(add->m_left);
    if (!ASR::is_a<ASR::IntegerBinOp_t>(*l)) return false;
    ASR::IntegerBinOp_t *sub = ASR::down_cast<ASR::IntegerBinOp_t>(l);
    if (sub->m_op != ASR::binopType::Sub) return false;
    if (!ASR::is_a<ASR::ArrayBound_t>(*sub->m_left)) return false;
    if (!ASR::is_a<ASR::ArrayBound_t>(*sub->m_right)) return false;
    ASR::ArrayBound_t *ub = ASR::down_cast<ASR::ArrayBound_t>(sub->m_left);
    ASR::ArrayBound_t *lb = ASR::down_cast<ASR::ArrayBound_t>(sub->m_right);
    if (ub->m_bound != ASR::arrayboundType::UBound) return false;
    if (lb->m_bound != ASR::arrayboundType::LBound) return false;
    std::string lname;
    if (!array_name(ub->m_v, name) || !array_name(lb->m_v, lname)) {
        return false;
    }
    if (name != lname) return false;
    int64_t ud, ld;
    if (!try_eval_int_constant(ub->m_dim, ud)) return false;
    if (!try_eval_int_constant(lb->m_dim, ld)) return false;
    if (ud != ld || ud < 1) return false;
    dim = (size_t)ud - 1;
    return true;
}

// The kernel argument and component chain an extent reads, when the extent
// is a scalar component of a derived-type kernel argument -- `s%m_`, or
// `s%in_%k_`. The kernel is handed `s` as a buffer, so the extent is not a
// scalar parameter it could be read from; the host reads the component
// itself before it launches.
inline bool resolve_extent_to_arg_member(ASR::expr_t *e,
        const std::vector<std::string> &arg_names, size_t &arg_index,
        std::vector<std::string> &member_path) {
    if (e == nullptr) return false;
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
    while (ASR::is_a<ASR::Cast_t>(*v)) {
        v = ASR::down_cast<ASR::Cast_t>(v)->m_arg;
    }
    if (!ASR::is_a<ASR::StructInstanceMember_t>(*v)) return false;
    std::vector<std::string> reversed;
    while (ASR::is_a<ASR::StructInstanceMember_t>(*v)) {
        ASR::StructInstanceMember_t *sm =
            ASR::down_cast<ASR::StructInstanceMember_t>(v);
        reversed.push_back(ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(sm->m_m)));
        v = ASRUtils::get_past_array_physical_cast(sm->m_v);
    }
    // Only a chain rooted at a whole kernel argument: a subscript in the
    // middle names an element the host cannot pick, because the index only
    // exists on the device.
    if (!ASR::is_a<ASR::Var_t>(*v)) return false;
    std::string root = ASRUtils::symbol_name(
        ASR::down_cast<ASR::Var_t>(v)->m_v);
    for (size_t a = 0; a < arg_names.size(); a++) {
        if (arg_names[a] != root) continue;
        member_path.assign(reversed.rbegin(), reversed.rend());
        arg_index = a;
        return true;
    }
    return false;
}

// The extent expression of one dimension of a local array of `symtab`,
// taken from the array's own type or from the `allocate` that gives it a
// shape. An extent written over another local -- `size(t) + 1` -- is
// resolved by asking this and carrying on through the answer.
inline ASR::alloc_arg_t* find_alloc_arg_for_var(ASR::Allocate_t *alloc,
        const std::string &var_name);

inline ASR::expr_t* gpu_local_array_extent(SymbolTable *symtab,
        ASR::stmt_t **body, size_t n_body, const std::string &name,
        size_t dim) {
    if (symtab == nullptr) return nullptr;
    ASR::symbol_t *sym = symtab->resolve_symbol(name);
    if (sym == nullptr || !ASR::is_a<ASR::Variable_t>(*sym)) return nullptr;
    ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
    if (ASRUtils::is_allocatable(var->m_type)) {
        ASR::Allocate_t *alloc = find_allocate_for_var(body, n_body, name);
        if (alloc == nullptr) return nullptr;
        ASR::alloc_arg_t *arg = find_alloc_arg_for_var(alloc, name);
        if (arg == nullptr || dim >= arg->n_dims) return nullptr;
        return arg->m_dims[dim].m_length;
    }
    ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(
        var->m_type);
    if (!ASR::is_a<ASR::Array_t>(*t)) return nullptr;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(t);
    if (dim >= arr->n_dims) return nullptr;
    return arr->m_dims[dim].m_length;
}

// The subscripts of an array section that span a range, in order. The
// section's rank is their number, and the extent of its `d`-th dimension is
// `(right - left) / step + 1` -- a value the host can work out whenever
// those three are, whatever the scalar subscripts alongside them are.
inline std::vector<ASR::array_index_t*> gpu_section_ranges(
        ASR::ArraySection_t *sec) {
    std::vector<ASR::array_index_t*> ranges;
    for (size_t i = 0; i < sec->n_args; i++) {
        if (sec->m_args[i].m_left == nullptr
                && sec->m_args[i].m_step == nullptr) {
            continue;
        }
        if (sec->m_args[i].m_left == nullptr
                || sec->m_args[i].m_right == nullptr) {
            return {};
        }
        ranges.push_back(&sec->m_args[i]);
    }
    return ranges;
}

// The range subscripts `size(section, dim)` stands for: one for a given
// dimension, all of them -- whose extents multiply to the element count --
// without one. Empty when the section has no shape to read this way.
inline std::vector<ASR::array_index_t*> gpu_section_extent_ranges(
        ASR::expr_t *array, ASR::expr_t *dim) {
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(array);
    if (v == nullptr || !ASR::is_a<ASR::ArraySection_t>(*v)) return {};
    std::vector<ASR::array_index_t*> ranges = gpu_section_ranges(
        ASR::down_cast<ASR::ArraySection_t>(v));
    if (ranges.empty()) return {};
    if (dim == nullptr) return ranges;
    int64_t d;
    if (!try_eval_int_constant(dim, d) || d < 1
            || (size_t)d > ranges.size()) {
        return {};
    }
    return {ranges[(size_t)d - 1]};
}

// The extents `size(array, dim)` stands for, read from the shape in the
// expression's own type. An expression that is not a designator still
// carries its shape there: a function result declared `real :: r(n)`
// records `n` as the extent of its one dimension, and semantics has already
// rewritten that in terms of the actual arguments, so the extent is written
// in symbols of the scope the call is made from -- which is the scope the
// kernel arguments come from. Whether the host can reproduce those
// expressions is then the same question as for any other extent.
// `dim` selects one dimension; without it the answer is every dimension,
// whose product is the element count.
inline bool gpu_expr_shape_extents(ASR::expr_t *array, ASR::expr_t *dim,
        std::vector<ASR::expr_t*> &lengths) {
    if (array == nullptr) return false;
    ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(
        ASRUtils::expr_type(array));
    if (t == nullptr || !ASR::is_a<ASR::Array_t>(*t)) return false;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(t);
    if (arr->n_dims == 0) return false;
    size_t begin = 0, end = arr->n_dims;
    if (dim != nullptr) {
        int64_t d;
        if (!try_eval_int_constant(dim, d) || d < 1
                || (size_t)d > arr->n_dims) {
            return false;
        }
        begin = (size_t)d - 1;
        end = begin + 1;
    }
    for (size_t d = begin; d < end; d++) {
        if (!arr->m_dims[d].m_length) return false;
        lengths.push_back(arr->m_dims[d].m_length);
    }
    return true;
}

// The sub-expression an elementwise array expression takes its shape from,
// or nullptr when `e` is not one.
//
// An elementwise operator conforms with its array operands, so
// `0.5*(a(1:n-1) + a(2:n))` has exactly the shape of `a(1:n-1)`, and a
// scalar broadcast into such an expression carries the shape it was
// broadcast against rather than one of its own. An expression like that has
// no shape in its own type -- the array constructor lowering writes the
// extent of its temporary as `size(<that expression>)` -- so a caller that
// wants the extent walks down to an operand that does carry it. Both the
// pre-flight in the offload pass and the launch that rebuilds the extent on
// the host walk it the same way, so they cannot disagree about the size.
inline ASR::expr_t* gpu_elementwise_shape_source(ASR::expr_t *e) {
    if (e == nullptr) return nullptr;
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
    if (v == nullptr || !ASRUtils::is_array(ASRUtils::expr_type(v))) {
        return nullptr;
    }
    // An operand that is itself an array. A broadcast scalar is one too,
    // but it borrows its shape from the operand beside it, so it is only
    // taken when nothing else is there -- and then it has no shape to give.
    auto pick = [](ASR::expr_t *left, ASR::expr_t *right) -> ASR::expr_t* {
        ASR::expr_t *operands[2] = {left, right};
        for (ASR::expr_t *o : operands) {
            if (o == nullptr) continue;
            ASR::expr_t *b = ASRUtils::get_past_array_physical_cast(o);
            if (b == nullptr || ASR::is_a<ASR::ArrayBroadcast_t>(*b)) {
                continue;
            }
            if (ASRUtils::is_array(ASRUtils::expr_type(b))) return b;
        }
        return nullptr;
    };
    auto unary = [](ASR::expr_t *arg) -> ASR::expr_t* {
        if (arg == nullptr) return nullptr;
        ASR::expr_t *b = ASRUtils::get_past_array_physical_cast(arg);
        if (b == nullptr || !ASRUtils::is_array(ASRUtils::expr_type(b))) {
            return nullptr;
        }
        return b;
    };
    switch (v->type) {
        case ASR::exprType::Cast:
            return unary(ASR::down_cast<ASR::Cast_t>(v)->m_arg);
        case ASR::exprType::IntegerUnaryMinus:
            return unary(ASR::down_cast<ASR::IntegerUnaryMinus_t>(v)->m_arg);
        case ASR::exprType::RealUnaryMinus:
            return unary(ASR::down_cast<ASR::RealUnaryMinus_t>(v)->m_arg);
        case ASR::exprType::ComplexUnaryMinus:
            return unary(ASR::down_cast<ASR::ComplexUnaryMinus_t>(v)->m_arg);
        case ASR::exprType::LogicalNot:
            return unary(ASR::down_cast<ASR::LogicalNot_t>(v)->m_arg);
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t *o = ASR::down_cast<ASR::IntegerBinOp_t>(v);
            return pick(o->m_left, o->m_right);
        }
        case ASR::exprType::RealBinOp: {
            ASR::RealBinOp_t *o = ASR::down_cast<ASR::RealBinOp_t>(v);
            return pick(o->m_left, o->m_right);
        }
        case ASR::exprType::ComplexBinOp: {
            ASR::ComplexBinOp_t *o = ASR::down_cast<ASR::ComplexBinOp_t>(v);
            return pick(o->m_left, o->m_right);
        }
        case ASR::exprType::LogicalBinOp: {
            ASR::LogicalBinOp_t *o = ASR::down_cast<ASR::LogicalBinOp_t>(v);
            return pick(o->m_left, o->m_right);
        }
        case ASR::exprType::IntegerCompare: {
            ASR::IntegerCompare_t *o =
                ASR::down_cast<ASR::IntegerCompare_t>(v);
            return pick(o->m_left, o->m_right);
        }
        case ASR::exprType::RealCompare: {
            ASR::RealCompare_t *o = ASR::down_cast<ASR::RealCompare_t>(v);
            return pick(o->m_left, o->m_right);
        }
        case ASR::exprType::ComplexCompare: {
            ASR::ComplexCompare_t *o =
                ASR::down_cast<ASR::ComplexCompare_t>(v);
            return pick(o->m_left, o->m_right);
        }
        case ASR::exprType::LogicalCompare: {
            ASR::LogicalCompare_t *o =
                ASR::down_cast<ASR::LogicalCompare_t>(v);
            return pick(o->m_left, o->m_right);
        }
        default:
            return nullptr;
    }
}

// Every routine a statement list calls, past external symbols and type bound
// procedure declarations.
//
// `procedure_values` says whether naming a routine without calling it --
// handing it over as an actual argument, say -- counts. It does when the
// question is what device code can reach, because whoever receives the
// routine can call it; it does not when the question is what a kernel body
// has to have spliced into it, which is only what that body actually calls.
inline std::set<ASR::Function_t*> gpu_callees(ASR::stmt_t **body,
        size_t n_body, bool procedure_values) {
    return ASRUtils::get_called_functions(body, n_body, procedure_values);
}

// Counts the writes to one scalar in a statement list, keeping the value of
// the last one. A name written exactly once stands for that value
// everywhere.
class GpuScalarBindingCounter :
        public ASRUtils::BlockBodyWalkVisitor<GpuScalarBindingCounter> {
public:
    ASR::symbol_t *target;
    size_t n_writes = 0;
    ASR::expr_t *value = nullptr;

    GpuScalarBindingCounter(ASR::symbol_t *target_) : target(target_) {}

    bool is_target(ASR::expr_t *e) {
        return e != nullptr && ASR::is_a<ASR::Var_t>(*e) &&
            ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v) == target;
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        if (is_target(x.m_target)) {
            n_writes++;
            value = x.m_value;
        }
        ASR::BaseWalkVisitor<GpuScalarBindingCounter>::visit_Assignment(x);
    }

    void visit_Associate(const ASR::Associate_t &x) {
        if (is_target(x.m_target)) {
            n_writes++;
            value = x.m_value;
        }
        ASR::BaseWalkVisitor<GpuScalarBindingCounter>::visit_Associate(x);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        if (is_target(x.m_head.m_v)) n_writes++;
        ASR::BaseWalkVisitor<GpuScalarBindingCounter>::visit_DoLoop(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (is_target(x.m_args[i].m_value)) n_writes++;
        }
        ASR::BaseWalkVisitor<GpuScalarBindingCounter>
            ::visit_SubroutineCall(x);
    }
};

// The operand a `size()` over `e` reads its shape off.
//
// An elementwise array expression carries no shape of its own, so the walk
// descends through its operands until it reaches something that does: a
// name, a struct component, or a section, whose ranges span the extent by
// themselves whatever the base is. Every consumer walks down to the same
// operand, so the count the device works out and the count the host sized
// a buffer with are read off the same expression.
inline ASR::expr_t* gpu_shape_source(ASR::expr_t *e, ASR::expr_t *dim) {
    ASR::expr_t *out = e;
    for (int hop = 0; hop < 8 && out != nullptr; hop++) {
        if (ASR::is_a<ASR::Var_t>(*out)
                || ASR::is_a<ASR::StructInstanceMember_t>(*out)
                || !gpu_section_extent_ranges(out, dim).empty()) {
            break;
        }
        ASR::expr_t *next = gpu_elementwise_shape_source(out);
        if (next == nullptr) break;
        out = next;
    }
    return out == nullptr ? e : out;
}

// The value bound to the integer scalar `name` in `body`, or nullptr when
// the name is not defined exactly once there. This is how a workspace
// extent reaches through an ASSOCIATE name: once the offload pass splices
// the construct in, `associate(rows => self%m_ + 2)` shows up as a local
// `rows` assigned `self%m_ + 2` once, and only the selector expression is
// something the host can evaluate.
inline ASR::expr_t* gpu_local_scalar_binding(ASR::symbol_t *sym,
        ASR::stmt_t **body, size_t n_body) {
    if (sym == nullptr) return nullptr;
    sym = ASRUtils::symbol_get_past_external(sym);
    if (sym == nullptr || !ASR::is_a<ASR::Variable_t>(*sym)) return nullptr;
    ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
    if (ASRUtils::is_array(var->m_type)) return nullptr;
    if (!ASR::is_a<ASR::Integer_t>(*ASRUtils::extract_type(var->m_type))) {
        return nullptr;
    }
    GpuScalarBindingCounter counter(sym);
    for (size_t i = 0; i < n_body; i++) {
        counter.visit_stmt(*body[i]);
    }
    if (counter.n_writes != 1) return nullptr;
    return counter.value;
}

// The selector bound to the array name `sym` in `body`, or nullptr when
// the name is not defined exactly once there. An ASSOCIATE array name is
// a pointer; its selector is the section or designator whose extents
// `size(name)` actually stands for.
inline ASR::expr_t* gpu_local_array_binding(ASR::symbol_t *sym,
        ASR::stmt_t **body, size_t n_body) {
    if (sym == nullptr || body == nullptr) return nullptr;
    sym = ASRUtils::symbol_get_past_external(sym);
    if (sym == nullptr || !ASR::is_a<ASR::Variable_t>(*sym)) return nullptr;
    ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
    ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(
        var->m_type);
    if (t == nullptr || !ASR::is_a<ASR::Array_t>(*t)) return nullptr;
    GpuScalarBindingCounter counter(sym);
    for (size_t i = 0; i < n_body; i++) {
        counter.visit_stmt(*body[i]);
    }
    if (counter.n_writes != 1) return nullptr;
    return counter.value;
}

// Whether the host can read the designator `e`: a kernel parameter, or a
// component or element reached from one, with every subscript something the
// host can work out too. `self%points_(1,1,1,1)%values_` is such a
// designator; `a(i)` for a loop index `i` is not, the index existing only
// once the kernel runs.
inline bool gpu_designator_is_host_readable(ASR::expr_t *e,
        const GpuExtentScope &scope);

// The one derivation of an extent. Returns the derived form, or a `None`
// extent when `e` is not something the host could work out -- a call, a
// subscript that only exists on the device. Arithmetic over integer
// literals, scalar parameters, scalar components of parameters and the
// extents of array parameters derives; anything else does not.
//
// Every consumer renders this one result rather than deciding for itself
// what the extent is, so the pre-flight that accepts a loop, the buffer the
// launch allocates and the stride the kernel walks cannot disagree.
inline GpuExtent gpu_derive_extent(ASR::expr_t *e,
        const GpuExtentScope &scope, int depth = 0);

// The kernel argument supplying that extent. An array parameter's extents
// are handed to the kernel as scalar parameters of their own, so an extent
// written over the array itself is read on the host from the same
// parameter the device reads it from.
inline bool resolve_extent_to_dim_arg(ASR::expr_t *e,
        const std::vector<std::string> &arg_names, size_t &arg_index) {
    std::string name;
    size_t dim = 0;
    if (!gpu_extent_of_array_dim(e, name, dim)) return false;
    std::string want = GpuNames::dim_arg(name, dim);
    for (size_t a = 0; a < arg_names.size(); a++) {
        if (arg_names[a] == want) {
            arg_index = a;
            return true;
        }
    }
    return false;
}

// The extent of a local array declared in a BLOCK the body opens. Passes
// that run after the offload pass -- array_struct_temporary is one -- create
// such locals, so a lookup that only reads one symbol table misses them.
inline ASR::expr_t* gpu_nested_local_array_extent(ASR::stmt_t **body,
        size_t n_body, const std::string &name, size_t dim) {
    for (size_t i = 0; i < n_body; i++) {
        ASR::stmt_t *stmt = body[i];
        if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            if (!b || !ASR::is_a<ASR::Block_t>(*b)) continue;
            ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
            ASR::expr_t *found = gpu_local_array_extent(blk->m_symtab,
                blk->m_body, blk->n_body, name, dim);
            if (found) return found;
            found = gpu_nested_local_array_extent(blk->m_body, blk->n_body,
                name, dim);
            if (found) return found;
        } else if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            ASR::expr_t *found = gpu_nested_local_array_extent(dl->m_body,
                dl->n_body, name, dim);
            if (found) return found;
        } else if (ASR::is_a<ASR::If_t>(*stmt)) {
            ASR::If_t *ifs = ASR::down_cast<ASR::If_t>(stmt);
            ASR::expr_t *found = gpu_nested_local_array_extent(ifs->m_body,
                ifs->n_body, name, dim);
            if (found) return found;
            found = gpu_nested_local_array_extent(ifs->m_orelse,
                ifs->n_orelse, name, dim);
            if (found) return found;
        }
    }
    return nullptr;
}

// The extent expression of one dimension of an array that is local to the
// kernel, wherever in it the array is declared.
inline ASR::expr_t* gpu_scope_local_array_extent(const GpuExtentScope &scope,
        const std::string &name, size_t dim) {
    ASR::expr_t *e = gpu_local_array_extent(scope.symtab, scope.body,
        scope.n_body, name, dim);
    if (e) return e;
    if (scope.kernel != nullptr) {
        e = gpu_local_array_extent(scope.kernel->m_symtab,
            scope.kernel->m_body, scope.kernel->n_body, name, dim);
        if (e) return e;
        return gpu_nested_local_array_extent(scope.kernel->m_body,
            scope.kernel->n_body, name, dim);
    }
    return gpu_nested_local_array_extent(scope.body, scope.n_body, name, dim);
}

// The product of already derived factors, or `None` when there are none or
// one of them did not derive.
inline GpuExtent gpu_extent_product(std::vector<GpuExtent> &&factors) {
    GpuExtent out;
    if (factors.empty()) return out;
    for (const GpuExtent &f : factors) {
        if (!f.ok()) return out;
    }
    if (factors.size() == 1) return std::move(factors[0]);
    out.kind = GpuExtentKind::Product;
    out.children = std::move(factors);
    return out;
}

// The literal integer `n`, as a derived extent.
inline GpuExtent gpu_extent_literal(int64_t n) {
    GpuExtent out;
    out.kind = GpuExtentKind::Constant;
    out.int_value = n;
    return out;
}

// The value of `e`, left for whoever renders the extent to spell. A leaf
// the host has no counterpart for: a section whose bounds are values the
// kernel alone holds is still an extent the shader can write out, in the
// names the kernel binds.
inline GpuExtent gpu_extent_opaque(ASR::expr_t *e) {
    GpuExtent out;
    if (e == nullptr) return out;
    out.kind = GpuExtentKind::Opaque;
    out.expr = e;
    return out;
}

// Fortran's `(hi - lo) / step + 1`, the extent one range subscript of a
// section spans, over three operands each side has already put in the form
// it can render. Written once here, so that the buffer the host sizes by it
// and the stride the device walks by it cannot be different formulas.
inline GpuExtent gpu_range_extent_of(GpuExtent lo, GpuExtent hi,
        GpuExtent step) {
    GpuExtent none;
    if (!lo.ok() || !hi.ok() || !step.ok()) return none;
    GpuExtent span;
    span.kind = GpuExtentKind::BinOp;
    span.binop = ASR::binopType::Sub;
    span.children.push_back(std::move(hi));
    span.children.push_back(std::move(lo));
    GpuExtent whole;
    whole.kind = GpuExtentKind::BinOp;
    whole.binop = ASR::binopType::Div;
    whole.children.push_back(std::move(span));
    whole.children.push_back(std::move(step));
    GpuExtent out;
    out.kind = GpuExtentKind::BinOp;
    out.binop = ASR::binopType::Add;
    out.children.push_back(std::move(whole));
    out.children.push_back(gpu_extent_literal(1));
    return out;
}

// That extent as the host derives it: every operand has to be something the
// host can work out for itself, or nothing derives at all.
inline GpuExtent gpu_derive_range_extent(ASR::array_index_t *range,
        const GpuExtentScope &scope, int depth) {
    return gpu_range_extent_of(
        gpu_derive_extent(range->m_left, scope, depth),
        gpu_derive_extent(range->m_right, scope, depth),
        range->m_step != nullptr
            ? gpu_derive_extent(range->m_step, scope, depth)
            : gpu_extent_literal(1));
}

// That same extent as the device spells it: each operand is written out
// through the node the section holds, which the shader can always do -- it
// is inside the kernel, where every name the bounds mention has a value.
inline GpuExtent gpu_device_range_extent(ASR::array_index_t *range) {
    return gpu_range_extent_of(
        gpu_extent_opaque(range->m_left),
        gpu_extent_opaque(range->m_right),
        range->m_step != nullptr
            ? gpu_extent_opaque(range->m_step)
            : gpu_extent_literal(1));
}

inline GpuExtent gpu_derive_extent(ASR::expr_t *e,
        const GpuExtentScope &scope, int depth) {
    GpuExtent none;
    if (e == nullptr || depth > 8) return none;
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
    // A compile-time constant -- a literal, or a name declared `parameter`
    // -- is known before the launch, so the host can size a workspace by
    // it. The value lives on the symbol rather than in the node, so a
    // `Var` has to be asked for it explicitly.
    if (ASR::expr_t *k = gpu_folded_int_constant(v)) {
        GpuExtent out;
        out.kind = GpuExtentKind::Constant;
        out.expr = k;
        out.int_value = ASR::down_cast<ASR::IntegerConstant_t>(k)->m_n;
        return out;
    }
    if (ASR::is_a<ASR::Cast_t>(*v)) {
        GpuExtent argument = gpu_derive_extent(
            ASR::down_cast<ASR::Cast_t>(v)->m_arg, scope, depth);
        if (!argument.ok()) return none;
        GpuExtent out;
        out.kind = GpuExtentKind::Cast;
        out.expr = v;
        out.children.push_back(std::move(argument));
        return out;
    }
    // One dimension of an array, however it is spelled: `size(a, d)`, or
    // the `ubound(a,d) - lbound(a,d) + 1` an assumed-shape dummy is lowered
    // to. Recognised before the arithmetic below, so that the second form
    // is read as the extent it is rather than taken apart.
    {
        std::string arr_name;
        size_t d = 0;
        size_t idx = 0;
        if (gpu_extent_of_array_dim(v, arr_name, d)) {
            if (scope.is_arg(arr_name, idx)) {
                GpuExtent out;
                out.kind = GpuExtentKind::ArrayDim;
                out.expr = v;
                out.arg_index = idx;
                out.int_value = (int64_t) d;
                out.name = arr_name;
                return out;
            }
            // An array parameter's extents are handed to the kernel as
            // scalar parameters of their own, so an extent written over an
            // array the kernel does not take is still read from one.
            if (resolve_extent_to_dim_arg(v, scope.arg_names, idx)) {
                GpuExtent out;
                out.kind = GpuExtentKind::ArgScalar;
                out.expr = v;
                out.arg_index = idx;
                return out;
            }
            // A local of the kernel, whose own extent is written over the
            // parameters. `size(t) + 1` is resolved by carrying on through
            // `t`'s extent. Fall through when it has none here, so the
            // general `ArraySize` case below can still read it.
            GpuExtent local = gpu_derive_extent(
                gpu_scope_local_array_extent(scope, arr_name, d), scope,
                depth + 1);
            if (local.ok()) return local;
        }
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*v)) {
        ASR::IntegerBinOp_t *op = ASR::down_cast<ASR::IntegerBinOp_t>(v);
        switch (op->m_op) {
            case ASR::binopType::Add:
            case ASR::binopType::Sub:
            case ASR::binopType::Mul:
            case ASR::binopType::Div:
                break;
            default: return none;
        }
        GpuExtent l = gpu_derive_extent(op->m_left, scope, depth);
        GpuExtent r = gpu_derive_extent(op->m_right, scope, depth);
        if (!l.ok() || !r.ok()) return none;
        GpuExtent out;
        out.kind = GpuExtentKind::BinOp;
        out.expr = v;
        out.binop = op->m_op;
        out.children.push_back(std::move(l));
        out.children.push_back(std::move(r));
        return out;
    }
    if (ASR::is_a<ASR::IntegerUnaryMinus_t>(*v)) {
        GpuExtent a = gpu_derive_extent(
            ASR::down_cast<ASR::IntegerUnaryMinus_t>(v)->m_arg, scope,
            depth);
        if (!a.ok()) return none;
        GpuExtent out;
        out.kind = GpuExtentKind::Neg;
        out.expr = v;
        out.children.push_back(std::move(a));
        return out;
    }
    if (ASR::is_a<ASR::IntegerCompare_t>(*v)) {
        ASR::IntegerCompare_t *cmp = ASR::down_cast<ASR::IntegerCompare_t>(v);
        GpuExtent l = gpu_derive_extent(cmp->m_left, scope, depth);
        GpuExtent r = gpu_derive_extent(cmp->m_right, scope, depth);
        if (!l.ok() || !r.ok()) return none;
        GpuExtent out;
        out.kind = GpuExtentKind::Compare;
        out.expr = v;
        out.cmpop = cmp->m_op;
        out.children.push_back(std::move(l));
        out.children.push_back(std::move(r));
        return out;
    }
    if (ASR::is_a<ASR::IfExp_t>(*v)) {
        ASR::IfExp_t *ie = ASR::down_cast<ASR::IfExp_t>(v);
        GpuExtent t = gpu_derive_extent(ie->m_test, scope, depth);
        GpuExtent bdy = gpu_derive_extent(ie->m_body, scope, depth);
        GpuExtent els = gpu_derive_extent(ie->m_orelse, scope, depth);
        if (!t.ok() || !bdy.ok() || !els.ok()) return none;
        GpuExtent out;
        out.kind = GpuExtentKind::Select;
        out.expr = v;
        out.children.push_back(std::move(t));
        out.children.push_back(std::move(bdy));
        out.children.push_back(std::move(els));
        return out;
    }
    {
        size_t idx = 0;
        std::vector<std::string> path;
        if (resolve_extent_to_arg_member(v, scope.arg_names, idx, path)) {
            GpuExtent out;
            out.kind = GpuExtentKind::ArgMember;
            out.expr = v;
            out.arg_index = idx;
            out.member_path = std::move(path);
            return out;
        }
    }
    // `size(<designator>, d)` where the host can read the designator.
    if (ASR::is_a<ASR::ArraySize_t>(*v)) {
        ASR::ArraySize_t *sz = ASR::down_cast<ASR::ArraySize_t>(v);
        ASR::expr_t *array = sz->m_v;
        // The shape asked for may sit one or more elementwise operators
        // below the expression itself, so walk down to the operand that
        // carries it. The rank is the same all the way down, so the
        // dimension asked for stays the same too.
        for (int hop = 0; hop < 8 && array != nullptr; hop++) {
            // `size(slice)` of an ASSOCIATE name is the size of the
            // selector.
            if (ASR::is_a<ASR::Var_t>(*array)) {
                ASR::expr_t *bound = gpu_local_array_binding(
                    ASR::down_cast<ASR::Var_t>(array)->m_v, scope.body,
                    scope.n_body);
                if (bound != nullptr) array = bound;
            }
            if (gpu_designator_is_host_readable(array, scope)) {
                GpuExtent out;
                out.kind = GpuExtentKind::Size;
                out.expr = v;
                out.array = array;
                out.dim = sz->m_dim;
                if (sz->m_dim != nullptr) {
                    GpuExtent d = gpu_derive_extent(sz->m_dim, scope, depth);
                    if (!d.ok()) return none;
                    out.children.push_back(std::move(d));
                }
                return out;
            }
            // A section whose base the host cannot read as it stands -- one
            // subscript is the loop index -- still has extents the host can
            // work out, because they come from the ranges alone.
            std::vector<ASR::array_index_t*> ranges =
                gpu_section_extent_ranges(array, sz->m_dim);
            if (!ranges.empty()) {
                std::vector<GpuExtent> factors;
                for (ASR::array_index_t *range : ranges) {
                    factors.push_back(gpu_derive_range_extent(range, scope,
                        depth + 1));
                }
                GpuExtent out = gpu_extent_product(std::move(factors));
                if (out.ok()) return out;
                return none;
            }
            // Not a designator the host can read -- a function call, say --
            // but its type still records its shape.
            std::vector<ASR::expr_t*> lengths;
            if (gpu_expr_shape_extents(array, sz->m_dim, lengths)) {
                std::vector<GpuExtent> factors;
                for (ASR::expr_t *length : lengths) {
                    factors.push_back(gpu_derive_extent(length, scope,
                        depth + 1));
                }
                GpuExtent out = gpu_extent_product(std::move(factors));
                if (out.ok()) return out;
            }
            // An elementwise array expression records no shape of its own;
            // it has the shape of its array operand.
            array = gpu_elementwise_shape_source(array);
        }
    }
    if (ASR::is_a<ASR::ArrayBound_t>(*v)) {
        ASR::ArrayBound_t *bd = ASR::down_cast<ASR::ArrayBound_t>(v);
        if (gpu_designator_is_host_readable(bd->m_v, scope)) {
            GpuExtent d = gpu_derive_extent(bd->m_dim, scope, depth);
            if (d.ok()) {
                GpuExtent out;
                out.kind = GpuExtentKind::Bound;
                out.expr = v;
                out.array = bd->m_v;
                out.dim = bd->m_dim;
                out.bound = bd->m_bound;
                out.children.push_back(std::move(d));
                return out;
            }
        }
    }
    // An element of an array parameter, at a subscript the host can work
    // out too. The loop index is not a parameter, so an element the
    // iteration picks is correctly not derivable here.
    if (ASR::is_a<ASR::ArrayItem_t>(*v)) {
        ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(v);
        ASR::expr_t *base = ASRUtils::get_past_array_physical_cast(item->m_v);
        if (!ASR::is_a<ASR::Var_t>(*base)) return none;
        std::string name = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(base)->m_v);
        size_t idx = 0;
        if (!scope.is_arg(name, idx)) return none;
        GpuExtent out;
        out.kind = GpuExtentKind::ArgElement;
        out.expr = v;
        out.arg_index = idx;
        for (size_t i = 0; i < item->n_args; i++) {
            if (item->m_args[i].m_left || item->m_args[i].m_step) return none;
            GpuExtent sub = gpu_derive_extent(item->m_args[i].m_right, scope,
                depth);
            if (!sub.ok()) return none;
            out.children.push_back(std::move(sub));
        }
        return out;
    }
    if (ASR::is_a<ASR::Var_t>(*v)) {
        ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(v)->m_v;
        std::string name = ASRUtils::symbol_name(sym);
        size_t idx = 0;
        if (scope.is_arg(name, idx)) {
            GpuExtent out;
            out.kind = GpuExtentKind::ArgScalar;
            out.expr = v;
            out.arg_index = idx;
            return out;
        }
        // A name of the kernel's own that stands for one value: what an
        // ASSOCIATE selector becomes once the construct is spliced in.
        // The value it is bound to is what the host evaluates.
        ASR::expr_t *bound = gpu_local_scalar_binding(sym, scope.body,
            scope.n_body);
        if (bound != nullptr) {
            return gpu_derive_extent(bound, scope, depth + 1);
        }
    }
    return none;
}

inline bool gpu_designator_is_host_readable(ASR::expr_t *e,
        const GpuExtentScope &scope) {
    if (e == nullptr) return false;
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
    if (ASR::is_a<ASR::Var_t>(*v)) {
        ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(v)->m_v;
        size_t idx = 0;
        if (scope.is_arg(ASRUtils::symbol_name(sym), idx)) return true;
        ASR::expr_t *bound = gpu_local_array_binding(sym, scope.body,
            scope.n_body);
        if (bound != nullptr) {
            return gpu_designator_is_host_readable(bound, scope);
        }
        return false;
    }
    if (ASR::is_a<ASR::StructInstanceMember_t>(*v)) {
        return gpu_designator_is_host_readable(
            ASR::down_cast<ASR::StructInstanceMember_t>(v)->m_v, scope);
    }
    if (ASR::is_a<ASR::ArrayItem_t>(*v)) {
        ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(v);
        for (size_t i = 0; i < item->n_args; i++) {
            if (item->m_args[i].m_left || item->m_args[i].m_step) return false;
            if (!gpu_derive_extent(item->m_args[i].m_right, scope).ok()) {
                return false;
            }
        }
        return gpu_designator_is_host_readable(item->m_v, scope);
    }
    return false;
}

// The struct component an array expression names, when it is a
// deferred-shape component such as `a(i)%v` or a section of one.
inline bool expr_struct_member_key(ASR::expr_t *e, GpuStructMemberKey &key,
        size_t *rank = nullptr, int64_t *elem_index = nullptr) {
    if (e == nullptr) return false;
    e = ASRUtils::get_past_array_physical_cast(e);
    while (ASR::is_a<ASR::Cast_t>(*e)) {
        e = ASR::down_cast<ASR::Cast_t>(e)->m_arg;
    }
    if (ASR::is_a<ASR::ArraySection_t>(*e)) {
        e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
        e = ASRUtils::get_past_array_physical_cast(e);
    }
    if (!ASR::is_a<ASR::StructInstanceMember_t>(*e)) return false;
    ASR::StructInstanceMember_t *sm =
        ASR::down_cast<ASR::StructInstanceMember_t>(e);
    ASR::symbol_t *ms = ASRUtils::symbol_get_past_external(sm->m_m);
    std::string member = ASRUtils::symbol_name(ms);
    ASR::expr_t *base = sm->m_v;
    int64_t index = 0;
    if (ASR::is_a<ASR::ArrayItem_t>(*base)) {
        // The column-major position of the element, which is the position
        // the flattened component buffers are laid out and read by. An
        // array of any rank has one as long as every subscript, lower
        // bound and extent it takes is known here.
        ASR::ArrayItem_t *item = ASR::down_cast<ASR::ArrayItem_t>(base);
        index = -1;
        ASR::Array_t *arr = nullptr;
        ASR::ttype_t *at = ASRUtils::type_get_past_allocatable(
            ASRUtils::type_get_past_pointer(
                ASRUtils::expr_type(item->m_v)));
        if (ASR::is_a<ASR::Array_t>(*at)) {
            arr = ASR::down_cast<ASR::Array_t>(at);
        }
        size_t known_dims = arr != nullptr ? arr->n_dims : 1;
        if (known_dims == item->n_args) {
            int64_t position = 0, stride = 1;
            bool known = true;
            for (size_t d = 0; d < item->n_args && known; d++) {
                ASR::expr_t *ie = item->m_args[d].m_right
                    ? item->m_args[d].m_right : item->m_args[d].m_left;
                int64_t v = 0;
                if (ie == nullptr || ASRUtils::expr_value(ie) == nullptr
                        || !ASRUtils::extract_value(
                            ASRUtils::expr_value(ie), v)) {
                    known = false;
                    break;
                }
                int64_t lb = 1;
                if (arr != nullptr && arr->m_dims[d].m_start != nullptr
                        && ASRUtils::expr_value(arr->m_dims[d].m_start)
                            != nullptr) {
                    ASRUtils::extract_value(ASRUtils::expr_value(
                        arr->m_dims[d].m_start), lb);
                }
                position += stride * (v - lb);
                if (d + 1 < item->n_args) {
                    int64_t extent = 0;
                    if (arr == nullptr || arr->m_dims[d].m_length == nullptr
                            || ASRUtils::expr_value(arr->m_dims[d].m_length)
                                == nullptr
                            || !ASRUtils::extract_value(ASRUtils::expr_value(
                                arr->m_dims[d].m_length), extent)) {
                        known = false;
                        break;
                    }
                    stride *= extent;
                }
            }
            if (known) index = position;
        }
        base = item->m_v;
    } else if (ASR::is_a<ASR::ArraySection_t>(*base)) {
        index = -1;
        base = ASR::down_cast<ASR::ArraySection_t>(base)->m_v;
    } else {
        // size(x%v) on a scalar struct is not an element of a struct
        // array: the host reads that extent from the argument itself.
        return false;
    }
    if (!ASR::is_a<ASR::Var_t>(*base)) return false;
    key.base = ASRUtils::symbol_name(
        ASR::down_cast<ASR::Var_t>(base)->m_v);
    key.member = member;
    if (rank != nullptr) {
        *rank = 1;
        if (ASR::is_a<ASR::Variable_t>(*ms)) {
            size_t r = gpu_struct_member_rank(
                ASR::down_cast<ASR::Variable_t>(ms));
            if (r > 0) *rank = r;
        }
    }
    if (elem_index != nullptr) *elem_index = index;
    return true;
}

// The member a workspace was copied from, taken from the assignment that
// writes `var_name`. Two allocatable members of the same type must not
// share a guessed size.
inline bool find_struct_member_key_from_assignments(
        ASR::stmt_t **body, size_t n_body, const std::string &var_name,
        GpuStructMemberKey &key) {
    if (body == nullptr) return false;
    for (size_t i = 0; i < n_body; i++) {
        ASR::stmt_t *stmt = body[i];
        if (ASR::is_a<ASR::Assignment_t>(*stmt)) {
            ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(stmt);
            if (!ASR::is_a<ASR::Var_t>(*asgn->m_target)) continue;
            std::string tname = ASRUtils::symbol_name(
                ASR::down_cast<ASR::Var_t>(asgn->m_target)->m_v);
            if (tname != var_name) continue;
            if (expr_struct_member_key(asgn->m_value, key)) return true;
        } else if (ASR::is_a<ASR::BlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmt)->m_m);
            if (b == nullptr || !ASR::is_a<ASR::Block_t>(*b)) continue;
            ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
            if (find_struct_member_key_from_assignments(
                    blk->m_body, blk->n_body, var_name, key)) {
                return true;
            }
        } else if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmt)) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmt)->m_m);
            if (b == nullptr || !ASR::is_a<ASR::AssociateBlock_t>(*b)) {
                continue;
            }
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(b);
            if (find_struct_member_key_from_assignments(
                    ab->m_body, ab->n_body, var_name, key)) {
                return true;
            }
        } else if (ASR::is_a<ASR::DoLoop_t>(*stmt)) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmt);
            if (find_struct_member_key_from_assignments(
                    dl->m_body, dl->n_body, var_name, key)) {
                return true;
            }
        } else if (ASR::is_a<ASR::WhileLoop_t>(*stmt)) {
            ASR::WhileLoop_t *wl = ASR::down_cast<ASR::WhileLoop_t>(stmt);
            if (find_struct_member_key_from_assignments(
                    wl->m_body, wl->n_body, var_name, key)) {
                return true;
            }
        } else if (ASR::is_a<ASR::If_t>(*stmt)) {
            ASR::If_t *ifs = ASR::down_cast<ASR::If_t>(stmt);
            if (find_struct_member_key_from_assignments(
                    ifs->m_body, ifs->n_body, var_name, key)
                    || find_struct_member_key_from_assignments(
                        ifs->m_orelse, ifs->n_orelse, var_name, key)) {
                return true;
            }
        }
    }
    return false;
}

// The struct component an extent reads the size of, such as
// `size(a(i)%v)`. The host cannot evaluate such an extent itself, because
// the element index only exists on the device; it sizes the workspace from
// the component's own sizes instead.
inline bool dim_expr_struct_member_key(ASR::expr_t *dim,
        GpuStructMemberKey &key,
        size_t *rank = nullptr, int64_t *elem_index = nullptr) {
    if (dim == nullptr) return false;
    ASR::expr_t *e = ASRUtils::get_past_array_physical_cast(dim);
    while (ASR::is_a<ASR::Cast_t>(*e)) {
        e = ASR::down_cast<ASR::Cast_t>(e)->m_arg;
    }
    if (!ASR::is_a<ASR::ArraySize_t>(*e)) return false;
    return expr_struct_member_key(
        ASR::down_cast<ASR::ArraySize_t>(e)->m_v, key, rank, elem_index);
}

// The size in bytes of an array's element, or 0 when the element type is
// not in the shared host/device width table. A default of 4 used to
// disagree with logical(1)/logical(2), which the table allows and the
// emitter maps to 1- and 2-byte types.
inline int gpu_vla_elem_size(ASR::Array_t *arr) {
    ASR::ttype_t *t = ASRUtils::extract_type(arr->m_type);
    if (!gpu_scalar_width_supported(t)) return 0;
    return ASRUtils::extract_kind_from_ttype_t(t);
}

// The argument of an Allocate statement that gives `var_name` its shape.
inline ASR::alloc_arg_t* find_alloc_arg_for_var(ASR::Allocate_t *alloc,
        const std::string &var_name) {
    for (size_t ai = 0; ai < alloc->n_args; ai++) {
        if (!alloc->m_args[ai].m_a) continue;
        if (!ASR::is_a<ASR::Var_t>(*alloc->m_args[ai].m_a)) continue;
        std::string aname = ASRUtils::symbol_name(
            ASR::down_cast<ASR::Var_t>(alloc->m_args[ai].m_a)->m_v);
        if (aname == var_name) return &alloc->m_args[ai];
    }
    return nullptr;
}

// How a workspace dimension that is not a compile-time constant is sized.
// An extent that reads the extents of an allocatable struct component names
// the loop index, so only the device can evaluate it and it is read from
// the per-element sizes buffer; anything else is the one derivation, which
// the host and the device then render each in its own names. False when the
// extent is neither.
inline bool classify_vla_dim(ASR::expr_t *dim, const GpuExtentScope &scope,
        GpuVlaDim &vd) {
    vd.source_extent = dim;
    vd.is_constant = false;
    vd.constant_value = 0;
    GpuStructMemberKey member_key;
    if (dim_expr_struct_member_key(dim, member_key, &vd.struct_member_rank,
            &vd.struct_member_elem_index)) {
        vd.is_struct_member_size = true;
        vd.struct_member_key = member_key;
        return true;
    }
    vd.derived = gpu_derive_extent(dim, scope);
    return vd.derived.ok();
}

// Describes, as a per-thread workspace, the shape an `allocate` gives an
// array. Returns false when every extent is known at compile time, in which
// case the array needs no workspace: the device code declares it in thread
// memory instead.
inline bool alloc_shape_to_vla_workspace(ASR::alloc_arg_t &alloc_arg,
        ASR::Array_t *arr, const std::string &var_name,
        const GpuExtentScope &scope, GpuVlaWorkspace &ws) {
    bool has_runtime_dim = false;
    for (size_t d = 0; d < alloc_arg.n_dims; d++) {
        if (alloc_arg.m_dims[d].m_length &&
                !ASR::is_a<ASR::IntegerConstant_t>(
                    *alloc_arg.m_dims[d].m_length)) {
            has_runtime_dim = true;
            break;
        }
    }
    if (!has_runtime_dim) return false;
    ws.var_name = var_name;
    ws.elem_size = gpu_vla_elem_size(arr);
    if (ws.elem_size <= 0) return false;
    for (size_t d = 0; d < alloc_arg.n_dims; d++) {
        ASR::expr_t *dim = alloc_arg.m_dims[d].m_length;
        GpuVlaDim vd;
        if (dim && ASR::is_a<ASR::IntegerConstant_t>(*dim)) {
            vd.constant_value =
                ASR::down_cast<ASR::IntegerConstant_t>(dim)->m_n;
        } else if (dim) {
            int64_t const_val;
            if (try_resolve_alloc_dim_constant(dim, scope.body,
                    scope.n_body, const_val)) {
                vd.constant_value = const_val;
            } else if (!classify_vla_dim(dim, scope, vd)) {
                // The host cannot size a workspace it cannot measure.
                // Leave the array to the device language, which either
                // declares it or reports that it cannot.
                return false;
            }
        }
        ws.dims.push_back(vd);
    }
    return true;
}

// Describes, as a per-thread workspace, the shape an array is declared with.
// Returns false when every extent is known at compile time, in which case the
// array needs no workspace: the device code declares it in thread memory
// instead.
inline bool declared_shape_to_vla_workspace(ASR::Array_t *arr,
        const std::string &var_name, const GpuExtentScope &scope,
        GpuVlaWorkspace &ws) {
    bool has_runtime_dim = false;
    for (size_t d = 0; d < arr->n_dims; d++) {
        if (arr->m_dims[d].m_length &&
                !ASR::is_a<ASR::IntegerConstant_t>(
                    *arr->m_dims[d].m_length)) {
            has_runtime_dim = true;
            break;
        }
    }
    if (!has_runtime_dim) return false;
    ws.var_name = var_name;
    ws.elem_size = gpu_vla_elem_size(arr);
    if (ws.elem_size <= 0) return false;
    for (size_t d = 0; d < arr->n_dims; d++) {
        ASR::expr_t *dim = arr->m_dims[d].m_length;
        GpuVlaDim vd;
        if (dim && ASR::is_a<ASR::IntegerConstant_t>(*dim)) {
            vd.constant_value =
                ASR::down_cast<ASR::IntegerConstant_t>(dim)->m_n;
        } else if (dim && !classify_vla_dim(dim, scope, vd)) {
            // The host cannot size a workspace it cannot measure. Leave
            // the array to the device language, which either declares it
            // or reports that it cannot.
            return false;
        }
        ws.dims.push_back(vd);
    }
    return true;
}

// A per-thread workspace for an allocatable array whose size is only known
// on the device, sized from the struct member the array is copied from.
// The one allocatable array component of the kernel's arguments, when the
// kernel has exactly one. Two members cannot share a size; those must be
// named by an assignment.
inline bool unique_struct_alloc_member_key(const ASR::Function_t &kernel,
        GpuStructMemberKey &key) {
    int n = 0;
    GpuStructMemberKey found;
    for (size_t ai = 0; ai < kernel.n_args; ai++) {
        ASR::Var_t *av = ASR::down_cast<ASR::Var_t>(kernel.m_args[ai]);
        ASR::Variable_t *avar = ASR::down_cast<ASR::Variable_t>(
            ASRUtils::symbol_get_past_external(av->m_v));
        ASR::ttype_t *atype =
            ASRUtils::type_get_past_allocatable(avar->m_type);
        if (!ASR::is_a<ASR::Array_t>(*atype)) continue;
        ASR::Array_t *arr_t = ASR::down_cast<ASR::Array_t>(atype);
        if (!ASR::is_a<ASR::StructType_t>(*arr_t->m_type)) continue;
        if (!avar->m_type_declaration) continue;
        ASR::symbol_t *decl_sym = ASRUtils::symbol_get_past_external(
            avar->m_type_declaration);
        if (!ASR::is_a<ASR::Struct_t>(*decl_sym)) continue;
        ASR::Struct_t *stype = ASR::down_cast<ASR::Struct_t>(decl_sym);
        for (auto &mem :
                ASRUtils::collect_allocatable_array_members(stype)) {
            n++;
            found = GpuStructMemberKey{avar->m_name, mem.first};
            if (n > 1) return false;
        }
    }
    if (n != 1) return false;
    key = found;
    return true;
}

inline size_t gpu_struct_member_rank_from_key(const ASR::Function_t &kernel,
        const GpuStructMemberKey &key) {
    if (key.empty()) return 1;
    const std::string &arr = key.base;
    const std::string &mem = key.member;
    for (size_t ai = 0; ai < kernel.n_args; ai++) {
        ASR::Var_t *av = ASR::down_cast<ASR::Var_t>(kernel.m_args[ai]);
        ASR::Variable_t *avar = ASR::down_cast<ASR::Variable_t>(
            ASRUtils::symbol_get_past_external(av->m_v));
        if (std::string(avar->m_name) != arr) continue;
        if (!avar->m_type_declaration) continue;
        ASR::symbol_t *decl = ASRUtils::symbol_get_past_external(
            avar->m_type_declaration);
        if (!ASR::is_a<ASR::Struct_t>(*decl)) continue;
        ASR::Struct_t *st = ASR::down_cast<ASR::Struct_t>(decl);
        for (auto &m : ASRUtils::collect_allocatable_array_members(st)) {
            if (m.first != mem) continue;
            size_t r = gpu_struct_member_rank(m.second);
            return r > 0 ? r : 1;
        }
    }
    return 1;
}

inline bool struct_member_vla_workspace(const ASR::Function_t &kernel,
        ASR::Array_t *arr, const std::string &var_name,
        GpuVlaWorkspace &ws, ASR::stmt_t **scope_body = nullptr,
        size_t scope_n_body = 0) {
    GpuStructMemberKey struct_key;
    if (scope_body != nullptr
            && find_struct_member_key_from_assignments(scope_body,
                scope_n_body, var_name, struct_key)) {
        // The assignment in this scope names the member.
    } else if (find_struct_member_key_from_assignments(kernel.m_body,
            kernel.n_body, var_name, struct_key)) {
        // The assignment in the kernel body names the member.
    } else if (!unique_struct_alloc_member_key(kernel, struct_key)) {
        return false;
    }
    ws.var_name = var_name;
    ws.elem_size = gpu_vla_elem_size(arr);
    if (ws.elem_size <= 0) return false;
    GpuVlaDim vd;
    vd.is_constant = false;
    vd.constant_value = 0;
    vd.is_struct_member_size = true;
    vd.struct_member_key = struct_key;
    vd.struct_member_rank = gpu_struct_member_rank_from_key(kernel,
        struct_key);
    vd.struct_member_elem_index = -1;
    ws.dims.push_back(vd);
    return true;
}

// Scan kernel-scope Allocatable(Array) variables for VLA workspaces.
inline void scan_kernel_scope_alloc_vlas(
        const ASR::Function_t &kernel,
        const std::vector<std::string> &arg_names,
        int &buffer_idx,
        std::vector<GpuVlaWorkspace> &result) {
    std::set<std::string> arg_set(arg_names.begin(), arg_names.end());
    std::set<ASR::symbol_t*> handled;
    for (auto &ws : result) handled.insert(ws.var);

    for (auto &item : kernel.m_symtab->get_scope()) {
        if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
        ASR::Variable_t *var =
            ASR::down_cast<ASR::Variable_t>(item.second);
        ASR::ttype_t *inner =
            ASRUtils::type_get_past_allocatable(var->m_type);
        if (!ASR::is_a<ASR::Array_t>(*inner)) continue;
        ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(inner);
        std::string vname(var->m_name);
        if (arg_set.count(vname)) continue;
        if (handled.count(item.second)) continue;
        GpuVlaWorkspace ws;
        ws.var = item.second;
        bool have = false;
        if (!ASRUtils::is_allocatable(var->m_type)) {
            // An array declared with extents the device cannot evaluate
            // when it enters the kernel.
            have = declared_shape_to_vla_workspace(arr, vname,
                GpuExtentScope{&kernel, arg_names, kernel.m_symtab,
                    kernel.m_body, kernel.n_body}, ws);
            if (!have) continue;
            ws.buffer_index = buffer_idx++;
            result.push_back(std::move(ws));
            continue;
        }
        ASR::Allocate_t *alloc = find_allocate_for_var(
            kernel.m_body, kernel.n_body, vname);
        if (alloc) {
            ASR::alloc_arg_t *target_arg = find_alloc_arg_for_var(
                alloc, vname);
            if (!target_arg) continue;
            have = alloc_shape_to_vla_workspace(*target_arg, arr, vname,
                GpuExtentScope{&kernel, arg_names, kernel.m_symtab,
                    kernel.m_body, kernel.n_body}, ws);
        } else {
            // No Allocate: this is a function-call result temporary whose
            // size depends on a struct member's allocatable array.
            have = struct_member_vla_workspace(kernel, arr, vname, ws,
                kernel.m_body, kernel.n_body);
        }
        if (!have) continue;
        ws.buffer_index = buffer_idx++;
        result.push_back(std::move(ws));
    }
}

// Every BLOCK and ASSOCIATE the statement list opens, at whatever depth,
// including those nested in `if` / serial `do` / `while`. The pre-flight
// and the workspace collector ask the same question of the same scopes.
template <typename F>
inline void gpu_walk_scopes(ASR::stmt_t **stmts, size_t n, F &&fn) {
    if (stmts == nullptr) return;
    for (size_t i = 0; i < n; i++) {
        if (ASR::is_a<ASR::BlockCall_t>(*stmts[i])) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(stmts[i])->m_m);
            if (b == nullptr || !ASR::is_a<ASR::Block_t>(*b)) continue;
            ASR::Block_t *blk = ASR::down_cast<ASR::Block_t>(b);
            fn(blk->m_symtab, blk->m_body, blk->n_body);
            gpu_walk_scopes(blk->m_body, blk->n_body, fn);
        } else if (ASR::is_a<ASR::AssociateBlockCall_t>(*stmts[i])) {
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::AssociateBlockCall_t>(stmts[i])->m_m);
            if (b == nullptr || !ASR::is_a<ASR::AssociateBlock_t>(*b)) {
                continue;
            }
            ASR::AssociateBlock_t *ab =
                ASR::down_cast<ASR::AssociateBlock_t>(b);
            fn(ab->m_symtab, ab->m_body, ab->n_body);
            gpu_walk_scopes(ab->m_body, ab->n_body, fn);
        } else if (ASR::is_a<ASR::DoLoop_t>(*stmts[i])) {
            ASR::DoLoop_t *dl = ASR::down_cast<ASR::DoLoop_t>(stmts[i]);
            gpu_walk_scopes(dl->m_body, dl->n_body, fn);
        } else if (ASR::is_a<ASR::WhileLoop_t>(*stmts[i])) {
            ASR::WhileLoop_t *wl = ASR::down_cast<ASR::WhileLoop_t>(stmts[i]);
            gpu_walk_scopes(wl->m_body, wl->n_body, fn);
        } else if (ASR::is_a<ASR::If_t>(*stmts[i])) {
            ASR::If_t *ifs = ASR::down_cast<ASR::If_t>(stmts[i]);
            gpu_walk_scopes(ifs->m_body, ifs->n_body, fn);
            gpu_walk_scopes(ifs->m_orelse, ifs->n_orelse, fn);
        }
    }
}

inline ASR::Variable_t* find_gpu_vla_variable(const ASR::Function_t &kernel,
        const std::string &name) {
    ASR::Variable_t *found = nullptr;
    gpu_walk_scopes(kernel.m_body, kernel.n_body,
        [&](SymbolTable *symtab, ASR::stmt_t **, size_t) {
            if (found != nullptr || symtab == nullptr) return;
            ASR::symbol_t *sym = symtab->get_symbol(name);
            if (sym != nullptr && ASR::is_a<ASR::Variable_t>(*sym)) {
                found = ASR::down_cast<ASR::Variable_t>(sym);
            }
        });
    if (found == nullptr) {
        ASR::symbol_t *sym = kernel.m_symtab->resolve_symbol(name);
        if (sym != nullptr && ASR::is_a<ASR::Variable_t>(*sym)) {
            found = ASR::down_cast<ASR::Variable_t>(sym);
        }
    }
    return found;
}

// Every per-thread workspace a GPU kernel needs, with buffer indices assigned
// sequentially from `buffer_idx`.
inline std::vector<GpuVlaWorkspace> collect_gpu_vla_workspaces(
        const ASR::Function_t &kernel, int buffer_idx) {
    // The kernel argument names, so that a workspace extent that is one of
    // them can be read on the host at dispatch time.
    std::vector<std::string> arg_names;
    for (size_t i = 0; i < kernel.n_args; i++) {
        ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(kernel.m_args[i]);
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
            ASRUtils::symbol_get_past_external(v->m_v));
        arg_names.push_back(std::string(var->m_name));
    }

    std::vector<GpuVlaWorkspace> result;

    gpu_walk_scopes(kernel.m_body, kernel.n_body,
        [&](SymbolTable *symtab, ASR::stmt_t **body, size_t n_body) {
            if (symtab == nullptr) return;

            // An automatic array of the scope, whose extents the device
            // cannot declare because they are not compile-time constants.
            for (auto &item : symtab->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                    item.second);
                if (!ASR::is_a<ASR::Array_t>(*var->m_type)) continue;
                ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(var->m_type);

                GpuVlaWorkspace ws;
                ws.var = item.second;
                if (!declared_shape_to_vla_workspace(arr, var->m_name,
                        GpuExtentScope{&kernel, arg_names, symtab,
                            kernel.m_body, kernel.n_body}, ws)) {
                    continue;
                }
                ws.buffer_index = buffer_idx++;
                result.push_back(std::move(ws));
            }

            // An allocatable array of the scope: its shape comes from the
            // `allocate` that gives it one, or, for a temporary the
            // subroutine_from_function pass created, from the struct member
            // it is copied from.
            for (auto &item2 : symtab->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item2.second)) continue;
                ASR::Variable_t *var2 = ASR::down_cast<ASR::Variable_t>(
                    item2.second);
                if (!ASRUtils::is_allocatable(var2->m_type)) continue;
                ASR::ttype_t *inner =
                    ASRUtils::type_get_past_allocatable(var2->m_type);
                if (!ASR::is_a<ASR::Array_t>(*inner)) continue;
                ASR::Array_t *arr2 = ASR::down_cast<ASR::Array_t>(inner);
                std::string vname(var2->m_name);
                bool already = false;
                for (auto &r : result) {
                    if (r.var == item2.second) { already = true; break; }
                }
                if (already) continue;
                GpuVlaWorkspace ws;
                ws.var = item2.second;
                bool have = false;
                ASR::Allocate_t *alloc = find_allocate_for_var(
                    body, n_body, vname);
                if (alloc) {
                    ASR::alloc_arg_t *target_arg = find_alloc_arg_for_var(
                        alloc, vname);
                    if (target_arg) {
                        have = alloc_shape_to_vla_workspace(*target_arg, arr2,
                            vname, GpuExtentScope{&kernel, arg_names, symtab,
                                kernel.m_body, kernel.n_body}, ws);
                    }
                }
                if (!have) {
                    have = struct_member_vla_workspace(kernel, arr2, vname, ws,
                        body, n_body);
                }
                if (!have) continue;
                ws.buffer_index = buffer_idx++;
                result.push_back(std::move(ws));
            }
        });

    scan_kernel_scope_alloc_vlas(kernel, arg_names, buffer_idx, result);

    return result;
}

// Count VLA workspaces in a kernel without assigning buffer indices.
inline int count_gpu_vla_workspaces(const ASR::Function_t &kernel) {
    return static_cast<int>(collect_gpu_vla_workspaces(kernel, 0).size());
}

static const int MAX_METAL_BUFFERS = 31;
static const int PACKED_BUFFER_ALIGN = 16;

// Determine whether a kernel needs buffer packing because its total
// buffer count exceeds Metal's 31-slot limit.
inline bool gpu_kernel_needs_buffer_packing(
        const ASR::Function_t &kernel) {
    auto [n_buffer, n_scalar] = classify_gpu_kernel_args(kernel);
    int n_vla = count_gpu_vla_workspaces(kernel);
    int total = n_buffer + (n_scalar > 0 ? 1 : 0) + n_vla;
    return total > MAX_METAL_BUFFERS;
}

// Compute the Metal buffer index where VLA workspace buffers start.
// Normal layout:  [buffer_args...] [scalar_struct?] [vla_workspaces...]
// Packed layout:  [packed_arrays(0)] [scalar_struct(1)] [vla_workspaces...]
inline int gpu_vla_buffer_start(const ASR::Function_t &kernel) {
    if (gpu_kernel_needs_buffer_packing(kernel)) {
        return 2;
    }
    auto [n_buffer, n_scalar] = classify_gpu_kernel_args(kernel);
    return n_buffer + (n_scalar > 0 ? 1 : 0);
}

// Analyze a GPU kernel function for the per-thread workspaces it needs, with
// buffer indices assigned sequentially after the kernel's packed arguments.
inline std::vector<GpuVlaWorkspace> analyze_gpu_vla_workspaces(
        const ASR::Function_t &kernel) {
    return collect_gpu_vla_workspaces(kernel, gpu_vla_buffer_start(kernel));
}

// Scan a kernel body for alloc-assign statements that write a VLA workspace
// array to a struct array member.  Returns a map from
// "struct_name.member_name" to the per-element size (number of elements)
// determined by the VLA workspace dimensions.
// The Allocate and ReAlloc statements a routine applies to a component of one
// of its variables, keyed by "variable.component".
class StructMemberShapeCollector:
    public ASR::BaseWalkVisitor<StructMemberShapeCollector> {
    public:

        std::map<GpuStructMemberKey, ASR::alloc_arg_t*> shapes;

        void collect(ASR::alloc_arg_t *args, size_t n_args) {
            for (size_t i = 0; i < n_args; i++) {
                if (!args[i].m_a || args[i].n_dims == 0) continue;
                if (!ASR::is_a<ASR::StructInstanceMember_t>(*args[i].m_a)) {
                    continue;
                }
                ASR::StructInstanceMember_t *sm =
                    ASR::down_cast<ASR::StructInstanceMember_t>(args[i].m_a);
                ASR::expr_t *base = sm->m_v;
                if (ASR::is_a<ASR::ArrayItem_t>(*base)) {
                    base = ASR::down_cast<ASR::ArrayItem_t>(base)->m_v;
                } else if (ASR::is_a<ASR::ArraySection_t>(*base)) {
                    base = ASR::down_cast<ASR::ArraySection_t>(base)->m_v;
                }
                if (!ASR::is_a<ASR::Var_t>(*base)) continue;
                GpuStructMemberKey key{
                    ASRUtils::symbol_name(
                        ASR::down_cast<ASR::Var_t>(base)->m_v),
                    ASRUtils::symbol_name(
                        ASRUtils::symbol_get_past_external(sm->m_m))};
                shapes.emplace(key, &args[i]);
            }
        }

        void visit_Allocate(const ASR::Allocate_t &x) {
            collect(x.m_args, x.n_args);
        }

        void visit_ReAlloc(const ASR::ReAlloc_t &x) {
            collect(x.m_args, x.n_args);
        }

        void visit_Function(const ASR::Function_t &/*x*/) {
            // A nested routine shapes its own variables.
        }
};

inline std::map<GpuStructMemberKey, ASR::alloc_arg_t*> struct_member_shapes(
        ASR::stmt_t **body, size_t n_body) {
    StructMemberShapeCollector collector;
    for (size_t i = 0; i < n_body; i++) {
        collector.visit_stmt(*body[i]);
    }
    return collector.shapes;
}

// Binds the variables of a routine to the arguments a call passes it, so that
// an extent the routine writes can be read at the call site.
struct GpuExtentContext {
    ASR::Function_t *callee = nullptr;
    ASR::call_arg_t *args = nullptr;
    size_t n_args = 0;
    const std::map<std::string, const GpuVlaWorkspace*> *workspaces = nullptr;

    // The argument a variable of the callee is bound to, if any.
    ASR::expr_t* bound_arg(ASR::symbol_t *sym) const {
        if (callee == nullptr) return nullptr;
        std::string name = ASRUtils::symbol_name(sym);
        for (size_t i = 0; i < callee->n_args && i < n_args; i++) {
            if (!ASR::is_a<ASR::Var_t>(*callee->m_args[i])) continue;
            if (std::string(ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(callee->m_args[i])->m_v))
                        != name) {
                continue;
            }
            return args[i].m_value;
        }
        return nullptr;
    }
};

inline bool gpu_extent_value(ASR::expr_t *e, const GpuExtentContext &ctx,
    int64_t &out);

// The number of elements an array expression has along `dim` (all dimensions
// when `dim` is zero), read from the type it is declared with or from the
// workspace that backs it.
inline bool gpu_array_extent(ASR::expr_t *array, int64_t dim,
        const GpuExtentContext &ctx, int64_t &out) {
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(array);
    if (ASR::is_a<ASR::Var_t>(*v)) {
        ASR::symbol_t *sym = ASR::down_cast<ASR::Var_t>(v)->m_v;
        ASR::expr_t *bound = ctx.bound_arg(sym);
        if (bound != nullptr) {
            GpuExtentContext caller_ctx;
            caller_ctx.workspaces = ctx.workspaces;
            return gpu_array_extent(bound, dim, caller_ctx, out);
        }
        if (ctx.workspaces != nullptr) {
            auto ws = ctx.workspaces->find(
                std::string(ASRUtils::symbol_name(sym)));
            if (ws != ctx.workspaces->end()) {
                int64_t total = 1;
                for (size_t d = 0; d < ws->second->dims.size(); d++) {
                    if (!ws->second->dims[d].is_constant) return false;
                    if (dim == 0 || dim == (int64_t) d + 1) {
                        total *= ws->second->dims[d].constant_value;
                    }
                }
                out = total;
                return true;
            }
        }
    }
    ASR::dimension_t *dims = nullptr;
    size_t n_dims = ASRUtils::extract_dimensions_from_ttype(
        ASRUtils::expr_type(v), dims);
    if (n_dims == 0) return false;
    int64_t total = 1;
    for (size_t d = 0; d < n_dims; d++) {
        if (dim != 0 && dim != (int64_t) d + 1) continue;
        if (dims[d].m_length == nullptr) return false;
        int64_t length;
        if (!gpu_extent_value(dims[d].m_length, ctx, length)) return false;
        total *= length;
    }
    out = total;
    return true;
}

// The value of an extent expression, with the variables of a routine bound to
// the arguments a call passes it.
inline bool gpu_extent_value(ASR::expr_t *e, const GpuExtentContext &ctx,
        int64_t &out) {
    if (e == nullptr) return false;
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
    while (ASR::is_a<ASR::Cast_t>(*v)) {
        v = ASR::down_cast<ASR::Cast_t>(v)->m_arg;
    }
    if (ASR::is_a<ASR::IntegerConstant_t>(*v)) {
        out = ASR::down_cast<ASR::IntegerConstant_t>(v)->m_n;
        return true;
    }
    if (ASR::is_a<ASR::ArraySize_t>(*v)) {
        ASR::ArraySize_t *size = ASR::down_cast<ASR::ArraySize_t>(v);
        int64_t dim = 0;
        if (size->m_dim && !gpu_extent_value(size->m_dim, ctx, dim)) {
            return false;
        }
        return gpu_array_extent(size->m_v, dim, ctx, out);
    }
    if (ASR::is_a<ASR::IntegerBinOp_t>(*v)) {
        ASR::IntegerBinOp_t *op = ASR::down_cast<ASR::IntegerBinOp_t>(v);
        int64_t left, right;
        if (!gpu_extent_value(op->m_left, ctx, left) ||
            !gpu_extent_value(op->m_right, ctx, right)) {
            return false;
        }
        switch (op->m_op) {
            case ASR::binopType::Add: out = left + right; return true;
            case ASR::binopType::Sub: out = left - right; return true;
            case ASR::binopType::Mul: out = left * right; return true;
            default: return false;
        }
    }
    if (ASR::is_a<ASR::Var_t>(*v)) {
        ASR::expr_t *bound = ctx.bound_arg(
            ASR::down_cast<ASR::Var_t>(v)->m_v);
        if (bound != nullptr) {
            GpuExtentContext caller_ctx;
            caller_ctx.workspaces = ctx.workspaces;
            return gpu_extent_value(bound, caller_ctx, out);
        }
    }
    int64_t value;
    if (try_eval_int_constant(v, value)) {
        out = value;
        return true;
    }
    return false;
}

// The struct component an extent reads its size from, when the size is only
// known once the host has measured that component.
inline bool gpu_extent_member_key(ASR::expr_t *e, const GpuExtentContext &ctx,
        GpuStructMemberKey &key) {
    if (e == nullptr) return false;
    ASR::expr_t *v = ASRUtils::get_past_array_physical_cast(e);
    while (ASR::is_a<ASR::Cast_t>(*v)) {
        v = ASR::down_cast<ASR::Cast_t>(v)->m_arg;
    }
    if (ASR::is_a<ASR::Var_t>(*v)) {
        // An extent the caller passes in as an argument of its own.
        ASR::expr_t *bound = ctx.bound_arg(
            ASR::down_cast<ASR::Var_t>(v)->m_v);
        if (bound == nullptr) return false;
        GpuExtentContext caller_ctx;
        caller_ctx.workspaces = ctx.workspaces;
        return gpu_extent_member_key(bound, caller_ctx, key);
    }
    if (!ASR::is_a<ASR::ArraySize_t>(*v)) return false;
    ASR::expr_t *array = ASRUtils::get_past_array_physical_cast(
        ASR::down_cast<ASR::ArraySize_t>(v)->m_v);
    if (ASR::is_a<ASR::Var_t>(*array)) {
        ASR::expr_t *bound = ctx.bound_arg(
            ASR::down_cast<ASR::Var_t>(array)->m_v);
        if (bound == nullptr) return false;
        array = ASRUtils::get_past_array_physical_cast(bound);
    }
    if (!ASR::is_a<ASR::StructInstanceMember_t>(*array)) return false;
    ASR::StructInstanceMember_t *sm =
        ASR::down_cast<ASR::StructInstanceMember_t>(array);
    ASR::expr_t *base = sm->m_v;
    if (ASR::is_a<ASR::ArrayItem_t>(*base)) {
        base = ASR::down_cast<ASR::ArrayItem_t>(base)->m_v;
    } else if (ASR::is_a<ASR::ArraySection_t>(*base)) {
        base = ASR::down_cast<ASR::ArraySection_t>(base)->m_v;
    }
    if (!ASR::is_a<ASR::Var_t>(*base)) return false;
    key.base = ASRUtils::symbol_name(
        ASR::down_cast<ASR::Var_t>(base)->m_v);
    key.member = ASRUtils::symbol_name(
        ASRUtils::symbol_get_past_external(sm->m_m));
    return true;
}

// Every routine a kernel calls with an element of one of its struct arrays,
// paired with the shapes that routine gives the components of that element.
// Reported as "struct_array.component" keys of the kernel's own arrays.
class KernelStructMemberShapes:
    public ASR::BaseWalkVisitor<KernelStructMemberShapes> {
    public:

        // key -> (shape, the context that reads the shape's extents)
        std::map<GpuStructMemberKey,
            std::pair<ASR::alloc_arg_t*, GpuExtentContext>> shapes;
        const std::map<std::string, const GpuVlaWorkspace*> *workspaces;

        KernelStructMemberShapes(
            const std::map<std::string, const GpuVlaWorkspace*> *workspaces_):
            workspaces(workspaces_) {}

        void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x.m_name);
            if (!ASR::is_a<ASR::Function_t>(*sym)) return;
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
            std::map<GpuStructMemberKey, ASR::alloc_arg_t*> callee_shapes =
                struct_member_shapes(fn->m_body, fn->n_body);
            if (callee_shapes.empty()) return;
            for (size_t i = 0; i < x.n_args && i < fn->n_args; i++) {
                if (!x.m_args[i].m_value) continue;
                ASR::expr_t *actual = ASRUtils::get_past_array_physical_cast(
                    x.m_args[i].m_value);
                if (!ASR::is_a<ASR::ArrayItem_t>(*actual)) continue;
                ASR::ArrayItem_t *item =
                    ASR::down_cast<ASR::ArrayItem_t>(actual);
                if (!ASR::is_a<ASR::StructType_t>(
                        *ASRUtils::extract_type(item->m_type))) {
                    continue;
                }
                if (!ASR::is_a<ASR::Var_t>(*item->m_v)) continue;
                std::string array_name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(item->m_v)->m_v);
                if (!ASR::is_a<ASR::Var_t>(*fn->m_args[i])) continue;
                std::string formal_name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
                for (auto &shape: callee_shapes) {
                    // The callee shapes a component of its own dummy; the
                    // same component of the kernel's array is what the
                    // launch has to size.
                    if (shape.first.base != formal_name) continue;
                    GpuExtentContext ctx;
                    ctx.callee = fn;
                    ctx.args = x.m_args;
                    ctx.n_args = x.n_args;
                    ctx.workspaces = workspaces;
                    shapes.emplace(
                        GpuStructMemberKey{array_name, shape.first.member},
                        std::make_pair(shape.second, ctx));
                }
            }
        }

        void visit_Function(const ASR::Function_t &/*x*/) {
            // Only the kernel's own calls reach the kernel's arrays.
        }
};

// The shape of every component of a kernel's struct arrays that the kernel
// writes without the caller having allocated it, gathered from the Allocate
// and ReAlloc statements that give the component its extents.
inline std::map<GpuStructMemberKey,
        std::pair<ASR::alloc_arg_t*, GpuExtentContext>>
    kernel_struct_member_shapes(const ASR::Function_t &kernel,
        const std::map<std::string, const GpuVlaWorkspace*> &ws_by_name) {
    KernelStructMemberShapes visitor(&ws_by_name);
    for (size_t i = 0; i < kernel.n_body; i++) {
        visitor.visit_stmt(*kernel.m_body[i]);
    }
    // A component the kernel shapes itself, rather than through a call.
    GpuExtentContext ctx;
    ctx.workspaces = &ws_by_name;
    for (auto &shape: struct_member_shapes(kernel.m_body, kernel.n_body)) {
        visitor.shapes.emplace(shape.first,
            std::make_pair(shape.second, ctx));
    }
    return visitor.shapes;
}

// The number of elements a kernel writes into each component of its struct
// arrays that the caller left unallocated, where that number is known before
// the kernel is dispatched.
inline std::map<GpuStructMemberKey, int64_t>
    find_struct_member_vla_write_sizes(
        const ASR::Function_t &kernel,
        const std::vector<GpuVlaWorkspace> &vla_workspaces) {
    std::map<std::string, const GpuVlaWorkspace*> ws_by_name;
    for (auto &ws : vla_workspaces) {
        ws_by_name[ws.var_name] = &ws;
    }
    std::map<GpuStructMemberKey, int64_t> result;
    for (auto &shape: kernel_struct_member_shapes(kernel, ws_by_name)) {
        int64_t total = 1;
        bool known = true;
        for (size_t d = 0; d < shape.second.first->n_dims; d++) {
            int64_t length;
            if (!gpu_extent_value(shape.second.first->m_dims[d].m_length,
                    shape.second.second, length)) {
                known = false;
                break;
            }
            total *= length;
        }
        if (known && total > 0) {
            result[shape.first] = total;
        }
    }
    return result;
}

// The components whose size a kernel only learns from another component of a
// struct array, as a map from the written component to the one it is sized
// from.
inline std::map<GpuStructMemberKey, GpuStructMemberKey>
    find_struct_member_vla_runtime_sources(const ASR::Function_t &kernel) {
    std::map<std::string, const GpuVlaWorkspace*> ws_by_name;
    std::map<GpuStructMemberKey, GpuStructMemberKey> result;
    for (auto &shape: kernel_struct_member_shapes(kernel, ws_by_name)) {
        if (shape.second.first->n_dims != 1) continue;
        GpuStructMemberKey source;
        if (gpu_extent_member_key(shape.second.first->m_dims[0].m_length,
                shape.second.second, source)) {
            result[shape.first] = source;
        }
    }
    return result;
}

} // namespace LCompilers

#endif // LFORTRAN_GPU_UTILS_H

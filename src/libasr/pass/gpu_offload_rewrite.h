#ifndef LIBASR_PASS_GPU_OFFLOAD_REWRITE_H
#define LIBASR_PASS_GPU_OFFLOAD_REWRITE_H

#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/symbol_expr_substitution.h>
#include <libasr/exception.h>
#include <libasr/pass/gpu_offload_designator.h>
#include <libasr/pass/gpu_offload_preflight.h>

namespace LCompilers {

// A new variable named `name` in `scope`, added to it.
//
// This pass creates a great many variables: loop indices, temporaries for
// intrinsic results, per-thread workspaces, kernel parameters. What such a
// variable looks like -- default storage, public access, required presence,
// no initialiser -- is the shared builder's answer rather than this pass's,
// so that a variable the offload path declares and one a host pass declares
// cannot come out differently.
ASR::symbol_t* gpu_new_variable(Allocator &al, const Location &loc,
        SymbolTable *scope, const std::string &name, ASR::ttype_t *type,
        ASR::intentType intent = ASR::intentType::Local,
        ASR::symbol_t *type_decl = nullptr);

// Fill in the `start`/`end` bounds of a synthesized `do` loop head for
// dimension `d` of `arr_expr`. Descriptor arrays (allocatables, pointers,
// assumed-shape dummies and the temporaries created for array-valued
// `associate` selectors) carry no compile-time `dimension_t` entries, so
// their `m_start`/`m_length` are null; fall back to the runtime bounds in
// that case instead of emitting a loop head with null bounds.
std::pair<ASR::expr_t*, ASR::expr_t*> get_dim_bounds(Allocator &al,
        const Location &loc, ASR::dimension_t *dims, size_t d,
        ASR::expr_t *arr_expr);

void set_loop_head_bounds(Allocator &al, const Location &loc,
        ASR::do_loop_head_t &head, ASR::dimension_t *dims, size_t d,
        ASR::expr_t *arr_expr);

// Name of the Struct that owns `member`, used as the m_module_name of an
// ExternalSymbol pointing at it. For an inherited member this is the
// ancestor type, not the type the reference was written through.
std::string struct_member_owner_name(ASR::symbol_t *member,
        const std::string &fallback);

// `size(f(...))`: the extent of an array expression that is not a
// designator.  Evaluating it on the host would mean calling `f` there,
// with whatever the arguments happen to be outside the loop -- the loop
// index has no value there at all.  The shape is already recorded in the
// expression's own type, though: a result declared `real :: r(n)` carries
// `n` as the length of its one dimension, written in the symbols of the
// scope the call is made from.  Rewrite the ArraySize to that length, so
// the host evaluates the extent without evaluating the call.  This is the
// same shape `build_gpu_array_extent_node` reads when it decides the
// extent is resolvable, so emitter and pre-flight agree.  Returns nullptr
// when the type does not record the length, leaving the node alone.
ASR::expr_t* gpu_array_size_from_type(Allocator &al,
        const ASR::ArraySize_t *sz);

// Rewrites every `size(<non-designator>)` in an expression tree to the
// extent recorded in that expression's own type.
class GpuArraySizeFromTypeReplacer :
        public ASR::BaseExprReplacer<GpuArraySizeFromTypeReplacer> {
public:
    Allocator &al;
    GpuArraySizeFromTypeReplacer(Allocator &al_) : al(al_) {}
    void replace_ArraySize(ASR::ArraySize_t *x) {
        ASR::BaseExprReplacer<GpuArraySizeFromTypeReplacer>
            ::replace_ArraySize(x);
        ASR::expr_t *rep = gpu_array_size_from_type(al, x);
        if (rep) *current_expr = rep;
    }
};

ASR::expr_t* gpu_simplify_array_sizes(Allocator &al,
        ASR::expr_t *expr);

// Replaces all Var references in-place to point to the kernel scope symbols
class GpuReplaceSymbols : public ASR::BaseExprReplacer<GpuReplaceSymbols> {
public:
    SymbolTable &kernel_scope;
    std::set<SymbolTable*> skip_scopes;
    // Snapshot restore walks nested Block tables whose parent is the outer
    // snapshot, not the host. Look up through that chain so an inner Var of
    // an outer-block local is retargeted at the copy, not left pointing at
    // the original Block that the kernel path is about to move.
    bool resolve_through_parents = false;
    GpuReplaceSymbols(SymbolTable &scope) : kernel_scope(scope) {}

    ASR::symbol_t *lookup_symbol(const std::string &name) {
        return resolve_through_parents ? kernel_scope.resolve_symbol(name)
                                       : kernel_scope.get_symbol(name);
    }

    void replace_Var(ASR::Var_t *x) {
        std::string name = ASRUtils::symbol_name(x->m_v);
        for (auto *ss : skip_scopes) {
            if (ss->get_symbol(name)) return;
        }
        ASR::symbol_t *new_sym = lookup_symbol(name);
        if (new_sym) {
            x->m_v = new_sym;
        }
    }

    void replace_StructInstanceMember(ASR::StructInstanceMember_t *x) {
        // Replace the struct variable expression (e.g., Var x)
        ASR::expr_t **current_expr_copy = current_expr;
        current_expr = &(x->m_v);
        replace_expr(x->m_v);
        current_expr = current_expr_copy;
        // Replace the member symbol to point to kernel scope's ExternalSymbol
        std::string mem_name = ASRUtils::symbol_name(x->m_m);
        ASR::symbol_t *new_mem = lookup_symbol(mem_name);
        if (new_mem) {
            x->m_m = new_mem;
        }
    }

    // A structure constructor names the derived type it builds. Left
    // naming the host's copy of the definition, the later lowering of the
    // constructor into per-component assignments takes the components from
    // that copy, and the kernel ends up describing one component through
    // two `Variable_t`s -- the dummy's through the kernel's copy, the
    // assignment's through the host's.
    void replace_StructConstructor(ASR::StructConstructor_t *x) {
        ASR::symbol_t *type_sym = ASRUtils::symbol_get_past_external(
            x->m_dt_sym);
        if (type_sym && ASR::is_a<ASR::Struct_t>(*type_sym)) {
            ASR::symbol_t *kernel_sym = lookup_symbol(
                ASRUtils::symbol_name(type_sym));
            if (kernel_sym && ASR::is_a<ASR::Struct_t>(
                    *ASRUtils::symbol_get_past_external(kernel_sym))) {
                x->m_dt_sym = kernel_sym;
            }
        }
        ASR::BaseExprReplacer<GpuReplaceSymbols>::replace_StructConstructor(x);
    }

    void replace_FunctionCall(ASR::FunctionCall_t *x) {
        // Remap m_name to kernel scope symbol
        std::string name = ASRUtils::symbol_name(x->m_name);
        ASR::symbol_t *new_sym = lookup_symbol(name);
        if (!new_sym && ASR::is_a<ASR::ExternalSymbol_t>(*x->m_name)) {
            // Try sanitized ExternalSymbol name (handles disambiguated
            // functions where different modules define same-named functions)
            std::string sanitized = name;
            for (char &c : sanitized) {
                if (c == '~' || c == '@') c = '_';
            }
            new_sym = lookup_symbol(sanitized);
            if (!new_sym) {
                // ExternalSymbol name differs from resolved function name;
                // try the underlying function's name (e.g., "construct"
                // instead of "~mytype_t@construct").
                std::string resolved_name = ASRUtils::symbol_name(
                    ASRUtils::symbol_get_past_external(x->m_name));
                new_sym = lookup_symbol(resolved_name);
            }
        }
        if (new_sym) {
            x->m_name = new_sym;
        }
        if (x->m_original_name) {
            std::string orig_name = ASRUtils::symbol_name(x->m_original_name);
            ASR::symbol_t *new_orig = lookup_symbol(orig_name);
            if (new_orig) {
                x->m_original_name = new_orig;
            }
        }
        // Call base to handle arguments, type, value, dt
        ASR::BaseExprReplacer<GpuReplaceSymbols>::replace_FunctionCall(x);
    }
};

class GpuReplaceSymbolsVisitor :
    public ASR::CallReplacerOnExpressionsVisitor<GpuReplaceSymbolsVisitor> {
public:
    GpuReplaceSymbols replacer;
    GpuReplaceSymbolsVisitor(SymbolTable &scope) : replacer(scope) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};

// Resolves associate variable references to their original targets.
// A workspace extent written as `size(a(i)%m, d)` -- the extent of an
// allocatable array component reached through a subscript into an array
// of derived types -- can be reproduced on neither side of the launch as
// it stands.  The kernel receives such a component as one flat data
// buffer plus a per-element total size, never the component's individual
// extents, and the host resolver cannot walk a subscript in the middle of
// a component path.  The value is an ordinary integer the host can
// compute before the launch, though, so it becomes one more scalar kernel
// argument: this replacer rewrites every such `size(...)` in the kernel
// body to a Var naming that argument and records the matching host-scope
// expression for the caller to pass as the actual.  Host workspace sizing
// and shader stride then read one and the same scalar and cannot
// disagree.
class GpuStructArrayMemberExtent :
        public ASR::BaseExprReplacer<GpuStructArrayMemberExtent> {
public:
    Allocator &al;
    SymbolTable *orig_scope;
    SymbolTable *kernel_scope;
    const std::vector<std::string> &arg_names;
    // Kernel parameter and the host expression that supplies its value,
    // in the order the parameters were created.
    std::vector<std::pair<ASR::symbol_t*, ASR::expr_t*>> &added;
    // Every slot overwritten, so the loop can be put back as it was. The
    // rewrite reaches the types of a BLOCK's locals, and a BLOCK is shared
    // with the host rather than copied for the kernel.
    std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> undo;
    std::map<std::string, ASR::symbol_t*> by_key;

    GpuStructArrayMemberExtent(Allocator &al_, SymbolTable *orig_scope_,
            SymbolTable *kernel_scope_,
            const std::vector<std::string> &arg_names_,
            std::vector<std::pair<ASR::symbol_t*, ASR::expr_t*>> &added_)
        : al(al_), orig_scope(orig_scope_), kernel_scope(kernel_scope_),
          arg_names(arg_names_), added(added_) {}

    // The component symbol as the host scope names it.  The kernel body
    // may reach a component through an ExternalSymbol that only the
    // kernel scope holds, which the host expression must not reference.
    ASR::symbol_t* host_member_ref(ASR::symbol_t *m) {
        ASR::symbol_t *target = ASRUtils::symbol_get_past_external(m);
        for (auto &item : orig_scope->get_scope()) {
            if (!ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) continue;
            if (ASRUtils::symbol_get_past_external(item.second) == target) {
                return item.second;
            }
        }
        return m;
    }

    void replace_ArraySize(ASR::ArraySize_t *x) {
        if (rewrite_array_size(x)) return;
        ASR::BaseExprReplacer<GpuStructArrayMemberExtent>
            ::replace_ArraySize(x);
    }

    bool rewrite_array_size(ASR::ArraySize_t *x) {
        GpuStructArrayMemberExtentRef ref;
        if (!match_gpu_struct_array_member_extent(
                ASRUtils::EXPR((ASR::asr_t*)x), arg_names, ref)) {
            return false;
        }
        ASR::ArrayItem_t *ai = ref.item;
        ASR::StructInstanceMember_t *sim = ref.member;
        int64_t d = ref.dim;
        std::string arr_name = ASRUtils::symbol_name(ref.base->m_v);
        ASR::symbol_t *host_arr = orig_scope->resolve_symbol(arr_name);
        if (!host_arr) return false;
        ASR::ttype_t *size_type = ASRUtils::extract_type(x->m_type);
        const Location &loc = x->base.base.loc;

        Vec<ASR::array_index_t> host_idx;
        host_idx.reserve(al, ai ? ai->n_args : 0);
        std::string key = arr_name;
        for (size_t k = 0; ai != nullptr && k < ai->n_args; k++) {
            ASR::array_index_t &ix = ai->m_args[k];
            ASR::expr_t *host_sub = nullptr;
            std::string tag;
            int64_t sub_val;
            if (try_eval_int_constant(ix.m_right, sub_val)) {
                host_sub = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al,
                    loc, sub_val, ASRUtils::expr_type(ix.m_right),
                    ASR::integerbozType::Decimal));
                tag = sub_val < 0 ? "m" + std::to_string(-sub_val)
                                  : std::to_string(sub_val);
            } else {
                std::string sub_name = ASRUtils::symbol_name(
                    ASR::down_cast<ASR::Var_t>(ix.m_right)->m_v);
                ASR::symbol_t *host_sub_sym =
                    orig_scope->resolve_symbol(sub_name);
                if (!host_sub_sym) return false;
                host_sub = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                    host_sub_sym));
                tag = sub_name;
            }
            ASR::array_index_t host_ix;
            host_ix.loc = ix.loc;
            host_ix.m_left = nullptr;
            host_ix.m_step = nullptr;
            host_ix.m_right = host_sub;
            host_idx.push_back(al, host_ix);
            key += "_" + tag;
        }
        std::string mem_name = ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(sim->m_m));
        key += "_" + mem_name + "_" + std::to_string(d);

        ASR::symbol_t *sym = nullptr;
        auto it = by_key.find(key);
        if (it != by_key.end()) {
            sym = it->second;
        } else {
            std::string param_name = kernel_scope->get_unique_name(
                "__memdim_" + key);
            sym = gpu_new_variable(al, loc, kernel_scope, param_name,
                ASRUtils::duplicate_type(al, size_type),
                ASR::intentType::InOut);
            ASR::expr_t *host_item = ASRUtils::EXPR(
                ASR::make_Var_t(al, loc, host_arr));
            if (ai != nullptr) {
                host_item = ASRUtils::EXPR(ASR::make_ArrayItem_t(al, loc,
                    host_item, host_idx.p, host_idx.n, ai->m_type,
                    ai->m_storage_format, nullptr));
            }
            ASR::expr_t *host_member = ASRUtils::EXPR(
                ASR::make_StructInstanceMember_t(al, loc, host_item,
                    host_member_ref(sim->m_m), sim->m_type, nullptr));
            ASR::expr_t *host_dim = ASRUtils::EXPR(
                ASR::make_IntegerConstant_t(al, loc, d, size_type,
                    ASR::integerbozType::Decimal));
            ASR::expr_t *host_size = ASRUtils::EXPR(ASR::make_ArraySize_t(
                al, loc, host_member, host_dim, size_type, nullptr));
            added.push_back({sym, host_size});
            by_key[key] = sym;
        }
        undo.push_back({current_expr, *current_expr});
        *current_expr = ASRUtils::EXPR(ASR::make_Var_t(al, loc, sym));
        return true;
    }
};

class GpuStructArrayMemberExtentVisitor :
        public ASR::CallReplacerOnExpressionsVisitor<
            GpuStructArrayMemberExtentVisitor> {
public:
    GpuStructArrayMemberExtent replacer;

    GpuStructArrayMemberExtentVisitor(Allocator &al,
            SymbolTable *orig_scope, SymbolTable *kernel_scope,
            const std::vector<std::string> &arg_names,
            std::vector<std::pair<ASR::symbol_t*, ASR::expr_t*>> &added)
        : replacer(al, orig_scope, kernel_scope, arg_names, added) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    // A BLOCK is where the workspace temporaries live, and the generated
    // visitor does not descend into one on its own.
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }
};

// Collects Var references in function bodies that point to symbols
// not reachable through the function's scope chain. This happens when
// a contained function references host-scope variables (e.g., Parameters)
// that are not present in the kernel scope hierarchy.
class DanglingVarCollector : public ASR::BaseWalkVisitor<DanglingVarCollector> {
public:
    SymbolTable *func_scope;
    std::map<std::string, ASR::symbol_t*> dangling;
    std::set<SymbolTable*> inner_scopes;
    DanglingVarCollector(SymbolTable *fs) : func_scope(fs) {}
    void visit_Var(const ASR::Var_t &x) {
        std::string name = ASRUtils::symbol_name(x.m_v);
        for (auto *scope : inner_scopes) {
            if (scope->get_symbol(name)) return;
        }
        if (!func_scope->resolve_symbol(name) &&
                dangling.find(name) == dangling.end()) {
            dangling[name] = x.m_v;
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *ab =
            ASR::down_cast<ASR::AssociateBlock_t>(x.m_m);
        inner_scopes.insert(ab->m_symtab);
        for (size_t i = 0; i < ab->n_body; i++) {
            visit_stmt(*ab->m_body[i]);
        }
        inner_scopes.erase(ab->m_symtab);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        inner_scopes.insert(block->m_symtab);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
        inner_scopes.erase(block->m_symtab);
    }
};

// Fixes dangling Var references in function bodies by resolving symbol
// names through the function's scope chain and replacing the Var target.
class DanglingVarFixer :
        public ASRUtils::BlockBodyWalkVisitor<DanglingVarFixer> {
public:
    SymbolTable *func_scope;
    std::set<std::string> &target_names;
    DanglingVarFixer(SymbolTable *fs, std::set<std::string> &names)
        : func_scope(fs), target_names(names) {}
    void visit_Var(const ASR::Var_t &x) {
        std::string name = ASRUtils::symbol_name(x.m_v);
        if (target_names.count(name)) {
            ASR::symbol_t *new_sym = func_scope->resolve_symbol(name);
            if (new_sym) {
                const_cast<ASR::Var_t&>(x).m_v = new_sym;
            }
        }
    }
};

// Replaces StructInstanceMember(Var(x), a) with Var(x__a) for
// allocatable array members that have been decomposed into separate
// kernel parameters.
class GpuDecomposeStructReplacer :
    public ASR::BaseExprReplacer<GpuDecomposeStructReplacer> {
public:
    Allocator &al;
    SymbolTable *kernel_scope;
    std::map<std::pair<std::string, std::string>, std::string> &decomp_map;

    GpuDecomposeStructReplacer(Allocator &al_, SymbolTable *scope,
        std::map<std::pair<std::string, std::string>, std::string> &dmap)
        : al(al_), kernel_scope(scope), decomp_map(dmap) {}

    void replace_StructInstanceMember(ASR::StructInstanceMember_t *x) {
        if (ASR::is_a<ASR::Var_t>(*x->m_v)) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x->m_v);
            std::string struct_name = ASRUtils::symbol_name(v->m_v);
            ASR::symbol_t *mem =
                ASRUtils::symbol_get_past_external(x->m_m);
            std::string mem_name = ASRUtils::symbol_name(mem);
            auto key = std::make_pair(struct_name, mem_name);
            auto it = decomp_map.find(key);
            if (it != decomp_map.end()) {
                ASR::symbol_t *param_sym =
                    kernel_scope->get_symbol(it->second);
                if (param_sym) {
                    *current_expr = ASRUtils::EXPR(
                        ASR::make_Var_t(al, x->base.base.loc, param_sym));
                    return;
                }
            }
        }
        ASR::BaseExprReplacer<GpuDecomposeStructReplacer>::
            replace_StructInstanceMember(x);
    }
};

class GpuDecomposeStructVisitor :
    public ASR::CallReplacerOnExpressionsVisitor<GpuDecomposeStructVisitor> {
public:
    GpuDecomposeStructReplacer replacer;
    GpuDecomposeStructVisitor(Allocator &al, SymbolTable *scope,
        std::map<std::pair<std::string, std::string>, std::string> &dmap)
        : replacer(al, scope, dmap) {}
    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};

// `x%c_(k)`: one element of an array of derived type that is itself a
// component of something the kernel is handed.  A Metal kernel receives
// such a component as flat data plus per-element offsets and extents,
// and nothing on the device carries the extents of the array components
// hanging off the element that was selected -- so an expression as
// ordinary as `size(x%c_(k)%upper_, 1)` cannot be evaluated there and
// the loop is declined.
//
// Copying the element into a temporary on the host before the launch
// removes the whole difficulty: the temporary is an ordinary derived-type
// kernel argument, and `size(t%upper_, 1)` is the shape the existing
// per-component extent machinery already resolves.
//
// An element reached directly from a variable, `c(k)`, is deliberately
// not collected: that shape is marshalled correctly today.
class GpuStructElementGatherCollector :
        public ASRUtils::BlockBodyWalkVisitor<GpuStructElementGatherCollector> {
public:
    std::vector<ASR::ArrayItem_t*> found;

    static bool is_gatherable(const ASR::ArrayItem_t &x) {
        ASR::ttype_t *t = ASRUtils::type_get_past_allocatable_pointer(
            const_cast<ASR::ttype_t*>(x.m_type));
        if (!t || ASR::is_a<ASR::Array_t>(*t)) return false;
        if (!ASR::is_a<ASR::StructType_t>(*t)) return false;
        // A polymorphic element carries a dynamic type the copy would
        // not reproduce.
        if (ASRUtils::is_class_type(t)) return false;
        ASR::expr_t *base = ASRUtils::get_past_array_physical_cast(x.m_v);
        if (!base || !ASR::is_a<ASR::StructInstanceMember_t>(*base)) {
            return false;
        }
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i].m_left || x.m_args[i].m_step) return false;
            if (!x.m_args[i].m_right) return false;
        }
        return gpu_designator_base(x.m_v).is_known();
    }

    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        if (is_gatherable(x)) {
            found.push_back(const_cast<ASR::ArrayItem_t*>(&x));
        }
        ASR::BaseWalkVisitor<GpuStructElementGatherCollector>
            ::visit_ArrayItem(x);
    }
};

// Every symbol an expression mentions.
class GpuExprSymbolCollector :
        public ASR::BaseWalkVisitor<GpuExprSymbolCollector> {
public:
    std::set<ASR::symbol_t*> syms;

    void visit_Var(const ASR::Var_t &x) {
        syms.insert(ASRUtils::symbol_get_past_external(x.m_v));
    }
};

// One gathered element: the designator that was copied and the host
// temporary that now stands for it.
struct GpuStructElementGather {
    ASR::expr_t *chain = nullptr;
    ASR::symbol_t *temp = nullptr;
    // The loop writes into this element, so the temporary has to be
    // copied back over it once the kernel has finished.
    bool scatter = false;
};

// Replaces each collected designator with a reference to its temporary,
// recording every slot it overwrites so the substitution can be undone
// when the loop turns out not to be offloadable after all.
class GpuStructElementGatherReplacer :
        public ASR::BaseExprReplacer<GpuStructElementGatherReplacer> {
public:
    Allocator &al;
    const std::vector<GpuStructElementGather> &gathers;
    std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> &undo;

    GpuStructElementGatherReplacer(Allocator &al_,
            const std::vector<GpuStructElementGather> &gathers_,
            std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> &undo_)
        : al(al_), gathers(gathers_), undo(undo_) {}

    void replace_ArrayItem(ASR::ArrayItem_t *x) {
        ASR::expr_t *e = ASRUtils::EXPR((ASR::asr_t*)x);
        for (const GpuStructElementGather &g : gathers) {
            if (!gpu_same_designator(g.chain, e)) continue;
            undo.push_back({current_expr, *current_expr});
            *current_expr = ASRUtils::EXPR(ASR::make_Var_t(al,
                x->base.base.loc, g.temp));
            return;
        }
        ASR::BaseExprReplacer<GpuStructElementGatherReplacer>
            ::replace_ArrayItem(x);
    }
};

class GpuStructElementGatherVisitor :
        public ASR::CallReplacerOnExpressionsVisitor<
            GpuStructElementGatherVisitor> {
public:
    GpuStructElementGatherReplacer replacer;

    GpuStructElementGatherVisitor(Allocator &al,
            const std::vector<GpuStructElementGather> &gathers,
            std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> &undo)
        : replacer(al, gathers, undo) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }
};

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_OFFLOAD_REWRITE_H

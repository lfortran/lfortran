#include <map>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_visitor.h>
#include <libasr/pass/parallel_canonicalize.h>
#include <libasr/pass/symbol_expr_substitution.h>

namespace LCompilers {

using ASR::down_cast;
using ASR::is_a;

namespace {

// The variables bound to an associate name in a statement list, at any
// depth, by the name.
class GpuAssociatedRoots :
        public ASRUtils::BlockBodyWalkVisitor<GpuAssociatedRoots> {
public:
    std::map<ASR::symbol_t*, ASR::symbol_t*> roots;
    void visit_Associate(const ASR::Associate_t &x) {
        ASR::symbol_t *root = gpu_designator_base(x.m_value).root;
        if (root && ASR::is_a<ASR::Var_t>(*x.m_target)) {
            roots[ASR::down_cast<ASR::Var_t>(x.m_target)->m_v] = root;
        }
    }
};

class GpuReadSymbols : public ASR::BaseWalkVisitor<GpuReadSymbols> {
public:
    std::set<ASR::symbol_t*> read;
    void visit_Var(const ASR::Var_t &x) {
        read.insert(ASRUtils::symbol_get_past_external(x.m_v));
    }

    // An associate name cannot change the shape of what it is bound to.
    void visit_ArrayBound(const ASR::ArrayBound_t &x) {
        if (!is_member_chain(x.m_v)) visit_expr(*x.m_v);
        if (x.m_dim) visit_expr(*x.m_dim);
    }

    void visit_ArraySize(const ASR::ArraySize_t &x) {
        if (!is_member_chain(x.m_v)) visit_expr(*x.m_v);
        if (x.m_dim) visit_expr(*x.m_dim);
    }

private:
    static bool is_member_chain(ASR::expr_t *e) {
        while (is_a<ASR::StructInstanceMember_t>(*e)) {
            e = down_cast<ASR::StructInstanceMember_t>(e)->m_v;
        }
        return is_a<ASR::Var_t>(*e);
    }
};

// Puts the selector in place of its associate name in the body and the
// declarations of a scope, also in the scopes it opens.
class GpuAssociateNameReplacer :
        public ASR::CallReplacerOnExpressionsVisitor<
            GpuAssociateNameReplacer> {
public:
    AssociateVarResolver replacer;
    GpuAssociateNameReplacer(Allocator &al,
            std::map<ASR::symbol_t*, ASR::expr_t*> &names)
        : replacer(al, names) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        visit_symbol(*x.m_m);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        visit_symbol(*x.m_m);
    }
};

bool is_scalar_associate_name(ASR::AssociateBlock_t &ab, ASR::stmt_t *s) {
    if (!is_a<ASR::Associate_t>(*s)) return false;
    ASR::expr_t *target = down_cast<ASR::Associate_t>(s)->m_target;
    if (!is_a<ASR::Var_t>(*target)) return false;
    ASR::symbol_t *sym = down_cast<ASR::Var_t>(target)->m_v;
    if (!is_a<ASR::Variable_t>(*sym)) return false;
    ASR::Variable_t *v = down_cast<ASR::Variable_t>(sym);
    return v->m_parent_symtab == ab.m_symtab
        && ASRUtils::is_pointer(v->m_type) && !ASRUtils::is_array(v->m_type);
}

// Inner scopes first, so that what an inner scope writes through a name
// of its own is a write of the variable when the outer scope looks.
class GpuScalarAssociateAliaser :
        public ASRUtils::BlockBodyWalkVisitor<GpuScalarAssociateAliaser> {
public:
    Allocator &al;
    explicit GpuScalarAssociateAliaser(Allocator &al_) : al(al_) {}

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASRUtils::BlockBodyWalkVisitor<GpuScalarAssociateAliaser>
            ::visit_AssociateBlockCall(x);
        ASR::symbol_t *s = ASRUtils::symbol_get_past_external(x.m_m);
        if (s && is_a<ASR::AssociateBlock_t>(*s)) {
            alias(*down_cast<ASR::AssociateBlock_t>(s));
        }
    }

    void alias(ASR::AssociateBlock_t &ab) {
        std::map<ASR::symbol_t*, ASR::expr_t*> names;
        Vec<ASR::stmt_t*> body;
        body.reserve(al, ab.n_body);
        for (size_t i = 0; i < ab.n_body; i++) {
            if (!is_scalar_associate_name(ab, ab.m_body[i])) {
                body.push_back(al, ab.m_body[i]);
                continue;
            }
            ASR::Associate_t *a = down_cast<ASR::Associate_t>(ab.m_body[i]);
            GpuAssociateScopeWrites writes(ab.m_body, ab.n_body, i + 1);
            names[down_cast<ASR::Var_t>(a->m_target)->m_v] =
                gpu_fix_selector_subscripts(al, a->m_value, writes,
                    ab.m_symtab, body);
        }
        if (names.empty()) return;
        ab.m_body = body.p;
        ab.n_body = body.n;
        GpuAssociateNameReplacer replace(al, names);
        replace.visit_symbol(ab.base);
        for (auto &item : names) {
            ab.m_symtab->erase_symbol(ASRUtils::symbol_name(item.first));
        }
    }
};

} // namespace

GpuAssociateScopeWrites::GpuAssociateScopeWrites(ASR::stmt_t **body,
        size_t n_body, size_t from)
        : changed(gpu_symbols_changed_in(body + from, n_body - from)) {
    GpuAssociatedRoots roots;
    for (size_t i = 0; i < n_body; i++) roots.visit_stmt(*body[i]);
    for (auto &item : roots.roots) {
        if (gpu_writes_symbol(*changed, item.first)) {
            aliased.insert(item.second);
        }
    }
}

bool GpuAssociateScopeWrites::may_change(ASR::expr_t *e) const {
    if (ASRUtils::expr_value(e)) return false;
    if (gpu_reads_changed(*changed, e)) return true;
    GpuReadSymbols reads;
    reads.visit_expr(*e);
    for (ASR::symbol_t *s : reads.read) {
        if (aliased.count(s)) return true;
    }
    return false;
}

ASR::expr_t* gpu_fix_selector_subscripts(Allocator &al,
        ASR::expr_t *selector, const GpuAssociateScopeWrites &writes,
        SymbolTable *scope, Vec<ASR::stmt_t*> &inits) {
    auto fix = [&](ASR::expr_t *&sub) {
        if (!sub || !writes.may_change(sub)) return;
        const Location &loc = sub->base.loc;
        ASR::symbol_t *v = gpu_new_variable(al, loc, scope,
            scope->get_unique_name("__gpu_associate_subscript", false),
            ASRUtils::duplicate_type(al, ASRUtils::expr_type(sub)));
        ASR::expr_t *var = ASRUtils::EXPR(ASR::make_Var_t(al, loc, v));
        inits.push_back(al, ASRUtils::STMT(ASR::make_Assignment_t(al, loc,
            var, sub, nullptr, false, false, false)));
        sub = var;
    };
    ASR::expr_t *e = selector;
    while (e) {
        if (is_a<ASR::ArrayItem_t>(*e)) {
            ASR::ArrayItem_t *item = down_cast<ASR::ArrayItem_t>(e);
            for (size_t i = 0; i < item->n_args; i++) {
                fix(item->m_args[i].m_left);
                fix(item->m_args[i].m_right);
                fix(item->m_args[i].m_step);
            }
            e = item->m_v;
        } else if (is_a<ASR::ArraySection_t>(*e)) {
            ASR::ArraySection_t *sec = down_cast<ASR::ArraySection_t>(e);
            for (size_t i = 0; i < sec->n_args; i++) {
                fix(sec->m_args[i].m_left);
                fix(sec->m_args[i].m_right);
                fix(sec->m_args[i].m_step);
            }
            e = sec->m_v;
        } else if (is_a<ASR::StructInstanceMember_t>(*e)) {
            e = down_cast<ASR::StructInstanceMember_t>(e)->m_v;
        } else if (is_a<ASR::ArrayPhysicalCast_t>(*e)) {
            e = down_cast<ASR::ArrayPhysicalCast_t>(e)->m_arg;
        } else {
            break;
        }
    }
    return selector;
}

void gpu_alias_scalar_associate_names(Allocator &al, ASR::Function_t &fn) {
    GpuScalarAssociateAliaser aliaser(al);
    for (size_t i = 0; i < fn.n_body; i++) {
        aliaser.visit_stmt(*fn.m_body[i]);
    }
}

// Substitutes the selector for every associate name the loop inherits from
// the ASSOCIATE scopes it sits in -- in the loop heads, in the body, and in
// the array-dimension expressions of the BLOCK locals around it -- and
// hands the caller the mapping it used, which the body rewrites below need
// again for the ASSOCIATE scopes inside the loop.
void GpuOffloadVisitor::resolve_enclosing_associates(ParallelLoopNest &work,
        size_t n_dims,
        std::map<ASR::symbol_t*, ASR::expr_t*> &enclosing_assoc_map) {
    std::map<ASR::symbol_t*, ASR::expr_t*> &assoc_map = enclosing_assoc_map;
    SymbolTable *scope = current_scope;
    while (scope && scope->asr_owner &&
           scope->asr_owner->type == ASR::asrType::symbol) {
        ASR::symbol_t *owner_sym = down_cast<ASR::symbol_t>(
            scope->asr_owner);
        if (is_a<ASR::Block_t>(*owner_sym)) {
            scope = scope->parent;
            continue;
        }
        if (!is_a<ASR::AssociateBlock_t>(*owner_sym)) break;
        ASR::AssociateBlock_t *ab =
            ASR::down_cast2<ASR::AssociateBlock_t>(scope->asr_owner);
        for (size_t i = 0; i < ab->n_body; i++) {
            if (is_a<ASR::Associate_t>(*ab->m_body[i])) {
                ASR::Associate_t *assoc = down_cast<ASR::Associate_t>(
                    ab->m_body[i]);
                if (is_a<ASR::Var_t>(*assoc->m_target)) {
                    ASR::symbol_t *assoc_sym =
                        down_cast<ASR::Var_t>(assoc->m_target)->m_v;
                    assoc_map[assoc_sym] = assoc->m_value;
                }
            } else if (is_a<ASR::Assignment_t>(*ab->m_body[i])) {
                // A name bound to an expression is an Assignment of
                // the value to a variable of this AssociateBlock. It
                // holds the value the expression had when the construct
                // was entered, and that value is what the kernel is
                // handed: writing the expression into the loop instead
                // would evaluate it again, after what it reads may have
                // changed, and hand the kernel everything it reads --
                // `size(a)` would copy all of `a` to the device at every
                // launch. Only a scalar constant is written in; an array
                // name is handed over as a buffer like any other array.
                // A name already mapped (from an Associate) keeps its
                // alias: a later assignment such as `v = 0.` may read `v`
                // itself.
                ASR::Assignment_t *asgn = down_cast<ASR::Assignment_t>(
                    ab->m_body[i]);
                if (is_a<ASR::Var_t>(*asgn->m_target)) {
                    ASR::symbol_t *sym =
                        down_cast<ASR::Var_t>(asgn->m_target)->m_v;
                    if (is_a<ASR::Variable_t>(*sym) &&
                        down_cast<ASR::Variable_t>(sym)->m_parent_symtab
                            == ab->m_symtab &&
                        !ASRUtils::is_array(
                            down_cast<ASR::Variable_t>(sym)->m_type) &&
                        ASRUtils::expr_value(asgn->m_value) != nullptr &&
                        assoc_map.find(sym) == assoc_map.end()) {
                        assoc_map[sym] = asgn->m_value;
                    }
                }
            }
        }
        scope = scope->parent;
    }
    if (!assoc_map.empty()) {
        AssociateVarResolver resolver(al, assoc_map);
        for (size_t d = 0; d < n_dims; d++) {
            ASR::do_loop_head_t &head = work.head(d);
            if (head.m_start) {
                resolver.current_expr = &(head.m_start);
                resolver.replace_expr(head.m_start);
            }
            if (head.m_end) {
                resolver.current_expr = &(head.m_end);
                resolver.replace_expr(head.m_end);
            }
            if (head.m_increment) {
                resolver.current_expr = &(head.m_increment);
                resolver.replace_expr(head.m_increment);
            }
        }
        AssociateVarResolverVisitor resolver_visitor(al, assoc_map);
        for (size_t i = 0; i < work.n_body; i++) {
            resolver_visitor.visit_stmt(*work.body[i]);
        }
        // Resolve associate aliases in enclosing Block scopes'
        // variable type expressions. When a parallel loop is
        // inside a Block that is inside an AssociateBlock, the
        // block-local arrays may use associate variables in
        // their dimension expressions (e.g., `real r(size(n))`
        // where `n` is an associate alias). These must be
        // resolved before kernel extraction moves the block
        // into the kernel scope where the AssociateBlock's
        // symtab is no longer reachable.
        {
            SymbolTable *bs = current_scope;
            while (bs && bs->asr_owner &&
                   bs->asr_owner->type == ASR::asrType::symbol) {
                ASR::symbol_t *owner = down_cast<ASR::symbol_t>(
                    bs->asr_owner);
                if (is_a<ASR::Block_t>(*owner)) {
                    AssociateVarResolver type_resolver(al,
                        assoc_map);
                    for (auto &item : bs->get_scope()) {
                        if (!ASR::is_a<ASR::Variable_t>(
                                *item.second))
                            continue;
                        ASR::Variable_t *var =
                            ASR::down_cast<ASR::Variable_t>(
                                item.second);
                        if (!ASR::is_a<ASR::Array_t>(
                                *var->m_type))
                            continue;
                        ASR::Array_t *arr =
                            ASR::down_cast<ASR::Array_t>(
                                var->m_type);
                        for (size_t d = 0; d < arr->n_dims;
                             d++) {
                            if (arr->m_dims[d].m_start) {
                                type_resolver.current_expr =
                                    &(arr->m_dims[d].m_start);
                                type_resolver.replace_expr(
                                    arr->m_dims[d].m_start);
                            }
                            if (arr->m_dims[d].m_length) {
                                type_resolver.current_expr =
                                    &(arr->m_dims[d].m_length);
                                type_resolver.replace_expr(
                                    arr->m_dims[d].m_length);
                            }
                        }
                    }
                    bs = bs->parent;
                } else if (is_a<ASR::AssociateBlock_t>(*owner)) {
                    bs = bs->parent;
                } else {
                    break;
                }
            }
        }
    }
}

} // namespace LCompilers

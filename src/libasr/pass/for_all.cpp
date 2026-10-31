#include <libasr/asr.h>
#include <libasr/containers.h>
#include <libasr/exception.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_verify.h>
#include <libasr/pass/replace_for_all.h>
#include <libasr/pass/replace_implied_do_loops.h>
#include <libasr/pass/stmt_walk_visitor.h>

#include <algorithm>
#include <set>

namespace LCompilers {

/*
 * This ASR pass replaces forall statement with Do Concurrent.
 *
 * Converts:
 *      forall(i=start:end:inc) array(i) = i
 *
 * to:
 *      do concurrent (i=start:end:inc)
 *          array(i) = i
 *      end do
 *
 * A FORALL evaluates the right-hand side, the mask and the subscripts of
 * the variable for every index value before it assigns any element
 * (F2018 10.2.4.2.3). The single loop above is equivalent unless these
 * may read a part of the variable that another iteration assigns. They
 * may not if every reference to the variable is either exactly the part
 * the same iteration assigns (`a(i) = a(i) + 1`, with subscripts that do
 * not depend on the variable) or a different component of it
 * (`s(i)%x = s(j)%y`), no pointer may alias it (ASSOCIATE names and
 * EQUIVALENCE are pointers too), no other member of its common block is
 * read, and no called function can access it by host or use association
 * or through a common block. Otherwise the FORALL is lowered to
 *
 *      n = 0
 *      do i = start, end, inc
 *          if (<mask>) n = n + 1
 *      end do
 *      allocate(rhs(n), idx(n))
 *      k = 0
 *      do i = start, end, inc
 *          if (<mask>) then
 *              k = k + 1
 *              idx(k) = i
 *              rhs(k) = <rhs>
 *          end if
 *      end do
 *      do k = 1, n
 *          i = idx(k)
 *          array(i) = rhs(k)
 *      end do
 *      deallocate(rhs, idx)
 *
 * For an array-valued variable, the first loop also finds the largest
 * extent of each of its dimensions, and `rhs` gets these as leading
 * dimensions. If the assignment reallocates the variable, the extents are
 * those of the right-hand side and are stored for every index value.
 * Subscripts and substring bounds of the variable that may read it are
 * stored like the right-hand side; a vector subscript is stored with its
 * size, with the largest size as the leading dimension.
 */

// Whether `x` and `y` are the same designator with structurally identical
// subscripts. Returns false for anything it does not recognize.
static bool same_designator(ASR::expr_t *x, ASR::expr_t *y) {
    if (!x || !y) return x == y;
    if (x->type != y->type) return false;
    auto same_indices = [](ASR::array_index_t *a, size_t n_a,
            ASR::array_index_t *b, size_t n_b) {
        if (n_a != n_b) return false;
        for (size_t i = 0; i < n_a; i++) {
            if (!same_designator(a[i].m_left, b[i].m_left)
                    || !same_designator(a[i].m_right, b[i].m_right)
                    || !same_designator(a[i].m_step, b[i].m_step)) {
                return false;
            }
        }
        return true;
    };
    switch (x->type) {
        case ASR::exprType::Var:
            return ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(x)->m_v)
                == ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(y)->m_v);
        case ASR::exprType::IntegerConstant:
            return ASR::down_cast<ASR::IntegerConstant_t>(x)->m_n
                == ASR::down_cast<ASR::IntegerConstant_t>(y)->m_n;
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t *a = ASR::down_cast<ASR::IntegerBinOp_t>(x);
            ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(y);
            return a->m_op == b->m_op && same_designator(a->m_left, b->m_left)
                && same_designator(a->m_right, b->m_right);
        }
        case ASR::exprType::IntegerUnaryMinus:
            return same_designator(ASR::down_cast<ASR::IntegerUnaryMinus_t>(x)->m_arg,
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(y)->m_arg);
        case ASR::exprType::Cast: {
            ASR::Cast_t *a = ASR::down_cast<ASR::Cast_t>(x);
            ASR::Cast_t *b = ASR::down_cast<ASR::Cast_t>(y);
            return a->m_kind == b->m_kind && ASRUtils::types_equal(a->m_type, b->m_type,
                nullptr, nullptr) && same_designator(a->m_arg, b->m_arg);
        }
        case ASR::exprType::ArrayItem: {
            ASR::ArrayItem_t *a = ASR::down_cast<ASR::ArrayItem_t>(x);
            ASR::ArrayItem_t *b = ASR::down_cast<ASR::ArrayItem_t>(y);
            return same_indices(a->m_args, a->n_args, b->m_args, b->n_args)
                && same_designator(a->m_v, b->m_v);
        }
        case ASR::exprType::ArraySection: {
            ASR::ArraySection_t *a = ASR::down_cast<ASR::ArraySection_t>(x);
            ASR::ArraySection_t *b = ASR::down_cast<ASR::ArraySection_t>(y);
            return same_indices(a->m_args, a->n_args, b->m_args, b->n_args)
                && same_designator(a->m_v, b->m_v);
        }
        case ASR::exprType::StructInstanceMember: {
            ASR::StructInstanceMember_t *a = ASR::down_cast<ASR::StructInstanceMember_t>(x);
            ASR::StructInstanceMember_t *b = ASR::down_cast<ASR::StructInstanceMember_t>(y);
            return ASRUtils::symbol_get_past_external(a->m_m)
                == ASRUtils::symbol_get_past_external(b->m_m)
                && same_designator(a->m_v, b->m_v);
        }
        default:
            return false;
    }
}

// The variable that designator `x` is a part of, and the components
// selected on the way from it to `x`, from the variable outward.
static ASR::symbol_t* component_path(ASR::expr_t *x, std::vector<ASR::symbol_t*> &components) {
    components.clear();
    while (true) {
        switch (x->type) {
            case ASR::exprType::Var:
                std::reverse(components.begin(), components.end());
                return ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(x)->m_v);
            case ASR::exprType::ArrayItem:
                x = ASR::down_cast<ASR::ArrayItem_t>(x)->m_v;
                break;
            case ASR::exprType::ArraySection:
                x = ASR::down_cast<ASR::ArraySection_t>(x)->m_v;
                break;
            case ASR::exprType::StructInstanceMember: {
                ASR::StructInstanceMember_t *m = ASR::down_cast<ASR::StructInstanceMember_t>(x);
                components.push_back(ASRUtils::symbol_get_past_external(m->m_m));
                x = m->m_v;
                break;
            }
            default:
                return nullptr;
        }
    }
}

// Collects what an expression reads that the assignment could overwrite.
// Occurrences of `own_part`, the part of the variable that the same
// iteration assigns, are skipped, and so are parts of the variable in
// another component than the ones `own_components` assigns.
class ForAllReferenceVisitor : public ASR::BaseWalkVisitor<ForAllReferenceVisitor>
{
public:
    ASR::expr_t *own_part = nullptr;
    ASR::symbol_t *own_base = nullptr;
    std::vector<ASR::symbol_t*> own_components;
    std::set<ASR::symbol_t*> symbols;
    std::vector<ASR::symbol_t*> callees;
    bool has_function_call = false;
    bool has_procedure_argument = false;
    bool has_pointer = false;
    bool has_target = false;

    bool is_own_part(const ASR::expr_t &x) {
        return own_part && same_designator(const_cast<ASR::expr_t*>(&x), own_part);
    }

    void visit_Var(const ASR::Var_t &x) {
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x.m_v);
        symbols.insert(sym);
        if (sym && ASR::is_a<ASR::Variable_t>(*sym)) {
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
            has_pointer = has_pointer || ASRUtils::is_pointer(v->m_type);
            has_target = has_target || v->m_target_attr;
        } else if (sym && ASR::is_a<ASR::Function_t>(*sym)) {
            has_procedure_argument = true;
        }
    }

    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        if (is_own_part(x.base)) return;
        ASR::BaseWalkVisitor<ForAllReferenceVisitor>::visit_ArrayItem(x);
    }

    void visit_ArraySection(const ASR::ArraySection_t &x) {
        if (is_own_part(x.base)) return;
        ASR::BaseWalkVisitor<ForAllReferenceVisitor>::visit_ArraySection(x);
    }

    // Whether `x` is a part of `own_base` in another component than the
    // assigned one, so that it cannot overlap any assigned part.
    bool is_other_component(const ASR::expr_t &x) {
        if (!own_base) return false;
        std::vector<ASR::symbol_t*> components;
        if (component_path(const_cast<ASR::expr_t*>(&x), components) != own_base) return false;
        for (size_t d = 0; d < components.size() && d < own_components.size(); d++) {
            if (components[d] != own_components[d]) return true;
        }
        return false;
    }

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        if (is_own_part(x.base)) return;
        has_pointer = has_pointer || ASRUtils::is_pointer(
            ASRUtils::symbol_type(x.m_m));
        if (is_other_component(x.base)) {
            // Only the subscripts on the way are read from the variable.
            ASR::expr_t *e = x.m_v;
            while (true) {
                if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
                    ASR::StructInstanceMember_t *m = ASR::down_cast<ASR::StructInstanceMember_t>(e);
                    has_pointer = has_pointer || ASRUtils::is_pointer(
                        ASRUtils::symbol_type(m->m_m));
                    e = m->m_v;
                } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
                    ASR::ArrayItem_t *a = ASR::down_cast<ASR::ArrayItem_t>(e);
                    for (size_t i = 0; i < a->n_args; i++) visit_array_index(a->m_args[i]);
                    e = a->m_v;
                } else if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                    ASR::ArraySection_t *a = ASR::down_cast<ASR::ArraySection_t>(e);
                    for (size_t i = 0; i < a->n_args; i++) visit_array_index(a->m_args[i]);
                    e = a->m_v;
                } else {
                    break;
                }
            }
            return;
        }
        ASR::BaseWalkVisitor<ForAllReferenceVisitor>::visit_StructInstanceMember(x);
    }

    // Bounds and sizes of a variable do not read its elements.
    void visit_ArrayBound(const ASR::ArrayBound_t &x) {
        if (!ASR::is_a<ASR::Var_t>(*x.m_v)) visit_expr(*x.m_v);
        if (x.m_dim) visit_expr(*x.m_dim);
    }

    void visit_ArraySize(const ASR::ArraySize_t &x) {
        if (!ASR::is_a<ASR::Var_t>(*x.m_v)) visit_expr(*x.m_v);
        if (x.m_dim) visit_expr(*x.m_dim);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        has_function_call = true;
        callees.push_back(x.m_name);
        ASR::BaseWalkVisitor<ForAllReferenceVisitor>::visit_FunctionCall(x);
    }
};

// Whether a called procedure may access `base`, a variable of `scope`, by
// host association. Only a procedure contained in `scope` can; it does if
// it, a procedure it contains, or a procedure contained in `scope` that it
// calls or passes on references `base` or a pointer of `scope`.
class HostAccessVisitor : public ASR::BaseWalkVisitor<HostAccessVisitor>
{
public:
    ASR::symbol_t *base;
    SymbolTable *scope;
    std::set<ASR::symbol_t*> visited;
    bool found = false;

    HostAccessVisitor(ASR::symbol_t *base, SymbolTable *scope)
        : base(base), scope(scope) {}

    void visit_procedure(ASR::symbol_t *proc) {
        proc = ASRUtils::symbol_get_past_external(proc);
        if (!proc || !ASR::is_a<ASR::Function_t>(*proc)) {
            found = true;
            return;
        }
        if (found || visited.count(proc)) return;
        visited.insert(proc);
        ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(proc);
        for (SymbolTable *s = f->m_symtab->parent; s; s = s->parent) {
            if (s == scope) {
                // The procedure that an interface declares is not known.
                if (ASRUtils::get_FunctionType(f)->m_deftype
                        == ASR::deftypeType::Interface) {
                    found = true;
                } else {
                    visit_Function(*f);
                }
                return;
            }
        }
    }

    void visit_Var(const ASR::Var_t &x) {
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x.m_v);
        if (sym == base) {
            found = true;
        } else if (sym && ASR::is_a<ASR::Function_t>(*sym)) {
            visit_procedure(sym);
        } else if (sym && ASR::is_a<ASR::Variable_t>(*sym)) {
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(sym);
            if (ASR::is_a<ASR::FunctionType_t>(*ASRUtils::type_get_past_pointer(v->m_type))
                    || (v->m_parent_symtab == scope && ASRUtils::is_pointer(v->m_type))) {
                found = true;
            }
        }
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        visit_procedure(x.m_name);
        ASR::BaseWalkVisitor<HostAccessVisitor>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        visit_procedure(x.m_name);
        ASR::BaseWalkVisitor<HostAccessVisitor>::visit_SubroutineCall(x);
    }
};

class ForAllVisitor : public ASR::StatementWalkVisitor<ForAllVisitor>
{
public:
    const LCompilers::PassOptions& pass_options;

    ForAllVisitor(Allocator &al, const LCompilers::PassOptions& pass_options)
        : StatementWalkVisitor(al), pass_options(pass_options) {
    }

    // The variable whose elements an assignment target designates, and
    // whether a pointer is followed to reach them.
    static ASR::symbol_t* target_base(ASR::expr_t *target, bool &through_pointer) {
        while (true) {
            switch (target->type) {
                case ASR::exprType::Var: {
                    ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                        ASR::down_cast<ASR::Var_t>(target)->m_v);
                    if (!sym || !ASR::is_a<ASR::Variable_t>(*sym)) return nullptr;
                    through_pointer = through_pointer ||
                        ASRUtils::is_pointer(ASRUtils::symbol_type(sym));
                    return sym;
                }
                case ASR::exprType::ArrayItem:
                    target = ASR::down_cast<ASR::ArrayItem_t>(target)->m_v;
                    break;
                case ASR::exprType::ArraySection:
                    target = ASR::down_cast<ASR::ArraySection_t>(target)->m_v;
                    break;
                case ASR::exprType::StringItem:
                    target = ASR::down_cast<ASR::StringItem_t>(target)->m_arg;
                    break;
                case ASR::exprType::StringSection:
                    target = ASR::down_cast<ASR::StringSection_t>(target)->m_arg;
                    break;
                case ASR::exprType::StructInstanceMember: {
                    ASR::StructInstanceMember_t *m =
                        ASR::down_cast<ASR::StructInstanceMember_t>(target);
                    through_pointer = through_pointer ||
                        ASRUtils::is_pointer(ASRUtils::symbol_type(m->m_m));
                    target = m->m_v;
                    break;
                }
                default:
                    return nullptr;
            }
        }
    }

    // Element types that a temporary array can hold.
    static bool is_supported_element_type(ASR::ttype_t *type) {
        switch (type->type) {
            case ASR::ttypeType::Integer:
            case ASR::ttypeType::UnsignedInteger:
            case ASR::ttypeType::Real:
            case ASR::ttypeType::Complex:
            case ASR::ttypeType::Logical:
            case ASR::ttypeType::String:
                return true;
            case ASR::ttypeType::StructType:
                return !ASRUtils::is_class_type(type);
            default:
                return false;
        }
    }

    static bool has_constant_length(ASR::ttype_t *type) {
        ASR::String_t *s = ASR::down_cast<ASR::String_t>(type);
        return s->m_len_kind == ASR::string_length_kindType::ExpressionLength
            && s->m_len && ASRUtils::is_value_constant(s->m_len);
    }

    // The subscript and substring bound expressions in `target`.
    static void collect_subscript_slots(ASR::expr_t *target,
            std::vector<ASR::expr_t**> &slots) {
        auto add = [&](ASR::array_index_t *args, size_t n) {
            for (size_t i = 0; i < n; i++) {
                if (args[i].m_left) slots.push_back(&args[i].m_left);
                if (args[i].m_right) slots.push_back(&args[i].m_right);
                if (args[i].m_step) slots.push_back(&args[i].m_step);
            }
        };
        while (true) {
            switch (target->type) {
                case ASR::exprType::ArrayItem: {
                    ASR::ArrayItem_t *a = ASR::down_cast<ASR::ArrayItem_t>(target);
                    add(a->m_args, a->n_args);
                    target = a->m_v;
                    break;
                }
                case ASR::exprType::ArraySection: {
                    ASR::ArraySection_t *a = ASR::down_cast<ASR::ArraySection_t>(target);
                    add(a->m_args, a->n_args);
                    target = a->m_v;
                    break;
                }
                case ASR::exprType::StructInstanceMember:
                    target = ASR::down_cast<ASR::StructInstanceMember_t>(target)->m_v;
                    break;
                case ASR::exprType::StringItem: {
                    ASR::StringItem_t *c = ASR::down_cast<ASR::StringItem_t>(target);
                    slots.push_back(&c->m_idx);
                    target = c->m_arg;
                    break;
                }
                case ASR::exprType::StringSection: {
                    ASR::StringSection_t *c = ASR::down_cast<ASR::StringSection_t>(target);
                    if (c->m_start) slots.push_back(&c->m_start);
                    if (c->m_end) slots.push_back(&c->m_end);
                    if (c->m_step) slots.push_back(&c->m_step);
                    target = c->m_arg;
                    break;
                }
                default:
                    return;
            }
        }
    }

    static bool references_any(ASR::expr_t *expr, const std::set<ASR::symbol_t*> &syms) {
        if (!expr) return false;
        ForAllReferenceVisitor v;
        v.visit_expr(*expr);
        for (ASR::symbol_t *s : v.symbols) {
            if (syms.count(s)) return true;
        }
        return false;
    }

    // Whether a procedure called in the FORALL could access `base` other
    // than through its arguments. A pure function can only reach a local
    // or dummy variable of a procedure or main program by host
    // association, or through a pointer, a target, a common block or a
    // procedure argument.
    static bool calls_may_access(ASR::Variable_t *base, bool through_pointer,
            const ForAllReferenceVisitor &v) {
        if (!v.has_function_call) return false;
        if (through_pointer || base->m_target_attr || base->m_common_block
                || v.has_procedure_argument) {
            return true;
        }
        SymbolTable *scope = base->m_parent_symtab;
        if (!scope || !scope->asr_owner || !ASR::is_a<ASR::symbol_t>(*scope->asr_owner)) {
            return true;
        }
        ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(scope->asr_owner);
        if (!ASR::is_a<ASR::Function_t>(*owner) && !ASR::is_a<ASR::Program_t>(*owner)
                && !ASR::is_a<ASR::Block_t>(*owner)) {
            return true;
        }
        HostAccessVisitor h(&base->base, scope);
        for (ASR::symbol_t *callee : v.callees) {
            h.visit_procedure(callee);
            if (h.found) return true;
        }
        return false;
    }

    // Whether the reads collected in `v` may observe an assignment to
    // `base` in another iteration. Pointers also implement ASSOCIATE names
    // and EQUIVALENCE, which alias variables without the TARGET attribute,
    // so any pointer read, or any variable read through a pointer, may
    // alias the variable. So may another member of a common block.
    static bool reads_may_observe(ASR::Variable_t *base, bool through_pointer,
            const ForAllReferenceVisitor &v, const std::set<ASR::symbol_t*> &index_syms) {
        if (v.symbols.count(&base->base)) return true;
        if (v.has_pointer) return true;
        for (ASR::symbol_t *sym : v.symbols) {
            if (!sym || !ASR::is_a<ASR::Variable_t>(*sym) || index_syms.count(sym)) continue;
            ASR::Variable_t *read = ASR::down_cast<ASR::Variable_t>(sym);
            if (read->m_storage == ASR::storage_typeType::Parameter) continue;
            if (through_pointer || (base->m_common_block && read->m_common_block)) return true;
        }
        return calls_may_access(base, through_pointer, v);
    }

    // Whether evaluating `exprs` after some elements of `target` have been
    // assigned in other iterations could observe the new values. Each
    // iteration assigns a different part of the variable, so reading
    // exactly the part `target` that the same iteration assigns is safe,
    // provided its subscripts do not depend on the variable.
    static bool may_read_target(ASR::expr_t *target, const std::vector<ASR::expr_t*> &exprs,
            const std::set<ASR::symbol_t*> &index_syms) {
        bool through_pointer = false;
        ASR::symbol_t *base_sym = target_base(target, through_pointer);
        if (!base_sym) return true;
        ASR::Variable_t *base = ASR::down_cast<ASR::Variable_t>(base_sym);
        std::vector<ASR::expr_t**> subscripts;
        collect_subscript_slots(target, subscripts);
        bool own_part_is_fixed = true;
        for (ASR::expr_t **slot : subscripts) {
            ForAllReferenceVisitor s;
            s.visit_expr(**slot);
            if (s.has_function_call || s.has_pointer || s.symbols.count(base_sym)
                    || ASRUtils::is_array(ASRUtils::expr_type(*slot))) {
                own_part_is_fixed = false;
            }
        }
        ForAllReferenceVisitor v;
        if (own_part_is_fixed) v.own_part = target;
        if (!through_pointer) v.own_base = component_path(target, v.own_components);
        for (ASR::expr_t *e : exprs) {
            v.visit_expr(*e);
        }
        return reads_may_observe(base, through_pointer, v, index_syms);
    }

    // Lowers a single FORALL assignment whose right-hand side, mask or
    // subscripts may read the assigned variable. Returns false if this
    // lowering does not apply; the FORALL is then lowered to a single
    // DO CONCURRENT loop.
    bool lower_with_temporary(const ASR::ForAllSingle_t &x) {
        // The index heads and masks, from the outermost to the innermost.
        std::vector<std::pair<ASR::do_loop_head_t*, ASR::expr_t*>> levels;
        ASR::stmt_t *inner = &const_cast<ASR::ForAllSingle_t&>(x).base;
        while (true) {
            if (ASR::is_a<ASR::ForAllSingle_t>(*inner)) {
                ASR::ForAllSingle_t *f = ASR::down_cast<ASR::ForAllSingle_t>(inner);
                if (!f->m_head.m_v || !ASR::is_a<ASR::Var_t>(*f->m_head.m_v)) return false;
                levels.push_back({&f->m_head, nullptr});
                inner = f->m_assign_stmt;
            } else if (ASR::is_a<ASR::If_t>(*inner)) {
                ASR::If_t *i = ASR::down_cast<ASR::If_t>(inner);
                if (i->n_body != 1 || i->n_orelse != 0) return false;
                levels.push_back({nullptr, i->m_test});
                inner = i->m_body[0];
            } else {
                break;
            }
        }
        if (!ASR::is_a<ASR::Assignment_t>(*inner)) return false;
        ASR::Assignment_t *asgn = ASR::down_cast<ASR::Assignment_t>(inner);
        ASR::ttype_t *target_type = ASRUtils::expr_type(asgn->m_target);
        size_t rank = ASRUtils::extract_n_dims_from_ttype(target_type);
        ASR::SubroutineCall_t *overloaded = nullptr;
        ASR::ttype_t *elem_type;
        if (asgn->m_overloaded) {
            if (rank > 0 || !ASR::is_a<ASR::SubroutineCall_t>(*asgn->m_overloaded)) return false;
            overloaded = ASR::down_cast<ASR::SubroutineCall_t>(asgn->m_overloaded);
            if (overloaded->n_args != 2) return false;
            elem_type = ASRUtils::expr_type(asgn->m_value);
            if (ASRUtils::is_array(elem_type)) return false;
        } else {
            elem_type = target_type;
        }
        elem_type = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable_pointer(elem_type));
        if (!is_supported_element_type(elem_type)) return false;

        std::set<ASR::symbol_t*> index_syms;
        int index_kind = 4;
        for (auto &level : levels) {
            if (!level.first) continue;
            index_syms.insert(ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(level.first->m_v)->m_v));
            index_kind = std::max(index_kind,
                ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(level.first->m_v)));
        }

        // A string temporary has the length of the elements of the array
        // that is assigned, which must not depend on the indices. A value
        // assigned to a substring is padded or truncated when it is
        // assigned, so it is stored with its own length.
        ASR::expr_t *string_length = nullptr;
        ASR::ttype_t *value_type = ASRUtils::type_get_past_allocatable_pointer(
            ASRUtils::expr_type(asgn->m_value));
        if (ASR::is_a<ASR::String_t>(*elem_type) && !has_constant_length(elem_type)
                && ASR::is_a<ASR::StringSection_t>(*asgn->m_target) && rank == 0 && !overloaded
                && ASR::is_a<ASR::String_t>(*value_type) && has_constant_length(value_type)) {
            elem_type = value_type;
        } else if (ASR::is_a<ASR::String_t>(*elem_type) && !has_constant_length(elem_type)) {
            if (overloaded || !ASR::is_a<ASR::ArrayItem_t>(*asgn->m_target)) return false;
            ASR::expr_t *array = ASR::down_cast<ASR::ArrayItem_t>(asgn->m_target)->m_v;
            if (references_any(array, index_syms)) return false;
            ASRUtils::ExprStmtDuplicator dup(al);
            string_length = ASRUtils::EXPR(ASR::make_StringLen_t(al, array->base.loc,
                dup.duplicate_expr(array), ASRUtils::TYPE(ASR::make_Integer_t(al,
                    array->base.loc, 4)), nullptr));
        }

        std::vector<ASR::expr_t**> subscripts;
        collect_subscript_slots(asgn->m_target, subscripts);
        std::vector<ASR::expr_t*> reads;
        for (auto &level : levels) {
            if (level.second) reads.push_back(level.second);
        }
        for (ASR::expr_t **slot : subscripts) reads.push_back(*slot);
        reads.push_back(asgn->m_value);
        if (!may_read_target(asgn->m_target, reads, index_syms)) return false;
        // A scalar assigned to an array-valued variable is stored once.
        ASR::expr_t *value = asgn->m_value;
        ASR::ArrayBroadcast_t *broadcast = nullptr;
        if (ASR::is_a<ASR::ArrayBroadcast_t>(*value) && !ASRUtils::is_array(
                ASRUtils::expr_type(ASR::down_cast<ASR::ArrayBroadcast_t>(value)->m_array))) {
            broadcast = ASR::down_cast<ASR::ArrayBroadcast_t>(value);
            value = broadcast->m_array;
        }
        size_t stored_rank = ASRUtils::is_array(ASRUtils::expr_type(value)) ? rank : 0;
        // A reallocated variable takes the shape of the value, which is
        // stored for every index value.
        bool value_shape = stored_rank > 0 && asgn->m_realloc_lhs
            && ASRUtils::is_allocatable(target_type);

        Location loc = x.base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASRUtils::ExprStmtDuplicator dup(al);
        ASR::ttype_t *int_type = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, index_kind));
        auto new_variable = [&](const std::string &hint, ASR::ttype_t *type,
                ASR::expr_t *type_source) -> ASR::symbol_t* {
            std::string name = current_scope->get_unique_name("__libasr_forall_" + hint);
            ASR::expr_t *v = PassUtils::create_auxiliary_variable(loc, name, al,
                current_scope, type, ASR::intentType::Local, nullptr, type_source);
            return ASR::down_cast<ASR::Var_t>(v)->m_v;
        };
        auto allocatable_array = [&](ASR::ttype_t *type, size_t n_dims) -> ASR::ttype_t* {
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, n_dims);
            for (size_t d = 0; d < n_dims; d++) {
                dims.push_back(al, b.set_dim(nullptr, nullptr));
            }
            return b.allocatable(ASRUtils::make_Array_t_util(al, loc,
                ASRUtils::duplicate_type(al, type), dims.p, dims.size()));
        };
        auto element = [&](ASR::symbol_t *array, ASR::symbol_t *position) -> ASR::expr_t* {
            Vec<ASR::array_index_t> args;
            args.reserve(al, 1);
            ASR::array_index_t ai;
            ai.loc = loc;
            ai.m_left = nullptr;
            ai.m_right = b.Var(position);
            ai.m_step = nullptr;
            args.push_back(al, ai);
            ASR::ttype_t *type = ASRUtils::duplicate_type(al,
                ASRUtils::type_get_past_array(ASRUtils::type_get_past_allocatable(
                    ASRUtils::symbol_type(array))));
            return ASRUtils::EXPR(ASRUtils::make_ArrayItem_t_util(al, loc, b.Var(array),
                args.p, args.size(), type, ASR::arraystorageType::ColMajor, nullptr));
        };
        auto extent = [&](ASR::expr_t *target, size_t d) -> ASR::expr_t* {
            return b.ArraySize(dup.duplicate_expr(target), b.i32(d + 1), int_type);
        };
        std::vector<ASR::symbol_t*> shapes;
        // The value stored for `position`: `rhs(position)`, or
        // `rhs(1:size(target,1), ..., position)` for an array `target`
        // (`rhs(1:shape_1(position), ..., position)` if reallocated).
        auto stored = [&](ASR::symbol_t *rhs, ASR::symbol_t *position,
                ASR::expr_t *target) -> ASR::expr_t* {
            if (stored_rank == 0) return element(rhs, position);
            Vec<ASR::array_index_t> args;
            args.reserve(al, stored_rank + 1);
            for (size_t d = 0; d < stored_rank; d++) {
                ASR::array_index_t ai;
                ai.loc = loc;
                ai.m_left = b.i_t(1, int_type);
                ai.m_right = value_shape ? element(shapes[d], position) : extent(target, d);
                ai.m_step = b.i_t(1, int_type);
                args.push_back(al, ai);
            }
            ASR::array_index_t ai;
            ai.loc = loc;
            ai.m_left = nullptr;
            ai.m_right = b.Var(position);
            ai.m_step = nullptr;
            args.push_back(al, ai);
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, stored_rank);
            for (size_t d = 0; d < stored_rank; d++) {
                dims.push_back(al, b.set_dim(nullptr, nullptr));
            }
            ASR::ttype_t *type = ASRUtils::make_Array_t_util(al, loc,
                ASRUtils::duplicate_type(al, elem_type), dims.p, dims.size());
            return ASRUtils::EXPR(ASR::make_ArraySection_t(al, loc, b.Var(rhs),
                args.p, args.size(), type, nullptr));
        };
        // The index loops and masks of the FORALL around `body`.
        auto index_loops = [&](std::vector<ASR::stmt_t*> body) -> ASR::stmt_t* {
            for (size_t l = levels.size(); l > 0; l--) {
                ASR::do_loop_head_t *head = levels[l - 1].first;
                ASR::stmt_t *s;
                if (head) {
                    s = b.DoLoop(dup.duplicate_expr(head->m_v),
                        dup.duplicate_expr(head->m_start), dup.duplicate_expr(head->m_end),
                        body, head->m_increment ? dup.duplicate_expr(head->m_increment) : nullptr);
                } else {
                    s = b.If(dup.duplicate_expr(levels[l - 1].second), body, {});
                }
                body = {s};
            }
            return body[0];
        };
        auto increment = [&](ASR::symbol_t *counter) {
            return b.Assignment(b.Var(counter), b.Add(b.Var(counter), b.i_t(1, int_type)));
        };

        Vec<ASR::stmt_t*> result;
        result.reserve(al, 8);
        Vec<ASR::expr_t*> deallocate_args;
        deallocate_args.reserve(al, 4);

        // The subscripts and substring bounds of the variable that may read
        // it, which are stored.
        bool through_pointer = false;
        ASR::symbol_t *base_sym = target_base(asgn->m_target, through_pointer);
        ASR::Variable_t *base = base_sym ? ASR::down_cast<ASR::Variable_t>(base_sym) : nullptr;
        std::vector<bool> stored_subscripts;
        for (ASR::expr_t **slot : subscripts) {
            ForAllReferenceVisitor v;
            v.visit_expr(**slot);
            stored_subscripts.push_back(!base || reads_may_observe(base, through_pointer, v,
                index_syms));
        }

        // Count the active index values, and find the largest extents of an
        // array-valued variable and of the stored vector subscripts.
        ASR::symbol_t *count = new_variable("count", int_type, nullptr);
        std::vector<ASR::stmt_t*> count_body = {increment(count)};
        result.push_back(al, b.Assignment(b.Var(count), b.i_t(0, int_type)));
        auto largest_extent = [&](ASR::expr_t *array, size_t d) -> ASR::symbol_t* {
            ASR::symbol_t *e = new_variable("extent", int_type, nullptr);
            result.push_back(al, b.Assignment(b.Var(e), b.i_t(0, int_type)));
            count_body.push_back(b.If(b.Gt(extent(array, d), b.Var(e)),
                {b.Assignment(b.Var(e), extent(array, d))}, {}));
            return e;
        };
        std::vector<ASR::symbol_t*> extents;
        for (size_t d = 0; d < stored_rank; d++) {
            extents.push_back(largest_extent(value_shape ? value : asgn->m_target, d));
        }
        std::vector<ASR::symbol_t*> subscript_extents(subscripts.size(), nullptr);
        for (size_t j = 0; j < subscripts.size(); j++) {
            if (stored_subscripts[j] && ASRUtils::is_array(ASRUtils::expr_type(*subscripts[j]))) {
                subscript_extents[j] = largest_extent(*subscripts[j], 0);
            }
        }
        result.push_back(al, index_loops(count_body));

        auto allocate = [&](ASR::symbol_t *array, const std::vector<ASR::symbol_t*> &leading,
                ASR::expr_t *len_expr) {
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, leading.size() + 1);
            for (ASR::symbol_t *e : leading) {
                dims.push_back(al, b.set_dim(b.i_t(1, int_type), b.Var(e)));
            }
            dims.push_back(al, b.set_dim(b.i_t(1, int_type), b.Var(count)));
            result.push_back(al, b.Allocate(b.Var(array), dims.p, dims.size(), len_expr));
            deallocate_args.push_back(al, b.Var(array));
        };
        ASR::ttype_t *rhs_elem_type = elem_type;
        if (string_length) {
            ASR::String_t *s = ASR::down_cast<ASR::String_t>(elem_type);
            rhs_elem_type = ASRUtils::TYPE(ASR::make_String_t(al, loc, s->m_kind,
                nullptr, ASR::string_length_kindType::DeferredLength, s->m_physical_type));
        }
        ASR::symbol_t *rhs = new_variable("rhs", allocatable_array(rhs_elem_type, stored_rank + 1),
            overloaded ? asgn->m_value : asgn->m_target);
        allocate(rhs, extents, string_length);

        // Evaluate everything the assignments need, in index order.
        ASR::symbol_t *position = new_variable("position", int_type, nullptr);
        std::vector<ASR::stmt_t*> evaluate_body = {increment(position)};
        std::vector<std::pair<ASR::expr_t*, ASR::symbol_t*>> indices;
        for (auto &level : levels) {
            if (!level.first) continue;
            ASR::expr_t *v = level.first->m_v;
            ASR::symbol_t *idx = new_variable("index",
                allocatable_array(ASRUtils::expr_type(v), 1), nullptr);
            allocate(idx, {}, nullptr);
            evaluate_body.push_back(b.Assignment(element(idx, position), dup.duplicate_expr(v)));
            indices.push_back({v, idx});
        }
        for (size_t d = 0; value_shape && d < stored_rank; d++) {
            ASR::symbol_t *shape = new_variable("shape", allocatable_array(int_type, 1), nullptr);
            allocate(shape, {}, nullptr);
            evaluate_body.push_back(b.Assignment(element(shape, position), extent(value, d)));
            shapes.push_back(shape);
        }
        ASR::expr_t *target = dup.duplicate_expr(asgn->m_target);
        std::vector<ASR::expr_t**> target_subscripts;
        collect_subscript_slots(target, target_subscripts);
        for (size_t j = 0; j < target_subscripts.size(); j++) {
            if (!stored_subscripts[j]) continue;
            ASR::expr_t **slot = target_subscripts[j];
            ASR::ttype_t *slot_type = ASRUtils::expr_type(*slot);
            if (!subscript_extents[j]) {
                ASR::symbol_t *sub = new_variable("subscript",
                    allocatable_array(slot_type, 1), nullptr);
                allocate(sub, {}, nullptr);
                evaluate_body.push_back(b.Assignment(element(sub, position), *slot));
                *slot = element(sub, position);
                continue;
            }
            // `sub(1:size(position), position)` for a vector subscript.
            ASR::symbol_t *size = new_variable("size", allocatable_array(int_type, 1), nullptr);
            allocate(size, {}, nullptr);
            ASR::symbol_t *sub = new_variable("subscript", allocatable_array(
                ASRUtils::type_get_past_array(slot_type), 2), nullptr);
            allocate(sub, {subscript_extents[j]}, nullptr);
            auto stored_subscript = [&]() -> ASR::expr_t* {
                Vec<ASR::array_index_t> args;
                args.reserve(al, 2);
                ASR::array_index_t ai;
                ai.loc = loc;
                ai.m_left = b.i_t(1, int_type);
                ai.m_right = element(size, position);
                ai.m_step = b.i_t(1, int_type);
                args.push_back(al, ai);
                ai.m_left = nullptr;
                ai.m_right = b.Var(position);
                ai.m_step = nullptr;
                args.push_back(al, ai);
                Vec<ASR::dimension_t> dims;
                dims.reserve(al, 1);
                dims.push_back(al, b.set_dim(nullptr, nullptr));
                ASR::ttype_t *type = ASRUtils::make_Array_t_util(al, loc, ASRUtils::duplicate_type(
                    al, ASRUtils::type_get_past_array(slot_type)), dims.p, dims.size());
                return ASRUtils::EXPR(ASR::make_ArraySection_t(al, loc, b.Var(sub),
                    args.p, args.size(), type, nullptr));
            };
            evaluate_body.push_back(b.Assignment(element(size, position), extent(*slot, 0)));
            evaluate_body.push_back(ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, loc,
                stored_subscript(), *slot, nullptr, false, false)));
            *slot = stored_subscript();
        }
        evaluate_body.push_back(ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al, loc,
            stored(rhs, position, asgn->m_target), value, nullptr, false, false)));
        result.push_back(al, b.Assignment(b.Var(position), b.i_t(0, int_type)));
        result.push_back(al, index_loops(evaluate_body));

        // Assign, for each stored index value.
        std::vector<ASR::stmt_t*> assign_body;
        for (auto &index : indices) {
            assign_body.push_back(b.Assignment(dup.duplicate_expr(index.first),
                element(index.second, position)));
        }
        ASR::expr_t *stored_value = stored(rhs, position, target);
        if (broadcast) {
            stored_value = ASRUtils::EXPR(ASR::make_ArrayBroadcast_t(al, loc, stored_value,
                dup.duplicate_expr(broadcast->m_shape),
                ASRUtils::duplicate_type(al, broadcast->m_type), nullptr));
        }
        ASR::stmt_t *call = nullptr;
        if (overloaded) {
            Vec<ASR::call_arg_t> args;
            args.reserve(al, 2);
            ASR::call_arg_t arg;
            arg.loc = overloaded->m_args[0].loc;
            arg.m_value = dup.duplicate_expr(target);
            args.push_back(al, arg);
            arg.loc = overloaded->m_args[1].loc;
            arg.m_value = dup.duplicate_expr(stored_value);
            args.push_back(al, arg);
            call = ASRUtils::STMT(ASR::make_SubroutineCall_t(al, overloaded->base.base.loc,
                overloaded->m_name, overloaded->m_original_name, args.p, args.size(),
                overloaded->m_dt, overloaded->m_strict_bounds_checking));
        }
        assign_body.push_back(ASRUtils::STMT(ASRUtils::make_Assignment_t_util(al,
            asgn->base.base.loc, target, stored_value, call, asgn->m_realloc_lhs, false)));
        result.push_back(al, b.DoLoop(b.Var(position), b.i_t(1, int_type), b.Var(count),
            assign_body));
        result.push_back(al, b.Deallocate(deallocate_args));
        pass_result = result;
        return true;
    }

    void visit_ForAllSingle(const ASR::ForAllSingle_t &x) {
        if (lower_with_temporary(x)) {
            replace_implied_do_loops_in_body(al, pass_result.p,
                pass_result.n, current_scope, pass_options);
            return;
        }
        Location loc = x.base.base.loc;
        Vec<ASR::do_loop_head_t> heads;
        heads.reserve(al, 4);
        heads.push_back(al, x.m_head);
        // Unwrap nested ForAllSingle nodes (from multi-index forall)
        ASR::stmt_t *inner = x.m_assign_stmt;
        while (ASR::is_a<ASR::ForAllSingle_t>(*inner)) {
            ASR::ForAllSingle_t &nested = *ASR::down_cast<ASR::ForAllSingle_t>(inner);
            heads.push_back(al, nested.m_head);
            inner = nested.m_assign_stmt;
        }
        Vec<ASR::stmt_t*> body;
        body.reserve(al, 1);
        body.push_back(al, inner);
        ASR::stmt_t *stmt = ASRUtils::STMT(
            ASR::make_DoConcurrentLoop_t(al, loc, heads.p, heads.n, nullptr, 0,
                nullptr, 0, nullptr, 0, body.p, body.size())
        );
        Vec<ASR::stmt_t*> result;
        result.reserve(al, 1);
        result.push_back(al, stmt);
        pass_result = result;
        // The indices are now bound inside the generated loops. Finish only
        // constructors deferred by the earlier pass, not the whole unit again.
        replace_implied_do_loops_in_body(al, pass_result.p,
            pass_result.n, current_scope, pass_options);
    }
};

void pass_replace_for_all(Allocator &al, ASR::TranslationUnit_t &unit,
                         const LCompilers::PassOptions& pass_options) {
    ForAllVisitor v(al, pass_options);
    v.visit_TranslationUnit(unit);
}

} // namespace LCompilers

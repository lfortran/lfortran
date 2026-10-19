#include <libasr/asr_utils.h>
#include <libasr/pass/gpu_affine.h>

#include <algorithm>
#include <numeric>
#include <set>

namespace LCompilers {

void GpuAffine::add(const GpuAffine &other, int64_t scale) {
    constant += other.constant * scale;
    for (auto &t : other.terms) {
        int64_t c = terms[t.first] + t.second * scale;
        if (c == 0) {
            terms.erase(t.first);
        } else {
            terms[t.first] = c;
        }
    }
}

std::string GpuAffine::str() const {
    std::string s;
    for (auto &t : terms) {
        s += std::to_string(t.second) + "*[" + t.first + "] + ";
    }
    return s + std::to_string(constant);
}

namespace {

bool is_integer_expr(ASR::expr_t *e) {
    return ASR::is_a<ASR::Integer_t>(*ASRUtils::type_get_past_allocatable(
        ASRUtils::expr_type(e)));
}

std::string ptr_key(const void *p) {
    return std::to_string(reinterpret_cast<uintptr_t>(p));
}

// The key of an array or derived-type designator a bound or size is taken
// of: a variable reached through components.
std::string designator_key(ASR::expr_t *e) {
    if (ASR::is_a<ASR::Var_t>(*e)) {
        return "v" + ptr_key(ASR::down_cast<ASR::Var_t>(e)->m_v);
    }
    if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
        ASR::StructInstanceMember_t *m =
            ASR::down_cast<ASR::StructInstanceMember_t>(e);
        std::string base = designator_key(m->m_v);
        if (base.empty()) return "";
        return base + "%" + ptr_key(ASRUtils::symbol_get_past_external(
            m->m_m));
    }
    return "";
}

} // namespace

std::string gpu_affine_key(ASR::expr_t *e) {
    switch (e->type) {
        case ASR::exprType::Var: {
            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v);
            return std::string("v") + ptr_key(s) + ":"
                + ASRUtils::symbol_name(s);
        }
        case ASR::exprType::IntegerConstant:
            return "c" + std::to_string(
                ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n);
        case ASR::exprType::StructInstanceMember: {
            std::string k = designator_key(e);
            return k.empty() ? "" : "m" + k;
        }
        case ASR::exprType::ArrayBound: {
            ASR::ArrayBound_t *b = ASR::down_cast<ASR::ArrayBound_t>(e);
            std::string v = designator_key(b->m_v);
            if (v.empty() || b->m_dim == nullptr) return "";
            std::string d = gpu_affine_key(b->m_dim);
            if (d.empty()) return "";
            return std::string(b->m_bound == ASR::arrayboundType::LBound
                ? "lbound(" : "ubound(") + v + "," + d + ")";
        }
        case ASR::exprType::ArraySize: {
            ASR::ArraySize_t *b = ASR::down_cast<ASR::ArraySize_t>(e);
            std::string v = designator_key(b->m_v);
            if (v.empty()) return "";
            std::string d = b->m_dim ? gpu_affine_key(b->m_dim) : "all";
            if (d.empty()) return "";
            return "size(" + v + "," + d + ")";
        }
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            std::string l = gpu_affine_key(b->m_left);
            std::string r = gpu_affine_key(b->m_right);
            if (l.empty() || r.empty()) return "";
            return "(" + l + " op" + std::to_string((int)b->m_op) + " "
                + r + ")";
        }
        case ASR::exprType::IntegerUnaryMinus: {
            std::string a = gpu_affine_key(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg);
            return a.empty() ? "" : "(-" + a + ")";
        }
        case ASR::exprType::Cast: {
            ASR::Cast_t *c = ASR::down_cast<ASR::Cast_t>(e);
            if (c->m_kind != ASR::cast_kindType::IntegerToInteger) return "";
            return gpu_affine_key(c->m_arg);
        }
        default:
            return "";
    }
}

bool GpuAffineBuilder::is_atom_expression(ASR::expr_t *e) {
    // Every variable an atom reads has to keep its value.
    switch (e->type) {
        case ASR::exprType::Var: {
            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v);
            if (ASR::is_a<ASR::Variable_t>(*s)
                    && ASR::down_cast<ASR::Variable_t>(s)->m_storage
                        == ASR::storage_typeType::Parameter) {
                return true;
            }
            return invariant_variable && invariant_variable(s);
        }
        case ASR::exprType::IntegerConstant:
            return true;
        case ASR::exprType::StructInstanceMember:
            return is_atom_expression(
                ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v);
        case ASR::exprType::ArrayBound: {
            ASR::ArrayBound_t *b = ASR::down_cast<ASR::ArrayBound_t>(e);
            // The bounds of an array do not change while it stays
            // allocated, which the analysed code checks separately.
            return b->m_dim && is_atom_expression(b->m_dim);
        }
        case ASR::exprType::ArraySize: {
            ASR::ArraySize_t *b = ASR::down_cast<ASR::ArraySize_t>(e);
            return b->m_dim == nullptr || is_atom_expression(b->m_dim);
        }
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            return is_atom_expression(b->m_left)
                && is_atom_expression(b->m_right);
        }
        case ASR::exprType::IntegerUnaryMinus:
            return is_atom_expression(
                ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg);
        case ASR::exprType::Cast:
            return is_atom_expression(ASR::down_cast<ASR::Cast_t>(e)->m_arg);
        default:
            return false;
    }
}

bool GpuAffineBuilder::affine(ASR::expr_t *e, GpuAffine &out) {
    if (!is_integer_expr(e)) return false;
    switch (e->type) {
        case ASR::exprType::IntegerConstant:
            out = GpuAffine(ASR::down_cast<ASR::IntegerConstant_t>(e)->m_n);
            return true;
        case ASR::exprType::Var: {
            ASR::symbol_t *s = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v);
            auto it = bindings.find(s);
            if (it != bindings.end()) {
                out = it->second;
                return true;
            }
            if (ASR::is_a<ASR::Variable_t>(*s)) {
                ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
                if (v->m_storage == ASR::storage_typeType::Parameter
                        && v->m_value
                        && ASR::is_a<ASR::IntegerConstant_t>(*v->m_value)) {
                    out = GpuAffine(ASR::down_cast<ASR::IntegerConstant_t>(
                        v->m_value)->m_n);
                    return true;
                }
            }
            break;
        }
        case ASR::exprType::IntegerBinOp: {
            ASR::IntegerBinOp_t *b = ASR::down_cast<ASR::IntegerBinOp_t>(e);
            GpuAffine l, r;
            bool lok = affine(b->m_left, l), rok = affine(b->m_right, r);
            if (lok && rok) {
                switch (b->m_op) {
                    case ASR::binopType::Add:
                        out = l.plus(r);
                        return true;
                    case ASR::binopType::Sub:
                        out = l.plus(r, -1);
                        return true;
                    case ASR::binopType::Mul:
                        if (l.is_constant()) {
                            out = r.scaled(l.constant);
                            return true;
                        }
                        if (r.is_constant()) {
                            out = l.scaled(r.constant);
                            return true;
                        }
                        break;
                    default:
                        break;
                }
            }
            break;
        }
        case ASR::exprType::IntegerUnaryMinus: {
            GpuAffine a;
            if (affine(ASR::down_cast<ASR::IntegerUnaryMinus_t>(e)->m_arg,
                    a)) {
                out = a.scaled(-1);
                return true;
            }
            break;
        }
        case ASR::exprType::Cast: {
            ASR::Cast_t *c = ASR::down_cast<ASR::Cast_t>(e);
            if (c->m_kind == ASR::cast_kindType::IntegerToInteger) {
                return affine(c->m_arg, out);
            }
            break;
        }
        default:
            break;
    }
    if (ASRUtils::expr_value(e)
            && ASR::is_a<ASR::IntegerConstant_t>(*ASRUtils::expr_value(e))) {
        out = GpuAffine(ASR::down_cast<ASR::IntegerConstant_t>(
            ASRUtils::expr_value(e))->m_n);
        return true;
    }
    // A non-affine expression of values that do not change is an unknown.
    if (is_atom_expression(e)) {
        std::string key = gpu_affine_key(e);
        if (!key.empty()) {
            atoms[key] = e;
            out = GpuAffine::atom(key);
            return true;
        }
    }
    return false;
}

GpuDnf gpu_dnf_and(const GpuDnf &a, const GpuDnf &b) {
    GpuDnf r;
    for (auto &x : a) {
        for (auto &y : b) {
            GpuConstraints c = x;
            c.insert(c.end(), y.begin(), y.end());
            r.push_back(c);
        }
    }
    // Too many alternatives: forget them (no constraint is always sound).
    if (r.size() > 64) return gpu_dnf_true();
    return r;
}

GpuDnf gpu_dnf_or(const GpuDnf &a, const GpuDnf &b) {
    GpuDnf r = a;
    r.insert(r.end(), b.begin(), b.end());
    if (r.size() > 64) return gpu_dnf_true();
    return r;
}

bool GpuAffineBuilder::exact_condition(ASR::expr_t *cond) {
    if (ASR::is_a<ASR::LogicalConstant_t>(*cond)) return true;
    if (ASR::is_a<ASR::IntegerCompare_t>(*cond)) {
        ASR::IntegerCompare_t *c = ASR::down_cast<ASR::IntegerCompare_t>(cond);
        GpuAffine l, r;
        return affine(c->m_left, l) && affine(c->m_right, r);
    }
    if (ASR::is_a<ASR::LogicalNot_t>(*cond)) {
        return exact_condition(ASR::down_cast<ASR::LogicalNot_t>(cond)->m_arg);
    }
    if (ASR::is_a<ASR::LogicalBinOp_t>(*cond)) {
        ASR::LogicalBinOp_t *b = ASR::down_cast<ASR::LogicalBinOp_t>(cond);
        return (b->m_op == ASR::logicalbinopType::And
                || b->m_op == ASR::logicalbinopType::Or)
            && exact_condition(b->m_left) && exact_condition(b->m_right);
    }
    return false;
}

GpuDnf GpuAffineBuilder::condition(ASR::expr_t *cond, bool truth) {
    if (ASR::is_a<ASR::LogicalConstant_t>(*cond)) {
        bool v = ASR::down_cast<ASR::LogicalConstant_t>(cond)->m_value;
        return v == truth ? gpu_dnf_true() : GpuDnf();
    }
    if (ASR::is_a<ASR::LogicalNot_t>(*cond)) {
        return condition(ASR::down_cast<ASR::LogicalNot_t>(cond)->m_arg,
            !truth);
    }
    if (ASR::is_a<ASR::LogicalBinOp_t>(*cond)) {
        ASR::LogicalBinOp_t *b = ASR::down_cast<ASR::LogicalBinOp_t>(cond);
        bool is_and = b->m_op == ASR::logicalbinopType::And;
        bool is_or = b->m_op == ASR::logicalbinopType::Or;
        if (!is_and && !is_or) return gpu_dnf_true();
        GpuDnf l = condition(b->m_left, truth);
        GpuDnf r = condition(b->m_right, truth);
        // a .and. b is true where both are; false where either is false.
        if (is_and == truth) return gpu_dnf_and(l, r);
        return gpu_dnf_or(l, r);
    }
    if (!ASR::is_a<ASR::IntegerCompare_t>(*cond)) return gpu_dnf_true();
    ASR::IntegerCompare_t *c = ASR::down_cast<ASR::IntegerCompare_t>(cond);
    GpuAffine l, r;
    if (!affine(c->m_left, l) || !affine(c->m_right, r)) {
        return gpu_dnf_true();
    }
    GpuAffine d = l.plus(r, -1);        // left - right
    GpuAffine nd = d.scaled(-1);        // right - left
    auto one = [](const GpuAffine &x) { return GpuDnf{GpuConstraints{x}}; };
    auto minus1 = [](GpuAffine x) { x.constant -= 1; return x; };
    ASR::cmpopType op = c->m_op;
    if (!truth) {
        switch (op) {
            case ASR::cmpopType::Eq: op = ASR::cmpopType::NotEq; break;
            case ASR::cmpopType::NotEq: op = ASR::cmpopType::Eq; break;
            case ASR::cmpopType::Lt: op = ASR::cmpopType::GtE; break;
            case ASR::cmpopType::LtE: op = ASR::cmpopType::Gt; break;
            case ASR::cmpopType::Gt: op = ASR::cmpopType::LtE; break;
            case ASR::cmpopType::GtE: op = ASR::cmpopType::Lt; break;
        }
    }
    switch (op) {
        case ASR::cmpopType::Eq:
            return GpuDnf{GpuConstraints{d, nd}};
        case ASR::cmpopType::NotEq:
            return GpuDnf{GpuConstraints{minus1(d)}, GpuConstraints{minus1(nd)}};
        case ASR::cmpopType::Lt:
            return one(minus1(nd));
        case ASR::cmpopType::LtE:
            return one(nd);
        case ASR::cmpopType::Gt:
            return one(minus1(d));
        case ASR::cmpopType::GtE:
            return one(d);
    }
    return gpu_dnf_true();
}

namespace {

// Divides a constraint by the gcd of its coefficients, rounding the
// constant down: over the integers sum(a x) + c >= 0 with g | a is
// sum(a/g x) + floor(c/g) >= 0.
void tighten(GpuAffine &a) {
    int64_t g = 0;
    for (auto &t : a.terms) g = std::gcd(g, t.second < 0 ? -t.second
        : t.second);
    if (g <= 1) return;
    for (auto &t : a.terms) t.second /= g;
    int64_t c = a.constant;
    a.constant = c >= 0 ? c / g : -((-c + g - 1) / g);
}

const size_t max_constraints = 2000;

} // namespace

bool gpu_affine_infeasible(const GpuConstraints &facts) {
    std::vector<GpuAffine> cs;
    std::set<std::string> vars;
    for (GpuAffine a : facts) {
        tighten(a);
        if (a.is_constant()) {
            if (a.constant < 0) return true;
            continue;
        }
        for (auto &t : a.terms) vars.insert(t.first);
        cs.push_back(a);
    }
    while (!vars.empty()) {
        // Eliminate the variable that makes the fewest new constraints.
        std::string best;
        size_t best_cost = SIZE_MAX;
        for (auto &v : vars) {
            size_t pos = 0, neg = 0;
            for (auto &c : cs) {
                int64_t k = c.coefficient(v);
                if (k > 0) pos++;
                if (k < 0) neg++;
            }
            size_t cost = pos * neg;
            if (cost < best_cost) {
                best_cost = cost;
                best = v;
            }
        }
        vars.erase(best);
        std::vector<GpuAffine> pos, neg, next;
        for (auto &c : cs) {
            int64_t k = c.coefficient(best);
            if (k > 0) pos.push_back(c);
            else if (k < 0) neg.push_back(c);
            else next.push_back(c);
        }
        for (auto &p : pos) {
            for (auto &n : neg) {
                int64_t kp = p.coefficient(best), kn = -n.coefficient(best);
                GpuAffine r = p.scaled(kn);
                r.add(n, kp);
                r.terms.erase(best);
                tighten(r);
                if (r.is_constant()) {
                    if (r.constant < 0) return true;
                    continue;
                }
                next.push_back(r);
            }
        }
        if (next.size() > max_constraints) return false;
        // Drop exact duplicates, keeping the strongest constant.
        std::map<std::map<std::string, int64_t>, int64_t> uniq;
        for (auto &c : next) {
            auto it = uniq.find(c.terms);
            if (it == uniq.end() || c.constant < it->second) {
                uniq[c.terms] = c.constant;
            }
        }
        cs.clear();
        for (auto &u : uniq) {
            GpuAffine a;
            a.terms = u.first;
            a.constant = u.second;
            cs.push_back(a);
        }
    }
    return false;
}

bool gpu_affine_proves(const GpuConstraints &facts, const GpuAffine &goal) {
    if (goal.is_constant()) {
        if (goal.constant >= 0) return true;
    }
    GpuConstraints cs = facts;
    // goal < 0, that is -goal - 1 >= 0, contradicts the facts.
    GpuAffine neg = goal.scaled(-1);
    neg.constant -= 1;
    cs.push_back(neg);
    return gpu_affine_infeasible(cs);
}

bool gpu_affine_proves(const GpuDnf &facts, const GpuAffine &goal) {
    for (auto &c : facts) {
        if (!gpu_affine_proves(c, goal)) return false;
    }
    return true;
}

ASR::expr_t *gpu_affine_to_expr(Allocator &al, const Location &loc,
        const GpuAffine &a, const std::map<std::string, ASR::expr_t*> &atoms) {
    ASR::ttype_t *t = ASRUtils::TYPE(ASR::make_Integer_t(al, loc, 4));
    auto constant = [&](int64_t v) {
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, loc, v, t,
            ASR::integerbozType::Decimal));
    };
    auto binop = [&](ASR::expr_t *l, ASR::binopType op, ASR::expr_t *r) {
        return ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, loc, l, op, r,
            ASRUtils::duplicate_type(al, t), nullptr));
    };
    ASR::expr_t *sum = nullptr;
    ASRUtils::ExprStmtDuplicator dup(al);
    for (auto &term : a.terms) {
        auto it = atoms.find(term.first);
        if (it == atoms.end()) return nullptr;
        ASR::expr_t *x = dup.duplicate_expr(it->second);
        if (!ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(x))
                || ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(x))
                    != 4) {
            x = ASRUtils::EXPR(ASR::make_Cast_t(al, loc, x,
                ASR::cast_kindType::IntegerToInteger,
                ASRUtils::duplicate_type(al, t), nullptr, nullptr));
        }
        int64_t c = term.second;
        if (sum == nullptr) {
            sum = c == 1 ? x : binop(constant(c), ASR::binopType::Mul, x);
        } else if (c > 0) {
            sum = binop(sum, ASR::binopType::Add, c == 1 ? x
                : binop(constant(c), ASR::binopType::Mul, x));
        } else {
            sum = binop(sum, ASR::binopType::Sub, c == -1 ? x
                : binop(constant(-c), ASR::binopType::Mul, x));
        }
    }
    if (sum == nullptr) return constant(a.constant);
    if (a.constant > 0) return binop(sum, ASR::binopType::Add,
        constant(a.constant));
    if (a.constant < 0) return binop(sum, ASR::binopType::Sub,
        constant(-a.constant));
    return sum;
}

} // namespace LCompilers

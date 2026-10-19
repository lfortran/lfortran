#ifndef LIBASR_PASS_GPU_AFFINE_H
#define LIBASR_PASS_GPU_AFFINE_H

#include <libasr/asr.h>

#include <cstddef>
#include <cstdint>
#include <functional>
#include <map>
#include <string>
#include <vector>

namespace LCompilers {

/*
 * Affine integer arithmetic over the subscripts and conditions of a loop
 * nest, for the loop transformations of gpu_loop_fusion.cpp and
 * gpu_index_set_split.cpp.
 *
 * An integer expression is affine when it is a sum of constant multiples of
 * *atoms* plus a constant. An atom is an integer expression the analysis
 * treats as an unknown: a loop index, or a value that does not change while
 * the loop nest runs (a scalar the nest does not assign, a bound of an array,
 * an expression built from them that is not affine). Atoms are identified
 * by a structural key (gpu_affine_key), so `nx + 1` read in two places is
 * the same affine expression.
 *
 * A condition is turned into a disjunction of conjunctions of constraints
 * `e >= 0`, for the value the condition has on a path (true in the `then`
 * part of an `if`, false in its `else` part). What the analysis cannot
 * express gives no constraint at all, which only weakens what it proves.
 *
 * gpu_affine_proves() decides whether a conjunction of constraints implies
 * another one, over the integers, by Fourier-Motzkin elimination with the
 * integer tightening of every derived constraint. A `false` answer means
 * "not proven", never "false".
 */

struct GpuAffine {
    std::map<std::string, int64_t> terms;
    int64_t constant = 0;

    GpuAffine() {}
    explicit GpuAffine(int64_t c) : constant(c) {}
    static GpuAffine atom(const std::string &key) {
        GpuAffine a;
        a.terms[key] = 1;
        return a;
    }
    void add(const GpuAffine &other, int64_t scale = 1);
    GpuAffine plus(const GpuAffine &other, int64_t scale = 1) const {
        GpuAffine r = *this;
        r.add(other, scale);
        return r;
    }
    GpuAffine scaled(int64_t scale) const {
        GpuAffine r;
        r.add(*this, scale);
        return r;
    }
    bool is_constant() const { return terms.empty(); }
    int64_t coefficient(const std::string &key) const {
        auto it = terms.find(key);
        return it == terms.end() ? 0 : it->second;
    }
    bool operator==(const GpuAffine &o) const {
        return constant == o.constant && terms == o.terms;
    }
};

// Each constraint is `e >= 0`.
using GpuConstraints = std::vector<GpuAffine>;
// A disjunction of conjunctions. One empty conjunction is "true", no
// conjunction at all is "false".
using GpuDnf = std::vector<GpuConstraints>;

// A structural key of an integer expression that does not change while the
// analysed code runs, or "" for one the key cannot describe (a call, a
// real-valued operand, an array element).
std::string gpu_affine_key(ASR::expr_t *e);

class GpuAffineBuilder {
public:
    // Scalar integer variables whose value on the analysed path is known
    // to be an affine expression: an index of the loop nest substituted by
    // a transformation, or a local assigned once from an affine value.
    std::map<ASR::symbol_t*, GpuAffine> bindings;
    // Whether a variable may be an atom, that is, keeps its value while the
    // analysed code runs. A variable that is neither bound nor invariant
    // makes the expression that reads it non-affine.
    std::function<bool(ASR::symbol_t*)> invariant_variable;
    // The atoms seen, by key.
    std::map<std::string, ASR::expr_t*> atoms;

    // The affine form of an integer expression, or false.
    bool affine(ASR::expr_t *e, GpuAffine &out);
    // The constraints that hold where `cond` has the value `truth`. A
    // condition that cannot be expressed gives "true" (no constraint), as
    // does one whose normal form has more than gpu_dnf_max_alternatives
    // alternatives: usable as a fact, not as a goal.
    GpuDnf condition(ASR::expr_t *cond, bool truth);
    // Whether `cond` can be expressed exactly (both its values, within
    // gpu_dnf_max_alternatives alternatives), so that `condition` can be
    // used as a goal to prove.
    bool exact_condition(ASR::expr_t *cond);

private:
    bool truncated = false;
    bool is_atom_expression(ASR::expr_t *e);
    bool exact_structure(ASR::expr_t *cond);
    // gpu_dnf_and and gpu_dnf_or that record a truncation in `truncated`.
    GpuDnf dnf_and(const GpuDnf &a, const GpuDnf &b);
    GpuDnf dnf_or(const GpuDnf &a, const GpuDnf &b);
};

// The most alternatives a normal form keeps; one with more is replaced by
// "true".
constexpr size_t gpu_dnf_max_alternatives = 64;

GpuDnf gpu_dnf_and(const GpuDnf &a, const GpuDnf &b);
GpuDnf gpu_dnf_or(const GpuDnf &a, const GpuDnf &b);
inline GpuDnf gpu_dnf_true() { return GpuDnf(1); }

// Whether the constraints `facts` imply `goal >= 0` over the integers.
bool gpu_affine_proves(const GpuConstraints &facts, const GpuAffine &goal);
// Whether the constraints have no integer solution (as far as
// Fourier-Motzkin with integer tightening can tell).
bool gpu_affine_infeasible(const GpuConstraints &facts);
// Whether every conjunction of `facts` implies `goal >= 0`; an infeasible
// conjunction implies anything.
bool gpu_affine_proves(const GpuDnf &facts, const GpuAffine &goal);

// The integer expression of an affine form over the atoms `atoms` holds
// the expressions of, or null when an atom is not among them.
ASR::expr_t *gpu_affine_to_expr(Allocator &al, const Location &loc,
    const GpuAffine &a, const std::map<std::string, ASR::expr_t*> &atoms);

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_AFFINE_H

#ifndef LIBASR_PASS_PARALLEL_CANONICALIZE_H
#define LIBASR_PASS_PARALLEL_CANONICALIZE_H

#include <libasr/asr.h>
#include <libasr/utils.h>

#include <vector>

namespace LCompilers {

    // The loop nest a canonical parallel region runs: one loop per index of
    // the iteration space it partitions, and the statements the innermost of
    // them runs.
    struct ParallelLoopNest {
        std::vector<ASR::DoLoop_t*> loops;
        ASR::stmt_t **body = nullptr;
        size_t n_body = 0;

        size_t n_heads() const { return loops.size(); }

        ASR::do_loop_head_t &head(size_t i) const {
            return loops[i]->m_head;
        }

        // Replaces the statements the innermost loop runs, so a rewrite of
        // the body is written back into the nest it came from.
        void set_body(ASR::stmt_t **body_, size_t n_body_) {
            loops.back()->m_body = body_;
            loops.back()->n_body = n_body_;
            body = body_;
            n_body = n_body_;
        }
    };

    // Reads the loop nest of a canonical parallel region. False when the
    // region is not one, which is the only question a lowering that consumes
    // canonical regions has to ask about a region's shape.
    bool parallel_loop_nest(const ASR::OMPRegion_t &x, ParallelLoopNest &nest);

    // The same descent, over a loop the caller holds rather than the region
    // that owns it. A lowering that rewrites the loop on its way to
    // something else works on a copy, and reads the copy through this.
    bool parallel_loop_nest_of(ASR::stmt_t *loop, int64_t collapse,
        ParallelLoopNest &nest);

    // How many loops of the nest the region's iteration space spans.
    int64_t parallel_collapse_count(const ASR::OMPRegion_t &x);

    // Whether a region carries the given clause.
    bool omp_region_has_clause(const ASR::OMPRegion_t &x,
        ASR::omp_clauseType clause);

    void pass_parallel_canonicalize(Allocator &al,
        ASR::TranslationUnit_t &unit, const PassOptions &pass_options);

    void pass_flatten_omp_regions(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options);

} // namespace LCompilers

#endif // LIBASR_PASS_PARALLEL_CANONICALIZE_H

#ifndef LIBASR_PASS_GPU_OFFLOAD_UNDO_H
#define LIBASR_PASS_GPU_OFFLOAD_UNDO_H

#include <set>
#include <string>
#include <utility>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/pass/parallel_canonicalize.h>

namespace LCompilers {

// Hands a body rewrite the loop's statements and writes whatever it made
// of them back into the loop. The rewrites report their result by moving
// the pointers they were given, and a nest holds those pointers twice --
// once as its own view and once in the loop it reads from -- so the two
// would otherwise drift apart.
class NestBodyWriteBack {
public:
    ParallelLoopNest &nest;
    ASR::stmt_t **body;
    size_t n_body;

    NestBodyWriteBack(ParallelLoopNest &nest_)
        : nest(nest_), body(nest_.body), n_body(nest_.n_body) {}

    ~NestBodyWriteBack() { nest.set_body(body, n_body); }
};

// `inline_device_function_calls` splices a device callee into the loop
// body by replacing whole statement vectors -- the loop's own and that of
// every BLOCK it recurses into -- and never edits a statement in place.
// It only ever adds symbols to the enclosing scope, one Block per splice.
// Recording those vectors and that scope's symbol names therefore makes
// the splice exactly reversible. That is what lets the workspace
// pre-flight run on the spliced shape, where a callee's locals have
// become kernel workspaces, and still leave the loop exactly as it was
// found when the offload is declined.
//
// Dropping the spliced-in Blocks again matters as much as restoring the
// statements: an orphaned Block is still a symbol of the enclosing scope,
// so the pass would walk into it on its next round, offload the loops it
// holds, splice once more and never reach a fixed point.
// The type a scope-local array carried before the pass replaced it, so
// that a declined offload leaves the variable exactly as it found it.
struct ScopeArrayDims {
    ASR::Variable_t *var;
    ASR::ttype_t *type;
};

class GpuLoopBodySnapshot {
public:
    void record(ParallelLoopNest &nest, SymbolTable *scope) {
        nest_ = &nest;
        body_ = nest.body;
        n_body_ = nest.n_body;
        scope_ = scope;
        if (scope_ != nullptr) {
            for (auto &item : scope_->get_scope()) {
                scope_symbols_.insert(item.first);
            }
        }
        record_blocks(nest.body, nest.n_body);
    }

    void restore() {
        if (nest_ == nullptr) return;
        nest_->set_body(body_, n_body_);
        for (auto &saved : blocks_) {
            saved.block->m_body = saved.body;
            saved.block->n_body = saved.n_body;
        }
        if (scope_ == nullptr) return;
        std::vector<std::string> added;
        for (auto &item : scope_->get_scope()) {
            if (scope_symbols_.count(item.first) == 0) {
                added.push_back(item.first);
            }
        }
        for (auto &name : added) {
            scope_->erase_symbol(name);
        }
    }

private:
    struct SavedBlock {
        ASR::Block_t *block;
        ASR::stmt_t **body;
        size_t n_body;
    };

    void record_blocks(ASR::stmt_t **body, size_t n_body) {
        for (size_t i = 0; i < n_body; i++) {
            if (!ASR::is_a<ASR::BlockCall_t>(*body[i])) continue;
            ASR::symbol_t *b = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::BlockCall_t>(body[i])->m_m);
            if (b == nullptr || !ASR::is_a<ASR::Block_t>(*b)) continue;
            ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
            blocks_.push_back({block, block->m_body, block->n_body});
            record_blocks(block->m_body, block->n_body);
        }
    }

    ParallelLoopNest *nest_ = nullptr;
    ASR::stmt_t **body_ = nullptr;
    size_t n_body_ = 0;
    std::vector<SavedBlock> blocks_;
    SymbolTable *scope_ = nullptr;
    std::set<std::string> scope_symbols_;
};

// Undoes the gather substitution when the loop turns out not to be
// offloadable.  The pass must leave a declined loop exactly as it found
// it, and the substitution is made before the eligibility checks so that
// they judge the shape the kernel would actually be built from.
class GpuGatherGuard {
public:
    GpuGatherGuard(SymbolTable *scope,
            std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> &undo,
            std::vector<std::string> &names)
        : scope_(scope), undo_(undo), names_(names) {}

    ~GpuGatherGuard() {
        if (committed_) return;
        for (size_t i = undo_.size(); i > 0; i--) {
            *undo_[i - 1].first = undo_[i - 1].second;
        }
        undo_.clear();
        for (const std::string &n : names_) {
            if (scope_ != nullptr) scope_->erase_symbol(n);
        }
        names_.clear();
    }

    void commit() { committed_ = true; }

private:
    SymbolTable *scope_;
    std::vector<std::pair<ASR::expr_t**, ASR::expr_t*>> &undo_;
    std::vector<std::string> &names_;
    bool committed_ = false;
};

// What the pass takes out of the host's scope while it drafts a kernel: the
// copies of the blocks in the loop nest, and the kernel number the draft
// would be named after. Every exit that leaves the loop on the host has to
// give both back -- the copies because a block nothing calls is left over ASR
// in a live function, half-rewritten once the inliners below have run, and
// the number because a gap makes an emitted kernel's name depend on unrelated
// declines elsewhere in the file. There are a dozen such exits, so a guard
// owns them rather than each exit remembering to.
class GpuKernelDraftGuard {
public:
    GpuKernelDraftGuard(SymbolTable *scope,
            std::vector<ASR::symbol_t*> &blocks, int &counter)
        : scope_(scope), blocks_(blocks), counter_(counter),
          saved_counter_(counter) {}

    ~GpuKernelDraftGuard() {
        if (committed_) return;
        for (ASR::symbol_t *b : blocks_) {
            if (scope_ == nullptr) continue;
            // One already moved into the kernel goes with it, and a name
            // the scope has since given to something else is not this
            // block's to drop.
            std::string name = ASRUtils::symbol_name(b);
            if (scope_->get_symbol(name) == b) {
                scope_->erase_symbol(name);
            }
        }
        blocks_.clear();
        counter_ = saved_counter_;
    }

    void commit() { committed_ = true; }

private:
    SymbolTable *scope_;
    std::vector<ASR::symbol_t*> &blocks_;
    int &counter_;
    int saved_counter_;
    bool committed_ = false;
};

// The splice snapshot and the ASSOCIATE-array types rewritten in place
// have to be given back on every decline after they are taken. There are
// several such exits, so a guard owns them rather than each one
// remembering to.
class GpuSpliceRestoreGuard {
public:
    GpuSpliceRestoreGuard(GpuLoopBodySnapshot &snapshot,
            std::vector<ScopeArrayDims> &scope_dims)
        : snapshot_(snapshot), scope_dims_(scope_dims) {}

    ~GpuSpliceRestoreGuard() {
        if (committed_) return;
        for (auto it = scope_dims_.rbegin();
                it != scope_dims_.rend(); ++it) {
            it->var->m_type = it->type;
        }
        scope_dims_.clear();
        snapshot_.restore();
    }

    void commit() { committed_ = true; }

private:
    GpuLoopBodySnapshot &snapshot_;
    std::vector<ScopeArrayDims> &scope_dims_;
    bool committed_ = false;
};

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_OFFLOAD_UNDO_H

#ifndef LIBASR_PASS_GPU_OFFLOAD_COLLECT_H
#define LIBASR_PASS_GPU_OFFLOAD_COLLECT_H

#include <map>
#include <set>
#include <string>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/exception.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/pass/parallel_canonicalize.h>

namespace LCompilers {

// Collects all symbols referenced in expressions/statements
class GpuSymbolCollector :
        public ASRUtils::BlockBodyWalkVisitor<GpuSymbolCollector> {
public:
    Allocator &al;
    std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> &symbols;
    std::set<SymbolTable*> enclosing_scopes;

    GpuSymbolCollector(Allocator &al_,
        std::map<std::string, std::pair<ASR::ttype_t*, ASR::expr_t*>> &syms,
        const std::set<SymbolTable*> &scopes = {})
        : al(al_), symbols(syms), enclosing_scopes(scopes) {}

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        // Walk variable types in the block's symbol table to collect
        // referenced symbols (e.g., VLA dimension expressions like n(i))
        for (auto &item : block->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(
                    item.second);
                ASR::ttype_t *type = var->m_type;
                if (ASR::is_a<ASR::Array_t>(*type)) {
                    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
                    for (size_t d = 0; d < arr->n_dims; d++) {
                        if (arr->m_dims[d].m_start) {
                            visit_expr(*arr->m_dims[d].m_start);
                        }
                        if (arr->m_dims[d].m_length) {
                            visit_expr(*arr->m_dims[d].m_length);
                        }
                    }
                }
            }
        }
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_Var(const ASR::Var_t &x) {
        // A variable owned by a Block or AssociateBlock scope that is
        // nested *inside* the loop travels with that scope into the
        // kernel, so it must not become a kernel parameter. A variable
        // owned by a Block or AssociateBlock that *encloses* the loop is
        // left behind when the loop is extracted, so it does have to be
        // passed in. `enclosing_scopes` holds exactly the latter.
        if (ASR::is_a<ASR::Variable_t>(*x.m_v)) {
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(x.m_v);
            if (var->m_parent_symtab->asr_owner &&
                var->m_parent_symtab->asr_owner->type
                    == ASR::asrType::symbol) {
                ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
                    var->m_parent_symtab->asr_owner);
                if ((ASR::is_a<ASR::Block_t>(*owner) ||
                     ASR::is_a<ASR::AssociateBlock_t>(*owner)) &&
                    enclosing_scopes.find(var->m_parent_symtab)
                        == enclosing_scopes.end()) {
                    return;
                }
            }
        }
        std::string name = ASRUtils::symbol_name(x.m_v);
        if (symbols.find(name) == symbols.end()) {
            symbols[name] = {ASRUtils::symbol_type(x.m_v),
                ASRUtils::EXPR(ASR::make_Var_t(al, x.base.base.loc, x.m_v))};
        }
    }
};

// Checks whether an expression tree contains a FunctionCall node.
class ContainsFunctionCall : public ASR::BaseWalkVisitor<ContainsFunctionCall> {
public:
    bool found = false;
    void visit_FunctionCall(const ASR::FunctionCall_t &) { found = true; }
};

bool expr_has_function_call(ASR::expr_t *expr);

// Collects the names every Var in an expression tree refers to.
class GpuVarNameCollector : public ASR::BaseWalkVisitor<GpuVarNameCollector> {
public:
    std::set<std::string> names;
    void visit_Var(const ASR::Var_t &x) {
        names.insert(ASRUtils::symbol_name(x.m_v));
    }
};

// A host-side expression may not name a loop index: the host evaluates it
// before the launch, where the index has no value.  The workspace
// pre-flight already declines such a loop, so reaching here means the
// emitter and the pre-flight disagree -- a compiler bug, which must be a
// clean failure rather than a plausible-but-wrong number read out of an
// undefined variable.
void gpu_check_host_expr_index_free(ASR::expr_t *host_expr,
        const std::set<std::string> &index_names, const std::string &what);

// A section whose base is itself a section -- what splicing a device
// function leaves behind when its assumed-shape dummy was already
// referenced through a section and the actual is a section too.  The
// Metal emitter renders a section's base with the ordinary expression
// emitter, which has no address to give for an `ArraySection`, so a
// kernel built from such a body compiles to a shader that does not
// build.  The offload declines instead.
class GpuNestedSectionFinder :
        public ASR::BaseWalkVisitor<GpuNestedSectionFinder> {
public:
    bool found = false;

    void visit_ArraySection(const ASR::ArraySection_t &x) {
        ASR::expr_t *base = ASRUtils::get_past_array_physical_cast(x.m_v);
        if (base != nullptr && ASR::is_a<ASR::ArraySection_t>(*base)) {
            found = true;
        }
        ASR::BaseWalkVisitor<GpuNestedSectionFinder>::visit_ArraySection(x);
    }

    // The generated walker stops at a BLOCK or ASSOCIATE call, and both
    // are where a spliced device function body ends up.
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }
};

// True when a statement list takes a section of `dummy` -- `c(1:k)` for a
// dummy `c`.  Splicing the callee substitutes the actual argument for the
// dummy, so such a section over a sectioned actual becomes a section of a
// section, which no device pointer can express.
class GpuDummySectionFinder :
        public ASR::BaseWalkVisitor<GpuDummySectionFinder> {
public:
    ASR::symbol_t *dummy;
    bool found = false;

    GpuDummySectionFinder(ASR::symbol_t *dummy_) : dummy(dummy_) {}

    void visit_ArraySection(const ASR::ArraySection_t &x) {
        ASR::expr_t *base = ASRUtils::get_past_array_physical_cast(x.m_v);
        if (base != nullptr && ASR::is_a<ASR::Var_t>(*base) &&
                ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(base)->m_v) == dummy) {
            found = true;
        }
        ASR::BaseWalkVisitor<GpuDummySectionFinder>::visit_ArraySection(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }
};

// True when a statement list holds an implied-do. The array-constructor
// lowering leaves one standing as a whole element of the constructor, and
// the Metal code generator has no rendering for it, so a kernel built from
// such a body reaches the driver as a shader that will not compile.
class GpuImpliedDoFinder :
        public ASR::BaseWalkVisitor<GpuImpliedDoFinder> {
public:
    bool found = false;

    void visit_ImpliedDoLoop(const ASR::ImpliedDoLoop_t &x) {
        found = true;
        ASR::BaseWalkVisitor<GpuImpliedDoFinder>::visit_ImpliedDoLoop(x);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        this->visit_symbol(*x.m_m);
    }
};

// Counts how many times a given symbol is written to within a list of
// statements. Used to distinguish a genuine ASSOCIATE selector temporary
// (written exactly once, at its point of definition) from an ordinary
// block-local variable such as the counter that array-constructor
// lowering introduces, which is written repeatedly and therefore must
// not be folded into a constant.
class AssignmentTargetCounter :
    public ASRUtils::BlockBodyWalkVisitor<AssignmentTargetCounter> {
public:
    ASR::symbol_t *target;
    size_t count;

    AssignmentTargetCounter(ASR::symbol_t *target_)
        : target(target_), count(0) {}

    void count_var(ASR::expr_t *e) {
        if (e && ASR::is_a<ASR::Var_t>(*e) &&
                ASR::down_cast<ASR::Var_t>(e)->m_v == target) {
            count++;
        }
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        count_var(x.m_target);
        ASR::BaseWalkVisitor<AssignmentTargetCounter>::visit_Assignment(x);
    }

    void visit_Associate(const ASR::Associate_t &x) {
        count_var(x.m_target);
        ASR::BaseWalkVisitor<AssignmentTargetCounter>::visit_Associate(x);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        count_var(x.m_head.m_v);
        ASR::BaseWalkVisitor<AssignmentTargetCounter>::visit_DoLoop(x);
    }


    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        // Conservatively treat any appearance as an argument as a write
        for (size_t i = 0; i < x.n_args; i++) {
            count_var(x.m_args[i].m_value);
        }
        ASR::BaseWalkVisitor<AssignmentTargetCounter>::visit_SubroutineCall(x);
    }
};

// True if `sym` is written exactly once across `body` — the shape of a
// single-assignment ASSOCIATE selector binding, which may safely be
// folded into its value at every use site.
bool is_single_assignment_binding(ASR::symbol_t *sym,
        ASR::stmt_t **body, size_t n_body);

// Collects local variables used in do concurrent body that are NOT
// arrays and NOT the loop variables — these are per-thread temporaries
class GpuLocalVarCollector :
        public ASRUtils::BlockBodyWalkVisitor<GpuLocalVarCollector> {
public:
    std::set<std::string> &local_vars;
    std::set<std::string> &assigned_vars;
    std::set<SymbolTable*> enclosing_scopes;

    GpuLocalVarCollector(std::set<std::string> &lv, std::set<std::string> &av,
        const std::set<SymbolTable*> &scopes = {})
        : local_vars(lv), assigned_vars(av), enclosing_scopes(scopes) {}

    void visit_Assignment(const ASR::Assignment_t &x) {
        // Check if target is a simple Var (not ArrayItem)
        if (ASR::is_a<ASR::Var_t>(*x.m_target)) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_target);
            // Skip variables local to Block or AssociateBlock scopes
            bool is_block_local = false;
            if (ASR::is_a<ASR::Variable_t>(*v->m_v)) {
                ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(v->m_v);
                if (var->m_parent_symtab->asr_owner) {
                    ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
                        var->m_parent_symtab->asr_owner);
                    if (ASR::is_a<ASR::Block_t>(*owner) ||
                        ASR::is_a<ASR::AssociateBlock_t>(*owner)) {
                        if (enclosing_scopes.find(var->m_parent_symtab)
                                == enclosing_scopes.end()) {
                            is_block_local = true;
                        }
                    }
                }
            }
            if (!is_block_local) {
                std::string name = ASRUtils::symbol_name(v->m_v);
                ASR::ttype_t *type = ASRUtils::symbol_type(v->m_v);
                if (!ASRUtils::is_array(type)) {
                    assigned_vars.insert(name);
                }
            }
        }
        // Check if target is a StructInstanceMember (e.g., x%v = ...)
        if (ASR::is_a<ASR::StructInstanceMember_t>(*x.m_target)) {
            ASR::StructInstanceMember_t *sm =
                ASR::down_cast<ASR::StructInstanceMember_t>(x.m_target);
            if (ASR::is_a<ASR::Var_t>(*sm->m_v)) {
                ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(sm->m_v);
                std::string name = ASRUtils::symbol_name(v->m_v);
                assigned_vars.insert(name);
            }
        }
        ASR::BaseWalkVisitor<GpuLocalVarCollector>::visit_Assignment(x);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        // DoLoop loop variables are local temporaries
        if (x.m_head.m_v && ASR::is_a<ASR::Var_t>(*x.m_head.m_v)) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_head.m_v);
            std::string name = ASRUtils::symbol_name(v->m_v);
            assigned_vars.insert(name);
        }
        ASR::BaseWalkVisitor<GpuLocalVarCollector>::visit_DoLoop(x);
    }
};

// Collects all Function symbols referenced by FunctionCall/SubroutineCall
// nodes in the loop body so they can be imported into the kernel.
class GpuFunctionCollector :
        public ASRUtils::BlockBodyWalkVisitor<GpuFunctionCollector> {
public:
    std::map<std::string, ASR::symbol_t*> functions;

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::symbol_t *resolved = ASRUtils::symbol_get_past_external(x.m_name);
        if (ASR::is_a<ASR::Function_t>(*resolved) ||
                ASR::is_a<ASR::StructMethodDeclaration_t>(*resolved)) {
            std::string name = ASRUtils::symbol_name(x.m_name);
            if (functions.find(name) == functions.end()) {
                functions[name] = x.m_name;
            }
        }
        if (x.m_original_name) {
            std::string orig_name = ASRUtils::symbol_name(x.m_original_name);
            if (functions.find(orig_name) == functions.end()) {
                functions[orig_name] = x.m_original_name;
            }
        }
        ASR::BaseWalkVisitor<GpuFunctionCollector>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::symbol_t *resolved = ASRUtils::symbol_get_past_external(x.m_name);
        if (ASR::is_a<ASR::Function_t>(*resolved) ||
                ASR::is_a<ASR::StructMethodDeclaration_t>(*resolved)) {
            std::string name = ASRUtils::symbol_name(x.m_name);
            if (functions.find(name) == functions.end()) {
                functions[name] = x.m_name;
            }
        }
        ASR::BaseWalkVisitor<GpuFunctionCollector>::visit_SubroutineCall(x);
    }
};

// Every routine a loop body reaches, however deep.
std::vector<ASR::Function_t*> reachable_routines(ASR::stmt_t **body,
        size_t n_body);

// Collects every parallel region reached from the statements walked,
// descending into BLOCK and ASSOCIATE scopes so a loop nested there is
// seen too.
class GpuParallelRegionCollector :
        public ASRUtils::BlockBodyWalkVisitor<GpuParallelRegionCollector> {
public:
    std::set<ASR::OMPRegion_t*> loops;

    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        if (omp_region_has_clause(x, ASR::omp_clauseType::OMPIndependent)) {
            loops.insert(const_cast<ASR::OMPRegion_t*>(&x));
        }
        ASR::BaseWalkVisitor<GpuParallelRegionCollector>::visit_OMPRegion(x);
    }
};

// A parallel loop that device code has to run cannot become a launch of
// its own, so it is marked to run serially -- which is what one thread of
// the enclosing kernel does with it.
// DoLoops. `do concurrent` only permits the iterations to run in any
// order, so running them in order is always correct; inside device code
// it is the only thing that can be done.
class GpuSerialRegionMarker {
public:
    static void run(const std::set<ASR::OMPRegion_t*> &loops) {
        for (ASR::OMPRegion_t *region : loops) {
            region->m_exec_target = ASR::exec_targetType::ExecSerial;
        }
    }
};

// Collects every FunctionCall in a statement, so the inliner can tell a
// call in a spliceable position (the whole right-hand side of an
// assignment) from one buried inside a larger expression.
class GpuCallSiteCollector :
        public ASR::BaseWalkVisitor<GpuCallSiteCollector> {
public:
    std::vector<const ASR::FunctionCall_t*> calls;

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        calls.push_back(&x);
        ASR::BaseWalkVisitor<GpuCallSiteCollector>::visit_FunctionCall(x);
    }
};

// Collects StructInstanceMember references to allocatable array members
// in the loop body. Used to decompose struct-typed kernel
// parameters into separate flat array buffers for Metal.
// Collects all variable names referenced (read) in a set of statements.
// Used to determine which variables are live after a parallel loop.
class PostLoopVarCollector :
        public ASRUtils::BlockBodyWalkVisitor<PostLoopVarCollector> {
public:
    std::set<std::string> &referenced_vars;
    PostLoopVarCollector(std::set<std::string> &rv) : referenced_vars(rv) {}
    void visit_Var(const ASR::Var_t &x) {
        referenced_vars.insert(ASRUtils::symbol_name(x.m_v));
    }
};

// Collects the symbols targeted by Var nodes in a set of statements,
// including the bodies of nested Block and AssociateBlock scopes. Used to
// decide whether a scope is still needed after its associate aliases have
// been inlined.
class VarSymbolCollector :
        public ASRUtils::BlockBodyWalkVisitor<VarSymbolCollector> {
public:
    std::set<ASR::symbol_t*> &referenced_syms;
    VarSymbolCollector(std::set<ASR::symbol_t*> &rs) : referenced_syms(rs) {}

    void visit_Var(const ASR::Var_t &x) {
        referenced_syms.insert(x.m_v);
    }
};

class GpuAllocStructMemberCollector :
    public ASRUtils::BlockBodyWalkVisitor<GpuAllocStructMemberCollector> {
public:
    // Maps struct_var_name -> { member_name -> (member_sym, member_type) }
    std::map<std::string,
        std::map<std::string, std::pair<ASR::symbol_t*, ASR::ttype_t*>>>
            alloc_members;
    // Struct var names that have any non-allocatable-array member access
    std::set<std::string> has_non_alloc_access;

    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        if (ASR::is_a<ASR::Var_t>(*x.m_v)) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_v);
            std::string struct_name = ASRUtils::symbol_name(v->m_v);
            ASR::symbol_t *mem = ASRUtils::symbol_get_past_external(x.m_m);
            std::string mem_name = ASRUtils::symbol_name(mem);
            ASR::ttype_t *mem_type = x.m_type;
            if (ASRUtils::is_allocatable(mem_type)) {
                ASR::ttype_t *inner =
                    ASRUtils::type_get_past_allocatable(mem_type);
                if (ASR::is_a<ASR::Array_t>(*inner)) {
                    alloc_members[struct_name][mem_name] =
                        {x.m_m, mem_type};
                } else {
                    has_non_alloc_access.insert(struct_name);
                }
            } else {
                has_non_alloc_access.insert(struct_name);
            }
        }
        ASR::BaseWalkVisitor<GpuAllocStructMemberCollector>::
            visit_StructInstanceMember(x);
    }
};

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_OFFLOAD_COLLECT_H

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
#include <libasr/pass/gpu_procedure_implementation.h>
#include <libasr/pass/parallel_canonicalize.h>
#include <libasr/pass/pass_utils.h>

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

// Whether `e` is a call of a function that is not elemental and returns an
// array of an intrinsic numeric, logical or derived type, past a physical cast.
bool gpu_is_array_function_result(ASR::expr_t *e);

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

// Whether the procedure a call names may define its `i`-th actual argument:
// the dummy argument it is associated with is not `intent(in)` and not
// passed by value, or is not known.
bool gpu_call_may_define(const ASR::SubroutineCall_t &x, size_t i);

// The scalar variable whose own storage a designator that is defined
// defines: `k`, `y`, `y%k`, `y%a%x` or `y%c(i)` for a component `c` that is
// neither allocatable nor a pointer. Nullptr for anything else: an array
// variable, or a part of an allocatable or pointer array or component,
// which is not stored in the variable.
ASR::symbol_t *gpu_defined_variable(ASR::expr_t *designator);

// Collects local variables used in do concurrent body that are NOT
// arrays and NOT the loop variables — these are per-thread temporaries
class GpuLocalVarCollector :
        public ASRUtils::BlockBodyWalkVisitor<GpuLocalVarCollector> {
public:
    std::set<std::string> &assigned_vars;
    std::set<std::string> &inner_loop_vars;
    std::set<SymbolTable*> enclosing_scopes;

    GpuLocalVarCollector(std::set<std::string> &av,
        std::set<std::string> &ilv,
        const std::set<SymbolTable*> &scopes = {})
        : assigned_vars(av), inner_loop_vars(ilv), enclosing_scopes(scopes) {}

    // A variable the body defines, unless it is a local of a BLOCK or
    // ASSOCIATE in the body.
    void note(ASR::symbol_t *sym) {
        if (sym == nullptr || ASR::is_a<ASR::Variable_t>(*sym) == false) {
            if (sym != nullptr && ASR::is_a<ASR::ExternalSymbol_t>(*sym)) {
                assigned_vars.insert(ASRUtils::symbol_name(sym));
            }
            return;
        }
        ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
        if (var->m_parent_symtab->asr_owner) {
            ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
                var->m_parent_symtab->asr_owner);
            if ((ASR::is_a<ASR::Block_t>(*owner) ||
                    ASR::is_a<ASR::AssociateBlock_t>(*owner)) &&
                    enclosing_scopes.find(var->m_parent_symtab)
                        == enclosing_scopes.end()) {
                return;
            }
        }
        assigned_vars.insert(ASRUtils::symbol_name(sym));
    }

    void visit_Assignment(const ASR::Assignment_t &x) {
        note(gpu_defined_variable(x.m_target));
        ASR::BaseWalkVisitor<GpuLocalVarCollector>::visit_Assignment(x);
    }

    // An actual argument the procedure may define.
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            if (x.m_args[i].m_value && gpu_call_may_define(x, i)) {
                note(gpu_defined_variable(x.m_args[i].m_value));
            }
        }
        if (x.m_dt) note(gpu_defined_variable(x.m_dt));
        ASR::BaseWalkVisitor<GpuLocalVarCollector>::visit_SubroutineCall(x);
    }

    void visit_DoLoop(const ASR::DoLoop_t &x) {
        // DoLoop loop variables are local temporaries
        if (x.m_head.m_v && ASR::is_a<ASR::Var_t>(*x.m_head.m_v)) {
            ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_head.m_v);
            std::string name = ASRUtils::symbol_name(v->m_v);
            assigned_vars.insert(name);
            inner_loop_vars.insert(name);
        }
        ASR::BaseWalkVisitor<GpuLocalVarCollector>::visit_DoLoop(x);
    }

    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x) {
        for (size_t i = 0; i < x.n_head; i++) {
            if (x.m_head[i].m_v && ASR::is_a<ASR::Var_t>(*x.m_head[i].m_v)) {
                ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(x.m_head[i].m_v);
                std::string name = ASRUtils::symbol_name(v->m_v);
                assigned_vars.insert(name);
                inner_loop_vars.insert(name);
            }
        }
        ASR::BaseWalkVisitor<GpuLocalVarCollector>::visit_DoConcurrentLoop(x);
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

// Every routine a loop body reaches, however deep: the implementation of
// a separate module procedure, which `implementations` finds, in place of
// the interface the call names.
std::vector<ASR::Function_t*> reachable_routines(ASR::stmt_t **body,
        size_t n_body, GpuProcedureImplementations &implementations);

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

// The routines the parallel loops of `unit` reach, whatever their target.
// The offload runs the parallel loops of these serially in the caller
// (GpuSerialRegionMarker), so the loop transformations leave them alone.
std::set<ASR::Function_t*> gpu_parallel_loop_routines(Allocator &al,
    ASR::TranslationUnit_t &unit, const PassOptions &pass_options);

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
            // What a launch need not copy in means nothing without one.
            size_t n = 0;
            for (size_t i = 0; i < region->n_clauses; i++) {
                if (!ASR::is_a<ASR::OMPOverwrite_t>(*region->m_clauses[i])) {
                    region->m_clauses[n++] = region->m_clauses[i];
                }
            }
            region->n_clauses = n;
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

// The one element of the buffer a scalar whose value is kept after a
// parallel loop is handed to the kernel in, and handed back in.
ASR::expr_t* gpu_liveout_slot(Allocator &al, const Location &loc,
    ASR::symbol_t *buffer, ASR::ttype_t *scalar_type);

// Puts the value of the private copy each thread of a kernel keeps of a
// variable whose value is kept after the loop back where it is shared,
// after the statements that define the copy or a part of it: only the
// part an assignment or the actual argument of a call defines is put
// back, so that what other iterations define of the variable is left
// alone. `entries` are the assignments that give each copy its value on
// entry, from where the value is shared, which is where it is put back.
//
// A definition in a loop of a statement of the kernel body would put the
// value back on every trip; a flag records it instead, and the value is
// put back once, after that statement, if the flag is set. That is only
// done for a definition of the whole copy or of a component of it (the
// part an element names can change from one trip to the next), and for a
// statement that nothing leaves early (CYCLE, EXIT, RETURN, GO TO, STOP),
// which could skip the write after it.
class GpuLiveoutWriteBack : public PassUtils::PassVisitor<GpuLiveoutWriteBack> {
public:
    std::set<ASR::stmt_t*> entries;

    GpuLiveoutWriteBack(Allocator &al_);

    void run(ASR::Function_t &kernel);

    void visit_Assignment(const ASR::Assignment_t &x);
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x);
    void visit_DoLoop(const ASR::DoLoop_t &x);
    void visit_WhileLoop(const ASR::WhileLoop_t &x);
    void visit_DoConcurrentLoop(const ASR::DoConcurrentLoop_t &x);

private:
    // The private copy, and where its value is shared.
    std::map<ASR::symbol_t*, ASR::expr_t*> shared;
    SymbolTable *kernel_scope = nullptr;
    // Whether the statement of the kernel body being visited can have a
    // write put after it, and how many loops of it are around the one
    // visited.
    bool deferring = false;
    int loops = 0;
    // The writes put after the statement, under their flags.
    std::vector<std::pair<ASR::symbol_t*, ASR::stmt_t*>> deferred;

    // The private copy `designator` is a part of, if it is one.
    ASR::symbol_t *copy_of(ASR::expr_t *designator);
    // `<the same part of the shared value> = designator`.
    ASR::stmt_t *put_back(const Location &loc, ASR::expr_t *designator,
        ASR::symbol_t *copy);
    // Puts a definition of `designator` back, after the statement visited
    // (into `after`) or after the statement of the kernel body.
    void define(const Location &loc, ASR::expr_t *designator,
        ASR::symbol_t *copy, std::vector<ASR::stmt_t*> &after);
};

// Whether `name`, of type `type`, is a derived-type variable of a type with
// allocatable or pointer parts and a derived-type component, which the
// kernel cannot hold as a local of its own (the lowering of such a local
// is not done yet). Each thread keeps private copies of the scalar parts
// of it the loop defines instead (gpu_offload_liveout.cpp), whether or not
// its value is kept after the loop.
bool gpu_no_kernel_local_struct(SymbolTable *scope,
    const std::string &name, ASR::ttype_t *type);

// Collects StructInstanceMember references to allocatable array members
// in the loop body. Used to decompose struct-typed kernel
// parameters into separate flat array buffers for Metal.
// Collects all variable names referenced in a set of statements, except
// in the parallel loop `skip`. Used to determine which variables are live
// after that loop.
class PostLoopVarCollector :
        public ASRUtils::BlockBodyWalkVisitor<PostLoopVarCollector> {
public:
    std::set<std::string> &referenced_vars;
    const ASR::OMPRegion_t *skip;
    PostLoopVarCollector(std::set<std::string> &rv,
        const ASR::OMPRegion_t *skip_ = nullptr)
        : referenced_vars(rv), skip(skip_) {}
    void visit_Var(const ASR::Var_t &x) {
        referenced_vars.insert(ASRUtils::symbol_name(x.m_v));
    }
    void visit_OMPRegion(const ASR::OMPRegion_t &x) {
        if (&x == skip) return;
        ASR::BaseWalkVisitor<PostLoopVarCollector>::visit_OMPRegion(x);
    }
};

// The names of the variables that may be referenced once the parallel
// loop `loop` has run: by a statement of the procedure (or main program)
// that holds it -- the statements after it, in the statement list that
// holds it and in each construct around it, and every other statement of
// a loop around it, which can run again -- or by a procedure it contains,
// which a later statement may call. When the procedure has a GO TO, which
// can run any statement again, `everything` is set instead.
std::set<std::string> gpu_names_referenced_after_loop(
    const ASR::OMPRegion_t &loop, SymbolTable *scope, bool &everything);

// Whether the value a parallel loop in `scope` gives the variable `sym`
// can be read once the procedure (or main program) holding the loop has
// returned, or by another procedure: it is a dummy argument or the
// function result, it is saved, or it belongs to a module or to a host
// procedure.
bool gpu_visible_outside_procedure(ASR::symbol_t *sym, SymbolTable *scope);

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
            // The variable is the base of this reference, not a use of
            // the whole value.
            visit_ttype(*x.m_type);
            if (x.m_value) visit_expr(*x.m_value);
            return;
        }
        ASR::BaseWalkVisitor<GpuAllocStructMemberCollector>::
            visit_StructInstanceMember(x);
    }

    // A derived-type value used whole -- passed to a procedure, assigned
    // -- reaches the kernel as a value, so the variable stays a kernel
    // argument even when its allocatable components are also handed over
    // as buffers of their own.
    void visit_Var(const ASR::Var_t &x) {
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(x.m_v);
        if (ASR::is_a<ASR::Variable_t>(*sym) &&
                ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(
                    ASR::down_cast<ASR::Variable_t>(sym)->m_type))) {
            has_non_alloc_access.insert(ASRUtils::symbol_name(x.m_v));
        }
    }
};

} // namespace LCompilers

#endif // LIBASR_PASS_GPU_OFFLOAD_COLLECT_H

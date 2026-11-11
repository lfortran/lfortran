#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/device_launch_expand.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/access_paths.h>
#include <libasr/pass/gpu_launch_sync.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/pass_utils.h>

#include <algorithm>
#include <functional>
#include <map>
#include <set>
#include <tuple>
#include <vector>

namespace LCompilers {

/*
Where the host waits for the GPU.

A GpuKernelLaunch does not wait for its kernel, and the device keeps what a
kernel writes until the host waits (lfortran_gpu_runtime.h). This pass puts a
GpuSync -- wait for every launch, and copy what the kernels wrote back to the
host -- where the host next needs that, and nowhere else:

* before a host statement that may read or write, allocate or free storage a
  pending launch may write: a read would see stale values, and the copy back
  at the next wait would undo a write;
* before a statement that acts outside the program -- input/output, `stop`,
  a call to a procedure that is not pure -- while a launch whose kernel can
  stop is pending, so that a stop the kernel recorded ends the program first,
  as it does on the CPU;
* before a launch of a kernel that can stop while another such launch is
  pending, so that the stop reported is the first one;
* before `return`, `goto` and the end of the procedure, when a launch is
  pending: what the caller sees is complete -- except in a deferring twin;
* before any statement this pass does not know.

Deferring twins. A module subroutine that `gets_twin` selects from its
declaration alone (so that every translation unit that uses the module from
its module file selects the same) hands its body to a new procedure of the
module, `<name>__gpu_deferred`, its twin, and only calls it: the subroutine
then waits at its end as any procedure does. What can call a procedure
without this pass seeing the call -- a procedure pointer, a dummy procedure,
a binding, C, a file compiled without a device -- reaches the subroutine.
The calls this pass sees by name are redirected to the twin, which may
return with launches pending. A subroutine defined in another object file
(a module loaded from its module file whose procedures this translation
unit does not compile) gets no twin if that object defines none: when the
module was compiled with separate compilation into an object of its own
and without a device (Module.host_only_object), such as a library built for
the host. Its calls reach the subroutine. In the default mode the unit with
the main program compiles the module's procedures, and is assumed to be
compiled with a device like this unit, so it defines their twins.

* At its exits the twin discards, or waits for, the launches on its locals,
  and returns without waiting when what is still pending writes and reads
  only what its dummy arguments hold and no pending launch may have
  recorded a stop (its replay reads a local status buffer). Otherwise it
  waits as before.
* Its caller treats the call as a launch that writes the actual arguments
  of its dummies that are not intent(in) and reads the others: what the
  twin's declaration allows, or, for a twin of this translation unit, what
  the launches pending at its exits write and read (Deferral::Pending). An
  actual argument this pass cannot follow is waited for after the call.
* The caller does not wait before the call for the launches pending on an
  array actual argument that only the twin can name; the twin starts as if
  launches were pending on what its array dummies hold, and waits where it
  accesses that on the host. For any other actual argument, for what other
  procedures can reach, and while a launch that may have recorded a stop is
  pending (the twin could not replay it), the caller waits before the call
  as before any call.

Storage a launch only reads the runtime may hand the kernel in place (the
unified and managed memory models), so a GpuWait -- wait for the launches
that read host memory in place, copying nothing back -- goes before a host
statement that may change or free storage a pending launch reads, and
where any GpuSync would go. A statement that may access such storage waits,
except where it only reads it: by evaluating an expression, such as the value
of an assignment, or as an actual argument of an intent(in) dummy of a
deferring twin that cannot change it through a pointer.

A local array of a procedure or `block` that a pending launch reads, where
its storage ends, is handed to GpuDiscard instead: the runtime waits there
only for a launch that uses that storage in place (one whose array is copied
to the device need not be waited for), not for every launch that reads host
memory in place, such as one that reads the arrays of the caller.

Where the host does not change an array a launch since the last wait read
until a later launch reads it, a GpuKeep naming it goes right before the
GpuSync or GpuWait, so that a runtime that copies what a kernel reads to the
device may keep that copy past the wait (see "Keeping copies" below).

After every GpuSync, the stops each pending launch may have recorded are
replayed (GpuKernelLaunch.stop_replay, see gpu_offload_stop.h), in launch
order; the status buffers are cleared once, on entry to the procedure.

What a statement accesses is worked out over access paths (access_paths.h).

Control flow is structured: `if`, `select case`, loops (to a fixed point),
`block` and `associate`, `exit` and `cycle`. A local of a `block` that a
pending launch writes, at the end of the block or where `exit` or `cycle`
leaves it, and an array the host deallocates, are not waited for: the host
does not need the values, so a GpuDiscard tells the runtime to forget them.
*/

namespace {

using namespace AccessPaths;

struct State {
    bool reachable = true;
    // Any launch may be pending.
    bool any = false;
    // What the pending launches may write, sorted; `Path::pointee()` for a
    // write through a pointer that may point to anything, and a path rooted
    // at the pointer for one that the procedure never associates
    // (`may_point_anywhere` holds for both).
    std::vector<Path> written;
    // What the launches since the last wait may read and not write, sorted,
    // in the same form as `written`.
    std::vector<Path> read;
    // The pending launches that may have recorded a stop, in launch order.
    std::vector<ASR::GpuKernelLaunch_t*> stops;

    static State unreachable() {
        State s;
        s.reachable = false;
        return s;
    }

    void clear() {
        any = false;
        written.clear();
        read.clear();
        stops.clear();
    }

    static void add_path(std::vector<Path> &paths, const Path &p) {
        auto it = std::lower_bound(paths.begin(), paths.end(), p);
        if (it == paths.end() || !(*it == p)) paths.insert(it, p);
    }

    void add_written(const Path &p) {
        add_path(written, p);
    }

    void add_read(const Path &p) {
        add_path(read, p);
    }

    void add_stop(ASR::GpuKernelLaunch_t *launch) {
        if (std::find(stops.begin(), stops.end(), launch) == stops.end()) {
            stops.push_back(launch);
        }
    }

    void join(const State &o) {
        if (!o.reachable) return;
        if (!reachable) {
            *this = o;
            return;
        }
        any = any || o.any;
        for (const Path &p : o.written) add_written(p);
        for (const Path &p : o.read) add_read(p);
        for (ASR::GpuKernelLaunch_t *l : o.stops) add_stop(l);
    }

    bool operator==(const State &o) const {
        return reachable == o.reachable && any == o.any
            && written == o.written && read == o.read && stops == o.stops;
    }
};


// The deferring twins of the procedures that have one (see the comment at
// the top), and what the twins this pass has placed the syncs of may leave
// pending.
struct Deferral {
    // A procedure's twin.
    std::map<ASR::Function_t*, ASR::Function_t*> twin;
    std::set<ASR::Function_t*> twins;
    // The twins of the modules of this translation unit, whose bodies this
    // pass sees as they are compiled.
    std::set<ASR::Function_t*> own;
    // For a twin of `own` whose syncs are placed: what the launches pending
    // at its exits may write and read, as paths rooted at its dummies, with
    // the index of the dummy.
    struct Pending {
        bool known = false;
        std::vector<std::pair<size_t, Path>> written;
        std::vector<std::pair<size_t, Path>> read;
    };
    std::map<ASR::Function_t*, Pending> pending;
    // The procedures whose syncs are placed or being placed.
    std::set<ASR::Function_t*> started;
    // Places the syncs of a procedure, once.
    std::function<void(ASR::Function_t*)> place;

    // The twin a call of `name` reaches or is redirected to, or nullptr.
    ASR::Function_t *callee(ASR::symbol_t *name, ASR::expr_t *dt) const {
        if (dt != nullptr) return nullptr;
        ASR::symbol_t *c = ASRUtils::symbol_get_past_external(name);
        if (!ASR::is_a<ASR::Function_t>(*c)) return nullptr;
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(c);
        if (twins.count(fn) > 0) return fn;
        auto it = twin.find(fn);
        return it == twin.end() ? nullptr : it->second;
    }
};

class LaunchFinder : public ASR::BaseWalkVisitor<LaunchFinder> {
public:
    bool found = false;
    // Counts a call of a procedure with a deferring twin as a launch.
    const Deferral *deferral = nullptr;
    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &/*x*/) {
        found = true;
    }
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        if (deferral != nullptr && deferral->callee(x.m_name, x.m_dt)) {
            found = true;
        }
        BaseWalkVisitor::visit_SubroutineCall(x);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
            ASRUtils::symbol_get_past_external(x.m_m));
        for (size_t i = 0; i < block->n_body && !found; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *block = ASR::down_cast<ASR::AssociateBlock_t>(
            ASRUtils::symbol_get_past_external(x.m_m));
        for (size_t i = 0; i < block->n_body && !found; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

bool launches(ASR::stmt_t **body, size_t n_body,
        const Deferral *deferral = nullptr) {
    LaunchFinder f;
    f.deferral = deferral;
    for (size_t i = 0; i < n_body && !f.found; i++) f.visit_stmt(*body[i]);
    return f.found;
}

// Whether a value of type `t` refers to variables it does not hold: a
// Reference, or a derived type with one, such as a frame (nested_vars).
bool refers(ASR::ttype_t *t) {
    t = ASRUtils::extract_type(t);
    if (ASR::is_a<ASR::Reference_t>(*t)) return true;
    if (!ASR::is_a<ASR::StructType_t>(*t)) return false;
    ASR::StructType_t *st = ASR::down_cast<ASR::StructType_t>(t);
    for (size_t i = 0; i < st->n_data_member_types; i++) {
        if (ASR::is_a<ASR::Reference_t>(*st->m_data_member_types[i])) {
            return true;
        }
    }
    return false;
}

// Whether the module procedure `fn` of `m` gets a deferring twin. Only its
// declaration decides, as it is in the module file, so that every
// translation unit that uses the module decides the same.
bool gets_twin(ASR::Module_t *m, ASR::Function_t *fn) {
    if (m->m_parent_module != nullptr || m->m_intrinsic
            || fn->m_gpu != nullptr || fn->m_return_var != nullptr) {
        return false;
    }
    // A procedure of a module loaded from its module file that another
    // object file defines has the ABI ExternalUndefined there; that object
    // defines no twin if it is the module's own, compiled without a device.
    // A bind(c) procedure is an interface.
    ASR::FunctionType_t *type = ASRUtils::get_FunctionType(fn);
    if (type->m_deftype != ASR::deftypeType::Implementation
            || (type->m_abi != ASR::abiType::Source
                && type->m_abi != ASR::abiType::ExternalUndefined)
            || (type->m_abi == ASR::abiType::ExternalUndefined
                && m->m_host_only_object)
            || type->m_elemental || type->m_module
            || type->m_exec_space != ASR::exec_spaceType::Host) {
        return false;
    }
    bool data = false;
    for (size_t i = 0; i < fn->n_args; i++) {
        if (fn->m_args[i] == nullptr
                || !ASR::is_a<ASR::Var_t>(*fn->m_args[i])) {
            return false;
        }
        ASR::symbol_t *d = ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v;
        if (!ASR::is_a<ASR::Variable_t>(*d)) return false;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(d);
        if (v->m_target_attr || ASRUtils::is_pointer(v->m_type)
                || ASR::is_a<ASR::FunctionType_t>(*v->m_type)
                || refers(v->m_type)) {
            return false;
        }
        if (ASRUtils::is_array(v->m_type) || ASR::is_a<ASR::StructType_t>(
                *ASRUtils::extract_type(v->m_type))) {
            data = true;
        }
    }
    return data;
}

// The variables of `scope` an expression or type names.
class DeclarationNames : public ASR::BaseWalkVisitor<DeclarationNames> {
public:
    SymbolTable *scope;
    std::vector<ASR::Variable_t*> found;
    explicit DeclarationNames(SymbolTable *scope_) : scope(scope_) {}
    void visit_Var(const ASR::Var_t &x) {
        if (ASR::is_a<ASR::Variable_t>(*x.m_v)
                && ASR::down_cast<ASR::Variable_t>(x.m_v)->m_parent_symtab
                    == scope) {
            found.push_back(ASR::down_cast<ASR::Variable_t>(x.m_v));
        }
    }
};

const std::string twin_suffix = "__gpu_deferred";

static bool ends_with(const std::string &s, const std::string &end) {
    return s.size() >= end.size()
        && s.compare(s.size() - end.size(), end.size(), end) == 0;
}

// The name of `fn` without the suffix of a specific procedure named like its
// generic interface, and that suffix.
static std::pair<std::string, std::string> split_generic(
        ASR::Function_t *fn) {
    std::string name = fn->m_name;
    const std::string &generic = ASRUtils::genericprocedure_suffix;
    if (name.size() > generic.size() && ends_with(name, generic)) {
        name.resize(name.size() - generic.size());
        return {name, generic};
    }
    return {name, ""};
}

// The name of the twin of `fn`, which keeps the generic suffix last.
std::string twin_name(ASR::Function_t *fn) {
    std::pair<std::string, std::string> n = split_generic(fn);
    return n.first + twin_suffix + n.second;
}

// Whether `fn` is a twin this pass made when it ran before.
bool made_twin(ASR::Function_t *fn) {
    return ends_with(split_generic(fn).first, twin_suffix);
}

// Moves the body of `fn` into a new procedure of `m`, its twin, and makes
// `fn` call the twin with its own dummies. Returns the twin, or nullptr when
// a symbol of the twin's name exists already.
ASR::Function_t *make_twin(Allocator &al, ASR::Module_t *m,
        ASR::Function_t *fn) {
    std::string name = twin_name(fn);
    SymbolTable *module_scope = m->m_symtab;
    if (module_scope->get_symbol(name) != nullptr) return nullptr;
    const Location &loc = fn->base.base.loc;
    SymbolTable *body_scope = fn->m_symtab;
    // The scope of `fn` from now on: copies of its dummies, and of the
    // symbols from other modules their declarations may name.
    SymbolTable *scope = al.make_new<SymbolTable>(module_scope);
    ASRUtils::SymbolDuplicator dup(al);
    for (auto &item : body_scope->get_scope()) {
        if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) {
            dup.duplicate_symbol(item.second, scope);
        }
    }
    // The dummies, and the variables their declarations name, such as the
    // locals that hold their extents (function_call_in_declaration), and
    // so on.
    std::vector<ASR::Variable_t*> copied;
    std::set<ASR::Variable_t*> seen;
    for (size_t i = 0; i < fn->n_args; i++) {
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(
            ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
        if (seen.insert(v).second) copied.push_back(v);
    }
    for (size_t k = 0; k < copied.size(); k++) {
        DeclarationNames names(body_scope);
        names.visit_ttype(*copied[k]->m_type);
        if (copied[k]->m_symbolic_value) {
            names.visit_expr(*copied[k]->m_symbolic_value);
        }
        if (copied[k]->m_value) names.visit_expr(*copied[k]->m_value);
        for (ASR::Variable_t *v : names.found) {
            if (seen.insert(v).second) copied.push_back(v);
        }
    }
    std::vector<std::pair<ASR::Variable_t*, ASR::Variable_t*>> pairs;
    for (ASR::Variable_t *v : copied) {
        dup.duplicate_symbol(&v->base, scope);
        ASR::symbol_t *copy = scope->get_symbol(v->m_name);
        if (copy == nullptr || !ASR::is_a<ASR::Variable_t>(*copy)) {
            return nullptr;
        }
        ASR::Variable_t *c = ASR::down_cast<ASR::Variable_t>(copy);
        if (c->m_type_declaration != nullptr
                && ASRUtils::symbol_parent_symtab(c->m_type_declaration)
                    == body_scope) {
            c->m_type_declaration = scope->get_symbol(
                ASRUtils::symbol_name(c->m_type_declaration));
        }
        pairs.push_back({v, c});
    }
    dup.remap_local_references(pairs, scope);
    Vec<ASR::expr_t*> args;
    args.reserve(al, fn->n_args);
    Vec<ASR::call_arg_t> call_args;
    call_args.reserve(al, fn->n_args);
    for (size_t i = 0; i < fn->n_args; i++) {
        // The dummies come first in `pairs`, in order.
        ASR::expr_t *var = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
            &pairs[i].second->base));
        args.push_back(al, var);
        ASR::call_arg_t a;
        a.loc = loc;
        a.m_value = var;
        call_args.push_back(al, a);
    }
    // The type of a procedure of a module loaded from its module file may
    // name the variables of its scope that hold its extents: the twin keeps
    // the scope, and `fn` gets a copy that names the copies of them.
    ASRUtils::ExprStmtWithScopeDuplicator signature(al, scope);
    ASR::ttype_t *wrapper_type = signature.duplicate_ttype(
        fn->m_function_signature);
    ASR::symbol_t *twin = ASR::down_cast<ASR::symbol_t>(ASR::make_Function_t(
        al, loc, body_scope, s2c(al, name), fn->m_function_signature,
        fn->m_dependencies, fn->n_dependencies, fn->m_args, fn->n_args,
        fn->m_body, fn->n_body, nullptr, fn->m_access, fn->m_deterministic,
        fn->m_side_effect_free, fn->m_module_file, nullptr, nullptr,
        fn->m_start_name, fn->m_end_name));
    body_scope->asr_owner = (ASR::asr_t*)twin;
    module_scope->add_symbol(name, twin);
    Vec<ASR::stmt_t*> body;
    body.reserve(al, 1);
    body.push_back(al, ASRUtils::STMT(ASR::make_SubroutineCall_t(al, loc,
        twin, nullptr, call_args.p, call_args.n, nullptr, false)));
    Vec<char*> deps;
    deps.reserve(al, 1);
    deps.push_back(al, s2c(al, name));
    scope->asr_owner = (ASR::asr_t*)fn;
    fn->m_symtab = scope;
    fn->m_function_signature = wrapper_type;
    fn->m_args = args.p;
    fn->n_args = args.n;
    fn->m_body = body.p;
    fn->n_body = body.n;
    fn->m_dependencies = deps.p;
    fn->n_dependencies = deps.n;
    return ASR::down_cast<ASR::Function_t>(twin);
}

// Gives every procedure of the modules of `unit` that gets_twin selects its
// twin, the modules loaded from module files included. A procedure that has
// one already, because the pass runs again, keeps it, and a twin gets none.
void make_twins(Allocator &al, ASR::TranslationUnit_t &unit,
        Deferral &deferral) {
    for (auto &item : unit.m_symtab->get_scope()) {
        if (!ASR::is_a<ASR::Module_t>(*item.second)) continue;
        ASR::Module_t *m = ASR::down_cast<ASR::Module_t>(item.second);
        std::vector<ASR::Function_t*> selected;
        std::set<ASR::Function_t*> existing;
        for (auto &member : m->m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Function_t>(*member.second)) continue;
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(
                member.second);
            if (made_twin(fn)) {
                deferral.twins.insert(fn);
                if (!m->m_loaded_from_mod) deferral.own.insert(fn);
                existing.insert(fn);
                continue;
            }
            ASR::symbol_t *t = m->m_symtab->get_symbol(twin_name(fn));
            if (t != nullptr && ASR::is_a<ASR::Function_t>(*t)) {
                ASR::Function_t *twin = ASR::down_cast<ASR::Function_t>(t);
                deferral.twin[fn] = twin;
                deferral.twins.insert(twin);
                if (!m->m_loaded_from_mod) deferral.own.insert(twin);
                existing.insert(fn);
                existing.insert(twin);
            }
        }
        for (auto &member : m->m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Function_t>(*member.second)) continue;
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(
                member.second);
            if (existing.count(fn) == 0 && gets_twin(m, fn)) {
                selected.push_back(fn);
            }
        }
        for (ASR::Function_t *fn : selected) {
            ASR::Function_t *twin = make_twin(al, m, fn);
            if (twin == nullptr) continue;
            deferral.twin[fn] = twin;
            deferral.twins.insert(twin);
            if (!m->m_loaded_from_mod) deferral.own.insert(twin);
        }
    }
}

// The variables a statement of a procedure may associate a pointer with:
// whatever the value of a `=>` names (an associate name, and a pointer
// component of a structure constructor, are bound that way too), the actual
// arguments of a call whose dummy may be a target or a pointer, the
// arguments of an intrinsic subroutine or function (`move_alloc`), what
// `c_loc` takes, and the selector of `select type`. Their subscripts are
// counted too. A dummy that is neither a pointer nor a target leaves no
// pointer associated with its actual argument once the procedure returns
// (every pointer associated with it becomes undefined), unless the procedure
// is not Fortran (`bind(c)`).
class PointerReach : public ASR::BaseWalkVisitor<PointerReach> {
public:
    std::set<ASR::symbol_t*> &vars;
    std::set<ASR::symbol_t*> blocks_seen;
    // Inside one of the expressions above.
    int inside = 0;
    // When set, what a local pointer of this procedure is associated with
    // counts only once the pointer itself is counted (`finish`): what
    // another procedure can reach through a pointer, rather than what a
    // pointer of this procedure may point to.
    SymbolTable *routine = nullptr;
    // Count everything the contained procedures name, whether or not they can
    // run elsewhere (what a call of one of them reaches).
    bool all_host = false;
    std::map<ASR::symbol_t*, std::vector<ASR::expr_t*>> local_targets;
    explicit PointerReach(std::set<ASR::symbol_t*> &vars_) : vars(vars_) {}

    // A pointer, or associate name, that only this procedure can name
    // (unless a procedure contained in it does, see `finish`).
    bool local_pointer(ASR::symbol_t *p) const {
        if (routine == nullptr || p == nullptr
                || !ASR::is_a<ASR::Variable_t>(*p)) {
            return false;
        }
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(p);
        return v->m_intent == ASR::intentType::Local
            && v->m_storage != ASR::storage_typeType::Save
            && enclosing_routine(v->m_parent_symtab) == routine;
    }

    // Counts what the local pointers that are counted are associated with,
    // and the variables of this procedure that a procedure contained in it
    // names, which a call of that procedure may reach.
    void finish() {
        if (routine == nullptr) return;
        // A contained procedure that is only called by name is run by this
        // procedure alone: no call of another procedure reaches the
        // variables it names, nor the frame they are bound in. Once its name
        // is used as a value -- an actual argument, a procedure pointer
        // target -- anything may call it.
        for (auto &item : routine->get_scope()) {
            if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
            ProcedureValues inner(procedure_values);
            inner.visit_symbol(*item.second);
        }
        bool escapes = false;
        for (auto &item : routine->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)
                    && procedure_values.count(item.second) > 0) {
                escapes = true;
            }
        }
        if (all_host) escapes = true;
        if (!escapes) return finish_targets();
        // The one that escapes may call its siblings.
        for (ASR::expr_t *e : bound) reached(e);
        bound.clear();
        for (auto &item : routine->get_scope()) {
            if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
            HostNames names(routine, vars);
            names.visit_symbol(*item.second);
        }
        finish_targets();
    }

    void finish_targets() {
        std::set<ASR::symbol_t*> done;
        bool changed = true;
        while (changed) {
            changed = false;
            for (auto &item : local_targets) {
                if (vars.count(item.first) == 0
                        || !done.insert(item.first).second) {
                    continue;
                }
                for (ASR::expr_t *value : item.second) reached(value);
                changed = true;
            }
        }
    }

    // The procedures whose name is used as a value, and the variables the
    // frames of the contained procedures are bound to.
    std::set<ASR::symbol_t*> procedure_values;
    std::vector<ASR::expr_t*> bound;
    class ProcedureValues : public ASR::BaseWalkVisitor<ProcedureValues> {
    public:
        std::set<ASR::symbol_t*> &values;
        explicit ProcedureValues(std::set<ASR::symbol_t*> &v) : values(v) {}
        void visit_Var(const ASR::Var_t &x) {
            ASR::symbol_t *v = ASRUtils::symbol_get_past_external(x.m_v);
            if (ASR::is_a<ASR::Function_t>(*v)) values.insert(v);
        }
    };

    // The variables of `routine` a procedure contained in it names.
    class HostNames : public ASR::BaseWalkVisitor<HostNames> {
    public:
        SymbolTable *routine;
        std::set<ASR::symbol_t*> &vars;
        HostNames(SymbolTable *routine_, std::set<ASR::symbol_t*> &vars_)
            : routine(routine_), vars(vars_) {}
        void visit_Var(const ASR::Var_t &x) {
            ASR::symbol_t *v = ASRUtils::symbol_get_past_external(x.m_v);
            if (ASR::is_a<ASR::Variable_t>(*v) && enclosing_routine(
                    ASR::down_cast<ASR::Variable_t>(v)->m_parent_symtab)
                        == routine) {
                vars.insert(v);
            }
        }
    };

    void visit_Var(const ASR::Var_t &x) {
        ASR::symbol_t *v = ASRUtils::symbol_get_past_external(x.m_v);
        if (ASR::is_a<ASR::Function_t>(*v)) procedure_values.insert(v);
        if (inside > 0) {
            vars.insert(v);
        }
    }
    void reached(const ASR::expr_t *e) {
        if (e == nullptr) return;
        inside++;
        visit_expr(*e);
        inside--;
    }
    void visit_Associate(const ASR::Associate_t &x) {
        visit_expr(*x.m_target);
        ASR::symbol_t *p = ASR::is_a<ASR::Var_t>(*x.m_target)
            ? ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(x.m_target)->m_v) : nullptr;
        if (local_pointer(p)) {
            local_targets[p].push_back(x.m_value);
            return;
        }
        reached(x.m_value);
    }
    // A frame refers to the variable: the procedures it is handed to can
    // reach it.
    void visit_BindReference(const ASR::BindReference_t &x) {
        visit_expr(*x.m_reference);
        if (routine != nullptr && !all_host) {
            // Counted by `finish`, if a contained procedure can run elsewhere.
            bound.push_back(x.m_variable);
        } else {
            reached(x.m_variable);
        }
    }
    void visit_SelectType(const ASR::SelectType_t &x) {
        reached(x.m_selector);
        BaseWalkVisitor::visit_SelectType(x);
    }
    // Whether a call of `name` with `n_args` arguments may leave a pointer
    // associated with its argument `i`.
    static bool may_keep(ASR::symbol_t *name, size_t n_args, size_t i) {
        ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(name);
        if (!ASR::is_a<ASR::Function_t>(*callee)) return true;
        ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(callee);
        if (ASRUtils::get_FunctionType(fn)->m_abi == ASR::abiType::BindC
                || fn->n_args != n_args || fn->m_args[i] == nullptr
                || !ASR::is_a<ASR::Var_t>(*fn->m_args[i])) {
            return true;
        }
        ASR::symbol_t *dummy = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v);
        if (!ASR::is_a<ASR::Variable_t>(*dummy)) return true;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(dummy);
        return v->m_target_attr || ASRUtils::is_pointer(v->m_type);
    }
    void call(ASR::symbol_t *name, ASR::call_arg_t *args, size_t n_args,
            ASR::expr_t *dt) {
        for (size_t i = 0; i < n_args; i++) {
            if (!args[i].m_value) continue;
            if (dt != nullptr || may_keep(name, n_args, i)) {
                reached(args[i].m_value);
            } else {
                visit_expr(*args[i].m_value);
            }
        }
        reached(dt);
    }
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        call(x.m_name, x.m_args, x.n_args, x.m_dt);
    }
    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        call(x.m_name, x.m_args, x.n_args, x.m_dt);
    }
    void visit_IntrinsicImpureSubroutine(
            const ASR::IntrinsicImpureSubroutine_t &x) {
        inside++;
        BaseWalkVisitor::visit_IntrinsicImpureSubroutine(x);
        inside--;
    }
    void visit_IntrinsicImpureFunction(
            const ASR::IntrinsicImpureFunction_t &x) {
        // Asking whether an array is allocated associates nothing with it.
        if (x.m_impure_intrinsic_id == static_cast<int64_t>(
                ASRUtils::IntrinsicImpureFunctions::Allocated)) {
            BaseWalkVisitor::visit_IntrinsicImpureFunction(x);
            return;
        }
        inside++;
        BaseWalkVisitor::visit_IntrinsicImpureFunction(x);
        inside--;
    }
    void visit_StructConstructor(const ASR::StructConstructor_t &x) {
        inside++;
        BaseWalkVisitor::visit_StructConstructor(x);
        inside--;
    }
    void visit_PointerToCPtr(const ASR::PointerToCPtr_t &x) {
        reached(x.m_arg);
    }
    void visit_GetPointer(const ASR::GetPointer_t &x) {
        reached(x.m_arg);
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks_seen.insert(b).second) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::symbol_t *b = ASRUtils::symbol_get_past_external(x.m_m);
        if (!blocks_seen.insert(b).second) return;
        ASR::AssociateBlock_t *block = ASR::down_cast<ASR::AssociateBlock_t>(
            b);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

class RoutineSync {
public:
    RoutineSync(Allocator &al_, SymbolTable *routine_,
            const PointerTargets &pointers_,
            const std::set<ASR::symbol_t*> &reached_,
            const std::set<ASR::symbol_t*> &escaped_,
            const std::set<ASR::symbol_t*> &escaped_host_, Deferral &deferral_,
            ASR::Function_t *self_)
        : al(al_), routine(routine_), pointers(pointers_),
          reached(reached_), escaped(escaped_), escaped_host(escaped_host_),
          deferral(deferral_),
          self(self_),
          twin(self_ != nullptr && deferral_.twins.count(self_) > 0) {}

    // The new body of the routine.
    Vec<ASR::stmt_t*> run(ASR::stmt_t **body, size_t n_body,
            const Location &loc) {
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n_body + 4);
        State end = process_body(body, n_body, entry(), true, &out);
        if (end.reachable && end.any) leave(end, true, &out, loc);
        place_keeps(out, loc);
        // Each status buffer is cleared once, before its first launch.
        Vec<ASR::stmt_t*> result;
        result.reserve(al, out.size() + status_buffers.size());
        ASRUtils::ASRBuilder b(al, loc);
        for (ASR::expr_t *status : status_buffers) {
            ASR::ttype_t *int4 = ASRUtils::TYPE(ASR::make_Integer_t(al, loc,
                4));
            Vec<ASR::array_index_t> first;
            first.reserve(al, 1);
            ASR::array_index_t ai;
            ai.loc = loc;
            ai.m_left = nullptr;
            ai.m_right = b.i32(1);
            ai.m_step = nullptr;
            first.push_back(al, ai);
            result.push_back(al, b.Assignment(ASRUtils::EXPR(
                ASR::make_ArrayItem_t(al, loc, status, first.p, first.n,
                    int4, ASR::arraystorageType::ColMajor, nullptr, false)),
                b.i32(0)));
        }
        for (size_t i = 0; i < out.size(); i++) result.push_back(al, out[i]);
        return result;
    }

private:
    Allocator &al;
    SymbolTable *routine;
    const PointerTargets &pointers;
    // What the procedure may associate a pointer with (PointerReach).
    const std::set<ASR::symbol_t*> &reached;
    // Of those, what other procedures can reach through a pointer: not
    // what only pointers that no other procedure can name point to.
    const std::set<ASR::symbol_t*> &escaped;
    // The same, with every variable a contained procedure names: what a
    // call of a contained procedure of this one may access.
    const std::set<ASR::symbol_t*> &escaped_host;
    Deferral &deferral;
    // The procedure, nullptr for a program.
    ASR::Function_t *self;
    // Whether it is a deferring twin.
    bool twin;
    // Which procedures may act outside the program.
    std::map<ASR::Function_t*, bool> outside;
    // What calls of procedures with a known body access.
    CallSummaries summaries{outside};
    std::vector<SymbolTable*> blocks;
    struct Loop {
        ASR::stmt_t *loop;
        std::string name;
        size_t blocks;
        // A while loop whose test reads what a launch in its body writes
        // waits at the end of every iteration, `cycle` included.
        bool sync_at_back_edge;
        State at_cycle;
        State at_exit;
    };
    std::vector<Loop> loops;
    std::vector<ASR::expr_t*> status_buffers;
    std::vector<ASR::symbol_t*> status_symbols;
    // What the declarations of twins allow them to leave pending.
    std::map<ASR::Function_t*, Deferral::Pending> contracts;
    // How many GpuSync and GpuWait have been placed, in either mode.
    size_t n_syncs = 0;
    size_t n_waits = 0;

    // The index of the dummy argument of the procedure `root` is, or -1.
    // A dummy passed by value is not counted: its storage is the
    // procedure's own.
    int dummy_index(ASR::symbol_t *root) const {
        if (self == nullptr || root == nullptr) return -1;
        for (size_t i = 0; i < self->n_args; i++) {
            ASR::symbol_t *d = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(self->m_args[i])->m_v);
            if (d == root) {
                return ASR::down_cast<ASR::Variable_t>(d)->m_value_attr
                    ? -1 : (int)i;
            }
        }
        return -1;
    }

    // Whether the call of a twin hands it its dummy `d` without waiting
    // for the launches pending on it (call_twin): an array dummy. The
    // caller waits for the launches pending on any other actual argument,
    // a scalar the twin is likely to read on the host.
    static bool handed_pending(ASR::Variable_t *d) {
        return !d->m_value_attr && ASRUtils::is_array(d->m_type);
    }

    // What is pending on entry: nothing, except in a twin, whose callers do
    // not wait for what launches they made may write or read in what its
    // array dummies hold.
    State entry() {
        State s;
        if (!twin) return s;
        for (size_t i = 0; i < self->n_args; i++) {
            ASR::symbol_t *d = ASR::down_cast<ASR::Var_t>(self->m_args[i])
                ->m_v;
            if (dummy_index(d) < 0
                    || !handed_pending(ASR::down_cast<ASR::Variable_t>(d))) {
                continue;
            }
            Path p;
            p.root = d;
            s.add_written(p);
            s.add_read(p);
            s.any = true;
        }
        return s;
    }

    // At a `return` or the end of the procedure, with launches pending.
    // A procedure waits for them, except a twin, which leaves pending the
    // launches that write and read only what its dummies hold, for its
    // caller to wait for, once its locals are discarded: the locals end
    // here, the stop replays read status buffers that are locals, and
    // anything else the caller cannot follow.
    void leave(State &s, bool emit, Vec<ASR::stmt_t*> *out,
            const Location &loc) {
        // A launch that may have recorded a stop is waited for, and its stop
        // replayed, before anything is discarded: its status buffer is a
        // local of the procedure.
        if (!twin || !s.stops.empty()) {
            sync(s, emit, out, loc);
            return;
        }
        leave_blocks(0, s, emit, out, loc);
        discard_locals(routine, s, emit, out, loc);
        if (!s.any) return;
        bool deferrable = true;
        for (const Path &p : s.written) {
            if (dummy_index(p.root) < 0) deferrable = false;
        }
        for (const Path &p : s.read) {
            if (dummy_index(p.root) < 0) deferrable = false;
        }
        if (!deferrable) {
            sync(s, emit, out, loc);
            return;
        }
        if (!emit) return;
        Deferral::Pending &pending = deferral.pending[self];
        for (const Path &p : s.written) {
            // Only a launch of the caller writes what an intent(in) dummy
            // holds, which the caller knows of.
            if (ASR::down_cast<ASR::Variable_t>(p.root)->m_intent
                    == ASR::intentType::In) {
                continue;
            }
            std::pair<size_t, Path> e((size_t)dummy_index(p.root), p);
            if (std::find(pending.written.begin(), pending.written.end(), e)
                    == pending.written.end()) {
                pending.written.push_back(e);
            }
        }
        for (const Path &p : s.read) {
            std::pair<size_t, Path> e((size_t)dummy_index(p.root), p);
            if (std::find(pending.read.begin(), pending.read.end(), e)
                    == pending.read.end()) {
                pending.read.push_back(e);
            }
        }
    }

    // Whether no launch can still use the storage of the dummy `d` once the
    // launch is made: a scalar of an intrinsic type, which a launch is handed
    // by value (a kernel scalar argument), and which a kernel never writes
    // (the host combines a reduction into it after a sync), such as the
    // extents pass_array_by_data adds.
    static bool launch_copies(ASR::Variable_t *d) {
        ASR::ttype_t *type = d->m_type;
        if (ASRUtils::is_array(type) || ASRUtils::is_pointer(type)
                || ASRUtils::is_allocatable(type)) {
            return false;
        }
        return ASR::is_a<ASR::Integer_t>(*type)
            || ASR::is_a<ASR::UnsignedInteger_t>(*type)
            || ASR::is_a<ASR::Real_t>(*type)
            || ASR::is_a<ASR::Complex_t>(*type)
            || ASR::is_a<ASR::Logical_t>(*type);
    }

    // What the launches a twin leaves pending may write and read, as
    // paths rooted at its dummies: what its syncs were placed to leave,
    // when this pass placed them, otherwise what its declaration allows --
    // writes of what a dummy that is not intent(in) holds, and reads of what
    // any dummy holds, except a scalar a launch is handed by value.
    const Deferral::Pending &twin_pending(ASR::Function_t *t) {
        // A twin of a module loaded from its module file whose body this
        // translation unit has (the default mode compiles the procedures of
        // every module it uses) is placed here as well, so what it leaves
        // pending is known as for a twin of the unit's own modules.
        bool has_body = false;
        if (deferral.own.count(t) == 0 && deferral.twins.count(t) > 0
                && t->n_body > 0) {
            ASR::FunctionType_t *tt = ASRUtils::get_FunctionType(t);
            has_body = tt->m_abi == ASR::abiType::Source
                && tt->m_deftype == ASR::deftypeType::Implementation;
        }
        if (deferral.own.count(t) > 0 || has_body) {
            deferral.place(t);
            auto it = deferral.pending.find(t);
            if (it != deferral.pending.end() && it->second.known) {
                return it->second;
            }
        }
        auto c = contracts.find(t);
        if (c != contracts.end()) return c->second;
        Deferral::Pending &p = contracts[t];
        for (size_t i = 0; i < t->n_args; i++) {
            ASR::Variable_t *d = ASR::down_cast<ASR::Variable_t>(
                ASR::down_cast<ASR::Var_t>(t->m_args[i])->m_v);
            if (d->m_value_attr || launch_copies(d)) continue;
            Path q;
            q.root = &d->base;
            if (d->m_intent != ASR::intentType::In) {
                p.written.push_back({i, q});
            }
            p.read.push_back({i, q});
        }
        return p;
    }

    // A call of a twin `t`, which `x` now names. The twin waits for what
    // the launches pending here may write into what its array dummies
    // hold, as far as it accesses that on the host, so this does not, for
    // such an actual argument that only the twin can name (a local or dummy
    // no pointer reaches). After the call, what the twin leaves pending
    // writes and reads the actual arguments: an actual argument this pass
    // cannot follow there is waited for at once.
    void call_twin(ASR::SubroutineCall_t &x, ASR::Function_t *t, State &s,
            bool emit, Vec<ASR::stmt_t*> *out) {
        const Location &loc = x.base.base.loc;
        Accesses acc;
        AccessCollector c(acc, routine, pointers, outside, &summaries);
        // What the call only reads: the actual arguments of intent(in)
        // dummies the twin cannot change through a pointer.
        Accesses read;
        AccessCollector r(read, routine, pointers, outside, &summaries);
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::expr_t *arg = x.m_args[i].m_value;
            if (arg == nullptr) continue;
            ASR::Variable_t *dummy = i < t->n_args
                ? ASR::down_cast<ASR::Variable_t>(ASR::down_cast<ASR::Var_t>(
                    t->m_args[i])->m_v) : nullptr;
            bool handed = dummy != nullptr && handed_pending(dummy);
            std::vector<Path> paths;
            std::vector<ASR::expr_t*> subscripts;
            if (!handed || !designated_paths(arg, paths, &subscripts)) {
                if (dummy != nullptr && only_read(dummy, arg)) {
                    r.visit_expr(*arg);
                } else {
                    c.visit_expr(*arg);
                }
                continue;
            }
            for (ASR::expr_t *sub : subscripts) c.visit_expr(*sub);
            for (const Path &p : paths) {
                if (is_pointer_variable(p.root)) acc.globals = true;
                if ((!reachable_elsewhere(p.root, routine)
                            && escaped.count(p.root) == 0)
                        || unnamed_by_others(p.root, escaped)) {
                    continue;
                }
                acc.paths.push_back(p);
            }
        }
        // The twin may access what other procedures can reach, and stop.
        acc.globals = true;
        if (acts_outside(t, outside)) acc.observable = true;
        if (!s.stops.empty()) sync(s, emit, out, loc);
        before(read, s, emit, out, loc);
        before(acc, s, emit, out, loc, true);
        if (emit) out->push_back(al, &x.base);
        const Deferral::Pending &pending = twin_pending(t);
        bool wait_written = false, wait_read = false;
        for (int pass = 0; pass < 2; pass++) {
            const std::vector<std::pair<size_t, Path>> &paths = pass == 0
                ? pending.written : pending.read;
            for (const auto &e : paths) {
                if (e.first >= x.n_args || x.m_args[e.first].m_value
                        == nullptr) {
                    continue;
                }
                std::vector<Path> actual;
                if (!designated_paths(x.m_args[e.first].m_value, actual,
                        nullptr)) {
                    (pass == 0 ? wait_written : wait_read) = true;
                    continue;
                }
                for (Path &p : actual) {
                    p.members.insert(p.members.end(),
                        e.second.members.begin(), e.second.members.end());
                    p.shallow = e.second.shallow;
                    if (pass == 0) s.add_written(p);
                    else s.add_read(p);
                }
            }
        }
        s.any = true;
        if (wait_written) {
            sync(s, emit, out, loc);
        } else if (wait_read) {
            wait(s, emit, out, loc);
        }
    }

    // Whether a call only reads its actual argument `arg` of the dummy `d`:
    // an intent(in) dummy, which the callee does not change, unless through
    // a pointer the value holds or the dummy is.
    static bool only_read(ASR::Variable_t *d, ASR::expr_t *arg) {
        return d->m_intent == ASR::intentType::In && !d->m_value_attr
            && !ASRUtils::is_pointer(d->m_type) && !may_hold_pointer(arg);
    }

    // The path of the designator `e` (path_of), or, through a local pointer
    // the procedure associates with one of a few arrays, one path for each
    // (alternative_paths), and the subscripts evaluated on the way.
    bool designated_paths(ASR::expr_t *e, std::vector<Path> &paths,
            std::vector<ASR::expr_t*> *subscripts) {
        Path p;
        std::vector<ASR::expr_t*> own;
        if (path_of(pointers, e, p, subscripts ? &own : nullptr)) {
            paths.push_back(p);
        } else {
            own.clear();
            if (!alternative_paths(pointers, e, paths,
                    subscripts ? &own : nullptr)) {
                return false;
            }
        }
        if (subscripts) {
            subscripts->insert(subscripts->end(), own.begin(), own.end());
        }
        return true;
    }

    // Makes the call `x` name the twin of the procedure it calls, which
    // `t` is.
    void redirect(ASR::SubroutineCall_t &x, ASR::Function_t *t) {
        if (ASRUtils::symbol_get_past_external(x.m_name) == &t->base) return;
        if (!ASR::is_a<ASR::ExternalSymbol_t>(*x.m_name)) {
            x.m_name = &t->base;
        } else {
            ASR::ExternalSymbol_t *ext = ASR::down_cast<ASR::ExternalSymbol_t>(
                x.m_name);
            std::string name = t->m_name;
            ASR::symbol_t *s = ext->m_parent_symtab->get_symbol(name);
            if (s == nullptr) {
                s = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
                    al, x.base.base.loc, ext->m_parent_symtab, s2c(al, name),
                    &t->base, ext->m_module_name, nullptr, 0, s2c(al, name),
                    ext->m_access));
                ext->m_parent_symtab->add_symbol(name, s);
            }
            x.m_name = s;
        }
        x.m_original_name = nullptr;
    }

    // Waits for every pending launch, and replays the stops they recorded.
    void sync(State &s, bool emit, Vec<ASR::stmt_t*> *out,
            const Location &loc) {
        n_syncs++;
        if (emit) {
            out->push_back(al, ASRUtils::STMT(ASR::make_GpuSync_t(al, loc)));
            ASRUtils::ExprStmtDuplicator dup(al);
            for (ASR::GpuKernelLaunch_t *launch : s.stops) {
                for (size_t i = 0; i < launch->n_stop_replay; i++) {
                    out->push_back(al,
                        dup.duplicate_stmt(launch->m_stop_replay[i]));
                }
            }
        }
        s.clear();
    }

    // Waits for the launches that read host memory in place, which leaves
    // what the pending launches write on the device.
    void wait(State &s, bool emit, Vec<ASR::stmt_t*> *out,
            const Location &loc) {
        n_waits++;
        if (emit) {
            out->push_back(al, ASRUtils::STMT(ASR::make_GpuWait_t(al, loc)));
        }
        s.read.clear();
    }

    // Whether `root` is a target local of a `block` (or `associate`) that
    // no pointer can point into: nothing in the procedure associates a
    // pointer with it or hands it to another procedure, and only the
    // construct can name it.
    bool out_of_reach(ASR::symbol_t *root) const {
        if (root == nullptr || !ASR::is_a<ASR::Variable_t>(*root)) {
            return false;
        }
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(root);
        return v->m_target_attr && !is_pointer_variable(root)
            && v->m_intent == ASR::intentType::Local
            && v->m_storage != ASR::storage_typeType::Save
            && v->m_parent_symtab != routine
            && enclosing_routine(v->m_parent_symtab) == routine
            && reached.count(root) == 0;
    }

    // Whether `root` is a local of this procedure that no pointer another
    // procedure can reach points to, and no procedure other than this one
    // (and its contained procedures that only it calls) names. The
    // procedures this one calls cannot access it, though a local pointer of
    // this procedure that points to it can (`may_overlap`).
    bool unnamed_by_others(ASR::symbol_t *root,
            const std::set<ASR::symbol_t*> &escaped) const {
        if (root == nullptr || !ASR::is_a<ASR::Variable_t>(*root)) {
            return false;
        }
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(root);
        return v->m_intent == ASR::intentType::Local
            && !is_pointer_variable(root)
            && v->m_storage != ASR::storage_typeType::Save
            && enclosing_routine(v->m_parent_symtab) == routine
            && escaped.count(root) == 0;
    }

    bool may_overlap(const Path &a, const Path &b) const {
        if (a.root != b.root
                && (out_of_reach(a.root) || out_of_reach(b.root))) {
            return false;
        }
        // Generated associations can take the address of non-TARGET storage.
        // That is a body-local alias fact, not a change to the dummy's ABI.
        if ((may_point_anywhere(a.root) && reached.count(b.root) > 0)
                || (may_point_anywhere(b.root) && reached.count(a.root) > 0)) {
            return true;
        }
        return overlap(a, b);
    }

    bool conflicts(const Accesses &acc, const State &s) {
        return conflicts(acc, s.written);
    }

    // Whether `acc` may access storage one of `paths` names.
    bool conflicts(const Accesses &acc, const std::vector<Path> &paths) {
        if (paths.empty()) return false;
        if (acc.everything) return true;
        const std::set<ASR::symbol_t*> &esc = acc.contained_call
            ? escaped_host : escaped;
        for (const Path &w : paths) {
            if (acc.globals && (reachable_elsewhere(w.root, routine)
                        || esc.count(w.root) > 0)
                    && !out_of_reach(w.root)
                    && !unnamed_by_others(w.root, esc)) {
                return true;
            }
            for (const Path &a : acc.paths) {
                if (may_overlap(a, w)) return true;
            }
        }
        return false;
    }

    // Whether evaluating an expression with these accesses may change what
    // a pending launch reads: only through a procedure it calls.
    bool expression_changes_read(const Accesses &acc, const State &s) {
        if (!acc.everything && !acc.globals) return false;
        return conflicts(acc, s.read);
    }

    // Waits before a statement with these accesses when it needs to. The
    // accesses of a statement (`statement`) may change what they access;
    // those of an expression only read it, except through the procedures
    // it calls.
    void before(const Accesses &acc, State &s, bool emit,
            Vec<ASR::stmt_t*> *out, const Location &loc,
            bool statement = false) {
        if (conflicts(acc, s) || (acc.observable && !s.stops.empty())) {
            sync(s, emit, out, loc);
        } else if (statement ? conflicts(acc, s.read)
                : expression_changes_read(acc, s)) {
            wait(s, emit, out, loc);
        }
    }

    // Whether `scope` has unsaved storage a pointer may point to, including
    // storage reached through compiler-generated associations.
    bool has_target_locals(SymbolTable *scope) const {
        for (auto &item : scope->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(
                item.second);
            if ((v->m_target_attr || reached.count(item.second) > 0)
                    && v->m_storage != ASR::storage_typeType::Save
                    && !out_of_reach(item.second)) {
                return true;
            }
        }
        return false;
    }

    // Whether `root` is a local variable of `scope`, not a dummy argument
    // or the result of the procedure whose scope it is.
    static bool is_local(ASR::symbol_t *root, SymbolTable *scope) {
        return !may_point_anywhere(root) && ASR::is_a<ASR::Variable_t>(*root)
            && ASR::down_cast<ASR::Variable_t>(root)->m_parent_symtab == scope
            && ASR::down_cast<ASR::Variable_t>(root)->m_intent
                == ASR::intentType::Local;
    }

    // Whether the storage of the local `root` of `scope` ends with the scope
    // and is an array the runtime can be handed by its address and size.
    static bool ending_array(ASR::symbol_t *root, SymbolTable *scope) {
        if (!is_local(root, scope)) return false;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(root);
        return v->m_storage != ASR::storage_typeType::Save
            && ASRUtils::is_array(v->m_type);
    }

    // Tells the runtime that the locals of `scope` end here: it forgets
    // what pending launches write into them, and waits for the pending
    // launches that read them in place, if any (lfortran_gpu_discard).
    void discard_locals(SymbolTable *scope, State &s, bool emit,
            Vec<ASR::stmt_t*> *out, const Location &loc) {
        // A local a pending launch reads in place ends too: its storage
        // must outlive the read. The runtime knows whether a launch reads
        // the storage of a whole array in place, and waits for it when the
        // array is discarded; only another local, or what a pointer may
        // point to, makes the host wait for every launch that reads host
        // memory in place.
        bool must_wait = false;
        for (const Path &p : s.read) {
            if (may_point_anywhere(p.root) && has_target_locals(scope)) {
                must_wait = true;
            } else if (is_local(p.root, scope)) {
                must_wait = must_wait || !ending_array(p.root, scope)
                    || !p.members.empty() || p.shallow;
            }
        }
        Vec<ASR::expr_t*> vars;
        vars.reserve(al, 1);
        if (must_wait) {
            wait(s, emit, out, loc);
        } else {
            std::vector<Path> read_kept;
            for (const Path &p : s.read) {
                if (!is_local(p.root, scope)) {
                    read_kept.push_back(p);
                    continue;
                }
                vars.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                    p.root)));
            }
            s.read = read_kept;
        }
        std::vector<Path> kept;
        bool must_sync = false;
        // The status buffers of the pending launches that may have recorded
        // a stop: the replay after the wait reads them.
        std::set<ASR::symbol_t*> statuses;
        for (ASR::GpuKernelLaunch_t *l : s.stops) {
            statuses.insert(ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(l->m_stop_status)->m_v));
        }
        for (const Path &p : s.written) {
            if (statuses.count(p.root) > 0) {
                must_sync = true;
                kept.push_back(p);
                continue;
            }
            if (may_point_anywhere(p.root) && has_target_locals(scope)) {
                // What a pointer may point to: a target local of `scope`
                // too.
                must_sync = true;
            }
            // A write through a local pointer is not into the pointer's
            // own storage.
            bool local = is_local(p.root, scope)
                && ASR::down_cast<ASR::Variable_t>(p.root)->m_storage
                    != ASR::storage_typeType::Save;
            if (!local) {
                kept.push_back(p);
                continue;
            }
            if (!p.members.empty() || p.shallow) {
                must_sync = true;
                kept.push_back(p);
                continue;
            }
            bool listed = false;
            for (size_t i = 0; i < vars.size(); i++) {
                listed = listed || ASR::down_cast<ASR::Var_t>(vars[i])->m_v
                    == p.root;
            }
            if (!listed) {
                vars.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                    p.root)));
            }
        }
        if (must_sync) {
            sync(s, emit, out, loc);
            return;
        }
        if (vars.size() == 0) return;
        if (emit) {
            out->push_back(al, ASRUtils::STMT(ASR::make_GpuDiscard_t(al, loc,
                vars.p, vars.n)));
        }
        s.written = kept;
    }

    void leave_blocks(size_t depth, State &s, bool emit,
            Vec<ASR::stmt_t*> *out, const Location &loc) {
        for (size_t i = blocks.size(); i > depth; i--) {
            discard_locals(blocks[i - 1], s, emit, out, loc);
        }
    }

    State process_body(ASR::stmt_t **body, size_t n_body, State s,
            bool emit, Vec<ASR::stmt_t*> *out) {
        for (size_t i = 0; i < n_body; i++) {
            s = process(body[i], s, emit, out);
        }
        return s;
    }

    Vec<ASR::stmt_t*> new_body(size_t n) {
        Vec<ASR::stmt_t*> v;
        v.reserve(al, n + 2);
        return v;
    }

    Loop* find_loop(const char *name) {
        for (size_t i = loops.size(); i > 0; i--) {
            if (name == nullptr || loops[i - 1].name == name) {
                return &loops[i - 1];
            }
        }
        return nullptr;
    }

    // A loop body run to a fixed point of the state at its head, then once
    // more to place the waits.
    State loop(ASR::stmt_t *x, const char *name, ASR::stmt_t **&body,
            size_t &n_body, ASR::expr_t *test, State entry, bool emit,
            Vec<ASR::stmt_t*> *out) {
        const Location &loc = x->base.loc;
        Accesses test_acc = expression_accesses(test, routine, pointers,
            outside, &summaries);
        if (test != nullptr) before(test_acc, entry, emit, out, loc);
        loops.push_back({x, name ? name : "", blocks.size(), false,
            State::unreachable(), State::unreachable()});
        if (entry.any && !launches(body, n_body, &deferral)) {
            // A loop that launches nothing and waits in its first
            // iteration waits before it instead, once.
            size_t placed = n_syncs, placed_waits = n_waits;
            process_body(body, n_body, entry, false, nullptr);
            if (n_syncs != placed) {
                sync(entry, emit, out, loc);
            } else if (n_waits != placed_waits) {
                wait(entry, emit, out, loc);
            }
        }
        State head = entry;
        for (int iteration = 0; ; iteration++) {
            Loop &l = loops.back();
            l.at_cycle = State::unreachable();
            l.at_exit = State::unreachable();
            State end = process_body(body, n_body, head, false, nullptr);
            State back = end;
            back.join(loops.back().at_cycle);
            if (test != nullptr && back.reachable
                    && (conflicts(test_acc, back)
                        || expression_changes_read(test_acc, back)
                        || (test_acc.observable && !back.stops.empty()))) {
                loops.back().sync_at_back_edge = true;
            }
            if (loops.back().sync_at_back_edge) back.clear();
            State next = entry;
            next.join(back);
            if (next == head || iteration > 32) {
                head = next;
                break;
            }
            head = next;
        }
        Loop &l = loops.back();
        l.at_cycle = State::unreachable();
        l.at_exit = State::unreachable();
        Vec<ASR::stmt_t*> nb = new_body(n_body);
        State end = process_body(body, n_body, head, emit,
            emit ? &nb : nullptr);
        if (loops.back().sync_at_back_edge && end.reachable && end.any) {
            sync(end, emit, &nb, loc);
        }
        State after = head;
        after.join(loops.back().at_exit);
        loops.pop_back();
        if (emit) {
            body = nb.p;
            n_body = nb.n;
            out->push_back(al, x);
        }
        return after;
    }

    void enter_block(SymbolTable *scope, ASR::stmt_t **&body, size_t &n_body,
            State &s, bool emit, const Location &loc) {
        blocks.push_back(scope);
        Vec<ASR::stmt_t*> nb = new_body(n_body);
        s = process_body(body, n_body, s, emit, emit ? &nb : nullptr);
        if (s.reachable) discard_locals(scope, s, emit, &nb, loc);
        blocks.pop_back();
        if (emit) {
            body = nb.p;
            n_body = nb.n;
        }
    }

    void launch(ASR::GpuKernelLaunch_t &x, State &s, bool emit,
            Vec<ASR::stmt_t*> *out) {
        const Location &loc = x.base.base.loc;
        ASR::Function_t *kernel = ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(x.m_kernel));
        const ASR::gpu_kernel_layout_t &layout = *kernel->m_gpu;
        // What the host itself reads for the launch: the scalars, the
        // geometry, and the arguments it copies.
        Accesses acc;
        AccessCollector c(acc, routine, pointers, outside, &summaries);
        c.visit_expr(*x.m_thread_count);
        c.visit_expr(*x.m_block_size);
        for (size_t i = 0; i < layout.n_scalars; i++) {
            const ASR::gpu_kernel_argument_t &e = layout.m_scalars[i];
            if (e.m_kind != ASR::gpu_argument_kindType::GpuScalar) continue;
            if ((size_t)e.m_argument_index >= x.n_args) continue;
            ASR::expr_t *v = x.m_args[e.m_argument_index].m_value;
            if (v) c.visit_expr(*v);
        }
        std::vector<Path> writes;
        // What it only reads, which the runtime may hand it in place.
        std::vector<Path> reads;
        // The launch writes a buffer this pass cannot follow.
        bool opaque = false;
        // It reads one.
        bool opaque_read = false;
        for (size_t i = 0; i < layout.n_buffers; i++) {
            const ASR::gpu_kernel_argument_t &e = layout.m_buffers[i];
            ASR::expr_t *arg = x.m_args[e.m_argument_index].m_value;
            GpuBufferHandling handling = gpu_launch_buffer_handling(x, e);
            if (handling == GpuBufferHandling::Scratch) continue;
            Path p;
            std::vector<ASR::expr_t*> subscripts;
            std::vector<Path> alternatives;
            if (!path_of(pointers, arg, p, &subscripts)) {
                std::vector<ASR::expr_t*> alternative_subscripts;
                // Through a local pointer associated with one of a few
                // arrays here: the launch uses whichever it is.
                if (e.m_member == nullptr
                        && !ASR::is_a<ASR::StructType_t>(
                            *ASRUtils::extract_type(ASRUtils::expr_type(arg)))
                        && alternative_paths(pointers, arg, alternatives,
                            &alternative_subscripts)) {
                    for (ASR::expr_t *sub : alternative_subscripts) {
                        c.visit_expr(*sub);
                    }
                    for (const Path &q : alternatives) {
                        if (handling == GpuBufferHandling::HostCopy) {
                            acc.paths.push_back(q);
                        } else if (handling
                                == GpuBufferHandling::HostCopyShallow) {
                            Path r = q;
                            r.shallow = true;
                            acc.paths.push_back(r);
                        }
                        if (e.m_written) writes.push_back(q);
                        else reads.push_back(q);
                    }
                    continue;
                }
                if (!is_pointer_variable(designator_base(arg))) {
                    // Not a designator this pass follows: the host may
                    // access anything for it, and the kernel may write
                    // anything there, so the launch is waited for at once.
                    // Unreachable today: launch buffers are designators
                    // that `path_of` follows.
                    acc.everything = true;
                    if (e.m_written) opaque = true;
                    else opaque_read = true;
                    continue;
                }
                // Through a pointer that may point to anything. The host
                // evaluates the subscripts, and accesses the data only when
                // it copies it: a later launch handed the storage itself
                // sees what this kernel writes there, as for a resolved
                // path (lfortran_gpu_runtime.h).
                for (ASR::expr_t *sub : subscripts) c.visit_expr(*sub);
                if (handling != GpuBufferHandling::Storage) {
                    acc.everything = true;
                }
                if (e.m_written) writes.push_back(Path::pointee());
                else reads.push_back(Path::pointee());
                continue;
            }
            for (ASR::expr_t *sub : subscripts) c.visit_expr(*sub);
            if (e.m_member) p.members.push_back(
                ASRUtils::symbol_get_past_external(e.m_member));
            ASR::ttype_t *type = ASRUtils::expr_type(arg);
            // The bytes of a derived-type value, or of the elements of an
            // array of them; their allocatable components are separate
            // buffers.
            if (e.m_member == nullptr && ASR::is_a<ASR::StructType_t>(
                    *ASRUtils::extract_type(type))) {
                p.shallow = true;
            }
            if (handling == GpuBufferHandling::HostCopy) {
                acc.paths.push_back(p);
            } else if (handling == GpuBufferHandling::HostCopyShallow) {
                Path q = p;
                q.shallow = true;
                acc.paths.push_back(q);
            }
            if (e.m_written) writes.push_back(p);
            else reads.push_back(p);
        }
        before(acc, s, emit, out, loc);
        if (x.m_stop_status != nullptr) {
            for (ASR::GpuKernelLaunch_t *other : s.stops) {
                if (other != &x) {
                    sync(s, emit, out, loc);
                    break;
                }
            }
            if (emit) {
                ASR::symbol_t *status = ASR::down_cast<ASR::Var_t>(
                    x.m_stop_status)->m_v;
                if (std::find(status_symbols.begin(), status_symbols.end(),
                        status) == status_symbols.end()) {
                    status_symbols.push_back(status);
                    status_buffers.push_back(x.m_stop_status);
                }
            }
        }
        if (emit) out->push_back(al, &x.base);
        s.any = true;
        for (const Path &p : writes) s.add_written(p);
        for (const Path &p : reads) s.add_read(p);
        if (x.m_stop_status != nullptr) s.add_stop(&x);
        if (opaque) {
            sync(s, emit, out, loc);
        } else if (opaque_read) {
            wait(s, emit, out, loc);
        }
    }

    // Deallocating storage a pending launch writes does not need the
    // values: the runtime forgets them instead.
    void deallocate(ASR::expr_t **vars, size_t n_vars, State &s, bool emit,
            Vec<ASR::stmt_t*> *out, const Location &loc) {
        Vec<ASR::expr_t*> forget;
        forget.reserve(al, n_vars);
        for (size_t i = 0; i < n_vars; i++) {
            Path p;
            if (!path_of(pointers, vars[i], p, nullptr)) continue;
            if (!ASRUtils::is_array(ASRUtils::expr_type(vars[i]))) continue;
            auto it = std::find(s.written.begin(), s.written.end(), p);
            if (it == s.written.end()) continue;
            forget.push_back(al, vars[i]);
            s.written.erase(it);
        }
        if (emit && forget.size() > 0) {
            out->push_back(al, ASRUtils::STMT(ASR::make_GpuDiscard_t(al, loc,
                forget.p, forget.n)));
        }
    }

    // Whether evaluating `e` calls a function that may change an actual
    // argument: one that is not pure and has a dummy that is a pointer or
    // neither intent(in) nor `value` (the lowering of an intrinsic gives
    // its functions intent(in) dummies).
    static bool changes_arguments(ASR::expr_t *e) {
        class Changes : public ASR::BaseWalkVisitor<Changes> {
        public:
            bool found = false;
            void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                ASR::symbol_t *f = ASRUtils::symbol_get_past_external(
                    x.m_name);
                if (!ASR::is_a<ASR::Function_t>(*f)) {
                    found = true;
                    return;
                }
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(f);
                for (size_t i = 0; i < fn->n_args && !found
                        && !ASRUtils::get_FunctionType(fn)->m_pure; i++) {
                    ASR::symbol_t *d = ASR::is_a<ASR::Var_t>(*fn->m_args[i])
                        ? ASR::down_cast<ASR::Var_t>(fn->m_args[i])->m_v
                        : nullptr;
                    if (d == nullptr || !ASR::is_a<ASR::Variable_t>(*d)) {
                        found = true;
                        break;
                    }
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(d);
                    found = ASRUtils::is_pointer(v->m_type)
                        || (v->m_intent != ASR::intentType::In
                            && !v->m_value_attr);
                }
                BaseWalkVisitor::visit_FunctionCall(x);
            }
        } v;
        v.visit_expr(*e);
        return v.found;
    }

    // A statement that may change whatever it accesses.
    State process_accesses(ASR::stmt_t *x, State s, bool emit,
            Vec<ASR::stmt_t*> *out, const Location &loc) {
        Accesses acc;
        AccessCollector c(acc, routine, pointers, outside, &summaries);
        c.visit_stmt(*x);
        before(acc, s, emit, out, loc, true);
        if (emit) out->push_back(al, x);
        return s;
    }

    State process(ASR::stmt_t *x, State s, bool emit,
            Vec<ASR::stmt_t*> *out) {
        const Location &loc = x->base.loc;
        if (!s.reachable) {
            if (emit) out->push_back(al, x);
            return s;
        }
        switch (x->type) {
            case ASR::stmtType::GpuKernelLaunch: {
                launch(*ASR::down_cast<ASR::GpuKernelLaunch_t>(x), s, emit,
                    out);
                return s;
            }
            case ASR::stmtType::GpuSync: {
                sync(s, emit, out, loc);
                return s;
            }
            case ASR::stmtType::GpuWait: {
                wait(s, emit, out, loc);
                return s;
            }
            case ASR::stmtType::GpuKeep: {
                if (emit) out->push_back(al, x);
                return s;
            }
            case ASR::stmtType::If: {
                ASR::If_t *i = ASR::down_cast<ASR::If_t>(x);
                before(expression_accesses(i->m_test, routine, pointers,
                    outside, &summaries), s, emit, out, loc);
                Vec<ASR::stmt_t*> body = new_body(i->n_body);
                Vec<ASR::stmt_t*> orelse = new_body(i->n_orelse);
                State a = process_body(i->m_body, i->n_body, s, emit,
                    emit ? &body : nullptr);
                State b = process_body(i->m_orelse, i->n_orelse, s, emit,
                    emit ? &orelse : nullptr);
                a.join(b);
                if (emit) {
                    i->m_body = body.p;
                    i->n_body = body.n;
                    i->m_orelse = orelse.p;
                    i->n_orelse = orelse.n;
                    out->push_back(al, x);
                }
                return a;
            }
            case ASR::stmtType::Select: {
                ASR::Select_t *sel = ASR::down_cast<ASR::Select_t>(x);
                Accesses acc = expression_accesses(sel->m_test, routine,
                    pointers, outside, &summaries);
                for (size_t i = 0; i < sel->n_body; i++) {
                    ASR::case_stmt_t *c = sel->m_body[i];
                    if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
                        ASR::CaseStmt_t *cs = ASR::down_cast<ASR::CaseStmt_t>(
                            c);
                        for (size_t j = 0; j < cs->n_test; j++) {
                            Accesses t = expression_accesses(cs->m_test[j],
                                routine, pointers, outside, &summaries);
                            acc.paths.insert(acc.paths.end(),
                                t.paths.begin(), t.paths.end());
                            acc.globals |= t.globals;
                            acc.everything |= t.everything;
                            acc.observable |= t.observable;
                        }
                    } else {
                        ASR::CaseStmt_Range_t *cr =
                            ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                        for (ASR::expr_t *e : {cr->m_start, cr->m_end}) {
                            Accesses t = expression_accesses(e, routine,
                                pointers, outside, &summaries);
                            acc.paths.insert(acc.paths.end(),
                                t.paths.begin(), t.paths.end());
                            acc.globals |= t.globals;
                            acc.everything |= t.everything;
                            acc.observable |= t.observable;
                        }
                    }
                }
                before(acc, s, emit, out, loc);
                State result = State::unreachable();
                State previous = State::unreachable();
                for (size_t i = 0; i < sel->n_body; i++) {
                    ASR::case_stmt_t *c = sel->m_body[i];
                    ASR::stmt_t **&cbody = ASR::is_a<ASR::CaseStmt_t>(*c)
                        ? ASR::down_cast<ASR::CaseStmt_t>(c)->m_body
                        : ASR::down_cast<ASR::CaseStmt_Range_t>(c)->m_body;
                    size_t &cn = ASR::is_a<ASR::CaseStmt_t>(*c)
                        ? ASR::down_cast<ASR::CaseStmt_t>(c)->n_body
                        : ASR::down_cast<ASR::CaseStmt_Range_t>(c)->n_body;
                    State in = s;
                    if (sel->m_enable_fall_through) in.join(previous);
                    Vec<ASR::stmt_t*> nb = new_body(cn);
                    State end = process_body(cbody, cn, in, emit,
                        emit ? &nb : nullptr);
                    if (emit) {
                        cbody = nb.p;
                        cn = nb.n;
                    }
                    previous = end;
                    result.join(end);
                }
                Vec<ASR::stmt_t*> nd = new_body(sel->n_default);
                State def = process_body(sel->m_default, sel->n_default, s,
                    emit, emit ? &nd : nullptr);
                if (emit) {
                    sel->m_default = nd.p;
                    sel->n_default = nd.n;
                    out->push_back(al, x);
                }
                result.join(def);
                return result;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *d = ASR::down_cast<ASR::DoLoop_t>(x);
                Accesses acc;
                AccessCollector c(acc, routine, pointers, outside, &summaries);
                for (ASR::expr_t *e : {d->m_head.m_start, d->m_head.m_end,
                        d->m_head.m_increment}) {
                    if (e) c.visit_expr(*e);
                }
                before(acc, s, emit, out, loc);
                State after = loop(x, d->m_name, d->m_body, d->n_body,
                    nullptr, s, emit, out);
                if (d->n_orelse > 0) {
                    // A loop with an `else` part is not one this pass
                    // places waits in.
                    if (after.any) sync(after, emit, out, loc);
                }
                return after;
            }
            case ASR::stmtType::DoConcurrentLoop: {
                ASR::DoConcurrentLoop_t *d =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(x);
                Accesses acc;
                AccessCollector c(acc, routine, pointers, outside, &summaries);
                for (size_t i = 0; i < d->n_head; i++) {
                    for (ASR::expr_t *e : {d->m_head[i].m_start,
                            d->m_head[i].m_end, d->m_head[i].m_increment}) {
                        if (e) c.visit_expr(*e);
                    }
                }
                before(acc, s, emit, out, loc);
                return loop(x, nullptr, d->m_body, d->n_body, nullptr, s,
                    emit, out);
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *w = ASR::down_cast<ASR::WhileLoop_t>(x);
                State after = loop(x, w->m_name, w->m_body, w->n_body,
                    w->m_test, s, emit, out);
                if (w->n_orelse > 0 && after.any) {
                    sync(after, emit, out, loc);
                }
                return after;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(x)->m_m);
                ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(sym);
                enter_block(block->m_symtab, block->m_body, block->n_body,
                    s, emit, loc);
                if (emit) out->push_back(al, x);
                return s;
            }
            case ASR::stmtType::AssociateBlockCall: {
                ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(x)->m_m);
                ASR::AssociateBlock_t *block =
                    ASR::down_cast<ASR::AssociateBlock_t>(sym);
                enter_block(block->m_symtab, block->m_body, block->n_body,
                    s, emit, loc);
                if (emit) out->push_back(al, x);
                return s;
            }
            case ASR::stmtType::Exit:
            case ASR::stmtType::Cycle: {
                bool is_exit = ASR::is_a<ASR::Exit_t>(*x);
                const char *name = is_exit
                    ? ASR::down_cast<ASR::Exit_t>(x)->m_stmt_name
                    : ASR::down_cast<ASR::Cycle_t>(x)->m_stmt_name;
                Loop *l = find_loop(name);
                if (l == nullptr) {
                    // Leaving a construct that is not a loop: wait.
                    if (s.any) sync(s, emit, out, loc);
                } else {
                    leave_blocks(l->blocks, s, emit, out, loc);
                    if (!is_exit && l->sync_at_back_edge && s.any) {
                        sync(s, emit, out, loc);
                    }
                    (is_exit ? l->at_exit : l->at_cycle).join(s);
                }
                if (emit) out->push_back(al, x);
                return State::unreachable();
            }
            case ASR::stmtType::Return: {
                if (s.any) leave(s, emit, out, loc);
                if (emit) out->push_back(al, x);
                return State::unreachable();
            }
            case ASR::stmtType::GoTo: {
                if (s.any) sync(s, emit, out, loc);
                if (emit) out->push_back(al, x);
                return State::unreachable();
            }
            case ASR::stmtType::GoToTarget: {
                if (s.any) sync(s, emit, out, loc);
                if (emit) out->push_back(al, x);
                return State();
            }
            case ASR::stmtType::Stop:
            case ASR::stmtType::ErrorStop: {
                if (!s.stops.empty()) sync(s, emit, out, loc);
                if (emit) out->push_back(al, x);
                return State::unreachable();
            }
            case ASR::stmtType::ExplicitDeallocate:
            case ASR::stmtType::ImplicitDeallocate: {
                ASR::expr_t **vars;
                size_t n_vars;
                if (ASR::is_a<ASR::ExplicitDeallocate_t>(*x)) {
                    vars = ASR::down_cast<ASR::ExplicitDeallocate_t>(x)
                        ->m_vars;
                    n_vars = ASR::down_cast<ASR::ExplicitDeallocate_t>(x)
                        ->n_vars;
                } else {
                    vars = ASR::down_cast<ASR::ImplicitDeallocate_t>(x)
                        ->m_vars;
                    n_vars = ASR::down_cast<ASR::ImplicitDeallocate_t>(x)
                        ->n_vars;
                }
                deallocate(vars, n_vars, s, emit, out, loc);
                Accesses acc;
                AccessCollector c(acc, routine, pointers, outside, &summaries);
                c.visit_stmt(*x);
                before(acc, s, emit, out, loc, true);
                if (emit) out->push_back(al, x);
                return s;
            }
            case ASR::stmtType::Associate:
            case ASR::stmtType::Nullify:
            case ASR::stmtType::BindReference: {
                // Binding a name, disassociating a pointer and storing the
                // address of a variable in a frame access no values.
                if (emit) out->push_back(al, x);
                return s;
            }
            case ASR::stmtType::SubroutineCall: {
                ASR::SubroutineCall_t *call =
                    ASR::down_cast<ASR::SubroutineCall_t>(x);
                if (ASR::Function_t *t = deferral.callee(call->m_name,
                        call->m_dt)) {
                    redirect(*call, t);
                    call_twin(*call, t, s, emit, out);
                    return s;
                }
                Accesses acc;
                AccessCollector c(acc, routine, pointers, outside, &summaries);
                c.visit_stmt(*x);
                before(acc, s, emit, out, loc, true);
                if (emit) out->push_back(al, x);
                return s;
            }
            case ASR::stmtType::Assignment: {
                // The value is only read, as an expression, unless a function
                // it calls may change an argument: storage a pending launch
                // only reads needs no wait for that. The target is written.
                ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(x);
                if (a->m_overloaded != nullptr || a->m_move_allocation
                        || changes_arguments(a->m_value)) {
                    return process_accesses(x, s, emit, out, loc);
                }
                before(expression_accesses(a->m_value, routine, pointers,
                    outside, &summaries), s, emit, out, loc);
                before(expression_accesses(a->m_target, routine, pointers,
                    outside, &summaries), s, emit, out, loc, true);
                if (emit) out->push_back(al, x);
                return s;
            }
            case ASR::stmtType::Allocate:
            case ASR::stmtType::ReAlloc:
            case ASR::stmtType::DebugCheckArrayBounds:
            case ASR::stmtType::IntrinsicImpureSubroutine:
            case ASR::stmtType::Expr:
            case ASR::stmtType::CPtrToPointer: {
                return process_accesses(x, s, emit, out, loc);
            }
            default: {
                // Input/output, and anything this pass does not know: wait
                // for everything.
                if (s.any) sync(s, emit, out, loc);
                if (emit) out->push_back(al, x);
                return s;
            }
        }
    }

    /*
    Keeping copies.

    A runtime with memory of its own copies an array a launch only reads to
    the device, and may keep that copy for the launches after it until the
    host next waits: the host changes such an array only after a wait. A
    GpuKeep right before a GpuSync or GpuWait names the arrays whose copies
    may outlive it too: on every path from the wait, the host changes none of
    them before the next GpuSync or GpuWait, which decides again. The
    candidates at a wait are the arrays a launch since the previous one read,
    and those the previous one kept, but not one a launch since the last
    GpuSync writes: the sync copies that back into the host.

    This is worked out over the body with its waits placed, forward: the
    state holds the (wait, array) pairs still kept, and a host statement
    that may change an array drops its pairs -- that wait does not keep it.
    Unlike the waits above, reads and writes are told apart here: an
    assignment changes its target, and an expression only what the
    procedures it calls may change. A procedure contained in this one
    changes a variable of this one only if it, or a procedure contained here
    that it calls, names it.

    A local of a `block` ends with the block, and a procedure's arrays may
    change once it returns, so at a `return` or the end of a procedure that
    is not the main program, a GpuWait with nothing kept goes in when a pair
    is still kept there, unless the wait that keeps it is the statement right
    before. Only whole arrays that are locals or dummy arguments of the
    procedure, contiguous and not pointers, are kept: the runtime is handed
    the bytes from their first element to their last.
    */
    struct KeepState {
        bool reachable = true;
        // Read by launches since the last wait.
        std::set<Path> read;
        // Written by launches since the last GpuSync.
        std::set<Path> written;
        std::set<std::pair<ASR::stmt_t*, Path>> kept;
        // The wait that is the statement before, on every path.
        ASR::stmt_t *adjacent = nullptr;

        static KeepState unreachable() {
            KeepState s;
            s.reachable = false;
            return s;
        }
        void join(const KeepState &o) {
            if (!o.reachable) return;
            if (!reachable) {
                *this = o;
                return;
            }
            read.insert(o.read.begin(), o.read.end());
            written.insert(o.written.begin(), o.written.end());
            kept.insert(o.kept.begin(), o.kept.end());
            if (adjacent != o.adjacent) adjacent = nullptr;
        }
        bool operator==(const KeepState &o) const {
            return reachable == o.reachable && read == o.read
                && written == o.written && kept == o.kept
                && adjacent == o.adjacent;
        }
    };
    struct KeepLoop {
        std::string name;
        size_t blocks;
        KeepState at_cycle;
        KeepState at_exit;
    };
    // What a statement or expression accesses, with the procedures contained
    // in this one that it calls.
    struct KeepAccesses {
        Accesses acc;
        std::vector<ASR::Function_t*> contained;
    };
    std::vector<KeepLoop> keep_loops;
    std::vector<SymbolTable*> keep_blocks;
    // What each wait may keep, and the pairs a host statement drops.
    std::map<ASR::stmt_t*, std::set<Path>> keep_candidates;
    std::set<std::pair<ASR::stmt_t*, Path>> keep_dropped;
    // The `return` statements a GpuWait goes before, and whether one goes at
    // the end of the procedure.
    std::set<ASR::stmt_t*> keep_wait_before;
    bool keep_wait_at_end = false;
    // The analysis did not settle: nothing is kept.
    bool keep_failed = false;
    // The variables of this procedure each contained procedure may name.
    std::map<ASR::Function_t*, std::set<ASR::symbol_t*>> keep_named;

    bool is_main_program() const {
        return routine->asr_owner != nullptr
            && ASR::is_a<ASR::symbol_t>(*routine->asr_owner)
            && ASR::is_a<ASR::Program_t>(*ASR::down_cast<ASR::symbol_t>(
                routine->asr_owner));
    }

    // Whether the runtime can be told about `p` at a wait: a whole array
    // that is a local or dummy argument of the procedure, not a pointer,
    // and contiguous.
    bool keepable(const Path &p) const {
        if (may_point_anywhere(p.root) || !p.members.empty() || p.shallow
                || !ASR::is_a<ASR::Variable_t>(*p.root)) {
            return false;
        }
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(p.root);
        if (enclosing_routine(v->m_parent_symtab) != routine
                || !ASRUtils::is_array(v->m_type)) {
            return false;
        }
        ASR::ttype_t *t = ASRUtils::type_get_past_allocatable(v->m_type);
        if (ASR::is_a<ASR::StructType_t>(*ASRUtils::extract_type(t))) {
            return false;
        }
        // An assumed-shape dummy may be a section with gaps.
        bool dummy = v->m_intent != ASR::intentType::Local
            && v->m_intent != ASR::intentType::ReturnVar;
        return !dummy || ASRUtils::is_allocatable(v->m_type)
            || ASRUtils::extract_physical_type(t)
                != ASR::array_physical_typeType::DescriptorArray;
    }

    // Collects the variables `fn` names, through the procedures contained
    // in this one that it calls too; null stands for any variable (a call
    // this does not follow).
    class Names : public ASR::BaseWalkVisitor<Names> {
    public:
        std::set<ASR::symbol_t*> &names;
        SymbolTable *routine;
        std::set<ASR::Function_t*> seen;
        Names(std::set<ASR::symbol_t*> &names_, SymbolTable *routine_)
            : names(names_), routine(routine_) {}
        void function(ASR::Function_t *fn) {
            if (!seen.insert(fn).second) return;
            for (auto &item : fn->m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Variable_t>(*item.second)) {
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(
                        item.second);
                    visit_ttype(*v->m_type);
                    if (v->m_symbolic_value) visit_expr(*v->m_symbolic_value);
                } else if (ASR::is_a<ASR::ExternalSymbol_t>(*item.second)) {
                    names.insert(ASRUtils::symbol_get_past_external(
                        item.second));
                }
            }
            for (size_t i = 0; i < fn->n_body; i++) {
                visit_stmt(*fn->m_body[i]);
            }
        }
        void visit_Var(const ASR::Var_t &x) {
            names.insert(ASRUtils::symbol_get_past_external(x.m_v));
        }
        void call(ASR::symbol_t *name) {
            ASR::symbol_t *callee = ASRUtils::symbol_get_past_external(name);
            if (!ASR::is_a<ASR::Function_t>(*callee)) {
                names.insert(nullptr);
                return;
            }
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(callee);
            if (contained_in(fn, routine)) function(fn);
        }
        void visit_FunctionCall(const ASR::FunctionCall_t &x) {
            call(x.m_name);
            BaseWalkVisitor::visit_FunctionCall(x);
        }
        void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
            call(x.m_name);
            BaseWalkVisitor::visit_SubroutineCall(x);
        }
        void visit_BlockCall(const ASR::BlockCall_t &x) {
            ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(
                ASRUtils::symbol_get_past_external(x.m_m));
            for (size_t i = 0; i < b->n_body; i++) visit_stmt(*b->m_body[i]);
        }
        void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
            ASR::AssociateBlock_t *b = ASR::down_cast<ASR::AssociateBlock_t>(
                ASRUtils::symbol_get_past_external(x.m_m));
            for (size_t i = 0; i < b->n_body; i++) visit_stmt(*b->m_body[i]);
        }
    };

    // Whether the contained procedure `fn` may access the variable `root`
    // of this procedure.
    bool contained_names(ASR::Function_t *fn, ASR::symbol_t *root) {
        auto it = keep_named.find(fn);
        if (it == keep_named.end()) {
            std::set<ASR::symbol_t*> names;
            Names n(names, routine);
            n.function(fn);
            it = keep_named.insert({fn, names}).first;
        }
        // A pointer may point to `root` only if it is reached; then
        // `conflicts` drops it already.
        return it->second.count(root) > 0 || it->second.count(nullptr) > 0;
    }

    KeepAccesses keep_accesses(ASR::expr_t *e) {
        KeepAccesses k;
        if (e == nullptr) return k;
        AccessCollector c(k.acc, routine, pointers, outside);
        c.contained = &k.contained;
        c.visit_expr(*e);
        return k;
    }

    KeepAccesses keep_accesses(ASR::stmt_t *x) {
        KeepAccesses k;
        AccessCollector c(k.acc, routine, pointers, outside);
        c.contained = &k.contained;
        c.visit_stmt(*x);
        return k;
    }

    void keep_drop(KeepState &s, const std::function<bool(const Path&)> &f) {
        for (auto it = s.kept.begin(); it != s.kept.end(); ) {
            if (f(it->second)) {
                keep_dropped.insert(*it);
                it = s.kept.erase(it);
            } else {
                ++it;
            }
        }
    }

    // A host statement (`statement`) may change what it accesses; an
    // expression only what the procedures it calls may.
    void keep_access(const KeepAccesses &k, KeepState &s, bool statement) {
        if (!statement && !k.acc.everything && !k.acc.globals) return;
        keep_drop(s, [&](const Path &p) {
            if (conflicts(k.acc, std::vector<Path>{p})) return true;
            for (ASR::Function_t *fn : k.contained) {
                if (contained_names(fn, p.root)) return true;
            }
            return false;
        });
    }

    // The locals of `scope` end.
    void keep_end_scope(SymbolTable *scope, KeepState &s) {
        auto local = [&](const Path &p) {
            return p.root != nullptr && ASR::is_a<ASR::Variable_t>(*p.root)
                && ASR::down_cast<ASR::Variable_t>(p.root)->m_parent_symtab
                    == scope;
        };
        keep_drop(s, local);
        for (auto it = s.read.begin(); it != s.read.end(); ) {
            if (local(*it)) it = s.read.erase(it);
            else ++it;
        }
    }

    void keep_drop_all(KeepState &s) {
        keep_drop(s, [](const Path &) { return true; });
    }

    // The procedure ends here (`ret`, or the end of its body when null).
    void keep_exit(ASR::stmt_t *ret, KeepState &s) {
        if (s.kept.empty() || is_main_program()) return;
        if (s.adjacent != nullptr) {
            keep_drop_all(s);
            return;
        }
        if (ret != nullptr) keep_wait_before.insert(ret);
        else keep_wait_at_end = true;
        s.kept.clear();
    }

    void keep_launch(ASR::GpuKernelLaunch_t &x, KeepState &s) {
        ASR::Function_t *kernel = ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(x.m_kernel));
        const ASR::gpu_kernel_layout_t &layout = *kernel->m_gpu;
        // What the host evaluates for the launch.
        KeepAccesses k;
        AccessCollector c(k.acc, routine, pointers, outside);
        c.contained = &k.contained;
        c.visit_expr(*x.m_thread_count);
        c.visit_expr(*x.m_block_size);
        for (size_t i = 0; i < layout.n_scalars; i++) {
            const ASR::gpu_kernel_argument_t &e = layout.m_scalars[i];
            if (e.m_kind != ASR::gpu_argument_kindType::GpuScalar) continue;
            if ((size_t)e.m_argument_index >= x.n_args) continue;
            ASR::expr_t *v = x.m_args[e.m_argument_index].m_value;
            if (v) c.visit_expr(*v);
        }
        for (size_t i = 0; i < layout.n_buffers; i++) {
            const ASR::gpu_kernel_argument_t &e = layout.m_buffers[i];
            ASR::expr_t *arg = x.m_args[e.m_argument_index].m_value;
            if (gpu_launch_buffer_handling(x, e)
                    == GpuBufferHandling::Scratch) {
                continue;
            }
            Path p;
            std::vector<ASR::expr_t*> subscripts;
            if (!path_of(pointers, arg, p, &subscripts)) {
                p = Path::pointee();
            }
            for (ASR::expr_t *sub : subscripts) c.visit_expr(*sub);
            if (e.m_member) p.members.push_back(
                ASRUtils::symbol_get_past_external(e.m_member));
            if (e.m_written) s.written.insert(p);
            else s.read.insert(p);
        }
        keep_access(k, s, false);
    }

    void keep_wait(ASR::stmt_t *w, bool is_sync, KeepState &s) {
        std::set<Path> candidates;
        auto consider = [&](const Path &p) {
            if (!keepable(p)) return;
            for (const Path &q : s.written) {
                if (may_overlap(p, q)) return;
            }
            candidates.insert(p);
        };
        for (const Path &p : s.read) consider(p);
        for (const auto &k : s.kept) consider(k.second);
        keep_candidates[w].insert(candidates.begin(), candidates.end());
        s.kept.clear();
        for (const Path &p : candidates) s.kept.insert({w, p});
        s.read.clear();
        if (is_sync) s.written.clear();
        s.adjacent = w;
    }

    KeepState keep_body(ASR::stmt_t **body, size_t n_body, KeepState s) {
        for (size_t i = 0; i < n_body; i++) s = keep_process(body[i], s);
        return s;
    }

    KeepState keep_loop(const char *name, ASR::stmt_t **body, size_t n_body,
            ASR::expr_t *test, KeepState entry) {
        KeepAccesses test_acc = keep_accesses(test);
        keep_loops.push_back({name ? name : "", keep_blocks.size(),
            KeepState::unreachable(), KeepState::unreachable()});
        KeepState head = entry;
        head.adjacent = nullptr;
        for (int iteration = 0; ; iteration++) {
            if (iteration > 64) {
                keep_failed = true;
                break;
            }
            KeepLoop &l = keep_loops.back();
            l.at_cycle = KeepState::unreachable();
            l.at_exit = KeepState::unreachable();
            KeepState in = head;
            keep_access(test_acc, in, false);
            KeepState back = keep_body(body, n_body, in);
            back.join(keep_loops.back().at_cycle);
            KeepState next = entry;
            next.join(back);
            next.adjacent = nullptr;
            if (next == head) break;
            head = next;
        }
        KeepState after = head;
        keep_access(test_acc, after, false);
        after.join(keep_loops.back().at_exit);
        after.adjacent = nullptr;
        keep_loops.pop_back();
        return after;
    }

    KeepState keep_process(ASR::stmt_t *x, KeepState s) {
        if (!s.reachable) return s;
        if (!ASR::is_a<ASR::GpuSync_t>(*x) && !ASR::is_a<ASR::GpuWait_t>(*x)) {
            s.adjacent = nullptr;
        }
        switch (x->type) {
            case ASR::stmtType::GpuKernelLaunch: {
                keep_launch(*ASR::down_cast<ASR::GpuKernelLaunch_t>(x), s);
                return s;
            }
            case ASR::stmtType::GpuSync:
            case ASR::stmtType::GpuWait: {
                keep_wait(x, ASR::is_a<ASR::GpuSync_t>(*x), s);
                return s;
            }
            case ASR::stmtType::GpuKeep:
            case ASR::stmtType::Associate: {
                return s;
            }
            case ASR::stmtType::If: {
                ASR::If_t *i = ASR::down_cast<ASR::If_t>(x);
                keep_access(keep_accesses(i->m_test), s, false);
                KeepState a = keep_body(i->m_body, i->n_body, s);
                KeepState b = keep_body(i->m_orelse, i->n_orelse, s);
                a.join(b);
                return a;
            }
            case ASR::stmtType::Select: {
                ASR::Select_t *sel = ASR::down_cast<ASR::Select_t>(x);
                keep_access(keep_accesses(sel->m_test), s, false);
                for (size_t i = 0; i < sel->n_body; i++) {
                    ASR::case_stmt_t *c = sel->m_body[i];
                    if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
                        ASR::CaseStmt_t *cs = ASR::down_cast<ASR::CaseStmt_t>(
                            c);
                        for (size_t j = 0; j < cs->n_test; j++) {
                            keep_access(keep_accesses(cs->m_test[j]), s,
                                false);
                        }
                    } else {
                        ASR::CaseStmt_Range_t *cr =
                            ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                        keep_access(keep_accesses(cr->m_start), s, false);
                        keep_access(keep_accesses(cr->m_end), s, false);
                    }
                }
                KeepState result = keep_body(sel->m_default, sel->n_default,
                    s);
                KeepState previous = KeepState::unreachable();
                for (size_t i = 0; i < sel->n_body; i++) {
                    ASR::case_stmt_t *c = sel->m_body[i];
                    KeepState in = s;
                    if (sel->m_enable_fall_through) in.join(previous);
                    KeepState end = ASR::is_a<ASR::CaseStmt_t>(*c)
                        ? keep_body(ASR::down_cast<ASR::CaseStmt_t>(c)
                            ->m_body, ASR::down_cast<ASR::CaseStmt_t>(c)
                            ->n_body, in)
                        : keep_body(ASR::down_cast<ASR::CaseStmt_Range_t>(c)
                            ->m_body, ASR::down_cast<ASR::CaseStmt_Range_t>(c)
                            ->n_body, in);
                    previous = end;
                    result.join(end);
                }
                return result;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *d = ASR::down_cast<ASR::DoLoop_t>(x);
                for (ASR::expr_t *e : {d->m_head.m_start, d->m_head.m_end,
                        d->m_head.m_increment}) {
                    keep_access(keep_accesses(e), s, false);
                }
                KeepState after = keep_loop(d->m_name, d->m_body, d->n_body,
                    nullptr, s);
                return keep_body(d->m_orelse, d->n_orelse, after);
            }
            case ASR::stmtType::DoConcurrentLoop: {
                ASR::DoConcurrentLoop_t *d =
                    ASR::down_cast<ASR::DoConcurrentLoop_t>(x);
                for (size_t i = 0; i < d->n_head; i++) {
                    for (ASR::expr_t *e : {d->m_head[i].m_start,
                            d->m_head[i].m_end, d->m_head[i].m_increment}) {
                        keep_access(keep_accesses(e), s, false);
                    }
                }
                return keep_loop(nullptr, d->m_body, d->n_body, nullptr, s);
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *w = ASR::down_cast<ASR::WhileLoop_t>(x);
                KeepState after = keep_loop(w->m_name, w->m_body, w->n_body,
                    w->m_test, s);
                return keep_body(w->m_orelse, w->n_orelse, after);
            }
            case ASR::stmtType::BlockCall:
            case ASR::stmtType::AssociateBlockCall: {
                ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                    ASR::is_a<ASR::BlockCall_t>(*x)
                        ? ASR::down_cast<ASR::BlockCall_t>(x)->m_m
                        : ASR::down_cast<ASR::AssociateBlockCall_t>(x)->m_m);
                SymbolTable *scope;
                ASR::stmt_t **body;
                size_t n_body;
                if (ASR::is_a<ASR::Block_t>(*sym)) {
                    ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(sym);
                    scope = b->m_symtab;
                    body = b->m_body;
                    n_body = b->n_body;
                } else {
                    ASR::AssociateBlock_t *b =
                        ASR::down_cast<ASR::AssociateBlock_t>(sym);
                    scope = b->m_symtab;
                    body = b->m_body;
                    n_body = b->n_body;
                }
                // What the declarations of its locals evaluate.
                KeepAccesses k;
                AccessCollector c(k.acc, routine, pointers, outside);
                c.contained = &k.contained;
                c.declarations(scope);
                keep_access(k, s, false);
                keep_blocks.push_back(scope);
                s = keep_body(body, n_body, s);
                keep_blocks.pop_back();
                if (s.reachable) keep_end_scope(scope, s);
                s.adjacent = nullptr;
                return s;
            }
            case ASR::stmtType::Exit:
            case ASR::stmtType::Cycle: {
                bool is_exit = ASR::is_a<ASR::Exit_t>(*x);
                const char *name = is_exit
                    ? ASR::down_cast<ASR::Exit_t>(x)->m_stmt_name
                    : ASR::down_cast<ASR::Cycle_t>(x)->m_stmt_name;
                KeepLoop *l = nullptr;
                for (size_t i = keep_loops.size(); i > 0; i--) {
                    if (name == nullptr || keep_loops[i - 1].name == name) {
                        l = &keep_loops[i - 1];
                        break;
                    }
                }
                if (l == nullptr) {
                    // Leaving a construct that is not a loop.
                    keep_drop_all(s);
                    return KeepState::unreachable();
                }
                for (size_t i = keep_blocks.size(); i > l->blocks; i--) {
                    keep_end_scope(keep_blocks[i - 1], s);
                }
                (is_exit ? l->at_exit : l->at_cycle).join(s);
                return KeepState::unreachable();
            }
            case ASR::stmtType::Return: {
                keep_exit(x, s);
                return KeepState::unreachable();
            }
            case ASR::stmtType::Stop:
            case ASR::stmtType::ErrorStop: {
                return KeepState::unreachable();
            }
            case ASR::stmtType::GoTo: {
                keep_drop_all(s);
                return KeepState::unreachable();
            }
            case ASR::stmtType::GoToTarget: {
                keep_drop_all(s);
                return KeepState();
            }
            case ASR::stmtType::Assignment: {
                // It changes its target only, unless it moves the
                // allocation of its value or is a defined assignment.
                ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(x);
                if (a->m_overloaded != nullptr || a->m_move_allocation) {
                    keep_access(keep_accesses(x), s, true);
                    return s;
                }
                keep_access(keep_accesses(a->m_target), s, true);
                keep_access(keep_accesses(a->m_value), s, false);
                return s;
            }
            case ASR::stmtType::Print: {
                keep_access(keep_accesses(
                    ASR::down_cast<ASR::Print_t>(x)->m_text), s, false);
                return s;
            }
            case ASR::stmtType::Allocate:
            case ASR::stmtType::ReAlloc:
            case ASR::stmtType::Nullify:
            case ASR::stmtType::DebugCheckArrayBounds:
            case ASR::stmtType::SubroutineCall:
            case ASR::stmtType::IntrinsicImpureSubroutine:
            case ASR::stmtType::Expr:
            case ASR::stmtType::CPtrToPointer:
            case ASR::stmtType::ExplicitDeallocate:
            case ASR::stmtType::ImplicitDeallocate:
            case ASR::stmtType::GpuDiscard:
            case ASR::stmtType::FileWrite:
            case ASR::stmtType::FileRead:
            case ASR::stmtType::FileOpen:
            case ASR::stmtType::FileClose:
            case ASR::stmtType::FileInquire:
            case ASR::stmtType::FileRewind:
            case ASR::stmtType::FileBackspace:
            case ASR::stmtType::FileEndfile:
            case ASR::stmtType::Flush: {
                // It may change what it accesses: what an input statement
                // reads into, the variables of its specifiers (`iostat`).
                keep_access(keep_accesses(x), s, true);
                return s;
            }
            default: {
                // Anything else may change anything.
                keep_drop_all(s);
                return s;
            }
        }
    }

    // Puts the GpuKeep statements before the waits, and the GpuWait
    // statements before the `return` statements that need one.
    void keep_rewrite(ASR::stmt_t **&body, size_t &n_body) {
        Vec<ASR::stmt_t*> nb;
        nb.reserve(al, n_body + 2);
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t *x = body[i];
            const Location &loc = x->base.loc;
            switch (x->type) {
                case ASR::stmtType::If: {
                    ASR::If_t *s = ASR::down_cast<ASR::If_t>(x);
                    keep_rewrite(s->m_body, s->n_body);
                    keep_rewrite(s->m_orelse, s->n_orelse);
                    break;
                }
                case ASR::stmtType::Select: {
                    ASR::Select_t *s = ASR::down_cast<ASR::Select_t>(x);
                    for (size_t j = 0; j < s->n_body; j++) {
                        ASR::case_stmt_t *c = s->m_body[j];
                        if (ASR::is_a<ASR::CaseStmt_t>(*c)) {
                            ASR::CaseStmt_t *cs =
                                ASR::down_cast<ASR::CaseStmt_t>(c);
                            keep_rewrite(cs->m_body, cs->n_body);
                        } else {
                            ASR::CaseStmt_Range_t *cr =
                                ASR::down_cast<ASR::CaseStmt_Range_t>(c);
                            keep_rewrite(cr->m_body, cr->n_body);
                        }
                    }
                    keep_rewrite(s->m_default, s->n_default);
                    break;
                }
                case ASR::stmtType::DoLoop: {
                    ASR::DoLoop_t *s = ASR::down_cast<ASR::DoLoop_t>(x);
                    keep_rewrite(s->m_body, s->n_body);
                    keep_rewrite(s->m_orelse, s->n_orelse);
                    break;
                }
                case ASR::stmtType::DoConcurrentLoop: {
                    ASR::DoConcurrentLoop_t *s =
                        ASR::down_cast<ASR::DoConcurrentLoop_t>(x);
                    keep_rewrite(s->m_body, s->n_body);
                    break;
                }
                case ASR::stmtType::WhileLoop: {
                    ASR::WhileLoop_t *s = ASR::down_cast<ASR::WhileLoop_t>(x);
                    keep_rewrite(s->m_body, s->n_body);
                    keep_rewrite(s->m_orelse, s->n_orelse);
                    break;
                }
                case ASR::stmtType::BlockCall: {
                    ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(
                        ASRUtils::symbol_get_past_external(
                            ASR::down_cast<ASR::BlockCall_t>(x)->m_m));
                    keep_rewrite(b->m_body, b->n_body);
                    break;
                }
                case ASR::stmtType::AssociateBlockCall: {
                    ASR::AssociateBlock_t *b =
                        ASR::down_cast<ASR::AssociateBlock_t>(
                            ASRUtils::symbol_get_past_external(
                                ASR::down_cast<ASR::AssociateBlockCall_t>(x)
                                    ->m_m));
                    keep_rewrite(b->m_body, b->n_body);
                    break;
                }
                case ASR::stmtType::GpuSync:
                case ASR::stmtType::GpuWait: {
                    auto it = keep_candidates.find(x);
                    if (it == keep_candidates.end()) break;
                    Vec<ASR::expr_t*> vars;
                    vars.reserve(al, it->second.size());
                    for (const Path &p : it->second) {
                        if (keep_dropped.count({x, p})) continue;
                        vars.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al,
                            loc, p.root)));
                    }
                    if (vars.size() > 0) {
                        nb.push_back(al, ASRUtils::STMT(ASR::make_GpuKeep_t(
                            al, loc, vars.p, vars.n)));
                    }
                    break;
                }
                case ASR::stmtType::Return: {
                    if (keep_wait_before.count(x)) {
                        nb.push_back(al, ASRUtils::STMT(ASR::make_GpuWait_t(
                            al, loc)));
                    }
                    break;
                }
                default: break;
            }
            nb.push_back(al, x);
        }
        body = nb.p;
        n_body = nb.n;
    }

    void place_keeps(Vec<ASR::stmt_t*> &out, const Location &loc) {
        KeepState end = keep_body(out.p, out.n, KeepState());
        if (end.reachable) keep_exit(nullptr, end);
        if (keep_failed) return;
        bool any = false;
        for (auto &c : keep_candidates) {
            for (const Path &p : c.second) {
                if (!keep_dropped.count({c.first, p})) any = true;
            }
        }
        if (!any) return;
        ASR::stmt_t **body = out.p;
        size_t n_body = out.n;
        keep_rewrite(body, n_body);
        Vec<ASR::stmt_t*> result;
        result.reserve(al, n_body + 1);
        for (size_t i = 0; i < n_body; i++) result.push_back(al, body[i]);
        if (keep_wait_at_end) {
            result.push_back(al, ASRUtils::STMT(ASR::make_GpuWait_t(al,
                loc)));
        }
        out = result;
    }
};

// The launches have had their stop replays placed; nothing reads them from
// the launch any more.
class ReplayClearer : public ASR::BaseWalkVisitor<ReplayClearer> {
public:
    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
        ASR::GpuKernelLaunch_t &xx = const_cast<ASR::GpuKernelLaunch_t&>(x);
        xx.m_stop_replay = nullptr;
        xx.n_stop_replay = 0;
    }
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(
            ASRUtils::symbol_get_past_external(x.m_m));
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *block = ASR::down_cast<ASR::AssociateBlock_t>(
            ASRUtils::symbol_get_past_external(x.m_m));
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

void place_syncs(Allocator &al, SymbolTable *scope, ASR::stmt_t **&body,
        size_t &n_body, const Location &loc, Deferral &deferral,
        ASR::Function_t *self) {
    // A twin may be entered with launches pending.
    bool twin = self != nullptr && deferral.twins.count(self) > 0;
    if (!twin && !launches(body, n_body, &deferral)) return;
    PointerTargets targets;
    PointerCollector collector(targets);
    for (size_t i = 0; i < n_body; i++) collector.visit_stmt(*body[i]);
    std::set<ASR::symbol_t*> reached;
    PointerReach reach(reached);
    for (size_t i = 0; i < n_body; i++) reach.visit_stmt(*body[i]);
    std::set<ASR::symbol_t*> escaped;
    PointerReach escape(escaped);
    escape.routine = scope;
    for (size_t i = 0; i < n_body; i++) escape.visit_stmt(*body[i]);
    escape.finish();
    std::set<ASR::symbol_t*> escaped_host;
    PointerReach escape_host(escaped_host);
    escape_host.routine = scope;
    escape_host.all_host = true;
    for (size_t i = 0; i < n_body; i++) escape_host.visit_stmt(*body[i]);
    escape_host.finish();
    RoutineSync sync(al, scope, targets, reached, escaped, escaped_host,
        deferral, self);
    Vec<ASR::stmt_t*> result = sync.run(body, n_body, loc);
    body = result.p;
    n_body = result.n;
    ReplayClearer clearer;
    for (size_t i = 0; i < n_body; i++) clearer.visit_stmt(*body[i]);
}

void visit_scope(Allocator &al, SymbolTable *scope, Deferral &deferral) {
    for (auto &item : scope->get_scope()) {
        ASR::symbol_t *sym = item.second;
        if (ASR::is_a<ASR::Function_t>(*sym)) {
            ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
            if (fn->m_gpu == nullptr) deferral.place(fn);
            visit_scope(al, fn->m_symtab, deferral);
        } else if (ASR::is_a<ASR::Program_t>(*sym)) {
            ASR::Program_t *p = ASR::down_cast<ASR::Program_t>(sym);
            place_syncs(al, p->m_symtab, p->m_body, p->n_body,
                p->base.base.loc, deferral, nullptr);
            visit_scope(al, p->m_symtab, deferral);
        } else if (ASR::is_a<ASR::Module_t>(*sym)) {
            visit_scope(al, ASR::down_cast<ASR::Module_t>(sym)->m_symtab,
                deferral);
        }
    }
}

} // namespace

void pass_gpu_launch_sync(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    if (!gpu_device_capabilities(pass_options).device_selected()) return;
    Deferral deferral;
    make_twins(al, unit, deferral);
    deferral.place = [&](ASR::Function_t *fn) {
        if (!deferral.started.insert(fn).second) return;
        place_syncs(al, fn->m_symtab, fn->m_body, fn->n_body,
            fn->base.base.loc, deferral, fn);
        if (deferral.twins.count(fn) > 0) deferral.pending[fn].known = true;
    };
    visit_scope(al, unit.m_symtab, deferral);
    if (!deferral.twins.empty()) {
        // The calls redirected to twins changed what procedures depend on.
        PassUtils::UpdateDependenciesVisitor dependencies(al);
        dependencies.visit_TranslationUnit(unit);
    }
}

} // namespace LCompilers

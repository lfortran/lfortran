#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/allocate_unallocated.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/pass_utils.h>

#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace LCompilers {

/*
Plain allocation of arrays that are certainly unallocated.

The reallocation on assignment of an allocatable array (ReAlloc, see
array_op) tests whether the array is allocated, and with which shape. Where
the array is certainly unallocated, as when a function first assigns its
result, the test always finds it unallocated: the ReAlloc becomes an
ALLOCATE with the same shape, which allocates without testing anything.

An array is certainly unallocated where every path that reaches the
statement leaves it unallocated, by rules that can be read off the source:

* a non-saved allocatable local of the procedure, or of a BLOCK, is
  unallocated where the procedure or the BLOCK starts; so is the copy of a
  selector of an ASSOCIATE construct that is an expression, a local of the
  construct, where the construct starts;
* `deallocate(x)`, and `if (allocated(x)) deallocate(x)`, leave `x`
  unallocated. This is also what the procedure does on entry for an
  INTENT(OUT) allocatable dummy argument (intent_out_deallocate) and for the
  allocatable components of an INTENT(OUT) derived-type dummy argument, and,
  once subroutine_from_function has made the result of a function its last
  dummy argument, for the result and its allocatable components: they are
  unallocated on entry because of these statements, not because of an
  assumption about the caller.

The array is a variable of the procedure (a local, or a dummy argument), or
a component of one, through scalars only: not an element of an array. It
has neither the SAVE, TARGET nor POINTER attribute, so that nothing else can
designate it or change its allocation between two of these statements.

A statement that names the array, a part of it, or what contains it, other
than in an inquiry about its allocation (`allocated(x)`), can allocate or
define it: after it the array is no longer known to be unallocated. So does
a loop that names it anywhere, from its first statement on, as its body runs
again; a call of any procedure that is not pure, in a procedure with
internal procedures, for the variables of the procedure that an internal
procedure names (host association, which nested_vars has made the variables
of storage HostAssociated of the internal procedures: an internal procedure
can change those, also when it is passed as an actual argument, and no
others); and a label or a GO TO. At the end of an IF
the array is unallocated when it is on every branch. Any statement this pass
does not know forgets everything, and its statements are left as they are.

An inquiry `allocated(x)` that is the test of an IF is false where the
array is certainly unallocated: the test becomes the constant, so that only
the ELSE branch is code that runs. This is also the test that
subroutine_from_function puts before the move of a function result into
its target, whose other branch copies the result into an allocated target
of its shape.

A wrong proof does not go unnoticed: an ALLOCATE of an allocated array is a
run-time error, without `--fast`.
*/

namespace {

using Path = std::vector<ASR::symbol_t*>;

// The variable and components that `e` designates, with `nullptr` for an
// element of an array
bool path_of(ASR::expr_t *e, Path &p) {
    e = ASRUtils::get_past_array_physical_cast(e);
    if (ASR::is_a<ASR::Var_t>(*e)) {
        p.push_back(ASRUtils::symbol_get_past_external(ASR::down_cast<ASR::Var_t>(e)->m_v));
        return true;
    }
    if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
        ASR::StructInstanceMember_t *m = ASR::down_cast<ASR::StructInstanceMember_t>(e);
        if (!path_of(m->m_v, p)) return false;
        p.push_back(ASRUtils::symbol_get_past_external(m->m_m));
        return true;
    }
    if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
        if (!path_of(ASR::down_cast<ASR::ArrayItem_t>(e)->m_v, p)) return false;
        p.push_back(nullptr);
        return true;
    }
    if (ASR::is_a<ASR::ArraySection_t>(*e)) {
        if (!path_of(ASR::down_cast<ASR::ArraySection_t>(e)->m_v, p)) return false;
        p.push_back(nullptr);
        return true;
    }
    return false;
}

// Whether one of `a` and `b` designates a part of the other
bool related(const Path &a, const Path &b) {
    size_t n = std::min(a.size(), b.size());
    for (size_t i = 0; i < n; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

// The designators a statement names, other than in an inquiry about their
// allocation, and the procedures it calls
class Mentions : public ASR::BaseWalkVisitor<Mentions> {
public:
    std::vector<Path> paths;
    std::vector<ASR::symbol_t*> callees;
    bool unknown = false;

    void designator(const ASR::expr_t &x) {
        Path p;
        if (path_of(const_cast<ASR::expr_t*>(&x), p)) {
            paths.push_back(p);
        } else {
            unknown = true;
        }
    }
    void visit_subscripts(ASR::expr_t *e) {
        while (true) {
            e = ASRUtils::get_past_array_physical_cast(e);
            ASR::array_index_t *args = nullptr;
            size_t n = 0;
            if (ASR::is_a<ASR::StructInstanceMember_t>(*e)) {
                e = ASR::down_cast<ASR::StructInstanceMember_t>(e)->m_v;
                continue;
            } else if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
                args = ASR::down_cast<ASR::ArrayItem_t>(e)->m_args;
                n = ASR::down_cast<ASR::ArrayItem_t>(e)->n_args;
                e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
            } else if (ASR::is_a<ASR::ArraySection_t>(*e)) {
                args = ASR::down_cast<ASR::ArraySection_t>(e)->m_args;
                n = ASR::down_cast<ASR::ArraySection_t>(e)->n_args;
                e = ASR::down_cast<ASR::ArraySection_t>(e)->m_v;
            } else {
                return;
            }
            for (size_t i = 0; i < n; i++) {
                if (args[i].m_left) visit_expr(*args[i].m_left);
                if (args[i].m_right) visit_expr(*args[i].m_right);
                if (args[i].m_step) visit_expr(*args[i].m_step);
            }
        }
    }
    void visit_Var(const ASR::Var_t &x) { designator(x.base); }
    void visit_StructInstanceMember(const ASR::StructInstanceMember_t &x) {
        Path p;
        if (path_of(const_cast<ASR::expr_t*>(&x.base), p)) {
            paths.push_back(p);
            visit_subscripts(x.m_v);
        } else {
            ASR::BaseWalkVisitor<Mentions>::visit_StructInstanceMember(x);
        }
    }
    void visit_ArrayItem(const ASR::ArrayItem_t &x) {
        Path p;
        if (path_of(const_cast<ASR::expr_t*>(&x.base), p)) {
            paths.push_back(p);
            visit_subscripts(const_cast<ASR::expr_t*>(&x.base));
        } else {
            ASR::BaseWalkVisitor<Mentions>::visit_ArrayItem(x);
        }
    }
    void visit_ArraySection(const ASR::ArraySection_t &x) {
        Path p;
        if (path_of(const_cast<ASR::expr_t*>(&x.base), p)) {
            paths.push_back(p);
            visit_subscripts(const_cast<ASR::expr_t*>(&x.base));
        } else {
            ASR::BaseWalkVisitor<Mentions>::visit_ArraySection(x);
        }
    }
    void visit_IntrinsicImpureFunction(const ASR::IntrinsicImpureFunction_t &x) {
        if (x.m_impure_intrinsic_id == static_cast<int64_t>(
                ASRUtils::IntrinsicImpureFunctions::Allocated)) {
            return;
        }
        ASR::BaseWalkVisitor<Mentions>::visit_IntrinsicImpureFunction(x);
    }
    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        callees.push_back(ASRUtils::symbol_get_past_external(x.m_name));
        ASR::BaseWalkVisitor<Mentions>::visit_FunctionCall(x);
    }
    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        callees.push_back(ASRUtils::symbol_get_past_external(x.m_name));
        ASR::BaseWalkVisitor<Mentions>::visit_SubroutineCall(x);
    }
    // Where they are executed, by the caller of this visitor
    void visit_Block(const ASR::Block_t &) {}
    void visit_AssociateBlock(const ASR::AssociateBlock_t &) {}
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(x.m_m);
        for (size_t i = 0; i < b->n_body; i++) visit_stmt(*b->m_body[i]);
    }
    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        ASR::AssociateBlock_t *b = ASR::down_cast<ASR::AssociateBlock_t>(x.m_m);
        for (size_t i = 0; i < b->n_body; i++) visit_stmt(*b->m_body[i]);
    }
};

// The designators that are certainly unallocated
using State = std::map<Path, bool>;

class Unallocated {
public:
    Allocator &al;
    SymbolTable *procedure = nullptr;
    bool has_internal_procedures = false;
    // The names of the variables of the procedure that its internal
    // procedures, or theirs, reach by host association
    std::set<std::string> reached;
    size_t allocations = 0;
    // Tests of allocation folded to a constant
    size_t folded = 0;

    explicit Unallocated(Allocator &al_) : al(al_) {}

    // Whether `s` is a variable of the procedure or of one of its BLOCKs
    // that nothing else can designate or change the allocation of
    bool own_variable(ASR::symbol_t *s) {
        if (s == nullptr || !ASR::is_a<ASR::Variable_t>(*s)) return false;
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
        // The result of a function that subroutine_from_function made its
        // last dummy argument is ResultArgument: the caller passes a new
        // variable for it, or one the function cannot name otherwise.
        if ((v->m_storage != ASR::storage_typeType::Default &&
                v->m_storage != ASR::storage_typeType::ResultArgument) ||
                v->m_target_attr ||
                ASRUtils::is_pointer(v->m_type) || v->m_symbolic_value != nullptr) {
            return false;
        }
        for (SymbolTable *t = v->m_parent_symtab; t != nullptr; t = t->parent) {
            if (t == procedure) return true;
            if (t->asr_owner == nullptr || !ASR::is_a<ASR::symbol_t>(*t->asr_owner) ||
                    !(ASR::is_a<ASR::Block_t>(*ASR::down_cast<ASR::symbol_t>(t->asr_owner)) ||
                      ASR::is_a<ASR::AssociateBlock_t>(*ASR::down_cast<ASR::symbol_t>(t->asr_owner)))) {
                return false;
            }
        }
        return false;
    }

    // Whether `p` can be known to be unallocated: an allocatable variable of
    // the procedure, or a component of one through scalars
    bool trackable(const Path &p, ASR::expr_t *e) {
        if (p.empty() || !own_variable(p[0])) return false;
        for (ASR::symbol_t *s : p) {
            if (s == nullptr) return false;
        }
        for (size_t i = 0; i + 1 < p.size(); i++) {
            ASR::ttype_t *t = ASRUtils::symbol_type(p[i]);
            if (ASRUtils::is_array(t) || ASRUtils::is_pointer(t) || ASRUtils::is_allocatable(t)) {
                return false;
            }
        }
        ASR::ttype_t *t = ASRUtils::expr_type(e);
        return ASRUtils::is_allocatable(t) && !ASRUtils::is_pointer(t);
    }

    void kill(State &s, const std::vector<Path> &paths) {
        for (auto &k : s) {
            if (!k.second) continue;
            for (const Path &p : paths) {
                if (related(k.first, p)) {
                    k.second = false;
                    break;
                }
            }
        }
    }

    static void kill_all(State &s) {
        for (auto &k : s) k.second = false;
    }

    // What a call that may call an internal procedure can change
    void kill_reached(State &s) {
        for (auto &k : s) {
            if (k.second && reached.count(ASRUtils::symbol_name(k.first[0]))) {
                k.second = false;
            }
        }
    }

    void collect_reached(SymbolTable *t) {
        for (auto &item : t->get_scope()) {
            if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
            ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(item.second);
            for (auto &v : f->m_symtab->get_scope()) {
                if (ASR::is_a<ASR::Variable_t>(*v.second) &&
                        ASR::down_cast<ASR::Variable_t>(v.second)->m_storage ==
                            ASR::storage_typeType::HostAssociated) {
                    reached.insert(v.first);
                }
            }
            collect_reached(f->m_symtab);
        }
    }

    static void join(State &into, const State &other) {
        for (auto &k : into) {
            auto o = other.find(k.first);
            if (o == other.end() || !o->second) k.second = false;
        }
    }

    bool is_pure(ASR::symbol_t *f) {
        if (ASR::is_a<ASR::Function_t>(*f)) {
            return ASRUtils::get_FunctionType(*ASR::down_cast<ASR::Function_t>(f))->m_pure;
        }
        return false;
    }

    void effects(const ASR::stmt_t &x, State &s) {
        Mentions m;
        m.visit_stmt(x);
        if (m.unknown) {
            kill_all(s);
            return;
        }
        if (has_internal_procedures) {
            for (ASR::symbol_t *c : m.callees) {
                if (!is_pure(c)) {
                    kill_reached(s);
                    break;
                }
            }
        }
        kill(s, m.paths);
    }

    void unallocated(ASR::expr_t *e, State &s) {
        Path p;
        if (path_of(e, p) && trackable(p, e)) {
            s[p] = true;
        }
    }

    // `p` of the test `allocated(p)`, or of `.not. allocated(p)` (with
    // `negated` set), or nullptr
    static ASR::expr_t* allocation_inquiry(ASR::expr_t *test, bool &negated) {
        negated = false;
        if (ASR::is_a<ASR::LogicalNot_t>(*test)) {
            negated = true;
            test = ASR::down_cast<ASR::LogicalNot_t>(test)->m_arg;
        }
        if (!ASR::is_a<ASR::IntrinsicImpureFunction_t>(*test)) return nullptr;
        ASR::IntrinsicImpureFunction_t *t = ASR::down_cast<ASR::IntrinsicImpureFunction_t>(test);
        if (t->m_impure_intrinsic_id != static_cast<int64_t>(
                ASRUtils::IntrinsicImpureFunctions::Allocated) || t->n_args != 1) {
            return nullptr;
        }
        return t->m_args[0];
    }

    // `if (allocated(p)) deallocate(p)`, or nullptr
    static ASR::expr_t* deallocates_if_allocated(const ASR::If_t &x) {
        if (x.n_orelse != 0 || x.n_body != 1 ||
                !ASR::is_a<ASR::ExplicitDeallocate_t>(*x.m_body[0]) ||
                !ASR::is_a<ASR::IntrinsicImpureFunction_t>(*x.m_test)) {
            return nullptr;
        }
        ASR::ExplicitDeallocate_t *d = ASR::down_cast<ASR::ExplicitDeallocate_t>(x.m_body[0]);
        ASR::IntrinsicImpureFunction_t *t = ASR::down_cast<ASR::IntrinsicImpureFunction_t>(x.m_test);
        if (t->m_impure_intrinsic_id != static_cast<int64_t>(
                    ASRUtils::IntrinsicImpureFunctions::Allocated) ||
                d->n_vars != 1 || t->n_args != 1 || d->m_stat != nullptr) {
            return nullptr;
        }
        Path a, b;
        if (!path_of(d->m_vars[0], a) || !path_of(t->m_args[0], b) || a != b) return nullptr;
        return d->m_vars[0];
    }

    void stmts(ASR::stmt_t **body, size_t n, State &s) {
        for (size_t i = 0; i < n; i++) {
            stmt(body[i], s);
        }
    }

    void reallocation(ASR::stmt_t *&x, State &s) {
        ASR::ReAlloc_t *r = ASR::down_cast<ASR::ReAlloc_t>(x);
        Path p;
        bool certain = r->n_args == 1 && path_of(r->m_args[0].m_a, p);
        if (certain) {
            auto k = s.find(p);
            certain = k != s.end() && k->second;
        }
        // The dimensions read nothing it changes: it is not allocated yet
        effects(*x, s);
        if (certain) {
            x = ASRUtils::STMT(ASR::make_Allocate_t(al, x->base.loc,
                r->m_args, r->n_args, nullptr, nullptr, nullptr));
            allocations++;
        }
    }

    void stmt(ASR::stmt_t *&x, State &s) {
        switch (x->type) {
            case ASR::stmtType::ReAlloc:
                reallocation(x, s);
                return;
            case ASR::stmtType::ExplicitDeallocate:
            case ASR::stmtType::ImplicitDeallocate: {
                effects(*x, s);
                bool is_explicit = x->type == ASR::stmtType::ExplicitDeallocate;
                ASR::expr_t **vars = is_explicit
                    ? ASR::down_cast<ASR::ExplicitDeallocate_t>(x)->m_vars
                    : ASR::down_cast<ASR::ImplicitDeallocate_t>(x)->m_vars;
                size_t nv = is_explicit
                    ? ASR::down_cast<ASR::ExplicitDeallocate_t>(x)->n_vars
                    : ASR::down_cast<ASR::ImplicitDeallocate_t>(x)->n_vars;
                // With STAT=, a failed deallocation leaves it allocated
                if (is_explicit && ASR::down_cast<ASR::ExplicitDeallocate_t>(x)->m_stat) {
                    return;
                }
                for (size_t k = 0; k < nv; k++) unallocated(vars[k], s);
                return;
            }
            case ASR::stmtType::If: {
                ASR::If_t *f = ASR::down_cast<ASR::If_t>(x);
                bool negated = false;
                if (ASR::expr_t *v = allocation_inquiry(f->m_test, negated)) {
                    Path p;
                    auto k = s.end();
                    if (path_of(v, p)) k = s.find(p);
                    if (k != s.end() && k->second) {
                        // The inquiry is false: only the ELSE branch runs.
                        // So is the test before the move of a function
                        // result into its target (subroutine_from_function).
                        bool runs_body = negated;
                        f->m_test = ASRUtils::EXPR(ASR::make_LogicalConstant_t(al,
                            f->m_test->base.loc, runs_body, ASRUtils::expr_type(f->m_test)));
                        folded++;
                        if (runs_body) {
                            stmts(f->m_body, f->n_body, s);
                        } else {
                            stmts(f->m_orelse, f->n_orelse, s);
                        }
                        return;
                    }
                }
                if (ASR::expr_t *v = deallocates_if_allocated(*f)) {
                    effects(*x, s);
                    unallocated(v, s);
                    return;
                }
                Mentions m;
                m.visit_expr(*f->m_test);
                if (m.unknown) kill_all(s);
                kill(s, m.paths);
                if (has_internal_procedures && !m.callees.empty()) kill_reached(s);
                State t = s, e = s;
                stmts(f->m_body, f->n_body, t);
                stmts(f->m_orelse, f->n_orelse, e);
                join(t, e);
                s = t;
                return;
            }
            case ASR::stmtType::DoLoop:
            case ASR::stmtType::WhileLoop:
            case ASR::stmtType::DoConcurrentLoop: {
                effects(*x, s);
                State t = s;
                if (x->type == ASR::stmtType::DoLoop) {
                    ASR::DoLoop_t *l = ASR::down_cast<ASR::DoLoop_t>(x);
                    stmts(l->m_body, l->n_body, t);
                } else if (x->type == ASR::stmtType::WhileLoop) {
                    ASR::WhileLoop_t *l = ASR::down_cast<ASR::WhileLoop_t>(x);
                    stmts(l->m_body, l->n_body, t);
                } else {
                    ASR::DoConcurrentLoop_t *l = ASR::down_cast<ASR::DoConcurrentLoop_t>(x);
                    stmts(l->m_body, l->n_body, t);
                }
                return;
            }
            case ASR::stmtType::BlockCall: {
                ASR::Block_t *b = ASR::down_cast<ASR::Block_t>(
                    ASR::down_cast<ASR::BlockCall_t>(x)->m_m);
                State t = s;
                std::vector<Path> locals;
                for (auto &item : b->m_symtab->get_scope()) {
                    if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(item.second);
                    if (v->m_intent == ASR::intentType::Local &&
                            ASRUtils::is_allocatable(v->m_type) &&
                            !ASRUtils::is_pointer(v->m_type) && own_variable(item.second)) {
                        t[{item.second}] = true;
                        locals.push_back({item.second});
                    }
                }
                stmts(b->m_body, b->n_body, t);
                for (const Path &p : locals) t.erase(p);
                s = t;
                return;
            }
            case ASR::stmtType::AssociateBlockCall: {
                // The copy of a selector that is an expression is a local
                // of the construct, unallocated where it starts, as one of
                // a BLOCK is
                ASR::AssociateBlock_t *b = ASR::down_cast<ASR::AssociateBlock_t>(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(x)->m_m);
                std::vector<Path> locals;
                for (auto &item : b->m_symtab->get_scope()) {
                    if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(item.second);
                    if (v->m_intent == ASR::intentType::Local &&
                            ASRUtils::is_allocatable(v->m_type) &&
                            !ASRUtils::is_pointer(v->m_type) && own_variable(item.second)) {
                        s[{item.second}] = true;
                        locals.push_back({item.second});
                    }
                }
                stmts(b->m_body, b->n_body, s);
                for (const Path &p : locals) s.erase(p);
                return;
            }
            case ASR::stmtType::Assignment:
            case ASR::stmtType::Allocate:
            case ASR::stmtType::SubroutineCall:
            case ASR::stmtType::IntrinsicImpureSubroutine:
            case ASR::stmtType::Associate:
            // The binding of a variable to the frame through which internal
            // procedures reach it (nested_vars): it names the variable
            case ASR::stmtType::BindReference:
            case ASR::stmtType::Nullify:
            case ASR::stmtType::Print:
            case ASR::stmtType::FileWrite:
            case ASR::stmtType::FileRead:
            case ASR::stmtType::Return:
            case ASR::stmtType::Exit:
            case ASR::stmtType::Cycle:
            case ASR::stmtType::Stop:
            case ASR::stmtType::ErrorStop:
            case ASR::stmtType::Assert:
            case ASR::stmtType::DebugCheckArrayBounds:
                effects(*x, s);
                return;
            default:
                // A label, a GO TO, or a statement this pass does not
                // follow: nothing is known after it, and nothing in it is
                // changed
                kill_all(s);
                return;
        }
    }

    template <typename T>
    void run(T &x) {
        procedure = x.m_symtab;
        for (auto &item : x.m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                has_internal_procedures = true;
            }
        }
        if (has_internal_procedures) collect_reached(x.m_symtab);
        State s;
        for (auto &item : x.m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(item.second);
            if (v->m_intent == ASR::intentType::Local &&
                    ASRUtils::is_allocatable(v->m_type) &&
                    !ASRUtils::is_pointer(v->m_type) && own_variable(item.second)) {
                s[{item.second}] = true;
            }
        }
        stmts(x.m_body, x.n_body, s);
    }
};

class Visitor : public ASR::BaseWalkVisitor<Visitor> {
public:
    Allocator &al;
    size_t allocations = 0;
    explicit Visitor(Allocator &al_) : al(al_) {}

    void visit_Module(const ASR::Module_t &x) {
        if (x.m_loaded_from_mod || x.m_intrinsic) return;
        ASR::BaseWalkVisitor<Visitor>::visit_Module(x);
    }
    void visit_Program(const ASR::Program_t &x) {
        Unallocated u(al);
        u.run(const_cast<ASR::Program_t&>(x));
        allocations += u.allocations + u.folded;
        for (auto &item : x.m_symtab->get_scope()) visit_symbol(*item.second);
    }
    void visit_Function(const ASR::Function_t &x) {
        if (ASRUtils::get_FunctionType(x)->m_deftype != ASR::deftypeType::Implementation ||
                ASRUtils::runs_on_device(x)) {
            return;
        }
        Unallocated u(al);
        u.run(const_cast<ASR::Function_t&>(x));
        allocations += u.allocations + u.folded;
        for (auto &item : x.m_symtab->get_scope()) visit_symbol(*item.second);
    }
    void visit_Block(const ASR::Block_t &) {}
    void visit_AssociateBlock(const ASR::AssociateBlock_t &) {}
};

} // namespace

void pass_allocate_unallocated(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &/*pass_options*/) {
    Visitor v(al);
    v.visit_TranslationUnit(unit);
    if (v.allocations > 0) {
        // The test of the shape that a ReAlloc had may call a function
        PassUtils::UpdateDependenciesVisitor u(al);
        u.visit_TranslationUnit(unit);
    }
}

} // namespace LCompilers

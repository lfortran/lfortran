#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_decline.h>
#include <libasr/pass/gpu_device_heap.h>
#include <libasr/pass/gpu_device_struct_storage.h>
#include <libasr/pass/gpu_kernel_abi.h>
#include <libasr/pass/gpu_offload_preflight.h>
#include <libasr/pass/gpu_offload_rewrite.h>
#include <libasr/pass/gpu_offload_stop.h>
#include <libasr/pass/intrinsic_function_registry.h>
#include <libasr/pass/pass_utils.h>

#include <algorithm>
#include <deque>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace LCompilers {

/*
The device heap (see "Allocating on the device" in doc/src/gpu_offloading.md).

A run-time sized array of device code whose extents the host cannot work out
before the launch -- they read a value the device computes -- is a *heap
array*. It is allocated at run time from an *arena*, an array of its element
type that the host allocates for the launch, by a relaxed atomic add to the
arena's next free element (`GpuAtomicFetchAdd`), which is held in the
*heap state*, an `integer(4)` array with one element per arena.

Everything here is ASR:

* A heap array is replaced by its *descriptor*: `integer(4)` locals holding
  the position in the arena of the element before its first (-1 when it is
  not allocated), and a lower bound and an extent per dimension. An element
  becomes an element of the arena, `size`, `lbound`, `ubound` and
  `allocated` read the descriptor, and the array passed whole -- an actual
  argument, the target of a pointer -- is the section of the arena that
  holds its elements, bound to a pointer before the statement.
* `allocate` and a reallocation take the elements from the arena and set
  the descriptor; when the arena has too few left, the procedure runs an
  `error stop`, lowered with every other `stop` of device code below.
  `deallocate` only marks the array unallocated: the memory is reclaimed when
  the launch ends.
* A procedure that allocates on the heap, or calls one that does, gets the
  arenas, their capacities and the heap state as hidden dummy arguments,
  appended after its others; the kernel gets them as arguments, which the
  launch passes. Before the launch the host sizes the arenas from
  `lfortran_gpu_heap_size()`, split evenly between them, allocates them and
  clears the heap state; after the launch it frees them.

The heap is used only for what the host cannot size: an array the host can
size keeps its workspace (gpu_device_workspace.cpp, the kernel layout).

Every `stop` device code reaches is lowered here too (gpu_offload_stop.h),
after everything that can add one.
*/

namespace {

const char *heap_exhausted_message =
    "the device heap is exhausted: set the environment variable "
    "LFORTRAN_GPU_HEAP_SIZE to more bytes than it has (256M by default)";

ASR::ttype_t* int_type(Allocator &al, const Location &loc, int kind = 4) {
    return ASRUtils::TYPE(ASR::make_Integer_t(al, loc, kind));
}

ASR::symbol_t* variable_of(ASR::expr_t *e) {
    if (e == nullptr) return nullptr;
    e = ASRUtils::get_past_array_physical_cast(e);
    if (!ASR::is_a<ASR::Var_t>(*e)) return nullptr;
    return ASRUtils::symbol_get_past_external(
        ASR::down_cast<ASR::Var_t>(e)->m_v);
}

// A name for an element type that can be spelled into a symbol name.
std::string type_tag(ASR::ttype_t *t, ASR::symbol_t *decl) {
    switch (t->type) {
        case ASR::ttypeType::Integer:
            return "i" + std::to_string(ASRUtils::extract_kind_from_ttype_t(t));
        case ASR::ttypeType::Real:
            return "r" + std::to_string(ASRUtils::extract_kind_from_ttype_t(t));
        case ASR::ttypeType::Complex:
            return "c" + std::to_string(ASRUtils::extract_kind_from_ttype_t(t));
        case ASR::ttypeType::Logical:
            return "l" + std::to_string(ASRUtils::extract_kind_from_ttype_t(t));
        case ASR::ttypeType::StructType:
            return decl ? std::string(ASRUtils::symbol_name(
                ASRUtils::symbol_get_past_external(decl))) : "t";
        default:
            return "";
    }
}

ASR::symbol_t* call_target(ASR::symbol_t *name) {
    ASR::symbol_t *s = ASRUtils::symbol_get_past_external(name);
    if (s == nullptr) return nullptr;
    s = ASRUtils::symbol_get_past_external(
        ASRUtils::symbol_get_past_StructMethodDeclaration(s));
    return s;
}

// Appends `args` to the dummy arguments of `fn`, keeping its signature in
// step. The signature may be shared with the procedure `fn` was copied
// from, so a new one is made rather than the old one changed.
void append_dummies(Allocator &al, ASR::Function_t &fn,
        const std::vector<ASR::symbol_t*> &args) {
    const Location &loc = fn.base.base.loc;
    Vec<ASR::expr_t*> all;
    all.reserve(al, fn.n_args + args.size());
    for (size_t i = 0; i < fn.n_args; i++) all.push_back(al, fn.m_args[i]);
    for (ASR::symbol_t *a : args) {
        all.push_back(al, ASRUtils::EXPR(ASR::make_Var_t(al, loc, a)));
    }
    fn.m_args = all.p;
    fn.n_args = all.n;
    fn.m_function_signature = ASRUtils::TYPE(
        ASRUtils::make_FunctionType_t_util(al, loc, fn.m_args, fn.n_args,
            fn.m_return_var, ASRUtils::get_FunctionType(fn), fn.m_symtab));
}

// Where each kernel is launched, so that a loop the pipeline cannot offload
// is reported at the loop.
class LaunchLocations : public ASR::BaseWalkVisitor<LaunchLocations> {
public:
    std::map<ASR::symbol_t*, Location> launches;

    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
        launches.emplace(x.m_kernel, x.base.base.loc);
    }
};

// One arena: the arrays of one element type.
struct Arena {
    std::string tag;
    ASR::ttype_t *element;
    // The derived type of a derived-type element, or nullptr.
    ASR::symbol_t *type_declaration;
};

// The arenas, capacities and heap state as one procedure of device code
// sees them: locals while the procedure is rewritten, and its dummy
// arguments (or the kernel's arguments) once every allocation is known.
struct HeapSymbols {
    std::map<size_t, std::pair<ASR::symbol_t*, ASR::symbol_t*>> arenas;
    ASR::symbol_t *state = nullptr;
};

class DeviceHeap {
public:
    DeviceHeap(Allocator &al_, ASR::TranslationUnit_t &unit_,
            const PassOptions &options_)
        : al(al_), unit(unit_), options(options_) {}

    Allocator &al;
    ASR::TranslationUnit_t &unit;
    const PassOptions &options;
    std::vector<Arena> arenas;
    std::map<ASR::Function_t*, HeapSymbols> symbols;
    std::map<ASR::symbol_t*, Location> launches;
    // A kernel that reaches each procedure of device code.
    std::map<ASR::Function_t*, ASR::Function_t*> kernel_of;
    // Kernels already reported, so that a loop is reported once.
    std::set<ASR::Function_t*> declined;

    // The index of the arena of `element`, registering it.
    size_t arena(ASR::ttype_t *element, ASR::symbol_t *decl) {
        std::string tag = type_tag(element, decl);
        for (size_t k = 0; k < arenas.size(); k++) {
            if (arenas[k].tag == tag) return k;
        }
        arenas.push_back({tag, element, decl});
        return arenas.size() - 1;
    }

    ASR::expr_t* state(ASR::Function_t &fn) {
        HeapSymbols &s = symbols[&fn];
        const Location &loc = fn.base.base.loc;
        if (s.state == nullptr) {
            s.state = new_local(fn, "__gpu_heap_state",
                state_type(loc, 1), nullptr);
        }
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, s.state));
    }

    std::pair<ASR::symbol_t*, ASR::symbol_t*> arena_symbols(
            ASR::Function_t &fn, size_t k) {
        HeapSymbols &s = symbols[&fn];
        state(fn);
        auto found = s.arenas.find(k);
        if (found != s.arenas.end()) return found->second;
        const Location &loc = fn.base.base.loc;
        ASR::symbol_t *capacity = new_local(fn,
            "__gpu_heap_cap_" + arenas[k].tag, int_type(al, loc), nullptr);
        ASR::symbol_t *array = new_local(fn, "__gpu_heap_" + arenas[k].tag,
            arena_type(loc, k, ASRUtils::EXPR(ASR::make_Var_t(al, loc,
                capacity))), arena_declaration(fn.m_symtab, k));
        s.arenas[k] = {array, capacity};
        return s.arenas[k];
    }

    ASR::expr_t* arena_array(ASR::Function_t &fn, size_t k) {
        return ASRUtils::EXPR(ASR::make_Var_t(al, fn.base.base.loc,
            arena_symbols(fn, k).first));
    }

    // Statements that take `count` elements from arena `k` in `fn`, and set
    // `base` to the position of the element before the first of them.
    void allocate(ASR::Function_t &fn, size_t k, ASR::expr_t *count,
            ASR::expr_t *base, const Location &loc,
            std::vector<ASR::stmt_t*> &out) {
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *int4 = int_type(al, loc);
        auto syms = arena_symbols(fn, k);
        ASR::expr_t *top = b.ArrayItem_01(state(fn), {b.i32(k + 1)});
        out.push_back(b.Assignment(base, ASRUtils::EXPR(
            ASR::make_GpuAtomicFetchAdd_t(al, loc, top, count, int4,
                nullptr))));
        ASR::expr_t *capacity = ASRUtils::EXPR(ASR::make_Var_t(al, loc,
            syms.second));
        std::string message = heap_exhausted_message;
        ASR::expr_t *code = b.StringConstant(message,
            b.String(b.i32(message.size()), ASR::ExpressionLength));
        out.push_back(b.If(b.Gt(base, b.Sub(capacity, count)),
            {ASRUtils::STMT(ASR::make_ErrorStop_t(al, loc, code))}, {}));
    }

    void decline(ASR::Function_t *kernel, const Location &loc,
            const GpuDecline &why) {
        if (kernel != nullptr) {
            if (!declined.insert(kernel).second) return;
            auto launch = launches.find(&kernel->base);
            if (launch != launches.end()) {
                report_gpu_decline(options, launch->second, why);
                return;
            }
        }
        report_gpu_decline(options, loc, why);
    }

    // Turns the heap symbols of every procedure into dummy arguments (of
    // routines) and arguments (of kernels), passes them at every call, and
    // gives every launch its arenas.
    void finalize(const std::vector<ASR::Function_t*> &kernels,
            const std::vector<ASR::Function_t*> &routines);

private:
    ASR::symbol_t* new_local(ASR::Function_t &fn, const std::string &name,
            ASR::ttype_t *type, ASR::symbol_t *decl) {
        ASRUtils::ASRBuilder b(al, fn.base.base.loc);
        ASR::expr_t *v = b.Variable(fn.m_symtab,
            fn.m_symtab->get_unique_name(name, false), type,
            ASR::intentType::Local, decl);
        return ASR::down_cast<ASR::Var_t>(v)->m_v;
    }

    ASR::ttype_t* state_type(const Location &loc, int64_t n) {
        ASRUtils::ASRBuilder b(al, loc);
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = b.i32(1);
        dim.m_length = b.i32(n);
        dims.push_back(al, dim);
        return ASRUtils::TYPE(ASR::make_Array_t(al, loc, int_type(al, loc),
            dims.p, dims.n, ASR::array_physical_typeType::FixedSizeArray,
            ASR::memory_spaceType::Global));
    }

    ASR::ttype_t* arena_type(const Location &loc, size_t k,
            ASR::expr_t *capacity) {
        ASRUtils::ASRBuilder b(al, loc);
        Vec<ASR::dimension_t> dims;
        dims.reserve(al, 1);
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = b.i32(1);
        dim.m_length = capacity;
        dims.push_back(al, dim);
        return ASRUtils::TYPE(ASR::make_Array_t(al, loc,
            ASRUtils::duplicate_type(al, arenas[k].element), dims.p, dims.n,
            ASR::array_physical_typeType::PointerArray,
            ASR::memory_spaceType::Global));
    }

    ASR::symbol_t* arena_declaration(SymbolTable *scope, size_t k) {
        if (arenas[k].type_declaration == nullptr) return nullptr;
        return scope->resolve_symbol(ASRUtils::symbol_name(
            arenas[k].type_declaration));
    }

    // The heap symbols of `fn` in the order they are passed.
    std::vector<ASR::symbol_t*> ordered(ASR::Function_t &fn) {
        std::vector<ASR::symbol_t*> out;
        HeapSymbols &s = symbols[&fn];
        for (auto &a : s.arenas) {
            out.push_back(a.second.first);
            out.push_back(a.second.second);
        }
        out.push_back(s.state);
        return out;
    }

    void launch_kernels(const std::vector<ASR::Function_t*> &kernels);
};

// Passes the heap symbols of the caller at every call of a procedure that
// gets them as dummy arguments.
class HeapCallSites : public ASR::BaseWalkVisitor<HeapCallSites> {
public:
    HeapCallSites(Allocator &al_,
            const std::map<ASR::Function_t*, std::vector<ASR::symbol_t*>>
                &callee_order_,
            DeviceHeap &heap_, ASR::Function_t &caller_)
        : al(al_), callee_order(callee_order_), heap(heap_),
          caller(caller_) {}

    void visit_Block(const ASR::Block_t &x) {
        for (size_t i = 0; i < x.n_body; i++) visit_stmt(*x.m_body[i]);
    }

    void visit_AssociateBlock(const ASR::AssociateBlock_t &x) {
        for (size_t i = 0; i < x.n_body; i++) visit_stmt(*x.m_body[i]);
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        visit_symbol(*x.m_m);
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        visit_symbol(*x.m_m);
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        ASR::BaseWalkVisitor<HeapCallSites>::visit_FunctionCall(x);
        auto &call = const_cast<ASR::FunctionCall_t&>(x);
        extend(x.m_name, call.m_args, call.n_args, x.base.base.loc);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        ASR::BaseWalkVisitor<HeapCallSites>::visit_SubroutineCall(x);
        auto &call = const_cast<ASR::SubroutineCall_t&>(x);
        extend(x.m_name, call.m_args, call.n_args, x.base.base.loc);
    }

private:
    Allocator &al;
    const std::map<ASR::Function_t*, std::vector<ASR::symbol_t*>>
        &callee_order;
    DeviceHeap &heap;
    ASR::Function_t &caller;

    void extend(ASR::symbol_t *name, ASR::call_arg_t *&args, size_t &n_args,
            const Location &loc) {
        ASR::symbol_t *target = call_target(name);
        if (target == nullptr || !ASR::is_a<ASR::Function_t>(*target)) {
            return;
        }
        auto found = callee_order.find(
            ASR::down_cast<ASR::Function_t>(target));
        if (found == callee_order.end()) return;
        HeapSymbols &mine = heap.symbols[&caller];
        Vec<ASR::call_arg_t> all;
        all.reserve(al, n_args + found->second.size());
        for (size_t i = 0; i < n_args; i++) all.push_back(al, args[i]);
        ASR::Function_t *callee = ASR::down_cast<ASR::Function_t>(target);
        HeapSymbols &theirs = heap.symbols[callee];
        for (auto &a : theirs.arenas) {
            auto m = mine.arenas.find(a.first);
            LCOMPILERS_ASSERT(m != mine.arenas.end());
            for (ASR::symbol_t *s : {m->second.first, m->second.second}) {
                ASR::call_arg_t arg;
                arg.loc = loc;
                arg.m_value = ASRUtils::EXPR(ASR::make_Var_t(al, loc, s));
                all.push_back(al, arg);
            }
        }
        ASR::call_arg_t arg;
        arg.loc = loc;
        arg.m_value = ASRUtils::EXPR(ASR::make_Var_t(al, loc, mine.state));
        all.push_back(al, arg);
        args = all.p;
        n_args = all.n;
    }
};

// Gives every launch of a kernel that allocates on the heap its arenas and
// heap state, and every launch of a kernel that reaches a `stop` its status
// buffer.
class HeapLaunches : public PassUtils::PassVisitor<HeapLaunches> {
public:
    HeapLaunches(Allocator &al_, DeviceHeap &heap_)
        : PassVisitor(al_, nullptr), heap(heap_) {}

    void visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x);

    void visit_GpuSync(const ASR::GpuSync_t &x) {
        if (pending_after.empty()) return;
        pass_result.reserve(al, pending_after.size() + 1);
        pass_result.push_back(al, const_cast<ASR::stmt_t*>(
            &x.base));
        for (ASR::stmt_t *s : pending_after) pass_result.push_back(al, s);
        pending_after.clear();
    }

private:
    DeviceHeap &heap;
    std::vector<ASR::stmt_t*> pending_after;

    ASR::symbol_t* heap_size_function(const Location &loc) {
        const std::string name = "lfortran_gpu_heap_size";
        SymbolTable *global = heap.unit.m_symtab;
        if (ASR::symbol_t *existing = global->get_symbol(name)) {
            return existing;
        }
        ASRUtils::ASRBuilder b(al, loc);
        SymbolTable *fn_symtab = al.make_new<SymbolTable>(global);
        ASR::expr_t *return_var = b.Variable(fn_symtab, name,
            int_type(al, loc, 8), ASRUtils::intent_return_var, nullptr,
            ASR::abiType::BindC, false);
        ASR::asr_t *fn = ASRUtils::make_Function_t_util(al, loc, fn_symtab,
            s2c(al, name), nullptr, 0, nullptr, 0, nullptr, 0, return_var,
            ASR::abiType::BindC, ASR::accessType::Public,
            ASR::deftypeType::Interface, s2c(al, name), false, false, false,
            false, false, nullptr, 0, false, false, false, nullptr);
        ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(fn);
        global->add_symbol(name, sym);
        return sym;
    }
};

void HeapLaunches::visit_GpuKernelLaunch(const ASR::GpuKernelLaunch_t &x) {
    ASR::Function_t *kernel = ASR::down_cast<ASR::Function_t>(
        ASRUtils::symbol_get_past_external(x.m_kernel));
    const Location &loc = x.base.base.loc;
    ASRUtils::ASRBuilder b(al, loc);
    ASR::GpuKernelLaunch_t &launch = const_cast<ASR::GpuKernelLaunch_t&>(x);
    std::vector<ASR::stmt_t*> before;
    std::vector<ASR::expr_t*> extra_args;
    std::vector<ASR::stmt_t*> after;
    std::vector<ASR::stmt_t*> release;

    auto found = heap.symbols.find(kernel);
    if (found != heap.symbols.end()) {
        HeapSymbols &kernel_symbols = found->second;
        ASR::ttype_t *int4 = int_type(al, loc);
        ASR::ttype_t *int_8 = int_type(al, loc, 8);
        // The heap is split evenly between the arenas of the launch.
        ASR::expr_t *bytes = b.Variable(current_scope,
            current_scope->get_unique_name("__gpu_heap_bytes", false), int_8,
            ASR::intentType::Local);
        Vec<ASR::call_arg_t> no_args;
        no_args.reserve(al, 1);
        before.push_back(b.Assignment(bytes, b.Call(heap_size_function(loc),
            no_args, int_8)));
        int64_t n_arenas = kernel_symbols.arenas.size();
        for (auto &a : kernel_symbols.arenas) {
            const Arena &arena = heap.arenas[a.first];
            ASR::expr_t *capacity = b.Variable(current_scope,
                current_scope->get_unique_name("__gpu_heap_cap_" + arena.tag,
                    false), int4, ASR::intentType::Local);
            ASR::symbol_t *decl = nullptr;
            if (arena.type_declaration != nullptr) {
                decl = current_scope->resolve_symbol(ASRUtils::symbol_name(
                    arena.type_declaration));
            }
            Vec<ASR::dimension_t> deferred;
            deferred.reserve(al, 1);
            ASR::dimension_t dim;
            dim.loc = loc;
            dim.m_start = nullptr;
            dim.m_length = nullptr;
            deferred.push_back(al, dim);
            ASR::expr_t *array = b.Variable(current_scope,
                current_scope->get_unique_name("__gpu_heap_" + arena.tag,
                    false),
                ASRUtils::TYPE(ASR::make_Allocatable_t(al, loc,
                    ASRUtils::TYPE(ASR::make_Array_t(al, loc,
                        ASRUtils::duplicate_type(al, arena.element),
                        deferred.p, deferred.n,
                        ASR::array_physical_typeType::DescriptorArray,
                        ASR::memory_spaceType::Global)))),
                ASR::intentType::Local, decl);
            ASR::expr_t *element_bytes = ASRUtils::EXPR(
                ASR::make_SizeOfType_t(al, loc,
                    ASRUtils::duplicate_type(al, arena.element), int_8,
                    nullptr));
            ASR::expr_t *elements = b.Div(b.Div(bytes, b.i64(n_arenas)),
                element_bytes);
            elements = b.Min(elements, b.i64(2147483647));
            before.push_back(b.Assignment(capacity, b.i2i_t(elements, int4)));
            Vec<ASR::dimension_t> dims;
            dims.reserve(al, 1);
            ASR::dimension_t d;
            d.loc = loc;
            d.m_start = b.i32(1);
            d.m_length = capacity;
            dims.push_back(al, d);
            before.push_back(b.Allocate(array, dims.p, dims.n));
            extra_args.push_back(array);
            extra_args.push_back(capacity);
            release.push_back(b.Deallocate(array));
        }
        ASR::expr_t *state = b.Variable(current_scope,
            current_scope->get_unique_name("__gpu_heap_state", false),
            ASRUtils::symbol_type(kernel_symbols.state),
            ASR::intentType::Local);
        for (size_t k = 0; k < heap.arenas.size(); k++) {
            before.push_back(b.Assignment(
                b.ArrayItem_01(state, {b.i32(k + 1)}), b.i32(0)));
        }
        extra_args.push_back(state);
    }

    // Every `stop` the kernel reaches records itself in a status buffer
    // the host reads after the launch, and runs there.
    GpuDeviceStops stops;
    GpuDecline decline = gpu_lower_device_stops(al, loc, *kernel,
        current_scope, stops);
    if (decline.declined()) {
        heap.decline(kernel, loc, decline);
        return;
    }
    if (stops.host_status != nullptr) {
        extra_args.push_back(ASRUtils::EXPR(ASR::make_Var_t(al, loc,
            stops.host_status)));
        for (ASR::stmt_t *s : stops.before_launch) before.push_back(s);
        // A thread that stopped ends the program here, as the statement
        // it reached does on the host, before anything the kernel wrote
        // is used.
        for (ASR::stmt_t *s : stops.after_launch) after.push_back(s);
    }
    for (ASR::stmt_t *s : release) after.push_back(s);

    if (extra_args.empty() && before.empty() && after.empty()) return;
    Vec<ASR::call_arg_t> args;
    args.reserve(al, launch.n_args + extra_args.size());
    for (size_t i = 0; i < launch.n_args; i++) {
        args.push_back(al, launch.m_args[i]);
    }
    for (ASR::expr_t *e : extra_args) {
        ASR::call_arg_t arg;
        arg.loc = loc;
        arg.m_value = e;
        args.push_back(al, arg);
    }
    launch.m_args = args.p;
    launch.n_args = args.n;
    pass_result.reserve(al, before.size() + 1);
    for (ASR::stmt_t *s : before) pass_result.push_back(al, s);
    pass_result.push_back(al, &launch.base);
    pending_after = after;
}

void DeviceHeap::finalize(const std::vector<ASR::Function_t*> &kernels,
        const std::vector<ASR::Function_t*> &routines) {
    // A procedure that calls one that allocates on the heap needs the
    // arenas that one needs, to pass them on.
    std::vector<ASR::Function_t*> all = kernels;
    all.insert(all.end(), routines.begin(), routines.end());
    for (bool changed = true; changed;) {
        changed = false;
        for (ASR::Function_t *fn : all) {
            for (ASR::Function_t *callee : ASRUtils::get_called_functions(
                    fn->m_body, fn->n_body)) {
                auto c = symbols.find(callee);
                if (c == symbols.end()) continue;
                std::vector<size_t> needed;
                for (auto &a : c->second.arenas) needed.push_back(a.first);
                bool had = symbols.count(fn) > 0;
                size_t before = had ? symbols[fn].arenas.size() : 0;
                state(*fn);
                for (size_t k : needed) arena_symbols(*fn, k);
                if (!had || symbols[fn].arenas.size() != before) {
                    changed = true;
                }
            }
        }
    }

    // The heap state has one element per arena.
    for (auto &s : symbols) {
        ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s.second.state);
        v->m_type = state_type(v->base.base.loc, (int64_t)arenas.size());
    }

    // Every routine gets its heap symbols as dummy arguments, and every
    // call to it passes the caller's. Only a routine that device code alone
    // calls can be given them.
    std::map<ASR::Function_t*, std::vector<ASR::symbol_t*>> order;
    for (ASR::Function_t *fn : routines) {
        if (symbols.count(fn) == 0) continue;
        if (ASRUtils::get_FunctionType(fn)->m_exec_space !=
                ASR::exec_spaceType::Device) {
            decline(kernel_of[fn], fn->base.base.loc, GpuDecline(
                GpuDeclineReason::HeapArrayUse, "allocated by '" +
                std::string(fn->m_name) + "', which the host calls too,"));
            continue;
        }
        std::vector<ASR::symbol_t*> dummies = ordered(*fn);
        for (ASR::symbol_t *s : dummies) {
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
            v->m_intent = ASRUtils::is_array(v->m_type)
                ? ASR::intentType::InOut : ASR::intentType::In;
        }
        append_dummies(al, *fn, dummies);
        order[fn] = dummies;
    }
    for (ASR::Function_t *fn : all) {
        if (symbols.count(fn) == 0) continue;
        HeapCallSites call_sites(al, order, *this, *fn);
        for (size_t i = 0; i < fn->n_body; i++) {
            call_sites.visit_stmt(*fn->m_body[i]);
        }
    }
    for (ASR::Function_t *kernel : kernels) {
        if (symbols.count(kernel) == 0) continue;
        std::vector<ASR::symbol_t*> params = ordered(*kernel);
        for (ASR::symbol_t *s : params) {
            ASR::down_cast<ASR::Variable_t>(s)->m_intent =
                ASR::intentType::InOut;
        }
        append_dummies(al, *kernel, params);
    }
    launch_kernels(kernels);
}

void DeviceHeap::launch_kernels(const std::vector<ASR::Function_t*> &) {
    HeapLaunches launches(al, *this);
    launches.visit_TranslationUnit(unit);
}

// The descriptor of a heap array: the position in its arena of the element
// before its first, and a lower bound and an extent per dimension.
struct HeapArray {
    ASR::symbol_t *var = nullptr;
    size_t arena = 0;
    ASR::symbol_t *base = nullptr;
    std::vector<ASR::symbol_t*> lower, extent;
    // A pointer to the section of the arena holding the elements, for the
    // array passed whole.
    ASR::symbol_t *view = nullptr;
};

// Rewrites the references to the heap arrays of one procedure.
class HeapArrayReplacer : public ASR::BaseExprReplacer<HeapArrayReplacer> {
    using Base = ASR::BaseExprReplacer<HeapArrayReplacer>;
public:
    HeapArrayReplacer(Allocator &al_, DeviceHeap &heap_,
            ASR::Function_t &fn_, std::map<ASR::symbol_t*, HeapArray> &arrays_)
        : al(al_), heap(heap_), fn(fn_), arrays(arrays_) {}

    // Statements a reference needs before the statement that holds it.
    std::vector<ASR::stmt_t*> before;
    // A use the heap does not lower yet, or "".
    std::string unsupported;

    HeapArray* heap_array(ASR::expr_t *e) {
        ASR::symbol_t *s = variable_of(e);
        if (s == nullptr) return nullptr;
        auto it = arrays.find(s);
        return it == arrays.end() ? nullptr : &it->second;
    }

    ASR::expr_t* var(ASR::symbol_t *s, const Location &loc) {
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, s));
    }

    // The number of elements of the array.
    ASR::expr_t* count(HeapArray &h, const Location &loc) {
        ASRUtils::ASRBuilder b(al, loc);
        ASR::expr_t *n = var(h.extent[0], loc);
        for (size_t d = 1; d < h.extent.size(); d++) {
            n = b.Mul(n, var(h.extent[d], loc));
        }
        return n;
    }

    // The section of the arena holding the elements of the array, in array
    // element order.
    ASR::expr_t* elements(HeapArray &h, ASR::expr_t *first,
            ASR::expr_t *last, ASR::expr_t *step, const Location &loc) {
        ASRUtils::ASRBuilder b(al, loc);
        Vec<ASR::array_index_t> range;
        range.reserve(al, 1);
        ASR::array_index_t index;
        index.loc = loc;
        index.m_left = first;
        index.m_right = last;
        index.m_step = step ? step : b.i32(1);
        range.push_back(al, index);
        ASR::ttype_t *element = ASRUtils::duplicate_type(al,
            heap.arenas[h.arena].element);
        Vec<ASR::dimension_t> unknown;
        unknown.reserve(al, 1);
        ASR::dimension_t dim;
        dim.loc = loc;
        dim.m_start = nullptr;
        dim.m_length = nullptr;
        unknown.push_back(al, dim);
        ASR::ttype_t *section_type = ASRUtils::TYPE(ASR::make_Array_t(al,
            loc, element, unknown.p, unknown.n,
            ASR::array_physical_typeType::DescriptorArray,
            ASR::memory_spaceType::Global));
        return ASRUtils::EXPR(ASR::make_ArraySection_t(al, loc,
            heap.arena_array(fn, h.arena), range.p, range.n, section_type,
            nullptr));
    }

    // The whole array, as a pointer to the section of the arena holding
    // its elements, bound before the statement.
    ASR::expr_t* whole(HeapArray &h, const Location &loc) {
        ASRUtils::ASRBuilder b(al, loc);
        if (h.extent.size() != 1) {
            unsupported = "'" + std::string(ASRUtils::symbol_name(h.var)) +
                "' of rank " + std::to_string(h.extent.size()) +
                ", used as a whole,";
        }
        if (h.view == nullptr) {
            Vec<ASR::dimension_t> unknown;
            unknown.reserve(al, 1);
            ASR::dimension_t dim;
            dim.loc = loc;
            dim.m_start = nullptr;
            dim.m_length = nullptr;
            unknown.push_back(al, dim);
            ASR::ttype_t *type = ASRUtils::TYPE(ASR::make_Pointer_t(al, loc,
                ASRUtils::TYPE(ASR::make_Array_t(al, loc,
                    ASRUtils::duplicate_type(al, heap.arenas[h.arena].element),
                    unknown.p, unknown.n,
                    ASR::array_physical_typeType::DescriptorArray,
                    ASR::memory_spaceType::Global))));
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(h.var);
            ASR::expr_t *view = b.Variable(v->m_parent_symtab,
                v->m_parent_symtab->get_unique_name(
                    "__gpu_heap_view_" + std::string(v->m_name), false),
                type, ASR::intentType::Local, v->m_type_declaration);
            h.view = ASR::down_cast<ASR::Var_t>(view)->m_v;
        }
        ASR::expr_t *base = var(h.base, loc);
        before.push_back(ASRUtils::STMT(ASR::make_Associate_t(al, loc,
            var(h.view, loc), elements(h, b.Add(base, b.i32(1)),
                b.Add(base, count(h, loc)), nullptr, loc))));
        return var(h.view, loc);
    }

    void replace_ArrayItem(ASR::ArrayItem_t *x) {
        HeapArray *h = heap_array(x->m_v);
        if (h == nullptr) {
            Base::replace_ArrayItem(x);
            return;
        }
        const Location &loc = x->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::expr_t *offset = nullptr;
        ASR::expr_t *stride = nullptr;
        for (size_t d = 0; d < x->n_args; d++) {
            ASR::expr_t **outer = current_expr;
            current_expr = &x->m_args[d].m_right;
            replace_expr(x->m_args[d].m_right);
            current_expr = outer;
            ASR::expr_t *sub = x->m_args[d].m_right;
            if (ASRUtils::extract_kind_from_ttype_t(
                    ASRUtils::expr_type(sub)) != 4) {
                sub = b.i2i_t(sub, int_type(al, loc));
            }
            ASR::expr_t *term = b.Sub(sub, var(h->lower[d], loc));
            if (stride != nullptr) term = b.Mul(term, stride);
            offset = offset ? b.Add(offset, term) : term;
            ASR::expr_t *n = var(h->extent[d], loc);
            stride = stride ? b.Mul(stride, n) : n;
        }
        ASR::expr_t *position = b.Add(b.Add(var(h->base, loc), b.i32(1)),
            offset);
        *current_expr = b.ArrayItem_01(heap.arena_array(fn, h->arena),
            {position});
    }

    void replace_ArraySize(ASR::ArraySize_t *x) {
        HeapArray *h = heap_array(x->m_v);
        if (h == nullptr) {
            Base::replace_ArraySize(x);
            return;
        }
        const Location &loc = x->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::expr_t *n = nullptr;
        if (x->m_dim == nullptr) {
            n = count(*h, loc);
        } else {
            int64_t d = 0;
            if (!ASRUtils::extract_value(ASRUtils::expr_value(x->m_dim), d)
                    || d < 1 || d > (int64_t)h->extent.size()) {
                unsupported = "'" + std::string(ASRUtils::symbol_name(h->var))
                    + "', with the size of a dimension known only at run "
                    "time,";
                return;
            }
            n = var(h->extent[d - 1], loc);
        }
        if (ASRUtils::extract_kind_from_ttype_t(x->m_type) != 4) {
            n = b.i2i_t(n, x->m_type);
        }
        *current_expr = n;
    }

    void replace_ArrayBound(ASR::ArrayBound_t *x) {
        HeapArray *h = heap_array(x->m_v);
        if (h == nullptr) {
            Base::replace_ArrayBound(x);
            return;
        }
        const Location &loc = x->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        int64_t d = 1;
        if (x->m_dim != nullptr && (!ASRUtils::extract_value(
                ASRUtils::expr_value(x->m_dim), d) || d < 1 ||
                d > (int64_t)h->extent.size())) {
            unsupported = "'" + std::string(ASRUtils::symbol_name(h->var)) +
                "', with a bound of a dimension known only at run time,";
            return;
        }
        if (x->m_dim == nullptr && h->extent.size() != 1) {
            unsupported = "'" + std::string(ASRUtils::symbol_name(h->var)) +
                "', with all its bounds at once,";
            return;
        }
        ASR::expr_t *lower = var(h->lower[d - 1], loc);
        ASR::expr_t *bound = x->m_bound == ASR::arrayboundType::LBound
            ? lower
            : b.Sub(b.Add(lower, var(h->extent[d - 1], loc)), b.i32(1));
        if (ASRUtils::extract_kind_from_ttype_t(x->m_type) != 4) {
            bound = b.i2i_t(bound, x->m_type);
        }
        *current_expr = bound;
    }

    // A section with one range is the elements of the arena that step from
    // its first to its last by the stride of that dimension; one with more
    // ranges is not a section of the arena.
    void replace_ArraySection(ASR::ArraySection_t *x) {
        HeapArray *h = heap_array(x->m_v);
        if (h == nullptr) {
            Base::replace_ArraySection(x);
            return;
        }
        const Location &loc = x->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        int range = -1;
        for (size_t d = 0; d < x->n_args; d++) {
            for (ASR::expr_t **part : {&x->m_args[d].m_left,
                    &x->m_args[d].m_right, &x->m_args[d].m_step}) {
                if (*part == nullptr) continue;
                ASR::expr_t **outer = current_expr;
                current_expr = part;
                replace_expr(*part);
                current_expr = outer;
            }
            bool scalar = x->m_args[d].m_left == nullptr &&
                x->m_args[d].m_step == nullptr && x->m_args[d].m_right;
            if (scalar) continue;
            if (range >= 0) {
                unsupported = "'" + std::string(ASRUtils::symbol_name(h->var))
                    + "', a section of it with more than one range,";
                return;
            }
            range = d;
        }
        if (x->n_args != h->extent.size() || range < 0) {
            unsupported = "'" + std::string(ASRUtils::symbol_name(h->var)) +
                "', a section of it,";
            return;
        }
        ASR::expr_t *offset = b.Add(var(h->base, loc), b.i32(1));
        ASR::expr_t *stride = nullptr;
        ASR::expr_t *range_stride = nullptr;
        for (size_t d = 0; d < x->n_args; d++) {
            if ((int)d == range) {
                range_stride = stride;
            } else {
                ASR::expr_t *term = b.Sub(as_int4(x->m_args[d].m_right, loc),
                    var(h->lower[d], loc));
                if (stride != nullptr) term = b.Mul(term, stride);
                offset = b.Add(offset, term);
            }
            ASR::expr_t *n = var(h->extent[d], loc);
            stride = stride ? b.Mul(stride, n) : n;
        }
        ASR::expr_t *lower = var(h->lower[range], loc);
        ASR::array_index_t &r = x->m_args[range];
        ASR::expr_t *left = r.m_left ? as_int4(r.m_left, loc) : lower;
        ASR::expr_t *right = r.m_right ? as_int4(r.m_right, loc)
            : b.Sub(b.Add(lower, var(h->extent[range], loc)), b.i32(1));
        auto position = [&](ASR::expr_t *i) {
            ASR::expr_t *term = b.Sub(i, lower);
            if (range_stride != nullptr) term = b.Mul(term, range_stride);
            return b.Add(offset, term);
        };
        // A procedure that gets a section takes its elements to be next to
        // each other, which the device does not check (ArrayIsContiguous
        // is true there), so a section whose elements are not is not
        // lowered yet.
        int64_t step_value = 1;
        if (range_stride != nullptr || (r.m_step != nullptr &&
                (!ASRUtils::extract_value(ASRUtils::expr_value(r.m_step),
                    step_value) || step_value != 1))) {
            unsupported = "'" + std::string(ASRUtils::symbol_name(h->var)) +
                "', a section of it whose elements are not contiguous,";
            return;
        }
        *current_expr = elements(*h, position(left), position(right), nullptr,
            loc);
    }

    ASR::expr_t* as_int4(ASR::expr_t *e, const Location &loc) {
        if (ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(e)) == 4) {
            return e;
        }
        ASRUtils::ASRBuilder b(al, loc);
        return b.i2i_t(e, int_type(al, loc));
    }

    void replace_ArrayIsContiguous(ASR::ArrayIsContiguous_t *x) {
        if (heap_array(x->m_array) != nullptr) {
            *current_expr = ASRUtils::EXPR(ASR::make_LogicalConstant_t(al,
                x->base.base.loc, true, x->m_type));
            return;
        }
        Base::replace_ArrayIsContiguous(x);
    }

    void replace_IntrinsicImpureFunction(ASR::IntrinsicImpureFunction_t *x) {
        if (x->m_impure_intrinsic_id == static_cast<int64_t>(
                ASRUtils::IntrinsicImpureFunctions::Allocated) &&
                x->n_args == 1) {
            if (HeapArray *h = heap_array(x->m_args[0])) {
                ASRUtils::ASRBuilder b(al, x->base.base.loc);
                *current_expr = b.GtE(var(h->base, x->base.base.loc),
                    b.i32(0));
                return;
            }
        }
        Base::replace_IntrinsicImpureFunction(x);
    }

    void replace_ArrayPhysicalCast(ASR::ArrayPhysicalCast_t *x) {
        Base::replace_ArrayPhysicalCast(x);
        ASR::ttype_t *type = ASRUtils::expr_type(x->m_arg);
        if (!ASRUtils::is_array(type)) return;
        x->m_old = ASRUtils::extract_physical_type(type);
        if (x->m_old == x->m_new) *current_expr = x->m_arg;
    }

    void replace_Var(ASR::Var_t *x) {
        HeapArray *h = heap_array(&x->base);
        if (h == nullptr) return;
        *current_expr = whole(*h, x->base.base.loc);
    }

private:
    Allocator &al;
    DeviceHeap &heap;
    ASR::Function_t &fn;
    std::map<ASR::symbol_t*, HeapArray> &arrays;
};

class HeapArrayStatements :
        public ASR::CallReplacerOnExpressionsVisitor<HeapArrayStatements> {
public:
    HeapArrayReplacer &replacer;
    explicit HeapArrayStatements(HeapArrayReplacer &r) : replacer(r) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};

// Lowers the heap arrays of one procedure of device code.
class HeapArrayLowering {
public:
    HeapArrayLowering(Allocator &al_, DeviceHeap &heap_, ASR::Function_t &fn_,
            std::map<ASR::symbol_t*, HeapArray> &arrays_)
        : al(al_), heap(heap_), fn(fn_),
          replacer(al_, heap_, fn_, arrays_), statements(replacer) {}

    std::string unsupported;

    void run() {
        std::set<ASR::symbol_t*> visited;
        transform(fn.m_body, fn.n_body, visited);
        if (!replacer.unsupported.empty()) unsupported = replacer.unsupported;
    }

private:
    Allocator &al;
    DeviceHeap &heap;
    ASR::Function_t &fn;
    HeapArrayReplacer replacer;
    HeapArrayStatements statements;

    ASR::expr_t* var(ASR::symbol_t *s, const Location &loc) {
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, s));
    }

    void expression(ASR::expr_t *&e) {
        if (e == nullptr) return;
        replacer.current_expr = &e;
        replacer.replace_expr(e);
    }

    // Allocates `h` with the dimensions `dims`.
    void allocate(HeapArray &h, ASR::dimension_t *dims, size_t n_dims,
            const Location &loc, std::vector<ASR::stmt_t*> &out) {
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *int4 = int_type(al, loc);
        for (size_t d = 0; d < n_dims; d++) {
            ASR::expr_t *start = dims[d].m_start ? dims[d].m_start : b.i32(1);
            ASR::expr_t *length = dims[d].m_length;
            expression(start);
            expression(length);
            if (ASRUtils::extract_kind_from_ttype_t(
                    ASRUtils::expr_type(start)) != 4) {
                start = b.i2i_t(start, int4);
            }
            if (ASRUtils::extract_kind_from_ttype_t(
                    ASRUtils::expr_type(length)) != 4) {
                length = b.i2i_t(length, int4);
            }
            out.push_back(b.Assignment(var(h.lower[d], loc), start));
            out.push_back(b.Assignment(var(h.extent[d], loc),
                b.Max(length, b.i32(0))));
        }
        ASR::expr_t *n = replacer.count(h, loc);
        heap.allocate(fn, h.arena, n, var(h.base, loc), loc, out);
    }

    // Whether the descriptor already has the extents `dims`.
    ASR::expr_t* same_shape(HeapArray &h, ASR::dimension_t *dims,
            size_t n_dims, const Location &loc) {
        ASRUtils::ASRBuilder b(al, loc);
        ASR::ttype_t *int4 = int_type(al, loc);
        ASR::expr_t *same = b.GtE(var(h.base, loc), b.i32(0));
        for (size_t d = 0; d < n_dims; d++) {
            ASRUtils::ExprStmtDuplicator duplicator(al);
            ASR::expr_t *length = duplicator.duplicate_expr(dims[d].m_length);
            expression(length);
            if (ASRUtils::extract_kind_from_ttype_t(
                    ASRUtils::expr_type(length)) != 4) {
                length = b.i2i_t(length, int4);
            }
            same = b.And(same, b.Eq(var(h.extent[d], loc),
                b.Max(length, b.i32(0))));
        }
        return same;
    }

    void allocation(ASR::stmt_t *s, bool reallocation,
            Vec<ASR::stmt_t*> &out) {
        ASR::alloc_arg_t *args;
        size_t n_args;
        if (reallocation) {
            args = ASR::down_cast<ASR::ReAlloc_t>(s)->m_args;
            n_args = ASR::down_cast<ASR::ReAlloc_t>(s)->n_args;
        } else {
            args = ASR::down_cast<ASR::Allocate_t>(s)->m_args;
            n_args = ASR::down_cast<ASR::Allocate_t>(s)->n_args;
        }
        Vec<ASR::alloc_arg_t> kept;
        kept.reserve(al, n_args);
        std::vector<ASR::stmt_t*> lowered;
        for (size_t k = 0; k < n_args; k++) {
            HeapArray *h = replacer.heap_array(args[k].m_a);
            if (h == nullptr) {
                kept.push_back(al, args[k]);
                continue;
            }
            const Location &loc = s->base.loc;
            std::vector<ASR::stmt_t*> take;
            allocate(*h, args[k].m_dims, args[k].n_dims, loc, take);
            if (reallocation) {
                ASRUtils::ASRBuilder b(al, loc);
                ASR::expr_t *same = same_shape(*h, args[k].m_dims,
                    args[k].n_dims, loc);
                lowered.push_back(b.If(b.Not(same), take, {}));
            } else {
                lowered.insert(lowered.end(), take.begin(), take.end());
            }
        }
        if (lowered.empty()) {
            statement(s, out);
            return;
        }
        if (kept.size() > 0) {
            if (reallocation) {
                ASR::down_cast<ASR::ReAlloc_t>(s)->m_args = kept.p;
                ASR::down_cast<ASR::ReAlloc_t>(s)->n_args = kept.n;
            } else {
                ASR::down_cast<ASR::Allocate_t>(s)->m_args = kept.p;
                ASR::down_cast<ASR::Allocate_t>(s)->n_args = kept.n;
            }
            statement(s, out);
        }
        flush_before(out);
        for (ASR::stmt_t *x : lowered) out.push_back(al, x);
    }

    void deallocation(ASR::expr_t **vars, size_t n_vars,
            std::function<void(ASR::expr_t**, size_t)> set, ASR::stmt_t *s,
            Vec<ASR::stmt_t*> &out) {
        Vec<ASR::expr_t*> kept;
        kept.reserve(al, n_vars);
        std::vector<ASR::stmt_t*> lowered;
        for (size_t k = 0; k < n_vars; k++) {
            HeapArray *h = replacer.heap_array(vars[k]);
            if (h == nullptr) {
                kept.push_back(al, vars[k]);
                continue;
            }
            ASRUtils::ASRBuilder b(al, s->base.loc);
            lowered.push_back(b.Assignment(var(h->base, s->base.loc),
                b.i32(-1)));
        }
        if (kept.size() > 0) {
            set(kept.p, kept.n);
            out.push_back(al, s);
        }
        for (ASR::stmt_t *x : lowered) out.push_back(al, x);
    }

    void flush_before(Vec<ASR::stmt_t*> &out) {
        for (ASR::stmt_t *x : replacer.before) out.push_back(al, x);
        replacer.before.clear();
    }

    // A statement without a body of its own.
    void statement(ASR::stmt_t *s, Vec<ASR::stmt_t*> &out) {
        statements.visit_stmt(*s);
        flush_before(out);
        out.push_back(al, s);
    }

    void transform(ASR::stmt_t **&body, size_t &n_body,
            std::set<ASR::symbol_t*> &visited) {
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n_body);
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t *s = body[i];
            switch (s->type) {
                case ASR::stmtType::If: {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    expression(x->m_test);
                    flush_before(out);
                    transform(x->m_body, x->n_body, visited);
                    transform(x->m_orelse, x->n_orelse, visited);
                    out.push_back(al, s);
                    break;
                }
                case ASR::stmtType::DoLoop: {
                    ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                    expression(x->m_head.m_start);
                    expression(x->m_head.m_end);
                    expression(x->m_head.m_increment);
                    flush_before(out);
                    transform(x->m_body, x->n_body, visited);
                    transform(x->m_orelse, x->n_orelse, visited);
                    out.push_back(al, s);
                    break;
                }
                case ASR::stmtType::DoConcurrentLoop: {
                    ASR::DoConcurrentLoop_t *x =
                        ASR::down_cast<ASR::DoConcurrentLoop_t>(s);
                    for (size_t h = 0; h < x->n_head; h++) {
                        expression(x->m_head[h].m_start);
                        expression(x->m_head[h].m_end);
                        expression(x->m_head[h].m_increment);
                    }
                    flush_before(out);
                    transform(x->m_body, x->n_body, visited);
                    out.push_back(al, s);
                    break;
                }
                case ASR::stmtType::WhileLoop: {
                    ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                    expression(x->m_test);
                    if (!replacer.before.empty()) {
                        replacer.unsupported = "used in the condition of a "
                            "do while loop,";
                    }
                    flush_before(out);
                    transform(x->m_body, x->n_body, visited);
                    transform(x->m_orelse, x->n_orelse, visited);
                    out.push_back(al, s);
                    break;
                }
                case ASR::stmtType::Select: {
                    ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                    expression(x->m_test);
                    flush_before(out);
                    for (size_t c = 0; c < x->n_body; c++) {
                        if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[c])) {
                            ASR::CaseStmt_t *t =
                                ASR::down_cast<ASR::CaseStmt_t>(x->m_body[c]);
                            transform(t->m_body, t->n_body, visited);
                        } else {
                            ASR::CaseStmt_Range_t *t =
                                ASR::down_cast<ASR::CaseStmt_Range_t>(
                                    x->m_body[c]);
                            transform(t->m_body, t->n_body, visited);
                        }
                    }
                    transform(x->m_default, x->n_default, visited);
                    out.push_back(al, s);
                    break;
                }
                case ASR::stmtType::BlockCall:
                case ASR::stmtType::AssociateBlockCall: {
                    ASR::symbol_t *m = ASRUtils::symbol_get_past_external(
                        ASR::is_a<ASR::BlockCall_t>(*s)
                            ? ASR::down_cast<ASR::BlockCall_t>(s)->m_m
                            : ASR::down_cast<ASR::AssociateBlockCall_t>(s)
                                ->m_m);
                    if (visited.insert(m).second) {
                        if (ASR::is_a<ASR::Block_t>(*m)) {
                            ASR::Block_t *x = ASR::down_cast<ASR::Block_t>(m);
                            transform(x->m_body, x->n_body, visited);
                        } else if (ASR::is_a<ASR::AssociateBlock_t>(*m)) {
                            ASR::AssociateBlock_t *x =
                                ASR::down_cast<ASR::AssociateBlock_t>(m);
                            transform(x->m_body, x->n_body, visited);
                        }
                    }
                    out.push_back(al, s);
                    break;
                }
                case ASR::stmtType::Allocate:
                    allocation(s, false, out);
                    break;
                case ASR::stmtType::ReAlloc:
                    allocation(s, true, out);
                    break;
                case ASR::stmtType::ExplicitDeallocate: {
                    ASR::ExplicitDeallocate_t *x =
                        ASR::down_cast<ASR::ExplicitDeallocate_t>(s);
                    deallocation(x->m_vars, x->n_vars,
                        [x](ASR::expr_t **v, size_t n) {
                            x->m_vars = v;
                            x->n_vars = n;
                        }, s, out);
                    break;
                }
                case ASR::stmtType::ImplicitDeallocate: {
                    ASR::ImplicitDeallocate_t *x =
                        ASR::down_cast<ASR::ImplicitDeallocate_t>(s);
                    deallocation(x->m_vars, x->n_vars,
                        [x](ASR::expr_t **v, size_t n) {
                            x->m_vars = v;
                            x->n_vars = n;
                        }, s, out);
                    break;
                }
                default:
                    statement(s, out);
                    break;
            }
        }
        body = out.p;
        n_body = out.n;
    }
};

// Whether `var` is a local array of device code whose shape is only known
// at run time: an allocatable array, or one of an explicit shape that is
// not a compile-time constant.
bool runtime_sized_array(const ASR::Variable_t &var) {
    if (var.m_intent != ASR::intentType::Local) return false;
    if (var.m_storage == ASR::storage_typeType::Parameter) return false;
    if (ASRUtils::is_pointer(var.m_type)) return false;
    ASR::ttype_t *type = ASRUtils::type_get_past_allocatable(var.m_type);
    if (!ASR::is_a<ASR::Array_t>(*type)) return false;
    if (ASRUtils::is_allocatable(var.m_type)) return true;
    ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
    for (size_t d = 0; d < arr->n_dims; d++) {
        if (arr->m_dims[d].m_length == nullptr) return false;
    }
    return !ASRUtils::is_fixed_size_array(arr->m_dims, arr->n_dims);
}

class SymbolReader : public ASR::BaseWalkVisitor<SymbolReader> {
public:
    std::set<ASR::symbol_t*> symbols;
    void visit_Var(const ASR::Var_t &x) {
        symbols.insert(ASRUtils::symbol_get_past_external(x.m_v));
    }
};

// The allocatable locals of a routine that an allocation gives a shape
// read from a value the routine computes: something other than its dummy
// arguments and named constants, which no caller can work out for it.
class ComputedAllocations :
        public ASRUtils::BlockBodyWalkVisitor<ComputedAllocations> {
    std::set<ASR::symbol_t*> dummies;

    template <typename T>
    void allocation(const T &x) {
        for (size_t i = 0; i < x.n_args; i++) {
            ASR::symbol_t *sym = variable_of(x.m_args[i].m_a);
            if (sym == nullptr) continue;
            SymbolReader reader;
            for (size_t d = 0; d < x.m_args[i].n_dims; d++) {
                for (ASR::expr_t *e : {x.m_args[i].m_dims[d].m_start,
                        x.m_args[i].m_dims[d].m_length}) {
                    if (e != nullptr) reader.visit_expr(*e);
                }
            }
            for (ASR::symbol_t *read : reader.symbols) {
                if (dummies.count(read)) continue;
                if (ASR::is_a<ASR::Variable_t>(*read)) {
                    ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(read);
                    if (v->m_storage == ASR::storage_typeType::Parameter) {
                        continue;
                    }
                }
                computed.insert(sym);
            }
        }
    }
public:
    std::set<ASR::symbol_t*> computed;
    explicit ComputedAllocations(ASR::Function_t &fn) {
        for (size_t i = 0; i < fn.n_args; i++) {
            if (ASR::symbol_t *s = variable_of(fn.m_args[i])) {
                dummies.insert(s);
            }
        }
    }
    void visit_Allocate(const ASR::Allocate_t &x) { allocation(x); }
    void visit_ReAlloc(const ASR::ReAlloc_t &x) { allocation(x); }
};

// Whether the element type of `var` can be held in an arena.
bool arena_element(ASR::Variable_t &var) {
    ASR::ttype_t *element = ASRUtils::extract_type(var.m_type);
    switch (element->type) {
        case ASR::ttypeType::Integer:
        case ASR::ttypeType::Real:
        case ASR::ttypeType::Complex:
        case ASR::ttypeType::Logical:
            return true;
        default:
            return false;
    }
}

class HeapPass {
public:
    HeapPass(Allocator &al_, ASR::TranslationUnit_t &unit_,
            const PassOptions &options_)
        : al(al_), unit(unit_), heap(al_, unit_, options_) {}

    void run() {
        LaunchLocations locations;
        locations.visit_TranslationUnit(unit);
        heap.launches = locations.launches;
        collect_device_code();
        heap.kernel_of = kernel_of;
        for (ASR::Function_t *kernel : kernels) lower_kernel_arrays(*kernel);
        for (ASR::Function_t *fn : routines) lower_routine_arrays(*fn);
        heap.finalize(kernels, routines);
    }

private:
    Allocator &al;
    ASR::TranslationUnit_t &unit;
    DeviceHeap heap;
    std::vector<ASR::Function_t*> kernels;
    std::vector<ASR::Function_t*> routines;
    std::map<ASR::Function_t*, ASR::Function_t*> kernel_of;

    void collect_device_code() {
        std::deque<ASR::Function_t*> work;
        for (auto &item : unit.m_symtab->get_scope()) {
            if (!ASRUtils::is_device_kernel(item.second)) continue;
            ASR::Function_t *kernel =
                ASR::down_cast<ASR::Function_t>(item.second);
            kernels.push_back(kernel);
            kernel_of[kernel] = kernel;
            work.push_back(kernel);
        }
        while (!work.empty()) {
            ASR::Function_t *fn = work.front();
            work.pop_front();
            for (ASR::Function_t *callee : ASRUtils::get_called_functions(
                    fn->m_body, fn->n_body)) {
                if (ASRUtils::get_FunctionType(callee)->m_exec_space
                        == ASR::exec_spaceType::Kernel) {
                    continue;
                }
                ASR::FunctionType_t *t = ASRUtils::get_FunctionType(callee);
                if (t->m_deftype != ASR::deftypeType::Implementation) continue;
                if (kernel_of.emplace(callee, kernel_of[fn]).second) {
                    routines.push_back(callee);
                    work.push_back(callee);
                }
            }
        }
    }

    // Makes `vars` of `fn` heap arrays and lowers them; returns false
    // (having reported the loop) when that cannot be done.
    bool lower(ASR::Function_t &fn, const std::vector<ASR::Variable_t*> &vars) {
        if (vars.empty()) return true;
        ASR::Function_t *kernel = kernel_of[&fn];
        std::map<ASR::symbol_t*, HeapArray> arrays;
        std::map<SymbolTable*, std::vector<ASR::stmt_t*>> entry;
        for (ASR::Variable_t *v : vars) {
            const Location &loc = v->base.base.loc;
            ASRUtils::ASRBuilder b(al, loc);
            if (!arena_element(*v)) {
                heap.decline(kernel, loc, GpuDecline(
                    GpuDeclineReason::HeapArrayUse, "'" +
                    std::string(v->m_name) + "', of a type no arena holds,"));
                return false;
            }
            HeapArray h;
            h.var = &v->base;
            h.arena = heap.arena(ASRUtils::extract_type(v->m_type), nullptr);
            SymbolTable *scope = v->m_parent_symtab;
            std::string name = "__gpu_heap_" + std::string(v->m_name);
            auto local = [&](const std::string &suffix) {
                ASR::expr_t *e = b.Variable(scope, scope->get_unique_name(
                    name + suffix, false), int_type(al, loc),
                    ASR::intentType::Local);
                return ASR::down_cast<ASR::Var_t>(e)->m_v;
            };
            h.base = local("_base");
            size_t rank = ASRUtils::extract_n_dims_from_ttype(v->m_type);
            for (size_t d = 0; d < rank; d++) {
                h.lower.push_back(local("_lb" + std::to_string(d + 1)));
                h.extent.push_back(local("_n" + std::to_string(d + 1)));
            }
            arrays[&v->base] = h;
        }
        HeapArrayLowering lowering(al, heap, fn, arrays);
        // An allocatable array starts unallocated.
        for (ASR::Variable_t *v : vars) {
            HeapArray &h = arrays[&v->base];
            ASRUtils::ASRBuilder b(al, v->base.base.loc);
            entry[v->m_parent_symtab].push_back(b.Assignment(b.Var(h.base),
                b.i32(-1)));
        }
        for (auto &e : entry) prepend(fn, e.first, e.second);
        lowering.run();
        if (!lowering.unsupported.empty()) {
            heap.decline(kernel, fn.base.base.loc, GpuDecline(
                GpuDeclineReason::HeapArrayUse, lowering.unsupported));
            return false;
        }
        for (ASR::Variable_t *v : vars) {
            v->m_parent_symtab->erase_symbol(v->m_name);
        }
        return true;
    }

    // Puts `stmts` at the start of the body of `scope`: after the prelude
    // of a kernel, which is where the iteration starts.
    void prepend(ASR::Function_t &fn, SymbolTable *scope,
            const std::vector<ASR::stmt_t*> &stmts) {
        ASR::stmt_t ***body = nullptr;
        size_t *n_body = nullptr;
        size_t at = 0;
        if (scope == fn.m_symtab) {
            body = &fn.m_body;
            n_body = &fn.n_body;
            if (ASRUtils::get_FunctionType(fn)->m_exec_space ==
                    ASR::exec_spaceType::Kernel) {
                at = kernel_prelude_length(fn);
            }
        } else {
            ASR::symbol_t *owner = ASR::down_cast<ASR::symbol_t>(
                scope->asr_owner);
            if (ASR::is_a<ASR::Block_t>(*owner)) {
                ASR::Block_t *x = ASR::down_cast<ASR::Block_t>(owner);
                body = &x->m_body;
                n_body = &x->n_body;
            } else if (ASR::is_a<ASR::AssociateBlock_t>(*owner)) {
                ASR::AssociateBlock_t *x =
                    ASR::down_cast<ASR::AssociateBlock_t>(owner);
                body = &x->m_body;
                n_body = &x->n_body;
            }
        }
        LCOMPILERS_ASSERT(body != nullptr);
        Vec<ASR::stmt_t*> out;
        out.reserve(al, *n_body + stmts.size());
        for (size_t i = 0; i < at; i++) out.push_back(al, (*body)[i]);
        for (ASR::stmt_t *s : stmts) out.push_back(al, s);
        for (size_t i = at; i < *n_body; i++) out.push_back(al, (*body)[i]);
        *body = out.p;
        *n_body = out.n;
    }

    // The statements a kernel starts with before its iteration: the tests
    // that send a thread past the end of the iteration space back, and the
    // assignments of the loop indices.
    static size_t kernel_prelude_length(ASR::Function_t &kernel) {
        size_t n = 0;
        for (; n < kernel.n_body; n++) {
            ASR::stmt_t *s = kernel.m_body[n];
            ASR::StructInstanceMember_t *component = nullptr;
            ASR::expr_t *storage = nullptr;
            if (gpu_struct_component_storage(s, component, storage)) continue;
            if (ASR::is_a<ASR::If_t>(*s)) {
                ASR::If_t *test = ASR::down_cast<ASR::If_t>(s);
                if (test->n_body == 1 && test->n_orelse == 0 &&
                        ASR::is_a<ASR::Return_t>(*test->m_body[0])) {
                    continue;
                }
                break;
            }
            if (ASR::is_a<ASR::Assignment_t>(*s) && ASR::is_a<ASR::Var_t>(
                    *ASR::down_cast<ASR::Assignment_t>(s)->m_target)) {
                continue;
            }
            break;
        }
        return n;
    }

    // A run-time sized array of the kernel whose extents the host cannot
    // work out before the launch.
    void lower_kernel_arrays(ASR::Function_t &kernel) {
        std::set<ASR::symbol_t*> chosen;
        for (const GpuVlaWorkspace &ws : collect_gpu_vla_workspaces(kernel,
                0)) {
            if (ws.var == nullptr) continue;
            for (const GpuVlaDim &dim : ws.dims) {
                if (gpu_workspace_host_extent(al, kernel, dim) == nullptr) {
                    chosen.insert(ws.var);
                    break;
                }
            }
        }
        std::string unresolved;
        if (!gpu_kernel_workspace_extents_resolvable(kernel, unresolved)) {
            ASR::Variable_t *v = find_gpu_vla_variable(kernel, unresolved);
            if (v != nullptr) chosen.insert(&v->base);
        }
        std::vector<ASR::Variable_t*> vars;
        for (ASR::symbol_t *s : chosen) {
            if (!ASR::is_a<ASR::Variable_t>(*s)) continue;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
            if (!runtime_sized_array(*v)) continue;
            // An array of an explicit shape a pass declares has the extents
            // of where it is used, not of where its scope starts, so only
            // an allocatable one, allocated where an `allocate` says, is
            // allocated on the heap.
            if (!ASRUtils::is_allocatable(v->m_type)) continue;
            vars.push_back(v);
        }
        std::sort(vars.begin(), vars.end(),
            [](ASR::Variable_t *a, ASR::Variable_t *b) {
                return std::string(a->m_name) < std::string(b->m_name);
            });
        lower(kernel, vars);
    }

    // An allocatable local the routine allocates with a shape read from a
    // value it computes, which no caller can give it.
    void lower_routine_arrays(ASR::Function_t &fn) {
        std::vector<SymbolTable*> scopes;
        gpu_routine_scopes(fn.m_symtab, scopes);
        ComputedAllocations shapes(fn);
        for (size_t i = 0; i < fn.n_body; i++) shapes.visit_stmt(*fn.m_body[i]);
        std::vector<ASR::Variable_t*> vars;
        for (SymbolTable *scope : scopes) {
            for (auto &item : scope->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *v =
                    ASR::down_cast<ASR::Variable_t>(item.second);
                if (!runtime_sized_array(*v)) continue;
                if (!ASRUtils::is_allocatable(v->m_type) ||
                        shapes.computed.count(item.second) == 0) {
                    continue;
                }
                vars.push_back(v);
            }
        }
        lower(fn, vars);
    }
};

} // namespace

void pass_gpu_device_heap(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &pass_options) {
    GpuDeviceCapabilities caps = gpu_device_capabilities(pass_options);
    if (!caps.device_selected()) return;
    HeapPass pass(al, unit, pass_options);
    pass.run();

    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers

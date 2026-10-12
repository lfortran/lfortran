#include <libasr/asr.h>
#include <libasr/asr_builder.h>
#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_data_layout.h>
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


// ---------------------------------------------------------------------
// Heap types, their images, and the descriptors of heap arrays.

struct Image;

// One component of a heap type as its image holds it.
struct ImageMember {
    enum class Kind { Plain, Image, Array };
    Kind kind = Kind::Plain;
    // The member of the image (Plain and Image).
    ASR::symbol_t *member = nullptr;
    // The descriptor of an allocatable array component (Array): the
    // position of the element before its first in its arena, and a lower
    // bound and an extent per dimension.
    ASR::symbol_t *base = nullptr;
    std::vector<ASR::symbol_t*> lower, extent;
    size_t arena = 0;
    // The image of the component (Image), or of the elements of an
    // allocatable array of a heap type (Array).
    Image *image = nullptr;
};

// The image type of a heap type: the derived type every value of the heap
// type that device code creates has.
struct Image {
    ASR::Struct_t *source = nullptr;
    ASR::symbol_t *symbol = nullptr;
    std::vector<std::pair<std::string, ImageMember>> members;

    ImageMember* member(const std::string &name) {
        for (auto &m : members) if (m.first == name) return &m.second;
        return nullptr;
    }
};

// The descriptor of a heap array: a local array (`root` is null and the
// fields are locals) or an allocatable array component of an image
// (`root` is the image and the fields are its members).
struct Descriptor {
    size_t arena = 0;
    ASR::expr_t *root = nullptr;
    ASR::symbol_t *base = nullptr;
    std::vector<ASR::symbol_t*> lower, extent;
    // The image of the elements, when they are values of a heap type.
    Image *element_image = nullptr;
    std::string name;
};

ASR::expr_t* duplicate_expr(Allocator &al, ASR::expr_t *e) {
    ASRUtils::ExprStmtDuplicator duplicator(al);
    duplicator.allow_procedure_calls = true;
    duplicator.allow_reshape = true;
    return duplicator.duplicate_expr(e);
}

ASR::Struct_t* struct_of(ASR::symbol_t *declaration) {
    if (declaration == nullptr) return nullptr;
    return gpu_struct_definition(declaration);
}

// The derived type of a value of type `type` declared by `declaration`,
// past arrays, allocatables and pointers, or nullptr.
ASR::Struct_t* struct_of(ASR::ttype_t *type, ASR::symbol_t *declaration) {
    ASR::ttype_t *t = ASRUtils::type_get_past_array(
        ASRUtils::type_get_past_allocatable_pointer(type));
    if (!ASR::is_a<ASR::StructType_t>(*t)) return nullptr;
    return struct_of(declaration);
}

// Replaces the derived type a type is of, keeping its array, allocatable
// and pointer parts.
ASR::ttype_t* with_struct(Allocator &al, ASR::ttype_t *type,
        ASR::ttype_t *element) {
    switch (type->type) {
        case ASR::ttypeType::Allocatable:
            return ASRUtils::TYPE(ASR::make_Allocatable_t(al, type->base.loc,
                with_struct(al, ASR::down_cast<ASR::Allocatable_t>(type)
                    ->m_type, element)));
        case ASR::ttypeType::Pointer:
            return ASRUtils::TYPE(ASR::make_Pointer_t(al, type->base.loc,
                with_struct(al, ASR::down_cast<ASR::Pointer_t>(type)->m_type,
                    element)));
        case ASR::ttypeType::Array: {
            ASR::Array_t *a = ASR::down_cast<ASR::Array_t>(type);
            return ASRUtils::TYPE(ASR::make_Array_t(al, type->base.loc,
                with_struct(al, a->m_type, element), a->m_dims, a->n_dims,
                a->m_physical_type, a->m_memory_space));
        }
        default:
            return element;
    }
}

// Every derived type device code holds, the heap types among them, and
// their images.
class HeapTypes {
public:
    HeapTypes(Allocator &al_, ASR::TranslationUnit_t &unit_)
        : al(al_), unit(unit_) {}

    std::set<std::string> heap;
    std::map<std::string, Image> images;
    std::map<ASR::symbol_t*, Image*> by_symbol;
    // A component of a heap type the heap does not hold yet, or "".
    std::string unsupported;

    // Whether a value of `st` holds an allocatable array, at any depth.
    bool holds_allocatable(ASR::Struct_t *st) {
        if (st == nullptr) return false;
        auto found = holds.find(st);
        if (found != holds.end()) return found->second;
        holds[st] = false;
        bool result = false;
        std::vector<ASR::symbol_t*> members;
        gpu_collect_data_members(st, members);
        for (ASR::symbol_t *m : members) {
            if (m == nullptr || !ASR::is_a<ASR::Variable_t>(*m)) continue;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(m);
            if (ASRUtils::is_pointer(v->m_type)) continue;
            if (ASRUtils::is_allocatable(v->m_type) &&
                    ASRUtils::is_array(v->m_type)) {
                result = true;
            } else if (holds_allocatable(struct_of(v->m_type,
                    v->m_type_declaration))) {
                result = true;
            }
        }
        holds[st] = result;
        return result;
    }

    bool is_heap(ASR::Struct_t *st) {
        return st != nullptr && heap.count(st->m_name) > 0;
    }

    // The image of a heap type, built the first time it is asked for.
    Image* image(ASR::Struct_t *st, std::function<size_t(ASR::ttype_t*,
            ASR::symbol_t*)> arena) {
        auto found = images.find(st->m_name);
        if (found != images.end()) {
            if (found->second.symbol == nullptr) {
                unsupported = "of the type '" + std::string(st->m_name) +
                    "', which holds a value of itself,";
                return nullptr;
            }
            return &found->second;
        }
        Image &img = images[st->m_name];
        img.source = st;
        const Location &loc = st->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        SymbolTable *global = unit.m_symtab;
        SymbolTable *symtab = al.make_new<SymbolTable>(global);
        SetChar names;
        names.reserve(al, 4);
        std::vector<std::pair<std::string, ImageMember>> members;
        auto add = [&](const std::string &name, ASR::ttype_t *type,
                ASR::symbol_t *decl) {
            b.VariableDeclaration(symtab, name, type, ASR::intentType::Local,
                decl);
            names.push_back(al, s2c(al, name));
            return symtab->get_symbol(name);
        };
        std::vector<ASR::symbol_t*> source_members;
        gpu_collect_data_members(st, source_members);
        for (ASR::symbol_t *m : source_members) {
            if (m == nullptr || !ASR::is_a<ASR::Variable_t>(*m)) continue;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(m);
            std::string name = v->m_name;
            ImageMember im;
            ASR::Struct_t *member_struct = struct_of(v->m_type,
                v->m_type_declaration);
            bool allocatable = ASRUtils::is_allocatable(v->m_type);
            if (ASRUtils::is_pointer(v->m_type) ||
                    ASRUtils::is_class_type(ASRUtils::extract_type(v->m_type))
                    || (allocatable && !ASRUtils::is_array(v->m_type))) {
                unsupported = "of the type '" + std::string(st->m_name) +
                    "', with the component '" + name + "',";
                images.erase(st->m_name);
                return nullptr;
            }
            if (allocatable) {
                ASR::ttype_t *element = ASRUtils::extract_type(v->m_type);
                im.kind = ImageMember::Kind::Array;
                if (member_struct != nullptr) {
                    if (!holds_allocatable(member_struct)) {
                        unsupported = "of the type '" +
                            std::string(st->m_name) + "', with the "
                            "allocatable component '" + name + "' of a type "
                            "without allocatable components,";
                        images.erase(st->m_name);
                        return nullptr;
                    }
                    heap.insert(member_struct->m_name);
                    im.image = image(member_struct, arena);
                    if (im.image == nullptr) return nullptr;
                    im.arena = arena(ASRUtils::make_StructType_t_util(al, loc,
                        im.image->symbol, true), im.image->symbol);
                } else {
                    im.arena = arena(element, nullptr);
                }
                ASR::ttype_t *int4 = int_type(al, loc);
                im.base = add("__gpu_" + name + "_base", int4, nullptr);
                size_t rank = ASRUtils::extract_n_dims_from_ttype(v->m_type);
                for (size_t d = 0; d < rank; d++) {
                    im.lower.push_back(add("__gpu_" + name + "_lb" +
                        std::to_string(d + 1), int4, nullptr));
                    im.extent.push_back(add("__gpu_" + name + "_n" +
                        std::to_string(d + 1), int4, nullptr));
                }
            } else if (member_struct != nullptr &&
                    holds_allocatable(member_struct)) {
                heap.insert(member_struct->m_name);
                im.kind = ImageMember::Kind::Image;
                im.image = image(member_struct, arena);
                if (im.image == nullptr) return nullptr;
                im.member = add(name, with_struct(al, v->m_type,
                    ASRUtils::make_StructType_t_util(al, loc,
                        im.image->symbol, true)), im.image->symbol);
            } else {
                im.kind = ImageMember::Kind::Plain;
                im.member = add(name, ASRUtils::duplicate_type(al, v->m_type),
                    v->m_type_declaration);
            }
            members.push_back({name, im});
        }
        std::string image_name = global->get_unique_name(
            "__gpu_heap_image_" + std::string(st->m_name), false);
        ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(
            ASR::make_Struct_t(al, loc, symtab, s2c(al, image_name), nullptr,
                nullptr, 0, names.p, names.n, nullptr, 0,
                ASR::abiType::Source, ASR::accessType::Public, false, false,
                false, nullptr, 0, nullptr, nullptr, nullptr, 0));
        ASR::down_cast<ASR::Struct_t>(sym)->m_struct_signature =
            ASRUtils::make_StructType_t_util(al, loc, sym, true);
        global->add_symbol(image_name, sym);
        // The map may have grown while the members were built.
        Image &done = images[st->m_name];
        done.source = st;
        done.symbol = sym;
        done.members = members;
        by_symbol[sym] = &done;
        return &done;
    }

    ASR::ttype_t* image_type(Image *img) {
        return ASRUtils::make_StructType_t_util(al, img->symbol->base.loc,
            img->symbol, true);
    }

    // The image a value of device code has, when it is a value of an image
    // type: past arrays, a whole array of images included.
    Image* image_of(ASR::expr_t *e) {
        if (e == nullptr) return nullptr;
        ASR::ttype_t *t = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(e)));
        if (!ASR::is_a<ASR::StructType_t>(*t)) return nullptr;
        ASR::symbol_t *s = nullptr;
        try {
            s = ASRUtils::get_struct_sym_from_struct_expr(
                ASRUtils::get_past_array_physical_cast(e));
        } catch (const LCompilersException &) {
            return nullptr;
        }
        if (s == nullptr) return nullptr;
        auto found = by_symbol.find(ASRUtils::symbol_get_past_external(s));
        return found == by_symbol.end() ? nullptr : found->second;
    }

    // The heap type a value of device code that is not an image has.
    ASR::Struct_t* heap_source_of(ASR::expr_t *e) {
        if (e == nullptr || image_of(e) != nullptr) return nullptr;
        ASR::ttype_t *t = ASRUtils::type_get_past_array(
            ASRUtils::type_get_past_allocatable_pointer(
                ASRUtils::expr_type(e)));
        if (!ASR::is_a<ASR::StructType_t>(*t)) return nullptr;
        ASR::symbol_t *s = nullptr;
        try {
            s = ASRUtils::get_struct_sym_from_struct_expr(
                ASRUtils::get_past_array_physical_cast(e));
        } catch (const LCompilersException &) {
            return nullptr;
        }
        ASR::Struct_t *st = struct_of(s);
        return is_heap(st) ? st : nullptr;
    }

private:
    Allocator &al;
    ASR::TranslationUnit_t &unit;
    std::map<ASR::Struct_t*, bool> holds;
};


// Initializes the image `x`: every allocatable array it holds, at any
// depth, is not allocated.
void initialize_image(Allocator &al, const Location &loc, Image *img,
        ASR::expr_t *x, std::vector<ASR::stmt_t*> &out,
        std::string &unsupported) {
    ASRUtils::ASRBuilder b(al, loc);
    for (auto &m : img->members) {
        ImageMember &im = m.second;
        if (im.kind == ImageMember::Kind::Array) {
            out.push_back(b.Assignment(ASRUtils::EXPR(
                ASR::make_StructInstanceMember_t(al, loc,
                    duplicate_expr(al, x), im.base, int_type(al, loc),
                    nullptr)), b.i32(-1)));
        } else if (im.kind == ImageMember::Kind::Image) {
            ASR::ttype_t *type = ASRUtils::symbol_type(im.member);
            if (ASRUtils::is_array(type)) {
                unsupported = "an array component of the type '" +
                    std::string(img->source->m_name) + "',";
                return;
            }
            initialize_image(al, loc, im.image, ASRUtils::EXPR(
                ASR::make_StructInstanceMember_t(al, loc,
                    duplicate_expr(al, x), im.member, type, nullptr)), out,
                unsupported);
        }
    }
}

// The procedures the heap types need on the device: copying an image into
// another, as an assignment of a whole value does, and copying a value the
// host handed over into an image.
class HeapRoutines {
public:
    HeapRoutines(Allocator &al_, ASR::TranslationUnit_t &unit_,
            DeviceHeap &heap_, HeapTypes &types_)
        : al(al_), unit(unit_), heap(heap_), types(types_) {}

    // Every procedure made, with the kernel it belongs to.
    std::vector<std::pair<ASR::Function_t*, ASR::Function_t*>> made;
    std::string unsupported;

    // Every kernel gets procedures of its own, which the lowering of its
    // `stop` statements can give hidden arguments.
    ASR::Function_t* copy(ASR::Function_t *kernel, Image *img) {
        ASR::Function_t *&memo = copies[{kernel, img}];
        if (memo != nullptr) return memo;
        const Location &loc = img->symbol->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::Function_t *fn = routine(kernel, "__gpu_heap_copy_" +
            std::string(img->source->m_name));
        memo = fn;
        ASR::expr_t *src = dummy(*fn, "src", types.image_type(img),
            img->symbol, ASR::intentType::In);
        ASR::expr_t *dst = dummy(*fn, "dst", types.image_type(img),
            img->symbol, ASR::intentType::Out);
        std::vector<ASR::stmt_t*> body;
        body.push_back(b.Assignment(dst, src));
        for (auto &m : img->members) {
            ImageMember &im = m.second;
            if (im.kind == ImageMember::Kind::Image) {
                ASR::ttype_t *type = ASRUtils::symbol_type(im.member);
                if (ASRUtils::is_array(type)) {
                    unsupported = "an array component of the type '" +
                        std::string(img->source->m_name) + "',";
                    continue;
                }
                ASR::expr_t *tmp = local(*fn, "__gpu_" + m.first,
                    types.image_type(im.image), im.image->symbol);
                body.push_back(call(copy(kernel, im.image), {member(src, im.member),
                    tmp}));
                body.push_back(b.Assignment(member(dst, im.member), tmp));
                continue;
            }
            if (im.kind != ImageMember::Kind::Array) continue;
            ASR::expr_t *n = count(src, im);
            ASR::expr_t *size = local(*fn, "__gpu_n_" + m.first,
                int_type(al, loc), nullptr);
            std::vector<ASR::stmt_t*> copied;
            copied.push_back(b.Assignment(size, n));
            heap.allocate(*fn, im.arena, size, member(dst, im.base), loc,
                copied);
            ASR::expr_t *k = local(*fn, "__gpu_k_" + m.first,
                int_type(al, loc), nullptr);
            ASR::expr_t *arena = heap.arena_array(*fn, im.arena);
            ASR::expr_t *from = b.ArrayItem_01(arena,
                {b.Add(member(src, im.base), k)});
            ASR::expr_t *to = b.ArrayItem_01(heap.arena_array(*fn, im.arena),
                {b.Add(member(dst, im.base), k)});
            std::vector<ASR::stmt_t*> each;
            if (im.image != nullptr) {
                ASR::expr_t *tmp = local(*fn, "__gpu_e_" + m.first,
                    types.image_type(im.image), im.image->symbol);
                each.push_back(call(copy(kernel, im.image), {from, tmp}));
                each.push_back(b.Assignment(to, tmp));
            } else {
                each.push_back(b.Assignment(to, from));
            }
            copied.push_back(b.DoLoop(k, b.i32(1), size, each));
            body.push_back(b.If(b.GtE(member(src, im.base), b.i32(0)),
                copied, {}));
        }
        set_body(*fn, body);
        return fn;
    }

    // Copies a value of `source` that device code reads from the buffers
    // the host handed over into an image.
    ASR::Function_t* copy_in(ASR::Function_t *kernel, Image *img,
            ASR::Struct_t *source) {
        ASR::Function_t *&memo = copies_in[{kernel, img}];
        if (memo != nullptr) return memo;
        const Location &loc = img->symbol->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::Function_t *fn = routine(kernel, "__gpu_heap_copy_in_" +
            std::string(img->source->m_name));
        memo = fn;
        ASR::expr_t *src = dummy(*fn, "src", ASRUtils::make_StructType_t_util(
            al, loc, &source->base, true), &source->base,
            ASR::intentType::In);
        ASR::expr_t *dst = dummy(*fn, "dst", types.image_type(img),
            img->symbol, ASR::intentType::Out);
        std::vector<ASR::stmt_t*> body;
        std::vector<ASR::symbol_t*> source_members;
        gpu_collect_data_members(source, source_members);
        for (auto &m : img->members) {
            ImageMember &im = m.second;
            ASR::symbol_t *from_member = nullptr;
            for (ASR::symbol_t *s : source_members) {
                if (s != nullptr && m.first == ASRUtils::symbol_name(s)) {
                    from_member = s;
                }
            }
            LCOMPILERS_ASSERT(from_member != nullptr);
            ASR::expr_t *from = member(src, from_member);
            if (im.kind == ImageMember::Kind::Plain) {
                body.push_back(b.Assignment(member(dst, im.member), from));
                continue;
            }
            if (im.kind == ImageMember::Kind::Image) {
                ASR::ttype_t *type = ASRUtils::symbol_type(im.member);
                if (ASRUtils::is_array(type)) {
                    unsupported = "an array component of the type '" +
                        std::string(img->source->m_name) + "',";
                    continue;
                }
                ASR::expr_t *tmp = local(*fn, "__gpu_" + m.first,
                    types.image_type(im.image), im.image->symbol);
                body.push_back(call(copy_in(kernel, im.image, struct_of(
                    ASR::down_cast<ASR::Variable_t>(from_member)
                        ->m_type_declaration)), {from, tmp}));
                body.push_back(b.Assignment(member(dst, im.member), tmp));
                continue;
            }
            if (im.image != nullptr) {
                unsupported = "an allocatable component of a derived type "
                    "the host hands over, of the type '" +
                    std::string(img->source->m_name) + "',";
                continue;
            }
            size_t rank = im.extent.size();
            for (size_t d = 0; d < rank; d++) {
                body.push_back(b.Assignment(member(dst, im.lower[d]),
                    ASRUtils::EXPR(ASR::make_ArrayBound_t(al, loc,
                        duplicate_expr(al, from), b.i32(d + 1),
                        int_type(al, loc), ASR::arrayboundType::LBound,
                        nullptr))));
                body.push_back(b.Assignment(member(dst, im.extent[d]),
                    b.ArraySize(duplicate_expr(al, from), b.i32(d + 1),
                        int_type(al, loc))));
            }
            ASR::expr_t *size = local(*fn, "__gpu_n_" + m.first,
                int_type(al, loc), nullptr);
            body.push_back(b.Assignment(size, count(dst, im)));
            heap.allocate(*fn, im.arena, size, member(dst, im.base), loc,
                body);
            // One loop per dimension, the first innermost, over the
            // elements in array element order.
            std::vector<ASR::expr_t*> index;
            for (size_t d = 0; d < rank; d++) {
                index.push_back(local(*fn, "__gpu_i" + std::to_string(d + 1)
                    + "_" + m.first, int_type(al, loc), nullptr));
            }
            ASR::expr_t *position = nullptr;
            ASR::expr_t *stride = nullptr;
            std::vector<ASR::expr_t*> subscripts;
            for (size_t d = 0; d < rank; d++) {
                ASR::expr_t *term = b.Sub(index[d],
                    member(dst, im.lower[d]));
                if (stride != nullptr) term = b.Mul(term, stride);
                position = position ? b.Add(position, term) : term;
                ASR::expr_t *n = member(dst, im.extent[d]);
                stride = stride ? b.Mul(stride, n) : n;
                subscripts.push_back(index[d]);
            }
            ASR::expr_t *to = b.ArrayItem_01(heap.arena_array(*fn, im.arena),
                {b.Add(b.Add(member(dst, im.base), b.i32(1)), position)});
            ASR::stmt_t *loop = b.Assignment(to,
                b.ArrayItem_01(duplicate_expr(al, from), subscripts));
            for (size_t d = 0; d < rank; d++) {
                ASR::expr_t *lower = member(dst, im.lower[d]);
                loop = b.DoLoop(index[d], lower, b.Sub(b.Add(
                    member(dst, im.lower[d]), member(dst, im.extent[d])),
                    b.i32(1)), {loop});
            }
            body.push_back(loop);
        }
        set_body(*fn, body);
        return fn;
    }

private:
    Allocator &al;
    ASR::TranslationUnit_t &unit;
    DeviceHeap &heap;
    HeapTypes &types;

    std::map<std::pair<ASR::Function_t*, Image*>, ASR::Function_t*> copies,
        copies_in;

    ASR::Function_t* routine(ASR::Function_t *kernel,
            const std::string &name) {
        const Location &loc = unit.base.base.loc;
        SymbolTable *global = kernel->m_symtab;
        SymbolTable *symtab = al.make_new<SymbolTable>(global);
        std::string unique = global->get_unique_name(name, false);
        ASR::asr_t *fn = ASRUtils::make_Function_t_util(al, loc, symtab,
            s2c(al, unique), nullptr, 0, nullptr, 0, nullptr, 0, nullptr,
            ASR::abiType::Source, ASR::accessType::Public,
            ASR::deftypeType::Implementation, nullptr, false, false, false,
            false, false, nullptr, 0, false, false, false, nullptr);
        ASR::symbol_t *sym = ASR::down_cast<ASR::symbol_t>(fn);
        global->add_symbol(unique, sym);
        ASR::Function_t *f = ASR::down_cast<ASR::Function_t>(sym);
        ASRUtils::get_FunctionType(f)->m_exec_space =
            ASR::exec_spaceType::Device;
        made.push_back({f, kernel});
        return f;
    }

    ASR::expr_t* dummy(ASR::Function_t &fn, const std::string &name,
            ASR::ttype_t *type, ASR::symbol_t *decl,
            ASR::intentType intent) {
        ASRUtils::ASRBuilder b(al, fn.base.base.loc);
        ASR::expr_t *v = b.Variable(fn.m_symtab, name, type, intent, decl);
        Vec<ASR::expr_t*> args;
        args.reserve(al, fn.n_args + 1);
        for (size_t i = 0; i < fn.n_args; i++) args.push_back(al, fn.m_args[i]);
        args.push_back(al, v);
        fn.m_args = args.p;
        fn.n_args = args.n;
        fn.m_function_signature = ASRUtils::TYPE(
            ASRUtils::make_FunctionType_t_util(al, fn.base.base.loc,
                fn.m_args, fn.n_args, nullptr, ASRUtils::get_FunctionType(fn),
                fn.m_symtab));
        return ASRUtils::EXPR(ASR::make_Var_t(al, fn.base.base.loc,
            ASR::down_cast<ASR::Var_t>(v)->m_v));
    }

    ASR::expr_t* local(ASR::Function_t &fn, const std::string &name,
            ASR::ttype_t *type, ASR::symbol_t *decl) {
        ASRUtils::ASRBuilder b(al, fn.base.base.loc);
        return b.Variable(fn.m_symtab, fn.m_symtab->get_unique_name(name,
            false), type, ASR::intentType::Local, decl);
    }

    ASR::expr_t* member(ASR::expr_t *base, ASR::symbol_t *m) {
        return ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al,
            base->base.loc, duplicate_expr(al, base), m,
            ASRUtils::duplicate_type(al, ASRUtils::symbol_type(m)), nullptr));
    }

    ASR::expr_t* count(ASR::expr_t *img, ImageMember &im) {
        ASRUtils::ASRBuilder b(al, img->base.loc);
        ASR::expr_t *n = member(img, im.extent[0]);
        for (size_t d = 1; d < im.extent.size(); d++) {
            n = b.Mul(n, member(img, im.extent[d]));
        }
        return n;
    }

    ASR::stmt_t* call(ASR::Function_t *callee,
            std::vector<ASR::expr_t*> actuals) {
        const Location &loc = callee->base.base.loc;
        Vec<ASR::call_arg_t> args;
        args.reserve(al, actuals.size());
        for (ASR::expr_t *a : actuals) {
            ASR::call_arg_t arg;
            arg.loc = loc;
            arg.m_value = a;
            args.push_back(al, arg);
        }
        return ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(al, loc,
            &callee->base, nullptr, args.p, args.n, nullptr, nullptr, false));
    }

    void set_body(ASR::Function_t &fn, std::vector<ASR::stmt_t*> &body) {
        Vec<ASR::stmt_t*> v;
        v.reserve(al, body.size());
        for (ASR::stmt_t *s : body) v.push_back(al, s);
        fn.m_body = v.p;
        fn.n_body = v.n;
    }
};


// Rewrites the references to heap arrays and heap values of one procedure
// of device code.
class HeapReplacer : public ASR::BaseExprReplacer<HeapReplacer> {
    using Base = ASR::BaseExprReplacer<HeapReplacer>;
public:
    HeapReplacer(Allocator &al_, DeviceHeap &heap_, HeapTypes &types_,
            HeapRoutines &routines_, ASR::Function_t &fn_,
            ASR::Function_t *kernel_,
            std::map<ASR::symbol_t*, Descriptor> &locals_)
        : al(al_), heap(heap_), types(types_), routines(routines_), fn(fn_),
          kernel(kernel_), locals(locals_) {}


    // Statements a reference needs before the statement that holds it.
    std::vector<ASR::stmt_t*> before;
    // A use the heap does not lower yet, or "".
    std::string unsupported;

    ASR::expr_t* var(ASR::symbol_t *s, const Location &loc) {
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, s));
    }

    ASR::expr_t* field(const Descriptor &d, ASR::symbol_t *s,
            const Location &loc) {
        if (d.root == nullptr) return var(s, loc);
        return ASRUtils::EXPR(ASR::make_StructInstanceMember_t(al, loc,
            duplicate_expr(al, d.root), s, int_type(al, loc), nullptr));
    }

    ASR::expr_t* replaced(ASR::expr_t *e) {
        if (e == nullptr) return nullptr;
        ASR::expr_t **outer = current_expr;
        current_expr = &e;
        replace_expr(e);
        current_expr = outer;
        return e;
    }

    // The heap array `e` designates: a local heap array, or an allocatable
    // array component of an image. Rewrites the image it is a component of.
    Descriptor* designator(ASR::expr_t *e) {
        if (e == nullptr) return nullptr;
        e = ASRUtils::get_past_array_physical_cast(e);
        if (ASR::is_a<ASR::Var_t>(*e)) {
            auto it = locals.find(ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(e)->m_v));
            return it == locals.end() ? nullptr : &it->second;
        }
        if (!ASR::is_a<ASR::StructInstanceMember_t>(*e)) return nullptr;
        ASR::StructInstanceMember_t *x =
            ASR::down_cast<ASR::StructInstanceMember_t>(e);
        x->m_v = replaced(x->m_v);
        Image *img = types.image_of(x->m_v);
        if (img == nullptr) return nullptr;
        ImageMember *im = img->member(ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(x->m_m)));
        if (im == nullptr || im->kind != ImageMember::Kind::Array) {
            return nullptr;
        }
        Descriptor d;
        d.arena = im->arena;
        d.root = x->m_v;
        d.base = im->base;
        d.lower = im->lower;
        d.extent = im->extent;
        d.element_image = im->image;
        d.name = ASRUtils::symbol_name(ASRUtils::symbol_get_past_external(
            x->m_m));
        components.push_back(d);
        return &components.back();
    }

    // The number of elements of the array.
    ASR::expr_t* count(const Descriptor &h, const Location &loc) {
        ASRUtils::ASRBuilder b(al, loc);
        ASR::expr_t *n = field(h, h.extent[0], loc);
        for (size_t d = 1; d < h.extent.size(); d++) {
            n = b.Mul(n, field(h, h.extent[d], loc));
        }
        return n;
    }

    // The section of the arena holding the elements of the array, in array
    // element order.
    ASR::expr_t* elements(const Descriptor &h, ASR::expr_t *first,
            ASR::expr_t *last, const Location &loc) {
        ASRUtils::ASRBuilder b(al, loc);
        Vec<ASR::array_index_t> range;
        range.reserve(al, 1);
        ASR::array_index_t index;
        index.loc = loc;
        index.m_left = first;
        index.m_right = last;
        index.m_step = b.i32(1);
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
    ASR::expr_t* whole(const Descriptor &h, const Location &loc) {
        ASRUtils::ASRBuilder b(al, loc);
        if (h.extent.size() != 1) {
            unsupported = "'" + h.name + "' of rank " +
                std::to_string(h.extent.size()) + ", used as a whole,";
        }
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
        ASR::expr_t *view = b.Variable(fn.m_symtab,
            fn.m_symtab->get_unique_name("__gpu_heap_view_" + h.name, false),
            type, ASR::intentType::Local,
            heap.arenas[h.arena].type_declaration);
        ASR::expr_t *base = field(h, h.base, loc);
        before.push_back(ASRUtils::STMT(ASR::make_Associate_t(al, loc,
            view, elements(h, b.Add(base, b.i32(1)),
                b.Add(field(h, h.base, loc), count(h, loc)), loc))));
        return var(ASR::down_cast<ASR::Var_t>(view)->m_v, loc);
    }

    ASR::expr_t* as_int4(ASR::expr_t *e, const Location &loc) {
        if (ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(e)) == 4) {
            return e;
        }
        ASRUtils::ASRBuilder b(al, loc);
        return b.i2i_t(e, int_type(al, loc));
    }

    void replace_ArrayItem(ASR::ArrayItem_t *x) {
        Descriptor *h = designator(x->m_v);
        if (h == nullptr) {
            Base::replace_ArrayItem(x);
            fix_struct_type(&x->base, x->m_type);
            return;
        }
        const Location &loc = x->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::expr_t *offset = nullptr;
        ASR::expr_t *stride = nullptr;
        for (size_t d = 0; d < x->n_args; d++) {
            ASR::expr_t *sub = as_int4(replaced(x->m_args[d].m_right), loc);
            ASR::expr_t *term = b.Sub(sub, field(*h, h->lower[d], loc));
            if (stride != nullptr) term = b.Mul(term, stride);
            offset = offset ? b.Add(offset, term) : term;
            ASR::expr_t *n = field(*h, h->extent[d], loc);
            stride = stride ? b.Mul(stride, n) : n;
        }
        ASR::expr_t *position = b.Add(b.Add(field(*h, h->base, loc),
            b.i32(1)), offset);
        *current_expr = b.ArrayItem_01(heap.arena_array(fn, h->arena),
            {position});
    }

    void replace_ArraySize(ASR::ArraySize_t *x) {
        Descriptor *h = designator(x->m_v);
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
                unsupported = "'" + h->name + "', with the size of a "
                    "dimension known only at run time,";
                return;
            }
            n = field(*h, h->extent[d - 1], loc);
        }
        if (ASRUtils::extract_kind_from_ttype_t(x->m_type) != 4) {
            n = b.i2i_t(n, x->m_type);
        }
        *current_expr = n;
    }

    void replace_ArrayBound(ASR::ArrayBound_t *x) {
        Descriptor *h = designator(x->m_v);
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
            unsupported = "'" + h->name + "', with a bound of a dimension "
                "known only at run time,";
            return;
        }
        if (x->m_dim == nullptr && h->extent.size() != 1) {
            unsupported = "'" + h->name + "', with all its bounds at once,";
            return;
        }
        ASR::expr_t *lower = field(*h, h->lower[d - 1], loc);
        ASR::expr_t *bound = x->m_bound == ASR::arrayboundType::LBound
            ? lower
            : b.Sub(b.Add(lower, field(*h, h->extent[d - 1], loc)), b.i32(1));
        if (ASRUtils::extract_kind_from_ttype_t(x->m_type) != 4) {
            bound = b.i2i_t(bound, x->m_type);
        }
        *current_expr = bound;
    }

    // A section with one range, along the first dimension and with a step
    // of one, is the elements of the arena between its first and its last.
    // A procedure that gets a section takes its elements to be next to each
    // other, which the device does not check (ArrayIsContiguous is true
    // there), so a section whose elements are not is not lowered yet.
    void replace_ArraySection(ASR::ArraySection_t *x) {
        Descriptor *h = designator(x->m_v);
        if (h == nullptr) {
            Base::replace_ArraySection(x);
            return;
        }
        const Location &loc = x->base.base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        if (x->n_args != h->extent.size()) {
            unsupported = "'" + h->name + "', a section of it,";
            return;
        }
        int range = -1;
        for (size_t d = 0; d < x->n_args; d++) {
            x->m_args[d].m_left = replaced(x->m_args[d].m_left);
            x->m_args[d].m_right = replaced(x->m_args[d].m_right);
            x->m_args[d].m_step = replaced(x->m_args[d].m_step);
            bool scalar = x->m_args[d].m_left == nullptr &&
                x->m_args[d].m_step == nullptr && x->m_args[d].m_right;
            if (scalar) continue;
            if (range >= 0) {
                unsupported = "'" + h->name + "', a section of it with "
                    "more than one range,";
                return;
            }
            range = d;
        }
        ASR::array_index_t &r = x->m_args[range < 0 ? 0 : range];
        int64_t step = 1;
        if (range != 0 || (r.m_step != nullptr && (!ASRUtils::extract_value(
                ASRUtils::expr_value(r.m_step), step) || step != 1))) {
            unsupported = "'" + h->name + "', a section of it whose elements "
                "are not contiguous,";
            return;
        }
        ASR::expr_t *offset = b.Add(field(*h, h->base, loc), b.i32(1));
        ASR::expr_t *stride = field(*h, h->extent[0], loc);
        for (size_t d = 1; d < x->n_args; d++) {
            ASR::expr_t *term = b.Mul(b.Sub(as_int4(x->m_args[d].m_right,
                loc), field(*h, h->lower[d], loc)), stride);
            offset = b.Add(offset, term);
            stride = b.Mul(stride, field(*h, h->extent[d], loc));
        }
        ASR::expr_t *lower = field(*h, h->lower[0], loc);
        ASR::expr_t *left = r.m_left ? as_int4(r.m_left, loc) : lower;
        ASR::expr_t *right = r.m_right ? as_int4(r.m_right, loc)
            : b.Sub(b.Add(field(*h, h->lower[0], loc),
                field(*h, h->extent[0], loc)), b.i32(1));
        *current_expr = elements(*h,
            b.Add(offset, b.Sub(left, field(*h, h->lower[0], loc))),
            b.Add(duplicate_expr(al, offset), b.Sub(right, lower)), loc);
    }

    void replace_ArrayIsContiguous(ASR::ArrayIsContiguous_t *x) {
        if (designator(x->m_array) != nullptr) {
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
            if (Descriptor *h = designator(x->m_args[0])) {
                ASRUtils::ASRBuilder b(al, x->base.base.loc);
                *current_expr = b.GtE(field(*h, h->base, x->base.base.loc),
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
        auto it = locals.find(ASRUtils::symbol_get_past_external(x->m_v));
        if (it == locals.end()) return;
        *current_expr = whole(it->second, x->base.base.loc);
    }

    void replace_StructInstanceMember(ASR::StructInstanceMember_t *x) {
        if (Descriptor *h = designator(&x->base)) {
            *current_expr = whole(*h, x->base.base.loc);
            return;
        }
        x->m_v = replaced(x->m_v);
        Image *img = types.image_of(x->m_v);
        if (img == nullptr) return;
        ImageMember *im = img->member(ASRUtils::symbol_name(
            ASRUtils::symbol_get_past_external(x->m_m)));
        if (im == nullptr) return;
        x->m_m = im->member;
        x->m_type = ASRUtils::duplicate_type(al,
            ASRUtils::symbol_type(im->member));
    }

    void replace_FunctionCall(ASR::FunctionCall_t *x) {
        Base::replace_FunctionCall(x);
        ASR::symbol_t *target = call_target(x->m_name);
        if (target == nullptr || !ASR::is_a<ASR::Function_t>(*target)) return;
        ASR::Function_t *callee = ASR::down_cast<ASR::Function_t>(target);
        if (callee->m_return_var != nullptr &&
                types.image_of(callee->m_return_var) != nullptr) {
            x->m_type = ASRUtils::duplicate_type(al,
                ASRUtils::expr_type(callee->m_return_var));
        }
        for (size_t i = 0; i < x->n_args && i < callee->n_args; i++) {
            if (x->m_args[i].m_value == nullptr) continue;
            ASR::Variable_t *d = ASR::down_cast<ASR::Variable_t>(
                ASR::down_cast<ASR::Var_t>(callee->m_args[i])->m_v);
            Image *img = types.image_of(callee->m_args[i]);
            if (img == nullptr) continue;
            ASR::Struct_t *source = types.heap_source_of(
                x->m_args[i].m_value);
            if (source == nullptr) continue;
            if (d->m_intent != ASR::intentType::In ||
                    ASRUtils::is_array(d->m_type)) {
                unsupported = "a value the host hands over, which '" +
                    std::string(callee->m_name) + "' defines,";
                continue;
            }
            x->m_args[i].m_value = copied_in(x->m_args[i].m_value, img,
                source);
        }
    }

    // A new image the value `value` the host handed over is copied into
    // before the statement.
    ASR::expr_t* copied_in(ASR::expr_t *value, Image *img,
            ASR::Struct_t *source) {
        const Location &loc = value->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        ASR::expr_t *tmp = b.Variable(fn.m_symtab,
            fn.m_symtab->get_unique_name("__gpu_heap_in", false),
            types.image_type(img), ASR::intentType::Local, img->symbol);
        Vec<ASR::call_arg_t> args;
        args.reserve(al, 2);
        for (ASR::expr_t *a : {value, tmp}) {
            ASR::call_arg_t arg;
            arg.loc = loc;
            arg.m_value = a;
            args.push_back(al, arg);
        }
        before.push_back(ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(
            al, loc, &routines.copy_in(kernel, img, source)->base, nullptr,
            args.p,
            args.n, nullptr, nullptr, false)));
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc,
            ASR::down_cast<ASR::Var_t>(tmp)->m_v));
    }

private:
    Allocator &al;
    DeviceHeap &heap;
    HeapTypes &types;
    HeapRoutines &routines;
    ASR::Function_t &fn;
    ASR::Function_t *kernel;
    std::map<ASR::symbol_t*, Descriptor> &locals;
    std::deque<Descriptor> components;

    // An element of an array of images is an image.
    void fix_struct_type(ASR::expr_t *e, ASR::ttype_t *&type) {
        if (!ASR::is_a<ASR::StructType_t>(*type)) return;
        Image *img = types.image_of(e);
        if (img != nullptr) type = types.image_type(img);
    }
};

class HeapStatements :
        public ASR::CallReplacerOnExpressionsVisitor<HeapStatements> {
public:
    HeapReplacer &replacer;
    explicit HeapStatements(HeapReplacer &r) : replacer(r) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};


// Lowers the heap arrays and the heap values of one procedure of device
// code.
class HeapLowering {
public:
    HeapLowering(Allocator &al_, DeviceHeap &heap_, HeapTypes &types_,
            HeapRoutines &routines_, ASR::Function_t &fn_,
            ASR::Function_t *kernel_,
            std::map<ASR::symbol_t*, Descriptor> &locals_)
        : al(al_), heap(heap_), types(types_), routines(routines_), fn(fn_),
          kernel(kernel_),
          replacer(al_, heap_, types_, routines_, fn_, kernel_, locals_),
          statements(replacer) {}

    std::string unsupported;

    void run() {
        std::set<ASR::symbol_t*> visited;
        transform(fn.m_body, fn.n_body, visited);
        if (!replacer.unsupported.empty()) unsupported = replacer.unsupported;
    }

private:
    Allocator &al;
    DeviceHeap &heap;
    HeapTypes &types;
    HeapRoutines &routines;
    ASR::Function_t &fn;
    ASR::Function_t *kernel;
    HeapReplacer replacer;
    HeapStatements statements;

    ASR::expr_t* var(ASR::symbol_t *s, const Location &loc) {
        return ASRUtils::EXPR(ASR::make_Var_t(al, loc, s));
    }

    void expression(ASR::expr_t *&e) {
        if (e == nullptr) return;
        replacer.current_expr = &e;
        replacer.replace_expr(e);
    }

    ASR::expr_t* image_local(Image *img, const Location &loc) {
        ASRUtils::ASRBuilder b(al, loc);
        ASR::expr_t *v = b.Variable(fn.m_symtab,
            fn.m_symtab->get_unique_name("__gpu_heap_value", false),
            types.image_type(img), ASR::intentType::Local, img->symbol);
        return var(ASR::down_cast<ASR::Var_t>(v)->m_v, loc);
    }

    ASR::stmt_t* call(ASR::Function_t *callee, ASR::expr_t *a,
            ASR::expr_t *b_, const Location &loc) {
        Vec<ASR::call_arg_t> args;
        args.reserve(al, 2);
        for (ASR::expr_t *e : {a, b_}) {
            ASR::call_arg_t arg;
            arg.loc = loc;
            arg.m_value = e;
            args.push_back(al, arg);
        }
        return ASRUtils::STMT(ASRUtils::make_SubroutineCall_t_util(al, loc,
            &callee->base, nullptr, args.p, args.n, nullptr, nullptr,
            false));
    }

    // Allocates `h` with the dimensions `dims`.
    void allocate(Descriptor &h, ASR::dimension_t *dims, size_t n_dims,
            const Location &loc, std::vector<ASR::stmt_t*> &out) {
        ASRUtils::ASRBuilder b(al, loc);
        for (size_t d = 0; d < n_dims; d++) {
            ASR::expr_t *start = dims[d].m_start ? dims[d].m_start : b.i32(1);
            ASR::expr_t *length = dims[d].m_length;
            expression(start);
            expression(length);
            out.push_back(b.Assignment(replacer.field(h, h.lower[d], loc),
                replacer.as_int4(start, loc)));
            out.push_back(b.Assignment(replacer.field(h, h.extent[d], loc),
                b.Max(replacer.as_int4(length, loc), b.i32(0))));
        }
        ASR::expr_t *n = replacer.count(h, loc);
        heap.allocate(fn, h.arena, n, replacer.field(h, h.base, loc), loc,
            out);
        if (h.element_image == nullptr) return;
        // The new elements are images with nothing allocated.
        ASR::expr_t *k = b.Variable(fn.m_symtab,
            fn.m_symtab->get_unique_name("__gpu_heap_k", false),
            int_type(al, loc), ASR::intentType::Local);
        std::vector<ASR::stmt_t*> each;
        initialize_image(al, loc, h.element_image,
            b.ArrayItem_01(heap.arena_array(fn, h.arena),
                {b.Add(replacer.field(h, h.base, loc), k)}), each,
            unsupported);
        out.push_back(b.DoLoop(k, b.i32(1), replacer.count(h, loc), each));
    }

    // Whether the descriptor already has the extents `dims`.
    ASR::expr_t* same_shape(Descriptor &h, ASR::dimension_t *dims,
            size_t n_dims, const Location &loc) {
        ASRUtils::ASRBuilder b(al, loc);
        ASR::expr_t *same = b.GtE(replacer.field(h, h.base, loc), b.i32(0));
        for (size_t d = 0; d < n_dims; d++) {
            ASR::expr_t *length = duplicate_expr(al, dims[d].m_length);
            expression(length);
            same = b.And(same, b.Eq(replacer.field(h, h.extent[d], loc),
                b.Max(replacer.as_int4(length, loc), b.i32(0))));
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
            Descriptor *h = replacer.designator(args[k].m_a);
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

    // `deallocate` marks a heap array unallocated; an image holds nothing
    // that has to be freed.
    void deallocation(ASR::expr_t **vars, size_t n_vars,
            std::function<void(ASR::expr_t**, size_t)> set, ASR::stmt_t *s,
            Vec<ASR::stmt_t*> &out) {
        Vec<ASR::expr_t*> kept;
        kept.reserve(al, n_vars);
        std::vector<ASR::stmt_t*> lowered;
        for (size_t k = 0; k < n_vars; k++) {
            Descriptor *h = replacer.designator(vars[k]);
            if (h != nullptr) {
                ASRUtils::ASRBuilder b(al, s->base.loc);
                lowered.push_back(b.Assignment(replacer.field(*h, h->base,
                    s->base.loc), b.i32(-1)));
                continue;
            }
            if (types.image_of(vars[k]) != nullptr) continue;
            kept.push_back(al, vars[k]);
        }
        if (kept.size() > 0) {
            set(kept.p, kept.n);
            out.push_back(al, s);
        }
        flush_before(out);
        for (ASR::stmt_t *x : lowered) out.push_back(al, x);
    }

    // An assignment of a whole image copies what it holds; one of a value
    // the host handed over copies that into the image.
    void assignment(ASR::stmt_t *s, Vec<ASR::stmt_t*> &out) {
        ASR::Assignment_t *x = ASR::down_cast<ASR::Assignment_t>(s);
        expression(x->m_target);
        expression(x->m_value);
        flush_before(out);
        const Location &loc = s->base.loc;
        ASRUtils::ASRBuilder b(al, loc);
        bool scalar = !ASRUtils::is_array(ASRUtils::expr_type(x->m_target));
        Image *target = scalar ? types.image_of(x->m_target) : nullptr;
        if (target != nullptr) {
            ASR::expr_t *tmp = image_local(target, loc);
            if (types.image_of(x->m_value) == target) {
                out.push_back(al, call(routines.copy(kernel, target),
                    x->m_value, tmp, loc));
            } else if (ASR::Struct_t *source = types.heap_source_of(
                    x->m_value)) {
                out.push_back(al, call(routines.copy_in(kernel, target,
                    source), x->m_value, tmp, loc));
            } else {
                unsupported = "of the type '" +
                    std::string(target->source->m_name) + "', assigned a "
                    "value of another kind,";
                return;
            }
            out.push_back(al, b.Assignment(x->m_target, tmp));
            return;
        }
        if (scalar && types.heap_source_of(x->m_target) != nullptr &&
                types.image_of(x->m_value) != nullptr) {
            unsupported = "of the type '" + std::string(types.heap_source_of(
                x->m_target)->m_name) + "', stored into a value the host "
                "handed over,";
            return;
        }
        out.push_back(al, s);
    }

    // A value the host handed over is copied into an image for a dummy
    // that is one, and an image in device memory goes through a local of
    // the caller for a dummy the procedure defines.
    void subroutine_call(ASR::stmt_t *s, Vec<ASR::stmt_t*> &out) {
        statements.visit_stmt(*s);
        ASR::SubroutineCall_t *x = ASR::down_cast<ASR::SubroutineCall_t>(s);
        ASR::symbol_t *target = call_target(x->m_name);
        std::vector<ASR::stmt_t*> after;
        if (target != nullptr && ASR::is_a<ASR::Function_t>(*target)) {
            ASR::Function_t *callee = ASR::down_cast<ASR::Function_t>(target);
            for (size_t i = 0; i < x->n_args && i < callee->n_args; i++) {
                ASR::expr_t *&actual = x->m_args[i].m_value;
                if (actual == nullptr) continue;
                Image *img = types.image_of(callee->m_args[i]);
                if (img == nullptr) continue;
                ASR::Variable_t *d = ASR::down_cast<ASR::Variable_t>(
                    ASR::down_cast<ASR::Var_t>(callee->m_args[i])->m_v);
                if (ASRUtils::is_array(d->m_type)) {
                    if (types.heap_source_of(actual) != nullptr) {
                        unsupported = "an array the host hands over, passed "
                            "to '" + std::string(callee->m_name) + "',";
                    }
                    continue;
                }
                if (ASR::Struct_t *source = types.heap_source_of(actual)) {
                    if (d->m_intent != ASR::intentType::In) {
                        unsupported = "of the type '" +
                            std::string(source->m_name) + "', stored into a "
                            "value the host handed over by '" +
                            std::string(callee->m_name) + "',";
                        continue;
                    }
                    actual = replacer.copied_in(actual, img, source);
                    continue;
                }
                if (d->m_intent == ASR::intentType::In ||
                        ASR::is_a<ASR::Var_t>(*actual)) {
                    continue;
                }
                const Location &loc = actual->base.loc;
                ASRUtils::ASRBuilder b(al, loc);
                ASR::expr_t *tmp = image_local(img, loc);
                if (d->m_intent != ASR::intentType::Out) {
                    replacer.before.push_back(b.Assignment(tmp,
                        duplicate_expr(al, actual)));
                }
                after.push_back(b.Assignment(actual, tmp));
                actual = var(ASR::down_cast<ASR::Var_t>(tmp)->m_v, loc);
            }
        }
        flush_before(out);
        out.push_back(al, s);
        for (ASR::stmt_t *a : after) out.push_back(al, a);
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
                case ASR::stmtType::Assignment:
                    assignment(s, out);
                    break;
                case ASR::stmtType::SubroutineCall:
                    subroutine_call(s, out);
                    break;
                // A check of the bounds of an assignment is not made on the
                // device, and the heap arrays it names have no bounds there.
                case ASR::stmtType::DebugCheckArrayBounds:
                    break;
                default:
                    statement(s, out);
                    break;
            }
        }
        body = out.p;
        n_body = out.n;
    }
};


// The `s%v => storage` statements of gpu_device_struct_storage in a body,
// at any depth.
class StorageAssociations :
        public ASRUtils::BlockBodyWalkVisitor<StorageAssociations> {
public:
    std::vector<std::pair<ASR::StructInstanceMember_t*, ASR::symbol_t*>>
        found;

    void visit_Associate(const ASR::Associate_t &x) {
        ASR::StructInstanceMember_t *component = nullptr;
        ASR::expr_t *storage = nullptr;
        if (gpu_struct_component_storage(const_cast<ASR::stmt_t*>(
                &x.base), component, storage)) {
            found.push_back({component, ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(storage)->m_v)});
        }
    }
};

// Puts the component back where the storage of a heap type's value was.
class StorageUndo : public ASR::BaseExprReplacer<StorageUndo> {
public:
    Allocator &al;
    std::map<ASR::symbol_t*, ASR::StructInstanceMember_t*> &storage;
    StorageUndo(Allocator &al_,
            std::map<ASR::symbol_t*, ASR::StructInstanceMember_t*> &s)
        : al(al_), storage(s) {}

    void replace_Var(ASR::Var_t *x) {
        auto it = storage.find(ASRUtils::symbol_get_past_external(x->m_v));
        if (it == storage.end()) return;
        *current_expr = duplicate_expr(al, &it->second->base);
    }
};

class StorageUndoVisitor :
        public ASR::CallReplacerOnExpressionsVisitor<StorageUndoVisitor> {
public:
    StorageUndo replacer;
    StorageUndoVisitor(Allocator &al,
            std::map<ASR::symbol_t*, ASR::StructInstanceMember_t*> &s)
        : replacer(al, s) {}

    void call_replacer() {
        replacer.current_expr = current_expr;
        replacer.replace_expr(*current_expr);
    }
};

// Removes the statements of `drop` from a body, at any depth.
void remove_statements(Allocator &al, ASR::stmt_t **&body, size_t &n_body,
        const std::set<ASR::stmt_t*> &drop) {
    Vec<ASR::stmt_t*> out;
    out.reserve(al, n_body);
    for (size_t i = 0; i < n_body; i++) {
        ASR::stmt_t *s = body[i];
        if (drop.count(s)) continue;
        switch (s->type) {
            case ASR::stmtType::If: {
                ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                remove_statements(al, x->m_body, x->n_body, drop);
                remove_statements(al, x->m_orelse, x->n_orelse, drop);
                break;
            }
            case ASR::stmtType::DoLoop: {
                ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                remove_statements(al, x->m_body, x->n_body, drop);
                break;
            }
            case ASR::stmtType::WhileLoop: {
                ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                remove_statements(al, x->m_body, x->n_body, drop);
                break;
            }
            case ASR::stmtType::BlockCall: {
                ASR::symbol_t *m = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::BlockCall_t>(s)->m_m);
                if (ASR::is_a<ASR::Block_t>(*m)) {
                    ASR::Block_t *x = ASR::down_cast<ASR::Block_t>(m);
                    remove_statements(al, x->m_body, x->n_body, drop);
                }
                break;
            }
            case ASR::stmtType::AssociateBlockCall: {
                ASR::symbol_t *m = ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::AssociateBlockCall_t>(s)->m_m);
                if (ASR::is_a<ASR::AssociateBlock_t>(*m)) {
                    ASR::AssociateBlock_t *x =
                        ASR::down_cast<ASR::AssociateBlock_t>(m);
                    remove_statements(al, x->m_body, x->n_body, drop);
                }
                break;
            }
            default:
                break;
        }
        out.push_back(al, s);
    }
    body = out.p;
    n_body = out.n;
}

class HeapPass {
public:
    HeapPass(Allocator &al_, ASR::TranslationUnit_t &unit_,
            const PassOptions &options_)
        : al(al_), unit(unit_), heap(al_, unit_, options_),
          types(al_, unit_), routines(al_, unit_, heap, types) {}

    void run() {
        LaunchLocations locations;
        locations.visit_TranslationUnit(unit);
        heap.launches = locations.launches;
        collect_device_code();
        heap.kernel_of = kernel_of;
        decide_heap_types();
        if (!types.heap.empty() && !build_images()) return;
        if (!types.heap.empty()) {
            for (ASR::Function_t *fn : device_code()) undo_storage(*fn);
            for (ASR::Function_t *fn : device_code()) retype(*fn);
        }
        for (ASR::Function_t *kernel : kernels) choose_kernel_arrays(*kernel);
        for (ASR::Function_t *fn : routines_) choose_routine_arrays(*fn);
        for (ASR::Function_t *fn : device_code()) {
            if (heap.declined.count(kernel_of[fn])) continue;
            lower(*fn);
        }
        if (!routines_made()) return;
        heap.finalize(kernels, routines_);
    }

private:
    Allocator &al;
    ASR::TranslationUnit_t &unit;
    DeviceHeap heap;
    HeapTypes types;
    HeapRoutines routines;
    std::vector<ASR::Function_t*> kernels;
    std::vector<ASR::Function_t*> routines_;
    std::map<ASR::Function_t*, ASR::Function_t*> kernel_of;
    // The heap arrays of every procedure, which are locals of it.
    std::map<ASR::Function_t*, std::vector<ASR::Variable_t*>> heap_arrays;
    std::map<ASR::symbol_t*, Descriptor> locals;
    // A derived type of each heap type's name.
    std::map<std::string, ASR::Struct_t*> structs;
    // The procedures each kernel reaches, for reporting a heap type the
    // heap cannot hold at the loops that use it.
    std::map<std::string, std::set<ASR::Function_t*>> type_kernels;

    std::vector<ASR::Function_t*> device_code() {
        std::vector<ASR::Function_t*> all = kernels;
        all.insert(all.end(), routines_.begin(), routines_.end());
        return all;
    }

    bool routines_made() {
        for (auto &made : routines.made) {
            routines_.push_back(made.first);
            kernel_of[made.first] = made.second;
        }
        heap.kernel_of = kernel_of;
        if (!routines.unsupported.empty()) {
            for (ASR::Function_t *k : kernels) {
                heap.decline(k, k->base.base.loc, GpuDecline(
                    GpuDeclineReason::HeapArrayUse, routines.unsupported));
            }
            return false;
        }
        return true;
    }

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
                    routines_.push_back(callee);
                    work.push_back(callee);
                }
            }
        }
    }

    bool is_dummy(ASR::Function_t &fn, ASR::symbol_t *s) {
        for (size_t i = 0; i < fn.n_args; i++) {
            if (variable_of(fn.m_args[i]) == s) return true;
        }
        return fn.m_return_var != nullptr && variable_of(fn.m_return_var) == s;
    }

    bool is_kernel(ASR::Function_t &fn) {
        return ASRUtils::get_FunctionType(fn)->m_exec_space ==
            ASR::exec_spaceType::Kernel;
    }

    // The arrays of a kernel the host cannot size before the launch.
    std::set<ASR::symbol_t*> unsizable(ASR::Function_t &kernel) {
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
        return chosen;
    }

    // A derived type is a heap type when device code creates a value of it
    // that host-sized storage cannot hold: an array of such values, a
    // value of a type holding one in a component, or a local whose
    // component storage is missing or cannot be sized on the host. A type
    // holding a heap type in a component is one too.
    void decide_heap_types() {
        std::map<ASR::Function_t*, std::set<ASR::symbol_t*>> kernel_unsized;
        for (ASR::Function_t *k : kernels) kernel_unsized[k] = unsizable(*k);
        std::set<std::string> created;
        for (ASR::Function_t *fn : device_code()) {
            StorageAssociations storage;
            for (size_t i = 0; i < fn->n_body; i++) {
                storage.visit_stmt(*fn->m_body[i]);
            }
            std::map<ASR::symbol_t*, std::map<std::string, ASR::symbol_t*>>
                stored;
            for (auto &a : storage.found) {
                stored[variable_of(a.first->m_v)][ASRUtils::symbol_name(
                    ASRUtils::symbol_get_past_external(a.first->m_m))] =
                    a.second;
            }
            std::vector<SymbolTable*> scopes;
            gpu_routine_scopes(fn->m_symtab, scopes);
            for (SymbolTable *scope : scopes) {
                for (auto &item : scope->get_scope()) {
                    if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                    ASR::Variable_t *v =
                        ASR::down_cast<ASR::Variable_t>(item.second);
                    ASR::Struct_t *st = struct_of(v->m_type,
                        v->m_type_declaration);
                    if (!types.holds_allocatable(st)) continue;
                    if (is_kernel(*fn) && is_dummy(*fn, item.second)) {
                        continue;
                    }
                    if (ASRUtils::is_pointer(v->m_type)) continue;
                    structs.emplace(st->m_name, st);
                    created.insert(st->m_name);
                    type_kernels[st->m_name].insert(kernel_of[fn]);
                    if (ASRUtils::is_array(v->m_type)) {
                        types.heap.insert(st->m_name);
                        continue;
                    }
                    if (is_dummy(*fn, item.second)) continue;
                    std::vector<ASR::symbol_t*> members;
                    gpu_collect_data_members(st, members);
                    for (ASR::symbol_t *m : members) {
                        if (m == nullptr || !ASR::is_a<ASR::Variable_t>(*m)) {
                            continue;
                        }
                        ASR::Variable_t *mv = ASR::down_cast<ASR::Variable_t>(m);
                        if (ASRUtils::is_pointer(mv->m_type)) continue;
                        bool array = ASRUtils::is_allocatable(mv->m_type) &&
                            ASRUtils::is_array(mv->m_type);
                        if (!array) {
                            if (types.holds_allocatable(struct_of(mv->m_type,
                                    mv->m_type_declaration))) {
                                types.heap.insert(st->m_name);
                            }
                            continue;
                        }
                        if (struct_of(mv->m_type, mv->m_type_declaration)) {
                            types.heap.insert(st->m_name);
                            continue;
                        }
                        auto s = stored.find(item.second);
                        if (s == stored.end() ||
                                s->second.count(mv->m_name) == 0) {
                            types.heap.insert(st->m_name);
                            continue;
                        }
                        if (is_kernel(*fn) && kernel_unsized[fn].count(
                                s->second[mv->m_name])) {
                            types.heap.insert(st->m_name);
                        }
                    }
                }
            }
        }
        // A type holding a heap type in a component is one, and so is
        // every type a heap type holds.
        for (bool changed = true; changed;) {
            changed = false;
            for (const std::string &name : created) {
                if (types.heap.count(name)) continue;
                std::vector<ASR::symbol_t*> members;
                gpu_collect_data_members(structs[name], members);
                for (ASR::symbol_t *m : members) {
                    if (m == nullptr || !ASR::is_a<ASR::Variable_t>(*m)) {
                        continue;
                    }
                    ASR::Variable_t *mv = ASR::down_cast<ASR::Variable_t>(m);
                    ASR::Struct_t *ms = struct_of(mv->m_type,
                        mv->m_type_declaration);
                    if (types.is_heap(ms)) {
                        types.heap.insert(name);
                        changed = true;
                        break;
                    }
                }
            }
        }
    }

    bool build_images() {
        std::vector<std::string> names(types.heap.begin(), types.heap.end());
        for (const std::string &name : names) {
            auto st = structs.find(name);
            if (st == structs.end()) continue;
            auto arena = [&](ASR::ttype_t *t, ASR::symbol_t *decl) {
                return heap.arena(t, decl);
            };
            if (types.image(st->second, arena) == nullptr) {
                for (ASR::Function_t *k : type_kernels[name]) {
                    heap.decline(k, k->base.base.loc, GpuDecline(
                        GpuDeclineReason::HeapArrayUse, "holding a value " +
                        types.unsupported));
                }
                return false;
            }
        }
        return true;
    }

    // Drops the storage gpu_device_struct_storage gave a value of a heap
    // type: the value holds its components on the heap instead.
    void undo_storage(ASR::Function_t &fn) {
        StorageAssociations storage;
        for (size_t i = 0; i < fn.n_body; i++) {
            storage.visit_stmt(*fn.m_body[i]);
        }
        std::map<ASR::symbol_t*, ASR::StructInstanceMember_t*> undone;
        std::set<ASR::stmt_t*> drop;
        for (auto &a : storage.found) {
            ASR::symbol_t *s = variable_of(a.first->m_v);
            if (s == nullptr || !ASR::is_a<ASR::Variable_t>(*s)) continue;
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(s);
            if (!types.is_heap(struct_of(v->m_type, v->m_type_declaration))) {
                continue;
            }
            undone[a.second] = a.first;
        }
        if (undone.empty()) return;
        DropAssociations finder(undone, drop);
        for (size_t i = 0; i < fn.n_body; i++) {
            finder.visit_stmt(*fn.m_body[i]);
        }
        remove_statements(al, fn.m_body, fn.n_body, drop);
        StorageUndoVisitor visitor(al, undone);
        visitor.visit_Function(fn);
        for (auto &u : undone) {
            ASR::Variable_t *v = ASR::down_cast<ASR::Variable_t>(u.first);
            v->m_parent_symtab->erase_symbol(v->m_name);
        }
    }

    class DropAssociations :
            public ASRUtils::BlockBodyWalkVisitor<DropAssociations> {
    public:
        std::map<ASR::symbol_t*, ASR::StructInstanceMember_t*> &undone;
        std::set<ASR::stmt_t*> &drop;
        DropAssociations(
                std::map<ASR::symbol_t*, ASR::StructInstanceMember_t*> &u,
                std::set<ASR::stmt_t*> &d) : undone(u), drop(d) {}
        void visit_Associate(const ASR::Associate_t &x) {
            ASR::symbol_t *s = variable_of(x.m_value);
            if (s != nullptr && undone.count(s)) {
                drop.insert(const_cast<ASR::stmt_t*>(&x.base));
            }
        }
    };

    // Gives every value of a heap type that device code creates the image
    // type, and makes every allocatable array of such values a heap array.
    void retype(ASR::Function_t &fn) {
        bool signature = false;
        std::vector<SymbolTable*> scopes;
        gpu_routine_scopes(fn.m_symtab, scopes);
        std::vector<ASR::Variable_t*> pointers;
        for (SymbolTable *scope : scopes) {
            for (auto &item : scope->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *v =
                    ASR::down_cast<ASR::Variable_t>(item.second);
                ASR::Struct_t *st = struct_of(v->m_type,
                    v->m_type_declaration);
                if (!types.is_heap(st)) continue;
                if (is_kernel(fn) && is_dummy(fn, item.second)) continue;
                if (ASRUtils::is_pointer(v->m_type)) {
                    pointers.push_back(v);
                    continue;
                }
                Image *img = types.images.count(st->m_name)
                    ? &types.images[st->m_name] : nullptr;
                if (img == nullptr) continue;
                v->m_type = with_struct(al, v->m_type, types.image_type(img));
                v->m_type_declaration = img->symbol;
                if (is_dummy(fn, item.second)) signature = true;
            }
        }
        // A pointer bound to an image is one to images.
        ImageAssociations bound(types);
        for (size_t i = 0; i < fn.n_body; i++) bound.visit_stmt(*fn.m_body[i]);
        for (ASR::Variable_t *v : pointers) {
            if (bound.targets.count(&v->base) == 0) continue;
            Image *img = bound.targets[&v->base];
            v->m_type = with_struct(al, v->m_type, types.image_type(img));
            v->m_type_declaration = img->symbol;
        }
        if (signature) {
            if (!is_kernel(fn) && ASRUtils::get_FunctionType(fn)->m_exec_space
                    != ASR::exec_spaceType::Device) {
                heap.decline(kernel_of[&fn], fn.base.base.loc, GpuDecline(
                    GpuDeclineReason::HeapArrayUse, "holding a value of a "
                    "heap type, a dummy argument of '" +
                    std::string(fn.m_name) + "', which the host calls too,"));
                return;
            }
            fn.m_function_signature = ASRUtils::TYPE(
                ASRUtils::make_FunctionType_t_util(al, fn.base.base.loc,
                    fn.m_args, fn.n_args, fn.m_return_var,
                    ASRUtils::get_FunctionType(fn), fn.m_symtab));
        }
    }

    class ImageAssociations :
            public ASRUtils::BlockBodyWalkVisitor<ImageAssociations> {
    public:
        HeapTypes &types;
        std::map<ASR::symbol_t*, Image*> targets;
        explicit ImageAssociations(HeapTypes &t) : types(t) {}
        void visit_Associate(const ASR::Associate_t &x) {
            ASR::symbol_t *s = variable_of(x.m_target);
            if (s == nullptr) return;
            ASR::expr_t *value = x.m_value;
            while (ASR::is_a<ASR::ArraySection_t>(*value)) {
                value = ASR::down_cast<ASR::ArraySection_t>(value)->m_v;
            }
            if (Image *img = types.image_of(value)) targets[s] = img;
        }
    };

    void choose(ASR::Function_t &fn, ASR::Variable_t *v) {
        heap_arrays[&fn].push_back(v);
    }

    // An allocatable array of the kernel whose extents the host cannot work
    // out before the launch, and every allocatable array of images.
    void choose_kernel_arrays(ASR::Function_t &kernel) {
        std::set<ASR::symbol_t*> chosen = unsizable(kernel);
        std::vector<SymbolTable*> scopes;
        gpu_routine_scopes(kernel.m_symtab, scopes);
        for (SymbolTable *scope : scopes) {
            for (auto &item : scope->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *v =
                    ASR::down_cast<ASR::Variable_t>(item.second);
                if (!runtime_sized_array(*v) ||
                        !ASRUtils::is_allocatable(v->m_type)) {
                    continue;
                }
                if (types.by_symbol.count(v->m_type_declaration) ||
                        chosen.count(item.second)) {
                    choose(kernel, v);
                }
            }
        }
    }

    // An allocatable local of a routine allocated with a shape read from a
    // value it computes, which no caller can give it, and every
    // allocatable array of images.
    void choose_routine_arrays(ASR::Function_t &fn) {
        std::vector<SymbolTable*> scopes;
        gpu_routine_scopes(fn.m_symtab, scopes);
        ComputedAllocations shapes(fn);
        for (size_t i = 0; i < fn.n_body; i++) shapes.visit_stmt(*fn.m_body[i]);
        for (SymbolTable *scope : scopes) {
            for (auto &item : scope->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *v =
                    ASR::down_cast<ASR::Variable_t>(item.second);
                if (!runtime_sized_array(*v) ||
                        !ASRUtils::is_allocatable(v->m_type)) {
                    continue;
                }
                if (types.by_symbol.count(v->m_type_declaration) ||
                        shapes.computed.count(item.second)) {
                    choose(fn, v);
                }
            }
        }
    }

    // Lowers the heap arrays and heap values of `fn`.
    void lower(ASR::Function_t &fn) {
        ASR::Function_t *kernel = kernel_of[&fn];
        std::map<SymbolTable*, std::vector<ASR::stmt_t*>> entry;
        std::vector<ASR::Variable_t*> &vars = heap_arrays[&fn];
        for (ASR::Variable_t *v : vars) {
            const Location &loc = v->base.base.loc;
            ASRUtils::ASRBuilder b(al, loc);
            Image *img = types.by_symbol.count(v->m_type_declaration)
                ? types.by_symbol[v->m_type_declaration] : nullptr;
            if (img == nullptr && !arena_element(*v)) {
                heap.decline(kernel, loc, GpuDecline(
                    GpuDeclineReason::HeapArrayUse, "'" +
                    std::string(v->m_name) + "', of a type no arena holds,"));
                return;
            }
            Descriptor h;
            h.name = v->m_name;
            h.element_image = img;
            h.arena = img ? heap.arena(types.image_type(img), img->symbol)
                : heap.arena(ASRUtils::extract_type(v->m_type), nullptr);
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
            locals[&v->base] = h;
            // An allocatable array starts unallocated.
            entry[scope].push_back(b.Assignment(b.Var(h.base), b.i32(-1)));
        }
        // An image starts with nothing allocated, and so does an
        // `intent(out)` dummy and a function result.
        std::vector<SymbolTable*> scopes;
        gpu_routine_scopes(fn.m_symtab, scopes);
        std::string unsupported;
        for (SymbolTable *scope : scopes) {
            for (auto &item : scope->get_scope()) {
                if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
                ASR::Variable_t *v =
                    ASR::down_cast<ASR::Variable_t>(item.second);
                if (types.by_symbol.count(v->m_type_declaration) == 0) {
                    continue;
                }
                if (ASRUtils::is_allocatable_or_pointer(v->m_type)) continue;
                bool result = fn.m_return_var != nullptr &&
                    variable_of(fn.m_return_var) == item.second;
                if (v->m_intent != ASR::intentType::Local &&
                        v->m_intent != ASR::intentType::Out && !result) {
                    continue;
                }
                const Location &loc = v->base.base.loc;
                ASRUtils::ASRBuilder b(al, loc);
                Image *img = types.by_symbol[v->m_type_declaration];
                if (!ASRUtils::is_array(v->m_type)) {
                    initialize_image(al, loc, img, b.Var(&v->base),
                        entry[scope], unsupported);
                    continue;
                }
                if (ASRUtils::extract_n_dims_from_ttype(v->m_type) != 1) {
                    unsupported = "'" + std::string(v->m_name) + "', an "
                        "array of rank more than one of a heap type,";
                    continue;
                }
                ASR::expr_t *k = b.Variable(fn.m_symtab,
                    fn.m_symtab->get_unique_name("__gpu_heap_k", false),
                    int_type(al, loc), ASR::intentType::Local);
                std::vector<ASR::stmt_t*> each;
                initialize_image(al, loc, img, b.ArrayItem_01(b.Var(&v->base),
                    {k}), each, unsupported);
                entry[scope].push_back(b.DoLoop(k, b.ArrayLBound(
                    b.Var(&v->base), 1), b.ArrayUBound(b.Var(&v->base), 1),
                    each));
            }
        }
        if (!unsupported.empty()) {
            heap.decline(kernel, fn.base.base.loc, GpuDecline(
                GpuDeclineReason::HeapArrayUse, unsupported));
            return;
        }
        HeapLowering lowering(al, heap, types, routines, fn, kernel, locals);
        for (auto &e : entry) prepend(fn, e.first, e.second);
        lowering.run();
        if (!lowering.unsupported.empty()) {
            heap.decline(kernel, fn.base.base.loc, GpuDecline(
                GpuDeclineReason::HeapArrayUse, lowering.unsupported));
            return;
        }
        for (ASR::Variable_t *v : vars) {
            v->m_parent_symtab->erase_symbol(v->m_name);
        }
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
            if (is_kernel(fn)) at = kernel_prelude_length(fn);
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

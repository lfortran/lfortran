#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_builder.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/gpu_memory_space.h>
#include <libasr/pass/pass_utils.h>

#include <map>
#include <set>
#include <string>
#include <vector>

namespace LCompilers {

/*
Give every array of device code the memory space its storage lives in, and
clone a device routine once per memory-space signature its callers ask for.

A GPU address space is a property of where an array lives, so it belongs in
the array's type rather than in a code generator's walking state:

  - a kernel argument is backed by a buffer the host allocated, so it is
    Global;
  - an array the device allocates for itself, whether a temporary of the
    kernel or a local of a device routine, lives in the thread's own memory,
    so it is Thread;
  - an array a pointer is associated with lends the pointer its space;
  - Shared and Constant are representable but nothing produces them yet.

A device routine is then written once per combination of spaces its arguments
have. A routine called with a kernel argument and a thread temporary at the
same time needs a signature that mixes the two, which no single definition
can provide; a clone per combination gives every call site an exact match.
The code generators emit what the type says, with no overloading of their
own, which also keeps this dialect neutral: CUDA does not qualify a pointer
by address space, so the clones collapse to identical bodies there and
`unused_functions` removes the ones nothing calls.
*/

namespace {

// Every routine a body calls, with the call node itself so the caller can be
// retargeted at a clone.
class GpuCallCollector :
        public ASRUtils::BlockBodyWalkVisitor<GpuCallCollector> {
public:
    std::vector<ASR::FunctionCall_t*> function_calls;
    std::vector<ASR::SubroutineCall_t*> subroutine_calls;

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        function_calls.push_back(const_cast<ASR::FunctionCall_t*>(&x));
        ASR::BaseWalkVisitor<GpuCallCollector>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        subroutine_calls.push_back(const_cast<ASR::SubroutineCall_t*>(&x));
        ASR::BaseWalkVisitor<GpuCallCollector>::visit_SubroutineCall(x);
    }
};

// Every Associate of a body, so a pointer can take the space of what it
// points at.
class GpuAssociateCollector
        : public ASRUtils::BlockBodyWalkVisitor<GpuAssociateCollector> {
public:
    std::vector<ASR::Associate_t*> associates;

    void visit_Associate(const ASR::Associate_t &x) {
        associates.push_back(const_cast<ASR::Associate_t*>(&x));
    }
};

char memory_space_tag(ASR::memory_spaceType space) {
    switch (space) {
        case ASR::memory_spaceType::Global: return 'g';
        case ASR::memory_spaceType::Shared: return 's';
        case ASR::memory_spaceType::Constant: return 'c';
        case ASR::memory_spaceType::Thread: return 't';
    }
    return 'g';
}

class GpuMemorySpaceAssigner {
public:
    explicit GpuMemorySpaceAssigner(Allocator &al_) : al(al_) {}

    void materialize_struct_arguments(ASR::TranslationUnit_t &unit) {
        only_struct_arguments = true;
        for (auto &item : unit.m_symtab->get_scope()) {
            if (!ASRUtils::is_device_kernel(item.second)) continue;
            copy_reference_arguments(
                *ASR::down_cast<ASR::Function_t>(item.second), true);
        }
    }

    void assign(ASR::TranslationUnit_t &unit) {
        std::vector<ASR::Function_t*> kernels;
        for (auto &item : unit.m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
            ASR::Function_t *fn =
                ASR::down_cast<ASR::Function_t>(item.second);
            if (!ASRUtils::is_device_kernel(item.second)) {
                continue;
            }
            kernels.push_back(fn);
        }
        for (ASR::Function_t *kernel : kernels) {
            process_function(kernel, true);
        }
    }

private:
    Allocator &al;
    bool only_struct_arguments = false;
    std::set<ASR::Function_t*> processed;
    // (original routine, memory-space signature) -> clone
    std::map<std::pair<ASR::symbol_t*, std::vector<ASR::memory_spaceType>>,
        ASR::symbol_t*> clones;

    static ASR::memory_spaceType type_memory_space(ASR::ttype_t *type) {
        ASR::ttype_t *base =
            ASRUtils::type_get_past_allocatable_pointer(type);
        if (ASR::is_a<ASR::Array_t>(*base)) {
            return ASR::down_cast<ASR::Array_t>(base)->m_memory_space;
        }
        return ASR::memory_spaceType::Global;
    }

    ASR::ttype_t* with_memory_space(ASR::ttype_t *type,
            ASR::memory_spaceType space) {
        if (ASR::is_a<ASR::Allocatable_t>(*type)) {
            ASR::Allocatable_t *alloc =
                ASR::down_cast<ASR::Allocatable_t>(type);
            ASR::ttype_t *inner = with_memory_space(alloc->m_type, space);
            if (inner == alloc->m_type) return type;
            return ASRUtils::TYPE(ASR::make_Allocatable_t(al,
                type->base.loc, inner));
        }
        if (ASR::is_a<ASR::Pointer_t>(*type)) {
            ASR::Pointer_t *ptr = ASR::down_cast<ASR::Pointer_t>(type);
            ASR::ttype_t *inner = with_memory_space(ptr->m_type, space);
            if (inner == ptr->m_type) return type;
            return ASRUtils::TYPE(ASR::make_Pointer_t(al,
                type->base.loc, inner));
        }
        if (ASR::is_a<ASR::Array_t>(*type)) {
            ASR::Array_t *arr = ASR::down_cast<ASR::Array_t>(type);
            if (arr->m_memory_space == space) return type;
            return ASRUtils::TYPE(ASR::make_Array_t(al, type->base.loc,
                arr->m_type, arr->m_dims, arr->n_dims,
                arr->m_physical_type, space));
        }
        return type;
    }

    // The variable an array expression ultimately reads from, so that a
    // section or a cast does not hide where the storage lives.
    static ASR::expr_t* base_of(ASR::expr_t *expr) {
        while (expr) {
            if (ASR::is_a<ASR::ArrayPhysicalCast_t>(*expr)) {
                expr = ASR::down_cast<ASR::ArrayPhysicalCast_t>(expr)->m_arg;
            } else if (ASR::is_a<ASR::ArraySection_t>(*expr)) {
                expr = ASR::down_cast<ASR::ArraySection_t>(expr)->m_v;
            } else if (ASR::is_a<ASR::ArrayItem_t>(*expr)) {
                expr = ASR::down_cast<ASR::ArrayItem_t>(expr)->m_v;
            } else if (ASR::is_a<ASR::ArrayReshape_t>(*expr)) {
                expr = ASR::down_cast<ASR::ArrayReshape_t>(expr)->m_array;
            } else if (ASR::is_a<ASR::ArrayBroadcast_t>(*expr)) {
                expr = ASR::down_cast<ASR::ArrayBroadcast_t>(expr)->m_array;
            } else if (ASR::is_a<ASR::Cast_t>(*expr)) {
                expr = ASR::down_cast<ASR::Cast_t>(expr)->m_arg;
            } else if (ASR::is_a<ASR::GetPointer_t>(*expr)) {
                expr = ASR::down_cast<ASR::GetPointer_t>(expr)->m_arg;
            } else if (ASR::is_a<ASR::StructInstanceMember_t>(*expr)) {
                expr = ASR::down_cast<ASR::StructInstanceMember_t>(expr)->m_v;
            } else {
                break;
            }
        }
        return expr;
    }

    static ASR::memory_spaceType expr_memory_space(ASR::expr_t *expr) {
        ASR::expr_t *base = base_of(expr);
        if (base && ASR::is_a<ASR::Var_t>(*base)) {
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(base)->m_v);
            if (sym && ASR::is_a<ASR::Variable_t>(*sym)) {
                return type_memory_space(
                    ASR::down_cast<ASR::Variable_t>(sym)->m_type);
            }
        }
        return ASR::memory_spaceType::Global;
    }

    void set_variable_space(ASR::Variable_t *var,
            ASR::memory_spaceType space) {
        var->m_type = with_memory_space(var->m_type, space);
    }

    // A local array of device code lives in the thread's own memory, unless
    // the host had to allocate a workspace buffer for it because the device
    // cannot size it on entry.
    void assign_scope(SymbolTable *scope,
            const std::set<ASR::symbol_t*> &device_backed) {
        for (auto &item : scope->get_scope()) {
            if (ASR::is_a<ASR::Block_t>(*item.second)) {
                assign_scope(ASR::down_cast<ASR::Block_t>(
                    item.second)->m_symtab, device_backed);
                continue;
            }
            if (ASR::is_a<ASR::AssociateBlock_t>(*item.second)) {
                assign_scope(ASR::down_cast<ASR::AssociateBlock_t>(
                    item.second)->m_symtab, device_backed);
                continue;
            }
            if (!ASR::is_a<ASR::Variable_t>(*item.second)) continue;
            ASR::Variable_t *var =
                ASR::down_cast<ASR::Variable_t>(item.second);
            if (var->m_intent != ASR::intentType::Local) continue;
            if (!ASRUtils::is_array(var->m_type)) continue;
            if (device_backed.count(item.second)) continue;
            if (ASR::is_a<ASR::Pointer_t>(
                    *ASRUtils::type_get_past_allocatable(var->m_type))) {
                // A pointer has the space of whatever it is associated
                // with, which the Associate statements below decide.
                continue;
            }
            set_variable_space(var, ASR::memory_spaceType::Thread);
        }
    }

    void assign_pointer_spaces(ASR::Function_t *fn) {
        GpuAssociateCollector collector;
        for (size_t i = 0; i < fn->n_body; i++) {
            collector.visit_stmt(*fn->m_body[i]);
        }
        for (ASR::Associate_t *assoc : collector.associates) {
            if (!ASR::is_a<ASR::Var_t>(*assoc->m_target)) continue;
            ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(assoc->m_target)->m_v);
            if (!sym || !ASR::is_a<ASR::Variable_t>(*sym)) continue;
            ASR::Variable_t *var = ASR::down_cast<ASR::Variable_t>(sym);
            if (!ASRUtils::is_array(var->m_type)) continue;
            set_variable_space(var, expr_memory_space(assoc->m_value));
        }
    }

    // A dummy argument a device routine defines (`intent(out)` or
    // `intent(inout)`), a scalar or a derived-type value, is passed by
    // reference, and a device routine is written for values in the memory
    // of the calling thread, as its locals are. An actual argument stored
    // elsewhere -- an element of a kernel argument, or of an array dummy
    // in device memory, a derived-type kernel argument -- is copied into a
    // local of the caller's own memory for the call, and copied back after
    // it, as Fortran allows for a dummy argument. The allocatable
    // components of a derived-type value are not copied: the copy hands the
    // routine the buffers of the original.
    void copy_reference_arguments(ASR::Function_t &fn, bool is_kernel) {
        std::set<ASR::symbol_t*> parameters;
        for (size_t i = 0; is_kernel && i < fn.n_args; i++) {
            if (ASR::is_a<ASR::Var_t>(*fn.m_args[i])) {
                parameters.insert(ASRUtils::symbol_get_past_external(
                    ASR::down_cast<ASR::Var_t>(fn.m_args[i])->m_v));
            }
        }
        std::set<ASR::symbol_t*> transformed;
        copy_reference_arguments(fn, parameters, transformed,
            fn.m_body, fn.n_body);
    }

    // Whether a scalar actual argument is stored in the memory of the
    // calling thread: a variable of the caller, or an element of an array
    // in thread memory. Anything else -- a kernel argument or an element of
    // one, an element of an array in device memory, a component -- is not
    // known to be.
    static bool in_thread_memory(ASR::expr_t *e,
            const std::set<ASR::symbol_t*> &parameters) {
        e = ASRUtils::get_past_array_physical_cast(e);
        ASR::expr_t *array = nullptr;
        if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
            array = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
            e = array;
        }
        if (!ASR::is_a<ASR::Var_t>(*e)) return false;
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(e)->m_v);
        if (parameters.count(sym)) return false;
        return array == nullptr || type_memory_space(
            ASRUtils::expr_type(array)) == ASR::memory_spaceType::Thread;
    }

    static bool is_intrinsic_scalar(ASR::ttype_t *type) {
        switch (type->type) {
            case ASR::ttypeType::Integer:
            case ASR::ttypeType::UnsignedInteger:
            case ASR::ttypeType::Real:
            case ASR::ttypeType::Complex:
            case ASR::ttypeType::Logical:
                return true;
            default:
                return false;
        }
    }

    // The kernel argument a derived-type actual argument is, or is an
    // element of, or nullptr.
    static ASR::Variable_t* device_struct(ASR::expr_t *e,
            const std::set<ASR::symbol_t*> &parameters) {
        e = ASRUtils::get_past_array_physical_cast(e);
        if (ASR::is_a<ASR::ArrayItem_t>(*e)) {
            e = ASR::down_cast<ASR::ArrayItem_t>(e)->m_v;
        }
        if (!ASR::is_a<ASR::Var_t>(*e)) return nullptr;
        ASR::symbol_t *sym = ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::Var_t>(e)->m_v);
        if (!parameters.count(sym) || !ASR::is_a<ASR::Variable_t>(*sym)) {
            return nullptr;
        }
        return ASR::down_cast<ASR::Variable_t>(sym);
    }

    void copy_reference_arguments(ASR::Function_t &kernel,
            const std::set<ASR::symbol_t*> &parameters,
            std::set<ASR::symbol_t*> &transformed,
            ASR::stmt_t **&body, size_t &n_body) {
        Vec<ASR::stmt_t*> out;
        out.reserve(al, n_body);
        for (size_t i = 0; i < n_body; i++) {
            ASR::stmt_t *s = body[i];
            switch (s->type) {
                case ASR::stmtType::If: {
                    ASR::If_t *x = ASR::down_cast<ASR::If_t>(s);
                    copy_reference_arguments(kernel, parameters, transformed,
                        x->m_body, x->n_body);
                    copy_reference_arguments(kernel, parameters, transformed,
                        x->m_orelse, x->n_orelse);
                    break;
                }
                case ASR::stmtType::DoLoop: {
                    ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(s);
                    copy_reference_arguments(kernel, parameters, transformed,
                        x->m_body, x->n_body);
                    break;
                }
                case ASR::stmtType::WhileLoop: {
                    ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(s);
                    copy_reference_arguments(kernel, parameters, transformed,
                        x->m_body, x->n_body);
                    break;
                }
                case ASR::stmtType::Select: {
                    ASR::Select_t *x = ASR::down_cast<ASR::Select_t>(s);
                    for (size_t c = 0; c < x->n_body; c++) {
                        if (ASR::is_a<ASR::CaseStmt_t>(*x->m_body[c])) {
                            ASR::CaseStmt_t *t =
                                ASR::down_cast<ASR::CaseStmt_t>(x->m_body[c]);
                            copy_reference_arguments(kernel, parameters,
                                transformed, t->m_body, t->n_body);
                        } else {
                            ASR::CaseStmt_Range_t *t =
                                ASR::down_cast<ASR::CaseStmt_Range_t>(
                                    x->m_body[c]);
                            copy_reference_arguments(kernel, parameters,
                                transformed, t->m_body, t->n_body);
                        }
                    }
                    copy_reference_arguments(kernel, parameters, transformed,
                        x->m_default, x->n_default);
                    break;
                }
                case ASR::stmtType::BlockCall:
                case ASR::stmtType::AssociateBlockCall: {
                    ASR::symbol_t *m = ASRUtils::symbol_get_past_external(
                        ASR::is_a<ASR::BlockCall_t>(*s)
                            ? ASR::down_cast<ASR::BlockCall_t>(s)->m_m
                            : ASR::down_cast<ASR::AssociateBlockCall_t>(s)
                                ->m_m);
                    if (!transformed.insert(m).second) break;
                    if (ASR::is_a<ASR::Block_t>(*m)) {
                        ASR::Block_t *x = ASR::down_cast<ASR::Block_t>(m);
                        copy_reference_arguments(kernel, parameters,
                            transformed, x->m_body, x->n_body);
                    } else if (ASR::is_a<ASR::AssociateBlock_t>(*m)) {
                        ASR::AssociateBlock_t *x =
                            ASR::down_cast<ASR::AssociateBlock_t>(m);
                        copy_reference_arguments(kernel, parameters,
                            transformed, x->m_body, x->n_body);
                    }
                    break;
                }
                case ASR::stmtType::SubroutineCall: {
                    ASR::SubroutineCall_t *x =
                        ASR::down_cast<ASR::SubroutineCall_t>(s);
                    std::vector<ASR::stmt_t*> after;
                    copy_call_arguments(kernel, parameters, *x, out, after);
                    out.push_back(al, s);
                    for (ASR::stmt_t *t : after) out.push_back(al, t);
                    continue;
                }
                default:
                    break;
            }
            out.push_back(al, s);
        }
        body = out.p;
        n_body = out.n;
    }

    void copy_call_arguments(ASR::Function_t &kernel,
            const std::set<ASR::symbol_t*> &parameters,
            ASR::SubroutineCall_t &call, Vec<ASR::stmt_t*> &before,
            std::vector<ASR::stmt_t*> &after) {
        ASR::symbol_t *target = ASRUtils::symbol_get_past_external(
            call.m_name);
        if (target && ASR::is_a<ASR::StructMethodDeclaration_t>(*target)) {
            target = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::StructMethodDeclaration_t>(
                    target)->m_proc);
        }
        if (!target || !ASR::is_a<ASR::Function_t>(*target)) return;
        ASR::Function_t *callee = ASR::down_cast<ASR::Function_t>(target);
        if (callee->n_args != call.n_args) return;
        for (size_t i = 0; i < call.n_args; i++) {
            ASR::expr_t *actual = call.m_args[i].m_value;
            if (actual == nullptr ||
                    !ASR::is_a<ASR::Var_t>(*callee->m_args[i])) {
                continue;
            }
            ASR::symbol_t *dummy_sym = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::Var_t>(callee->m_args[i])->m_v);
            if (!ASR::is_a<ASR::Variable_t>(*dummy_sym)) continue;
            ASR::Variable_t *dummy =
                ASR::down_cast<ASR::Variable_t>(dummy_sym);
            if (dummy->m_intent != ASR::intentType::Out &&
                    dummy->m_intent != ASR::intentType::InOut) {
                continue;
            }
            ASR::ttype_t *actual_type = ASRUtils::expr_type(actual);
            std::string copy_name;
            ASR::symbol_t *type_declaration = nullptr;
            if (ASR::is_a<ASR::StructType_t>(*dummy->m_type)) {
                ASR::Variable_t *argument = device_struct(actual,
                    parameters);
                if (argument == nullptr ||
                        !ASR::is_a<ASR::StructType_t>(*actual_type)) {
                    continue;
                }
                copy_name = "__gpu_struct_argument";
                type_declaration = argument->m_type_declaration;
            } else if (!only_struct_arguments &&
                    is_intrinsic_scalar(dummy->m_type) &&
                    is_intrinsic_scalar(actual_type)) {
                if (in_thread_memory(actual, parameters)) continue;
                copy_name = "__gpu_scalar_argument";
            } else {
                continue;
            }
            const Location &loc = actual->base.loc;
            ASRUtils::ASRBuilder b(al, loc);
            ASR::expr_t *copy = b.Variable(kernel.m_symtab,
                kernel.m_symtab->get_unique_name(copy_name, false),
                ASRUtils::duplicate_type(al, actual_type),
                ASR::intentType::Local, type_declaration);
            ASRUtils::ExprStmtDuplicator duplicator(al);
            before.push_back(al, b.Assignment(copy,
                duplicator.duplicate_expr(actual)));
            after.push_back(b.Assignment(duplicator.duplicate_expr(actual),
                copy));
            call.m_args[i].m_value = copy;
        }
    }

    void process_function(ASR::Function_t *fn, bool is_kernel) {
        if (processed.count(fn)) return;
        processed.insert(fn);

        std::set<ASR::symbol_t*> device_backed;
        if (is_kernel) {
            for (auto &workspace : collect_gpu_vla_workspaces(*fn, 0, true)) {
                device_backed.insert(workspace.var);
            }
        }
        assign_scope(fn->m_symtab, device_backed);
        assign_pointer_spaces(fn);
        copy_reference_arguments(*fn, is_kernel);

        GpuCallCollector collector;
        for (size_t i = 0; i < fn->n_body; i++) {
            collector.visit_stmt(*fn->m_body[i]);
        }
        // A clone is not a procedure of the type a type-bound call names,
        // so a call retargeted at one is an ordinary call. The routine was
        // resolved already and the passed object is among the arguments.
        for (ASR::FunctionCall_t *call : collector.function_calls) {
            ASR::symbol_t *target = specialise(call->m_name, call->m_args,
                call->n_args);
            if (target) {
                call->m_name = target;
                call->m_dt = nullptr;
            }
        }
        for (ASR::SubroutineCall_t *call : collector.subroutine_calls) {
            ASR::symbol_t *target = specialise(call->m_name, call->m_args,
                call->n_args);
            if (target) {
                call->m_name = target;
                call->m_dt = nullptr;
            }
        }
    }

    // The routine this call should reach, cloning the callee when the call
    // mixes memory spaces the callee was not written for. Returns nullptr
    // when the call already reaches the right routine.
    ASR::symbol_t* specialise(ASR::symbol_t *called,
            ASR::call_arg_t *args, size_t n_args) {
        ASR::symbol_t *target = ASRUtils::symbol_get_past_external(called);
        if (target && ASR::is_a<ASR::StructMethodDeclaration_t>(*target)) {
            target = ASRUtils::symbol_get_past_external(
                ASR::down_cast<ASR::StructMethodDeclaration_t>(
                    target)->m_proc);
        }
        if (!target || !ASR::is_a<ASR::Function_t>(*target)) return nullptr;
        ASR::Function_t *callee = ASR::down_cast<ASR::Function_t>(target);
        ASR::FunctionType_t *ftype = ASRUtils::get_FunctionType(callee);
        if (ftype->m_deftype != ASR::deftypeType::Implementation &&
                ftype->m_abi != ASR::abiType::BindC &&
                ftype->m_abi != ASR::abiType::Intrinsic) {
            return nullptr;
        }
        if (callee->n_args != n_args) return nullptr;

        std::vector<ASR::memory_spaceType> spaces(n_args,
            ASR::memory_spaceType::Global);
        std::string signature;
        bool needs_clone = ftype->m_exec_space == ASR::exec_spaceType::Host ||
            ftype->m_exec_space == ASR::exec_spaceType::HostDevice;
        for (size_t i = 0; i < n_args; i++) {
            if (!args[i].m_value) continue;
            if (!ASR::is_a<ASR::Var_t>(*callee->m_args[i])) continue;
            ASR::symbol_t *dummy_sym =
                ASR::down_cast<ASR::Var_t>(callee->m_args[i])->m_v;
            if (!ASR::is_a<ASR::Variable_t>(*dummy_sym)) continue;
            ASR::Variable_t *dummy =
                ASR::down_cast<ASR::Variable_t>(dummy_sym);
            if (!ASRUtils::is_array(dummy->m_type)) continue;
            if (!ASRUtils::is_array(ASRUtils::expr_type(args[i].m_value))) {
                continue;
            }
            spaces[i] = expr_memory_space(args[i].m_value);
            signature += memory_space_tag(spaces[i]);
            if (spaces[i] != type_memory_space(dummy->m_type)) {
                needs_clone = true;
            }
        }
        if (!needs_clone) {
            process_function(callee, false);
            return nullptr;
        }

        auto key = std::make_pair(target, spaces);
        auto it = clones.find(key);
        if (it != clones.end()) {
            return reachable_symbol(called, it->second);
        }

        SymbolTable *destination = callee->m_symtab->parent;
        std::string clone_name = destination->get_unique_name(
            std::string(callee->m_name) + "_device_" + signature);
        ASRUtils::SymbolDuplicator duplicator(al);
        ASR::symbol_t *clone_sym = duplicator.duplicate_Function(callee,
            destination);
        if (!clone_sym) {
            // The body holds something the duplicator cannot copy; the
            // call keeps its original target and the code generator
            // reports the mismatch it cannot resolve.
            return nullptr;
        }
        ASR::Function_t *clone = ASR::down_cast<ASR::Function_t>(clone_sym);
        clone->m_name = s2c(al, clone_name);
        destination->add_symbol(clone_name, clone_sym);
        clones[key] = clone_sym;

        ASR::FunctionType_t *clone_type = ASRUtils::get_FunctionType(clone);
        clone_type->m_exec_space = ASR::exec_spaceType::Device;
        if (clone_type->m_deftype != ASR::deftypeType::Implementation &&
                clone_type->m_abi == ASR::abiType::BindC &&
                clone_type->m_bindc_name == nullptr) {
            clone_type->m_bindc_name = s2c(al, callee->m_name);
        }
        for (size_t i = 0; i < n_args; i++) {
            if (!ASR::is_a<ASR::Var_t>(*clone->m_args[i])) continue;
            ASR::symbol_t *dummy_sym =
                ASR::down_cast<ASR::Var_t>(clone->m_args[i])->m_v;
            if (!ASR::is_a<ASR::Variable_t>(*dummy_sym)) continue;
            ASR::Variable_t *dummy =
                ASR::down_cast<ASR::Variable_t>(dummy_sym);
            if (!ASRUtils::is_array(dummy->m_type)) continue;
            set_variable_space(dummy, spaces[i]);
            if (i < clone_type->n_arg_types) {
                clone_type->m_arg_types[i] = with_memory_space(
                    clone_type->m_arg_types[i], spaces[i]);
            }
        }

        process_function(clone, false);
        return reachable_symbol(called, clone_sym);
    }

    // A call that reached the original through an external symbol needs one
    // of its own to reach the clone.
    ASR::symbol_t* reachable_symbol(ASR::symbol_t *called,
            ASR::symbol_t *clone) {
        if (!ASR::is_a<ASR::ExternalSymbol_t>(*called)) return clone;
        ASR::ExternalSymbol_t *ext =
            ASR::down_cast<ASR::ExternalSymbol_t>(called);
        std::string name = ASRUtils::symbol_name(clone);
        if (ASR::symbol_t *existing = ext->m_parent_symtab->get_symbol(name)) {
            return existing;
        }
        // The clone is placed next to the original routine, whose owner
        // is not necessarily the one the external symbol named: a call
        // through a type-bound procedure names the type.
        ASR::symbol_t *owner = ASRUtils::get_asr_owner(clone);
        char *module_name = owner
            ? s2c(al, ASRUtils::symbol_name(owner)) : ext->m_module_name;
        ASR::asr_t *new_ext = ASR::make_ExternalSymbol_t(al,
            clone->base.loc, ext->m_parent_symtab, s2c(al, name), clone,
            module_name, nullptr, 0, s2c(al, name),
            ASR::accessType::Public);
        ASR::symbol_t *new_sym = ASR::down_cast<ASR::symbol_t>(new_ext);
        ext->m_parent_symtab->add_symbol(name, new_sym);
        return new_sym;
    }
};

} // namespace

void gpu_materialize_struct_arguments(Allocator &al,
        ASR::TranslationUnit_t &unit) {
    GpuMemorySpaceAssigner materializer(al);
    materializer.materialize_struct_arguments(unit);
}

void pass_gpu_memory_space(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &/*pass_options*/) {
    GpuMemorySpaceAssigner assigner(al);
    assigner.assign(unit);

    // A retargeted call and a clone both change who depends on whom.
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers

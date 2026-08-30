#include <libasr/asr.h>
#include <libasr/asr_utils.h>
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
class GpuCallCollector : public ASR::BaseWalkVisitor<GpuCallCollector> {
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

    // The base walker stops at a block, whose body is where a kernel keeps
    // most of its work.
    void visit_BlockCall(const ASR::BlockCall_t &x) {
        if (!ASR::is_a<ASR::Block_t>(*x.m_m)) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        if (!ASR::is_a<ASR::AssociateBlock_t>(*x.m_m)) return;
        ASR::AssociateBlock_t *block =
            ASR::down_cast<ASR::AssociateBlock_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }
};

// Every Associate of a body, so a pointer can take the space of what it
// points at.
class GpuAssociateCollector
        : public ASR::BaseWalkVisitor<GpuAssociateCollector> {
public:
    std::vector<ASR::Associate_t*> associates;

    void visit_Associate(const ASR::Associate_t &x) {
        associates.push_back(const_cast<ASR::Associate_t*>(&x));
    }

    void visit_BlockCall(const ASR::BlockCall_t &x) {
        if (!ASR::is_a<ASR::Block_t>(*x.m_m)) return;
        ASR::Block_t *block = ASR::down_cast<ASR::Block_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
    }

    void visit_AssociateBlockCall(const ASR::AssociateBlockCall_t &x) {
        if (!ASR::is_a<ASR::AssociateBlock_t>(*x.m_m)) return;
        ASR::AssociateBlock_t *block =
            ASR::down_cast<ASR::AssociateBlock_t>(x.m_m);
        for (size_t i = 0; i < block->n_body; i++) {
            visit_stmt(*block->m_body[i]);
        }
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

    void assign(ASR::TranslationUnit_t &unit) {
        std::vector<ASR::Function_t*> kernels;
        for (auto &item : unit.m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::Function_t>(*item.second)) continue;
            ASR::Function_t *fn =
                ASR::down_cast<ASR::Function_t>(item.second);
            if (ASRUtils::get_FunctionType(fn)->m_exec_space
                    != ASR::exec_spaceType::Device) {
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
    std::set<ASR::Function_t*> processed;
    // (original routine, memory-space signature) -> clone
    std::map<std::pair<ASR::symbol_t*, std::string>, ASR::symbol_t*> clones;

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
            const std::set<std::string> &device_backed) {
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
            if (device_backed.count(std::string(var->m_name))) continue;
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

    void process_function(ASR::Function_t *fn, bool is_kernel) {
        if (processed.count(fn)) return;
        processed.insert(fn);

        std::set<std::string> device_backed;
        if (is_kernel) {
            for (auto &workspace : collect_gpu_vla_workspaces(*fn, 0)) {
                device_backed.insert(workspace.var_name);
            }
        }
        assign_scope(fn->m_symtab, device_backed);
        assign_pointer_spaces(fn);

        GpuCallCollector collector;
        for (size_t i = 0; i < fn->n_body; i++) {
            collector.visit_stmt(*fn->m_body[i]);
        }
        for (ASR::FunctionCall_t *call : collector.function_calls) {
            ASR::symbol_t *target = specialise(call->m_name, call->m_args,
                call->n_args);
            if (target) call->m_name = target;
        }
        for (ASR::SubroutineCall_t *call : collector.subroutine_calls) {
            ASR::symbol_t *target = specialise(call->m_name, call->m_args,
                call->n_args);
            if (target) call->m_name = target;
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
        if (ftype->m_deftype != ASR::deftypeType::Implementation) {
            return nullptr;
        }
        if (callee->n_args != n_args) return nullptr;

        std::vector<ASR::memory_spaceType> spaces(n_args,
            ASR::memory_spaceType::Global);
        std::string signature;
        bool needs_clone = false;
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
            if (spaces[i] != ASR::memory_spaceType::Global) {
                needs_clone = true;
            }
        }
        if (!needs_clone) {
            process_function(callee, false);
            return nullptr;
        }

        auto key = std::make_pair(target, signature);
        auto it = clones.find(key);
        if (it != clones.end()) {
            return reachable_symbol(called, it->second);
        }

        SymbolTable *destination = callee->m_symtab->parent;
        std::string clone_name = destination->get_unique_name(
            std::string(callee->m_name) + "_" + signature);
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
        for (size_t i = 0; i < n_args; i++) {
            if (spaces[i] == ASR::memory_spaceType::Global) continue;
            if (!ASR::is_a<ASR::Var_t>(*clone->m_args[i])) continue;
            ASR::symbol_t *dummy_sym =
                ASR::down_cast<ASR::Var_t>(clone->m_args[i])->m_v;
            if (!ASR::is_a<ASR::Variable_t>(*dummy_sym)) continue;
            ASR::Variable_t *dummy =
                ASR::down_cast<ASR::Variable_t>(dummy_sym);
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
        ASR::asr_t *new_ext = ASR::make_ExternalSymbol_t(al,
            clone->base.loc, ext->m_parent_symtab, s2c(al, name), clone,
            ext->m_module_name, nullptr, 0, s2c(al, name),
            ASR::accessType::Public);
        ASR::symbol_t *new_sym = ASR::down_cast<ASR::symbol_t>(new_ext);
        ext->m_parent_symtab->add_symbol(name, new_sym);
        return new_sym;
    }
};

} // namespace

void pass_gpu_memory_space(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &/*pass_options*/) {
    GpuMemorySpaceAssigner assigner(al);
    assigner.assign(unit);

    // A retargeted call and a clone both change who depends on whom.
    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers

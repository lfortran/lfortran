#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/codegen/gpu_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/device_partition.h>
#include <libasr/pass/pass_utils.h>

#include <deque>
#include <map>
#include <string>
#include <set>
#include <vector>

namespace LCompilers {

/*
Partition the call graph into the code that runs on the host and the code that
runs on the device.

`gpu_offload` marks only the kernel it creates, but a kernel body calls
routines, and those routines call more. Everything the kernel reaches has to
be compiled for the device too: the device-preparation passes have to give a
routine's arrays a shape and a memory space before the device code generators
read them, and they can only do that for a routine they can recognise as
device code.

The closure is taken over the calls a body makes, past external symbols and
type bound procedure declarations, and over the routines a device routine
contains, which nothing outside it can reach. A routine the host also reaches
is cloned: the device code calls the clone, which is Device, and the host
keeps the original. The passes below rewrite a device routine for the device
-- `pass_array_by_data` hands its arrays over as plain data, for one -- and
must never change a routine the host calls, as the host would then call it
with arguments it no longer takes. Only a routine without a body here, such
as a `bind(c)` interface, is shared, as HostDevice.

A module procedure is left as it is. Its signature belongs to the module's
interface, which another translation unit was compiled against, and the
device passes rewrite the signature of what they are given.

The marking is monotone, so the pass can run more than once: a routine only
ever moves from Host towards Device or HostDevice, and a kernel is never
touched. That
matters because a helper such as `_lcompilers_matmul` is created by a pass
that runs well after `gpu_offload`, and so is only visible to a later run.
*/

namespace {

class DevicePartition {
public:
    explicit DevicePartition(Allocator &al_) : al(al_) {}

    void partition(ASR::TranslationUnit_t &unit) {
        collect_symbols(unit.m_symtab);

        std::set<ASR::Function_t*> device;
        std::deque<ASR::Function_t*> work;
        for (ASR::Function_t *fn : functions) {
            if (ASRUtils::get_exec_space(*fn)
                    != ASR::exec_spaceType::Kernel) {
                continue;
            }
            device.insert(fn);
            work.push_back(fn);
        }
        while (!work.empty()) {
            ASR::Function_t *fn = work.front();
            work.pop_front();
            for (ASR::Function_t *callee : reached_by(fn)) {
                if (device.insert(callee).second) work.push_back(callee);
            }
        }

        // What the host reaches: every routine the host can enter, and
        // everything those reach in turn. A routine the device closure did
        // not take is host code itself, and so is a starting point.
        std::set<ASR::Function_t*> host;
        for (ASR::Program_t *program : programs) {
            for (ASR::Function_t *callee : gpu_callees(program->m_body,
                    program->n_body, true)) {
                if (host.insert(callee).second) work.push_back(callee);
            }
        }
        for (ASR::Function_t *fn : functions) {
            if (device.count(fn) > 0) continue;
            for (ASR::Function_t *callee : gpu_callees(fn->m_body,
                    fn->n_body, true)) {
                if (host.insert(callee).second) work.push_back(callee);
            }
        }
        while (!work.empty()) {
            ASR::Function_t *fn = work.front();
            work.pop_front();
            for (ASR::Function_t *callee : gpu_callees(fn->m_body,
                    fn->n_body, true)) {
                if (host.insert(callee).second) work.push_back(callee);
            }
        }

        std::map<ASR::symbol_t*, ASR::symbol_t*> clones =
            clone_shared(al, device, host);

        for (ASR::Function_t *fn : functions) {
            if (device.count(fn) == 0) continue;
            if (clones.count(&fn->base) > 0) continue;
            ASR::exec_spaceType current = ASRUtils::get_exec_space(*fn);
            if (current == ASR::exec_spaceType::Kernel) continue;
            if (current == ASR::exec_spaceType::HostDevice) continue;
            if (module_procedures.count(fn) > 0) {
                // A module procedure is left alone. Its signature is part of
                // the module's interface, which another translation unit was
                // compiled against, and the device passes below rewrite the
                // signature of what they are given.
                continue;
            }
            if (inside_clone_source(fn, clones)) continue;
            ASRUtils::get_FunctionType(fn)->m_exec_space =
                host.count(fn) > 0 ? ASR::exec_spaceType::HostDevice
                                   : ASR::exec_spaceType::Device;
        }
        retarget_device_calls(al, device, clones);
    }

private:
    Allocator &al;
    std::vector<ASR::Function_t*> functions;
    std::vector<ASR::Program_t*> programs;
    std::set<ASR::Function_t*> module_procedures;

    void collect_symbols(SymbolTable *symtab, bool in_module=false) {
        for (auto &item : symtab->get_scope()) {
            ASR::symbol_t *sym = item.second;
            if (ASR::is_a<ASR::Function_t>(*sym)) {
                ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
                functions.push_back(fn);
                if (in_module) module_procedures.insert(fn);
                collect_symbols(fn->m_symtab, in_module);
            } else if (ASR::is_a<ASR::Module_t>(*sym)) {
                collect_symbols(ASR::down_cast<ASR::Module_t>(sym)->m_symtab,
                    true);
            } else if (ASR::is_a<ASR::Program_t>(*sym)) {
                ASR::Program_t *program = ASR::down_cast<ASR::Program_t>(sym);
                programs.push_back(program);
                collect_symbols(program->m_symtab, in_module);
            } else if (ASR::is_a<ASR::Struct_t>(*sym)) {
                collect_symbols(ASR::down_cast<ASR::Struct_t>(sym)->m_symtab,
                    in_module);
            }
        }
    }

    // Whether `fn` is a routine that contains, or is contained in, a routine
    // that is cloned: it was copied with it, and the original stays host
    // code.
    static bool inside_clone_source(ASR::Function_t *fn,
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &clones) {
        for (SymbolTable *s = fn->m_symtab->parent; s; s = s->parent) {
            if (s->asr_owner && ASR::is_a<ASR::symbol_t>(*s->asr_owner) &&
                    clones.count(ASR::down_cast<ASR::symbol_t>(
                        s->asr_owner)) > 0) {
                return true;
            }
        }
        return false;
    }

    // A device clone of every routine with a body that both the host and
    // the device reach, placed next to it. A routine contained in another
    // that is cloned is copied with it. Returns original -> clone.
    std::map<ASR::symbol_t*, ASR::symbol_t*> clone_shared(Allocator &al,
            std::set<ASR::Function_t*> &device,
            const std::set<ASR::Function_t*> &host) {
        std::map<ASR::symbol_t*, ASR::symbol_t*> clones;
        for (ASR::Function_t *fn : functions) {
            if (device.count(fn) == 0 || host.count(fn) == 0) continue;
            if (module_procedures.count(fn) > 0) continue;
            if (ASRUtils::get_exec_space(*fn) != ASR::exec_spaceType::Host) {
                continue;
            }
            if (ASRUtils::get_FunctionType(fn)->m_deftype !=
                    ASR::deftypeType::Implementation) {
                continue;
            }
            if (inside_clone_source(fn, clones)) continue;
            SymbolTable *destination = fn->m_symtab->parent;
            ASRUtils::SymbolDuplicator duplicator(al);
            ASR::symbol_t *clone_sym = duplicator.duplicate_Function(fn,
                destination);
            LCOMPILERS_ASSERT(clone_sym);
            std::string name = destination->get_unique_name(
                std::string(fn->m_name) + "_device");
            ASR::Function_t *clone =
                ASR::down_cast<ASR::Function_t>(clone_sym);
            clone->m_name = s2c(al, name);
            destination->add_symbol(name, clone_sym);
            mark_device(clone);
            clones[&fn->base] = clone_sym;
            device.insert(clone);
        }
        return clones;
    }

    // A clone and the routines it contains are device code only.
    static void mark_device(ASR::Function_t *fn) {
        ASRUtils::get_FunctionType(fn)->m_exec_space =
            ASR::exec_spaceType::Device;
        for (auto &item : fn->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                mark_device(ASR::down_cast<ASR::Function_t>(item.second));
            }
        }
    }

    // Point every call device code makes to a cloned routine at its clone.
    // A call through an external symbol is given one of its own for the
    // clone, next to the original's.
    void retarget_device_calls(Allocator &al,
            const std::set<ASR::Function_t*> &device,
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &clones) {
        if (clones.empty()) return;
        struct Retarget : ASR::BaseWalkVisitor<Retarget> {
            Allocator &al;
            const std::map<ASR::symbol_t*, ASR::symbol_t*> &clones;
            Retarget(Allocator &al_,
                    const std::map<ASR::symbol_t*, ASR::symbol_t*> &c)
                : al(al_), clones(c) {}

            ASR::symbol_t *clone_of(ASR::symbol_t *called) {
                auto it = clones.find(
                    ASRUtils::symbol_get_past_external(called));
                if (it == clones.end()) return nullptr;
                if (!ASR::is_a<ASR::ExternalSymbol_t>(*called)) {
                    return it->second;
                }
                ASR::ExternalSymbol_t *ext =
                    ASR::down_cast<ASR::ExternalSymbol_t>(called);
                std::string name = ASRUtils::symbol_name(it->second);
                if (ASR::symbol_t *existing =
                        ext->m_parent_symtab->get_symbol(name)) {
                    return existing;
                }
                ASR::symbol_t *new_sym = ASR::down_cast<ASR::symbol_t>(
                    ASR::make_ExternalSymbol_t(al, called->base.loc,
                        ext->m_parent_symtab, s2c(al, name), it->second,
                        ext->m_module_name, nullptr, 0, s2c(al, name),
                        ext->m_access));
                ext->m_parent_symtab->add_symbol(name, new_sym);
                return new_sym;
            }

            void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                if (ASR::symbol_t *c = clone_of(x.m_name)) {
                    const_cast<ASR::FunctionCall_t&>(x).m_name = c;
                }
                ASR::BaseWalkVisitor<Retarget>::visit_FunctionCall(x);
            }

            void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
                if (ASR::symbol_t *c = clone_of(x.m_name)) {
                    const_cast<ASR::SubroutineCall_t&>(x).m_name = c;
                }
                ASR::BaseWalkVisitor<Retarget>::visit_SubroutineCall(x);
            }

            void visit_Var(const ASR::Var_t &x) {
                if (ASR::is_a<ASR::Function_t>(
                        *ASRUtils::symbol_get_past_external(x.m_v))) {
                    if (ASR::symbol_t *c = clone_of(x.m_v)) {
                        const_cast<ASR::Var_t&>(x).m_v = c;
                    }
                }
            }
        };
        Retarget retarget(al, clones);
        for (ASR::Function_t *fn : device) {
            if (clones.count(&fn->base) > 0) continue;
            if (inside_clone_source(fn, clones)) continue;
            if (!ASRUtils::is_device_only_function(*fn)) continue;
            retarget.visit_Function(*fn);
        }
    }

    // What device code inside `fn` reaches: what its body calls, what the
    // extents of its arrays call, and the routines it contains, which are
    // reachable from nowhere else.
    static std::set<ASR::Function_t*> reached_by(ASR::Function_t *fn) {
        std::set<ASR::Function_t*> reached = gpu_callees(fn->m_body,
            fn->n_body, true);
        gpu_type_callees(fn->m_symtab, reached);
        for (auto &item : fn->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::Function_t>(*item.second)) {
                reached.insert(ASR::down_cast<ASR::Function_t>(item.second));
            }
        }
        return reached;
    }
};

} // namespace

void pass_device_partition(Allocator &al, ASR::TranslationUnit_t &unit,
        const PassOptions &/*pass_options*/) {
    DevicePartition partition(al);
    partition.partition(unit);

    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers

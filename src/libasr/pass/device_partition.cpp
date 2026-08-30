#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/containers.h>
#include <libasr/pass/device_partition.h>
#include <libasr/pass/pass_utils.h>

#include <deque>
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
becomes HostDevice rather than Device, so that the host code generator still
emits it: the two spaces share one definition here. Cloning the routine per
space is only needed once a later pass has to transform the two copies
differently, and nothing does yet.

A module procedure is left as it is. Its signature belongs to the module's
interface, which another translation unit was compiled against, and the
device passes rewrite the signature of what they are given.

The marking is monotone, so the pass can run more than once: a routine only
ever moves from Host towards HostDevice, and a kernel is never touched. That
matters because a helper such as `_lcompilers_matmul` is created by a pass
that runs well after `gpu_offload`, and so is only visible to a later run.
*/

namespace {

// Every routine a body reaches directly. A call through an external symbol or
// a type bound procedure declaration counts as a call to what it resolves to,
// and a routine handed over as an argument counts as one too, because
// whoever receives it can call it.
class CalleeCollector : public ASR::BaseWalkVisitor<CalleeCollector> {
public:
    std::set<ASR::Function_t*> callees;

    void add(ASR::symbol_t *sym) {
        if (sym == nullptr) return;
        sym = ASRUtils::symbol_get_past_external(sym);
        if (sym == nullptr) return;
        sym = ASRUtils::symbol_get_past_StructMethodDeclaration(sym);
        if (sym != nullptr && ASR::is_a<ASR::Function_t>(*sym)) {
            callees.insert(ASR::down_cast<ASR::Function_t>(sym));
        }
    }

    void visit_FunctionCall(const ASR::FunctionCall_t &x) {
        add(x.m_name);
        ASR::BaseWalkVisitor<CalleeCollector>::visit_FunctionCall(x);
    }

    void visit_SubroutineCall(const ASR::SubroutineCall_t &x) {
        add(x.m_name);
        ASR::BaseWalkVisitor<CalleeCollector>::visit_SubroutineCall(x);
    }

    void visit_Var(const ASR::Var_t &x) {
        add(x.m_v);
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

class DevicePartition {
public:
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
            for (ASR::Function_t *callee : called_by(program->m_body,
                    program->n_body)) {
                if (host.insert(callee).second) work.push_back(callee);
            }
        }
        for (ASR::Function_t *fn : functions) {
            if (device.count(fn) > 0) continue;
            for (ASR::Function_t *callee : called_by(fn->m_body,
                    fn->n_body)) {
                if (host.insert(callee).second) work.push_back(callee);
            }
        }
        while (!work.empty()) {
            ASR::Function_t *fn = work.front();
            work.pop_front();
            for (ASR::Function_t *callee : called_by(fn->m_body,
                    fn->n_body)) {
                if (host.insert(callee).second) work.push_back(callee);
            }
        }

        for (ASR::Function_t *fn : device) {
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
            ASRUtils::get_FunctionType(fn)->m_exec_space =
                host.count(fn) > 0 ? ASR::exec_spaceType::HostDevice
                                   : ASR::exec_spaceType::Device;
        }
    }

private:
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

    static std::set<ASR::Function_t*> called_by(ASR::stmt_t **body,
            size_t n_body) {
        CalleeCollector collector;
        for (size_t i = 0; i < n_body; i++) {
            collector.visit_stmt(*body[i]);
        }
        return collector.callees;
    }

    // What device code inside `fn` reaches: what its body calls, and the
    // routines it contains, which are reachable from nowhere else.
    static std::set<ASR::Function_t*> reached_by(ASR::Function_t *fn) {
        std::set<ASR::Function_t*> reached = called_by(fn->m_body,
            fn->n_body);
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
    DevicePartition partition;
    partition.partition(unit);

    PassUtils::UpdateDependenciesVisitor u(al);
    u.visit_TranslationUnit(unit);
}

} // namespace LCompilers

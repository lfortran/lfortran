#ifndef LIBASR_ASR_SIDE_EFFECT_H
#define LIBASR_ASR_SIDE_EFFECT_H

#include <libasr/asr_utils.h>
#include <libasr/pass/intrinsic_subroutines.h>

#include <map>

namespace LCompilers {

namespace ASR {

inline bool is_side_effect_free_intrinsic_impure_subroutine(int64_t id) {
    return id == static_cast<int64_t>(ASRUtils::IntrinsicImpureSubroutines::MoveAlloc)
        || id == static_cast<int64_t>(ASRUtils::IntrinsicImpureSubroutines::Mvbits);
}

class SideEffectFinder : public BaseWalkVisitor<SideEffectFinder> {
public:
    bool found = false;
    Location loc;
    std::string description;

    void mark_found(const Location &l, const std::string &desc) {
        found = true;
        loc = l;
        description = desc;
    }

    void visit_Print(const Print_t &x) {
        if (found) return;
        mark_found(x.base.base.loc, "PRINT statement");
    }

    void visit_FileOpen(const FileOpen_t &x) {
        if (found) return;
        mark_found(x.base.base.loc, "OPEN statement");
    }

    void visit_FileClose(const FileClose_t &x) {
        if (found) return;
        mark_found(x.base.base.loc, "CLOSE statement");
    }

    void visit_FileBackspace(const FileBackspace_t &x) {
        if (found) return;
        mark_found(x.base.base.loc, "BACKSPACE statement");
    }

    void visit_FileRewind(const FileRewind_t &x) {
        if (found) return;
        mark_found(x.base.base.loc, "REWIND statement");
    }

    void visit_FileEndfile(const FileEndfile_t &x) {
        if (found) return;
        mark_found(x.base.base.loc, "ENDFILE statement");
    }

    void visit_FileInquire(const FileInquire_t &x) {
        if (found) return;
        mark_found(x.base.base.loc, "INQUIRE statement");
    }

    void visit_Flush(const Flush_t &x) {
        if (found) return;
        mark_found(x.base.base.loc, "FLUSH statement");
    }

    // Procedure variables that hold a procedure with an implicit interface,
    // mapped to that procedure, e.g. the temporary a call through an
    // implicit interface is made through. Such a procedure is not known to
    // be pure (F2018 15.4.2.2), so a call through the variable is a call to
    // an impure procedure. Not owned.
    const std::map<const symbol_t*, symbol_t*>* implicit_interface_procedures = nullptr;

    // Marks the call to `name` at `l` if the called procedure is not known to
    // be free of side effects.
    bool check_call(const Location &l, symbol_t* name) {
        symbol_t* sym = ASRUtils::symbol_get_past_external(name);
        std::string proc_name;
        if (is_a<Function_t>(*sym)) {
            if (down_cast<Function_t>(sym)->m_side_effect_free) {
                return false;
            }
            proc_name = down_cast<Function_t>(sym)->m_name;
        } else {
            if (implicit_interface_procedures == nullptr) {
                return false;
            }
            auto proc = implicit_interface_procedures->find(sym);
            if (proc == implicit_interface_procedures->end()) {
                return false;
            }
            proc_name = ASRUtils::symbol_name(proc->second);
        }
        mark_found(l, "Call to impure procedure '" + proc_name + "'");
        return true;
    }

    void visit_SubroutineCall(const SubroutineCall_t &x) {
        if (found) return;
        if (x.m_name && check_call(x.base.base.loc, x.m_name)) {
            return;
        }
        BaseWalkVisitor::visit_SubroutineCall(x);
    }

    void visit_IntrinsicImpureSubroutine(const IntrinsicImpureSubroutine_t &x) {
        if (found) return;
        if (is_side_effect_free_intrinsic_impure_subroutine(x.m_sub_intrinsic_id)) {
            return;
        }
        mark_found(x.base.base.loc, "Call to impure intrinsic subroutine");
    }

    void visit_FunctionCall(const FunctionCall_t &x) {
        if (found) return;
        if (x.m_name && check_call(x.base.base.loc, x.m_name)) {
            return;
        }
        BaseWalkVisitor::visit_FunctionCall(x);
    }

    bool check_trait_call(const Location &l, symbol_t *name, int64_t slot) {
        if (!check_call(l, name)) return false;
        auto *contract = down_cast<TraitRuntimeContract_t>(ASRUtils::get_asr_owner(
            ASRUtils::symbol_get_past_external(name)));
        auto *origin = ASRUtils::symbol_get_past_external(
            contract->m_slots[slot].m_origins[0]);
        description = "call to impure trait procedure '" +
            std::string(ASRUtils::symbol_name(origin)) + "'";
        return true;
    }

    void visit_TraitFunctionCall(const TraitFunctionCall_t &x) {
        if (found) return;
        if (check_trait_call(x.base.base.loc, x.m_name, x.m_slot)) return;
        BaseWalkVisitor::visit_TraitFunctionCall(x);
    }

    void visit_TraitSubroutineCall(const TraitSubroutineCall_t &x) {
        if (found) return;
        if (check_trait_call(x.base.base.loc, x.m_name, x.m_slot)) return;
        BaseWalkVisitor::visit_TraitSubroutineCall(x);
    }

    void visit_TraitAllocate(const TraitAllocate_t &x) {
        if (!found) mark_found(x.base.base.loc,
            "runtime trait allocation with unchecked dynamic lifecycle effects");
    }

    void visit_TraitAssignment(const TraitAssignment_t &x) {
        if (!found) mark_found(x.base.base.loc,
            "runtime trait assignment with unchecked dynamic lifecycle effects");
    }

    void visit_ExplicitDeallocate(const ExplicitDeallocate_t &x) {
        for (size_t i = 0; i < x.n_vars && !found; i++) {
            if (ASRUtils::is_trait_owner(ASRUtils::expr_type(x.m_vars[i]))) {
                mark_found(x.base.base.loc,
                    "runtime trait deallocation with unchecked dynamic lifecycle effects");
            }
        }
        if (!found) BaseWalkVisitor::visit_ExplicitDeallocate(x);
    }
};

} // namespace ASR

} // namespace LCompilers

#endif // LIBASR_ASR_SIDE_EFFECT_H

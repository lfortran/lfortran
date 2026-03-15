#ifndef LIBASR_ASR_SIDE_EFFECT_H
#define LIBASR_ASR_SIDE_EFFECT_H

#include <libasr/asr_utils.h>
#include <libasr/pass/intrinsic_subroutines.h>

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

    void visit_SubroutineCall(const SubroutineCall_t &x) {
        if (found) return;
        if (x.m_name) {
            symbol_t* sym = ASRUtils::symbol_get_past_external(x.m_name);
            if (is_a<Function_t>(*sym)) {
                Function_t* f = down_cast<Function_t>(sym);
                if (!f->m_side_effect_free) {
                    mark_found(x.base.base.loc, "Call to impure procedure '" +
                        std::string(f->m_name) + "'");
                    return;
                }
            }
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
        if (x.m_name) {
            symbol_t* sym = ASRUtils::symbol_get_past_external(x.m_name);
            if (is_a<Function_t>(*sym)) {
                Function_t* f = down_cast<Function_t>(sym);
                if (!f->m_side_effect_free) {
                    mark_found(x.base.base.loc, "Call to impure procedure '" +
                        std::string(f->m_name) + "'");
                    return;
                }
            }
        }
        BaseWalkVisitor::visit_FunctionCall(x);
    }
};

} // namespace ASR

} // namespace LCompilers

#endif // LIBASR_ASR_SIDE_EFFECT_H

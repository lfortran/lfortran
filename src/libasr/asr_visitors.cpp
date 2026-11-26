#include <libasr/asr.h>
#include <libasr/asr_walk_visitor.h>
#include <libasr/asr_pass_walk_visitor.h>
#include <libasr/asr_expr_call_replacer_visitor.h>
#include <libasr/asr_expr_stmt_duplicator_visitor.h>
#include <libasr/asr_expr_base_replacer_visitor.h>
#include <libasr/asr_stmt_base_replacer_visitor.h>

// With LCOMPILERS_VIRTUAL_VISITORS each generated visitor family is compiled
// here, once, instead of once for every visitor that derives from it.

#if defined(LCOMPILERS_VIRTUAL_VISITORS)

namespace LCompilers::ASR {

template class BaseWalkVisitorVirtual<void>;
template class ASRPassBaseWalkVisitorVirtual<void>;
template class CallReplacerOnExpressionsVisitorVirtual<void>;
template class BaseExprStmtDuplicatorVirtual<void>;
template class BaseExprReplacerVirtual<void>;
template class BaseStmtReplacerVirtual<void>;

} // namespace LCompilers::ASR

#endif

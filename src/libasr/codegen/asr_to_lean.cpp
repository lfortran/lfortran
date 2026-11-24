#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <functional>
#include <iomanip>
#include <map>
#include <set>
#include <sstream>
#include <vector>

#include <libasr/asr.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_walk_visitor.h>
#include <libasr/codegen/asr_to_lean.h>
#include <libasr/pass/intrinsic_array_function_registry.h>
#include <libasr/pass/intrinsic_function_registry.h>

namespace LCompilers {

// One Lean backend. `--backend=lean` emits computable code against
// `LFortran.Exec`. `--show-lean` uses the same translator in template mode,
// against `LFortran.Arith` (`[FInt ι] [FReal α]`, plus `_float` wrappers).
#include "asr_to_lean_exec.inc"

Result<std::string> asr_to_lean(ASR::TranslationUnit_t &asr,
        diag::Diagnostics &diagnostics, const std::string &filename)
{
    ASRToLeanExec v;
    v.filename = filename;
    v.proof = true;
    try {
        v.translate(asr);
    } catch (const ExecError &e) {
        diagnostics.diagnostics.push_back(e.d);
        return Error();
    } catch (const LCompilersException &e) {
        diagnostics.diagnostics.push_back(diag::Diagnostic(
            std::string("Lean backend: ") + e.what(),
            diag::Level::Error, diag::Stage::CodeGen,
            {diag::Label("", {asr.base.base.loc})}));
        return Error();
    }
    return v.out;
}

} // namespace LCompilers

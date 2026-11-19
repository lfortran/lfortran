#ifndef LIBASR_CODEGEN_CODEGEN_ERROR_H
#define LIBASR_CODEGEN_CODEGEN_ERROR_H

#include <string>

#include <libasr/diagnostics.h>
#include <libasr/location.h>

namespace LCompilers {

// Thrown to stop a code generator that has already reported its error into
// its diagnostics. The function that runs the generator catches it.
class CodeGenAbort {};

// Thrown to stop a code generator with an error. The function that runs the
// generator catches it and adds the error to its diagnostics, which can
// already contain other errors or warnings. Reporting the error into the
// diagnostics directly and throwing CodeGenAbort does the same.
class CodeGenError {
public:
    diag::Diagnostic d;

    CodeGenError(const std::string &msg)
        : d{diag::Diagnostic(msg, diag::Level::Error, diag::Stage::CodeGen)} {}

    CodeGenError(const std::string &msg, const Location &loc)
        : d{diag::Diagnostic(msg, diag::Level::Error, diag::Stage::CodeGen,
            {diag::Label("", {loc})})} {}
};

} // namespace LCompilers

#endif // LIBASR_CODEGEN_CODEGEN_ERROR_H

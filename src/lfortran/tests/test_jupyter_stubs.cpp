#include <lfortran/jupyter.h>
#include <libasr/config.h>

// Keep a reference to the platform stub: a declaration-only stub must fail
// at link time.
#ifndef HAVE_LFORTRAN_XEUS
int (*volatile kernel)(const std::string &, LCompilers::CompilerOptions &, bool)
    = &LCompilers::LFortran::run_kernel;
#endif

int main()
{
    return 0;
}

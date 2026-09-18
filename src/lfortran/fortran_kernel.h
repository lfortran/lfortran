#ifndef LFORTRAN_FORTRAN_KERNEL_H
#define LFORTRAN_FORTRAN_KERNEL_H

#include <string>

#include <libasr/config.h>

#ifdef HAVE_LFORTRAN_XEUS
#include <libasr/utils.h>
#endif

namespace LCompilers::LFortran {

#ifdef HAVE_LFORTRAN_XEUS
    int run_kernel(const std::string &connection_filename,
        const CompilerOptions &compiler_options);
#endif

} // namespace LCompilers::LFortran

#endif // LFORTRAN_FORTRAN_KERNEL_H

#ifndef LFORTRAN_FORTRAN_KERNEL_H
#define LFORTRAN_FORTRAN_KERNEL_H

#include <libasr/config.h>

namespace LCompilers::LFortran {

#ifdef HAVE_LCOMPILERS_XEUS
    int run_kernel(const std::string &connection_filename);
#endif

}

#endif // LFORTRAN_FORTRAN_KERNEL_H

/***************************************************************************
* Copyright (c) 2021, Thorsten Beier                                       *                                                       *
* Copyright (c) 2021, QuantStack                                           *
*                                                                          *
* Distributed under the terms of the BSD 3-Clause License.                 *
*                                                                          *
* The full license is in the file LICENSE, distributed with this software. *
****************************************************************************/

#include <iostream>
#include <memory>


#include <emscripten/bind.h>

#include "fortran_kernel.h"
#include "xeus/xembind.hpp"


EMSCRIPTEN_BINDINGS(lfortran) {
    xeus::export_core();
    using interpreter_type = LFortran::custom_interpreter;
    xeus::export_kernel<interpreter_type>("xkernel");
}

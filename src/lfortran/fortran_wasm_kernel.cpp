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

#include <lfortran/fortran_kernel.h>
#include <xeus/xembind.hpp>

#include "fortran_kernel.cpp"

namespace LFortran
{
    class FortranWASMEvaluator {

    };
}

void stdout_redirector(const std::string& msg)
{
    auto& intp = xeus::get_interpreter();
    intp.publish_stream("stdout", msg);
}

EMSCRIPTEN_BINDINGS(my_module) {
    xeus::export_core();

    em::function("stdout_redirector", &stdout_redirector);

    using interpreter_type = LFortran::custom_interpreter<LFortran::FortranEvaluator>;
    xeus::export_kernel<interpreter_type>("xkernel");
}

#include <iostream>
#include <sstream>

#include <lfortran/fortran_evaluator.h>

using namespace LFortran;

const std::string code = R"RAW(
program expr2
    implicit none

    integer :: x

    x = (2+3)*5
    !print *, x

end program
)RAW";

int main() {
    CompilerOptions copt;
    FortranEvaluator e(copt);

    std::cout << "OK Running in Node?!" << std::endl;
    std::string code0 = code;
    {
        LocationManager lm;
        diag::Diagnostics diagnostics;
        auto res1 = e.get_wasm(code0, lm, diagnostics);
        std::cout << "RES is " << res1.ok << std::endl;
    }
    {
        try {
            LocationManager lm;
            diag::Diagnostics diagnostics;
            auto res1 = e.get_wasm(code0, lm, diagnostics);
            std::cout << "RES is " << res1.ok << std::endl;
        } catch (std::exception& e) 
        {
            std::cout << e.what() << std::endl;
        }
    }
}
#include <tests/doctest.h>

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

TEST_CASE("generator_wasm") {
	CompilerOptions copt;
	FortranEvaluator e(copt);

    std::string code0 = code;
    {
	    LocationManager lm;
	    diag::Diagnostics diagnostics;
		auto res1 = e.get_wasm(code0, lm, diagnostics);
		CHECK(res1.ok);
    }
    {
	    LocationManager lm;
	    diag::Diagnostics diagnostics;
		auto res1 = e.get_wasm(code0, lm, diagnostics);
		CHECK(res1.ok);
    }

	// auto res2 = e.get_wasm(code0, lm, diagnostics);

	// std::cout << "RES 1: " << res1.ok << std::endl;
	// CHECK(res1.ok == res2.ok);

}
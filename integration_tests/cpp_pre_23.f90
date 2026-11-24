program cpp_pre_23
implicit none
#define CURRENT_LINE __LINE__
#define CURRENT_FILE __FILE__
#line 100 "logical/main.f90"
if (CURRENT_LINE /= 100) error stop 1
#include "cpp_pre_23_include/include.h"
if (CURRENT_LINE /= 102) error stop 2
if (CURRENT_FILE /= "logical/main.f90") error stop 3
if (INCLUDED_LINE /= 104) error stop 4
if (INCLUDED_FILE /= "logical/main.f90") error stop 5
end program

program cpp_pre_25
implicit none
#define CURRENT_LINE __LINE__
#define CURRENT_FILE __FILE__
#define ID(x) x
#line 10"adjacent.f90"
if (CURRENT_LINE /= 10) error stop 1
if (CURRENT_FILE /= "adjacent.f90") error stop 2
#line 20""
if (__LINE__ /= 20) error stop 3
if (__FILE__ /= "") error stop 4
#define TARGET_LINE 30
#line TARGET_LINE"macro.f90"
if (ID(CURRENT_LINE) /= 30) error stop 5
if (ID(CURRENT_FILE) /= "macro.f90") error stop 6
# 40"numeric.f90"
if (CURRENT_LINE /= 40) error stop 7
if (CURRENT_FILE /= "numeric.f90") error stop 8
# 50"numeric flags.f90"3 4
if (__LINE__ /= 50) error stop 9
if (__FILE__ /= "numeric flags.f90") error stop 10
# 0"zero.f90"
if (ID(CURRENT_LINE) /= 0) error stop 11
if (ID(CURRENT_FILE) /= "zero.f90") error stop 12
end program

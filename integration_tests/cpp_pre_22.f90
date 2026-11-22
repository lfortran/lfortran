program cpp_pre_22
implicit none
#define CURRENT_LINE __LINE__
#define CURRENT_FILE __FILE__
#define ID(x) x
# 1 "original.f90"
if (__LINE__ /= 1) error stop 1
if (__FILE__ /= "original.f90") error stop 2
# 42 "entered.f90" 1 3
if (CURRENT_LINE /= 42) error stop 3
if (CURRENT_FILE /= "entered.f90") error stop 4
# 70 "original.f90" 2
if (ID(CURRENT_LINE) /= 70) error stop 5
if (ID(CURRENT_FILE) /= "original.f90") error stop 6
# 7
if (__LINE__ /= 7) error stop 7
if (__FILE__ /= "original.f90") error stop 8
# 0 "zero.f90"
if (ID(CURRENT_LINE) /= 0) error stop 9
if (ID(CURRENT_FILE) /= "zero.f90") error stop 10
# 9 ""
if (__LINE__ /= 9) error stop 11
if (__FILE__ /= "") error stop 12
# 123 "space in name.f90" 3 4
if (__LINE__ /= 123) error stop 13
if (__FILE__ /= "space in name.f90") error stop 14
end program

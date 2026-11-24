program cpp_pre_30
implicit none
#define ID(x) x
#define CURRENT_FILE __FILE__
#define NUL_FILE "macro\000suffix.f90"
#line 40 "prefix\000suffix.f90"
if (__FILE__ /= "prefix") error stop 1
if (ID(CURRENT_FILE) /= "prefix") error stop 2
#line __LINE__ __FILE__
if (__FILE__ /= "prefix") error stop 3
if (__LINE__ /= 43) error stop 4
#line 50 "\0suffix.f90"
if (len(CURRENT_FILE) /= 0) error stop 5
#line 60 "hex\x00suffix.f90"
if (ID(CURRENT_FILE) /= "hex") error stop 6
# 70 "numeric\000suffix.f90"
if (__FILE__ /= "numeric") error stop 7
#line 80 NUL_FILE
if (__FILE__ /= "macro") error stop 8
#line 90 "slash\\prefix\000ignored\"ID(.f90"
if (CURRENT_FILE /= 'slash\\prefix') error stop 9
if (__LINE__ /= 91) error stop 10
end program

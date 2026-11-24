program cpp_pre_21
implicit none
integer :: line
if (__LINE__ /= 4) error stop 1
#define CURRENT_LINE __LINE__
#define CURRENT_FILE __FILE__
#define INDIRECT_LINE CURRENT_LINE
#define INDIRECT_FILE CURRENT_FILE
#define ID(x) x
#line 1 "original.f90"
if (__LINE__ /= 1) error stop 2
if (__FILE__ /= "original.f90") error stop 3
#line 50
if (__LINE__ /= 50) error stop 4
if (__FILE__ /= "original.f90") error stop 5
#line 100 "logical/source.f90"
if (INDIRECT_LINE /= 100) error stop 6
if (ID(CURRENT_LINE) /= 101) error stop 7
if (INDIRECT_FILE /= "logical/source.f90") error stop 8
if (ID(CURRENT_FILE) /= "logical/source.f90") error stop 9
#if 0
#line 900 "ignored.f90"
#endif
if (CURRENT_LINE /= 107) error stop 10
if (CURRENT_FILE /= "logical/source.f90") error stop 11
#define TARGET_LINE 200
#define TARGET_FILE "expanded.f90"
#line TARGET_LINE TARGET_FILE
if (__LINE__ /= 200) error stop 12
if (CURRENT_FILE /= "expanded.f90") error stop 13
#line __LINE__
if (CURRENT_LINE /= 202) error stop 14
#line 300 "spliced.f90"
line = 1000 * \
__LINE__
if (line /= 301000) error stop 15
#line 400 \
"continued.f90"
if (CURRENT_LINE /= 400) error stop 16
if (CURRENT_FILE /= "continued.f90") error stop 17
end program

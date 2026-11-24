program cpp_pre_24
implicit none
character(64) :: filename
#define CURRENT_FILE __FILE__
#define ID(x) x
#line 10 "standard\056f90"
if (CURRENT_FILE /= "standard.f90") error stop 1
if (__LINE__ /= 11) error stop 2
#line 20 "hex\x2Ename.f90"
if (ID(CURRENT_FILE) /= "hex.name.f90") error stop 3
#line 30 "escaped\'\?\\name.f90"
if (__FILE__ /= "escaped'?\\name.f90") error stop 4
#line 40 "tab\tname.f90"
filename = CURRENT_FILE
if (filename /= "tab" // achar(9) // "name.f90") error stop 5
# 50 "numeric\056f90"
if (CURRENT_FILE /= "numeric.f90") error stop 6
if (__LINE__ /= 51) error stop 7
# 60 "octal\56\0612.f90" 3
if (ID(CURRENT_FILE) /= "octal.12.f90") error stop 8
# 70 "slash\134path.f90" 3 4
if (__FILE__ /= "slash\\path.f90") error stop 9
# 80 "hex\x2ename.f90"
if (CURRENT_FILE /= "hex.name.f90") error stop 10
#line 90 "unicode\u00e9\U000000e9.f90"
if (__FILE__ /= "unicodeéé.f90") error stop 11
end program

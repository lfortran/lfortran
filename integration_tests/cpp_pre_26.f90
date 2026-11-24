program cpp_pre_26
implicit none
character(64) :: filename
#define CURRENT_FILE __FILE__
#define ID(x) x
#line 10 "standard\nname.f90"
if (__FILE__ /= 'standard\nname.f90') error stop 1
if (__LINE__ /= 11) error stop 2
# 20 "numeric\nname.f90"
if (ID(CURRENT_FILE) /= 'numeric\nname.f90') error stop 3
if (__LINE__ /= 21) error stop 4
#line 30 "octal\012name.f90"
if (CURRENT_FILE /= 'octal\nname.f90') error stop 5
# 40 "hex\x0aname.f90"
if (CURRENT_FILE /= 'hex\nname.f90') error stop 6
#line 50 "slash\\n-and-\nname.f90"
if (ID(CURRENT_FILE) /= 'slash\\n-and-\nname.f90') error stop 7
#line 60 "tab\tbell\aname.f90"
filename = CURRENT_FILE
if (filename /= "tab" // achar(9) // "bell" // achar(7) // "name.f90") error stop 8
end program

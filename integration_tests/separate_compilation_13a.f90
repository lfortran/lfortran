module separate_compilation_13a_module
use, intrinsic:: ieee_arithmetic, only: ieee_quiet_nan, ieee_class_type
contains
subroutine check(x)
real, intent(inout) :: x
TYPE(ieee_class_type) :: xy
xy = ieee_quiet_nan
x = 151.15981
end subroutine
end module

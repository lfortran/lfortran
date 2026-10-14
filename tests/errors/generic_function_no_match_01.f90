! A reference to a generic function that matches none of its specific
! procedures is an error with implicit interfaces too.
module generic_function_no_match_01_mod
implicit none
interface gen_res
    module procedure res_r4
end interface
contains
integer function res_r4(x)
    real(4), intent(in) :: x
    res_r4 = int(x)
end function
end module

program generic_function_no_match_01
use generic_function_no_match_01_mod
implicit none
print *, gen_res(1)
end program

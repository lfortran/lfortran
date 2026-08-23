! `format` statements belong to the implicit-part (F2018 R505, R506) and may
! therefore appear before the `implicit` statement.
module decl_order_01_mod
use iso_fortran_env, only: int32
implicit none

contains

    integer function double_it(x)
    100 format ("x = ", i0)
    implicit none
    integer(int32), intent(in) :: x
    write (*, 100) x
    double_it = 2*x
    end function

end module

program decl_order_01
use decl_order_01_mod, only: double_it
200 format ("result = ", i0)
implicit none
integer :: r
r = double_it(21)
write (*, 200) r
if (r /= 42) error stop
end program

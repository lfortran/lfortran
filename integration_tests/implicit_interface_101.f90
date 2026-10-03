! A module subroutine with an assumed-shape array dummy calls an external
! subroutine through an implicit interface; the procedure is specialised
! for its array dummy and the copy must keep its own call-site interface.
module implicit_interface_101_m
implicit none
contains
subroutine fill(a)
    real(8), intent(inout) :: a(:)
    external implicit_interface_101_dbl
    integer :: i
    do i = 1, size(a)
        call implicit_interface_101_dbl(a(i))
    end do
end subroutine
end module

subroutine implicit_interface_101_dbl(x)
real(8), intent(inout) :: x
x = 2*x
end subroutine

program implicit_interface_101
use implicit_interface_101_m
implicit none
real(8) :: a(3) = [1d0, 2d0, 3d0]
call fill(a)
print *, a
if (abs(a(1) - 2d0) > 1d-12) error stop
if (abs(a(2) - 4d0) > 1d-12) error stop
if (abs(a(3) - 6d0) > 1d-12) error stop
end program

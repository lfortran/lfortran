! A module procedure with an assumed-shape array dummy references a
! `real(8), external` dummy function and CALLs an `external` dummy
! subroutine through implicit interfaces.
module implicit_interface_102_m
implicit none
contains
subroutine apply(f, s, a)
    real(8), external :: f
    external s
    real(8), intent(inout) :: a(:)
    integer :: i
    do i = 1, size(a)
        a(i) = f(a(i))
    end do
    call s(a(1))
end subroutine

real(8) function total(f, a)
    real(8), external :: f
    real(8), intent(in) :: a(:)
    integer :: i
    total = 0
    do i = 1, size(a)
        total = total + f(a(i))
    end do
end function
end module

real(8) function implicit_interface_102_sq(x)
real(8), intent(in) :: x
implicit_interface_102_sq = x*x
end function

subroutine implicit_interface_102_neg(x)
real(8), intent(inout) :: x
x = -x
end subroutine

program implicit_interface_102
use implicit_interface_102_m
implicit none
real(8), external :: implicit_interface_102_sq
external implicit_interface_102_neg
real(8) :: a(2) = [3d0, 4d0]
call apply(implicit_interface_102_sq, implicit_interface_102_neg, a)
print *, a
if (abs(a(1) + 9d0) > 1d-12) error stop
if (abs(a(2) - 16d0) > 1d-12) error stop
print *, total(implicit_interface_102_sq, a)
if (abs(total(implicit_interface_102_sq, a) - 337d0) > 1d-12) error stop
end program

! A module procedure with an assumed-shape dummy and a saved counter, whose
! other dummy's bound inquires, with a DIM known only at run time, about a
! local array (F2018 10.1.11). Calls with whole arrays and with a section
! share one saved variable.
module save_45_m
implicit none
contains
    subroutine save_45_sub(a, dd, x, k_out)
    real, intent(in) :: a(:)
    integer, intent(in) :: dd
    real :: w(3, 4)
    real, intent(inout) :: x(ubound(w, dd))
    integer, intent(out) :: k_out
    integer, save :: k = 0
    k = k + 1
    k_out = k + 0 * size(x)
    end subroutine
end module

program save_45
use save_45_m
implicit none
real :: a(4), x(4)
integer :: k
a = 1
call save_45_sub(a, 1, x, k)
if (k /= 1) error stop 1
call save_45_sub(a, 2, x, k)
if (k /= 2) error stop 2
call save_45_sub(a(1:4:2), 2, x, k)
if (k /= 3) error stop 3
print *, k
end program

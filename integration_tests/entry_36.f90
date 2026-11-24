! An assumed-size dummy array that appears only in an ENTRY statement: an
! element of it is passed to an array dummy argument (sequence association).
module entry_36_m
contains
    subroutine fill(x, n)
    integer, intent(in) :: n
    real, intent(inout) :: x(*)
    x(1:n) = 1
    end subroutine
end module

subroutine entry_36_s(k)
use entry_36_m, only: fill
implicit none
integer :: k
real :: x(*)
integer :: n
k = 0
return
entry entry_36_e(x, n)
call fill(x(2), n)
end subroutine

program entry_36
implicit none
real :: a(4)
integer :: k
call entry_36_s(k)
if (k /= 0) error stop 1
a = 0
call entry_36_e(a, 2)
if (any(abs(a - [0, 1, 1, 0]) > 1e-6)) error stop 2
print *, a
end program

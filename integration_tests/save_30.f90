module save_30_mod
implicit none
type :: pt
    integer :: x
    real :: y
end type
contains
integer function count_calls() result(r)
    type(pt) :: b(3) = [pt(1, 1.5), pt(2, 2.5), pt(3, 3.5)]
    b(2)%x = b(2)%x + 10
    r = b(2)%x
end function
end module

program save_30
use save_30_mod, only: count_calls
implicit none
type :: t
    integer :: k
end type

call s(6)
call s(7)
call s(8)

if (count_calls() /= 12) error stop
if (count_calls() /= 22) error stop

print *, "ok"

contains

subroutine s(expected)
    integer, intent(in) :: expected
    type(t) :: a(2) = [t(5), t(6)]
    a(1)%k = a(1)%k + 1
    print *, a(1)%k, a(2)%k
    if (a(1)%k /= expected) error stop
    if (a(2)%k /= 6) error stop
end subroutine
end program

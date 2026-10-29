! Sequence association: an array element actual argument designates the
! elements of the array from it on (F2018 15.5.2.11), so two dummy arguments
! share storage that neither modifies, with noalias dummy arguments under
! --fast.
module noalias_dummy_07_m
implicit none
contains

subroutine tail_sum(t, a, n, r)
    integer, intent(in) :: n
    real, intent(in) :: t(*), a(n)
    real, intent(out) :: r
    integer :: i
    r = 0
    do i = 1, n - 2
        r = r + t(i)*a(i + 2)
    end do
end subroutine

subroutine tail_scale(t, a, n, r)
    integer, intent(in) :: n
    real, intent(inout) :: t(n - 2)
    real, intent(in) :: a(n)
    real, intent(out) :: r
    integer :: i
    r = 0
    do i = 1, n - 2
        r = r + t(i) + a(i)
    end do
end subroutine

end module

program noalias_dummy_07
use noalias_dummy_07_m
implicit none
real :: a(5), r

a = [1., 2., 3., 4., 5.]
call tail_sum(a(3), a, 5, r)
! 3*3 + 4*4 + 5*5
if (abs(r - 50) > 1e-5) error stop 1
call tail_scale(a(3), a, 5, r)
! (3 + 4 + 5) + (1 + 2 + 3)
if (abs(r - 18) > 1e-5) error stop 2
if (any(abs(a - [1., 2., 3., 4., 5.]) > 1e-6)) error stop 3
print *, "ok"
end program

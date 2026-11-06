! Dummy arguments that share storage that no procedure modifies through
! them (F2018 15.5.2.13 allows it). The parameters are noalias under --fast.
module noalias_dummy_01_m
implicit none
real :: g(5)
contains

! The same array for two intent(in) dummy arguments.
subroutine dot2(x, y, n, r)
    integer, intent(in) :: n
    real, intent(in) :: x(n), y(n)
    real, intent(out) :: r
    integer :: i
    r = 0
    do i = 1, n
        r = r + x(i)*y(i)
    end do
end subroutine

! An intent(inout) dummy argument that is not modified while an
! intent(in) one shares its storage.
subroutine scale_if(a, b, n, doit)
    integer, intent(in) :: n
    real, intent(inout) :: a(n)
    real, intent(in) :: b(n)
    logical, intent(in) :: doit
    integer :: i
    real :: s
    s = 0
    do i = 1, n
        s = s + a(i) + b(i)
    end do
    if (doit) a(1) = s
end subroutine

! A module variable passed as the actual argument and also read directly.
subroutine sum_with_module(x, r)
    real, intent(in) :: x(:)
    real, intent(out) :: r
    integer :: i
    r = 0
    do i = 1, size(x)
        r = r + x(i) + g(i)
    end do
end subroutine

! The same character variable for two dummy arguments.
subroutine count_same(s1, s2, k)
    character(len=*), intent(in) :: s1, s2
    integer, intent(out) :: k
    integer :: i
    k = 0
    do i = 1, len(s1)
        if (s1(i:i) == s2(i:i)) k = k + 1
    end do
end subroutine

end module

program noalias_dummy_01
use noalias_dummy_01_m
implicit none
real :: a(5), r
integer :: k
character(len=6) :: s

a = [1., 2., 3., 4., 5.]
call dot2(a, a, 5, r)
if (abs(r - 55) > 1e-5) error stop 1

call scale_if(a, a, 5, .false.)
if (any(abs(a - [1., 2., 3., 4., 5.]) > 1e-6)) error stop 2
call scale_if(a, [1., 1., 1., 1., 1.], 5, .true.)
if (abs(a(1) - 20) > 1e-5) error stop 3

g = [10., 20., 30., 40., 50.]
call sum_with_module(g, r)
if (abs(r - 300) > 1e-4) error stop 4

s = "abcdef"
call count_same(s, s, k)
if (k /= 6) error stop 5
call count_same(s(1:3), s(4:6), k)
if (k /= 0) error stop 6
print *, "ok"
end program

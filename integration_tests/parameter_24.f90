! Local named constant arrays are read in place from static read-only data:
! indexing, sections, whole-array and section actual arguments, host
! association, recursion, equal constants and repeated array constructors.
module parameter_24_mod
implicit none
real :: a(8, 4)
real, parameter :: c(8) = [1., 2., 3., 4., 5., 6., 7., 8.]
contains

subroutine init()
    a(:, 1) = [1., 2., 3., 4., 5., 6., 7., 8.]
    a(:, 2) = c
    a(:, 3) = [1., 2., 3., 4., 5., 6., 7., 8.]
    a(:, 4) = c
end subroutine

real function total(x)
    real, intent(in) :: x(:)
    total = sum(x)
end function

integer function total_explicit(n, x)
    integer, intent(in) :: n
    integer, intent(in) :: x(n)
    total_explicit = sum(x)
end function

recursive integer function rec(n) result(r)
    integer, intent(in) :: n
    integer, parameter :: p(3) = [10, 20, 30]
    if (n <= 1) then
        r = p(1)
    else
        r = p(mod(n, 3) + 1) + rec(n - 1)
    end if
end function

end module

program parameter_24
use parameter_24_mod
implicit none
integer, parameter :: ip(5) = [1, -2, 3, -4, 5]
integer, parameter :: ip_same(5) = [1, -2, 3, -4, 5]
integer(1), parameter :: i1(3) = [1_1, -2_1, 127_1]
integer(8), parameter :: i8(2) = [huge(1_8), -3_8]
real(8), parameter :: r8(4) = [0.5d0, 1.5d0, 2.5d0, 3.5d0]
real, parameter :: zero(3) = 0.
logical, parameter :: l(4) = [.true., .false., .false., .true.]
complex, parameter :: z(2) = [(1., 2.), (3., -4.)]
complex(8), parameter :: z8(2) = [(1d0, 2d0), (3d0, -4d0)]
integer, parameter :: m(2, 3) = reshape([1, 2, 3, 4, 5, 6], [2, 3])
integer :: i, j, k, b(5)
real(8) :: s

call init()
do i = 1, 4
    do j = 1, 8
        if (abs(a(j, i) - real(j)) > 0) error stop 1
    end do
end do

k = 0
do i = 1, size(ip)
    k = k + ip(i) * ip_same(i)
end do
if (k /= 55) error stop 2
b = ip
b(2:4) = ip_same(4:2:-1)
if (any(b /= [1, -4, 3, -2, 5])) error stop 3
if (total_explicit(5, ip) /= 3) error stop 4
if (total_explicit(3, ip(2:4)) /= -3) error stop 5
if (abs(total(c) - 36.) > 0) error stop 6
if (abs(total(c(2:8:2)) - 20.) > 0) error stop 7

if (i1(3) /= 127_1 .or. sum(int(i1)) /= 126) error stop 8
if (i8(1) /= huge(1_8) .or. i8(2) /= -3_8) error stop 9
s = 0
do i = 1, 4
    s = s + r8(i)
end do
if (abs(s - 8d0) > 0) error stop 10
if (any(zero /= 0.)) error stop 11
if (.not. (l(1) .and. l(4)) .or. l(2) .or. l(3)) error stop 12
if (count(l) /= 2) error stop 13
if (z(2) /= (3., -4.) .or. abs(sum(z) - (4., -2.)) > 0) error stop 14
if (z8(1) /= (1d0, 2d0) .or. z8(2) /= (3d0, -4d0)) error stop 15
k = 0
do j = 1, 3
    do i = 1, 2
        k = k + m(i, j) * (10 * i + j)
    end do
end do
if (k /= 380) error stop 16
if (any(m(:, 2) /= [3, 4]) .or. any(m(2, :) /= [2, 4, 6])) error stop 17
if (rec(4) /= 70) error stop 18
call host()
print *, "ok"

contains

subroutine host()
    if (ip(5) /= 5 .or. abs(r8(3) - 2.5d0) > 0) error stop 19
    if (sum(m) /= 21) error stop 20
end subroutine

end program

! Reductions over elementwise expressions of array sections, which are
! accumulated in a loop that indexes each section through its base array
module sum_05_mod
implicit none

type :: conv
    real, allocatable :: kernel(:, :, :, :)
    real :: w(3, 4)
end type

contains

    ! The neural-fortran conv1d pattern: rank-2 sections of a rank-4
    ! component with scalar subscripts and of an assumed-shape dummy
    subroutine forward(self, input, z)
        type(conv), intent(in) :: self
        real, intent(in) :: input(:, :)
        real, intent(out) :: z(:, :)
        integer :: n, j, k, iws, iwe, ic, ik
        real :: r
        k = size(self%kernel, 4)
        do j = 1, size(z, 2)
            iws = j
            iwe = j + k - 1
            do n = 1, size(z, 1)
                z(n, j) = sum(self%kernel(n, j, :, :) * input(:, iws:iwe))
                r = 0
                do ik = 1, k
                    do ic = 1, size(input, 1)
                        r = r + self%kernel(n, j, ic, ik) * input(ic, iws + ik - 1)
                    end do
                end do
                if (abs(z(n, j) - r) > 1e-5 * max(1.0, abs(r))) error stop 1
            end do
        end do
    end subroutine

    ! Sections of an assumed-shape dummy whose actual is itself strided
    real function strided_dot(x, y, lo, hi, st) result(s)
        real, intent(in) :: x(:, :), y(:)
        integer, intent(in) :: lo, hi, st
        s = sum(x(lo:hi:st, 2) * y(size(y):1:-st))
    end function

    integer function count_pos(p) result(c)
        real, pointer, intent(in) :: p(:)
        c = count(p(2:size(p)) - p(1:size(p) - 1) > 0)
    end function

end module

program sum_05
use sum_05_mod
implicit none
real :: a(10), b(10), s, r, big(20, 3), y(5)
real :: c(-3:6), d(0:4, -1:2, 3)
real, allocatable :: al(:, :), z(:, :), input(:, :)
real, pointer :: p(:), q(:)
real, target :: t(12)
integer :: ia(-2:7), i, j, k, lo, hi, n, cnt
integer(8) :: lo8, hi8
logical :: la(10), lres
type(conv) :: cv

do i = 1, 10
    a(i) = i
    b(i) = 11 - i
    c(i - 4) = 0.5 * i
    ia(i - 3) = mod(3 * i, 7) - 3
    la(i) = mod(i, 3) == 0
end do

! Non-unit and negative strides, both operands sections
s = sum(a(2:10:3) * b(9:1:-4))
r = a(2) * b(9) + a(5) * b(5) + a(8) * b(1)
if (s /= r) error stop 2
! Non-default lower bounds, the section that drives the loop is the second
s = sum(b(1:4) * c(6:-3:-3))
r = b(1) * c(6) + b(2) * c(3) + b(3) * c(0) + b(4) * c(-3)
if (s /= r) error stop 3
! Runtime bounds and strides, including a zero-size section
do lo = -3, 6
    do hi = lo - 1, 6
        s = sum(c(lo:hi) * a(1:hi - lo + 1))
        r = 0
        do j = lo, hi
            r = r + c(j) * a(j - lo + 1)
        end do
        if (s /= r) error stop 4
    end do
end do
lo = 7
hi = 3
s = sum(a(lo:hi) * b(lo:hi))
if (s /= 0) error stop 5
s = product(a(lo:hi) + b(lo:hi))
if (s /= 1) error stop 6
! Rank-3 sections with scalar subscripts in different positions
do k = 1, 3
    do j = -1, 2
        do i = 0, 4
            d(i, j, k) = i + 10 * j + 100 * k
        end do
    end do
end do
do k = 1, 3
    s = sum(d(1:4, 0, k) * d(4:1:-1, 2, 4 - k))
    r = 0
    do i = 1, 4
        r = r + d(i, 0, k) * d(5 - i, 2, 4 - k)
    end do
    if (s /= r) error stop 7
    s = sum(d(:, :, k) - 2 * d(:, :, 1))
    r = 0
    do j = -1, 2
        do i = 0, 4
            r = r + d(i, j, k) - 2 * d(i, j, 1)
        end do
    end do
    if (s /= r) error stop 8
end do
s = maxval(d(0:4:2, -1, :) - d(4:0:-2, 2, :))
if (s /= d(4, -1, 3) - d(0, 2, 3)) error stop 9
s = minval(d(0:4:2, -1, :) - d(4:0:-2, 2, :))
if (s /= d(0, -1, 1) - d(4, 2, 1)) error stop 10
! Subscripts that are bounds and sizes of the arrays
s = sum(a(lbound(a, 1):size(a) - 1) * b(2:ubound(b, 1)))
r = 0
do i = 1, 9
    r = r + a(i) * b(i + 1)
end do
if (s /= r) error stop 11
! integer(8) subscripts
lo8 = 3
hi8 = 8
s = sum(a(lo8:hi8) * b(lo8 - 2:hi8 - 2))
r = 0
do i = 3, 8
    r = r + a(i) * b(i - 2)
end do
if (s /= r) error stop 12
! Masks and logical reductions over sections
s = sum(a(1:9) * b(2:10), mask=la(1:9) .or. ia(-2:6) > 0)
r = 0
do i = 1, 9
    if (la(i) .or. ia(i - 3) > 0) r = r + a(i) * b(i + 1)
end do
if (s /= r) error stop 13
cnt = count(ia(-2:7:2) > ia(7:-2:-2))
n = 0
do i = 0, 4
    if (ia(-2 + 2 * i) > ia(7 - 2 * i)) n = n + 1
end do
if (cnt /= n) error stop 14
lres = any(a(2:10) < b(1:9) .and. la(2:10))
if (.not. lres) error stop 15
lres = all(a(1:5) + b(1:5) == 11.0)
if (.not. lres) error stop 16
if (maxval(ia(-2:7) * ia(7:-2:-1)) /= maxval([(ia(i) * ia(5 - i), i = -2, 7)])) error stop 17
! The section aliases the target
a(1) = sum(a(1:3) * a(2:4))
if (a(1) /= 1 * 2 + 2 * 3 + 3 * 4) error stop 18
a(1) = 1
! Allocatable arrays with non-default lower bounds
allocate(al(-1:3, 2:4))
do j = 2, 4
    do i = -1, 3
        al(i, j) = i * j
    end do
end do
s = sum(al(3:-1:-2, 2) * al(-1:3:2, 4))
r = al(3, 2) * al(-1, 4) + al(1, 2) * al(1, 4) + al(-1, 2) * al(3, 4)
if (s /= r) error stop 19
! Strided actual of an assumed-shape dummy
do j = 1, 3
    do i = 1, 20
        big(i, j) = i - 2 * j
    end do
end do
do i = 1, 5
    y(i) = 0.5 * i
end do
s = strided_dot(big(1:20:2, :), y, 1, 5, 2)
r = big(1, 2) * y(5) + big(5, 2) * y(3) + big(9, 2) * y(1)
if (s /= r) error stop 20
! Pointer arrays, associated with a strided target
do i = 1, 12
    t(i) = mod(5 * i, 7)
end do
p => t(1:12:2)
q => t(12:1:-3)
s = sum(p(2:4) * q(1:3))
r = t(3) * t(12) + t(5) * t(9) + t(7) * t(6)
if (s /= r) error stop 21
cnt = count_pos(p)
n = 0
do i = 3, 11, 2
    if (t(i) - t(i - 2) > 0) n = n + 1
end do
if (cnt /= n) error stop 22
! Sections of a component of a derived type and of the dummy argument
allocate(cv%kernel(4, 6, 2, 3), input(2, 8), z(4, 6))
do k = 1, 3
    do j = 1, 6
        do i = 1, 4
            cv%kernel(i, j, 1, k) = 0.25 * i - 0.5 * j + k
            cv%kernel(i, j, 2, k) = 0.125 * i * j - k
        end do
    end do
end do
do j = 1, 8
    input(1, j) = 0.5 * j
    input(2, j) = 1 - 0.25 * j
end do
call forward(cv, input, z)
do j = 1, 4
    do i = 1, 3
        cv%w(i, j) = i - j
    end do
end do
s = sum(cv%w(3:1:-1, 2) * cv%w(1:3, 4))
r = cv%w(3, 2) * cv%w(1, 4) + cv%w(2, 2) * cv%w(2, 4) + cv%w(1, 2) * cv%w(3, 4)
if (s /= r) error stop 23
print *, "ok"
end program

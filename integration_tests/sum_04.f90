! Reductions with a scalar result over elementwise array expressions, which
! are accumulated in a loop without an array temporary
module sum_04_mod
implicit none

type :: layer
    real, allocatable :: kernel(:, :, :, :)
    real, allocatable :: biases(:, :)
    real, allocatable :: z(:, :)
end type

integer :: ncalls = 0

contains

    subroutine forward(self, input)
        type(layer), intent(inout) :: self
        real, intent(in) :: input(:, :)
        integer :: n, j, k, iws, iwe
        k = size(self%kernel, 4)
        do j = 1, size(self%z, 2)
            iws = j
            iwe = j + k - 1
            do n = 1, size(self%z, 1)
                self%z(n, j) = sum(self%kernel(n, j, :, :) * input(:, iws:iwe)) &
                    + self%biases(n, j)
            end do
        end do
    end subroutine

    real function dot_assumed(x, y) result(r)
        real, intent(in) :: x(:), y(:)
        r = sum(x * y)
    end function

    real function scale()
        ncalls = ncalls + 1
        scale = 2.0
    end function

end module

program sum_04
use sum_04_mod
implicit none
real :: a(10), b(10), s, r, x(4), y(4), m(3, 3), v(3)
real :: c(-3:6)
real(8) :: d(6), e(6), sd, rd
integer :: ia(8), ib(8), is, ir, i, j, l, n, cnt
complex :: za(5), zb(5), zs, zr
logical :: lres
real, allocatable :: al(:), bl(:)
real, pointer :: p(:)
real, target :: t(10)
type(layer) :: lay
real :: input(3, 6)

do i = 1, 10
    a(i) = i
    b(i) = 11 - i
    c(i - 4) = 0.5 * i
    t(i) = i - 5
end do

! Runtime bounds, stride, negative stride, zero size
do i = 1, 4
    s = sum(a(i:i+5) * b(i:i+5))
    r = 0
    do j = i, i + 5
        r = r + a(j) * b(j)
    end do
    if (s /= r) error stop 1
end do
s = sum(a(1:9:2) * b(2:10:2))
r = 0
do j = 1, 9, 2
    r = r + a(j) * b(j + 1)
end do
if (s /= r) error stop 2
s = sum(a(10:1:-3) * b(1:4))
r = a(10) * b(1) + a(7) * b(2) + a(4) * b(3) + a(1) * b(4)
if (s /= r) error stop 3
n = 0
s = sum(a(5:n) * b(5:n))
if (s /= 0) error stop 4
if (product(a(5:n) * b(5:n)) /= 1) error stop 5
if (maxval(a(5:n) * b(5:n)) /= -huge(s)) error stop 6
if (minval(a(5:n) * b(5:n)) /= huge(s)) error stop 7
if (count(a(5:n) > b(5:n)) /= 0) error stop 8
if (any(a(5:n) > b(5:n))) error stop 9
if (.not. all(a(5:n) > b(5:n))) error stop 10

! Non-default lower bounds
s = sum(c(-1:4) * a(3:8))
r = 0
do j = -1, 4
    r = r + c(j) * a(j + 4)
end do
if (s /= r) error stop 11

! Inside an expression, with a scalar operand evaluated once
s = 2 * sum(a(2:4) * scale()) + 1
if (s /= 2 * (2 * (2 + 3 + 4)) + 1) error stop 12
if (ncalls /= 1) error stop 13

! Other reductions
if (product(a(1:4) * 0.5) /= 0.5 * 1.0 * 1.5 * 2.0) error stop 14
if (maxval(a(1:6) * b(1:6)) /= 30) error stop 15
if (minval(-a(1:6) * b(1:6)) /= -30) error stop 16
if (count(a(1:10) > b(1:10)) /= 5) error stop 17
if (.not. any(a(1:10) * 2 > 19)) error stop 18
if (any(a(1:10) * 2 > 20)) error stop 19
if (.not. all(a(1:10) + b(1:10) == 11)) error stop 20
if (all(a(2:10) - b(2:10) > -7)) error stop 21

! With a mask
s = sum(a(1:10) * b(1:10), mask=a(1:10) > 5)
r = 0
do j = 6, 10
    r = r + a(j) * b(j)
end do
if (s /= r) error stop 22
s = sum(a(1:10), mask=a(1:10) * 2 > 15)
if (s /= 8 + 9 + 10) error stop 23
if (maxval(a(1:10) * b(1:10), mask=a(1:10) > 7) /= 24) error stop 24
if (minval(a(1:10) - b(1:10), mask=b(1:10) < 5) /= 3) error stop 25
if (product(a(1:10) + 0, mask=mod(int(a(1:10)), 3) == 0) /= 162) error stop 26

! Integer, real(8) and complex
do i = 1, 8
    ia(i) = 2 * i - 7
    ib(i) = i
end do
is = sum(ia(2:7) * ib(2:7))
ir = 0
do j = 2, 7
    ir = ir + ia(j) * ib(j)
end do
if (is /= ir) error stop 27
if (maxval(ia(1:8) * ib(1:8)) /= 72) error stop 28
if (minval(ia(1:8) * ib(1:8)) /= -6) error stop 29
do i = 1, 6
    d(i) = 0.1d0 * i
    e(i) = 1.0d0 / i
end do
sd = sum(d(1:6) * e(1:6) + 1)
rd = 0
do j = 1, 6
    rd = rd + (d(j) * e(j) + 1)
end do
if (abs(sd - rd) > 1d-12) error stop 30
do i = 1, 5
    za(i) = cmplx(i, -i)
    zb(i) = cmplx(1, i)
end do
zs = sum(za(1:5) * zb(1:5))
zr = 0
do j = 1, 5
    zr = zr + za(j) * zb(j)
end do
if (zs /= zr) error stop 31
zs = product(za(1:3) + zb(1:3))
if (zs /= (za(1) + zb(1)) * (za(2) + zb(2)) * (za(3) + zb(3))) error stop 32

! Elemental intrinsic functions
s = sqrt(sum(abs(a(1:4) - b(1:4))**2))
if (abs(s - sqrt(81.0 + 49.0 + 25.0 + 9.0)) > 1e-5) error stop 33

! The variable is referenced by the expression
s = 2
s = sum(a(1:3) * s)
if (s /= 12) error stop 34
x = [1, 2, 3, 4]
y = [4, 3, 2, 1]
x(1) = sum(x * y)
if (x(1) /= 20) error stop 35
x(2) = sum(x(1:2) * x(1:2))
if (x(2) /= 404) error stop 36

! Nested reductions
s = sum(a(1:3) * sum(b(1:2) * a(1:2)))
if (s /= 6 * (10 + 18)) error stop 37

! Assumed-shape, allocatable and pointer arrays
if (dot_assumed(a(2:6), b(2:6)) /= sum([2*9, 3*8, 4*7, 5*6, 6*5])) error stop 38
if (dot_assumed(a(1:10:3), b(1:10:3)) /= 1*10 + 4*7 + 7*4 + 10*1) error stop 39
allocate(al(0:4), bl(5))
al = [1, 2, 3, 4, 5]
bl = [5, 4, 3, 2, 1]
if (sum(al * bl) /= 35) error stop 40
if (sum(al(1:3) * bl(2:4)) /= 2*4 + 3*3 + 4*2) error stop 41
p => t(2:10:2)
if (sum(p * p) /= (-3)**2 + (-1)**2 + 1**2 + 3**2 + 5**2) error stop 42
if (count(p * 2 > 0) /= 3) error stop 43

! A non-elemental array-valued function in the expression
m = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
v = [1, 2, 3]
s = sum(matmul(m, v) * v)
if (s /= 30 * 1 + 36 * 2 + 42 * 3) error stop 44

! Rank 2: the neural-fortran convolution
allocate(lay%kernel(2, 4, 3, 3), lay%biases(2, 4), lay%z(2, 4))
do l = 1, 3
    do j = 1, 3
        do i = 1, 4
            do n = 1, 2
                lay%kernel(n, i, j, l) = n + 2 * i + 3 * j + 5 * l
            end do
        end do
    end do
end do
do i = 1, 6
    do j = 1, 3
        input(j, i) = j - i
    end do
end do
lay%biases = 1
call forward(lay, input)
do i = 1, 4
    do n = 1, 2
        r = 1
        do l = 1, 3
            do j = 1, 3
                r = r + lay%kernel(n, i, j, l) * input(j, i + l - 1)
            end do
        end do
        if (lay%z(n, i) /= r) error stop 45
    end do
end do

cnt = count(lay%z(:, :) > 0)
lres = any(lay%z(1, :) * lay%z(2, :) < 0)
if (cnt /= count(lay%z > 0)) error stop 46
if (lres .neqv. any(lay%z(1, :) < 0 .neqv. lay%z(2, :) < 0)) error stop 47
print *, "ok"
end program

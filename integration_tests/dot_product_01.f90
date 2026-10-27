! DOT_PRODUCT of elementwise expressions of arrays and array sections, which
! is accumulated in a loop without evaluating the expressions into arrays
module dot_product_01_mod
implicit none

type :: layer
    real, allocatable :: w(:, :)
    real :: v(6)
end type

contains

    ! Sections of assumed-shape dummies with runtime bounds and stride
    real function strided_dot(x, y, lo, hi, st) result(s)
        real, intent(in) :: x(:, :), y(:)
        integer, intent(in) :: lo, hi, st
        s = dot_product(x(lo:hi:st, 2) * 2, y(hi:lo:-st) + 1)
    end function

    real function loop_dot(x, y, lo, hi, st) result(s)
        real, intent(in) :: x(:, :), y(:)
        integer, intent(in) :: lo, hi, st
        integer :: i, j
        s = 0
        j = hi
        do i = lo, hi, st
            s = s + x(i, 2) * 2 * (y(j) + 1)
            j = j - st
        end do
    end function

    real function layer_dot(l, j, x) result(s)
        type(layer), intent(in) :: l
        integer, intent(in) :: j
        real, intent(in) :: x(:)
        s = dot_product(l%w(:, j) - l%v(1:size(l%w, 1)), x)
    end function

end module

program dot_product_01
use dot_product_01_mod
implicit none
real :: a(10), b(10), c(-3:6), s, r, big(20, 3), y(20)
real(8) :: da(10), db(10), ds, dr
integer :: ia(-2:7), ib(10), is, ir, i, j, n
complex :: ca(6), cb(6), cs, cr
complex(8) :: za(4), zb(4), zs, zr
logical :: la(8), lb(8), lc(8), ls, lr
real, allocatable :: al(:)
real, pointer :: p(:)
real, target :: t(12)
type(layer) :: ly

do i = 1, 10
    a(i) = i
    b(i) = 11 - i
    c(i - 4) = 0.5 * i
    ia(i - 3) = mod(3 * i, 7) - 3
    ib(i) = 2 * i - 9
    da(i) = 0.25d0 * i
    db(i) = 3 - i
end do
do i = 1, 6
    ca(i) = cmplx(i, 2 - i)
    cb(i) = cmplx(3 * i - 7, i)
end do
do i = 1, 4
    za(i) = cmplx(0.5d0 * i, -i, kind=8)
    zb(i) = cmplx(i - 2, 2 * i, kind=8)
end do
do i = 1, 8
    la(i) = mod(i, 2) == 0
    lb(i) = mod(i, 3) == 0
    lc(i) = i > 5
end do

! Real, an expression and a whole array
s = dot_product(a * 2, b)
r = 0
do i = 1, 10
    r = r + a(i) * 2 * b(i)
end do
if (s /= r) error stop 1
! Both arguments are expressions of sections with non-unit and negative
! strides, the second section has non-default lower bounds
s = dot_product(a(2:10:3) - 1, c(6:-3:-4) * b(1:3))
r = (a(2) - 1) * (c(6) * b(1)) + (a(5) - 1) * (c(2) * b(2)) + &
    (a(8) - 1) * (c(-2) * b(3))
if (s /= r) error stop 2
! Only the first argument is a section
s = dot_product(c(-3:6:3), b(1:4) / 2)
r = c(-3) * (b(1) / 2) + c(0) * (b(2) / 2) + c(3) * (b(3) / 2) + c(6) * (b(4) / 2)
if (s /= r) error stop 3
! Elemental intrinsic functions
s = dot_product(abs(a(10:1:-1) - 5), sqrt(a * a))
r = 0
do i = 1, 10
    r = r + abs(a(11 - i) - 5) * a(i)
end do
if (s /= r) error stop 4

! Real(8)
ds = dot_product(da(1:9:2) + db(2:10:2), db(9:1:-2))
dr = 0
do i = 1, 5
    dr = dr + (da(2 * i - 1) + db(2 * i)) * db(11 - 2 * i)
end do
if (ds /= dr) error stop 5

! Integer, with non-default lower bounds
is = dot_product(ia(-2:7:3) + 1, ib(10:1:-3))
ir = (ia(-2) + 1) * ib(10) + (ia(1) + 1) * ib(7) + (ia(4) + 1) * ib(4) + &
    (ia(7) + 1) * ib(1)
if (is /= ir) error stop 6
! Integer and real: the integer argument is converted to real
s = dot_product(ib(1:5) * 2, a(6:10))
r = 0
do i = 1, 5
    r = r + real(ib(i) * 2) * a(5 + i)
end do
if (s /= r) error stop 7

! Complex: the first argument is conjugated
cs = dot_product(ca * 2, cb)
cr = 0
do i = 1, 6
    cr = cr + conjg(ca(i) * 2) * cb(i)
end do
if (cs /= cr) error stop 8
cs = dot_product(ca(6:1:-2), cb(1:5:2) - ca(1:3))
cr = conjg(ca(6)) * (cb(1) - ca(1)) + conjg(ca(4)) * (cb(3) - ca(2)) + &
    conjg(ca(2)) * (cb(5) - ca(3))
if (cs /= cr) error stop 9
zs = dot_product(za + zb, zb(4:1:-1))
zr = 0
do i = 1, 4
    zr = zr + conjg(za(i) + zb(i)) * zb(5 - i)
end do
if (zs /= zr) error stop 10

! Logical: any(x .and. y)
ls = dot_product(la .and. lb, lc)
lr = any(la .and. lb .and. lc)
if (ls .neqv. lr) error stop 11
if (.not. ls) error stop 12
ls = dot_product(.not. la(1:7:2), lb(8:2:-2))
if (.not. ls) error stop 13
ls = dot_product(la .and. lb, .not. lb)
if (ls) error stop 30
ls = dot_product(la(2:8:2) .or. lb(1:4), lc(5:8))
if (.not. ls) error stop 14

! Zero-size sections
n = 0
s = dot_product(a(1:n) * 2, b(1:n))
if (s /= 0) error stop 15
is = dot_product(ib(5:4) + 1, ib(3:2))
if (is /= 0) error stop 16
cs = dot_product(ca(4:1) * 2, cb(1:0))
if (cs /= (0, 0)) error stop 17
ls = dot_product(la(3:2) .or. .true., lb(1:0))
if (ls) error stop 18

! The result is an operand of an expression and of an IF condition
s = dot_product(a(1:3) + 1, b(1:3)) * 2 + 1
if (s /= 2 * (2 * 10 + 3 * 9 + 4 * 8) + 1) error stop 19
if (dot_product(a(1:3) + 1, b(1:3)) /= 79) error stop 20

! The array that is assigned appears in the expression
a(1) = dot_product(a(1:3) * 2, a(2:4))
if (a(1) /= 2 * (1 * 2 + 2 * 3 + 3 * 4)) error stop 21
a(1) = 1
ir = (ia(-2) - 1) * ia(-1) + (ia(-1) - 1) * ia(0) + (ia(0) - 1) * ia(1)
ia(-2) = dot_product(ia(-2:0) - 1, ia(-1:1))
if (ia(-2) /= ir) error stop 22

! Assumed-shape dummies, runtime bounds and strides
do j = 1, 3
    do i = 1, 20
        big(i, j) = i * j - 7
    end do
end do
do i = 1, 20
    y(i) = i - 13
end do
s = strided_dot(big, y, 3, 19, 4)
if (s /= loop_dot(big, y, 3, 19, 4)) error stop 23
s = strided_dot(big(1:20:2, :), y(1:20:2), 1, 9, 2)
if (s /= loop_dot(big(1:20:2, :), y(1:20:2), 1, 9, 2)) error stop 24
s = strided_dot(big, y, 2, 1, 1)
if (s /= 0) error stop 25

! Allocatable with non-default lower bounds, and a pointer to a strided target
allocate(al(-4:5))
do i = -4, 5
    al(i) = i
end do
do i = 1, 12
    t(i) = 2 * i
end do
p => t(12:1:-2)
s = dot_product(al(-4:1) * 3, p + 1)
r = 0
do i = 1, 6
    r = r + al(i - 5) * 3 * (t(14 - 2 * i) + 1)
end do
if (s /= r) error stop 26
s = dot_product(p(2:6:2) - 1, al(5:1:-2))
r = (t(10) - 1) * al(5) + (t(6) - 1) * al(3) + (t(2) - 1) * al(1)
if (s /= r) error stop 27

! Components of a derived type
allocate(ly%w(4, 2))
do j = 1, 2
    do i = 1, 4
        ly%w(i, j) = i + 10 * j
    end do
end do
do i = 1, 6
    ly%v(i) = i
end do
s = layer_dot(ly, 2, b(1:4))
r = 0
do i = 1, 4
    r = r + (ly%w(i, 2) - ly%v(i)) * b(i)
end do
if (s /= r) error stop 28
s = dot_product(ly%v(6:1:-1) * 2, ly%v)
if (s /= 2 * (6 * 1 + 5 * 2 + 4 * 3 + 3 * 4 + 2 * 5 + 1 * 6)) error stop 29

print *, "ok"
end program

! Results of `t = matmul(..) <op> ..` are computed into `t` when the
! expression cannot read `t`, and into a temporary otherwise (#16085).
module matmul_14_mod
implicit none
real :: zm(2)
type :: layer
    real, allocatable :: w(:,:), b(:), z(:)
    real :: zf(2)
    real, pointer :: p(:) => null()
end type
contains

function zm_reversed() result(r)
    real :: r(2)
    r = zm(2:1:-1)
end function

subroutine forward(self, x)
    class(layer), intent(inout) :: self
    real, intent(in) :: x(:)
    self%z = matmul(x, self%w) + self%b
end subroutine

subroutine dummies(z, x, w, b)
    real, intent(inout) :: z(:)
    real, intent(in) :: x(:), w(:,:), b(:)
    z = matmul(x, w) + b
end subroutine

end module

program matmul_14
use matmul_14_mod
implicit none
real, allocatable :: x(:), w(:,:), b(:), z(:), sq(:,:)
real, allocatable :: a(:,:), c(:,:), d(:,:), v(:), g(:)
real, allocatable, target :: zt(:)
real, pointer :: p(:), zp(:)
real :: zf(2), xf(3), wf(3,2), bf(2), e1(2)
integer, allocatable :: ix(:), iw(:,:), ib(:), iz(:)
integer(8), allocatable :: kx(:), kw(:,:), kz(:)
complex, allocatable :: cx(:), cw(:,:), cz(:)
complex(8), allocatable :: dcx(:), dcw(:,:), dcz(:)
real(8), allocatable :: z8(:)
logical :: mask(2)
type(layer) :: l
type(layer), target :: lt
real :: eqz(2), eqb(2)
equivalence (eqz, eqb)

allocate(x(3), w(3,2), b(2), z(2), sq(2,2))
x = [1, 2, 3]
w = reshape([1, 2, 3, 4, 5, 6], [3, 2])
b = [10, 20]
e1 = [14, 32]          ! matmul(x, w)
sq = reshape([1, 2, 3, 4], [2, 2])

! computed into the target
z = matmul(x, w) + b
if (any(z /= e1 + b)) error stop 1
z = b - matmul(x, w)
if (any(z /= b - e1)) error stop 2
z = (matmul(x, w) + b) * 2
if (any(z /= (e1 + b) * 2)) error stop 3
z = 2 * matmul(x, w) - b / 2 + 1
if (any(z /= 2 * e1 - b / 2 + 1)) error stop 4
z = matmul(x, w) + matmul(x, w) * b
if (any(z /= e1 + e1 * b)) error stop 5

! the expression reads the target: a temporary
z = b
z = matmul(z, sq) + b
if (any(z /= [50 + 10, 110 + 20])) error stop 6
z = b
z = matmul(x, w) + z(2:1:-1)
if (any(z /= e1 + [20, 10])) error stop 7
z = b
z = matmul(x, w) + z(1)
if (any(z /= e1 + 10)) error stop 8
z = b
z = matmul(x, w) * size(z) + sum(z)
if (any(z /= e1 * 2 + 30)) error stop 9
z = b
z = matmul(x, w) + z
if (any(z /= e1 + b)) error stop 10

! pointers, targets, associate names, functions, equivalence
allocate(zt(2))
zt = b
p => zt
zt = matmul(x, w) + p
if (any(zt /= e1 + b)) error stop 11
zp => zt
zt = b
zp = matmul(x, w) + zt
if (any(zt /= e1 + b)) error stop 12
z = b
associate (q => z)
    q = matmul(x, w) + z
end associate
if (any(z /= e1 + b)) error stop 13
z = b
associate (q => z)
    z = matmul(x, w) + q
end associate
if (any(z /= e1 + b)) error stop 14
zm = b
zm = matmul(x, w) + zm_reversed()
if (any(zm /= e1 + [20, 10])) error stop 15
eqb = b
eqz = matmul(x, w) + eqb
if (any(eqz /= e1 + b)) error stop 16

! derived type components
allocate(l%w(3,2), l%b(2), l%z(2))
l%w = w; l%b = b
call forward(l, x)
if (any(l%z /= e1 + b)) error stop 17
l%z = b
l%z = matmul(x, l%w) + l%z(2:1:-1)
if (any(l%z /= e1 + [20, 10])) error stop 18
l%z = b
l%z = matmul(l%z, sq) + l%b
if (any(l%z /= [60, 130])) error stop 19
l%zf = matmul(x, l%w) + l%b
if (any(l%zf /= e1 + b)) error stop 20
lt%zf = e1 + b
lt%p => lt%zf
lt%zf = matmul(x, w) + lt%p
if (any(lt%zf /= e1 + e1 + b)) error stop 21

! fixed-size arrays and dummy arguments
xf = x; wf = w; bf = b
zf = matmul(xf, wf) + bf
if (any(zf /= e1 + b)) error stop 22
call dummies(z, x, w, b)
if (any(z /= e1 + b)) error stop 23

! non-default lower bounds of the target are kept
deallocate(z)
allocate(z(0:1))
z = matmul(x, w) + b
if (lbound(z, 1) /= 0 .or. any(z /= e1 + b)) error stop 24

! a different kind: the result is converted, through a temporary
allocate(z8(2))
z8 = matmul(x, w) + 0.5d0
if (any(z8 /= e1 + 0.5d0)) error stop 25

! masked assignment
z = 0
mask = [.true., .false.]
where (mask) z = matmul(x, w) + b
if (any(z /= [e1(1) + b(1), 0.0])) error stop 26

! rank-2 x rank-1 and rank-2 x rank-2
allocate(a(2,3), v(3), g(2), c(3,2), d(2,2))
a = transpose(w)
v = x
g = matmul(a, v) + b
if (any(g /= e1 + b)) error stop 27
c = w
d = matmul(a, c) - 1
if (any(d(:,1) /= [14 - 1, 32 - 1])) error stop 28
if (any(d(:,2) /= [32 - 1, 77 - 1])) error stop 29
d = matmul(a, c) + d
if (any(d(:,2) /= [2 * 32 - 1, 2 * 77 - 1])) error stop 30

! integer, integer(8), complex and complex(8)
allocate(ix(3), iw(3,2), ib(2), iz(2))
ix = [1, 2, 3]; iw = reshape([1, 2, 3, 4, 5, 6], [3, 2]); ib = [10, 20]
iz = matmul(ix, iw) + ib
if (any(iz /= [24, 52])) error stop 31
iz = matmul(ix, iw) - iz
if (any(iz /= [14 - 24, 32 - 52])) error stop 32
allocate(kx(3), kw(3,2), kz(2))
kx = ix; kw = iw
kz = matmul(kx, kw) * 3_8 + 4000000000_8
if (any(kz /= [42 + 4000000000_8, 96 + 4000000000_8])) error stop 33
allocate(cx(3), cw(3,2), cz(2))
cx = cmplx(x, 1.0); cw = cmplx(w, 0.0)
cz = matmul(cx, cw) + cmplx(b, [-6.0, -15.0])
if (any(cz /= cmplx(e1 + b, 0.0))) error stop 34
allocate(dcx(3), dcw(3,2), dcz(2))
dcx = cx; dcw = cw
dcz = matmul(dcx, dcw) * (0.0d0, 1.0d0)
if (any(dcz /= cmplx([-6.0, -15.0], e1, kind=8))) error stop 35
print *, "ok"
end program

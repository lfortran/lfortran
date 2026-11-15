program real16_parameter_matmul_01
! matmul of real(16) and complex(16) constants is folded in binary128
implicit none
! e = 2**(-60): 1 + e and 1 - e round to 1 in double precision
real(16), parameter :: e = 8.67361737988403547205962240695953369140625e-19_16
real(16), parameter :: ep = 1.0_16 + e, em = 1.0_16 - e, third = 1.0_16/3.0_16
real(16), parameter :: a(2, 3) = reshape([ep, 2.0_16, 0.1_16, &
    -3.0_16, 0.3_16, em], [2, 3])
real(16), parameter :: b(3, 2) = reshape([0.7_16, ep, -0.2_16, &
    2.0_16, 0.5_16 + e, third], [3, 2])
real(16), parameter :: x(3) = [ep, 0.1_16, 3.0_16]
real(16), parameter :: y(2) = [0.3_16, em]
integer, parameter :: ia(2, 3) = reshape([1, 2, 3, 4, 5, 6], [2, 3])
! Complex products of these are exact, so the result does not depend on
! how the complex multiplication is rounded
complex(16), parameter :: c(2, 2) = reshape([(ep, 0.5_16), &
    (2.0_16, -3.0_16), (0.25_16, em), (-1.0_16, 2.0_16)], [2, 2])
complex(16), parameter :: d(2, 2) = reshape([(1.0_16, 0.5_16), &
    (-2.0_16, 1.0_16), (0.5_16, -1.0_16), (3.0_16, 0.0_16)], [2, 2])
complex(16), parameter :: w(2) = [(0.5_16, 2.0_16), (3.0_16, -1.0_16)]
! Inexact complex products
complex(16), parameter :: f(2, 2) = reshape([(0.1_16, 0.3_16), &
    (0.7_16, -0.2_16), (third, 0.9_16), (-0.6_16, 0.4_16)], [2, 2])
complex(16), parameter :: g(2) = [(0.3_16, -0.1_16), (0.2_16, 0.7_16)]

real(16), parameter :: r1(2) = matmul(a, x)
real(16), parameter :: r2(3) = matmul(y, a)
real(16), parameter :: r3(2, 2) = matmul(a, b)
real(16), parameter :: r4(2) = matmul(reshape([ep, 2.0_16, &
    3.0_16, 4.0_16], [2, 2]), [1.0_16, ep])
real(16), parameter :: r5(2) = matmul(ia, x)
complex(16), parameter :: c1(2) = matmul(c, w)
complex(16), parameter :: c2(2, 2) = matmul(c, d)
complex(16), parameter :: c3(2) = matmul(w, c)
complex(16), parameter :: c4(2) = matmul(y, c)
complex(16), parameter :: c5(2) = matmul(f, g)
complex(16), parameter :: c6(2) = matmul([(ep, 1.0_16), (2.0_16, -0.5_16)], &
    reshape([(1.0_16, 0.0_16), (0.0_16, 1.0_16), (2.0_16, 0.0_16), &
    (0.0_16, 0.25_16)], [2, 2]))

real(16) :: va(2, 3), vb(3, 2), vx(3), vy(2), vm(2, 2), vv(2)
complex(16) :: vc(2, 2), vd(2, 2), vw(2), vf(2, 2), vg(2), t(2)
integer :: via(2, 3)

va = a
vb = b
vx = x
vy = y
via = ia
vc = c
vd = d
vw = w
vf = f
vg = g
vm = reshape([ep, 2.0_16, 3.0_16, 4.0_16], [2, 2])
vv = [1.0_16, ep]

if (any(r1 /= matmul(va, vx))) error stop 1
if (any(r2 /= matmul(vy, va))) error stop 2
if (any(r3 /= matmul(va, vb))) error stop 3
if (any(r4 /= matmul(vm, vv))) error stop 4
if (any(r5 /= matmul(via, vx))) error stop 5
if (any(c1 /= matmul(vc, vw))) error stop 6
if (any(c2 /= matmul(vc, vd))) error stop 7
if (any(c3 /= matmul(vw, vc))) error stop 8
if (any(c4 /= matmul(vy, vc))) error stop 9
t = matmul(vf, vg)
if (any(abs(c5 - t) > 1.0e-32_16)) error stop 10

! Results that differ from double precision values only beyond it
if (r4(1) /= 4.0_16 + 4.0_16*e) error stop 11
if (r4(2) /= 6.0_16 + 4.0_16*e) error stop 12
if (.not. (r4(1) > 4.0_16)) error stop 13
if (real(c1(1), 16) /= 1.25_16 - e/2.0_16) error stop 14
if (aimag(c1(1)) /= 5.0_16 - e) error stop 15
if (real(c6(1), 16) /= 1.5_16 + e .or. aimag(c6(1)) /= 3.0_16) error stop 16
if (real(c6(2), 16) /= 2.125_16 + 2.0_16*e) error stop 17
if (aimag(c6(2)) /= 2.5_16) error stop 18

print *, r1, r2
print *, r3, r4, r5
print *, c1, c2
print *, c3, c4
print *, c5, c6
end program

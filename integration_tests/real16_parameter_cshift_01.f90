program real16_parameter_cshift_01
! cshift and eoshift of real(16) and complex constants are folded exactly:
! the elements are copied without rounding to double precision
implicit none
! e = 2**(-60): 1 + e and 3 + e round to 1 and 3 in double precision
real(16), parameter :: e = 8.67361737988403547205962240695953369140625e-19_16
real(16), parameter :: x = 1.0_16 + e, y = -2.0_16*(1.0_16 + e)
real(16), parameter :: a(4) = [x, y, 3.0_16 + e, -4.0_16]
complex(16), parameter :: z(3) = [(x, y), (y, x), (e, x)]
complex(8), parameter :: w(3) = [(1.0_8, 2.0_8), (3.0_8, 4.0_8), (5.0_8, 6.0_8)]

real(16), parameter :: c1(4) = cshift(a, 1)
real(16), parameter :: c2(4) = cshift(a, -1)
real(16), parameter :: c3(4) = cshift(a, 0)
real(16), parameter :: c4(4) = cshift(a, 4)
complex(16), parameter :: c5(3) = cshift(z, 2)
complex(8), parameter :: c6(3) = cshift(w, 1)
real(16), parameter :: o1(4) = eoshift(a, 1)
real(16), parameter :: o2(4) = eoshift(a, -1, x)
real(16), parameter :: o3(4) = eoshift(a, 2, 9.0_16)
real(16), parameter :: o4(4) = eoshift(a, -4)
complex(16), parameter :: o5(3) = eoshift(z, -1, (e, y))
complex(8), parameter :: o6(3) = eoshift(w, 1, (7.0_8, 8.0_8))

real(16) :: va(4)
complex(16) :: vz(3)

va = a
vz = z

if (.not. (a(1) > 1.0_16)) error stop 1
if (a(1) - 1.0_16 /= e) error stop 2

if (any(c1 /= [va(2), va(3), va(4), va(1)])) error stop 3
if (c1(4) - 1.0_16 /= e) error stop 4
if (c1(2) - 3.0_16 /= e) error stop 5
if (any(c2 /= [va(4), va(1), va(2), va(3)])) error stop 6
if (c2(2) - 1.0_16 /= e) error stop 7
if (any(c3 /= va)) error stop 8
if (any(c4 /= va)) error stop 9
if (any(c5 /= [vz(3), vz(1), vz(2)])) error stop 10
if (real(c5(1), 16) /= e .or. real(c5(2), 16) - 1.0_16 /= e) error stop 11
if (any(c6 /= [w(2), w(3), w(1)])) error stop 12

if (any(o1 /= [va(2), va(3), va(4), 0.0_16])) error stop 13
if (o1(2) - 3.0_16 /= e) error stop 14
if (any(o2 /= [x, va(1), va(2), va(3)])) error stop 15
if (o2(1) - 1.0_16 /= e .or. o2(2) - 1.0_16 /= e) error stop 16
if (any(o3 /= [va(3), va(4), 9.0_16, 9.0_16])) error stop 17
if (o3(1) - 3.0_16 /= e) error stop 18
if (any(o4 /= 0.0_16)) error stop 19
if (any(o5 /= [cmplx(e, y, 16), vz(1), vz(2)])) error stop 20
if (aimag(o5(1)) + 2.0_16 /= -2.0_16*e) error stop 21
if (any(o6 /= [w(2), w(3), (7.0_8, 8.0_8)])) error stop 22

print *, "ok"
end program

program real16_parameter_spread_01
! spread, unpack and pack of real(16) and complex(16) constants are folded
! exactly: the elements are copied without rounding to double precision
implicit none
! e = 2**(-60): 1 + e and 2*(1 + e) round to 1 and 2 in double precision
real(16), parameter :: e = 8.67361737988403547205962240695953369140625e-19_16
real(16), parameter :: x = 1.0_16 + e, y = -2.0_16*(1.0_16 + e)
real(16), parameter :: a(2) = [x, 3.0_16 + e]
complex(16), parameter :: z(2) = [(x, y), (y, x)]
logical, parameter :: m(3) = [.true., .false., .true.]

real(16), parameter :: s1(3) = spread(a(1), 1, 3)
real(16), parameter :: s2(2, 2) = spread(a, 1, 2)
real(16), parameter :: s3(2, 2) = spread(a, 2, 2)
complex(16), parameter :: s4(2, 2) = spread(z, 1, 2)
complex(16), parameter :: s5(2, 3) = spread(z, 2, 3)
real(16), parameter :: u1(3) = unpack(a, m, 0.0_16)
real(16), parameter :: u2(3) = unpack(a, [.false., .true., .true.], [e, y, x])
complex(16), parameter :: u3(3) = unpack(z, m, (0.0_16, 0.0_16))
real(16), parameter :: p1(2) = pack([x, 5.0_16, 3.0_16 + e], [.true., .false., .true.])
real(16), parameter :: p2(2) = pack([x, 5.0_16, 3.0_16 + e], m)
real(16), parameter :: p3(3) = pack(a, [.false., .true.], [y, x, e])
complex(16), parameter :: p4(1) = pack(z, [.false., .true.])
complex(16), parameter :: p5(2) = pack(z, .true.)

real(16) :: va(2)
complex(16) :: vz(2)
integer :: i

va = a
vz = z

if (.not. (a(1) > 1.0_16)) error stop 1
if (a(1) - 1.0_16 /= e) error stop 2

if (size(s1) /= 3) error stop 3
if (any(s1 /= va(1))) error stop 4
if (s1(2) - 1.0_16 /= e) error stop 5

if (any(shape(s2) /= [2, 2])) error stop 6
do i = 1, 2
    if (any(s2(:, i) /= va(i))) error stop 7
    if (any(s3(:, i) /= va)) error stop 8
    if (any(s4(:, i) /= vz(i))) error stop 9
end do
do i = 1, 3
    if (any(s5(:, i) /= vz)) error stop 10
end do
if (aimag(s4(2, 2)) - 1.0_16 /= e) error stop 11
if (real(s5(2, 3), 16) + 2.0_16 /= -2.0_16*e) error stop 12

if (any(u1 /= [va(1), 0.0_16, va(2)])) error stop 13
if (u1(3) - 3.0_16 /= e) error stop 14
if (any(u2 /= [e, va(1), va(2)])) error stop 15
if (u3(1) /= vz(1) .or. u3(2) /= (0.0_16, 0.0_16) .or. u3(3) /= vz(2)) error stop 16
if (aimag(u3(3)) - 1.0_16 /= e) error stop 17

if (any(p1 /= va)) error stop 18
if (any(p2 /= va)) error stop 19
if (p2(1) - 1.0_16 /= e) error stop 20
if (any(p3 /= [va(2), x, e])) error stop 21
if (p4(1) /= vz(2)) error stop 22
if (any(p5 /= vz)) error stop 23
if (real(p5(1), 16) - 1.0_16 /= e) error stop 24

print *, s1(1), u1(3), p2(1)
print *, p4(1)
end program

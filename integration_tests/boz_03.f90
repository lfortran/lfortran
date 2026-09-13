program boz_03
! A BOZ literal constant is typeless: it is an ordered sequence of bits. When
! it is an argument of DBLE, REAL or CMPLX those bits are used directly as the
! internal representation of the result, they are not converted from an integer
! value.
implicit none

real(4) :: r4
real(8) :: r8
complex(4) :: c4
complex(8) :: c8

! DBLE reinterprets the bits as a real(8)
r8 = dble(z'3ff0000000000000')
if (r8 /= 1.0_8) error stop
r8 = dble(z'c000000000000000')
if (r8 /= -2.0_8) error stop
r8 = dble(b'0011111111110000000000000000000000000000000000000000000000000000')
if (r8 /= 1.0_8) error stop
r8 = dble(o'400000000000000000000')
if (r8 /= 2.0_8) error stop
r8 = dble(z'ff')
if (r8 /= 255.0_8 * tiny(1.0_8) / 4503599627370496.0_8) error stop

! REAL reinterprets the bits at the kind it returns
r4 = real(z'3f800000')
if (r4 /= 1.0_4) error stop
r4 = real(z'bf800000', kind=4)
if (r4 /= -1.0_4) error stop
r4 = real(o'7770000000')
if (r4 /= 1.75_4) error stop
r4 = real(b'00111111100000000000000000000000')
if (r4 /= 1.0_4) error stop
r8 = real(z'3ff0000000000000', kind=8)
if (r8 /= 1.0_8) error stop
r8 = real(z'4010000000000000', 8)
if (r8 /= 4.0_8) error stop

! CMPLX reinterprets the bits of both parts
c4 = cmplx(z'3f800000', z'40000000')
if (c4 /= (1.0_4, 2.0_4)) error stop
c8 = cmplx(z'3ff0000000000000', z'c000000000000000', kind=8)
if (c8 /= (1.0_8, -2.0_8)) error stop
c4 = cmplx(z'3f800000')
if (c4 /= (1.0_4, 0.0_4)) error stop

! INT keeps using the bits as an integer value
if (int(z'ff') /= 255) error stop
if (int(b'01011101') /= 93) error stop
if (int(o'2347') /= 1255) error stop

print *, r4, r8, c4, c8
end program boz_03

program real16_parameter_norm2_01
! norm2 of real(16) constants is folded exactly, in binary128
implicit none
! e = 2**(-60): 3*(1+e) and 4*(1+e) round to 3 and 4 in double precision
real(16), parameter :: e = 8.67361737988403547205962240695953369140625e-19_16
real(16), parameter :: a(2) = [3.0_16*(1.0_16 + e), 4.0_16*(1.0_16 + e)]
real(16), parameter :: b(2, 2) = reshape([1.0_16, 2.0_16, 2.0_16, 4.0_16], [2, 2])
real(16), parameter :: c(2) = [1.0_16, 1.0_16]
real(16), parameter :: x1 = norm2(a)
real(16), parameter :: x2 = norm2(b)
real(16), parameter :: x3 = norm2(c)
real(16), parameter :: x4 = 2.0_16*norm2([0.6_16, 0.8_16]*(1.0_16 + e))
real(16) :: va(2), vb(2, 2), vc(2)

va = a
vb = b
vc = c

if (x1 /= 5.0_16*(1.0_16 + e)) error stop 1
if (.not. (x1 > 5.0_16)) error stop 2
if (x1 /= norm2(va)) error stop 3

if (x2 /= 5.0_16) error stop 4
if (x2 /= norm2(vb)) error stop 5

if (x3 /= sqrt(2.0_16)) error stop 6
if (x3 /= norm2(vc)) error stop 7
if (abs(x3 - 1.41421356237309504880168872420969808_16) > 1.0e-33_16) error stop 8

if (abs(x4 - 2.0_16*(1.0_16 + e)) > 1.0e-32_16) error stop 9
if (.not. (x4 > 2.0_16)) error stop 10

print *, x1, x2, x3, x4
end program

program intrinsics_503
! Test C/C++ backend lowering of elemental intrinsics:
! sqrt, log, log10, atan2, sign, mod, modulo, merge,
! floor, ceiling, nint, real, int.
real :: x, y, tol
real, parameter :: pi = 3.141592653589793
integer :: a, p
complex :: z

tol = 1.0e-6

! sqrt, log, log10 (runtime values to force codegen, not folding)
x = 16.0
if (abs(sqrt(x) - 4.0) > tol) error stop
x = 100.0
if (abs(log10(x) - 2.0) > tol) error stop
x = exp(1.0)
if (abs(log(x) - 1.0) > tol) error stop

! atan2
x = 1.0; y = 1.0
if (abs(atan2(y, x) - pi/4.0) > tol) error stop
x = 1.0; y = 0.0
if (abs(atan2(y, x) - 0.0) > tol) error stop

! sign: real
x = 5.0; y = -3.0
if (abs(sign(x, y) - (-5.0)) > tol) error stop
x = -5.0; y = 3.0
if (abs(sign(x, y) - 5.0) > tol) error stop
! sign: integer
a = 5; p = -3
if (sign(a, p) /= -5) error stop
a = -5; p = 3
if (sign(a, p) /= 5) error stop
a = -5; p = 0
if (sign(a, p) /= 5) error stop

! mod: real has sign of A, integer is C remainder
x = -5.0; y = 3.0
if (abs(mod(x, y) - (-2.0)) > tol) error stop
x = 5.0; y = -3.0
if (abs(mod(x, y) - 2.0) > tol) error stop
a = -5; p = 3
if (mod(a, p) /= -2) error stop
a = 5; p = -3
if (mod(a, p) /= 2) error stop

! modulo: real and integer take the sign of P
x = -5.0; y = 3.0
if (abs(modulo(x, y) - 1.0) > tol) error stop
x = 5.0; y = -3.0
if (abs(modulo(x, y) - (-1.0)) > tol) error stop
x = -5.0; y = -3.0
if (abs(modulo(x, y) - (-2.0)) > tol) error stop
x = 5.0; y = 3.0
if (abs(modulo(x, y) - 2.0) > tol) error stop
a = -5; p = 3
if (modulo(a, p) /= 1) error stop
a = 5; p = -3
if (modulo(a, p) /= -1) error stop
a = -5; p = -3
if (modulo(a, p) /= -2) error stop
a = 5; p = 3
if (modulo(a, p) /= 2) error stop
a = -6; p = 3
if (modulo(a, p) /= 0) error stop

! merge
if (merge(1, 2, .true.) /= 1) error stop
if (merge(1, 2, .false.) /= 2) error stop
x = 1.5; y = 2.5
if (abs(merge(x, y, .true.) - 1.5) > tol) error stop
if (abs(merge(x, y, .false.) - 2.5) > tol) error stop

! floor, ceiling, nint
x = 2.5
if (floor(x) /= 2) error stop
if (ceiling(x) /= 3) error stop
if (nint(x) /= 3) error stop
x = -2.5
if (floor(x) /= -3) error stop
if (ceiling(x) /= -2) error stop
if (nint(x) /= -3) error stop
x = 2.4
if (nint(x) /= 2) error stop
if (floor(x, kind=8) /= 2_8) error stop
if (ceiling(x, kind=8) /= 3_8) error stop
if (nint(x, kind=8) /= 2_8) error stop

! real, int (including complex argument)
a = 5
if (abs(real(a) - 5.0) > tol) error stop
x = 5.7
if (int(x) /= 5) error stop
x = -5.7
if (int(x) /= -5) error stop
z = (3.0, 4.0)
if (abs(real(z) - 3.0) > tol) error stop
if (int(z) /= 3) error stop

print *, "ok"
end program

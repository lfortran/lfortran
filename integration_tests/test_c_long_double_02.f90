program test_c_long_double_02
! c_long_double must be the kind of the C `long double` of the target
! (10 on x86_64 Linux, 8 on arm64 macOS and Windows, 16 on aarch64 Linux)
use iso_c_binding, only: c_int, c_long_double, c_long_double_complex, c_sizeof
use test_c_long_double_02b, only: ldk, wp, c_long_double_size
implicit none

interface
    real(c_long_double) function c_long_double_add(a, b) &
            bind(c, name="c_long_double_add")
    import :: c_long_double
    real(c_long_double), value :: a, b
    end function
end interface

real(c_long_double) :: x
real(ldk) :: y
real(wp) :: z

print *, c_long_double, c_long_double_complex, c_sizeof(x), c_long_double_size()

if (c_long_double < 0) error stop
if (c_long_double_complex /= c_long_double) error stop
if (c_sizeof(x) /= c_long_double_size()) error stop

! The kind seen through a module loaded from its modfile, renamed there or
! as a named constant defined there, is the same
if (ldk /= c_long_double) error stop
if (wp /= c_long_double) error stop
if (c_sizeof(y) /= c_long_double_size()) error stop
if (c_sizeof(z) /= c_long_double_size()) error stop

! A value passes to and from C by value
x = 1.5_c_long_double
y = c_long_double_add(x, 2.25_c_long_double)
if (y /= 3.75_c_long_double) error stop
end program

! The result of a function referenced through an external procedure pointer
! without a type is typed by the implicit typing rules, not by the
! procedures the pointer is associated with, so the pointer can be
! associated with procedures of other interfaces (#16414).
program procedure_pointer_46
pointer :: ifp, xfp, sp
external :: ifp, xfp, sp
integer :: v, w
ifp => isquare
if (ifp(7) /= 49) error stop 1
ifp => iadd
if (ifp(7, 8) /= 15) error stop 2
ifp => isquare
if (ifp(3) /= 9) error stop 3
xfp => half
if (abs(xfp(3.0) - 1.5) > 1e-6) error stop 4
xfp => scaled
if (abs(xfp(3.0, 2.0) - 6.0) > 1e-6) error stop 5
sp => setv
call sp(v)
if (v /= 42) error stop 6
sp => setvw
call sp(v, w)
if (v /= 1 .or. w /= 2) error stop 7
print *, "ok"
contains
integer function isquare(n)
    integer, intent(in) :: n
    isquare = n * n
end function
integer function iadd(a, b)
    integer, intent(in) :: a, b
    iadd = a + b
end function
real function half(x)
    real, intent(in) :: x
    half = x / 2
end function
real function scaled(x, s)
    real, intent(in) :: x, s
    scaled = x * s
end function
subroutine setv(v)
    integer, intent(out) :: v
    v = 42
end subroutine
subroutine setvw(v, w)
    integer, intent(out) :: v, w
    v = 1
    w = 2
end subroutine
end program

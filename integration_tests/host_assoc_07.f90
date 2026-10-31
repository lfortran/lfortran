! An internal or statement function uses a dummy procedure of its host
! (#12879), and an internal procedure uses a procedure pointer of its host
! (#15193)
module host_assoc_07_m
implicit none
abstract interface
    integer function fi(x)
        integer, intent(in) :: x
    end function
end interface
contains
integer function impl(x)
    integer, intent(in) :: x
    impl = x + 1
end function

integer function impl2(x)
    integer, intent(in) :: x
    impl2 = 2*x
end function

subroutine s()
    procedure(fi), pointer :: b
    b => impl
    if (b(1) /= 2) error stop 1
    call nested()
    if (b(3) /= 6) error stop 2
    if (call_b(5) /= 10) error stop 3
contains
    subroutine nested()
        procedure(fi), pointer :: c
        c => b
        if (c(1) /= 2) error stop 4
        b => impl2
    end subroutine
    integer function call_b(x)
        integer, intent(in) :: x
        call_b = b(x)
    end function
end subroutine
end module host_assoc_07_m

subroutine drive(f, r)
implicit none
procedure(real) :: f
real :: r
r = twice(3.0)
contains
real function twice(y)
    real :: y
    twice = 2*f(y)
end function
end subroutine

subroutine drive_stmt(f, r)
implicit none
procedure(real) :: f
real :: r, sf, y
sf(y) = 2*f(y)
r = sf(3.0)
end subroutine

subroutine drive_interface(f, r)
implicit none
interface
    real function f(x)
        real :: x
    end function
end interface
real :: r
r = thrice(2.0)
contains
real function thrice(y)
    real :: y
    thrice = 3*f(y)
end function
end subroutine

program host_assoc_07
use host_assoc_07_m
implicit none
real, external :: h
real :: r
call s()
call drive(h, r)
if (abs(r - 6.0) > 1e-5) error stop 5
call drive_stmt(h, r)
if (abs(r - 6.0) > 1e-5) error stop 6
call drive_interface(h, r)
if (abs(r - 6.0) > 1e-5) error stop 7
print *, "ok"
end program

real function h(x)
real :: x
h = x
end function

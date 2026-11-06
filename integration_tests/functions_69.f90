! The result of an array or derived-type function is evaluated completely
! before the variable it is assigned to is defined (F2018 10.2.1.3), also
! when the function reads that variable by use or host association, through
! a common block or in a procedure it calls (#16191).
module functions_69_m
implicit none
type t
    real :: w(3)
end type
real :: g(3), gv(3)
type(t) :: gt
contains

function f(x) result(r)
    real, intent(in) :: x
    real :: r(3)
    r(1) = x
    r(2) = g(1) + 10
    r(3) = g(1) + 20
end function

function fv() result(rr)
    real :: rr(3)
    rr(3:1:-1) = gv + 1
end function

function ft() result(r)
    type(t) :: r
    call fill(r)
end function

subroutine fill(d)
    type(t), intent(out) :: d
    d%w = gt%w(3:1:-1)
end subroutine

function fd() result(r)
    type(t) :: r
    r%w = gt%w(3:1:-1)
end function

function fc() result(r)
    real :: r(3)
    real :: c(3)
    common /functions_69_c/ c
    r(1) = 100
    r(2) = c(1) + 10
    r(3) = c(1) + 20
end function

subroutine host_dummy(y)
    real, intent(inout) :: y(3)
    y = fy(100.0)
contains
    function fy(x) result(r)
        real, intent(in) :: x
        real :: r(3)
        r(1) = x
        r(2) = y(1) + 10
        r(3) = y(1) + 20
    end function
end subroutine

end module

program functions_69
use functions_69_m
implicit none
real :: h(3), y(3), z(3), c(3)
type(t) :: lt
common /functions_69_c/ c

! A module variable that the function reads.
g = [1.0, 2.0, 3.0]
g = f(100.0)
print *, g
if (any(abs(g - [100.0, 11.0, 21.0]) > 1e-6)) error stop 1

gv = [1.0, 2.0, 3.0]
gv = fv()
print *, gv
if (any(abs(gv - [4.0, 3.0, 2.0]) > 1e-6)) error stop 2

! A derived-type module variable, read by a procedure the function calls.
gt%w = [1.0, 2.0, 3.0]
gt = ft()
print *, gt%w
if (any(abs(gt%w - [3.0, 2.0, 1.0]) > 1e-6)) error stop 3

gt%w = [1.0, 2.0, 3.0]
gt = fd()
print *, gt%w
if (any(abs(gt%w - [3.0, 2.0, 1.0]) > 1e-6)) error stop 4

! A variable in a common block.
c = [1.0, 2.0, 3.0]
c = fc()
print *, c
if (any(abs(c - [100.0, 11.0, 21.0]) > 1e-6)) error stop 5

! A host variable of an internal function.
h = [1.0, 2.0, 3.0]
h = fh(100.0)
print *, h
if (any(abs(h - [100.0, 11.0, 21.0]) > 1e-6)) error stop 6

! A host dummy argument of an internal function.
z = [1.0, 2.0, 3.0]
call host_dummy(z)
print *, z
if (any(abs(z - [100.0, 11.0, 21.0]) > 1e-6)) error stop 7

! Locals that no function can reach.
g = [1.0, 2.0, 3.0]
y = [5.0, 6.0, 7.0]
y = f(100.0)
print *, y
if (any(abs(y - [100.0, 11.0, 21.0]) > 1e-6)) error stop 8

gt%w = [1.0, 2.0, 3.0]
lt = fd()
print *, lt%w
if (any(abs(lt%w - [3.0, 2.0, 1.0]) > 1e-6)) error stop 9

contains

function fh(x) result(r)
    real, intent(in) :: x
    real :: r(3)
    r(1) = x
    r(2) = h(1) + 10
    r(3) = h(1) + 20
end function

end program

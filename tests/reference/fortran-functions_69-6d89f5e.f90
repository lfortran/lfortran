module functions_69_m
implicit none
type :: t
    real(4), dimension(3) :: w
end type t
real(4), dimension(3) :: g
type(t) :: gt
real(4), dimension(3) :: gv

contains

subroutine f(x, r)
    real(4), dimension(3), intent(out) :: r
    real(4), intent(in) :: x
    r(1) = x
    r(2) = g(1) + real(10, kind=4)
    r(3) = g(1) + real(20, kind=4)
end subroutine f

subroutine fc(r)
    real(4), dimension(3) :: c
    real(4), dimension(3), intent(out) :: r
    common /functions_69_c/ c
    r(1) = real(100, kind=4)
    r(2) = c(1) + real(10, kind=4)
    r(3) = c(1) + real(20, kind=4)
end subroutine fc

subroutine fd(r)
    type(t), intent(out) :: r
    r%w = gt%w(3:1:-1)
end subroutine fd

subroutine fill(d)
    type(t), intent(out) :: d
    d%w = gt%w(3:1:-1)
end subroutine fill

subroutine ft(r)
    type(t), intent(out) :: r
    call fill(r)
end subroutine ft

subroutine fv(rr)
    real(4), dimension(3), intent(out) :: rr
    rr(3:1:-1) = gv + real(1, kind=4)
end subroutine fv

subroutine host_dummy(y)
    real(4), dimension(3) :: __libasr_created__subroutine_from_function_
    real(4), dimension(3), intent(inout) :: y
    call fy(1.00000000e+02, __libasr_created__subroutine_from_function_)
    y = __libasr_created__subroutine_from_function_
    contains
    subroutine fy(x, r)
        real(4), dimension(3), intent(out) :: r
        real(4), intent(in) :: x
        r(1) = x
        r(2) = y(1) + real(10, kind=4)
        r(3) = y(1) + real(20, kind=4)
    end subroutine fy

end subroutine host_dummy

end module functions_69_m

program functions_69
use functions_69_m, only: f
use functions_69_m, only: fc
use functions_69_m, only: fd
use functions_69_m, only: fill
use functions_69_m, only: ft
use functions_69_m, only: fv
use functions_69_m, only: g
use functions_69_m, only: gt
use functions_69_m, only: gv
use functions_69_m, only: host_dummy
use functions_69_m, only: t
implicit none
real(4), dimension(3) :: __libasr_created__subroutine_from_function_
real(4), dimension(3) :: __libasr_created__subroutine_from_function_1
type(t) :: __libasr_created__subroutine_from_function_2
type(t) :: __libasr_created__subroutine_from_function_3
real(4), dimension(3) :: __libasr_created__subroutine_from_function_4
real(4), dimension(3) :: __libasr_created__subroutine_from_function_5
real(4), dimension(3) :: c
real(4), dimension(3) :: h
type(t) :: lt
real(4), dimension(3) :: y
real(4), dimension(3) :: z
common /functions_69_c/ c
g = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
call f(1.00000000e+02, __libasr_created__subroutine_from_function_)
g = __libasr_created__subroutine_from_function_
print *, g
if (Any(Abs(g - [1.00000000e+02, 1.10000000e+01, 2.10000000e+01]) > 9.99999997e-07)) then
    error stop
end if
gv = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
call fv(__libasr_created__subroutine_from_function_1)
gv = __libasr_created__subroutine_from_function_1
print *, gv
if (Any(Abs(gv - [4.00000000e+00, 3.00000000e+00, 2.00000000e+00]) > 9.99999997e-07)) then
    error stop
end if
gt%w = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
call ft(__libasr_created__subroutine_from_function_2)
gt = __libasr_created__subroutine_from_function_2
print *, gt%w
if (Any(Abs(gt%w - [3.00000000e+00, 2.00000000e+00, 1.00000000e+00]) > 9.99999997e-07)) then
    error stop
end if
gt%w = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
call fd(__libasr_created__subroutine_from_function_3)
gt = __libasr_created__subroutine_from_function_3
print *, gt%w
if (Any(Abs(gt%w - [3.00000000e+00, 2.00000000e+00, 1.00000000e+00]) > 9.99999997e-07)) then
    error stop
end if
c = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
call fc(__libasr_created__subroutine_from_function_4)
c = __libasr_created__subroutine_from_function_4
print *, c
if (Any(Abs(c - [1.00000000e+02, 1.10000000e+01, 2.10000000e+01]) > 9.99999997e-07)) then
    error stop
end if
h = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
call fh(1.00000000e+02, __libasr_created__subroutine_from_function_5)
h = __libasr_created__subroutine_from_function_5
print *, h
if (Any(Abs(h - [1.00000000e+02, 1.10000000e+01, 2.10000000e+01]) > 9.99999997e-07)) then
    error stop
end if
z = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
call host_dummy(z)
print *, z
if (Any(Abs(z - [1.00000000e+02, 1.10000000e+01, 2.10000000e+01]) > 9.99999997e-07)) then
    error stop
end if
g = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
y = [5.00000000e+00, 6.00000000e+00, 7.00000000e+00]
call f(1.00000000e+02, y)
print *, y
if (Any(Abs(y - [1.00000000e+02, 1.10000000e+01, 2.10000000e+01]) > 9.99999997e-07)) then
    error stop
end if
gt%w = [1.00000000e+00, 2.00000000e+00, 3.00000000e+00]
call fd(lt)
print *, lt%w
if (Any(Abs(lt%w - [3.00000000e+00, 2.00000000e+00, 1.00000000e+00]) > 9.99999997e-07)) then
    error stop
end if

contains

subroutine fh(x, r)
    real(4), dimension(3), intent(out) :: r
    real(4), intent(in) :: x
    r(1) = x
    r(2) = h(1) + real(10, kind=4)
    r(3) = h(1) + real(20, kind=4)
end subroutine fh

end program functions_69

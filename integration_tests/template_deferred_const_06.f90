! A deferred constant declared in a templated subprogram (F2028 R1618 in the
! specification part of a subroutine or function with deferred arguments,
! R1615). It is a deferred argument, so it belongs to the template of the
! subprogram: a templated subroutine and a templated function both accept it,
! and each instantiation computes with the value of its instantiation argument,
! in an expression, as the bound of a local array and as a logical or character
! constant (lfortran/lfortran#13360).

module template_deferred_const_06_m
    implicit none
contains

    template integer function add_n{n}(x) result(res)
        deferred integer, parameter :: n
        integer, intent(in) :: x
        res = x + n
    end function

    template subroutine add_sum{n}(x)
        deferred integer, parameter :: n
        integer, intent(inout) :: x
        integer :: buf(n)
        integer :: i
        do i = 1, n
            buf(i) = i
        end do
        x = x + sum(buf)
    end subroutine

    template integer function pick{b}(x, y) result(z)
        deferred logical, parameter :: b
        integer, intent(in) :: x, y
        if (b) then
            z = x
        else
            z = y
        end if
    end function

    template subroutine text_len{s}(k)
        deferred character(*), parameter :: s
        integer, intent(out) :: k
        k = len(s)
    end subroutine

end module

program template_deferred_const_06
use template_deferred_const_06_m
implicit none

integer, parameter :: three = 3, five = 5
logical, parameter :: yes = .true., no = .false.
character(*), parameter :: hello = "hello"
integer :: x

instantiate add_n {three}, only: add3 => add_n
instantiate add_sum {five}, only: add_sum5 => add_sum
instantiate pick {yes}, only: pick_first => pick
instantiate pick {no}, only: pick_second => pick
instantiate text_len {hello}, only: hello_len => text_len

if (add3(4) /= 7) error stop
if (add_n{five}(4) /= 9) error stop

x = 1
call add_sum5(x)
if (x /= 16) error stop
x = 1
call add_sum{three}(x)
if (x /= 7) error stop

if (pick_first(7, 9) /= 7) error stop
if (pick_second(7, 9) /= 9) error stop

call hello_len(x)
if (x /= 5) error stop

print *, "ok"

end program

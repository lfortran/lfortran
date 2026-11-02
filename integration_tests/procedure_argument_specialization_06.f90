! More cases of --fast copying a procedure into the host of the internal
! procedures passed to it: two dummy procedures at once, a procedure that is
! itself an internal procedure of the host, a procedure with ENTRY
! statements, and a procedure that passes its dummy on to another one.
module procedure_argument_specialization_06_m
implicit none
contains
subroutine combine(f, g, x, r)
    interface
        integer function f(y)
            integer, intent(in) :: y
        end function
        integer function g(y)
            integer, intent(in) :: y
        end function
    end interface
    integer, intent(in) :: x
    integer, intent(out) :: r
    r = f(g(x))
end subroutine

subroutine twice(f, x)
    interface
        subroutine f(y)
            integer, intent(inout) :: y
        end subroutine
    end interface
    integer, intent(inout) :: x
    call f(x)
    call f(x)
    return
entry thrice(f, x)
    call f(x)
    call f(x)
    call f(x)
end subroutine

subroutine outer(f, x)
    interface
        subroutine f(y)
            integer, intent(inout) :: y
        end subroutine
    end interface
    integer, intent(inout) :: x
    call twice(f, x)
    call f(x)
end subroutine
end module

program procedure_argument_specialization_06
use procedure_argument_specialization_06_m
implicit none
integer :: a, b, r, x
a = 3
b = 10
call combine(plus_a, times_b, 2, r)
if (r /= 23) error stop 1
call combine(times_b, plus_a, 2, r)
if (r /= 50) error stop 2
x = 0
call twice(inc_a, x)
if (x /= 6) error stop 3
call thrice(inc_a, x)
if (x /= 15) error stop 4
call outer(inc_a, x)
if (x /= 24) error stop 5
if (local_sum(plus_a, 4) /= 4*5 + 4*a) error stop 6
print *, r, x
contains
integer function plus_a(y)
    integer, intent(in) :: y
    plus_a = y + a
end function

integer function times_b(y)
    integer, intent(in) :: y
    times_b = y*b
end function

subroutine inc_a(y)
    integer, intent(inout) :: y
    y = y + a
end subroutine

integer function local_sum(f, n) result(s)
    interface
        integer function f(y)
            integer, intent(in) :: y
        end function
    end interface
    integer, intent(in) :: n
    integer :: i
    s = 0
    do i = 1, n
        s = s + f(i) + i
    end do
end function
end program

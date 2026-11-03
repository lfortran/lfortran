! With --fast (#16665): an internal procedure `c` of the copied `a` takes a
! pair of its own (the copy of `c` is made before the calls in the copy of
! `a` pass frames) and calls the host's pair `x` too; and a dummy procedure
! that is called in a declaration is not paired, so `d` keeps passing its
! internal procedure through a thunk (a shallow recursion).
module frame_specialization_05_m
implicit none
contains
    recursive integer function a(n, x) result(r)
        integer, intent(in) :: n
        interface
            integer function x(i)
                integer, intent(in) :: i
            end function
        end interface
        if (n == 0) then
            r = c(x)
        else
            r = a(n - 1, b)
        end if
    contains
        integer function b(i)
            integer, intent(in) :: i
            b = x(i) + n
        end function
        integer function c(y)
            interface
                integer function y(i)
                    integer, intent(in) :: i
                end function
            end interface
            c = y(1) + x(2)
        end function
    end function

    recursive integer function d(n, x) result(r)
        integer, intent(in) :: n
        interface
            pure integer function x(i)
                integer, intent(in) :: i
            end function
        end interface
        integer :: tmp(x(1))
        tmp = 1
        if (n == 0) then
            r = x(1) + size(tmp)
        else
            r = d(n - 1, e) + size(tmp)
        end if
    contains
        pure integer function e(i)
            integer, intent(in) :: i
            e = x(i) + n
        end function
    end function

    pure integer function base(i)
        integer, intent(in) :: i
        base = i
    end function
end module

program frame_specialization_05
use frame_specialization_05_m
implicit none
integer :: r
! 1 + 2 + 2*(1 + ... + 20)
r = a(20, base)
if (r /= 423) error stop 1
r = d(4, base)
print *, r
if (r /= 46) error stop 2
end program

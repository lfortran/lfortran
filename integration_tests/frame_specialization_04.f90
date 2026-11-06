! With --fast (#16665), the copies that take dummy procedures with the frames
! of their hosts also handle: a dummy declared with the interface of a module
! procedure (`procedure(base)`), a pair passed on to an internal procedure of
! the copied procedure, a call in a BLOCK construct, a subroutine dummy, and
! module procedures passed from an internal procedure of the main program.
! Each recursion is deeper than the default --internal-procedure-thunks.
module frame_specialization_04_m
implicit none
contains
    recursive integer function a(n, x) result(r)
        integer, intent(in) :: n
        procedure(base) :: x
        if (n == 0) then
            r = c(x)
        else
            block
                integer :: q
                q = n
                r = a(n - 1, b) + q
            end block
        end if
    contains
        integer function b(i)
            integer, intent(in) :: i
            b = x(i) + n
        end function
        integer function c(y)
            procedure(base) :: y
            c = y(1)
        end function
    end function

    recursive subroutine walk(n, s, acc)
        integer, intent(in) :: n
        integer, intent(inout) :: acc
        interface
            subroutine s(v, k)
                integer, intent(inout) :: v
                integer, intent(in) :: k
            end subroutine
        end interface
        integer :: v
        v = n
        if (n == 0) then
            call s(v, 3)
            acc = acc + v
        else
            call walk(n - 1, inner, acc)
        end if
    contains
        subroutine inner(v, k)
            integer, intent(inout) :: v
            integer, intent(in) :: k
            call s(v, k + n)
            v = v + n
        end subroutine
    end subroutine

    integer function base(i)
        integer, intent(in) :: i
        base = i
    end function

    subroutine leaf(v, k)
        integer, intent(inout) :: v
        integer, intent(in) :: k
        v = v + k
    end subroutine
end module

program frame_specialization_04
use frame_specialization_04_m
implicit none
integer :: acc
! 1 + (1 + ... + 20) for b, plus the q of each level
if (a(20, base) /= 421) error stop 1
acc = 0
call walk(20, leaf, acc)
if (acc /= 423) error stop 2
call driver()
print *, acc
contains
    subroutine driver()
        integer :: acc2
        if (a(12, base) /= 157) error stop 3
        acc2 = 0
        call walk(12, leaf, acc2)
        if (acc2 /= 159) error stop 4
    end subroutine
end program

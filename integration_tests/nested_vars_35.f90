! An internal procedure calls a dummy procedure of its host whose interface
! has an assumed-length character dummy, and the host passes the internal
! procedure on in a recursive call.
module nested_vars_35_m
implicit none
contains
    recursive subroutine walk(n, s, acc)
        integer, intent(in) :: n
        integer, intent(inout) :: acc
        interface
            subroutine s(name, k)
                character(len=*), intent(in) :: name
                integer, intent(inout) :: k
            end subroutine
        end interface
        integer :: k
        k = 0
        if (n == 0) then
            call s("end", k)
            acc = acc + k
        else
            call walk(n - 1, inner, acc)
        end if
    contains
        subroutine inner(name, k)
            character(len=*), intent(in) :: name
            integer, intent(inout) :: k
            call s(name // "x", k)
            k = k + n
        end subroutine
    end subroutine

    subroutine leaf(name, k)
        character(len=*), intent(in) :: name
        integer, intent(inout) :: k
        k = k + len(name)
    end subroutine
end module

program nested_vars_35
use nested_vars_35_m
implicit none
integer :: acc
acc = 0
call walk(3, leaf, acc)
if (acc /= 12) error stop 1
print *, acc
end program

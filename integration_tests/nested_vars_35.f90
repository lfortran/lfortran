! An internal procedure calls a dummy procedure and a procedure pointer of
! its host whose interface has an assumed-length character dummy. The host
! passes the internal procedure on in a recursive call, which checks that
! each call reaches the entities of its own host.
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
        procedure(leaf), pointer :: q
        integer :: k
        k = 0
        q => s
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
            call q("", k)
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
if (acc /= 27) error stop 1
print *, acc
end program

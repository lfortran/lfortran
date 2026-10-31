! Internal procedures of an internal procedure (an extension): a variable of
! the middle procedure that its own internal procedures modify, a variable of
! the outer host, and calls between the levels (#14868)
program host_assoc_06
implicit none
integer :: top
top = 100
call outer()
if (top /= 111) error stop 1
call deep(3)
print *, "ok"
contains
    subroutine outer()
        integer :: a
        a = 0
        call inner1()
        call inner2()
        if (a /= 3) error stop 2
        if (top /= 111) error stop 3
    contains
        subroutine inner1()
            a = a + 1
            top = top + 1
        end subroutine inner1

        subroutine inner2()
            a = a + 2
            call bump_top()
        end subroutine inner2
    end subroutine outer

    subroutine bump_top()
        top = top + 10
    end subroutine bump_top

    recursive subroutine deep(n)
        integer, intent(in) :: n
        integer :: here
        here = n
        call middle()
        if (here /= 10*n) error stop 4
    contains
        subroutine middle()
            integer :: m
            m = here
            call leaf()
            if (m /= n + 1) error stop 5
        contains
            subroutine leaf()
                if (here > 1) call deep(here - 1)
                if (here /= n) error stop 6
                m = m + 1
                call set_here()
            end subroutine leaf
        end subroutine middle
        subroutine set_here()
            here = 10*n
        end subroutine set_here
    end subroutine deep
end program host_assoc_06

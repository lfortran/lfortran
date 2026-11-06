! With --mangle-underscore-external, global procedures are defined under the
! same names by which other units reference them (#14164)
program mangle_underscore_external_02
    implicit none
    interface
        subroutine mue02_add(k, n)
            integer, intent(inout) :: k
            integer, intent(in) :: n
        end subroutine
        integer function mue02_twice(n)
            integer, intent(in) :: n
        end function
        subroutine mue02_call_back(k)
            integer, intent(inout) :: k
        end subroutine
    end interface
    integer :: k
    k = 1
    call mue02_add(k, 4)
    if (k /= 5) error stop
    k = mue02_twice(k)
    if (k /= 10) error stop
    call mue02_call_back(k)
    if (k /= 17) error stop
    print *, k
end program

! Called from the other file
subroutine mue02_seven(k)
    integer, intent(inout) :: k
    k = k + 7
end subroutine

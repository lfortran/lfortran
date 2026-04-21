program interface_33
    implicit none
    integer :: result

    interface
        subroutine check_char(c, n, result)
            character, intent(in) :: c
            integer, intent(in) :: n
            integer, intent(out) :: result
        end subroutine
    end interface

    call check_char('L', 3, result)
    if (result /= 0) error stop
    print *, "ok"
end program

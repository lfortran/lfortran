! Saved complex locals of a procedure compiled separately from the main
! program keep their initial values (#15096)
program save_27
    implicit none
    interface
        subroutine save_27_sub(n)
            integer, intent(in) :: n
        end subroutine
    end interface
    integer :: i
    do i = 1, 3
        call save_27_sub(i)
    end do
end program

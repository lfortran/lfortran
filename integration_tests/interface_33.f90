! Test that scalar character arguments to external interface subroutines
! use the standard Fortran calling convention (char pointer + hidden length).
program interface_33
    implicit none
    integer :: info

    interface
        subroutine check_char_arg(uplo, n, info)
            character, intent(in) :: uplo
            integer, intent(in) :: n
            integer, intent(out) :: info
        end subroutine

        subroutine check_two_chars(side, uplo, m, info)
            character, intent(in) :: side
            character, intent(in) :: uplo
            integer, intent(in) :: m
            integer, intent(out) :: info
        end subroutine
    end interface

    ! Test single character argument
    call check_char_arg('L', 3, info)
    if (info /= 0) error stop "check_char_arg failed"
    print *, "check_char_arg: OK"

    ! Test two character arguments (verifies hidden lengths are ordered correctly)
    call check_two_chars('R', 'U', 5, info)
    if (info /= 0) error stop "check_two_chars failed"
    print *, "check_two_chars: OK"
end program

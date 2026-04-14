! External subroutine with character arguments for testing the
! standard Fortran character ABI (i8* + hidden length).
subroutine check_char_arg(uplo, n, info)
    implicit none
    character, intent(in) :: uplo
    integer, intent(in) :: n
    integer, intent(out) :: info

    if (uplo == 'L' .and. n == 3) then
        info = 0
    else
        info = -1
    end if
end subroutine

subroutine check_two_chars(side, uplo, m, info)
    implicit none
    character, intent(in) :: side
    character, intent(in) :: uplo
    integer, intent(in) :: m
    integer, intent(out) :: info

    if (side == 'R' .and. uplo == 'U' .and. m == 5) then
        info = 0
    else
        info = -1
    end if
end subroutine

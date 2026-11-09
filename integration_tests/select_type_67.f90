! select type on an assumed-rank class(*) dummy.
! Fortran 2018 and 2023 disallow an assumed-rank name as a select type
! selector, so GFortran and Flang reject this program. LFortran accepts it:
! the associate name stays assumed-rank, and len() reads the character length.
program select_type_67
    implicit none

    character(len=5) :: s
    integer :: n

    s = "hello"
    n = 1
    if (get_char_len(s) /= 5) error stop 1
    if (get_char_len(n) /= -1) error stop 2

contains

    integer function get_char_len(obj) result(length)
        class(*), intent(in) :: obj(..)

        select type (obj)
        type is (character(*))
            length = len(obj)
        class default
            length = -1
        end select
    end function get_char_len

end program

program bindc_52
    use iso_c_binding, only: c_char, c_null_char
    implicit none

    character(len=1, kind=c_char) :: msg(6)

    call copy_message_c(msg)
    if (msg(1) /= 'h') error stop
    if (msg(2) /= 'e') error stop
    if (msg(3) /= 'l') error stop
    if (msg(4) /= 'l') error stop
    if (msg(5) /= 'o') error stop
    if (msg(6) /= c_null_char) error stop

contains

    subroutine copy_message_c(msg) bind(c)
        character(len=1, kind=c_char), intent(inout) :: msg(*)
        character(:), allocatable :: str
        integer :: i

        str = message()
        do i = 1, len_trim(str)
            msg(i) = str(i:i)
        end do
        msg(len_trim(str) + 1) = c_null_char
    end subroutine

    function message() result(str)
        character(:), allocatable :: str
        str = 'hello'
    end function

end program

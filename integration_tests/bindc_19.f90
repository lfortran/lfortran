! Test: automatic-size character array with trim() in dimension expression
! Verifies that when a function returns an automatic-size character array
! whose dimension depends on trim(), the temporary buffer is allocated
! with the correct size (not evaluated from uninitialized return slots).
module bindc_19_mod
    use iso_c_binding, only: c_char, c_null_char
    implicit none

contains

    function to_c_char(value) result(cstr)
        character(len=*), intent(in) :: value
        character(kind=c_char) :: cstr(len(value)+1)
        integer :: i
        do i = 1, len(value)
            cstr(i) = value(i:i)
        end do
        cstr(len(value)+1) = c_null_char
    end function

    subroutine use_c_char(cstr, expected_len)
        character(kind=c_char), intent(in) :: cstr(*)
        integer, intent(in) :: expected_len
        integer :: i
        do i = 1, expected_len
            if (cstr(i) == c_null_char) error stop
        end do
        if (cstr(expected_len + 1) /= c_null_char) error stop
    end subroutine

    subroutine do_check(path)
        character(len=*), intent(in) :: path
        character(:), allocatable :: msg
        call use_c_char(to_c_char(trim(path)), len_trim(path))
        msg = "x"
        if (len(msg) /= 1) error stop
        if (msg /= "x") error stop
    end subroutine

end module

program bindc_19
    use bindc_19_mod
    implicit none
    call do_check("rand_nonexistent_path")
    call do_check("short")
    call do_check("a much longer test string for safety")
    print *, "ok"
end program

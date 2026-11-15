subroutine array_constant_char_assumed_len_01_set(x)
    character(len=*), intent(inout) :: x(2)
    x = ["XXX", "YYY"]
end subroutine

program array_constant_char_assumed_len_01
    ! A constant array assigned to an explicit-shape character(len=*) dummy
    implicit none
    interface
        subroutine array_constant_char_assumed_len_01_set(x)
            character(len=*), intent(inout) :: x(2)
        end subroutine
    end interface
    character(len=3) :: c(2)
    character(len=5) :: wide(2)
    character(len=2) :: narrow(2)

    c = "aaa"
    call array_constant_char_assumed_len_01_set(c)
    if (c(1) /= "XXX" .or. c(2) /= "YYY") error stop 1

    wide = "zzzzz"
    call array_constant_char_assumed_len_01_set(wide)
    if (wide(1) /= "XXX" .or. wide(2) /= "YYY") error stop 2
    if (wide(1)(4:5) /= "  ") error stop 3

    narrow = "zz"
    call array_constant_char_assumed_len_01_set(narrow)
    if (narrow(1) /= "XX" .or. narrow(2) /= "YY") error stop 4

    call fill(wide)
    if (wide(1) /= "ab" .or. wide(2) /= "wx") error stop 5
    print *, "OK"
contains
    subroutine fill(y)
        character(len=*), intent(out) :: y(2)
        y = [character(len=2) :: "abcd", "wxyz"]
    end subroutine
end program

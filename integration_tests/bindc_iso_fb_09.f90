program bindc_iso_fb_09
    use iso_c_binding, only: c_int, c_char
    implicit none
    interface
        function c_elem_len(s) bind(c) result(n)
            import :: c_int
            character(len=*), intent(in) :: s(:)
            integer(c_int) :: n
        end function
        function c_byte_stride(s) bind(c) result(n)
            import :: c_int
            character(len=*), intent(in) :: s(:)
            integer(c_int) :: n
        end function
        function c_first_char(s, i) bind(c) result(c)
            import :: c_int, c_char
            character(len=*), intent(in) :: s(:)
            integer(c_int), value :: i
            character(kind=c_char) :: c
        end function
    end interface
    character(len=5) :: a(4)
    a = ["ab001", "cd002", "ef003", "gh004"]

    if (c_elem_len(a) /= 5) error stop 1
    if (c_byte_stride(a) /= 5) error stop 2
    if (c_first_char(a, 3) /= "g") error stop 3

    if (c_elem_len(a(2:4:2)) /= 5) error stop 4
    if (c_byte_stride(a(2:4:2)) /= 10) error stop 5
    if (c_first_char(a(2:4:2), 1) /= "g") error stop 6

    call check_assumed_len(a)
    print *, "ok"
contains
    subroutine check_assumed_len(x)
        character(len=*), intent(in) :: x(:)
        if (c_elem_len(x) /= 5) error stop 7
        if (c_byte_stride(x) /= 5) error stop 8
        if (c_first_char(x, 2) /= "e") error stop 9
    end subroutine
end program

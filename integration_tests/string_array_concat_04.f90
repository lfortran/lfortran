module string_array_concat_04_mod
    implicit none
contains
    function fm(x) result(y)
        character(len=*), intent(in) :: x(:)
        character(len=len(x)+1) :: y(size(x))
        y = x
    end function
end module

program string_array_concat_04
    use string_array_concat_04_mod
    implicit none
    call sub(["abc", "def", "ghi"])
contains
    subroutine sub(x)
        character(len=*), intent(in) :: x(:)
        call chk(fm(x(2:3)) // "'")
    end subroutine
    subroutine chk(c)
        character(len=*), intent(in) :: c(:)
        if (len(c) /= 5 .or. c(2) /= "ghi '") error stop
    end subroutine
end program

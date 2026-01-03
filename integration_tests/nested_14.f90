module nested_14_mod 
contains 
    subroutine temp()
        integer :: col_width
        character(len=5) :: x(2)
        col_width = 5
        call temp_nested(x)
        if (x(1) /= "hello" .or. x(2) /= "world") error stop
    contains 
        subroutine temp_nested(basename)
            character(len=*), intent(inout) :: basename(:)
            character(len=col_width) :: tmp(2)
            tmp(1) = "hello"
            tmp(2) = "world"
            basename = tmp
        end subroutine
    end subroutine
end module

program nested_14
    use nested_14_mod
    call temp()
end program nested_14
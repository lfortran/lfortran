program unallocated_string_arg
    character(:), allocatable :: y

    call ss2("hello")
    call ss2(y)

contains

    subroutine ss2(x)
        character(5) :: x
        print *, x
    end subroutine

end program

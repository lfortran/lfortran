program T

    character(:), allocatable :: y
    call ss2("hello")
    call ss2(y)  ! should give runtime error

contains
    subroutine ss2(x)
    character(5) :: x
    print *, x
    end subroutine ss2
end

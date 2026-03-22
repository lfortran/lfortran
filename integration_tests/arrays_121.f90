program arrays_121
    implicit none
    integer :: x = 1
    call sub(x)
    if (x /= 1) error stop
    print *, x
    contains

    subroutine sub(inp)
        integer, dimension(..) :: inp
    end subroutine

end program arrays_121

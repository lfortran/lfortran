program expr_15
    implicit none

    integer :: x

    x = 25
    print *, x
    if (x /= 25) error stop

    call abc(x)

    print *, x
    if (x /= 26) error stop

    contains

    subroutine abc(p)
        integer, intent(inout) :: p
        print *, p
        p = p + 1
        print *, p
    end subroutine
end program

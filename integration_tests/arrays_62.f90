program arrays_62
    real :: xpt(1, 1)
    xpt = 25.0
    call rescue(xpt)
    print *, xpt
    if (any(abs(xpt - 625.0) > 1e-7)) error stop
    contains
    subroutine rescue(xpt)
        real, intent(inout) :: xpt(1, 1)
        print *, sum(xpt**2)
        xpt = sum(xpt**2)
    end subroutine
end program

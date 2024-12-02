program arrays_61
    real(4) :: xpt(2, 2)
    xpt = 25.0
    call rescue(xpt)
    print *, sum(xpt)
    if (abs(sum(xpt) - 120.710678) > 1e-8) error stop
    contains
    subroutine rescue(xpt)
        real(4), intent(inout) :: xpt(:, :)
        print *, sqrt(sum(xpt**2, dim=1))
        xpt(1, :) = sqrt(sum(xpt**2, dim=1))
    end subroutine
end program

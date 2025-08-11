program common_14
    implicit none
    real :: x, y

    common /coords/ x, y

    x = 5.0
    y = 10.0

    call show_coords
contains
    subroutine show_coords
        implicit none
        real :: x, y
        common /coords/ x, y
        print *, "x =", x, ", y =", y
        if ( abs(x - 5.0) > 1e-8 ) error stop
        if ( abs(y - 10.0) > 1e-8 ) error stop
    end subroutine show_coords
end program common_14

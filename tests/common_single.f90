program common_single
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
    end subroutine show_coords
end program common_single

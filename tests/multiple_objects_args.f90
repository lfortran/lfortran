program main
    implicit none
    
    type :: Circle
        real :: radius = 1.0
    end type Circle
    
    call print_radius(Circle(1.5), Circle())

contains

    subroutine print_radius(m, n)
        type(Circle), intent(in) :: m, n
        print *, m%radius, n%radius
    end subroutine

end program main

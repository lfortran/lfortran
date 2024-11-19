program main
    implicit none
    
    type :: Circle
        real :: radius = 1.0
    end type Circle
    
    logical :: was_called = .false.
    
    call print_radius(Circle(1.5), Circle())

    if (.not. was_called) then
        error stop 
    end if

contains

    subroutine print_radius(m, n)
        type(Circle), intent(in) :: m, n
        print *, m%radius, n%radius
        was_called = .true.
    end subroutine

end program main

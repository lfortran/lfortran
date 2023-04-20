program main

    implicit none
    real(4), allocatable :: x(:)
    
    allocate(x(5))
    x = [1.0, 2.0, 3.0, 4.0, 5.0]

    if (.not.allocated(x)) error stop

end program main

program main

    implicit none
    real(4), allocatable :: x(:)

    allocate(x(10))
    
    if (.not.allocated(x)) error stop 

end program main

program main

    implicit none
    real(4), allocatable :: x(:)

    allocate(x(10))
    
    if (allocated(x)) then
        print *, "x is allocated"
    else 
        print *, "x is not allocated"
    end if

end program main

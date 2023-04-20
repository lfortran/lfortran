program main

    implicit none
    real(4), allocatable :: x(:)
    
    allocate(x(5))
    x = [1.0, 2.0, 3.0, 4.0, 5.0]

    if (allocated(x)) then
        print *, "x is allocated"
    else 
        print *, "x is not allocated"
    end if

end program main

program main

    implicit none
    real(4), allocatable :: x(:)


    print *, allocated(x)
    if (allocated(x)) error stop

end program main

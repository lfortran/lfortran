program scalar_allocation_check_04
    integer, allocatable :: xx
    call ff(xx)

    contains 
        subroutine ff(x)
            integer :: x
            x = 10
        end subroutine
end program

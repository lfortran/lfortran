program test_random
    implicit none
    real :: u
    call Log10f(u)
    
    contains
    
        subroutine Log10f(u)
        real :: u
        u = 0.5
        print*, log10(u)
        end subroutine
    
    end program
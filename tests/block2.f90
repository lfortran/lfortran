subroutine hybrd()
    implicit none
    real :: ratio, delta

    interface 
        subroutine fcn(n)
        integer, intent(in) :: n
        end subroutine fcn
    end interface

    main : block
        delta = abs(ratio)
    end block main

end subroutine

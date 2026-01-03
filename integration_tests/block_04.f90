subroutine hybrd()
    implicit none
    real, dimension(3):: wa3
    real :: temp

    interface
        subroutine fcn(n)
        implicit none
        integer, intent(in) :: n
        end subroutine fcn
    end interface

    main : block
        temp = enorm(3, Wa3)
    end block main

    contains
    real function enorm(n, x) result(y)
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: x
        y = 0.0
    end function enorm

end subroutine

program main
    implicit none
    call hybrd()
end program

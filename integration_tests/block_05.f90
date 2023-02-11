subroutine hybrd()
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

end subroutine

program main
    call hybrd()
end program

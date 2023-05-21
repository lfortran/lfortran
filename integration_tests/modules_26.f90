module modern_minpack
    implicit none
contains
    subroutine fdjac1(fcn1)
        implicit none
        interface
            subroutine fcn1(n1, x)
                implicit none
                integer, intent(in) :: n1
                real, intent(in) :: x(n1)
            end subroutine fcn1
        end interface

    end subroutine

    subroutine fdjac2(fcn, m)

        implicit none
        integer, intent(in) :: m
        interface
            subroutine fcn(m)

                implicit none
                integer, intent(in) :: m
            end subroutine
        end interface
        call fcn(m)

    end subroutine fdjac2


end module modern_minpack

program main
use modern_minpack
implicit none

call fdjac1(fcn)
call fdjac2(fcn2, 1)

contains
subroutine fcn(n, x)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: x(n)
end subroutine fcn

subroutine fcn2(m)
    implicit none
    integer, intent(in) :: m
end subroutine fcn2

end program

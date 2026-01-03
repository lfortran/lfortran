program subroutines_19
    implicit none
    integer :: x = 1
    call f(x)

    contains

    subroutine f(a)
        integer :: a 
        intent(in) :: a 
        print * , a
        if (a /= 1) error stop
    end subroutine
end program

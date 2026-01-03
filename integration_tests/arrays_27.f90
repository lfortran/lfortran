program arrays_27
    implicit none
    integer :: x(3) = [1, 2, 3]
    integer :: m = 3
    call f(3, x)
    call f(m, x)

    contains
        subroutine f(n, x)
            integer, intent(in) :: x(n)
            integer ::  n
            if (n /= 3) error stop
            if (x(1) /= 1) error stop
            if (x(3) /= 3) error stop
        end subroutine

end program

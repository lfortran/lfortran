program arrays_56
    implicit none
    integer, parameter :: N = 10
    real :: x(N), y(N)
    x = 3
    y = 4
    call f(g(g(x(:4)-y(:4))**2), 3.0)

    contains

    subroutine f(A, correct)
        real, intent(in) :: A(:)
        real, intent(in) :: correct
        print *, A
        if( any(A /= correct) ) error stop
    end subroutine

    function g(A) result(r)
        real, intent(in) :: A(:)
        real :: r(size(A))
        r = A+2
    end function

end program

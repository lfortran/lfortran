program arrays_55
    implicit none
    integer, parameter :: N = 10
    real :: x(N), y(3)
    x = 3
    y = g(x(:3))+1
    if( any(y /= 6.00) ) error stop
    print *, y

    contains

    function g(A) result(r)
        real, intent(in) :: A(:)
        real :: r(size(A))
        r = A+2
        print *, "g(A) called"
    end function
end program

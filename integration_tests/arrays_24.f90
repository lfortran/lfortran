program arrays_24
    implicit none

    real :: x(1), y(1), z(1)
    x = [5.15]
    y = [-3.20]

    z = f(x, y)
    print *, z

    if (abs(z(1) - 1.95) > 1e-5) error stop

    contains

    function f(a, b) result(r)
        real, intent(in) :: a(:), b(:)
        real :: r(size(a))
        integer :: i
        do i = 1, size(a)
            r(i) = a(i) + b(i)
        end do
    end function
end program
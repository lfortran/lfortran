program main
    implicit none

    integer :: pi, radius, area
    real :: pi2, radius2, area2
    
    pi = 3
    radius = 5
    area = pi * sqr(radius)
    print *, area

    pi2 = 3.14
    radius2 = 5.0
    area2 = pi2 * radius2 * radius2
    print *, area2

    print *, "radius of the circle is", radius2, radius
    print *, "and its area is", area2, "cm^2"

    contains

    function sqr(x) result(r)
        integer, intent(in) :: x
        integer :: r
        r = x * x
    end function

end program

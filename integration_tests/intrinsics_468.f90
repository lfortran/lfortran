program main
    implicit none

    integer, dimension(2, 2) :: a

    a = reshape((/ 1, 2, 3, 4 /), shape(a))
    if (any(product(a, dim=2) /= (/ 3, 8 /))) error stop
end program

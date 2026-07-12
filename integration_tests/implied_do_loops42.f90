program implied_do_loops42
    implicit none

    integer :: i
    integer, parameter :: a(3, 4, 5) = reshape([(i, i = 1, size(a))], shape(a))
    integer :: b(3, 4, 5), c(3, 5)

    b = a
    c = product(b, 2)

    if (c(1, 1) /= 280) error stop 1
    if (c(3, 5) /= 9418680) error stop 2
end program implied_do_loops42

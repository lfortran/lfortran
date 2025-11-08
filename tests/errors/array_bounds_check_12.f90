program array_bounds_check_12
    integer, allocatable :: a(:)
    integer, allocatable :: e(:)
    allocate(a(3))
    allocate(e(3))

    a = 3
    e = f(a)

    print *, e

    contains
        function f(x) result(y)
            integer, intent(in) :: x(:)
            integer :: y(size(x) - 1)
            y = 2
        end function
end program

program array_bounds_check_09
    integer, allocatable :: a(:)
    integer, allocatable :: b(:)
    integer, allocatable :: e(:)
    allocate(a(3))
    allocate(b(3))
    allocate(e(3))

    a = 3
    b = 3

    e = a + b + f(a)

    print *, e

    contains
        function f(x) result(y)
            integer, intent(in) :: x(:)
            integer, allocatable :: y(:)
            allocate(y(size(x) - 1))
            y = 2
        end function
end program

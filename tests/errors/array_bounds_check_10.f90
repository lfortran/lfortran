program array_bounds_check_10
    integer, allocatable :: a(:, :)
    integer, allocatable :: b(:, :)
    integer, allocatable :: e(:, :)
    allocate(a(2, 2))
    allocate(b(2, 3))
    allocate(e(2, 2))

    e = a + b
end program

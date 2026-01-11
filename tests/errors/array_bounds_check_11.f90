program array_bounds_check_11
    integer, allocatable :: a(:, :)
    integer, allocatable :: b(:, :)
    integer, allocatable :: e(:, :)
    allocate(a(2, 2))
    allocate(b(2, 2))
    allocate(e(4, 1))

    e = a + b
end program

program array_section_06
    integer, allocatable :: arr(:, :)
    allocate(arr(2, 2))
    arr = reshape([1, 2, 3, 4], [2, 2])

    if (size(arr(4:2, 1)) /= 0) error stop
end program

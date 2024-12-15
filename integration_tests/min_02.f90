program min_02
    integer(8) :: y
    integer :: z
    print *, min(y, z)
    print *, "kind of min(y,z):", kind(min(y,z))
    if (kind(min(y,z)) /= 8) error stop "Incorrect kind for min"
end program
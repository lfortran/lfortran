program max_02
    integer(8) :: y
    integer :: z
    print *, max(y, z)
    print *, "Kind of max(y, z):", kind(max(y,z))
    if (kind(max(y,z)) /= 8) error stop "Incorrect kind of max"
end program
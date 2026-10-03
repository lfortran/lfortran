program read_43
    implicit none
    character(len=:), allocatable :: raw_line
    real(8), allocatable :: vals(:)

    raw_line = '1.D0, 2.D0, 4.D0'
    allocate(vals(3))
    vals = -1D0

    read(raw_line, *) vals

    if (abs(vals(1) - 1D0) > 1D-12) error stop 'vals(1) mismatch'
    if (abs(vals(2) - 2D0) > 1D-12) error stop 'vals(2) mismatch'
    if (abs(vals(3) - 4D0) > 1D-12) error stop 'vals(3) mismatch'

    print *, 'All tests passed.'
end program read_43

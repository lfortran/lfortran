program lbound_02
    ! Per Fortran 2018 (16.9.109/16.9.197), if dimension DIM has zero
    ! extent, LBOUND returns 1 and UBOUND returns 0 regardless of the
    ! array's declared bounds.
    integer, allocatable :: a(:)
    integer, allocatable :: b(:,:)
    allocate(a(5:4))
    if (size(a) /= 0) error stop "size a"
    if (lbound(a, 1) /= 1) error stop "lbound a"
    if (ubound(a, 1) /= 0) error stop "ubound a"

    allocate(b(3, 7:6))
    if (size(b) /= 0) error stop "size b"
    if (lbound(b, 1) /= 1) error stop "lbound b 1"
    if (ubound(b, 1) /= 3) error stop "ubound b 1"
    if (lbound(b, 2) /= 1) error stop "lbound b 2"
    if (ubound(b, 2) /= 0) error stop "ubound b 2"
    print *, "PASS"
end program

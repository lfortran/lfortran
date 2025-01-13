program arrays_65
    integer :: A(2) = [1,2]
    integer :: B(2) = [1, 2]
    integer :: n = 2
    integer :: npt = 3
    integer :: dim1 = 1
    integer :: dim2 = 2
    integer :: X1(3, 2) = reshape([1, 1, 1, 2, 2, 2], [3, 2])
    integer :: X2(2, 3) = reshape([1, 2, 1, 2, 1, 2], [2, 3])
    A = sum(spread(A, dim1, n), dim=dim1)
    if (A(1) /= 2) error stop
    if (A(2) /= 4) error stop

    print *, spread(B, dim=dim1, ncopies=npt)
    if (any(spread(B, dim=dim1, ncopies=npt) /= X1)) error stop
    print *, spread(B, dim=dim2, ncopies=npt)
    if (any(spread(B, dim=dim2, ncopies=npt) /= X2)) error stop
end program

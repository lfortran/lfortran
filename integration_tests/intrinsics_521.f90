program intrinsics_521
    implicit none
    real, allocatable :: c(:)
    integer, allocatable :: i(:)

    allocate(c(3), i(3))
    c = [1.0, 2.0, 3.0]
    i = [1, 2, 3]
    if (sum(dble(c)) /= 6.0d0) error stop 1
    if (sum(float(i)) /= 6.0) error stop 2
    if (sum(dfloat(i)) /= 6.0d0) error stop 3
end program

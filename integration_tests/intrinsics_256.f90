program intrinsics_256
    implicit none
    integer(4), parameter :: i1 = minval([1, 2, 3])
    real(4), parameter :: i2 = minval([1.0, 2.0, 3.0])
    integer(8), parameter :: i3 = minval([1, 2, 3], [.true., .true., .true.])
    real(8), parameter :: i4 = minval([1.5_8, 22.9_8, 3.0_8], mask = [.true., .false., .true.])
    integer(4), parameter :: i5 = minval([11, 2, 5], 1, mask = [.true., .false., .true.])
    real(4), parameter :: i6 = minval([1.0, 3.0, 55.9], mask = [.true., .false., .true.], dim = 1)

    integer(4) :: ar1(4) = [1, 2, 7, 9]
    real(4) :: ar2(4) = [1.0, 3.1, 7.2, 9.0]
    logical(4) :: mask(4) = [.true., .false., .true., .true.]
    integer(4) :: dim = 1

    print *, i1
    if (i1 /= 1) error stop
    print *, i2
    if (abs(i2 - 1.00000000e+00) > 1e-6) error stop
    print *, i3
    if (i3 /= 1) error stop
    print *, i4
    if (abs(i4 - 1.50000000000000000e+00) > 1e-12) error stop
    print *, i5
    if (i5 /= 5) error stop
    print *, i6
    if (abs(i6 - 1.00000000e+00) > 1e-6) error stop

    print *, minval(ar1)
    if (minval(ar1) /= 1) error stop
    print *, minval(ar2)
    if (abs(minval(ar2) - 1.00000000e+00) >1e-6 ) error stop
    print *, minval(ar1, mask)
    if (minval(ar1, mask) /= 1) error stop
    print *, minval(ar2, mask = mask)
    if (abs(minval(ar2, mask = mask) - 1.00000000e+00) > 1e-6) error stop
    print *, minval(ar1, dim)
    if (minval(ar1, dim) /= 1) error stop
    print *, minval(ar2, dim = dim)
    if (abs(minval(ar2, dim = dim) - 1.00000000e+00) > 1e-6) error stop
    print *, minval(ar1, mask = mask, dim = dim)
    if (minval(ar1, mask = mask, dim = dim) /= 1) error stop
    print *, minval(ar2, dim, mask)
    if (abs(minval(ar2, dim, mask) - 1.00000000e+00) > 1e-6) error stop

end program
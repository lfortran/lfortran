program intrinsics_260
    implicit none
    integer(4), parameter :: i1 = sum([1, 2, 3])
	real(4), parameter :: i2 = sum([1.0, 2.0, 3.0])
	complex(4), parameter :: i3 = sum([(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)])
	integer(8), parameter :: i4 = sum([1, 2, 3], [.true., .true., .true.])
	real(8), parameter :: i5 = sum([1.5_8, 22.9_8, 3.0_8], mask = [.true., .false., .true.])
	complex(8), parameter :: i6 = sum([(1.5_8, 2.2_8), (22.9_8, 1.4_8), (3.0_8, 1.1_8)], mask = [.true., .false., .true.])
	integer(4), parameter :: i7 = sum([11, 2, 5], 1, [.true., .false., .true.])
	real(4), parameter :: i8 = sum([1.0, 3.0, 55.9], mask = [.true., .false., .true.], dim = 1)
    complex(4), parameter :: i9 = sum([(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)], dim = 1, mask = [.true., .false., .true.])

    integer(4) :: ar1(4) = [1, 2, 7, 9]
    real(4) :: ar2(4) = [1.0, 3.1, 7.2, 9.0]
    complex(4) :: ar3(4) = [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0), (7.0, 8.0)]
    logical(4) :: mask(4) = [.true., .false., .true., .true.]
    integer(4) :: dim = 1

    print *, i1
    if (i1 /= 6) error stop
    print *, i2
    if (abs(i2 - 6.0) > 1e-6) error stop
    print *, i3
    if (abs(i3 - (9.0, 12.0)) > 1e-6) error stop
    print *, i4
    if (i4 /= 6) error stop
    print *, i5
    if (abs(i5 - 4.5_8) > 1e-12) error stop
    print *, i6
    if (abs(i6 - (4.5000000000000000, 3.3000000000000003)) > 1e-6) error stop
    print *, i7
    if (i7 /= 16) error stop
    print *, i8
    if (abs(i8 - 5.69000015e+01) > 1e-6) error stop
    print *, i9
    if (abs(i9 - (6.0, 8.0)) > 1e-6) error stop

    print *, sum(ar1)
    if (sum(ar1) /= 19) error stop
    print *, sum(ar2)
    if (abs(sum(ar2) - 20.3) >1e-6 ) error stop
    print *, sum(ar3)
    if (abs(sum(ar3) - (16.0, 20.0)) > 1e-6) error stop
    print *, sum(ar1, mask = mask)
    if (sum(ar1, mask = mask) /= 17) error stop
    print *, sum(ar2, mask)
    if (abs(sum(ar2, mask) - 17.2) > 1e-6) error stop
    print *, sum(ar3, mask = mask)
    if (abs(sum(ar3, mask = mask) - (13.0, 16.0)) > 1e-6) error stop
    print *, sum(ar1, dim)
    if (sum(ar1, dim) /= 19) error stop
    print *, sum(ar2, dim = dim)
    if (abs(sum(ar2, dim = dim) - 20.3) > 1e-6) error stop
    print *, sum(ar3, dim)
    if (abs(sum(ar3, dim) - (16.0, 20.0)) > 1e-6) error stop
    print *, sum(ar1, mask = mask, dim = dim)
    if (sum(ar1, mask = mask, dim = dim) /= 17) error stop
    print *, sum(ar2, dim, mask)
    if (abs(sum(ar2, dim, mask) - 17.2) > 1e-6) error stop
    print *, sum(ar3, mask = mask, dim = dim)
    if (abs(sum(ar3, mask = mask, dim = dim) - (13.0, 16.0)) > 1e-6) error stop

end program
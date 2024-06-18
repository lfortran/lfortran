program intrinsics_260
    integer(4), parameter :: i1(1) = minloc([1,2,3])
	integer(4), parameter :: i2(1) = minloc([1.0, 2.0, 3.0])
	integer(4), parameter :: i3(1) = minloc(["aa", "db", "ca"])
	integer(4), parameter :: i4(1) = minloc([1,2,3], mask = [.true., .true., .true.])
	integer(8), parameter :: i5(1) = minloc([1,2,3], mask=[.true., .false., .true.], kind = 8)
	integer(4), parameter :: i6(1) = minloc([1,2,3], mask=[.true., .false., .true.], dim=1)
	integer(4), parameter :: i7(1) = minloc(["aa", "db", "ca"], mask=[.true., .false., .true.])
	integer(4), parameter :: i8(1) = minloc(["aa", "db", "ca"], mask=[.true., .false., .true.], dim = 1)
	integer(4), parameter :: i9(1) = minloc([1, 3, 2], mask = [.true., .false., .true.], back = .true.)
	integer(4), parameter :: i10(1) = minloc([3, 2, 1, 3], back = .true.)
	integer(4), parameter :: i11(1) = minloc([3.0, 2.0, 1.0, 3.0], back = .true., dim=1)
	integer(4), parameter :: i12(1) = minloc(["aa", "db", "ca"], mask = [.false., .false., .false.], kind = 4)

	print *, i1
	if (i1(1) /= 1) error stop
	print *, i2
	if (i2(1) /= 1) error stop
	print *, i3
	if (i3(1) /= 1) error stop
	print *, i4
	if (i4(1) /= 1) error stop
	print *, i5
	if (i5(1) /= 1) error stop
	print *, i6
	if (i6(1) /= 1) error stop
	print *, i7
	if (i7(1) /= 1) error stop
	print *, i8
	if (i8(1) /= 1) error stop
	print *, i9
	if (i9(1) /= 1) error stop
	print *, i10
	if (i10(1) /= 3) error stop
	print *, i11
	if (i11(1) /= 3) error stop
	print *, i12
	if (i12(1) /= 0) error stop

	print *, kind(minloc(["aa", "db", "ca"], mask = [.false., .false., .false.], kind = 8))
	if (kind(minloc(["aa", "db", "ca"], mask = [.false., .false., .false.], kind = 8)) /= 8) error stop
end program
program intrinsics_301
    implicit none

    integer(4), parameter :: i1(1) = findloc([1, 2, 3], 2)
    integer(4), parameter :: i2(1) = findloc([1.0, 2.0, 3.0], 2.0)
    integer(4), parameter :: i3(1) = findloc(["aa", "db", "ca"], "aa")
    integer(4), parameter :: i4(1) = findloc([1,2,3], 3, mask = [.true., .true., .true.])
    integer(8), parameter :: i5(1) = findloc([1,2,3], 1, mask=[.true., .false., .true.], kind = 8)
    integer(4), parameter :: i6(1) = findloc([1,2,3], 2, mask=[.true., .false., .true.], dim=1)
    integer(4), parameter :: i7(1) = findloc(["aa", "db", "ca"], "aa", mask=[.true., .false., .true.])
    integer(4), parameter :: i8(1) = findloc(["aa", "db", "ca"], "db", mask=[.true., .false., .true.], dim = 1)
    integer(4), parameter :: i9(1) = findloc([1, 3, 2, 3], 3, mask = [.true., .true., .false., .true.], back = .true.)
    integer(4), parameter :: i10(1) = findloc([3, 2, 1, 3], 3, back = .true.)
    integer(4), parameter :: i11(1) = findloc([3.0, 2.0, 1.0, 3.0], 2.0, back = .true., dim=1)
    integer(4), parameter :: i12(1) = findloc(["aa", "db", "ca"], "aa", mask = [.false., .false., .false.], kind = 4)
    integer(4), parameter :: i13 = findloc([1, 3, 2, 3], 3, 1, mask = [.true., .true., .false., .true.], back = .true., kind = 4)

    integer :: x1(5) = [1, 3, 2, 4, 2], y1 = 2
    real :: x2(5) = [1.0, 3.0, 2.0, 4.0, 2.0], y2 = 2.0
    character(len=2) :: x3(3) = ["aa", "db", "ca"], y3 = "aa"
    logical :: mask1(5) = [.true., .true., .false., .true., .true.]
    logical :: mask2(3) = [.true., .false., .true.]
    logical :: back1 = .false.
    logical :: back2 = .true.

    print *, findloc(x1, y1)
    if (any(findloc(x1, y1) /= 3)) error stop
    print *, findloc(x2, y2)
    if (any(findloc(x2, y2) /= 3)) error stop
    print *, findloc(x3, y3)
    if (any(findloc(x3, y3) /= 1)) error stop
    print *, findloc(x1, y1, mask = mask1)
    if (any(findloc(x1, y1, mask = mask1) /= 5)) error stop
    print *, findloc(x2, y2, mask = mask1)
    if (any(findloc(x2, y2, mask = mask1) /= 5)) error stop
    print *, findloc(x3, y3, mask = mask2)
    if (any(findloc(x3, y3, mask = mask2) /= 1)) error stop
    print *, findloc(x1, y1, mask = mask1, back = back1)
    if (any(findloc(x1, y1, mask = mask1, back = back1) /= 5)) error stop
    print *, findloc(x1, y1, mask = mask1, back = back2)
    if (any(findloc(x1, y1, mask = mask1, back = back2) /= 5)) error stop
    print *, findloc(x2, y2, mask = mask1, back = back2)
    if (any(findloc(x2, y2, mask = mask1, back = back2) /= 5)) error stop
    print *, findloc(x1, y1, mask = mask1, back = back2, kind = 8)
    if (any(findloc(x1, y1, mask = mask1, back = back2, kind = 8) /= 5)) error stop

    print *, i1
    if (i1(1) /= 2) error stop
    print *, i2
    if (i2(1) /= 2) error stop
    print *, i3
    if (i3(1) /= 1) error stop
    print *, i4
    if (i4(1) /= 3) error stop
    print *, i5
    if (i5(1) /= 1) error stop
    print *, i6
    if (i6(1) /= 0) error stop
    print *, i7
    if (i7(1) /= 1) error stop
    print *, i8
    if (i8(1) /= 0) error stop
    print *, i9
    if (i9(1) /= 4) error stop
    print *, i10
    if (i10(1) /= 4) error stop
    print *, i11
    if (i11(1) /= 2) error stop
    print *, i12
    if (i12(1) /= 0) error stop
    print *, i13
    if (i13 /= 4) error stop

    print *, kind(findloc(["aa", "db", "ca"], "aa", 1, mask = [.false., .false., .false.], kind = 8))
    if (kind(findloc(["aa", "db", "ca"], "db", 1, mask = [.false., .false., .false.], kind = 8)) /= 8) error stop

end program intrinsics_301

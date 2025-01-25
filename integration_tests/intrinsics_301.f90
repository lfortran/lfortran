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
    complex, dimension(2) :: dc = [(1.4, 1.1), (2.3, 2.2)]
    complex, dimension(2) :: dc2 = [(1.0, 2.0), (3.0, 4.0)]

    integer :: x1(5) = [1, 3, 2, 4, 2], y1 = 2
    integer, parameter :: x1_param(5) = [1, 3, 2, 4, 2]
    integer, parameter :: y1_param = 2
    integer, parameter :: i14(1) = findloc(x1_param, y1_param)
    ! complex argument complex-time search
    integer, parameter :: i15(1) = findloc([(1.0, 2.0), (3.0, 4.0)], (3.0, 4.0))
    integer, parameter :: i16(1) = findloc([(1, 2), (3, 4)], (3, 4))
    ! complex argument search with no-find of "value"
    integer, parameter :: i17(1) = findloc([(1.0, 2.0), (3.0, 4.0), (4.0, 2.0)], (3.0, 2.0))
    integer, parameter :: i18(1) = findloc(["aa", "bb", "cc"], "bb")

    real :: x2(5) = [1.0, 3.0, 2.0, 4.0, 2.0], y2 = 2.0
    character(len=2) :: x3(3) = ["aa", "db", "ca"], y3 = "aa"
    logical :: mask1(5) = [.true., .true., .false., .true., .true.]
    logical :: mask2(3) = [.true., .false., .true.]
    logical :: back1 = .false.
    logical :: back2 = .true.
    complex :: x = (1.4, 1.1)
    complex :: x5 = (3.0, 4.0)

    integer, dimension(1) :: result
    integer, dimension(1) :: result2

    ! findloc(Array, value, mask)
    logical :: x4(3) = [.true., .false., .true.], y4 = .true., mask3 = .true.

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
    print *, findloc(x4, y4, mask3)
    if (any(findloc(x4, y4, mask3) /= 1)) error stop

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

    print *, findloc(dc, x)
    if (any(findloc(dc, x) /= 1)) error stop
    print*, findloc([(1.0, 2.0), (3.0, 4.0)], (3.0, 4.0))
    if (any(findloc([(1.0, 2.0), (3.0, 4.0)], (3.0, 4.0)) /= 2)) error stop
    print*, findloc([(1.0, 2.0), (3.0, 4.0)], (1.0, 2.0), mask = [.true., .true.])
    if (any(findloc([(1.0, 2.0), (3.0, 4.0)], (1.0, 2.0), mask = [.true., .true.]) /= 1)) error stop
    print*, findloc([(1.0, 2.0), (3.0, 4.0)], (3.0, 4.0), mask=[.true., .true.], kind = 8)
    if (any(findloc([(1.0, 2.0), (3.0, 4.0)], (3.0, 4.0), mask = [.true., .true.], kind = 8) /= 2)) error stop

    result = findloc(dc2, x5)
    result2 = findloc(dc2, x5, mask = [.true., .true.])

    print *, result
    if (any(result /= 2)) error stop
    
    print*, result2
    if (any(result2 /= 2)) error stop
    print *, i14
    if (any(i14 /= 3)) error stop
    print *, i15
    if (any(i15 /= 2)) error stop
    print *, i16
    if (any(i16 /= 2)) error stop
    print *, i17
    if (any(i17 /= 0)) error stop
    print *, i18
    if (any(i18 /= 2)) error stop

    print *, findloc(x1_param, y1)
    if (any(findloc(x1_param, y1) /= 3)) error stop
    
end program intrinsics_301

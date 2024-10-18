program intrinsics_276
    implicit none
    integer :: a1 = 5, a2 = 8
    integer(8) :: a3 = -1, a4 = -4
    integer :: a5 = -2, a6 = -5

    integer, parameter :: i1 = or(5, 8)
    integer, parameter :: i2 = or(-1, 5)
    integer, parameter :: i3 = or(-4_8, 2_8)
    integer(8), parameter :: i4 = or(-2_8, 5_8)
    logical, parameter :: l1 = or(.true., .false.) 
    logical, parameter :: l2 = or(.false., .false.) 
    logical :: true = .true., false = .false.

    integer :: arr1(3), arr3(3)
    integer(8) :: arr2(3)
    integer :: res(3)
    arr1 = [5, 8, 9]
    arr3 = [8, 5, 2]
    arr2 = [-1_8, -5_8, -10_8]

    print *, i1
    if (i1 /= 13) error stop
    print *, i2
    if (i2 /= -1) error stop
    print *, i3
    if (i3 /= -2) error stop
    print *, i4
    if (i4 /= -1) error stop
    print *, l1
    if (l1 .neqv. .true.) error stop
    print *, l2
    if (l2 .neqv. .false.) error stop

    print *, or(true, false)
    if (or(true, false) .neqv. .true.) error stop
    print *, or(false, false)
    if (or(false, false) .neqv. .false.) error stop
    print*, or(5, 8)
    if (or(5, 8) /= 13) error stop
    print*, or(-1, 5)
    if (or(-1, 5) /= -1) error stop
    print*, or(8, -4)
    if (or(8, -4) /= -4) error stop
    print*, or(-2, -5)
    if (or(-2, -5) /= -1) error stop
  
    print*, or(a1, a2)
    if (or(a1, a2) /= 13) error stop
    print*, or(a3, a4)
    if (or(a3, a4) /= -1) error stop
    print*, or(a2, a5)
    if (or(a2, a5) /= -2) error stop
    print*, or(a5, a6)
    if (or(a5, a6) /= -1) error stop

end program 
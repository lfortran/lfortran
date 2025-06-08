program intrinsics_277
    implicit none
    integer :: a1 = 5, a2 = 8
    integer(8) :: a3 = -1, a4 = -4
    integer :: a5 = -2, a6 = -5

    integer, parameter :: i1 = xor(5, 8)
    integer, parameter :: i2 = xor(-1, 5)
    integer, parameter :: i3 = xor(-4_8, 2_8)
    integer(8), parameter :: i4 = xor(-2_8, 5_8)
    logical, parameter :: l1 = xor(.true., .false.) 
    logical, parameter :: l2 = xor(.false., .false.) 
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
    if (i2 /= -6) error stop
    print *, i3
    if (i3 /= -2) error stop
    print *, i4
    if (i4 /= -5) error stop
    print *, l1
    if (l1 .neqv. .true.) error stop
    print *, l2
    if (l2 .neqv. .false.) error stop

    print *, xor(true, false)
    if (xor(true, false) .neqv. .true.) error stop
    print *, xor(false, false)
    if (xor(false, false) .neqv. .false.) error stop
    print*, xor(5, 8)
    if (xor(5, 8) /= 13) error stop
    print*, xor(-1, 5)
    if (xor(-1, 5) /= -6) error stop
    print*, xor(8, -4)
    if (xor(8, -4) /= -12) error stop
    print*, xor(-2, -5)
    if (xor(-2, -5) /= 5) error stop
  
    print*, xor(a1, a2)
    if (xor(a1, a2) /= 13) error stop
    print*, xor(a3, a4)
    if (xor(a3, a4) /= 3) error stop
    print*, xor(a2, a5)
    if (xor(a2, a5) /= -10) error stop
    print*, xor(a5, a6)
    if (xor(a5, a6) /= 5) error stop

end program 
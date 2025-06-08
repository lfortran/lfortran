program intrinsics_275
    implicit none
    integer :: a1 = 5, a2 = 8
    integer(8) :: a3 = -1, a4 = -4
    integer :: a5 = -2, a6 = -5

    integer, parameter :: i1 = and(5, 8)
    integer, parameter :: i2 = and(-1, 5)
    integer, parameter :: i3 = and(-4_8, 2_8)
    integer(8), parameter :: i4 = and(-2_8, 5_8)
    logical, parameter :: l1 = and(.true., .false.) 
    logical, parameter :: l2 = and(.true., .true.) 
    logical :: true = .true., false = .false.

    print *, i1
    if (i1 /= 0) error stop
    print *, i2
    if (i2 /= 5) error stop
    print *, i3
    if (i3 /= 0) error stop
    print *, i4
    if (i4 /= 4) error stop

    ! logical argument testing
    print *, l1
    if (l1 .neqv. .false.) error stop
    print *, l2
    if (l2 .neqv. .true.) error stop
    print *, and(true, false)
    if (and(true, false) .neqv. .false.) error stop 
    print *, and(true, true)
    if (and(true, true) .neqv. .true.) error stop 

    print*, and(5, 8)
    if (and(5, 8) /= 0) error stop
    print*, and(-1, 5)
    if (and(-1, 5) /= 5) error stop
    print*, and(8, -4)
    if (and(8, -4) /= 8) error stop
    print*, and(-2, -5)
    if (and(-2, -5) /= -6) error stop
  
    print*, and(a1, a2)
    if (and(a1, a2) /= 0) error stop
    print*, and(a3, a4)
    if (and(a3, a4) /= -4) error stop
    print*, and(a2, a5)
    if (and(a2, a5) /= 8) error stop
    print*, and(a5, a6)
    if (and(a5, a6) /= -6) error stop

    print *, kind(and(5, 8))
    if (kind(and(5, 8)) /= 4) error stop
    print *, kind(and(-1_8, 5_8))
    if (kind(and(-1_8, 5_8)) /= 8) error stop

end program 
! This MRE tests the following:
! List-directed internal read from character into a whole logical array.
! List-directed internal read from character into complex(kind=4 or kind=8) array.

program read_90
    implicit none
    character(len=10) :: buf = ' T F T '
    logical :: v(3)
    character(len=30) :: buf1 = ' (1.0,2.0) (3.0,4.0) '
    complex(kind=4) :: v1(2)

    character(len=30) :: buf2 = ' (1.d0,2.d0) (3.d0,4.d0) '
    complex(kind=8) :: v2(2)

    ! Part-1: Logical array
    read(buf, *) v
    print *, v
    if (v(1) .neqv. .true.) error stop
    if (v(2) .neqv. .false.) error stop
    if (v(3) .neqv. .true.) error stop

    ! Part-2: Complex array
    read(buf1, *) v1
    read(buf2, *) v2
    print *, v1
    if (abs(v1(1) - (1.0,2.0)) > 1.0e-6) error stop
    if (abs(v1(2) - (3.0,4.0)) > 1.0e-6) error stop
    print *, v2
    if (abs(v2(1) - (1.d0,2.d0)) > 1.0e-6) error stop
    if (abs(v2(2) - (3.d0,4.d0)) > 1.0e-6) error stop

    print *, "All tests passed"
end program read_90
  
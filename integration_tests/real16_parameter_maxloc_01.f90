program real16_parameter_maxloc_01
    ! maxloc, minloc and findloc of real(16) constants fold exactly
    ! in a PARAMETER initializer
    implicit none
    integer, parameter :: qp = 16
    real(qp), parameter :: a(3) = [1.0_qp, 3.0_qp, 2.0_qp]
    ! Elements 2 and 4 differ from 1 only beyond double precision
    real(qp), parameter :: big = 1.000000000000000000000001_qp
    real(qp), parameter :: small = 0.999999999999999999999999_qp
    real(qp), parameter :: b(4) = [1.0_qp, big, 1.0_qp, big]
    real(qp), parameter :: c(4) = [1.0_qp, small, 1.0_qp, small]
    integer, parameter :: i1(1) = maxloc(a)
    integer, parameter :: i2 = maxloc(a, 1)
    integer, parameter :: i3(1) = minloc(a)
    integer, parameter :: i4 = minloc(a, 1)
    integer, parameter :: i5(1) = findloc(a, 2.0_qp)
    integer, parameter :: i6 = findloc(a, 2.0_qp, 1)
    integer, parameter :: j1(1) = maxloc(b)
    integer, parameter :: j2(1) = maxloc(b, back=.true.)
    integer, parameter :: j3(1) = minloc(c)
    integer, parameter :: j4 = minloc(c, 1, back=.true.)
    integer, parameter :: j5(1) = findloc(b, big)
    integer, parameter :: j6(1) = findloc(b, big, back=.true.)
    integer, parameter :: j7(1) = findloc(b, 1.0_qp, back=.true.)
    integer, parameter :: j8(1) = maxloc(b, mask=[.true., .false., .true., .true.])
    integer, parameter :: j9(1) = maxloc(b, mask=[.true., .false., .true., .false.])
    integer, parameter :: j10(1) = minloc(c, mask=.false.)
    integer, parameter :: j11(1) = findloc(b, 2.0_qp)
    integer, parameter :: j12 = findloc(c, small, 1, mask=[.true., .false., .true., .true.])
    if (i1(1) /= 2 .or. i2 /= 2) error stop 1
    if (i3(1) /= 1 .or. i4 /= 1) error stop 2
    if (i5(1) /= 3 .or. i6 /= 3) error stop 3
    if (j1(1) /= 2) error stop 4
    if (j2(1) /= 4) error stop 5
    if (j3(1) /= 2) error stop 6
    if (j4 /= 4) error stop 7
    if (j5(1) /= 2) error stop 8
    if (j6(1) /= 4) error stop 9
    if (j7(1) /= 3) error stop 10
    if (j8(1) /= 4) error stop 11
    if (j9(1) /= 1) error stop 12
    if (j10(1) /= 0) error stop 13
    if (j11(1) /= 0) error stop 14
    if (j12 /= 4) error stop 15
    print *, i1, i2, i3, i4, i5, i6
end program

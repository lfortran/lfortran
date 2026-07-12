program unpack_scalar_field_02
    implicit none

    logical, parameter :: mask1(5) = [.true., .false., .true., .false., .true.]
    logical, parameter :: mask2(4) = [.false., .true., .false., .true.]
    logical, parameter :: mask3(3) = [.true., .true., .false.]
    logical, parameter :: mask4(4) = [.true., .false., .true., .false.]

    integer, parameter :: a(5) = unpack([100, 200, 300], mask1, 0)
    real, parameter :: b(4) = unpack([1.5, 2.5], mask2, -1.0)
    logical, parameter :: c(3) = unpack([.true., .false.], mask3, .true.)
    complex, parameter :: d(4) = unpack([(1.0, 2.0), (3.0, 4.0)], mask4, (0.0, -1.0))

    if (a(1) /= 100 .or. a(2) /= 0 .or. a(3) /= 200 .or. a(4) /= 0 .or. a(5) /= 300) error stop 1
    if (abs(b(1) - (-1.0)) > 0.001) error stop 2
    if (abs(b(2) - 1.5) > 0.001) error stop 3
    if (abs(b(3) - (-1.0)) > 0.001) error stop 4
    if (abs(b(4) - 2.5) > 0.001) error stop 5
    if (c(1) .neqv. .true.) error stop 6
    if (c(2) .neqv. .false.) error stop 7
    if (c(3) .neqv. .true.) error stop 8
    if (abs(real(d(1)) - 1.0) > 0.001) error stop 9
    if (abs(aimag(d(1)) - 2.0) > 0.001) error stop 10
    if (abs(real(d(2)) - 0.0) > 0.001) error stop 11
    if (abs(aimag(d(2)) - (-1.0)) > 0.001) error stop 12
    if (abs(real(d(3)) - 3.0) > 0.001) error stop 13
    if (abs(aimag(d(3)) - 4.0) > 0.001) error stop 14
    if (abs(real(d(4)) - 0.0) > 0.001) error stop 15
    if (abs(aimag(d(4)) - (-1.0)) > 0.001) error stop 16

    print *, "PASS"

end program

program real128_array_const_05
    ! Conversions of real(16) parameter arrays to and from other kinds and
    ! integer, and negation of real(16) array constructors, in named constants
    implicit none
    real(16), parameter :: q(2) = [1.5_16, 2.5_16]
    real(8), parameter :: d(2) = [1.5_16, 2.5_16]
    real(4), parameter :: s(2) = [1.5_16, 2.5_16]
    integer, parameter :: i4(2) = [1.5_16, 2.5_16]
    integer(8), parameter :: i8(2) = [1.5_16, 2.5_16]
    real(16), parameter :: from4(2) = [1.5, 2.5]
    real(16), parameter :: from8(2) = [1.5d0, 2.5d0]
    real(16), parameter :: fromi(2) = [1, 2]
    real(16), parameter :: n1(1) = -[1.5_16]
    real(16), parameter :: n2(2) = -[1.5_16, 2.0_16]
    real(16), parameter :: n3(2) = -q
    real(8), parameter :: dn(2) = -q
    complex(16), parameter :: cn(2) = -[(1.5_16, 2.5_16), (0.5_16, -1.0_16)]
    complex(8), parameter :: cd(2) = [(1.5_16, 2.5_16), (0.5_16, -1.0_16)]
    complex(16), parameter :: cq(2) = [(1.5_8, 2.5_8), (0.5_8, -1.0_8)]

    if (d(1) /= 1.5_8 .or. d(2) /= 2.5_8) error stop 1
    if (s(1) /= 1.5_4 .or. s(2) /= 2.5_4) error stop 2
    if (i4(1) /= 1 .or. i4(2) /= 2) error stop 3
    if (i8(1) /= 1 .or. i8(2) /= 2) error stop 4
    if (from4(1) /= 1.5_16 .or. from4(2) /= 2.5_16) error stop 5
    if (from8(1) /= 1.5_16 .or. from8(2) /= 2.5_16) error stop 6
    if (fromi(1) /= 1.0_16 .or. fromi(2) /= 2.0_16) error stop 7
    if (n1(1) /= -1.5_16) error stop 8
    if (n2(1) /= -1.5_16 .or. n2(2) /= -2.0_16) error stop 9
    if (n3(1) /= -1.5_16 .or. n3(2) /= -2.5_16) error stop 10
    if (dn(1) /= -1.5_8 .or. dn(2) /= -2.5_8) error stop 11
    if (cn(1) /= (-1.5_16, -2.5_16) .or. cn(2) /= (-0.5_16, 1.0_16)) error stop 12
    if (cd(1) /= (1.5_8, 2.5_8) .or. cd(2) /= (0.5_8, -1.0_8)) error stop 13
    if (cq(1) /= (1.5_16, 2.5_16) .or. cq(2) /= (0.5_16, -1.0_16)) error stop 14
    print *, "ok"
end program

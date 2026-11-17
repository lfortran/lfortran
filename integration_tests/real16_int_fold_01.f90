program real16_int_fold_01
    ! int of real(16) constants is folded exactly, not through double, for
    ! array and scalar named constants, initializers and expressions
    implicit none
    ! 2 - 2**-100 and 2**62 + 3.5 are not representable in double
    real(16), parameter :: q(3) = [2.0_16 - 2.0_16**(-100), 2.0_16**62 + 3.5_16, &
        -7.75_16]
    integer(8), parameter :: ei(3) = [1_8, 2_8**62 + 3, -7_8]
    integer(8), parameter :: pi8(3) = int(q, 8), si8 = int(q(2), 8)
    integer, parameter :: pi4(2) = int(q(1:3:2)), si4 = int(q(1))
    integer(8) :: vi8(3) = int(q, 8)
    integer(8) :: i8(3)
    integer :: i4(2)

    if (any(pi8 /= ei) .or. si8 /= ei(2) .or. any(vi8 /= ei)) error stop
    if (any(pi4 /= ei(1:3:2)) .or. si4 /= 1) error stop
    i8 = int(q, 8)
    if (any(i8 /= ei)) error stop
    i4 = int(q(1:3:2))
    if (any(i4 /= ei(1:3:2))) error stop
    print *, i8
end program

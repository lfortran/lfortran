program stdin_read_list_directed_repeat_01
    ! List-directed READ from the standard input: r*c is r values c and r*
    ! is r null values, which continue over the items of the statement.
    implicit none
    integer :: ia(4), i1, i2, i3, k, ios
    real :: ra(3), r1, r2
    double precision :: d1, d2
    logical :: la(4), l1, l2
    character(len=4) :: ca(3), c1, c2
    complex :: za(3), z1, z2

    ia = -9
    read(*, *) ia
    if (any(ia /= 7)) error stop 1

    ! A repeat over a scalar, an array section and a scalar.
    read *, i1, ia(1:3), i2
    if (i1 /= 5 .or. any(ia(1:3) /= [5, 9, 9]) .or. i2 /= 9) error stop 2

    ! Null values leave the items unchanged.
    ia = -9
    read(*, *) ia
    if (any(ia /= [1, -9, -9, 4])) error stop 3
    i1 = -1; i2 = -2; i3 = -3; k = -4
    read(*, *) i1, i2, i3, k
    if (i1 /= 1 .or. i2 /= -2 .or. i3 /= -3 .or. k /= 3) error stop 4
    ia = -9
    read(*, *) ia
    if (any(ia /= [1, -9, 3, -9])) error stop 5

    read(*, *) ra
    if (any(ra /= 2.5)) error stop 6
    read(*, *) r1, r2, ra(1), d1, d2
    if (r1 /= 1.5 .or. r2 /= 1.5 .or. ra(1) /= 0.5) error stop 7
    if (d1 /= 0.25d0 .or. d2 /= 0.25d0) error stop 8

    read(*, *) la, l1, l2
    if (any(la) .or. .not. l1 .or. .not. l2) error stop 9
    la = .true.
    read(*, *) la(1:3)
    if (la(1) .or. .not. all(la(2:4))) error stop 20

    read(*, *) ca, c1, c2
    if (any(ca /= "ab") .or. c1 /= "xy" .or. c2 /= "xy") error stop 10

    read(*, *) za, z1, z2
    if (any(za /= (1.0, 2.0))) error stop 11
    if (z1 /= (3.0, 4.0) .or. z2 /= (3.0, 4.0)) error stop 12

    ! The values of a repeat that the statement does not take are dropped
    ! with the rest of the record.
    read(*, *) i1, i2
    if (i1 /= 6 .or. i2 /= 6) error stop 13
    read(*, *) k
    if (k /= 11) error stop 14

    ! A slash ends the statement.
    i3 = -3
    read(*, *) i1, i2, i3
    if (i1 /= 8 .or. i2 /= 8 .or. i3 /= -3) error stop 15

    ! The values of a repeat left by a READ that fails are dropped too.
    read(*, *, iostat=ios) k
    if (ios <= 0) error stop 16
    k = -4
    read(*, *, iostat=ios) k
    if (ios /= 0 .or. k /= 5) error stop 17
    r1 = -1; k = -4
    read(*, *, iostat=ios) r1, k
    if (ios <= 0 .or. r1 /= 1.5) error stop 18
    k = -4
    read(*, *, iostat=ios) k
    if (ios /= 0 .or. k /= 6) error stop 19
end program

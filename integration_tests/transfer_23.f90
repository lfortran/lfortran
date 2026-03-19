program transfer_23
    integer :: i = -1974590819
    integer, parameter :: m3 = int(z'E9139917', 4)
    integer(2) :: j(2) = [8028, 6459]
    integer(8) :: len
    integer :: len32_lb(0:1)
    integer :: len32_ref(2)

    real(4), parameter :: r0 = 1.5_4
    integer(2), parameter :: jr(2) = transfer(r0, 0_2, 2)
    real(4), parameter :: r1 = transfer(jr, 0.0_4)

    character(4), parameter :: c0 = 'ABCD'
    integer(1), parameter :: cb(4) = transfer(c0, [0_1])
    character(4), parameter :: c1 = transfer(cb, '    ')

    j = transfer( i, 0_2, 2 )
    j = j * transfer( m3, 0_2, 2 )
    print *, j
    if (j(1) /= -3813 .or. j(2) /= 16842) error stop

    print *, jr
    print *, r1
    if (r1 /= r0) error stop

    print *, c1
    if (c1 /= c0) error stop

    len = 912320821391_8
    len32_lb = transfer(len, 0, 2)
    len32_ref = transfer(len, 0, 2)
    if (len32_lb(0) /= len32_ref(1) .or. len32_lb(1) /= len32_ref(2)) error stop

    print *, len32_lb
end program transfer_23
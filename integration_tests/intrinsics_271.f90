program intrinsics_271
    implicit none
    integer(4), parameter :: i1 = leadz(1050)
    integer(8), parameter :: i2 = leadz(00)
    integer, parameter :: ar1(3) = leadz([82471095, 829012001, 039024800])
    integer(4) :: i3 = 283200001
    integer(8) :: i4 = 289000001
    integer(4) :: arr1(3) = [82471095, 829012001, 039024800]
    integer(8) :: arr2(3) = [13471095, 8290000, 0392414800]

    print *, i1
    if (i1 /= 21) error stop
    print *, i2
    if (i2 /= 32) error stop
    print *, ar1
    if (any(ar1 /= [5, 2, 6])) error stop

    print *, leadz(i3)
    if (leadz(i3) /= 3) error stop
    print *, leadz(i4)
    if (leadz(i4) /= 35) error stop

    print *, leadz(arr1)
    if (any(leadz(arr1) /= [5, 2, 6])) error stop
    print *, leadz(arr2)
    if (any(leadz(arr2) /= [40, 41, 35])) error stop

    print *, kind(leadz(0))
    if (kind(leadz(0)) /= 4) error stop
    print *, kind(leadz(0_4))
    if (kind(leadz(0_4)) /= 4) error stop
    print *, kind(leadz(0_8))
    ! if (kind(leadz(0_8)) /= 4) error stop ! Gives wrong output

end program
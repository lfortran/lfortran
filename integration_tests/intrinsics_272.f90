program intrinsics_272
    implicit none
    integer(4), parameter :: i1 = trailz(1050)
    integer(8), parameter :: i2 = trailz(000)
    integer, parameter :: ar1(3) = trailz([0000821095, 829012001, 039024800])
    integer(4) :: i3 = 283200001
    integer(8) :: i4 = 289000001
    integer(4) :: arr1(3) = [02471095, 820012001, 039024800]
    integer(8) :: arr2(3) = [13471095, 8290000, 0392414800]

    print *, i1
    if (i1 /= 1) error stop
    print *, i2
    if (i2 /= 32) error stop
    print *, ar1
    if (any(ar1 /= [0, 0, 5])) error stop

    print *, trailz(i3)
    if (trailz(i3) /= 0) error stop
    print *, trailz(i4)
    if (trailz(i4) /= 0) error stop

    print *, trailz(arr1)
    ! if (any(trailz(arr1) /= [0, 0, 5])) error stop ! Does not work #4363
    print *, trailz(arr2)
    ! if (any(trailz(arr2) /= [0, 4, 4])) error stop

    print *, kind(trailz(0))
    if (kind(trailz(0)) /= 4) error stop
    print *, kind(trailz(0_4))
    if (kind(trailz(0_4)) /= 4) error stop
    print *, kind(trailz(0_8))
    ! if (kind(trailz(0_8)) /= 4) error stop ! Gives wrong output

end program

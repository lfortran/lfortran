program bits_05
    implicit none
    ! integer(4) :: from, res
    ! integer(8) :: from8, res8
    integer :: i, len!, pos
    integer(8) :: pos8!, i8, len8
    ! integer, parameter :: i1 = ibits(10, 2, 2)
    ! integer(8), parameter :: i2 = ibits(10_8, 2, 2)
    ! integer, parameter :: i3 = ibits(10, 5_8, 2)
    ! integer(8), parameter :: i4 = ibits(10_8, 5_8, 2)
    ! integer, parameter :: ar1(3) = ibits([10, 20, 30], 2, 2)
    ! integer(8), parameter :: ar2(3) = ibits([10_8, 20_8, 30_8], 2, 2)
    ! integer :: arr1(3) = [102134, 20, 30]
    ! integer(8) :: arr2(3) = [1042890_8, 20_8, 30_8]

    ! i = 102394
    ! pos = 2
    ! len = 2

    ! print *, ibits(i, pos, len)
    ! if (ibits(i, pos, len) /= 2) error stop

    ! i8 = 102394_8
    ! pos8 = 12_8
    ! len8 = 7_8

    ! print *, ibits(i8, pos8, len8)
    ! if (ibits(i8, pos8, len8) /= 24) error stop

    print *, ibits(i, pos8, len)
    ! if (ibits(i, pos8, len) /= 0) error stop

    ! from = 10
    ! from8 = 10_8

    ! res = ibits(from, 2, 2)
    ! if (res /= 2) error stop

    ! res8 = ibits(from8, 2, 2)
    ! if (res8 /= 2_8) error stop

    ! res = ibits(from, 0, 2)
    ! if (res /= 2) error stop

    ! res8 = ibits(from8, 0, 2)
    ! if (res8 /= 2_8) error stop

    ! from  = -20
    ! from8 = -20_8

    ! res = ibits(from, 29, 2)
    ! if (res /= 3) error stop

    ! res8 = ibits(from8, 29, 2)
    ! if (res8 /= 3) error stop

    ! res = ibits(from, 2, 2)
    ! if (res /= 3) error stop

    ! res8 = ibits(from8, 2, 2)
    ! if (res8 /= 3) error stop

    ! print *, ibits(arr1, 2, 2)
    ! if (any(ibits(arr1, 2, 2) /= [1, 1, 3])) error stop
    ! print *, ibits(arr2, 2, 2)
    ! if (any(ibits(arr2, 2, 2) /= [2, 1, 3])) error stop

    ! print *, kind(ibits(10, 2, 2))
    ! if (kind(ibits(10, 2, 2)) /= 4) error stop
    ! print *, kind(ibits(10_8, 2, 2))
    ! if (kind(ibits(10_8, 2, 2)) /= 8) error stop
    ! print *, kind(ibits(10, 2_8, 2_8))
    ! if (kind(ibits(10, 2_8, 2_8)) /= 4) error stop
end program

program arrays_reshape_54
    implicit none
    real :: single(1,1)
    real(8) :: ranked(-1:0,3:5)
    integer(8) :: padded(2,3), ordered(2,3)
    integer :: i, j

    single = reshape([2.0], [1,1])
    if (single(1,1) /= 2.0) error stop

    ranked = reshape([1.25_8,2.25_8,3.25_8,4.25_8,5.25_8,6.25_8], [2,3])
    do j = 3, 5
        do i = -1, 0
            if (ranked(i,j) /= 1.25_8 + (i+1) + 2*(j-3)) error stop
        end do
    end do

    padded = reshape([10000000001_8,10000000002_8], [2,3], &
                     pad=[10000000003_8,10000000004_8])
    if (padded(1,1) /= 10000000001_8) error stop
    if (padded(2,1) /= 10000000002_8) error stop
    do j = 2, 3
        if (padded(1,j) /= 10000000003_8) error stop
        if (padded(2,j) /= 10000000004_8) error stop
    end do

    ordered = reshape([11_8,12_8,13_8,14_8,15_8,16_8], [2,3], order=[2,1])
    do j = 1, 3
        do i = 1, 2
            if (ordered(i,j) /= 10_8 + j + 3*(i-1)) error stop
        end do
    end do
end program

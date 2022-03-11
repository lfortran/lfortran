program constant_arrays
    implicit none
    integer :: i
    real, parameter :: realArray(5) = [1.1, 3.0, 10.0, 2.1, 5.5]
    integer, parameter:: matrix(3,2,2) = reshape([1,2,3,4,5,6,7,8,9,10,11,12], [3,2,2])

    do i = 1,5
        print *, realArray(i)
    end do

    print *, matrix(2,1,2)
end program constant_arrays

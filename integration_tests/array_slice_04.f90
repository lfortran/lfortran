program array_slice_04
implicit none

    integer :: i, j
    integer :: array1(10)
    integer :: array2(10, 10)
    real, allocatable :: y(:)

    allocate(y(13))
    y = [(real(i - 1, 4)**2, i = 1, 13)]

    print *, y(2:12:2)
    if( any(y(2:12:2) /= [1.0, 9.0, 25.0, 49.0, 81.0, 121.0]) ) error stop


    array1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    print *, array1(1:10)
    if( any(array1(1:10) /= [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) ) error stop

    print *, array1(10:1:-1)
    if( any(array1(10:1:-1) /= [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]) ) error stop

    array1 = [(i, i = 1, 10)]
    array1(:) = 0
    array1(1:5) = 1
    array1(6:) = 1

    do i = 1, 10
        do j = 1, 10
            array2(i, j) = i * j
        end do
    end do

    print *, array1(1)
    if( array1(1) /= 1 ) error stop

    print *, array1(1:10:2)
    if( any(array1(1:10:2) /= [1, 1, 1, 1, 1]) ) error stop

    print *, array2(:, 3)
    if( any(array2(:, 3) /= [3, 6, 9, 12, 15, 18, 21, 24, 27, 30]) ) error stop

    print *, array1(10:1:-1)
    if( any(array1(10:1:-1) /= [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]) ) error stop

end program array_slice_04

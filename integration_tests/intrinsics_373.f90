program intrinsics_373
    integer :: a(2, 3, 4), a1(2), i, j, k, n
    integer :: b(2, 3, 2, 4)

    a1(1) = 3
    a1(2) = 4
    do i = 1, 2
        do j = 1, 3
            do k = 1, 4
                a(i, j, k) = i + j + k
            end do
        end do
    end do

    print *, a
    print *, a1

    print *, spread(a, 3, 2)
    print *, spread(a1, 1, 3)
    b = spread(a, 3, 2)
    do n = 1, 2
        do i = 1, 2
            do j = 1, 3
                do k = 1, 4
                    if( b(i, j, n, k) /= i + j + k ) error stop
                end do
            end do
        end do
    end do

    if( any(spread(a1, 1, 3) /= reshape([3, 3, 3, 4, 4, 4], [3, 2])) ) error stop
end program

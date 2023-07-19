program implied_do_loops5
    integer :: x(3,4), i, j
    integer :: expected_x(3,4)
    integer :: y(12)
    y = [1,2,3,4,2,3,4,5,3,4,5,6]
    expected_x = reshape(y, [3, 4])
    x = reshape([(i,(i+j,j=1,3),i=1,3)], [3, 4])

    do i = 1, 3
        do j = 1, 4
            if (x(i,j) /= expected_x(i,j)) error stop
        end do
    end do
    print *, x

end program
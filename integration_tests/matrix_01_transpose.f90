program matrix_01_tranpose
implicit none
integer :: i, j, a(3, 4), b(4, 3)
b = transpose(a)

do i = 1, 3
    do j = 1, 3
        if( a(i, j) /= b(j, i) ) error stop
    end do
end do
end program matrix_01_tranpose
program matrix_02_matmul
implicit none
integer :: i, j, k, a(3, 4)
real :: b(4, 3), cmat(3, 3), centry

a = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], shape(a))
b = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0], shape(b))
cmat = matmul(a, b)

do i = 1, 3
    do j = 1, 3
        centry = 0
        do k = 1, 4
            centry = centry + a(i, k) * b(k, j)
        end do
        if( cmat(i, j) /= centry ) error stop
    end do
end do
end program matrix_02_matmul
program matrix_02_matmul
implicit none
integer :: i, j, k, a(3, 4)
real :: b(4, 3), cmat(3, 3), centry
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
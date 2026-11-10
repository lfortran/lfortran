program matmul_17
    ! matmul of explicit-shape dummy arrays inside a contained subroutine (#3918)
    implicit none
    double precision :: matrix1(2, 3), matrix2(3, 1)
    double precision :: matrix3(2, 1)
    integer :: k, i, j

    k = 1
    do i = 1, 2
        do j = 1, 3
            matrix1(i, j) = k
            k = k + 1
        end do
    end do
    do i = 1, 3
        do j = 1, 1
            matrix2(i, j) = k
            k = k + 1
        end do
    end do

    call matmul2d(matrix1, 2, 3, matrix2, 3, 1, matrix3)
    print *, matrix3
    if (abs(matrix3(1, 1) - 50.0d0) > 1.0d-12) error stop
    if (abs(matrix3(2, 1) - 122.0d0) > 1.0d-12) error stop

contains

    subroutine matmul2d(matrix1, rows1, cols1, matrix2, rows2, cols2, matrix3)
        integer, intent(in) :: rows1, cols1, rows2, cols2
        double precision, intent(in) :: matrix1(rows1, cols1), matrix2(cols1, cols2)
        double precision, intent(out) :: matrix3(rows1, cols2)
        matrix3 = matmul(matrix1, matrix2)
    end subroutine

end program

program arrays_op_8
implicit none

call array_expr_01()

contains

subroutine array_expr_01()
    integer :: dim1, dim2, dim3, dim1d, i
    integer :: shape1d(1), shape3d(3)
    real(8) :: eps
    real(8) :: e(10, 10, 5), f(10, 10, 5)
    real(8) :: g(500), e1d(500), f1d(500)

    eps = 1e-12

    dim1 = 10
    dim2 = 10
    dim3 = 5
    dim1d = dim1 * dim2 * dim3

    do i = 1, dim1d
        e1d(i) = i + 1
        f1d(i) = i + 1
    end do

    shape3d(1) = dim1
    shape3d(2) = dim2
    shape3d(3) = dim3
    shape1d(1) = dim1d
    e = reshape(e1d, shape3d)
    f = reshape(f1d, shape3d)
    g = reshape(e + f, shape1d)

    do i = 1, dim1d
        if ( abs(g(i) - 2*(i + 1)) > eps ) error stop
    end do

end subroutine

end program

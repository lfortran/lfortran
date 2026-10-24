program gpu_metal_491
    ! A loop nest with integer(8) indices and a negative lower bound, which
    ! a CUDA launch runs in tiles of 32 by 8 iterations, the last ones
    ! partial (256 by 31).
    implicit none
    integer(8) :: w(-5:250, 31)
    integer(8) :: i8, j8
    integer :: i, j
    w = 0
    do concurrent (j8 = 1_8:31_8, i8 = -5_8:250_8)
        w(i8, j8) = w(i8, j8) + i8 + 1000_8 * j8
    end do
    do j = 1, 31
        do i = -5, 250
            if (w(i, j) /= i + 1000 * j) error stop 1
        end do
    end do
    print *, "ok"
end program

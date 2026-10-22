! With `--openmp` as well, a file that uses a module compiled separately
! leaves the loops of the module's procedures to the module: neither the
! offload nor the OpenMP pass lowers them here, and the calls here agree
! with the procedures the module's compilation built.
program gpu_separate_compilation_02
    use gpu_separate_compilation_02_mod, only: scale, shift, tri
    implicit none
    real :: x(6), y(6), z(5)
    integer :: j

    do j = 1, 6
        x(j) = j
    end do
    call scale(x, 3.0)
    call shift(x, y)
    do j = 1, 6
        if (x(j) /= 3.0 * j) error stop 1
        if (y(j) /= 4.0 * j) error stop 2
    end do

    do concurrent (j = 1:5)
        z(j) = tri(j)
    end do
    do j = 1, 5
        if (z(j) /= 36.0 * j) error stop 3
    end do
    print *, "ok"
end program

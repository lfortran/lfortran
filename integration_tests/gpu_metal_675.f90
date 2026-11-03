program gpu_metal_675
    ! Single loops with constant bounds whose iteration count is above the
    ! chunk limit (lfortran/lfortran#16569). The test is compiled with
    ! --gpu-chunk-limit=7, so the compile-time decision for constant extents
    ! must still split them: 100 iterations run as 14 chunks of 7 and a
    ! last one of 2, 7 iterations from a negative start fit in one, and a
    ! masked loop of 21 iterations runs as 3 chunks.
    implicit none
    integer :: i
    integer :: x(100), y(-3:3), z(0:20)

    x = 0
    do concurrent (i = 1:100)
        x(i) = 3 * i - 1
    end do
    do i = 1, 100
        if (x(i) /= 3 * i - 1) error stop 1
    end do

    y = 0
    do concurrent (i = -3:3)
        y(i) = i * i
    end do
    do i = -3, 3
        if (y(i) /= i * i) error stop 2
    end do

    z = -1
    do concurrent (i = 0:20, mod(i, 3) == 0)
        z(i) = i
    end do
    do i = 0, 20
        if (mod(i, 3) == 0) then
            if (z(i) /= i) error stop 3
        else
            if (z(i) /= -1) error stop 4
        end if
    end do
    print *, "ok"
end program

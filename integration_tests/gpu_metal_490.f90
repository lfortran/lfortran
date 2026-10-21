module gpu_metal_490_m
    implicit none
contains
    pure real function triangle(n) result(s)
        integer, intent(in) :: n
        real :: t(n)
        integer :: k
        do k = 1, n
            t(k) = real(k)
        end do
        s = sum(t)
    end function
end module

program gpu_metal_490
    ! A per-thread workspace whose extent depends on the loop indices, in
    ! nests that a CUDA launch runs in tiles (62 by 62 and 60 by 30 by 3,
    ! lower bounds other than one, partial tiles) and flat (37 by 11, which
    ! whole tiles would pad too much): the host sizes the workspace by the
    ! largest extent over the threads of either numbering.
    use gpu_metal_490_m
    implicit none
    real :: r(-2:59, 3:64), v(60, 30, 3), q(37, 11)
    integer :: i, j, k
    r = -1
    do concurrent (j = 3:64, i = -2:59)
        r(i, j) = triangle(i + j + 3)
    end do
    v = -1
    do concurrent (i = 1:60, j = 1:30, k = 1:3)
        v(i, j, k) = triangle(i + 2 * j + k)
    end do
    q = -1
    do concurrent (j = 1:11, i = 1:37)
        q(i, j) = triangle(i + j)
    end do
    do j = 3, 64
        do i = -2, 59
            if (abs(r(i, j) - real((i + j + 3) * (i + j + 4) / 2)) > 1e-2) &
                error stop 1
        end do
    end do
    do k = 1, 3
        do j = 1, 30
            do i = 1, 60
                if (abs(v(i, j, k) - real((i + 2 * j + k) &
                        * (i + 2 * j + k + 1) / 2)) > 1e-2) error stop 2
            end do
        end do
    end do
    do j = 1, 11
        do i = 1, 37
            if (abs(q(i, j) - real((i + j) * (i + j + 1) / 2)) > 1e-3) &
                error stop 3
        end do
    end do
    print *, "ok"
end program

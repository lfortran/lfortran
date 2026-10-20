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
    ! nests that a CUDA launch runs in tiles (64 by 40) and flat (37 by 11,
    ! which whole tiles would pad too much): the host sizes the workspace
    ! by the largest extent over the threads of either numbering.
    use gpu_metal_490_m
    implicit none
    real :: r(64, 40), q(37, 11)
    integer :: i, j
    r = -1
    do concurrent (j = 1:40, i = 1:64)
        r(i, j) = triangle(i + j)
    end do
    q = -1
    do concurrent (j = 1:11, i = 1:37)
        q(i, j) = triangle(i + j)
    end do
    do j = 1, 40
        do i = 1, 64
            if (abs(r(i, j) - real((i + j) * (i + j + 1) / 2)) > 1e-3) &
                error stop 1
        end do
    end do
    do j = 1, 11
        do i = 1, 37
            if (abs(q(i, j) - real((i + j) * (i + j + 1) / 2)) > 1e-3) &
                error stop 2
        end do
    end do
    print *, "ok"
end program

program gpu_metal_690
    ! Array section assignments, and sums of elemental expressions of
    ! sections, inside an offloaded loop, compiled with
    ! --descriptor-index-64, so the kernels compute element offsets in
    ! 64 bits. The loops the offload passes build for them have to count
    ! the section's extent, and compute its subscripts, in integer(8) too
    ! (#16571): a stride of 2**32 + 1 makes a one-element section of seven
    ! candidates, and a lower bound 2**32 above the upper bound an empty
    ! one, both of which integer(4) arithmetic gets wrong; the lower
    ! bounds 2147483645 and 6442450941 make the subscripts cross huge(0)
    ! and 2**32 + huge(0), for sections and for a whole allocatable alike.
    ! The arrays are small, so the test runs on the device (cuda_cpu in CI)
    ! and on the host.
    implicit none
    real, allocatable :: z(:,:), v(:,:), w(:,:), u(:,:), r(:)
    real :: s(2), t(2), rt(2)
    integer :: j, k
    integer(8) :: i8, lo8, n8, big, los(4)
    los = [1_8, 2147483645_8, 6442450941_8, -3000000000_8]
    big = 4294967296_8
    n8 = 7
    do k = 1, 4
        lo8 = los(k)
        allocate(z(lo8:lo8 + n8 - 1, 2), v(lo8:lo8 + n8 - 1, 2))
        allocate(w(lo8:lo8 + n8 - 1, 2), u(lo8:lo8 + n8 - 1, 2))
        allocate(r(lo8:lo8 + n8 - 1))
        do j = 1, 2
            do i8 = lo8, lo8 + n8 - 1
                z(i8, j) = real(i8 - lo8 + 1 + 10*(j - 1))
                r(i8) = real(i8 - lo8 + 1)
            end do
        end do
        v = -1
        w = -1
        u = -1
        do concurrent (j = 1:2)
            ! The single element v(lo8, j).
            v(lo8:lo8 + 6:big + 1, j) = 5
            ! No element at all.
            v(lo8 + big:lo8 + 3, j) = 9
            ! w(lo8, j) = z(lo8 + 6, j) + 1, one element each side.
            w(lo8:lo8 + 6:big + 1, j) = z(lo8 + 6:lo8:-big - 1, j) + 1
            ! Every element, the subscripts crossing huge(0).
            u(:, j) = z(:, j)*2
            u(lo8 + 1:lo8 + 5:2, j) = z(lo8 + 6:lo8 + 2:-2, j) + 100
            s(j) = sum(z(lo8:lo8 + 6:big + 1, j)*2)
            t(j) = sum(z(lo8 + big:lo8 + 3, j)*2)
            ! A whole allocatable.
            rt(j) = j*sum(r)
            z(:, j) = z(:, j) + r*j
        end do
        print *, lo8, v(:, 1), w(:, 2), u(:, 2), s, t, rt, z(:, 2)
        do j = 1, 2
            if (v(lo8, j) /= 5) error stop 1
            if (any(v(lo8 + 1:, j) /= -1)) error stop 2
            if (w(lo8, j) /= real(7 + 10*(j - 1)) + 1) error stop 3
            if (any(w(lo8 + 1:, j) /= -1)) error stop 4
            do i8 = lo8, lo8 + n8 - 1
                if (i8 == lo8 + 1 .or. i8 == lo8 + 3 .or. i8 == lo8 + 5) then
                    if (u(i8, j) /= real(lo8 + 8 - i8 + 10*(j - 1)) + 100) &
                        error stop 5
                else
                    if (u(i8, j) /= real(2*(i8 - lo8 + 1 + 10*(j - 1)))) &
                        error stop 6
                end if
                if (z(i8, j) /= real((i8 - lo8 + 1)*(j + 1) + 10*(j - 1))) &
                    error stop 9
            end do
        end do
        if (abs(s(1) - 2) > 1e-4 .or. abs(s(2) - 22) > 1e-4) error stop 7
        if (any(t /= 0)) error stop 8
        if (abs(rt(1) - 28) > 1e-4 .or. abs(rt(2) - 56) > 1e-4) error stop 10
        deallocate(z, v, w, u, r)
    end do
end program

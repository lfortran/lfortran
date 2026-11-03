program gpu_metal_692
    ! Whole-array assignments to arrays the device holds run on the device,
    ! as a loop over the elements. Compiled with --descriptor-index-64 the
    ! kernels compute element offsets in 64 bits, so that loop counts its
    ! indices and computes the subscripts in integer(8) too (#16571): the
    ! lower bounds 2147483645, 6442450941 and -3000000000 do not fit
    ! integer(4). The arrays are small, so the test runs on the device
    ! (cuda_cpu in CI) and on the host.
    implicit none
    real, allocatable :: v(:,:), w(:,:), s(:)
    integer(8) :: lo8, los(4), i8
    integer :: j, k
    los = [1_8, 2147483645_8, 6442450941_8, -3000000000_8]
    do k = 1, 4
        lo8 = los(k)
        allocate(v(lo8:lo8 + 6, 2), w(lo8 - 3:lo8 + 3, 3:4), s(lo8:lo8 + 1))
        v = -1
        w = 2
        s = 0
        do concurrent (j = 1:2)
            v(lo8 + j, j) = 7
            w(lo8 + j - 3, j + 2) = 5
            s(lo8 + j - 1) = 1
        end do
        v = v + 2*w
        s = s + 2*s
        print *, lo8, s
        do j = 1, 2
            do i8 = lo8, lo8 + 6
                if (i8 == lo8 + j) then
                    if (v(i8, j) /= 17) error stop 1
                else
                    if (v(i8, j) /= 3) error stop 2
                end if
            end do
            if (s(lo8 + j - 1) /= 3) error stop 3
        end do
        deallocate(v, w, s)
    end do
    print *, "ok"
end program

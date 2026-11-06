module gpu_metal_601_m
    implicit none
contains
    pure real function f(a)
        real, intent(in) :: a(:)
        integer :: i
        f = 0
        do i = 1, size(a)
            f = f + a(i)
        end do
    end function
end module

program gpu_metal_601
    ! Array sections and sum() inside offloaded loops over arrays whose
    ! lower bound, 3000000000, is beyond huge(0), compiled with
    ! --descriptor-index-64, so the kernels compute element offsets in
    ! 64 bits. The offload passes count the subscripts of the array
    ! expressions they lower inside a kernel in integer(8) from the lower
    ! bound too (#16571).
    ! A section with one bound omitted (z(lo8 + 3:, j), z(:lo8 + 3, j))
    ! needs the omitted bound in integer(8): in integer(4) it is
    ! truncated, while the explicit bound beside it is exact. The last
    ! part sums sections of arrays whose bounds cross huge(0) (or
    ! huge(0) + 2**32): their loop's last index needs integer(8) even
    ! where the lower bound fits integer(4). The arrays are small, so the
    ! test runs on the device (cuda_cpu in CI) and on the host.
    use gpu_metal_601_m, only: f
    implicit none
    real, allocatable :: z(:,:), v(:,:), w(:,:), r(:)
    real :: tot(2), wt(2), rt(2), t1(2), t2(2), t3(2)
    integer :: j, k
    integer(8) :: i8, lo8, n8, los(2)
    lo8 = 3000000000_8
    n8 = 7
    allocate(z(lo8:lo8 + n8 - 1, 2), v(lo8:lo8 + n8 - 1, 2))
    allocate(r(lo8:lo8 + n8 - 1))
    do j = 1, 2
        do i8 = lo8, lo8 + n8 - 1
            z(i8, j) = real(i8 - lo8 + 10*j)
            r(i8) = real(i8 - lo8 + 1)
        end do
    end do
    do concurrent (j = 1:2)
        v(:, j) = z(:, j) + 1
    end do
    print *, v(lo8, 1), v(lo8 + n8 - 1, 2)
    do j = 1, 2
        do i8 = lo8, lo8 + n8 - 1
            if (v(i8, j) /= real(i8 - lo8 + 10*j + 1)) error stop 1
        end do
    end do
    do concurrent (j = 1:2)
        v(lo8:lo8 + 2, j) = 0
        tot(j) = sum(z(:, j))
        rt(j) = j*sum(r)
    end do
    print *, tot, rt
    do j = 1, 2
        do i8 = lo8, lo8 + n8 - 1
            if (i8 <= lo8 + 2) then
                if (v(i8, j) /= 0) error stop 2
            else
                if (v(i8, j) /= real(i8 - lo8 + 10*j + 1)) error stop 3
            end if
        end do
    end do
    if (abs(tot(1) - 91) > 1e-4 .or. abs(tot(2) - 161) > 1e-4) error stop 4
    if (abs(rt(1) - 28) > 1e-4 .or. abs(rt(2) - 56) > 1e-4) error stop 7
    deallocate(r)
    do concurrent (j = 1:2)
        t1(j) = sum(z(lo8 + 3:, j))
        t2(j) = sum(z(:lo8 + 3, j))
        t3(j) = f(z(:lo8 + 1, j))
    end do
    print *, t1, t2, t3
    if (abs(t1(1) - 58) > 1e-4 .or. abs(t1(2) - 98) > 1e-4) error stop 8
    if (abs(t2(1) - 46) > 1e-4 .or. abs(t2(2) - 86) > 1e-4) error stop 9
    if (abs(t3(1) - 21) > 1e-4 .or. abs(t3(2) - 41) > 1e-4) error stop 10
    los = [2147483645_8, 6442450941_8]
    do k = 1, 2
        lo8 = los(k)
        allocate(w(lo8:lo8 + n8 - 1, 2))
        do i8 = lo8, lo8 + n8 - 1
            do j = 1, 2
                w(i8, j) = real(i8 - lo8 + 10*j)
            end do
        end do
        do concurrent (j = 1:2)
            wt(j) = sum(w(:, j))
            t1(j) = sum(w(lo8 + 3:, j))
        end do
        print *, wt, t1
        if (abs(wt(1) - 91) > 1e-4 .or. abs(wt(2) - 161) > 1e-4) error stop 5
        if (abs(t1(1) - 58) > 1e-4 .or. abs(t1(2) - 98) > 1e-4) error stop 6
        deallocate(w)
    end do
end program

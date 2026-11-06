module gpu_metal_696_m
    implicit none
contains
    ! Fusion of the two loops renames f in the second one to a copy, and
    ! the time steps alternate between f and the copy, which is allocated
    ! with the bounds of f and filled from it on the device.
    subroutine fused(q, f, lo, n, steps)
        real, allocatable, intent(inout) :: q(:), f(:)
        integer(8), intent(in) :: lo, n
        integer, intent(in) :: steps
        integer(8) :: i
        integer :: step
        do step = 1, steps
            do concurrent (i = lo:lo + n - 1)
                f(i) = 2 * q(i)
            end do
            do concurrent (i = lo + 1:lo + n - 1)
                q(i) = q(i) + f(i) - f(i - 1)
            end do
        end do
    end subroutine
end module

program gpu_metal_696
    ! Compiled with --descriptor-index-64, the copy fusion makes of an
    ! array has to get the array's bounds in integer(8), and the device
    ! loop that fills it has to count in integer(8) (#16571): the lower
    ! bounds 2147483645, 3000000000 and 6442450941 do not fit integer(4).
    ! The arrays are small, so the test runs on the device (cuda_cpu in
    ! CI) and on the host.
    use gpu_metal_696_m
    implicit none
    real, allocatable :: q(:), f(:)
    real :: qq(8), ff(8)
    integer(8) :: lo8, los(4), i, n
    integer :: k, s, m
    los = [1_8, 2147483645_8, 3000000000_8, 6442450941_8]
    n = 8
    do m = 1, 8
        qq(m) = real(m)
    end do
    do s = 1, 2
        ff = 2 * qq
        do m = 2, 8
            qq(m) = qq(m) + ff(m) - ff(m - 1)
        end do
    end do
    do k = 1, 4
        lo8 = los(k)
        allocate(q(lo8:lo8 + n - 1), f(lo8:lo8 + n - 1))
        do i = 1, n
            q(lo8 + i - 1) = real(i)
        end do
        f = 0
        call fused(q, f, lo8, n, 2)
        print *, lo8, q
        if (lbound(q, 1, kind=8) /= lo8) error stop 1
        if (lbound(f, 1, kind=8) /= lo8) error stop 2
        do i = 1, n
            if (abs(q(lo8 + i - 1) - qq(i)) > 1e-3) error stop 3
            if (abs(f(lo8 + i - 1) - ff(i)) > 1e-3) error stop 4
        end do
        deallocate(q, f)
    end do
    print *, "ok"
end program

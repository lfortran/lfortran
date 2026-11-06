program gpu_metal_693
    ! An array of LOCAL locality whose bounds are known only at run time
    ! is privatized: each thread gets a copy with the bounds of the array,
    ! which the offload pass reads before the kernel runs. Compiled with
    ! --descriptor-index-64 the kernels compute element offsets in 64
    ! bits, so those bounds are read in integer(8) too (#16571): the lower
    ! bounds 2147483645, 3000000000 and -3000000000 do not fit integer(4).
    ! The arrays are small, so the test runs on the device (cuda_cpu in
    ! CI) and on the host.
    implicit none
    integer(8) :: los(4)
    integer :: k, nfail
    los = [1_8, 2147483645_8, 3000000000_8, -3000000000_8]
    nfail = 0
    do k = 1, 4
        call run(los(k), nfail)
    end do
    if (nfail > 0) error stop 1
    print *, "ok"
contains
    subroutine run(lo, nfail)
        integer(8), intent(in) :: lo
        integer, intent(inout) :: nfail
        real :: t(lo:lo + 3, 2), r(6)
        integer(8) :: i8
        integer :: j
        t = 0
        r = 0
        do concurrent (j = 1:6) local(t)
            do i8 = lo, lo + 3
                t(i8, 1) = j + (i8 - lo)
                t(i8, 2) = 10*j
            end do
            r(j) = t(lo, 1) + t(lo + 3, 1) + t(lo + 1, 2)
        end do
        print *, lo, r
        do j = 1, 6
            if (r(j) /= 12*j + 3) nfail = nfail + 1
        end do
    end subroutine
end program

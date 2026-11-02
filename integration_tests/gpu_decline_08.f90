! gpu decline reason: ScalarTypeWidth
! a scalar kernel argument is numeric but of a kind the device has no scalar
! type of the same width for.
program gpu_decline_08
    implicit none
    logical(8) :: k
    integer :: a(4), i
    k = .true._8
    do concurrent (i = 1:4)
        a(i) = i * merge(3, 0, k)
    end do
    do i = 1, 4
        if (a(i) /= 3*i) error stop "bad a"
    end do
end program

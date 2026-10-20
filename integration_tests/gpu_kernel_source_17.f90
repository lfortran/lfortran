program gpu_kernel_source_17
    ! A centred difference with one-sided rows at both ends: the nest runs
    ! as one launch, the branch-free interior with the frame's two
    ! iterations packed in front of it.
    implicit none
    integer, parameter :: n = 10
    real :: a(n), d(n)
    integer :: i
    do i = 1, n
        a(i) = real(i * i)
    end do
    do concurrent (i = 1:n)
        if (i == 1) then
            d(i) = a(2) - a(1)
        else if (i == n) then
            d(i) = a(n) - a(n - 1)
        else
            d(i) = 0.5 * (a(i + 1) - a(i - 1))
        end if
    end do
    if (abs(d(1) - 3) > 1e-6 .or. abs(d(n) - 19) > 1e-6) error stop 1
    do i = 2, n - 1
        if (abs(d(i) - 2 * i) > 1e-5) error stop 2
    end do
    print *, "ok"
end program

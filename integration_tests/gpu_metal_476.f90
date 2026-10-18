program gpu_metal_476
implicit none
integer, parameter :: m = 150000, n = 4
real :: t(m), y(n)
integer :: j, k, mutable_size

! Too large for either device stack, but only 2.4 MB of global workspace.
do concurrent (j = 1:n)
    do k = 1, m
        t(k) = j + k
    end do
    y(j) = t(1) + t(m)
end do
do j = 1, n
    if (y(j) /= 2 * j + 1 + m) error stop 1
end do
call runtime_private(128, 4)
mutable_size = 128
call captured_private(mutable_size, 4)
if (mutable_size /= 64) error stop 3
print *, "ok"
contains
    subroutine runtime_private(m, n)
        integer, intent(in) :: m, n
        real :: t(m), y(n)
        integer :: j
        do concurrent (j = 1:n)
            t = j
            y(j) = sum(t) + t(1) + t(m)
        end do
        do j = 1, n
            if (y(j) /= (m + 2) * j) error stop 2
        end do
    end subroutine
    subroutine captured_private(m, n)
        integer, intent(inout) :: m
        integer, intent(in) :: n
        real :: t(m), y(n)
        integer :: j
        m = m / 2
        ! The shape was established on entry, before m changed.
        do concurrent (j = 1:n)
            t = j
            y(j) = sum(t)
        end do
        do j = 1, n
            if (y(j) /= 2 * m * j) error stop 4
        end do
    end subroutine
end program

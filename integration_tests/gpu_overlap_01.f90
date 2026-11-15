! Chained kernels of one stream with --gpu-overlap: each loop reads what the
! one before wrote, so a kernel that began before its predecessor finished
! would read stale values. Many short, dependent launches make that likely.
program gpu_overlap_01
implicit none
integer, parameter :: n = 200000, steps = 200
real(4), allocatable :: a(:), b(:)
integer :: i, s
allocate(a(n), b(n))
do concurrent (i = 1:n)
    a(i) = 0
    b(i) = 0
end do
do s = 1, steps
    do concurrent (i = 1:n)
        b(i) = a(i) + 1
    end do
    do concurrent (i = 1:n)
        a(i) = b(i)
    end do
end do
do i = 1, n
    if (a(i) /= real(steps)) error stop
end do
print *, a(1), a(n)
end program

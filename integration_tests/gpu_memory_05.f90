program gpu_memory_05
! Small allocatable arrays (8 KB), which a runtime puts in memory the host and
! the device share too (the managed memory model): the host writes an
! array between two kernels that read it, reads single elements of what a
! kernel wrote, grows an array by reallocation on assignment, moves one
! with move_alloc, and frees one a pending kernel writes.
implicit none
integer, parameter :: dp = kind(0.d0), n = 2**10
real(dp), allocatable :: a(:), b(:), c(:), d(:)
integer :: i
allocate(a(n), b(n), c(n))
do i = 1, n
    a(i) = i
    b(i) = 1
end do
do concurrent (i = 1:n)
    c(i) = a(i) + b(i)
end do
if (c(1) /= 2 .or. c(n) /= n + 1) error stop "first kernel"
! The host changes what the next kernel reads.
do i = 1, n
    b(i) = 2*i
end do
do concurrent (i = 1:n)
    c(i) = c(i) + b(i)
end do
if (c(7) /= 7 + 1 + 14) error stop "host write seen"
do i = 1, n
    if (c(i) /= 3*real(i, dp) + 1) error stop "second kernel"
end do
! Reallocation on assignment, then a kernel over the larger array.
a = [a, a]
do concurrent (i = 1:2*n)
    a(i) = 2*a(i)
end do
if (size(a) /= 2*n .or. a(n + 3) /= 6) error stop "grown array"
call move_alloc(a, d)
do concurrent (i = 1:2*n)
    d(i) = d(i) + 1
end do
if (allocated(a) .or. d(2*n) /= 2*n + 1) error stop "moved array"
! Freed while the kernel that writes it may still run.
do concurrent (i = 1:n)
    c(i) = -1
end do
deallocate(c)
allocate(c(n))
do concurrent (i = 1:n)
    c(i) = b(i)
end do
if (c(n) /= 2*n) error stop "reallocated array"
print *, "ok"
end program

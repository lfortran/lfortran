program gpu_memory_01
! What the host may do with the arrays of a launch, under either memory model
! of a GPU runtime (copies in device memory, or host memory used in place):
! change an array a kernel only reads right after the launch, read what a
! kernel wrote, hand it to another kernel, and free an array a kernel wrote.
! Arrays both larger and smaller than the size up to which a runtime that
! uses host memory in place still copies an array a kernel only reads.
implicit none
integer, parameter :: dp = kind(0.d0)
integer, parameter :: n = 2**22, m = 1000
real(dp), allocatable :: a(:), b(:), c(:), d(:), e(:)
real(dp) :: s(m), t(m)
integer :: i

allocate(a(n), b(n), c(n), d(n))
do i = 1, n
    a(i) = i
    b(i) = 2*i
end do
do i = 1, m
    s(i) = i
end do

do concurrent (i = 1:n)
    c(i) = a(i) + b(i)
end do
! The kernel only reads a and s: the host may change them right away.
a = -1
do concurrent (i = 1:m)
    t(i) = 3*s(i)
end do
s = -1
! A second kernel reads what the first one wrote.
do concurrent (i = 1:n)
    d(i) = 2*c(i)
end do

do i = 1, n
    if (abs(c(i) - 3*real(i, dp)) > 0) error stop "c"
    if (abs(d(i) - 6*real(i, dp)) > 0) error stop "d"
end do
do i = 1, m
    if (abs(t(i) - 3*real(i, dp)) > 0) error stop "t"
end do
if (any(a /= -1) .or. any(s /= -1)) error stop "host writes"

! A kernel writes an array the host then frees without reading it.
allocate(e(n))
do concurrent (i = 1:n)
    e(i) = b(i) - 1
end do
deallocate(e)
do concurrent (i = 1:n)
    c(i) = b(i) + 1
end do
if (abs(c(n) - (2*real(n, dp) + 1)) > 0) error stop "c after free"
print *, "ok"
end program

! The launch of a loop that assigns every element of `a` says when the
! kernel overwrites it -- when the loop runs over exactly its bounds -- so
! that it need not be copied to the device first. `b` is read before it is
! assigned, so the launch says nothing of it.
program gpu_kernel_source_35
implicit none
real, allocatable :: a(:), b(:)
integer :: i, n
n = 10
allocate(a(n), b(n))
b = 1
do concurrent (i = 1:n)
    a(i) = 2 * real(i)
    b(i) = b(i) + a(i)
end do
if (any(a /= [(2 * real(i), i = 1, n)])) error stop 1
if (any(b /= [(1 + 2 * real(i), i = 1, n)])) error stop 2
print *, "ok"
end program

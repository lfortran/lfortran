! Checks the results of a vectorizable loop over local arrays stored on the
! stack, which are aligned to 16 bytes: explicit-shape local arrays and, under
! --fast, an allocatable array of constant size that is promoted to a stack
! array. The alignment itself is not observable here; the LLVM reference
! outputs of other tests show it.
program arrays_147
implicit none
integer, parameter :: n = 12, m = 5
real, allocatable :: g(:)
real :: w(n, m), v(m)
integer :: i, k
allocate(g(n))
do k = 1, m
    v(k) = real(k)
    do i = 1, n
        w(i, k) = real(i - k)
    end do
end do
g = 0
do k = 1, m
    do i = 1, n
        g(i) = g(i) + w(i, k) * v(k)
    end do
end do
do i = 1, n
    if (abs(g(i) - real(15*i - 55)) > 1e-5) error stop
end do
print *, sum(g)
if (abs(sum(g) - 510.0) > 1e-4) error stop
end program

! A module procedure, compiled separately from the loop that calls it,
! whose loop bounds and local arrays, and those of the procedures it
! contains, use named constants of its module that the calling program
! does not see, or named constants of their own.
program gpu_metal_489
use gpu_metal_489_m, only: block_sums
implicit none
integer, parameter :: nx = 100, ny = 61, nb = (ny - 2) / 3
real, allocatable :: q(:,:), o(:,:)
real :: expected
integer :: i, j, jb
allocate(q(nx, ny), o(nx, ny))
do j = 1, ny
    do i = 1, nx
        q(i, j) = i + 0.5 * j * j
    end do
end do
o = 0
do concurrent (i = 2:nx, jb = 0:nb - 1)
    call block_sums(q, i, 2 + jb * 3, o(i, 2 + jb * 3:4 + jb * 3))
end do
do j = 2, 1 + 3 * nb
    do i = 2, nx
        expected = (q(i, j) + q(i, j + 1)) - (q(i, j - 1) + q(i, j)) &
            + q(i - 1, j)
        if (abs(o(i, j) - expected) > 1e-3 * abs(expected)) error stop 1
    end do
end do
if (any(o(:, 2 + 3 * nb:) /= 0)) error stop 2
if (any(o(1, :) /= 0)) error stop 3
print *, "ok"
end program gpu_metal_489

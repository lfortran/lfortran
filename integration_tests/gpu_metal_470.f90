! The temporary of an overlapping array assignment in a `do concurrent`
! belongs to the iteration, also when its extent is a constant.
!
! `a(:,j) = a(n:1:-1,j)` reads what it writes, so it is assigned through a
! temporary. A temporary declared in the procedure holding the loop would be
! one array shared by every thread of the kernel: with many iterations
! running at once, a column would end up holding another column's numbers.
program gpu_metal_470
implicit none
integer, parameter :: n = 64, m = 4000
real :: a(n, m)
integer :: i, j
do j = 1, m
    do i = 1, n
        a(i, j) = real(1000*j + i)
    end do
end do
do concurrent (j = 1:m)
    a(:, j) = a(n:1:-1, j)
end do
do j = 1, m
    do i = 1, n
        if (a(i, j) /= real(1000*j + n + 1 - i)) error stop "column mixed up"
    end do
end do
print *, "ok"
end program

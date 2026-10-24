! A parallel loop whose iterations do nothing -- an empty body, or a body
! of loops that are themselves empty -- is not given a kernel: only the one
! loop that assigns `y` is launched.
program gpu_kernel_source_36
implicit none
integer :: i, j, n
real :: x(1000), y(1000)
n = size(x)
x = 1
y = 0
do concurrent (i = 1:size(x))
end do
do concurrent (i = 1:n, j = 1:n)
end do
do concurrent (i = 1:n)
    do concurrent (j = 1:n)
    end do
end do
do concurrent (i = 1:n)
    do j = 1, 3
    end do
end do
do concurrent (i = 1:n)
    y(i) = 2 * x(i)
end do
if (abs(sum(x) - 1000) > 1e-3) error stop 1
if (abs(sum(y) - 2000) > 1e-3) error stop 2
print *, sum(x), sum(y)
end program

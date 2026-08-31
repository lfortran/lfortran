! Fence for gpu_metal_216: the same array-valued ASSOCIATE inside a
! `do concurrent`, but over FIXED-SIZE operands. Those carry a static
! shape, so the selector temporary can be declared in the kernel and the
! loop keeps being offloaded. This must keep working unchanged.
program gpu_metal_217
implicit none
real :: x(3), y(2,3), g(3), h(2,3)
integer :: i, j

x = [1.0, 2.0, 3.0]
do j = 1, 3
    do i = 1, 2
        y(i,j) = real(10*i + j)
    end do
end do

g = 0.0
do concurrent (j = 1:3)
    associate (r => x*2.0)
        g(j) = r(j)
    end associate
end do

h = 0.0
do concurrent (j = 1:3)
    associate (r => y + 1.0)
        h(1,j) = r(1,j)
        h(2,j) = r(2,j)
    end associate
end do

do j = 1, 3
    if (abs(g(j) - 2.0*x(j)) > 1.0e-5) error stop "g"
    do i = 1, 2
        if (abs(h(i,j) - (y(i,j) + 1.0)) > 1.0e-5) error stop "h"
    end do
end do

print *, g(1), h(1,1)
print *, "ok"
end program

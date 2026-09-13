! Fence for the array-valued ASSOCIATE fix: a SCALAR associate name
! nested inside a `do concurrent` body is still inlined into the kernel,
! and an ASSOCIATE construct that ENCLOSES the loop still has its
! selector left in the host scope and passed in as a kernel buffer.
program gpu_metal_229
implicit none
real :: x(4), g(4,3), h(4,3)
integer :: i, j, k

x = [1.0, 2.0, 3.0, 4.0]
g = 0.0
h = 0.0

! scalar-valued associate nested inside the loop
do concurrent (k=1:3)
    associate(c => 2.0*real(k))
        g(:,k) = c*x
    end associate
end do

! array-valued associate ENCLOSING the loop
associate(w => 5.0*x)
    do concurrent (k=1:3)
        h(:,k) = w + real(k)
    end do
end associate

do j = 1, 3
    do i = 1, 4
        if (abs(g(i,j) - 2.0*real(j)*x(i)) > 1.0e-5) error stop
        if (abs(h(i,j) - (5.0*x(i) + real(j))) > 1.0e-5) error stop
    end do
end do

print *, g(2,2), h(2,2)
print *, "ok"
end program

! An ASSOCIATE construct nested inside a `do concurrent` body whose
! associate name is ARRAY valued materialises the selector into a
! temporary owned by the ASSOCIATE scope. That temporary must stay
! private to each loop iteration and travel into the generated GPU
! kernel together with the statements that use it.
program gpu_metal_214
implicit none
real :: x(4), y(3), g(4,3), h(4,3), p(4,3)
integer :: i, j, k

x = [1.0, 2.0, 3.0, 4.0]
y = [10.0, 20.0, 30.0]
g = 0.0
h = 0.0
p = 0.0

! loop-variant array-valued selector
do concurrent (k=1:3)
    associate(r => x + y(k))
        g(:,k) = 2.0*r
    end associate
end do

! loop-invariant array-valued selector
do concurrent (k=1:3)
    associate(s => 3.0*x)
        h(:,k) = s + real(k)
    end associate
end do

! array-valued associate nested inside another array-valued associate
do concurrent (k=1:3)
    associate(a => x + y(k))
        associate(b => 2.0*a + 1.0)
            p(:,k) = b - a
        end associate
    end associate
end do

do j = 1, 3
    do i = 1, 4
        if (abs(g(i,j) - 2.0*(x(i) + y(j))) > 1.0e-5) error stop
        if (abs(h(i,j) - (3.0*x(i) + real(j))) > 1.0e-5) error stop
        if (abs(p(i,j) - (x(i) + y(j) + 1.0)) > 1.0e-5) error stop
    end do
end do

print *, g(1,1), h(1,1), p(1,1)
print *, "ok"
end program

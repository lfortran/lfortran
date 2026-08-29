! An ASSOCIATE whose selector is a non-variable ARRAY expression is
! materialized by the compiler into a temporary held in the ASSOCIATE
! block's own symbol table. When a `do concurrent` inside that block uses
! the associate name, the selector expression is inlined at the use site,
! so the kernel body ends up referring to symbols owned by the enclosing
! ASSOCIATE scope. Those symbols are left behind by kernel extraction and
! must be passed into the kernel like any other outer-scope variable.
program gpu_metal_210
implicit none
real :: a(4), b(4)
integer :: ia(4), ib(4)
integer :: i

a = [1.0, 2.0, 3.0, 4.0]
b = 0.0

! Array-constant selector, used indexed inside the loop.
associate(c => [0.5, 0.25, 0.125])
    do concurrent (integer :: j = 1:4)
        b(j) = c(2) * a(j)
    end do
end associate
do i = 1, 4
    if (abs(b(i) - 0.25 * a(i)) > 1.0e-5) error stop "array constant selector"
end do

! Two distinct uses of the same array-constant selector.
b = 0.0
associate(c => [0.5, 0.25, 0.125])
    do concurrent (integer :: j = 1:4)
        b(j) = c(1) * a(j) + c(3)
    end do
end associate
do i = 1, 4
    if (abs(b(i) - (0.5 * a(i) + 0.125)) > 1.0e-5) error stop "two uses of selector"
end do

! Same shape with integers, and a nested ASSOCIATE around the loop.
ia = [10, 20, 30, 40]
ib = 0
associate(k => [1, 2, 3, 4])
    associate(m => ia + k)
        do concurrent (integer :: j = 1:4)
            ib(j) = m(j) + k(4)
        end do
    end associate
end associate
do i = 1, 4
    if (ib(i) /= ia(i) + i + 4) error stop "nested associate selectors"
end do

print *, "ok"
end program

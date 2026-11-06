! A device-local allocatable sized by `size()` of an assumed-shape dummy, in
! a pure function called from a `do concurrent` kernel (#15891). Under
! --descriptor-index-64 the size is integer(8) while the declared extents of
! the actual argument are integer(4).
program gpu_metal_614
implicit none
real :: v(3), m(2, 3), r(4), s(4)
integer :: i
v = 1
m = 2
do concurrent (i = 1:4)
    r(i) = f(v) + i
    s(i) = g(m)
end do
if (any(abs(r - [4, 5, 6, 7]) > 1e-6)) error stop
if (any(abs(s - 18) > 1e-6)) error stop
print *, r
print *, s
contains
    pure real function f(a)
    real, intent(in) :: a(:)
    real, allocatable :: w(:)
    allocate(w(size(a)))
    w = a
    f = sum(w)
    end function

    pure real function g(a)
    real, intent(in) :: a(:,:)
    real, allocatable :: w(:,:), z(:)
    allocate(w(size(a, 1), size(a, 2)))
    allocate(z(size(a)))
    w = a
    z = 1
    g = sum(w) + sum(z)
    end function
end program

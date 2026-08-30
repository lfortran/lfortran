! An array constructor holding an array-valued element (a user function or an
! intrinsic result) has no direct Metal representation, and the backend used
! to drop the whole expression, emitting a placeholder comment where the value
! belonged.  The constructor is now expanded into a sequence of element writes
! into the target, so both the element order and the element count matter.
program gpu_metal_222
implicit none

real :: a(3,4), b(3,4), c(4,4), d(3,4), e(2,4)
real :: v(2), m(2,2)
integer :: j

v = [10.0, 20.0]
m = reshape([1.0, 0.0, 0.0, 1.0], [2,2])

! Array-valued user function result as a constructor element.
a = 0.0
do concurrent (j = 1:4)
    a(:,j) = with_call(v)
end do
do j = 1, 4
    if (any(a(:,j) /= [0.0, 20.0, 40.0])) error stop "with_call"
end do

! Array-valued intrinsic (matmul) result as a constructor element.
b = 0.0
do concurrent (j = 1:4)
    b(:,j) = with_matmul(m, v)
end do
do j = 1, 4
    if (any(b(:,j) /= [-1.0, 10.0, 20.0])) error stop "with_matmul"
end do

! Scalars on both sides of the array element: order and count are checked.
c = 0.0
do concurrent (j = 1:4)
    c(:,j) = surrounded(v)
end do
do j = 1, 4
    if (any(c(:,j) /= [1.0, 20.0, 40.0, 2.0])) error stop "surrounded"
end do

! Fence: a constructor of scalars only must keep working.
d = 0.0
do concurrent (j = 1:4)
    d(:,j) = scalars_only()
end do
do j = 1, 4
    if (any(d(:,j) /= [3.0, 4.0, 5.0])) error stop "scalars_only"
end do

! Fence: a constructor whose only element is a whole local array.
e = 0.0
do concurrent (j = 1:4)
    e(:,j) = whole_array(v)
end do
do j = 1, 4
    if (any(e(:,j) /= [10.0, 20.0])) error stop "whole_array"
end do

print *, a(:,1)
print *, b(:,1)
print *, c(:,1)
print *, "ok"

contains

    pure function doubled(x) result(r)
        real, intent(in) :: x(:)
        real :: r(2)
        r(1) = 2 * x(1)
        r(2) = 2 * x(2)
    end function

    pure function with_call(x) result(r)
        real, intent(in) :: x(:)
        real :: r(3)
        r = [0.0, doubled(x)]
    end function

    pure function with_matmul(mm, x) result(r)
        real, intent(in) :: mm(2,2), x(:)
        real :: r(3)
        r = [-1.0, matmul(mm, x)]
    end function

    pure function surrounded(x) result(r)
        real, intent(in) :: x(:)
        real :: r(4)
        r = [1.0, doubled(x), 2.0]
    end function

    pure function scalars_only() result(r)
        real :: r(3)
        r = [3.0, 4.0, 5.0]
    end function

    pure function whole_array(x) result(r)
        real, intent(in) :: x(:)
        real :: r(2)
        r = [x]
    end function

end program

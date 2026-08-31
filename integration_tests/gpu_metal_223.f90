! A compiler-created array temporary whose extent comes from an ALLOCATE with
! a non-constant bound (`allocate(r(size(mm,1)))`) reached the Metal backend
! without any size: the caller's temporary was declared with a single element
! and the size arguments of the matmul call were emitted as placeholders.  The
! extent is now re-evaluated in terms of the actual arguments, both to size
! the caller's temporary and to pass the size down to the callee.
program gpu_metal_223
implicit none

real :: a(3,4), b(3,4)
real :: v(3), m(3,3)
integer :: j

v = [1.0, 2.0, 3.0]
m = reshape([2.0, 0.0, 0.0, &
             0.0, 3.0, 0.0, &
             0.0, 0.0, 4.0], [3,3])

! Assumed-shape operands into an unknown-extent (allocatable) result.
a = 0.0
do concurrent (j = 1:4)
    a(:,j) = assumed(m, v)
end do
do j = 1, 4
    if (any(a(:,j) /= [2.0, 6.0, 12.0])) error stop "assumed"
end do

! Fence: explicit-shape operands must keep working.
b = 0.0
do concurrent (j = 1:4)
    b(:,j) = explicit(m, v)
end do
do j = 1, 4
    if (any(b(:,j) /= [2.0, 6.0, 12.0])) error stop "explicit"
end do

print *, a(:,1)
print *, b(:,1)
print *, "ok"

contains

    pure function assumed(mm, x) result(r)
        real, intent(in) :: mm(:,:), x(:)
        real, allocatable :: r(:)
        allocate(r(size(mm, 1)))
        r = matmul(mm, x)
    end function

    pure function explicit(mm, x) result(r)
        real, intent(in) :: mm(3,3), x(3)
        real :: r(3)
        r = matmul(mm, x)
    end function

end program

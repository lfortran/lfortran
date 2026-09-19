module gpu_metal_355_mod
implicit none
contains
pure real function row_sum(x)
    real, intent(in) :: x(:)
    row_sum = sum(x)
end function
end module

program gpu_metal_355
! A row section of an array with run-time extents passed to a device
! function, whose extent is held in a local of the loop and so changes with
! the iteration. The gathered buffer is sized as the whole base dimension
! and the callee gets its leading part.
use gpu_metal_355_mod
implicit none
real, allocatable :: a(:,:)
real :: s(3)
integer :: i, j, n

allocate(a(3,5))
do i = 1, 3
    do j = 1, 5
        a(i,j) = 10 * i + j
    end do
end do
do concurrent (i = 1:3) local(n)
    n = 6 - i
    s(i) = row_sum(a(i,1:n))
end do
print *, s
do i = 1, 3
    if (s(i) /= sum(a(i,1:6-i))) error stop
end do
end program

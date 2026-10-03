! gpu decline reason: SectionLeadingExtentVaries
! A section passed to a procedure is copied into a contiguous per-thread
! buffer the host sizes before the launch. Here its first dimension, which
! is not its last, has an extent that depends on the loop index, so the
! buffer cannot be sized on the host.
module gpu_decline_10_mod
implicit none
contains
pure real function weighted_sum(x)
    real, intent(in) :: x(:,:)
    integer :: k, l
    weighted_sum = 0
    do l = 1, size(x, 2)
        do k = 1, size(x, 1)
            weighted_sum = weighted_sum + k * l * x(k, l)
        end do
    end do
end function
end module

program gpu_decline_10
use gpu_decline_10_mod
implicit none
real, allocatable :: a(:,:)
real :: s(3)
integer :: i, j
allocate(a(3,5))
do i = 1, 3
    do j = 1, 5
        a(i,j) = 10 * i + j
    end do
end do
do concurrent (i = 1:3)
    s(i) = weighted_sum(a(1:i,:))
end do
print *, s
do i = 1, 3
    if (s(i) /= weighted_sum(a(1:i,:))) error stop
end do
end program

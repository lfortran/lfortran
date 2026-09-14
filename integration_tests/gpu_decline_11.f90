! gpu decline reason: SectionLeadingExtentVaries
! A section passed to a procedure is copied into a contiguous per-thread
! buffer the host sizes before the launch. Here its first dimension, which
! is not its last, has an extent that depends on the index of a do
! concurrent nested in the offloaded one, so the buffer cannot be sized on
! the host.
module gpu_decline_11_mod
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

program gpu_decline_11
use gpu_decline_11_mod
implicit none
real, allocatable :: a(:,:)
real :: s(3,3)
integer :: i, j, k, l
allocate(a(3,5))
do k = 1, 3
    do l = 1, 5
        a(k,l) = 10 * k + l
    end do
end do
do concurrent (i = 1:3)
    do concurrent (j = 1:3)
        s(i,j) = weighted_sum(a(1:j,:)) + i
    end do
end do
print *, s
do k = 1, 3
    do l = 1, 3
        if (s(k,l) /= weighted_sum(a(1:l,:)) + k) error stop
    end do
end do
end program

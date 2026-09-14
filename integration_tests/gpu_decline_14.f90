! gpu decline reason: SectionCopyNotPlaceable
! A section passed to a procedure is copied into a contiguous per-thread
! buffer right before the statement that makes the call. A WHERE construct
! holds only assignments, so no copy can be placed inside it.
module gpu_decline_14_mod
implicit none
contains
pure real function row_sum(x)
    real, intent(in) :: x(:)
    row_sum = sum(x)
end function
end module

program gpu_decline_14
use gpu_decline_14_mod
implicit none
real, allocatable :: a(:,:)
real :: b(3,4), c(3,4)
integer :: i, k, l
allocate(a(3,5))
do k = 1, 3
    do l = 1, 5
        a(k,l) = 10 * k + l
    end do
end do
b = 1
c = 0
do concurrent (i = 1:3)
    where (b(i,:) > 0) c(i,:) = row_sum(a(i,1:i))
end do
print *, c
do k = 1, 3
    if (any(c(k,:) /= sum(a(k,1:k)))) error stop
end do
end program

! gpu decline reason: SectionCopyNotPlaceable
! A section passed to a procedure is copied into a contiguous per-thread
! buffer right before the statement that makes the call. In a FORALL the
! section's extent reads the FORALL index, which a copy placed before the
! statement does not have.
module gpu_decline_13_mod
implicit none
contains
pure real function row_sum(x)
    real, intent(in) :: x(:)
    row_sum = sum(x)
end function
end module

program gpu_decline_13
use gpu_decline_13_mod
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
j = 1000
do concurrent (i = 1:3)
    forall (j = 1:3) s(i,j) = row_sum(a(i,1:j))
end do
print *, s
do k = 1, 3
    do l = 1, 3
        if (s(k,l) /= sum(a(k,1:l))) error stop
    end do
end do
end program

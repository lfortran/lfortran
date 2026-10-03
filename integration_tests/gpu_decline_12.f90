! gpu decline reason: SectionCopyNotPlaceable
! A section passed to a procedure in a do while condition is copied into a
! contiguous per-thread buffer once, before the loop. That copy cannot
! serve a condition whose section changes in the loop, as here, where its
! extent reads n, which the body increments.
module gpu_decline_12_mod
implicit none
contains
pure real function row_sum(x)
    real, intent(in) :: x(:)
    row_sum = sum(x)
end function
end module

program gpu_decline_12
use gpu_decline_12_mod
implicit none
real, allocatable :: a(:,:)
real :: s(3)
integer :: i, k, l, n
allocate(a(3,5))
do k = 1, 3
    do l = 1, 5
        a(k,l) = 10 * k + l
    end do
end do
do concurrent (i = 1:3)
    n = 0
    do while (row_sum(a(i,1:n)) < 30)
        n = n + 1
    end do
    s(i) = n
end do
print *, s
if (s(1) /= 3 .or. s(2) /= 2 .or. s(3) /= 1) error stop
end program

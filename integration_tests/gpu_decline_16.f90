! gpu decline reason: HeapArrayUse
! `w` is allocated on the device heap, because the host cannot work out its
! shape, and passed to `total` for an allocatable dummy argument, which the
! procedure could allocate, reallocate or deallocate itself.
module gpu_decline_16_mod
implicit none
contains
pure real function total(r)
    real, allocatable, intent(in) :: r(:)
    total = 0
    if (allocated(r)) total = sum(r)
end function
end module

program gpu_decline_16
use gpu_decline_16_mod
implicit none
real :: x(3, 2), s(2)
integer :: k
x = reshape([1., 2., 3., 4., 5., 6.], [3, 2])
do concurrent (k = 1:2)
    block
        real, allocatable :: w(:)
        integer :: c
        c = count(x(:, k) > 2)
        allocate(w(c))
        w = 1
        s(k) = total(w)
    end block
end do
print *, s
if (any(s /= [1., 3.])) error stop
end program

! The copy of an ASSOCIATE selector that is an expression is unallocated
! where the construct starts, every time it starts, also in a loop: it is
! allocated without a test. A selector that is a variable is an alias.
module realloc_lhs_unallocated_04_m
implicit none
contains
pure function numbers(n) result(r)
    integer, intent(in) :: n
    real, allocatable :: r(:)
    integer :: i
    r = [(real(i), i = 1, n)]
end function
end module

program realloc_lhs_unallocated_04
use realloc_lhs_unallocated_04_m
implicit none
real, allocatable :: a(:), b(:)
real :: s
integer :: k

a = [1., 2., 3., 4.]
b = [4., 3., 2., 1.]
s = 0
do k = 1, 4
    ! An expression of another size in every iteration
    associate(x => a(1:k) + b(1:k), y => numbers(k))
        if (size(x) /= k .or. lbound(x, 1) /= 1) error stop 1
        if (any(x /= 5)) error stop 2
        if (size(y) /= k .or. y(k) /= k) error stop 3
        ! A nested construct with an expression of the outer names
        associate(z => 2*x + y)
            if (size(z) /= k .or. z(1) /= 11) error stop 4
            s = s + sum(z)
        end associate
    end associate
end do
if (abs(s - (11 + 23 + 36 + 50)) > 1e-4) error stop 5

! Left early, then entered again
do k = 1, 3
    associate(x => numbers(k + 1))
        if (k == 2) exit
        if (size(x) /= k + 1) error stop 6
    end associate
end do
associate(x => numbers(2))
    if (size(x) /= 2 .or. x(2) /= 2) error stop 7
end associate

! A variable as the selector is an alias of it
associate(x => a)
    x(1) = 10
end associate
if (a(1) /= 10) error stop 8
print *, "ok"
end program

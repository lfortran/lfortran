! gpu decline reason: SectionCopyNotPlaceable
! A section passed to a procedure in an arm of a conditional expression is
! copied into a contiguous per-thread buffer before the statement, under the
! same condition. Here the condition calls a procedure that is not pure, so
! it cannot be evaluated for the copy. Such a call is not Fortran inside a
! do concurrent, so this is a reference test rather than an integration test.
module gpu_decline_impure_condition_mod
implicit none
contains
pure real function row_sum(x)
    real, intent(in) :: x(:)
    row_sum = sum(x)
end function
integer function h(n)
    integer, intent(in) :: n
    h = n
end function
end module

program gpu_decline_impure_condition
use gpu_decline_impure_condition_mod
implicit none
real :: a(3,5), v(3)
integer :: i, k, l
do k = 1, 3
    do l = 1, 5
        a(k,l) = 10 * k + l
    end do
end do
v = 0
do concurrent (i = 1:3)
    v(i) = (h(i) <= 2 ? row_sum(a(i,:)) : 0.0)
end do
print *, v
end program

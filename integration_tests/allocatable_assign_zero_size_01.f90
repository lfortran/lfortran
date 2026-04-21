program allocatable_assign_zero_size_01
! Test that assigning a zero-size allocatable function result to a
! fixed-size array raises a runtime shape mismatch error. This is
! non-conformable per the Fortran standard; shapes must match for
! intrinsic array assignment.
implicit none

real :: d(2)

d = 1.0

! Assign zero-size allocatable function result to fixed-size array.
! This must trigger a runtime shape mismatch error (size 2 vs size 0).
d = f(0)

print *, d

contains

function f(n) result(r)
    integer, intent(in) :: n
    real, allocatable :: r(:)
    allocate(r(n))
    r = 0.0
end function

end program

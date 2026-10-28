! Array expression actual arguments for the C/C++ backends: the
! argument is materialized into a descriptor temporary before the call.
program arrays_138
implicit none
real :: b(3)
b = 2.0
call sub(b + 1.0)
contains
subroutine sub(x)
real :: x(3)
if (abs(x(2) - 3.0) > 1e-6) error stop 1
print *, x(2)
end subroutine sub
end program arrays_138

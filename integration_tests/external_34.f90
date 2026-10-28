! With implicit typing, a module function references an external function
! defined later in the same file, with no interface for it.
module external_34_mod
contains
    real function add_one(x)
    real, intent(in) :: x
    add_one = external_34_plus(x, 1.0)
    end function
end module

program external_34
use external_34_mod
implicit none
real :: y
y = add_one(2.0)
print *, y
if (abs(y - 3) > 1e-6) error stop 1
end program

real function external_34_plus(x, d)
implicit none
real, intent(in) :: x, d
external_34_plus = x + d
end function

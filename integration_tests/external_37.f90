! With --implicit-interface, a module references external procedures
! defined in this file through an implicit interface when their interfaces
! name an entity of a module that uses it.
module external_37_a
contains
    subroutine call_it(a, s)
        real, intent(out) :: a(2)
        real, intent(out) :: s
        call external_37_set(a)
        s = external_37_sum(a)
    end subroutine
end module

module external_37_b
use external_37_a
implicit none
integer :: m = 2
end module

subroutine external_37_set(a)
use external_37_b, only: m
implicit none
real, intent(out) :: a(m)
a = 7
end subroutine

real function external_37_sum(a)
use external_37_b, only: m
implicit none
real, intent(in) :: a(m)
external_37_sum = sum(a)
end function

program external_37
use external_37_a
implicit none
real :: a(2), s
call call_it(a, s)
print *, a, s
if (any(a /= 7)) error stop 1
if (s /= 14) error stop 2
end program

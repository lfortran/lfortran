! With --implicit-interface, a module references an external function
! defined in this file through an implicit interface when its interface
! names an entity of a module that uses it. The actuals of the reference
! are still checked against the definition.
module implicit_interface_external_definition_02_a
contains
    subroutine call_it(ia, s)
        integer, intent(in) :: ia(2)
        real, intent(out) :: s
        s = implicit_interface_external_definition_02_sum(ia)
    end subroutine
end module

module implicit_interface_external_definition_02_b
use implicit_interface_external_definition_02_a
implicit none
integer :: m = 2
end module

real function implicit_interface_external_definition_02_sum(a)
use implicit_interface_external_definition_02_b, only: m
implicit none
real, intent(in) :: a(m)
implicit_interface_external_definition_02_sum = sum(a)
end function

program implicit_interface_external_definition_02
use implicit_interface_external_definition_02_a
implicit none
real :: s
call call_it([1, 2], s)
print *, s
end program

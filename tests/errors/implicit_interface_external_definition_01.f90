! With --implicit-interface, a module references an external procedure
! defined in this file through an implicit interface when its interface
! names an entity of a module that uses it. The actuals of the call are
! still checked against the definition.
module implicit_interface_external_definition_01_a
contains
    subroutine call_it(ia)
        integer, intent(out) :: ia(2)
        call implicit_interface_external_definition_01_set(ia)
    end subroutine
end module

module implicit_interface_external_definition_01_b
use implicit_interface_external_definition_01_a
implicit none
integer :: m = 2
end module

subroutine implicit_interface_external_definition_01_set(a)
use implicit_interface_external_definition_01_b, only: m
implicit none
real, intent(out) :: a(m)
a = 7.5
end subroutine

program implicit_interface_external_definition_01
use implicit_interface_external_definition_01_a
implicit none
integer :: ia(2)
call call_it(ia)
print *, ia
end program

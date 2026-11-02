! With --implicit-interface, a module references an external procedure
! defined in this file through an implicit interface when its interface
! names an entity of a module that uses it. Such a reference cannot leave
! an optional argument of the definition absent.
module implicit_interface_external_definition_03_a
contains
    subroutine call_it(x)
        real, intent(inout) :: x(2)
        call implicit_interface_external_definition_03_add(x)
    end subroutine
end module

module implicit_interface_external_definition_03_b
use implicit_interface_external_definition_03_a
implicit none
integer :: m = 2
end module

subroutine implicit_interface_external_definition_03_add(a, k)
use implicit_interface_external_definition_03_b, only: m
implicit none
real, intent(inout) :: a(m)
integer, intent(in), optional :: k
if (present(k)) then
    a = a + k
else
    a = a + 1
end if
end subroutine

program implicit_interface_external_definition_03
use implicit_interface_external_definition_03_a
implicit none
real :: x(2)
x = 1
call call_it(x)
print *, x
end program

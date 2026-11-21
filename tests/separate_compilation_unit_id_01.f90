! The getter of `n` is named with the ID of the compilation unit. The ID is
! derived from the source file, so the name is the same in every compilation.
module separate_compilation_unit_id_01_a
implicit none
integer :: n = 4
end module

module separate_compilation_unit_id_01_b
use separate_compilation_unit_id_01_a, only: n
implicit none
contains
    subroutine scale(x)
    real, intent(inout) :: x(n)
    x = 2.0*x
    end subroutine
end module

program separate_compilation_unit_id_01
use separate_compilation_unit_id_01_b, only: scale
implicit none
real :: x(4)
x = 1.0
call scale(x)
if (any(abs(x - 2.0) > 1e-6)) error stop
print *, x
end program

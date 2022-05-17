module submodule_03_mod
    implicit none
    interface
        module integer function f(a) result(r)
        integer, intent(in) :: a
        end function
    end interface
end module


submodule (submodule_03_mod) submodule_03_submod
contains
    module integer function f(a) result(r)
    integer, intent(in) :: a
    r = a + 1
    end function
end submodule

program submodules_03_prog
use submodule_03_mod, only: f
implicit none
integer :: i
i = f(5)
print *, i
if (i /= 6) error stop
end program

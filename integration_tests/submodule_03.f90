module submodule_03_mod
    implicit none
    interface
        module function f(a) result(r)
        integer, intent(in) :: a
        integer :: r
        end function
    end interface
end module


submodule (submodule_03_mod) submodule_03_submod
contains
    module function f(a) result(r)
    integer, intent(in) :: a
    integer :: r
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

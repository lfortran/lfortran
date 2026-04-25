submodule(submodule_29_mod:submodule_29_child) submodule_29_grandchild
    implicit none
contains
    subroutine helper(x, y)
        integer, intent(in) :: x
        integer, intent(out) :: y
        y = x + multiplier
    end subroutine
end submodule

module submodule_28_mod
    implicit none

    interface
        module subroutine compute(x, y)
            integer, intent(in) :: x
            integer, intent(out) :: y
        end subroutine
    end interface
end module

submodule(submodule_28_mod) submodule_28_child
    implicit none
contains
    module procedure compute
        y = helper(x)
    end procedure

    function helper(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x * 3
    end function
end submodule

submodule(submodule_28_mod:submodule_28_child) submodule_28_grandchild
    implicit none
contains
    subroutine compute_via_helper(x, y)
        integer, intent(in) :: x
        integer, intent(out) :: y
        y = helper(x)
    end subroutine
end submodule

program submodule_28
    use submodule_28_mod, only: compute
    implicit none
    integer :: x, y

    x = 5
    call compute(x, y)
    if (y /= 15) error stop
    print *, "ok"
end program

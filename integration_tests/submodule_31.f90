module submodule_31_mod
    implicit none

    interface
        module subroutine greet(x)
            integer, intent(inout) :: x
        end subroutine

        module subroutine caller(x)
            integer, intent(inout) :: x
        end subroutine
    end interface
end module

submodule(submodule_31_mod) submodule_31_impl
    implicit none
contains
    module procedure greet
        x = x + 10
    end procedure
end submodule

submodule(submodule_31_mod) submodule_31_middle
    implicit none

    interface
        module subroutine do_call(x)
            integer, intent(inout) :: x
        end subroutine
    end interface

contains
    module procedure caller
        call do_call(x)
    end procedure
end submodule

submodule(submodule_31_mod:submodule_31_middle) submodule_31_leaf
    implicit none
contains
    module procedure do_call
        call greet(x)
    end procedure
end submodule

program submodule_31
    use submodule_31_mod, only: caller
    implicit none
    integer :: x

    x = 5
    call caller(x)
    if (x /= 15) error stop
    print *, x
end program

module submodule_24_mod
    implicit none

    interface
        integer function i()
        end function
    end interface

    interface
        module function f(p) result(r)
            procedure(i) :: p
            integer :: r
        end function
    end interface
end module

submodule(submodule_24_mod) submodule_24_sub
    implicit none
contains
    module procedure f
        r = p()
    end procedure
end submodule

program submodule_24
    use submodule_24_mod, only: f
    implicit none
    if (f(get_four) /= 4) error stop
    print *, "ok"
contains
    integer function get_four()
        get_four = 4
    end function
end program

module submodule_56_c
    implicit none
    integer, parameter :: np = 3
end module submodule_56_c

module submodule_56_m
    use submodule_56_c, only: np
    implicit none
    interface
        module function f(x) result(r)
            integer, intent(in) :: x
            integer :: r(np)
        end function f
    end interface
end module submodule_56_m

submodule (submodule_56_m) submodule_56_s
contains
    module procedure f
        r = 2*x
    end procedure f
end submodule submodule_56_s

program submodule_56
    ! A separate module procedure whose result bound is a named constant
    ! of another module
    use submodule_56_m
    implicit none
    integer :: a(3)
    a = f(2)
    if (any(a /= 4)) error stop
    print *, "ok"
end program submodule_56

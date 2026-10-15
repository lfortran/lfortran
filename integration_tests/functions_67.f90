module functions_67_c
    implicit none
    integer, parameter :: np = 3
end module functions_67_c

module functions_67_m
    use functions_67_c, only: np
    implicit none
contains
    function fp(x) result(r)
        integer, intent(in) :: x
        integer :: r(np)
        r = x
    end function fp
    function apply(f, x) result(r)
        interface
            function f(x) result(r)
                integer, intent(in) :: x
                integer :: r(3)
            end function f
        end interface
        integer, intent(in) :: x
        integer :: r(3)
        r = f(x)
    end function apply
end module functions_67_m

program functions_67
    ! A result whose bound is a named constant of another module keeps
    ! its constant shape
    use functions_67_m, only: fp, apply
    implicit none
    abstract interface
        function iface3(x) result(r)
            integer, intent(in) :: x
            integer :: r(3)
        end function iface3
    end interface
    procedure(iface3), pointer :: q
    if (any(apply(fp, 4) /= 4)) error stop
    q => fp
    if (any(q(5) /= 5)) error stop
    print *, "ok"
end program functions_67

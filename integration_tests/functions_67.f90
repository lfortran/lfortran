module functions_67_c
    implicit none
    integer, parameter :: np = 3
    abstract interface
        function array_callback(x) result(r)
            import np
            integer, intent(in) :: x
            integer :: r(np)
        end function
        function array_apply(f, x) result(r)
            import np, array_callback
            procedure(array_callback) :: f
            integer, intent(in) :: x
            integer :: r(np)
        end function
    end interface
end module functions_67_c

module functions_67_m
    use functions_67_c, only: np, array_callback, array_apply
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
    function dispatch(g, f, x) result(r)
        procedure(array_apply) :: g
        procedure(array_callback) :: f
        integer, intent(in) :: x
        integer :: r(np)
        r = g(f, x)
    end function dispatch
end module functions_67_m

program functions_67
    ! A result whose bound is a named constant of another module keeps
    ! its constant shape
    use functions_67_m, only: fp, apply, dispatch
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
    if (any(dispatch(apply, fp, 6) /= 6)) error stop
    print *, "ok"
end program functions_67

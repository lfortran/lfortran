module template_instantiate_order_01_m
implicit none

! A deferred procedure listed before the deferred type its interface uses
template proc_first {F, T}
    deferred type :: T
    deferred interface
        function F(x) result(z)
            type(T), intent(in) :: x
            type(T) :: z
        end function
    end interface
contains
    function g(x) result(z)
        type(T), intent(in) :: x
        type(T) :: z
        z = F(x)
    end function
end template

! The same template with the deferred type listed first
template type_first {T, F}
    deferred type :: T
    deferred interface
        function F(x) result(z)
            type(T), intent(in) :: x
            type(T) :: z
        end function
    end interface
contains
    function g(x) result(z)
        type(T), intent(in) :: x
        type(T) :: z
        z = F(x)
    end function
end template

! Two deferred types and two procedures, in mixed order
template mixed {F, T, G, U}
    deferred type :: T, U
    deferred interface
        function F(x) result(z)
            type(T), intent(in) :: x
            type(U) :: z
        end function
        function G(x) result(z)
            type(U), intent(in) :: x
            type(T) :: z
        end function
    end interface
contains
    function h(x) result(z)
        type(T), intent(in) :: x
        type(T) :: z
        z = G(F(x))
    end function
end template

! A deferred binary procedure listed before the deferred type, to be
! instantiated with an intrinsic operator or an intrinsic function
template binop_first {op, T}
    deferred type :: T
    deferred interface
        function op(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
        end function
    end interface
contains
    function k(x, y) result(z)
        type(T), intent(in) :: x, y
        type(T) :: z
        z = op(x, y)
    end function
end template

contains

    integer function inc(x) result(z)
        integer, intent(in) :: x
        z = x + 1
    end function

    real function to_real(x) result(z)
        integer, intent(in) :: x
        z = real(x) * 2.0
    end function

    integer function to_int(x) result(z)
        real, intent(in) :: x
        z = int(x) + 3
    end function

end module

program template_instantiate_order_01
use template_instantiate_order_01_m
implicit none
instantiate proc_first {inc, integer}, only: g1 => g
instantiate type_first {integer, inc}, only: g2 => g
instantiate mixed {to_real, integer, to_int, real}, only: h1 => h
instantiate binop_first {operator(+), integer}, only: k1 => k
instantiate binop_first {max, integer}, only: k2 => k

if (g1(2) /= 3) error stop
if (g2(5) /= 6) error stop
if (h1(4) /= 11) error stop
if (k1(3, 5) /= 8) error stop
if (k2(4, -2) /= 4) error stop
print *, g1(2), g2(5), h1(4), k1(3, 5), k2(4, -2)
end program

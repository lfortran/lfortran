module template_instantiate_order_02_m
implicit none

requirement unary_r {T, F}
    deferred type :: T
    deferred interface
        function F(x) result(z)
            type(T), intent(in) :: x
            type(T) :: z
        end function
    end interface
end requirement

requirement convert_r {T, U, F, G}
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
end requirement

requirement binary_r {T, F}
    deferred type :: T
    deferred interface
        function F(x, y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
        end function
    end interface
end requirement

contains

    ! A deferred procedure listed before the deferred type its interface uses
    template function proc_first{F, T}(x) result(z)
        require :: unary_r {T, F}
        type(T), intent(in) :: x
        type(T) :: z
        z = F(x)
    end function

    ! The same function with the deferred type listed first
    template function type_first{T, F}(x) result(z)
        require :: unary_r {T, F}
        type(T), intent(in) :: x
        type(T) :: z
        z = F(x)
    end function

    ! Two deferred types and two procedures, in mixed order
    template function mixed{F, T, G, U}(x) result(z)
        require :: convert_r {T, U, F, G}
        type(T), intent(in) :: x
        type(T) :: z
        z = G(F(x))
    end function

    ! A deferred binary procedure listed before the deferred type, to be
    ! instantiated with an intrinsic operator
    template function binop_first{F, T}(x, y) result(z)
        require :: binary_r {T, F}
        type(T), intent(in) :: x, y
        type(T) :: z
        z = F(x, y)
    end function

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

program template_instantiate_order_02
use template_instantiate_order_02_m
implicit none

if (proc_first{inc, integer}(2) /= 3) error stop
if (type_first{integer, inc}(5) /= 6) error stop
if (mixed{to_real, integer, to_int, real}(4) /= 11) error stop
if (abs(binop_first{operator(+), real}(4.5, 4.5) - 9.0) > 1.0e-6) error stop
print *, proc_first{inc, integer}(2), type_first{integer, inc}(5), &
    mixed{to_real, integer, to_int, real}(4), &
    binop_first{operator(+), real}(4.5, 4.5)
end program

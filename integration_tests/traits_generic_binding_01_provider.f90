! Provider for traits_generic_binding_01.f90, compiled separately. Adder sorts
! before its generic binding sum, so a client reading this module file meets
! the binding before the generic definition that owns its procedure.
module traits_generic_binding_01_provider_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: INumeric, ISum, IValue, Adder

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum

    abstract interface :: IValue
        integer function value()
        end function value
    end interface IValue

    type, sealed, implements(ISum + IValue) :: Adder
        integer :: base = 5
    contains
        procedure, nopass :: sum
        procedure, nopass :: value => adder_value
    end type Adder

contains

    function sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function sum

    integer function adder_value()
        adder_value = 5
    end function adder_value
end module traits_generic_binding_01_provider_m

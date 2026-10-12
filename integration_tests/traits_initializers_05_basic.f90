! Binds private initializers and a method to MyType without acknowledging
! any trait (traits.tex inline-05).
module traits_initializers_05_basic
    implicit none
    private
    public :: MyType

    type :: MyType
        private
        real :: a = 0.0
    end type MyType

    implements :: MyType
        initial :: init_by_int, init_by_real
        procedure, pass :: value
    end implements MyType

contains

    function init_by_int(n) result(res)
        integer, intent(in) :: n
        type(MyType)        :: res
        res%a = real(n)
    end function init_by_int

    function init_by_real(a) result(res)
        real, intent(in) :: a
        type(MyType)     :: res
        res%a = 10.0 * a
    end function init_by_real

    real function value(self)
        class(MyType), intent(in) :: self
        value = self%a
    end function value
end module traits_initializers_05_basic

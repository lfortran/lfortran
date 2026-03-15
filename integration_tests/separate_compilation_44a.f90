module separate_compilation_44a_module
    implicit none

    type :: MyType
        real(8) :: val
    end type MyType

    interface operator(**)
        module procedure dpow
        module procedure ipow
    end interface operator(**)

contains

    function dpow(a, y) result(res)
        type(MyType), intent(in) :: a
        real(8),      intent(in) :: y
        type(MyType)             :: res
        res%val = a%val ** y
    end function dpow

    function ipow(a, n) result(res)
        type(MyType), intent(in) :: a
        integer(4),   intent(in) :: n
        type(MyType)             :: res
        res%val = a%val ** n
    end function ipow

end module separate_compilation_44a_module

! The contracts of traits_runtime_conformance_query_separate_01: traits, a
! public base type, a public type without conformances, and the interfaces of
! factories whose concrete types the consumer never sees.
module traits_runtime_conformance_query_separate_01_contracts
    implicit none
    abstract interface :: IValue
        integer function value(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    abstract interface :: ILabel
        integer function label(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    abstract interface, extends(IValue) :: IScaled
        integer function scaled(self, k)
            type(itself), intent(in), pass :: self
            integer, intent(in) :: k
        end function
        subroutine grow(self, by)
            type(itself), intent(inout), pass :: self
            integer, intent(in) :: by
        end subroutine
    end interface
    type :: Base
        integer :: n = 0
    end type
    type :: Shared
        integer :: n = 0
    end type
    interface
        function make_any(choice, n) result(object)
            integer, intent(in) :: choice, n
            class(*), allocatable :: object
        end function
        function make_base(choice, n) result(object)
            import :: Base
            integer, intent(in) :: choice, n
            class(Base), allocatable :: object
        end function
        function make_value(choice, n) result(object)
            import :: IValue
            integer, intent(in) :: choice, n
            class(IValue), allocatable :: object
        end function
    end interface
end module

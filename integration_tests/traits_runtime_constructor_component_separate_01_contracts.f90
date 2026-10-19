! The traits of traits_runtime_constructor_component_separate_01 and types
! with runtime trait components.
module traits_runtime_constructor_component_separate_01_contracts_m
    implicit none
    abstract interface :: IShape
        real function area(self)
            type(itself), intent(in), pass :: self
        end function
        subroutine scale(self, f)
            type(itself), intent(inout), pass :: self
            real, intent(in) :: f
        end subroutine
    end interface
    abstract interface :: IName
        function name(self) result(res)
            type(itself), intent(in), pass :: self
            character(len=:), allocatable :: res
        end function
    end interface
    type :: Holder
        class(IShape), allocatable :: tobj
        integer :: tag = 0
    end type
    type :: Duo
        class(IShape), allocatable :: first
        class(IShape+IName), allocatable :: named
    end type
end module

module traits_component_loaded_pure_lib
    implicit none
    abstract interface :: IValue
        pure integer function value()
        end function
    end interface
    type :: Seed
        integer :: n = 1
    end type
    implements IValue :: Seed
        procedure :: value => seed_value
    end implements
    type :: Holder
        class(IValue), allocatable :: item
    end type
    abstract interface
        subroutine copy_holder(x, y)
            import :: Holder
            type(Holder), intent(inout) :: x
            type(Holder), intent(in) :: y
        end subroutine
    end interface
contains
    pure integer function seed_value(self)
        type(Seed), intent(in) :: self
        seed_value = self%n
    end function
    subroutine middle(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call overwrite(x, y)
    end subroutine
    subroutine apply(f, x, y)
        procedure(copy_holder) :: f
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        call f(x, y)
    end subroutine
    subroutine overwrite(x, y)
        type(Holder), intent(inout) :: x
        type(Holder), intent(in) :: y
        x = y
    end subroutine
end module

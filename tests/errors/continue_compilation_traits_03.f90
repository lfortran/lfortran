module traits_component_boundary_types
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
    type :: Box
        type(Holder) :: inner
    end type
contains
    pure integer function seed_value(self)
        type(Seed), intent(in) :: self
        seed_value = self%n
    end function
end module

module traits_component_boundary_move_alloc
    use traits_component_boundary_types
contains
    subroutine move_holder(a, b)
        type(Holder), allocatable, intent(inout) :: a, b
        call move_alloc(a, b)
    end subroutine
    subroutine move_class_holder(a, b)
        class(Holder), allocatable, intent(inout) :: a, b
        call move_alloc(a, b)
    end subroutine
    subroutine move_nested_holder(a, b)
        type(Box), allocatable, intent(inout) :: a, b
        call move_alloc(from=a, to=b)
    end subroutine
    subroutine move_holder_arrays(a, b)
        type(Holder), allocatable, intent(inout) :: a(:), b(:)
        call move_alloc(a, b)
    end subroutine
end module

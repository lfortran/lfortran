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

! Closed numeric member slots: a runtime member is selected only for a declared
! member of the message's type set, and other signatures stay diagnosed.
module traits_numeric_runtime_boundary_contracts
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric
    abstract interface :: IOtherNumeric
        integer | real(real64)
    end interface IOtherNumeric
    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum
end module

module traits_numeric_runtime_boundary_members
    use traits_numeric_runtime_boundary_contracts
    implicit none
contains
    integer(8) function wide_total(summer, x) result(r)
        class(ISum), intent(in) :: summer
        integer(8), intent(in) :: x(:)
        r = summer%sum(x)
    end function
    real function single_total(summer, y) result(r)
        class(ISum), intent(in) :: summer
        real, intent(in) :: y(:)
        r = summer%sum{real(4)}(y)
    end function
    function distinct_forward{IOtherNumeric :: U}(summer, x) result(r)
        class(ISum), intent(in) :: summer
        type(U), intent(in) :: x(:)
        type(U) :: r
        r = summer%sum(x)
    end function
end module

module traits_numeric_runtime_boundary_complex
    implicit none
    abstract interface :: IComplexMember
        integer | complex(8)
    end interface IComplexMember
    abstract interface :: IComplexSum
        function sum{IComplexMember :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface IComplexSum
contains
    subroutine use_complex(summer)
        class(IComplexSum), intent(in) :: summer
    end subroutine
end module

module traits_numeric_runtime_boundary_mixed
    use traits_numeric_runtime_boundary_contracts, only: INumeric
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface IValue
    abstract interface :: IMixedBinders
        function apply{INumeric :: T, IValue :: V}(x, w) result(s)
            type(T), intent(in) :: x(:)
            type(V), intent(in) :: w
            type(T)             :: s
        end function apply
    end interface IMixedBinders
    abstract interface :: ISubroutineMember
        subroutine total{INumeric :: T}(x)
            type(T), intent(in) :: x(:)
        end subroutine total
    end interface ISubroutineMember
    abstract interface :: IMutableMember
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(inout) :: x(:)
            type(T)                :: s
        end function sum
    end interface IMutableMember
contains
    subroutine use_mixed(method)
        class(IMixedBinders), intent(in) :: method
    end subroutine
    subroutine use_subroutine(method)
        class(ISubroutineMember), intent(in) :: method
    end subroutine
    subroutine use_mutable(method)
        class(IMutableMember), intent(in) :: method
    end subroutine
end module

module traits_numeric_runtime_boundary_surface
    implicit none
    abstract interface :: IAllocatableArray
        integer function count(x)
            integer, allocatable, intent(in) :: x(:)
        end function count
    end interface IAllocatableArray
    abstract interface :: ICharacterResult
        character(len=4) function name()
        end function name
    end interface ICharacterResult
contains
    subroutine use_allocatable(method)
        class(IAllocatableArray), intent(in) :: method
    end subroutine
    subroutine use_character(method)
        class(ICharacterResult), intent(in) :: method
    end subroutine
end module

! A value without visible conformance does not select an initializer whose
! dummy is a trait view; the structure constructor remains the fallback.
module traits_numeric_runtime_boundary_values
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface IValue
    type :: Unrelated
    end type Unrelated
    type :: Holder
        class(IValue), allocatable :: item
    contains
        initial :: make_holder
    end type Holder
contains
    function make_holder(item) result(object)
        class(IValue), intent(in) :: item
        type(Holder) :: object
        object%item = item
    end function
    subroutine use_unrelated()
        type(Holder) :: h
        h = Holder(Unrelated())
    end subroutine
end module

! An explicit bound completed at the end of its module is not an assumed-shape
! runtime array argument, although it is still unresolved at the trait.
module traits_numeric_runtime_boundary_bounds
    implicit none
    type :: Bounds
        integer :: extent(2)
    end type Bounds
    abstract interface :: IBounded
        function count(n, a) result(r)
            import :: Bounds
            type(Bounds), intent(in) :: n
            integer, intent(in) :: a(n%extent(1))
            integer :: r
        end function count
    end interface IBounded
contains
    subroutine use_bounded(method)
        class(IBounded), intent(in) :: method
    end subroutine
end module

module traits_component_boundary_types
    abstract interface :: IValue
        pure integer function value(self)
            type(itself), intent(in), pass :: self
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
        function sum{INumeric :: T}(self, x) result(s)
            type(itself), intent(in), pass :: self
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

module traits_numeric_runtime_boundary_mixed
    use traits_numeric_runtime_boundary_contracts, only: INumeric
    implicit none
    abstract interface :: IValue
        integer function value(self)
            type(itself), intent(in), pass :: self
        end function
    end interface IValue
    abstract interface :: IMixedBinders
        function apply{INumeric :: T, IValue :: V}(self, x, w) result(s)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: x(:)
            type(V), intent(in) :: w
            type(T)             :: s
        end function apply
    end interface IMixedBinders
    abstract interface :: ISubroutineMember
        subroutine total{INumeric :: T}(self, x)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: x(:)
        end subroutine total
    end interface ISubroutineMember
    abstract interface :: IMutableMember
        function sum{INumeric :: T}(self, x) result(s)
            type(itself), intent(in), pass :: self
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
        integer function count(self, x)
            type(itself), intent(in), pass :: self
            integer, allocatable, intent(in) :: x(:)
        end function count
    end interface IAllocatableArray
    abstract interface :: ICharacterResult
        function name(self) result(r)
            type(itself), intent(in), pass :: self
            character(len=:), allocatable :: r
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
        integer function value(self)
            type(itself), intent(in), pass :: self
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
        function count(self, n, a) result(r)
            import :: Bounds
            type(itself), intent(in), pass :: self
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

! Without an exact match, trait conformance must select a single specific:
! unrelated views that both accept a value, or a parent and a child view that
! both accept it, make the reference ambiguous.
module traits_runtime_resolution_ambiguous
    implicit none
    abstract interface :: IA
        integer function a()
        end function a
    end interface IA
    abstract interface :: IB
        integer function b()
        end function b
    end interface IB
    abstract interface, extends(IA) :: IAChild
        integer function c()
        end function c
    end interface IAChild
    type, sealed, implements(IA + IB) :: Both
    contains
        procedure, nopass :: a => both_a
        procedure, nopass :: b => both_b
    end type Both
    type, sealed, implements(IAChild) :: Leaf
    contains
        procedure, nopass :: a => leaf_a
        procedure, nopass :: c => leaf_c
    end type Leaf
    interface pick
        module procedure pick_a, pick_b
    end interface pick
    interface level
        module procedure level_parent, level_child
    end interface level
contains
    integer function both_a()
        both_a = 1
    end function
    integer function both_b()
        both_b = 2
    end function
    integer function leaf_a()
        leaf_a = 3
    end function
    integer function leaf_c()
        leaf_c = 4
    end function
    integer function pick_a(item)
        class(IA), intent(in) :: item
        pick_a = item%a()
    end function
    integer function pick_b(item)
        class(IB), intent(in) :: item
        pick_b = item%b()
    end function
    integer function level_parent(item)
        class(IA), intent(in) :: item
        level_parent = item%a()
    end function
    integer function level_child(item)
        class(IAChild), intent(in) :: item
        level_child = item%c()
    end function
    subroutine use_both()
        type(Both) :: x
        print *, pick(x)
    end subroutine
    subroutine use_leaf()
        type(Leaf) :: y
        print *, level(y)
    end subroutine
end module

! A conformance outside module scope owns no entries for its generic messages,
! so it has no runtime witness for them: static calls and views of its other
! contracts work, and each runtime view of a generic contract is diagnosed.
module traits_local_generic_contracts
    implicit none
    abstract interface :: INumeric
        integer | real(8)
    end interface
    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T) :: s
        end function
    end interface
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface :: IApply
        function apply{IValue :: T}(object) result(r)
            type(T), intent(in) :: object
            integer :: r
        end function
    end interface
end module

program traits_local_generic_views
    use traits_local_generic_contracts
    implicit none
    type, sealed, implements(ISum + IValue) :: Adder
    contains
        procedure, nopass :: sum => adder_sum
        procedure, nopass :: value => adder_value
    end type
    type :: Applier
    end type
    implements IApply :: Applier
        procedure, nopass :: apply
    end implements
    type(Adder) :: a
    type(Applier) :: p
    class(IValue), allocatable :: item
    class(ISum), allocatable :: total
    integer :: xi(3) = [1, 2, 3]
    print *, a%sum(xi), p%apply(a)
    allocate(item, source=a)
    allocate(total, source=a)
    call consume(p)
contains
    function adder_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = x(1)
    end function
    integer function adder_value()
        adder_value = 1
    end function
    function apply{IValue :: T}(object) result(r)
        type(T), intent(in) :: object
        integer :: r
        r = object%value()
    end function
    subroutine consume(action)
        class(IApply), intent(in) :: action
    end subroutine
end program

! A type-bound binding of a generic procedure over closed type sets dispatches
! through member slots, so an extension can override it with a corresponding
! generic procedure (Child), but not replace an ordinary binding with one
! (Special).
module traits_generic_override_contracts
    implicit none
    abstract interface :: INumeric
        integer | real(8)
    end interface
    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T) :: s
        end function
    end interface
end module

module traits_generic_override
    use traits_generic_override_contracts
    implicit none
    type, implements(ISum) :: Base
    contains
        procedure, nopass :: sum => base_sum
    end type
    type, extends(Base) :: Child
    contains
        procedure, nopass :: sum => child_sum
    end type
    type :: Tool
    contains
        procedure, nopass :: run => tool_run
    end type
    type, extends(Tool) :: Special
    contains
        procedure, nopass :: run => special_run
    end type
contains
    function base_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = x(1)
    end function
    function child_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = x(2)
    end function
    integer function tool_run(x)
        integer, intent(in) :: x(:)
        tool_run = x(1)
    end function
    function special_run{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = x(2)
    end function
end module

! A rejected override keeps the inherited binding; later units call the
! bindings through the extension and through its ancestor.
module traits_generic_override_use
    use traits_generic_override
    implicit none
contains
    subroutine probe(c, b, x)
        type(Child), intent(in) :: c
        class(Base), intent(in) :: b
        integer, intent(in) :: x(:)
        print *, c%sum(x), b%sum(x)
    end subroutine
end module

! Overriding a generic binding whose type parameter has an open nominal
! constraint is not implemented: it has no member slots.
module traits_generic_override_open
    implicit none
    abstract interface :: IValue
        integer function value(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    type :: Holder
    contains
        procedure, nopass :: first => holder_first
    end type
    type, extends(Holder) :: Other
    contains
        procedure, nopass :: first => other_first
    end type
contains
    function holder_first{IValue :: T}(x) result(s)
        type(T), intent(in) :: x
        integer :: s
        s = x%value()
    end function
    function other_first{IValue :: T}(x) result(s)
        type(T), intent(in) :: x
        integer :: s
        s = x%value() + 1
    end function
end module

! Extending a generic type requires its type arguments, which are parameters
! of the extending type that satisfy the parent's constraints (TD-45).
module traits_generic_extends_base
    implicit none
    abstract interface :: ISum
        integer function total(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    abstract interface, extends(ISum) :: IFastSum
    end interface
    abstract interface :: IOther
        integer function other(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    type, implements(ISum) :: Leaf
    contains
        procedure :: total => leaf_total
    end type
    type :: Plain
        integer :: n = 0
    end type
contains
    integer function leaf_total(self)
        class(Leaf), intent(in) :: self
        leaf_total = 1
    end function
end module

module traits_generic_extends_simple
    use traits_generic_extends_base, only: ISum
    implicit none
    type :: SimpleSum{ISum :: U}
        type(U), allocatable :: basic
    end type
    type :: Wrap{ISum :: U}
        type(U), allocatable :: item
    end type
end module

module traits_generic_extends_errors
    use traits_generic_extends_base
    use traits_generic_extends_simple, only: SimpleSum, Wrap
    implicit none
    ! The parent's type arguments are missing.
    type, extends(SimpleSum) :: Missing{ISum :: V}
    end type
    type, extends(SimpleSum) :: Implicit
    end type
    type, extends(SimpleSum{V, V}) :: TooMany{ISum :: V}
    end type
    type, extends(SimpleSum{V}) :: Unconstrained{V}
    end type
    type, extends(SimpleSum{V}) :: WrongTrait{IOther :: V}
    end type
    ! A trait that extends the parent's constraint satisfies it, and so does
    ! a type that implements it.
    type, extends(SimpleSum{V}) :: Narrower{IFastSum :: V}
    end type
    type, extends(SimpleSum{Leaf}) :: Concrete{ISum :: V}
    end type
    type, extends(SimpleSum{Plain}) :: Unsatisfied{ISum :: V}
    end type
    ! Not implemented: a generic type applied to a parameter and a type that
    ! is not generic extending an instance.
    type, extends(SimpleSum{Wrap{V}}) :: Nested{ISum :: V}
    end type
    type, extends(SimpleSum{Leaf}) :: Instance
    end type
    type, extends(Plain{V}) :: NotGeneric{ISum :: V}
    end type
    ! The type's own name requires its type arguments.
    type, extends(SimpleSum{V}) :: BareSelf{ISum :: V}
    contains
        procedure :: bare
    end type
contains
    integer function bare(self)
        class(BareSelf), intent(in) :: self
        bare = 0
    end function
end module

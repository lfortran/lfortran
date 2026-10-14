! Located diagnostics for binding procedures and initializers to a type with
! an implements statement, and for completing the conformances that rely on
! them. Each numbered case is an independent violation; all other code is
! valid.
module cc_traits_binding_completion_contracts
    implicit none
    abstract interface :: IConstructable
        initial(n)
            integer, intent(in) :: n
        end initial
    end interface IConstructable

    abstract interface :: IPrintable
        subroutine output(self)
            type(itself), intent(in), pass :: self
        end subroutine output
    end interface IPrintable

contains

    function make{IConstructable :: T}(n) result(r)
        integer, intent(in) :: n
        type(T)             :: r
        r = T(n)
    end function make
end module cc_traits_binding_completion_contracts

module cc_traits_binding_completion_bound
    implicit none
    type :: Bound
        private
        real :: a = 0.0
    end type Bound

    implements :: Bound
        initial :: bound_from_integer
        procedure, pass :: output => bound_output
    end implements Bound
contains
    function bound_from_integer(n) result(res)
        integer, intent(in) :: n
        type(Bound)         :: res
        res%a = real(n)
    end function bound_from_integer

    subroutine bound_output(self)
        class(Bound), intent(in) :: self
        print *, self%a
    end subroutine bound_output
end module cc_traits_binding_completion_bound

! E01: binding procedures without traits acknowledges no trait, even when
! the signatures match: Bound neither satisfies the constraint of make nor
! forms an IPrintable view.
program cc_traits_binding_completion_nominal
    use cc_traits_binding_completion_contracts, only: IPrintable, make
    use cc_traits_binding_completion_bound, only: Bound
    implicit none
    type(Bound) :: b
    class(IPrintable), allocatable :: printer
    b = make{Bound}(3)
    allocate(printer, source=Bound(4))
end program cc_traits_binding_completion_nominal

! E02: procedures are bound without traits only to a type of the unit.
module cc_traits_binding_completion_foreign
    use cc_traits_binding_completion_bound, only: Bound
    implicit none
    implements :: Bound
        procedure, pass :: twice
    end implements Bound
contains
    subroutine twice(self)
        class(Bound), intent(in) :: self
        call self%output()
        call self%output()
    end subroutine twice
end module cc_traits_binding_completion_foreign

! E03: the type already binds the name output.
module cc_traits_binding_completion_duplicate
    implicit none
    type :: Twice
        integer :: n = 0
    contains
        procedure, pass :: output => first_output
    end type Twice

    implements :: Twice
        procedure, pass :: output => second_output
    end implements Twice
contains
    subroutine first_output(self)
        class(Twice), intent(in) :: self
        print *, self%n
    end subroutine first_output

    subroutine second_output(self)
        class(Twice), intent(in) :: self
        print *, -self%n
    end subroutine second_output
end module cc_traits_binding_completion_duplicate

! E04: without a trait there is no requirement for a static binding to
! implement.
module cc_traits_binding_completion_static
    implicit none
    type, sealed :: Made
        integer :: n = 0
    end type Made

    implements :: Made
        procedure, static :: create => create_made
    end implements Made
contains
    function create_made() result(res)
        type(Made) :: res
        res%n = 1
    end function create_made
end module cc_traits_binding_completion_static

! E05: an implements statement does not declare deferred bindings.
module cc_traits_binding_completion_deferred
    implicit none
    type :: Open
        integer :: n = 0
    end type Open

    abstract interface
        subroutine open_output(self)
            import :: Open
            class(Open), intent(in) :: self
        end subroutine open_output
    end interface

    implements :: Open
        procedure(open_output), deferred, pass :: output
    end implements Open
end module cc_traits_binding_completion_deferred

! E06: two initializers the type already binds implement the initializer
! requirement that a later statement acknowledges.
module cc_traits_binding_completion_ambiguous
    use cc_traits_binding_completion_contracts, only: IConstructable
    implicit none
    type, sealed :: Counted
        integer :: n = 0
    contains
        initial :: from_count
    end type Counted

    implements :: Counted
        initial :: from_total
    end implements Counted

    implements IConstructable :: Counted
    end implements Counted
contains
    function from_count(n) result(res)
        integer, intent(in) :: n
        type(Counted)       :: res
        res%n = n
    end function from_count

    function from_total(n) result(res)
        integer, intent(in) :: n
        type(Counted)       :: res
        res%n = -n
    end function from_total
end module cc_traits_binding_completion_ambiguous

! E07: the initializer the type binds takes integer(8), not the integer(4)
! that the adopted trait requires.
module cc_traits_binding_completion_incompatible
    use cc_traits_binding_completion_contracts, only: IConstructable
    implicit none
    type, sealed, implements(IConstructable) :: Wide
        integer(8) :: n = 0
    contains
        initial :: from_wide
    end type Wide
contains
    function from_wide(n) result(res)
        integer(8), intent(in) :: n
        type(Wide)             :: res
        res%n = n
    end function from_wide
end module cc_traits_binding_completion_incompatible

module cc_traits_binding_completion_views
    use cc_traits_binding_completion_contracts, only: IConstructable, IPrintable
    implicit none
    abstract interface, extends(IConstructable + IPrintable) :: IAdmissible
        subroutine input(self, filename)
            type(itself), intent(in), pass :: self
            character(*), intent(in) :: filename
        end subroutine input
    end interface IAdmissible
end module cc_traits_binding_completion_views

! E08: a runtime view has no slot for an initializer, but a conformance to
! a trait with an initializer requirement still binds one.
module cc_traits_binding_completion_unconstructed
    use cc_traits_binding_completion_views, only: IAdmissible
    implicit none
    type, sealed :: Unconstructed
        integer :: n = 0
    end type Unconstructed

    implements IAdmissible :: Unconstructed
        procedure, pass :: output => unconstructed_output
        procedure, pass :: input => unconstructed_input
    end implements Unconstructed
contains
    subroutine unconstructed_output(self)
        type(Unconstructed), intent(in) :: self
        print *, self%n
    end subroutine unconstructed_output

    subroutine unconstructed_input(self, filename)
        type(Unconstructed), intent(in) :: self
        character(*), intent(in)        :: filename
        print *, self%n, filename
    end subroutine unconstructed_input
end module cc_traits_binding_completion_unconstructed

module cc_traits_binding_completion_printed
    use cc_traits_binding_completion_contracts, only: IPrintable
    implicit none
    type, sealed :: Printed
        integer :: n = 0
    end type Printed

    implements IPrintable :: Printed
        procedure, pass :: output => printed_output
    end implements Printed
contains
    subroutine printed_output(self)
        type(Printed), intent(in) :: self
        print *, self%n
    end subroutine printed_output
end module cc_traits_binding_completion_printed

! E09: a type that only implements IPrintable does not conform to the
! combination with IConstructable. E10: an initializer requirement is not a
! method of a runtime view.
module cc_traits_binding_completion_view_use
    use cc_traits_binding_completion_contracts, only: IConstructable, IPrintable
    use cc_traits_binding_completion_views, only: IAdmissible
    use cc_traits_binding_completion_printed, only: Printed
    implicit none
contains
    subroutine use_views()
        class(IConstructable + IPrintable), allocatable :: pair
        class(IAdmissible), allocatable :: obj
        allocate(pair, source=Printed(1))
        call obj%initial(3)
    end subroutine use_views
end module cc_traits_binding_completion_view_use

! E11: a procedure of the constructor interface of a type implements an
! initializer requirement only with exactly a type(Wrapped) result, not an
! allocatable one.
module cc_traits_binding_completion_allocatable_result
    use cc_traits_binding_completion_contracts, only: IConstructable
    implicit none
    type :: Wrapped
        integer :: n = 0
    end type Wrapped

    interface Wrapped
        module procedure wrapped_from_integer
    end interface Wrapped

    implements IConstructable :: Wrapped
    end implements Wrapped
contains
    function wrapped_from_integer(n) result(res)
        integer, intent(in)        :: n
        type(Wrapped), allocatable :: res
        allocate(res)
        res%n = n
    end function wrapped_from_integer
end module cc_traits_binding_completion_allocatable_result

! E12: nor with a result of another type.
module cc_traits_binding_completion_other_result
    use cc_traits_binding_completion_contracts, only: IConstructable
    implicit none
    type :: Payload
        integer :: n = 0
    end type Payload

    type :: Labelled
        integer :: n = 0
    end type Labelled

    interface Labelled
        module procedure payload_from_integer
    end interface Labelled

    implements IConstructable :: Labelled
    end implements Labelled
contains
    function payload_from_integer(n) result(res)
        integer, intent(in) :: n
        type(Payload)       :: res
        res%n = n
    end function payload_from_integer
end module cc_traits_binding_completion_other_result

! E13: the structure constructor of a type is not an initializer.
module cc_traits_binding_completion_structure_constructor
    use cc_traits_binding_completion_contracts, only: IConstructable
    implicit none
    type, sealed :: Plain
        integer :: n = 0
    end type Plain

    implements IConstructable :: Plain
    end implements Plain
end module cc_traits_binding_completion_structure_constructor

! E14: a matching constructor interface does not make a type conform to
! IConstructable without an implements statement or attribute.
module cc_traits_binding_completion_unacknowledged
    use cc_traits_binding_completion_contracts, only: make
    implicit none
    type :: Unacknowledged
        integer :: n = 0
    end type Unacknowledged

    interface Unacknowledged
        module procedure unacknowledged_from_integer
    end interface Unacknowledged
contains
    function unacknowledged_from_integer(n) result(res)
        integer, intent(in)  :: n
        type(Unacknowledged) :: res
        res%n = n
    end function unacknowledged_from_integer

    subroutine construct()
        type(Unacknowledged) :: u
        u = make{Unacknowledged}(3)
    end subroutine construct
end module cc_traits_binding_completion_unacknowledged

! E15: an operator binding without traits implements no requirement.
module cc_traits_binding_completion_operator
    implicit none
    type, sealed :: Summed
        integer :: n = 0
    end type Summed

    implements :: Summed
        procedure, static :: operator(+) => summed_plus
    end implements Summed
contains
    function summed_plus(lhs, rhs) result(res)
        type(Summed), intent(in) :: lhs, rhs
        type(Summed)             :: res
        res%n = lhs%n + rhs%n
    end function summed_plus
end module cc_traits_binding_completion_operator

! E16: binding procedures to an intrinsic type without traits.
module cc_traits_binding_completion_intrinsic
    implicit none
    implements :: real(8)
        procedure, pass :: show => show_real
    end implements real(8)
contains
    subroutine show_real(self)
        real(8), intent(in) :: self
        print *, self
    end subroutine show_real
end module cc_traits_binding_completion_intrinsic

! E17: binding procedures to a generic type without traits.
module cc_traits_binding_completion_generic
    use cc_traits_binding_completion_contracts, only: IPrintable
    implicit none
    type :: Holder{IPrintable :: T}
        type(T) :: item
    end type Holder

    implements :: Holder{IPrintable :: T}
        initial :: holder_from_item
    end implements Holder
contains
    function holder_from_item(item) result(res)
        type(T), intent(in) :: item
        type(Holder{T})     :: res
        res%item = item
    end function holder_from_item
end module cc_traits_binding_completion_generic

! E18: an implements statement cannot target an abstract type.
module cc_traits_binding_completion_abstract
    implicit none
    type, abstract :: Shape
        real :: area = 0.0
    end type Shape

    implements :: Shape
        procedure, pass :: show => show_shape
    end implements Shape
contains
    subroutine show_shape(self)
        class(Shape), intent(in) :: self
        print *, self%area
    end subroutine show_shape
end module cc_traits_binding_completion_abstract

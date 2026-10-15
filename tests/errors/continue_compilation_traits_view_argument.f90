! Trait-object arguments of runtime messages that are not their receivers.
! Each procedure whose name starts with from_ has exactly one invalid actual
! argument: an intent(inout) view argument needs a definable payload, whether
! the message is dispatched through a view or bound statically, and a
! read-only receiver cannot call a message with an intent(inout) receiver; a
! view of an unrelated trait or a type without a conformance cannot be
! borrowed for the argument. Mismatched implements update with a view of
! another trait. Traits whose messages take allocatable, pointer, TARGET or
! generic view arguments have no runtime views yet, so each variable whose
! name ends with _argument is invalid.
module traits_view_argument_negative_m
    implicit none
    abstract interface :: IEstimates
        subroutine some_method(self)
            type(itself), pass, intent(inout) :: self
        end subroutine some_method
    end interface
    abstract interface :: IOther
        subroutine other(self)
            type(itself), pass, intent(inout) :: self
        end subroutine other
    end interface
    abstract interface :: ICoeffs
        subroutine update(self, estimates)
            type(itself), pass, intent(inout) :: self
            class(IEstimates), intent(inout) :: estimates
        end subroutine update
    end interface
    abstract interface :: IOwning
        subroutine take(self, estimates)
            type(itself), pass, intent(inout) :: self
            class(IEstimates), allocatable, intent(inout) :: estimates
        end subroutine take
    end interface
    abstract interface :: IPointing
        subroutine point(self, estimates)
            type(itself), pass, intent(inout) :: self
            class(IEstimates), pointer, intent(in) :: estimates
        end subroutine point
    end interface
    abstract interface :: ITargeting
        subroutine aim(self, estimates)
            type(itself), pass, intent(inout) :: self
            class(IEstimates), target, intent(inout) :: estimates
        end subroutine aim
    end interface
    abstract interface :: INumber
        integer | real(8)
    end interface INumber
    abstract interface :: IFeeding
        function feed{INumber :: T}(self, x, estimates) result(s)
            type(itself), pass, intent(in) :: self
            type(T), intent(in) :: x
            class(IEstimates), intent(inout) :: estimates
            type(T) :: s
        end function feed
    end interface
    type :: Tally
        integer :: n = 0
    end type
    implements IEstimates :: Tally
        procedure, pass :: some_method => step
    end implements
    type :: Coeffs
        integer :: updates = 0
    end type
    implements ICoeffs :: Coeffs
        procedure, pass :: update => coeffs_update
    end implements
    type :: Mismatched
        integer :: updates = 0
    end type
    implements ICoeffs :: Mismatched
        procedure, pass :: update => mismatched_update
    end implements
    type :: Unrelated
        integer :: n = 0
    end type
    type :: Holder
        type(Tally) :: cell
        class(IEstimates), allocatable :: item
    end type
    type(Tally), parameter :: fixed = Tally(4)
    class(IOwning), allocatable :: allocatable_argument
    class(IPointing), allocatable :: pointer_argument
    class(ITargeting), allocatable :: target_argument
    class(IFeeding), allocatable :: generic_argument
contains
    subroutine step(self)
        class(Tally), intent(inout) :: self
        self%n = self%n + 1
    end subroutine
    subroutine coeffs_update(self, estimates)
        class(Coeffs), intent(inout) :: self
        class(IEstimates), intent(inout) :: estimates
        call estimates%some_method()
    end subroutine
    subroutine mismatched_update(self, estimates)
        class(Mismatched), intent(inout) :: self
        class(IOther), intent(inout) :: estimates
        call estimates%other()
    end subroutine
    type(Tally) function make_estimates()
        make_estimates%n = 5
    end function
    subroutine from_constant(c)
        class(ICoeffs), intent(inout) :: c
        call c%update(fixed)
    end subroutine
    subroutine from_constructor(c)
        class(ICoeffs), intent(inout) :: c
        call c%update(Tally(3))
    end subroutine
    subroutine from_function(c)
        class(ICoeffs), intent(inout) :: c
        call c%update(make_estimates())
    end subroutine
    subroutine from_readonly_concrete(c, e)
        class(ICoeffs), intent(inout) :: c
        type(Tally), intent(in) :: e
        call c%update(e)
    end subroutine
    subroutine from_readonly_component(c, h)
        class(ICoeffs), intent(inout) :: c
        type(Holder), intent(in) :: h
        call c%update(h%cell)
    end subroutine
    subroutine from_readonly_owner_component(c, h)
        class(ICoeffs), intent(inout) :: c
        type(Holder), intent(in) :: h
        call c%update(estimates=h%item)
    end subroutine
    subroutine from_readonly_view(c, e)
        class(ICoeffs), intent(inout) :: c
        class(IEstimates), intent(in) :: e
        call c%update(e)
    end subroutine
    subroutine from_readonly_slot(c, e)
        class(ICoeffs), intent(inout) :: c
        class(IEstimates), allocatable, intent(in) :: e
        call c%update(e)
    end subroutine
    subroutine from_readonly_association(c, e)
        class(ICoeffs), intent(inout) :: c
        class(IEstimates), intent(in) :: e
        select type (alias => e)
        class default
            call c%update(alias)
        end select
    end subroutine
    subroutine from_parenthesized(c, e)
        class(ICoeffs), intent(inout) :: c
        type(Tally), intent(inout) :: e
        call c%update((e))
    end subroutine
    subroutine from_expression_associate(c)
        class(ICoeffs), intent(inout) :: c
        associate (alias => make_estimates())
            call c%update(alias)
        end associate
    end subroutine
    subroutine from_unrelated_view(c, o)
        class(ICoeffs), intent(inout) :: c
        class(IOther), intent(inout) :: o
        call c%update(o)
    end subroutine
    subroutine from_unrelated_type(c, u)
        class(ICoeffs), intent(inout) :: c
        type(Unrelated), intent(inout) :: u
        call c%update(u)
    end subroutine
    subroutine from_readonly_receiver(c, e)
        class(ICoeffs), intent(in) :: c
        type(Tally), intent(inout) :: e
        call c%update(e)
    end subroutine
    subroutine from_static_readonly(c, e)
        type(Coeffs), intent(inout) :: c
        class(IEstimates), intent(in) :: e
        call c%update(e)
    end subroutine
end module

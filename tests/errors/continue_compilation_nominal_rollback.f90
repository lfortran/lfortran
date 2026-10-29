module cc_nominal_rollback_templates
    implicit none
    abstract interface :: IMarker
    end interface
    type :: Ref{T}
        type(T), pointer :: target
    end type
    type :: Box{T}
        type(T) :: item
    end type
    type :: Guard{IMarker :: T}
        type(T), pointer :: target
    end type
end module

module cc_nominal_rollback
    use cc_nominal_rollback_templates
    implicit none
    type(Ref{integer}) :: shared_before
    type, sealed, implements(IMarker) :: Anchor
        integer :: value
    end type
    type :: FailedSubject
        type(Ref{integer}) :: shared
        type(Box{real}) :: independent
        type(Guard{Anchor}) :: pending_independent
        type(Box{Ref{FailedSubject}}) :: observer
        integer :: value
        integer :: value
    end type
    type :: InlineSubject
        type(Box{InlineSubject}) :: item
    end type
    type, sealed, implements(IMarker) :: GoodSubject
        type(Box{Ref{GoodSubject}}) :: observer
        type(Guard{GoodSubject}) :: guarded
    end type
    type(Ref{integer}) :: shared_after
    type(Box{real}) :: independent_after
contains
    subroutine valid(self)
        type(GoodSubject), intent(inout) :: self
        self%observer%item%target => self%guarded%target
        shared_after%target => shared_before%target
        independent_after%item = 42.0
    end subroutine
end module

module cc_nominal_pending_rollback
    use cc_nominal_rollback_templates
    implicit none
    type, sealed, implements(IMarker) :: FailedSubject
        type(Guard{FailedSubject}) :: observer
        integer :: value
        integer :: value
    end type
    type, sealed, implements(IMarker) :: GoodSubject
        type(Guard{GoodSubject}) :: observer
    end type
contains
    subroutine valid(self)
        type(GoodSubject), intent(inout) :: self
        nullify(self%observer%target)
    end subroutine
end module

module cc_nominal_rollback_after
    implicit none
contains
    subroutine independent()
        missing_after_nominal_failure = 1
    end subroutine
end module

module cc_nominal_binding_rollback
    implicit none
    abstract interface
        subroutine callback_interface()
        end subroutine
    end interface
    type :: BeforeBinding
    contains
        procedure, nopass :: before_binding
        generic :: run_before => before_binding
    end type
    type, abstract :: FailedBinding
        type(FailedBinding) :: item
        procedure(callback_interface), pointer, nopass :: callback => abandoned_target
    contains
        procedure, nopass :: harmless
        procedure(abandoned_interface), nopass, deferred :: abandoned_deferred
        generic :: abandoned_generic => harmless
        initial :: abandoned_initializer
    end type
    type :: IndependentBinding
        integer :: value
        procedure(callback_interface), pointer, nopass :: callback => independent_binding
    contains
        procedure, nopass :: independent_binding
        generic :: run => independent_binding
        initial :: new_independent
    end type
contains
    subroutine before_binding()
    end subroutine
    subroutine harmless()
    end subroutine
    subroutine independent_binding()
    end subroutine
    function new_independent() result(value)
        type(IndependentBinding) :: value
        value%value = 42
    end function
end module

module cc_nominal_binding_after
    implicit none
contains
    subroutine independent()
        missing_after_binding_failure = 1
    end subroutine
end module

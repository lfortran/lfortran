! Private concrete types whose conformances are declared with them, and the
! factories. Hidden extends Twin and inherits its declared conformance.
module traits_runtime_conformance_query_separate_01_provider
    use traits_runtime_conformance_query_separate_01_contracts
    implicit none
    private
    public :: make
    type, extends(Base), implements(IValue) :: Twin
    contains
        procedure :: value => twin_value
    end type
    type, extends(Twin) :: Hidden
    contains
        procedure :: value => hidden_value
    end type
    type, extends(Base), implements(IScaled + ILabel) :: Rich
    contains
        procedure :: value => rich_value
        procedure :: scaled => rich_scaled
        procedure :: grow => rich_grow
        procedure :: label => rich_label
    end type
contains
    integer function twin_value(self)
        class(Twin), intent(in) :: self
        twin_value = self%n
    end function
    integer function hidden_value(self)
        class(Hidden), intent(in) :: self
        hidden_value = self%n + 50
    end function
    integer function rich_value(self)
        class(Rich), intent(in) :: self
        rich_value = self%n + 1
    end function
    integer function rich_scaled(self, k)
        class(Rich), intent(in) :: self
        integer, intent(in) :: k
        rich_scaled = self%n * k
    end function
    subroutine rich_grow(self, by)
        class(Rich), intent(inout) :: self
        integer, intent(in) :: by
        self%n = self%n + by
    end subroutine
    integer function rich_label(self)
        class(Rich), intent(in) :: self
        rich_label = 7
    end function
    subroutine make(choice, n, object)
        integer, intent(in) :: choice, n
        class(Base), allocatable, intent(out) :: object
        select case (choice)
        case (1)
            allocate(Twin :: object)
        case (2)
            allocate(Hidden :: object)
        case default
            allocate(Rich :: object)
        end select
        object%n = n
    end subroutine
end module

function make_any(choice, n) result(object)
    use traits_runtime_conformance_query_separate_01_contracts, only: Base, Shared
    use traits_runtime_conformance_query_separate_01_provider, only: make
    implicit none
    integer, intent(in) :: choice, n
    class(*), allocatable :: object
    class(Base), allocatable :: made
    type(Shared) :: plain
    if (choice == 0) then
        plain%n = n
        allocate(object, source=plain)
    else
        call make(choice, n, made)
        allocate(object, source=made)
    end if
end function

function make_base(choice, n) result(object)
    use traits_runtime_conformance_query_separate_01_contracts, only: Base
    use traits_runtime_conformance_query_separate_01_provider, only: make
    implicit none
    integer, intent(in) :: choice, n
    class(Base), allocatable :: object
    call make(choice, n, object)
end function

function make_value(choice, n) result(object)
    use traits_runtime_conformance_query_separate_01_contracts, only: Base, IValue
    use traits_runtime_conformance_query_separate_01_provider, only: make
    implicit none
    integer, intent(in) :: choice, n
    class(IValue), allocatable :: object
    class(Base), allocatable :: made
    call make(choice, n, made)
    select type (x => made)
    class is (IValue)
        object = x
    end select
end function

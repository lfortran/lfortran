! Compiled, archived and hidden before the late module and the driver exist.
! It sees only the open contract: no concrete numeric type or conformance is
! visible here, so its generic messages cannot be specialized for a closed
! list of element types.
module traits_protocol_separate_01_provider_m
    use traits_protocol_separate_01_contracts_m, only: INumeric, ISum, IAverager
    implicit none
    private
    public :: build_sum, build_averager

    type, sealed, implements(ISum) :: OpenSimple
    contains
        procedure, nopass :: sum => open_simple_sum
    end type OpenSimple

    type, sealed, implements(ISum) :: OpenReverse
    contains
        procedure, nopass :: sum => open_reverse_sum
    end type OpenReverse

    type, sealed, implements(ISum) :: OpenPairwise
        private
        class(ISum), allocatable :: other
    contains
        initial :: open_pairwise_init
        procedure, pass :: sum => open_pairwise_sum
    end type OpenPairwise

    type, sealed, implements(IAverager) :: OpenAverager
        private
        class(ISum), allocatable :: drv
    contains
        initial :: open_averager_init
        procedure, pass :: average => open_average
    end type OpenAverager

contains

    function open_simple_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function open_simple_sum

    function open_reverse_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: i
        s = T(0)
        do i = size(x), 1, -1
            s = s + x(i)
        end do
    end function open_reverse_sum

    function open_pairwise_init(other) result(res)
        class(ISum), intent(in) :: other
        type(OpenPairwise)      :: res
        res%other = other
    end function open_pairwise_init

    function open_pairwise_sum{INumeric :: T}(self, x) result(s)
        type(OpenPairwise), intent(in) :: self
        type(T),            intent(in) :: x(:)
        type(T)                        :: s
        integer                        :: m
        if (size(x) <= 2) then
            s = self%other%sum(x)
        else
            m = size(x) / 2
            s = self%sum(x(:m)) + self%sum(x(m+1:))
        end if
    end function open_pairwise_sum

    function open_averager_init(drv) result(res)
        class(ISum), intent(in) :: drv
        type(OpenAverager)      :: res
        res%drv = drv
    end function open_averager_init

    function open_average{INumeric :: T}(self, x) result(a)
        type(OpenAverager), intent(in) :: self
        type(T),            intent(in) :: x(:)
        type(T)                        :: a
        a = self%drv%sum(x) / T(size(x))
    end function open_average

    subroutine build_sum(choice, object)
        integer, intent(in) :: choice
        class(ISum), allocatable, intent(out) :: object
        select case (choice)
        case (1)
            object = OpenSimple()
        case (2)
            object = OpenPairwise(OpenSimple())
        case (3)
            object = OpenPairwise(OpenPairwise(OpenReverse()))
        case default
            error stop 'unknown sum choice'
        end select
    end subroutine build_sum

    subroutine build_averager(choice, object)
        integer, intent(in) :: choice
        class(IAverager), allocatable, intent(out) :: object
        select case (choice)
        case (1)
            object = OpenAverager(OpenSimple())
        case (2)
            object = OpenAverager(OpenPairwise(OpenSimple()))
        case (3)
            object = OpenAverager(OpenPairwise(OpenPairwise(OpenReverse())))
        case default
            error stop 'unknown averager choice'
        end select
    end subroutine build_averager
end module traits_protocol_separate_01_provider_m

subroutine make_sum(choice, object)
    use traits_protocol_separate_01_contracts_m, only: ISum
    use traits_protocol_separate_01_provider_m, only: build_sum
    implicit none
    integer, intent(in) :: choice
    class(ISum), allocatable, intent(out) :: object
    call build_sum(choice, object)
end subroutine make_sum

subroutine make_averager(choice, object)
    use traits_protocol_separate_01_contracts_m, only: IAverager
    use traits_protocol_separate_01_provider_m, only: build_averager
    implicit none
    integer, intent(in) :: choice
    class(IAverager), allocatable, intent(out) :: object
    call build_averager(choice, object)
end subroutine make_averager

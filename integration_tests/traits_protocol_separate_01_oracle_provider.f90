! Standard-Fortran oracle for traits_protocol_separate_01, part 3 of 5: the
! provider, compiled against the contracts only and then archived and hidden.
module traits_protocol_separate_01_oracle_provider_m
    use traits_protocol_separate_01_oracle_contracts_m, only: INumeric, ISum, IAverager, &
        initial_of
    implicit none
    private
    public :: build_sum, build_averager

    type, extends(ISum) :: OpenSimple
    contains
        procedure :: sum => open_simple_sum
    end type OpenSimple

    type, extends(ISum) :: OpenReverse
    contains
        procedure :: sum => open_reverse_sum
    end type OpenReverse

    type, extends(ISum) :: OpenPairwise
        private
        class(ISum), allocatable :: other
    contains
        procedure :: sum => open_pairwise_sum
    end type OpenPairwise

    interface OpenPairwise
        module procedure open_pairwise_init
    end interface OpenPairwise

    type, extends(IAverager) :: OpenAverager
        private
        class(ISum), allocatable :: drv
    contains
        procedure :: average => open_average
    end type OpenAverager

    interface OpenAverager
        module procedure open_averager_init
    end interface OpenAverager

contains

    function open_simple_sum(self, x) result(s)
        class(OpenSimple), intent(in)  :: self
        class(INumeric),   intent(in)  :: x(:)
        class(INumeric),   allocatable :: s
        integer                        :: i
        s = initial_of(x, 0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function open_simple_sum

    function open_reverse_sum(self, x) result(s)
        class(OpenReverse), intent(in)  :: self
        class(INumeric),    intent(in)  :: x(:)
        class(INumeric),    allocatable :: s
        integer                         :: i
        s = initial_of(x, 0)
        do i = size(x), 1, -1
            s = s + x(i)
        end do
    end function open_reverse_sum

    function open_pairwise_init(other) result(res)
        class(ISum), intent(in) :: other
        type(OpenPairwise)      :: res
        res%other = other
    end function open_pairwise_init

    recursive function open_pairwise_sum(self, x) result(s)
        class(OpenPairwise), intent(in)  :: self
        class(INumeric),     intent(in)  :: x(:)
        class(INumeric),     allocatable :: s
        class(INumeric),     allocatable :: left, right
        integer                          :: m
        if (size(x) <= 2) then
            s = self%other%sum(x)
        else
            m = size(x) / 2
            left = self%sum(x(:m))
            right = self%sum(x(m+1:))
            s = left + right
        end if
    end function open_pairwise_sum

    function open_averager_init(drv) result(res)
        class(ISum), intent(in) :: drv
        type(OpenAverager)      :: res
        res%drv = drv
    end function open_averager_init

    function open_average(self, x) result(a)
        class(OpenAverager), intent(in)  :: self
        class(INumeric),     intent(in)  :: x(:)
        class(INumeric),     allocatable :: a
        class(INumeric),     allocatable :: total
        total = self%drv%sum(x)
        a = total / initial_of(x, size(x))
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
end module traits_protocol_separate_01_oracle_provider_m

subroutine make_sum(choice, object)
    use traits_protocol_separate_01_oracle_contracts_m, only: ISum
    use traits_protocol_separate_01_oracle_provider_m, only: build_sum
    implicit none
    integer, intent(in) :: choice
    class(ISum), allocatable, intent(out) :: object
    call build_sum(choice, object)
end subroutine make_sum

subroutine make_averager(choice, object)
    use traits_protocol_separate_01_oracle_contracts_m, only: IAverager
    use traits_protocol_separate_01_oracle_provider_m, only: build_averager
    implicit none
    integer, intent(in) :: choice
    class(IAverager), allocatable, intent(out) :: object
    call build_averager(choice, object)
end subroutine make_averager

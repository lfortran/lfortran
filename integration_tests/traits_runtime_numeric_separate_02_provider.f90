module traits_runtime_numeric_separate_02_provider_m
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use traits_runtime_numeric_separate_02_contracts_m, only: INumeric, ISum, &
        IAverager, IPhase
    implicit none
    private
    public :: build_sum, build_averager, build_phase

    type, sealed, implements(ISum) :: HiddenSimple
    contains
        procedure, pass :: sum => hidden_simple_sum
    end type HiddenSimple

    ! The passed object is the second dummy argument.
    type, sealed, implements(ISum) :: HiddenPairwise
        private
        class(ISum), allocatable :: other
    contains
        initial :: hidden_pairwise_init
        procedure, pass(self) :: sum => hidden_pairwise_sum
    end type HiddenPairwise

    type, sealed, implements(ISum) :: HiddenScaled
        private
        integer :: factor = 1
    contains
        initial :: hidden_scaled_init
        procedure, pass :: sum => hidden_scaled_sum
    end type HiddenScaled

    type, sealed, implements(IAverager) :: HiddenAverager
        private
        class(ISum), allocatable :: drv
    contains
        initial :: hidden_averager_init
        procedure, pass :: average => hidden_average
    end type HiddenAverager

    type, sealed, implements(IPhase) :: HiddenTurn
    contains
        procedure, nopass :: turn => hidden_turn
        procedure, nopass :: mirror => hidden_mirror
    end type HiddenTurn

contains

    function hidden_simple_sum{INumeric :: T}(self, x) result(s)
        type(HiddenSimple), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function hidden_simple_sum

    function hidden_pairwise_init(other) result(res)
        class(ISum), intent(in) :: other
        type(HiddenPairwise)    :: res
        res%other = other
    end function hidden_pairwise_init

    function hidden_pairwise_sum{INumeric :: T}(x, self) result(s)
        type(T),              intent(in) :: x(:)
        type(HiddenPairwise), intent(in) :: self
        type(T)                          :: s
        integer                          :: m
        if (size(x) <= 2) then
            s = self%other%sum(x)
        else
            m = size(x) / 2
            s = self%sum(x(:m)) + self%sum(x=x(m+1:))
        end if
    end function hidden_pairwise_sum

    function hidden_scaled_init(factor) result(res)
        integer, intent(in) :: factor
        type(HiddenScaled)  :: res
        res%factor = factor
    end function hidden_scaled_init

    function hidden_scaled_sum{INumeric :: T}(self, x) result(s)
        type(HiddenScaled), intent(in) :: self
        type(T),            intent(in) :: x(:)
        type(T)                        :: s
        integer                        :: i
        s = T(0)
        do i = size(x), 1, -1
            s = s + x(i)
        end do
        s = T(self%factor) * s
    end function hidden_scaled_sum

    function hidden_averager_init(drv) result(res)
        class(ISum), intent(in) :: drv
        type(HiddenAverager)    :: res
        res%drv = drv
    end function hidden_averager_init

    function hidden_average{INumeric :: T}(self, x) result(a)
        type(HiddenAverager), intent(in) :: self
        type(T),              intent(in) :: x(:)
        type(T)                          :: a
        a = self%drv%sum(x) / T(size(x))
    end function hidden_average

    function hidden_turn(z, quarters) result(r)
        complex(real64), intent(in) :: z
        integer,         intent(in) :: quarters
        complex(real64)             :: r
        integer                     :: i
        r = z
        do i = 1, modulo(quarters, 4)
            r = r * (0.0_real64, 1.0_real64)
        end do
    end function hidden_turn

    function hidden_mirror(z) result(r)
        complex(real32), intent(in) :: z
        complex(real32)             :: r
        r = conjg(z)
    end function hidden_mirror

    subroutine build_sum(choice, object)
        integer, intent(in) :: choice
        class(ISum), allocatable, intent(out) :: object
        select case (choice)
        case (1)
            object = HiddenSimple()
        case (2)
            object = HiddenPairwise(HiddenSimple())
        case (3)
            object = HiddenPairwise(HiddenScaled(3))
        case default
            error stop 'unknown sum choice'
        end select
    end subroutine build_sum

    subroutine build_averager(choice, object)
        integer, intent(in) :: choice
        class(IAverager), allocatable, intent(out) :: object
        select case (choice)
        case (1)
            object = HiddenAverager(HiddenSimple())
        case (2)
            object = HiddenAverager(HiddenPairwise(HiddenSimple()))
        case (3)
            object = HiddenAverager(HiddenPairwise(HiddenScaled(3)))
        case default
            error stop 'unknown averager choice'
        end select
    end subroutine build_averager

    subroutine build_phase(object)
        class(IPhase), allocatable, intent(out) :: object
        object = HiddenTurn()
    end subroutine build_phase
end module traits_runtime_numeric_separate_02_provider_m

subroutine make_sum(choice, object)
    use traits_runtime_numeric_separate_02_contracts_m, only: ISum
    use traits_runtime_numeric_separate_02_provider_m, only: build_sum
    implicit none
    integer, intent(in) :: choice
    class(ISum), allocatable, intent(out) :: object
    call build_sum(choice, object)
end subroutine make_sum

subroutine make_averager(choice, object)
    use traits_runtime_numeric_separate_02_contracts_m, only: IAverager
    use traits_runtime_numeric_separate_02_provider_m, only: build_averager
    implicit none
    integer, intent(in) :: choice
    class(IAverager), allocatable, intent(out) :: object
    call build_averager(choice, object)
end subroutine make_averager

subroutine make_phase(object)
    use traits_runtime_numeric_separate_02_contracts_m, only: IPhase
    use traits_runtime_numeric_separate_02_provider_m, only: build_phase
    implicit none
    class(IPhase), allocatable, intent(out) :: object
    call build_phase(object)
end subroutine make_phase

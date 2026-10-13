! Standard-Fortran counterpart of traits_runtime_numeric_separate_01_provider.f90.
module traits_runtime_numeric_separate_01_oracle_provider_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_runtime_numeric_separate_01_oracle_contracts_m, only: ISum, IAverager
    implicit none
    private
    public :: build_sum, build_averager

    type, extends(ISum) :: HiddenSimple
    contains
        procedure :: sum_integer => hidden_simple_integer
        procedure :: sum_real64 => hidden_simple_real64
    end type HiddenSimple

    type, extends(ISum) :: HiddenPairwise
        private
        class(ISum), allocatable :: other
    contains
        procedure :: sum_integer => hidden_pairwise_integer
        procedure :: sum_real64 => hidden_pairwise_real64
    end type HiddenPairwise

    type, extends(ISum) :: HiddenScaled
        private
        integer :: factor = 1
    contains
        procedure :: sum_integer => hidden_scaled_integer
        procedure :: sum_real64 => hidden_scaled_real64
    end type HiddenScaled

    type, extends(IAverager) :: HiddenAverager
        private
        class(ISum), allocatable :: drv
    contains
        procedure :: average_integer => hidden_average_integer
        procedure :: average_real64 => hidden_average_real64
    end type HiddenAverager

    interface HiddenPairwise
        module procedure hidden_pairwise_init
    end interface HiddenPairwise

    interface HiddenScaled
        module procedure hidden_scaled_init
    end interface HiddenScaled

    interface HiddenAverager
        module procedure hidden_averager_init
    end interface HiddenAverager

contains

    function hidden_simple_integer(self, x) result(s)
        class(HiddenSimple), intent(in) :: self
        integer,             intent(in) :: x(:)
        integer                         :: s
        integer                         :: i
        s = int(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function hidden_simple_integer

    function hidden_simple_real64(self, x) result(s)
        class(HiddenSimple), intent(in) :: self
        real(real64),        intent(in) :: x(:)
        real(real64)                    :: s
        integer                         :: i
        s = real(0, kind=real64)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function hidden_simple_real64

    function hidden_pairwise_init(other) result(res)
        class(ISum), intent(in) :: other
        type(HiddenPairwise)    :: res
        res%other = other
    end function hidden_pairwise_init

    recursive function hidden_pairwise_integer(self, x) result(s)
        class(HiddenPairwise), intent(in) :: self
        integer,               intent(in) :: x(:)
        integer                           :: s
        integer                           :: m
        if (size(x) <= 2) then
            s = self%other%sum(x)
        else
            m = size(x) / 2
            s = self%sum(x(:m)) + self%sum(x(m+1:))
        end if
    end function hidden_pairwise_integer

    recursive function hidden_pairwise_real64(self, x) result(s)
        class(HiddenPairwise), intent(in) :: self
        real(real64),          intent(in) :: x(:)
        real(real64)                      :: s
        integer                           :: m
        if (size(x) <= 2) then
            s = self%other%sum(x)
        else
            m = size(x) / 2
            s = self%sum(x(:m)) + self%sum(x(m+1:))
        end if
    end function hidden_pairwise_real64

    function hidden_scaled_init(factor) result(res)
        integer, intent(in) :: factor
        type(HiddenScaled)  :: res
        res%factor = factor
    end function hidden_scaled_init

    function hidden_scaled_integer(self, x) result(s)
        class(HiddenScaled), intent(in) :: self
        integer,             intent(in) :: x(:)
        integer                         :: s
        integer                         :: i
        s = int(0)
        do i = size(x), 1, -1
            s = s + x(i)
        end do
        s = int(self%factor) * s
    end function hidden_scaled_integer

    function hidden_scaled_real64(self, x) result(s)
        class(HiddenScaled), intent(in) :: self
        real(real64),        intent(in) :: x(:)
        real(real64)                    :: s
        integer                         :: i
        s = real(0, kind=real64)
        do i = size(x), 1, -1
            s = s + x(i)
        end do
        s = real(self%factor, kind=real64) * s
    end function hidden_scaled_real64

    function hidden_averager_init(drv) result(res)
        class(ISum), intent(in) :: drv
        type(HiddenAverager)    :: res
        res%drv = drv
    end function hidden_averager_init

    function hidden_average_integer(self, x) result(a)
        class(HiddenAverager), intent(in) :: self
        integer,               intent(in) :: x(:)
        integer                           :: a
        a = self%drv%sum(x) / int(size(x))
    end function hidden_average_integer

    function hidden_average_real64(self, x) result(a)
        class(HiddenAverager), intent(in) :: self
        real(real64),          intent(in) :: x(:)
        real(real64)                      :: a
        a = self%drv%sum(x) / real(size(x), kind=real64)
    end function hidden_average_real64

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
end module traits_runtime_numeric_separate_01_oracle_provider_m

subroutine make_sum(choice, object)
    use traits_runtime_numeric_separate_01_oracle_contracts_m, only: ISum
    use traits_runtime_numeric_separate_01_oracle_provider_m, only: build_sum
    implicit none
    integer, intent(in) :: choice
    class(ISum), allocatable, intent(out) :: object
    call build_sum(choice, object)
end subroutine make_sum

subroutine make_averager(choice, object)
    use traits_runtime_numeric_separate_01_oracle_contracts_m, only: IAverager
    use traits_runtime_numeric_separate_01_oracle_provider_m, only: build_averager
    implicit none
    integer, intent(in) :: choice
    class(IAverager), allocatable, intent(out) :: object
    call build_averager(choice, object)
end subroutine make_averager

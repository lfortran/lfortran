! Generic derived types that extend a generic type, giving it their own
! parameters as its type arguments: one with the parent's parameter name,
! sealed and repeating the conformance it inherits (PairwiseSum), one with
! another name (Scaled). Their instances extend the parent's instance for
! those arguments; an inherited component of a parameter type, constructors
! with positional values in component order and with the parent component by
! keyword, overrides of the parent's generic binding through the parent's
! instance, and runtime views of the instances are covered. A counter of leaf
! sums tells the pairwise summation from the simple one.
module traits_generic_type_extends_01_interfaces
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: INumeric, ISum, IAverager

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(self, x) result(s)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: x(:)
            type(T) :: s
        end function sum
    end interface ISum

    abstract interface :: IAverager
        function average{INumeric :: T}(self, x) result(a)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: x(:)
            type(T) :: a
        end function average
    end interface IAverager
end module traits_generic_type_extends_01_interfaces

module traits_generic_type_extends_01_basic
    use traits_generic_type_extends_01_interfaces, only: ISum, INumeric
    implicit none
    private
    public :: BasicSum, leaves

    integer :: leaves = 0

    type, sealed, implements(ISum) :: BasicSum
    contains
        procedure :: sum
    end type BasicSum

contains

    function sum{INumeric :: T}(self, x) result(s)
        type(BasicSum), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        leaves = leaves + 1
        s = T(0)
        do i := 1, size(x)
            s = s + x(i)
        end do
    end function sum
end module traits_generic_type_extends_01_basic

module traits_generic_type_extends_01_simple
    use traits_generic_type_extends_01_interfaces, only: ISum, INumeric
    implicit none
    private
    public :: SimpleSum

    type, implements(ISum) :: SimpleSum{ISum :: U}
        type(U), allocatable :: basic
    contains
        initial :: init
        procedure :: sum
    end type SimpleSum

contains

    function init(basic) result(res)
        type(U), intent(in) :: basic
        type(SimpleSum{U}) :: res
        res%basic = basic
    end function init

    function sum{INumeric :: T}(self, x) result(s)
        class(SimpleSum{U}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = self%basic%sum(x)
    end function sum
end module traits_generic_type_extends_01_simple

module traits_generic_type_extends_01_pairwise
    use traits_generic_type_extends_01_interfaces, only: ISum, INumeric
    use traits_generic_type_extends_01_simple, only: SimpleSum
    implicit none
    private
    public :: PairwiseSum, Scaled

    ! The conformance to ISum, which SimpleSum{U} already has, is repeated.
    type, sealed, extends(SimpleSum{U}), implements(ISum) :: PairwiseSum{ISum :: U}
    contains
        procedure, pass :: sum
    end type PairwiseSum

    ! SimpleSum's U is W.
    type, extends(SimpleSum{W}) :: Scaled{ISum :: W}
        integer :: factor = 10
    contains
        procedure :: sum => scaled_sum
    end type Scaled

contains

    function sum{INumeric :: T}(self, x) result(s)
        type(PairwiseSum{U}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        if (size(x) <= 2) then
            s = self%basic%sum(x)
        else
            m := size(x) / 2
            s = self%sum(x(:m)) + self%sum(x(m+1:))
        end if
    end function sum

    function scaled_sum{INumeric :: T}(self, x) result(s)
        class(Scaled{W}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(10) * self%basic%sum(x)
    end function scaled_sum
end module traits_generic_type_extends_01_pairwise

module traits_generic_type_extends_01_averager
    use traits_generic_type_extends_01_interfaces, only: IAverager, ISum, INumeric
    implicit none
    private
    public :: Averager

    type, sealed, implements(IAverager) :: Averager{ISum :: U}
        private
        type(U), allocatable :: drv
    contains
        initial :: init
        procedure, pass :: average
    end type Averager

contains

    function init(drv) result(res)
        type(U), intent(in) :: drv
        type(Averager{U}) :: res
        res%drv = drv
    end function init

    function average{INumeric :: T}(self, x) result(a)
        type(Averager{U}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: a
        a = self%drv%sum(x) / T(size(x))
    end function average
end module traits_generic_type_extends_01_averager

program traits_generic_type_extends_01
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_type_extends_01_interfaces, only: IAverager
    use traits_generic_type_extends_01_basic, only: BasicSum, leaves
    use traits_generic_type_extends_01_simple, only: SimpleSum
    use traits_generic_type_extends_01_pairwise, only: PairwiseSum, Scaled
    use traits_generic_type_extends_01_averager, only: Averager
    implicit none
    class(IAverager), allocatable :: av
    class(SimpleSum{BasicSum}), allocatable :: s
    type(PairwiseSum{BasicSum}) :: p
    integer :: key, xi(5)
    real(real64) :: xf(5)

    xi = [1, 2, 3, 4, 5]
    xf = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]

    do key = 1, 3
        select case (key)
        case (1)
            av = Averager(drv = SimpleSum(BasicSum()))
        case (2)
            ! Positional: U = SimpleSum{BasicSum}, basic = SimpleSum(BasicSum()).
            av = Averager(drv = PairwiseSum(SimpleSum(BasicSum())))
        case (3)
            ! The parent component by keyword: U = BasicSum.
            av = Averager(drv = PairwiseSum(simplesum = SimpleSum(BasicSum())))
        end select
        leaves = 0
        if (av%average(xi) /= 3) error stop 1
        if (abs(av%average(xf) - 3.0_real64) > 1e-12_real64) error stop 2
        if (key == 1 .and. leaves /= 2) error stop 3
        if (key > 1 .and. leaves /= 6) error stop 4
        deallocate(av)
    end do

    ! The parent component and the inherited component of the instance.
    p = PairwiseSum(simplesum = SimpleSum(BasicSum()))
    leaves = 0
    if (p%sum(xi) /= 15) error stop 5
    if (leaves /= 3) error stop 6
    if (p%simplesum%sum(xi) /= 15) error stop 7
    if (leaves /= 4) error stop 8

    ! Through the parent's instance, the dynamic type's override runs.
    allocate(s, source = SimpleSum(BasicSum()))
    leaves = 0
    if (s%sum(xi) /= 15 .or. leaves /= 1) error stop 9
    deallocate(s)
    allocate(s, source = p)
    leaves = 0
    if (s%sum(xi) /= 15 .or. leaves /= 3) error stop 10
    deallocate(s)
    allocate(s, source = Scaled(BasicSum()))
    if (s%sum(xi) /= 150) error stop 11
    if (abs(s%sum(xf) - 150.0_real64) > 1e-12_real64) error stop 12

    print *, "ok"
end program traits_generic_type_extends_01

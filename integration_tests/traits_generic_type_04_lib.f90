! Libraries of traits_generic_type_04: traits, a provider and two generic
! derived types.
module traits_generic_type_04_contracts
    implicit none
    private
    public :: INumeric, ISum, IAverager
    abstract interface :: INumeric
        integer | real(8)
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
end module traits_generic_type_04_contracts

module traits_generic_type_04_simple
    use traits_generic_type_04_contracts, only: ISum, INumeric
    implicit none
    private
    public :: SimpleSum
    type, sealed, implements(ISum) :: SimpleSum
    contains
        procedure, pass :: sum => simple_sum
    end type SimpleSum
contains
    function simple_sum{INumeric :: T}(self, x) result(s)
        type(SimpleSum), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        integer :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function simple_sum
end module traits_generic_type_04_simple

module traits_generic_type_04_pairwise
    use traits_generic_type_04_contracts, only: ISum, INumeric
    implicit none
    private
    public :: PairwiseSum
    type, sealed, implements(ISum) :: PairwiseSum{ISum :: U}
        type(U), allocatable :: other
    contains
        initial :: init
        procedure, pass :: sum
    end type PairwiseSum
contains
    function init(other) result(res)
        type(U), intent(in) :: other
        type(PairwiseSum{U}) :: res
        res%other = other
    end function init
    function sum{INumeric :: T}(self, x) result(s)
        type(PairwiseSum{U}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        integer :: m
        if (size(x) <= 2) then
            s = self%other%sum(x)
        else
            m = size(x) / 2
            s = self%sum(x(:m)) + self%sum(x(m+1:))
        end if
    end function sum
end module traits_generic_type_04_pairwise

module traits_generic_type_04_averager
    use traits_generic_type_04_contracts, only: IAverager, ISum, INumeric
    implicit none
    private
    public :: Averager
    type, sealed, implements(IAverager) :: Averager{ISum :: U}
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
end module traits_generic_type_04_averager

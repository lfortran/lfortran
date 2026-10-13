! Generic derived types: located diagnostics for the visibility of instance
! evidence, the privacy of a module's instances, the runtime evidence a main
! program may own, conformances its unit declares later and requirement
! calls the prototype does not support. Each case is isolated in its own
! scoping unit so continue-compilation reports exactly one diagnostic per
! case without cascades.
module gt_rep_contracts
    implicit none
    private
    public :: INumeric, ISum, IAverager
    abstract interface :: INumeric
        integer | real(8)
    end interface INumeric
    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum
    abstract interface :: IAverager
        function average{INumeric :: T}(x) result(a)
            type(T), intent(in) :: x(:)
            type(T)             :: a
        end function average
    end interface IAverager
end module gt_rep_contracts

module gt_rep_leaf
    implicit none
    private
    public :: Leaf
    type :: Leaf
        integer :: pad = 0
    end type Leaf
end module gt_rep_leaf

module gt_rep_provider
    use gt_rep_contracts, only: ISum, INumeric
    use gt_rep_leaf, only: Leaf
    implicit none
    implements ISum :: Leaf
        procedure, nopass :: sum => leaf_sum
    end implements
contains
    function leaf_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = x(1)
    end function leaf_sum
end module gt_rep_provider

module gt_rep_simple
    use gt_rep_contracts, only: ISum, INumeric
    implicit none
    private
    public :: SimpleSum
    type, sealed, implements(ISum) :: SimpleSum
    contains
        procedure, nopass :: sum => simple_sum
    end type SimpleSum
contains
    function simple_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = x(1)
    end function simple_sum
end module gt_rep_simple

module gt_rep_types
    use gt_rep_contracts, only: ISum, IAverager, INumeric
    implicit none
    private
    public :: PairwiseSum, Averager
    type, sealed, implements(ISum) :: PairwiseSum{ISum :: U}
        type(U), allocatable :: other
    contains
        initial :: init_pair
        procedure, pass :: sum
    end type PairwiseSum
    type, sealed, implements(IAverager) :: Averager{ISum :: U}
        type(U), allocatable :: drv
    contains
        initial :: init_avg
        procedure, pass :: average
    end type Averager
contains
    function init_pair(other) result(res)
        type(U), intent(in)  :: other
        type(PairwiseSum{U}) :: res
        res%other = other
    end function init_pair
    function sum{INumeric :: T}(self, x) result(s)
        type(PairwiseSum{U}), intent(in) :: self
        type(T),              intent(in) :: x(:)
        type(T)                          :: s
        s = self%other%sum(x)
    end function sum
    function init_avg(drv) result(res)
        type(U), intent(in) :: drv
        type(Averager{U})   :: res
        res%drv = drv
    end function init_avg
    function average{INumeric :: T}(self, x) result(a)
        type(Averager{U}), intent(in) :: self
        type(T),           intent(in) :: x(:)
        type(T)                       :: a
        a = self%drv%sum(x) / T(size(x))
    end function average
end module gt_rep_types

module gt_rep_generic
    use gt_rep_contracts, only: ISum
    implicit none
contains
    function total_of{ISum :: V}(item) result(r)
        type(V), intent(in) :: item
        integer             :: r
        r = item%sum([1, 2, 3])
    end function total_of
end module gt_rep_generic

! R01: the conformance a procedure sees for an instance's argument does not
! become visible to the rest of its module.
module gt_rep_01
    use gt_rep_leaf, only: Leaf
    use gt_rep_types, only: PairwiseSum
    implicit none
contains
    integer function with_provider() result(r)
        use gt_rep_provider
        type(PairwiseSum{Leaf}) :: p
        p = PairwiseSum(Leaf())
        r = p%sum([1, 2])
    end function with_provider
    integer function without_provider() result(r)
        q := PairwiseSum(Leaf())
        r = 0
    end function without_provider
end module gt_rep_01

! R02: nor for a generic procedure's type parameter.
module gt_rep_02
    use gt_rep_leaf, only: Leaf
    use gt_rep_types, only: PairwiseSum
    use gt_rep_generic, only: total_of
    implicit none
contains
    integer function with_provider() result(r)
        use gt_rep_provider
        type(PairwiseSum{Leaf}) :: p
        p = PairwiseSum(Leaf())
        r = p%sum([1, 2])
    end function with_provider
    integer function without_provider() result(r)
        r = total_of(Leaf())
    end function without_provider
end module gt_rep_02

! A private module that owns instances.
module gt_rep_owner
    use gt_rep_simple, only: SimpleSum
    use gt_rep_types, only: Averager
    implicit none
    private
    public :: make_avg
contains
    function make_avg() result(a)
        type(Averager{SimpleSum}) :: a
        a = Averager(SimpleSum())
    end function make_avg
end module gt_rep_owner

! R03: the names an instance and its procedures once had are no entities of
! the owner.
module gt_rep_03
    use gt_rep_owner, only: averager__simplesum
    implicit none
end module gt_rep_03

! R04 (boundary): a USE of the owner imports none of its instances'
! conformances, so a value of its instance has no runtime view here.
module gt_rep_04
    use gt_rep_contracts, only: IAverager
    use gt_rep_owner
    implicit none
contains
    subroutine view()
        class(IAverager), allocatable :: av
        av = make_avg()
    end subroutine view
end module gt_rep_04

! R05: a type of the same unit that never gets the conformance is diagnosed
! where the instance is declared.
module gt_rep_05
    use gt_rep_types, only: PairwiseSum
    implicit none
    type :: NoSum
        integer :: n = 0
    end type NoSum
    type(PairwiseSum{NoSum}), save :: shared
end module gt_rep_05

! R06 (prototype boundary): a requirement call through a type parameter with
! concrete type arguments is diagnosed where the generic type is compiled.
module gt_rep_06
    use gt_rep_contracts, only: ISum, INumeric
    implicit none
    private
    public :: Fixed
    type, sealed :: Fixed{ISum :: U}
        type(U), allocatable :: other
    contains
        procedure, pass :: probe
    end type Fixed
contains
    function probe{INumeric :: T}(self, x) result(s)
        type(Fixed{U}), intent(in) :: self
        type(T),        intent(in) :: x(:)
        type(T)                    :: s
        integer                    :: k
        k = self%other%sum([1, 2, 3])
        s = self%other%sum(x) + T(k)
    end function probe
end module gt_rep_06

! R07-R10 (boundary): a conformance the main program writes itself, and an
! instance whose argument uses one, directly or through another instance,
! has no runtime evidence, whether or not its procedures read the
! program's state.
program gt_rep_program
    use gt_rep_contracts, only: ISum, IAverager, INumeric
    use gt_rep_types, only: PairwiseSum, Averager
    implicit none
    type, sealed, implements(ISum) :: ReadsState
    contains
        procedure, nopass :: sum => reads_state
    end type ReadsState
    type, sealed, implements(ISum) :: CallsReader
    contains
        procedure, nopass :: sum => calls_reader
    end type CallsReader
    type, sealed, implements(ISum) :: Pure
    contains
        procedure, nopass :: sum => pure_sum
    end type Pure
    class(IAverager), allocatable :: av
    integer :: bias
    bias = 100
    av = Averager(drv = ReadsState())
    av = Averager(drv = CallsReader())
    av = Averager(drv = Pure())
    av = Averager(drv = PairwiseSum(other = Pure()))
contains
    integer function get_bias() result(b)
        b = bias
    end function get_bias
    function reads_state{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = T(bias) + x(1)
    end function reads_state
    function calls_reader{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = T(get_bias()) + x(1)
    end function calls_reader
    function pure_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = x(1)
    end function pure_sum
end program gt_rep_program

! Generic derived type instances select the conformance of each type argument
! where they are used. Two internal procedures that see different
! conformances of one type create two instances, and the conformance one of
! them sees stays invisible to the rest of the program. A type whose
! conformance its own unit declares after the instance is declared gets that
! conformance, and a conformance the main program writes itself is used
! statically.
module traits_generic_type_02_contracts
    implicit none
    private
    public :: INumeric, ISum, IAverager
    abstract interface :: INumeric
        integer | real(8)
    end interface INumeric
    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T) :: s
        end function sum
    end interface ISum
    abstract interface :: IAverager
        function average{INumeric :: T}(x) result(a)
            type(T), intent(in) :: x(:)
            type(T) :: a
        end function average
    end interface IAverager
end module traits_generic_type_02_contracts

module traits_generic_type_02_leaf
    implicit none
    private
    public :: Leaf
    type :: Leaf
        integer :: pad = 0
    end type Leaf
end module traits_generic_type_02_leaf

module traits_generic_type_02_provider_a
    use traits_generic_type_02_contracts, only: ISum, INumeric
    use traits_generic_type_02_leaf, only: Leaf
    implicit none
    implements ISum :: Leaf
        procedure, nopass :: sum => sum_a
    end implements
contains
    function sum_a{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T) :: s
        integer :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function sum_a
end module traits_generic_type_02_provider_a

module traits_generic_type_02_provider_b
    use traits_generic_type_02_contracts, only: ISum, INumeric
    use traits_generic_type_02_leaf, only: Leaf
    implicit none
    implements ISum :: Leaf
        procedure, nopass :: sum => sum_b
    end implements
contains
    function sum_b{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T) :: s
        integer :: i
        s = T(100)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function sum_b
end module traits_generic_type_02_provider_b

module traits_generic_type_02_pairwise
    use traits_generic_type_02_contracts, only: ISum, INumeric
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
end module traits_generic_type_02_pairwise

module traits_generic_type_02_averager
    use traits_generic_type_02_contracts, only: IAverager, ISum, INumeric
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
end module traits_generic_type_02_averager

! The module's own conformance of MySum is complete only at the end of the
! module, after the declarations that use PairwiseSum{MySum}.
module traits_generic_type_02_app
    use traits_generic_type_02_contracts, only: ISum, INumeric
    use traits_generic_type_02_pairwise, only: PairwiseSum
    implicit none
    private
    public :: run_shared, run_local
    type, sealed, implements(ISum) :: MySum
    contains
        procedure, nopass :: sum => my_sum
    end type MySum
    type(PairwiseSum{MySum}), save :: shared
contains
    function my_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T) :: s
        integer :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function my_sum
    integer function run_shared() result(r)
        shared = PairwiseSum(MySum())
        r = shared%sum([1, 2, 3, 4, 5])
    end function run_shared
    integer function run_local() result(r)
        type(PairwiseSum{MySum}) :: p
        p = PairwiseSum(MySum())
        r = p%sum([10, 20, 30])
    end function run_local
end module traits_generic_type_02_app

program traits_generic_type_02
    use traits_generic_type_02_contracts, only: ISum, INumeric
    use traits_generic_type_02_leaf, only: Leaf
    use traits_generic_type_02_pairwise, only: PairwiseSum
    use traits_generic_type_02_averager, only: Averager
    use traits_generic_type_02_app, only: run_shared, run_local
    implicit none
    ! A conformance the main program writes itself: its instances are used
    ! statically only.
    type, sealed, implements(ISum) :: LocalSum
    contains
        procedure, nopass :: sum => local_sum
    end type LocalSum
    type(Averager{LocalSum}) :: a
    integer :: bias, checks
    checks = 0
    ! Each procedure sees one provider of Leaf's conformance; its instance
    ! of PairwiseSum{Leaf} uses that provider's.
    call check(run_a() == 15)
    call check(run_b() == 315)
    call check(run_c() == 15)
    call check(run_d() == 315)
    call check(run_a() + run_b() == 330)
    ! The conformance the module declares after the declarations.
    call check(run_shared() == 15)
    call check(run_local() == 60)
    ! The program's own conformance, statically: the copies read the
    ! program's current state through LocalSum's procedure.
    bias = 100
    a = Averager(drv = LocalSum())
    call check(a%average([1, 2, 3, 4, 5]) == 23)
    bias = 200
    call check(a%average([1, 2, 3, 4, 5]) == 43)
    call check(abs(a%average([1.d0, 2.d0, 3.d0, 4.d0]) - 52.5d0) < 1.d-12)
    print '(a, i0)', "traits_generic_type_02 checks: ", checks
contains
    subroutine check(condition)
        logical, intent(in) :: condition
        if (.not. condition) error stop
        checks = checks + 1
    end subroutine check
    integer function run_a() result(r)
        use traits_generic_type_02_provider_a
        type(PairwiseSum{Leaf}) :: p
        p = PairwiseSum(Leaf())
        r = p%sum([1, 2, 3, 4, 5])
    end function run_a
    integer function run_b() result(r)
        use traits_generic_type_02_provider_b
        type(PairwiseSum{Leaf}) :: q
        q = PairwiseSum(Leaf())
        r = q%sum([1, 2, 3, 4, 5])
    end function run_b
    integer function run_c() result(r)
        use traits_generic_type_02_provider_a
        p := PairwiseSum(Leaf())
        r = p%sum([1, 2, 3, 4, 5])
    end function run_c
    integer function run_d() result(r)
        use traits_generic_type_02_provider_b
        q := PairwiseSum(Leaf())
        r = q%sum([1, 2, 3, 4, 5])
    end function run_d
    integer function get_bias() result(b)
        b = bias
    end function get_bias
    function local_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T) :: s
        integer :: i
        s = T(get_bias())
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function local_sum
end program traits_generic_type_02

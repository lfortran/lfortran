! Generic derived types: an internal procedure of the main program cannot yet
! use, through host association, a variable of the program whose type is or
! holds a generic type instance the program creates. Each such variable is
! diagnosed at its first use (H1-H6). An instance type used inside an
! internal procedure, an instance passed as an argument, host association of
! an ordinary program type and of an instance a module creates stay
! supported (C1-C4) and get no diagnostic.
module gt_host_contracts
    implicit none
    private
    public :: INumeric, ISum
    abstract interface :: INumeric
        integer | real(8)
    end interface INumeric
    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T) :: s
        end function sum
    end interface ISum
end module gt_host_contracts

module gt_host_simple
    use gt_host_contracts, only: ISum, INumeric
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
        type(T) :: s
        integer :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function simple_sum
end module gt_host_simple

module gt_host_types
    use gt_host_contracts, only: ISum, INumeric
    implicit none
    private
    public :: Averager, Holder
    type, sealed :: Averager{ISum :: U}
        type(U), allocatable :: drv
    contains
        initial :: init
        procedure, pass :: average
    end type Averager
    ! A generic type with no trait of its own.
    type, sealed :: Holder{ISum :: U}
        type(U) :: item
        integer :: scale = 1
    contains
        procedure, pass :: total
    end type Holder
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
    function total{INumeric :: T}(self, x) result(s)
        type(Holder{U}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        s = T(self%scale) * self%item%sum(x)
    end function total
end module gt_host_types

module gt_host_owner
    use gt_host_simple, only: SimpleSum
    use gt_host_types, only: Averager
    implicit none
    private
    public :: make_avg
contains
    function make_avg() result(a)
        type(Averager{SimpleSum}) :: a
        a = Averager(drv = SimpleSum())
    end function make_avg
end module gt_host_owner

program gt_host_program
    use gt_host_simple, only: SimpleSum
    use gt_host_types, only: Averager, Holder
    use gt_host_owner, only: make_avg
    implicit none
    type :: Wrap
        type(Averager{SimpleSum}) :: inner
    end type Wrap
    type :: Plain
        integer :: k = 1
    end type Plain
    type(Averager{SimpleSum}) :: a
    type(Wrap) :: w
    type(Holder{SimpleSum}) :: h
    type(Averager{SimpleSum}), allocatable :: aa
    type(Averager{SimpleSum}) :: arr(2)
    type(Averager{SimpleSum}), pointer :: ap => null()
    type(Plain) :: pl
    a = Averager(drv = SimpleSum())
    m := make_avg()
    print *, h1(), h2(), h3(), h4(), h5(), h6()
    print *, c1(a), c2(), c3(), c4()
contains
    ! H1: an instance the program creates.
    integer function h1() result(r)
        r = a%average([1, 2, 3])
    end function h1
    ! H2: a program type with a component of such an instance.
    integer function h2() result(r)
        r = w%inner%average([1, 2, 3])
    end function h2
    ! H3: an instance of a generic type with no trait of its own.
    integer function h3() result(r)
        type(Holder{SimpleSum}) :: g
        g = h
        r = g%total([1, 2, 3])
    end function h3
    ! H4: an allocatable instance, used only in an inquiry.
    integer function h4() result(r)
        r = merge(1, 0, allocated(aa))
    end function h4
    ! H5: an array of instances.
    integer function h5() result(r)
        r = arr(1)%average([1, 2, 3])
    end function h5
    ! H6: a pointer to an instance.
    integer function h6() result(r)
        r = merge(1, 0, associated(ap))
    end function h6
    ! C1: an instance passed as an argument.
    integer function c1(x) result(r)
        type(Averager{SimpleSum}), intent(in) :: x
        r = x%average([1, 2, 3])
    end function c1
    ! C2: an instance the internal procedure declares itself.
    integer function c2() result(r)
        type(Averager{SimpleSum}) :: local
        local = Averager(drv = SimpleSum())
        r = local%average([1, 2, 3])
    end function c2
    ! C3: an ordinary program type.
    integer function c3() result(r)
        r = pl%k
    end function c3
    ! C4: an instance a module creates.
    integer function c4() result(r)
        r = m%average([1, 2, 3])
    end function c4
end program gt_host_program

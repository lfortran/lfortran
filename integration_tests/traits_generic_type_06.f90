! Host association next to generic derived type instances that stays
! supported: an internal procedure of the main program gets an instance the
! program creates as an argument or declares one itself, and uses the
! program's variables of an ordinary type and of an instance a module creates
! through host association; an internal procedure of a module procedure uses
! that procedure's instance variable through host association.
module traits_generic_type_06_contracts
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
end module traits_generic_type_06_contracts

module traits_generic_type_06_simple
    use traits_generic_type_06_contracts, only: ISum, INumeric
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
end module traits_generic_type_06_simple

module traits_generic_type_06_types
    use traits_generic_type_06_contracts, only: ISum, INumeric
    implicit none
    private
    public :: Averager
    type, sealed :: Averager{ISum :: U}
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
end module traits_generic_type_06_types

module traits_generic_type_06_owner
    use traits_generic_type_06_simple, only: SimpleSum
    use traits_generic_type_06_types, only: Averager
    implicit none
    private
    public :: make_avg, run_inner
contains
    function make_avg() result(a)
        type(Averager{SimpleSum}) :: a
        a = Averager(drv = SimpleSum())
    end function make_avg
    integer function run_inner() result(r)
        type(Averager{SimpleSum}) :: a
        a = Averager(drv = SimpleSum())
        r = inner()
    contains
        integer function inner() result(q)
            q = a%average([2, 4, 6])
        end function inner
    end function run_inner
end module traits_generic_type_06_owner

program traits_generic_type_06
    use traits_generic_type_06_simple, only: SimpleSum
    use traits_generic_type_06_types, only: Averager
    use traits_generic_type_06_owner, only: make_avg, run_inner
    implicit none
    type :: Plain
        integer :: k = 7
    end type Plain
    type(Averager{SimpleSum}) :: a
    type(Plain) :: pl
    integer :: checks
    checks = 0
    a = Averager(drv = SimpleSum())
    m := make_avg()
    call check(by_argument(a) == 3)
    call check(own_instance() == 5)
    call check(plain_host() == 7)
    call check(module_instance_host() == 2)
    call check(run_inner() == 4)
    print '(a, i0)', "traits_generic_type_06 checks: ", checks
contains
    subroutine check(condition)
        logical, intent(in) :: condition
        if (.not. condition) error stop
        checks = checks + 1
    end subroutine check
    integer function by_argument(x) result(r)
        type(Averager{SimpleSum}), intent(in) :: x
        r = x%average([1, 2, 3, 4, 5])
    end function by_argument
    integer function own_instance() result(r)
        type(Averager{SimpleSum}) :: c
        c = Averager(drv = SimpleSum())
        r = c%average([4, 6])
    end function own_instance
    integer function plain_host() result(r)
        r = pl%k
    end function plain_host
    integer function module_instance_host() result(r)
        r = m%average([1, 3])
    end function module_instance_host
end program traits_generic_type_06

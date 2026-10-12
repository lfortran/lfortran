! Standard-Fortran oracle for traits_protocol_separate_01, part 1 of 5. The
! open INumeric protocol is an abstract type whose deferred bindings are the
! initializer (a NOPASS function of an integer) and the operators + and /.
! ISum and IAverager are abstract types whose generic messages take
! class(INumeric) arrays and return class(INumeric) values of the arrays'
! dynamic type, so the provider never names a concrete numeric type.
module traits_protocol_separate_01_oracle_contracts_m
    implicit none
    private
    public :: INumeric, ISum, IAverager, initial_of, make_sum, make_averager

    type, abstract :: INumeric
    contains
        procedure(initial_iface), deferred, nopass :: initial
        procedure(binary_iface),  deferred         :: add
        procedure(binary_iface),  deferred         :: divide
        generic :: operator(+) => add
        generic :: operator(/) => divide
    end type INumeric

    abstract interface
        function initial_iface(n) result(res)
            import :: INumeric
            integer,         intent(in)  :: n
            class(INumeric), allocatable :: res
        end function initial_iface
        function binary_iface(lhs, rhs) result(res)
            import :: INumeric
            class(INumeric), intent(in)  :: lhs, rhs
            class(INumeric), allocatable :: res
        end function binary_iface
    end interface

    type, abstract :: ISum
    contains
        procedure(sum_iface), deferred :: sum
    end type ISum

    type, abstract :: IAverager
    contains
        procedure(average_iface), deferred :: average
    end type IAverager

    abstract interface
        function sum_iface(self, x) result(s)
            import :: ISum, INumeric
            class(ISum),     intent(in)  :: self
            class(INumeric), intent(in)  :: x(:)
            class(INumeric), allocatable :: s
        end function sum_iface
        function average_iface(self, x) result(a)
            import :: IAverager, INumeric
            class(IAverager), intent(in)  :: self
            class(INumeric),  intent(in)  :: x(:)
            class(INumeric),  allocatable :: a
        end function average_iface
    end interface

    interface
        subroutine make_sum(choice, object)
            import :: ISum
            integer, intent(in) :: choice
            class(ISum), allocatable, intent(out) :: object
        end subroutine make_sum

        subroutine make_averager(choice, object)
            import :: IAverager
            integer, intent(in) :: choice
            class(IAverager), allocatable, intent(out) :: object
        end subroutine make_averager
    end interface

contains

    ! T(n) for the element type T of x, also when x is empty.
    function initial_of(x, n) result(res)
        class(INumeric), intent(in)  :: x(:)
        integer,         intent(in)  :: n
        class(INumeric), allocatable :: res
        class(INumeric), allocatable :: probe(:)
        allocate(probe(1), mold=x)
        res = probe(1)%initial(n)
    end function initial_of
end module traits_protocol_separate_01_oracle_contracts_m

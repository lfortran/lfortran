! Valid operands and initializer arguments: an operator requirement operand
! with the value attribute, and an initializer whose argument is
! intent(inout) called with a definable actual, by a derived type and
! integer, in generic and concrete code.
module traits_protocol_operands_01_contracts
    implicit none
    private
    public :: ICount, IScale, issue, scale_by

    abstract interface :: ICount
        initial(n)
            integer, intent(inout) :: n
        end initial
    end interface ICount

    abstract interface :: IScale
        function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs
            integer, value           :: rhs
            type(itself)             :: res
        end function operator(*)
    end interface IScale

contains

    function issue{ICount :: T}(counter) result(r)
        integer, intent(inout) :: counter
        type(T)                :: r
        r = T(counter)
    end function issue

    function scale_by{IScale :: T}(x, k) result(r)
        type(T), intent(in) :: x
        integer, intent(in) :: k
        type(T)             :: r
        r = x * k
    end function scale_by
end module traits_protocol_operands_01_contracts

module traits_protocol_operands_01_ticket
    use traits_protocol_operands_01_contracts, only: ICount, IScale
    implicit none
    private

    type, public, sealed :: Ticket
        integer :: id = 0
    end type Ticket

    implements (ICount + IScale) :: Ticket
        initial :: next_ticket
        procedure, static :: operator(*) => scale_ticket
    end implements Ticket

    implements (ICount + IScale) :: integer
    end implements integer

contains

    function next_ticket(n) result(res)
        integer, intent(inout) :: n
        type(Ticket)           :: res
        res%id = n
        n = n + 1
    end function next_ticket

    function scale_ticket(lhs, rhs) result(res)
        type(Ticket), intent(in) :: lhs
        integer, value           :: rhs
        type(Ticket)             :: res
        rhs = 10 * rhs
        res%id = lhs%id * rhs
    end function scale_ticket
end module traits_protocol_operands_01_ticket

program traits_protocol_operands_01
    use traits_protocol_operands_01_contracts, only: issue, scale_by
    use traits_protocol_operands_01_ticket
    implicit none
    type(Ticket) :: t
    integer :: counter, k

    counter = 5
    t = issue{Ticket}(counter)
    if (t%id /= 5 .or. counter /= 6) error stop 1
    t = Ticket(counter)
    if (t%id /= 6 .or. counter /= 7) error stop 2
    if (issue{integer}(counter) /= 7 .or. counter /= 7) error stop 3

    k = 3
    t = scale_by(t, k)
    if (t%id /= 180 .or. k /= 3) error stop 4
    t = t * k
    if (t%id /= 5400 .or. k /= 3) error stop 5
    if (scale_by(4, k) /= 12 .or. k /= 3) error stop 6
    print '(a)', 'traits_protocol_operands_01 ok'
end program traits_protocol_operands_01

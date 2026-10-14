! The open explicit numeric protocol of the paper's ocp.f90 with runtime
! generic messages. Clients need only this contract; the separately compiled
! provider implements the two factories.
module traits_protocol_separate_01_contracts_m
    implicit none

    abstract interface :: INumeric
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(+)
        function operator(/)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(/)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(self, x) result(s)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum

    abstract interface :: IAverager
        function average{INumeric :: T}(self, x) result(a)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: x(:)
            type(T)             :: a
        end function average
    end interface IAverager

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
end module traits_protocol_separate_01_contracts_m

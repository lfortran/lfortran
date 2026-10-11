module traits_runtime_numeric_separate_01_contracts_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none

    abstract interface :: INumeric
        integer | real(real64)
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

    ! Implemented by the separately compiled provider; clients need only this
    ! contract module.
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
end module traits_runtime_numeric_separate_01_contracts_m

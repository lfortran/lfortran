module traits_runtime_numeric_separate_02_contracts_m
    use, intrinsic :: iso_fortran_env, only: real32, real64
    implicit none

    ! Each member returns its result with a different calling convention:
    ! an integer, and the complex types of two kinds.
    abstract interface :: INumeric
        integer | complex(real32) | complex(real64)
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

    ! Ordinary messages with complex results of both kinds.
    abstract interface :: IPhase
        function turn(z, quarters) result(r)
            complex(real64), intent(in) :: z
            integer,         intent(in) :: quarters
            complex(real64)             :: r
        end function turn
        function mirror(z) result(r)
            complex(real32), intent(in) :: z
            complex(real32)             :: r
        end function mirror
    end interface IPhase

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

        subroutine make_phase(object)
            import :: IPhase
            class(IPhase), allocatable, intent(out) :: object
        end subroutine make_phase
    end interface
end module traits_runtime_numeric_separate_02_contracts_m

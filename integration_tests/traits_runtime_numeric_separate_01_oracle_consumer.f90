! Standard-Fortran counterpart of traits_runtime_numeric_separate_01_consumer.f90.
module traits_runtime_numeric_separate_01_oracle_consumer_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_runtime_numeric_separate_01_oracle_contracts_m, only: ISum, IAverager
    implicit none
    private
    public :: integer_total, real64_total, integer_mean, real64_mean, spread

    interface spread
        module procedure spread_integer, spread_real64
    end interface spread

contains

    function integer_total(summer, x) result(r)
        class(ISum), intent(in) :: summer
        integer,     intent(in) :: x(:)
        integer                 :: r
        r = summer%sum(x)
    end function integer_total

    function real64_total(summer, x) result(r)
        class(ISum),  intent(in) :: summer
        real(real64), intent(in) :: x(:)
        real(real64)             :: r
        r = summer%sum_real64(x)
    end function real64_total

    function integer_mean(averager, x) result(r)
        class(IAverager), intent(in) :: averager
        integer,          intent(in) :: x(:)
        integer                      :: r
        r = averager%average(x)
    end function integer_mean

    function real64_mean(averager, x) result(r)
        class(IAverager), intent(in) :: averager
        real(real64),     intent(in) :: x(:)
        real(real64)                 :: r
        r = averager%average(x)
    end function real64_mean

    function spread_integer(averager, x) result(r)
        class(IAverager), intent(in) :: averager
        integer,          intent(in) :: x(:)
        integer                      :: r
        r = averager%average(x(size(x):1:-1)) - averager%average_integer(x(1:size(x):2))
    end function spread_integer

    function spread_real64(averager, x) result(r)
        class(IAverager), intent(in) :: averager
        real(real64),     intent(in) :: x(:)
        real(real64)                 :: r
        r = averager%average(x(size(x):1:-1)) - averager%average_real64(x(1:size(x):2))
    end function spread_real64
end module traits_runtime_numeric_separate_01_oracle_consumer_m

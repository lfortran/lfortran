module traits_runtime_numeric_separate_01_consumer_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_runtime_numeric_separate_01_contracts_m, only: INumeric, ISum, IAverager
    implicit none
    private
    public :: integer_total, real64_total, integer_mean, real64_mean, spread

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
        r = summer%sum{real(real64)}(x)
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

    ! Checked and serialized before any client instantiates it.
    function spread{INumeric :: T}(averager, x) result(r)
        class(IAverager), intent(in) :: averager
        type(T),          intent(in) :: x(:)
        type(T)                      :: r
        r = averager%average(x(size(x):1:-1)) - averager%average{T}(x(1:size(x):2))
    end function spread
end module traits_runtime_numeric_separate_01_consumer_m

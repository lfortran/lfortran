module traits_runtime_numeric_separate_02_consumer_m
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use traits_runtime_numeric_separate_02_contracts_m, only: INumeric, ISum, &
        IAverager, IPhase
    implicit none
    private
    public :: integer_total, complex32_total, complex64_total, complex32_mean, &
        complex64_mean, spread, quarter_turns, mirrored

contains

    function integer_total(summer, x) result(r)
        class(ISum), intent(in) :: summer
        integer,     intent(in) :: x(:)
        integer                 :: r
        r = summer%sum(x)
    end function integer_total

    function complex32_total(summer, x) result(r)
        class(ISum),     intent(in) :: summer
        complex(real32), intent(in) :: x(:)
        complex(real32)             :: r
        r = summer%sum(x=x)
    end function complex32_total

    function complex64_total(summer, x) result(r)
        class(ISum),     intent(in) :: summer
        complex(real64), intent(in) :: x(:)
        complex(real64)             :: r
        r = summer%sum{complex(real64)}(x)
    end function complex64_total

    function complex32_mean(averager, x) result(r)
        class(IAverager), intent(in) :: averager
        complex(real32),  intent(in) :: x(:)
        complex(real32)              :: r
        r = averager%average(x)
    end function complex32_mean

    function complex64_mean(averager, x) result(r)
        class(IAverager), intent(in) :: averager
        complex(real64),  intent(in) :: x(:)
        complex(real64)              :: r
        r = averager%average(x=x)
    end function complex64_mean

    ! Checked and serialized before any client instantiates it.
    function spread{INumeric :: T}(averager, x) result(r)
        class(IAverager), intent(in) :: averager
        type(T),          intent(in) :: x(:)
        type(T)                      :: r
        r = averager%average(x(size(x):1:-1)) - averager%average{T}(x(1:size(x):2))
    end function spread

    function quarter_turns(phase, z, quarters) result(r)
        class(IPhase),   intent(in) :: phase
        complex(real64), intent(in) :: z
        integer,         intent(in) :: quarters
        complex(real64)             :: r
        r = phase%turn(quarters=quarters, z=z)
    end function quarter_turns

    function mirrored(phase, z) result(r)
        class(IPhase),   intent(in) :: phase
        complex(real32), intent(in) :: z
        complex(real32)             :: r
        r = phase%mirror(z)
    end function mirrored
end module traits_runtime_numeric_separate_02_consumer_m

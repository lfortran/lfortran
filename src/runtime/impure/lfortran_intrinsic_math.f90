module lfortran_intrinsic_math
use, intrinsic :: iso_fortran_env, only: i8 => int8, i16 => int16, i32 => int32, i64 => int64, sp => real32, dp => real64
use, intrinsic :: iso_c_binding, only: c_float, c_double
implicit none

interface system_clock
    module procedure i32sys_clock, i64sys_clock, i64r64sys_clock
end interface

interface srand
    module procedure f_srand
end interface

interface cpu_time
    module procedure sp_cpu_time, dp_cpu_time
end interface

contains

! cpu_time ---------------------------------------------------------------------

pure subroutine sp_cpu_time(t)
real(dp), intent(out) :: t
interface
    pure subroutine c_d_cpu_time(t) bind(c, name="_lfortran_d_cpu_time")
    import :: c_double
    real(c_double), intent(out) :: t
    end subroutine
end interface
call c_d_cpu_time(t)
end subroutine

pure subroutine dp_cpu_time(t)
real(sp), intent(out) :: t
interface
    pure subroutine c_s_cpu_time(t) bind(c, name="_lfortran_s_cpu_time")
    import :: c_float
    real(c_float), intent(out) :: t
    end subroutine
end interface
call c_s_cpu_time(t)
end subroutine

! system_clock------------------------------------------------------------------

pure subroutine i32sys_clock(count, count_rate, count_max)
integer(4), intent(out), optional :: count, count_rate, count_max
interface
    pure subroutine c_i32sys_clock(count, count_rate, count_max) &
        bind(c, name="_lfortran_i32sys_clock")
        integer(4), intent(out) :: count, count_rate, count_max
    end subroutine
end interface
call c_i32sys_clock(count, count_rate, count_max)
end subroutine

pure subroutine i64sys_clock(count, count_rate, count_max)
integer(8), intent(out) :: count
integer(8), intent(out), optional :: count_rate, count_max
interface
    pure subroutine c_i64sys_clock(count, count_rate, count_max) &
        bind(c, name="_lfortran_i64sys_clock")
        integer(8), intent(out) :: count, count_rate, count_max
    end subroutine
end interface
call c_i64sys_clock(count, count_rate, count_max)
end subroutine

pure subroutine i64r64sys_clock(count, count_rate, count_max)
integer(8), intent(out) :: count
real(8), intent(out), optional :: count_rate
integer(8), intent(out), optional :: count_max
interface
    pure subroutine c_i64r64sys_clock(count, count_rate, count_max) &
        bind(c, name="_lfortran_i64r64sys_clock")
        integer(8), intent(out) :: count, count_max
        real(8), intent(out) :: count_rate
    end subroutine
end interface
call c_i64r64sys_clock(count, count_rate, count_max)
end subroutine

! srand ----------------------------------------------------------------

pure subroutine f_srand(seed)
integer(4), intent(in) :: seed
interface
    pure subroutine c_srand(seed) &
        bind(c, name="_lfortran_init_random_seed")
        integer(4), intent(in) :: seed
    end subroutine
end interface
call c_srand(seed)
end subroutine

end module

module lfortran_intrinsic_math
use, intrinsic :: iso_fortran_env, only: i8 => int8, i16 => int16, i32 => int32, i64 => int64, sp => real32, dp => real64
use, intrinsic :: iso_c_binding, only: c_float, c_double
implicit none

interface exp
    module procedure sexp, dexp, cexp, zexp
end interface

interface log
    module procedure slog, dlog, clog, zlog
end interface

interface erf
    module procedure serf, derf
end interface

interface erfc
    module procedure serfc, derfc
end interface

interface log10
    module procedure slog10, dlog10
end interface

interface sinh
    module procedure ssinh, dsinh, csinh, zsinh
end interface

interface cosh
    module procedure scosh, dcosh, ccosh, zcosh
end interface

interface tanh
    module procedure stanh, dtanh, ctanh, ztanh
end interface

interface asinh
    module procedure sasinh, dasinh, casinh, zasinh
end interface

interface acosh
    module procedure sacosh, dacosh, cacosh, zacosh
end interface

interface atanh
    module procedure satanh, datanh, catanh, zatanh
end interface

interface range
    module procedure srange, drange
    module procedure crange, zrange
    module procedure int8range, int16range, i32range, i64range
end interface

interface epsilon
    module procedure sepsilon, depsilon
end interface

interface tiny
    module procedure stiny, dtiny
end interface

interface system_clock
    module procedure i32sys_clock, i64sys_clock
end interface

interface srand
    module procedure f_srand
end interface

interface random_number
    module procedure sp_rand_num, dp_rand_num
end interface

interface sign
    module procedure signi8, signi16, signi32, signi64, signr32, signr64
end interface

interface conjg
    module procedure conjgz32, conjgz64
end interface

interface dot_product
    module procedure dotproductr32r32, dotproductr64r64, dotproductz32z32, dotproductz64z64
end interface

contains


! exp --------------------------------------------------------------------------

elemental real(sp) function sexp(x) result(r)
real(sp), intent(in) :: x
interface
    pure real(c_float) function c_sexp(x) bind(c, name="_lfortran_sexp")
    import :: c_float
    real(c_float), intent(in), value :: x
    end function
end interface
r = c_sexp(x)
end function

elemental real(dp) function dexp(x) result(r)
real(dp), intent(in) :: x
interface
    pure real(c_double) function c_dexp(x) bind(c, name="_lfortran_dexp")
    import :: c_double
    real(c_double), intent(in), value :: x
    end function
end interface
r = c_dexp(x)
end function

elemental complex(sp) function cexp(x) result(r)
complex(sp), intent(in) :: x
interface
    pure complex(c_float) function c_cexp(x) bind(c, name="_lfortran_cexp")
    import :: c_float
    complex(c_float), intent(in), value :: x
    end function
end interface
r = c_cexp(x)
end function

elemental complex(dp) function zexp(x) result(r)
complex(dp), intent(in) :: x
interface
    pure complex(c_double) function c_zexp(x) bind(c, name="_lfortran_zexp")
    import :: c_double
    complex(c_double), intent(in), value :: x
    end function
end interface
r = c_zexp(x)
end function

! log --------------------------------------------------------------------------

elemental real(sp) function slog(x) result(r)
real(sp), intent(in) :: x
interface
    pure real(c_float) function c_slog(x) bind(c, name="_lfortran_slog")
    import :: c_float
    real(c_float), intent(in), value :: x
    end function
end interface
r = c_slog(x)
end function

elemental real(dp) function dlog(x) result(r)
real(dp), intent(in) :: x
interface
    pure real(c_double) function c_dlog(x) bind(c, name="_lfortran_dlog")
    import :: c_double
    real(c_double), intent(in), value :: x
    end function
end interface
r = c_dlog(x)
end function

elemental real(dp) function alog(x) result(r)
real(dp), intent(in) :: x
interface
    pure real(c_double) function c_dlog(x) bind(c, name="_lfortran_dlog")
    import :: c_double
    real(c_double), intent(in), value :: x
    end function
end interface
r = c_dlog(x)
end function

elemental complex(sp) function clog(x) result(r)
complex(sp), intent(in) :: x
interface
    pure complex(c_float) function c_clog(x) bind(c, name="_lfortran_clog")
    import :: c_float
    complex(c_float), intent(in), value :: x
    end function
end interface
r = c_clog(x)
end function

elemental complex(dp) function zlog(x) result(r)
complex(dp), intent(in) :: x
interface
    pure complex(c_double) function c_zlog(x) bind(c, name="_lfortran_zlog")
    import :: c_double
    complex(c_double), intent(in), value :: x
    end function
end interface
r = c_zlog(x)
end function

! erf --------------------------------------------------------------------------

elemental real(sp) function serf(x) result(r)
real(sp), intent(in) :: x
interface
    pure real(c_float) function c_serf(x) bind(c, name="_lfortran_serf")
    import :: c_float
    real(c_float), intent(in), value :: x
    end function
end interface
r = c_serf(x)
end function

elemental real(dp) function derf(x) result(r)
real(dp), intent(in) :: x
interface
    pure real(c_double) function c_derf(x) bind(c, name="_lfortran_derf")
    import :: c_double
    real(c_double), intent(in), value :: x
    end function
end interface
r = c_derf(x)
end function

! erfc -------------------------------------------------------------------------

elemental real(sp) function serfc(x) result(r)
real(sp), intent(in) :: x
interface
    pure real(c_float) function c_serfc(x) bind(c, name="_lfortran_serfc")
    import :: c_float
    real(c_float), intent(in), value :: x
    end function
end interface
r = c_serfc(x)
end function

elemental real(dp) function derfc(x) result(r)
real(dp), intent(in) :: x
interface
    pure real(c_double) function c_derfc(x) bind(c, name="_lfortran_derfc")
    import :: c_double
    real(c_double), intent(in), value :: x
    end function
end interface
r = c_derfc(x)
end function

! log10 ------------------------------------------------------------------------

elemental real(sp) function slog10(x) result(r)
real(sp), intent(in) :: x
interface
    pure real(c_float) function c_slog10(x) bind(c, name="_lfortran_slog10")
    import :: c_float
    real(c_float), intent(in), value :: x
    end function
end interface
r = c_slog10(x)
end function

elemental real(dp) function dlog10(x) result(r)
real(dp), intent(in) :: x
interface
    pure real(c_double) function c_dlog10(x) bind(c, name="_lfortran_dlog10")
    import :: c_double
    real(c_double), intent(in), value :: x
    end function
end interface
r = c_dlog10(x)
end function

! sinh --------------------------------------------------------------------------

elemental real(sp) function ssinh(x) result(r)
real(sp), intent(in) :: x
interface
    pure real(c_float) function c_ssinh(x) bind(c, name="_lfortran_ssinh")
    import :: c_float
    real(c_float), intent(in), value :: x
    end function
end interface
r = c_ssinh(x)
end function

elemental real(dp) function dsinh(x) result(r)
real(dp), intent(in) :: x
interface
    pure real(c_double) function c_dsinh(x) bind(c, name="_lfortran_dsinh")
    import :: c_double
    real(c_double), intent(in), value :: x
    end function
end interface
r = c_dsinh(x)
end function

elemental complex(sp) function csinh(x) result(r)
complex(sp), intent(in) :: x
interface
    pure complex(c_float) function c_csinh(x) bind(c, name="_lfortran_csinh")
    import :: c_float
    complex(c_float), intent(in), value :: x
    end function
end interface
r = c_csinh(x)
end function

elemental complex(dp) function zsinh(x) result(r)
complex(dp), intent(in) :: x
interface
    pure complex(c_double) function c_zsinh(x) bind(c, name="_lfortran_zsinh")
    import :: c_double
    complex(c_double), intent(in), value :: x
    end function
end interface
r = c_zsinh(x)
end function

! cosh --------------------------------------------------------------------------

elemental real(sp) function scosh(x) result(r)
real(sp), intent(in) :: x
interface
    pure real(c_float) function c_scosh(x) bind(c, name="_lfortran_scosh")
    import :: c_float
    real(c_float), intent(in), value :: x
    end function
end interface
r = c_scosh(x)
end function

elemental real(dp) function dcosh(x) result(r)
real(dp), intent(in) :: x
interface
    pure real(c_double) function c_dcosh(x) bind(c, name="_lfortran_dcosh")
    import :: c_double
    real(c_double), intent(in), value :: x
    end function
end interface
r = c_dcosh(x)
end function

elemental complex(sp) function ccosh(x) result(r)
complex(sp), intent(in) :: x
interface
    pure complex(c_float) function c_ccosh(x) bind(c, name="_lfortran_ccosh")
    import :: c_float
    complex(c_float), intent(in), value :: x
    end function
end interface
r = c_ccosh(x)
end function

elemental complex(dp) function zcosh(x) result(r)
complex(dp), intent(in) :: x
interface
    pure complex(c_double) function c_zcosh(x) bind(c, name="_lfortran_zcosh")
    import :: c_double
    complex(c_double), intent(in), value :: x
    end function
end interface
r = c_zcosh(x)
end function

! tanh --------------------------------------------------------------------------

elemental real(sp) function stanh(x) result(r)
real(sp), intent(in) :: x
interface
    pure real(c_float) function c_stanh(x) bind(c, name="_lfortran_stanh")
    import :: c_float
    real(c_float), intent(in), value :: x
    end function
end interface
r = c_stanh(x)
end function

elemental real(dp) function dtanh(x) result(r)
real(dp), intent(in) :: x
interface
    pure real(c_double) function c_dtanh(x) bind(c, name="_lfortran_dtanh")
    import :: c_double
    real(c_double), intent(in), value :: x
    end function
end interface
r = c_dtanh(x)
end function

elemental complex(sp) function ctanh(x) result(r)
complex(sp), intent(in) :: x
interface
    pure complex(c_float) function c_ctanh(x) bind(c, name="_lfortran_ctanh")
    import :: c_float
    complex(c_float), intent(in), value :: x
    end function
end interface
r = c_ctanh(x)
end function

elemental complex(dp) function ztanh(x) result(r)
complex(dp), intent(in) :: x
interface
    pure complex(c_double) function c_ztanh(x) bind(c, name="_lfortran_ztanh")
    import :: c_double
    complex(c_double), intent(in), value :: x
    end function
end interface
r = c_ztanh(x)
end function

! asinh --------------------------------------------------------------------------

elemental real(sp) function sasinh(x) result(r)
real(sp), intent(in) :: x
interface
    pure real(c_float) function c_sasinh(x) bind(c, name="_lfortran_sasinh")
    import :: c_float
    real(c_float), intent(in), value :: x
    end function
end interface
r = c_sasinh(x)
end function

elemental real(dp) function dasinh(x) result(r)
real(dp), intent(in) :: x
interface
    pure real(c_double) function c_dasinh(x) bind(c, name="_lfortran_dasinh")
    import :: c_double
    real(c_double), intent(in), value :: x
    end function
end interface
r = c_dasinh(x)
end function

elemental complex(sp) function casinh(x) result(r)
complex(sp), intent(in) :: x
interface
    pure complex(c_float) function c_casinh(x) bind(c, name="_lfortran_casinh")
    import :: c_float
    complex(c_float), intent(in), value :: x
    end function
end interface
r = c_casinh(x)
end function

elemental complex(dp) function zasinh(x) result(r)
complex(dp), intent(in) :: x
interface
    pure complex(c_double) function c_zasinh(x) bind(c, name="_lfortran_zasinh")
    import :: c_double
    complex(c_double), intent(in), value :: x
    end function
end interface
r = c_zasinh(x)
end function

! acosh --------------------------------------------------------------------------

elemental real(sp) function sacosh(x) result(r)
real(sp), intent(in) :: x
interface
    pure real(c_float) function c_sacosh(x) bind(c, name="_lfortran_sacosh")
    import :: c_float
    real(c_float), intent(in), value :: x
    end function
end interface
r = c_sacosh(x)
end function

elemental real(dp) function dacosh(x) result(r)
real(dp), intent(in) :: x
interface
    pure real(c_double) function c_dacosh(x) bind(c, name="_lfortran_dacosh")
    import :: c_double
    real(c_double), intent(in), value :: x
    end function
end interface
r = c_dacosh(x)
end function

elemental complex(sp) function cacosh(x) result(r)
complex(sp), intent(in) :: x
interface
    pure complex(c_float) function c_cacosh(x) bind(c, name="_lfortran_cacosh")
    import :: c_float
    complex(c_float), intent(in), value :: x
    end function
end interface
r = c_cacosh(x)
end function

elemental complex(dp) function zacosh(x) result(r)
complex(dp), intent(in) :: x
interface
    pure complex(c_double) function c_zacosh(x) bind(c, name="_lfortran_zacosh")
    import :: c_double
    complex(c_double), intent(in), value :: x
    end function
end interface
r = c_zacosh(x)
end function

! atanh --------------------------------------------------------------------------

elemental real(sp) function satanh(x) result(r)
real(sp), intent(in) :: x
interface
    pure real(c_float) function c_satanh(x) bind(c, name="_lfortran_satanh")
    import :: c_float
    real(c_float), intent(in), value :: x
    end function
end interface
r = c_satanh(x)
end function

elemental real(dp) function datanh(x) result(r)
real(dp), intent(in) :: x
interface
    pure real(c_double) function c_datanh(x) bind(c, name="_lfortran_datanh")
    import :: c_double
    real(c_double), intent(in), value :: x
    end function
end interface
r = c_datanh(x)
end function

elemental complex(sp) function catanh(x) result(r)
complex(sp), intent(in) :: x
interface
    pure complex(c_float) function c_catanh(x) bind(c, name="_lfortran_catanh")
    import :: c_float
    complex(c_float), intent(in), value :: x
    end function
end interface
r = c_catanh(x)
end function

elemental complex(dp) function zatanh(x) result(r)
complex(dp), intent(in) :: x
interface
    pure complex(c_double) function c_zatanh(x) bind(c, name="_lfortran_zatanh")
    import :: c_double
    complex(c_double), intent(in), value :: x
    end function
end interface
r = c_zatanh(x)
end function

! epsilon ---------------------------------------------------------------------

elemental real(sp) function sepsilon(x) result(r)
real(sp), intent(in) :: x
r = 1.19209290E-07
end function

elemental real(dp) function depsilon(x) result(r)
real(dp), intent(in) :: x
r = 2.2204460492503131E-016
end function

! tiny ---------------------------------------------------------------------

elemental real(sp) function stiny(x) result(r)
real(sp), intent(in) :: x
r = 1.17549435E-38
end function

elemental real(dp) function dtiny(x) result(r)
real(dp), intent(in) :: x
r = 2.2250738585072014E-308
end function

! range ---------------------------------------------------------------------

elemental integer function int8range(x) result(r)
integer(i8), intent(in) :: x
r = 2
end function

elemental integer function int16range(x) result(r)
integer(i16), intent(in) :: x
r = 4
end function

elemental integer function i32range(x) result(r)
integer(4), intent(in) :: x
r = 9
end function

elemental integer function i64range(x) result(r)
integer(8), intent(in) :: x
r = 18
end function

elemental integer function srange(x) result(r)
real(4), intent(in) :: x
r = 37
end function

elemental integer function drange(x) result(r)
real(8), intent(in) :: x
r = 307
end function

elemental integer function crange(x) result(r)
complex(4), intent(in) :: x
r = 37
end function

elemental integer function zrange(x) result(r)
complex(8), intent(in) :: x
r = 307
end function

! cpu_time ---------------------------------------------------------------------

pure subroutine cpu_time(t)
real(dp), intent(out) :: t
interface
    pure subroutine c_cpu_time(t) bind(c, name="_lfortran_cpu_time")
    import :: c_double
    real(c_double), intent(out) :: t
    end subroutine
end interface
call c_cpu_time(t)
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

! random_number ----------------------------------------------------------------

elemental pure subroutine sp_rand_num(harvest)
real(sp), intent(out) :: harvest
interface
    pure subroutine c_sp_rand_num(harvest) &
        bind(c, name="_lfortran_sp_rand_num")
        import :: c_float
        real(c_float), intent(out) :: harvest
    end subroutine
end interface
call c_sp_rand_num(harvest)
end subroutine

elemental pure subroutine dp_rand_num(harvest)
real(dp), intent(out) :: harvest
interface
    pure subroutine c_dp_rand_num(harvest) &
        bind(c, name="_lfortran_dp_rand_num")
        import :: c_double
        real(c_double), intent(out) :: harvest
    end subroutine
end interface
call c_dp_rand_num(harvest)
end subroutine

! sign -------------------------------------------------------------------------

elemental integer(i8) function signi8(x, y) result(r)
integer(i8), intent(in) :: x, y
if ((x >= 0 .and. y >= 0) .or. (x <= 0 .and. y <= 0)) then
    r = x
else
    r = -x
end if
end function

elemental integer(i16) function signi16(x, y) result(r)
integer(i16), intent(in) :: x, y
if ((x >= 0 .and. y >= 0) .or. (x <= 0 .and. y <= 0)) then
    r = x
else
    r = -x
end if
end function

elemental integer(i32) function signi32(x, y) result(r)
integer(i32), intent(in) :: x, y
if ((x >= 0 .and. y >= 0) .or. (x <= 0 .and. y <= 0)) then
    r = x
else
    r = -x
end if
end function

elemental integer(i64) function signi64(x, y) result(r)
integer(i64), intent(in) :: x, y
if ((x >= 0 .and. y >= 0) .or. (x <= 0 .and. y <= 0)) then
    r = x
else
    r = -x
end if
end function

elemental real(sp) function signr32(x, y) result(r)
real(sp), intent(in) :: x, y
if ((x >= 0 .and. y >= 0) .or. (x <= 0 .and. y <= 0)) then
    r = x
else
    r = -x
end if
end function

elemental real(dp) function signr64(x, y) result(r)
real(dp), intent(in) :: x, y
if ((x >= 0 .and. y >= 0) .or. (x <= 0 .and. y <= 0)) then
    r = x
else
    r = -x
end if
end function

function conjgz32(x) result(r)
complex(sp) :: x
complex(sp) :: r
r = real(x) - aimag(x)*(0,1)
end function

function conjgz64(x) result(r)
complex(dp) :: x
complex(dp) :: r
r = real(x, dp) - aimag(x)*(0,1)
end function

function dotproductr32r32(x, y) result(r)
real(sp) :: x(:), y(:)
real(sp) :: r
end function

function dotproductr64r64(x, y) result(r)
real(dp) :: x(:), y(:)
real(dp) :: r
end function

function dotproductz32z32(x, y) result(r)
complex(sp) :: x(:), y(:)
complex(sp) :: r
end function

function dotproductz64z64(x, y) result(r)
complex(dp) :: x(:), y(:)
complex(dp) :: r
end function


end module

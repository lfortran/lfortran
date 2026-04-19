program abs_06
use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
implicit none
integer, parameter :: sp = kind(1.0)
integer, parameter :: dp = kind(1.0d0)
complex(sp) :: cs
complex(dp) :: cd
real(sp) :: rs
real(dp) :: rd

! abs of complex with NaN real part (single precision)
cs = cmplx(ieee_value(1._sp, ieee_quiet_nan), 0._sp)
rs = abs(cs)
print *, rs
if (.not. ieee_is_nan(rs)) error stop "abs_06: sp NaN real part failed"

! abs of complex with NaN imaginary part (single precision)
cs = cmplx(0._sp, ieee_value(1._sp, ieee_quiet_nan))
rs = abs(cs)
print *, rs
if (.not. ieee_is_nan(rs)) error stop "abs_06: sp NaN imag part failed"

! abs of complex with both NaN (single precision)
cs = cmplx(ieee_value(1._sp, ieee_quiet_nan), ieee_value(1._sp, ieee_quiet_nan))
rs = abs(cs)
print *, rs
if (.not. ieee_is_nan(rs)) error stop "abs_06: sp both NaN failed"

! abs of complex with NaN real part (double precision)
cd = cmplx(ieee_value(1._dp, ieee_quiet_nan), 0._dp, dp)
rd = abs(cd)
print *, rd
if (.not. ieee_is_nan(rd)) error stop "abs_06: dp NaN real part failed"

! abs of complex with NaN imaginary part (double precision)
cd = cmplx(0._dp, ieee_value(1._dp, ieee_quiet_nan), dp)
rd = abs(cd)
print *, rd
if (.not. ieee_is_nan(rd)) error stop "abs_06: dp NaN imag part failed"

! abs of normal complex still works
cs = cmplx(3._sp, 4._sp)
rs = abs(cs)
if (abs(rs - 5._sp) > 1.e-6_sp) error stop "abs_06: normal complex failed"

print *, "all tests passed"
end program

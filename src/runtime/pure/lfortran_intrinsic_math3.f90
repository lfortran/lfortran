module lfortran_intrinsic_math3
use, intrinsic :: iso_fortran_env, only: i8 => int8, i16 => int16, i32 => int32, i64 => int64, sp => real32, dp => real64
implicit none

interface floor
    module procedure sfloor_i32, sfloor_i64, dfloor_i32, dfloor_i64
end interface

contains

! floor ------------------------------------------------------------------------

elemental integer(i32) function sfloor_i32(x, kind) result(r)
real(sp), intent(in) :: x
integer(i32), intent(in) :: kind
if (x >= 0) then
    r = x
else
    r = x-1
end if
end function

elemental integer(i64) function sfloor_i64(x, kind) result(r)
real(sp), intent(in) :: x
integer(i64), intent(in) :: kind
if (x >= 0) then
    r = x
else
    r = x-1
end if
end function

elemental integer(i32) function dfloor_i32(x, kind) result(r)
real(dp), intent(in) :: x
integer(i32), intent(in) :: kind
if (x >= 0) then
    r = x
else
    r = x-1
end if
end function

elemental integer(i64) function dfloor_i64(x, kind) result(r)
real(dp), intent(in) :: x
integer(i64), intent(in) :: kind
if (x >= 0) then
    r = x
else
    r = x-1
end if
end function

end module

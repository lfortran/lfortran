module lcompilers_test_module
use iso_fortran_env, only: sp => real32, dp => real64

interface lcompilers_test
procedure :: lcompilers_test_r32_r32
procedure :: lcompilers_test_r64_r64
end interface

contains
elemental subroutine lcompilers_test_r32_r32(actual, expected, tol)
real(sp), intent(in) :: actual
real(sp), intent(in) :: expected
real(sp), optional, intent(in) :: tol

if (present(tol)) then
  if (abs(actual - expected) > tol * 1e-8_sp) error stop
else
  if (abs(actual - expected) > 1e-8_sp) error stop
end if
end subroutine

elemental impure subroutine lcompilers_test_r64_r64(actual, expected, tol)
real(dp), intent(in) :: actual
real(dp), intent(in) :: expected
real(dp), optional, intent(in) :: tol

if (present(tol)) then
  if (abs(actual - expected) > tol * 1e-12_dp) error stop
else
  print *, actual, expected
  print *, abs(actual - expected)
  if (abs(actual - expected) > 1e-12_dp) error stop
end if
end subroutine


end module

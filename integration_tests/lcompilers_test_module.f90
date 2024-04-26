module lcompilers_test_module
use iso_fortran_env, only: sp => real32, dp => real64, int32, int64

interface lcompilers_test
  module procedure :: lcompilers_test_bool
  module procedure :: lcompilers_test_r32_r32
  module procedure :: lcompilers_test_r64_r64
  module procedure :: lcompilers_test_i32_i32
  module procedure :: lcompilers_test_i64_i64
end interface

contains

elemental impure subroutine lcompilers_test_bool(value)
logical, intent(in) :: value

print *, "value: ", value
if (.not. value) error stop
end subroutine

elemental impure subroutine lcompilers_test_i32_i32(actual, expected)
integer(int32), intent(in) :: actual
integer(int32), intent(in) :: expected

print *, "actual: ", actual
print *, "expected: ", expected
if (actual /= expected) error stop
end subroutine

elemental impure subroutine lcompilers_test_i64_i64(actual, expected)
integer(int64), intent(in) :: actual
integer(int64), intent(in) :: expected

print *, "actual: ", actual
print *, "expected: ", expected
if (actual /= expected) error stop
end subroutine

elemental impure subroutine lcompilers_test_r32_r32(actual, expected, tol)
real(sp), intent(in) :: actual
real(sp), intent(in) :: expected
real(sp), optional, intent(in) :: tol

print *, "actual: ", actual
print *, "expected: ", expected
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

print *, "actual: ", actual
print *, "expected: ", expected
if (present(tol)) then
  if (abs(actual - expected) > tol * 1e-12_dp) error stop
else
  if (abs(actual - expected) > 1e-12_dp) error stop
end if
end subroutine

end module

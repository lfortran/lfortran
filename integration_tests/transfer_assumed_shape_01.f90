program transfer_assumed_shape_01
! Test transfer() with assumed-shape (runtime-sized) source arrays
! and different element kinds. Verifies the byte-level reinterpretation
! works correctly when the source array size is only known at runtime.
implicit none
integer(4) :: v(2)
v = [10, 20]
call test_fixed(v)
call test_same_kind(v)
print *, "All tests passed"
contains

subroutine test_fixed(x)
  ! Different kind: int32 array -> int8 array (fixed-size target)
  integer(4), intent(in) :: x(:)
  integer(1) :: r(8)
  r = transfer(x, r)
  ! Little-endian: 10 = 0x0A000000, 20 = 0x14000000
  if (r(1) /= 10) error stop
  if (r(2) /= 0) error stop
  if (r(3) /= 0) error stop
  if (r(4) /= 0) error stop
  if (r(5) /= 20) error stop
  if (r(6) /= 0) error stop
  if (r(7) /= 0) error stop
  if (r(8) /= 0) error stop
end subroutine

subroutine test_same_kind(x)
  ! Same kind: int32 array -> int32 array (fixed-size target)
  integer(4), intent(in) :: x(:)
  integer(4) :: r(2)
  r = transfer(x, r)
  if (r(1) /= 10) error stop
  if (r(2) /= 20) error stop
end subroutine

end program

program storage_size_logical_test
  use, intrinsic :: iso_fortran_env
  implicit none

  logical(int8)  :: b8
  logical(int16) :: b16
  logical(int32) :: b32
  logical(int64) :: b64

  if (storage_size(b8) /= 8)  error stop "incorrect storage size of int8"
  if (storage_size(b16) /= 16) error stop "incorrect storage size of int16"
  if (storage_size(b32) /= 32) error stop "incorrect storage size of int32"
  if (storage_size(b64) /= 64) error stop "incorrect storage size of int64"
  print *, "test passed"
end program storage_size_logical_test

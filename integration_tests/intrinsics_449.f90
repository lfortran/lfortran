program intrinsics_449
  use iso_c_binding, only: c_ptr, c_null_ptr
  implicit none

  type :: t
    type(c_ptr) :: p
  end type

  type(c_ptr) :: p
  type(t) :: x

  p = c_null_ptr
  x%p = c_null_ptr

  print *, storage_size(p)
  if (storage_size(p) /= 64) error stop 1

  print *, storage_size(x)
  if (storage_size(x) /= 64) error stop 2
end program

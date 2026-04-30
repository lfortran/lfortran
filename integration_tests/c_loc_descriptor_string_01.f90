program c_loc_descriptor_string_warning
  ! LFortran represents fixed-length character components of derived
  ! types as a descriptor (pointer + length), so the in-memory layout
  ! differs from gfortran/flang. c_loc() on such a type should compile
  ! (and emit a non-portable warning), not error out.
  use iso_c_binding, only: c_ptr, c_loc, c_associated
  implicit none

  type :: t_with_string
    integer :: i
    character(len=5) :: s
  end type

  type(t_with_string), target :: x
  type(c_ptr) :: p

  x%i = 42
  x%s = "hello"
  p = c_loc(x)
  if (.not. c_associated(p)) error stop "c_loc returned null"

  print *, "PASS"
end program

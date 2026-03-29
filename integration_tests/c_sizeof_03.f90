program c_sizeof_03
  use iso_c_binding
  implicit none

  ! bind(C) struct with mixed-alignment fields requiring padding
  type, bind(C) :: t
    integer(c_int) :: a
    integer(c_size_t) :: b
    integer(c_int) :: c
    type(c_ptr) :: d
  end type

  type(t) :: x
  integer(c_size_t) :: sz

  sz = c_sizeof(x)
  print *, "c_sizeof(t) =", sz
  if (sz /= 32) error stop

  print *, "OK"
end program

program iso_c_binding_05
  use iso_c_binding, only: c_loc, c_intptr_t
  implicit none
  type :: wrap_t
    integer, pointer :: p
  end type
  integer, target :: x = 10
  type(wrap_t) :: w
  integer(c_intptr_t) :: expected, actual
  w%p => x
  expected = transfer(c_loc(x), expected)
  actual = transfer(c_loc(w%p), actual)
  if (expected /= actual) error stop "c_loc on pointer component returns wrong address"
  print *, "PASS"
end program

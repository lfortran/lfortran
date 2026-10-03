module bindc_49_m
  use iso_c_binding
  implicit none
  type, bind(C) :: handle_t
    type(c_ptr) :: info = c_null_ptr
  end type
contains
  function deref(h) result(v)
    type(handle_t), intent(in) :: h
    integer(c_int) :: v
    integer(c_int), pointer :: p
    call c_f_pointer(h%info, p)
    v = p
  end function

  subroutine use_handle(h, v) bind(C)
    type(handle_t), value, intent(in) :: h
    integer(c_int), intent(out) :: v
    v = deref(h)
  end subroutine
end module

program bindc_49
  use bindc_49_m
  implicit none
  type(handle_t) :: h
  integer(c_int) :: v
  integer(c_int), target, save :: d = 42
  h%info = c_loc(d)
  call use_handle(h, v)
  if (v /= 42) error stop 1
  print *, v
end program

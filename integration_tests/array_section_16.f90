module array_section_16_mod
contains
  subroutine test_pointer(w_ptr, w)
    real, pointer, intent(out) :: w_ptr(:)
    real, intent(in), target :: w(:)
    w_ptr(1:size(w)) => w
  end subroutine test_pointer
end module array_section_16_mod


program array_section_16
  use array_section_16_mod
  implicit none
  real, pointer :: wptr(:)
  real, allocatable :: w(:)
  real, target :: arg(6)
  arg = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
  call test_pointer(wptr, arg)
  allocate(w(size(wptr)))
  w(1:size(wptr)) = wptr
  if (size(w) /= 6 .or. size(wptr) /= 6) error stop
  if (any(w /= arg)) error stop
end program array_section_16
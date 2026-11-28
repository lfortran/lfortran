program bindc_cchar_deferred_ptr_01
  use iso_c_binding
  implicit none
  character(kind=C_CHAR, len=:), pointer :: ptr
  call s(ptr)
  if (.not. associated(ptr)) error stop 1
  if (ptr /= 'bar') error stop 2
contains
  subroutine s(ptr) bind(c)
    use iso_c_binding
    character(kind=C_CHAR, len=:), pointer :: ptr
    allocate(character(len=3) :: ptr)
    ptr = 'bar'
  end subroutine
end program

program separate_compilation_51
  use separate_compilation_51a, only: stop_cb_iface, register_cb, fire, ncalls
  implicit none
  ! A procedure pointer declared with an interface holds a copy of that
  ! interface's signature. The pass that turns an optional dummy into a
  ! presence flag rewrites the interface and the procedure it points at, so
  ! the copy has to be rewritten with them.
  procedure(stop_cb_iface), pointer :: callback_ptr
  callback_ptr => callback
  call register_cb(callback_ptr)
  call fire()
  if (ncalls /= 2) error stop "callback was not invoked twice"
contains
  subroutine callback(is_error, quiet, code_int, code_char)
    logical, intent(in) :: is_error, quiet
    integer, intent(in), optional :: code_int
    character(len=*), intent(in), optional :: code_char
    ncalls = ncalls + 1
    if (present(code_int)) then
      if (code_int /= 7) error stop "wrong optional integer"
      if (.not. present(code_char)) error stop "expected both optionals"
      if (code_char /= "code") error stop "wrong optional string"
    else
      if (present(code_char)) error stop "expected neither optional"
    end if
  end subroutine
end program

module separate_compilation_51a
  implicit none
  abstract interface
    subroutine stop_cb_iface(is_error, quiet, code_int, code_char)
      logical, intent(in) :: is_error, quiet
      integer, intent(in), optional :: code_int
      character(len=*), intent(in), optional :: code_char
    end subroutine
  end interface
  procedure(stop_cb_iface), pointer :: saved => null()
  integer :: ncalls = 0
contains
  subroutine register_cb(callback)
    procedure(stop_cb_iface), pointer :: callback
    saved => callback
  end subroutine

  subroutine fire()
    if (.not. associated(saved)) error stop "callback was not registered"
    call saved(.false., .true.)
    call saved(.true., .false., 7, "code")
  end subroutine
end module

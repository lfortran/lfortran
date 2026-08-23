program pointer_intent_in_01
  implicit none
  ! A POINTER, INTENT(IN) dummy also accepts a non-pointer actual that has
  ! the TARGET attribute; the dummy becomes associated with it.
  integer, target :: n
  character(len=:), allocatable, target :: text
  n = 42
  call check_int(n)

  text = "hello"
  call check_text(text)

contains

  subroutine check_int(p)
    integer, pointer, intent(in) :: p
    if (.not. associated(p)) error stop "dummy is not associated"
    if (p /= 42) error stop "wrong value through the dummy"
  end subroutine

  subroutine check_text(s)
    character(len=:), pointer, intent(in) :: s
    if (.not. associated(s)) error stop "string dummy is not associated"
    if (len(s) /= 5) error stop "wrong length through the dummy"
    if (s /= "hello") error stop "wrong contents through the dummy"
  end subroutine

end program

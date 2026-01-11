module jtypes
  implicit none

contains
  subroutine dummy(message, line)
    character(len=*), intent(in), optional :: message
    integer, intent(in), optional :: line
    if (present(line)) error stop 1
  end subroutine
end module jtypes

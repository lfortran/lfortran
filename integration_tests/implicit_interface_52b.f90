! Companion to implicit_interface_52: check the trimmed value and its length.
integer function check_text(n, text) result(status)
  implicit none
  integer, intent(in) :: n
  character(len=*), intent(in) :: text
  status = 0
  if (len(text) /= n) status = status + 1
  if (text /= "hours") status = status + 2
end function check_text

! A TRIM result passed through an implicit interface must preserve its data
! and length in the CHARACTER descriptor. The callee is compiled separately.
module implicit_interface_52_mod
  implicit none
contains
  integer function put_text(values) result(status)
    character(len=*), intent(in) :: values
    integer, external :: check_text
    status = check_text(len_trim(values), trim(values))
  end function put_text
end module implicit_interface_52_mod

program implicit_interface_52
  use implicit_interface_52_mod
  implicit none
  integer :: status
  status = put_text("hours   ")
  print *, "status =", status
  if (status /= 0) error stop
  print *, "OK"
end program implicit_interface_52

! A module procedure passed as a dummy procedure must receive the CHARACTER
! descriptor intact when called through its interface.
module implicit_interface_57_mod
  implicit none
contains
  subroutine record(msg, n)
    character(len=*), intent(in) :: msg
    integer, intent(out) :: n
    n = len(msg)
    if (msg /= 'hello') n = -1
  end subroutine record

  subroutine driver_54(cb, n)
    interface
      subroutine cb(msg, n)
        character(len=*), intent(in) :: msg
        integer, intent(out) :: n
      end subroutine cb
    end interface
    integer, intent(out) :: n
    call cb('hello', n)
  end subroutine driver_54
end module implicit_interface_57_mod

program implicit_interface_57
  use implicit_interface_57_mod, only: record, driver_54
  implicit none
  integer :: n
  n = 0
  call driver_54(record, n)
  print *, "len seen by callback =", n, " (expected 5)"
  if (n /= 5) error stop
  print *, "OK"
end program implicit_interface_57

! A dummy procedure whose interface body is declared inside the receiving
! procedure (a non-module scope). A call through the dummy procedure must use
! the ordinary string-descriptor ABI, because the actual procedure bound to it
! here is a module procedure (descriptor ABI). Regression test for the
! hidden-length character ABI classification
! (ASRUtils::is_external_implicit_interface_proc): a subprogram-scope interface
! body that types a dummy procedure must NOT be classified as an external
! procedure using the classic (data pointer + hidden trailing length) ABI, or
! the callback receives a garbage length.
module implicit_interface_54_mod
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
end module implicit_interface_54_mod

program implicit_interface_54
  use implicit_interface_54_mod, only: record, driver_54
  implicit none
  integer :: n
  n = 0
  call driver_54(record, n)
  print *, "len seen by callback =", n, " (expected 5)"
  if (n /= 5) error stop
  print *, "OK"
end program implicit_interface_54

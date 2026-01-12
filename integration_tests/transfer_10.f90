program mre_transfer_no_c_char
  implicit none

  character(len=1) :: buf(20)
  character(len=*), parameter :: fstr = "hello"
  integer :: i
  integer, parameter :: expected(5) = [104, 101, 108, 108, 111]

  call f_character_transfer(fstr, buf, size(buf))

  do i = 1, 5
     if (iachar(buf(i)) /= expected(i)) error stop "wrong value"
  end do

  print *, "test passed"

contains

  subroutine f_character_transfer(rhs, lhs, length)
    implicit none
    character(len=*), intent(in) :: rhs
    character(len=1), intent(out) :: lhs(*)
    integer, intent(in) :: length
    integer :: n

    n = min(length, len(rhs))

    lhs(1:n) = transfer(rhs, lhs(1:n))
  end subroutine f_character_transfer

end program mre_transfer_no_c_char

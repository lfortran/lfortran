program array_130
  implicit none
  integer :: runtime_len = 5

  call f("a")
  call test_runtime_len(runtime_len)
  call test_empty_string("")

contains

  subroutine f(mold)
    character(*), intent(in) :: mold
    if (size(transfer(0.0, [mold])) /= 4) error stop
  end subroutine f

  subroutine test_runtime_len(n)
    integer, intent(in) :: n
    character(len=n) :: mold_str
    if (size(transfer(0.0, [mold_str])) /= 1) error stop
  end subroutine test_runtime_len

  subroutine test_empty_string(mold)
    character(*), intent(in) :: mold
    if (size(transfer(0.0, [mold])) /= 0) error stop
  end subroutine test_empty_string

end program array_130
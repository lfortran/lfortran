module allocate_46_mod
  implicit none
contains
  subroutine test(n)
    integer, intent(in) :: n
    integer, allocatable :: arr(:)
    allocate(arr(2), source=n)
    if (arr(1) /= n) error stop
    if (arr(2) /= n) error stop
    if (size(arr) /= 2) error stop
    print *, arr
  end subroutine test
end module allocate_46_mod

program allocate_46
  use allocate_46_mod
  implicit none
  call test(n=100)
  call test(n=200)
end program allocate_46

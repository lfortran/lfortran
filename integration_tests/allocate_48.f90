program allocate_48
  implicit none
  call test(n=100)
  call test(n=200)
contains
  subroutine test(n)
    integer, intent(in) :: n
    integer, allocatable :: arr(:)
    allocate(arr(2), source=n)
    if (arr(1) /= n) error stop
    if (arr(2) /= n) error stop
    if (size(arr) /= 2) error stop
    print "(A,2I12)", 'subroutine result', arr
  end subroutine test
end program allocate_48

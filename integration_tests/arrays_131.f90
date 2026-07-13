program main
  implicit none

  integer, target :: x(10, 10)
  integer, pointer :: p(:)

  p(4:) => x(4:7:3, 5)

  call check(p)
  print *, "passed"

contains

  subroutine check(a)
    integer, pointer, intent(in) :: a(:)

    if (lbound(a, 1) /= 4) error stop
    if (ubound(a, 1) /= 5) error stop
    if (size(a) /= 2) error stop
  end subroutine check

end program main
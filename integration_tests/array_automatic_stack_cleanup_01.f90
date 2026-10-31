program array_automatic_stack_cleanup
  implicit none
  call check(7)
  call check(0)
  call check(3)
contains
  subroutine check(n)
    integer, intent(in) :: n
    real :: r(n)
    integer :: i(n)
    complex :: c(n)
    logical :: l(n)
    r = 2
    i = 3
    c = cmplx(r, real(i))
    l = abs(c) > 0
    if (sum(r) /= 2*n) error stop 1
    if (sum(i) /= 3*n) error stop 2
    if (sum(c) /= cmplx(2*n, 3*n)) error stop 3
    if (.not. all(l)) error stop 4
  end subroutine
end program

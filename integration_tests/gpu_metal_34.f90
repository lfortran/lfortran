subroutine learn(n)
  integer, intent(in) :: n
  integer :: pair
  real :: a(n)
  do concurrent (pair = 1:n)
    a(pair) = real(pair)
  end do
  if (abs(a(1) - 1.0) > 1e-6) error stop
  if (abs(a(n) - real(n)) > 1e-6) error stop
end subroutine

program main
  implicit none
  call learn(5)
  print *, "PASS"
end program

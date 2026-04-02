program array_section_allocatable_01
  implicit none
  real, allocatable :: a(:), b(:), c(:)
  integer :: n, i
  real :: total

  n = 5
  allocate(a(n), b(n), c(n))
  a = [(real(i), i=1,n)]
  b = [(real(n - i + 1), i=1,n)]

  c(:) = (a(:) - b(:))**2

  total = 0.0
  do i = 1, n
    total = total + c(i)
  end do

  if (abs(c(1) - 16.0) > 1e-5) error stop
  if (abs(c(2) - 4.0) > 1e-5) error stop
  if (abs(c(3) - 0.0) > 1e-5) error stop
  if (abs(c(4) - 4.0) > 1e-5) error stop
  if (abs(c(5) - 16.0) > 1e-5) error stop
  if (abs(total - 40.0) > 1e-5) error stop

  c(2:4) = a(2:4) + b(2:4)
  if (abs(c(2) - 6.0) > 1e-5) error stop
  if (abs(c(3) - 6.0) > 1e-5) error stop
  if (abs(c(4) - 6.0) > 1e-5) error stop

  print *, "PASS"
end program

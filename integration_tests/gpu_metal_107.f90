! Test: sum() with array expression argument inside do concurrent.
! Previously generated invalid Metal shader: (a + b)[idx] treated as pointer
! arithmetic instead of element-wise array addition.
program gpu_metal_107
  implicit none
  integer :: i
  real :: a(3), b(3), c(3)

  a = [1.0, 2.0, 3.0]
  b = [4.0, 5.0, 6.0]

  do concurrent (i = 1:3)
    c(i) = sum(a + b)
  end do

  ! a + b = [5.0, 7.0, 9.0], sum = 21.0
  if (abs(c(1) - 21.0) > 1e-5) error stop
  if (abs(c(2) - 21.0) > 1e-5) error stop
  if (abs(c(3) - 21.0) > 1e-5) error stop

  print *, "PASSED"
end program

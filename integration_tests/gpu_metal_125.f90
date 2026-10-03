! Test: struct with allocatable member in do concurrent reads struct data
! and computes a result. Exercises correct Metal buffer index counting:
! struct array args with allocatable members need 3 extra buffers per
! allocatable member (data, offsets, sizes).
program gpu_metal_125
  implicit none
  type :: t
    real, allocatable :: v(:)
  end type
  integer, parameter :: n = 3
  type(t) :: a(n)
  real :: s(n)
  integer :: i

  do i = 1, n
    allocate(a(i)%v(2))
    a(i)%v = [real(i), real(i) * 10.0]
  end do

  do concurrent(i=1:n)
    s(i) = a(i)%v(1) + a(i)%v(2)
  end do

  if (abs(s(1) - 11.0) > 1e-5) error stop
  if (abs(s(2) - 22.0) > 1e-5) error stop
  if (abs(s(3) - 33.0) > 1e-5) error stop
  print *, "PASS"
end program

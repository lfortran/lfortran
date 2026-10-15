program gpu_metal_395
  ! A type-bound procedure of a separately compiled submodule with an
  ! allocatable local sized from a component of the passed object, which
  ! binds an associate name to the result of another procedure with an
  ! allocatable local, called from an offloaded loop.
  use gpu_metal_395_mod
  implicit none
  type(net_t) :: net
  real :: x(6), y(6), e
  integer :: i
  allocate(net%nodes_(3))
  net%nodes_ = [2, 5, 3]
  x = [(real(i), i = 1, 6)]
  do concurrent (i = 1:6)
    y(i) = net%infer(x(i)) + 0
  end do
  do i = 1, 6
    e = 5 + 5*x(i) + 15
    if (abs(y(i) - e) > 1e-4*e) error stop 1
  end do
  print *, "ok"
end program

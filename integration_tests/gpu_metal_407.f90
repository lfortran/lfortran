module gpu_metal_407_m
  ! A local of a device routine whose extent calls a function of a module,
  ! `real :: w(twice(n))`, is declared in the kernel's copy of the routine
  ! with the kernel's copy of the function. The host cannot call that copy
  ! to size the array before the launch, so the GPU backends report an
  ! error (see tests/tests.toml).
  implicit none
contains
  pure integer function twice(n)
    integer, intent(in) :: n
    twice = 2*n
  end function

  pure real function work(n, x) result(r)
    integer, intent(in) :: n
    real, intent(in) :: x
    real :: w(twice(n))
    integer :: j
    do j = 1, size(w)
      w(j) = x + j
    end do
    r = sum(w)
  end function
end module

program gpu_metal_407
  use gpu_metal_407_m
  implicit none
  integer, parameter :: m = 8
  real :: x(m), r(m)
  integer :: i
  do i = 1, m
    x(i) = i
  end do
  do concurrent (i = 1:m)
    r(i) = work(3, x(i)) + 0
  end do
  do i = 1, m
    if (abs(r(i) - (6*x(i) + 21)) > 1e-3) error stop 1
  end do
  print *, "ok"
end program

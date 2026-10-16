module gpu_metal_391_m
  ! The result of `ramp(x(i), k)` is a temporary of the compiler whose
  ! extent `k` the loop sets twice, so the host cannot size it before the
  ! launch: it is allocated on the device heap.
  implicit none
contains
  pure function ramp(x, n) result(y)
    real, intent(in) :: x
    integer, intent(in) :: n
    real :: y(n)
    integer :: j
    do j = 1, n
      y(j) = x + j
    end do
  end function
end module

program gpu_metal_391
  use gpu_metal_391_m
  implicit none
  integer, parameter :: m = 8
  real :: x(m), r(m)
  integer :: i
  do i = 1, m
    x(i) = i
  end do
  do concurrent (i = 1:m)
    block
      integer :: k
      k = 2
      if (x(i) > 3) k = k + 1
      r(i) = sum(ramp(x(i), k))
    end block
  end do
  do i = 1, m
    if (x(i) > 3) then
      if (abs(r(i) - (3*x(i) + 6)) > 1e-5) error stop 1
    else
      if (abs(r(i) - (2*x(i) + 3)) > 1e-5) error stop 2
    end if
  end do
  print *, "ok"
end program

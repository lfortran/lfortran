module gpu_metal_405_m
  implicit none
contains
  pure function g2(a, k) result(r)
    integer, allocatable, intent(in) :: a(:)
    integer, intent(in) :: k
    integer :: r
    r = a(k) * 10 + lbound(a, 1)
  end function

  pure function g(a, k) result(r)
    integer, allocatable, intent(in) :: a(:)
    integer, intent(in) :: k
    integer :: r
    r = g2(a, k) + size(a)
  end function
end module

program gpu_metal_405
  use gpu_metal_405_m
  implicit none
  integer, allocatable :: y(:)
  integer :: i, res(3)
  allocate(y(0:2))
  y = [4, 5, 6]
  do concurrent (i = 1:3)
    res(i) = g(y, i - 1)
  end do
  print *, res
  if (any(res /= [43, 53, 63])) error stop
end program

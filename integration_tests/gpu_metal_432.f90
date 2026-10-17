module gpu_metal_432_m
  implicit none
  type :: net_t
    integer, allocatable :: nodes_(:)
  end type
contains
  pure function f(net, k) result(r)
    type(net_t), intent(in) :: net
    integer, intent(in) :: k
    integer :: r
    r = net%nodes_(k) * 10 + lbound(net%nodes_, 1)
  end function
end module

program gpu_metal_432
  use gpu_metal_432_m
  implicit none
  type(net_t) :: net
  integer :: i, res(3), res2(3)
  allocate(net%nodes_(0:2))
  net%nodes_ = [5, 6, 7]
  do concurrent (i = 1:3)
    res(i) = net%nodes_(i - 1)
    res2(i) = f(net, i - 1)
  end do
  print *, res, res2
  if (any(res /= [5, 6, 7])) error stop 1
  if (any(res2 /= [50, 60, 70])) error stop 2
  do concurrent (i = 1:3)
    net%nodes_(i - 1) = net%nodes_(i - 1) + 1
    res2(i) = f(net, i - 1)
  end do
  print *, net%nodes_, res2
  if (any(net%nodes_ /= [6, 7, 8])) error stop 3
  if (any(res2 /= [60, 70, 80])) error stop 4
end program

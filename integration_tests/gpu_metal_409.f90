module gpu_metal_409_m
  implicit none
  type :: pt
    real :: x, y
  end type
contains
  pure function norm1(p) result(r)
    type(pt), intent(in) :: p
    real :: r
    r = abs(p%x) + abs(p%y)
  end function

  pure function f(k) result(r)
    integer, intent(in) :: k
    real :: r
    type(pt) :: q
    q%x = k
    q%y = -2*k
    r = norm1(q)
  end function
end module

program gpu_metal_409
  use gpu_metal_409_m
  implicit none
  real :: res(3), res2(3)
  integer :: i
  do concurrent (i = 1:3)
    block
      type(pt) :: q
      q%x = i
      q%y = -2*i
      res(i) = norm1(q)
    end block
  end do
  print *, res
  if (any(res /= [3, 6, 9])) error stop 1
  do concurrent (i = 1:3)
    res(i) = f(i)
    res2(i) = 1 + f(i)
  end do
  print *, res, res2
  if (any(res /= [3, 6, 9])) error stop 2
  if (any(res2 /= [4, 7, 10])) error stop 3
end program

module gpu_metal_428_m
  implicit none
contains
  pure function total(a, k) result(s)
    integer, intent(in) :: a(:), k
    integer :: s
    s = sum(a, mask=a > k)
  end function
end module
program gpu_metal_428
  use gpu_metal_428_m
  implicit none
  integer :: x(3), res(3), res2(3), i
  real :: y(2, 3), r(3)
  x = [1, 2, 3]
  y = reshape([1., 2., 3., 4., 5., 6.], [2, 3])
  do concurrent (i = 1:3)
    res(i) = total(x, i - 1)
    res2(i) = sum(x, mask=x /= i)
    r(i) = sum(y(:, i), mask=y(:, i) > 2.5)
  end do
  print *, res, res2, r
  if (any(res /= [6, 5, 3])) error stop 1
  if (any(res2 /= [5, 4, 3])) error stop 2
  if (any(r /= [0., 7., 11.])) error stop 3
end program

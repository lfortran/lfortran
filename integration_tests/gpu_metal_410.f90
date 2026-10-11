module gpu_metal_410_m
  implicit none
contains
  pure function total(a, k) result(s)
    integer, intent(in) :: a(:), k
    integer :: s
    s = sum(a, mask=a > k)
  end function
end module
program gpu_metal_410
  use gpu_metal_410_m
  implicit none
  integer :: x(3), res(3), i
  x = [1, 2, 3]
  do concurrent (i = 1:3)
    res(i) = 1 + total(x, i - 1)
  end do
  print *, res
  if (any(res /= [7, 6, 4])) error stop
end program

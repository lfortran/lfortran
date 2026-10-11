module gpu_metal_403_m
  implicit none
contains
  pure function offset_sum(k) result(s)
    integer, intent(in) :: k
    real :: s
    real :: c(3, 2)
    c = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [3, 2])
    s = c(k, 2) + sum(c)
  end function
end module
program gpu_metal_403
  use gpu_metal_403_m
  implicit none
  real :: s3(3)
  integer :: i
  do concurrent (i = 1:3)
    s3(i) = 1 + offset_sum(i)
  end do
  print *, s3
  if (any(s3 /= [26, 27, 28])) error stop
end program

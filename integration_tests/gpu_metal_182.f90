program gpu_metal_182
  implicit none
  real :: vals(2, 2)
  logical :: res(2)
  integer :: i
  vals = reshape([0.5, 0.6, 0.1, 0.2], [2, 2])
  do concurrent(i=1:2)
    res(i) = sum(vals(:,i)) > 0.99
  end do
  print *, res
  if (.not. res(1)) error stop
  if (res(2)) error stop
end program

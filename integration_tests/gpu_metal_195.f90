program gpu_metal_195
  ! Test: do concurrent with non-default lower bound on allocatable arrays
  implicit none
  integer, allocatable :: n(:)
  integer :: r, l

  allocate(n(0:3))
  n = [10, 20, 30, 40]

  do concurrent(l = 3:3)
    r = n(l)
  end do

  if (r /= 40) error stop
  print *, "PASS"
end program

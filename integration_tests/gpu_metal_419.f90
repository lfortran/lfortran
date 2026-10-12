program gpu_metal_419
  ! Many launches of a kernel handed a large array: every launch releases
  ! what it set up (the device's copy of each argument), or the program
  ! runs out of memory long before the last launch.
  implicit none
  integer, parameter :: n = 4 * 1024 * 1024, launches = 2000
  real, allocatable :: x(:)
  integer :: i, k
  allocate(x(n))
  x = 0
  do k = 1, launches
    do concurrent (i = 1:8)
      x(i) = x(i) + 1
    end do
  end do
  print *, x(1), x(8), x(n)
  if (any(x(1:8) /= launches)) error stop
  if (any(x(9:) /= 0)) error stop
end program

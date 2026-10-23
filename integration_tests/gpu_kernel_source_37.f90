! One element of an array with unspecified locality is defined and then
! referenced in each iteration: the array is private to the iteration, so
! no shared buffer may be passed to the kernel.
program gpu_kernel_source_array_element_temporary
  implicit none
  integer, parameter :: n = 1000
  integer :: w(2), i, m, nbad
  integer, allocatable :: a(:), b(:)
  allocate(a(n), b(n))
  m = 1
  do concurrent (i = 1:n)
    w(1) = i
    a(i) = w(m)
  end do
  do concurrent (i = 1:n)
    w(1) = 2*i
    b(i) = w(1) + 1
  end do
  nbad = 0
  do i = 1, n
    if (a(i) /= i) nbad = nbad + 1
    if (b(i) /= 2*i + 1) nbad = nbad + 1
  end do
  print *, "wrong elements:", nbad
  if (nbad /= 0) error stop 1
end program

program alloc_source
  implicit none

  integer, allocatable :: a(:), b(:)

  allocate(a(4))
  a = [1, 2, 3, 4]

  allocate(b, source=a)
  if (any(b /= a)) error stop "wrong output: b contents do not match a"
  print *, "test passed"
end program alloc_source

program intrinsics_378
  implicit none
  integer, allocatable :: a(:), b(:)

  allocate(a(3), b(3))
  a = [1, 2, 3]

  call move_alloc(a, b)

  print *, allocated(a)

end program intrinsics_378

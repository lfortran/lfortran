program intrinsics_378
  implicit none
  integer, allocatable :: a(:), b(:), c(:)

  allocate(a(3), b(3))
  a = [1, 2, 3]

  call move_alloc(a, b)

  print *, allocated(a)
  print *, b

  if (.not. allocated(b)) error stop
  if (allocated(c)) error stop
  if (any(b /= [1, 2, 3])) error stop
    
  call move_alloc(b, c)

  print *, allocated(b)
  print *, c

  if (allocated(b)) error stop
  if (.not. allocated(c)) error stop
  if (any(c /= [1, 2, 3])) error stop

end program intrinsics_378

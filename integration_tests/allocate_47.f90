program allocate_47
  implicit none
  character(:), allocatable :: stuff(:)
  allocate(character(5) :: stuff(3))
  stuff(1) = 'hello'
  stuff(2) = 'world'
  stuff(3) = 'test!'
  if (len(stuff(1)) /= 5) error stop
  if (size(stuff) /= 3) error stop
  if (stuff(1) /= 'hello') error stop
  if (stuff(2) /= 'world') error stop
  if (stuff(3) /= 'test!') error stop
  print *, stuff
  deallocate(stuff)
end program allocate_47

implicit none
  character(:),allocatable::a,b,words(:)
  a = 'first'
  b = 'second'
  allocate(character(max(len(a),len(b))) :: words(2))
  words(1) = a
  words(2) = b
  if (len(words(1)) /= 6) stop 1
  if (len(words(2)) /= 6) stop 2
  if (words(1) /= 'first ') stop 3
  if (words(2) /= 'second') stop 4
  print "(A,2I2)",'Lengths:',len(words(1)),len(words(2))
  print *, 'Test passed'
end program
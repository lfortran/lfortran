! Test allocatable character array with max len allocation
! Related to issue #6877
program string_102
  implicit none
  character(:),allocatable::a,b,words(:)
  a = 'first'
  b = 'second'
  allocate(character(max(len(a),len(b))) :: words(2))
  words(1) = a
  words(2) = b
  if (len(words(1)) /= 6) error stop
  if (len(words(2)) /= 6) error stop
end program

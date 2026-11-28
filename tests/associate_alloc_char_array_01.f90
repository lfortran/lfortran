program associate_alloc_char_array_01
  implicit none
  character(:), allocatable :: dum
  dum = 's2'
  associate(y => [dum])
    if (any(y /= [dum])) error stop 1
  end associate
end program

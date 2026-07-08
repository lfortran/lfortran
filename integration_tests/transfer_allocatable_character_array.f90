program transfer_allocatable_character_array
  implicit none

  character(len=1), allocatable :: chars(:)

  chars = transfer(" ABCDEFG abcdefg ", "A", size=17)

  print *, chars
end program transfer_allocatable_character_array

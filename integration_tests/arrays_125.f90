program arrays_125
  character(:), allocatable :: c(:)
  c = ["assem", "mehat", "fathy"]
  deallocate(c)
end program
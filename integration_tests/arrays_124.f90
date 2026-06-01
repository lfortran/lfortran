program test
  character(:), allocatable :: c(:)
  c = ["assem", "mehat", "fathy"]
  deallocate(c)
  print *, len(c)
  print *, c, "d"
end program

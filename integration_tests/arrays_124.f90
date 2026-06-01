program test
  character(:), allocatable :: c(:)
  c = ["apple", "mango", "peach"]
  deallocate(c)
  print *, len(c)
  print *, c, "d"
end program

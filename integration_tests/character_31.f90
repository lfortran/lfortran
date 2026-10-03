program character_31
    implicit none
    character(len=1) :: tmp(1)
    tmp = transfer("A", "x", 1)

    if (tmp(1) /= "A") error stop
  print *, "Test passed"
end program character_31

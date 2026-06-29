program character_30
  character(4), dimension(8) :: abuf = (/"0123","4567","89AB","CDEF", &
                                       "GHIJ","KLMN","OPQR","STUV"/)
  character(4), dimension(2,4) :: buf
  character(8) :: a

  read(abuf, "(8A4)") buf
  a = buf(1,1)

  print *, a
  if (a /= "0123") error stop
end program
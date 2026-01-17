program linetest
  character(80) :: line
  write(line(1:3), '(I3)') 666
  
  if (line(1:3) /= "666") then
    write(*, *) "Test Failed: Expected '666', got '", line(1:3), "'"
    error stop 1
  end if
  
  write(*, '(A)') line(1:3)
end program linetest
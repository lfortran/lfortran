program linetest
  character(80):: line
  write(line(1:3),'(I3)') 666
  write(*,'(A)') line(1:3)
end program linetest
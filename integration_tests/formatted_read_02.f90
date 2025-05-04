program formatted_read_input_02
  character(len=10) :: str1

  open(unit=10, file='formatted_read_input_02.txt', status='old')

  read(unit=10, fmt='(a5)') str1
  print *, str1
  print *, len(str1)

  close(10)
end program formatted_read_input_02
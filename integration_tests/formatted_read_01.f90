program formatted_read_01
  character(len=10) :: str1
  character(len=15) :: str2
  character(len=5) :: str3
  character(len=10) :: str4

  open(unit=10, file="formatted_read_input_01.txt", status="old")

  read(unit=10, fmt='(a10)') str1
  print *, str1
  print *, len(str1)

  read(unit=10, fmt='(a10)') str2
  print *, str2
  print *, len(str2)

  read(unit=10, fmt='(a10)') str3
  print *, str3
  print *, len(str3)

  read(unit=10, fmt='(a5)') str1
  print *, str1
  print *, len(str1)

  close(10)
end program formatted_read_01
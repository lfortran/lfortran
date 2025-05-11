program formatted_read_01
  character(len=10) :: str1
  character(len=15) :: str2
  character(len=5) :: str3
  character(len=10) :: str4

  open(unit=10, file="formatted_read_input_01.txt", status="old")

  read(unit=10, fmt='(a10)') str1
  if (trim(str1) /= "hello") then
    print *, "Error: str1 should be 'hello'"
    error stop 1
  end if

  read(unit=10, fmt='(a10)') str2
  if (trim(str2) /= "world") then
    print *, "Error: str2 should be 'world'"
    error stop 2
  end if

  read(unit=10, fmt='(a10)') str3
  if (trim(str3) /= "horty") then
    print *, "Error: str3 should be 'horty'"
    error stop 3
  end if

  read(unit=10, fmt='(a5)') str4
  if (trim(str4) /= "longe     ") then
    print *, "Error: str4 should be 'longe'"
    error stop 4
  end if

  close(10)
  print *, "All tests passed!"
end program formatted_read_01
! This file check string concat functionality for arrays
program intrinsics_416
  implicit none
  character(:),allocatable:: str1(:), str2
  character(4):: str3(2)
  character(4),parameter :: str4(2) = ['abc ','de  ']

  character(2):: str5
  character(16):: str6 

  str5 = 'xy'
  str1 = ['abc ', 'de  ']
  str2 = 'abc '
  str3 = ['abc ', 'de  ']

  ! 1. Check str1 (Array)
  str6 = ''
  print "(*(A))", '"'//str1(:)//str5//'"'
  write(str6, "(*(A))") '"'//str1(:)//str5//'"'
  if ((str6) /= ('"abc xy""de  xy"')) error stop

  ! 2. Check str2 (Scalar)
  str6 = ''
  print "(*(A))", '"'//str2//str5//'"'
  write(str6, "(*(A))") '"'//str2//str5//'"'
  if (str6 /= '"abc xy"') error stop

  ! 3. Check str3 (Fixed Array)
  str6 = ''
  print "(*(A))", '"'//str3//str5//'"'
  write(str6, "(*(A))") '"'//str3(:)//str5//'"'
  if (str6 /= '"abc xy""de  xy"') error stop
  
  ! Check str4 (Parameter Arrays)
  str6 = ''
  print "(A)",'"'//str4//str5//'"'
  write(str6, "(*(A))") '"'//str4(:)//str5//'"'
  if (str6 /= '"abc xy""de  xy"') error stop

end program intrinsics_416
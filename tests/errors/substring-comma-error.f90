program issue2073bug
  character(len = *),parameter:: string = 'A character string'
  integer, parameter:: n = len(string)
  integer i
  character(len = 1) charray(n)
  do i = 1,n
     charray(i) = string(i,i) ! (i:i) would have been OK
  end do
  print "(*(A))" , charray
end program issue2073bug
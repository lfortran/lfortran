! Test for https://github.com/lfortran/lfortran/issues/4888
! String array concatenation (//) in write statement
! Exact MRE from issue body
program writetesta
  implicit none
  character(3):: c(6) = 'ab'
  character(80) :: output
  write(output,"(6A)") c//' '
  print "(A)", trim(output)
  if (trim(output) /= 'ab  ab  ab  ab  ab  ab') error stop
end program writetesta

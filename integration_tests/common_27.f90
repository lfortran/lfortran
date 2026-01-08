program common_save
  call sub()
  print *, "test passed"
end program

subroutine sub()
  real :: xyzzy
  common /block/ xyzzy
  save /block/
  
  xyzzy = xyzzy + 1.0
  
  if (xyzzy /= 1.0) error stop "wrong value"
end subroutine

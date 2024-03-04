! program test_achar
!   integer ::c = 0
!   print*, achar(c)
! end program test_achar

program NullCharacterExample
  character :: null_char

  ! Using ACHAR to represent null character
  null_char = ACHAR(0)

  ! Printing the null character
  print *, "Null character: '", null_char, "'"
  print*, iachar(null_char)
end program NullCharacterExample

program equivalence_46
  implicit none
  integer :: i
  common /block/ i
  equivalence (i, i)
  
  i = 1
  if (i /= 1) then
     error stop "Assertion failed: i should be 1"
  end if
  print *, "Test passed!"
end program equivalence_46
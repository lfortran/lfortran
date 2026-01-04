program statement_07
  implicit logical (l)
  
  ! Both function and argument 'l' should be implicitly logical
  lfis01(l) = .not. l
  
  if (lfis01(.true.)) error stop "Statement function returned wrong value"
  
  print *, "All tests passed!"
end program statement_07

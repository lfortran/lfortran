program MaxExample
  integer :: a, b, result
  a = 10
  b = 7
  if ( .not. min0(a1=a, a2=b) == 7) error stop
end program MaxExample

program MaxExample
  integer :: a, b, result
  a = 10
  b = 7
  if ( .not. max0(a1=a, a2=b) == 10) error stop
end program MaxExample

! AST only
subroutine test
  implicit none
  real a,b,c,d
  common // a,b
  common / / a,b
  common a, b
  common a /b1/ c,d // b
  common a, /b1/ c,d, // b
  common // b(2:4)
  common b(:4)
  common b(4)
  common /b1/ c,d, // a,b
  common /b1/ c,d // a, b
  common /b1/ c,d / / a, b
end subroutine test
  

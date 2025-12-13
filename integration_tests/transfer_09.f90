program transfer_09
  integer, dimension(3) :: lhs, rhs
  real, dimension(3):: r1 
  real:: a,b,c 

  rhs = [10, 20, 30]

  a = transfer(rhs(1), a)
  b = transfer(rhs(2), b)
  c = transfer(rhs(3), c)
  r1(1:3) = transfer(rhs, r1)
  lhs(1:3) = transfer(rhs, lhs)
  print *, "Correct Values", rhs, a, b, c
  print *, "Transferred Values", lhs, r1

  ! Check aray transfer of same type
  if (lhs(1) /= rhs(1) .or. lhs(2) /= rhs(2) .or. lhs(3) /= rhs(3)) error stop

  !Check with scalar transfers for real type
  if (r1(1) /= a .or. r1(2) /= b .or. r1(3) /= c) error stop
end program transfer_09
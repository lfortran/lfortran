program write_17
  real:: x(3) = [ 1.1, 2.2, 3.3 ]
  integer i
  character(4)::cx1(3), cx2(2), cx3(3) = " 6.0"

  ! Check full-array assignment
  write(cx1,"(F4.1)") x
  print "(3A)", cx1
  if (cx1(1) /= " 1.1" .and. cx1(2) /= " 2.2" .and. cx1(3) /= " 3.3") error stop

  ! Check do-loops on RHS
  write(cx2,"(F4.1)") (x(i),i=2,3)
  print "(3A)", cx2
  if (cx2(1) /= " 2.2" .and. cx2(2) /= " 3.3") error stop

  ! Check do-loops on LHS and RHS, with different bounds
  write(cx3(2:3),"(F4.1)") (x(i), i=1,2)
  print "(3A)", cx3
  if (cx3(1) /= " 6.0" .and. cx3(2) /= " 1.1" .and. cx3(3) /= " 2.2") error stop

  write(cx3(2:3),"(F4.1)") (x(i), i=2,3)
  print "(3A)", cx3
  if (cx3(1) /= " 6.0" .and. cx3(2) /= " 2.2" .and. cx3(3) /= " 3.3") error stop
end program write_17

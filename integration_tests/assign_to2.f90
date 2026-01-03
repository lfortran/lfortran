program main
integer :: i, sum, ZERO
10 assign 30 to next
sum = ZERO
i = 1
20    GO TO next,(30, 50, 70)
30 IF( 0<1) GO TO 70
50 print *, 50
assign 50 to next
70 print *, 70
end program

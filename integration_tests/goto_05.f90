program goto_05
implicit none
integer :: a, n

! Jump to the label n = 1 -> 1
assign 1 to n
a = 0
go to n, (1, 2, 3)
  a = a + 1
1 a = a + 2
2 a = a + 4
3 a = a + 8
print *, a
if(a /= 2+4+8) error stop

! Jump to the label n = 6 -> 6
assign 6 to n
a = 0
go to n, (4, 5, 6)
  a = a + 1
4 a = a + 2
5 a = a + 4
6 a = a + 8
print *, a
if(a /= 8) error stop

! Jump to the label n = 8 -> 8
assign 8 to n
a = 0
go to n, (7, 8, 9)
  a = a + 1
7 a = a + 2
8 a = a + 4
9 a = a + 8
print *, a
if(a /= 4+8) error stop

end program

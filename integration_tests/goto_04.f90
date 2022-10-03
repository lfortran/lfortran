program goto_04
implicit none
integer :: a, n
n = 2
a = 10
! Jump to the label n-1 -> 1
go to (1, 2, 3), n - 1
a = a + 5
1 a = a + 10
2 a = a + 20
3 a = a + 30

if(a /= 70) error stop

go to (4, 5, 6), n + 1

4 a = a + 40
5 a = a + 50
6 a = a + 60

if(a /= 130) error stop
! Jump to the label n*n -> 4 -> noop
go to (7, 8, 9), n * n

7 a = a + 70
8 a = a + 80
9 a = a + 90

if(a /= 370) error stop

end program

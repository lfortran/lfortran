! A formatted READ from an internal file of an implied DO loop that does not
! cover the whole array
program read_105
implicit none
integer :: a(5), i, n
character(len=20) :: s

s = ' 1 2 3 4 5'
n = 3
a = 9
read(s, '(5i2)') (a(i), i = 1, n)
if (any(a /= [1, 2, 3, 9, 9])) error stop 1
if (i /= 4) error stop 2
a = 9
read(s, '(5i2)') (a(i), i = n, 5)
if (any(a /= [9, 9, 1, 2, 3])) error stop 3
if (i /= 6) error stop 4
a = 9
read(s, '(5i2)') (a(i), i = n + 2, 1, -2)
if (any(a /= [3, 9, 2, 9, 1])) error stop 5
if (i /= -1) error stop 6
a = 9
read(s, '(5i2)') (a(i), i = 1, n), (a(i), i = n + 1, n + 1)
if (any(a /= [1, 2, 3, 4, 9])) error stop 7
if (i /= 5) error stop 8
print *, 'ok'
end program

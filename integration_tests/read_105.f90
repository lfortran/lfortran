! A formatted READ from an internal file of an implied DO loop that does not
! cover the whole array
program read_105
implicit none
integer :: a(5), b(5), i, j, n
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
a = 9
read(s, '(5i2)') (a(i), i = n, 2), a(5)
if (any(a /= [9, 9, 9, 9, 1])) error stop 9
if (i /= 3) error stop 10
! The bounds of a later implied DO loop use the variable of an earlier one
a = 9
i = 0
read(s, '(5i2)') (a(i), i = 1, n - 1), (a(j), j = n, 1 + i)
if (any(a /= [1, 2, 3, 4, 9])) error stop 11
if (i /= 3) error stop 12
if (j /= 5) error stop 13
a = 9
i = 0
read(s, '(5i2)') (a(i), i = 2, n), (a(j), j = i, i)
if (any(a /= [9, 1, 2, 3, 9])) error stop 14
if (j /= 5) error stop 15
! A later item uses the variable of an earlier implied DO loop
a = 9
b = 9
i = 0
read(s, '(5i2)') (a(i), i = 1, n - 1), b(i)
if (any(a /= [1, 2, 9, 9, 9])) error stop 16
if (any(b /= [9, 9, 3, 9, 9])) error stop 17
if (i /= 3) error stop 18
print *, 'ok'
end program

! An internal file that is an array that is not contiguous
program read_104
implicit none
character(len=4), target :: lines(6)
character(len=4), pointer :: pl(:)
integer :: a(3)

lines = ['  11', '  xx', '  22', '  xx', '  33', '  xx']
pl => lines(1:5:2)
a = 0
read(pl, '(i4)') a
if (any(a /= [11, 22, 33])) error stop 1
a = 0
read(pl, *) a
if (any(a /= [11, 22, 33])) error stop 2
a = 0
read(lines(1:5:2), '(i4)') a
if (any(a /= [11, 22, 33])) error stop 3

lines = 'zzzz'
pl => lines(2:6:2)
write(pl, '(i4)') 1, 2, 3
if (any(lines /= ['zzzz', '   1', 'zzzz', '   2', 'zzzz', '   3'])) error stop 4
lines = 'zzzz'
write(pl, '(i4)') 4
if (any(lines /= ['zzzz', '   4', 'zzzz', 'zzzz', 'zzzz', 'zzzz'])) error stop 5
lines = 'zzzz'
pl => lines(2:4)
write(pl, '(i4)') 5, 6
if (any(lines /= ['zzzz', '   5', '   6', 'zzzz', 'zzzz', 'zzzz'])) error stop 6
print *, 'ok'
end program

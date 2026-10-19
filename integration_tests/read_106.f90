! A READ statement defines its items in the order of the list, also when a
! later item shares storage with an earlier non-contiguous one
program read_106
implicit none
integer, target :: a(6)
integer, pointer :: p(:), q(:), ip
character(len=4), target :: c(4)
character(len=4), pointer :: cp(:)
character(len=20) :: s
integer :: u, k, n, i

p => a(1:5:2)
q => a(1:3)
s = '1 2 3 4 5 6'
a = 0
read(s, *) p, q
if (any(a /= [4, 5, 6, 0, 3, 0])) error stop 1
a = 0
read(s, *) a(1:5:2), a(1:3)
if (any(a /= [4, 5, 6, 0, 3, 0])) error stop 2
a = 0
read(s, *) a(1:5:2), a(1)
if (any(a /= [4, 0, 2, 0, 3, 0])) error stop 3
a = 0
read(s, 10) a(1:5:2), a(1:3)
10 format(6i2)
if (any(a /= [4, 5, 6, 0, 3, 0])) error stop 4

ip => a(1)
a = 0
read(s, *) p, ip
if (any(a /= [4, 0, 2, 0, 3, 0])) error stop 5

! A subscript that uses a variable read earlier in the statement
a = 0
s = '1 2 3 4 5'
read(s, *) p, k, a(k)
if (any(a /= [1, 0, 2, 5, 3, 0])) error stop 6
if (k /= 4) error stop 7

c = ['aaaa', 'bbbb', 'cccc', 'dddd']
cp => c(1:3:2)
s = 'wwwwxxxxyyyy'
read(s, '(3a4)') cp, c(1)
if (any(c /= ['yyyy', 'bbbb', 'xxxx', 'dddd'])) error stop 8

! Items after a non-contiguous one: an implied DO loop, and an item whose
! subscript, or the bound of an implied DO loop, is read earlier in the
! statement
n = 3
s = '1 2 3 4 5 6'
a = 0
read(s, *) a(1:5:2), (a(i), i = 1, n)
if (any(a /= [4, 5, 6, 0, 3, 0])) error stop 11
a = 0
read(s, *) p, (a(i), i = 1, n)
if (any(a /= [4, 5, 6, 0, 3, 0])) error stop 12
s = '1 2 3 1 9'
a = 0
n = 5
read(s, *) a(1:5:2), n, a(n)
if (any(a /= [9, 0, 2, 0, 3, 0]) .or. n /= 1) error stop 13
a = 0
n = 5
read(s, *) p, n, q(n)
if (any(a /= [9, 0, 2, 0, 3, 0]) .or. n /= 1) error stop 14
s = '1 2 3 2 8 9'
a = 0
read(s, *) a(1:5:2), n, (a(i), i = 1, n)
if (any(a /= [8, 9, 2, 0, 3, 0]) .or. n /= 2) error stop 15

open(newunit=u, status='scratch', form='unformatted', access='stream')
write(u) 1, 2, 3, 4, 5, 6
rewind(u)
a = 0
read(u) p, q
if (any(a /= [4, 5, 6, 0, 3, 0])) error stop 9
rewind(u)
a = 0
read(u) p, a(3)
if (any(a /= [1, 0, 4, 0, 3, 0])) error stop 10
rewind(u)
a = 0
n = 3
read(u) a(1:5:2), (a(i), i = 1, n)
if (any(a /= [4, 5, 6, 0, 3, 0])) error stop 16
rewind(u)
write(u) 1, 2, 3, 1, 9
rewind(u)
a = 0
n = 5
read(u) a(1:5:2), n, a(n)
if (any(a /= [9, 0, 2, 0, 3, 0]) .or. n /= 1) error stop 17
rewind(u)
a = 0
n = 5
read(u) p, n, q(n)
if (any(a /= [9, 0, 2, 0, 3, 0]) .or. n /= 1) error stop 18
close(u)
print *, 'ok'
end program

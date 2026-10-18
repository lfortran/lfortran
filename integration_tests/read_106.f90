! A READ statement defines its items in the order of the list, also when a
! later item shares storage with an earlier non-contiguous one
program read_106
implicit none
integer, target :: a(6)
integer, pointer :: p(:), q(:), ip
character(len=4), target :: c(4)
character(len=4), pointer :: cp(:)
character(len=20) :: s
integer :: u, k

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
close(u)
print *, 'ok'
end program

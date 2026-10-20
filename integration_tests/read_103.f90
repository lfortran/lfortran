! READ into arrays that are not contiguous
program read_103
implicit none
integer, target :: a(6), b(4, 4)
integer, pointer :: p(:), q(:, :)
character(len=3), target :: s(6)
character(len=3), pointer :: ps(:)
complex, target :: z(4)
integer :: u

open(newunit=u, status='scratch', form='unformatted')
write(u) 10, 30, 50
write(u) 10, 30, 50
write(u) 10, 30, 50
write(u) 1, 2, 3, 4
write(u) 'xxx', 'yyy', 'zzz'
write(u) (1, 1), (3, 3)
write(u) 10, 30, 50
write(u) 70
rewind(u)
a = 0
read(u) a(1:6:2)
if (any(a /= [10, 0, 30, 0, 50, 0])) error stop 1
a = 0
p => a(2:6:2)
read(u) p
if (any(a /= [0, 10, 0, 30, 0, 50])) error stop 2
a = 0
call read_dummy(u, a(1:6:2))
if (any(a /= [10, 0, 30, 0, 50, 0])) error stop 3
b = 0
q => b(1:3:2, 2:4:2)
read(u) q
if (b(1, 2) /= 1 .or. b(3, 2) /= 2 .or. b(1, 4) /= 3 .or. b(3, 4) /= 4) error stop 4
if (sum(b) /= 10) error stop 5
s = 'aaa'
ps => s(6:2:-2)
read(u) ps
if (any(s /= ['aaa', 'zzz', 'aaa', 'yyy', 'aaa', 'xxx'])) error stop 6
z = (0, 0)
read(u) z(2:4:2)
if (any(z /= [(0, 0), (1, 1), (0, 0), (3, 3)])) error stop 7
a = 0
call read_rank(u, a(2:6:2))
if (any(a /= [0, 10, 0, 30, 0, 50])) error stop 11
call read_rank(u, a(3))
if (any(a /= [0, 10, 70, 30, 0, 50])) error stop 13
close(u)

open(newunit=u, status='scratch', form='formatted')
write(u, '(a)') '10 30 50'
write(u, '(a)') '20 40 60'
write(u, '(3i3)') 10, 30, 50
rewind(u)
a = 0
read(u, *) a(1:6:2)
if (any(a /= [10, 0, 30, 0, 50, 0])) error stop 8
a = 7
p => a(2:6:2)
read(u, *) p
if (any(a /= [7, 20, 7, 40, 7, 60])) error stop 9
a = 0
read(u, '(3i3)') a(6:1:-2)
if (any(a /= [0, 50, 0, 30, 0, 10])) error stop 10
close(u)
print *, 'ok'

contains

subroutine read_dummy(u, x)
integer, intent(in) :: u
integer, intent(inout) :: x(:)
read(u) x
end subroutine

subroutine read_rank(u, x)
integer, intent(in) :: u
integer, intent(inout) :: x(..)
select rank (x)
rank (0)
    read(u) x
rank (1)
    read(u) x
rank default
    error stop 12
end select
end subroutine

end program

! Unformatted WRITE of arrays that are not contiguous
program write_53
implicit none
integer, target :: a(6), b(4, 4), c(3, 3)
integer, pointer :: p(:), q(:, :)
character(len=3), target :: s(6)
character(len=3), pointer :: ps(:)
logical, target :: l(6)
real(8) :: d(6)
complex :: z(4)
integer :: r3(3), r4(4), u, i
character(len=3) :: rs(3)
logical :: rl(3)
real(8) :: rd(3)
complex :: rz(2)

a = [1, 2, 3, 4, 5, 6]
b = reshape([(i, i = 1, 16)], [4, 4])
c = reshape([(i, i = 1, 9)], [3, 3])
s = ['aaa', 'bbb', 'ccc', 'ddd', 'eee', 'fff']
l = [.true., .false., .true., .false., .true., .false.]
d = [1d0, 2d0, 3d0, 4d0, 5d0, 6d0]
z = [(1, 1), (2, 2), (3, 3), (4, 4)]

open(newunit=u, status='scratch', form='unformatted')
write(u) a(1:6:2)
p => a(2:6:2)
write(u) p
call write_dummy(u, a(1:6:2))
q => b(1:3:2, 2:4:2)
write(u) q
p => c(2, :)
write(u) p
ps => s(2:6:2)
write(u) ps
write(u) l(1:5:2)
write(u) d(6:1:-2)
write(u) z(1:4:2)
p => a(2:4)
write(u) p

rewind(u)
read(u) r3
if (any(r3 /= [1, 3, 5])) error stop 1
read(u) r3
if (any(r3 /= [2, 4, 6])) error stop 2
read(u) r3
if (any(r3 /= [1, 3, 5])) error stop 3
read(u) r4
if (any(r4 /= [5, 7, 13, 15])) error stop 4
read(u) r3
if (any(r3 /= [2, 5, 8])) error stop 5
read(u) rs
if (any(rs /= ['bbb', 'ddd', 'fff'])) error stop 6
read(u) rl
if (.not. all(rl)) error stop 7
read(u) rd
if (any(rd /= [6d0, 4d0, 2d0])) error stop 8
read(u) rz
if (any(rz /= [(1, 1), (3, 3)])) error stop 9
read(u) r3
if (any(r3 /= [2, 3, 4])) error stop 10
close(u)
print *, 'ok'

contains

subroutine write_dummy(u, x)
integer, intent(in) :: u
integer, intent(in) :: x(:)
write(u) x
end subroutine

end program

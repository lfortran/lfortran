! c_f_pointer into a character-array pointer must not change its old target
! (#17884); it also works after NULLIFY and after DEALLOCATE.
program char_array_pointer_02
use iso_c_binding, only: c_ptr, c_loc, c_f_pointer
implicit none
character(len=4), target :: t(3), u(2)
character(len=4), pointer :: p(:)
type(c_ptr) :: cp
t = ['aaaa', 'bbbb', 'cccc']
u = ['uuuu', 'vvvv']
cp = c_loc(t)
p => u
call c_f_pointer(cp, p, [3])
if (any(u /= ['uuuu', 'vvvv'])) error stop 1
if (any(p /= ['aaaa', 'bbbb', 'cccc'])) error stop 2
nullify(p)
call c_f_pointer(cp, p, [2])
if (len(p) /= 4 .or. any(p /= ['aaaa', 'bbbb'])) error stop 3
nullify(p)
allocate(p(2))
p = 'xxxx'
deallocate(p)
call c_f_pointer(cp, p, [3])
if (len(p) /= 4 .or. any(p /= ['aaaa', 'bbbb', 'cccc'])) error stop 4
print *, 'ok'
end program

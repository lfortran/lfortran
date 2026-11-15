! c_f_pointer into a disassociated character-array pointer, after NULLIFY
! and after DEALLOCATE.
program allocate_char_pointer_array_02
use iso_c_binding, only: c_ptr, c_loc, c_f_pointer
implicit none
character(len=4), target :: t(3)
character(len=4), pointer :: p(:)
type(c_ptr) :: cp
t = ['aaaa', 'bbbb', 'cccc']
cp = c_loc(t)
nullify(p)
call c_f_pointer(cp, p, [2])
if (len(p) /= 4 .or. any(p /= ['aaaa', 'bbbb'])) error stop 1
allocate(p(2))
p = 'xxxx'
deallocate(p)
call c_f_pointer(cp, p, [3])
if (len(p) /= 4 .or. any(p /= ['aaaa', 'bbbb', 'cccc'])) error stop 2
print *, 'ok'
end program

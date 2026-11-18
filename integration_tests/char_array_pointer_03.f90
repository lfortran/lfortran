! A character-array pointer keeps its own string descriptor after pointer
! assignment, so ALLOCATE does not change the old target.
module char_array_pointer_03_mod
implicit none
type :: holder
    character(len=2), pointer :: c(:) => null()
end type
type :: item
    character(len=2) :: name
end type
character(len=2), target :: mt(2) = ['mm', 'nn']
contains
function get_mt() result(r)
    character(len=2), pointer :: r(:)
    r => mt
end function
end module

program char_array_pointer_03
use char_array_pointer_03_mod
implicit none
character(len=2), target :: t(3)
character(len=2), pointer :: p(:)
type(holder) :: h, h2
type(item), target :: s(2)

t = ['ab', 'cd', 'ef']
p => t
if (.not. associated(p, t)) error stop 1
p(1:2) => t
allocate(p(4))
p = 'zz'
if (any(t /= ['ab', 'cd', 'ef'])) error stop 2
deallocate(p)

h%c => t
h2 = h
allocate(h%c(1))
if (.not. associated(h2%c, t)) error stop 3
deallocate(h%c)

s%name = ['s1', 's2']
p => s%name
allocate(p(2))
p = 'yy'
if (any(s%name /= ['s1', 's2'])) error stop 4
deallocate(p)

p => get_mt()
allocate(p(1))
p = 'xx'
if (any(mt /= ['mm', 'nn'])) error stop 5
deallocate(p)
print *, 'ok'
end program

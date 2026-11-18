! A module character-array pointer used from a C main: the program has no
! Fortran main to set up its descriptor.
module char_array_pointer_04_mod
implicit none
character(len=4), pointer :: gp(:)
character(len=4), target :: gt(3) = ['aaaa', 'bbbb', 'cccc']
end module

subroutine char_array_pointer_04_run() bind(c, name="char_array_pointer_04_run")
use char_array_pointer_04_mod
implicit none
nullify(gp)
if (associated(gp)) error stop 1
gp => gt(2:3)
if (len(gp) /= 4 .or. any(gp /= ['bbbb', 'cccc'])) error stop 2
allocate(gp(2))
gp = 'zzzz'
if (any(gt /= ['aaaa', 'bbbb', 'cccc'])) error stop 3
deallocate(gp)
if (associated(gp)) error stop 4
print *, 'ok'
end subroutine

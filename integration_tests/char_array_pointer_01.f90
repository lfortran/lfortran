! ALLOCATE of a character-array pointer creates a new target and must leave
! the old target unchanged; DEALLOCATE disassociates the pointer.
module char_array_pointer_01_mod
implicit none
type :: holder
    character(len=2), pointer :: c(:) => null()
end type
contains
subroutine alloc_assumed_len(p)
    character(*), pointer, intent(inout) :: p(:)
    allocate(p(3))
    if (len(p) /= 4 .or. size(p) /= 3) error stop 20
    p = 'qrst'
end subroutine
end module

program char_array_pointer_01
use char_array_pointer_01_mod
implicit none
character(len=4), pointer :: ca(:), ca1(:), sp(:)
character(len=4), target :: carr(2), t(4)
character(:), allocatable, target :: dcarr(:)
character(:), pointer :: dca(:)
character(len=2), pointer :: nc(:) => null()
type(holder) :: h
integer :: ierr

carr = ['cccc', 'dddd']
ca => carr
allocate(ca(3), stat=ierr)
if (ierr /= 0 .or. size(ca) /= 3) error stop 1
ca = 'zzzz'
if (any(carr /= ['cccc', 'dddd'])) error stop 2
deallocate(ca)
if (associated(ca)) error stop 3

ca => carr
allocate(ca(3))
ca = 'zzzz'
if (any(carr /= ['cccc', 'dddd'])) error stop 4
ca1 => ca
allocate(ca(2))
ca = 'yyyy'
if (size(ca1) /= 3 .or. any(ca1 /= 'zzzz')) error stop 5
deallocate(ca1, ca)

allocate(character(4) :: dcarr(2))
dcarr = ['cccc', 'dddd']
dca => dcarr
allocate(character(3) :: dca(5), stat=ierr)
if (ierr /= 0 .or. size(dca) /= 5 .or. len(dca) /= 3) error stop 6
dca = 'yyy'
if (len(dcarr) /= 4 .or. any(dcarr /= ['cccc', 'dddd'])) error stop 7
deallocate(dca, dcarr)

t = ['aaaa', 'bbbb', 'cccc', 'dddd']
sp => t(2:3)
allocate(sp(4))
sp = 'xxxx'
if (any(t /= ['aaaa', 'bbbb', 'cccc', 'dddd'])) error stop 8
deallocate(sp)

allocate(nc(3))
nc = 'ab'
if (size(nc) /= 3 .or. any(nc /= 'ab')) error stop 9
deallocate(nc)
deallocate(nc, stat=ierr)
if (ierr == 0) error stop 13

allocate(h%c(3))
h%c = 'cd'
if (size(h%c) /= 3 .or. any(h%c /= 'cd')) error stop 10
deallocate(h%c)

allocate(ca(1))
ca = 'mmmm'
ca1 => ca
call alloc_assumed_len(ca)
if (any(ca /= 'qrst') .or. any(ca1 /= 'mmmm')) error stop 11
deallocate(ca1, ca)

nullify(ca)
call alloc_assumed_len(ca)
if (any(ca /= 'qrst')) error stop 12
deallocate(ca)
print *, 'ok'
end program
